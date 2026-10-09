{-# LANGUAGE PatternSynonyms #-}

-- | Tests that need no terminal. Whole editing sessions run against a fake
-- one: keys come from a list, and what is written is played into a small
-- terminal emulator to see what would be on screen.
module Main (main) where

import Control.Monad (filterM)
import Data.Char (isDigit)
import Data.IORef
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)
import qualified Data.Map as Map
import Lambline.Binding
import Lambline.Command
import Lambline.Config
import Lambline.Engine
import Lambline.Helper
import qualified Lambline.History as History
import Lambline.Keys
import qualified Lambline.KillRing as KillRing
import Lambline.Layout hiding (display)
import Lambline.LineBuffer (LineBuffer (..))
import qualified Lambline.LineBuffer as LB
import Lambline.Terminal
import qualified Lambline.Undo as Undo
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let tests = keyTests ++ bufferTests ++ undoTests ++ killRingTests ++ historyTests ++ helperTests ++ renderTests ++ emacsTests ++ viTests
  failures <- map fst <$> filterM (fmap not . snd) tests
  mapM_ (putStrLn . ("FAIL " ++)) failures
  putStrLn (show (length tests) ++ " tests, " ++ show (length failures) ++ " failures")
  if null failures then pure () else exitFailure

type Test = (String, IO Bool)

(==>) :: String -> Bool -> Test
name ==> ok = (name, pure ok)

-- Keys --------------------------------------------------------------------

-- | Characters from a string, all of which have arrived.
newtype Feed a = Feed {run :: String -> (a, String)}

instance Functor Feed where
  fmap f (Feed g) = Feed (\s -> let (a, s') = g s in (f a, s'))

instance Applicative Feed where
  pure a = Feed (\s -> (a, s))
  Feed f <*> Feed g = Feed (\s -> let (h, s') = f s; (a, s'') = g s' in (h a, s''))

instance Monad Feed where
  Feed g >>= k = Feed (\s -> let (a, s') = g s in run (k a) s')

feedSource :: Source Feed
feedSource = Source (Feed pop) (const (Feed (\s -> (not (null s), s))))
  where
    pop = \case
      c : cs -> (Just c, cs)
      [] -> (Nothing, [])

-- | Decode the keys in a string.
keys :: String -> [KeyEvent]
keys "" = []
keys input = case run (readKey feedSource Nothing True) input of
  (Just k, rest) -> k : keys rest
  (Nothing, _) -> []

shifted, alted :: KeyCode -> KeyEvent
shifted k = KeyEvent k noMods {modShift = True}
alted k = KeyEvent k noMods {modAlt = True}

keyTests :: [Test]
keyTests =
  [ "plain characters" ==> (keys "aé" == [Plain (KChar 'a'), Plain (KChar 'é')]),
    "enter, tab, backspace" ==> (keys "\r\t\DEL\b" == [Plain KEnter, Plain KTab, Plain KBackspace, Plain KBackspace]),
    "control keys" ==> (keys "\SOH\ETX\EOT\n\US" == [Ctrl 'A', Ctrl 'C', Ctrl 'D', Ctrl 'J', Ctrl '_']),
    "arrows" ==> (keys "\ESC[A\ESC[B\ESC[C\ESC[D" == map Plain [KUp, KDown, KRight, KLeft]),
    "application-mode arrows" ==> (keys "\ESCOA\ESCOH\ESCOF\ESCOP" == map Plain [KUp, KHome, KEnd, KF 1]),
    "home, end, insert, delete, pages" ==> (keys "\ESC[H\ESC[F\ESC[1~\ESC[4~\ESC[2~\ESC[3~\ESC[5~\ESC[6~" == map Plain [KHome, KEnd, KHome, KEnd, KInsert, KDelete, KPageUp, KPageDown]),
    "function keys" ==> (keys "\ESC[15~\ESC[24~\ESC[[A" == map Plain [KF 5, KF 12, KF 1]),
    "modified arrows" ==> (keys "\ESC[1;5C\ESC[1;3D\ESC[1;2A\ESC[1;6H" == [CtrlKey KRight, AltKey KLeft, shifted KUp, KeyEvent KHome (Mods True False True)]),
    "modified delete" ==> (keys "\ESC[3;5~" == [CtrlKey KDelete]),
    "shift-tab" ==> (keys "\ESC[Z" == [Plain KBackTab]),
    "alt keys" ==> (keys "\ESCb\ESC\r\ESC\DEL" == [Alt 'b', alted KEnter, alted KBackspace]),
    "escape twice is alt" ==> (keys "\ESC\ESC[A" == [alted KUp]),
    "lone escape" ==> (keys "\ESC" == [Plain KEsc]),
    "unknown sequence is one key" ==> (keys "\ESC[99~x" == [Plain KUnknown, Plain (KChar 'x')]),
    "cursor report is ignored" ==> (keys "\ESC[12;40Rx" == [Plain KUnknown, Plain (KChar 'x')]),
    "paste markers" ==> (keys "\ESC[200~\ESC[201~" == map Plain [KPasteStart, KPasteEnd]),
    "pasted text" ==> (run (readPaste feedSource) "a\r\nb\rc\ESC[201~x" == ("a\nb\nc", "x")),
    "ctrl and alt constructors" ==> (ctrl 'a' == Ctrl 'A' && alt 'b' == Alt 'b' && alt '\r' == alted KEnter),
    "normalize" ==> (normalize (KeyEvent (KChar '\t') noMods {modShift = True}) == Plain KBackTab)
  ]

-- Line buffer -------------------------------------------------------------

-- | A buffer written with @|@ for the cursor.
buf :: String -> LineBuffer
buf s = let (l, r) = break (== '|') s in LB.fromParts l (drop 1 r)

unbuf :: LineBuffer -> String
unbuf (LineBuffer t p) = take p t ++ "|" ++ drop p t

moved :: (LineBuffer -> Maybe LineBuffer) -> String -> Maybe String
moved f = fmap unbuf . f . buf

edited :: LB.Edit -> String -> Maybe String
edited f = fmap (unbuf . fst) . f . buf

bufferTests :: [Test]
bufferTests =
  [ "insert" ==> (edited (LB.insert 'x' 2) "a|b" == Just "axx|b"),
    "yank" ==> (edited (LB.yank "xy" 2) "a|b" == Just "axyxy|b"),
    "backward char" ==> (moved (LB.moveBackward 2) "abc|" == Just "a|bc"),
    "backward from start" ==> (moved (LB.moveBackward 1) "|abc" == Nothing),
    "home and end are per line" ==> (moved LB.moveHome "ab\ncd|e" == Just "ab\n|cde" && moved LB.moveEnd "a|b\ncd" == Just "ab|\ncd"),
    "emacs word back" ==> (moved (LB.moveToPrevWord EmacsWord 1) "foo bar  |" == Just "foo |bar  "),
    "emacs word back twice" ==> (moved (LB.moveToPrevWord EmacsWord 2) "foo bar baz|" == Just "foo |bar baz"),
    "emacs word forward" ==> (moved (LB.moveToNextWord AfterEnd EmacsWord 1) "|  foo bar" == Just "  foo| bar"),
    "vi word forward" ==> (moved (LB.moveToNextWord AtStart ViWord 1) "|foo.bar baz" == Just "foo|.bar baz"),
    "vi big word forward" ==> (moved (LB.moveToNextWord AtStart BigWord 1) "|foo.bar baz" == Just "foo.bar |baz"),
    "vi word end" ==> (moved (LB.moveToNextWord BeforeEnd ViWord 1) "|foo bar" == Just "fo|o bar"),
    "vi word end again" ==> (moved (LB.moveToNextWord BeforeEnd ViWord 1) "fo|o bar" == Just "foo ba|r"),
    "line up keeps the column" ==> (moved (LB.moveToLineUp 1 0) "abcd\nef|g" == Just "ab|cd\nefg"),
    "line up to a short line" ==> (moved (LB.moveToLineUp 1 0) "a\nbcd|" == Just "a|\nbcd"),
    "line up from the first line" ==> (moved (LB.moveToLineUp 1 0) "ab|c\nd" == Nothing),
    "line up allows for the prompt" ==> (moved (LB.moveToLineUp 1 2) "abcd\nefg|" == Just "a|bcd\nefg"),
    "line down keeps the column" ==> (moved (LB.moveToLineDown 1 0) "ab|cd\nefg" == Just "abcd\nef|g"),
    "line down allows for the prompt" ==> (moved (LB.moveToLineDown 1 2) "a|bcd\nefgh" == Just "abcd\nefg|h"),
    "line down from the last line" ==> (moved (LB.moveToLineDown 1 0) "a\nb|" == Nothing),
    "find a character" ==> (moved (LB.moveTo (SearchForward ',') 1) "|a,b,c" == Just "a|,b,c"),
    "find the second" ==> (moved (LB.moveTo (SearchForward ',') 2) "|a,b,c" == Just "a,b|,c"),
    "find, stopping short" ==> (moved (LB.moveTo (SearchForwardBefore ',') 1) "|ab,c" == Just "a|b,c"),
    "find backwards" ==> (moved (LB.moveTo (SearchBackward ',') 1) "a,b,c|" == Just "a,b|,c"),
    "kill to line end" ==> (edited (LB.kill EndOfLine) "a|bc\nd" == Just "a|\nd"),
    "kill a line break" ==> (edited (LB.kill EndOfLine) "a|\nd" == Just "a|d"),
    "kill to line start" ==> (edited (LB.kill BeginningOfLine) "x\nab|c" == Just "x\n|c"),
    "kill word back" ==> (edited (LB.kill (BackwardWord 1 EmacsWord)) "foo bar |baz" == Just "foo |baz"),
    "kill word forward" ==> (edited (LB.kill (ForwardWord 1 AfterEnd EmacsWord)) "foo| bar baz" == Just "foo| baz"),
    "kill whole line" ==> (edited (LB.kill WholeLine) "a\nb|c\nd" == Just "a\n|\nd"),
    "kill to a character" ==> (edited (LB.kill (ViCharSearch 1 (SearchForward ','))) "|ab,c" == Just "|c"),
    "kill up to a character" ==> (edited (LB.kill (ViCharSearch 1 (SearchForwardBefore ','))) "|ab,c" == Just "|,c"),
    "copy" ==> (LB.copy (ForwardWord 1 AtStart ViWord) (buf "|foo bar") == Just "foo "),
    "transpose chars" ==> (edited LB.transposeChars "ab|" == Just "ba|" && edited LB.transposeChars "a|bc" == Just "ba|c"),
    "transpose words" ==> (edited (LB.transposeWords 1) "foo bar|" == Just "bar foo|"),
    "capitalize" ==> (edited (LB.editWord LB.Capitalize) "| fOO bar" == Just " Foo| bar"),
    "uppercase" ==> (edited (LB.editWord LB.Uppercase) "|foo bar" == Just "FOO| bar"),
    "indent" ==> (unbuf (fst (LB.indent WholeBuffer 2 False (buf "a\n|b"))) == "  a\n  |b"),
    "dedent" ==> (unbuf (fst (LB.indent WholeBuffer 2 True (buf "   a\n |b"))) == " a\n|b"),
    "end of input ignores trailing blanks" ==> (LB.isEndOfInput (buf "ab|  ") && not (LB.isEndOfInput (buf "a|b")))
  ]

-- Undo --------------------------------------------------------------------

-- | Apply edits, recording them, then undo so many times.
undoing :: Int -> [LB.Edit] -> String
undoing n edits = unbuf (go n lb cs)
  where
    (lb, cs) = foldl step (LB.empty, Undo.new) edits
    step (b, c) e = maybe (b, c) (\(b', changes) -> (b', foldl (flip Undo.record) c changes)) (e b)
    go :: Int -> LineBuffer -> Undo.Changeset -> LineBuffer
    go 0 b _ = b
    go k b c = maybe b (\(b', c') -> go (k - 1) b' c') (Undo.undo 1 b c)

typing :: String -> [LB.Edit]
typing = map (`LB.insert` 1)

undoTests :: [Test]
undoTests =
  [ "typing undoes a word at a time" ==> (undoing 1 (typing "foo bar") == "foo|"),
    "and then the word before" ==> (undoing 2 (typing "foo bar") == "|"),
    "a kill undoes whole" ==> (undoing 1 (typing "foo bar" ++ [LB.kill (BackwardWord 1 EmacsWord)]) == "foo bar|"),
    "undo with nothing to undo" ==> (fmap fst (Undo.undo 1 LB.empty Undo.new) == Nothing),
    "a group undoes as one" ==> (fmap (unbuf . fst) (Undo.undo 1 (buf "a |") grouped) == Just "|"),
    "last insert" ==> (Undo.lastInsert (foldl (flip Undo.record) Undo.new [LB.InsertedChar 0 'a', LB.InsertedChar 1 'b']) == Just "ab")
  ]
  where
    grouped = snd (Undo.end (foldl (flip Undo.record) (snd (Undo.begin Undo.new)) [LB.InsertedChar 0 'a', LB.InsertedChar 1 ' ']))

-- Kill ring ---------------------------------------------------------------

killRingTests :: [Test]
killRingTests =
  [ "yank what was killed" ==> (fmap fst (KillRing.yank (kills ["a"])) == Just "a"),
    "kills in a row join" ==> (fmap fst (KillRing.yank (KillRing.kill "b" KillRing.Append (kills ["a"]))) == Just "ab"),
    "backward kills join at the front" ==> (fmap fst (KillRing.yank (KillRing.kill "b" KillRing.Prepend (kills ["a"]))) == Just "ba"),
    "separate kills are separate entries" ==> (fmap fst (KillRing.yank (kills ["a", "b"])) == Just "b"),
    "yank-pop goes back an entry" ==> ((KillRing.yank (kills ["a", "b"]) >>= KillRing.yankPop . snd >>= \(n, s, _) -> Just (n, s)) == Just (1, "a")),
    "yank-pop needs a yank first" ==> (fmap (\(n, s, _) -> (n, s)) (KillRing.yankPop (kills ["a"])) == Nothing),
    "empty ring" ==> (fmap fst (KillRing.yank (KillRing.new 10)) == Nothing)
  ]
  where
    kills = foldl (\ring s -> KillRing.kill s KillRing.Append (KillRing.reset ring)) (KillRing.new 10)

-- History -----------------------------------------------------------------

historyOf :: Config -> [String] -> History.History
historyOf cfg = foldl (\h e -> snd (History.add e h)) (History.new cfg)

historyTests :: [Test]
historyTests =
  [ "oldest first" ==> (History.entries (some ["a", "b"]) == ["a", "b"]),
    "no immediate repeats" ==> (History.entries (some ["a", "a", "b", "a"]) == ["a", "b", "a"]),
    "repeats if asked" ==> (History.entries (historyOf defaultConfig {historyDuplicates = AlwaysAdd} ["a", "a"]) == ["a", "a"]),
    "nothing empty" ==> (History.entries (some ["", "a"]) == ["a"]),
    "ignore leading space if asked" ==> (History.entries (historyOf defaultConfig {historyIgnoreSpace = True} [" a", "b"]) == ["b"]),
    "limit" ==> (History.entries (historyOf defaultConfig {maxHistorySize = 2} ["a", "b", "c"]) == ["b", "c"]),
    "search backwards" ==> (fmap found (History.search "it" 2 History.Reverse hist) == Just (2, 3)),
    "search from an earlier entry" ==> (fmap found (History.search "git" 1 History.Reverse hist) == Just (0, 3)),
    "search forwards" ==> (fmap found (History.search "l" 0 History.Forward hist) == Just (1, 1)),
    "search finds nothing" ==> (History.search "zz" 2 History.Reverse hist == Nothing),
    "starts with" ==> (fmap found (History.startsWith "git s" 2 History.Reverse hist) == Just (0, 5)),
    "file round trip" ==> (History.decode (History.encode awkward) == awkward),
    "one entry per line, after the version" ==> (length (lines (History.encode awkward)) == length awkward + 1),
    "a file with no version is plain lines" ==> (History.decode "a\\nb\nc\n" == ["a\\nb", "c"]),
    ("save, then load", withFile $ \path -> do
        _ <- History.save path (some awkward)
        (== awkward) . History.entries <$> History.load path (some [])),
    ("append only what is new", withFile $ \path -> do
        saved <- History.save path (some ["a", "b"])
        _ <- History.append path (snd (History.add "c" saved))
        (== ["a", "b", "c"]) . History.entries <$> History.load path (some [])),
    ("append to no file makes one", withFile $ \path -> do
        removeFile path
        _ <- History.append path (some ["a\nb"])
        (== ["a\nb"]) . History.entries <$> History.load path (some [])),
    ("loading no file changes nothing", (== ["a"]) . History.entries <$> History.load "/nonexistent/history" (some ["a"]))
  ]
  where
    withFile test = do
      dir <- getTemporaryDirectory
      let path = dir ++ "/lambline-test-history"
      writeFile path ""
      test path <* removeFile path
    some = historyOf defaultConfig
    hist = some ["git status", "ls", "git log"]
    found r = (History.resultIndex r, History.resultPos r)
    awkward = ["x = \"a\\nb\"", "f (\n  1)\n", "plain", "back\\slash"]

-- Helpers -----------------------------------------------------------------

helperTests :: [Test]
helperTests =
  [ "extract word" ==> (extractWord "ls /usr/lo" Nothing (== ' ') == (3, "/usr/lo")),
    "extract word past an escaped space" ==> (extractWord "cat my\\ fi" (Just '\\') (== ' ') == (4, "my\\ fi")),
    "extract word with none" ==> (extractWord "abc" Nothing (== ' ') == (0, "abc")),
    "common prefix" ==> (longestCommonPrefix (map candidate ["foobar", "foobaz"]) == Just "fooba"),
    "common prefix of one" ==> (longestCommonPrefix [candidate "foo"] == Just "foo"),
    "no common prefix" ==> (longestCommonPrefix (map candidate ["a", "b"]) == Nothing),
    "escape and unescape" ==> (escape (Just '\\') (== ' ') NoQuote "a b" == "a\\ b" && unescape (Just '\\') "a\\ b" == "a b"),
    ("complete a file name", (\(start, cs) -> start == 4 && map replacement cs == ["lambline.cabal"]) <$> filenameCompleter "cat lambline.ca" 15 ctx),
    ("complete a directory", (\(_, cs) -> map replacement cs == ["cbits/"]) <$> filenameCompleter "ls cbi" 6 ctx),
    ("complete inside quotes", (\(start, cs) -> start == 5 && map display cs == ["Main.hs"]) <$> filenameCompleter "cat \"demo/Ma" 12 ctx),
    "history hint" ==> (historyHinter "git s" 5 (Context (historyOf defaultConfig ["git status", "ls"]) 2) == Just (Hint "tatus" (Just "tatus"))),
    "no hint mid-line" ==> (historyHinter "git s" 2 (Context (historyOf defaultConfig ["git status"]) 1) == Nothing),
    "matching bracket, cursor after the close" ==> (matchingBracketHighlighter "(ab)" 4 Other == "\ESC[1;34m(\ESC[0mab)"),
    "matching bracket, cursor on the open" ==> (matchingBracketHighlighter "(ab)" 0 Other == "(ab\ESC[1;34m)\ESC[0m"),
    "nested brackets" ==> (matchingBracketHighlighter "((a))" 0 Other == "((a)\ESC[1;34m)\ESC[0m"),
    "no highlight once accepted" ==> (matchingBracketHighlighter "(ab)" 4 ForcedRefresh == "(ab)"),
    "brackets balanced" ==> (matchingBracketValidator "[x{(x)x}]" == Valid Nothing),
    "bracket open" ==> (matchingBracketValidator "[x{(x)x}x" == Incomplete),
    "bracket mismatched" ==> (matchingBracketValidator "[x{(x)x]" == Invalid (Just "Mismatched brackets: '{' is not properly closed")),
    "bracket unpaired" ==> (matchingBracketValidator "[x]]" == Invalid (Just "Mismatched brackets: ']' is unpaired"))
  ]
  where
    ctx = Context (History.new defaultConfig) 0

-- Rendering ---------------------------------------------------------------

-- | A terminal: what is on it and where the cursor is. As on a real one,
-- writing in the last column does not wrap until the next character.
data Screen = Screen
  { cells :: Map.Map (Int, Int) Char,
    cursorRow :: Int,
    cursorCol :: Int,
    wrapPending :: Bool
  }

blank :: Screen
blank = Screen Map.empty 0 0 False

feed :: Int -> Screen -> String -> Screen
feed width = go
  where
    go s = \case
      [] -> s
      '\ESC' : '[' : rest ->
        let (params, rest') = span (\c -> isDigit c || c `elem` "?;") rest
            n = case filter isDigit (takeWhile (/= ';') params) of "" -> 1; ds -> read ds
         in go (control params n (take 1 rest') s {wrapPending = False}) (drop 1 rest')
      '\r' : rest -> go s {cursorCol = 0, wrapPending = False} rest
      '\n' : rest -> go s {cursorRow = cursorRow s + 1, cursorCol = 0, wrapPending = False} rest
      '\a' : rest -> go s rest
      c : rest ->
        let s1 = if wrapPending s then s {cursorRow = cursorRow s + 1, cursorCol = 0, wrapPending = False} else s
            s2 = s1 {cells = Map.insert (cursorRow s1, cursorCol s1) c (cells s1)}
         in go (if cursorCol s1 == width - 1 then s2 {wrapPending = True} else s2 {cursorCol = cursorCol s1 + 1}) rest
    control params n final s = case final of
      "A" -> s {cursorRow = max 0 (cursorRow s - n)}
      "B" -> s {cursorRow = cursorRow s + n}
      "C" -> s {cursorCol = min (width - 1) (cursorCol s + n)}
      "H" -> s {cursorRow = 0, cursorCol = 0}
      "J"
        | params == "2" -> s {cells = Map.empty}
        | otherwise -> s {cells = Map.filterWithKey (\k _ -> k < (cursorRow s, cursorCol s)) (cells s)}
      "K" -> s {cells = Map.filterWithKey (\(r, c) _ -> r /= cursorRow s || c < cursorCol s) (cells s)}
      -- Colours and terminal modes change nothing that is checked here.
      _ -> s

contents :: Screen -> [String]
contents s = dropWhileEnd null [line r | r <- [0 .. maximum (0 : map fst (Map.keys (cells s)))]]
  where
    line r = dropWhileEnd (== ' ') [Map.findWithDefault ' ' (r, c) (cells s) | c <- [0 .. maximum (0 : [c' | (r', c') <- Map.keys (cells s), r' == r])]]

-- | What should be on screen, worked out the long way: each line of the
-- text, with its prompt, cut into rows.
expected :: Int -> String -> String -> String -> ([String], (Int, Int))
expected width prompt cont s = (dropWhileEnd null (map (dropWhileEnd (== ' ')) (concatMap rows shownLines)), cursor)
  where
    shownLines = zipWith (++) (prompt : repeat cont) (splitLines s)
    rows "" = [""]
    rows l = let (a, b) = splitAt width l in a : if null b then [] else rows b
    splitLines t = case break (== '\n') t of
      (l, _ : rest) -> l : splitLines rest
      (l, []) -> [l]
    cursor =
      let lastLine = last shownLines
       in (sum (map (length . rows) (init shownLines)) + length lastLine `div` width, length lastLine `mod` width)

-- | Draw a series of buffers one after another on the same screen, as an
-- editing session would, and check each against what should be there.
renderTests :: [Test]
renderTests =
  [ ("render at width " ++ show width ++ " with prompts " ++ show (prompt, cont))
      ==> and (snd (foldl (step width prompt cont) ((blank, 0), []) buffers))
    | width <- [4, 7, 10, 40],
      (prompt, cont) <- [("> ", "| "), ("", ""), ("ab> ", "")]
  ]
    ++ [ "a hint is drawn after the input" ==> (shown (plain {frameInfo = Just "llo", frameInfoStyled = Just "\ESC[2mllo\ESC[0m"}) == (["> hello"], (0, 4))),
         "styling takes no room" ==> (shown (plain {frameLineStyled = "\ESC[31mhe\ESC[0m"}) == (["> he"], (0, 4))),
         "a tab runs to the next stop" ==> (snd (shown (plain {frameBefore = "a\tb", frameLine = "a\tb", frameLineStyled = "a\tb"})) == (0, 9)),
         "wide characters take two columns" ==> (position 10 8 "日本" == Pos 0 4 && strWidth "日本" == 4)
       ]
  where
    texts = ["", "x", "hello world", "12345678", "ab\ncd", "one\n\nthree", "trailing\n", "123456\n12345678\n1234", "a long line that wraps\nand another one that does too\nend"]
    buffers = [splitAt i t | t <- texts ++ reverse texts, i <- [0 .. length t]]
    step width prompt cont ((screen, oldRow), results) (l, r) =
      let (output, cursor, _) = render (Frame width 8 oldRow prompt prompt cont l (l ++ r) (l ++ r) Nothing Nothing)
          screen' = feed width screen output
          (wantLines, _) = expected width prompt cont (l ++ r)
          (_, wantHere) = expected width prompt cont l
          ok = contents screen' == wantLines && (cursorRow screen', cursorCol screen') == wantHere && (row cursor, col cursor) == wantHere
       in ((screen', row cursor), ok : results)
    plain = Frame 40 8 0 "> " "> " "" "he" "he" "he" Nothing Nothing
    shown f = let (output, cursor, _) = render f in (contents (feed 40 blank output), (row cursor, col cursor))

-- Sessions ----------------------------------------------------------------

data Session = Session
  { sConfig :: Config,
    sHelper :: Maybe Helper,
    sBindings :: Bindings,
    sHistory :: [String],
    -- | What a bracketed paste holds.
    sPaste :: String
  }

session :: Session
session = Session defaultConfig Nothing Map.empty [] ""

data Outcome = Outcome
  { result :: Either ReadlineError String,
    -- | The screen just before the last key, and at the end.
    before :: [String],
    after :: [String],
    written :: String
  }

-- | Press keys in an editor and see what comes of it.
play :: Session -> [KeyEvent] -> IO Outcome
play s input = do
  pending <- newIORef input
  out <- newIORef ""
  snapshot <- newIORef ""
  let term =
        Term
          { termEvent = \_ -> do
              remaining <- readIORef pending
              case remaining of
                [] -> pure EndOfInput
                k : ks -> do
                  writeIORef pending ks
                  if null ks then readIORef out >>= writeIORef snapshot else pure ()
                  pure (Key k),
            termChar = atomicModifyIORef' pending $ \case
              KeyEvent (KChar c) _ : ks -> (ks, Just c)
              Plain KTab : ks -> (ks, Just '\t')
              ks -> (drop 1 ks, Nothing),
            termPaste = pure (sPaste s),
            termWrite = \str -> modifyIORef' out (++ str),
            termColumns = pure 40,
            termRows = pure 24,
            termColors = True,
            termBindings = [(Ctrl 'D', EndOfFile), (Ctrl 'C', Interrupt), (Ctrl 'Z', Suspend)],
            termSuspend = pure (),
            termCursorColumn = pure (Just 1)
          }
      env = Env term (sConfig s) (sHelper s) (historyOf (sConfig s) (sHistory s)) (sBindings s) "> "
  (r, _) <- edit env ("", "") (KillRing.new 60)
  o <- readIORef out
  snap <- readIORef snapshot
  pure (Outcome r (contents (feed 40 blank snap)) (contents (feed 40 blank o)) o)

typed :: String -> [KeyEvent]
typed = map key

enter, esc, tab, up, down, left, right, backspace :: KeyEvent
enter = Plain KEnter
esc = Plain KEsc
tab = Plain KTab
up = Plain KUp
down = Plain KDown
left = Plain KLeft
right = Plain KRight
backspace = Plain KBackspace

-- | A session's keys, then Enter, give this line.
gives :: Session -> [KeyEvent] -> String -> IO Bool
gives s input want = (== Right want) . result <$> play s (input ++ [enter])

fails :: Session -> [KeyEvent] -> ReadlineError -> IO Bool
fails s input want = (== Left want) . result <$> play s input

-- | A session's keys leave this on screen, before Enter is pressed.
leaves :: Session -> [KeyEvent] -> [String] -> IO Bool
leaves s input want = (== want) . before <$> play s (input ++ [enter])

-- | Complete the word before the cursor from a list.
wordCompleter :: [String] -> String -> Int -> Context -> IO (Int, [Candidate])
wordCompleter wordList line p _ = pure (start, [candidate w | w <- wordList, word `isPrefixOf` w])
  where
    (start, word) = extractWord (take p line) Nothing (== ' ')

completing :: Session
completing = session {sHelper = Just defaultHelper {complete = wordCompleter ["foobar", "foobaz", "quux"]}}

emacsTests :: [Test]
emacsTests =
  [ ("typing", gives session (typed "hello") "hello"),
    ("start and end of line", gives session (typed "bc" ++ [Ctrl 'A'] ++ typed "a" ++ [Ctrl 'E'] ++ typed "d") "abcd"),
    ("arrows", gives session (typed "ac" ++ [left] ++ typed "b" ++ [right] ++ typed "d") "abcd"),
    ("backspace and delete", gives session (typed "abcd" ++ [backspace, Ctrl 'A', Plain KDelete]) "bc"),
    ("word motion", gives session (typed "foo bar baz" ++ [Alt 'b', Alt 'b'] ++ typed "X" ++ [Alt 'f'] ++ typed "Y") "foo XbarY baz"),
    ("ctrl-arrows move by word", gives session (typed "foo bar" ++ [CtrlKey KLeft] ++ typed "X") "foo Xbar"),
    ("kill and yank", gives session (typed "foo bar" ++ [Ctrl 'W', Ctrl 'A', Ctrl 'Y']) "barfoo "),
    ("kills in a row join", gives session (typed "one two three" ++ [AltKey KBackspace, AltKey KBackspace, Ctrl 'A', Ctrl 'Y']) "two threeone "),
    ("kill to end of line", gives session (typed "foo bar" ++ [Alt 'b', Ctrl 'K', Ctrl 'A', Ctrl 'Y']) "barfoo "),
    ("kill word forward", gives session (typed "foo bar" ++ [Ctrl 'A', Alt 'd']) " bar"),
    ("yank-pop", gives session (typed "aa" ++ [Ctrl 'U'] ++ typed "bb" ++ [Ctrl 'U', Ctrl 'Y', Alt 'y']) "aa"),
    ("undo", gives session (typed "foo bar" ++ [Ctrl '_']) "foo"),
    ("undo a kill", gives session (typed "foo bar" ++ [Ctrl 'U', Ctrl 'X', Ctrl 'U']) "foo bar"),
    ("transpose characters", gives session (typed "ab" ++ [Ctrl 'T']) "ba"),
    ("transpose words", gives session (typed "foo bar" ++ [Alt 't']) "bar foo"),
    ("change case", gives session (typed "foo bar baz" ++ [Ctrl 'A', Alt 'u', Alt 'c', Alt 'c', Alt 'b', Alt 'l']) "FOO Bar baz"),
    ("numeric argument", gives session ([Alt '3'] ++ typed "x") "xxx"),
    ("numeric argument of two digits", gives session ([Alt '1', Plain (KChar '2')] ++ typed "x") (replicate 12 'x')),
    ("negative argument reverses", gives session (typed "abc" ++ [Ctrl 'A', Alt '-', Ctrl 'B'] ++ typed "X") "aXbc"),
    ("the argument shows in the prompt", (== ["(arg: 3)"]) . before <$> play session [Alt '3', Plain (KChar 'x')]),
    ("character search", gives session (typed "ab,c" ++ [Ctrl 'A', Ctrl ']', Plain (KChar ',')] ++ typed "X") "aXb,c"),
    ("character search backwards", gives session (typed "a,bc" ++ [CtrlAlt ']', Plain (KChar ',')] ++ typed "X") "aX,bc"),
    ("quoted insert", gives session ([Ctrl 'V', tab] ++ typed "x") "\tx"),
    ("paste is not submitted", gives session {sPaste = "a\nb"} [Plain KPasteStart] "a\nb"),
    ("ctrl-d on empty input", fails session [Ctrl 'D'] Eof),
    ("ctrl-d deletes otherwise", gives session (typed "ab" ++ [Ctrl 'A', Ctrl 'D']) "b"),
    ("ctrl-c", fails session (typed "ab" ++ [Ctrl 'C']) Interrupted),
    ("the input ends", fails session (typed "ab") Eof),
    ("clear screen", leaves session (typed "ab" ++ [Ctrl 'L']) ["> ab"]),
    -- History.
    ("previous entries", gives session {sHistory = ["one", "two"]} [up, up] "one"),
    ("and back", gives session {sHistory = ["one", "two"]} [up, up, down] "two"),
    ("the input is kept", gives session {sHistory = ["one"]} (typed "new" ++ [up, down]) "new"),
    ("oldest and newest", gives session {sHistory = ["one", "two", "three"]} [Alt '<'] "one"),
    ("and back to the input", gives session {sHistory = ["one", "two"]} (typed "x" ++ [Alt '<', Alt '>']) "x"),
    ("ctrl-p and ctrl-n", gives session {sHistory = ["one", "two"]} [Ctrl 'P', Ctrl 'P', Ctrl 'N'] "two"),
    ("reverse search", gives session {sHistory = gitHistory} ([Ctrl 'R'] ++ typed "git") "git log"),
    ("search further back", gives session {sHistory = gitHistory} ([Ctrl 'R'] ++ typed "git" ++ [Ctrl 'R']) "git status"),
    ("search shows its prompt", leaves session {sHistory = gitHistory} ([Ctrl 'R'] ++ typed "st") ["(reverse-i-search)`st': git status"]),
    ("a failed search says so", leaves session {sHistory = gitHistory} ([Ctrl 'R'] ++ typed "zz") ["(failed reverse-i-search)`zz':"]),
    ("abandon a search", gives session {sHistory = gitHistory} (typed "x" ++ [Ctrl 'R'] ++ typed "git" ++ [Ctrl 'G']) "x"),
    ("edit what a search found", gives session {sHistory = gitHistory} ([Ctrl 'R'] ++ typed "log" ++ [Ctrl 'E'] ++ typed " -p") "git log -p"),
    ("prefix search, when bound", gives session {sHistory = gitHistory, sBindings = Map.fromList [(KeySeq [Alt 'p'], Simple HistorySearchBackward)]} (typed "git s" ++ [Alt 'p']) "git status"),
    -- Completion.
    ("complete", gives completing (typed "qu" ++ [tab]) "quux"),
    ("each tab is the next candidate", gives completing (typed "foo" ++ [tab, tab]) "foobaz"),
    ("then the input as it was", gives completing (typed "foo" ++ [tab, tab, tab]) "foo"),
    ("shift-tab goes back", gives completing (typed "foo" ++ [tab, tab, Plain KBackTab]) "foobar"),
    ("abandon a completion", gives completing (typed "foo" ++ [tab, Ctrl 'G']) "foo"),
    ("carry on after a completion", gives completing (typed "qu" ++ [tab] ++ typed " x") "quux x"),
    ("complete mid-line", gives completing (typed "qu z" ++ [left, left, tab]) "quux z"),
    ("undo a completion", gives completing (typed "qu" ++ [tab, Plain (KChar ' '), Ctrl '_', Ctrl '_']) "qu"),
    ("nothing to complete", gives completing (typed "zz" ++ [tab]) "zz"),
    ("list: complete what is common", gives listing (typed "f" ++ [tab]) "fooba"),
    ("list: a second tab lists", (== ["> fooba", "foobar  foobaz", "> fooba"]) . after <$> play listing (typed "f" ++ [tab, tab, enter])),
    ("list: a lone candidate completes", gives listing (typed "q" ++ [tab]) "quux"),
    ("list: ask before a long list", any ("Display all 2 possibilities? (y or n)" `isInfixOf`) . before <$> play listing {sConfig = (sConfig listing) {completionPromptLimit = 1}} (typed "f" ++ [tab, tab, Plain (KChar 'n')])),
    -- Hints.
    ("a hint is shown", leaves hinting (typed "git s") ["> git status"]),
    ("right accepts it", gives hinting (typed "git s" ++ [right]) "git status"),
    ("no hint mid-line", leaves hinting (typed "git s" ++ [left]) ["> git s"]),
    ("the accepted input has no hint", (== ["> git s"]) . after <$> play hinting (typed "git s" ++ [enter])),
    ("hints are highlighted", ("\ESC[2mtatus\ESC[0m" `isInfixOf`) . written <$> play hinting {sHelper = fmap (\h -> h {highlightHint = \t -> "\ESC[2m" ++ t ++ "\ESC[0m"}) (sHelper hinting)} (typed "git s" ++ [enter])),
    -- Highlighting.
    ("the input is highlighted", ("<a>" `isInfixOf`) . written <$> play session {sHelper = Just defaultHelper {highlight = \l _ _ -> "<" ++ l ++ ">"}} (typed "a" ++ [enter])),
    ("the prompt is highlighted", ("[> ]" `isInfixOf`) . written <$> play session {sHelper = Just defaultHelper {highlightPrompt = \p _ -> "[" ++ p ++ "]"}} [enter]),
    ("the matching bracket is highlighted", ("\ESC[1;34m(\ESC[0mab)" `isInfixOf`) . written <$> play bracketed (typed "(ab)" ++ [enter])),
    -- Validation.
    ("unfinished input continues", gives bracketed (typed "f (" ++ [enter] ++ typed "x)") "f (\nx)"),
    ("each line shows its prompt", leaves bracketed {sConfig = defaultConfig {continuationPrompt = ". "}} (typed "f (" ++ [enter] ++ typed "x)") ["> f (", ". x)"]),
    ("wrong input stays, with the reason", (== "> f )Mismatched brackets: ')' is unpaired") . concat . before <$> play bracketed (typed "f )" ++ [enter, enter])),
    ("and can be put right", gives bracketed (typed "f )" ++ [enter, backspace]) "f "),
    ("a message with accepted input", (== ["> ok fine"]) . after <$> play session {sHelper = Just defaultHelper {validate = const (Valid (Just " fine"))}} (typed "ok" ++ [enter])),
    ("enter mid-input accepts", gives bracketed (typed "(ab)" ++ [left, left]) "(ab)"),
    ("up and down move between lines", gives bracketed (typed "(abcd" ++ [enter] ++ typed "efgh)" ++ [up] ++ typed "X" ++ [down] ++ typed "Y") "(abXcd\nefgh)Y"),
    ("with a continuation prompt, by column", gives bracketed {sConfig = defaultConfig {continuationPrompt = ". "}} (typed "(abcd" ++ [enter] ++ typed "efgh)" ++ [up] ++ typed "X") "(abcdX\nefgh)"),
    ("up from the first line is history", gives bracketed {sHistory = ["old"]} (typed "(a" ++ [enter] ++ typed "b)" ++ [up, up]) "old"),
    ("a history entry of several lines", gives bracketed {sHistory = ["(a\nb)"]} ([up, up] ++ typed "X") "X(a\nb)"),
    -- Bindings.
    ("bind a key", gives session {sBindings = Map.fromList [(KeySeq [alt '\r'], Simple Newline)]} (typed "a" ++ [alt '\r'] ++ typed "b") "a\nb"),
    ("bind a sequence", gives session {sBindings = Map.fromList [(KeySeq [Ctrl 'X', Ctrl 'E'], Simple (Insert 1 "!"))]} (typed "a" ++ [Ctrl 'X', Ctrl 'E']) "a!"),
    ("other sequences still work", gives session {sBindings = Map.fromList [(KeySeq [Ctrl 'X', Ctrl 'E'], Simple (Insert 1 "!"))]} (typed "a b" ++ [Ctrl 'U', Ctrl 'X', Ctrl 'U']) "a b"),
    ("bind conditionally", gives completing {sBindings = indentOrComplete} ([tab] ++ typed "qu" ++ [tab]) "  quux"),
    ("bind every key", gives session {sBindings = Map.fromList [(AnyKey, Conditional (\_ _ _ c -> if contextLine c == "ab" then Just (Insert 1 "!") else Nothing))]} (typed "abc") "ab!")
  ]
  where
    gitHistory = ["git status", "ls", "git log"]
    listing = completing {sConfig = defaultConfig {completionType = List}}
    hinting = session {sHelper = Just defaultHelper {hint = historyHinter}, sHistory = gitHistory}
    bracketed = session {sHelper = Just defaultHelper {validate = matchingBracketValidator, highlight = matchingBracketHighlighter}}
    -- Tab indents at the start of a line and completes elsewhere.
    indentOrComplete = Map.fromList [(KeySeq [tab], Conditional (\_ _ _ c -> if all (== ' ') (take (contextPos c) (contextLine c)) then Just (Insert 1 "  ") else Nothing))]

vi :: Session
vi = session {sConfig = defaultConfig {editMode = Vi}}

viTests :: [Test]
viTests =
  [ ("vi: typing", gives vi (typed "hello") "hello"),
    ("vi: escape, then a command", gives vi (typed "hello" ++ [esc] ++ typed "0x") "ello"),
    ("vi: enter in command mode", gives vi (typed "hello" ++ [esc]) "hello"),
    ("vi: h and l", gives vi (typed "abcd" ++ [esc] ++ typed "hhiX") "aXbcd"),
    ("vi: a appends", gives vi (typed "ac" ++ [esc] ++ typed "0ab") "abc"),
    ("vi: A and I", gives vi (typed "b" ++ [esc] ++ typed "Ia" ++ [esc] ++ typed "Ac") "abc"),
    ("vi: w, b and e", gives vi (typed "foo bar baz" ++ [esc] ++ typed "0wiX" ++ [esc] ++ typed "eaY" ++ [esc] ++ typed "bbiZ") "Zfoo XbarY baz"),
    ("vi: $ and ^", gives vi (typed "  foo" ++ [esc] ++ typed "^iX" ++ [esc] ++ typed "$aY") "  XfooY"),
    ("vi: x with a count", gives vi (typed "abcdef" ++ [esc] ++ typed "03x") "def"),
    ("vi: X", gives vi (typed "abc" ++ [esc] ++ typed "X") "ac"),
    ("vi: dw", gives vi (typed "foo bar" ++ [esc] ++ typed "0dw") "bar"),
    ("vi: d with a count", gives vi (typed "a b c d" ++ [esc] ++ typed "0d2w") "c d"),
    ("vi: a count before d", gives vi (typed "a b c d" ++ [esc] ++ typed "02dw") "c d"),
    ("vi: D", gives vi (typed "foo bar" ++ [esc] ++ typed "0wD") "foo "),
    ("vi: dd", gives vi (typed "foo" ++ [esc] ++ typed "dd") ""),
    ("vi: db", gives vi (typed "foo bar" ++ [esc] ++ typed "db") "foo r"),
    ("vi: df and dt", gives vi (typed "a,b;c" ++ [esc] ++ typed "0df,dt;") ";c"),
    ("vi: cw", gives vi (typed "foo bar" ++ [esc] ++ typed "0cwbaz") "baz bar"),
    ("vi: C", gives vi (typed "foo bar" ++ [esc] ++ typed "0wCx") "foo x"),
    ("vi: s and S", gives vi (typed "abc" ++ [esc] ++ typed "0sX" ++ [esc] ++ typed "SY") "Y"),
    ("vi: r", gives vi (typed "abc" ++ [esc] ++ typed "0rX") "Xbc"),
    ("vi: R overwrites", gives vi (typed "abc" ++ [esc] ++ typed "0RXY") "XYc"),
    ("vi: yank and put", gives vi (typed "foo bar" ++ [esc] ++ typed "0ywP") "foo foo bar"),
    ("vi: put after", gives vi (typed "ab cd" ++ [esc] ++ typed "0dwp") "cab d"),
    ("vi: f, ; and ,", gives vi (typed "a,b,c,d" ++ [esc] ++ typed "0f,;;,iX") "a,bX,c,d"),
    ("vi: t and T", gives vi (typed "ab,cd" ++ [esc] ++ typed "0t,iX" ++ [esc] ++ typed "$T,iY") "aXb,Ycd"),
    ("vi: repeat a deletion", gives vi (typed "abcdef" ++ [esc] ++ typed "0x..") "def"),
    ("vi: repeat with a new count", gives vi (typed "abcdef" ++ [esc] ++ typed "0x3.") "ef"),
    ("vi: repeat an insertion", gives vi (typed "c" ++ [esc] ++ typed "0iab" ++ [esc] ++ typed ".") "aabbc"),
    ("vi: undo", gives vi (typed "foo bar" ++ [esc] ++ typed "0dwu") "foo bar"),
    ("vi: undo an insertion as one", gives vi (typed "x" ++ [esc] ++ typed "afoo bar" ++ [esc] ++ typed "u") "x"),
    ("vi: indent and dedent", gives vi (typed "a" ++ [esc] ++ typed ">>>><<") "  a"),
    ("vi: alt with a key is the command", gives vi (typed "abc" ++ [Alt '0'] ++ typed "x") "bc"),
    ("vi: history with k and j", gives vi {sHistory = ["one", "two"]} ([esc] ++ typed "kkj") "two"),
    ("vi: search", gives vi {sHistory = ["git status", "ls"]} ([esc, Ctrl 'R'] ++ typed "git") "git status"),
    ("vi: completion in insert mode", gives vi {sHelper = sHelper completing} (typed "qu" ++ [tab]) "quux"),
    ("vi: ctrl-d submits", (== Right "ab") . result <$> play vi (typed "ab" ++ [Ctrl 'D'])),
    ("vi: the count shows in the prompt", (== ["(arg: 12) abc"]) . before <$> play vi (typed "abc" ++ [esc] ++ typed "12x"))
  ]
