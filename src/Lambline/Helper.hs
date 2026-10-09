-- | What an application supplies to make the editor understand its input:
-- completion, hints, highlighting and validation.
module Lambline.Helper
  ( Helper (..),
    defaultHelper,
    Context (..),

    -- * Completion
    Candidate (..),
    candidate,
    filenameCompleter,
    extractWord,
    longestCommonPrefix,
    escape,
    unescape,
    Quote (..),

    -- * Hints
    Hint (..),
    historyHinter,

    -- * Highlighting
    CmdKind (..),
    matchingBracketHighlighter,

    -- * Validation
    ValidationResult (..),
    matchingBracketValidator,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Char (toLower)
import Data.List (isPrefixOf, sortOn)
import Lambline.Config (CompletionType)
import Lambline.History (History)
import qualified Lambline.History as History
import System.Directory (doesDirectoryExist, doesPathExist, getHomeDirectory, listDirectory)
import System.Info (os)

-- | The parts default to doing nothing, so build one by overriding
-- 'defaultHelper':
--
-- > defaultHelper {complete = filenameCompleter, hint = historyHinter}
data Helper = Helper
  { -- | Given the input and the cursor's position in it, the position
    -- where the text to complete starts, and what could replace it.
    complete :: String -> Int -> Context -> IO (Int, [Candidate]),
    -- | A suggestion to show after the input, given it and the cursor's
    -- position. Only asked for, by convention, with the cursor at the end.
    hint :: String -> Int -> Context -> Maybe Hint,
    -- | The input with styling added, given the cursor's position and why
    -- it is being redrawn. Styling means control sequences such as
    -- colours; it must not change the width of the text.
    highlight :: String -> Int -> CmdKind -> String,
    -- | The prompt with styling added. The flag is false for a prompt
    -- that stands in for the usual one, such as that of a history search.
    highlightPrompt :: String -> Bool -> String,
    highlightHint :: String -> String,
    -- | A completion candidate with styling added, for a listing.
    highlightCandidate :: String -> CompletionType -> String,
    -- | Whether the input is finished, asked when Enter is pressed.
    validate :: String -> ValidationResult
  }

defaultHelper :: Helper
defaultHelper =
  Helper
    { complete = \_ _ _ -> pure (0, []),
      hint = \_ _ _ -> Nothing,
      highlight = \line _ _ -> line,
      highlightPrompt = const,
      highlightHint = id,
      highlightCandidate = const,
      validate = const (Valid Nothing)
    }

-- | What a completer or hinter can see besides the input.
data Context = Context
  { contextHistory :: History,
    -- | Which history entry is being edited; the history's size if none.
    contextHistoryIndex :: Int
  }

-- Completion --------------------------------------------------------------

data Candidate = Candidate
  { -- | What to show in a list of candidates.
    display :: String,
    -- | What to put in the input.
    replacement :: String
  }
  deriving (Eq, Show)

-- | A candidate shown as what it inserts.
candidate :: String -> Candidate
candidate s = Candidate s s

data Quote = DoubleQuote | SingleQuote | NoQuote
  deriving (Eq, Show)

-- | Complete the file name before the cursor, in or out of quotes.
filenameCompleter :: String -> Int -> Context -> IO (Int, [Candidate])
filenameCompleter line p _ = do
  let before = take p line
      (start, path, escChar, breaks, quote) = case findUnclosedQuote before of
        Just (i, DoubleQuote) -> (i + 1, unescape (Just '\\') (drop (i + 1) before), Just '\\', (`elem` "\"$\\`"), DoubleQuote)
        Just (i, quote') -> (i + 1, drop (i + 1) before, Nothing, isBreakChar, quote')
        Nothing ->
          let (s, word) = extractWord before (Just '\\') isBreakChar
           in (s, unescape (Just '\\') word, Just '\\', isBreakChar, NoQuote)
  matches <- filenameComplete path escChar breaks quote
  pure (start, sortOn display matches)

-- | Characters that end a word on a command line.
isBreakChar :: Char -> Bool
isBreakChar = (`elem` " \t\n\"\\'`@$><=;|&{(\0")

filenameComplete :: String -> Maybe Char -> (Char -> Bool) -> Quote -> IO [Candidate]
filenameComplete path escChar breaks quote = do
  home <- getHomeDirectory
  let (dirName, fileName) = let (f, d) = break (== '/') (reverse path) in (reverse d, reverse f)
      dir = case dirName of
        "" -> "."
        '~' : rest@('/' : _) -> home ++ rest
        _ -> dirName
      normal = if os == "darwin" then map toLower else id
  names <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
  matching <- filterM (\n -> doesPathExist (dir ++ "/" ++ n)) [n | n <- names, normal fileName `isPrefixOf` normal n]
  mapM (entry dir dirName) matching
  where
    entry dir dirName name = do
      isDir <- doesDirectoryExist (dir ++ "/" ++ name)
      pure (Candidate name (escape escChar breaks quote (dirName ++ name ++ if isDir then "/" else "")))

-- | Remove an escape character from before whatever it escapes.
unescape :: Maybe Char -> String -> String
unescape Nothing s = s
unescape (Just esc) s = go s
  where
    go = \case
      c : d : cs | c == esc -> d : go cs
      [c] | c == esc -> []
      c : cs -> c : go cs
      [] -> []

-- | Put an escape character before each break character. Nothing is
-- escaped in single quotes.
escape :: Maybe Char -> (Char -> Bool) -> Quote -> String -> String
escape escChar breaks quote s = case escChar of
  Just esc | quote /= SingleQuote -> concatMap (\c -> if breaks c then [esc, c] else [c]) s
  _ -> s

-- | The word ending at the end of the text, and where it starts. A break
-- character does not end a word if it is escaped.
extractWord :: String -> Maybe Char -> (Char -> Bool) -> (Int, String)
extractWord line escChar breaks = let start = go Nothing (reverse (zip [0 ..] line)) in (start, drop start line)
  where
    go found = \case
      [] -> maybe 0 id found
      (i, c) : rest -> case (escChar, found) of
        (Just esc, Just s)
          | c == esc -> go Nothing rest
          | otherwise -> s
        _
          | breaks c -> if escChar == Nothing then i + 1 else go (Just (i + 1)) rest
          | otherwise -> go found rest

-- | What every candidate's replacement starts with.
longestCommonPrefix :: [Candidate] -> Maybe String
longestCommonPrefix cs = case map replacement cs of
  [] -> Nothing
  [r] -> Just r
  r : rs -> case foldl common r rs of
    "" -> Nothing
    prefix -> Just prefix
  where
    common a b = map fst (takeWhile (uncurry (==)) (zip a b))

-- | The last quote in the text that is still open.
findUnclosedQuote :: String -> Maybe (Int, Quote)
findUnclosedQuote = go Nothing . zip [0 ..]
  where
    go open = \case
      [] -> open
      (i, c) : rest -> case open of
        Just (_, DoubleQuote)
          | c == '\\' -> go open (drop 1 rest)
          | c == '"' -> go Nothing rest
        Just (_, SingleQuote) | c == '\'' -> go Nothing rest
        Just _ -> go open rest
        Nothing
          | c == '\\' -> go open (drop 1 rest)
          | c == '"' -> go (Just (i, DoubleQuote)) rest
          | c == '\'' -> go (Just (i, SingleQuote)) rest
          | otherwise -> go open rest

-- Hints -------------------------------------------------------------------

data Hint = Hint
  { -- | What to show after the input.
    hintDisplay :: String,
    -- | What accepting the hint inserts, if it can be accepted.
    hintCompletion :: Maybe String
  }
  deriving (Eq, Show)

-- | Suggest the rest of the most recent history entry that starts with the
-- input.
historyHinter :: String -> Int -> Context -> Maybe Hint
historyHinter line p ctx
  | null line || p < length line = Nothing
  | otherwise = do
      let h = contextHistory ctx
          start = if contextHistoryIndex ctx == History.size h then contextHistoryIndex ctx - 1 else contextHistoryIndex ctx
      found <- History.startsWith line start History.Reverse h
      let rest = drop p (History.resultEntry found)
      if null rest then Nothing else Just (Hint rest (Just rest))

-- Highlighting ------------------------------------------------------------

-- | Why the input is being redrawn.
data CmdKind
  = -- | The cursor moved.
    MoveCursor
  | Other
  | -- | The input is being left behind, accepted or abandoned: draw it as
    -- it should stay.
    ForcedRefresh
  deriving (Eq, Show)

-- | Show in bold blue the bracket matching the one under or before the
-- cursor.
matchingBracketHighlighter :: String -> Int -> CmdKind -> String
matchingBracketHighlighter line p kind
  | kind == ForcedRefresh || length line <= 1 = line
  | otherwise = case checkBracket line p >>= \(b, i) -> findMatchingBracket line i b of
      Just (i, m) -> take i line ++ "\ESC[1;34m" ++ [m] ++ "\ESC[0m" ++ drop (i + 1) line
      Nothing -> line

-- | The bracket under the cursor, or failing that before it.
checkBracket :: String -> Int -> Maybe (Char, Int)
checkBracket line p
  | null line = Nothing
  | p >= length line = let i = length line - 1; b = line !! i in if isClose b then Just (b, i) else Nothing
  | otherwise = go True p
  where
    go underCursor i
      | isClose b = if i == 0 then Nothing else Just (b, i)
      | isOpen b = if i + 1 == length line then Nothing else Just (b, i)
      | underCursor && i > 0 = go False (i - 1)
      | otherwise = Nothing
      where
        b = line !! i

findMatchingBracket :: String -> Int -> Char -> Maybe (Int, Char)
findMatchingBracket line p bracket
  | isOpen bracket = scan (drop (p + 1) (zip [0 ..] line))
  | otherwise = scan (reverse (take p (zip [0 ..] line)))
  where
    matching = matchingBracket bracket
    scan = go (1 :: Int)
    go unmatched = \case
      [] -> Nothing
      (i, c) : rest
        | c == matching -> if unmatched == 1 then Just (i, matching) else go (unmatched - 1) rest
        | c == bracket -> go (unmatched + 1) rest
        | otherwise -> go unmatched rest

isOpen, isClose :: Char -> Bool
isOpen = (`elem` "([{")
isClose = (`elem` ")]}")

matchingBracket :: Char -> Char
matchingBracket c = maybe c id (lookup c (zip "([{)]}" ")]}([{"))

-- Validation --------------------------------------------------------------

data ValidationResult
  = -- | More input is needed: Enter starts a new line.
    Incomplete
  | -- | The input is wrong and stays in the editor, with a message shown
    -- after it if there is one.
    Invalid (Maybe String)
  | -- | Accept the input, showing a message after it if there is one.
    Valid (Maybe String)
  deriving (Eq, Show)

-- | Input is incomplete while it has an unclosed bracket, and invalid if
-- its brackets do not match.
matchingBracketValidator :: String -> ValidationResult
matchingBracketValidator = go ""
  where
    go stack = \case
      [] -> if null stack then Valid Nothing else Incomplete
      c : cs
        | isOpen c -> go (c : stack) cs
        | isClose c -> case stack of
            open : rest
              | matchingBracket open == c -> go rest cs
              | otherwise -> Invalid (Just ("Mismatched brackets: " ++ show open ++ " is not properly closed"))
            [] -> Invalid (Just ("Mismatched brackets: " ++ show c ++ " is unpaired"))
        | otherwise -> go stack cs
