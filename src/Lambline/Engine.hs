{-# LANGUAGE PatternSynonyms #-}

-- | The editor proper: reading keys, turning them into commands in Emacs
-- or Vi mode, and carrying the commands out.
module Lambline.Engine
  ( ReadlineError (..),
    Env (..),
    edit,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isDigit, ord, toLower)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Lambline.Binding
import Lambline.Command
import Lambline.Config
import Lambline.Helper
import Lambline.History (History, SearchDirection (..), SearchResult (..))
import qualified Lambline.History as History
import Lambline.Keys
import Lambline.KillRing (KillRing)
import qualified Lambline.KillRing as KillRing
import Lambline.Layout hiding (display)
import Lambline.LineBuffer (Change (..), LineBuffer (..), WordAction (..))
import qualified Lambline.LineBuffer as LB
import Lambline.Terminal
import Lambline.Undo (Changeset)
import qualified Lambline.Undo as Undo

-- | Why there is no line.
data ReadlineError
  = -- | Ctrl-D on empty input, or the input ended.
    Eof
  | -- | Ctrl-C.
    Interrupted
  deriving (Eq, Show)

-- | What an edit works with.
data Env = Env
  { envTerm :: Term,
    envConfig :: Config,
    envHelper :: Maybe Helper,
    envHistory :: History,
    envBindings :: Bindings,
    envPrompt :: String
  }

data St = St
  { stLine :: LineBuffer,
    stChanges :: Changeset,
    stRing :: KillRing,
    -- | The input as it was before moving into the history.
    stSaved :: LineBuffer,
    stHistoryIndex :: Int,
    stHint :: Maybe Hint,
    -- | The row the cursor was left on by the last draw.
    stRow :: Int,
    -- | A message from the validator to leave after the accepted input.
    stFinalMsg :: Maybe String,
    -- Keymap state.
    stNumArgs :: Int,
    stInputMode :: InputMode,
    stLastCmd :: Cmd,
    stLastCharSearch :: Maybe CharSearch
  }

type Ed = ReaderT Env (ExceptT ReadlineError (StateT St IO))

-- | Read a line on the terminal, starting from text before and after the
-- cursor. The kill ring is passed in and out so that it outlives the line.
edit :: Env -> (String, String) -> KillRing -> IO (Either ReadlineError String, KillRing)
edit env (left, right) ring = do
  (result, st) <- runStateT (runExceptT (runReaderT (begin >> loop) env)) st0
  (_, st') <- runStateT (runExceptT (runReaderT finish env)) st
  pure (text (stLine st') <$ result, stRing st')
  where
    st0 =
      St
        { stLine = LB.fromParts left right,
          stChanges = Undo.new,
          stRing = KillRing.reset ring,
          stSaved = LB.empty,
          stHistoryIndex = History.size (envHistory env),
          stHint = Nothing,
          stRow = 0,
          stFinalMsg = Nothing,
          stNumArgs = 0,
          stInputMode = InsertMode,
          stLastCmd = Noop,
          stLastCharSearch = Nothing
        }
    begin = do
      check <- asks (checkCursorPosition . envConfig)
      when check $ do
        column <- term termCursorColumn
        when (maybe False (> 1) column) (write "\n")
      refreshLine

-- Plumbing ----------------------------------------------------------------

term :: (Term -> IO a) -> Ed a
term f = asks envTerm >>= liftIO . f

write :: String -> Ed ()
write s = term (`termWrite` s)

config :: (Config -> a) -> Ed a
config f = asks (f . envConfig)

beep :: Ed ()
beep =
  config bellStyle >>= \case
    Audible -> write "\a"
    Visible -> write "\ESC[?5h\ESC[?5l"
    NoBell -> pure ()

-- | The highlighter, if colours are wanted.
highlighter :: Ed (Maybe Helper)
highlighter = do
  colors <- asks (termColors . envTerm)
  if colors then asks envHelper else pure Nothing

context :: Ed Context
context = Context <$> asks envHistory <*> gets stHistoryIndex

isEmacs :: Ed Bool
isEmacs = (== Emacs) <$> config editMode

isViCommandMode :: Ed Bool
isViCommandMode = (&&) <$> ((== Vi) <$> config editMode) <*> gets ((== CommandMode) . stInputMode)

-- Changing the input ------------------------------------------------------

recordChanges :: [Change] -> Ed ()
recordChanges cs = modify (\st -> st {stChanges = foldl (flip Undo.record) (stChanges st) cs})

-- | Apply an edit if it applies, saying whether it did.
apply :: LB.Edit -> Ed Bool
apply f =
  gets (f . stLine) >>= \case
    Nothing -> pure False
    Just (lb, cs) -> True <$ (modify (\st -> st {stLine = lb}) >> recordChanges cs)

applyAlways :: (LineBuffer -> (LineBuffer, [Change])) -> Ed ()
applyAlways f = void (apply (Just . f))

move :: (LineBuffer -> Maybe LineBuffer) -> Ed Bool
move f =
  gets (f . stLine) >>= \case
    Nothing -> pure False
    Just lb -> True <$ modify (\st -> st {stLine = lb})

changesBegin :: Ed Int
changesBegin = state (\st -> let (mark, cs) = Undo.begin (stChanges st) in (mark, st {stChanges = cs}))

changesEnd :: Ed Bool
changesEnd = state (\st -> let (touched, cs) = Undo.end (stChanges st) in (touched, st {stChanges = cs}))

-- | Do several changes that undo as one.
grouped :: Ed a -> Ed a
grouped action = changesBegin *> action <* changesEnd

lastInsert :: Ed (Maybe String)
lastInsert = gets (Undo.lastInsert . stChanges)

-- Drawing -----------------------------------------------------------------

data Info = NoHint | ShowHint | Msg (Maybe String)

-- | Redraw with a given prompt; the flag says whether it is the usual one.
refresh :: String -> Bool -> CmdKind -> Info -> Ed ()
refresh prompt isDefault kind info = do
  (output, cursor, _) <- frame prompt isDefault kind info id
  synchronized (write output)
  modify (\st -> st {stRow = row cursor})

frame :: String -> Bool -> CmdKind -> Info -> (LineBuffer -> LineBuffer) -> Ed (String, Pos, Pos)
frame prompt isDefault kind info adjust = do
  st <- get
  cfg <- asks envConfig
  hl <- highlighter
  width <- term termColumns
  let lb = adjust (stLine st)
      shown = case info of
        NoHint -> Nothing
        ShowHint -> hintDisplay <$> stHint st
        Msg msg -> msg
  pure . render $
    Frame
      { frameWidth = width,
        frameTab = tabStop cfg,
        frameOldRow = stRow st,
        framePrompt = prompt,
        framePromptStyled = maybe prompt (\h -> highlightPrompt h prompt isDefault) hl,
        frameContinuation = continuationPrompt cfg,
        frameBefore = take (pos lb) (text lb),
        frameLine = text lb,
        frameLineStyled = maybe (text lb) (\h -> highlight h (text lb) (pos lb) kind) hl,
        frameInfo = shown,
        frameInfoStyled = (\i -> maybe i (`highlightHint` i) hl) <$> shown
      }

synchronized :: Ed a -> Ed a
synchronized action = do
  sync <- config enableSynchronizedOutput
  if sync then write "\ESC[?2026h" *> action <* write "\ESC[?2026l" else action

updateHint :: Ed ()
updateHint = do
  helper <- asks envHelper
  lb <- gets stLine
  ctx <- context
  let found = helper >>= \h -> hint h (text lb) (pos lb) ctx
  modify (\st -> st {stHint = found >>= \h -> if null (hintDisplay h) then Nothing else Just h})

refreshLine :: Ed ()
refreshLine = do
  updateHint
  prompt <- asks envPrompt
  refresh prompt True Other ShowHint

refreshLineWithMsg :: Maybe String -> CmdKind -> Ed ()
refreshLineWithMsg msg kind = do
  modify (\st -> st {stHint = Nothing})
  prompt <- asks envPrompt
  refresh prompt True kind (Msg msg)

refreshPromptAndLine :: String -> Ed ()
refreshPromptAndLine prompt = updateHint >> refresh prompt False Other ShowHint

-- | Put the terminal's cursor after everything shown, without moving the
-- cursor in the input, so that what is written next goes below it.
cursorBelowInput :: Ed ()
cursorBelowInput = do
  prompt <- asks envPrompt
  (output, cursor, end) <- frame prompt True Other ShowHint (\lb -> lb {pos = length (text lb)})
  write (output ++ down (row end - row cursor))
  modify (\st -> st {stRow = row end})
  where
    down n = if n > 0 then "\ESC[" ++ show n ++ "B" else ""

-- | Whatever is written next starts a fresh drawing of the input.
forgetLayout :: Ed ()
forgetLayout = modify (\st -> st {stRow = 0})

externalPrint :: String -> Ed ()
externalPrint msg = synchronized $ do
  oldRow <- gets stRow
  write ((if oldRow > 0 then "\ESC[" ++ show oldRow ++ "A" else "") ++ "\r\ESC[J" ++ msg ++ (if "\n" `isSuffixOf'` msg then "" else "\n"))
  forgetLayout
  refreshLine
  where
    isSuffixOf' s t = reverse s `isPrefixOf` reverse t

-- | Leave the input as it should stay, with the cursor on the line below.
finish :: Ed ()
finish = do
  msg <- gets stFinalMsg
  modify (\st -> st {stHint = Nothing})
  prompt <- asks envPrompt
  (output, cursor, end) <- frame prompt True ForcedRefresh (Msg msg) (\lb -> lb {pos = length (text lb)})
  let below = row end - row cursor
  write (output ++ (if below > 0 then "\ESC[" ++ show below ++ "B" else "") ++ (if col end == 0 && row end > 0 then "\r" else "\n"))

-- Keys to commands --------------------------------------------------------

nextKey :: Bool -> Ed KeyEvent
nextKey singleEsc =
  term (`termEvent` singleEsc) >>= \case
    Key k -> pure k
    EndOfInput -> throwError Eof
    ExternalPrint msg -> externalPrint msg >> nextKey singleEsc
    Resized -> refreshLine >> nextKey singleEsc

-- | Read keys until they make a command. The flag says a lone Escape
-- should abort whatever is under way rather than start a key sequence.
nextCmd :: Bool -> Ed Cmd
nextCmd singleEscAbort = do
  mode <- config editMode
  inputMode <- gets stInputMode
  k <- nextKey (mode == Emacs && singleEscAbort)
  cmd <- case mode of
    Emacs -> emacs k
    Vi
      | inputMode /= CommandMode -> viInsert k
      | otherwise -> viCommand k
  case cmd of
    Replace _ _ -> cmd <$ changesBegin
    _ -> pure cmd

-- | The count typed before a command, and whether it is positive.
takeNumArgs :: Ed (RepeatCount, Bool)
takeNumArgs = state $ \st ->
  let n = if stNumArgs st == 0 then 1 else stNumArgs st
   in ((abs n, n >= 0), st {stNumArgs = 0})

hasHint, cursorAtEnd :: Ed Bool
hasHint = gets (isJust . stHint)
cursorAtEnd = gets (LB.isCursorAtEnd . stLine)

-- | An application's binding for exactly these keys, or for any key.
customBinding :: [KeyEvent] -> RepeatCount -> Bool -> Ed (Maybe Cmd)
customBinding keys n positive = do
  bindings <- asks envBindings
  case Map.lookup (KeySeq keys) bindings of
    Just handler -> handle handler
    Nothing -> maybe (pure Nothing) handle (Map.lookup AnyKey bindings)
  where
    handle = runHandler (KeySeq keys) n positive

runHandler :: Event -> RepeatCount -> Bool -> EventHandler -> Ed (Maybe Cmd)
runHandler evt n positive = \case
  Simple cmd -> pure (Just cmd)
  Conditional f -> do
    st <- get
    mode <- config editMode
    pure . f evt n positive $
      EventContext
        { contextMode = mode,
          contextInputMode = stInputMode st,
          contextHasHint = isJust (stHint st),
          contextHintText = hintDisplay <$> stHint st,
          contextLine = text (stLine st),
          contextPos = pos (stLine st)
        }

-- | Follow a binding of several keys: while some binding starts with the
-- keys so far, read another. Gives the command, if any, and the keys read.
customSeqBinding :: [KeyEvent] -> RepeatCount -> Bool -> Ed (Maybe Cmd, [KeyEvent])
customSeqBinding keys n positive = do
  bindings <- asks envBindings
  let continues = or [keys `isPrefixOf` ks && ks /= keys | KeySeq ks <- Map.keys bindings]
  if not continues
    then pure (Nothing, keys)
    else do
      k <- nextKey True
      let keys' = keys ++ [k]
      found <- maybe (pure Nothing) (runHandler (KeySeq keys') n positive) (Map.lookup (KeySeq keys') bindings)
      if isJust found then pure (found, keys') else customSeqBinding keys' n positive

-- | The terminal's own keys: end of file (only on empty input), interrupt
-- and suspend.
termBinding :: KeyEvent -> Ed (Maybe Cmd)
termBinding k = do
  bindings <- asks (termBindings . envTerm)
  lineEmpty <- gets (null . text . stLine)
  pure $ case lookup k bindings of
    Just EndOfFile | not lineEmpty -> Nothing
    found -> found

-- | A command again, with a new count if one is given.
redo :: Maybe RepeatCount -> Cmd -> Ed Cmd
redo new = \case
  Dedent mvt -> pure (Dedent (redoMovement new mvt))
  Indent mvt -> pure (Indent (redoMovement new mvt))
  Insert n s -> pure (Insert (count n) s)
  Kill mvt -> pure (Kill (redoMovement new mvt))
  Move mvt -> pure (Move (redoMovement new mvt))
  ReplaceChar n c -> pure (ReplaceChar (count n) c)
  Replace mvt Nothing -> do
    inserted <- lastInsert
    pure $ case mvt of
      ForwardChar 0 -> Replace (ForwardChar (maybe 0 length inserted)) inserted
      _ -> Replace (redoMovement new mvt) inserted
  Replace mvt s -> pure (Replace (redoMovement new mvt) s)
  -- Typing is repeated as a whole, not as its last character.
  SelfInsert n c -> maybe (SelfInsert (count n) c) (Insert (count n)) <$> lastInsert
  ViYankTo mvt -> pure (ViYankTo (redoMovement new mvt))
  Yank n anchor -> pure (Yank (count n) anchor)
  cmd -> pure cmd
  where
    count n = fromMaybe n new

-- | Alt with a digit or minus starts a count; more digits extend it.
digitArgument :: Bool -> Char -> Ed KeyEvent
digitArgument emacsMode digit = do
  modify (\st -> st {stNumArgs = if digit == '-' then -1 else ord digit - ord '0'})
  go
  where
    go = do
      n <- gets stNumArgs
      refreshPromptAndLine ("(arg: " ++ show n ++ ") ")
      k <- nextKey emacsMode
      case k of
        KeyEvent (KChar d) mods | isDigit d && (mods == noMods || emacsMode && mods == noMods {modAlt = True}) -> do
          let v = ord d - ord '0'
          modify $ \st ->
            st
              { stNumArgs = case stNumArgs st of
                  -1 -> negate v
                  m | abs m < 1000 -> m * 10 + signum m * v
                  m -> m
              }
          go
        KeyEvent (KChar '-') mods | emacsMode && (mods == noMods || mods == noMods {modAlt = True}) -> go
        _ -> k <$ refreshLine

emacs :: KeyEvent -> Ed Cmd
emacs key0 = do
  k <- case key0 of
    Alt c | c == '-' || isDigit c -> digitArgument True c
    _ -> pure key0
  (n, positive) <- takeNumArgs
  custom <- customBinding [k] n positive
  fromTerm <- termBinding k
  hinted <- (&&) <$> hasHint <*> cursorAtEnd
  let pick a b = if positive then a else b
      backWord = BackwardWord n EmacsWord
      forwardWord = ForwardWord n AfterEnd EmacsWord
      is c letter = toLower c == letter
  case (custom, fromTerm) of
    (Just cmd, _) -> if isRepeatable cmd then redo (Just n) cmd else pure cmd
    (_, Just cmd) -> pure cmd
    _ -> case k of
      Plain (KChar c) -> pure (if positive then SelfInsert n c else Unknown)
      Ctrl 'A' -> pure (Move BeginningOfLine)
      Ctrl 'B' -> pure (Move (pick (BackwardChar n) (ForwardChar n)))
      Ctrl 'E' -> pure (Move EndOfLine)
      Ctrl 'F' -> pure (Move (pick (ForwardChar n) (BackwardChar n)))
      Ctrl 'G' -> pure Abort
      CtrlAlt 'G' -> pure Abort
      Plain KEsc -> pure Abort
      Ctrl 'H' -> pure (Kill (pick (BackwardChar n) (ForwardChar n)))
      Plain KBackspace -> pure (Kill (pick (BackwardChar n) (ForwardChar n)))
      Plain KBackTab -> pure CompleteBackward
      Ctrl 'I' -> pure (pick Complete CompleteBackward)
      Plain KTab -> pure (pick Complete CompleteBackward)
      Plain KRight | hinted -> pure CompleteHint
      Ctrl 'K' -> pure (Kill (pick EndOfLine BeginningOfLine))
      Ctrl 'L' -> pure ClearScreen
      Ctrl 'N' -> pure NextHistory
      Ctrl 'P' -> pure PreviousHistory
      Ctrl 'X' -> do
        (found, keys) <- customSeqBinding [k] n positive
        case found of
          Just cmd -> pure cmd
          Nothing -> do
            second <- case keys of
              _ : s : _ -> pure s
              _ -> nextKey True
            pure $ case second of
              Ctrl 'G' -> Abort
              Plain KEsc -> Abort
              Ctrl 'U' -> Undo n
              Plain KBackspace -> Kill (pick BeginningOfLine EndOfLine)
              _ -> Unknown
      -- Search for a character: forwards with Ctrl-], backwards with Alt.
      KeyEvent (KChar ']') mods | modCtrl mods -> do
        target <- nextKey False
        pure $ case target of
          Plain (KChar c) -> Move (ViCharSearch n (if positive == modAlt mods then SearchBackward c else SearchForwardBefore c))
          _ -> Unknown
      AltKey KBackspace -> pure (Kill (pick backWord forwardWord))
      Alt '<' -> pure BeginningOfHistory
      Alt '>' -> pure EndOfHistory
      Alt c | c `is` 'b' -> pure (Move (pick backWord forwardWord))
      AltKey KLeft -> pure (Move (pick backWord forwardWord))
      CtrlKey KLeft -> pure (Move (pick backWord forwardWord))
      Alt c | c `is` 'c' -> pure CapitalizeWord
      Alt c | c `is` 'd' -> pure (Kill (pick forwardWord backWord))
      Alt c | c `is` 'f' -> pure (Move (pick forwardWord backWord))
      AltKey KRight -> pure (Move (pick forwardWord backWord))
      CtrlKey KRight -> pure (Move (pick forwardWord backWord))
      Alt c | c `is` 'l' -> pure DowncaseWord
      Alt c | c `is` 't' -> pure (TransposeWords n)
      Alt c | c `is` 'u' -> pure UpcaseWord
      Alt c | c `is` 'y' -> pure YankPop
      _ -> common k n positive

-- | Bindings the modes share.
common :: KeyEvent -> RepeatCount -> Bool -> Ed Cmd
common k n positive = do
  emacsMode <- isEmacs
  lineEmpty <- gets (null . text . stLine)
  let pick a b = if positive then a else b
  case k of
    Plain KHome -> pure (Move BeginningOfLine)
    Plain KLeft -> pure (Move (pick (BackwardChar n) (ForwardChar n)))
    Ctrl 'D'
      | emacsMode && not lineEmpty -> pure (Kill (pick (ForwardChar n) (BackwardChar n)))
      | not lineEmpty -> pure EndOfFile
      | otherwise -> pure Unknown
    Plain KDelete -> pure (Kill (pick (ForwardChar n) (BackwardChar n)))
    Plain KEnd -> pure (Move EndOfLine)
    Plain KRight -> pure (Move (pick (ForwardChar n) (BackwardChar n)))
    Ctrl 'J' -> pure (AcceptOrInsertLine True)
    Ctrl 'M' -> pure (AcceptOrInsertLine True)
    Plain KEnter -> pure (AcceptOrInsertLine True)
    Plain KDown -> pure (LineDownOrNextHistory 1)
    Plain KUp -> pure (LineUpOrPreviousHistory 1)
    Ctrl 'R' -> pure ReverseSearchHistory
    Ctrl 'S' -> pure ForwardSearchHistory
    Ctrl 'T' -> pure TransposeChars
    Ctrl 'U' -> pure (Kill (pick BeginningOfLine EndOfLine))
    Ctrl 'Q' -> pure QuotedInsert
    Ctrl 'V' -> pure QuotedInsert
    Ctrl 'W' -> pure (Kill (pick (BackwardWord n BigWord) (ForwardWord n AfterEnd BigWord)))
    Ctrl 'Y' -> pure (if positive then Yank n Before else Unknown)
    Ctrl '_' -> pure (Undo n)
    Plain KUnknown -> pure Noop
    Plain KPasteStart -> Insert 1 <$> term termPaste
    _ -> fromMaybe Unknown . fst <$> customSeqBinding [k] n positive

-- Vi ----------------------------------------------------------------------

setInputMode :: InputMode -> Ed ()
setInputMode m = modify (\st -> st {stInputMode = m})

-- | Enter insert mode. What is typed until leaving it undoes as one.
startInserting :: Ed ()
startInserting = setInputMode InsertMode >> void changesBegin

viCommand :: KeyEvent -> Ed Cmd
viCommand key0 = do
  k <- case key0 of
    Plain (KChar d) | d >= '1' && d <= '9' -> digitArgument False d
    _ -> pure key0
  noNumArgs <- gets ((== 0) . stNumArgs)
  (n, _) <- takeNumArgs
  let again = redo (if noNumArgs then Nothing else Just n)
  custom <- customBinding [k] n True
  fromTerm <- termBinding k
  cmd <- case (custom, fromTerm) of
    (Just cmd, _) -> if isRepeatable cmd then again cmd else pure cmd
    (_, Just cmd) -> pure cmd
    _ -> viCommandKey k n again
  when (isRepeatableChange cmd && custom == Nothing && fromTerm == Nothing) $
    modify (\st -> st {stLastCmd = cmd})
  pure cmd

viCommandKey :: KeyEvent -> RepeatCount -> (Cmd -> Ed Cmd) -> Ed Cmd
viCommandKey k n again = case k of
  Plain (KChar '$') -> pure (Move EndOfLine)
  Plain KEnd -> pure (Move EndOfLine)
  -- Repeat the last change.
  Plain (KChar '.') -> do
    lastCmd <- gets stLastCmd
    if isRepeatable lastCmd then again lastCmd else pure Noop
  Plain (KChar '0') -> pure (Move BeginningOfLine)
  Plain (KChar '^') -> pure (Move ViFirstPrint)
  Plain (KChar 'a') -> Move (ForwardChar n) <$ startInserting
  Plain (KChar 'A') -> Move EndOfLine <$ startInserting
  Plain (KChar 'b') -> pure (Move (BackwardWord n ViWord))
  Plain (KChar 'B') -> pure (Move (BackwardWord n BigWord))
  Plain (KChar 'c') -> do
    setInputMode InsertMode
    maybe Unknown (`Replace` Nothing) <$> viMotion k n
  Plain (KChar 'C') -> Replace EndOfLine Nothing <$ setInputMode InsertMode
  Plain (KChar 'd') -> maybe Unknown Kill <$> viMotion k n
  Plain (KChar 'D') -> pure (Kill EndOfLine)
  Ctrl 'K' -> pure (Kill EndOfLine)
  Plain (KChar 'e') -> pure (Move (ForwardWord n BeforeEnd ViWord))
  Plain (KChar 'E') -> pure (Move (ForwardWord n BeforeEnd BigWord))
  Plain (KChar 'i') -> Noop <$ startInserting
  Plain (KChar 'I') -> Move BeginningOfLine <$ startInserting
  Plain (KChar c) | c `elem` "fFtT" -> maybe Unknown (Move . ViCharSearch n) <$> viCharSearch c
  Plain (KChar ';') -> maybe Noop (Move . ViCharSearch n) <$> gets stLastCharSearch
  Plain (KChar ',') -> maybe Noop (Move . ViCharSearch n . opposite) <$> gets stLastCharSearch
  Plain (KChar 'p') -> pure (Yank n After)
  Plain (KChar 'P') -> pure (Yank n Before)
  Plain (KChar 'r') ->
    nextKey False >>= \case
      Plain (KChar c) -> pure (ReplaceChar n c)
      Plain KEsc -> pure Noop
      _ -> pure Unknown
  Plain (KChar 'R') -> Replace (ForwardChar 0) Nothing <$ setInputMode ReplaceMode
  Plain (KChar 's') -> Replace (ForwardChar n) Nothing <$ setInputMode InsertMode
  Plain (KChar 'S') -> Replace WholeLine Nothing <$ setInputMode InsertMode
  Plain (KChar 'u') -> pure (Undo n)
  Plain (KChar 'w') -> pure (Move (ForwardWord n AtStart ViWord))
  Plain (KChar 'W') -> pure (Move (ForwardWord n AtStart BigWord))
  Plain (KChar 'x') -> pure (Kill (ForwardChar n))
  Plain (KChar 'X') -> pure (Kill (BackwardChar n))
  Plain (KChar 'y') -> maybe Unknown ViYankTo <$> viMotion k n
  Plain (KChar 'h') -> pure (Move (BackwardChar n))
  Ctrl 'H' -> pure (Move (BackwardChar n))
  Plain KBackspace -> pure (Move (BackwardChar n))
  Ctrl 'G' -> pure Abort
  Plain (KChar c) | c `elem` "l " -> pure (Move (ForwardChar n))
  Ctrl 'L' -> pure ClearScreen
  Plain (KChar c) | c `elem` "+j" -> pure (LineDownOrNextHistory n)
  Ctrl 'N' -> pure NextHistory
  Plain (KChar c) | c `elem` "-k" -> pure (LineUpOrPreviousHistory n)
  Ctrl 'P' -> pure PreviousHistory
  Ctrl 'R' -> ReverseSearchHistory <$ setInputMode InsertMode
  Ctrl 'S' -> ForwardSearchHistory <$ setInputMode InsertMode
  Plain (KChar '<') -> maybe Unknown Dedent <$> viMotion k n
  Plain (KChar '>') -> maybe Unknown Indent <$> viMotion k n
  Plain KEsc -> pure Noop
  _ -> common k n True

viInsert :: KeyEvent -> Ed Cmd
viInsert k = do
  custom <- customBinding [k] 0 True
  fromTerm <- termBinding k
  hinted <- (&&) <$> hasHint <*> cursorAtEnd
  inputMode <- gets stInputMode
  cmd <- case (custom, fromTerm) of
    (Just cmd, _) -> if isRepeatable cmd then redo Nothing cmd else pure cmd
    (_, Just cmd) -> pure cmd
    _ -> case k of
      Plain (KChar c) -> pure (if inputMode == ReplaceMode then Overwrite c else SelfInsert 1 c)
      Ctrl 'H' -> pure (Kill (BackwardChar 1))
      Plain KBackspace -> pure (Kill (BackwardChar 1))
      Plain KBackTab -> pure CompleteBackward
      Ctrl 'I' -> pure Complete
      Plain KTab -> pure Complete
      Plain KRight | hinted -> pure CompleteHint
      -- Alt with a key is that key as a command.
      Alt c -> stopInserting >> viCommand (Plain (KChar c))
      Plain KEsc -> Move (BackwardChar 1) <$ stopInserting
      _ -> common k 1 True
  -- A run of typing is one change to repeat, as is typing after a replace.
  lastCmd <- gets stLastCmd
  let continuing = case (lastCmd, cmd) of
        (Replace _ _, SelfInsert _ _) -> True
        (SelfInsert _ _, SelfInsert _ _) -> True
        _ -> False
  when (isRepeatableChange cmd && not continuing && custom == Nothing && fromTerm == Nothing) $
    modify (\st -> st {stLastCmd = cmd})
  pure cmd
  where
    stopInserting = setInputMode CommandMode >> void changesEnd

-- | The movement after an operator such as @d@. The operator's own key
-- again means the whole line.
viMotion :: KeyEvent -> RepeatCount -> Ed (Maybe Movement)
viMotion operator n0 = do
  first <- nextKey False
  if first == operator
    then pure (Just WholeLine)
    else do
      (k, n) <- case first of
        Plain (KChar d) | d >= '1' && d <= '9' -> do
          k <- digitArgument False d
          (m, _) <- takeNumArgs
          pure (k, m * n0)
        _ -> pure (first, n0)
      -- Changing to the next word stops at the end of this one.
      let wordEnd = if operator == Plain (KChar 'c') then AfterEnd else AtStart
      case k of
        Plain (KChar '$') -> pure (Just EndOfLine)
        Plain (KChar '0') -> pure (Just BeginningOfLine)
        Plain (KChar '^') -> pure (Just ViFirstPrint)
        Plain (KChar 'b') -> pure (Just (BackwardWord n ViWord))
        Plain (KChar 'B') -> pure (Just (BackwardWord n BigWord))
        Plain (KChar 'e') -> pure (Just (ForwardWord n AfterEnd ViWord))
        Plain (KChar 'E') -> pure (Just (ForwardWord n AfterEnd BigWord))
        Plain (KChar c) | c `elem` "fFtT" -> fmap (ViCharSearch n) <$> viCharSearch c
        Plain (KChar ';') -> fmap (ViCharSearch n) <$> gets stLastCharSearch
        Plain (KChar ',') -> fmap (ViCharSearch n . opposite) <$> gets stLastCharSearch
        Plain (KChar 'h') -> pure (Just (BackwardChar n))
        Ctrl 'H' -> pure (Just (BackwardChar n))
        Plain KBackspace -> pure (Just (BackwardChar n))
        Plain (KChar c) | c `elem` "l " -> pure (Just (ForwardChar n))
        Plain (KChar c) | c `elem` "j+" -> pure (Just (LineDown n))
        Plain (KChar c) | c `elem` "k-" -> pure (Just (LineUp n))
        Plain (KChar 'w') -> pure (Just (ForwardWord n wordEnd ViWord))
        Plain (KChar 'W') -> pure (Just (ForwardWord n wordEnd BigWord))
        _ -> pure Nothing

viCharSearch :: Char -> Ed (Maybe CharSearch)
viCharSearch which =
  nextKey False >>= \case
    Plain (KChar c) -> do
      let cs = case which of
            'f' -> SearchForward c
            't' -> SearchForwardBefore c
            'F' -> SearchBackward c
            _ -> SearchBackwardAfter c
      Just cs <$ modify (\st -> st {stLastCharSearch = Just cs})
    _ -> pure Nothing

-- The main loop -----------------------------------------------------------

loop :: Ed ()
loop = do
  cmd0 <- nextCmd False
  when (shouldResetKillRing cmd0) (modify (\st -> st {stRing = KillRing.reset (stRing st)}))
  -- Completion and searching read keys of their own, and hand back the
  -- first that was not for them.
  helper <- asks envHelper
  cmd1 <- case (cmd0, helper) of
    (Complete, Just h) -> completeLine h
    _ -> pure (Just cmd0)
  cmd2 <- case cmd1 of
    Just ReverseSearchHistory -> incrementalSearch
    other -> pure other
  case cmd2 of
    Nothing -> loop
    Just Suspend -> do
      cursorBelowInput
      write "\r\n"
      term termSuspend
      forgetLayout
      refreshLine
      loop
    Just QuotedInsert -> do
      c <- term termChar >>= maybe (throwError Eof) pure
      void (apply (LB.insert c 1))
      refreshLine
      loop
    Just cmd -> do
      submit <- execute cmd
      unless submit loop

-- | Carry out a command, saying whether the input is to be submitted.
execute :: Cmd -> Ed Bool
execute cmd = case cmd of
  CompleteHint -> do
    completion <- gets (\st -> stHint st >>= hintCompletion)
    void (move LB.moveBufferEnd)
    applied <- maybe (pure False) (\s -> apply (LB.yank s 1)) completion
    unless applied beep
    proceed
  SelfInsert n c -> apply (LB.insert c n) >> proceed
  Insert n s -> editYank s Before n >> proceed
  Move mvt -> editMove mvt >> proceed
  ReplaceChar n c -> do
    grouped $ do
      lb <- gets stLine
      deleted <- apply (LB.delete n)
      when deleted $ do
        removed <- gets (\st -> length (text lb) - length (text (stLine st)))
        void (apply (LB.insert c removed))
        void (move (LB.moveBackward 1))
    proceed
  Replace mvt s -> do
    editKill mvt
    mapM_ (\t -> applyAlways (\lb -> let (lb', cs) = LB.insertStr (pos lb) t lb in (lb' {pos = pos lb + length t}, cs))) s
    proceed
  Overwrite c -> do
    void (apply (\lb -> if LB.isCursorAtEnd lb then Nothing else Just (LB.replaceRange (pos lb) (pos lb + 1) [c] lb)))
    proceed
  EndOfFile -> do
    lineEmpty <- gets (null . text . stLine)
    emacsMode <- isEmacs
    if lineEmpty then throwError Eof else if emacsMode then proceed else pure True
  ClearScreen -> write "\ESC[H\ESC[2J" >> forgetLayout >> proceed
  NextHistory -> historyNext False >> proceed
  PreviousHistory -> historyNext True >> proceed
  LineUpOrPreviousHistory n -> do
    moved <- lineUp n
    unless moved (historyNext True)
    proceed
  LineDownOrNextHistory n -> do
    moved <- lineDown n
    unless moved (historyNext False)
    proceed
  HistorySearchBackward -> historySearch Reverse >> proceed
  HistorySearchForward -> historySearch Forward >> proceed
  TransposeChars -> grouped (apply LB.transposeChars) >> proceed
  Yank n anchor -> do
    yanked <- gets (KillRing.yank . stRing)
    forM_ yanked $ \(s, ring) -> do
      modify (\st -> st {stRing = ring})
      editYank s anchor n
    proceed
  ViYankTo mvt -> do
    copied <- gets (LB.copy mvt . stLine)
    forM_ copied $ \s -> modify (\st -> st {stRing = KillRing.kill s KillRing.Append (stRing st)})
    proceed
  Newline -> apply (LB.insert '\n' 1) >> proceed
  Repaint -> proceed
  AcceptLine -> acceptLine cmd
  AcceptOrInsertLine _ -> acceptLine cmd
  BeginningOfHistory -> historyEnd True >> proceed
  EndOfHistory -> historyEnd False >> proceed
  CapitalizeWord -> grouped (apply (LB.editWord Capitalize)) >> proceed
  Kill mvt -> editKill mvt >> proceed
  DowncaseWord -> grouped (apply (LB.editWord Lowercase)) >> proceed
  TransposeWords n -> grouped (apply (LB.transposeWords n)) >> proceed
  UpcaseWord -> grouped (apply (LB.editWord Uppercase)) >> proceed
  YankPop -> do
    popped <- gets (KillRing.yankPop . stRing)
    forM_ popped $ \(size, s, ring) -> do
      modify (\st -> st {stRing = ring})
      grouped (apply (LB.yankPop size s))
    proceed
  Undo n -> do
    undone <- gets (\st -> Undo.undo n (stLine st) (stChanges st))
    forM_ undone (\(lb, cs) -> modify (\st -> st {stLine = lb, stChanges = cs}))
    proceed
  Dedent mvt -> config indentSize >>= \amount -> applyAlways (LB.indent mvt amount True) >> proceed
  Indent mvt -> config indentSize >>= \amount -> applyAlways (LB.indent mvt amount False) >> proceed
  Interrupt -> throwError Interrupted
  _ -> pure False
  where
    proceed = False <$ refreshLine

acceptLine :: Cmd -> Ed Bool
acceptLine cmd = do
  helper <- asks envHelper
  lb <- gets stLine
  let result = maybe (Valid Nothing) (\h -> validate h (text lb)) helper
      accept msg = True <$ modify (\st -> st {stFinalMsg = msg})
      newline = False <$ (apply (LB.insert '\n' 1) >> refreshLine)
  case (cmd, result) of
    (AcceptLine, Valid msg) -> accept msg
    (AcceptLine, _) -> accept Nothing
    (AcceptOrInsertLine inTheMiddle, Valid msg)
      | inTheMiddle || LB.isEndOfInput lb -> accept msg
      | otherwise -> newline
    (_, Incomplete) -> newline
    -- Wrong input stays, with the reason shown after it.
    (_, Invalid (Just msg)) -> False <$ refreshLineWithMsg (Just msg) Other
    _ -> newline

editMove :: Movement -> Ed ()
editMove mvt = void $ case mvt of
  BeginningOfLine -> move LB.moveHome
  ViFirstPrint -> do
    _ <- move LB.moveHome
    blank <- gets (\st -> take 1 (drop (pos (stLine st)) (text (stLine st))) `elem` [" ", "\t"])
    if blank then move (LB.moveToNextWord AtStart BigWord 1) else pure False
  BackwardChar n -> move (LB.moveBackward n)
  ForwardChar n -> move (LB.moveForward n)
  EndOfLine -> move LB.moveEnd
  BackwardWord n def -> move (LB.moveToPrevWord def n)
  ForwardWord n at def -> move (LB.moveToNextWord at def n)
  LineUp n -> lineUp n
  LineDown n -> lineDown n
  BeginningOfBuffer -> move LB.moveBufferStart
  EndOfBuffer -> move LB.moveBufferEnd
  ViCharSearch n cs -> move (LB.moveTo cs n)
  WholeLine -> pure False
  WholeBuffer -> pure False

-- | How much further right the first line of input starts than the rest.
firstLineOffset :: Ed Int
firstLineOffset = do
  prompt <- asks envPrompt
  cont <- config continuationPrompt
  pure (strWidth (lastLine prompt) - strWidth cont)
  where
    lastLine = reverse . takeWhile (/= '\n') . reverse

lineUp, lineDown :: RepeatCount -> Ed Bool
lineUp n = firstLineOffset >>= move . LB.moveToLineUp n
lineDown n = firstLineOffset >>= move . LB.moveToLineDown n

-- | Insert text at the cursor, or after the character under it.
editYank :: String -> Anchor -> RepeatCount -> Ed ()
editYank s anchor n = do
  when (anchor == After) (void (move (LB.moveForward 1)))
  yanked <- apply (LB.yank s n)
  commandMode <- isViCommandMode
  when (yanked && commandMode) (void (move (LB.moveBackward 1)))

-- | Remove what a movement covers. Anything more than single characters
-- goes to the kill ring.
editKill :: Movement -> Ed ()
editKill mvt =
  gets (LB.kill mvt . stLine) >>= \case
    Nothing -> pure ()
    Just (lb, cs) -> do
      modify (\st -> st {stLine = lb})
      recordChanges cs
      let single = case mvt of
            ForwardChar _ -> True
            BackwardChar _ -> True
            _ -> False
      unless single . forM_ cs $ \case
        Deleted _ s dir ->
          let mode = if dir == LB.Forward then KillRing.Append else KillRing.Prepend
           in modify (\st -> st {stRing = KillRing.kill s mode (stRing st)})
        _ -> pure ()

-- History -----------------------------------------------------------------

-- | Replace the whole input, as one change.
setLine :: String -> Int -> Ed ()
setLine s p = grouped (applyAlways (LB.update s p))

-- | Show the previous or next history entry in place of the input, which
-- is kept to come back to.
historyNext :: Bool -> Ed ()
historyNext prev = do
  history <- asks envHistory
  index <- gets stHistoryIndex
  let size = History.size history
      target = if prev then index - 1 else index + 1
  unless (History.isEmpty history || (index == size && not prev) || (index == 0 && prev)) $ do
    when (index == size) (modify (\st -> st {stSaved = stLine st}))
    modify (\st -> st {stHistoryIndex = target})
    case History.get target history of
      Just entry -> setLine entry (length entry)
      Nothing -> gets stSaved >>= \saved -> grouped (applyAlways (LB.update (text saved) (pos saved)))

-- | Find the nearest entry that starts with the text before the cursor.
historySearch :: SearchDirection -> Ed ()
historySearch dir = do
  history <- asks envHistory
  index <- gets stHistoryIndex
  lb <- gets stLine
  let size = History.size history
      start = if dir == Reverse then index - 1 else index + 1
  if History.isEmpty history || (index == size && dir == Forward) || (index == 0 && dir == Reverse)
    then beep
    else case History.startsWith (take (pos lb) (text lb)) start dir history of
      Just found -> do
        modify (\st -> st {stHistoryIndex = resultIndex found})
        setLine (resultEntry found) (resultPos found)
      Nothing -> beep

-- | Go to the oldest entry, or back to the input being typed.
historyEnd :: Bool -> Ed ()
historyEnd first = do
  history <- asks envHistory
  index <- gets stHistoryIndex
  let size = History.size history
  unless (History.isEmpty history || (index == size && not first) || (index == 0 && first)) $ do
    when (index == size) (modify (\st -> st {stSaved = stLine st}))
    if first
      then forM_ (History.get 0 history) $ \entry -> do
        modify (\st -> st {stHistoryIndex = 0})
        setLine entry (length entry)
      else do
        modify (\st -> st {stHistoryIndex = size})
        gets stSaved >>= \saved -> grouped (applyAlways (LB.update (text saved) (pos saved)))

-- | Search the history as the term is typed. Gives back the command that
-- ended the search, or nothing if it was abandoned.
incrementalSearch :: Ed (Maybe Cmd)
incrementalSearch = do
  history <- asks envHistory
  if History.isEmpty history
    then pure Nothing
    else do
      mark <- changesBegin
      backup <- gets stLine
      go history mark backup "" (History.size history - 1) Reverse True
  where
    go history mark backup term' index dir success = do
      refreshPromptAndLine ((if success then "(" else "(failed ") ++ "reverse-i-search)`" ++ term' ++ "': ")
      cmd <- nextCmd True
      let again t i d = do
            case History.search t i d history of
              Just found -> do
                applyAlways (LB.update (resultEntry found) (resultPos found))
                go history mark backup t (resultIndex found) d True
              Nothing -> go history mark backup t i d False
      case cmd of
        SelfInsert _ c -> again (term' ++ [c]) index dir
        Kill (BackwardChar _) -> go history mark backup (take (length term' - 1) term') index dir success
        ReverseSearchHistory
          | index > 0 -> again term' (index - 1) Reverse
          | otherwise -> go history mark backup term' index Reverse False
        ForwardSearchHistory
          | index < History.size history - 1 -> again term' (index + 1) Forward
          | otherwise -> go history mark backup term' index Forward False
        Abort -> do
          applyAlways (LB.update (text backup) (pos backup))
          refreshLine
          modify (\st -> st {stChanges = Undo.truncateTo mark (stChanges st)})
          pure Nothing
        _ -> do
          refreshLine
          Just cmd <$ changesEnd

-- Completion --------------------------------------------------------------

-- | Complete the text before the cursor. Gives back the command that ended
-- the completing, if there is one to carry on with.
completeLine :: Helper -> Ed (Maybe Cmd)
completeLine helper = do
  lb <- gets stLine
  ctx <- context
  (start, candidates) <- liftIO (complete helper (text lb) (pos lb) ctx)
  kind <- config completionType
  let substitute s = applyAlways (\b -> LB.replaceRange start (pos b) s b)
  case candidates of
    [] -> Nothing <$ beep
    _ | kind == Circular -> do
      -- Each Tab shows the next candidate, and after the last the input as
      -- it was.
      mark <- changesBegin
      let count = length candidates
          restore = applyAlways (LB.update (text lb) (pos lb))
          cycleFrom i = do
            if i < count then substitute (replacement (candidates !! i)) else restore
            refreshLine
            nextCmd True >>= \case
              Complete -> do
                let j = (i + 1) `mod` (count + 1)
                when (j == count) beep
                cycleFrom j
              CompleteBackward
                | i == 0 -> beep >> cycleFrom count
                | otherwise -> cycleFrom (i - 1)
              Abort -> do
                when (i < count) (restore >> refreshLine)
                modify (\st -> st {stChanges = Undo.truncateTo mark (stChanges st)})
                pure Nothing
              cmd -> Just cmd <$ changesEnd
      cycleFrom 0
    _ -> do
      -- Complete as far as the candidates agree; a second Tab lists them.
      forM_ (longestCommonPrefix candidates) $ \prefix ->
        when (length prefix > pos lb - start || length candidates == 1) (substitute prefix >> refreshLine)
      if length candidates == 1
        then pure Nothing
        else do
          beep
          showAll <- config completionShowAllIfAmbiguous
          next' <- if showAll then pure Complete else nextCmd True
          if next' /= Complete
            then pure (Just next')
            else do
              cursorBelowInput
              limit <- config completionPromptLimit
              let asked = length candidates > limit
              wanted <-
                if not asked
                  then pure True
                  else do
                    write ("\nDisplay all " ++ show (length candidates) ++ " possibilities? (y or n)")
                    answer <- waitFor (\c -> c `elem` map (SelfInsert 1) "yYnN" || c == Complete || c == Kill (BackwardChar 1))
                    pure (answer `elem` [SelfInsert 1 'y', SelfInsert 1 'Y', Complete])
              if wanted then pageCompletions helper candidates else write "\n"
              forgetLayout
              Nothing <$ refreshLine

-- | Read commands until one satisfies a test.
waitFor :: (Cmd -> Bool) -> Ed Cmd
waitFor ok = nextCmd False >>= \cmd -> if ok cmd then pure cmd else waitFor ok

-- | List candidates in columns, a screen at a time.
pageCompletions :: Helper -> [Candidate] -> Ed ()
pageCompletions helper candidates = do
  cols <- term termColumns
  rows <- term termRows
  hl <- highlighter
  let widest = min cols (maximum (map (strWidth . display) candidates) + 2)
      perRow = max 1 (cols `div` widest)
      count = length candidates
      numRows = (count + perRow - 1) `div` perRow
      styled s = maybe s (const (highlightCandidate helper s List)) hl
      cell r c =
        let i = c * numRows + r
            s = display (candidates !! i)
            padding = if (c + 1) * numRows + r < count then replicate (widest - strWidth s) ' ' else ""
         in if i < count then styled s ++ padding else ""
      line r = concatMap (cell r) [0 .. perRow - 1]
      isAccept = \case
        AcceptLine -> True
        Newline -> True
        AcceptOrInsertLine _ -> True
        _ -> False
      page r pauseRow
        | r >= numRows = pure ()
        | r == pauseRow = do
            write "\n--More--"
            answer <- waitFor (\c -> c `elem` map (SelfInsert 1) "yYnNqQ " || c == Kill (BackwardChar 1) || isAccept c)
            write "\r\ESC[K\ESC[A"
            if answer `elem` map (SelfInsert 1) "yY "
              then page r (pauseRow + rows - 1)
              else when (isAccept answer) (page r (pauseRow + 1))
        | otherwise = write ("\n" ++ line r) >> page (r + 1) pauseRow
  page 0 (rows - 1)
  write "\n"

