-- | A line editor: a port of rustyline.
--
-- > import Lambline
-- >
-- > main :: IO ()
-- > main = do
-- >   editor <- newEditor defaultConfig
-- >   setHelper editor (Just defaultHelper {validate = matchingBracketValidator})
-- >   let loop =
-- >         readline editor "> " >>= \case
-- >           Right line -> addHistoryEntry editor line >> putStrLn line >> loop
-- >           Left Interrupted -> loop
-- >           Left Eof -> pure ()
-- >   loop
--
-- Editing is in Emacs or Vi mode, with history, a kill ring and undo. A
-- 'Helper' adds completion, hints, highlighting and validation; it is the
-- validator that makes input run over several lines, by calling input
-- 'Incomplete' when Enter is pressed.
module Lambline
  ( -- * The editor
    Editor,
    newEditor,
    readline,
    readlineWithInitial,
    ReadlineError (..),
    setHelper,
    getHelper,
    createExternalPrinter,

    -- * History
    addHistoryEntry,
    history,
    clearHistory,
    loadHistory,
    saveHistory,
    appendHistory,

    -- * Key bindings
    bindSequence,
    unbindSequence,
    module Lambline.Binding,
    module Lambline.Command,
    module Lambline.Keys,

    -- * Configuration and helpers
    module Lambline.Config,
    module Lambline.Helper,
  )
where

import Control.Concurrent.MVar
import Control.Exception (AsyncException (UserInterrupt), catchJust)
import Control.Monad (void, when)
import Data.IORef
import qualified Data.Map as Map
import Lambline.Binding
import Lambline.Command
import Lambline.Config
import qualified Lambline.Console as Console
import Lambline.Engine
import Lambline.Helper
import Lambline.History (History)
import qualified Lambline.History as History
import Lambline.Keys hiding (Source (..), readKey, readPaste)
import Lambline.KillRing (KillRing)
import qualified Lambline.KillRing as KillRing
import Lambline.Terminal
import System.IO

data Editor = Editor
  { edConfig :: Config,
    edHelper :: IORef (Maybe Helper),
    edHistory :: IORef History,
    edBindings :: IORef Bindings,
    edKillRing :: IORef KillRing,
    -- | Messages waiting to be shown above the input.
    edMessages :: MVar [String]
  }

newEditor :: Config -> IO Editor
newEditor config =
  Editor config
    <$> newIORef Nothing
    <*> newIORef (History.new config)
    <*> newIORef Map.empty
    <*> newIORef (KillRing.new 60)
    <*> newMVar []

-- | Supply, or take away, completion, hints, highlighting and validation.
setHelper :: Editor -> Maybe Helper -> IO ()
setHelper = writeIORef . edHelper

getHelper :: Editor -> IO (Maybe Helper)
getHelper = readIORef . edHelper

-- | Show a prompt and read a line, which may in fact be several.
--
-- On a terminal the line is edited. Otherwise, when stdin is a file or a
-- pipe or the terminal is one that cannot be driven, lines are read
-- plainly, and more are read for as long as the validator finds the input
-- incomplete.
readline :: Editor -> String -> IO (Either ReadlineError String)
readline editor prompt = readlineWithInitial editor prompt ("", "")

-- | 'readline' with input already there: text to go before the cursor,
-- and text to go after it.
readlineWithInitial :: Editor -> String -> (String, String) -> IO (Either ReadlineError String)
readlineWithInitial editor prompt initial = do
  tty <- interactive
  helper <- getHelper editor
  result <-
    if not tty
      then direct helper prompt
      else do
        env <- Env undefined (edConfig editor) helper <$> readIORef (edHistory editor) <*> readIORef (edBindings editor) <*> pure prompt
        ring <- readIORef (edKillRing editor)
        -- With signals enabled Ctrl-C arrives as an exception.
        catchJust
          (\e -> if e == UserInterrupt then Just () else Nothing)
          ( withTerminal (edConfig editor) (edMessages editor) $ \term -> do
              (result, ring') <- edit env {envTerm = term} initial ring
              result <$ writeIORef (edKillRing editor) ring'
          )
          (\() -> Left Interrupted <$ (hPutStr stdout "\n" >> hFlush stdout))
  case result of
    Right line | tty && autoAddHistory (edConfig editor) -> void (addHistoryEntry editor line)
    _ -> pure ()
  pure result

-- | Read input without a terminal to edit it on.
direct :: Maybe Helper -> String -> IO (Either ReadlineError String)
direct helper prompt = do
  tty <- Console.inputIsTerminal
  when tty (hPutStr stdout prompt >> hFlush stdout)
  go ""
  where
    go acc = do
      eof <- isEOF
      if eof
        then pure (if null acc then Left Eof else Right (init acc))
        else do
          input <- (acc ++) . backspaces . dropCarriageReturn <$> getLine
          case maybe (Valid Nothing) (`validate` input) helper of
            Valid msg -> Right input <$ mapM_ (hPutStrLn stderr) msg
            Incomplete -> go (input ++ "\n")
            Invalid msg -> mapM_ (hPutStrLn stderr) msg >> go ""
    dropCarriageReturn s = if take 1 (reverse s) == "\r" then init s else s
    -- A backspace in the input removes the character before it.
    backspaces = reverse . foldl (\out c -> if c == '\b' then drop 1 out else c : out) ""

-- | A way to print while 'readline' is waiting, from another thread: the
-- message appears above the input, which is redrawn below it.
createExternalPrinter :: Editor -> IO (String -> IO ())
createExternalPrinter editor = pure (\msg -> modifyMVar_ (edMessages editor) (pure . (++ [msg])))

-- History -----------------------------------------------------------------

-- | Remember an entry, saying whether it was added: an empty one is not,
-- and the configuration may rule out repeats and entries starting with a
-- space.
addHistoryEntry :: Editor -> String -> IO Bool
addHistoryEntry editor line = atomicModifyIORef' (edHistory editor) (\h -> let (added, h') = History.add line h in (h', added))

-- | The history, oldest first.
history :: Editor -> IO [String]
history editor = History.entries <$> readIORef (edHistory editor)

clearHistory :: Editor -> IO ()
clearHistory editor = modifyIORef' (edHistory editor) History.clear

-- | Add the entries in a file to the history. Does nothing if there is no
-- such file.
loadHistory :: Editor -> FilePath -> IO ()
loadHistory = withHistory History.load

-- | Write the history to a file, replacing it.
saveHistory :: Editor -> FilePath -> IO ()
saveHistory = withHistory History.save

-- | Add to a file the entries made since the history was last loaded or
-- saved.
appendHistory :: Editor -> FilePath -> IO ()
appendHistory = withHistory History.append

withHistory :: (FilePath -> History -> IO History) -> Editor -> FilePath -> IO ()
withHistory f editor path = readIORef (edHistory editor) >>= f path >>= writeIORef (edHistory editor)

-- Bindings ----------------------------------------------------------------

-- | Bind a key or a sequence of keys, in place of whatever it did before.
--
-- > bindSequence editor (KeySeq [alt '\r']) (Simple Newline)
bindSequence :: Editor -> Event -> EventHandler -> IO ()
bindSequence editor event handler = modifyIORef' (edBindings editor) (Map.insert (normalizeEvent event) handler)

unbindSequence :: Editor -> Event -> IO ()
unbindSequence editor event = modifyIORef' (edBindings editor) (Map.delete (normalizeEvent event))

normalizeEvent :: Event -> Event
normalizeEvent = \case
  KeySeq keys -> KeySeq (map normalize keys)
  AnyKey -> AnyKey
