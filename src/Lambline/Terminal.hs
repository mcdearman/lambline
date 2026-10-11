-- | The terminal: what the editor needs from one, and the real thing.
module Lambline.Terminal
  ( Term (..),
    TermEvent (..),
    interactive,
    withTerminal,
  )
where

import Control.Concurrent.MVar
import Control.Exception (bracket, bracket_, catch, throwIO)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.IORef
import Data.Maybe (isNothing)
import qualified Lambline.Command as Cmd
import Lambline.Config
import qualified Lambline.Console as Console
import Lambline.Keys
import System.Environment (lookupEnv)
import System.IO
import System.IO.Error (isEOFError)

-- | Something that happens while the editor waits.
data TermEvent
  = Key KeyEvent
  | EndOfInput
  | -- | The application wants a message shown above the input.
    ExternalPrint String
  | Resized

data Term = Term
  { -- | Wait for the next event. The flag says a lone Escape is wanted, so
    -- not to wait to see whether it starts a sequence.
    termEvent :: Bool -> IO TermEvent,
    -- | The next character as it comes, undecoded.
    termChar :: IO (Maybe Char),
    -- | The text of a bracketed paste, after its opening marker.
    termPaste :: IO String,
    termWrite :: String -> IO (),
    termColumns :: IO Int,
    termRows :: IO Int,
    -- | Whether to use the highlighter.
    termColors :: Bool,
    -- | Commands for the keys the terminal driver would otherwise handle:
    -- end of file, interrupt and suspend.
    termBindings :: [(KeyEvent, Cmd.Cmd)],
    -- | Stop the process as Ctrl-Z would, returning when it is resumed.
    termSuspend :: IO (),
    -- | Ask the terminal which column the cursor is in, from 1.
    termCursorColumn :: IO (Maybe Int)
  }

-- | Whether there is a terminal to edit on: stdin and stdout are both one,
-- and it understands control sequences.
interactive :: IO Bool
interactive = do
  input <- Console.inputIsTerminal
  output <- Console.outputIsTerminal
  supported <- Console.supported
  pure (input && output && supported)

-- | Run an action with the terminal ready for editing: keys delivered as
-- they are typed, unechoed and uninterpreted. Output is left alone, so a
-- line feed still returns to the first column.
--
-- Messages put in the queue arrive as 'ExternalPrint' events.
withTerminal :: Config -> MVar [String] -> (Term -> IO a) -> IO a
withTerminal config queue action = do
  colors <- case colorMode config of
    Forced -> pure True
    Disabled -> pure False
    Enabled -> do
      output <- Console.outputIsTerminal
      supported <- Console.supported
      noColor <- lookupEnv "NO_COLOR"
      pure (output && supported && isNothing noColor)
  lastColumns <- columns >>= newIORef
  let enter = do
        buffering <- hGetBuffering stdin
        saved <- Console.raw (enableSignals config)
        hSetBuffering stdin NoBuffering
        pure (buffering, saved)
      leave (buffering, saved) = do
        hSetBuffering stdin buffering
        Console.restore saved
      paste = when (enableBracketedPaste config) . write
      term entered@(_, saved) =
        Term
          { termEvent = event lastColumns,
            termChar = next source,
            termPaste = readPaste source,
            termWrite = write,
            termColumns = columns,
            termRows = Console.rows,
            termColors = colors,
            termBindings = Console.controls saved (enableSignals config),
            termSuspend = do
              leave entered
              Console.suspend
              _ <- enter
              pure (),
            termCursorColumn = cursorColumn
          }
  bracket enter leave $ \entered ->
    bracket_ (paste "\ESC[?2004h") (paste "\ESC[?2004l") (action (term entered))
  where
    write s = hPutStr stdout s >> hFlush stdout
    columns = Console.columns

    -- Wake every so often to look for messages and a change of width.
    event lastColumns singleEsc = do
      pendingMessage <- modifyMVar queue (\q -> pure (drop 1 q, take 1 q))
      width <- columns
      oldWidth <- readIORef lastColumns
      ready <- hWaitForInput stdin 50 `orAtEof` True
      case pendingMessage of
        [msg] -> pure (ExternalPrint msg)
        _
          | width /= oldWidth -> Resized <$ writeIORef lastColumns width
          | ready -> maybe EndOfInput Key <$> readKey source (keyseqTimeout config) singleEsc
          | otherwise -> event lastColumns singleEsc

    cursorColumn = do
      write "\ESC[6n"
      answered <- hWaitForInput stdin 100 `orAtEof` False
      if not answered then pure Nothing else parseReport <$> report ""
    report acc =
      next source >>= \case
        Just 'R' -> pure (reverse acc)
        Just c -> report (c : acc)
        Nothing -> pure (reverse acc)
    parseReport s = case break (== ';') s of
      (_, _ : column) | not (null column) && all isDigit column -> Just (read column)
      _ -> Nothing

-- | Characters from stdin.
source :: Source IO
source =
  Source
    { next = (Just <$> hGetChar stdin) `orAtEof` Nothing,
      poll = \case
        Just ms -> hWaitForInput stdin ms `orAtEof` False
        Nothing -> forever
    }
  where
    forever = do
      ready <- hWaitForInput stdin 100 `orAtEof` True
      if ready then pure True else forever

orAtEof :: IO a -> a -> IO a
orAtEof action a = action `catch` \e -> if isEOFError e then pure a else throwIO e
