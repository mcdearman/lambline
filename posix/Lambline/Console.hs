-- | What differs between systems in driving a terminal. This is the one for
-- POSIX systems, over termios.
module Lambline.Console
  ( Saved,
    inputIsTerminal,
    outputIsTerminal,
    supported,
    columns,
    rows,
    raw,
    restore,
    controls,
    suspend,
  )
where

import Foreign.C.Types (CInt (..))
import qualified Lambline.Command as Cmd
import Lambline.Keys
import System.Environment (lookupEnv)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Signals (raiseSignal, sigTSTP)
import System.Posix.Terminal

foreign import ccall unsafe "lambline_columns" c_columns :: CInt -> IO CInt

foreign import ccall unsafe "lambline_rows" c_rows :: CInt -> IO CInt

-- | How the terminal was before it was made ready for editing.
type Saved = TerminalAttributes

inputIsTerminal, outputIsTerminal :: IO Bool
inputIsTerminal = queryTerminal stdInput
outputIsTerminal = queryTerminal stdOutput

-- | Whether the terminal understands control sequences.
supported :: IO Bool
supported = (`notElem` [Nothing, Just "", Just "dumb", Just "cons25", Just "emacs"]) <$> lookupEnv "TERM"

-- | The size of the terminal, or 80 by 24 if there is none.
columns, rows :: IO Int
columns = fromIntegral <$> c_columns 1
rows = fromIntegral <$> c_rows 1

-- | Have keys delivered as they are typed, unechoed and uninterpreted. The
-- flag leaves the terminal's signal keys working. Output is left alone, so
-- a line feed still returns to the first column.
raw :: Bool -> IO Saved
raw signals = do
  attrs <- getTerminalAttributes stdInput
  let off =
        [EnableEcho, ProcessInput, ExtendedFunctions, StartStopOutput, MapCRtoLF, InterruptOnBreak, CheckParity, StripHighBit]
          ++ [KeyboardInterrupts | not signals]
  setTerminalAttributes stdInput (withTime (withMinInput (withBits (foldl withoutMode attrs off) 8) 1) 0) WhenDrained
  pure attrs

restore :: Saved -> IO ()
restore attrs = setTerminalAttributes stdInput attrs WhenDrained

-- | Commands for the keys the terminal driver would otherwise handle: end
-- of file, interrupt and suspend. With signals left working the driver
-- handles the last two itself.
controls :: Saved -> Bool -> [(KeyEvent, Cmd.Cmd)]
controls attrs signals =
  [(key c, cmd) | (which, cmd) <- (EndOfFile, Cmd.EndOfFile) : special, Just c <- [controlChar attrs which]]
  where
    special = if signals then [] else [(Interrupt, Cmd.Interrupt), (Quit, Cmd.Interrupt), (Suspend, Cmd.Suspend)]

-- | Stop the process as Ctrl-Z would, returning when it is resumed.
suspend :: IO ()
suspend = raiseSignal sigTSTP
