-- | What differs between systems in driving a terminal. This is the one for
-- Windows, over the console.
--
-- The console is asked to speak as a terminal does: to send keys as the
-- control sequences a terminal sends, and to obey the ones it is sent. With
-- that the rest of the editor is the same as anywhere else. A console that
-- cannot do this, which is one older than Windows 10, is not edited on.
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

import Control.Monad (when)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (Ptr)
import qualified Lambline.Command as Cmd
import Lambline.Keys
import System.Environment (lookupEnv)
import System.IO

foreign import ccall unsafe "lambline_is_console" c_is_console :: CInt -> IO CInt

foreign import ccall unsafe "lambline_speaks" c_speaks :: IO CInt

foreign import ccall unsafe "lambline_columns" c_columns :: IO CInt

foreign import ccall unsafe "lambline_rows" c_rows :: IO CInt

foreign import ccall unsafe "lambline_raw" c_raw :: CInt -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "lambline_restore" c_restore :: Ptr CUInt -> IO ()

-- | How the console was before it was made ready for editing: its modes
-- and code pages, and how the program read and wrote text.
data Saved = Saved [CUInt] (Maybe TextEncoding) (Maybe TextEncoding)

inputIsTerminal, outputIsTerminal :: IO Bool
inputIsTerminal = (/= 0) <$> c_is_console 0
outputIsTerminal = (/= 0) <$> c_is_console 1

-- | Whether the console understands control sequences. Nothing sets @TERM@
-- on Windows as a rule, so it only counts when it says the terminal is one
-- that cannot be driven.
supported :: IO Bool
supported = do
  term <- lookupEnv "TERM"
  speaks <- c_speaks
  pure (term `notElem` [Just "dumb", Just "emacs"] && speaks /= 0)

-- | The size of the console's window, or 80 by 24 if there is none.
columns, rows :: IO Int
columns = fromIntegral <$> c_columns
rows = fromIntegral <$> c_rows

-- | Have keys delivered as they are typed, unechoed and uninterpreted, as
-- the sequences a terminal sends. The flag leaves Ctrl-C to the console,
-- which then interrupts the program.
--
-- Text is read and written as UTF-8 meanwhile, and a carriage return is
-- read as it comes: the Enter key sends one alone, and a reader that turns
-- a carriage return and a line feed into one character would wait to see
-- what follows it.
raw :: Bool -> IO Saved
raw signals = do
  input <- hGetEncoding stdin
  output <- hGetEncoding stdout
  modes <- allocaArray 4 $ \saved -> do
    done <- c_raw (if signals then 1 else 0) saved
    when (done == 0) (ioError (userError "the console cannot be made ready for editing"))
    peekArray 4 saved
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetNewlineMode stdin noNewlineTranslation
  pure (Saved modes input output)

restore :: Saved -> IO ()
restore (Saved modes input output) = do
  hSetNewlineMode stdin nativeNewlineMode
  mapM_ (hSetEncoding stdin) input
  mapM_ (hSetEncoding stdout) output
  withArray modes c_restore

-- | Commands for the keys the console would otherwise handle: Ctrl-D for
-- end of file, as on other systems, and Ctrl-C to interrupt unless that is
-- left to the console. There is nothing to suspend a program with.
controls :: Saved -> Bool -> [(KeyEvent, Cmd.Cmd)]
controls _ signals = (key '\EOT', Cmd.EndOfFile) : [(key '\ETX', Cmd.Interrupt) | not signals]

-- | A program cannot be stopped and resumed from its console on Windows.
suspend :: IO ()
suspend = pure ()
