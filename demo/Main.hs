-- | Echoes what you type, with a bit of everything: file name completion,
-- hints from the history, the matching bracket picked out, and input that
-- continues onto a new line while a bracket is open.
--
-- Run it with @vi@ as an argument for Vi mode, or with @ticks@ to see
-- another thread print while the editor waits.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, when)
import Lambline
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let vi = "vi" `elem` args
  editor <-
    newEditor
      defaultConfig
        { editMode = if vi then Vi else Emacs,
          -- In Vi mode Escape is a key in its own right, so do not wait
          -- long to see whether it starts a sequence.
          keyseqTimeout = if vi then Just 50 else Nothing,
          completionType = List,
          continuationPrompt = ". "
        }
  setHelper editor . Just $
    defaultHelper
      { complete = filenameCompleter,
        hint = historyHinter,
        highlight = matchingBracketHighlighter,
        highlightHint = \h -> "\ESC[2m" ++ h ++ "\ESC[0m",
        validate = matchingBracketValidator
      }
  bindSequence editor (KeySeq [alt '\r']) (Simple Newline)
  when ("ticks" `elem` args) $ do
    printer <- createExternalPrinter editor
    _ <- forkIO (forM_ [1 :: Int .. 3] (\n -> threadDelay 700000 >> printer ("tick " ++ show n)))
    pure ()
  let loop =
        readline editor "> " >>= \case
          Right line -> addHistoryEntry editor line >> putStrLn ("got: " ++ show line) >> loop
          Left Interrupted -> loop
          Left Eof -> pure ()
  loop
