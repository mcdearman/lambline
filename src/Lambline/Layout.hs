-- | Working out where text lands on the screen, and redrawing it.
module Lambline.Layout
  ( Pos (..),
    charWidth,
    strWidth,
    position,
    display,
    Frame (..),
    render,
  )
where

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Maybe (fromMaybe)

-- | A screen position relative to where the prompt starts.
data Pos = Pos {row :: !Int, col :: !Int}
  deriving (Eq, Show)

-- | How many columns a character takes: an approximation of @wcwidth@.
charWidth :: Char -> Int
charWidth c
  | n < 0x20 || (n >= 0x7f && n < 0xa0) = 0
  | generalCategory c `elem` [NonSpacingMark, EnclosingMark, Format] = 0
  | any (\(lo, hi) -> n >= lo && n <= hi) wide = 2
  | otherwise = 1
  where
    n = ord c
    wide =
      [ (0x1100, 0x115f), (0x2e80, 0x303e), (0x3041, 0x33ff), (0x3400, 0x4dbf),
        (0x4e00, 0x9fff), (0xa000, 0xa4cf), (0xac00, 0xd7a3), (0xf900, 0xfaff),
        (0xfe30, 0xfe4f), (0xff00, 0xff60), (0xffe0, 0xffe6), (0x1f300, 0x1f64f),
        (0x1f900, 0x1f9ff), (0x20000, 0x3fffd)
      ]

-- | The width of text on one line, ignoring control sequences.
strWidth :: String -> Int
strWidth = \case
  [] -> 0
  '\ESC' : '[' : cs -> strWidth (dropSequence cs)
  c : cs -> charWidth c + strWidth cs

-- | The rest of a control sequence: parameters up to a final character.
dropSequence :: String -> String
dropSequence = drop 1 . dropWhile (\c -> c < '@' || c > '~')

-- | Where the cursor is after writing text from the origin, given the
-- terminal's width and the width of a tab. Control sequences take no space.
--
-- A terminal does not wrap until a character needs the room, so text that
-- exactly fills a row leaves the cursor on it. The position reported is the
-- start of the next row, where 'render' then puts it.
position :: Int -> Int -> String -> Pos
position width tab = go (Pos 0 0)
  where
    go p = \case
      [] -> if col p >= width then Pos (row p + 1) 0 else p
      '\n' : cs -> go (Pos (row p + 1) 0) cs
      '\ESC' : '[' : cs -> go p (dropSequence cs)
      c : cs ->
        let w = if c == '\t' then tab - col p `mod` tab else charWidth c
         in go (if col p + w > width then Pos (row p + 1) w else p {col = col p + w}) cs

-- | The text as shown: each line after the first starts with the
-- continuation prompt.
display :: String -> String -> String
display continuation = concatMap (\c -> if c == '\n' then '\n' : continuation else [c])

-- | Everything a redraw shows. Text comes plain, to measure, and styled, to
-- write; styling must not change its width.
data Frame = Frame
  { frameWidth :: Int,
    frameTab :: Int,
    -- | The row the cursor was left on by the last draw.
    frameOldRow :: Int,
    framePrompt :: String,
    framePromptStyled :: String,
    frameContinuation :: String,
    -- | The input up to the cursor.
    frameBefore :: String,
    frameLine :: String,
    frameLineStyled :: String,
    -- | A hint or message shown after the input.
    frameInfo :: Maybe String,
    frameInfoStyled :: Maybe String
  }

-- | The output that redraws a frame over the last one, where it leaves the
-- cursor, and where what is shown ends.
render :: Frame -> (String, Pos, Pos)
render f = (output, cursor, end)
  where
    cont = display (frameContinuation f)
    info = fromMaybe "" (frameInfo f)
    cursor = position (frameWidth f) (frameTab f) (framePrompt f ++ cont (frameBefore f))
    end = position (frameWidth f) (frameTab f) (framePrompt f ++ cont (frameLine f) ++ info)
    shown = framePromptStyled f ++ cont (frameLineStyled f) ++ fromMaybe "" (frameInfoStyled f)
    lastChar = take 1 (reverse (if null info then cont (frameLine f) else info))
    output =
      concat
        [ -- Back to the start of the prompt, and clear everything below.
          if frameOldRow f > 0 then "\ESC[" ++ show (frameOldRow f) ++ "A" else "",
          "\r\ESC[J",
          shown,
          -- Text that exactly fills the last row has not wrapped yet.
          if col end == 0 && row end > 0 && lastChar /= "\n" then "\n" else "",
          if row end > row cursor then "\ESC[" ++ show (row end - row cursor) ++ "A" else "",
          "\r",
          if col cursor > 0 then "\ESC[" ++ show (col cursor) ++ "C" else ""
        ]
