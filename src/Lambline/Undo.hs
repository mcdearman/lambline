-- | The record of changes to the input, for undoing them.
module Lambline.Undo
  ( Changeset,
    new,
    begin,
    end,
    record,
    undo,
    truncateTo,
    lastInsert,
  )
where

import Data.Char (isAlphaNum)
import Lambline.Command (RepeatCount)
import Lambline.LineBuffer (Change (..), LineBuffer (..), deleteRange, insertStr, replaceRange)

data Entry
  = -- | The start and end of changes that undo as one.
    Begin
  | End
  | Insert Int String
  | Delete Int String
  | Replace Int String String

data Changeset = Changeset
  { level :: Int,
    -- | Newest first.
    undos :: [Entry]
  }

new :: Changeset
new = Changeset 0 []

-- | Start a group, giving a mark to 'truncateTo'.
begin :: Changeset -> (Int, Changeset)
begin cs = (length (undos cs), cs {level = level cs + 1, undos = Begin : undos cs})

-- | Close every open group, saying whether any held a change.
end :: Changeset -> (Bool, Changeset)
end cs = foldl close (False, cs {level = 0}) (replicate (level cs) ())
  where
    close (touched, c) () = case undos c of
      Begin : rest -> (touched, c {undos = rest})
      _ -> (True, c {undos = End : undos c})

-- | Note a change. A letter or digit typed or deleted next to the last one
-- joins it, so that undo works a word at a time.
record :: Change -> Changeset -> Changeset
record change cs = cs {undos = merged}
  where
    merged = case (change, undos cs) of
      (InsertedChar i c, Insert j s : rest) | isAlphaNum c && j + length s == i -> Insert j (s ++ [c]) : rest
      (InsertedChar i c, rest) -> Insert i [c] : rest
      (Inserted _ "", rest) -> rest
      (Inserted i s, rest) -> Insert i s : rest
      (Deleted _ "" _, rest) -> rest
      (Deleted i [c] _, Delete j s : rest)
        | isAlphaNum c && j == i -> Delete j (s ++ [c]) : rest
        | isAlphaNum c && j == i + 1 -> Delete i (c : s) : rest
      (Deleted i s _, rest) -> Delete i s : rest
      (Replaced i old s, Replace j old' s' : rest) | j + length s' == i -> Replace j (old' ++ old) (s' ++ s) : rest
      (Replaced i old s, rest) -> Replace i old s : rest

-- | Undo the last n changes or groups, if there is anything to undo.
undo :: RepeatCount -> LineBuffer -> Changeset -> Maybe (LineBuffer, Changeset)
undo n lb0 cs0 = go 0 (0 :: Int) False lb0 cs0
  where
    go count waiting undone lb cs = case undos cs of
      [] -> finish undone lb cs
      entry : rest ->
        let (waiting', lb', undone') = case entry of
              Begin -> (waiting - 1, lb, undone)
              End -> (waiting + 1, lb, undone)
              Insert i s -> (waiting, fst (deleteRange i (i + length s) lb), True)
              Delete i s -> (waiting, (fst (insertStr i s lb)) {pos = i + length s}, True)
              Replace i old s -> (waiting, fst (replaceRange i (i + length s) old lb), True)
            cs' = cs {undos = rest}
            count' = if waiting' <= 0 then count + 1 else count
         in if waiting' <= 0 && count' >= n then finish undone' lb' cs' else go count' waiting' undone' lb' cs'
    finish undone lb cs = if undone then Just (lb, cs) else Nothing

-- | Forget every change after a mark from 'begin'.
truncateTo :: Int -> Changeset -> Changeset
truncateTo mark cs = cs {undos = drop (length (undos cs) - mark) (undos cs)}

-- | The text most recently inserted, if inserting was the last change.
lastInsert :: Changeset -> Maybe String
lastInsert = go . undos
  where
    go = \case
      Insert _ s : _ -> Just s
      Replace _ _ s : _ -> Just s
      End : rest -> go rest
      _ -> Nothing
