-- | Text that has been killed, to be yanked back.
module Lambline.KillRing
  ( KillRing,
    Mode (..),
    new,
    reset,
    kill,
    yank,
    yankPop,
  )
where

data Action = Killed | Yanked Int | Other

-- | Which end of the current entry a kill joins.
data Mode = Append | Prepend

data KillRing = KillRing
  { slots :: [String],
    index :: Int,
    lastAction :: Action,
    capacity :: Int
  }

new :: Int -> KillRing
new = KillRing [] 0 Other

-- | End a run of kills: the next one starts a new entry.
reset :: KillRing -> KillRing
reset ring = ring {lastAction = Other}

-- | Kills in a row build up one entry.
kill :: String -> Mode -> KillRing -> KillRing
kill s mode ring
  | capacity ring == 0 = ring {lastAction = Killed}
  | Killed <- lastAction ring = ring {slots = adjust (index ring) join (slots ring)}
  | otherwise =
      let i
            | index ring == capacity ring - 1 = 0
            | null (slots ring) = index ring
            | otherwise = index ring + 1
          slots'
            | i == length (slots ring) = slots ring ++ [s]
            | otherwise = adjust i (const s) (slots ring)
       in ring {slots = slots', index = i, lastAction = Killed}
  where
    join old = case mode of
      Append -> old ++ s
      Prepend -> s ++ old
    adjust i f xs = [if j == i then f x else x | (j, x) <- zip [0 :: Int ..] xs]

-- | The current entry.
yank :: KillRing -> Maybe (String, KillRing)
yank ring = case drop (index ring) (slots ring) of
  s : _ -> Just (s, ring {lastAction = Yanked (length s)})
  [] -> Nothing

-- | Straight after a yank: the entry before, and the length of the text it
-- is to replace.
yankPop :: KillRing -> Maybe (Int, String, KillRing)
yankPop ring = case lastAction ring of
  Yanked size | not (null (slots ring)) ->
    let i = if index ring == 0 then length (slots ring) - 1 else index ring - 1
        s = slots ring !! i
     in Just (size, s, ring {index = i, lastAction = Yanked (length s)})
  _ -> Nothing
