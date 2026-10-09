-- | The text being edited and the cursor within it. May span several lines.
--
-- Operations that can fail to apply, such as moving left from the start,
-- give 'Nothing'. Those that alter the text also report what they did, for
-- undo and for the kill ring.
module Lambline.LineBuffer
  ( LineBuffer (..),
    empty,
    fromParts,
    Change (..),
    Direction (..),
    Edit,
    isCursorAtEnd,
    isEndOfInput,
    update,

    -- * Moving
    moveBackward,
    moveForward,
    moveHome,
    moveEnd,
    moveBufferStart,
    moveBufferEnd,
    moveToPrevWord,
    moveToNextWord,
    moveToLineUp,
    moveToLineDown,
    moveTo,

    -- * Editing
    insert,
    yank,
    yankPop,
    replaceRange,
    insertStr,
    deleteRange,
    delete,
    transposeChars,
    transposeWords,
    WordAction (..),
    editWord,
    copy,
    kill,
    indent,
  )
where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isSpace, toLower, toUpper)
import Data.List (dropWhileEnd, findIndex)
import Data.Maybe (fromMaybe, listToMaybe)
import Lambline.Command
import Lambline.Layout (strWidth)

-- | The text, and the cursor as a count of the characters before it.
data LineBuffer = LineBuffer {text :: String, pos :: Int}
  deriving (Eq, Show)

empty :: LineBuffer
empty = LineBuffer "" 0

-- | A buffer from the text before and after the cursor.
fromParts :: String -> String -> LineBuffer
fromParts l r = LineBuffer (l ++ r) (length l)

-- | Which way a deletion ran: it decides whether text joins the front or
-- the back of the current kill.
data Direction = Forward | Backward
  deriving (Eq, Show)

data Change
  = InsertedChar Int Char
  | Inserted Int String
  | Deleted Int String Direction
  | Replaced Int String String
  deriving (Eq, Show)

type Edit = LineBuffer -> Maybe (LineBuffer, [Change])

len :: LineBuffer -> Int
len = length . text

slice :: Int -> Int -> LineBuffer -> String
slice a b = take (b - a) . drop a . text

isCursorAtEnd :: LineBuffer -> Bool
isCursorAtEnd lb = pos lb == len lb

-- | Whether only blanks follow the cursor.
isEndOfInput :: LineBuffer -> Bool
isEndOfInput lb = pos lb >= length (dropWhileEnd isSpace (text lb))

-- | Replace everything.
update :: String -> Int -> LineBuffer -> (LineBuffer, [Change])
update new p lb =
  ( LineBuffer new (min p (length new)),
    [Deleted 0 (text lb) Forward | not (null (text lb))] ++ [Inserted 0 new | not (null new)]
  )

-- | The start and end of the line the cursor is on.
startOfLine, endOfLine :: LineBuffer -> Int
startOfLine lb = lineStartAt (pos lb) lb
endOfLine lb = lineEndAt (pos lb) lb

lineStartAt, lineEndAt :: Int -> LineBuffer -> Int
lineStartAt p lb = p - length (takeWhile (/= '\n') (reverse (take p (text lb))))
lineEndAt p lb = p + length (takeWhile (/= '\n') (drop p (text lb)))

nextPos, prevPos :: RepeatCount -> LineBuffer -> Maybe Int
nextPos n lb = min (len lb) (pos lb + n) <$ guard (pos lb < len lb)
prevPos n lb = max 0 (pos lb - n) <$ guard (pos lb > 0)

-- Moving ------------------------------------------------------------------

moveTo' :: LineBuffer -> Int -> LineBuffer
moveTo' lb p = lb {pos = p}

moveBackward, moveForward :: RepeatCount -> LineBuffer -> Maybe LineBuffer
moveBackward n lb = moveTo' lb <$> prevPos n lb
moveForward n lb = moveTo' lb <$> nextPos n lb

-- | To the start or end of the current line.
moveHome, moveEnd :: LineBuffer -> Maybe LineBuffer
moveHome lb = moveTo' lb (startOfLine lb) <$ guard (startOfLine lb /= pos lb)
moveEnd lb = moveTo' lb (endOfLine lb) <$ guard (endOfLine lb /= pos lb)

moveBufferStart, moveBufferEnd :: LineBuffer -> Maybe LineBuffer
moveBufferStart lb = moveTo' lb 0 <$ guard (pos lb > 0)
moveBufferEnd lb = moveTo' lb (len lb) <$ guard (pos lb < len lb)

isWordChar :: WordDef -> Char -> Bool
isWordChar = \case
  EmacsWord -> isAlphaNum
  ViWord -> isViWordChar
  BigWord -> not . isSpace

isViWordChar, isOtherChar :: Char -> Bool
isViWordChar c = isAlphaNum c || c == '_'
isOtherChar c = not (isSpace c || isViWordChar c)

-- | Whether a word starts at the second of two adjacent characters, or
-- ends at the first.
isStartOfWord, isEndOfWord :: WordDef -> Char -> Char -> Bool
isStartOfWord def previous c =
  (not (isWordChar def previous) && isWordChar def c)
    || (def == ViWord && not (isOtherChar previous) && isOtherChar c)
isEndOfWord def c following =
  (not (isWordChar def following) && isWordChar def c)
    || (def == ViWord && not (isOtherChar following) && isOtherChar c)

-- | The start of the n-th word back from a position.
prevWordPos :: Int -> WordDef -> RepeatCount -> LineBuffer -> Maybe Int
prevWordPos p def n lb
  | p == 0 = Nothing
  | otherwise = Just (outer n 0 (reverse (zip [0 ..] (take p (text lb)))))
  where
    outer :: Int -> Int -> [(Int, Char)] -> Int
    outer 0 sow _ = sow
    outer k _ chars = case chars of
      c : rest -> inner k c rest
      [] -> 0
    inner k (j, y) = \case
      (i, x) : rest
        | isStartOfWord def x y -> outer (k - 1) j rest
        | otherwise -> inner k (i, x) rest
      [] -> 0

-- | Where the n-th word forward from a position starts or ends.
nextWordPos :: Int -> At -> WordDef -> RepeatCount -> LineBuffer -> Maybe Int
nextWordPos p at def n lb
  | p == len lb = Nothing
  | wp /= 0 = Just (wp + p)
  | def == EmacsWord || at == AfterEnd = Just (len lb)
  | otherwise = case final of
      Just (i, _) | i /= 0 -> Just (i + p)
      _ -> Nothing
  where
    chars = zip [0 ..] (drop p (text lb))
    (wp, final)
      | at == BeforeEnd = outer n 0 (listToMaybe chars) (drop 1 chars)
      | otherwise = outer n 0 Nothing chars

    outer :: Int -> Int -> Maybe (Int, Char) -> [(Int, Char)] -> (Int, Maybe (Int, Char))
    outer 0 found current _ = (found, current)
    outer k _ _ rest = case rest of
      c : rest' -> inner k c rest'
      [] -> (0, Nothing)
    inner k current@(i, x) = \case
      following@(j, y) : rest
        | at == AtStart && isStartOfWord def x y -> outer (k - 1) j (Just current) rest
        | at /= AtStart && isEndOfWord def x y ->
            outer (k - 1) (if def == EmacsWord || at == AfterEnd then j else i) (Just current) rest
        | otherwise -> inner k following rest
      [] -> (0, Just current)

moveToPrevWord :: WordDef -> RepeatCount -> LineBuffer -> Maybe LineBuffer
moveToPrevWord def n lb = moveTo' lb <$> prevWordPos (pos lb) def n lb

moveToNextWord :: At -> WordDef -> RepeatCount -> LineBuffer -> Maybe LineBuffer
moveToNextWord at def n lb = moveTo' lb <$> nextWordPos (pos lb) at def n lb

-- | To the same column n lines up or down. The first line starts so many
-- columns further right than the others (the prompt may be wider than the
-- continuation prompt).
moveToLineUp, moveToLineDown :: RepeatCount -> Int -> LineBuffer -> Maybe LineBuffer
moveToLineUp n firstLineOffset lb = do
  let start = startOfLine lb
  guard (start > 0)
  let column = strWidth (slice start (pos lb) lb)
      target = iterate (\s -> if s == 0 then 0 else lineStartAt (s - 1) lb) start !! n
  pure (moveTo' lb (atColumn column firstLineOffset target lb))
moveToLineDown n firstLineOffset lb = do
  let start = startOfLine lb
      end = endOfLine lb
  guard (end < len lb)
  let column = strWidth (slice start (pos lb) lb) + (if start == 0 then firstLineOffset else 0)
      target = iterate (\s -> let e = lineEndAt s lb in if e == len lb then s else e + 1) start !! n
  pure (moveTo' lb (atColumn column firstLineOffset target lb))

-- | The position at a screen column of the line starting at a position, or
-- its end if it is too short.
atColumn :: Int -> Int -> Int -> LineBuffer -> Int
atColumn column firstLineOffset start lb = start + min (max 0 wanted) (lineEndAt start lb - start)
  where
    wanted = if start == 0 then column - firstLineOffset else column

-- | Where a character search lands.
searchCharPos :: CharSearch -> RepeatCount -> LineBuffer -> Maybe Int
searchCharPos cs n lb = case cs of
  SearchBackward c -> backward c
  SearchBackwardAfter c -> (+ 1) <$> backward c
  SearchForward c -> forward c
  SearchForwardBefore c -> subtract 1 <$> forward c
  where
    backward c = lastOf [i | (i, ch) <- reverse (zip [0 ..] (take (pos lb) (text lb))), ch == c]
    forward c = do
      guard (pos lb + 1 < len lb)
      lastOf [i | (i, ch) <- drop (pos lb + 1) (zip [0 ..] (text lb)), ch == c]
    lastOf = listToMaybe . reverse . take n

moveTo :: CharSearch -> RepeatCount -> LineBuffer -> Maybe LineBuffer
moveTo cs n lb = moveTo' lb <$> searchCharPos cs n lb

-- Editing -----------------------------------------------------------------

-- | Remove a range, leaving the cursor where it is.
drain :: Int -> Int -> Direction -> LineBuffer -> (LineBuffer, [Change])
drain a b dir lb = (lb {text = take a (text lb) ++ drop b (text lb)}, [Deleted a (slice a b lb) dir | b > a])

-- | Insert a character n times at the cursor.
insert :: Char -> RepeatCount -> Edit
insert c n lb = do
  guard (n > 0)
  let (l, r) = splitAt (pos lb) (text lb)
      change = if n == 1 then InsertedChar (pos lb) c else Inserted (pos lb) (replicate n c)
  pure (LineBuffer (l ++ replicate n c ++ r) (pos lb + n), [change])

-- | Insert text n times at the cursor.
yank :: String -> RepeatCount -> Edit
yank s n lb = do
  guard (not (null s) && n > 0)
  let (l, r) = splitAt (pos lb) (text lb)
      new = concat (replicate n s)
  pure (LineBuffer (l ++ new ++ r) (pos lb + length new), [Inserted (pos lb) new])

-- | Replace the text just yanked, of the given length, with other text.
yankPop :: Int -> String -> Edit
yankPop size s lb = do
  guard (not (null s) && size <= pos lb)
  pure (replaceRange (pos lb - size) (pos lb) s lb)

-- | Replace a range, leaving the cursor after the new text.
replaceRange :: Int -> Int -> String -> LineBuffer -> (LineBuffer, [Change])
replaceRange a b new lb =
  (LineBuffer (take a (text lb) ++ new ++ drop b (text lb)) (a + length new), [Replaced a (slice a b lb) new])

-- | Insert text at a position, leaving the cursor's count as it is.
insertStr :: Int -> String -> LineBuffer -> (LineBuffer, [Change])
insertStr i s lb = (lb {text = take i (text lb) ++ s ++ drop i (text lb)}, [Inserted i s | not (null s)])

-- | Remove a range, leaving the cursor at its start.
deleteRange :: Int -> Int -> LineBuffer -> (LineBuffer, [Change])
deleteRange a b lb = drain a b Forward lb {pos = a}

-- | Delete n characters at the cursor.
delete :: RepeatCount -> Edit
delete n lb = (\p -> drain (pos lb) p Forward lb) <$> nextPos n lb

backspace :: RepeatCount -> Edit
backspace n lb = (\p -> drain p (pos lb) Backward lb {pos = p}) <$> prevPos n lb

-- | Run edits one after another, as far as they apply.
chain :: [LineBuffer -> (LineBuffer, [Change])] -> LineBuffer -> (LineBuffer, [Change])
chain steps lb = foldl (\(b, cs) step -> let (b', cs') = step b in (b', cs ++ cs')) (lb, []) steps

try :: Edit -> LineBuffer -> (LineBuffer, [Change])
try edit lb = fromMaybe (lb, []) (edit lb)

move :: (LineBuffer -> Maybe LineBuffer) -> LineBuffer -> (LineBuffer, [Change])
move f lb = (fromMaybe lb (f lb), [])

-- | Kill from the cursor to the end of the line, or the line break if
-- already there.
killLine, killBuffer, discardLine, discardBuffer :: Edit
killLine lb = do
  guard (pos lb < len lb)
  if pos lb == endOfLine lb then delete 1 lb else pure (drain (pos lb) (endOfLine lb) Forward lb)
killBuffer lb = drain (pos lb) (len lb) Forward lb <$ guard (pos lb < len lb)
discardLine lb = do
  guard (pos lb > 0)
  let start = startOfLine lb
  if pos lb == start then backspace 1 lb else pure (drain start (pos lb) Backward lb {pos = start})
discardBuffer lb = drain 0 (pos lb) Backward lb {pos = 0} <$ guard (pos lb > 0)

-- | Swap the character before the cursor with the one under it.
transposeChars :: Edit
transposeChars lb = do
  guard (pos lb > 0 && len lb >= 2)
  let lb1 = if isCursorAtEnd lb then lb {pos = pos lb - 1} else lb
      c = slice (pos lb1) (pos lb1 + 1) lb1
  pure (chain [try (delete 1), move (moveBackward 1), try (yank c 1), move (moveForward 1)] lb1)

deletePrevWord :: WordDef -> RepeatCount -> Edit
deletePrevWord def n lb = (\p -> drain p (pos lb) Backward lb {pos = p}) <$> prevWordPos (pos lb) def n lb

deleteWord :: At -> WordDef -> RepeatCount -> Edit
deleteWord at def n lb = (\p -> drain (pos lb) p Forward lb) <$> nextWordPos (pos lb) at def n lb

-- | The range a character search covers, for deleting or copying.
searchRange :: CharSearch -> RepeatCount -> LineBuffer -> Maybe (Int, Int, Direction)
searchRange cs n lb = case cs of
  SearchBackward _ -> (\p -> (p, pos lb, Backward)) <$> searchCharPos cs n lb
  SearchBackwardAfter _ -> (\p -> (p, pos lb, Backward)) <$> searchCharPos cs n lb
  SearchForwardBefore c -> (\p -> (pos lb, p, Forward)) <$> searchCharPos (SearchForward c) n lb
  SearchForward _ -> (\p -> (pos lb, p + 1, Forward)) <$> searchCharPos cs n lb

-- | The lines from n above the cursor's to the cursor's, or from the
-- cursor's to n below.
nLinesUp, nLinesDown :: RepeatCount -> LineBuffer -> Maybe (Int, Int)
nLinesUp n lb = do
  let start = startOfLine lb
  guard (start > 0)
  let end = min (len lb) (endOfLine lb + 1)
  pure (iterate (\s -> if s == 0 then 0 else lineStartAt (s - 1) lb) start !! n, end)
nLinesDown n lb = do
  let end = endOfLine lb
  guard (end < len lb)
  let start = max 0 (startOfLine lb - 1)
  pure (start, iterate (\e -> if e >= len lb then len lb else min (len lb) (lineEndAt e lb + 1)) (end + 1) !! n)

data WordAction = Capitalize | Lowercase | Uppercase

-- | Change the case of the next word.
editWord :: WordAction -> Edit
editWord action lb = do
  start <- (+ pos lb) <$> findIndex isAlphaNum (drop (pos lb) (text lb))
  end <- nextWordPos start AfterEnd EmacsWord 1 lb
  guard (start < end)
  let word = slice start end lb
      result = case action of
        Capitalize -> map toUpper (take 1 word) ++ map toLower (drop 1 word)
        Lowercase -> map toLower word
        Uppercase -> map toUpper word
      (lb', changes) = chain [drain start end Forward, insertStr start result] lb
  pure (lb' {pos = start + length result}, changes)

-- | Swap the word at the cursor with the one before it.
transposeWords :: RepeatCount -> Edit
transposeWords n lb = do
  let step f b = fromMaybe b (f b)
      w2End = step (moveToNextWord AfterEnd EmacsWord n) lb
      w2Beg = step (moveToPrevWord EmacsWord 1) w2End
      w1Beg = step (moveToPrevWord EmacsWord n) w2Beg
      w1End = step (moveToNextWord AfterEnd EmacsWord 1) w1Beg
  guard (pos w1Beg /= pos w2Beg && pos w2Beg >= pos w1End)
  let w1 = slice (pos w1Beg) (pos w1End) lb
      w2 = slice (pos w2Beg) (pos w2End) lb
      (lb', changes) =
        chain
          [ drain (pos w2Beg) (pos w2End) Forward,
            insertStr (pos w2Beg) w1,
            drain (pos w1Beg) (pos w1End) Forward,
            insertStr (pos w1Beg) w2
          ]
          lb
  pure (lb' {pos = pos w2End}, changes)

-- | The text a movement covers.
copy :: Movement -> LineBuffer -> Maybe String
copy mvt lb = do
  guard (not (null (text lb)))
  let p = pos lb
      from a = slice a p lb <$ guard (a /= p)
      to b = slice p b lb <$ guard (b /= p)
  case mvt of
    WholeLine -> slice (startOfLine lb) p lb <$ guard (startOfLine lb /= endOfLine lb)
    BeginningOfLine -> from (startOfLine lb)
    ViFirstPrint -> guard (p > 0) >> nextWordPos 0 AtStart BigWord 1 lb >>= \a -> pure (slice a p lb)
    EndOfLine -> to (endOfLine lb)
    EndOfBuffer -> to (len lb)
    WholeBuffer -> pure (text lb)
    BeginningOfBuffer -> from 0
    BackwardWord n def -> (\a -> slice a p lb) <$> prevWordPos p def n lb
    ForwardWord n at def -> (\b -> slice p b lb) <$> nextWordPos p at def n lb
    ViCharSearch n cs -> (\(a, b, _) -> slice a b lb) <$> searchRange cs n lb
    BackwardChar n -> (\a -> slice a p lb) <$> prevPos n lb
    ForwardChar n -> (\b -> slice p b lb) <$> nextPos n lb
    LineUp n -> (\(a, b) -> slice a b lb) <$> nLinesUp n lb
    LineDown n -> (\(a, b) -> slice a b lb) <$> nLinesDown n lb

-- | Remove the text a movement covers.
kill :: Movement -> Edit
kill mvt lb = case mvt of
  ForwardChar n -> delete n lb
  BackwardChar n -> backspace n lb
  EndOfLine -> killLine lb
  WholeLine -> killLine (fromMaybe lb (moveHome lb))
  BeginningOfLine -> discardLine lb
  BackwardWord n def -> deletePrevWord def n lb
  ForwardWord n at def -> deleteWord at def n lb
  ViCharSearch n cs -> (\(a, b, dir) -> drain a b dir lb {pos = a}) <$> searchRange cs n lb
  LineUp n -> (\(a, b) -> deleteRange a b lb) <$> nLinesUp n lb
  LineDown n -> (\(a, b) -> deleteRange a b lb) <$> nLinesDown n lb
  ViFirstPrint -> Nothing
  EndOfBuffer -> killBuffer lb
  BeginningOfBuffer -> discardBuffer lb
  WholeBuffer -> killBuffer lb {pos = 0}

-- | Indent, or dedent, the lines a movement covers by so many spaces.
indent :: Movement -> Int -> Bool -> LineBuffer -> (LineBuffer, [Change])
indent mvt amount dedent lb = go first (lines' (slice first lastEnd lb)) (lb, [])
  where
    p = pos lb
    (a, b) = fromMaybe (p, p) $ case mvt of
      EndOfBuffer -> Just (p, len lb)
      WholeBuffer -> Just (0, len lb)
      BeginningOfBuffer -> Just (0, p)
      BackwardWord n def -> (\x -> (x, p)) <$> prevWordPos p def n lb
      ForwardWord n at def -> (\x -> (p, x)) <$> nextWordPos p at def n lb
      LineUp n -> nLinesUp n lb
      LineDown n -> nLinesDown n lb
      _ -> Nothing
    first = lineStartAt a lb
    lastEnd = lineEndAt (if b > a && slice (b - 1) b lb == "\n" then b - 1 else b) lb
    lines' s = case break (== '\n') s of
      (l, _ : rest) -> l : lines' rest
      (l, []) -> [l]

    go _ [] acc = acc
    go index (l : ls) (cur, changes)
      | dedent =
          let removing = min amount (length (takeWhile isSpace l))
              (cur', cs) = drain index (index + removing) Forward cur
              moved
                | pos cur < index = pos cur
                | pos cur - index < removing = index
                | otherwise = pos cur - removing
           in go (index + length l + 1 - removing) ls (cur' {pos = moved}, changes ++ cs)
      | otherwise =
          let (cur', cs) = insertStr index (replicate amount ' ') cur
              moved = if pos cur >= index then pos cur + amount else pos cur
           in go (index + amount + length l + 1) ls (cur' {pos = moved}, changes ++ cs)

