-- | Earlier input. An entry may span several lines.
module Lambline.History
  ( History,
    new,
    entries,
    size,
    isEmpty,
    get,
    add,
    clear,
    setMaxLen,
    SearchDirection (..),
    SearchResult (..),
    search,
    startsWith,

    -- * Files
    encode,
    decode,
    load,
    save,
    append,
  )
where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)
import Data.Maybe (listToMaybe)
import Lambline.Config
import System.Directory (doesFileExist)
import System.IO (readFile')

data History = History
  { -- | Oldest first.
    entries :: [String],
    maxLen :: Int,
    ignoreSpace :: Bool,
    ignoreDups :: Bool,
    -- | How many entries have been added since the history was last loaded
    -- or saved.
    unsaved :: Int
  }

new :: Config -> History
new config = History [] (maxHistorySize config) (historyIgnoreSpace config) (historyDuplicates config == IgnoreConsecutive) 0

size :: History -> Int
size = length . entries

isEmpty :: History -> Bool
isEmpty = null . entries

get :: Int -> History -> Maybe String
get i h = listToMaybe (drop i (entries h)) >>= \e -> if i < 0 then Nothing else Just e

-- | Add an entry, unless it is empty or the configuration says to skip it.
add :: String -> History -> (Bool, History)
add line h
  | maxLen h == 0 || null line = (False, h)
  | ignoreSpace h && all isSpace (take 1 line) = (False, h)
  | ignoreDups h && take 1 (reverse (entries h)) == [line] = (False, h)
  | otherwise = (True, trim h {entries = entries h ++ [line], unsaved = unsaved h + 1})

trim :: History -> History
trim h = h {entries = drop (length (entries h) - maxLen h) (entries h)}

clear :: History -> History
clear h = h {entries = [], unsaved = 0}

setMaxLen :: Int -> History -> History
setMaxLen n h = trim h {maxLen = n}

data SearchDirection = Forward | Reverse
  deriving (Eq, Show)

data SearchResult = SearchResult
  { resultEntry :: String,
    resultIndex :: Int,
    -- | Where in the entry the term ends.
    resultPos :: Int
  }
  deriving (Eq, Show)

-- | The nearest entry containing a term, from an index in a direction.
search :: String -> Int -> SearchDirection -> History -> Maybe SearchResult
search term = searchMatch term (\e -> listToMaybe [i + length term | (i, t) <- zip [0 ..] (tails e), term `isPrefixOf` t])

-- | The nearest entry starting with a term.
startsWith :: String -> Int -> SearchDirection -> History -> Maybe SearchResult
startsWith term = searchMatch term (\e -> if term `isPrefixOf` e then Just (length term) else Nothing)

searchMatch :: String -> (String -> Maybe Int) -> Int -> SearchDirection -> History -> Maybe SearchResult
searchMatch term match start dir h
  | null term || start < 0 || start >= size h = Nothing
  | otherwise = listToMaybe [SearchResult e i p | (i, e) <- candidates, Just p <- [match e]]
  where
    indexed = zip [0 ..] (entries h)
    candidates = case dir of
      Forward -> drop start indexed
      Reverse -> reverse (take (start + 1) indexed)

-- Files -------------------------------------------------------------------

-- | A history file starts with a version line and has one entry per line,
-- with line breaks and backslashes escaped.
encode :: [String] -> String
encode es = unlines (version : map (concatMap escape) es)
  where
    escape = \case
      '\n' -> "\\n"
      '\\' -> "\\\\"
      c -> [c]

version :: String
version = "#V2"

-- | Read a history file, with or without the version line: without it,
-- lines are taken as they are.
decode :: String -> [String]
decode contents = case lines contents of
  v : rest | v == version -> map unescape rest
  ls -> ls
  where
    unescape = \case
      '\\' : 'n' : cs -> '\n' : unescape cs
      '\\' : c : cs -> c : unescape cs
      c : cs -> c : unescape cs
      [] -> []

-- | Add a file's entries to the history. Does nothing if there is no file.
load :: FilePath -> History -> IO History
load path h = do
  exists <- doesFileExist path
  if not exists
    then pure h
    else do
      contents <- readFile' path
      pure (foldl (\acc e -> snd (add e acc)) h (decode contents)) {unsaved = 0}

-- | Write the whole history to a file.
save :: FilePath -> History -> IO History
save path h = h {unsaved = 0} <$ writeFile path (encode (entries h))

-- | Add the entries not yet saved to the end of a file, which other
-- sessions may have written to as well.
append :: FilePath -> History -> IO History
append path h = do
  exists <- doesFileExist path
  unless exists (writeFile path (version ++ "\n"))
  let fresh = drop (size h - unsaved h) (entries h)
  when (not (null fresh)) (appendFile path (unlines (drop 1 (lines (encode fresh)))))
  pure h {unsaved = 0}
