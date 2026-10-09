-- | The commands keys are bound to.
module Lambline.Command
  ( RepeatCount,
    Cmd (..),
    Movement (..),
    WordDef (..),
    At (..),
    Anchor (..),
    CharSearch (..),
    opposite,
    shouldResetKillRing,
    isRepeatableChange,
    isRepeatable,
    redoMovement,
  )
where

import Data.Maybe (fromMaybe)

-- | How many times to do something.
type RepeatCount = Int

data Cmd
  = Abort
  | AcceptLine
  | BeginningOfHistory
  | CapitalizeWord
  | ClearScreen
  | Complete
  | CompleteBackward
  | -- | Take the hint into the input.
    CompleteHint
  | Dedent Movement
  | DowncaseWord
  | EndOfFile
  | EndOfHistory
  | -- | Incremental search, forwards.
    ForwardSearchHistory
  | -- | Search for an entry starting with the text before the cursor.
    HistorySearchBackward
  | HistorySearchForward
  | Indent Movement
  | Insert RepeatCount String
  | Interrupt
  | Kill Movement
  | Move Movement
  | NextHistory
  | Noop
  | Repaint
  | -- | Replace the character under the cursor (Vi replace mode).
    Overwrite Char
  | PreviousHistory
  | -- | Insert the next character typed, whatever it is.
    QuotedInsert
  | ReplaceChar RepeatCount Char
  | -- | Kill what a movement covers and insert text, if given.
    Replace Movement (Maybe String)
  | -- | Incremental search, backwards.
    ReverseSearchHistory
  | SelfInsert RepeatCount Char
  | Suspend
  | TransposeChars
  | TransposeWords RepeatCount
  | Undo RepeatCount
  | Unknown
  | UpcaseWord
  | ViYankTo Movement
  | Yank RepeatCount Anchor
  | YankPop
  | -- | Up a line, or to the previous history entry from the first line.
    LineUpOrPreviousHistory RepeatCount
  | LineDownOrNextHistory RepeatCount
  | -- | Insert a line break.
    Newline
  | -- | Accept the input if it is valid, and otherwise insert a line break.
    -- The flag says whether to accept with the cursor in the middle of the
    -- input: 'True' suits mostly single-line input, 'False' input that is
    -- mostly several lines.
    AcceptOrInsertLine Bool
  deriving (Eq, Show)

data WordDef
  = -- | Anything but blanks.
    BigWord
  | -- | Letters and digits.
    EmacsWord
  | -- | Letters, digits and underscore.
    ViWord
  deriving (Eq, Show)

-- | Where a word movement stops.
data At = AtStart | BeforeEnd | AfterEnd
  deriving (Eq, Show)

-- | Where to paste, relative to the cursor.
data Anchor = After | Before
  deriving (Eq, Show)

data CharSearch
  = SearchForward Char
  | SearchForwardBefore Char
  | SearchBackward Char
  | SearchBackwardAfter Char
  deriving (Eq, Show)

opposite :: CharSearch -> CharSearch
opposite = \case
  SearchForward c -> SearchBackward c
  SearchForwardBefore c -> SearchBackwardAfter c
  SearchBackward c -> SearchForward c
  SearchBackwardAfter c -> SearchForwardBefore c

data Movement
  = -- | Not a movement but a range: the whole current line.
    WholeLine
  | BeginningOfLine
  | EndOfLine
  | BackwardWord RepeatCount WordDef
  | ForwardWord RepeatCount At WordDef
  | ViCharSearch RepeatCount CharSearch
  | -- | The first non-blank character of the line.
    ViFirstPrint
  | BackwardChar RepeatCount
  | ForwardChar RepeatCount
  | LineUp RepeatCount
  | LineDown RepeatCount
  | -- | Not a movement but a range: all of the input.
    WholeBuffer
  | BeginningOfBuffer
  | EndOfBuffer
  deriving (Eq, Show)

-- | Whether a command ends a run of kills, so that the next kill starts a
-- new entry in the kill ring instead of adding to the last.
shouldResetKillRing :: Cmd -> Bool
shouldResetKillRing = \case
  Kill (BackwardChar _) -> True
  Kill (ForwardChar _) -> True
  ClearScreen -> False
  Kill _ -> False
  Replace _ _ -> False
  Noop -> False
  Suspend -> False
  Yank _ _ -> False
  YankPop -> False
  _ -> True

-- | Whether Vi's @.@ can repeat a command.
isRepeatableChange :: Cmd -> Bool
isRepeatableChange = \case
  Dedent _ -> True
  Indent _ -> True
  Insert _ _ -> True
  Kill _ -> True
  ReplaceChar _ _ -> True
  Replace _ _ -> True
  SelfInsert _ _ -> True
  ViYankTo _ -> True
  Yank _ _ -> True
  _ -> False

isRepeatable :: Cmd -> Bool
isRepeatable = \case
  Move _ -> True
  cmd -> isRepeatableChange cmd

-- | A movement again, with a new count if one is given.
redoMovement :: Maybe RepeatCount -> Movement -> Movement
redoMovement new = \case
  BackwardWord n w -> BackwardWord (count n) w
  ForwardWord n at w -> ForwardWord (count n) at w
  ViCharSearch n cs -> ViCharSearch (count n) cs
  BackwardChar n -> BackwardChar (count n)
  ForwardChar n -> ForwardChar (count n)
  LineUp n -> LineUp (count n)
  LineDown n -> LineDown (count n)
  mvt -> mvt
  where
    count n = fromMaybe n new
