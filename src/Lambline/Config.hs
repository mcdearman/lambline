-- | Preferences.
module Lambline.Config
  ( Config (..),
    defaultConfig,
    HistoryDuplicates (..),
    CompletionType (..),
    EditMode (..),
    BellStyle (..),
    ColorMode (..),
  )
where

data Config = Config
  { -- | How many history entries to keep.
    maxHistorySize :: Int,
    historyDuplicates :: HistoryDuplicates,
    -- | Whether to leave lines that start with a space out of the history.
    historyIgnoreSpace :: Bool,
    completionType :: CompletionType,
    -- | With 'List' completion, list the candidates on the first Tab rather
    -- than the second.
    completionShowAllIfAmbiguous :: Bool,
    -- | With 'List' completion, ask before listing more candidates than
    -- this.
    completionPromptLimit :: Int,
    -- | How long, in milliseconds, to wait after Escape to see whether it
    -- starts a key sequence. 'Nothing' waits for the next key, so Escape
    -- then @b@ is Alt-B however slowly typed.
    keyseqTimeout :: Maybe Int,
    editMode :: EditMode,
    -- | Add each line 'Lambline.readline' returns to the history.
    autoAddHistory :: Bool,
    bellStyle :: BellStyle,
    colorMode :: ColorMode,
    -- | The width of a tab.
    tabStop :: Int,
    -- | How far Vi's @>@ and @<@ indent.
    indentSize :: Int,
    -- | Before showing the prompt, ask the terminal where the cursor is,
    -- and start a new line if it is not in the first column.
    checkCursorPosition :: Bool,
    -- | Have the terminal mark pasted text, so that a pasted line break is
    -- text rather than Enter.
    enableBracketedPaste :: Bool,
    -- | Have the terminal show each redraw all at once.
    enableSynchronizedOutput :: Bool,
    -- | Leave the terminal's signal keys working: Ctrl-C then raises
    -- SIGINT rather than arriving as a key.
    enableSignals :: Bool,
    -- | Shown at the start of each line of input after the first. Give it
    -- the width of the prompt to keep indentation lined up. (rustyline has
    -- no such thing: its later lines start in the first column.)
    continuationPrompt :: String
  }

-- | rustyline's defaults.
defaultConfig :: Config
defaultConfig =
  Config
    { maxHistorySize = 100,
      historyDuplicates = IgnoreConsecutive,
      historyIgnoreSpace = False,
      completionType = Circular,
      completionShowAllIfAmbiguous = False,
      completionPromptLimit = 100,
      keyseqTimeout = Nothing,
      editMode = Emacs,
      autoAddHistory = False,
      bellStyle = Audible,
      colorMode = Enabled,
      tabStop = 8,
      indentSize = 2,
      checkCursorPosition = False,
      enableBracketedPaste = True,
      enableSynchronizedOutput = True,
      enableSignals = False,
      continuationPrompt = ""
    }

data HistoryDuplicates
  = AlwaysAdd
  | -- | Skip a line that repeats the one before it.
    IgnoreConsecutive
  deriving (Eq, Show)

data CompletionType
  = -- | Each Tab puts the next candidate in the input, as in Vim.
    Circular
  | -- | Tab completes as far as the candidates agree, and lists them, as in
    -- Bash.
    List
  deriving (Eq, Show)

data EditMode = Emacs | Vi
  deriving (Eq, Show)

data BellStyle
  = Audible
  | NoBell
  | -- | Flash the screen.
    Visible
  deriving (Eq, Show)

data ColorMode
  = -- | Use the highlighter when writing to a terminal that takes colour.
    Enabled
  | -- | Use it regardless.
    Forced
  | Disabled
  deriving (Eq, Show)
