-- | Binding keys to commands of your own choosing.
module Lambline.Binding
  ( Event (..),
    EventHandler (..),
    EventContext (..),
    InputMode (..),
    Bindings,
  )
where

import qualified Data.Map as Map
import Lambline.Command (Cmd, RepeatCount)
import Lambline.Config (EditMode)
import Lambline.Keys (KeyEvent)

-- | What a binding is for.
data Event
  = -- | Every key that has no binding of its own.
    AnyKey
  | -- | A key, or several pressed one after another.
    KeySeq [KeyEvent]
  deriving (Eq, Ord, Show)

data EventHandler
  = -- | Always the same command.
    Simple Cmd
  | -- | A command chosen by looking at the state of the editor, given the
    -- event, the repeat count and whether it is positive. 'Nothing' leaves
    -- the key to its usual binding.
    Conditional (Event -> RepeatCount -> Bool -> EventContext -> Maybe Cmd)

-- | What a conditional handler can see.
data EventContext = EventContext
  { contextMode :: EditMode,
    contextInputMode :: InputMode,
    contextHasHint :: Bool,
    contextHintText :: Maybe String,
    contextLine :: String,
    contextPos :: Int
  }

-- | Vi's modes. Emacs mode is always inserting.
data InputMode = CommandMode | InsertMode | ReplaceMode
  deriving (Eq, Show)

type Bindings = Map.Map Event EventHandler
