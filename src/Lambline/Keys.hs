{-# LANGUAGE PatternSynonyms #-}

-- | Keys, and decoding terminal input into them.
module Lambline.Keys
  ( KeyCode (..),
    Mods (..),
    noMods,
    KeyEvent (..),
    pattern Plain,
    pattern Ctrl,
    pattern Alt,
    pattern CtrlAlt,
    pattern CtrlKey,
    pattern AltKey,
    keyEvent,
    key,
    ctrl,
    alt,
    normalize,
    Source (..),
    readKey,
    readPaste,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char (chr, isAsciiLower, isAsciiUpper, isControl, isDigit, ord, toUpper)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

data KeyCode
  = -- | An escape sequence that means nothing here.
    KUnknown
  | KBackspace
  | -- | Shift-Tab.
    KBackTab
  | KPasteStart
  | KPasteEnd
  | KChar Char
  | KDelete
  | KDown
  | KEnd
  | KEnter
  | KEsc
  | -- | A function key.
    KF Int
  | KHome
  | KInsert
  | KLeft
  | KNull
  | KPageDown
  | KPageUp
  | KRight
  | KTab
  | KUp
  deriving (Eq, Ord, Show)

data Mods = Mods {modCtrl :: Bool, modAlt :: Bool, modShift :: Bool}
  deriving (Eq, Ord, Show)

noMods :: Mods
noMods = Mods False False False

-- | A key with its modifiers. A control character is 'KChar' of the
-- uppercase letter with 'modCtrl': Ctrl-A is @Ctrl \'A\'@.
data KeyEvent = KeyEvent KeyCode Mods
  deriving (Eq, Ord, Show)

pattern Plain :: KeyCode -> KeyEvent
pattern Plain k = KeyEvent k (Mods False False False)

pattern Ctrl, Alt, CtrlAlt :: Char -> KeyEvent
pattern Ctrl c = KeyEvent (KChar c) (Mods True False False)
pattern Alt c = KeyEvent (KChar c) (Mods False True False)
pattern CtrlAlt c = KeyEvent (KChar c) (Mods True True False)

pattern CtrlKey, AltKey :: KeyCode -> KeyEvent
pattern CtrlKey k = KeyEvent k (Mods True False False)
pattern AltKey k = KeyEvent k (Mods False True False)

-- | The key a character stands for, with extra modifiers.
keyEvent :: Char -> Mods -> KeyEvent
keyEvent c mods
  | not (isControl c) = KeyEvent (KChar c) (if mods == noMods then mods else mods {modShift = False})
  | otherwise = case c of
      '\b' -> KeyEvent KBackspace mods
      '\DEL' -> KeyEvent KBackspace mods
      '\t'
        | modShift mods -> KeyEvent KBackTab mods {modShift = False}
        | otherwise -> KeyEvent KTab mods
      '\r' -> KeyEvent KEnter mods
      '\ESC' -> KeyEvent KEsc mods
      '\x9b' -> KeyEvent KEsc mods {modShift = True}
      _
        | ord c < 0x20 -> KeyEvent (KChar (chr (ord c + 64))) mods {modCtrl = True}
        | otherwise -> KeyEvent KNull mods

-- | A character typed on its own, with Control, or with Alt.
key, ctrl, alt :: Char -> KeyEvent
key c = keyEvent c noMods
ctrl c = normalize (KeyEvent (KChar c) noMods {modCtrl = True})
alt c = keyEvent c noMods {modAlt = True}

-- | The form in which the decoder reports a key, so that a binding written
-- another way still matches.
normalize :: KeyEvent -> KeyEvent
normalize e@(KeyEvent code mods) = case code of
  KChar c
    | isControl c -> keyEvent c mods
    | isAsciiLower c && modCtrl mods -> KeyEvent (KChar (toUpper c)) mods
    | isAsciiUpper c && modShift mods -> KeyEvent code mods {modShift = False}
  KTab | modShift mods -> KeyEvent KBackTab mods {modShift = False}
  _ -> e

-- | Where characters come from.
data Source m = Source
  { -- | The next character, or 'Nothing' at end of input.
    next :: m (Maybe Char),
    -- | Whether more input arrives within so many milliseconds; 'Nothing'
    -- waits for as long as it takes.
    poll :: Maybe Int -> m Bool
  }

-- | Read one key, or 'Nothing' at end of input.
--
-- Escape is both a key and the start of every other key's sequence. It is
-- taken as a prefix if more input follows within the timeout. With no
-- timeout that means waiting for the next key, unless a lone Escape is
-- wanted, in which case only input already there counts.
readKey :: (Monad m) => Source m -> Maybe Int -> Bool -> m (Maybe KeyEvent)
readKey src timeout singleEscAbort = runMaybeT $ do
  c <- char
  if c /= '\ESC'
    then pure (key c)
    else do
      more <- lift (poll src (if singleEscAbort && timeout == Nothing then Just 0 else timeout))
      if more then escape True else pure (Plain KEsc)
  where
    char = MaybeT (next src)

    escape allowRecurse =
      char >>= \case
        '[' -> csi
        'O' -> ss3 <$> char
        -- Escape twice, then a sequence: Alt with that key.
        '\ESC'
          | allowRecurse -> do
              more <- lift (poll src (Just (fromMaybe 100 timeout)))
              if more then withAlt <$> escape False else pure (Plain KEsc)
          | otherwise -> pure (Plain KEsc)
        c -> pure (alt c)

    withAlt (KeyEvent k mods) = KeyEvent k mods {modAlt = True}

    -- A control sequence: parameters, then a final character.
    csi =
      char >>= \case
        '[' -> linuxConsole <$> char
        c -> collect [c]
    collect acc = case acc of
      c : params | c >= '@' && c <= '~' -> pure (decode (reverse params) c)
      _ -> char >>= collect . (: acc)

    linuxConsole c = maybe (Plain KUnknown) (Plain . KF) (lookup c (zip "ABCDE" [1 ..]))

    ss3 c = case c of
      'a' -> CtrlKey KUp
      'b' -> CtrlKey KDown
      'c' -> CtrlKey KRight
      'd' -> CtrlKey KLeft
      _ -> maybe (Plain KUnknown) Plain (lookup c (letters ++ zip "PQRS" (map KF [1 ..])))

    letters = [('A', KUp), ('B', KDown), ('C', KRight), ('D', KLeft), ('F', KEnd), ('H', KHome)]

    decode params final = fromMaybe (Plain KUnknown) $ case (splitParams params, final) of
      ([], 'Z') -> Just (Plain KBackTab)
      ([], c) | Just k <- lookup c (zip "abcd" [KUp, KDown, KRight, KLeft]) -> Just (KeyEvent k noMods {modShift = True})
      ([], c) -> Plain <$> lookup c letters
      ([1, m], c)
        | Just k <- lookup c (letters ++ zip "PQS" [KF 1, KF 2, KF 4]) -> KeyEvent k <$> modifier m
        | c >= 'p' && c <= 'y' -> KeyEvent (KChar (chr (ord c - ord 'p' + ord '0'))) <$> modifier m
      ([n], '~') -> Plain <$> tilde n
      ([n, m], '~') -> KeyEvent <$> tilde n <*> modifier m
      _ -> Nothing

    splitParams s = case break (== ';') s of
      (n, rest) | not (null n) && all isDigit n -> read n : splitParams (drop 1 rest)
      _ -> []

    tilde :: Int -> Maybe KeyCode
    tilde n = lookup n table
      where
        table =
          [(1, KHome), (7, KHome), (2, KInsert), (3, KDelete), (4, KEnd), (8, KEnd), (5, KPageUp), (6, KPageDown), (200, KPasteStart), (201, KPasteEnd)]
            ++ zip [11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 23, 24] (map KF [1 ..])

    modifier :: Int -> Maybe Mods
    modifier m
      | m >= 2 && m <= 8 = let b = m - 1 in Just (Mods (b `div` 4 `mod` 2 == 1) (b `div` 2 `mod` 2 == 1) (b `mod` 2 == 1))
      | otherwise = Nothing

-- | The text of a bracketed paste, read once its opening marker has been.
-- Line endings become line feeds.
readPaste :: (Monad m) => Source m -> m String
readPaste src = go ""
  where
    closing = reverse "\ESC[201~"
    go acc
      | closing `isPrefixOf` acc = pure (lineFeeds (reverse (drop (length closing) acc)))
      | otherwise = next src >>= maybe (pure (lineFeeds (reverse acc))) (go . (: acc))
    lineFeeds = \case
      '\r' : '\n' : cs -> '\n' : lineFeeds cs
      '\r' : cs -> '\n' : lineFeeds cs
      c : cs -> c : lineFeeds cs
      [] -> []
