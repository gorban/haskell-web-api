-- | Bounded declarations for an action response that needs to remove
-- application-owned Web Storage state.
--
-- This deliberately models only a named key in its explicit storage class.
-- It is not a credential store, a namespace policy, or an escape hatch for
-- arbitrary browser JavaScript.  In particular, it cannot express an
-- origin-wide @clear()@, IndexedDB deletion, or service-worker mutation.
--
-- Decision record (AHI-4C, 2026-09-09): this extends the typed client-action
-- response boundary with an opaque, bounded declaration rather than adding an
-- application JavaScript callback or a login-specific storage convention.
-- Applications own the keys they created and select a later action effect;
-- Harch checks only universal safety properties (an explicit class, a
-- non-empty key, and bounded declaration size).  The initial primitive is not
-- yet attached to any response, so existing examples make no claim that
-- browser storage is erased on logout.  A following AHI-4C slice owns action
-- transport, failure navigation, and browser proof.
module HarchWeb.ClientStorage
  ( BrowserStorageClass (..),
    BrowserStorageKey,
    BrowserStorageKeyError (..),
    browserStorageKey,
    browserStorageKeyClass,
    browserStorageKeyText,
    ClientStorageCleanup,
    ClientStorageCleanupError (..),
    clientStorageCleanup,
    clientStorageCleanupEntries,
    noClientStorageCleanup,
  )
where

import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text

-- | The Web Storage area an application explicitly owns a key in.  The two
-- constructors remain distinct even when their key text is the same.
data BrowserStorageClass
  = LocalStorage
  | SessionStorage
  deriving (Eq, Ord, Show)

-- | An application-authored browser-storage key with a validated explicit
-- class.  Its constructor is private so request-derived text cannot become a
-- cleanup target without passing the bound check.
data BrowserStorageKey = BrowserStorageKey BrowserStorageClass Text
  deriving (Eq, Ord, Show)

-- | A rejected authored key exposes no key text: callers can report a stable
-- configuration error without turning a storage name into diagnostics.
data BrowserStorageKeyError
  = EmptyBrowserStorageKey
  | BrowserStorageKeyTooLong
  deriving (Eq, Show)

-- | Build a key declaration.  The 256-character bound matches the framework's
-- other authored identifier bounds and prevents a response declaration from
-- becoming an unbounded client payload.
browserStorageKey :: BrowserStorageClass -> Text -> Either BrowserStorageKeyError BrowserStorageKey
browserStorageKey storageClass keyText
  | Text.null keyText = Left EmptyBrowserStorageKey
  | Text.length keyText > maximumBrowserStorageKeyLength = Left BrowserStorageKeyTooLong
  | otherwise = Right (BrowserStorageKey storageClass keyText)

browserStorageKeyClass :: BrowserStorageKey -> BrowserStorageClass
browserStorageKeyClass (BrowserStorageKey storageClass _) = storageClass

browserStorageKeyText :: BrowserStorageKey -> Text
browserStorageKeyText (BrowserStorageKey _ keyText) = keyText

-- | A bounded, duplicate-free collection of application-owned keys.  Entries
-- from the two browser stores stay distinct, so an identically named local and
-- session key can intentionally be removed together.
newtype ClientStorageCleanup = ClientStorageCleanup [BrowserStorageKey]
  deriving (Eq, Show)

data ClientStorageCleanupError
  = TooManyBrowserStorageKeys
  | DuplicateBrowserStorageKey
  deriving (Eq, Show)

-- | Validate one action's cleanup declaration.  A duplicate has no useful
-- semantic meaning and would make failure aggregation needlessly ambiguous.
clientStorageCleanup :: [BrowserStorageKey] -> Either ClientStorageCleanupError ClientStorageCleanup
clientStorageCleanup entries
  | length entries > maximumBrowserStorageCleanupEntries = Left TooManyBrowserStorageKeys
  | length (nub entries) /= length entries = Left DuplicateBrowserStorageKey
  | otherwise = Right (ClientStorageCleanup entries)

clientStorageCleanupEntries :: ClientStorageCleanup -> [BrowserStorageKey]
clientStorageCleanupEntries (ClientStorageCleanup entries) = entries

-- | The safe default: an action does not claim ownership of browser storage.
noClientStorageCleanup :: ClientStorageCleanup
noClientStorageCleanup = ClientStorageCleanup []

maximumBrowserStorageKeyLength :: Int
maximumBrowserStorageKeyLength = 256

maximumBrowserStorageCleanupEntries :: Int
maximumBrowserStorageCleanupEntries = 32
