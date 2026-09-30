-- | Compile-time checked ICU message-template authoring. Its implementation
-- lives beside HarchWeb's established Template Haskell machinery so GHC's
-- compiler-process-only execution remains within the documented coverage
-- boundary; this module is the stable localization-facing import path.
module HarchWeb.Localization.Quasi
  ( message,
    validateMessageTemplate,
  )
where

import HarchWeb.Markup.Quasi (message, validateMessageTemplate)
