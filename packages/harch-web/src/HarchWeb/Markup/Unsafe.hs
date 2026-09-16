module HarchWeb.Markup.Unsafe
  ( TrustedHtml,
    unsafeTrustHtml,
  )
where

import Data.Text (Text)
import HarchWeb.Markup.Internal (TrustedHtml (..))

unsafeTrustHtml :: Text -> TrustedHtml
unsafeTrustHtml = TrustedHtml
