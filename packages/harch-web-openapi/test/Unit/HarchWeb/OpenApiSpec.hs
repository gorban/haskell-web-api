{-# SPEC #-}

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LazyByteString
import HarchWeb.OpenApi (OpenApi)

spec =
  describe "HarchWeb.OpenApi" $ do
    it "encodes the public OpenAPI model selected by the optional package" $ do
      encode (mempty :: OpenApi) `shouldSatisfy` (not . LazyByteString.null)
