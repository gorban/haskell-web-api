{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON byte-level encoding helpers backing
-- "HarchWeb.Observability.Otlp.Wire"'s OTLP JSON export. Public only so this
-- package's own test suite can exercise it directly; nothing outside
-- "HarchWeb.Observability.Otlp.Wire" is expected to depend on it, and it is
-- deliberately not re-exported through the "HarchWeb.Acme" facade.
--
-- This module predates OTLP export: it began as JSON support (both encoding
-- and a hand-rolled parser into a 'JsonValue' tree) for ACME's native
-- protocol client, which was removed for having zero production callers
-- (see the DG decision record in @docs/design-guidance.md@). The parsing
-- half had no other caller either — OTLP export only ever used these three
-- byte-builder encoders — so it was not carried forward (see the DH decision
-- record in @docs/design-guidance.md@).
module HarchWeb.Acme.Json
  ( jsonArrayBytes,
    jsonObjectBytes,
    jsonStringBytes,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)

jsonStringBytes :: Text -> LazyByteString.ByteString
jsonStringBytes = Aeson.encode . Aeson.String

jsonArrayBytes :: [LazyByteString.ByteString] -> LazyByteString.ByteString
jsonArrayBytes items =
  "[" <> LazyByteString.intercalate "," items <> "]"

jsonObjectBytes :: [(Text, LazyByteString.ByteString)] -> LazyByteString.ByteString
jsonObjectBytes fields =
  "{"
    <> LazyByteString.intercalate
      ","
      [ jsonStringBytes fieldName <> ":" <> fieldValue
      | (fieldName, fieldValue) <- fields
      ]
    <> "}"
