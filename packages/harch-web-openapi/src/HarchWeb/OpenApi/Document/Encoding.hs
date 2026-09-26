{-# LANGUAGE OverloadedStrings #-}

-- | The narrow raw-JSON adapters for 'HarchWeb.OpenApi.Document'
-- (AHI-4E-MH): @openapi3@'s typed model can neither retain validated @x-*@
-- operation members nor emit an explicit empty @security@ array, so encoding
-- applies both to the encoded value after the typed model is final. Each
-- adapter updates only its named path/method pairs and leaves a missing path,
-- non-object path item, or non-object operation unchanged, so an
-- application-transformed model whose selected operation has been removed
-- before encoding stays safe.
--
-- Split from 'HarchWeb.OpenApi.Document' by ownership (mirroring
-- 'HarchWeb.Api.Endpoint.Internal'/'Family'/'Runtime'): the facade keeps the
-- public document types and assembly, 'HarchWeb.OpenApi.Document.Operation'
-- keeps operation construction, and this module keeps the wire adapters.
module HarchWeb.OpenApi.Document.Encoding
  ( applyOpenApiOperationExtensions,
    applyOpenApiAnonymousSecurity,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api (ApiMethod, apiMethodText)
import HarchWeb.OpenApi.Metadata
  ( OpenApiSpecificationExtension,
    openApiSpecificationExtensionName,
    openApiSpecificationExtensionValue,
  )

-- | Apply validated @x-*@ operation members to a raw encoded OpenAPI value.
-- This is the narrow wire adapter required because @openapi3@ has no typed
-- representation for specification extensions.  It updates the OpenAPI
-- version to 3.0.3 only for an object value, and leaves a missing path,
-- non-object path item, or non-object operation unchanged.  Those cases make
-- the adapter safe for an application-transformed model whose selected
-- operation has been removed before encoding.
applyOpenApiOperationExtensions :: [(Text, ApiMethod, [OpenApiSpecificationExtension])] -> Value -> Value
applyOpenApiOperationExtensions extensions value =
  case value of
    Object root -> Object (KeyMap.insert "openapi" (String "3.0.3") (applyToPaths extensions root))
    _ -> value

-- | Force an explicit empty @security@ array onto every anonymous
-- operation. @openapi3@'s generic encoder treats an empty list as that
-- field's default value and omits it entirely from the encoded JSON (its
-- @AesonDefaultValue [a]@ instance), which would otherwise leave an
-- anonymous operation with no @security@ member at all — silently inheriting
-- any top-level security declaration instead of explicitly requiring none.
-- This mirrors 'applyOpenApiOperationExtensions': the same narrow,
-- already-established raw-JSON adapter for a typed-model gap, not a second
-- encoding path. It updates only the named path/method pairs and leaves a
-- missing path, non-object path item, or non-object operation unchanged.
applyOpenApiAnonymousSecurity :: [(Text, ApiMethod)] -> Value -> Value
applyOpenApiAnonymousSecurity anonymousOperations value =
  case value of
    Object root -> Object (applyAnonymousToPaths anonymousOperations root)
    _ -> value

applyAnonymousToPaths :: [(Text, ApiMethod)] -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
applyAnonymousToPaths anonymousOperations root =
  case KeyMap.lookup "paths" root of
    Just (Object paths) -> KeyMap.insert "paths" (Object (foldr applyOne paths anonymousOperations)) root
    _ -> root
  where
    applyOne (anonymousPath, anonymousMethod) =
      mapKey (applyAnonymousToPathItem anonymousMethod) (Key.fromText anonymousPath)

applyAnonymousToPathItem :: ApiMethod -> Value -> Value
applyAnonymousToPathItem method value =
  case value of
    Object pathItem -> Object (mapKey applyAnonymousToOperation (Key.fromText (Text.toLower (apiMethodText method))) pathItem)
    _ -> value

applyAnonymousToOperation :: Value -> Value
applyAnonymousToOperation value =
  case value of
    Object operation -> Object (KeyMap.insert "security" (Array mempty) operation)
    _ -> value

applyToPaths :: [(Text, ApiMethod, [OpenApiSpecificationExtension])] -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
applyToPaths extensions root =
  case KeyMap.lookup "paths" root of
    Just (Object paths) -> KeyMap.insert "paths" (Object (foldr applyOne paths extensions)) root
    _ -> root
  where
    applyOne (extensionPath, extensionMethod, extensionValues) =
      mapKey (applyToPathItem extensionMethod extensionValues) (Key.fromText extensionPath)

applyToPathItem :: ApiMethod -> [OpenApiSpecificationExtension] -> Value -> Value
applyToPathItem method extensions value =
  case value of
    Object pathItem -> Object (mapKey (applyToOperation extensions) (Key.fromText (Text.toLower (apiMethodText method))) pathItem)
    _ -> value

mapKey :: (Value -> Value) -> Key.Key -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
mapKey transform key values =
  case KeyMap.lookup key values of
    Nothing -> values
    Just value -> KeyMap.insert key (transform value) values

applyToOperation :: [OpenApiSpecificationExtension] -> Value -> Value
applyToOperation extensions value =
  case value of
    Object operation ->
      Object
        ( foldr
            (\extension -> KeyMap.insert (Key.fromText (openApiSpecificationExtensionName extension)) (openApiSpecificationExtensionValue extension))
            operation
            extensions
        )
    _ -> value
