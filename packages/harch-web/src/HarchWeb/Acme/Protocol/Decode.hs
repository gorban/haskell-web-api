{-# LANGUAGE OverloadedStrings #-}

-- | Private decoding of ACME directory and order responses.
module HarchWeb.Acme.Protocol.Decode
  ( parseAcmeAuthorizationResponse,
    parseAcmeChallengeResponse,
    parseAcmeDirectoryResponse,
    parseAcmeOrderIdentifier,
    parseAcmeOrderResponse,
  )
where

import HarchWeb.Acme.Json
  ( JsonValue,
    jsonArrayItems,
    jsonObjectFields,
    jsonOptionalTextArrayField,
    jsonOptionalTextField,
    jsonRequiredField,
    jsonRequiredTextField,
  )
import HarchWeb.Acme.Protocol.Types
  ( AcmeAuthorizationResponse (..),
    AcmeChallengeResponse (..),
    AcmeDirectoryResponse (..),
    AcmeOrderIdentifier (..),
    AcmeOrderResponse (..),
  )

parseAcmeDirectoryResponse :: JsonValue -> Either String AcmeDirectoryResponse
parseAcmeDirectoryResponse value = do
  fields <- jsonObjectFields "AcmeDirectoryResponse" value
  AcmeDirectoryResponse
    <$> jsonRequiredTextField "newNonce" fields
    <*> jsonRequiredTextField "newAccount" fields
    <*> jsonRequiredTextField "newOrder" fields

parseAcmeOrderIdentifier :: JsonValue -> Either String AcmeOrderIdentifier
parseAcmeOrderIdentifier value = do
  fields <- jsonObjectFields "AcmeOrderIdentifier" value
  AcmeOrderIdentifier
    <$> jsonRequiredTextField "type" fields
    <*> jsonRequiredTextField "value" fields

parseAcmeChallengeResponse :: JsonValue -> Either String AcmeChallengeResponse
parseAcmeChallengeResponse value = do
  fields <- jsonObjectFields "AcmeChallengeResponse" value
  AcmeChallengeResponse
    <$> jsonRequiredTextField "type" fields
    <*> jsonRequiredTextField "url" fields
    <*> jsonRequiredTextField "token" fields

parseAcmeAuthorizationResponse :: JsonValue -> Either String AcmeAuthorizationResponse
parseAcmeAuthorizationResponse value = do
  fields <- jsonObjectFields "AcmeAuthorizationResponse" value
  AcmeAuthorizationResponse
    <$> (jsonRequiredField "identifier" fields >>= parseAcmeOrderIdentifier)
    <*> (jsonRequiredField "challenges" fields >>= jsonArrayItems "challenges" >>= traverse parseAcmeChallengeResponse)

parseAcmeOrderResponse :: JsonValue -> Either String AcmeOrderResponse
parseAcmeOrderResponse value = do
  fields <- jsonObjectFields "AcmeOrderResponse" value
  AcmeOrderResponse
    <$> jsonRequiredTextField "status" fields
    <*> jsonOptionalTextArrayField "authorizations" fields
    <*> jsonOptionalTextField "finalize" fields
    <*> jsonOptionalTextField "certificate" fields
