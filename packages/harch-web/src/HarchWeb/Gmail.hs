{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Gmail
  ( GmailAccessTokenProvider,
    GmailApiConfig,
    GmailHttpRequest (..),
    GmailHttpResponse (..),
    GmailHttpRunner,
    deliverGmailApiEmailWithRunner,
    mkGmailApiConfig,
    runGmailHttpRequest,
  )
where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Email
  ( EmailAddress,
    EmailMessage,
    renderEmailMessage,
  )
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types qualified as HttpTypes

type GmailAccessTokenProvider = IO Text

data GmailApiConfig = GmailApiConfig
  { gmailSender :: EmailAddress,
    gmailAccessTokenProvider :: GmailAccessTokenProvider
  }

data GmailHttpRequest = GmailHttpRequest
  { gmailHttpMethod :: Text,
    gmailHttpUrl :: Text,
    gmailHttpHeaders :: [(Text, Text)],
    gmailHttpBody :: Text
  }

data GmailHttpResponse = GmailHttpResponse
  { gmailHttpStatus :: Int,
    gmailHttpResponseBody :: Text
  }

type GmailHttpRunner = GmailHttpRequest -> IO GmailHttpResponse

mkGmailApiConfig :: EmailAddress -> GmailAccessTokenProvider -> GmailApiConfig
mkGmailApiConfig = GmailApiConfig

-- | PR-SEC4: a Gmail response body is provider-controlled and may contain
-- recipient or account data, so failure diagnostics retain only HTTP status.
deliverGmailApiEmailWithRunner :: GmailHttpRunner -> GmailApiConfig -> EmailMessage -> IO ()
deliverGmailApiEmailWithRunner runRequest config message = do
  accessToken <- gmailAccessTokenProvider config
  unless (validAccessToken accessToken) $
    ioError (userError "Gmail API access-token provider returned an invalid token")
  response <- runRequest (gmailSendRequest config accessToken message)
  unless (gmailHttpStatus response >= 200 && gmailHttpStatus response < 300) $
    ioError (userError ("Gmail API send failed with status " <> show (gmailHttpStatus response)))

gmailSendRequest :: GmailApiConfig -> Text -> EmailMessage -> GmailHttpRequest
gmailSendRequest config accessToken message =
  GmailHttpRequest
    { gmailHttpMethod = "POST",
      gmailHttpUrl = "https://gmail.googleapis.com/gmail/v1/users/me/messages/send",
      gmailHttpHeaders =
        [ ("Authorization", "Bearer " <> accessToken),
          ("Content-Type", "application/json")
        ],
      gmailHttpBody = "{\"raw\":\"" <> base64Url (renderEmailMessage (gmailSender config) message) <> "\"}"
    }

-- | Takes an explicit 'HttpClient.Manager' rather than minting one per
-- request (which defeats TLS connection reuse) or reaching for a process-
-- global one: a caller constructs a manager once, e.g. via
-- 'HttpClientTls.newTlsManager', and passes it here for every request it
-- makes, matching @docs/design-guidance.md@'s explicit-props rule rather
-- than ambient application state.
runGmailHttpRequest :: HttpClient.Manager -> GmailHttpRunner
runGmailHttpRequest manager request = do
  initialRequest <- HttpClient.parseRequest (Text.unpack (gmailHttpUrl request))
  response <-
    HttpClient.httpLbs
      initialRequest
        { HttpClient.method = TextEncoding.encodeUtf8 (gmailHttpMethod request),
          HttpClient.requestHeaders = map (\(name, value) -> (CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 value)) (gmailHttpHeaders request),
          HttpClient.requestBody = HttpClient.RequestBodyBS (TextEncoding.encodeUtf8 (gmailHttpBody request))
        }
      manager
  pure
    GmailHttpResponse
      { gmailHttpStatus = HttpTypes.statusCode (HttpClient.responseStatus response),
        gmailHttpResponseBody = TextEncoding.decodeUtf8 (LazyByteString.toStrict (HttpClient.responseBody response))
      }

base64Url :: ByteString.ByteString -> Text
base64Url =
  TextEncoding.decodeUtf8
    . ByteString.dropWhileEnd (== 61)
    . ByteString.map replaceBase64Character
    . Base64.encode
  where
    replaceBase64Character character =
      case character of
        43 -> 45
        47 -> 95
        _ -> character

validAccessToken :: Text -> Bool
validAccessToken value =
  not (Text.null value) && Text.all (\character -> character /= '\r' && character /= '\n') value
