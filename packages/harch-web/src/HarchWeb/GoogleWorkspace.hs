{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.GoogleWorkspace
  ( GoogleWorkspaceServiceAccount,
    GoogleWorkspaceClock,
    GoogleWorkspaceTokenCache,
    decodeGoogleWorkspaceServiceAccount,
    gmailSendScope,
    mkGoogleWorkspaceAccessTokenProvider,
    newGoogleWorkspaceTokenCache,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (unless)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2osp)
import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.PubKey.RSA.Types qualified as RSA
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.PEM qualified as PEM
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import HarchWeb.Gmail
  ( GmailAccessTokenProvider,
    GmailHttpRequest (..),
    GmailHttpResponse (..),
    GmailHttpRunner,
  )

data GoogleWorkspaceServiceAccount = GoogleWorkspaceServiceAccount
  { serviceAccountClientEmail :: Text,
    serviceAccountPrivateKeyPem :: Text,
    serviceAccountImpersonatedEmail :: Text
  }

type GoogleWorkspaceClock = IO UTCTime

gmailSendScope :: Text
gmailSendScope = "https://www.googleapis.com/auth/gmail.send"

decodeGoogleWorkspaceServiceAccount :: Text -> Text -> Either Text GoogleWorkspaceServiceAccount
decodeGoogleWorkspaceServiceAccount encodedCredentials impersonatedEmail = do
  validImpersonatedEmail <- ensureNonEmpty "Google Workspace impersonated email must not be empty" impersonatedEmail
  decodedCredentials <- mapFailure "Google Workspace credentials must be base64-encoded JSON" (Base64.decode (TextEncoding.encodeUtf8 encodedCredentials))
  credentials <- mapFailure "Google Workspace credentials must contain client_email and private_key" (Aeson.eitherDecodeStrict decodedCredentials :: Either String Aeson.Value)
  (clientEmail, privateKeyPem) <- serviceAccountFields credentials
  validClientEmail <- ensureNonEmpty "Google Workspace credentials must contain a nonempty client_email" clientEmail
  validPrivateKeyPem <- ensureNonEmpty "Google Workspace credentials must contain a nonempty private_key" privateKeyPem
  pure
    GoogleWorkspaceServiceAccount
      { serviceAccountClientEmail = validClientEmail,
        serviceAccountPrivateKeyPem = validPrivateKeyPem,
        serviceAccountImpersonatedEmail = validImpersonatedEmail
      }

serviceAccountFields :: Aeson.Value -> Either Text (Text, Text)
serviceAccountFields value =
  case value of
    Aeson.Object object ->
      case (AesonTypes.parseMaybe (Aeson..: "client_email") object, AesonTypes.parseMaybe (Aeson..: "private_key") object) of
        (Just clientEmail, Just privateKeyPem) -> Right (clientEmail, privateKeyPem)
        _ -> Left "Google Workspace credentials must contain client_email and private_key"
    _ -> Left "Google Workspace credentials must contain client_email and private_key"

-- | Decision (BX, 2026-08-21, per @docs/design-guidance.md@'s
-- explicit-props rule): see @docs/design-guidance.md@'s
-- \"Follow-up decision — BX\" for why this is an explicit prop rather than
-- a global CAF matching 'HarchWeb.Observability.Otlp'\'s existing manager.
--
-- An opaque, explicitly-owned holder for the most recently minted access
-- token and the time it should be treated as expired. Passed as a prop to
-- 'mkGoogleWorkspaceAccessTokenProvider' rather than kept as global mutable
-- state, per @docs/design-guidance.md@'s explicit-props rule — a caller that
-- wants two independently-refreshing providers (or a test that wants a
-- fresh cache per case) allocates two.
newtype GoogleWorkspaceTokenCache = GoogleWorkspaceTokenCache (MVar (Maybe CachedAccessToken))

data CachedAccessToken = CachedAccessToken
  { cachedAccessTokenValue :: Text,
    -- | POSIX seconds after which this cached token must be re-minted.
    cachedAccessTokenExpiresAt :: Integer
  }

newGoogleWorkspaceTokenCache :: IO GoogleWorkspaceTokenCache
newGoogleWorkspaceTokenCache = GoogleWorkspaceTokenCache <$> newMVar Nothing

-- | A signed JWT is exchanged for an access token good for (typically) one
-- hour; re-minting one for every email costs an RSA-2048 signature and two
-- extra HTTPS round trips, and risks Google's token endpoint rate limits
-- under volume. Cached to @expires_in - 'accessTokenCacheMarginSeconds'@ so
-- a token already close to expiry is refreshed early rather than risking a
-- send that starts just before it lapses. `modifyMVar` serializes concurrent
-- callers onto one in-flight mint rather than each racing their own.
accessTokenCacheMarginSeconds :: Integer
accessTokenCacheMarginSeconds = 60

mkGoogleWorkspaceAccessTokenProvider :: GoogleWorkspaceTokenCache -> GmailHttpRunner -> GoogleWorkspaceClock -> GoogleWorkspaceServiceAccount -> GmailAccessTokenProvider
mkGoogleWorkspaceAccessTokenProvider (GoogleWorkspaceTokenCache cacheVar) runRequest currentTime serviceAccount =
  modifyMVar cacheVar $ \cached -> do
    now <- currentTime
    let nowSeconds = floor (utcTimeToPOSIXSeconds now) :: Integer
    case cached of
      Just existing | cachedAccessTokenExpiresAt existing > nowSeconds -> pure (cached, cachedAccessTokenValue existing)
      _ -> do
        fresh <- mintAccessToken runRequest now serviceAccount
        pure (Just fresh, cachedAccessTokenValue fresh)

mintAccessToken :: GmailHttpRunner -> UTCTime -> GoogleWorkspaceServiceAccount -> IO CachedAccessToken
mintAccessToken runRequest now serviceAccount = do
  assertion <- eitherToIoError (signedAssertion serviceAccount now)
  response <-
    runRequest
      GmailHttpRequest
        { gmailHttpMethod = "POST",
          gmailHttpUrl = "https://oauth2.googleapis.com/token",
          gmailHttpHeaders = [("Content-Type", "application/x-www-form-urlencoded")],
          gmailHttpBody = "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=" <> assertion
        }
  unless (gmailHttpStatus response >= 200 && gmailHttpStatus response < 300) $
    ioError (userError ("Google Workspace token exchange failed with status " <> show (gmailHttpStatus response)))
  (accessToken, expiresInSeconds) <- eitherToIoError (accessTokenFromResponse (gmailHttpResponseBody response))
  unless (validAccessToken accessToken) $
    ioError (userError "Google Workspace token exchange returned an invalid access token")
  pure
    CachedAccessToken
      { cachedAccessTokenValue = accessToken,
        cachedAccessTokenExpiresAt = floor (utcTimeToPOSIXSeconds now) + max 0 (expiresInSeconds - accessTokenCacheMarginSeconds)
      }

signedAssertion :: GoogleWorkspaceServiceAccount -> UTCTime -> Either Text Text
signedAssertion serviceAccount now = do
  let issuedAt = floor (utcTimeToPOSIXSeconds now) :: Integer
      header = base64Url "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"
      claims =
        base64Url
          ( LazyByteString.toStrict
              ( Aeson.encode
                  ( Aeson.object
                      [ "iss" Aeson..= serviceAccountClientEmail serviceAccount,
                        "scope" Aeson..= gmailSendScope,
                        "aud" Aeson..= ("https://oauth2.googleapis.com/token" :: Text),
                        "exp" Aeson..= (issuedAt + 3600),
                        "iat" Aeson..= issuedAt,
                        "sub" Aeson..= serviceAccountImpersonatedEmail serviceAccount
                      ]
                  )
              )
          )
      signingInput = header <> "." <> claims
  signature <- signRs256 (serviceAccountPrivateKeyPem serviceAccount) (TextEncoding.encodeUtf8 signingInput)
  pure (signingInput <> "." <> base64Url signature)

signRs256 :: Text -> ByteString -> Either Text ByteString
signRs256 pem signingInput = do
  privateKey <- rsaPrivateKeyFromPem pem
  forceRsaPrivateKey privateKey `seq`
    case RSA.sign Nothing (Just $! SHA256) privateKey signingInput of
      Left _ -> Left "Google Workspace private key could not sign the JWT"
      Right signature -> ByteString.length signature `seq` Right signature
{-# ANN signRs256 ("HLint: ignore Redundant $!" :: String) #-}

rsaPrivateKeyFromPem :: Text -> Either Text RSA.PrivateKey
rsaPrivateKeyFromPem pem = do
  parsedPem <- mapFailure "Google Workspace private key is not valid PEM" (PEM.pemParseBS (TextEncoding.encodeUtf8 pem))
  der <-
    case parsedPem of
      [privateKey] ->
        case PEM.pemName privateKey of
          "PRIVATE KEY" -> Right (PEM.pemContent privateKey)
          _ -> Left "Google Workspace private key must contain one PKCS#8 PRIVATE KEY block"
      _ -> Left "Google Workspace private key must contain one PKCS#8 PRIVATE KEY block"
  parsedKey <- maybeToEither "Google Workspace private key is not valid PKCS#8" (decodeDer der)
  pkcs1Der <-
    case parsedKey of
      [DerSequence [DerInteger 0, DerSequence [DerObjectIdentifier rsaEncryptionOid, DerNull], DerOctetString encodedKey]]
        | rsaEncryptionOid == rsaEncryptionObjectIdentifier -> Right encodedKey
      _ -> Left "Google Workspace private key must be an RSA PKCS#8 key"
  pkcs1 <- maybeToEither "Google Workspace private key is not valid RSA data" (decodeDer pkcs1Der)
  case pkcs1 of
    [DerSequence [DerInteger 0, DerInteger modulus, DerInteger publicExponent, DerInteger privateExponent, DerInteger primeOne, DerInteger primeTwo, DerInteger exponentOne, DerInteger exponentTwo, DerInteger coefficient]] ->
      modulus `seq`
        publicExponent `seq`
          privateExponent `seq`
            primeOne `seq`
              primeTwo `seq`
                exponentOne `seq`
                  exponentTwo `seq`
                    coefficient `seq`
                      Right
                        ( RSA.PrivateKey
                            (RSA.PublicKey (ByteString.length (i2osp modulus)) modulus publicExponent)
                            privateExponent
                            primeOne
                            primeTwo
                            exponentOne
                            exponentTwo
                            coefficient
                        )
    _ -> Left "Google Workspace private key must contain an RSA private key"

forceRsaPrivateKey :: RSA.PrivateKey -> ()
forceRsaPrivateKey privateKey =
  RSA.public_size (RSA.private_pub privateKey) `seq`
    RSA.public_n (RSA.private_pub privateKey) `seq`
      RSA.public_e (RSA.private_pub privateKey) `seq`
        RSA.private_d privateKey `seq`
          RSA.private_p privateKey `seq`
            RSA.private_q privateKey `seq`
              RSA.private_dP privateKey `seq`
                RSA.private_dQ privateKey `seq`
                  RSA.private_qinv privateKey `seq`
                    ()

rsaEncryptionObjectIdentifier :: ByteString
rsaEncryptionObjectIdentifier = "\x2a\x86\x48\x86\xf7\x0d\x01\x01\x01"

data DerValue
  = DerSequence [DerValue]
  | DerInteger Integer
  | DerObjectIdentifier ByteString
  | DerNull
  | DerOctetString ByteString

decodeDer :: ByteString -> Maybe [DerValue]
decodeDer value =
  if ByteString.null value
    then Nothing
    else parseDerValues value

parseDerValues :: ByteString -> Maybe [DerValue]
parseDerValues value =
  if ByteString.null value
    then Just []
    else do
      (derValue, remaining) <- parseDerValue value
      values <- parseDerValues remaining
      pure (derValue : values)

parseDerValue :: ByteString -> Maybe (DerValue, ByteString)
parseDerValue value = do
  (tag, afterTag) <- takeDerByte value
  (contentLength, afterLength) <- parseDerLength afterTag
  (content, remaining) <- takeDerBytes contentLength afterLength
  derValue <-
    case tag of
      2 -> Just (DerInteger (ByteString.foldl' appendDerByte 0 content))
      4 -> Just (DerOctetString content)
      5 -> Just DerNull
      6 -> Just (DerObjectIdentifier content)
      48 -> DerSequence <$> decodeDer content
      _ -> Nothing
  pure (derValue, remaining)

parseDerLength :: ByteString -> Maybe (Int, ByteString)
parseDerLength value = do
  (firstByte, remaining) <- takeDerByte value
  if firstByte < 128
    then Just (fromIntegral firstByte, remaining)
    else do
      (lengthBytes, afterLength) <- takeDerBytes (fromIntegral (firstByte - 128)) remaining
      pure (ByteString.foldl' appendDerLengthByte 0 lengthBytes, afterLength)

takeDerByte :: ByteString -> Maybe (Word8, ByteString)
takeDerByte = ByteString.uncons

takeDerBytes :: Int -> ByteString -> Maybe (ByteString, ByteString)
takeDerBytes contentLength value =
  if contentLength <= ByteString.length value
    then Just (ByteString.splitAt contentLength value)
    else Nothing

appendDerByte :: Integer -> Word8 -> Integer
appendDerByte total byte = total * 256 + fromIntegral byte

appendDerLengthByte :: Int -> Word8 -> Int
appendDerLengthByte total byte = total * 256 + fromIntegral byte

accessTokenFromResponse :: Text -> Either Text (Text, Integer)
accessTokenFromResponse response = do
  value <- mapFailure "Google Workspace token exchange returned invalid JSON" (Aeson.eitherDecodeStrict (TextEncoding.encodeUtf8 response) :: Either String Aeson.Value)
  case value of
    Aeson.Object object ->
      case AesonTypes.parseMaybe (Aeson..: "access_token") object of
        Nothing -> Left "Google Workspace token exchange response did not contain access_token"
        Just accessToken ->
          case AesonTypes.parseMaybe (Aeson..: "expires_in") object of
            Nothing -> Left "Google Workspace token exchange response did not contain expires_in"
            Just expiresIn -> Right (accessToken, expiresIn)
    _ -> Left "Google Workspace token exchange returned invalid JSON"

base64Url :: ByteString -> Text
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

ensureNonEmpty :: Text -> Text -> Either Text Text
ensureNonEmpty message value =
  case Text.uncons value of
    Nothing -> Left message
    Just _ -> Right value

mapFailure :: Text -> Either error value -> Either Text value
mapFailure message = either (const (Left message)) Right

maybeToEither :: Text -> Maybe value -> Either Text value
maybeToEither message = maybe (Left message) Right

eitherToIoError :: Either Text value -> IO value
eitherToIoError = either (ioError . userError . Text.unpack) pure

validAccessToken :: Text -> Bool
validAccessToken value =
  not (Text.null value) && Text.all (\character -> character /= '\r' && character /= '\n') value
