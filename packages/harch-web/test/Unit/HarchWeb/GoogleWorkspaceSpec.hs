{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (SomeException, displayException, try)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.PubKey.RSA qualified as RSA
import Crypto.PubKey.RSA.PKCS15 qualified as RSAPKCS15
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (Sequence))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (fromLeft, fromRight)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.PEM qualified as PEM
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import HarchWeb.Gmail (GmailHttpRequest (..), GmailHttpResponse (..))
import HarchWeb.GoogleWorkspace

spec = do
  describe "decodeGoogleWorkspaceServiceAccount" $ do
    it "validates encoded credentials without exposing their contents" $ do
      errorText (decodeGoogleWorkspaceServiceAccount "not-base64" "worker@example.test")
        `shouldBe` "Google Workspace credentials must be base64-encoded JSON"
      errorText (decodeGoogleWorkspaceServiceAccount (encodeBase64 "not json") "worker@example.test")
        `shouldBe` "Google Workspace credentials must contain client_email and private_key"
      errorText (decodeGoogleWorkspaceServiceAccount (encodeBase64 "{}") "worker@example.test")
        `shouldBe` "Google Workspace credentials must contain client_email and private_key"
      errorText (decodeGoogleWorkspaceServiceAccount (encodeBase64 "[]") "worker@example.test")
        `shouldBe` "Google Workspace credentials must contain client_email and private_key"
      errorText (decodeGoogleWorkspaceServiceAccount (encodeCredentials "" "key") "worker@example.test")
        `shouldBe` "Google Workspace credentials must contain a nonempty client_email"
      errorText (decodeGoogleWorkspaceServiceAccount (encodeCredentials "service@example.test" "") "worker@example.test")
        `shouldBe` "Google Workspace credentials must contain a nonempty private_key"
      errorText (decodeGoogleWorkspaceServiceAccount (encodeCredentials "service@example.test" "key") "")
        `shouldBe` "Google Workspace impersonated email must not be empty"

  describe "mkGoogleWorkspaceAccessTokenProvider" $ do
    it "exchanges a signed domain-wide-delegation JWT for an access token" $ do
      (encodedCredentials, publicKey) <- generatedCredentials
      let serviceAccount = requiredEither (decodeGoogleWorkspaceServiceAccount encodedCredentials "mailer@example.test")
      cache <- newGoogleWorkspaceTokenCache
      receivedRequest <- newIORef Nothing
      let runner request = writeIORef receivedRequest (Just request) >> pure (GmailHttpResponse 200 "{\"access_token\":\"delegated-token\",\"expires_in\":3600}")
      mkGoogleWorkspaceAccessTokenProvider cache runner fixedClock serviceAccount `shouldReturn` "delegated-token"
      request <- required <$> readIORef receivedRequest
      gmailHttpMethod request `shouldBe` "POST"
      gmailHttpUrl request `shouldBe` "https://oauth2.googleapis.com/token"
      gmailHttpHeaders request `shouldBe` [("Content-Type", "application/x-www-form-urlencoded")]
      let assertion = Text.drop (Text.length jwtGrantPrefix) (gmailHttpBody request)
          (header, claims, signature) = jwtParts assertion
      decodeBase64Url header `shouldBe` "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"
      decodeBase64Url claims `shouldSatisfy` Text.isInfixOf "\"iss\":\"service@example.test\""
      decodeBase64Url claims `shouldSatisfy` Text.isInfixOf "\"scope\":\"https://www.googleapis.com/auth/gmail.send\""
      decodeBase64Url claims `shouldSatisfy` Text.isInfixOf "\"sub\":\"mailer@example.test\""
      RSAPKCS15.verify (Just SHA256) publicKey (TextEncoding.encodeUtf8 (header <> "." <> claims)) (decodeBase64UrlBytes signature) `shouldBe` True

    it "surfaces token failures without exposing the signed assertion" $ do
      (encodedCredentials, _) <- generatedCredentials
      let serviceAccount = requiredEither (decodeGoogleWorkspaceServiceAccount encodedCredentials "mailer@example.test")
      cache <- newGoogleWorkspaceTokenCache
      let runner _ = pure (GmailHttpResponse 403 "domain-wide delegation is missing")
      result <- try (mkGoogleWorkspaceAccessTokenProvider cache runner fixedClock serviceAccount) :: IO (Either SomeException Text)
      case result of
        Left failure -> do
          displayException failure `shouldContain` "Google Workspace token exchange failed with status 403"
          displayException failure `shouldNotContain` "BEGIN PRIVATE KEY"
        Right _ -> expectationFailure "Expected token exchange to fail"

    it "mints a token once and reuses it until it is close to expiring, then re-mints" $ do
      (encodedCredentials, _) <- generatedCredentials
      let serviceAccount = requiredEither (decodeGoogleWorkspaceServiceAccount encodedCredentials "mailer@example.test")
      cache <- newGoogleWorkspaceTokenCache
      mintCount <- newIORef (0 :: Int)
      clockRef <- newIORef (UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 0))
      let stepClock = readIORef clockRef
          runner _ = do
            modifyIORef' mintCount (+ 1)
            count <- readIORef mintCount
            pure (GmailHttpResponse 200 ("{\"access_token\":\"token-" <> Text.pack (show count) <> "\",\"expires_in\":3600}"))
          provider = mkGoogleWorkspaceAccessTokenProvider cache runner stepClock serviceAccount
      provider `shouldReturn` "token-1"
      writeIORef clockRef (UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 10))
      provider `shouldReturn` "token-1"
      readIORef mintCount `shouldReturn` 1
      -- Past expires_in (3600s) minus the 60s safety margin: must re-mint.
      writeIORef clockRef (UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 3541))
      provider `shouldReturn` "token-2"
      readIORef mintCount `shouldReturn` 2

    it "rejects failed token statuses before accepting their response bodies" $ do
      (encodedCredentials, _) <- generatedCredentials
      let serviceAccount = requiredEither (decodeGoogleWorkspaceServiceAccount encodedCredentials "mailer@example.test")
      tooEarly <- tryStatus serviceAccount 199
      rejected <- tryStatus serviceAccount 403
      map displayException [tooEarly, rejected]
        `shouldBe` [ "user error (Google Workspace token exchange failed with status 199)",
                     "user error (Google Workspace token exchange failed with status 403)"
                   ]

    it "rejects malformed and unsafe token responses" $ do
      (encodedCredentials, _) <- generatedCredentials
      let serviceAccount = requiredEither (decodeGoogleWorkspaceServiceAccount encodedCredentials "mailer@example.test")
      malformed <- tryProvider serviceAccount "not json"
      missing <- tryProvider serviceAccount "{}"
      nonObject <- tryProvider serviceAccount "[]"
      missingExpiry <- tryProvider serviceAccount "{\"access_token\":\"only-token\"}"
      unsafe <- tryProvider serviceAccount "{\"access_token\":\"bad\\ntoken\",\"expires_in\":3600}"
      map displayException [malformed, missing, nonObject, missingExpiry, unsafe]
        `shouldBe` [ "user error (Google Workspace token exchange returned invalid JSON)",
                     "user error (Google Workspace token exchange response did not contain access_token)",
                     "user error (Google Workspace token exchange returned invalid JSON)",
                     "user error (Google Workspace token exchange response did not contain expires_in)",
                     "user error (Google Workspace token exchange returned an invalid access token)"
                   ]

    it "rejects malformed PEM and RSA key encodings before sending a token request" $ do
      let malformedPem = "-----BEGIN PRIVATE KEY-----\n%%%\n-----END PRIVATE KEY-----\n"
          emptyPem = "\n"
          wrongPemLabel = pemText "RSA PRIVATE KEY" ""
          emptyPkcs8 = pemText "PRIVATE KEY" ""
          -- A zero-length DER BIT STRING (its content must start with an
          -- "unused bits" byte, so empty content is itself malformed) crashes
          -- asn1-encoding's decoder with an uncaught partial-function error
          -- instead of returning a clean parse failure — confirmed directly,
          -- not assumed. Regression for that crash being caught and turned
          -- into the same "Google Workspace ..." domain error every other
          -- rejection here already surfaces; see eitherToIoError's Haddock.
          crashingBitStringPkcs8 = pemText "PRIVATE KEY" "\x03\&\x00"
          incompleteTag = pemText "PRIVATE KEY" "\x30"
          incompleteContent = pemText "PRIVATE KEY" "\x30\x01"
          nonRsaPkcs8 = pemText "PRIVATE KEY" (encodeASN1' DER [Null])
          nonRsaOid = pemText "PRIVATE KEY" (pkcs8PrivateKeyWithOid [1, 2, 3] "")
          invalidRsaData = pemText "PRIVATE KEY" (pkcs8PrivateKey "not DER")
          malformedRsa = pemText "PRIVATE KEY" (pkcs8PrivateKey (encodeASN1' DER [Null]))
          unsignableRsa = pemText "PRIVATE KEY" (pkcs8PrivateKey (rsaPrivateKeyDer 1 1 1 1 1 1 1 1))
      failures <- mapM tryInvalidPrivateKey [malformedPem, emptyPem, wrongPemLabel, emptyPkcs8, crashingBitStringPkcs8, incompleteTag, incompleteContent, nonRsaPkcs8, nonRsaOid, invalidRsaData, malformedRsa, unsignableRsa]
      map displayException failures
        `shouldBe` [ "user error (Google Workspace private key is not valid PEM)",
                     "user error (Google Workspace private key must contain one PKCS#8 PRIVATE KEY block)",
                     "user error (Google Workspace private key must contain one PKCS#8 PRIVATE KEY block)",
                     "user error (Google Workspace private key must be an RSA PKCS#8 key)",
                     "user error (Google Workspace request could not be prepared: Data.ByteString.head: empty ByteString)",
                     "user error (Google Workspace private key is not valid PKCS#8)",
                     "user error (Google Workspace private key is not valid PKCS#8)",
                     "user error (Google Workspace private key must be an RSA PKCS#8 key)",
                     "user error (Google Workspace private key must be an RSA PKCS#8 key)",
                     "user error (Google Workspace private key is not valid RSA data)",
                     "user error (Google Workspace private key must contain an RSA private key)",
                     "user error (Google Workspace private key could not sign the JWT)"
                   ]

jwtGrantPrefix :: Text
jwtGrantPrefix = "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion="

fixedClock :: GoogleWorkspaceClock
fixedClock = pure (UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 3))

tryProvider :: GoogleWorkspaceServiceAccount -> Text -> IO SomeException
tryProvider serviceAccount body = do
  cache <- newGoogleWorkspaceTokenCache
  result <- try (mkGoogleWorkspaceAccessTokenProvider cache (const (pure (GmailHttpResponse 200 body))) fixedClock serviceAccount) :: IO (Either SomeException Text)
  case result of
    Left failure -> pure failure
    Right _ -> expectationFailure "Expected token exchange to fail" >> error "unreachable"

tryStatus :: GoogleWorkspaceServiceAccount -> Int -> IO SomeException
tryStatus serviceAccount status = do
  cache <- newGoogleWorkspaceTokenCache
  result <- try (mkGoogleWorkspaceAccessTokenProvider cache (const (pure (GmailHttpResponse status "response body is not exposed"))) fixedClock serviceAccount) :: IO (Either SomeException Text)
  case result of
    Left failure -> pure failure
    Right _ -> expectationFailure "Expected token exchange to fail" >> error "unreachable"

tryInvalidPrivateKey :: Text -> IO SomeException
tryInvalidPrivateKey privateKeyPem = do
  let serviceAccount = requiredEither (decodeGoogleWorkspaceServiceAccount (encodeCredentials "service@example.test" privateKeyPem) "mailer@example.test")
  tryProvider serviceAccount "{\"access_token\":\"not-reached\"}"

generatedCredentials :: IO (Text, RSA.PublicKey)
generatedCredentials = do
  (publicKey, privateKey) <- RSA.generate 1024 65537
  let RSA.PrivateKey (RSA.PublicKey _ modulus publicExponent) privateExponent primeOne primeTwo exponentOne exponentTwo coefficient = privateKey
      pkcs1 = rsaPrivateKeyDer modulus publicExponent privateExponent primeOne primeTwo exponentOne exponentTwo coefficient
      pkcs8 = pkcs8PrivateKey pkcs1
      pem = pemText "PRIVATE KEY" pkcs8
  pure (encodeCredentials "service@example.test" pem, publicKey)

encodeCredentials :: Text -> Text -> Text
encodeCredentials clientEmail privateKey =
  encodeBase64
    ( LazyByteString.toStrict
        (Aeson.encode (Aeson.object ["client_email" Aeson..= clientEmail, "private_key" Aeson..= privateKey]))
    )

pkcs8PrivateKey :: ByteString.ByteString -> ByteString.ByteString
pkcs8PrivateKey = pkcs8PrivateKeyWithOid [1, 2, 840, 113549, 1, 1, 1]

pkcs8PrivateKeyWithOid :: [Integer] -> ByteString.ByteString -> ByteString.ByteString
pkcs8PrivateKeyWithOid objectIdentifier pkcs1 =
  encodeASN1'
    DER
    [ Start Sequence,
      IntVal 0,
      Start Sequence,
      OID objectIdentifier,
      Null,
      End Sequence,
      OctetString pkcs1,
      End Sequence
    ]

rsaPrivateKeyDer :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ByteString.ByteString
rsaPrivateKeyDer modulus publicExponent privateExponent primeOne primeTwo exponentOne exponentTwo coefficient =
  encodeASN1'
    DER
    [ Start Sequence,
      IntVal 0,
      IntVal modulus,
      IntVal publicExponent,
      IntVal privateExponent,
      IntVal primeOne,
      IntVal primeTwo,
      IntVal exponentOne,
      IntVal exponentTwo,
      IntVal coefficient,
      End Sequence
    ]

pemText :: String -> ByteString.ByteString -> Text
pemText name content = TextEncoding.decodeUtf8 (PEM.pemWriteBS (PEM.PEM name [] content))

encodeBase64 :: ByteString.ByteString -> Text
encodeBase64 = TextEncoding.decodeUtf8 . Base64.encode

decodeBase64Url :: Text -> Text
decodeBase64Url = TextEncoding.decodeUtf8 . decodeBase64UrlBytes

decodeBase64UrlBytes :: Text -> ByteString.ByteString
decodeBase64UrlBytes value =
  requiredEither
    ( Base64.decode
        ( TextEncoding.encodeUtf8
            ( Text.map restoreBase64Character value
                <> Text.replicate ((4 - Text.length value `mod` 4) `mod` 4) "="
            )
        )
    )
  where
    restoreBase64Character character =
      case character of
        '-' -> '+'
        '_' -> '/'
        _ -> character

jwtParts :: Text -> (Text, Text, Text)
jwtParts assertion =
  case Text.splitOn "." assertion of
    [header, claims, signature] -> (header, claims, signature)
    _ -> error "Expected a three-part JWT"

required :: Maybe value -> value
required = fromMaybe (error "Expected a valid Google Workspace test value")

requiredEither :: Either error value -> value
requiredEither = fromRight (error "Expected a valid Google Workspace test value")

errorText :: Either Text value -> Text
errorText = fromLeft "Expected credential validation to fail"
