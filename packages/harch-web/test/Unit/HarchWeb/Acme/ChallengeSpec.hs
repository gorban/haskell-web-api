{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# SPEC #-}

import Control.Concurrent (newMVar, readMVar)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

defaultRequestPolicy :: RequestPolicyConfig
defaultRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      httpsRedirectAuthority = Nothing,
      strictTransportSecurity = Nothing,
      forwardedHeaderTrust = NeverTrustForwarded,
      requestHeadLimits = unboundedRequestHeadLimits,
      requestTransportLimits = warpDefaultRequestTransportLimits,
      requestConcurrencyLimit = Nothing,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }

spec =
  describe "ACME challenge matching and store helpers" $ do
    it "covers derived Eq and Show instances for internal ACME helper types" $ do
      let challenge =
            ActiveAcmeChallenge
              { activeAcmeChallengeDomain = "example.com",
                activeAcmeChallengeToken = "token",
                activeAcmeChallengeResponse = "token.thumbprint"
              }
      expectAll
        ( (challenge `shouldBe` challenge)
            :| [show challenge `shouldContain` "activeAcmeChallengeDomain = \"example.com\""]
        )

    it "covers challenge matching and store update helpers" $ do
      challengeStore <- AcmeChallengeStore <$> newMVar []
      webrootStore <- newCertbotWebrootStore
      let challenge =
            ActiveAcmeChallenge
              { activeAcmeChallengeDomain = "example.com",
                activeAcmeChallengeToken = "token-1",
                activeAcmeChallengeResponse = "response-1"
              }
          matchingRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token-1",
                Wai.requestHeaders = [("Host", "example.com:80")]
              }
          mismatchedHostRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token-1",
                Wai.requestHeaders = [("Host", "other.example.com")]
              }
          missingTokenRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/missing",
                Wai.requestHeaders = [("Host", "example.com")]
              }
          hostlessRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token-1"
              }
      acmeHttp01ChallengeToken defaultRequestPolicy matchingRequest `shouldBe` Just "token-1"
      acmeHttp01ChallengeToken defaultRequestPolicy missingTokenRequest `shouldBe` Nothing
      requestHostWithoutPort matchingRequest `shouldBe` Just "example.com"
      matchesRuntimeAcmeChallenge defaultRequestPolicy matchingRequest challenge `shouldBe` True
      matchesRuntimeAcmeChallenge defaultRequestPolicy mismatchedHostRequest challenge `shouldBe` False
      matchesRuntimeAcmeChallenge defaultRequestPolicy hostlessRequest challenge `shouldBe` True
      matchesRuntimeAcmeChallenge defaultRequestPolicy missingTokenRequest challenge `shouldBe` False
      registerAcmeChallenges challengeStore [challenge]
      registeredChallenges <- unwrapChallengeStore challengeStore
      registeredChallenges `shouldBe` [challenge]
      challengeResponse <- acmeChallengeResponseForRequest defaultRequestPolicy challengeStore webrootStore matchingRequest
      case challengeResponse of
        Just response -> do
          Wai.responseStatus response `shouldBe` Http.ok200
          Wai.responseHeaders response `shouldBe` [("Content-Type", "text/plain; charset=utf-8")]
          readResponseBody response `shouldReturn` "response-1"
        Nothing -> expectationFailure "expected a registered ACME challenge response"
      unregisterAcmeChallenges challengeStore [challenge]
      unwrapChallengeStore challengeStore `shouldReturn` []
      registerCertbotAcmeChallengeWebroot webrootStore "/tmp/webroot-a"
      registerCertbotAcmeChallengeWebroot webrootStore "/tmp/webroot-b"
      unwrapWebrootStore webrootStore `shouldReturn` ["/tmp/webroot-b", "/tmp/webroot-a"]
      unregisterCertbotAcmeChallengeWebroot webrootStore "/tmp/webroot-a"
      unwrapWebrootStore webrootStore `shouldReturn` ["/tmp/webroot-b"]

instance Eq ActiveAcmeChallenge where
  left == right =
    activeAcmeChallengeDomain left == activeAcmeChallengeDomain right
      && activeAcmeChallengeToken left == activeAcmeChallengeToken right
      && activeAcmeChallengeResponse left == activeAcmeChallengeResponse right

instance Show ActiveAcmeChallenge where
  show challenge =
    "ActiveAcmeChallenge {activeAcmeChallengeDomain = "
      <> show (activeAcmeChallengeDomain challenge)
      <> ", activeAcmeChallengeToken = "
      <> show (activeAcmeChallengeToken challenge)
      <> ", activeAcmeChallengeResponse = "
      <> show (activeAcmeChallengeResponse challenge)
      <> "}"

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> atomicModifyIORef' chunksReference (\chunks -> (chunks <> [Builder.toLazyByteString builder], ())))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)))

unwrapChallengeStore :: AcmeChallengeStore -> IO [ActiveAcmeChallenge]
unwrapChallengeStore (AcmeChallengeStore challengeStore) =
  readMVar challengeStore

unwrapWebrootStore :: CertbotWebrootStore -> IO [FilePath]
unwrapWebrootStore (CertbotWebrootStore webrootStore) =
  readMVar webrootStore
