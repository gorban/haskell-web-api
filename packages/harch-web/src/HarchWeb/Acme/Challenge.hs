{-# LANGUAGE OverloadedStrings #-}

-- | Private HTTP-01 challenge state and response handling.
--
-- ACME protocol and certificate-management code use this module through the
-- framework facade. It deliberately owns both in-process challenges and the
-- temporary certbot webroots so request dispatch has one safe HTTP-01 path.
module HarchWeb.Acme.Challenge
  ( AcmeChallengeStore (..),
    ActiveAcmeChallenge (..),
    acmeChallengeResponseForRequest,
    acmeChallengeRoutePath,
    acmeHttp01ChallengeToken,
    matchesRuntimeAcmeChallenge,
    registerAcmeChallenges,
    registerCertbotAcmeChallengeWebroot,
    unregisterAcmeChallenges,
    unregisterCertbotAcmeChallengeWebroot,
    validAcmeHttp01ChallengeToken,
  )
where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Security
  ( RequestPolicyConfig,
    applyRequestPathPrefix,
    requestHostWithoutPort,
    requestPathPrefix,
    waiRequestPath,
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

data ActiveAcmeChallenge = ActiveAcmeChallenge
  { activeAcmeChallengeDomain :: Text,
    activeAcmeChallengeToken :: Text,
    activeAcmeChallengeResponse :: Text
  }

newtype AcmeChallengeStore = AcmeChallengeStore (MVar [ActiveAcmeChallenge])

acmeChallengeResponseForRequest :: RequestPolicyConfig -> AcmeChallengeStore -> Wai.Request -> IO (Maybe Wai.Response)
acmeChallengeResponseForRequest requestPolicyConfig (AcmeChallengeStore challengeStore) request = do
  challenges <- readMVar challengeStore
  case fmap
    ( Wai.responseLBS
        Http.ok200
        [("Content-Type", "text/plain; charset=utf-8")]
        . LazyByteString.fromStrict
        . TextEncoding.encodeUtf8
        . activeAcmeChallengeResponse
    )
    (find (matchesRuntimeAcmeChallenge requestPolicyConfig request) challenges) of
    Just challengeResponse ->
      pure (Just challengeResponse)
    Nothing ->
      certbotAcmeChallengeResponseForRequest requestPolicyConfig request

matchesRuntimeAcmeChallenge :: RequestPolicyConfig -> Wai.Request -> ActiveAcmeChallenge -> Bool
matchesRuntimeAcmeChallenge requestPolicyConfig request challenge =
  case acmeHttp01ChallengeToken requestPolicyConfig request of
    Just challengeToken ->
      challengeToken == activeAcmeChallengeToken challenge
        && maybe True (== activeAcmeChallengeDomain challenge) (requestHostWithoutPort request)
    Nothing -> False

acmeHttp01ChallengeToken :: RequestPolicyConfig -> Wai.Request -> Maybe Text
acmeHttp01ChallengeToken requestPolicyConfig request =
  Text.stripPrefix "/.well-known/acme-challenge/" (waiRequestPath requestPolicyConfig request)

acmeChallengeRoutePath :: RequestPolicyConfig -> Wai.Request -> Text
acmeChallengeRoutePath requestPolicyConfig request =
  applyRequestPathPrefix
    (requestPathPrefix requestPolicyConfig request)
    "/.well-known/acme-challenge/*"

registerAcmeChallenges :: AcmeChallengeStore -> [ActiveAcmeChallenge] -> IO ()
registerAcmeChallenges (AcmeChallengeStore challengeStore) newChallenges =
  modifyMVar_ challengeStore (pure . (newChallenges <>))

unregisterAcmeChallenges :: AcmeChallengeStore -> [ActiveAcmeChallenge] -> IO ()
unregisterAcmeChallenges (AcmeChallengeStore challengeStore) completedChallenges =
  modifyMVar_ challengeStore (pure . filter (not . (`sameActiveAcmeChallengeAny` completedChallenges)))

{-# NOINLINE certbotAcmeChallengeWebrootDirectories #-}
certbotAcmeChallengeWebrootDirectories :: MVar [FilePath]
certbotAcmeChallengeWebrootDirectories =
  unsafePerformIO (newMVar [])

registerCertbotAcmeChallengeWebroot :: FilePath -> IO ()
registerCertbotAcmeChallengeWebroot webrootDirectory =
  modifyMVar_ certbotAcmeChallengeWebrootDirectories (pure . (webrootDirectory :))

unregisterCertbotAcmeChallengeWebroot :: FilePath -> IO ()
unregisterCertbotAcmeChallengeWebroot webrootDirectory =
  modifyMVar_ certbotAcmeChallengeWebrootDirectories (pure . filter (/= webrootDirectory))

certbotAcmeChallengeResponseForRequest :: RequestPolicyConfig -> Wai.Request -> IO (Maybe Wai.Response)
certbotAcmeChallengeResponseForRequest requestPolicyConfig request =
  case acmeHttp01ChallengeToken requestPolicyConfig request >>= validAcmeHttp01ChallengeToken of
    Nothing ->
      pure Nothing
    Just challengeToken -> do
      webrootDirectories <- readMVar certbotAcmeChallengeWebrootDirectories
      maybeChallengeFile <-
        firstExistingFile
          [ webrootDirectory </> ".well-known" </> "acme-challenge" </> Text.unpack challengeToken
          | webrootDirectory <- webrootDirectories
          ]
      pure
        ( fmap
            (\challengeFile -> Wai.responseFile Http.ok200 [("Content-Type", "text/plain; charset=utf-8")] challengeFile Nothing)
            maybeChallengeFile
        )

validAcmeHttp01ChallengeToken :: Text -> Maybe Text
validAcmeHttp01ChallengeToken challengeToken
  | Text.null challengeToken = Nothing
  | Text.any (\character -> character == '/' || character == '\\') challengeToken = Nothing
  | challengeToken == "." || challengeToken == ".." = Nothing
  | Text.isInfixOf ".." challengeToken = Nothing
validAcmeHttp01ChallengeToken challengeToken = Just challengeToken

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
firstExistingFile candidatePaths =
  case candidatePaths of
    [] ->
      pure Nothing
    candidatePath : remainingPaths -> do
      candidateExists <- doesFileExist candidatePath
      if candidateExists
        then pure (Just candidatePath)
        else firstExistingFile remainingPaths

sameActiveAcmeChallengeAny :: ActiveAcmeChallenge -> [ActiveAcmeChallenge] -> Bool
sameActiveAcmeChallengeAny candidate =
  any (sameActiveAcmeChallenge candidate)

sameActiveAcmeChallenge :: ActiveAcmeChallenge -> ActiveAcmeChallenge -> Bool
sameActiveAcmeChallenge left right =
  activeAcmeChallengeDomain left == activeAcmeChallengeDomain right
    && activeAcmeChallengeToken left == activeAcmeChallengeToken right
    && activeAcmeChallengeResponse left == activeAcmeChallengeResponse right
