{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.ByteString qualified as ByteString
import Data.Maybe (fromMaybe)
import HarchWeb.Account (mkAccountId)
import HarchWeb.Api (ApiResponse (..), MissingContentTypePolicy (RejectMissingContentType))
import HarchWeb.Authentication (encodedJwtFromBytes)
import HarchWeb.Email (mkEmailAddress)
import HarchWeb.Username (mkUsername)
import Network.HTTP.Types qualified as HttpTypes
import WebApi.Account (AccountProfile (..), AccountStoreError (AccountStoreUnavailable))
import WebApi.Api.Endpoints (MeApiFailure (..), TokenApiFailure (..), meApiFailureResponse, meApiOutcomeResponse, requiredApiHeaderNameOrDie, requiredApiHeaderValueOrDie, tokenApiFailureResponse, tokenApiMissingContentTypePolicy, tokenApiOutcomeResponse)
import WebApi.ApiClientToken (ApiClientTokenOutcome (..))
import WebApi.Profile (ProfileLoadError (..), ProfileState (..))

spec =
  describe "WebApi.Api.Endpoints" $ do
    it "reports the token endpoint unavailable rather than emitting an unencodable access token" $
      case tokenApiOutcomeResponse (ApiClientTokenIssued (encodedJwtFromBytes (ByteString.singleton 255)) [] 900) of
        Left TokenApiUnavailable -> pure ()
        Left _ -> expectationFailure "expected the generic unavailable outcome for an unencodable signed token"
        Right _ -> expectationFailure "expected malformed EncodedJwt bytes to be rejected, not rendered as a token"

    it "collapses every non-protocol issuance failure into the same generic unavailable outcome" $
      mapM_
        ( \case
            Left TokenApiUnavailable -> pure ()
            Left _ -> expectationFailure "expected the generic unavailable outcome"
            Right _ -> expectationFailure "expected an unavailable outcome, not a token"
        )
        [ tokenApiOutcomeResponse ApiClientTokenStoreUnavailable,
          tokenApiOutcomeResponse ApiClientTokenWorkBudgetExhausted,
          tokenApiOutcomeResponse ApiClientTokenIssueFailed
        ]

    it "renders the generic unavailable outcome as a safe 503 body" $ do
      let response = tokenApiFailureResponse TokenApiUnavailable
      apiEndpointResponseStatus response `shouldBe` HttpTypes.status503
      apiEndpointResponseValue response `shouldBe` "{\"error\":\"token-issuance-unavailable\"}"

    it "declares the RFC 6749 form-body content-type policy exactly once" $
      tokenApiMissingContentTypePolicy `shouldBe` RejectMissingContentType

    it "fails immediately when a program-owned header literal is invalid" $ do
      requiredApiHeaderNameOrDie "WWW-Authenticate" `shouldBe` requiredApiHeaderNameOrDie "WWW-Authenticate"
      requiredApiHeaderValueOrDie "Basic" `shouldBe` requiredApiHeaderValueOrDie "Basic"
      -- 'isInfixOf' on only the literal prefix would never force the
      -- appended invalid value, leaving that concatenation genuinely
      -- untested; comparing the exact message forces the whole thunk.
      evaluate (requiredApiHeaderNameOrDie "" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "invalid API header name literal: "
      evaluate (requiredApiHeaderValueOrDie "not a valid header value\n" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "invalid API header value literal: not a valid header value\n"

    it "reports the account-self resource unavailable for a durable-store failure or a lookup that disagrees with an already-established session" $
      mapM_
        ( \outcome -> case meApiOutcomeResponse outcome of
            Left MeApiUnavailable -> pure ()
            Right _ -> expectationFailure "expected the generic unavailable outcome, not a rendered profile"
        )
        [ Left (ProfileAccountStoreError (AccountStoreUnavailable "test")),
          Right ProfileUnauthenticated
        ]

    it "renders the account-self unavailable outcome as a safe 503 body" $ do
      let response = meApiFailureResponse MeApiUnavailable
      apiEndpointResponseStatus response `shouldBe` HttpTypes.status503
      apiEndpointResponseValue response `shouldBe` "{\"error\":\"profile-unavailable\"}"

    it "renders an authenticated (email-verified) account's own username and email, and a pending account's the same way" $
      mapM_
        ( \profileState -> case meApiOutcomeResponse (Right profileState) of
            Right response -> apiEndpointResponseValue response `shouldBe` "{\"username\":\"account-holder\",\"email\":\"me-spec@example.test\"}"
            Left _ -> expectationFailure "expected a rendered profile, not the generic unavailable outcome"
        )
        [ProfileAuthenticated testAccountProfile, ProfilePending testAccountProfile]

testAccountProfile :: AccountProfile
testAccountProfile =
  AccountProfile
    { accountProfileId = required "account id" (mkAccountId "account_01"),
      accountProfileEmail = required "email address" (mkEmailAddress "me-spec@example.test"),
      accountProfileUsername = Just (required "username" (mkUsername "account-holder")),
      accountProfileDisplayName = Nothing,
      accountProfileEmailVerified = True
    }

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected a valid test " <> label))
