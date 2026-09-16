{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Authentication (OAuth2Scope, mkOAuth2Scope, oauth2ScopeText)
import HarchWeb.Password (PasswordHash (..))
import WebApi.ApiClient

spec =
  describe "WebApi.ApiClient" $ do
    it "keeps API clients separate, bounded, and scope-subset constrained" $ do
      let identifier = requiredClientId "automation-client"
          sharedRead = requiredScope "resource:read"
          selfRead = requiredScope "profile:read:self"
          configured = mkApiClient identifier (PasswordHash "configured-secret" :| [PasswordHash "rotating-secret"]) [sharedRead, selfRead] [sharedRead]
          configuredClient = requiredClient configured
          scopeTexts = fmap oauth2ScopeText
      expectAll
        ( (apiClientIdText identifier `shouldBe` "automation-client")
            :| [ apiClientIdText (apiClientId configuredClient) `shouldBe` "automation-client",
                 scopeTexts (apiClientAllowedScopes configuredClient) `shouldBe` ["resource:read", "profile:read:self"],
                 scopeTexts (apiClientDefaultScopes configuredClient) `shouldBe` ["resource:read"],
                 scopeTexts <$> selectApiClientScopes configuredClient [] `shouldBe` Right ["resource:read"],
                 scopeTexts <$> selectApiClientScopes configuredClient [selfRead] `shouldBe` Right ["profile:read:self"],
                 scopeError (selectApiClientScopes configuredClient [sharedRead, sharedRead]) `shouldBe` Just ApiClientRequestedScopeDuplicate,
                 scopeError (selectApiClientScopes configuredClient [requiredScope "other:scope"]) `shouldBe` Just ApiClientRequestedScopeNotAllowed,
                 clientIdError (mkApiClientId "") `shouldBe` Just ApiClientIdEmpty,
                 clientIdError (mkApiClientId (mconcat (replicate 129 "a"))) `shouldBe` Just ApiClientIdTooLong,
                 clientIdError (mkApiClientId "bad client") `shouldBe` Just ApiClientIdInvalidCharacter,
                 configurationError (mkApiClient identifier (PasswordHash "configured-secret" :| []) [] []) `shouldBe` Just ApiClientAllowedScopesEmpty,
                 configurationError (mkApiClient identifier (PasswordHash "configured-secret" :| []) [sharedRead, sharedRead] []) `shouldBe` Just ApiClientAllowedScopesDuplicate,
                 configurationError (mkApiClient identifier (PasswordHash "configured-secret" :| []) [sharedRead] [sharedRead, sharedRead]) `shouldBe` Just ApiClientDefaultScopesDuplicate,
                 configurationError (mkApiClient identifier (PasswordHash "configured-secret" :| []) [sharedRead] [selfRead]) `shouldBe` Just ApiClientDefaultScopeNotAllowed
               ]
        )

requiredClientId :: Text -> ApiClientId
requiredClientId value =
  case mkApiClientId value of
    Right clientId -> clientId
    Left identifierError -> error ("expected valid API client id: " <> show identifierError)

requiredScope :: Text -> OAuth2Scope
requiredScope value =
  case mkOAuth2Scope value of
    Right scope -> scope
    Left invalidScope -> error ("expected valid OAuth scope: " <> show invalidScope)

requiredClient :: Either ApiClientConfigurationError ApiClient -> ApiClient
requiredClient result =
  case result of
    Right client -> client
    Left invalidConfiguration -> error ("expected valid API client: " <> show invalidConfiguration)

configurationError :: Either ApiClientConfigurationError ApiClient -> Maybe ApiClientConfigurationError
configurationError result =
  case result of
    Left errorValue -> Just errorValue
    Right _ -> Nothing

clientIdError :: Either ApiClientIdError ApiClientId -> Maybe ApiClientIdError
clientIdError result =
  case result of
    Left errorValue -> Just errorValue
    Right _ -> Nothing

scopeError :: Either ApiClientScopeError [OAuth2Scope] -> Maybe ApiClientScopeError
scopeError result =
  case result of
    Left errorValue -> Just errorValue
    Right _ -> Nothing
