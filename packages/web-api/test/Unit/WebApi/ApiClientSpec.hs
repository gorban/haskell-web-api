{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Authentication (OAuth2Scope, mkOAuth2Scope, oauth2ScopeText)
import HarchWeb.Password (PasswordHash (..), passwordHashText)
import WebApi.ApiClient

spec =
  describe "WebApi.ApiClient" $ do
    it "keeps API clients separate, bounded, and scope-subset constrained" $ do
      let identifier = requiredClientId "automation-client"
          sharedRead = requiredScope "resource:read"
          selfRead = requiredScope "profile:read:self"
          configured = mkApiClient identifier (PasswordHash "configured-secret" :| [PasswordHash "rotating-secret"]) [sharedRead, selfRead] [sharedRead]
          configuredClient = requiredClient configured
          establishedClient = establishApiClient configuredClient
          durableEstablishedClient = requiredEstablishedClient (mkEstablishedApiClient identifier [sharedRead, selfRead])
          scopeTexts = fmap oauth2ScopeText
      expectAll
        ( (apiClientIdText identifier `shouldBe` "automation-client")
            :| [ apiClientIdText (apiClientId configuredClient) `shouldBe` "automation-client",
                 scopeTexts (apiClientAllowedScopes configuredClient) `shouldBe` ["resource:read", "profile:read:self"],
                 scopeTexts (apiClientDefaultScopes configuredClient) `shouldBe` ["resource:read"],
                 (passwordHashText <$> apiClientSecretHashes configuredClient) `shouldBe` ("configured-secret" :| ["rotating-secret"]),
                 apiClientIdText (establishedApiClientId establishedClient) `shouldBe` "automation-client",
                 scopeTexts (establishedApiClientAllowedScopes establishedClient) `shouldBe` ["resource:read", "profile:read:self"],
                 apiClientIdText (establishedApiClientId durableEstablishedClient) `shouldBe` "automation-client",
                 scopeTexts (establishedApiClientAllowedScopes durableEstablishedClient) `shouldBe` ["resource:read", "profile:read:self"],
                 scopeTexts (intersectEstablishedApiClientScopes establishedClient [selfRead, requiredScope "other:scope"]) `shouldBe` ["profile:read:self"],
                 selectedScopeTextsAre ["resource:read"] (selectApiClientScopes configuredClient []) `shouldBe` True,
                 selectedScopeTextsAre ["profile:read:self"] (selectApiClientScopes configuredClient [selfRead]) `shouldBe` True,
                 scopeErrorIs ApiClientRequestedScopeDuplicate (selectApiClientScopes configuredClient [sharedRead, sharedRead]) `shouldBe` True,
                 scopeErrorIs ApiClientRequestedScopeNotAllowed (selectApiClientScopes configuredClient [requiredScope "other:scope"]) `shouldBe` True,
                 clientIdErrorIs ApiClientIdEmpty (mkApiClientId "") `shouldBe` True,
                 clientIdErrorIs ApiClientIdTooLong (mkApiClientId (mconcat (replicate 129 "a"))) `shouldBe` True,
                 clientIdErrorIs ApiClientIdInvalidCharacter (mkApiClientId "bad client") `shouldBe` True,
                 configurationErrorIs ApiClientAllowedScopesEmpty (mkApiClient identifier (PasswordHash "configured-secret" :| []) [] []) `shouldBe` True,
                 configurationErrorIs ApiClientAllowedScopesDuplicate (mkApiClient identifier (PasswordHash "configured-secret" :| []) [sharedRead, sharedRead] []) `shouldBe` True,
                 configurationErrorIs ApiClientDefaultScopesDuplicate (mkApiClient identifier (PasswordHash "configured-secret" :| []) [sharedRead] [sharedRead, sharedRead]) `shouldBe` True,
                 configurationErrorIs ApiClientDefaultScopeNotAllowed (mkApiClient identifier (PasswordHash "configured-secret" :| []) [sharedRead] [selfRead]) `shouldBe` True,
                 establishedConfigurationErrorIs ApiClientAllowedScopesEmpty (mkEstablishedApiClient identifier []) `shouldBe` True,
                 establishedConfigurationErrorIs ApiClientAllowedScopesDuplicate (mkEstablishedApiClient identifier [sharedRead, sharedRead]) `shouldBe` True
               ]
        )

requiredClientId :: Text -> ApiClientId
requiredClientId value =
  case mkApiClientId value of
    Right clientId -> clientId
    Left _ -> error "expected valid API client id"

requiredScope :: Text -> OAuth2Scope
requiredScope value =
  case mkOAuth2Scope value of
    Right scope -> scope
    Left invalidScope -> error ("expected valid OAuth scope: " <> show invalidScope)

requiredClient :: Either ApiClientConfigurationError ApiClient -> ApiClient
requiredClient result =
  case result of
    Right client -> client
    Left _ -> error "expected valid API client"

requiredEstablishedClient :: Either ApiClientConfigurationError EstablishedApiClient -> EstablishedApiClient
requiredEstablishedClient result =
  case result of
    Right client -> client
    Left _ -> error "expected valid established API client"

configurationErrorIs :: ApiClientConfigurationError -> Either ApiClientConfigurationError ApiClient -> Bool
configurationErrorIs expected actual =
  case (expected, actual) of
    (ApiClientAllowedScopesEmpty, Left ApiClientAllowedScopesEmpty) -> True
    (ApiClientAllowedScopesDuplicate, Left ApiClientAllowedScopesDuplicate) -> True
    (ApiClientDefaultScopesDuplicate, Left ApiClientDefaultScopesDuplicate) -> True
    (ApiClientDefaultScopeNotAllowed, Left ApiClientDefaultScopeNotAllowed) -> True
    _ -> False

establishedConfigurationErrorIs :: ApiClientConfigurationError -> Either ApiClientConfigurationError EstablishedApiClient -> Bool
establishedConfigurationErrorIs expected actual =
  case (expected, actual) of
    (ApiClientAllowedScopesEmpty, Left ApiClientAllowedScopesEmpty) -> True
    (ApiClientAllowedScopesDuplicate, Left ApiClientAllowedScopesDuplicate) -> True
    _ -> False

clientIdErrorIs :: ApiClientIdError -> Either ApiClientIdError ApiClientId -> Bool
clientIdErrorIs expected actual =
  case (expected, actual) of
    (ApiClientIdEmpty, Left ApiClientIdEmpty) -> True
    (ApiClientIdTooLong, Left ApiClientIdTooLong) -> True
    (ApiClientIdInvalidCharacter, Left ApiClientIdInvalidCharacter) -> True
    _ -> False

scopeErrorIs :: ApiClientScopeError -> Either ApiClientScopeError [OAuth2Scope] -> Bool
scopeErrorIs expected actual =
  case (expected, actual) of
    (ApiClientRequestedScopeDuplicate, Left ApiClientRequestedScopeDuplicate) -> True
    (ApiClientRequestedScopeNotAllowed, Left ApiClientRequestedScopeNotAllowed) -> True
    _ -> False

selectedScopeTextsAre :: [Text] -> Either ApiClientScopeError [OAuth2Scope] -> Bool
selectedScopeTextsAre expected result =
  case result of
    Right scopes -> (oauth2ScopeText <$> scopes) == expected
    Left _ -> False
