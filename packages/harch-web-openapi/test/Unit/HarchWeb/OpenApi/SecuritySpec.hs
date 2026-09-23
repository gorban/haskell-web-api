{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.OpenApi

spec =
  describe "OpenAPI security schemes" $ do
    it "rejects an empty cookie session name" $
      mkOpenApiCookieSessionSecurityScheme "" `shouldBeLeft` EmptyOpenApiCookieSessionName

    it "rejects a non-https OAuth2 token URL" $
      mkOpenApiOAuth2ClientCredentialsSecurityScheme "http://api.example.test/oauth/token" [] `shouldBeLeft` InvalidOpenApiOAuth2TokenUrl "http://api.example.test/oauth/token"

    it "rejects a relative OAuth2 token URL" $
      mkOpenApiOAuth2ClientCredentialsSecurityScheme "/oauth/token" [] `shouldBeLeft` InvalidOpenApiOAuth2TokenUrl "/oauth/token"

    it "rejects a malformed OAuth2 token URL" $
      mkOpenApiOAuth2ClientCredentialsSecurityScheme "not a url" [] `shouldBeLeft` InvalidOpenApiOAuth2TokenUrl "not a url"

    it "compares and shows every security-scheme construction error, including cross-constructor order and lists" $ do
      let sameError = EmptyOpenApiCookieSessionName
          otherError = InvalidOpenApiOAuth2TokenUrl "http://insecure.example.test"
          otherErrorAgain = InvalidOpenApiOAuth2TokenUrl "http://insecure.example.test"
          differentUrlError = InvalidOpenApiOAuth2TokenUrl "http://another.example.test"
      expectAll
        ( ((EmptyOpenApiCookieSessionName == sameError) `shouldBe` True)
            :| [ (EmptyOpenApiCookieSessionName /= sameError) `shouldBe` False,
                 (otherError == otherErrorAgain) `shouldBe` True,
                 (otherError /= differentUrlError) `shouldBe` True,
                 (EmptyOpenApiCookieSessionName == otherError) `shouldBe` False,
                 (otherError == EmptyOpenApiCookieSessionName) `shouldBe` False,
                 (EmptyOpenApiCookieSessionName /= otherError) `shouldBe` True,
                 show EmptyOpenApiCookieSessionName `shouldBe` "EmptyOpenApiCookieSessionName",
                 show otherError `shouldBe` "InvalidOpenApiOAuth2TokenUrl \"http://insecure.example.test\"",
                 show [EmptyOpenApiCookieSessionName, otherError] `shouldBe` "[EmptyOpenApiCookieSessionName,InvalidOpenApiOAuth2TokenUrl \"http://insecure.example.test\"]"
               ]
        )

    it "compares and shows every security scheme, including cross-constructor order and lists" $ do
      cookieScheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-harch-session")
      anotherCookieScheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-harch-session")
      differentCookieScheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-other-session")
      oauthScheme <- requireRight (mkOpenApiOAuth2ClientCredentialsSecurityScheme "https://api.example.test/oauth/token" [("catalog:read", "Read the catalog")])
      expectAll
        ( ((cookieScheme == anotherCookieScheme) `shouldBe` True)
            :| [ (cookieScheme /= anotherCookieScheme) `shouldBe` False,
                 (cookieScheme /= differentCookieScheme) `shouldBe` True,
                 (cookieScheme == oauthScheme) `shouldBe` False,
                 (oauthScheme == cookieScheme) `shouldBe` False,
                 show cookieScheme `shouldBe` "OpenApiCookieSessionSecurityScheme \"__Host-harch-session\"",
                 show oauthScheme `shouldBe` "OpenApiOAuth2ClientCredentialsSecurityScheme \"https://api.example.test/oauth/token\" [(\"catalog:read\",\"Read the catalog\")]",
                 show [cookieScheme, oauthScheme] `shouldBe` "[OpenApiCookieSessionSecurityScheme \"__Host-harch-session\",OpenApiOAuth2ClientCredentialsSecurityScheme \"https://api.example.test/oauth/token\" [(\"catalog:read\",\"Read the catalog\")]]"
               ]
        )

requireRight :: Either errorValue value -> IO value
requireRight result =
  case result of
    Left _ -> expectationFailure "expected Right" >> fail "expected Right"
    Right value -> pure value

shouldBeLeft :: (Eq errorValue, Show errorValue, Show value) => Either errorValue value -> errorValue -> Expectation
shouldBeLeft result expectedError =
  case result of
    Left actualError -> actualError `shouldBe` expectedError
    Right value -> expectationFailure ("expected Left, got Right " <> show value)
