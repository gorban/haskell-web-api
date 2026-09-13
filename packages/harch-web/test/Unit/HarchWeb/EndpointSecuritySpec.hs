{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import HarchWeb
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Unit.HarchWeb.TestSupport (TestContext (requestLanguage), TestRoute (DataRoute), defaultContext)

data EndpointSecurityPolicy = ReadData
  deriving (Eq, Show)

spec = do
  describe "endpoint metadata" $ do
    it "accepts bounded declared names and templates but rejects request-like values" $
      expectAll
        ( (mkEndpointName "orders.order-detail" `shouldBe` Right (requiredEndpointName "orders.order-detail"))
            :| [ mkEndpointName "" `shouldBe` Left EmptyEndpointName,
                 mkEndpointName (Text.replicate 129 "a") `shouldBe` Left EndpointNameTooLong,
                 mkEndpointName "orders/42" `shouldBe` Left InvalidEndpointName,
                 mkRouteTemplate "/{locale}/orders/{orderId}" `shouldBe` Right (requiredRouteTemplate "/{locale}/orders/{orderId}"),
                 mkRouteTemplate "" `shouldBe` Left EmptyRouteTemplate,
                 mkRouteTemplate ("/" <> Text.replicate 256 "a") `shouldBe` Left RouteTemplateTooLong,
                 mkRouteTemplate "/orders?customer=42" `shouldBe` Left InvalidRouteTemplate,
                 mkRouteTemplate "orders" `shouldBe` Left InvalidRouteTemplate,
                 mkAuthenticationProfileName "" `shouldBe` Left EmptyAuthenticationProfileName,
                 mkAuthenticationProfileName (Text.replicate 129 "a") `shouldBe` Left AuthenticationProfileNameTooLong,
                 mkAuthenticationProfileName "api/v1" `shouldBe` Left InvalidAuthenticationProfileName
               ]
        )

    it "fails immediately when a program-owned declaration literal is invalid" $ do
      requiredEndpointNameOrDie "orders.order-detail" `shouldBe` requiredEndpointName "orders.order-detail"
      requiredRouteTemplateOrDie "/{locale}/orders/{orderId}" `shouldBe` requiredRouteTemplate "/{locale}/orders/{orderId}"
      evaluate (requiredEndpointNameOrDie "orders/42" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid endpoint name" `isInfixOf` message && "InvalidEndpointName" `isInfixOf` message
      evaluate (requiredRouteTemplateOrDie "/orders?customer=42" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid route template" `isInfixOf` message && "InvalidRouteTemplate" `isInfixOf` message
      evaluate (requiredAuthenticationProfileNameOrDie "api/v1" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid authentication profile name" `isInfixOf` message && "InvalidAuthenticationProfileName" `isInfixOf` message

    it "keeps declared metadata typed and inspectable without request values" $ do
      let endpointNameValue = requiredEndpointName "orders.order-detail"
          routeTemplateValue = requiredRouteTemplate "/{locale}/orders/{orderId}"
          otherEndpointName = requiredEndpointName "orders.order-list"
          otherRouteTemplate = requiredRouteTemplate "/{locale}/orders"
          profileNameValue = requiredAuthenticationProfileNameOrDie "api"
          otherProfileName = requiredAuthenticationProfileNameOrDie "webhook"
          metadata :: EndpointMetadata EndpointSecurityPolicy
          metadata = mkEndpointMetadata endpointNameValue routeTemplateValue HtmlEndpoint AllowUnauthenticated
          otherMetadata = mkEndpointMetadata otherEndpointName otherRouteTemplate ApiEndpoint RequireAuthenticated
          protocols = [HtmlEndpoint, ApiEndpoint, ActionEndpoint, AssetEndpoint]
          requirements = [AllowUnauthenticated, RequireAuthenticated, RequireAuthorized ReadData]
          errors = [EmptyEndpointName, EndpointNameTooLong, InvalidEndpointName, EmptyRouteTemplate, RouteTemplateTooLong, InvalidRouteTemplate]
      expectAll
        ( (endpointNameText endpointNameValue `shouldBe` "orders.order-detail")
            :| [ routeTemplateText routeTemplateValue `shouldBe` "/{locale}/orders/{orderId}",
                 endpointName metadata `shouldBe` endpointNameValue,
                 endpointRouteTemplate metadata `shouldBe` routeTemplateValue,
                 endpointProtocol metadata `shouldBe` HtmlEndpoint,
                 endpointAccess metadata `shouldBe` AllowUnauthenticated,
                 endpointNameValue /= otherEndpointName `shouldBe` True,
                 compare endpointNameValue endpointNameValue `shouldBe` EQ,
                 compare endpointNameValue otherEndpointName `shouldBe` LT,
                 endpointNameValue < otherEndpointName `shouldBe` True,
                 endpointNameValue <= endpointNameValue `shouldBe` True,
                 otherEndpointName > endpointNameValue `shouldBe` True,
                 otherEndpointName >= otherEndpointName `shouldBe` True,
                 max endpointNameValue otherEndpointName `shouldBe` otherEndpointName,
                 min endpointNameValue otherEndpointName `shouldBe` endpointNameValue,
                 routeTemplateValue /= otherRouteTemplate `shouldBe` True,
                 compare routeTemplateValue routeTemplateValue `shouldBe` EQ,
                 compare routeTemplateValue otherRouteTemplate `shouldBe` GT,
                 otherRouteTemplate < routeTemplateValue `shouldBe` True,
                 routeTemplateValue <= routeTemplateValue `shouldBe` True,
                 routeTemplateValue > otherRouteTemplate `shouldBe` True,
                 routeTemplateValue >= routeTemplateValue `shouldBe` True,
                 max routeTemplateValue otherRouteTemplate `shouldBe` routeTemplateValue,
                 min routeTemplateValue otherRouteTemplate `shouldBe` otherRouteTemplate,
                 hasDerivedContract [endpointNameValue, otherEndpointName] `shouldBe` True,
                 hasDerivedContract [routeTemplateValue, otherRouteTemplate] `shouldBe` True,
                 hasDerivedContract [profileNameValue, otherProfileName] `shouldBe` True,
                 authenticationProfileNameText profileNameValue `shouldBe` "api",
                 compare profileNameValue otherProfileName `shouldBe` LT,
                 profileNameValue < otherProfileName `shouldBe` True,
                 profileNameValue <= profileNameValue `shouldBe` True,
                 otherProfileName > profileNameValue `shouldBe` True,
                 otherProfileName >= otherProfileName `shouldBe` True,
                 max profileNameValue otherProfileName `shouldBe` otherProfileName,
                 min profileNameValue otherProfileName `shouldBe` profileNameValue,
                 hasDerivedContract protocols `shouldBe` True,
                 hasDerivedContract requirements `shouldBe` True,
                 hasDerivedContract errors `shouldBe` True,
                 hasDerivedContract [metadata, otherMetadata] `shouldBe` True
               ]
        )

  describe "runEndpointGuardPipeline" $ do
    it "runs guards in declaration order, carries context, and stops before a handler could run" $ do
      visits <- newIORef ([] :: [Text.Text])
      let metadata = mkEndpointMetadata (requiredEndpointName "test.data") (requiredRouteTemplate "/data") ApiEndpoint (RequireAuthorized ReadData)
          endpointRequest =
            EndpointRequest
              { endpointWaiRequest = Wai.defaultRequest,
                endpointRouteRequest = RouteRequest DataRoute defaultContext,
                endpointMetadata = metadata,
                endpointSecurityEventSink = Nothing,
                endpointDispatchKind = EndpointMatched
              }
          enrich =
            EndpointGuard $ \request -> do
              modifyIORef' visits (<> ["enrich"])
              endpointAccess (endpointMetadata request) `shouldBe` RequireAuthorized ReadData
              pure (ContinueEndpoint ((requestContext (endpointRouteRequest request)) {requestLanguage = "es"}))
          halt =
            EndpointGuard $ \request -> do
              modifyIORef' visits (<> ["halt"])
              requestLanguage (requestContext (endpointRouteRequest request)) `shouldBe` "es"
              pure (HaltEndpoint (NonPageBodyResponse deniedResponse))
          skipped = EndpointGuard $ \_ -> do
            modifyIORef' visits (<> ["skipped"])
            pure (ContinueEndpoint defaultContext)
      runEndpointGuardPipeline [enrich, halt, skipped] endpointRequest
        `shouldReturn` HaltEndpoint (NonPageBodyResponse deniedResponse)
      readIORef visits `shouldReturn` ["enrich", "halt"]

    it "exposes each root security phase without making disabled authentication implicit" $ do
      let preGuard = EndpointGuard (const (pure (ContinueEndpoint defaultContext)))
          authenticate = AuthenticationGuard (const (pure (ContinueEndpoint defaultContext)))
          postGuard = EndpointGuard (const (pure (ContinueEndpoint defaultContext)))
          disabled = AuthenticationDisabled [preGuard]
          enabled = AuthenticationEnabled [preGuard] authenticate [postGuard]
          dispatchKinds = [EndpointMatched, EndpointMatchedHead, EndpointMethodNotAllowed, EndpointOptions, EndpointClientAction]
      expectAll
        ( (length (unauthenticatedApplicationGuards disabled) `shouldBe` 1)
            :| [ length (unauthenticatedApplicationGuards enabled) `shouldBe` 0,
                 length (beforeAuthenticationGuards disabled) `shouldBe` 0,
                 length (beforeAuthenticationGuards enabled) `shouldBe` 1,
                 isNothing (authenticationGuard disabled) `shouldBe` True,
                 runAuthenticationGuard (requiredAuthenticationGuard (authenticationGuard enabled)) (endpointRequestFor AllowUnauthenticated)
                   `shouldReturn` ContinueEndpoint defaultContext,
                 length (afterAuthenticationGuards disabled) `shouldBe` 0,
                 length (afterAuthenticationGuards enabled) `shouldBe` 1,
                 hasDerivedContract dispatchKinds `shouldBe` True,
                 hasDerivedContract
                   ( [ ContinueEndpoint defaultContext,
                       HaltEndpoint (NonPageBodyResponse deniedResponse)
                     ] ::
                       [EndpointGuardResult TestRoute TestContext]
                   )
                   `shouldBe` True
               ]
        )

    it "resolves a protected descendant through an enabled profile beneath an anonymous default" $ do
      let publicProfile = requiredAuthenticationProfileNameOrDie "public"
          apiProfile = requiredAuthenticationProfileNameOrDie "api"
          authenticated = AuthenticationGuard (const (pure (ContinueEndpoint defaultContext)))
          profiles =
            mkAuthenticationProfiles
              []
              (mkAuthenticationProfile publicProfile Nothing :| [mkAuthenticationProfile apiProfile (Just authenticated)])
              publicProfile
              []
          publicEndpoint = mkEndpointMetadata (requiredEndpointName "test.public") (requiredRouteTemplate "/public") ApiEndpoint AllowUnauthenticated
          protectedEndpoint = withAuthenticationProfile apiProfile (mkEndpointMetadata (requiredEndpointName "test.api") (requiredRouteTemplate "/api") ApiEndpoint RequireAuthenticated)
      security <-
        case profiles of
          Left configurationError -> expectationFailure (show configurationError) >> fail "could not construct profile registry"
          Right configuredSecurity -> pure configuredSecurity
      let resolvedPublic = resolveAuthenticationProfile security publicEndpoint
          resolvedProtected = resolveAuthenticationProfile security protectedEndpoint
      expectAll
        ( (either (const False) isNothing resolvedPublic `shouldBe` True)
            :| [ either (const True) isNothing resolvedProtected `shouldBe` False,
                 validateAuthenticationProfileRequirements security [publicEndpoint, protectedEndpoint] `shouldBe` Right (),
                 validateAuthenticationProfileRequirements security [publicEndpoint {endpointAccess = RequireAuthenticated}]
                   `shouldBe` Left (ProtectedEndpointWithoutAuthenticationProfile (requiredEndpointName "test.public")),
                 validateAuthenticationProfileRequirements security [withAuthenticationProfile (requiredAuthenticationProfileNameOrDie "missing") protectedEndpoint]
                   `shouldBe` Left (UnknownAuthenticationProfile (requiredAuthenticationProfileNameOrDie "missing"))
               ]
        )

    it "keeps profile registries in the existing security-phase API without accepting legacy overrides" $ do
      let publicProfile = requiredAuthenticationProfileNameOrDie "public"
          apiProfile = requiredAuthenticationProfileNameOrDie "api"
          missingProfile = requiredAuthenticationProfileNameOrDie "missing"
          preGuard = EndpointGuard (const (pure (ContinueEndpoint defaultContext)))
          authenticate = AuthenticationGuard (const (pure (ContinueEndpoint defaultContext)))
          postGuard = EndpointGuard (const (pure (ContinueEndpoint defaultContext)))
          configuredProfiles =
            mkAuthenticationProfiles
              [preGuard]
              (mkAuthenticationProfile publicProfile Nothing :| [mkAuthenticationProfile apiProfile (Just authenticate)])
              publicProfile
              [postGuard]
          legacyDisabled = AuthenticationDisabled []
          legacyEnabled = AuthenticationEnabled [] authenticate []
          publicEndpoint = mkEndpointMetadata (requiredEndpointName "test.public") (requiredRouteTemplate "/public") ApiEndpoint AllowUnauthenticated
          authorizedPublicEndpoint = publicEndpoint {endpointAccess = RequireAuthorized ReadData}
          authorizedApiEndpoint = withAuthenticationProfile apiProfile (mkEndpointMetadata (requiredEndpointName "test.api-admin") (requiredRouteTemplate "/api-admin") ApiEndpoint (RequireAuthorized ReadData))
      security <-
        case configuredProfiles of
          Left configurationError -> expectationFailure (show configurationError) >> fail "could not construct profile registry"
          Right configuredSecurity -> pure configuredSecurity
      expectAll
        ( (length (unauthenticatedApplicationGuards security) `shouldBe` 0)
            :| [ length (beforeAuthenticationGuards security) `shouldBe` 1,
                 isNothing (authenticationGuard security) `shouldBe` True,
                 length (afterAuthenticationGuards security) `shouldBe` 1,
                 profileResolutionError (resolveAuthenticationProfile legacyDisabled (withAuthenticationProfile missingProfile publicEndpoint))
                   `shouldBe` Just (UnknownAuthenticationProfile missingProfile),
                 profileResolutionError (resolveAuthenticationProfile legacyEnabled (withAuthenticationProfile missingProfile publicEndpoint))
                   `shouldBe` Just (UnknownAuthenticationProfile missingProfile),
                 profileRequirementsAreValid security [publicEndpoint],
                 profileRequirementsAreValid security [authorizedApiEndpoint],
                 profileResolutionError
                   ( validateAuthenticationProfileRequirements
                       security
                       [ publicEndpoint,
                         withAuthenticationProfile missingProfile authorizedApiEndpoint
                       ]
                   )
                   `shouldBe` Just (UnknownAuthenticationProfile missingProfile),
                 profileResolutionError (validateAuthenticationProfileRequirements security [authorizedPublicEndpoint])
                   `shouldBe` Just (ProtectedEndpointWithoutAuthenticationProfile (requiredEndpointName "test.public")),
                 hasDerivedContract
                   [ DuplicateAuthenticationProfile publicProfile,
                     MissingDefaultAuthenticationProfile apiProfile
                   ]
                   `shouldBe` True,
                 hasDerivedContract
                   [ UnknownAuthenticationProfile missingProfile,
                     ProtectedEndpointWithoutAuthenticationProfile (requiredEndpointName "test.protected")
                   ]
                   `shouldBe` True
               ]
        )

    it "rejects duplicate registries and defaults that are not configured" $ do
      let publicProfile = requiredAuthenticationProfileNameOrDie "public"
          otherProfile = requiredAuthenticationProfileNameOrDie "other"
          duplicate = mkAuthenticationProfile publicProfile Nothing
      expectAll
        ( ( profileConfigurationError (mkAuthenticationProfiles [] (duplicate :| [duplicate]) publicProfile [])
              `shouldBe` Just (DuplicateAuthenticationProfile publicProfile)
          )
            :| [ profileConfigurationError (mkAuthenticationProfiles [] (duplicate :| []) otherProfile [])
                   `shouldBe` Just (MissingDefaultAuthenticationProfile otherProfile),
                 mkAuthenticationProfileName "api/v1" `shouldBe` Left InvalidAuthenticationProfileName,
                 authenticationProfileNameText publicProfile `shouldBe` "public"
               ]
        )

requiredEndpointName :: Text.Text -> EndpointName
requiredEndpointName endpointNameValue =
  case mkEndpointName endpointNameValue of
    Right parsedEndpointName -> parsedEndpointName
    Left metadataError -> error ("invalid endpoint-name test literal: " <> show metadataError)

requiredRouteTemplate :: Text.Text -> RouteTemplate
requiredRouteTemplate routeTemplateValue =
  case mkRouteTemplate routeTemplateValue of
    Right parsedRouteTemplate -> parsedRouteTemplate
    Left metadataError -> error ("invalid route-template test literal: " <> show metadataError)

deniedResponse :: ResponseBody
deniedResponse =
  ResponseBody
    { responseStatus = Http.status401,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = "Sign in required",
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }

endpointRequestFor :: AccessRequirement EndpointSecurityPolicy -> EndpointRequest TestRoute TestContext EndpointSecurityPolicy
endpointRequestFor access =
  EndpointRequest
    { endpointWaiRequest = Wai.defaultRequest,
      endpointRouteRequest = RouteRequest DataRoute defaultContext,
      endpointMetadata = mkEndpointMetadata (requiredEndpointName "test.data") (requiredRouteTemplate "/data") ApiEndpoint access,
      endpointSecurityEventSink = Nothing,
      endpointDispatchKind = EndpointMatched
    }

requiredAuthenticationGuard :: Maybe (AuthenticationGuard TestRoute TestContext EndpointSecurityPolicy) -> AuthenticationGuard TestRoute TestContext EndpointSecurityPolicy
requiredAuthenticationGuard maybeGuard =
  case maybeGuard of
    Just guard -> guard
    Nothing -> error "expected configured authentication guard"

profileConfigurationError :: Either AuthenticationProfileConfigurationError value -> Maybe AuthenticationProfileConfigurationError
profileConfigurationError = either Just (const Nothing)

profileResolutionError :: Either AuthenticationProfileResolutionError value -> Maybe AuthenticationProfileResolutionError
profileResolutionError = either Just (const Nothing)

profileRequirementsAreValid :: ApplicationSecurity TestRoute TestContext EndpointSecurityPolicy -> [EndpointMetadata EndpointSecurityPolicy] -> Expectation
profileRequirementsAreValid security metadata =
  case validateAuthenticationProfileRequirements security metadata of
    Right unit -> evaluate unit
    Left resolutionError -> expectationFailure (show resolutionError)

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)
    && sum [length (show item) + length (showList [item] "") | item <- values] > 0
