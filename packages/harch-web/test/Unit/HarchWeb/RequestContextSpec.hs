{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.EndpointMetadata (EndpointProtocol (HtmlEndpoint), mkEndpointMetadata, mkEndpointName, mkRouteTemplate)
import HarchWeb.EndpointMetadata qualified as EndpointMetadata
import HarchWeb.Localization (locale)
import HarchWeb.Observability (RequestTraceContext (..))
import HarchWeb.RequestContext
import HarchWeb.Security (emptyPathPrefix)
import HarchWeb.SecurityEvent (ModuleName, RouteObservation, mkModuleName, rootRouteObservation)

spec = describe "HarchWeb.RequestContext" $ do
  it "accepts configured HTTP(S) origins without changing their authority" $
    expectAll
      ( (canonicalOriginText (requiredOrigin "https://app.example.test:8443") `shouldBe` "https://app.example.test:8443")
          :| [ canonicalOriginText (requiredOrigin "http://localhost:3000") `shouldBe` "http://localhost:3000"
             ]
      )

  it "fails fast for an invalid static origin declaration while runtime validation stays on Either" $ do
    canonicalOriginText (requiredCanonicalOriginOrDie "https://app.example.test") `shouldBe` "https://app.example.test"
    evaluate (requiredCanonicalOriginOrDie "app.example.test")
      `shouldThrow` \case
        ErrorCall message -> message == "invalid canonical origin declaration: missing-scheme"

  it "rejects origins that could change browser origin semantics" $
    expectAll
      ( (originErrorCode (mkCanonicalOrigin "app.example.test") `shouldBe` Just "missing-scheme")
          :| [ originErrorCode (mkCanonicalOrigin "https://") `shouldBe` Just "empty-authority",
               originErrorCode (mkCanonicalOrigin "http://") `shouldBe` Just "empty-authority",
               originErrorCode (mkCanonicalOrigin "https://app.example.test path") `shouldBe` Just "unsafe-character",
               originErrorCode (mkCanonicalOrigin "https://app.example.test\n") `shouldBe` Just "unsafe-character",
               originErrorCode (mkCanonicalOrigin "https://app.example.test/path") `shouldBe` Just "unsafe-character",
               originErrorCode (mkCanonicalOrigin "https://app.example.test?query") `shouldBe` Just "unsafe-character",
               originErrorCode (mkCanonicalOrigin "https://app.example.test#fragment") `shouldBe` Just "unsafe-character",
               originErrorCode (mkCanonicalOrigin "https://user@app.example.test") `shouldBe` Just "unsafe-character",
               originErrorCode (mkCanonicalOrigin "https://app.example.test\\path") `shouldBe` Just "unsafe-character"
             ]
      )

  it "keeps canonical-origin failures distinct with stable diagnostic codes" $
    expectAll
      ( (CanonicalOriginMissingScheme /= CanonicalOriginEmptyAuthority `shouldBe` True)
          :| [ CanonicalOriginEmptyAuthority /= CanonicalOriginUnsafeCharacter `shouldBe` True,
               canonicalOriginErrorCode CanonicalOriginMissingScheme `shouldBe` "missing-scheme",
               canonicalOriginErrorCode CanonicalOriginEmptyAuthority `shouldBe` "empty-authority",
               canonicalOriginErrorCode CanonicalOriginUnsafeCharacter `shouldBe` "unsafe-character"
             ]
      )

  it "retains an absent or validated incoming trace without synthesizing one" $
    expectAll
      ( (correlationTraceContext (correlationContext Nothing) `shouldBe` Nothing)
          :| [ correlationTraceContext (correlationContext (Just traceContext)) `shouldBe` Just traceContext
             ]
      )

  it "keeps root-owned facts intact while a projection narrows package-selected values" $ do
    let parentContext :: RequestContext (RequestIdentity Text.Text) Text.Text Int
        parentContext =
          RequestContext
            { requestCore = coreContext,
              requestIdentity = AuthenticatedIdentity "account-42",
              requestClient = "web",
              requestLocal = 17
            }
        ContextProjection project =
          defaultContextProjection
            (\case AnonymousIdentity -> Nothing; AuthenticatedIdentity account -> Just account)
            ("client:" <>)
            show
        childContext = project parentContext
    expectAll
      ( (requestCore childContext `shouldBe` coreContext)
          :| [ requestIdentity childContext `shouldBe` Just "account-42",
               requestClient childContext `shouldBe` "client:web",
               requestLocal childContext `shouldBe` "17"
             ]
      )

  it "models anonymous and authenticated identity as distinct ordinary states" $
    expectAll
      ( (AnonymousIdentity `shouldBe` (AnonymousIdentity :: RequestIdentity Text.Text))
          :| [ (AnonymousIdentity :: RequestIdentity Text.Text) `shouldNotBe` AuthenticatedIdentity "account-42",
               (AnonymousIdentity :: RequestIdentity Text.Text) /= AuthenticatedIdentity "account-42" `shouldBe` True,
               AuthenticatedIdentity "account-42" `shouldBe` (AuthenticatedIdentity "account-42" :: RequestIdentity Text.Text),
               show (AnonymousIdentity :: RequestIdentity Text.Text) `shouldBe` "AnonymousIdentity",
               show (AuthenticatedIdentity "account-42" :: RequestIdentity Text.Text) `shouldBe` "AuthenticatedIdentity \"account-42\"",
               showList [AnonymousIdentity, AuthenticatedIdentity "account-42" :: RequestIdentity Text.Text] "" `shouldSatisfy` (not . null)
             ]
      )

  it "keeps core and complete request contexts comparable and printable" $ do
    let alternateCore = coreContext {requestLocale = locale "es"}
        anonymousContext :: RequestContext (RequestIdentity Text.Text) Text.Text Int
        anonymousContext = RequestContext coreContext AnonymousIdentity "web" 17
        authenticatedContext :: RequestContext (RequestIdentity Text.Text) Text.Text Int
        authenticatedContext = RequestContext coreContext (AuthenticatedIdentity "account-42") "web" 17
    expectAll
      ( (coreContext `shouldNotBe` alternateCore)
          :| [ coreContext `shouldBe` coreContext,
               length (show coreContext) `shouldSatisfy` (> 0),
               length (showList [coreContext] "") `shouldSatisfy` (> 0),
               anonymousContext `shouldNotBe` authenticatedContext,
               length (show authenticatedContext) `shouldSatisfy` (> 0),
               length (showList [authenticatedContext] "") `shouldSatisfy` (> 0),
               correlationContext Nothing `shouldNotBe` correlationContext (Just traceContext),
               length (show (correlationContext (Just traceContext))) `shouldSatisfy` (> 0),
               show (requiredOrigin "https://app.example.test") `shouldBe` "CanonicalOrigin \"https://app.example.test\"",
               requiredOrigin "https://app.example.test" /= requiredOrigin "http://app.example.test" `shouldBe` True,
               showsPrec 11 (requiredOrigin "https://app.example.test") "" `shouldSatisfy` (not . null),
               showList [requiredOrigin "https://app.example.test"] "" `shouldSatisfy` (not . null),
               correlationContext (Just traceContext) `shouldBe` correlationContext (Just traceContext),
               showList [correlationContext (Just traceContext)] "" `shouldSatisfy` (not . null),
               authenticatedContext `shouldBe` authenticatedContext,
               showList [authenticatedContext] "" `shouldSatisfy` (not . null)
             ]
      )

requiredOrigin :: Text.Text -> CanonicalOrigin
requiredOrigin origin =
  case mkCanonicalOrigin origin of
    Left originError -> error ("invalid test origin: " <> Text.unpack (canonicalOriginErrorCode originError))
    Right canonicalOrigin -> canonicalOrigin

originErrorCode :: Either CanonicalOriginError CanonicalOrigin -> Maybe Text.Text
originErrorCode = either (Just . canonicalOriginErrorCode) (const Nothing)

coreContext :: CoreRequestContext
coreContext =
  CoreRequestContext
    { requestLocale = locale "en-GB",
      requestLocaleFallbacks = [locale "en"],
      requestRouteObservation = Just routeObservation,
      requestCorrelation = correlationContext (Just traceContext),
      requestCanonicalOrigin = requiredOrigin "https://app.example.test",
      requestPathPrefix = emptyPathPrefix
    }

traceContext :: RequestTraceContext
traceContext =
  RequestTraceContext
    { traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
      traceContextParentSpanId = "00f067aa0ba902b7",
      traceContextState = Just "vendor=value"
    }

routeObservation :: RouteObservation
routeObservation =
  rootRouteObservation
    (requiredModuleName "root")
    (locale "en-GB")
    (EndpointMetadata.endpointName endpointMetadata)
    (EndpointMetadata.endpointRouteTemplate endpointMetadata)

endpointMetadata :: EndpointMetadata.EndpointMetadata ()
endpointMetadata =
  mkEndpointMetadata
    (requiredEndpointName "root.context")
    (requiredRouteTemplate "/context")
    HtmlEndpoint
    EndpointMetadata.AllowUnauthenticated

requiredModuleName :: Text.Text -> ModuleName
requiredModuleName moduleName =
  case mkModuleName moduleName of
    Left moduleNameError -> error ("invalid test module name: " <> show moduleNameError)
    Right validModuleName -> validModuleName

requiredEndpointName :: Text.Text -> EndpointMetadata.EndpointName
requiredEndpointName endpointName =
  case mkEndpointName endpointName of
    Left metadataError -> error ("invalid test endpoint name: " <> show metadataError)
    Right validEndpointName -> validEndpointName

requiredRouteTemplate :: Text.Text -> EndpointMetadata.RouteTemplate
requiredRouteTemplate routeTemplate =
  case mkRouteTemplate routeTemplate of
    Left metadataError -> error ("invalid test route template: " <> show metadataError)
    Right validRouteTemplate -> validRouteTemplate
