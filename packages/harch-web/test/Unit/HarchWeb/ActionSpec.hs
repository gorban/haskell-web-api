{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception (ErrorCall (..), evaluate)
import Control.Monad ()
import Data.ByteString qualified as ByteString (length)
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either (fromRight)
import Data.Functor.Compose (Compose (..))
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text (isInfixOf, null, pack)
import Data.Text.Encoding qualified as TextEncoding (encodeUtf8)
import HarchWeb (ActionCapability (ConditionalLeaveConfirmation, HandlerSafeRetry, IdempotentMutationRetry, NativeFallback), ActionFormAttributes (actionFormCapabilities), ActionIdempotency (actionIdempotencyKey), ActionRecoveryCopy (actionCancelCopy, actionCancelledCopy, actionDelayedCopy, actionPendingCopy, actionReadyCopy, actionRecoverableCopy, actionRetryCopy), FormMethod (FormGet, FormPost), NativeActionFallback (NativeActionFallback, nativeActionFallbackCsrfToken, nativeActionFallbackMethod, nativeActionFallbackPath), actionForm, actionIdempotency, defaultActionFormAttributes, defaultActionRecoveryCopy, defaultCaptureKernelByteBudget, defaultCaptureKernelScript, defaultNavigationRuntimeScript, mkCsrfToken, mkRetainedActionLifetime, renderActionForm, renderHtml, retainedActionLifetimeMilliseconds, staticActionForm, text)
import HarchWeb qualified as Web
import HarchWeb.Action qualified as Action (ActionCodec, ActionCodecError (..), ActionDecoder, ActionMethod (ActionDelete, ActionGet, ActionPatch, ActionPost, ActionPut), ClientActionDecodeResult (..), ClientActionParseError (DuplicateActionField, InvalidActionField, MissingActionField), ClientActionPayload (ClientActionPayload, clientActionCsrfToken, clientActionFields, clientActionIdempotencyKey, clientActionMethod, clientActionPath, clientActionPayloadContext), action, actionCodec, actionEndpointMetadata, actionEndpointTarget, actionMethod, actionMethodText, actionPath, combineActionCodecs, decodeAction, delete, deleteAt, emptyActionCodec, exactlyOne, formField, get, getAt, mapActionCodec, methodAt, mountActionCodecAtPrefix, optional, parseField, patch, patchAt, post, postAt, prefixActionCodecByContext, publicAction, put, putAt, required, singleActionCodec, singleActionCodecWithMetadata, singleOrDefault, staticActionEndpointMetadata, staticActionPath, textValue)
import HarchWeb.ApplicationModule (ActionMount (..), AuthorizationProjection (..), ContextProjection (..), mountActionCodec)
import HarchWeb.Database qualified as Database ()
import HarchWeb.EndpointMetadata qualified as EndpointMetadata
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Routing (requiredPathSegment)
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestContext (testContextPathPrefix), defaultContext, spanishContext, testActionCodec)

data ChildActionTarget = ChildSaveTarget
  deriving (Eq, Show)

testFallbackCsrfToken :: Web.CsrfToken
testFallbackCsrfToken = fromMaybe (error "expected valid CSRF token") (mkCsrfToken "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

data ParentActionTarget = ParentCatalogActionTarget
  deriving (Eq, Show)

data ChildAction = ChildSaved
  deriving (Eq, Show)

newtype ParentAction = ParentCatalogAction ChildAction
  deriving (Eq, Show)

data ChildPolicy = MaySaveCatalog
  deriving (Eq, Show)

data ParentPolicy = MayManageCatalog
  deriving (Eq, Show)

spec = do
  describe "HarchWeb.Action" $ do
    it "mounts a child action codec through typed context and policy projections" $ do
      let childMetadata =
            EndpointMetadata.mkEndpointMetadata
              (EndpointMetadata.requiredEndpointNameOrDie "catalog.save")
              (EndpointMetadata.requiredRouteTemplateOrDie "/save")
              EndpointMetadata.ActionEndpoint
              (EndpointMetadata.RequireAuthorized MaySaveCatalog)
          childCodec =
            Action.singleActionCodecWithMetadata
              ChildSaveTarget
              (Action.postAt "/save" (\childContext -> "/catalog/" <> childContext <> "/save"))
              childMetadata
              (pure ChildSaved)
          mountedCodec =
            mountActionCodec
              ActionMount
                { embedChildActionTarget = const ParentCatalogActionTarget,
                  projectChildActionTarget = \case
                    ParentCatalogActionTarget -> Just ChildSaveTarget,
                  embedChildAction = ParentCatalogAction,
                  projectChildAction = \case
                    ParentCatalogAction childAction -> Just childAction
                }
              (ContextProjection (\parentContext -> "tenant-" <> Text.pack (show (parentContext :: Int))))
              (AuthorizationProjection (\MaySaveCatalog -> MayManageCatalog))
              childCodec
          expectedMetadata =
            childMetadata
              { EndpointMetadata.endpointAccess = EndpointMetadata.RequireAuthorized MayManageCatalog
              }
          payload =
            Action.ClientActionPayload
              { Action.clientActionMethod = "POST",
                Action.clientActionPath = "/catalog/tenant-42/save",
                Action.clientActionFields = [],
                Action.clientActionCsrfToken = Nothing,
                Action.clientActionIdempotencyKey = Nothing,
                Action.clientActionPayloadContext = 42
              }
      expectAll
        ( ( Action.actionPath mountedCodec 42 ParentCatalogActionTarget
              `shouldBe` Just "/catalog/tenant-42/save"
          )
            :| [ Action.actionEndpointMetadata mountedCodec 42 "POST" "/catalog/tenant-42/save" `shouldBe` Just expectedMetadata,
                 Action.actionEndpointTarget mountedCodec 42 "POST" "/catalog/tenant-42/save" `shouldBe` Just ParentCatalogActionTarget,
                 Action.decodeAction mountedCodec payload `shouldBe` Action.DecodedClientAction (ParentCatalogAction ChildSaved)
               ]
        )

    it "prefixes validated action declarations from trusted request context" $ do
      let metadata =
            EndpointMetadata.mkEndpointMetadata
              (EndpointMetadata.requiredEndpointNameOrDie "tenant.save")
              (EndpointMetadata.requiredRouteTemplateOrDie "/")
              EndpointMetadata.ActionEndpoint
              EndpointMetadata.AllowUnauthenticated
          sourceCodec :: Action.ActionCodec Text Text () Text
          sourceCodec = Action.singleActionCodecWithMetadata "save" (Action.post "/") metadata (pure "saved")
          prefixedCodec = Action.prefixActionCodecByContext ("/tenants/" <>) "/{tenant}" sourceCodec
          expectedMetadata = metadata {EndpointMetadata.endpointRouteTemplate = EndpointMetadata.requiredRouteTemplateOrDie "/{tenant}"}
          payload = Action.ClientActionPayload "POST" "/tenants/42" [] Nothing Nothing "42"
      case prefixedCodec of
        Left codecError -> expectationFailure (show codecError)
        Right codec ->
          expectAll
            ( (Action.actionPath codec "42" "save" `shouldBe` Just "/tenants/42")
                :| [ Action.actionEndpointMetadata codec "42" "POST" "/tenants/42" `shouldBe` Just expectedMetadata,
                     Action.staticActionEndpointMetadata codec "POST" "/tenants/42" `shouldBe` Nothing,
                     Action.decodeAction codec payload `shouldBe` Action.DecodedClientAction "saved"
                   ]
            )
      case (prefixedCodec, Action.prefixActionCodecByContext ("/tenants/" <>) "/{tenant}" sourceCodec) of
        (Right firstCodec, Right duplicateCodec) ->
          case Action.combineActionCodecs (firstCodec :| [duplicateCodec]) of
            Left codecError -> codecError `shouldBe` Action.DuplicateActionEndpoint Action.ActionPost "/{tenant}"
            Right _ -> expectationFailure "expected duplicate context-prefixed action identity"
        (Left codecError, _) -> expectationFailure (show codecError)
        (_, Left codecError) -> expectationFailure (show codecError)
      case Action.prefixActionCodecByContext (const "/tenant") "not a route" sourceCodec of
        Left codecError -> codecError `shouldBe` Action.InvalidActionEndpointMetadata EndpointMetadata.InvalidRouteTemplate
        Right _ -> expectationFailure "expected an invalid context-prefix template"

      let dynamicSource :: Action.ActionCodec Text Text () Text
          dynamicSource = Action.singleActionCodecWithMetadata "save" (Action.postAt "/save" ("/save-" <>)) metadata (pure "saved")
      case Action.prefixActionCodecByContext ("/tenants/" <>) "/{tenant}/save" dynamicSource of
        Left codecError -> expectationFailure (show codecError)
        Right codec ->
          Action.actionPath codec "42" "save" `shouldBe` Just "/tenants/42/save-42"

    it "preserves static action proofs and non-domain access requirements through mapping" $ do
      let anonymousMetadata :: EndpointMetadata.EndpointMetadata ChildPolicy
          anonymousMetadata =
            EndpointMetadata.mkEndpointMetadata
              (EndpointMetadata.requiredEndpointNameOrDie "catalog.public-save")
              (EndpointMetadata.requiredRouteTemplateOrDie "/save")
              EndpointMetadata.ActionEndpoint
              EndpointMetadata.AllowUnauthenticated
          authenticatedMetadata :: EndpointMetadata.EndpointMetadata ChildPolicy
          authenticatedMetadata =
            EndpointMetadata.mkEndpointMetadata
              (EndpointMetadata.requiredEndpointNameOrDie "catalog.authenticated-save")
              (EndpointMetadata.requiredRouteTemplateOrDie "/authenticated-save")
              EndpointMetadata.ActionEndpoint
              EndpointMetadata.RequireAuthenticated
          anonymousCodec :: Action.ActionCodec ChildActionTarget Text ChildPolicy ChildAction
          anonymousCodec = Action.singleActionCodecWithMetadata ChildSaveTarget (Action.post "/save") anonymousMetadata (pure ChildSaved)
          authenticatedCodec :: Action.ActionCodec ChildActionTarget Text ChildPolicy ChildAction
          authenticatedCodec = Action.singleActionCodecWithMetadata ChildSaveTarget (Action.post "/authenticated-save") authenticatedMetadata (pure ChildSaved)
          mapCodec = Action.mapActionCodec (\ChildSaveTarget -> ParentCatalogActionTarget) (const "child") (\MaySaveCatalog -> MayManageCatalog) ParentCatalogAction
          mappedAnonymousCodec = mapCodec anonymousCodec
          mappedAuthenticatedCodec = mapCodec authenticatedCodec
          mappedBooleanTargetCodec :: Action.ActionCodec Bool Int ParentPolicy ParentAction
          mappedBooleanTargetCodec =
            Action.mapActionCodec
              (\ChildSaveTarget -> True)
              (const "child")
              (\MaySaveCatalog -> MayManageCatalog)
              ParentCatalogAction
              anonymousCodec
          anonymousExpected = anonymousMetadata {EndpointMetadata.endpointAccess = EndpointMetadata.AllowUnauthenticated}
          authenticatedExpected = authenticatedMetadata {EndpointMetadata.endpointAccess = EndpointMetadata.RequireAuthenticated}
      expectAll
        ( (Action.staticActionPath mappedAnonymousCodec ParentCatalogActionTarget `shouldBe` Just "/save")
            :| [ Action.actionEndpointMetadata mappedAnonymousCodec (42 :: Int) "POST" "/save" `shouldBe` Just anonymousExpected,
                 Action.staticActionEndpointMetadata mappedAnonymousCodec "POST" "/save" `shouldBe` Just anonymousExpected,
                 Action.actionEndpointMetadata mappedAuthenticatedCodec (42 :: Int) "POST" "/authenticated-save" `shouldBe` Just authenticatedExpected,
                 Action.staticActionEndpointMetadata mappedAuthenticatedCodec "POST" "/authenticated-save" `shouldBe` Just authenticatedExpected,
                 Action.decodeAction mappedAnonymousCodec (Action.ClientActionPayload "POST" "/save" [] Nothing Nothing (42 :: Int)) `shouldBe` Action.DecodedClientAction (ParentCatalogAction ChildSaved),
                 Action.staticActionPath mappedBooleanTargetCodec True `shouldBe` Just "/save",
                 Action.staticActionPath mappedBooleanTargetCodec False `shouldBe` Nothing,
                 case Action.actionEndpointMetadata mappedAnonymousCodec (42 :: Int) "POST" "/save" of
                   Nothing -> False `shouldBe` True
                   Just endpoint -> EndpointMetadata.endpointAccess endpoint `shouldBe` EndpointMetadata.AllowUnauthenticated
               ]
        )
      case Action.combineActionCodecs (mappedAnonymousCodec :| [mappedAnonymousCodec]) of
        Left codecError -> codecError `shouldBe` Action.DuplicateActionEndpoint Action.ActionPost "/save"
        Right _ -> expectationFailure "expected mapped action identities to remain duplicate-detectable"

    it "mounts a static root action as the mount path without losing its endpoint contract" $ do
      let metadata :: EndpointMetadata.EndpointMetadata ChildPolicy
          metadata =
            EndpointMetadata.mkEndpointMetadata
              (EndpointMetadata.requiredEndpointNameOrDie "catalog.root")
              (EndpointMetadata.requiredRouteTemplateOrDie "/")
              EndpointMetadata.ActionEndpoint
              EndpointMetadata.AllowUnauthenticated
          childCodec :: Action.ActionCodec ChildActionTarget Text ChildPolicy ChildAction
          childCodec = Action.singleActionCodecWithMetadata ChildSaveTarget (Action.post "/") metadata (pure ChildSaved)
      case Action.mountActionCodecAtPrefix
        (requiredPathSegment "catalog" :| [])
        "root.catalog"
        (\ChildSaveTarget -> ParentCatalogActionTarget)
        (const "child")
        (\MaySaveCatalog -> MayManageCatalog)
        ParentCatalogAction
        childCodec of
        Left codecError -> expectationFailure (show codecError)
        Right mountedCodec ->
          expectAll
            ( (Action.staticActionPath mountedCodec ParentCatalogActionTarget `shouldBe` Just "/catalog")
                :| [ Action.staticActionEndpointMetadata mountedCodec "POST" "/catalog"
                       `shouldBe` Just
                         ( EndpointMetadata.mkEndpointMetadata
                             (EndpointMetadata.requiredEndpointNameOrDie "root.catalog.catalog.root")
                             (EndpointMetadata.requiredRouteTemplateOrDie "/catalog")
                             EndpointMetadata.ActionEndpoint
                             EndpointMetadata.AllowUnauthenticated
                         ),
                     Action.decodeAction mountedCodec (Action.ClientActionPayload "POST" "/catalog" [] Nothing Nothing (42 :: Int))
                       `shouldBe` Action.DecodedClientAction (ParentCatalogAction ChildSaved),
                     case Action.staticActionEndpointMetadata mountedCodec "POST" "/catalog" of
                       Nothing -> False `shouldBe` True
                       Just endpoint -> EndpointMetadata.endpointAccess endpoint `shouldBe` EndpointMetadata.AllowUnauthenticated
                   ]
            )

    it "preserves an authenticated static root action while mounting it" $ do
      let metadata :: EndpointMetadata.EndpointMetadata ChildPolicy
          metadata =
            EndpointMetadata.mkEndpointMetadata
              (EndpointMetadata.requiredEndpointNameOrDie "catalog.authenticated-root")
              (EndpointMetadata.requiredRouteTemplateOrDie "/")
              EndpointMetadata.ActionEndpoint
              EndpointMetadata.RequireAuthenticated
          childCodec :: Action.ActionCodec ChildActionTarget Text ChildPolicy ChildAction
          childCodec = Action.singleActionCodecWithMetadata ChildSaveTarget (Action.post "/") metadata (pure ChildSaved)
      case Action.mountActionCodecAtPrefix
        (requiredPathSegment "catalog" :| [])
        "root.catalog"
        (\ChildSaveTarget -> ParentCatalogActionTarget)
        (const "child")
        (\MaySaveCatalog -> MayManageCatalog)
        ParentCatalogAction
        childCodec of
        Left codecError -> expectationFailure (show codecError)
        Right mountedCodec ->
          fmap EndpointMetadata.endpointAccess (Action.staticActionEndpointMetadata mountedCodec "POST" "/catalog")
            `shouldBe` Just EndpointMetadata.RequireAuthenticated

    it "derives accessible field relationships and a non-empty linked error summary" $ do
      let controlId = Web.literalElementId "email"
          hintId = Web.literalElementId "email-hint"
          errorId = Web.literalElementId "email-error"
          renderInput attributes = Web.voidElement Web.inputTag (Web.fieldControlIdAttribute attributes : Web.fieldControlRelationshipAttributes attributes)
          hintedField =
            Web.accessibleField
              (Web.AccessibleFieldProps controlId (Web.text "Email") (Just (Web.DescribedContent hintId (Web.text "Use your work address."))) Web.FieldValid)
              renderInput
          bareField =
            Web.accessibleField
              (Web.AccessibleFieldProps controlId (Web.text "Email") Nothing Web.FieldValid)
              renderInput
          invalidField =
            Web.accessibleField
              (Web.AccessibleFieldProps controlId (Web.text "Email") (Just (Web.DescribedContent hintId (Web.text "Use your work address."))) (Web.FieldInvalid (Web.DescribedContent errorId (Web.text "Enter a valid address."))))
              renderInput
          invalidFieldWithoutHint =
            Web.accessibleField
              (Web.AccessibleFieldProps controlId (Web.text "Email") Nothing (Web.FieldInvalid (Web.DescribedContent errorId (Web.text "Enter a valid address."))))
              renderInput
          summary =
            Web.errorSummary
              (Web.ErrorSummary (Web.literalElementId "errors") (Web.text "Fix these problems") (Web.FieldErrorLink controlId (Web.text "Email is invalid") :| []))
          hintedHtml = Web.renderHtml hintedField
          bareHtml = Web.renderHtml bareField
          invalidHtml = Web.renderHtml invalidField
          invalidWithoutHintHtml = Web.renderHtml invalidFieldWithoutHint
          summaryHtml = Web.renderHtml summary
      expectAll
        ( (hintedHtml `shouldSatisfy` Text.isInfixOf "<label for=\"email\">Email</label><input id=\"email\" aria-describedby=\"email-hint\">")
            :| [ bareHtml `shouldSatisfy` (not . Text.isInfixOf "aria-describedby"),
                 hintedHtml `shouldSatisfy` (not . Text.isInfixOf "aria-invalid"),
                 invalidHtml `shouldSatisfy` Text.isInfixOf "aria-describedby=\"email-hint email-error\" aria-invalid=\"true\" aria-errormessage=\"email-error\"",
                 invalidWithoutHintHtml `shouldSatisfy` Text.isInfixOf "aria-describedby=\"email-error\" aria-invalid=\"true\"",
                 invalidHtml `shouldSatisfy` Text.isInfixOf "id=\"email-error\" data-field-error",
                 summaryHtml `shouldSatisfy` Text.isInfixOf "id=\"errors\" tabindex=\"-1\" data-error-summary",
                 summaryHtml `shouldSatisfy` Text.isInfixOf "<a href=\"#email\">Email is invalid</a>"
               ]
        )

    it "keeps accessible control values comparable and printable" $ do
      let controlId = Web.literalElementId "email"
          otherId = Web.literalElementId "other"
          hint = Web.DescribedContent controlId (Web.text "Hint")
          otherHint = Web.DescribedContent otherId (Web.text "Other")
          validProps = Web.AccessibleFieldProps controlId (Web.text "Email") (Just hint) Web.FieldValid
          invalidProps = validProps {Web.accessibleFieldValidity = Web.FieldInvalid otherHint}
          controlAttributes = Web.FieldControlAttributes (Web.elementId controlId) [Web.ariaDescribedBy (controlId :| [])]
          otherControlAttributes = Web.FieldControlAttributes (Web.elementId otherId) []
          errorLink = Web.FieldErrorLink controlId (Web.text "Invalid")
          otherErrorLink = Web.FieldErrorLink otherId (Web.text "Other")
          summary = Web.ErrorSummary controlId (Web.text "Errors") (errorLink :| [])
          otherSummary = Web.ErrorSummary otherId (Web.text "Other errors") (otherErrorLink :| [])
          valuesDiffer =
            [ hint /= otherHint,
              Web.FieldValid /= Web.FieldInvalid hint,
              validProps /= invalidProps,
              controlAttributes /= otherControlAttributes,
              errorLink /= otherErrorLink,
              summary /= otherSummary
            ]
          printedLength =
            sum
              [ length (show hint) + length (showList [hint] ""),
                length (show Web.FieldValid) + length (showList [Web.FieldValid] ""),
                length (show validProps) + length (showList [validProps] ""),
                length (show controlAttributes) + length (showList [controlAttributes] ""),
                length (show errorLink) + length (showList [errorLink] ""),
                length (show summary) + length (showList [summary] "")
              ]
      valuesDiffer `shouldBe` replicate 6 True
      printedLength `shouldSatisfy` (> 0)

    it "prints codec paths and methods from the same declarations used for parsing and form markup" $ do
      let prefixedContext = defaultContext {testContextPathPrefix = "/app"}
          renderedForm = renderHtml (renderActionForm (actionForm testActionCodec prefixedContext "save" defaultActionFormAttributes [text "Save"]))
      Action.actionPath testActionCodec prefixedContext "save" `shouldBe` Just "/app/known"
      Action.actionMethod testActionCodec "save" `shouldBe` Just Action.ActionPost
      expectAll
        ( (Text.isInfixOf "data-harch-action-method=\"post\"" renderedForm `shouldBe` True)
            :| [ Text.isInfixOf "data-harch-action-capabilities=\"exclusive-client-handler\"" renderedForm `shouldBe` True,
                 Text.isInfixOf "action=\"/app/known\" method=\"dialog\"" renderedForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-status" renderedForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-retry" renderedForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-cancel" renderedForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-retention-ms=\"600000\"" renderedForm `shouldBe` True
               ]
        )

    it "keeps retained action envelopes in the capture kernel with a positive configurable lifetime" $ do
      let customLifetime = fromMaybe (error "expected positive retained action lifetime") (mkRetainedActionLifetime 120000)
          customAttributes = defaultActionFormAttributes {Web.actionFormRetainedActionLifetime = customLifetime}
          renderedForm = renderHtml (renderActionForm (actionForm testActionCodec defaultContext "save" customAttributes [text "Save"]))
          runtimeSources = defaultCaptureKernelScript <> defaultNavigationRuntimeScript
      expectAll
        ( (mkRetainedActionLifetime 0 `shouldBe` Nothing)
            :| [ retainedActionLifetimeMilliseconds customLifetime `shouldBe` 120000,
                 customLifetime == customLifetime `shouldBe` True,
                 customLifetime /= fromMaybe (error "expected a distinct positive retained action lifetime") (mkRetainedActionLifetime 120001) `shouldBe` True,
                 Text.isInfixOf "data-harch-action-retention-ms=\"120000\"" renderedForm `shouldBe` True,
                 Text.isInfixOf "retainForReauthentication" defaultCaptureKernelScript `shouldBe` True,
                 Text.isInfixOf "replayRetained" defaultCaptureKernelScript `shouldBe` True,
                 Text.isInfixOf "harch:action-reauthentication-required" defaultNavigationRuntimeScript `shouldBe` True,
                 Text.isInfixOf "refreshPageSecurityForRetainedAction" defaultNavigationRuntimeScript `shouldBe` True,
                 Text.isInfixOf "localStorage" runtimeSources `shouldBe` False,
                 Text.isInfixOf "sessionStorage" runtimeSources `shouldBe` False,
                 length (show customLifetime) + length (showList [customLifetime] "") `shouldSatisfy` (> 0)
               ]
        )

    it "renders a declaration-proven static action without inventing request context" $ do
      let staticCodec :: Action.ActionCodec Text TestContext () Text
          staticCodec =
            fromRight (error "invalid static action codec") $
              Action.actionCodec [Action.action "save" (Action.post "/actions/save") (pure "save")]
          renderedForm = renderHtml (renderActionForm (staticActionForm staticCodec "save" defaultActionFormAttributes [text "Save"]))
          dynamicRenderedForm = renderHtml (renderActionForm (staticActionForm testActionCodec "save" defaultActionFormAttributes [text "Save"]))
      expectAll
        ( (Action.staticActionPath staticCodec "save" `shouldBe` Just "/actions/save")
            :| [ Action.staticActionPath testActionCodec "save" `shouldBe` Nothing,
                 Text.isInfixOf "data-harch-action-path=\"/actions/save\"" renderedForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-configuration-error" dynamicRenderedForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action=\"true\"" dynamicRenderedForm `shouldBe` False
               ]
        )

    it "declares protected actions by default and anonymous actions only through the explicit constructor" $ do
      let protectedCodec :: Action.ActionCodec Text TestContext () Text
          protectedCodec =
            fromRight (error "invalid protected action codec") $
              Action.actionCodec [Action.action "save" (Action.post "/actions/save") (pure "save")]
          publicCodec :: Action.ActionCodec Text TestContext () Text
          publicCodec =
            fromRight (error "invalid public action codec") $
              Action.actionCodec
                [ Action.publicAction
                    "status"
                    (Action.get "/actions/status")
                    (fromRight (error "invalid endpoint name") (EndpointMetadata.mkEndpointName "action.status"))
                    (fromRight (error "invalid route template") (EndpointMetadata.mkRouteTemplate "/actions/status"))
                    (pure "status")
                ]
          invalidCodec :: Either Action.ActionCodecError (Action.ActionCodec Text TestContext () Text)
          invalidCodec =
            Action.actionCodec [Action.action "broken" (Action.post "not-a-route") (pure "broken")]
      expectAll
        ( ( Action.actionEndpointMetadata protectedCodec defaultContext "POST" "/actions/save"
              `shouldBe` Just
                ( EndpointMetadata.mkEndpointMetadata
                    (fromRight (error "invalid endpoint name") (EndpointMetadata.mkEndpointName "action.post.actions-save"))
                    (fromRight (error "invalid route template") (EndpointMetadata.mkRouteTemplate "/actions/save"))
                    EndpointMetadata.ActionEndpoint
                    EndpointMetadata.RequireAuthenticated
                )
          )
            :| [ Action.actionEndpointMetadata publicCodec defaultContext "GET" "/actions/status"
                   `shouldBe` Just
                     ( EndpointMetadata.mkEndpointMetadata
                         (fromRight (error "invalid endpoint name") (EndpointMetadata.mkEndpointName "action.status"))
                         (fromRight (error "invalid route template") (EndpointMetadata.mkRouteTemplate "/actions/status"))
                         EndpointMetadata.ActionEndpoint
                         EndpointMetadata.AllowUnauthenticated
                     ),
                 Action.actionPath publicCodec defaultContext "status" `shouldBe` Just "/actions/status",
                 Action.decodeAction publicCodec (Action.ClientActionPayload "GET" "/actions/status" [] Nothing Nothing defaultContext) `shouldBe` Action.DecodedClientAction "status",
                 Action.actionEndpointMetadata protectedCodec defaultContext "GET" "/actions/save" `shouldBe` Nothing,
                 Action.actionEndpointMetadata protectedCodec defaultContext "POST" "/other" `shouldBe` Nothing
               ]
        )
      case invalidCodec of
        Left codecError -> codecError `shouldBe` Action.InvalidActionEndpointMetadata EndpointMetadata.InvalidRouteTemplate
        Right _ -> expectationFailure "expected invalid action metadata"

    it "renders only explicitly declared native fallback and recovery capabilities" $ do
      let idempotency = fromMaybe (error "expected a valid test idempotency key") (actionIdempotency "mutation-1")
          nativeFallback = NativeActionFallback "/native-subscribe" FormPost testFallbackCsrfToken
          capabilities = [HandlerSafeRetry, ConditionalLeaveConfirmation, IdempotentMutationRetry idempotency, NativeFallback nativeFallback]
          nativeAttributes = defaultActionFormAttributes {actionFormCapabilities = capabilities}
          nativeForm = renderHtml (renderActionForm (actionForm testActionCodec defaultContext "save" nativeAttributes []))
          nativeGetForm = renderHtml (renderActionForm (actionForm testActionCodec defaultContext "read" nativeAttributes []))
      expectAll
        ( (Text.isInfixOf "method=\"post\"" nativeForm `shouldBe` True)
            :| [ Text.isInfixOf "method=\"post\"" nativeGetForm `shouldBe` True,
                 Text.isInfixOf "action=\"/native-subscribe\" method=\"post\"" nativeForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-path=\"/known\"" nativeForm `shouldBe` True,
                 Text.isInfixOf "name=\"_harch_csrf\" value=\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\"" nativeForm `shouldBe` True,
                 Text.isInfixOf "</input>" nativeForm `shouldBe` False,
                 Text.isInfixOf "data-harch-action-idempotency-key=\"mutation-1\"" nativeForm `shouldBe` True,
                 Text.isInfixOf "handler-safe-retry,conditional-leave-confirmation,idempotent-mutation-retry,native-fallback" nativeForm `shouldBe` True,
                 actionReadyCopy defaultActionRecoveryCopy `shouldBe` "Ready.",
                 actionPendingCopy defaultActionRecoveryCopy `shouldBe` "Submitting…",
                 actionDelayedCopy defaultActionRecoveryCopy `shouldBe` "Still waiting for this action to be handled.",
                 actionRecoverableCopy defaultActionRecoveryCopy `shouldBe` "This action needs your attention.",
                 actionCancelledCopy defaultActionRecoveryCopy `shouldBe` "Action cancelled.",
                 actionRetryCopy defaultActionRecoveryCopy `shouldBe` "Retry action",
                 actionCancelCopy defaultActionRecoveryCopy `shouldBe` "Cancel action",
                 actionIdempotencyKey idempotency `shouldBe` "mutation-1",
                 actionIdempotency "" `shouldBe` Nothing,
                 nativeActionFallbackPath nativeFallback `shouldBe` "/native-subscribe",
                 nativeActionFallbackMethod nativeFallback `shouldBe` FormPost,
                 nativeActionFallbackCsrfToken nativeFallback `shouldBe` testFallbackCsrfToken,
                 length (show (NativeFallback nativeFallback)) `shouldSatisfy` (> 0),
                 show (NativeFallback nativeFallback) `shouldNotContain` "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
                 showList capabilities "" `shouldSatisfy` (not . null),
                 sum [fromEnum (left == right) | left <- capabilities, right <- capabilities] `shouldBe` length capabilities,
                 sum [fromEnum (left /= right) | left <- capabilities, right <- capabilities] `shouldBe` length capabilities * (length capabilities - 1)
               ]
        )

    it "keeps FormMethod, NativeActionFallback, and ActionIdempotency comparable and printable, and renders a GET native fallback" $ do
      let getFallback = NativeActionFallback "/native-get" FormGet testFallbackCsrfToken
          postFallback = NativeActionFallback "/native-post" FormPost testFallbackCsrfToken
          idempotencyA = fromMaybe (error "expected a valid test idempotency key") (actionIdempotency "a")
          idempotencyB = fromMaybe (error "expected a valid test idempotency key") (actionIdempotency "b")
          getAttributes = defaultActionFormAttributes {actionFormCapabilities = [NativeFallback getFallback]}
          getForm = renderHtml (renderActionForm (actionForm testActionCodec defaultContext "save" getAttributes []))
      expectAll
        ( (FormGet `shouldNotBe` FormPost)
            :| [ length (show FormGet) + length (showList [FormGet] "") `shouldSatisfy` (> 0),
                 getFallback `shouldNotBe` postFallback,
                 length (show getFallback) + length (showList [getFallback] "") `shouldSatisfy` (> 0),
                 idempotencyA `shouldNotBe` idempotencyB,
                 length (show idempotencyA) + length (showList [idempotencyA] "") `shouldSatisfy` (> 0),
                 Text.isInfixOf "action=\"/native-get\" method=\"get\"" getForm `shouldBe` True
               ]
        )

    it "keeps the first-fold capture kernel within its rendered source budget" $
      ByteString.length (TextEncoding.encodeUtf8 defaultCaptureKernelScript)
        `shouldSatisfy` (<= defaultCaptureKernelByteBudget)

    it "renders a named native dialog with one complete link fallback" $ do
      let triggerName = fromMaybe (error "expected trigger name") (Web.mkAccessibleName "Language")
          otherName = fromMaybe (error "expected other name") (Web.mkAccessibleName "Other language")
          closeName = fromMaybe (error "expected close name") (Web.mkAccessibleName "Close language picker")
          trigger = Web.DialogLinkTrigger "language" triggerName (Web.text "Language") (Just (Web.GlobalCssClass "language-trigger"))
          renderDialogRoute :: Text -> Web.SafeUrl
          renderDialogRoute route =
            Web.requiredSafeUrlOrDie
              "test dialog route must be safe"
              (Web.mkSafeUrl ("/" <> route))
          props =
            Web.DialogControlProps
              (Web.literalElementId "language-dialog")
              (Web.literalElementId "language-heading")
              (Web.text "Choose a language")
              (Web.literalElementId "language-en")
              trigger
              [Web.element Web.anchorTag [Web.elementId (Web.literalElementId "language-en"), Web.href "/en/language"] [Web.text "English"]]
              closeName
              (Just (Web.GlobalCssClass "language-dialog"))
              (Just (Web.GlobalCssClass "language-close"))
          rendered = Web.renderHtml (Web.dialogControl renderDialogRoute props)
          unstyledRendered =
            Web.renderHtml
              ( Web.dialogControl
                  renderDialogRoute
                  props
                    { Web.dialogTrigger = trigger {Web.dialogTriggerClass = Nothing},
                      Web.dialogClass = Nothing,
                      Web.dialogCloseClass = Nothing
                    }
              )
      expectAll
        ( (rendered `shouldSatisfy` Text.isInfixOf "<a href=\"/language\" aria-label=\"Language\" aria-haspopup=\"dialog\" aria-controls=\"language-dialog\" aria-expanded=\"false\"")
            :| [ rendered `shouldSatisfy` Text.isInfixOf "<dialog id=\"language-dialog\" aria-labelledby=\"language-heading\" data-harch-dialog-root data-harch-dialog-initial-focus-id=\"language-en\"",
                 rendered `shouldSatisfy` Text.isInfixOf "<button type=\"button\" aria-label=\"Close language picker\" data-harch-dialog-close",
                 Web.accessibleNameText triggerName `shouldBe` "Language",
                 Web.mkAccessibleName "   " `shouldBe` Nothing,
                 triggerName `shouldNotBe` otherName,
                 length (show triggerName) + length (showList [triggerName] "") `shouldSatisfy` (> 0),
                 trigger `shouldBe` Web.DialogLinkTrigger "language" triggerName (Web.text "Language") (Just (Web.GlobalCssClass "language-trigger")),
                 trigger `shouldNotBe` trigger {Web.dialogTriggerRoute = "other-language"},
                 props `shouldNotBe` props {Web.dialogClass = Nothing},
                 length (show trigger) + length (showList [trigger] "") `shouldSatisfy` (> 0),
                 length (show props) + length (showList [props] "") `shouldSatisfy` (> 0),
                 unstyledRendered `shouldSatisfy` (not . Text.isInfixOf "class=")
               ]
        )
      evaluate (Web.requiredAccessibleNameOrDie "test name" Nothing `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "test name" `Text.isInfixOf` Text.pack message

    it "covers every method helper, empty codec, and public action result values" $ do
      let methodCodec :: Action.ActionCodec Text TestContext () Text
          methodCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action "get" (Action.get "/get") (pure "get"),
                  Action.action "put" (Action.put "/put") (pure "put"),
                  Action.action "patch" (Action.patch "/patch") (pure "patch"),
                  Action.action "delete" (Action.delete "/delete") (pure "delete"),
                  Action.action "dynamic" (Action.methodAt Action.ActionPost "/dynamic" (\actionContext -> testContextPathPrefix actionContext <> "/dynamic")) (Action.exactlyOne (Action.formField "name" Action.textValue))
                ]
          singleCodec :: Action.ActionCodec Text TestContext () Text
          singleCodec = fromRight (error "invalid single action codec") (Action.singleActionCodec "single" (Action.post "/single") (pure "single"))
          explicitSingleMetadata :: EndpointMetadata.EndpointMetadata ()
          explicitSingleMetadata =
            EndpointMetadata.mkEndpointMetadata
              (fromRight (error "invalid endpoint name") (EndpointMetadata.mkEndpointName "action.explicit-single"))
              (fromRight (error "invalid route template") (EndpointMetadata.mkRouteTemplate "/explicit-single"))
              EndpointMetadata.ActionEndpoint
              EndpointMetadata.AllowUnauthenticated
          explicitSingleCodec :: Action.ActionCodec Text TestContext () Text
          explicitSingleCodec = Action.singleActionCodecWithMetadata "explicit-single" (Action.post "/explicit-single") explicitSingleMetadata (pure "explicit-single")
          rootCodec :: Action.ActionCodec Text TestContext () Text
          rootCodec =
            fromRight (error "invalid root action codec") $
              Action.actionCodec [Action.action "root" (Action.post "/") (pure "root")]
          payload methodValue path fields =
            Action.ClientActionPayload
              { Action.clientActionMethod = methodValue,
                Action.clientActionPath = path,
                Action.clientActionFields = fields,
                Action.clientActionCsrfToken = Just "csrf",
                Action.clientActionIdempotencyKey = Nothing,
                Action.clientActionPayloadContext = defaultContext {testContextPathPrefix = "/app"}
              }
      expectAll
        ( (map Action.actionMethodText [Action.ActionGet, Action.ActionPost, Action.ActionPut, Action.ActionPatch, Action.ActionDelete] `shouldBe` ["GET", "POST", "PUT", "PATCH", "DELETE"])
            :| [ Action.decodeAction methodCodec (payload "GET" "/get" []) `shouldBe` Action.DecodedClientAction "get",
                 Action.decodeAction methodCodec (payload "PUT" "/put" []) `shouldBe` Action.DecodedClientAction "put",
                 Action.decodeAction methodCodec (payload "PATCH" "/patch" []) `shouldBe` Action.DecodedClientAction "patch",
                 Action.decodeAction methodCodec (payload "DELETE" "/delete" []) `shouldBe` Action.DecodedClientAction "delete",
                 Action.actionPath methodCodec (defaultContext {testContextPathPrefix = "/app"}) "dynamic" `shouldBe` Just "/app/dynamic",
                 Action.decodeAction methodCodec (payload "POST" "/app/dynamic" [("name", "Ada")]) `shouldBe` Action.DecodedClientAction "Ada",
                 Action.actionPath singleCodec defaultContext "single" `shouldBe` Just "/single",
                 Action.decodeAction singleCodec (payload "POST" "/single" []) `shouldBe` Action.DecodedClientAction "single",
                 Action.actionPath explicitSingleCodec defaultContext "explicit-single" `shouldBe` Just "/explicit-single",
                 Action.decodeAction explicitSingleCodec (payload "POST" "/explicit-single" []) `shouldBe` Action.DecodedClientAction "explicit-single",
                 Action.actionEndpointMetadata explicitSingleCodec defaultContext "POST" "/explicit-single" `shouldBe` Just explicitSingleMetadata,
                 Action.staticActionEndpointMetadata explicitSingleCodec "POST" "/explicit-single" `shouldBe` Just explicitSingleMetadata,
                 Action.actionEndpointMetadata methodCodec (defaultContext {testContextPathPrefix = "/app"}) "POST" "/app/dynamic"
                   `shouldBe` Just
                     ( EndpointMetadata.mkEndpointMetadata
                         (fromRight (error "invalid endpoint name") (EndpointMetadata.mkEndpointName "action.post.dynamic"))
                         (fromRight (error "invalid route template") (EndpointMetadata.mkRouteTemplate "/dynamic"))
                         EndpointMetadata.ActionEndpoint
                         EndpointMetadata.RequireAuthenticated
                     ),
                 Action.actionEndpointMetadata rootCodec defaultContext "POST" "/"
                   `shouldBe` Just
                     ( EndpointMetadata.mkEndpointMetadata
                         (fromRight (error "invalid endpoint name") (EndpointMetadata.mkEndpointName "action.post.root"))
                         (fromRight (error "invalid route template") (EndpointMetadata.mkRouteTemplate "/"))
                         EndpointMetadata.ActionEndpoint
                         EndpointMetadata.RequireAuthenticated
                     ),
                 Action.staticActionEndpointMetadata methodCodec "POST" "/app/dynamic" `shouldBe` Nothing,
                 Action.decodeAction (Action.emptyActionCodec :: Action.ActionCodec Text TestContext () Text) (payload "POST" "/missing" []) `shouldBe` Action.UnrecognizedClientAction,
                 Action.DuplicateActionField "name" /= Action.InvalidActionField "name" `shouldBe` True,
                 Action.DuplicateActionEndpoint Action.ActionPost "/same" /= Action.DuplicateActionEndpoint Action.ActionGet "/same" `shouldBe` True
               ]
        )
      case (Action.singleActionCodec "invalid" (Action.post "not-a-route") (pure "invalid") :: Either Action.ActionCodecError (Action.ActionCodec Text TestContext () Text)) of
        Left codecError -> codecError `shouldBe` Action.InvalidActionEndpointMetadata EndpointMetadata.InvalidRouteTemplate
        Right _ -> expectationFailure "expected invalid single action codec"

    it "keeps action protocol equality total for every constructor" $ do
      let methods = [Action.ActionGet, Action.ActionPost, Action.ActionPut, Action.ActionPatch, Action.ActionDelete]
          idempotencyKey = "retry-1"
          payload = Action.ClientActionPayload "POST" "/action" [("name", "Ada")] (Just "csrf") Nothing defaultContext
          payloads =
            [ payload,
              payload {Action.clientActionMethod = "GET"},
              payload {Action.clientActionPath = "/other"},
              payload {Action.clientActionFields = []},
              payload {Action.clientActionCsrfToken = Nothing},
              payload {Action.clientActionIdempotencyKey = Just idempotencyKey},
              payload {Action.clientActionPayloadContext = spanishContext}
            ]
          parseErrors = [Action.MissingActionField "name", Action.DuplicateActionField "name", Action.InvalidActionField "name"]
          decodedResults :: [Action.ClientActionDecodeResult Text]
          decodedResults =
            [ Action.DecodedClientAction "saved",
              Action.UnrecognizedClientAction,
              Action.MethodNotAllowedClientAction (Action.ActionGet :| [Action.ActionPost]),
              Action.MalformedClientAction (Action.MissingActionField "name" :| [Action.InvalidActionField "name"]),
              Action.InvalidClientActionDecoder
            ]
      expectAll
        ( (methods /= reverse methods `shouldBe` True)
            :| [ sum [fromEnum (left == right) | left <- methods, right <- methods] `shouldBe` length methods,
                 sum [fromEnum (left /= right) | left <- methods, right <- methods] `shouldBe` length methods * (length methods - 1),
                 sum [length (show methodValue) + length (showList [methodValue] "") | methodValue <- methods] `shouldSatisfy` (> 0),
                 sum [fromEnum (left == right) | left <- payloads, right <- payloads] `shouldBe` length payloads,
                 sum [fromEnum (left /= right) | left <- payloads, right <- payloads] `shouldBe` length payloads * (length payloads - 1),
                 sum [length (show payloadValue) + length (showList [payloadValue] "") | payloadValue <- payloads] `shouldSatisfy` (> 0),
                 map (== Action.InvalidActionField "name") parseErrors `shouldBe` [False, False, True],
                 sum [fromEnum (left == right) | left <- parseErrors, right <- parseErrors] `shouldBe` length parseErrors,
                 sum [fromEnum (left /= right) | left <- parseErrors, right <- parseErrors] `shouldBe` length parseErrors * (length parseErrors - 1),
                 sum [length (show parseError) + length (showList [parseError] "") | parseError <- parseErrors] `shouldSatisfy` (> 0),
                 decodedResults /= reverse decodedResults `shouldBe` True,
                 sum [fromEnum (left == right) | left <- decodedResults, right <- decodedResults] `shouldBe` length decodedResults,
                 sum [fromEnum (left /= right) | left <- decodedResults, right <- decodedResults] `shouldBe` length decodedResults * (length decodedResults - 1),
                 sum [length (show decodedResult) + length (showList [decodedResult] "") | decodedResult <- decodedResults] `shouldSatisfy` (> 0),
                 length (show (Action.DuplicateActionEndpoint Action.ActionPost "/same")) `shouldSatisfy` (> 0),
                 length (showList [Action.DuplicateActionEndpoint Action.ActionPost "/same"] "") `shouldSatisfy` (> 0)
               ]
        )

    it "makes static helpers, recovery decoders, and method negotiation observable through the codec" $ do
      let staticCodec :: Action.ActionCodec Text TestContext () Text
          staticCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action "get" (Action.get "/get") (pure "get"),
                  Action.action "post" (Action.post "/post") (pure "post"),
                  Action.action "put" (Action.put "/put") (pure "put"),
                  Action.action "patch" (Action.patch "/patch") (pure "patch"),
                  Action.action "delete" (Action.delete "/delete") (pure "delete")
                ]
          duplicateMethodCodec :: Action.ActionCodec Text TestContext () Text
          duplicateMethodCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action "first-get" (Action.getAt "/first-get" (const "/same")) (pure "first-get"),
                  Action.action "second-get" (Action.getAt "/second-get" (const "/same")) (pure "second-get"),
                  Action.action "post" (Action.postAt "/post" (const "/same")) (pure "post"),
                  Action.action "put" (Action.putAt "/put" (const "/same")) (pure "put")
                ]
          dynamicMethodCodec :: Action.ActionCodec Text TestContext () Text
          dynamicMethodCodec =
            fromRight (error "invalid dynamic action codec") $
              Action.actionCodec
                [ Action.action "put" (Action.putAt "/put" (const "/put")) (pure "put"),
                  Action.action "patch" (Action.patchAt "/patch" (const "/patch")) (pure "patch"),
                  Action.action "delete" (Action.deleteAt "/delete" (const "/delete")) (pure "delete")
                ]
          optionalCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/optional") (Action.optional (Action.formField "name" (Action.parseField nonEmptyValue)))]
          defaultCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/default") (Action.singleOrDefault "guest" (Action.formField "name" (Action.parseField nonEmptyValue)))]
          actionPayload methodValue path fields =
            Action.ClientActionPayload methodValue path fields Nothing Nothing defaultContext
          nonEmptyValue fieldText = if Text.null fieldText then Nothing else Just fieldText
      expectAll
        ( (map (Action.actionPath staticCodec defaultContext) ["get", "post", "put", "patch", "delete"] `shouldBe` map Just ["/get", "/post", "/put", "/patch", "/delete"])
            :| [ map (Action.actionMethod staticCodec) ["get", "post", "put", "patch", "delete"] `shouldBe` map Just [Action.ActionGet, Action.ActionPost, Action.ActionPut, Action.ActionPatch, Action.ActionDelete],
                 Action.decodeAction duplicateMethodCodec (actionPayload "PATCH" "/same" []) `shouldBe` Action.MethodNotAllowedClientAction (Action.ActionGet :| [Action.ActionPost, Action.ActionPut]),
                 show (Action.decodeAction duplicateMethodCodec (actionPayload "PATCH" "/same" [])) `shouldBe` "MethodNotAllowedClientAction (ActionGet :| [ActionPost,ActionPut])",
                 map (Action.actionPath dynamicMethodCodec defaultContext) ["put", "patch", "delete"] `shouldBe` map Just ["/put", "/patch", "/delete"],
                 Action.decodeAction optionalCodec (actionPayload "POST" "/optional" [("name", "Ada")]) `shouldBe` Action.DecodedClientAction (Just "Ada"),
                 Action.decodeAction optionalCodec (actionPayload "POST" "/optional" [("name", "")]) `shouldBe` Action.MalformedClientAction (Action.InvalidActionField "name" :| []),
                 Action.decodeAction optionalCodec (actionPayload "POST" "/optional" [("name", "Ada"), ("name", "Grace")]) `shouldBe` Action.MalformedClientAction (Action.DuplicateActionField "name" :| []),
                 Action.decodeAction defaultCodec (actionPayload "POST" "/default" [("name", "Ada")]) `shouldBe` Action.DecodedClientAction "Ada",
                 Action.decodeAction defaultCodec (actionPayload "POST" "/default" [("name", "")]) `shouldBe` Action.MalformedClientAction (Action.InvalidActionField "name" :| []),
                 Action.decodeAction defaultCodec (actionPayload "POST" "/default" [("name", "Ada"), ("name", "Grace")]) `shouldBe` Action.MalformedClientAction (Action.DuplicateActionField "name" :| [])
               ]
        )

    it "returns explicit undeclared-target results and renders a PUT-declared action safely through its native fallback" $ do
      let emptyCodec :: Action.ActionCodec Text TestContext () Text
          emptyCodec = Action.emptyActionCodec
          unsupportedCodec :: Action.ActionCodec Text TestContext () Text
          unsupportedCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action "put" (Action.put "/put") (pure "put")]
          nativeAttributes =
            defaultActionFormAttributes
              { actionFormCapabilities = [NativeFallback (NativeActionFallback "/native" FormPost testFallbackCsrfToken)]
              }
      let undeclaredForm = renderHtml (renderActionForm (actionForm emptyCodec defaultContext "missing" nativeAttributes [text "Preserved input"]))
          unsupportedForm = renderHtml (renderActionForm (actionForm unsupportedCodec defaultContext "put" nativeAttributes []))
      expectAll
        ( (Action.actionPath emptyCodec defaultContext "missing" `shouldBe` Nothing)
            :| [ Action.actionMethod emptyCodec "missing" `shouldBe` Nothing,
                 Text.isInfixOf "data-harch-action-configuration-error" undeclaredForm `shouldBe` True,
                 Text.isInfixOf "Preserved input" undeclaredForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action=\"true\"" undeclaredForm `shouldBe` False,
                 Text.isInfixOf "data-harch-action-method=\"put\"" unsupportedForm `shouldBe` True,
                 Text.isInfixOf "action=\"/native\" method=\"post\"" unsupportedForm `shouldBe` True
               ]
        )

    it "matches declared paths and methods, and reports unknown paths or the allowed methods precisely" $ do
      let invalidDecoder :: Action.ActionDecoder Text
          invalidDecoder = Compose (const (Compose ([], Nothing)))
          invalidCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/invalid") invalidDecoder]
          errorWithValueDecoder :: Action.ActionDecoder Text
          errorWithValueDecoder = Compose (const (Compose ([Action.InvalidActionField "email"], Just "ignored")))
          errorWithValueCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/error-with-value") errorWithValueDecoder]
          payload methodValue path =
            Action.ClientActionPayload
              { Action.clientActionMethod = methodValue,
                Action.clientActionPath = path,
                Action.clientActionFields = [("email", "ada@example.test")],
                Action.clientActionCsrfToken = Nothing,
                Action.clientActionIdempotencyKey = Nothing,
                Action.clientActionPayloadContext = defaultContext
              }
      Action.decodeAction testActionCodec (payload "POST" "/known") `shouldBe` Action.DecodedClientAction "save:ada@example.test"
      Action.decodeAction testActionCodec (payload "GET" "/known") `shouldBe` Action.DecodedClientAction "read"
      Action.decodeAction testActionCodec (payload "PUT" "/known") `shouldBe` Action.MethodNotAllowedClientAction (Action.ActionPost :| [Action.ActionGet])
      Action.decodeAction testActionCodec (payload "POST" "/missing") `shouldBe` Action.UnrecognizedClientAction
      Action.decodeAction invalidCodec (payload "POST" "/invalid") `shouldBe` Action.InvalidClientActionDecoder
      Action.decodeAction errorWithValueCodec (payload "POST" "/error-with-value") `shouldBe` Action.MalformedClientAction (Action.InvalidActionField "email" :| [])

    it "accumulates field errors deterministically and supports required, optional, defaulted, and parsed values" $ do
      let validationCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action
                    ()
                    (Action.post "/validate")
                    ( (,)
                        <$> Action.required (Action.formField "email" Action.textValue)
                        <*> Action.required (Action.formField "code" (Action.parseField (\fieldText -> if fieldText == "valid" then Just fieldText else Nothing)))
                    )
                ]
          validationPayload fields =
            Action.ClientActionPayload
              { Action.clientActionMethod = "POST",
                Action.clientActionPath = "/validate",
                Action.clientActionFields = fields,
                Action.clientActionCsrfToken = Nothing,
                Action.clientActionIdempotencyKey = Nothing,
                Action.clientActionPayloadContext = ()
              }
          defaultCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/default") (Action.singleOrDefault "guest" (Action.formField "name" Action.textValue))]
          optionalCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/optional") (Action.optional (Action.formField "name" Action.textValue))]
      Action.decodeAction validationCodec (validationPayload [("email", "one"), ("email", "two")])
        `shouldBe` Action.MalformedClientAction (Action.DuplicateActionField "email" :| [Action.MissingActionField "code"])
      Action.decodeAction validationCodec (validationPayload [("email", "one"), ("code", "invalid")])
        `shouldBe` Action.MalformedClientAction (Action.InvalidActionField "code" :| [])
      Action.decodeAction validationCodec (validationPayload [("email", "one"), ("code", "valid")])
        `shouldBe` Action.DecodedClientAction ("one", "valid")
      Action.decodeAction validationCodec (validationPayload [("code", "valid")])
        `shouldBe` Action.MalformedClientAction (Action.MissingActionField "email" :| [])
      Action.decodeAction defaultCodec ((validationPayload []) {Action.clientActionPath = "/default"})
        `shouldBe` Action.DecodedClientAction "guest"
      Action.decodeAction optionalCodec ((validationPayload []) {Action.clientActionPath = "/optional"})
        `shouldBe` Action.DecodedClientAction Nothing

    it "rejects ambiguous endpoint declarations during codec construction" $ do
      case Action.actionCodec [Action.action () (Action.post "/duplicate") (pure ()), Action.action () (Action.post "/duplicate") (pure ())] of
        Left codecError -> do
          codecError `shouldBe` Action.DuplicateActionEndpoint Action.ActionPost "/duplicate"
          show codecError `shouldBe` "DuplicateActionEndpoint ActionPost \"/duplicate\""
        Right _ -> expectationFailure "expected duplicate endpoint construction to fail"

    it "rejects a later duplicate declaration after checking every earlier endpoint" $ do
      let codec =
            Action.actionCodec
              [ Action.action "first" (Action.post "/first") (pure "first"),
                Action.action "second" (Action.post "/second") (pure "second"),
                Action.action "duplicate" (Action.post "/first") (pure "duplicate")
              ] ::
              Either Action.ActionCodecError (Action.ActionCodec Text TestContext () Text)
      case codec of
        Left codecError -> do
          codecError `shouldBe` Action.DuplicateActionEndpoint Action.ActionPost "/first"
          show codecError `shouldBe` "DuplicateActionEndpoint ActionPost \"/first\""
        Right _ -> expectationFailure "expected a later duplicate endpoint declaration to fail"

    it "constructs an empty action codec without a duplicate endpoint" $
      case Action.actionCodec [] :: Either Action.ActionCodecError (Action.ActionCodec Text TestContext () Text) of
        Left _ -> expectationFailure "an empty codec must not have a duplicate endpoint"
        Right _ -> pure ()
