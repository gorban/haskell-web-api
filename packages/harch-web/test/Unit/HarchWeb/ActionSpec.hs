{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
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
import Data.Text qualified as Text (isInfixOf, null)
import Data.Text.Encoding qualified as TextEncoding (encodeUtf8)
import HarchWeb (ActionCapability (ConditionalLeaveConfirmation, HandlerSafeRetry, IdempotentMutationRetry, NativeFallback), ActionFormAttributes (actionFormCapabilities), ActionIdempotency (actionIdempotencyKey), ActionRecoveryCopy (actionCancelCopy, actionCancelledCopy, actionDelayedCopy, actionPendingCopy, actionReadyCopy, actionRecoverableCopy, actionRetryCopy), FormMethod (FormGet, FormPost), NativeActionFallback (NativeActionFallback, nativeActionFallbackCsrfToken, nativeActionFallbackMethod, nativeActionFallbackPath), actionForm, actionIdempotency, defaultActionFormAttributes, defaultActionRecoveryCopy, defaultCaptureKernelByteBudget, defaultCaptureKernelScript, renderActionForm, renderHtml, staticActionForm, text)
import HarchWeb.Action qualified as Action (ActionCodec, ActionCodecError (..), ActionDecoder, ActionMethod (ActionDelete, ActionGet, ActionPatch, ActionPost, ActionPut), ClientActionDecodeResult (..), ClientActionParseError (DuplicateActionField, InvalidActionField, MissingActionField), ClientActionPayload (ClientActionPayload, clientActionCsrfToken, clientActionFields, clientActionIdempotencyKey, clientActionMethod, clientActionPath, clientActionPayloadContext), action, actionCodec, actionMethod, actionMethodText, actionPath, decodeAction, delete, deleteAt, emptyActionCodec, exactlyOne, formField, get, getAt, methodAt, optional, parseField, patch, patchAt, post, postAt, put, putAt, required, singleActionCodec, singleOrDefault, staticActionPath, textValue)
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
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

spec = do
  describe "HarchWeb.Action" $ do
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
                 Text.isInfixOf "data-harch-action-cancel" renderedForm `shouldBe` True
               ]
        )

    it "renders a declaration-proven static action without inventing request context" $ do
      let staticCodec :: Action.ActionCodec Text TestContext Text
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

    it "renders only explicitly declared native fallback and recovery capabilities" $ do
      let idempotency = fromMaybe (error "expected a valid test idempotency key") (actionIdempotency "mutation-1")
          nativeFallback = NativeActionFallback "/native-subscribe" FormPost "csrf-token"
          capabilities = [HandlerSafeRetry, ConditionalLeaveConfirmation, IdempotentMutationRetry idempotency, NativeFallback nativeFallback]
          nativeAttributes = defaultActionFormAttributes {actionFormCapabilities = capabilities}
          nativeForm = renderHtml (renderActionForm (actionForm testActionCodec defaultContext "save" nativeAttributes []))
          nativeGetForm = renderHtml (renderActionForm (actionForm testActionCodec defaultContext "read" nativeAttributes []))
      expectAll
        ( (Text.isInfixOf "method=\"post\"" nativeForm `shouldBe` True)
            :| [ Text.isInfixOf "method=\"post\"" nativeGetForm `shouldBe` True,
                 Text.isInfixOf "action=\"/native-subscribe\" method=\"post\"" nativeForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-path=\"/known\"" nativeForm `shouldBe` True,
                 Text.isInfixOf "name=\"_harch_csrf\" value=\"csrf-token\"" nativeForm `shouldBe` True,
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
                 nativeActionFallbackCsrfToken nativeFallback `shouldBe` "csrf-token",
                 length (show (NativeFallback nativeFallback)) `shouldSatisfy` (> 0),
                 showList capabilities "" `shouldSatisfy` (not . null),
                 sum [fromEnum (left == right) | left <- capabilities, right <- capabilities] `shouldBe` length capabilities,
                 sum [fromEnum (left /= right) | left <- capabilities, right <- capabilities] `shouldBe` length capabilities * (length capabilities - 1)
               ]
        )

    it "keeps FormMethod, NativeActionFallback, and ActionIdempotency comparable and printable, and renders a GET native fallback" $ do
      let getFallback = NativeActionFallback "/native-get" FormGet "csrf-token"
          postFallback = NativeActionFallback "/native-post" FormPost "csrf-token"
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

    it "covers every method helper, empty codec, and public action result values" $ do
      let methodCodec :: Action.ActionCodec Text TestContext Text
          methodCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action "get" (Action.get "/get") (pure "get"),
                  Action.action "put" (Action.put "/put") (pure "put"),
                  Action.action "patch" (Action.patch "/patch") (pure "patch"),
                  Action.action "delete" (Action.delete "/delete") (pure "delete"),
                  Action.action "dynamic" (Action.methodAt Action.ActionPost "/dynamic" (\actionContext -> testContextPathPrefix actionContext <> "/dynamic")) (Action.exactlyOne (Action.formField "name" Action.textValue))
                ]
          singleCodec :: Action.ActionCodec Text TestContext Text
          singleCodec = Action.singleActionCodec "single" (Action.post "/single") (pure "single")
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
                 Action.decodeAction (Action.emptyActionCodec :: Action.ActionCodec Text TestContext Text) (payload "POST" "/missing" []) `shouldBe` Action.UnrecognizedClientAction,
                 Action.DuplicateActionField "name" /= Action.InvalidActionField "name" `shouldBe` True,
                 Action.DuplicateActionEndpoint Action.ActionPost "/same" /= Action.DuplicateActionEndpoint Action.ActionGet "/same" `shouldBe` True
               ]
        )

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
      let staticCodec :: Action.ActionCodec Text TestContext Text
          staticCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action "get" (Action.get "/get") (pure "get"),
                  Action.action "post" (Action.post "/post") (pure "post"),
                  Action.action "put" (Action.put "/put") (pure "put"),
                  Action.action "patch" (Action.patch "/patch") (pure "patch"),
                  Action.action "delete" (Action.delete "/delete") (pure "delete")
                ]
          duplicateMethodCodec :: Action.ActionCodec Text TestContext Text
          duplicateMethodCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec
                [ Action.action "first-get" (Action.getAt "/first-get" (const "/same")) (pure "first-get"),
                  Action.action "second-get" (Action.getAt "/second-get" (const "/same")) (pure "second-get"),
                  Action.action "post" (Action.postAt "/post" (const "/same")) (pure "post"),
                  Action.action "put" (Action.putAt "/put" (const "/same")) (pure "put")
                ]
          dynamicMethodCodec :: Action.ActionCodec Text TestContext Text
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
      let emptyCodec :: Action.ActionCodec Text TestContext Text
          emptyCodec = Action.emptyActionCodec
          unsupportedCodec :: Action.ActionCodec Text TestContext Text
          unsupportedCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action "put" (Action.put "/put") (pure "put")]
          nativeAttributes =
            defaultActionFormAttributes
              { actionFormCapabilities = [NativeFallback (NativeActionFallback "/native" FormPost "csrf-token")]
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
              Either Action.ActionCodecError (Action.ActionCodec Text TestContext Text)
      case codec of
        Left codecError -> do
          codecError `shouldBe` Action.DuplicateActionEndpoint Action.ActionPost "/first"
          show codecError `shouldBe` "DuplicateActionEndpoint ActionPost \"/first\""
        Right _ -> expectationFailure "expected a later duplicate endpoint declaration to fail"

    it "constructs an empty action codec without a duplicate endpoint" $
      case Action.actionCodec [] :: Either Action.ActionCodecError (Action.ActionCodec Text TestContext Text) of
        Left _ -> expectationFailure "an empty codec must not have a duplicate endpoint"
        Right _ -> pure ()
