{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.Either (fromRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb

spec = do
  describe "security event contracts" $ do
    it "constructs root-owned route observation from declarations rather than request text" $ do
      invalidModuleName <- newIORef "Root/Web" >>= readIORef
      let observation = rootRouteObservation (requiredModuleName "root.web") (locale "es") (requiredEndpointName "account.login") (requiredRouteTemplate "/{locale}/public/login")
      expectAll
        ( (observedEndpointName observation `shouldBe` requiredEndpointName "account.login")
            :| [ observedMountChain observation `shouldBe` requiredModuleName "root.web" :| [],
                 observedRouteTemplate observation `shouldBe` requiredRouteTemplate "/{locale}/public/login",
                 observedLocale observation `shouldBe` locale "es",
                 moduleNameText (requiredModuleName "root.web") `shouldBe` "root.web",
                 moduleNameText (requiredModuleNameOrDie "root.web") `shouldBe` "root.web",
                 mkModuleName "" `shouldBe` Left EmptyModuleName,
                 mkModuleName invalidModuleName `shouldBe` Left InvalidModuleName
               ]
        )
      evaluate (requiredModuleNameOrDie "Root/Web")
        `shouldThrow` \case
          ErrorCall message -> message == "invalid module name declaration: InvalidModuleName"

    it "projects only static declaration facts and closed event classifications to telemetry" $ do
      let eventEnvelope =
            SecurityEventEnvelope
              { securityEventRoute = rootRouteObservation (requiredModuleName "root.web") (locale "account@example.test") (requiredEndpointName "account.login") (requiredRouteTemplate "/{locale}/public/login"),
                securityEventBody = AuthenticationEvaluated (AuthenticationEvent AuthenticationRejected (Just (requiredFailureCode "proof.rejected"))),
                securityEventRequirement = TelemetryBestEffort
              }
          telemetryEvent = projectTelemetryEvent eventEnvelope
          renderedTelemetry = Text.pack (show telemetryEvent)
      expectAll
        ( (telemetryEventEndpointName telemetryEvent `shouldBe` requiredEndpointName "account.login")
            :| [ telemetryEventRouteTemplate telemetryEvent `shouldBe` requiredRouteTemplate "/{locale}/public/login",
                 telemetryEventKind telemetryEvent `shouldBe` "authentication",
                 telemetryEventOutcome telemetryEvent `shouldBe` "rejected",
                 renderedTelemetry `shouldSatisfy` (not . Text.isInfixOf "account@example.test"),
                 renderedTelemetry `shouldSatisfy` (not . Text.isInfixOf "proof.rejected")
               ]
        )

    it "makes undelivered audit-required events explicit to the application transaction" $ do
      let deliveryFailure = mkEventDeliveryFailure (requiredFailureCode "audit.store-unavailable")
          eventEnvelope =
            SecurityEventEnvelope
              { securityEventRoute = rootRouteObservation (requiredModuleName "root.web") (locale "en") (requiredEndpointName "account.logout") (requiredRouteTemplate "/logout"),
                securityEventBody = SessionStateChanged (SessionEvent SessionRejected Nothing),
                securityEventRequirement = AuditRequired
              }
          delivery = SecurityEventDelivery (const (pure (SecurityEventUndelivered deliveryFailure)))
      deliverSecurityEvent delivery eventEnvelope `shouldReturn` SecurityEventUndelivered deliveryFailure

    it "attaches root-owned facts and reports an undelivered result to the health hook" $ do
      delivered <- newIORef []
      reportedFailures <- newIORef []
      let deliveryFailure = mkEventDeliveryFailure (requiredFailureCode "telemetry.store-unavailable")
          delivery =
            SecurityEventDelivery $ \eventEnvelope -> do
              modifyIORef' delivered (<> [eventEnvelope])
              pure (SecurityEventUndelivered deliveryFailure)
          eventRoot =
            SecurityEventRoot
              { securityEventRootModule = requiredModuleName "root.web",
                securityEventRootLocale = const (locale "account@example.test"),
                securityEventRootDelivery = delivery,
                securityEventRootUndelivered = \failure -> modifyIORef' reportedFailures (<> [failure])
              }
          eventBody = AuthenticationEvaluated (AuthenticationEvent AuthenticationMissing Nothing)
          sink = rootSecurityEventSink eventRoot (requiredEndpointName "account.login") (requiredRouteTemplate "/{locale}/public/login") ()
          expectedEnvelope =
            SecurityEventEnvelope
              { securityEventRoute = rootRouteObservation (requiredModuleName "root.web") (locale "account@example.test") (requiredEndpointName "account.login") (requiredRouteTemplate "/{locale}/public/login"),
                securityEventBody = eventBody,
                securityEventRequirement = TelemetryBestEffort
              }
      deliveryResult <- emitSecurityEvent sink TelemetryBestEffort eventBody
      expectAll
        ( (deliveryResult `shouldBe` SecurityEventUndelivered deliveryFailure)
            :| [ readIORef delivered `shouldReturn` [expectedEnvelope],
                 readIORef reportedFailures `shouldReturn` [deliveryFailure]
               ]
        )

    it "uses the matched context only through the root locale projection and accepts best-effort success" $ do
      delivered <- newIORef []
      let eventRoot =
            SecurityEventRoot
              { securityEventRootModule = requiredModuleName "root.web",
                securityEventRootLocale = locale,
                securityEventRootDelivery = SecurityEventDelivery (\eventEnvelope -> modifyIORef' delivered (<> [eventEnvelope]) >> pure SecurityEventDelivered),
                securityEventRootUndelivered = \_ -> expectationFailure "did not expect an undelivered event"
              }
          eventBody = SessionStateChanged (SessionEvent SessionEstablished Nothing)
          sink = rootSecurityEventSink eventRoot (requiredEndpointName "account.login") (requiredRouteTemplate "/login") "es"
          expectedEnvelope = SecurityEventEnvelope (rootRouteObservation (requiredModuleName "root.web") (locale "es") (requiredEndpointName "account.login") (requiredRouteTemplate "/login")) eventBody TelemetryBestEffort
      emitSecurityEvent sink TelemetryBestEffort eventBody `shouldReturn` SecurityEventDelivered
      readIORef delivered `shouldReturn` [expectedEnvelope]

    it "maps every closed event outcome without promoting a failure code to telemetry" $ do
      let routeObservation = rootRouteObservation (requiredModuleName "root.web") (locale "en") (requiredEndpointName "account.login") (requiredRouteTemplate "/login")
          envelope eventBody = SecurityEventEnvelope routeObservation eventBody TelemetryBestEffort
          failureCode = requiredFailureCode "identity.rejected"
          telemetryOutcomes =
            [ telemetryEventOutcome (projectTelemetryEvent (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationAnonymous Nothing)))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationEstablished Nothing)))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationMissing Nothing)))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationRejected (Just failureCode))))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationDependencyUnavailable (Just failureCode))))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (AuthorizationDenied (AuthorizationEvent failureCode)))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (SessionStateChanged (SessionEvent SessionEstablished Nothing)))),
              telemetryEventOutcome (projectTelemetryEvent (envelope (SessionStateChanged (SessionEvent SessionRejected (Just failureCode)))))
            ]
          telemetryKinds =
            [ telemetryEventKind (projectTelemetryEvent (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationAnonymous Nothing)))),
              telemetryEventKind (projectTelemetryEvent (envelope (AuthorizationDenied (AuthorizationEvent failureCode)))),
              telemetryEventKind (projectTelemetryEvent (envelope (SessionStateChanged (SessionEvent SessionEstablished Nothing))))
            ]
          successfulDelivery = SecurityEventDelivery (const (pure SecurityEventDelivered))
      expectAll
        ( (telemetryOutcomes `shouldBe` ["anonymous", "established", "missing", "rejected", "dependency-unavailable", "denied", "established", "rejected"])
            :| [ telemetryKinds `shouldBe` ["authentication", "authorization", "session"],
                 deliverSecurityEvent successfulDelivery (envelope (AuthenticationEvaluated (AuthenticationEvent AuthenticationAnonymous Nothing))) `shouldReturn` SecurityEventDelivered,
                 mkModuleName (Text.replicate 129 "a") `shouldBe` Left ModuleNameTooLong
               ]
        )

    it "keeps every event contract constructor comparable and printable" $ do
      let failureCode = requiredFailureCode "identity.rejected"
          otherFailureCode = requiredFailureCode "identity.unavailable"
          rootModule = requiredModuleName "root.web"
          otherModule = requiredModuleName "root.api"
          moduleErrors = [EmptyModuleName, ModuleNameTooLong, InvalidModuleName]
          authenticationOutcomes = [AuthenticationAnonymous, AuthenticationEstablished, AuthenticationMissing, AuthenticationRejected, AuthenticationDependencyUnavailable]
          sessionOutcomes = [SessionEstablished, SessionRejected]
          requirements = [TelemetryBestEffort, AuditRequired]
          eventFailure = mkEventDeliveryFailure failureCode
          otherEventFailure = mkEventDeliveryFailure otherFailureCode
          routeObservation = rootRouteObservation rootModule (locale "en") (requiredEndpointName "account.login") (requiredRouteTemplate "/login")
          otherRouteObservation = rootRouteObservation otherModule (locale "es") (requiredEndpointName "account.logout") (requiredRouteTemplate "/logout")
          authenticationEvent = AuthenticationEvent AuthenticationRejected (Just failureCode)
          anonymousAuthenticationEvent = AuthenticationEvent AuthenticationAnonymous Nothing
          authorizationEvent = AuthorizationEvent failureCode
          otherAuthorizationEvent = AuthorizationEvent otherFailureCode
          sessionEvent = SessionEvent SessionRejected (Just failureCode)
          establishedSessionEvent = SessionEvent SessionEstablished Nothing
          events = [AuthenticationEvaluated authenticationEvent, AuthorizationDenied authorizationEvent, SessionStateChanged sessionEvent]
          envelope = SecurityEventEnvelope routeObservation (AuthenticationEvaluated authenticationEvent) AuditRequired
          otherEnvelope = SecurityEventEnvelope otherRouteObservation (SessionStateChanged establishedSessionEvent) TelemetryBestEffort
          deliveries = [SecurityEventDelivered, SecurityEventUndelivered eventFailure]
          telemetryEvents = [projectTelemetryEvent envelope, projectTelemetryEvent otherEnvelope]
      expectAll
        ( (rootModule /= otherModule `shouldBe` True)
            :| [ compare rootModule rootModule `shouldBe` EQ,
                 compare rootModule otherModule `shouldBe` GT,
                 otherModule < rootModule `shouldBe` True,
                 rootModule <= rootModule `shouldBe` True,
                 rootModule > otherModule `shouldBe` True,
                 rootModule >= rootModule `shouldBe` True,
                 max rootModule otherModule `shouldBe` rootModule,
                 min rootModule otherModule `shouldBe` otherModule,
                 hasDerivedContract [rootModule, otherModule] `shouldBe` True,
                 hasDerivedContract moduleErrors `shouldBe` True,
                 hasDerivedContract [routeObservation, otherRouteObservation] `shouldBe` True,
                 hasDerivedContract authenticationOutcomes `shouldBe` True,
                 hasDerivedContract [authenticationEvent, anonymousAuthenticationEvent] `shouldBe` True,
                 hasDerivedContract [authorizationEvent, otherAuthorizationEvent] `shouldBe` True,
                 hasDerivedContract sessionOutcomes `shouldBe` True,
                 hasDerivedContract [sessionEvent, establishedSessionEvent] `shouldBe` True,
                 hasDerivedContract events `shouldBe` True,
                 hasDerivedContract requirements `shouldBe` True,
                 hasDerivedContract [eventFailure, otherEventFailure] `shouldBe` True,
                 hasDerivedContract [envelope, otherEnvelope] `shouldBe` True,
                 hasDerivedContract deliveries `shouldBe` True,
                 hasDerivedContract telemetryEvents `shouldBe` True,
                 securityEventRoute envelope `shouldBe` routeObservation,
                 securityEventBody envelope `shouldBe` AuthenticationEvaluated authenticationEvent,
                 securityEventRequirement envelope `shouldBe` AuditRequired,
                 eventFailure /= otherEventFailure `shouldBe` True
               ]
        )

requiredModuleName :: Text.Text -> ModuleName
requiredModuleName moduleNameValue = fromRight (error "invalid module name") (mkModuleName moduleNameValue)

requiredEndpointName :: Text.Text -> EndpointName
requiredEndpointName endpointNameValue = fromRight (error "invalid endpoint name") (mkEndpointName endpointNameValue)

requiredRouteTemplate :: Text.Text -> RouteTemplate
requiredRouteTemplate routeTemplateValue = fromRight (error "invalid route template") (mkRouteTemplate routeTemplateValue)

requiredFailureCode :: Text.Text -> SecurityFailureCode
requiredFailureCode failureCodeValue = fromRight (error "invalid failure code") (mkSecurityFailureCode failureCodeValue)

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)
    && sum [length (show item) + length (showList [item] "") | item <- values] > 0
