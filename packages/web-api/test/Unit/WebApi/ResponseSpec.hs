{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Config (defaultAppConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), PageRepository (..), buildSeededPageRepository, defaultDatabaseSeed, defaultPageRepository)
import WebApi.Page (renderPage, renderPageFromRouteData)
import WebApi.Postgres.Testing (buildPostgresPageRepositoryWithRunner)
import WebApi.Response (selectResponse, selectResponseWithDatabase)
import WebApi.RouteData (RouteDataResult (..), SecondRouteData (..))

spec = do
  describe "selectResponse" $ do
    it "resolves page routes to page responses that still flow through the shared shell" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      selectResponse defaultAppConfig secondRequest `shouldReturn` HarchWeb.RenderedPage renderedPage

    it "attaches typed database operations to postgres-backed page responses" $ do
      let postgresRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded second summary."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      successfulPostgresResult "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresPageRepositoryWithRunner postgresRunner postgresTestConfig
      let renderedSecondPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              ( SecondRouteDataResult
                  ( Right
                      SecondRouteData
                        { secondRouteSummary = "Loaded second summary.",
                          secondRouteHighlights = ["Fast SSR", "Shared route data"]
                        }
                  )
              )
      fmap stripVolatilePageResult (selectResponseWithDatabase defaultAppConfig postgresEffect secondRequest)
        `shouldReturn` HarchWeb.RenderedPageWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = expectedSecondDatabaseOperations
            }
          renderedSecondPage
    it "keeps not-found handling in the page-result boundary" $ do
      renderedPage <- renderPage defaultAppConfig notFoundRequest
      selectResponse defaultAppConfig notFoundRequest `shouldReturn` HarchWeb.RenderedPage renderedPage

    it "maps required second-page failures into explicit HTML 500 responses" $ do
      let failingDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
          renderedPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              (SecondRouteDataResult (Left (SecondPageDataError "seed unavailable")))
      response <- selectResponseWithDatabase defaultAppConfig failingDatabaseEffect secondRequest
      response
        `shouldBe` HarchWeb.RenderedPageWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status500,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "error.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.failure.code",
                      Observability.attributeValue = Observability.TextAttribute "database.second-page-data"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "page"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required second-page page response: SecondPageDataError \"seed unavailable\""],
              HarchWeb.responseDatabaseOperations = []
            }
          renderedPage
      case response of
        HarchWeb.RenderedPageWithMetadata metadata _ ->
          case HarchWeb.responseObservabilityAttributes metadata of
            errorType : failureCode : _ -> do
              Observability.attributeValue errorType
                `shouldBe` Observability.TextAttribute "SecondPageDataError"
              Observability.attributeValue failureCode
                `shouldBe` Observability.TextAttribute "database.second-page-data"
            _ -> expectationFailure "expected error-type and failure-code diagnostics"
        _ -> expectationFailure "expected a page response with failure diagnostics"

    it "includes observed query diagnostics in required page failure responses" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          failingDatabaseEffect =
            defaultPageRepository
              { loadSecondPage =
                  \_ ->
                    pure
                      DatabaseResult
                        { databaseResultValue = Left (SecondPageDataError "summary unavailable"),
                          databaseResultOperations = [databaseOperation, databaseOperation {databaseOperationName = "load-second-page-highlights"}]
                        }
              }
      response <- selectResponseWithDatabase defaultAppConfig failingDatabaseEffect secondRequest
      case response of
        HarchWeb.RenderedPageWithMetadata metadata _ -> do
          HarchWeb.responseLogEntries metadata
            `shouldBe` ["Database failure while rendering required second-page page response after database operations [load-second-page-summary (SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;), load-second-page-highlights (SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;)]: SecondPageDataError \"summary unavailable\""]
          HarchWeb.responseDatabaseOperations metadata
            `shouldBe` [expectedDatabaseOperation "load-second-page-summary" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;", expectedDatabaseOperation "load-second-page-highlights" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"]
        _ -> expectationFailure "expected a page response with failure diagnostics"

    it "is deterministic for repeated requests" $ do
      firstResponse <- selectResponse defaultAppConfig notFoundRequest
      secondResponse <- selectResponse defaultAppConfig notFoundRequest
      firstResponse `shouldBe` secondResponse

stripVolatilePageResult :: HarchWeb.PageResult route context -> HarchWeb.PageResult route context
stripVolatilePageResult pageResult =
  case pageResult of
    HarchWeb.RenderedPage page -> HarchWeb.RenderedPage page
    HarchWeb.RenderedPageWithMetadata metadata page ->
      HarchWeb.RenderedPageWithMetadata (stripVolatileDatabaseTimingResponseBody metadata) page
