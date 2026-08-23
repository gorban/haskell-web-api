{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Config (defaultAppConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), PageRepository (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed, defaultPageRepository)
import WebApi.Page (renderPage, renderPageFromRouteData)
import WebApi.Postgres.Testing (buildPostgresPageRepositoryWithRunner)
import WebApi.Response (renderApiResponseFromRouteData, selectResponse, selectResponseWithDatabase)
import WebApi.Route (AppRequestContext (..), AppRoute (..))
import WebApi.RouteData (RouteDataResult (..), SecondRouteData (..))

spec = do
  describe "selectResponse" $ do
    it "resolves page routes to page responses that still flow through the shared shell" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      selectResponse defaultAppConfig secondRequest `shouldReturn` HarchWeb.PageResponse renderedPage

    it "resolves API-only routes to explicit status, content type, and body values" $ do
      selectResponse defaultAppConfig apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"en\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = []
            }
      selectResponse defaultAppConfig apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = []
            }

    it "keeps API payload rendering locale-aware without touching page routing" $ do
      selectResponse defaultAppConfig spanishApiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"es\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = []
            }
      selectResponse defaultAppConfig spanishApiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = []
            }

    it "attaches typed database operations to postgres-backed page and API responses" $ do
      let postgresRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'home'" sql ->
                      successfulPostgresResult "Loaded home summary."
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
      fmap stripVolatileDatabaseTimingResponse (selectResponseWithDatabase defaultAppConfig postgresEffect secondRequest)
        `shouldReturn` HarchWeb.PageResponseWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = expectedSecondDatabaseOperations
            }
          renderedSecondPage
      fmap stripVolatileDatabaseTimingResponse (selectResponseWithDatabase defaultAppConfig postgresEffect apiSecondRequest)
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Loaded second summary.\",\"highlights\":[\"Fast SSR\",\"Shared route data\"]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = expectedSecondDatabaseOperations
            }

    it "keeps not-found handling consistent across page and non-page responses" $ do
      renderedPage <- renderPage defaultAppConfig notFoundRequest
      selectResponse defaultAppConfig notFoundRequest `shouldReturn` HarchWeb.PageResponse renderedPage
      selectResponse defaultAppConfig apiNotFoundRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status404,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"not-found\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = []
            }

    it "maps shared second-page load failures into explicit API error responses" $
      selectResponseWithDatabase
        defaultAppConfig
        ( buildSeededPageRepository
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status503,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
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
                      Observability.attributeValue = Observability.TextAttribute "api"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required second-page api response: SecondPageDataError \"seed unavailable\""],
              HarchWeb.responseDatabaseOperations = []
            }

    it "omits volatile database timing fields when a database effect reports untimed operations" $ do
      let untimedOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          untimedDatabaseEffect =
            defaultPageRepository
              { loadSecondPage =
                  \_ ->
                    pure
                      DatabaseResult
                        { databaseResultValue =
                            Right
                              SecondPageData
                                { secondPageDataSummary = "Untimed summary.",
                                  secondPageDataHighlights = []
                                },
                          databaseResultOperations = [untimedOperation]
                        }
              }
      response <- selectResponseWithDatabase defaultAppConfig untimedDatabaseEffect apiSecondRequest
      case response of
        HarchWeb.BodyResponse responseBody ->
          HarchWeb.responseDatabaseOperations responseBody
            `shouldBe` [expectedDatabaseOperation "load-second-page-summary" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"]
        _ ->
          expectationFailure "expected API response body for untimed database operation"

    it "adds safe database operation details to postgres-backed failure diagnostics" $ do
      let failingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded second summary."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      failingPostgresResult "highlights unavailable"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresPageRepositoryWithRunner failingRunner postgresTestConfig
      fmap stripVolatileDatabaseTimingResponse (selectResponseWithDatabase defaultAppConfig postgresEffect apiSecondRequest)
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status503,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
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
                      Observability.attributeValue = Observability.TextAttribute "api"
                    }
                ],
              HarchWeb.responseLogEntries =
                [ "Database failure while rendering required second-page api response after database operations [load-second-page-summary (SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;), load-second-page-highlights (SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;)]: SecondPageDataError \"highlights unavailable\""
                ],
              HarchWeb.responseDatabaseOperations = expectedSecondDatabaseOperations
            }

    it "preserves unexpected database error constructors in API diagnostics" $
      renderApiResponseFromRouteData (SecondRouteDataResult (Left (HomePageDataError "wrong loader")))
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = Http.status503,
            HarchWeb.responseContentType = "application/json",
            HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
            HarchWeb.responseObservabilityAttributes =
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "error.type",
                    Observability.attributeValue = Observability.TextAttribute "HomePageDataError"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.failure.code",
                    Observability.attributeValue = Observability.TextAttribute "database.home-page-data"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.route",
                    Observability.attributeValue = Observability.TextAttribute "/second"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.surface",
                    Observability.attributeValue = Observability.TextAttribute "api"
                  }
              ],
            HarchWeb.responseLogEntries =
              ["Database failure while rendering required second-page api response: HomePageDataError \"wrong loader\""],
            HarchWeb.responseDatabaseOperations = []
          }

    it "maps required second-page failures into explicit HTML 500 responses" $ do
      let failingDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
          renderedPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              (SecondRouteDataResult (Left (SecondPageDataError "seed unavailable")))
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect secondRequest
        `shouldReturn` HarchWeb.PageResponseWithMetadata
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

    it "redirects root requests before a home-page database failure can be observed" $ do
      let failingDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect homeRequest
        `shouldReturn` (HarchWeb.redirectResponse Http.status302 "/spaces" :: HarchWeb.Response AppRoute AppRequestContext)

    it "keeps locale and forwarded path prefixes in root redirect locations" $ do
      let prefixedSpanishRequest =
            HarchWeb.RouteRequest
              HomeRoute
              spanishRequestContext {requestPathPrefix = "/app"}
      selectResponse defaultAppConfig spanishHomeRequest
        `shouldReturn` (HarchWeb.redirectResponse Http.status302 "/es/spaces" :: HarchWeb.Response AppRoute AppRequestContext)
      selectResponse defaultAppConfig prefixedSpanishRequest
        `shouldReturn` (HarchWeb.redirectResponse Http.status302 "/app/es/spaces" :: HarchWeb.Response AppRoute AppRequestContext)

    it "keeps routes without required database data on their existing responses" $ do
      let failingDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  spanishSecondPageData = Left (SecondPageDataError "seed unavailable")
                }
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect homeRequest
        `shouldReturn` (HarchWeb.redirectResponse Http.status302 "/spaces" :: HarchWeb.Response AppRoute AppRequestContext)
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = Http.status200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"en\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = [],
              HarchWeb.responseDatabaseOperations = []
            }

    it "is deterministic for repeated requests" $ do
      firstResponse <- selectResponse defaultAppConfig apiStatusRequest
      secondResponse <- selectResponse defaultAppConfig apiStatusRequest
      firstResponse `shouldBe` secondResponse
