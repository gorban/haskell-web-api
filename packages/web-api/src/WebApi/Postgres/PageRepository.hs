{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.PageRepository
  ( pageRepository,
  )
where

import Control.Exception (evaluate)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import WebApi.Database
  ( DatabaseError (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    HomePageData (..),
    PageRepository (..),
    SecondPageData (..),
  )
import WebApi.Postgres.QueryRunner (PageQueryRunner (..))
import WebApi.Route (AppLocale (..))

type PageLoadWorkflow = ExceptT DatabaseError (WriterT [DatabaseOperation] IO)

pageRepository :: PageQueryRunner -> PageRepository
pageRepository runner =
  PageRepository
    { loadHomePage = loadHomePageDataWith runner,
      loadSecondPage = loadSecondPageDataWith runner
    }

loadHomePageDataWith :: PageQueryRunner -> AppLocale -> IO (DatabaseResult HomePageData)
loadHomePageDataWith runner locale =
  runPageLoadWorkflow $ do
    summary <-
      observedQuery
        homeSummaryOperation
        HomePageDataError
        (runRequiredTextQuery runner (homeSummaryQuery locale))
    pure (HomePageData summary)

loadSecondPageDataWith :: PageQueryRunner -> AppLocale -> IO (DatabaseResult SecondPageData)
loadSecondPageDataWith runner locale =
  runPageLoadWorkflow $ do
    summary <-
      observedQuery
        secondSummaryOperation
        SecondPageDataError
        (runRequiredTextQuery runner (secondSummaryQuery locale))
    highlights <-
      observedQuery
        secondHighlightsOperation
        SecondPageDataError
        (runTextRowsQuery runner (secondHighlightsQuery locale))
    pure
      SecondPageData
        { secondPageDataSummary = summary,
          secondPageDataHighlights = highlights
        }

runPageLoadWorkflow :: PageLoadWorkflow value -> IO (DatabaseResult value)
runPageLoadWorkflow workflow = do
  (result, operations) <- runWriterT (runExceptT workflow)
  pure
    DatabaseResult
      { databaseResultValue = result,
        databaseResultOperations = operations
      }

observedQuery ::
  DatabaseOperation ->
  (Text -> DatabaseError) ->
  IO (Either Text value) ->
  PageLoadWorkflow value
observedQuery operation toDomainError query = do
  (result, completedOperation) <- liftIO (timedDatabaseOperation operation query)
  lift (tell [completedOperation])
  either (throwError . toDomainError) pure result

timedDatabaseOperation :: DatabaseOperation -> IO value -> IO (value, DatabaseOperation)
timedDatabaseOperation databaseOperation action = do
  _ <- evaluate (databaseOperationStartedAtNanoseconds databaseOperation)
  _ <- evaluate (databaseOperationEndedAtNanoseconds databaseOperation)
  startedAt <- getMonotonicTimeNSec
  result <- action
  endedAt <- getMonotonicTimeNSec
  pure
    ( result,
      databaseOperation
        { databaseOperationStartedAtNanoseconds = Just startedAt,
          databaseOperationEndedAtNanoseconds = Just endedAt
        }
    )

homeSummaryQuery :: AppLocale -> Text
homeSummaryQuery locale =
  "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = '"
    <> renderLocaleCode locale
    <> "';"

secondSummaryQuery :: AppLocale -> Text
secondSummaryQuery locale =
  "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = '"
    <> renderLocaleCode locale
    <> "';"

secondHighlightsQuery :: AppLocale -> Text
secondHighlightsQuery locale =
  "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = '"
    <> renderLocaleCode locale
    <> "' ORDER BY position ASC;"

homeSummaryOperation :: DatabaseOperation
homeSummaryOperation =
  DatabaseOperation
    { databaseOperationName = "load-home-page-summary",
      databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

secondSummaryOperation :: DatabaseOperation
secondSummaryOperation =
  DatabaseOperation
    { databaseOperationName = "load-second-page-summary",
      databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

secondHighlightsOperation :: DatabaseOperation
secondHighlightsOperation =
  DatabaseOperation
    { databaseOperationName = "load-second-page-highlights",
      databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

renderLocaleCode :: AppLocale -> Text
renderLocaleCode locale =
  case locale of
    English -> "en"
    Spanish -> "es"
