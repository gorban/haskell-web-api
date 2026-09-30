-- | Typed URL rendering for the route algebra. This leaf sits below
-- 'WebApi.Route', which imports the generated page aggregator, so discovered
-- page bodies can build typed links without importing their own aggregate back
-- through the route-codec facade. Prefix and locale handling remain the one
-- application route projection.
module WebApi.Route.Url
  ( renderRouteLocation,
    renderRoutePath,
    renderRouteUrl,
    requiredRouteUrl,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Pages.Route.Generated qualified as Generated
import WebApi.Route.Context (requestLocale, requestLocaleIsExplicit, requestPathPrefix)
import WebApi.Route.Types
  ( ApiRoute (..),
    AppLocale (..),
    AppRequestContext,
    AppRoute (..),
    pageRouteMetadata,
    routePageSegment,
  )

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath = HarchWeb.safeUrlText . HarchWeb.encodeRouteLocation . renderRouteLocation

renderRouteLocation :: HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.RouteLocation
renderRouteLocation routeRequest =
  HarchWeb.prefixRouteLocation
    (requestPathPrefix requestContext)
    HarchWeb.RouteLocation
      { HarchWeb.routePathSegments = renderedSegments,
        HarchWeb.routeQueryFields = []
      }
  where
    requestContext = HarchWeb.requestContext routeRequest
    renderedSegments =
      case HarchWeb.requestRoute routeRequest of
        Api apiRoute -> NonEmpty.toList (apiRouteSegments apiRoute)
        Page pageRoute -> localeSegments <> maybe [] (pure . HarchWeb.requiredPathSegment) (routePageSegment (pageRouteMetadata pageRoute))
        GeneratedPages generatedPage ->
          localeSegments <> map HarchWeb.requiredPathSegment (filter (not . Text.null) (Text.splitOn "/" (Text.dropWhile (== '/') (Generated.pageRoutePath generatedPage))))
    localeSegments =
      case (requestLocale requestContext, requestLocaleIsExplicit requestContext) of
        (English, False) -> []
        (English, True) -> [HarchWeb.requiredPathSegment "en"]
        (Spanish, _) -> [HarchWeb.requiredPathSegment "es"]

apiRouteSegments :: ApiRoute -> NonEmpty.NonEmpty HarchWeb.PathSegment
apiRouteSegments apiRoute =
  case apiRoute of
    StatusApi -> pathSegment "api" NonEmpty.:| [pathSegment "status"]
    SecondApi -> pathSegment "api" NonEmpty.:| [pathSegment "second"]
    MeApi -> pathSegment "api" NonEmpty.:| [pathSegment "me"]
    TokenApi -> pathSegment "api" NonEmpty.:| [pathSegment "oauth", pathSegment "token"]
    -- The documentation specification deliberately lives at the default
    -- @\/docs@ path rather than under @\/api@: it is a support surface, not
    -- part of the documented API it describes.
    DocsOpenApiSpec -> pathSegment "docs" NonEmpty.:| [pathSegment "openapi.json"]
    ApiNotFound -> pathSegment "api" NonEmpty.:| [pathSegment "404"]
  where
    pathSegment = HarchWeb.requiredPathSegment

-- | Turn the closed application's typed route rendering into a safe link
-- target. A rejection is an application route-table defect, not a request
-- outcome; 'requiredRouteUrl' keeps that invariant directly testable.
renderRouteUrl :: HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.SafeUrl
renderRouteUrl = HarchWeb.encodeRouteLocation . renderRouteLocation

requiredRouteUrl :: Text -> HarchWeb.SafeUrl
requiredRouteUrl renderedPath =
  HarchWeb.requiredSafeUrlOrDie
    ("WebApi.Route rendered an unsafe URL: " <> renderedPath)
    (HarchWeb.mkSafeUrl renderedPath)
