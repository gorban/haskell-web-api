# Route templates

**Status:** Aspirational

This is the next example shape to define before continuing the numbered ladder. It should stay
small and focused: one layout, a few static index links, one typed path-parameter page, and one
typed query-string page.

The goal is to prove that route templates are still ordinary typed routes, not stringly URL
helpers:

- path parameters are values in route constructors,
- query-string parameters are values in route constructors or typed GET form inputs,
- `harch` links and forms render both through the same route codec,
- direct load, reload, enhanced navigation, Back/Forward, and no-JavaScript fallback all keep
  working.

## Desired route shape

```hs
module App.Routes where

import Harch.Route

newtype PostSlug = PostSlug Text
newtype SearchQuery = SearchQuery Text

data Route
  = HomeRoute
  | PostRoute PostSlug
  | SearchRoute SearchQuery
  | SearchRouteEmpty
  | NotFoundRoute

routeTemplates :: RouteTemplates Route
routeTemplates =
  routes do
    page HomeRoute "/"
    page (PostRoute slug) "/posts/#{slug}"
    page (SearchRoute query) "/search?q=#{query}"
    page SearchRouteEmpty "/search"
    notFound NotFoundRoute
```

`PostSlug` and `SearchQuery` should have explicit parse/render behavior so route generation can
reject invalid values and encode valid values consistently. The template syntax is illustrative;
the requirement is that route values own path and query rendering.

## Path-parameter link

```hs
module App.Pages.Home where

import App.Routes (PostSlug (..), Route (..))
import Harch.Page

homePage :: Page Route
homePage =
  page HomeRoute do
    title "Route templates"

    markup [harch|
      <section>
        <h1>Route templates</h1>
        <p>
          <a href=@{PostRoute (PostSlug "hello-world")}>
            Read hello-world
          </a>
        </p>
      </section>
    |]
```

The generated anchor should render `/posts/hello-world`, but app code should never concatenate
`"/posts/" <> slug`.

## Query-string form

```hs
module App.Components.SearchForm where

import App.Routes (Route (..), SearchQuery (..))
import Harch.Component

searchForm :: Component Route
searchForm =
  component do
    markup [harch|
      <form method="get" action=@{SearchRouteEmpty}>
        <label for="site-search">Search</label>
        <input id="site-search" name=#{queryParam SearchRoute.query} type="search" />
        <button type="submit">Search</button>
      </form>
    |]
```

The form submits through normal browser GET behavior when JavaScript is disabled. With enhancement
enabled, the runtime can intercept the same GET navigation and route it through the same typed
rendering path.

For links built from existing state, query strings should still be typed:

```hs
markup [harch|
  <a href=@{SearchRoute (SearchQuery "server rendering")}>
    Search for server rendering
  </a>
|]
```

That should render `/search?q=server%20rendering` through the route codec.

## Search results page

```hs
module App.Pages.Search where

import App.Routes (Route (..), SearchQuery (..))
import Harch.Page

searchPage :: SearchQuery -> Page Route
searchPage query =
  page (SearchRoute query) do
    title "Search"

    markup [harch|
      <section>
        <h1>Search</h1>
        <p>Results for <strong>#{query}</strong></p>
      </section>
    |]
```

The query value should be decoded and validated before page rendering. Missing or empty query values
should be represented explicitly, either by `SearchRouteEmpty` or by a dedicated typed empty-query
case.

## Test shape

Unit route tests should cover:

- `/posts/hello-world` parses to `PostRoute (PostSlug "hello-world")`,
- `PostRoute (PostSlug "hello-world")` renders `/posts/hello-world`,
- path params are decoded and invalid path params are rejected,
- `/search?q=server%20rendering` parses to `SearchRoute (SearchQuery "server rendering")`,
- `SearchRoute (SearchQuery "server rendering")` renders `/search?q=server%20rendering`,
- `/search`, `/search?q=`, repeated query keys, and unsupported query keys have explicit behavior,
- harch-generated anchors and GET forms use the route codec instead of hard-coded strings.

Integration/e2e tests should cover:

- direct load and reload for `/posts/hello-world`,
- direct load and reload for `/search?q=server%20rendering`,
- enhanced same-origin navigation to both routes,
- Back/Forward across static, path-parameter, and query-string pages,
- no-JavaScript fallback through anchors and the GET search form.

## Main stacked example

`packages/web-api` should eventually include one path-parameter route and one query-string route
after the small route-template example is settled. That keeps the full app honest under database,
observability, middleware, and deployment concerns without making the first route-template example
carry the whole stack.
