# src/App/Pages/Home.hs

```hs
module App.Pages.Home where

homePage :: Page
homePage =
  page "/"
    { title = "Home"
    , body =
        section_ $ do
          h2_ "Home"
          p_ "This page is fully server-rendered on first load."
          p_ "The link below should upgrade to progressive same-origin navigation."
          pageLink secondRoute "Go to the second page"
    }
```

The real requirement is hard-reload equivalence: opening `/` directly and navigating there from a
different page should produce equivalent content.
