# test/Unit/AppSpec.hs

```hs
module Unit.AppSpec where

spec :: Spec
spec = do
  describe "route rendering" $ do
    it "renders the second page link from the home page" $ do
      renderHomePage `shouldContain` "/second"
```

App examples should stay small enough that the expected Unit tests are obvious before the runtime or
browser layers are involved.
