{-# SPEC #-}

import WebApi (run)

spec = describe "Run app" $
  it "runs" $ do
    run
