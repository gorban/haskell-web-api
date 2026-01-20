{-# SPEC #-}

import WebApi (run)

spec = describe "Run app" $
  it "Runs" $ do
    run
