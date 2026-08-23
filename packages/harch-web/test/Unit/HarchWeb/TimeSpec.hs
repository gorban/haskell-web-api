{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Time

spec =
  describe "Unix time" $ do
    it "keeps durable nanoseconds distinct while preserving their unsigned ordering" $ do
      let zero = unixTimeNanoseconds 0
          instant = unixTimeNanoseconds 1700000000123456789
      expectAll
        ( (unixTimeNanosecondsValue instant `shouldBe` 1700000000123456789)
            :| [ zero < instant `shouldBe` True,
                 instant > zero `shouldBe` True,
                 zero <= instant `shouldBe` True,
                 instant >= zero `shouldBe` True,
                 compare zero instant `shouldBe` LT,
                 max zero instant `shouldBe` instant,
                 min zero instant `shouldBe` zero,
                 instant == unixTimeNanoseconds 1700000000123456789 `shouldBe` True,
                 instant /= zero `shouldBe` True,
                 minBound < instant `shouldBe` True,
                 maxBound > instant `shouldBe` True,
                 instant + 1 `shouldBe` unixTimeNanoseconds 1700000000123456790,
                 instant - 1 `shouldBe` unixTimeNanoseconds 1700000000123456788,
                 unixTimeNanoseconds 3 * 2 `shouldBe` unixTimeNanoseconds 6,
                 abs instant `shouldBe` instant,
                 signum instant `shouldBe` unixTimeNanoseconds 1,
                 negate zero `shouldBe` zero,
                 show instant `shouldBe` "UnixTimeNanoseconds 1700000000123456789",
                 show [instant] `shouldBe` "[UnixTimeNanoseconds 1700000000123456789]"
               ]
        )

    it "converts durable instants to whole Unix seconds without rounding up" $ do
      let instant = unixTimeNanoseconds 1700000000123456789
          seconds = unixTimeSeconds 1700000000
      expectAll
        ( (unixTimeSecondsFromNanoseconds instant `shouldBe` seconds)
            :| [ unixTimeSecondsValue seconds `shouldBe` 1700000000,
                 seconds < unixTimeSeconds 1700000001 `shouldBe` True,
                 seconds <= unixTimeSeconds 1700000001 `shouldBe` True,
                 seconds > unixTimeSeconds 1699999999 `shouldBe` True,
                 seconds >= unixTimeSeconds 1699999999 `shouldBe` True,
                 compare seconds (unixTimeSeconds 1700000001) `shouldBe` LT,
                 max seconds (unixTimeSeconds 1700000001) `shouldBe` unixTimeSeconds 1700000001,
                 min seconds (unixTimeSeconds 1700000001) `shouldBe` seconds,
                 seconds == unixTimeSeconds 1700000000 `shouldBe` True,
                 seconds /= unixTimeSeconds 1700000001 `shouldBe` True,
                 minBound < seconds `shouldBe` True,
                 maxBound > seconds `shouldBe` True,
                 seconds + 1 `shouldBe` unixTimeSeconds 1700000001,
                 seconds - 1 `shouldBe` unixTimeSeconds 1699999999,
                 unixTimeSeconds 3 * 2 `shouldBe` unixTimeSeconds 6,
                 abs seconds `shouldBe` seconds,
                 signum seconds `shouldBe` unixTimeSeconds 1,
                 negate (unixTimeSeconds 0) `shouldBe` unixTimeSeconds 0,
                 show seconds `shouldBe` "UnixTimeSeconds 1700000000",
                 show [seconds] `shouldBe` "[UnixTimeSeconds 1700000000]"
               ]
        )

    it "adds durable durations only when the resulting instant is representable" $ do
      expectAll
        ( (addUnixTimeNanoseconds (unixTimeNanoseconds 10) 15 `shouldBe` Just (unixTimeNanoseconds 25))
            :| [ addUnixTimeNanoseconds maxBound 1 `shouldBe` Nothing,
                 addUnixTimeNanoseconds maxBound 0 `shouldBe` Just maxBound
               ]
        )

    it "reads a positive Unix-epoch instant" $ do
      instant <- currentUnixTimeNanoseconds
      instant > unixTimeNanoseconds 0 `shouldBe` True
