{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Main where

import           Prometheus
import           Type.No

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           GHC.Generics
import           Test.Hspec



main :: IO ()
main =
  hspec $
    describe "Consistency" $ do
      it "Basic test" $ do
        reg <- genericRegister test
        plus 200 $ tGauge reg
        observe 1.7 $ tHistogram reg
        observe 0.9 $ tSummary reg
        plus 1 $ tCounter reg
        plus 2 $ tCounter reg
        res <- genericExport reg
        res `shouldBe` result

      it "Vector test" $ do
        reg <- genericRegister testVec
        withLabel "that" (tvCounter reg) increment
        withLabel ("left", "right") (tvSummary reg) $ observe 3.8
        res <- genericExport reg
        res `shouldBe` resultVec



data Test f = Test
                { tCounter   :: No Identity f Counter
                , tGauge     :: No Identity f Gauge
                , tHistogram :: No Identity f Histogram
                , tSummary   :: No Identity f Summary
                }
              deriving Generic

test :: Test Metric
test =
  Test
    { tCounter   = counter  $ mkInfo "tCounter"   "tCounterHelp"
    , tGauge     = gauge    $ mkInfo "tGauge"     "tGaugeHelp"
    , tHistogram = histogram (mkInfo "tHistogram" "tHistogramHelp") defBuckets
    , tSummary   = summary   (mkInfo "tSummary"   "tSummaryHelp")   defQuantiles
    }



data TestVec f = TestVec
                   { tvCounter :: No Identity f (Vector1 Counter)
                   , tvSummary :: No Identity f (Vector2 Summary)
                   }
                 deriving Generic

testVec :: TestVec Metric
testVec =
  TestVec
    { tvCounter = vector "this" . counter $ mkInfo "tvCounter" "tvCounterHelp"
    , tvSummary = vector ("one", "two") $ summary (mkInfo "tvSummary" "tvSummaryHelp") defQuantiles
    }



result :: BSLC.ByteString
result =
  BSLC.intercalate "\n"
    [ "# HELP tCounter tCounterHelp"
    , "# TYPE tCounter counter"
    , "tCounter 3.0"
    , "# HELP tGauge tGaugeHelp"
    , "# TYPE tGauge gauge"
    , "tGauge 200.0"
    , "# HELP tHistogram tHistogramHelp"
    , "# TYPE tHistogram histogram"
    , "tHistogram_bucket{le=\"5.0e-3\"} 0"
    , "tHistogram_bucket{le=\"1.0e-2\"} 0"
    , "tHistogram_bucket{le=\"2.5e-2\"} 0"
    , "tHistogram_bucket{le=\"5.0e-2\"} 0"
    , "tHistogram_bucket{le=\"0.1\"} 0"
    , "tHistogram_bucket{le=\"0.25\"} 0"
    , "tHistogram_bucket{le=\"0.5\"} 0"
    , "tHistogram_bucket{le=\"1.0\"} 0"
    , "tHistogram_bucket{le=\"2.5\"} 1"
    , "tHistogram_bucket{le=\"5.0\"} 1"
    , "tHistogram_bucket{le=\"10.0\"} 1"
    , "tHistogram_bucket{le=\"+Inf\"} 1"
    , "tHistogram_sum 1.7"
    , "tHistogram_count 1"
    , "# HELP tSummary tSummaryHelp"
    , "# TYPE tSummary summary"
    , "tSummary{quantile=\"0.5\"} 0.9"
    , "tSummary{quantile=\"0.9\"} 0.9"
    , "tSummary{quantile=\"0.99\"} 0.9"
    , "tSummary_sum 0.9"
    , "tSummary_count 1"
    , ""
    ]

resultVec :: BSLC.ByteString
resultVec =
  BSLC.intercalate "\n"
    [ "# HELP tvCounter tvCounterHelp"
    , "# TYPE tvCounter counter"
    , "tvCounter{this=\"that\"} 1.0"
    , "# HELP tvSummary tvSummaryHelp"
    , "# TYPE tvSummary summary"
    , "tvSummary{one=\"left\", two=\"right\", quantile=\"0.5\"} 3.8"
    , "tvSummary{one=\"left\", two=\"right\", quantile=\"0.9\"} 3.8"
    , "tvSummary{one=\"left\", two=\"right\", quantile=\"0.99\"} 3.8"
    , "tvSummary_sum{one=\"left\", two=\"right\"} 3.8"
    , "tvSummary_count{one=\"left\", two=\"right\"} 1"
    , ""
    ]



