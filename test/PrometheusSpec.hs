module PrometheusSpec
  ( spec
  ) where

import           Prometheus
import           Protolude
import           Test.Hspec

spec :: Spec
spec =
  describe "Prometheus Tests" $ do
    it "Simple Metrics Register Test" $ do
      reg <- genericRegister test
      tGauge reg .+. 200
      tHistogram reg `observe` 1.7
      tSummary reg `observe` 0.9
      tCounter reg .+. 1
      tCounter reg .+. 2
      result <- genericExport reg
      result `shouldBe` expectedResult1
    it "Simple Metrics Register With None Value Test" $ do
      reg2 <- genericRegister test2
      withLabel "that" (tvCounter reg2) increment
      withLabel ("left", "right") (tvSummary reg2) (`observe` 3.8)
      genericExport reg2 >>= flip shouldBe expectedResult2



data Test f =
  Test
    { tCounter   :: NoIdentity f Counter
    , tGauge     :: NoIdentity f Gauge
    , tHistogram :: NoIdentity f Histogram
    , tSummary   :: NoIdentity f Summary
    }
  deriving (Generic)

data Test2 f =
  Test2
    { tvCounter :: NoIdentity f (Vector LByteString Counter)
    , tvSummary :: NoIdentity f (Vector (LByteString, LByteString) Summary)
    , holeM     :: NoIdentity f None
    }
  deriving (Generic)

expectedResult2 = "# HELP tvCounter tvCounterHelp\n# TYPE tvCounter counter\ntvCounter{this=\"that\"} 1.0\n# HELP tvSummary tvSummaryHelp\n# TYPE tvSummary summary\ntvSummary{one=\"left\", two=\"right\", quantile=\"0.5\"} 3.8\ntvSummary{one=\"left\", two=\"right\", quantile=\"0.9\"} 3.8\ntvSummary{one=\"left\", two=\"right\", quantile=\"0.99\"} 3.8\ntvSummary_sum{one=\"left\", two=\"right\"} 3.8\ntvSummary_count{one=\"left\", two=\"right\"} 1\n"

expectedResult1 = "# HELP tCounter tCounterHelp\n\
\# TYPE tCounter counter\n\
\tCounter 3.0\n\
\# HELP tGauge tGaugeHelp\n\
\# TYPE tGauge gauge\n\
\tGauge 200.0\n\
\# HELP tHistogram tHistogramHelp\n\
\# TYPE tHistogram histogram\n\
\tHistogram_bucket{le=\"5.0e-3\"} 0\n\
\tHistogram_bucket{le=\"1.0e-2\"} 0\n\
\tHistogram_bucket{le=\"2.5e-2\"} 0\n\
\tHistogram_bucket{le=\"5.0e-2\"} 0\n\
\tHistogram_bucket{le=\"0.1\"} 0\n\
\tHistogram_bucket{le=\"0.25\"} 0\n\
\tHistogram_bucket{le=\"0.5\"} 0\n\
\tHistogram_bucket{le=\"1.0\"} 0\n\
\tHistogram_bucket{le=\"2.5\"} 1\n\
\tHistogram_bucket{le=\"5.0\"} 1\n\
\tHistogram_bucket{le=\"10.0\"} 1\n\
\tHistogram_bucket{le=\"+Inf\"} 1\n\
\tHistogram_sum 1.7\n\
\tHistogram_count 1\n\
\# HELP tSummary tSummaryHelp\n\
\# TYPE tSummary summary\n\
\tSummary{quantile=\"0.5\"} 0.9\n\
\tSummary{quantile=\"0.9\"} 0.9\n\
\tSummary{quantile=\"0.99\"} 0.9\n\
\tSummary_sum 0.9\ntSummary_count 1\n"

test :: Test Metric
test =
  Test
    (counter $ infoM "tCounter" "tCounterHelp")
    (gauge $ infoM "tGauge" "tGaugeHelp")
    (histogram (infoM "tHistogram" "tHistogramHelp") def)
    (summary (infoM "tSummary" "tSummaryHelp") def)

test2 :: Test2 Metric
test2 =
  Test2
    (vector "this" . counter $ infoM "tvCounter" "tvCounterHelp")
    (vector ("one", "two") $ summary (infoM "tvSummary" "tvSummaryHelp") def)
    none
