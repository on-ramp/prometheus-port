{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Concurrent.Async
import           Prometheus
import           Protolude

data Test f =
  Test
    { tCounter   :: NoIdentity f Counter
    , tGauge     :: NoIdentity f Gauge
    , tHistogram :: NoIdentity f Histogram
    , tSummary   :: NoIdentity f Summary
    , tvCounter  :: NoIdentity f (Vector LByteString Counter)
    , tvSummary  :: NoIdentity f (Vector (LByteString, LByteString) Summary)
    }
  deriving (Generic)

test :: Test Metric
test =
  Test
    (counter $ infoM "tCounter" "tCounterHelp")
    (gauge $ infoM "tGauge" "tGaugeHelp")
    (histogram (infoM "tHistogram" "tHistogramHelp") def)
    (summary (infoM "tSummary" "tSummaryHelp") def)
    (vector "this" . counter $ infoM "tvCounter" "tvCounterHelp")
    (vector ("one", "two") $ summary (infoM "tvSummary" "tvSummaryHelp") def)

main :: IO ()
main = do
  reg <- genericRegister test
  concurrently_ (serveMetrics 9090 reg) $ do
    tGauge reg .+. 200
    tHistogram reg `observe` 1.7
    tSummary reg `observe` 0.9
    withLabel "that" (tvCounter reg) increment
    withLabel ("left", "right") (tvSummary reg) (`observe` 3.8)
    tCounter reg .+. 1
    tCounter reg .+. 2
