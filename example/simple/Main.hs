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
    } deriving Generic

data Test2 f =
  Test2
    { tvCounter  :: NoIdentity f (Vector LByteString Counter)
    , tvSummary  :: NoIdentity f (Vector (LByteString, LByteString) Summary)
    , holeM      :: NoIdentity f None
    }
  deriving Generic

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

main :: IO ()
main = do
  reg <- genericRegister test
  reg2 <- genericRegister test2
  race_ (serveMetricsM 9090 (reg :# reg2 :# HNil)) $ do
    tGauge reg .+. 200
    tHistogram reg `observe` 1.7
    tSummary reg `observe` 0.9
    withLabel "that" (tvCounter reg2) increment
    withLabel ("left", "right") (tvSummary reg2) (`observe` 3.8)
    tCounter reg .+. 1
    tCounter reg .+. 2
