module Main where

import           Network.HTTP.Types
import           Network.Wai
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
  deriving Generic

test :: Test Metric
test =
  Test
    (counter $ infoM "tCounter" "tCounterHelp")
    (gauge $ infoM "tGauge" "tGaugeHelp")
    (histogram (infoM "tHistogram" "tHistogramHelp") def)
    (summary (infoM "tSummary" "tSummaryHelp") def)
    (vector "this" . counter $ infoM "tvCounter" "tvCounterHelp")
    (vector ("one", "two") $ summary (infoM "tvSummary" "tvSummaryHelp") def)

{-
   This example provides an idea on how to start your HTTP application
   with provided http latency and status code metrics already in place as a middleware
   and at the same time provide your own custom metrics to export all togheter
-}
main :: IO ()
main = do
  reg <- genericRegister test
  serveAppWithMetrics "my_app" 9090 reg Nothing 3000 $ \req respond -> do
    putText ("Request: " <> show req)
    tGauge reg .+. 200
    tHistogram reg `observe` 1.7
    tSummary reg `observe` 0.9
    withLabel "that" (tvCounter reg) increment
    withLabel ("left", "right") (tvSummary reg) (`observe` 3.8)
    tCounter reg .+. 1
    tCounter reg .+. 2
    (respond $ responseLBS status200 [] "Hello World")
