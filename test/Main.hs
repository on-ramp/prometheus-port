{-# LANGUAGE DeriveGeneric #-}

module Test where

import Prometheus

import Protolude



data Test f = Test
              { tCounter   :: NoIdentity f Counter
              , tGauge     :: NoIdentity f Gauge
              , tHistogram :: NoIdentity f Histogram
              , tSummary   :: NoIdentity f Summary
              , tvCounter  :: NoIdentity f (Vector LByteString Counter)
              , tvSummary  :: NoIdentity f (Vector (LByteString, LByteString) Summary)
              } deriving Generic

test :: Test Metric
test = Test
         ( counter $ Info "tCounter" "tCounterHelp" )
         ( gauge $ Info "tGauge" "tGaugeHelp" )
         ( histogram (Info "tHistogram" "tHistogramHelp") def )
         ( summary (Info "tSummary" "tSummaryHelp") def )
         ( vector "this" . counter $ Info "tvCounter" "tvCounterHelp" )
         ( vector ("one", "two") $ summary (Info "tvSummary" "tvSummaryHelp") def )

stuff :: IO ()
stuff = do
  reg <- genericRegister test

  (_, dur) <- time $ do tGauge reg .-. 200
                        tHistogram reg `observe` 1.7
                        tSummary reg `observe` 0.9
                        withLabel "that" (tvCounter reg) increment
                        withLabel ("left", "right") (tvSummary reg) (`observe` 3.8)

  tCounter reg .+. dur

  void $ push (genericExport reg) "http://localhost:9091/metrics/job/test"

