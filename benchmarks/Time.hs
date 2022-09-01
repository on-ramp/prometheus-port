{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Ord (comparing)
import Gauge.Main
import Prometheus
import Prometheus.Internal.Pure (Bucket, Quantile)
import qualified Prometheus.Internal.Pure as Pure
import System.Random (initStdGen)
import qualified System.Random as Random

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Pure"
        [ bgroup
            "Increment"
            ( concat
                [ benchIncrPure setIncrPureCounter "Counter",
                  benchIncrPure setIncrPureGauge "Gauge"
                ]
            ),
          bgroup
            "Observe"
            ( concat
                [ benchObservePure setObservePureHistogram "Histogram",
                  benchObservePure setObservePureSummary "Summary"
                ]
            ),
          bgroup
            "Export"
            ( concat
                [ benchExportPure setExportPureCounter "Counter",
                  benchExportPure setExportPureHistogram "Histogram",
                  benchExportPure setExportPureSummary "Summary"
                ]
            )
        ],
      bgroup
        "Impure"
        [ bgroup
            "Increment"
            ( concat
                [ benchIncr setIncrCounter "Counter",
                  benchIncr setIncrCounter "Gauge"
                ]
            ),
          bgroup
            "Observe"
            ( concat
                [ benchObserve setObserveHistogram "Histogram",
                  benchObserve setObserveSummary "Summary"
                ]
            )
        ]
    ]
  where
    benchIncrPure ::
      (Pure.Increment m, NFData m) =>
      IO m ->
      String ->
      [Benchmark]
    benchIncrPure setMetric title =
      [ env setMetric $ \m ->
          bench (title ++ ": " ++ show i) $ nf (incrementN i) m
        | i <- [10000, 100000]
      ]
      where
        incrementN 0 m = m
        incrementN n m = incrementN (n - 1) $ Pure.plus 1 m

    benchIncr ::
      (Increment m, Extract m e, NFData m, NFData e) =>
      IO m ->
      String ->
      [Benchmark]
    benchIncr setMetric title =
      [ env setMetric $ \m ->
          bench (title ++ ": " ++ show i) $ nfAppIO (incrementN i) m
        | i <- [10000, 100000]
      ]
      where
        incrementN 0 m = extract m
        incrementN n m = plus 1 m >> incrementN (n - 1) m

    benchObservePure ::
      (Pure.Observe m, Pure.Extract m e, NFData m, NFData e) =>
      (Int -> IO (m, [Double])) ->
      String ->
      [Benchmark]
    benchObservePure setUp title =
      [ env (setUp i) $ \ ~(m, vs) ->
          bench (title ++ ": " ++ show i) $ nf (\m -> List.foldl' (flip Pure.observe) m vs) m
        | i <- [10000, 100000]
      ]

    benchObserve ::
      (Observe m, NFData m) =>
      (Int -> IO (m, [Double])) ->
      String ->
      [Benchmark]
    benchObserve setUp title =
      [ env (setUp i) $ \ ~(m, vs) -> do
          let observeN [] = return ()
              observeN (v : vs) = observe v m >> observeN vs
          bench (title ++ ": " ++ show i) $ nfIO (observeN vs)
        | i <- [10000, 100000]
      ]
      where

    benchExportPure ::
      (Pure.Export m, NFData m) =>
      (Int -> IO m) ->
      String ->
      [Benchmark]
    benchExportPure setUp title =
      [ env (setUp i) $ \m ->
          bench (title ++ ": " ++ show i) $ nf Pure.export m
        | i <- [10000, 100000]
      ]

    setIncrPureCounter :: IO Pure.Counter
    setIncrPureCounter = pure . force $ Pure.construct ()

    setIncrCounter :: IO Counter
    setIncrCounter = do
      let m = counter $ Info "" ""
      register m

    setIncrPureGauge :: IO Pure.Gauge
    setIncrPureGauge = pure . force $ Pure.construct ()

    setIncrGauge :: IO Gauge
    setIncrGauge = do
      let m = gauge $ Info "" ""
      register m

    setObservePureHistogram :: Int -> IO (Pure.Histogram, [Double])
    setObservePureHistogram n = do
      let buckets = Pure.defBuckets
          histo = force $ Pure.construct buckets
      values <- genHistoValues n buckets
      return (histo, values)

    setObserveHistogram :: Int -> IO (Histogram, [Double])
    setObserveHistogram n = do
      let buckets = Pure.defBuckets
          m = histogram (Info "" "") buckets
      values <- genHistoValues n buckets
      hist <- register m
      return (hist, values)

    setObservePureSummary :: Int -> IO (Pure.Estimator, [Double])
    setObservePureSummary n = do
      let qts = Pure.defQuantiles
          summary' = force $ Pure.construct qts
      values <- genSummaryValues n qts
      return (summary', values)

    setObserveSummary :: Int -> IO (Summary, [Double])
    setObserveSummary n = do
      let qts = Pure.defQuantiles
          m = summary (Info "" "") qts
      values <- genSummaryValues n qts
      summary' <- register m
      return (summary', values)

    setExportPureCounter :: Int -> IO Pure.Counter
    setExportPureCounter n = do
      m <- setIncrPureCounter
      return $ force $ Pure.set (fromIntegral n) m

    setExportPureHistogram :: Int -> IO Pure.Histogram
    setExportPureHistogram n = do
      (m, vs) <- setObservePureHistogram n
      return $ force $ List.foldl' (flip Pure.observe) m vs

    setExportPureSummary :: Int -> IO Pure.Estimator
    setExportPureSummary n = do
      (m, vs) <- setObservePureSummary n
      return $ force $ List.foldl' (flip Pure.observe) m vs

genHistoValues :: MonadIO m => Int -> [Bucket] -> m [Double]
genHistoValues n buckets = do
  g <- initStdGen
  shuffle $
    take n $
      Random.randomRs (0.0, 2 * last buckets) g

genSummaryValues :: MonadIO m => Int -> [Quantile] -> m [Double]
genSummaryValues n _qts = do
  g <- initStdGen
  shuffle $
    take n $
      Random.randoms g

shuffle :: MonadIO m => [a] -> m [a]
shuffle xs = do
  g <- initStdGen
  let ns = Random.randomRs (minBound :: Int, maxBound :: Int) g
      sort' = fmap snd . List.sortBy (comparing fst)
  return $ sort' (zip ns xs)

-- TODO Not sure how to proceed with this.
instance NFData Counter where
  rnf = const ()

instance NFData Gauge where
  rnf = const ()

instance NFData Histogram where
  rnf = const ()

instance NFData Summary where
  rnf = const ()
