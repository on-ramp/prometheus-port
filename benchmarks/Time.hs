module Main where

import Control.DeepSeq
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Ord (comparing)
import Gauge.Main
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
                  benchObservePure setObservePureEstimator "Estimator"
                ]
            ),
          bgroup
            "Export"
            ( concat
                [ benchExportPure setExportPureCounter "Counter",
                  benchExportPure setExportPureHistogram "Histogram",
                  benchExportPure setExportPureEstimator "Estimator"
                ]
            )
        ]
    ]
  where
    benchIncrPure ::
      (Pure.Increment m, Pure.Extract m e, NFData m, NFData e) =>
      IO m ->
      String ->
      [Benchmark]
    benchIncrPure setM title =
      [ env setM $ \pM ->
          bench (title ++ ": " ++ show i) $ nf (incrementN i) pM
        | i <- [10000, 100000]
      ]
      where
        incrementN 0 m = m
        incrementN n m = incrementN (n - 1) $ Pure.plus 1 m

    benchObservePure ::
      (Pure.Observe m, Pure.Extract m e, NFData m, NFData e) =>
      (Int -> IO (m, [Double])) ->
      String ->
      [Benchmark]
    benchObservePure setUp title =
      [ env (setUp i) $ \ ~(m, vs) ->
          bench (title ++ ": " ++ show i) $ nf (\m -> List.foldl' (flip Pure.observe) m vs) m
        | i <- [1000, 10000]
      ]

    benchExportPure ::
      (Pure.Export m, NFData m) =>
      (Int -> IO m) ->
      String ->
      [Benchmark]
    benchExportPure setUp title =
      [ env (setUp i) $ \m ->
          bench (title ++ ": " ++ show i) $ nf Pure.export m
        | i <- [1000, 10000]
      ]

    setIncrPureCounter :: IO Pure.Counter
    setIncrPureCounter = pure . force $ Pure.construct ()

    setIncrPureGauge :: IO Pure.Gauge
    setIncrPureGauge = pure . force $ Pure.construct ()

    setObservePureHistogram :: Int -> IO (Pure.Histogram, [Double])
    setObservePureHistogram n = do
      let buckets = Pure.defBuckets
          histo = force $ Pure.construct buckets
      values <- genHistoValues n buckets
      return (histo, values)

    setObservePureEstimator :: Int -> IO (Pure.Estimator, [Double])
    setObservePureEstimator n = do
      let qts = Pure.defQuantiles
          summ = force $ Pure.construct qts
      values <- genEstimatorValues n qts
      return (summ, values)

    setExportPureCounter :: Int -> IO Pure.Counter
    setExportPureCounter n = do
      m <- setIncrPureCounter
      return $ force $ Pure.set (fromIntegral n) m

    setExportPureHistogram :: Int -> IO Pure.Histogram
    setExportPureHistogram n = do
      (m, vs) <- setObservePureHistogram n
      return $ force $ List.foldl' (flip Pure.observe) m vs

    setExportPureEstimator :: Int -> IO Pure.Estimator
    setExportPureEstimator n = do
      (m, vs) <- setObservePureEstimator n
      return $ force $ List.foldl' (flip Pure.observe) m vs

genHistoValues :: MonadIO m => Int -> [Bucket] -> m [Double]
genHistoValues n buckets = do
  g <- initStdGen
  shuffle $
    take n $
      Random.randomRs (0.0, 2 * last buckets) g

genEstimatorValues :: MonadIO m => Int -> [Quantile] -> m [Double]
genEstimatorValues n _qts = do
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
