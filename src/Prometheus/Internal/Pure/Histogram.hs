{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Histogram where

import           Prometheus.Internal.Pure.Base

import           Protolude

import           Data.Default
import qualified Data.Map as Map

-- | >>> def ::  [Bucket]
-- [5.0e-3,1.0e-2,2.5e-2,5.0e-2,0.1,0.25,0.5,1.0,2.5,5.0,10.0]
type Bucket = Double

instance {-# OVERLAPS #-} Default [Bucket] where
  def = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]



-- | A histogram is merely a set of buckets, each representing an upper boundary, last always
--   being 'infinity'.
--
--   Note: 'infinity' is always attached to the end of the bucket list, you don't need to add it.
--
--   Note: Prometheus histograms are cumulative, while the one stored is __not__, meaning that
--         'pureExtract' and 'pureExport' output different values.
--
--   > pureExtract >>>> 0 0 1 0 0 0 2 0 0
--   > pureExport  >>>> 0 0 1 1 1 1 3 3 3
data Histogram = Histogram
                   { hSum     :: !Double -- ^ Total sum of all observations
                   , hCount   :: !Int    -- ^ Number of observations made
                   , hBuckets :: Map Bucket Int
                   } deriving Show

mkHisto :: Double -> Int -> Map Bucket Int -> Histogram
mkHisto = Histogram

instance PureNamed Histogram where
  pureName _ = "histogram"

instance PureConstructible [Bucket] Histogram where
  pureConstruct buckets = Histogram 0 0 . Map.fromList . fmap (, 0) $ buckets <> [inf]
    where
      inf = fromRational infinity

type instance Extract Histogram = Histogram

instance Extract Histogram ~ e => PureExtractable e Histogram where
  pureExtract = identity



instance PureExportable Histogram where
  pureExport (Histogram hsum count buckets) =
    ExportSample (converted buckets <> [ DoubleSample "_sum" [] hsum, IntSample "_count" [] count ])
    where
      converted = fmap (\(k, a) -> IntSample "_bucket" [("le", showWithInf k)] a) . cumulative . Map.toList

      cumulative = tailSafe . scanl (\(_, a) (b, c) -> (b, c + a)) (0, 0)

      showWithInf v | v == fromRational infinity = "+Inf"
                    | otherwise                  = show v



instance PureObservable Histogram where
  pureObserve (Histogram hsum count buckets) value =
    Histogram (hsum + value) (count + 1)
      $ case Map.lookupGE value buckets of
          Nothing         -> buckets
          Just (upper, _) -> Map.adjust (+ 1) upper buckets
