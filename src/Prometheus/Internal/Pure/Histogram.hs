{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Histogram
  ( Bucket
  , defBuckets
  , Histogram(..)
  ) where


import           Prometheus.Internal.Pure.Base

import           Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Real


type Bucket = Double

-- | >>> defBuckets
-- [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]
defBuckets :: [Double]
defBuckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]


-- | A histogram is merely a set of buckets, each representing an upper boundary, last always
--   being 'infinity'.
--
--   Note: 'infinity' is always attached to the end of the bucket list.
data Histogram = Histogram
                   { hSum     :: Double        -- ^ Total sum of all observations
                   , hCount   :: Int           -- ^ Number of observations made
                   , hBuckets :: Map Bucket Int
                   }

instance NFData Histogram where
  rnf (Histogram s c b) = rnf (s, c, b)

instance Name Histogram where
  name _ = "histogram"

instance Construct [Bucket] Histogram where
  construct buckets =
    Histogram 0 0 . Map.fromList $ flip (,) 0 <$> buckets <> [fromRational infinity]

instance Extract Histogram Histogram where
  extract = id

instance Export Histogram where
  export (Histogram hsum count buckets) =
    converted buckets <> [ DoubleSample "_sum" [] hsum, IntSample "_count" [] count ]
    where
      converted = fmap (\(k, a) -> IntSample "_bucket" [("le", showWithInf k)] a) . tailSafe . cumulative . Map.toList

      cumulative as = scanl (\(_, a) (b, c) -> (b, c + a)) (0, 0) as

      tailSafe [] = []
      tailSafe as = tail as

      showWithInf v | v == fromRational infinity = "+Inf"
                    | otherwise                  = BSLC.pack $ show v

instance Observe Histogram where
  observe value (Histogram hsum count buckets) =
    Histogram (hsum + value) (count + 1) $
      case Map.lookupGE value buckets of
        Nothing         -> buckets
        Just (upper, _) -> Map.adjust (+ 1) upper buckets
