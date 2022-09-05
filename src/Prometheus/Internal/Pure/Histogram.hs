{-# LANGUAGE BangPatterns
           , FlexibleInstances
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
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
                   { hSum     :: {-# UNPACK #-} !Double -- ^ Total sum of all observations
                   , hCount   :: {-# UNPACK #-} !Int    -- ^ Number of observations made
                   , hBuckets :: Map Bucket Int
                   }

instance NFData Histogram where
  rnf (Histogram s c b) = rnf (s, c, b)

instance Name Histogram where
  name _ = "histogram"
  {-# INLINABLE name #-}

instance Construct [Bucket] Histogram where
  construct buckets =
    Histogram 0 0 . Map.fromList $! flip (,) 0 <$> buckets <> [fromRational infinity]
  {-# INLINABLE construct #-}

instance Extract Histogram Histogram where
  extract = id
  {-# INLINABLE extract #-}

instance Export Histogram where
  export (Histogram hsum count buckets) =
    converted buckets <> [ DoubleSample "_sum" [] hsum, IntSample "_count" [] count ]
    where
      converted = fmap (\(k, a) -> IntSample "_bucket" [("le", showWithInf k)] a) . tailSafe . cumulative . Map.toList

      cumulative as = List.scanl' (\(_, a) (b, !c) -> (b, c + a)) (0, 0) as

      tailSafe [] = []
      tailSafe as = tail as

      showWithInf v | v == fromRational infinity = "+Inf"
                    | otherwise                  = BSLC.pack $ show v
  {-# INLINABLE export #-}

instance Observe Histogram where
  observe value (Histogram hsum count buckets) =
    Histogram (hsum + value) (count + 1) $
      case Map.lookupGE value buckets of
        Nothing         -> buckets
        Just (upper, _) -> Map.adjust (+ 1) upper buckets
  {-# INLINABLE observe #-}
