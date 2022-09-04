{-# LANGUAGE BangPatterns
           , DerivingStrategies
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverloadedStrings
           , TypeFamilies #-}

{-# OPTIONS_HADDOCK hide #-}

{- | The summary code is a 1-1 copy from [prometheus-client]
     (https://hackage.haskell.org/package/prometheus-client/docs/src/Prometheus.Metric.Summary.html)
     and should probably be rewritten one day, as this implementation seems rather weak (but
     I never bothered to figure out how it actually works, take that >:P)
 -}

module Prometheus.Internal.Pure.Summary
  ( Quantile
  , defQuantiles
  , Item(..)
  , Estimator(..)
  , Summary(..)
  , insert
  , compress
  , query
  , invariant
  ) where


import           Prometheus.Internal.Pure.Base

import           Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Foldable
import           Data.Function
import           Data.Int
import qualified Data.List as List
import           GHC.Real


type Quantile = (Double, Double)

-- | >>> defQuantiles
-- [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]
defQuantiles :: [Quantile]
defQuantiles = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]


data Item = Item
              { iValue :: {-# UNPACK #-} !Double
              , iG     :: {-# UNPACK #-} !Int64
              , iD     :: {-# UNPACK #-} !Int64
              }
            deriving stock Eq

instance Ord Item where
  compare = compare `on` iValue

instance NFData Item where
  rnf (Item v g d) = rnf (v, g, d)


-- | 'Estimator' is the underlying structure of a summary, aggregating values over time.
data Estimator = Estimator
                   { eCount     :: {-# UNPACK #-} !Int64
                   , eSum       :: {-# UNPACK #-} !Double
                   , eQuantiles :: [Quantile]
                   , eItems     :: [Item]
                   }

instance NFData Estimator where
  rnf (Estimator c s q i) = rnf (c, s, q, i)


-- | A summary exposes streaming φ-quantiles (0 ≤ φ ≤ 1) of observed events over a sliding time
--   window.
--
--   Note: used implementation has no notion of a time window per se, it just compresses
--         the 'Estimator' on every observation.
data Summary = Summary
                 { sCount     :: {-# UNPACK #-} !Int                -- ^ Number of observations made
                 , sDouble    :: {-# UNPACK #-} !Double             -- ^ Total sum of all observations
                 , sQuantiles :: [(Double, Double)] -- ^ [(quantile, φ-quantile)]
                 }

instance NFData Summary where
  rnf (Summary c d qs) = rnf (c, d, qs)

instance Name Estimator where
  name _ = "summary"
  {-# INLINABLE name #-}

instance Construct [Quantile] Estimator where
  construct quantiles = Estimator 0 0 quantiles []
  {-# INLINABLE construct #-}

instance Extract Estimator Summary where
  extract estimator' =
    let estimator@(Estimator count esum quantiles _) = compress estimator'
        quants = fmap fst quantiles
    in Summary (fromIntegral count) esum . zip quants $ map (query estimator) quants
  {-# INLINE extract #-}

instance Export Estimator where
  export estimator =
    let Summary count ssum quantiles = extract estimator
    in converted quantiles <> [ DoubleSample "_sum" [] ssum, IntSample "_count" [] count ]
    where
      converted = fmap (\(k, a) -> DoubleSample "" [("quantile", BSLC.pack $ show k)] a)
  {-# INLINE export #-}

instance Observe Estimator where
  observe !value = insert value . compress
  {-# INLINE observe #-}

insert :: Double -> Estimator -> Estimator
insert value summaryData@(Estimator oldCount oldSum quantiles items) =
        newEstimator $ insertItem 0 items
    where
        newEstimator = Estimator (oldCount + 1) (oldSum + value) quantiles

        insertItem _ [] = [Item value 1 0]
        insertItem !r [x]
            -- The first two cases cover the scenario where the initial size of
            -- the list is one.
            | r == 0 && value < iValue x = Item value 1 0 : [x]
            | r == 0                     = x : [Item value 1 0]
            -- The last case covers the scenario where the we have walked off
            -- the end of a list with more than 1 element in the final case of
            -- insertItem in which case we already know that x < value.
            | otherwise                   = x : [Item value 1 0]
        insertItem r (x:y:xs)
            -- This first case only covers the scenario where value is less than
            -- the first item in a multi-item list. For subsequent steps of
            -- a multi valued list, this case cannot happen as it would have
            -- fallen through to the case below in the previous step.
            | value <= iValue x = Item value 1 0 : x : y : xs
            | value <= iValue y = x : Item value 1 (calcD $ r + iG x)
                                    : y : xs
            | otherwise         = x : insertItem (iG x + r) (y : xs)

        calcD !r = max 0
                $ floor (invariant summaryData (fromIntegral r)) - 1
{-# INLINABLE insert #-}

compress :: Estimator -> Estimator
compress est@(Estimator _ _ _ items)    =
  case items of
    []        -> est
    minItem:_ -> est
                   { eItems = (minItem :)
                                . foldr' compressPair []
                                . drop 1  -- Drops the minimum item
                                . zip items
                                $ List.scanl' (\ !b -> (+) b) 0 (map iG items)
                   }
    where
        compressPair (a, _) [] = [a]
        compressPair (a@(Item _ aG _), r) (b@(Item bVal bG bD):bs)
            | bD == 0             = a : b : bs
            | aG + bG + bD <= inv = Item bVal (aG + bG) bD : bs
            | otherwise           = a : b : bs
            where
                inv = floor $ invariant est (fromIntegral r)
{-# INLINABLE compress #-}

query :: Estimator -> Double -> Double
query est@(Estimator count _ _ items) q = findQuantile allRs items
    where
        allRs = List.scanl (\ !b -> (+) b) 0 $ map iG items

        !n = fromIntegral count
        !f = invariant est

        !rank  = q * n
        !bound = rank + (f rank / 2)

        findQuantile _        []   = realToFrac notANumber
        findQuantile _        [a]  = iValue a
        findQuantile (_:bR:rs) (a@(Item{}):b@(Item _ bG bD):xs)
            | fromIntegral (bR + bG + bD) > bound = iValue a
            | otherwise            = findQuantile (bR:rs) (b:xs)
        findQuantile _        _    = realToFrac notANumber -- This is supposed to fail with an error,
                                                           -- but we don't do that here
{-# INLINABLE query #-}

invariant :: Estimator -> Double -> Double
invariant (Estimator count _ quantiles _) !r =
  let n = fromIntegral count
      fj (q, e) | q * n <= r = 2 * e * r / q
                | otherwise  = 2 * e * (n - r) / (1 - q)
  in max 1 . minimum $ map fj quantiles
{-# INLINABLE invariant #-}
