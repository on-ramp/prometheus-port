{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

{-|
    The summary code is a 1-1 copy from [prometheus-client]
    (https://hackage.haskell.org/package/prometheus-client/docs/src/Prometheus.Metric.Summary.html) 
    and should probably be rewritten one day, as this implementation seems rather weak (but
    I never bothered to figure out how it actually works, take that >:P)
 -}

module Prometheus.Internal.Pure.Summary
  ( -- * Summary
    Quantile
  , Summary (..)
  , Estimator
  ) where

import           Prometheus.Internal.Pure.Base

import           Protolude

import           Data.Default


-- | >>> def :: [Quantile]
--   [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]
type Quantile = (Double, Double)

instance {-# OVERLAPS #-} Default [Quantile] where
  def = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]



data Item = Item
              { _iValue :: Double
              , _iG     :: !Int64
              , _iD     :: !Int64
              } deriving Eq

instance Ord Item where
  compare = compare `on` _iValue


-- | 'Estimator' is the underlying structure of a summary, aggregating values over time.
data Estimator = Estimator
                   { _eCount     :: !Int64
                   , _eSum       :: !Double
                   , _eQuantiles :: [Quantile]
                   , _eItems     :: [Item]
                   }

-- | A summary exposes streaming φ-quantiles (0 ≤ φ ≤ 1) of observed events over a sliding time
--   window.
--
--   Note: used implementation has no notion of a time window per se, it just compresses
--         the 'Estimator' on everya observation.
data Summary = Summary
                 { sCount :: !Int -- ^ Number of observations made
                 , sDouble :: !Double -- ^ Total sum of all observations
                 , sQuantiles :: [(Double, Double)] -- ^ [(quantile, φ-quantile)]
                 } deriving Show



instance PureNamed Estimator where
  pureName _ = "summary"

instance PureConstructible [Quantile] Estimator where
  pureConstruct quantiles = Estimator 0 0 quantiles []

type instance Extract Estimator = Summary

instance Extract Estimator ~ e => PureExtractable e Estimator where
  pureExtract estimator' =
    let estimator@(Estimator count esum quantiles _) = compress estimator'
        quants = fmap fst quantiles
    in Summary (fromIntegral count) esum . zip quants $ map (query estimator) quants

instance PureExportable Estimator where
  pureExport estimator =
    let (Summary count ssum quantiles) = pureExtract estimator
    in converted quantiles <> [ DoubleSample "_sum" [] ssum, IntSample "_count" [] count ]
    where
      converted = fmap (\(k, a) -> DoubleSample "" [("quantile", show k)] a)

instance PureObservable Estimator where
  pureObserve estimator value = insert value $ compress estimator



insert :: Double -> Estimator -> Estimator
insert value summaryData@(Estimator oldCount oldSum quantiles items) =
        newEstimator $ insertItem 0 items
    where
        newEstimator = Estimator (oldCount + 1) (oldSum + value) quantiles

        insertItem _ [] = [Item value 1 0]
        insertItem r [x]
            -- The first two cases cover the scenario where the initial size of
            -- the list is one.
            | r == 0 && value < _iValue x = Item value 1 0 : [x]
            | r == 0                      = x : [Item value 1 0]
            -- The last case covers the scenario where the we have walked off
            -- the end of a list with more than 1 element in the final case of
            -- insertItem in which case we already know that x < value.
            | otherwise                   = x : [Item value 1 0]
        insertItem r (x:y:xs)
            -- This first case only covers the scenario where value is less than
            -- the first item in a multi-item list. For subsequent steps of
            -- a multi valued list, this case cannot happen as it would have
            -- fallen through to the case below in the previous step.
            | value <= _iValue x = Item value 1 0 : x : y : xs
            | value <= _iValue y = x : Item value 1 (calcD $ r + _iG x)
                                     : y : xs
            | otherwise          = x : insertItem (_iG x + r) (y : xs)

        calcD r = max 0
                $ floor (invariant summaryData (fromIntegral r)) - 1


compress :: Estimator -> Estimator
compress est@(Estimator _ _ _ items)    =
  case head items of
    Nothing      -> est
    Just minItem ->
      est
        { _eItems = (minItem :)
                      . foldr' compressPair []
                      . drop 1  -- The exact minimum item must be kept exactly.
                      . zip items
                      $ scanl (+) 0 (map _iG items)
        }
    where
        compressPair (a, _) [] = [a]
        compressPair (a@(Item _ aG _), r) (b@(Item bVal bG bD):bs)
            | bD == 0             = a : b : bs
            | aG + bG + bD <= inv = Item bVal (aG + bG) bD : bs
            | otherwise           = a : b : bs
            where
                inv = floor $ invariant est (fromIntegral r)

query :: Estimator -> Double -> Double
query est@(Estimator count _ _ items) q = findQuantile allRs items
    where
        allRs = scanl (+) 0 $ map _iG items

        n = fromIntegral count
        f = invariant est

        rank  = q * n
        bound = rank + (f rank / 2)

        findQuantile _        []   = 0 / 0  -- NaN
        findQuantile _        [a]  = _iValue a
        findQuantile (_:bR:rs) (a@(Item{}):b@(Item _ bG bD):xs)
            | fromIntegral (bR + bG + bD) > bound = _iValue a
            | otherwise            = findQuantile (bR:rs) (b:xs)
        findQuantile _        _    = 0 / 0 -- This is supposed to fail with an error,
                                           -- but we don't do that here

invariant :: Estimator -> Double -> Double
invariant (Estimator count _ quantiles _) r =
  let n = fromIntegral count
      fj (q, e) | q * n <= r = 2 * e * r / q
                | otherwise  = 2 * e * (n - r) / (1 - q)
  in max 1 . minimum $ map fj quantiles
