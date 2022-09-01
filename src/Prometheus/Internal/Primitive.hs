{-# LANGUAGE MultiParamTypeClasses
           , TypeFamilies
           , ScopedTypeVariables #-}

module Prometheus.Internal.Primitive
  ( register_
  , extract_
  , export_
  , plus_
  , minus_
  , set_
  , observe_
  , Counter(..)
  , counter
  , Gauge(..)
  , gauge
  , Histogram(..)
  , histogram
  , Summary(..)
  , summary
  ) where


import           Prometheus.Internal.Base as Base
import qualified Prometheus.Internal.Pure.Base as Pure
import qualified Prometheus.Internal.Pure.Counter as Pure (Counter)
import qualified Prometheus.Internal.Pure.Gauge as Pure (Gauge)
import qualified Prometheus.Internal.Pure.Histogram as Pure (Bucket, Histogram)
import qualified Prometheus.Internal.Pure.Summary as Pure (Quantile, Estimator, Summary)

import           Control.Concurrent.STM.TVar
import           Control.DeepSeq
import           Control.Monad.STM
import           Data.Functor.Identity
import           Data.Proxy


register_ :: Rank a ~ Identity => Metric a -> IO (Impure () TVar (Purify a))
register_ (Metric (Impure _def info base)) = do
  tvar <- newTVarIO $ runIdentity base
  return $ Impure () info tvar

extract_ :: Pure.Extract a e => Impure d TVar a -> IO e
extract_ (Impure _ _ t) = Pure.extract <$> readTVarIO t

export_ :: (Pure.Name a, Pure.Export a) => Impure d TVar a -> IO Template
export_ (Impure _ info t :: Impure d TVar a) =
  Template info (Pure.name (Proxy :: Proxy a)) . Pure.export <$> readTVarIO t

plus_ :: (NFData a, Pure.Increment a) => Double -> Impure d TVar a -> IO ()
plus_ a (Impure _ _ t) = atomically . modifyTVar' t $ force . Pure.plus a

minus_ :: (NFData a, Pure.Decrement a) => Double -> Impure d TVar a -> IO ()
minus_ a (Impure _ _ t) = atomically . modifyTVar' t $ force . Pure.minus a

set_ :: (NFData a, Pure.Set a) => Double -> Impure d TVar a -> IO ()
set_ a (Impure _ _ t) = atomically . modifyTVar' t $ force . Pure.set a

observe_ :: (NFData a, Pure.Observe a) => Double -> Impure d TVar a -> IO ()
observe_ a (Impure _ _ t) = atomically . modifyTVar' t $ force . Pure.observe a



-- | A [monotonically increasing counter](https://prometheus.io/docs/concepts/metric_types/#counter).
--
--   Note: 'plus' and 'set' only update the counter if the new value is larger than the old one.
newtype Counter = Counter { unCounter :: Impure () TVar Pure.Counter }

type instance Rank Counter = Identity

type instance Purify Counter = Pure.Counter

type instance Extra Counter = Identity

instance Register Counter where
  register = fmap Counter . register_

instance Extract Counter Double where
  extract = extract_ . unCounter

instance Export Counter where
  export = export_ . unCounter

instance Increment Counter where
  plus a = plus_ a . unCounter

-- | The [doc](https://prometheus.io/docs/instrumenting/writing_clientlibs/) doesn't require
--   this. It is useful for when the value is known to be monotonic, but you have no control
--   over it, such as with GHC RTS values sampled by "Prometheus.GHC".
instance Set Counter where
  set a (Counter (Impure _ _ t)) = atomically . modifyTVar' t $ Pure.set a

counter :: Info -> Metric Counter
counter info = Metric $ let new = Pure.construct ()
                        in Impure (Identity new) info $ Identity new



-- | A [freely shiftable gauge](https://prometheus.io/docs/concepts/metric_types/#gauge).
newtype Gauge = Gauge { unGauge :: Impure () TVar Pure.Gauge }

type instance Rank Gauge = Identity

type instance Purify Gauge = Pure.Gauge

type instance Extra Gauge = Identity

instance Register Gauge where
  register = fmap Gauge . register_

instance Extract Gauge Double where
  extract = extract_ . unGauge

instance Export Gauge where
  export = export_ . unGauge

instance Increment Gauge where
  plus a = plus_ a . unGauge

instance Decrement Gauge where
  minus a = minus_ a . unGauge

instance Set Gauge where
  set a = set_ a . unGauge

gauge :: Info -> Metric Gauge
gauge info = Metric $ let new = Pure.construct ()
                      in Impure (Identity new) info $ Identity new



-- | A [simple cumulative histogram](https://prometheus.io/docs/concepts/metric_types/#histogram)
newtype Histogram = Histogram { unHistogram :: Impure () TVar Pure.Histogram }

type instance Rank Histogram = Identity

type instance Purify Histogram = Pure.Histogram

type instance Extra Histogram = Identity

instance Register Histogram where
  register = fmap Histogram . register_

instance Extract Histogram Pure.Histogram where
  extract = extract_ . unHistogram

instance Export Histogram where
  export = export_ . unHistogram

instance Observe Histogram where
  observe a = observe_ a . unHistogram

histogram :: Info -> [Pure.Bucket] -> Metric Histogram
histogram info buckets = Metric $ let new = Pure.construct buckets
                                  in Impure (Identity new) info (Identity new)



-- | A [complex φ-quantile summary](https://prometheus.io/docs/concepts/metric_types/#summary)
newtype Summary = Summary { unSummary :: Impure () TVar Pure.Estimator }

type instance Rank Summary = Identity

type instance Purify Summary = Pure.Estimator

type instance Extra Summary = Identity

instance Register Summary where
  register = fmap Summary . register_

instance Extract Summary Pure.Summary where
  extract = extract_ . unSummary

instance Export Summary where
  export = export_ . unSummary

instance Observe Summary where
  observe a = observe_ a . unSummary

summary :: Info -> [Pure.Quantile] -> Metric Summary
summary info quantiles = Metric $ let new = Pure.construct quantiles
                                  in Impure (Identity new) info (Identity new)
