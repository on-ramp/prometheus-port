{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prometheus.Primitive where

import           Control.Concurrent.STM.TVar
import           Prometheus.Internal.Base    as Base
import           Prometheus.Internal.Pure    hiding (Counter, Gauge, Histogram, Summary)
import qualified Prometheus.Internal.Pure    as Pure
import           Prometheus.Vector
import           Protolude

type Counter = Impure () Identity Pure.Counter

type Gauge = Impure () Identity Pure.Gauge

type Histogram = Impure [Bucket] Identity Pure.Histogram

type Summary = Impure [Quantile] Identity Estimator

type instance Glue (Impure o Identity s) = Metric' o Identity s

instance Glue (Impure o Identity s) ~ Metric' o Identity s =>
         Constructible (Info, o) (Impure o Identity s) where
  construct o = Metric . Metric' o $ Impure o

instance (Glue (Impure o f i) ~ Metric' o f i, PureConstructible o (Pure f i)) =>
         Registrable (Impure o f i) where
  register (Metric (Metric' (_, i) raw)) = raw <$> newTVarIO (pureConstruct i)

instance PureExtractable e i => Extractable e (Impure o Identity i) where
  extract (Impure _ s) = pureExtract <$> readTVarIO s

instance (PureNamed i, PureExportable i) =>
         Exportable (Impure o Identity i) where
  export (Impure (info, _) i) =
    let proxy = Proxy :: Proxy i
     in Template info (pureName proxy) . pureExport <$> readTVarIO i

instance PureIncrementable (Pure f i) => Incrementable (Impure o f i) where
  increment (Impure _ s) = atomically $ modifyTVar' s pureIncrement
  (.+.) (Impure _ s) d = atomically $ modifyTVar' s (+.+ d)

instance PureDecrementable (Pure f i) => Decrementable (Impure o f i) where
  decrement (Impure _ s) = atomically . modifyTVar' s $ pureDecrement
  (.-.) (Impure _ s) d = atomically . modifyTVar' s $ (-.- d)

instance PureSettable (Pure f i) => Settable (Impure o f i) where
  (.=.) (Impure _ s) d = atomically . modifyTVar' s $ (=.= d)

instance PureObservable (Pure f i) => Observable (Impure o f i) where
  observe (Impure _ s) d = atomically . modifyTVar' s $ flip pureObserve d

-- | A [monotonically increasing counter](https://prometheus.io/docs/concepts/metric_types/#counter).
--
--   Member of typeclass 'Incrementable'.
--
--   Note: '.=.' only updates the counter if the new value is larger than
--   the previous one. Do __not__ use '.+.' with negative values when using counters,
--   [it makes kittens cry](https://prometheus.io/docs/instrumenting/writing_clientlibs/#counter)
counter :: Info -> Metric Counter
counter = construct . (, ())

-- | A [freely shiftable gauge](https://prometheus.io/docs/concepts/metric_types/#gauge).
--
--   Member of typeclasses 'Incrementable' and 'Shiftable'.
gauge :: Info -> Metric Gauge
gauge = construct . (, ())

-- | A [simple cumulative histogram](https://prometheus.io/docs/concepts/metric_types/#histogram)
--
--   Member of typeclass 'Observable'.
histogram :: Info -> [Bucket] -> Metric Histogram
histogram = curry construct

-- | A [complicated φ-quantile summary](https://prometheus.io/docs/concepts/metric_types/#summary)
--
--   Member of typeclass 'Observable'.
summary :: Info -> [Quantile] -> Metric Summary
summary = curry construct

export :: Exportable a => a -> IO LByteString
export = fmap template . Base.export

-- | A 'Vector' is an array of similar metrics that differ only in labels.
vector ::
     ( Glue s ~ Metric' o Identity i
     , Glue (Vector l s) ~ Metric' (l, o) (Map l) i
     )
  => l
  -> Metric s
  -> Metric (Vector l s)
vector = curry construct

-- | The only way to use a vector.
--
--   If basic metrics are used as e.g.
--
--   > metric `observe` 2.6
--
--   then the 'withLabel' usage looks like
--
--   > withLabel "label" metric (`observe` 2.6)
withLabel :: l -> Vector l s -> ((l, Vector l s) -> t) -> t
withLabel l v a = a (l, v)
