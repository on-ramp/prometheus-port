{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prometheus.Primitive where

import           Prometheus.Internal.Base
import           Prometheus.Internal.Pure hiding ( Counter, Gauge, Histogram )
import qualified Prometheus.Internal.Pure       as Pure

import           Protolude

import           Control.Concurrent.STM.TVar




type Counter   = Impure ()         Identity Pure.Counter
type Gauge     = Impure ()         Identity Pure.Gauge
type Histogram = Impure [Bucket]   Identity Pure.Histogram
type Summary   = Impure [Quantile] Identity      Estimator

type instance Glue (Impure o Identity s) = Metric' o Identity s



instance Glue (Impure o Identity s) ~ Metric' o Identity s
         => Constructible (Info, o) (Impure o Identity s) where
  construct o = Metric . Metric' o $ Impure o

instance ( Glue (Impure o f i) ~ Metric' o f i
         , PureConstructible o (Pure f i)
         )
         => Registrable (Impure o f i) where
  register (Metric (Metric' (_, i) raw)) =
    raw <$> newTVarIO ( pureConstruct i)

instance PureExtractable e i
         => Extractable e (Impure o Identity i) where
  extract (Impure _ s) =
    pureExtract <$> readTVarIO s

instance ( PureNamed i
         , PureExportable i
         )
         => Exportable (Impure o Identity i) where
  export (Impure (info, _) i) =
    let proxy = Proxy :: Proxy i
    in Template info (pureName proxy) . pureExport <$> readTVarIO i



instance PureIncrementable (Pure f i) => Incrementable (Impure o f i) where
  increment (Impure _ s)   = atomically $ modifyTVar' s pureIncrement
  (.+.)     (Impure _ s) d = atomically $ modifyTVar' s (+.+ d)

instance PureShiftable (Pure f i) => Shiftable (Impure o f i) where
  decrement (Impure _ s)   = atomically . modifyTVar' s $ pureDecrement
  (.-.)     (Impure _ s) d = atomically . modifyTVar' s $ (-.- d)
  (.=.)     (Impure _ s) d = atomically . writeTVar   s $ (=.=) d

instance PureObservable (Pure f i) => Observable (Impure o f i) where
  observe (Impure _ s) d = atomically . modifyTVar' s $ flip pureObserve d
