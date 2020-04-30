{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Counter where

import           Prometheus.Internal.Pure.Base

import           Protolude

import           Data.Default



-- | A pure Counter is merely a wrapper around 'Double' with an instance of 'PureIncrementable'
newtype Counter = Counter { unCounter :: Double }
                  deriving Default

pureUnsafeSet :: Pure Identity Counter -> Double -> Pure Identity Counter
pureUnsafeSet _ = Pure . Counter



instance PureNamed Counter where
  pureName _ = "counter"

instance PureConstructible () Counter where
  pureConstruct _ = Counter 0

type instance Extract Counter = Double

instance Extract Counter ~ e => PureExtractable e Counter where
  pureExtract = unCounter

instance PureExportable Counter where
  pureExport (Counter v) = [ DoubleSample "" [] v ]

instance PureIncrementable Counter where
  pureIncrement = Counter . (+ 1) . unCounter
  (+.+)     a d = Counter . (+ d) $ unCounter a

instance PureSettable Counter where
  (=.=) (Counter a) d =
    if d > a
      then Counter d
      else Counter a
