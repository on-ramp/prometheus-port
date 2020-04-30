{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Gauge where

import           Prometheus.Internal.Pure.Base

import           Protolude

import           Data.Default



-- | A pure Gauge is merely a wrapper around 'Double' with instances of 'PureIncrementable'
--   and 'PureShiftable'
newtype Gauge = Gauge { unGauge :: Double }
                deriving Default

instance PureNamed Gauge where
  pureName _ = "gauge"

instance PureConstructible () Gauge where
  pureConstruct () = Gauge 0

type instance Extract Gauge = Double

instance Extract Gauge ~ e => PureExtractable e Gauge where
  pureExtract = unGauge

instance PureExportable Gauge where
  pureExport (Gauge v) = [ DoubleSample "" [] v ]

instance PureIncrementable Gauge where
  pureIncrement = Gauge . (+ 1) . unGauge
  (+.+)     a d = Gauge . (+ d) $ unGauge a

instance PureDecrementable Gauge where
  pureDecrement = Gauge . (1 `subtract`) . unGauge
  (-.-)     a d = Gauge . (d `subtract`) $ unGauge a

instance PureSettable Gauge where
  (=.=)     _   = Gauge
