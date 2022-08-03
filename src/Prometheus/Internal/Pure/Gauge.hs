{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Gauge where

import           Prometheus.Internal.Pure.Base

import           Control.DeepSeq
import           Data.String



newtype Gauge = Gauge { unGauge :: Double }
                deriving NFData

instance Construct () Gauge where
  construct () = Gauge 0

instance Name Gauge where
  name _ = "gauge"

instance Extract Gauge Double where
  extract = unGauge

instance Export Gauge where
  export = pure . DoubleSample "" [] . unGauge

instance Increment Gauge where
  plus a = Gauge . (+) a . unGauge

instance Decrement Gauge where
  minus a = Gauge . subtract a . unGauge

instance Set Gauge where
  set = const . Gauge
