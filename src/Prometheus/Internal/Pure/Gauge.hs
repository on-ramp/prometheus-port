{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Gauge
  ( Gauge(..)
  ) where


import           Prometheus.Internal.Pure.Base

import           Control.DeepSeq


newtype Gauge = Gauge { unGauge :: Double }
                deriving newtype NFData

instance Construct () Gauge where
  construct () = Gauge 0
  {-# INLINABLE construct #-}

instance Name Gauge where
  name _ = "gauge"
  {-# INLINABLE name #-}

instance Extract Gauge Double where
  extract = unGauge
  {-# INLINABLE extract #-}

instance Export Gauge where
  export = pure . DoubleSample "" [] . unGauge
  {-# INLINABLE export #-}

instance Increment Gauge where
  plus a = Gauge . (+) a . unGauge
  {-# INLINABLE plus #-}

instance Decrement Gauge where
  minus a = Gauge . subtract a . unGauge
  {-# INLINABLE minus #-}

instance Set Gauge where
  set = const . Gauge
  {-# INLINABLE set #-}
