{-# LANGUAGE BangPatterns
           , DerivingStrategies
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
  {-# INLINE extract #-}

instance Export Gauge where
  export = pure . DoubleSample "" [] . unGauge
  {-# INLINE export #-}

instance Increment Gauge where
  plus a = Gauge . force . (+) a . unGauge
  {-# INLINE plus #-}

instance Decrement Gauge where
  minus a = Gauge . force . subtract a . unGauge
  {-# INLINE minus #-}

instance Set Gauge where
  set !a = const (Gauge a)
  {-# INLINE set #-}
