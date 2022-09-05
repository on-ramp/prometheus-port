{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Counter
  ( Counter(..)
  ) where


import           Prometheus.Internal.Pure.Base

import           Control.DeepSeq


newtype Counter = Counter { unCounter :: Double }
                  deriving newtype NFData

instance Construct () Counter where
  construct () = Counter 0
  {-# INLINABLE construct #-}

instance Name Counter where
  name _ = "counter"
  {-# INLINABLE name #-}

instance Extract Counter Double where
  extract = unCounter
  {-# INLINABLE extract #-}

instance Export Counter where
  export = pure . DoubleSample "" [] . unCounter
  {-# INLINABLE export #-}

instance Increment Counter where
  plus a (Counter c) = Counter $ max c (c + a)
  {-# INLINABLE plus #-}

instance Set Counter where
  set a (Counter c) = Counter $ max c a
  {-# INLINABLE set #-}
