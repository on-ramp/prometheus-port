{-# LANGUAGE BangPatterns
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

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
  {-# INLINE extract #-}

instance Export Counter where
  export = pure . DoubleSample "" [] . unCounter
  {-# INLINE export #-}

instance Increment Counter where
  plus a (Counter c) = Counter $
    let !c' = c + a
    in force $ max c c'
  {-# INLINE plus #-}

instance Set Counter where
  set !a (Counter c) = Counter $ force $ max c a
  {-# INLINE set #-}
