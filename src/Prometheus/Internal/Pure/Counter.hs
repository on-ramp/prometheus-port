{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Counter where

import           Prometheus.Internal.Pure.Base

import           Control.DeepSeq
import           Data.String
import           Data.Functor.Identity



newtype Counter = Counter { unCounter :: Double }
                  deriving NFData

instance Construct () Counter where
  construct () = Counter 0

instance Name Counter where
  name _ = "counter"

instance Extract Counter Double where
  extract = unCounter

instance Export Counter where
  export = pure . DoubleSample "" [] . unCounter

instance Increment Counter where
  plus a (Counter c) = Counter $ max c (c + a)

instance Set Counter where
  set a (Counter c) = Counter $ max c a
