{-# LANGUAGE DeriveFunctor
           , DerivingStrategies
           , FlexibleInstances
           , FunctionalDependencies
 #-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Prometheus.Internal.Pure.Base
  ( Tags
  , Sample(..)
  , addTags
  , Construct(..)
  , Name(..)
  , Export(..)
  , Extract(..)
  , Increment(..)
  , Decrement(..)
  , Set(..)
  , Observe(..)
  ) where

import           Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy
import           Data.String
import           GHC.Generics (Generic)

type Tags = [(BSL.ByteString, BSL.ByteString)]

data Sample = DoubleSample BSL.ByteString Tags {-# UNPACK #-}!Double
            | IntSample    BSL.ByteString Tags {-# UNPACK #-}!Int
              deriving stock (Show, Generic)
              deriving anyclass (NFData)

addTags :: Tags -> [Sample] -> [Sample]
addTags = fmap . add
  where
    add :: Tags -> Sample -> Sample
    add tags (DoubleSample a t d) = DoubleSample a (tags <> t) d
    add tags (IntSample    a t d) = IntSample    a (tags <> t) d
{-# INLINABLE addTags #-}

class Construct c a | a -> c where
  construct :: c -> a

class Name a where
  name :: IsString s => Proxy a -> s

class Export a where
  export :: a -> [Sample]

class Extract a e | a -> e where
  extract :: a -> e

class Increment a where
  plus :: Double -> a -> a

class Decrement a where
  minus :: Double -> a -> a

class Set a where
  set :: Double -> a -> a

class Observe a where
  observe :: Double -> a -> a
