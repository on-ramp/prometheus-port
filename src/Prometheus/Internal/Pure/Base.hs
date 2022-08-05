{-# LANGUAGE DeriveFunctor
           , FlexibleInstances
           , FunctionalDependencies
 #-}

{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Base where

import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.String



type Tags = [(BSL.ByteString, BSL.ByteString)]

data Sample = DoubleSample BSL.ByteString Tags Double
            | IntSample    BSL.ByteString Tags Int
              deriving Show

addTags :: Tags -> [Sample] -> [Sample]
addTags = fmap . add
  where
    add :: Tags -> Sample -> Sample
    add tags (DoubleSample a t d) = DoubleSample a (tags <> t) d
    add tags (IntSample    a t d) = IntSample    a (tags <> t) d



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
