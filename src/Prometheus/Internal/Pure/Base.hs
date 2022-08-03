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



type Tags = [(BSL.ByteString, BSL.ByteString)]

type Label1 =  BSL.ByteString
type Label2 = (BSL.ByteString, BSL.ByteString)
type Label3 = (BSL.ByteString, BSL.ByteString, BSL.ByteString)
type Label4 = (BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString)
type Label5 = (BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString)
type Label6 = (BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString)
type Label7 = (BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString, BSL.ByteString)

class Zip l where
  zipper :: Ord l => l -> l -> Tags

instance Zip Label1 where
  zipper a n = [(a, n)]

instance Zip Label2 where
  zipper (a, b) (n, o) = [(a, n), (b, o)]

instance Zip Label3 where
  zipper (a, b, c) (n, o, p) = [(a, n), (b, o), (c, p)]

instance Zip Label4 where
  zipper (a, b, c, d) (n, o, p, q) = [(a, n), (b, o), (c, p), (d, q)]

instance Zip Label5 where
  zipper (a, b, c, d, e) (n, o, p, q, r) = [(a, n), (b, o), (c, p), (d, q), (e, r)]

instance Zip Label6 where
  zipper (a, b, c, d, e, f) (n, o, p, q, r, s) = [(a, n), (b, o), (c, p), (d, q), (e, r), (s, f)]

instance Zip Label7 where
  zipper (a, b, c, d, e, f, g) (n, o, p, q, r, s, t) = [(a, n), (b, o), (c, p), (d, q), (e, r), (s, f), (t, g)]
