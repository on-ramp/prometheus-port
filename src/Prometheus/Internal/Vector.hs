{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeFamilies
           , UndecidableInstances #-}

module Prometheus.Internal.Vector
  ( Vector(..)
  , vector
  , withLabel
  , Vector1
  , Vector2
  , Vector3
  , Vector4
  , Vector5
  , Vector6
  , Vector7
  , Label1
  , Label2
  , Label3
  , Label4
  , Label5
  , Label6
  , Label7
  , Zip(..)
  ,
  ) where

import           Prometheus.Internal.Base
import qualified Prometheus.Internal.Pure.Base as Pure

import           Control.Concurrent.STM.TVar
import           Control.DeepSeq
import           Control.Monad.STM
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy


newtype Vector l s = Vector { unVector :: Impure (l, Purify s) (Compose TVar (Map l)) (Purify s) }

type instance Rank (Vector l s) = Map l

type instance Purify (Vector l s) = Purify s

type instance Extra (Vector l s) = (,) l

vector :: Extra s ~ Identity => l -> Metric s -> Metric (Vector l s)
vector l (Metric (Impure (Identity def) info _base)) = Metric $ Impure (l, def) info Map.empty

withLabel :: l -> Vector l s -> ((l, Vector l s) -> t) -> t
withLabel l v a = a (l, v)


type Vector1 = Vector Label1
type Vector2 = Vector Label2
type Vector3 = Vector Label3
type Vector4 = Vector Label4
type Vector5 = Vector Label5
type Vector6 = Vector Label6
type Vector7 = Vector Label7


type Label1 =  BSLC.ByteString    
type Label2 = (BSLC.ByteString, BSLC.ByteString)    
type Label3 = (BSLC.ByteString, BSLC.ByteString, BSLC.ByteString)    
type Label4 = (BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString)    
type Label5 = (BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString)    
type Label6 = (BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString)    
type Label7 = (BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString, BSLC.ByteString)

    
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


instance Register (Vector l s) where
  register (Metric (Impure def info _)) = do
    tvar <- newTVarIO Map.empty
    return . Vector . Impure def info $ Compose tvar

instance Pure.Extract (Purify s) e => Extract (Vector l s) (Map l e) where
  extract (Vector (Impure _ _ (Compose tvar))) =
    fmap Pure.extract <$> readTVarIO tvar

instance (Zip l, Ord l, Pure.Name (Purify s), Pure.Export (Purify s)) => Export (Vector l s) where
  export (Vector (Impure (tags, _) info (Compose tvar))) =
    Template info (Pure.name (Proxy :: Proxy (Purify s))) . f tags . Map.toList <$> readTVarIO tvar
    where
      f :: l -> [(l, Purify s)] -> [Pure.Sample]
      f k = foldMap $ \(l, o) -> Pure.addTags (zipper k l) $ Pure.export o

instance (NFData l, NFData (Purify s), Ord l, Pure.Increment (Purify s)) => Increment (l, Vector l s) where
  plus a (l, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.plus a . fromMaybe def) l

instance (NFData l, NFData (Purify s), Ord l, Pure.Decrement (Purify s)) => Decrement (l, Vector l s) where
  minus a (l, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.minus a . fromMaybe def) l

instance (NFData l, NFData (Purify s), Ord l, Pure.Set (Purify s)) => Set (l, Vector l s) where
  set a (l, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.set a . fromMaybe def) l

instance (NFData l, NFData (Purify s), Ord l, Pure.Observe (Purify s)) => Observe (l, Vector l s) where
  observe a (l, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.observe a . fromMaybe def) l
