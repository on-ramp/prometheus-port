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


-- | Models multi-dimensional metrics shared under the same name.
--
-- @
-- 'Vector Label2 Counter' corresponds to the 2-dimensional counter metric e.g. coordinates in a plane.
-- 'Vector Label3 Counter' corresponds to the 3-dimensional counter metric e.g. coordinates in a space.
-- @
newtype Vector label metric = Vector { unVector :: Impure (label, Purify metric) (Compose TVar (Map label)) (Purify metric) }

type instance Rank (Vector label metric) = Map label

type instance Purify (Vector label metric) = Purify metric

type instance Extra (Vector label metric) = (,) label

-- | Create a new 'Vector' of dimensions @label@ and metric @Metric metric@.
--
-- @
-- let coordMetrics = vector ("x", "y") $ counter (Info "coordinates" "Coordinates counter in a 2-dimensional space")
-- let httpMetrics = vector ("path", "method", "status") $ histogram (Info "http_req_duration" "HTTP request duration (in seconds)") buckets
-- @
--
-- The constraint @Extra metric ~ Identity@ limits the @metric@ type to the primitive metrics (e.g. 'Counter', 'Histogram'...).
vector :: (Extra metric ~ Identity) => label -> Metric metric -> Metric (Vector label metric)
vector label (Metric (Impure (Identity def) info _base)) = Metric $ Impure (label, def) info Map.empty

-- | Operate on a specific metric of a 'Vector'
--
-- @
-- httpMetrics :: Vector3 Histogram
-- httpMetrics = vector ("path", "method", "status") $
--   histogram (Info "http_req_duration" "HTTP request duration (in seconds)") buckets
--
-- withLabel ("/users", "GET", "200") $ observe time
-- @
withLabel :: label -> Vector label metric -> ((label, Vector label metric) -> t) -> t
withLabel label v a = a (label, v)


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

    
class Zip label where
  zipper :: Ord label => label -> label -> Tags
    
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
  zipper (a, b, c, d, e, f) (n, o, p, q, r, metric) = [(a, n), (b, o), (c, p), (d, q), (e, r), (metric, f)]
    
instance Zip Label7 where    
  zipper (a, b, c, d, e, f, g) (n, o, p, q, r, metric, t) = [(a, n), (b, o), (c, p), (d, q), (e, r), (metric, f), (t, g)]


instance Register (Vector label metric) where
  register (Metric (Impure def info _)) = do
    tvar <- newTVarIO Map.empty
    return . Vector . Impure def info $ Compose tvar

instance Pure.Extract (Purify metric) e => Extract (Vector label metric) (Map label e) where
  extract (Vector (Impure _ _ (Compose tvar))) =
    fmap Pure.extract <$> readTVarIO tvar

instance (Zip label, Ord label, Pure.Name (Purify metric), Pure.Export (Purify metric)) => Export (Vector label metric) where
  export (Vector (Impure (tags, _) info (Compose tvar))) =
    Template info (Pure.name (Proxy :: Proxy (Purify metric))) . f tags . Map.toList <$> readTVarIO tvar
    where
      f :: label -> [(label, Purify metric)] -> [Pure.Sample]
      f k = foldMap $ \(label, o) -> Pure.addTags (zipper k label) $ Pure.export o

instance (NFData label, NFData (Purify metric), Ord label, Pure.Increment (Purify metric)) => Increment (label, Vector label metric) where
  plus a (label, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.plus a . fromMaybe def) label

instance (NFData label, NFData (Purify metric), Ord label, Pure.Decrement (Purify metric)) => Decrement (label, Vector label metric) where
  minus a (label, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.minus a . fromMaybe def) label

instance (NFData label, NFData (Purify metric), Ord label, Pure.Set (Purify metric)) => Set (label, Vector label metric) where
  set a (label, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.set a . fromMaybe def) label

instance (NFData label, NFData (Purify metric), Ord label, Pure.Observe (Purify metric)) => Observe (label, Vector label metric) where
  observe a (label, Vector (Impure (_, def) _info (Compose tvar))) =
    atomically . modifyTVar' tvar $ force . Map.alter (Just . Pure.observe a . fromMaybe def) label
