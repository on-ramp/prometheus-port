{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Prometheus.Vector where

import           Prometheus.Internal.Base
import           Prometheus.Internal.Pure.Base

import           Protolude

import           Control.Concurrent.STM.TVar
import qualified Data.Map as Map



type family Vector' (l :: *) (a :: *)

type instance Vector' l (Impure o Identity i) = Impure (l, o) (Map l) i

newtype Vector l s = Vector { unVector :: Vector' l s }

type instance Glue (Vector l (Impure o Identity i)) = Metric' (l, o) (Map l) i



type Vector1 = Vector  LByteString
type Vector2 = Vector (LByteString, LByteString)
type Vector3 = Vector (LByteString, LByteString, LByteString)
type Vector4 = Vector (LByteString, LByteString, LByteString, LByteString)



instance ( Glue           s  ~ Metric'     o  Identity i
         , Glue (Vector l s) ~ Metric' (l, o) (Map l)  i
         )
         => Constructible (l, Metric s) (Vector l s) where
  construct (l, Metric (Metric' (info, o) _)) = Metric . Metric' (info, (l, o)) $ Impure (info, (l, o))

instance (       Vector' l s  ~ Impure  (l, o) (Map l) i
         , Glue (Vector  l s) ~ Metric' (l, o) (Map l) i
         )
         => Registrable (Vector l s) where
  register (Metric (Metric' _ raw)) =
    Vector . raw <$> newTVarIO (Pure $ Map.empty)

instance ( Vector' l s ~ Impure (l, o) (Map l) i
         , PureExtractable e i
         , m ~ Map l e
         )
         => Extractable m (Vector l s) where
  extract (Vector (Impure _ s)) =
    fmap pureExtract . unPure <$> readTVarIO s

instance ( Label l
         , Vector' l s ~ Impure (l, o) (Map l) i
         , PureNamed i
         , PureExportable i
         )
         => Exportable (Vector l s) where
  export (Vector (Impure (info, (labels, _)) i)) =
    let proxy = Proxy :: Proxy i
    in Template info (pureName proxy) . mconcat . fmap f . Map.toList
          . unPure <$> readTVarIO i
    where
      f (l, o) = addTags (flatten labels l) <$> pureExport o



instance ( Vector' l s ~ Impure (l, o) f i
         , PureConstructible o b
         , PureIncrementable b
         , PureMappable l (Pure f i) b
         )
         => Incrementable (l, Vector l s) where
  increment (l, Vector (Impure (_, (_, o)) s))   =
    atomically . modifyTVar' s $ pmap l (pureConstruct o) pureIncrement

  (.+.)     (l, Vector (Impure (_, (_, o)) s)) d =
    atomically . modifyTVar' s $ pmap l (pureConstruct o) (+.+ d)

instance ( Vector' l s ~ Impure (l, o) f i
         , PureConstructible o b
         , PureDecrementable b
         , PureMappable l (Pure f i) b
         )
         => Decrementable (l, Vector l s) where
  decrement (l, Vector (Impure (_, (_, o)) s))   =
    atomically . modifyTVar' s . pmap l (pureConstruct o) $ pureDecrement

  (.-.)     (l, Vector (Impure (_, (_, o)) s)) d =
    atomically . modifyTVar' s . pmap l (pureConstruct o) $ (-.- d)

instance ( Vector' l s ~ Impure (l, o) f i
         , PureConstructible o b
         , PureSettable b
         , PureMappable l (Pure f i) b
         )
         => Settable (l, Vector l s) where
  (.=.)     (l, Vector (Impure (_, (_, o)) s)) d =
    atomically . modifyTVar' s . pmap l (pureConstruct o) $ (=.= d)

instance ( Vector' l s ~ Impure (l, o) f i
         , PureConstructible o b
         , PureObservable b
         , PureMappable l (Pure f i) b
         )
         => Observable (l, Vector l s) where
  observe   (l, Vector (Impure (_, (_, o)) s)) d =
    atomically . modifyTVar' s . pmap l (pureConstruct o) $ flip pureObserve d
