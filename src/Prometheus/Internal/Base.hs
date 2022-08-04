{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , OverloadedStrings
           , PolyKinds
           , TypeFamilies
           , TypeFamilyDependencies
           , TypeOperators
           , UndecidableInstances #-}

module Prometheus.Internal.Base where

import           Prometheus.Internal.Pure.Base (Sample (..))

import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           Data.Kind
import qualified Data.Text as Text
import           GHC.Generics



type Tags = [(BSLC.ByteString, BSLC.ByteString)]

-- Basic description of metrics
data Info = Info
              { iName :: BSLC.ByteString -- ^ Name of the metric
              , iHelp :: BSLC.ByteString -- ^ Additional commentary
              , iTags :: Tags            -- ^ Additional tags
              }
            deriving Show

mkInfo :: BSLC.ByteString -> BSLC.ByteString -> Info
mkInfo name help = Info name help []



--  A core datatype of this module, takes an existing 'Pure' implementation of a metric
--  and moves it in a separate 'TVar' with additional information on the side.
data Impure d f s = Impure d Info (f s)



type family Rank (s :: Type) :: Type -> Type

type family Purify (s :: Type) :: Type

type family Extra (s :: Type) :: Type -> Type

newtype Metric m = Metric { unMetric :: Impure (Extra m (Purify m)) (Rank m) (Purify m) }

extraTags :: Tags -> Metric s -> Metric s
extraTags t' (Metric (Impure d (Info n h t) s)) = Metric $ Impure d (Info n h $ t <> t') s



class Register s where
  register :: Metric s -> IO s

class Extract s e | s -> e where
  extract :: s -> IO e

class Export s where
  export :: s -> IO Template



genericRegister
  :: ( Generic (f Metric)
     , Generic (f Identity)
     , GRegister (Rep (f Metric)) (Rep (f Identity))
     )
  => f Metric -> IO (f Identity)
genericRegister = fmap to . gregister . from

class GRegister i o where
  gregister :: i a -> IO (o a)

instance (GRegister i o, GRegister j p) =>
         GRegister (i :+: j) (o :+: p) where
  gregister (L1 l) = L1 <$> gregister l
  gregister (R1 r) = R1 <$> gregister r

instance (GRegister i o, GRegister j p) =>
         GRegister (i :*: j) (o :*: p) where
  gregister (l :*: r) = (:*:) <$> gregister l <*> gregister r

instance GRegister i o => GRegister (M1 k m i) (M1 l n o) where
  gregister (M1 m) = M1 <$> gregister m

instance Register s => GRegister (K1 a (Metric s)) (K1 a s) where
  gregister (K1 k) = K1 <$> register k

instance ( Generic (f Metric)
         , Generic (f Identity)
         , GRegister (Rep (f Metric)) (Rep (f Identity))
         )
        => GRegister (K1 a (f Metric)) (K1 a (f Identity)) where
  gregister (K1 k) = K1 <$> genericRegister k



genericExport
  :: ( Generic (f Identity)
     , GExport (Rep (f Identity))
     )
  => f Identity -> IO [Template]
genericExport = gexport . from

class GExport i where
  gexport :: i a -> IO [Template]

instance (GExport i, GExport j) => GExport (i :+: j) where
  gexport (L1 l) = gexport l
  gexport (R1 r) = gexport r

instance (GExport i, GExport j) => GExport (i :*: j) where
  gexport (l :*: r) = (<>) <$> gexport l <*> gexport r

instance GExport i => GExport (M1 k m i) where
  gexport (M1 m) = gexport m

instance Export s => GExport (K1 a s) where
  gexport (K1 k) = pure <$> export k

instance ( Generic (f Identity)
         , GExport (Rep (f Identity))
         )
        => GExport (K1 a (f Identity)) where
  gexport (K1 k) = genericExport k



class Increment s where
  increment :: s -> IO ()
  increment = plus 1

  plus :: Double -> s -> IO ()

class Decrement s where
  decrement :: s -> IO ()
  decrement = minus 1

  minus :: Double -> s -> IO ()

class Set s where
  set :: Double -> s -> IO ()

class Observe s where
  observe :: Double -> s -> IO ()



data Template = Template Info BSLC.ByteString [Sample]
                deriving (Show)

escape :: BSLC.ByteString -> BSLC.ByteString
escape bs =
  let (bef, aft) = BSLC.break (== '/') bs
  in case BSLC.uncons aft of
       Nothing      -> bef
       Just (b, bs) -> "\\" <> escape bs

template :: Template -> BSLC.ByteString
template (Template (Info name help extra) metric samples)
  | null samples = mempty
  | otherwise    = mconcat $ [ "# HELP ", name, " ", help  , "\n"
                             , "# TYPE ", name, " ", metric, "\n"
                             ]
                               <> fmap fromSamples samples
 where
  fromSamples (DoubleSample suffix labels value) =
    mconcat
      [name, suffix, fromLabels $ labels <> extra, " ", BSLC.pack $ show value, "\n"]

  fromSamples (IntSample suffix labels value) =
    mconcat
      [name, suffix, fromLabels $ labels <> extra, " ", BSLC.pack $ show value, "\n"]

  fromLabels []     = ""
  fromLabels labels = let expand (k, a) = mconcat [k, "=\"", escape a, "\""]
                      in mconcat ["{", BSLC.intercalate ", " $ fmap expand labels, "}"]
