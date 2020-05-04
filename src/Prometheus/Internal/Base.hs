{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Prometheus.Internal.Base where

import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy          as BL
import           Prometheus.Internal.Pure.Base
import           Protolude

-- | Basic description of metrics
data Info =
  Info
    { iName           :: LByteString -- ^ Name of the metric
    , iHelp           :: LByteString -- ^ Additional commentary
    , iAdditionalTags :: [(LByteString, LByteString)]
    }
  deriving (Show)

infoM :: LByteString -> LByteString -> Info
infoM name help = Info name help []

-- | A wrapper that duplicates all of additional information stored by 'Impure' into a separate
--   argument. It's full duplication only because Vectors need to convert '_mUninitialized'
--   from Identity to (Map l) and that requires constructing another 'Impure'.
data Metric' o f s =
  Metric'
    { _mOptional      :: (Info, o)
    , _mUninitialized :: TVar (Pure f s) -> Impure o f s
    }

-- | A core datatype of this module, takes an existing 'Pure' implementation of a metric
--   and moves it in a separate 'TVar' with additional information on the side.
data Impure o f s =
  Impure (Info, o) (TVar (Pure f s))

-- | 'Glue' + 'Metric' serve as a way to hide pointless dependencies inside the library.
type family Glue (a :: *)

newtype Metric a =
  Metric
    { unMetric :: Glue a
    }

class Constructible o s where
  construct :: o -> Metric s

-- | Initialization of metrics.
--
--   For 'Generic' data structures use 'genericRegister'.
class Registrable s where
  register :: Metric s -> IO s

-- | Retrieval of what Metrics store inside.
class Extractable e s where
  extract :: s -> IO e

-- | Packing metrics into 'Template's.
--
--   Note: I tried to put 'genericExport' as a default Signature, but then it started
--         declining the 'Generic' instance with "No Exportable" class error :/
class Exportable s where
  export :: s -> IO Template

type GenericRegistrable f
   = ( Generic (f Metric)
     , Generic (f Identity)
     , GRegistrable (Rep (f Metric)) (Rep (f Identity)))

genericRegister :: GenericRegistrable f => f Metric -> IO (f Identity)
genericRegister = fmap to . gregister . from

-- | A 'Generic' 'register' wrapper. The types of `register` and `gregister` are
--   drastically different, so I don't think it's possible to make a default
--   signature out of `genericRegister`.
class GRegistrable i o where
  gregister :: i a -> IO (o a)

instance (GRegistrable i o, GRegistrable j p) =>
         GRegistrable (i :+: j) (o :+: p) where
  gregister (L1 l) = L1 <$> gregister l
  gregister (R1 r) = R1 <$> gregister r

instance (GRegistrable i o, GRegistrable j p) =>
         GRegistrable (i :*: j) (o :*: p) where
  gregister (l :*: r) = (:*:) <$> gregister l <*> gregister r

instance GRegistrable i o => GRegistrable (M1 k m i) (M1 l n o) where
  gregister (M1 m) = M1 <$> gregister m

instance Registrable s => GRegistrable (K1 a (Metric s)) (K1 a s) where
  gregister (K1 k) = K1 <$> register k

-- | A 'Generic' 'export' wrapper.
class GExportable i where
  gexport :: i a -> IO [Template]

instance (GExportable i, GExportable j) => GExportable (i :+: j) where
  gexport (L1 l) = gexport l
  gexport (R1 r) = gexport r

instance (GExportable i, GExportable j) => GExportable (i :*: j) where
  gexport (l :*: r) = (<>) <$> gexport l <*> gexport r

instance GExportable i => GExportable (M1 k m i) where
  gexport (M1 m) = gexport m

instance Exportable s => GExportable (K1 a s) where
  gexport (K1 k) = (: []) <$> export k

type GenericExportable f
   = (Generic (f Identity), GExportable (Rep (f Identity)))

genericExport' :: GenericExportable f => f Identity -> IO [Template]
genericExport' = gexport . from

-- | Convert any 'Generic' metric datatype into a Prometheus-compatible 'LByteString'
genericExport :: GenericExportable f => f Identity -> IO LByteString
genericExport = fmap (mconcat . fmap template) . genericExport'

class Incrementable s where
  increment :: s -> IO ()
  (.+.) :: s -> Double -> IO ()

class Decrementable s where
  decrement :: s -> IO ()
  (.-.) :: s -> Double -> IO ()

class Settable s where
  (.=.) :: s -> Double -> IO ()

class Observable s where
  observe :: s -> Double -> IO ()

data Template =
  Template Info LByteString [Sample]
  deriving (Show)

-- | Class of objects that can be transformed into Prometheus metrics.
--
--   This doesn't really have to be a class ¯\_(ツ)_/¯
template :: Template -> LByteString
template (Template (Info name help additionalTags) metric samples) =
  mconcat $
  ["# HELP ", name, " ", help, "\n", "# TYPE ", name, " ", metric, "\n"] <>
  fmap fromSamples samples
  where
    fromSamples (DoubleSample suffix labels value) =
      mconcat [name, suffix, fromLabels labels, " ", show value, "\n"]
    fromSamples (IntSample suffix labels value) =
      mconcat
        [ name
        , suffix
        , fromLabels (labels ++ additionalTags)
        , " "
        , show value
        , "\n"
        ]
    fromLabels [] = ""
    fromLabels labels =
      let expand (k, a) = mconcat [k, "=\"", a, "\""]
       in mconcat ["{", BL.intercalate ", " $ fmap expand labels, "}"]
