{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , OverloadedStrings
           , PatternSynonyms
           , PolyKinds
           , TypeFamilies
           , TypeFamilyDependencies
           , TypeOperators
           , UndecidableInstances #-}

module Prometheus.Internal.Base
  ( Tags
  , Info(..)
  , pattern Info
  , Impure(..)
  , Rank
  , Purify
  , Extra
  , Metric(..)
  , tag
  , genericTag
  , GTag(..)
  , Register(..)
  , Extract(..)
  , Export(..)
  , genericRegister
  , GRegister(..)
  , genericExport
  , GExport(..)
  , Increment(..)
  , Decrement(..)
  , Set(..)
  , Observe(..)
  , Template(..)
  , escape
  , template
  ) where


import           Prometheus.Internal.Pure.Base (Sample (..))

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           Data.Kind
import           GHC.Generics


-- | List of tags where @[(label, value)]@.
type Tags = [(BSLC.ByteString, BSLC.ByteString)]


-- | Basic description of metrics
data Info = MkInfo
              { iName :: BSLC.ByteString -- ^ Name of the metric
              , iHelp :: BSLC.ByteString -- ^ Additional commentary
              , iTags :: Tags            -- ^ Additional tags
              }

pattern Info :: BSLC.ByteString -> BSLC.ByteString -> Info
pattern Info name help <- MkInfo name help _
  where
    Info name help = MkInfo name help []


-- | A core datatype of this module, takes an existing /Pure/ implementation of a metric
-- and moves it in a separate 'Control.Concurrent.STM.TVar' with additional information on the side.
data Impure def f metric = Impure def Info (f metric)


-- | Returns the effect associated with the actual representation of a metric.
--
-- For most metrics, this is just `Identity`.
-- For vectors is a @Map label@ since each metric of the vector will be associated to a specific label.
--
-- @
-- Rank Gauge ~ Identity
-- Rank (Vector label metric) ~ Map l
-- @
type family Rank (metric :: Type) :: Type -> Type

-- | Returns the 'Prometheus.Internal.Pure.Base' version.
--
-- @
-- Purify Gauge ~ Pure.Gauge
-- Purify (Vector label metric) ~ Purify metric
-- @
type family Purify (metric :: Type) :: Type

-- | Returns additional data associated to the metric.
--
-- For most metrics, this is just `Identity`.
--
-- @
-- Extra Gauge ~ Identity
-- Extra (Vector label metric) ~ (,) label
-- @
type family Extra (metric :: Type) :: Type -> Type

-- | A metric represents a value that is being monitored.
--
-- @
-- Metric Counter ~ Impure (Identity P.Counter) Info (Identity P.Counter)
--        Gauge                      P.Gauge                   P.Gauge
--        Histogram                  P.Histogram               P.Histogram
--        Summary                    P.Summary                 P.Summary
--
-- Metric (Vector label metric) ~ Impure (label, Purify metric) Info (Map label (Purify metric))
-- @
newtype Metric m = Metric { unMetric :: Impure (Extra m (Purify m)) (Rank m) (Purify m) }


-- | Appends 'Tags' to the given 'Metric'.
tag :: Tags -> Metric metric -> Metric metric
tag t' (Metric (Impure def (MkInfo n h t) metric)) = Metric $ Impure def (MkInfo n h $ t <> t') metric
{-# INLINABLE tag #-}

-- | Provides 'tag' for any type that derives 'Generic'.
genericTag
  :: ( Generic (f Metric)
     , GTag (Rep (f Metric))
     )
  => Tags -> f Metric -> f Metric
genericTag t = to . gtag t . from
{-# INLINABLE genericTag #-}

class GTag i where
  gtag :: Tags -> i a -> i a

instance GTag U1 where
  gtag _ U1 = U1

instance (GTag i, GTag j) => GTag (i :*: j) where
  gtag t (i :*: j) = gtag t i :*: gtag t j

instance GTag i => GTag (M1 k m i) where
  gtag t (M1 m) = M1 $ gtag t m

instance GTag (K1 a (Metric metric)) where
  gtag t (K1 k) = K1 $ tag t k

instance ( Generic (f Metric)
         , GTag (Rep (f Metric))
         )
        => GTag (K1 a (f Metric)) where
  gtag t (K1 k) = K1 . to . gtag t $ from k


class Register metric where
  register :: Metric metric -> IO metric

class Extract metric e | metric -> e where
  extract :: metric -> IO e

class Export metric where
  export :: metric -> IO Template

-- | Provides 'register' for any type that derives 'Generic'.
--
-- @
-- data ServerMetrics f =
--   ServerMetrics
--     { smErrors  :: No Identity f Counter
--     , smLatency :: No Identity f Summary
--     , ...
--     }
--   deriving stock Generic
--
-- serverMetrics :: ServerMetrics Metric
-- serverMetrics = ServerMetrics
--   {
--   , smErrors = counter (Info "errors" "Fatal errors registered on the server")
--   , smLatency = summary (Info "latency" "Server's latency to response reply") defQuantiles
--   }
--
-- main = do
--   ServerMetrics{..} <- genericRegister serverMetrics
--   ...
--   increment smErrors
--   ...
--   observe latency smLatency
--   ...
-- @
genericRegister
  :: ( Generic (f Metric)
     , Generic (f Identity)
     , GRegister (Rep (f Metric)) (Rep (f Identity))
     )
  => f Metric -> IO (f Identity)
genericRegister = fmap to . gregister . from
{-# INLINABLE genericRegister #-}

class GRegister i o where
  gregister :: i a -> IO (o a)

instance GRegister U1 U1 where
  gregister U1 = pure U1

instance (GRegister i o, GRegister j p) =>
         GRegister (i :*: j) (o :*: p) where
  gregister (l :*: r) = (:*:) <$> gregister l <*> gregister r

instance GRegister i o => GRegister (M1 k m i) (M1 l n o) where
  gregister (M1 m) = M1 <$> gregister m

instance Register metric => GRegister (K1 a (Metric metric)) (K1 a metric) where
  gregister (K1 k) = K1 <$> register k

instance ( Generic (f Metric)
         , Generic (f Identity)
         , GRegister (Rep (f Metric)) (Rep (f Identity))
         )
        => GRegister (K1 a (f Metric)) (K1 a (f Identity)) where
  gregister (K1 k) = K1 <$> genericRegister k


-- | Provides 'export' for any type that derives 'Generic'.
genericExport
  :: ( Generic (f Identity)
     , GExport (Rep (f Identity))
     )
  => f Identity -> IO [Template]
genericExport = gexport . from
{-# INLINABLE genericExport #-}

class GExport i where
  gexport :: i a -> IO [Template]

instance GExport U1 where
  gexport U1 = pure []

instance (GExport i, GExport j) => GExport (i :*: j) where
  gexport (l :*: r) = (<>) <$> gexport l <*> gexport r

instance GExport i => GExport (M1 k m i) where
  gexport (M1 m) = gexport m

instance {-# OVERLAPPABLE #-} Export metric => GExport (K1 a metric) where
  gexport (K1 k) = pure <$> export k

instance ( Generic (f Identity)
         , GExport (Rep (f Identity))
         )
        => GExport (K1 a (f Identity)) where
  gexport (K1 k) = gexport $ from k


class Increment metric where
  increment :: metric -> IO ()
  increment = plus 1

  plus :: Double -> metric -> IO ()

class Decrement metric where
  decrement :: metric -> IO ()
  decrement = minus 1

  minus :: Double -> metric -> IO ()

class Set metric where
  set :: Double -> metric -> IO ()

class Observe metric where
  observe :: Double -> metric -> IO ()


data Template = Template Info BSLC.ByteString [Sample]

escape :: BSLC.ByteString -> BSLC.ByteString
escape bs =
  let (bef, aft) = BSLC.break (flip elem ("\t\n\v\f\r\"\\" :: String)) bs

      conv '\t' = "\\t"
      conv '\n' = "\\n"
      conv '\v' = "\\v"
      conv '\f' = "\\f"
      conv '\r' = "\\r"
      conv '"'  = "\\\""
      conv '\\' = "\\\\"
      conv char = BSLC.singleton char

  in case BSLC.uncons aft of
       Nothing      -> bef
       Just (b, bs) -> bef <> conv b <> escape bs

template :: Template -> BSLC.ByteString
template (Template (MkInfo name help extra) metric samples)
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
