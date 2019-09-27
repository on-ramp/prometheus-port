{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prometheus
  ( -- * Basic metric types
    counter
  , gauge
  , histogram
  , summary
    -- * Operations on metric types
    -- ** Construction/Teardown
  , Registrable (..)
  , GenericRegistrable
  , genericRegister
  , Extractable (..)
  , Prometheus.export
  , GenericExportable
  , genericExport
    -- ** Collecting data
  , Incrementable (..)
  , Shiftable (..)
  , Observable (..)
    -- * Vector
  , vector
  , withLabel
    -- * Additional operations
  , time
  , push
    -- * Additional types
  , Info (..)
    -- * Re-exports
  , module Data.Default
  , module Prometheus.Primitive
  , module Prometheus.Vector
  , NoIdentity
  , Metric
  ) where

import qualified Prometheus.Internal.Base       as Base
import           Prometheus.Internal.Base hiding ( genericExport )
import           Prometheus.Internal.Pure        ( Bucket, Quantile, NoIdentity, Label )
import           Prometheus.Primitive
import           Prometheus.Vector

import           Protolude               

import           Network.HTTP.Client.Conduit
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

import           Data.Default
import           Data.Time.Clock



-- | A [monotonically increasing counter](https://prometheus.io/docs/concepts/metric_types/#counter).
--
--   Member of typeclass 'Incrementable'.
--
--   Note: do __not__ use '.+.' with negative values when using counters,
--   [it makes kittens cry](https://prometheus.io/docs/instrumenting/writing_clientlibs/#counter)
counter :: Info -> Metric Counter
counter = construct . (, ())

-- | A [freely shiftable gauge](https://prometheus.io/docs/concepts/metric_types/#gauge).
--
--   Member of typeclasses 'Incrementable' and 'Shiftable'.
gauge :: Info -> Metric Gauge
gauge = construct . (, ())

-- | A [simple cumulative histogram](https://prometheus.io/docs/concepts/metric_types/#histogram)
--
--   Member of typeclass 'Observable'.
histogram :: Info -> [Bucket] -> Metric Histogram
histogram = curry construct

-- | A [complicated φ-quantile summary](https://prometheus.io/docs/concepts/metric_types/#summary)
--
--   Member of typeclass 'Observable'.
summary :: Info -> [Quantile] -> Metric Summary
summary = curry construct



export :: Exportable a => a -> IO LByteString
export = fmap template . Base.export

-- | Convert any 'Generic' metric datatype into a Prometheus-compatible 'LByteString'
genericExport :: GenericExportable f => f Identity -> IO LByteString
genericExport = fmap (mconcat . fmap template) . Base.genericExport



-- | A 'Vector' is an array of similar metrics that differ only in labels.
vector :: ( Label l
          , Glue           s  ~ Metric'     o  Identity i
          , Glue (Vector l s) ~ Metric' (l, o) (Map l)  i
          )
       => l -> Metric s -> Metric (Vector l s)
vector = curry construct

-- | The only way to use a vector.
--
--   If basic metrics are used as e.g.
--
--   > metric `observe` 2.6
--
--   then the 'withLabel' usage looks like
--
--   > withLabel "label" metric (`observe` 2.6)
withLabel :: Label l => l -> Vector l s -> ((l, Vector l s) -> t) -> t
withLabel l v a = a (l, v)



-- | Measure the time it takes for the action to process, then passes time to the action
--   in a separate thread.
--
--   The first argument is either 'liftIO' in case of 'MonadIO' or 'identity' in case of 'IO'
time :: Monad m => (forall b. IO b -> m b) -> (Double -> IO ()) -> m a -> m a
time toIO f action = do
  t <- toIO getCurrentTime
  result <- action
  t' <- toIO getCurrentTime
  _thread <- toIO . forkIO . f . realToFrac $ diffUTCTime t' t
  return result


-- | Pushes metrics to a server.
--
--   Every push is attempted only once,
--   any connection failure or non-2XX status codes drop messages to stdout
push
  :: IO LByteString
  -> [Char] -- ^ Server to push to
  -> IO ()
push exportfunc mayAddress = do
  case parseRequest mayAddress of
    Nothing      -> putText $ show mayAddress <> " could not be parsed"
    Just address -> do
      exported <- exportfunc
      sent <- try $ httpBS
                      . setRequestMethod "POST"
                      . setRequestBodyLBS exported
                      $ address

      let Left (err :: SomeException) = sent
          Right response = sent
          status = statusCode $ getResponseStatus response

      if | isLeft sent -> putText $ mconcat [ "Could not send metrics to address "
                                            , show address
                                            , " due to "
                                            , show err
                                            ]

         | status < 200 || status >= 300 -> putText $ mconcat [ "Is the metrics server at address "
                                                              , show address
                                                              , " fine? It's returning status "
                                                              , show status
                                                              ]
         | otherwise -> return ()
