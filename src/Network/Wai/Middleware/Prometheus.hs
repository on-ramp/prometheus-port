{-# LANGUAGE DeriveAnyClass #-}

module Network.Wai.Middleware.Prometheus
  ( prometheus
  , prometheusHandlerValue
  , HttpMetrics
  , httpMetrics
  ) where

import           Data.Time.Clock.POSIX         (getPOSIXTime)
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai

import           Prometheus.Internal.Base
import           Prometheus.Internal.Pure.Base
import           Prometheus.Primitive
import           Prometheus.Vector
import           Protolude

data HttpMetrics f =
  HttpMetrics
    { _hmLatency :: NoIdentity f (Vector3 Histogram)
    , _hmStatus  :: NoIdentity f (Vector3 Counter)
    }
  deriving Generic

{-# NOINLINE httpMetrics #-}
httpMetrics :: Text -> Maybe Tags -> HttpMetrics Metric
httpMetrics component maybeTags =
  HttpMetrics
    { _hmLatency =
        vector ("handler", "method", "status_code") $
        histogram
          infoL
          [ 0
          , 0.2
          , 0.4
          , 0.5
          , 0.7
          , 1
          , 10
          , 50
          , 100
          , 200
          , 500
          , 1000
          , 2000
          , 5000
          , 10000
          ]
    , _hmStatus = vector ("handler", "method", "status_code") $ counter infoC
    }
  where
    tags = maybe [] identity maybeTags
    infoL =
      Info
        (toSL (component <> "_" <> "http_request_duration_milliseconds"))
        "The HTTP request latencies in milliseconds."
        tags
    infoC =
      Info
        (toSL (component <> "_" <> "http_status"))
        "The HTTP respond status code count for each request."
        tags

-- | This function is used to populate the @handler@ label of all Prometheus metrics recorded by this library.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentHandlerValue ::
     (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> HttpMetrics Identity -- ^Metric
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValue f metric app req respond = do
  start <- currentTimeInMilliseconds
  app req $ \res -> do
    end <- currentTimeInMilliseconds
    let method = toSL $ decodeUtf8 (Wai.requestMethod req)
    let status = show (HTTP.statusCode (Wai.responseStatus res))
    let latency = end - start
    observeLatency (toSL $ f req, method, status) (_hmLatency metric) latency
    incrementCounter (toSL $ f req, method, status) (_hmStatus metric)
    respond res

currentTimeInMilliseconds :: IO Double
currentTimeInMilliseconds = (fromInteger . round . (* 1000)) <$> getPOSIXTime

observeLatency ::
     (LByteString, LByteString, LByteString)
  -> Vector3 Histogram
  -> Double
  -> IO ()
observeLatency tags vc latency = withLabel tags vc (flip observe latency)

incrementCounter ::
     (LByteString, LByteString, LByteString) -> Vector3 Counter -> IO ()
incrementCounter tags vc = withLabel tags vc increment

prometheus :: HttpMetrics Identity -> Wai.Middleware
prometheus = prometheusHandlerValue (toSL . Wai.rawPathInfo)

prometheusHandlerValue ::
     (Wai.Request -> Text) -> HttpMetrics Identity -> Wai.Middleware
prometheusHandlerValue f metrics app = instrumentHandlerValue f metrics app
