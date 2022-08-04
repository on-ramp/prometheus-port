{-# LANGUAGE DeriveGeneric
           , NumericUnderscores
           , OverloadedStrings #-}

module Network.Wai.Middleware.Prometheus where

import           Prometheus
import           Type.No

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           Data.Text.Encoding
import           GHC.Clock
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai



data HttpMetrics f =
       HttpMetrics
         { hmLatency :: No Identity f (Vector3 Histogram)
         , hmStatus  :: No Identity f (Vector3 Counter)
         }
       deriving Generic

{-# NOINLINE httpMetrics #-}
httpMetrics :: BSLC.ByteString -> HttpMetrics Metric
httpMetrics component =
  HttpMetrics
    { hmLatency = vector ("handler", "method", "status_code") $ histogram infoL buckets
    , hmStatus  = vector ("handler", "method", "status_code") $ counter infoC
    }
  where
    infoL = mkInfo
              (component <> "_http_request_duration_milliseconds")
              "The HTTP request latencies in milliseconds."

    infoC = mkInfo
              (component <> "_http_status")
              "The HTTP respond status code count for each request."

    buckets = [ 0, 0.2, 0.4, 0.5, 0.7, 1, 10, 50, 100, 200, 500, 1_000, 2_000, 5_000, 10_000 ]



prometheus'
  :: (Request -> BSLC.ByteString) -- ^ Endpoint path ("handler" label)
  -> HttpMetrics Identity
  -> Middleware
prometheus' f metric app req respond = do
  start <- getMonotonicTimeNSec
  app req $ \res -> do
    end <- getMonotonicTimeNSec
    let method  = BSLC.fromStrict $ requestMethod req
        status  = BSLC.pack . show . statusCode $ responseStatus res
        latency = fromIntegral (end - start) / 1_000_000
    withLabel (f req, method, status) (hmLatency metric) $ observe latency
    withLabel (f req, method, status) (hmStatus metric) increment
    respond res



prometheus :: HttpMetrics Identity -> Middleware
prometheus = prometheus' $ BSLC.fromStrict . rawPathInfo
