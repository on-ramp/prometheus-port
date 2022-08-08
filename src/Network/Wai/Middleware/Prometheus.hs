{-# LANGUAGE DeriveGeneric
           , NumericUnderscores
           , OverloadedStrings #-}

module Network.Wai.Middleware.Prometheus where

import           Prometheus
import           Type.No

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           Data.Text.Encoding
import           GHC.Clock
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai



newtype HttpMetrics f =
          HttpMetrics
            { hmDuration :: No Identity f (Vector3 Histogram)
            }
          deriving Generic

{-# NOINLINE httpMetrics #-}
httpMetrics :: HttpMetrics Metric
httpMetrics =
  let buckets = take 14 . iterate (*2) $ 1 / 1024
  in HttpMetrics
       { hmDuration = vector ("path", "method", "status") $
                        histogram (Info "http_req_duration" "HTTP request duration (in seconds)") buckets
       }



prometheus
  :: HttpMetrics Identity
  -> Middleware
prometheus metric app req respond = do
  start <- getMonotonicTimeNSec
  app req $ \res -> do
    end <- getMonotonicTimeNSec
    let path    = BSLC.fromStrict $ rawPathInfo req
        method  = BSLC.fromStrict $ requestMethod req
        status  = toLazyByteString . intDec . statusCode $ responseStatus res
        latency = fromIntegral (end - start) / 1_000_000_000
    withLabel (path, method, status) (hmDuration metric) $ observe latency
    respond res
