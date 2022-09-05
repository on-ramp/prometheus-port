{-# LANGUAGE DeriveGeneric
           , DerivingStrategies
           , NumericUnderscores
           , OverloadedStrings #-}

module Network.Wai.Middleware.Prometheus
  ( HttpMetrics(..)
  , httpMetrics
  , prometheus'
  , prometheus
  ) where

import           Prometheus
import           Type.No

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai


-- | Records the latency of the HTTP calls with 'prometheus' middleware.
newtype HttpMetrics f =
          HttpMetrics
            { hmDuration :: No Identity f (Vector3 Histogram)
            }
          deriving stock Generic

-- | The pure 'HttpMetrics'.
-- In order to use this, you would need to 'register' the metric.
httpMetrics :: HttpMetrics Metric
httpMetrics =
  let buckets = take 14 . iterate (*2) $ 1 / 1024
  in HttpMetrics
       { hmDuration = vector ("path", "method", "status") $
                        histogram (Info "http_req_duration" "HTTP request duration (in seconds)") buckets
       }
{-# NOINLINE httpMetrics #-}

-- | Same as 'prometheus', but allows to modify a custom @path@ getter.
prometheus'
  :: (Request -> BSLC.ByteString) -- ^ @path@
  -> HttpMetrics Identity
  -> Middleware
prometheus' modifier metric app req respond =
  measureCont $ \stamp ->
    app req $ \res -> do
      let path    = modifier req
          method  = BSLC.fromStrict $ requestMethod req
          status  = toLazyByteString . intDec . statusCode $ responseStatus res
      time <- stamp
      withLabel (path, method, status) (hmDuration metric) $ observe time
      respond res

-- | 'Middleware' to collect 'HttpMetrics'.
--
-- @
-- main = do
--   metrics <- genericRegister httpMetrics
--   ...
--   Warp.run port
--     $ prometheus metrics
--     $ app
-- @
prometheus
  :: HttpMetrics Identity
  -> Middleware
prometheus = prometheus' $ BSLC.fromStrict . rawPathInfo
