{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Prometheus.Http.Server
  ( serveMetrics
  , serveMetricsM
  , serveApp
  , serveAppWithMetrics
  , module Data.List.HList
  , AllExportable
  ) where

import           Control.Concurrent.Async
import           Data.List.HList
import           Network.HTTP.Types                (hContentType, status404)
import           Network.Wai                       (Application, responseLBS)
import           Network.Wai.Handler.Warp          (Port, run)
import           Network.Wai.Middleware.Prometheus
import           Prometheus.Http.Internal
import           Prometheus.Internal.Base
import           Prometheus.Internal.Pure.Base
import           Prometheus.Primitive
import           Protolude

data NoMetric f = NoMetric (NoIdentity f None)
  deriving Generic

serveMetrics' :: AllExportable f => Port -> Maybe (HList f) -> IO ()
serveMetrics' port =
  run port . flip prometheusWrap response404

serveMetrics :: GenericExportable f => Port -> f Identity -> IO ()
serveMetrics port appM =
  serveMetrics' port (Just (appM :# HNil))

serveMetricsM :: AllExportable f => Port -> HList f -> IO ()
serveMetricsM port appM =
  serveMetrics' port (Just appM)

serveApp ::
     Text -- ^ Component name
  -> Port -- ^ Metric Serve port
  -> Maybe Tags
  -> Port -- ^ Http App Port
  -> Application
  -> IO ()
serveApp component port maybeTags portApp app = do
  noAppMetric <- genericRegister (NoMetric none)
  serveAppWithMetrics component port noAppMetric maybeTags portApp app

serveAppWithMetrics ::
     GenericExportable f
  => Text
  -> Port
  -> f Identity
  -> Maybe Tags
  -> Port
  -> Application
  -> IO ()
serveAppWithMetrics component port metrics maybeTags portApp app = do
  httpM <- genericRegister (httpMetrics component maybeTags)
  concurrently_
    (serveMetrics' port (Just $ (metrics :# httpM :# HNil)))
    (run portApp (prometheus httpM app))

response404 :: Application
response404 = \_ resp -> resp $ responseLBS status404 header404 body404
  where
    header404 = [(hContentType, "text/plain")]
    body404 = "404"
