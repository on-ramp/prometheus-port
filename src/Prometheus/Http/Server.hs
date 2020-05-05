{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Prometheus.Http.Server
  ( serveMetrics
  , serveApp
  , serveAppWithMetrics
  ) where

import           Control.Concurrent.Async
import           Network.HTTP.Types                (hContentType, status404)
import           Network.Wai                       (Application, responseLBS)
import           Network.Wai.Handler.Warp          (Port, run)
import           Network.Wai.Middleware.Prometheus
import           Prometheus.Http.Internal
import           Prometheus.Internal.Base
import           Prometheus.Internal.Pure.Base
import           Prometheus.Primitive
import           Protolude

data NoMetric f = NoMetric (NoIdentity f Counter)
  deriving Generic

serveMetrics'' :: GenericExportable f => Port -> WrappedMetrics f -> IO ()
serveMetrics'' port =
  run port . flip prometheusWrap response404

serveMetrics' :: GenericExportable f => Port -> Maybe (f Identity) -> Maybe (HttpMetrics Identity) -> IO ()
serveMetrics' port appM httpM = serveMetrics'' port (WrappedMetrics appM httpM)

serveMetrics :: GenericExportable f => Port -> f Identity -> IO ()
serveMetrics port appM =
  serveMetrics' port (Just appM) Nothing

serveApp ::
     Text -- ^ Component name
  -> Port -- ^ Metric Serve port
  -> Maybe Tags
  -> Port -- ^ Http App Port
  -> Application
  -> IO ()
serveApp component port = serveAppWithMetrics' @NoMetric component port Nothing

serveAppWithMetrics ::
     GenericExportable f
  => Text
  -> Port
  -> f Identity
  -> Maybe Tags
  -> Port
  -> Application
  -> IO ()
serveAppWithMetrics component port metrics =
  serveAppWithMetrics' component port (Just metrics)

serveAppWithMetrics' ::
     GenericExportable f
  => Text
  -> Port
  -> Maybe (f Identity)
  -> Maybe Tags
  -> Port
  -> Application
  -> IO ()
serveAppWithMetrics' component port metrics maybeTags portApp app = do
  httpM <- genericRegister (httpMetrics component maybeTags)
  concurrently_
    (serveMetrics' port metrics (Just httpM))
    (run portApp (prometheus httpM app))

response404 :: Application
response404 = \_ resp -> resp $ responseLBS status404 header404 body404
  where
    header404 = [(hContentType, "text/plain")]
    body404 = "404"
