module Prometheus.Http.Server
  ( serveMetrics
  ) where

import           Network.HTTP.Types       (hContentType, status404)
import           Network.Wai              (Application, responseLBS)
import           Network.Wai.Handler.Warp (Port, run)
import           Prometheus.Http.Internal
import           Prometheus.Internal.Base
import           Protolude

serveMetrics :: GenericExportable f => Port -> f Identity -> IO ()
serveMetrics port = liftIO . run port . flip prometheusWrap response404

response404 :: Application
response404 = \_ resp -> resp $ responseLBS status404 header404 body404
  where
    header404 = [(hContentType, "text/plain")]
    body404 = "404"
