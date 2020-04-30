module Prometheus.Http.Internal
  ( prometheusWrap
  ) where

import           Network.HTTP.Types       (hContentType, methodGet, status200)
import           Network.Wai              (Application, Request, pathInfo, requestMethod, responseLBS)
import           Prometheus.Internal.Base
import           Protolude

prometheusPath :: [Text]
prometheusPath = ["metrics"]

prometheusWrap ::
     GenericExportable f => f Identity -> Application -> Application
prometheusWrap runSample app request respond
  | isPrometheusRequest request = respond =<< prometheusResponse runSample
  | otherwise = app request respond
  where
    prometheusResponse = fmap (responseLBS status200 headers) . genericExport
    headers = [(hContentType, "text/plain; version=0.0.4")]

isPrometheusRequest :: Request -> Bool
isPrometheusRequest request = isGet && matchesPath
  where
    matchesPath = pathInfo request == prometheusPath
    isGet = requestMethod request == methodGet
