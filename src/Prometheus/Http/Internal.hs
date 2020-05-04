module Prometheus.Http.Internal
  ( prometheusWrap
  , WrappedMetrics(..)
  ) where

import           Network.HTTP.Types                (hContentType, methodGet, status200)
import           Network.Wai                       (Application, Request, pathInfo, requestMethod, responseLBS)
import           Network.Wai.Middleware.Prometheus
import           Prometheus.Internal.Base
import           Protolude

data WrappedMetrics f =
  WrappedMetrics
    { _wmAppM  :: Maybe (f Identity)
    , _wmHttpM :: Maybe (HttpMetrics Identity)
    }


exportMetrics :: GenericExportable f => WrappedMetrics f -> IO LByteString
exportMetrics WrappedMetrics{..} = let exportAppM = maybe (return "") genericExport _wmAppM
                                       exportHttpM = maybe (return "") genericExport _wmHttpM
                                    in foldMap identity [exportAppM, exportHttpM]

prometheusPath :: [Text]
prometheusPath = ["metrics"]

prometheusWrap ::
     GenericExportable f
  => WrappedMetrics f
  -> Application
  -> Application
prometheusWrap wrapped app request respond
  | isPrometheusRequest request = respond =<< prometheusResponse
  | otherwise = app request respond
  where
    prometheusResponse =
      fmap (responseLBS status200 headers) (exportMetrics wrapped)
    headers = [(hContentType, "text/plain; version=0.0.4")]

isPrometheusRequest :: Request -> Bool
isPrometheusRequest request = isGet && matchesPath
  where
    matchesPath = pathInfo request == prometheusPath
    isGet = requestMethod request == methodGet
