{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes#-}

module Prometheus.Http.Internal
  ( prometheusWrap
  ) where

import           Data.List.HList
import           Network.HTTP.Types                (hContentType, methodGet, status200)
import           Network.Wai                       (Application, Request, pathInfo, requestMethod, responseLBS)
import           Prometheus.Internal.Base
import           Protolude

data Export = Export
instance GenericExportable f => Apply Export (f Identity) LByteString IO where
  apply _ x y = flip mappend y <$> genericExport x

instance AllExportable xs =>
         FoldrH Export LByteString xs IO where
  foldrH _ acc HNil = return acc
  foldrH f acc (x :# xs) = apply f x =<< foldrH f acc xs

exportMetrics :: forall f. AllExportable f => HList f -> IO LByteString
exportMetrics = foldrH Export ""

prometheusPath :: [Text]
prometheusPath = ["metrics"]

prometheusWrap ::
     AllExportable f => Maybe (HList f) -> Application -> Application
prometheusWrap wrapped app request respond
  | isPrometheusRequest request = respond =<< prometheusResponse
  | otherwise = app request respond
  where
    prometheusResponse =
      fmap (responseLBS status200 headers) (maybe (return "") exportMetrics wrapped)
    headers = [(hContentType, "text/plain; version=0.0.4")]

isPrometheusRequest :: Request -> Bool
isPrometheusRequest request = isGet && matchesPath
  where
    matchesPath = pathInfo request == prometheusPath
    isGet = requestMethod request == methodGet
