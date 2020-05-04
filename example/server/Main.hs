module Main where

import           Network.HTTP.Types
import           Network.Wai
import           Prometheus
import           Protolude

main :: IO ()
main =
  serveApp "my_app" 9090 3000 $ \req respond -> do
    putText ("Request: " <> show req)
    (respond $ responseLBS status200 [] "Hello World")
