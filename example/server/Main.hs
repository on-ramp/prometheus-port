module Main where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Prometheus
import           Protolude

main :: IO ()
main =
  run 3000 . (prometheus "my_app") $ \req respond -> do
    putText ("Request: " <> show req)
    (respond $ responseLBS status200 [] "Hello World")
