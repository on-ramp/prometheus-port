module Main where

import Gauge.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bench "10" $ whnf fib 10,
          bench "11" $ whnf fib 11
        ]
    ]
