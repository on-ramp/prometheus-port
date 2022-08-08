{-# LANGUAGE FlexibleContexts
           , NumericUnderscores #-}

module Prometheus
  ( -- * Info
    Info (Info)
  , Tags
  , tag
  , GTag
  , genericTag
    -- * Counter
  , Counter
  , counter
    -- * Gauge
  , Gauge
  , gauge
    -- * Histogram
  , Histogram
  , histogram
  , Bucket
  , defBuckets
    -- * Summary
  , Summary
  , summary
  , Quantile
  , defQuantiles
    -- * Vector
  , Vector
  , vector
  , withLabel
    -- ** Shorthands
  , Vector1
  , Vector2
  , Vector3
  , Vector4
  , Vector5
  , Vector6
  , Vector7
    -- * Classes
    -- ** Register
  , Register (..)
  , GRegister
  , genericRegister
  , Metric
    -- ** Extract
  , Extract (..)
    -- ** Export
  , Export
  , export
  , GExport
  , genericExport
    -- ** Increment
  , Increment (..)
    -- ** Decrement
  , Decrement (..)
    -- ** Set
  , Set (..)
    -- ** Observe
  , Observe (..)

    -- * Helper functions
  , measure
  , measureCont
  )
where

import           Prometheus.Internal.Base hiding (export, genericExport)
import qualified Prometheus.Internal.Base as Base
import           Prometheus.Internal.Primitive
import           Prometheus.Internal.Pure.Histogram (Bucket, defBuckets)
import           Prometheus.Internal.Pure.Summary (Quantile, defQuantiles)
import           Prometheus.Internal.Vector

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           GHC.Clock
import           GHC.Generics



export :: Export a => a -> IO BSLC.ByteString
export = fmap template . Base.export



genericExport :: (Generic (f Identity), GExport (Rep (f Identity))) => f Identity -> IO BSLC.ByteString
genericExport = fmap (foldMap template) . Base.genericExport



-- | Calculates how much time an operation takes (in seconds).
measure :: IO a -> IO (a, Double)
measure io = do
  start <- getMonotonicTimeNSec
  a <- io
  end <- getMonotonicTimeNSec
  return (a, fromIntegral (end - start) / 1_000_000_000)

-- | Same as 'measure' in continuation-passing style. Time is measured between the
--   invocation of 'measureCont' and the evaluation of @'IO' 'Double'@.
measureCont :: (IO Double -> IO a) -> IO a
measureCont f = do
  start <- getMonotonicTimeNSec
  f $ do
    end <- getMonotonicTimeNSec
    return $ fromIntegral (end - start) / 1_000_000_000
