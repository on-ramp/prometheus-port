{-# LANGUAGE FlexibleContexts #-}

module Prometheus
  ( -- * Info
    Info (..)
  , mkInfo
  , Tags
  , extraTags
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
  , genericRegister
  , Metric
    -- ** Extract
  , Extract (..)
    -- ** Export
  , Export
  , export
  , genericExport
    -- ** Increment
  , Increment (..)
    -- ** Decrement
  , Decrement (..)
    -- ** Set
  , Set (..)
    -- ** Observe
  , Observe (..)
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
import           GHC.Generics



export :: Export a => a -> IO BSLC.ByteString
export = fmap template . Base.export



genericExport :: (Generic (f Identity), GExport (Rep (f Identity))) => f Identity -> IO BSLC.ByteString
genericExport = fmap (foldMap template) . Base.genericExport
