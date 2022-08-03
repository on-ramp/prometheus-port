{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Prometheus.GHC
  ( Metrics (..)
  , metrics
  , update
  ) where

import           Prometheus

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Identity
import           GHC.Generics
import           GHC.Stats

import           Type.No



data Metrics f =
       Metrics
         { mGcsTotal                         :: No Identity f Counter
         , mMajorGcsTotal                    :: No Identity f Counter
         , mAllocatedBytesTotal              :: No Identity f Counter
         , mMaxLiveBytes                     :: No Identity f Gauge
         , mMaxLargeObjectsBytes             :: No Identity f Gauge
         , mMaxCompactBytes                  :: No Identity f Gauge
         , mMaxSlopBytes                     :: No Identity f Gauge
         , mMaxMemInUseBytes                 :: No Identity f Gauge
         , mCumulativeLiveBytesTotal         :: No Identity f Counter
         , mCopiedBytesTotal                 :: No Identity f Counter
         , mParCopiedBytesTotal              :: No Identity f Counter
         , mCumulativeParMaxCopiedBytesTotal :: No Identity f Counter
         , mMutatorCpuSecondsTotal           :: No Identity f Counter
         , mMutatorElapsedSecondsTotal       :: No Identity f Counter
         , mGcCpuSecondsTotal                :: No Identity f Counter
         , mGcElapsedSecondsTotal            :: No Identity f Counter
         , mCpuSecondsTotal                  :: No Identity f Counter
         , mElapsedSecondsTotal              :: No Identity f Counter
         , mGcdetailsGen                     :: No Identity f Gauge
         , mGcdetailsThreads                 :: No Identity f Gauge
         , mGcdetailsAllocatedBytes          :: No Identity f Gauge
         , mGcdetailsLiveBytes               :: No Identity f Gauge
         , mGcdetailsLargeObjectsBytes       :: No Identity f Gauge
         , mGcdetailsCompactBytes            :: No Identity f Gauge
         , mGcdetailsSlopBytes               :: No Identity f Gauge
         , mGcdetailsMemInUseBytes           :: No Identity f Counter
         , mGcdetailsCopiedBytes             :: No Identity f Gauge
         , mGcdetailsParMaxCopiedBytes       :: No Identity f Gauge
         , mGcdetailsSyncElapsedSeconds      :: No Identity f Gauge
         , mGcdetailsCpuSeconds              :: No Identity f Gauge
         , mGcdetailsElapsedSeconds          :: No Identity f Gauge
         }
       deriving Generic

metrics :: Metrics Metric
metrics =
  Metrics
    { mGcsTotal                         = counter $ Info "ghc_gcs_total" "Total number of GCs" []
    , mMajorGcsTotal                    = counter $ Info "ghc_major_gcs_total" "Total number of major (oldest generation) GCs" []
    , mAllocatedBytesTotal              = counter $ Info "ghc_allocated_bytes_total" "Total bytes allocated" []
    , mMaxLiveBytes                     = gauge $ Info "ghc_max_live_bytes" "Maximum live data (including large objects + compact regions)" []
    , mMaxLargeObjectsBytes             = gauge $ Info "ghc_max_large_objects_bytes" "Maximum live data in large objects" []
    , mMaxCompactBytes                  = gauge $ Info "ghc_max_compact_bytes" "Maximum live data in compact regions" []
    , mMaxSlopBytes                     = gauge $ Info "ghc_max_slop_bytes" "Maximum slop" []
    , mMaxMemInUseBytes                 = gauge $ Info "ghc_max_mem_in_use_bytes" "Maximum memory in use by the RTS" []
    , mCumulativeLiveBytesTotal         = counter $ Info "ghc_cumulative_live_bytes_total" "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program." []
    , mCopiedBytesTotal                 = counter $ Info "ghc_copied_bytes_total" "Sum of copied_bytes across all GCs" []
    , mParCopiedBytesTotal              = counter $ Info "ghc_par_copied_bytes_total" "Sum of copied_bytes across all parallel GCs" []
    , mCumulativeParMaxCopiedBytesTotal = counter $ Info "ghc_cumulative_par_max_copied_bytes_total" "Sum of par_max_copied_bytes across all parallel GCs" []
    , mMutatorCpuSecondsTotal           = counter $ Info "ghc_mutator_cpu_seconds_total" "Total CPU time used by the mutator" []
    , mMutatorElapsedSecondsTotal       = counter $ Info "ghc_mutator_elapsed_seconds_total" "Total elapsed time used by the mutator" []
    , mGcCpuSecondsTotal                = counter $ Info "ghc_gc_cpu_seconds_total" "Total CPU time used by the GC" []
    , mGcElapsedSecondsTotal            = counter $ Info "ghc_gc_elapsed_seconds_total" "Total elapsed time used by the GC" []
    , mCpuSecondsTotal                  = counter $ Info "ghc_cpu_seconds_total" "Total CPU time (at the previous GC)" []
    , mElapsedSecondsTotal              = counter $ Info "ghc_elapsed_seconds_total" "Total elapsed time (at the previous GC)" []
    , mGcdetailsGen                     = gauge $ Info "ghc_gcdetails_gen" "The generation number of this GC" []
    , mGcdetailsThreads                 = gauge $ Info "ghc_gcdetails_threads" "Number of threads used in this GC" []
    , mGcdetailsAllocatedBytes          = gauge $ Info "ghc_gcdetails_allocated_bytes" "Number of bytes allocated since the previous GC" []
    , mGcdetailsLiveBytes               = gauge $ Info "ghc_gcdetails_live_bytes" "Total amount of live data in the heap (including large + compact data)" []
    , mGcdetailsLargeObjectsBytes       = gauge $ Info "ghc_gcdetails_large_objects_bytes" "Total amount of live data in large objects" []
    , mGcdetailsCompactBytes            = gauge $ Info "ghc_gcdetails_compact_bytes" "Total amount of live data in compact regions" []
    , mGcdetailsSlopBytes               = gauge $ Info "ghc_gcdetails_slop_bytes" "Total amount of slop (wasted memory)" []
    , mGcdetailsMemInUseBytes           = counter $ Info "ghc_gcdetails_mem_in_use_bytes" "Total amount of memory in use by the RTS" []
    , mGcdetailsCopiedBytes             = gauge $ Info "ghc_gcdetails_copied_bytes" "Total amount of data copied during this GC" []

    , mGcdetailsParMaxCopiedBytes       = gauge $ Info "ghc_gcdetails_par_max_copied_bytes" "In parallel GC, the max amount of data copied by any one thread" []
    , mGcdetailsSyncElapsedSeconds      = gauge $ Info "ghc_gcdetails_sync_elapsed_seconds" "The time elapsed during synchronisation before GC" []
    , mGcdetailsCpuSeconds              = gauge $ Info "ghc_gcdetails_cpu_seconds" "The CPU time used during GC itself" []
    , mGcdetailsElapsedSeconds          = gauge $ Info "ghc_gcdetails_elapsed_seconds" "The time elapsed during GC itself" []
    }

update :: Metrics Identity -> IO ()
update metrics = do
  isEnabled <- getRTSStatsEnabled
  when isEnabled $ do
    rtsStats <- getRTSStats
    let update this from = from rtsStats `set` this metrics
    update mGcsTotal                         $ fromIntegral     . gcs
    update mMajorGcsTotal                    $ fromIntegral     . major_gcs
    update mAllocatedBytesTotal              $ fromIntegral     . allocated_bytes
    update mMaxLiveBytes                     $ fromIntegral     . max_live_bytes
    update mMaxLargeObjectsBytes             $ fromIntegral     . max_large_objects_bytes
    update mMaxCompactBytes                  $ fromIntegral     . max_compact_bytes
    update mMaxSlopBytes                     $ fromIntegral     . max_slop_bytes
    update mMaxMemInUseBytes                 $ fromIntegral     . max_mem_in_use_bytes
    update mCumulativeLiveBytesTotal         $ fromIntegral     . cumulative_live_bytes
    update mCopiedBytesTotal                 $ fromIntegral     . copied_bytes
    update mParCopiedBytesTotal              $ fromIntegral     . par_copied_bytes
    update mCumulativeParMaxCopiedBytesTotal $ fromIntegral     . cumulative_par_max_copied_bytes
    update mMutatorCpuSecondsTotal           $ rtsTimeToSeconds . mutator_cpu_ns
    update mMutatorElapsedSecondsTotal       $ rtsTimeToSeconds . mutator_elapsed_ns
    update mGcCpuSecondsTotal                $ rtsTimeToSeconds . gc_cpu_ns
    update mGcElapsedSecondsTotal            $ rtsTimeToSeconds . gc_elapsed_ns
    update mCpuSecondsTotal                  $ rtsTimeToSeconds . cpu_ns
    update mElapsedSecondsTotal              $ rtsTimeToSeconds . elapsed_ns
    update mGcdetailsGen                     $ fromIntegral     . gcdetails_gen                  . gc
    update mGcdetailsThreads                 $ fromIntegral     . gcdetails_threads              . gc
    update mGcdetailsAllocatedBytes          $ fromIntegral     . gcdetails_allocated_bytes      . gc
    update mGcdetailsLiveBytes               $ fromIntegral     . gcdetails_live_bytes           . gc
    update mGcdetailsLargeObjectsBytes       $ fromIntegral     . gcdetails_large_objects_bytes  . gc
    update mGcdetailsCompactBytes            $ fromIntegral     . gcdetails_compact_bytes        . gc
    update mGcdetailsSlopBytes               $ fromIntegral     . gcdetails_slop_bytes           . gc
    update mGcdetailsMemInUseBytes           $ fromIntegral     . gcdetails_mem_in_use_bytes     . gc
    update mGcdetailsCopiedBytes             $ fromIntegral     . gcdetails_copied_bytes         . gc
    update mGcdetailsParMaxCopiedBytes       $ fromIntegral     . gcdetails_par_max_copied_bytes . gc
    update mGcdetailsSyncElapsedSeconds      $ rtsTimeToSeconds . gcdetails_sync_elapsed_ns      . gc
    update mGcdetailsCpuSeconds              $ rtsTimeToSeconds . gcdetails_cpu_ns               . gc
    update mGcdetailsElapsedSeconds          $ rtsTimeToSeconds . gcdetails_elapsed_ns           . gc

rtsTimeToSeconds :: RtsTime -> Double
rtsTimeToSeconds = (/ 1e9) . fromIntegral
