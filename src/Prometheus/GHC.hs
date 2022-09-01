{-# LANGUAGE DeriveGeneric
           , DerivingStrategies
           , OverloadedStrings #-}

module Prometheus.GHC
  ( RtsMetrics (..)
  , rtsMetrics
  , rtsRefresh
  ) where

import           Prometheus

import           Control.Monad
import           Data.Functor.Identity
import           GHC.Generics
import           GHC.Stats

import           Type.No


data RtsMetrics f =
       RtsMetrics
         { rmGcsTotal                         :: No Identity f Counter
         , rmMajorGcsTotal                    :: No Identity f Counter
         , rmAllocatedBytesTotal              :: No Identity f Counter
         , rmMaxLiveBytes                     :: No Identity f Gauge
         , rmMaxLargeObjectsBytes             :: No Identity f Gauge
         , rmMaxCompactBytes                  :: No Identity f Gauge
         , rmMaxSlopBytes                     :: No Identity f Gauge
         , rmMaxMemInUseBytes                 :: No Identity f Gauge
         , rmCumulativeLiveBytesTotal         :: No Identity f Counter
         , rmCopiedBytesTotal                 :: No Identity f Counter
         , rmParCopiedBytesTotal              :: No Identity f Counter
         , rmCumulativeParMaxCopiedBytesTotal :: No Identity f Counter
         , rmMutatorCpuSecondsTotal           :: No Identity f Counter
         , rmMutatorElapsedSecondsTotal       :: No Identity f Counter
         , rmGcCpuSecondsTotal                :: No Identity f Counter
         , rmGcElapsedSecondsTotal            :: No Identity f Counter
         , rmCpuSecondsTotal                  :: No Identity f Counter
         , rmElapsedSecondsTotal              :: No Identity f Counter
         , rmGcdetailsGen                     :: No Identity f Gauge
         , rmGcdetailsThreads                 :: No Identity f Gauge
         , rmGcdetailsAllocatedBytes          :: No Identity f Gauge
         , rmGcdetailsLiveBytes               :: No Identity f Gauge
         , rmGcdetailsLargeObjectsBytes       :: No Identity f Gauge
         , rmGcdetailsCompactBytes            :: No Identity f Gauge
         , rmGcdetailsSlopBytes               :: No Identity f Gauge
         , rmGcdetailsMemInUseBytes           :: No Identity f Counter
         , rmGcdetailsCopiedBytes             :: No Identity f Gauge
         , rmGcdetailsParMaxCopiedBytes       :: No Identity f Gauge
         , rmGcdetailsSyncElapsedSeconds      :: No Identity f Gauge
         , rmGcdetailsCpuSeconds              :: No Identity f Gauge
         , rmGcdetailsElapsedSeconds          :: No Identity f Gauge
         }
       deriving stock Generic

rtsMetrics :: RtsMetrics Metric
rtsMetrics =
  RtsMetrics
    { rmGcsTotal                         = counter $ Info "ghc_gcs_total" "Total number of GCs"
    , rmMajorGcsTotal                    = counter $ Info "ghc_major_gcs_total" "Total number of major (oldest generation) GCs"
    , rmAllocatedBytesTotal              = counter $ Info "ghc_allocated_bytes_total" "Total bytes allocated"
    , rmMaxLiveBytes                     = gauge $ Info "ghc_max_live_bytes" "Maximum live data (including large objects + compact regions)"
    , rmMaxLargeObjectsBytes             = gauge $ Info "ghc_max_large_objects_bytes" "Maximum live data in large objects"
    , rmMaxCompactBytes                  = gauge $ Info "ghc_max_compact_bytes" "Maximum live data in compact regions"
    , rmMaxSlopBytes                     = gauge $ Info "ghc_max_slop_bytes" "Maximum slop"
    , rmMaxMemInUseBytes                 = gauge $ Info "ghc_max_mem_in_use_bytes" "Maximum memory in use by the RTS"
    , rmCumulativeLiveBytesTotal         = counter $ Info "ghc_cumulative_live_bytes_total" "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program."
    , rmCopiedBytesTotal                 = counter $ Info "ghc_copied_bytes_total" "Sum of copied_bytes across all GCs"
    , rmParCopiedBytesTotal              = counter $ Info "ghc_par_copied_bytes_total" "Sum of copied_bytes across all parallel GCs"
    , rmCumulativeParMaxCopiedBytesTotal = counter $ Info "ghc_cumulative_par_max_copied_bytes_total" "Sum of par_max_copied_bytes across all parallel GCs"
    , rmMutatorCpuSecondsTotal           = counter $ Info "ghc_mutator_cpu_seconds_total" "Total CPU time used by the mutator"
    , rmMutatorElapsedSecondsTotal       = counter $ Info "ghc_mutator_elapsed_seconds_total" "Total elapsed time used by the mutator"
    , rmGcCpuSecondsTotal                = counter $ Info "ghc_gc_cpu_seconds_total" "Total CPU time used by the GC"
    , rmGcElapsedSecondsTotal            = counter $ Info "ghc_gc_elapsed_seconds_total" "Total elapsed time used by the GC"
    , rmCpuSecondsTotal                  = counter $ Info "ghc_cpu_seconds_total" "Total CPU time (at the previous GC)"
    , rmElapsedSecondsTotal              = counter $ Info "ghc_elapsed_seconds_total" "Total elapsed time (at the previous GC)"
    , rmGcdetailsGen                     = gauge $ Info "ghc_gcdetails_gen" "The generation number of this GC"
    , rmGcdetailsThreads                 = gauge $ Info "ghc_gcdetails_threads" "Number of threads used in this GC"
    , rmGcdetailsAllocatedBytes          = gauge $ Info "ghc_gcdetails_allocated_bytes" "Number of bytes allocated since the previous GC"
    , rmGcdetailsLiveBytes               = gauge $ Info "ghc_gcdetails_live_bytes" "Total amount of live data in the heap (including large + compact data)"
    , rmGcdetailsLargeObjectsBytes       = gauge $ Info "ghc_gcdetails_large_objects_bytes" "Total amount of live data in large objects"
    , rmGcdetailsCompactBytes            = gauge $ Info "ghc_gcdetails_compact_bytes" "Total amount of live data in compact regions"
    , rmGcdetailsSlopBytes               = gauge $ Info "ghc_gcdetails_slop_bytes" "Total amount of slop (wasted memory)"
    , rmGcdetailsMemInUseBytes           = counter $ Info "ghc_gcdetails_mem_in_use_bytes" "Total amount of memory in use by the RTS"
    , rmGcdetailsCopiedBytes             = gauge $ Info "ghc_gcdetails_copied_bytes" "Total amount of data copied during this GC"
    , rmGcdetailsParMaxCopiedBytes       = gauge $ Info "ghc_gcdetails_par_max_copied_bytes" "In parallel GC, the max amount of data copied by any one thread"
    , rmGcdetailsSyncElapsedSeconds      = gauge $ Info "ghc_gcdetails_sync_elapsed_seconds" "The time elapsed during synchronisation before GC"
    , rmGcdetailsCpuSeconds              = gauge $ Info "ghc_gcdetails_cpu_seconds" "The CPU time used during GC itself"
    , rmGcdetailsElapsedSeconds          = gauge $ Info "ghc_gcdetails_elapsed_seconds" "The time elapsed during GC itself"
    }

rtsRefresh :: RtsMetrics Identity -> IO ()
rtsRefresh metrics = do
  isEnabled <- getRTSStatsEnabled
  when isEnabled $ do
    rtsStats <- getRTSStats
    let update this from = from rtsStats `set` this metrics
    update rmGcsTotal                         $ fromIntegral     . gcs
    update rmMajorGcsTotal                    $ fromIntegral     . major_gcs
    update rmAllocatedBytesTotal              $ fromIntegral     . allocated_bytes
    update rmMaxLiveBytes                     $ fromIntegral     . max_live_bytes
    update rmMaxLargeObjectsBytes             $ fromIntegral     . max_large_objects_bytes
    update rmMaxCompactBytes                  $ fromIntegral     . max_compact_bytes
    update rmMaxSlopBytes                     $ fromIntegral     . max_slop_bytes
    update rmMaxMemInUseBytes                 $ fromIntegral     . max_mem_in_use_bytes
    update rmCumulativeLiveBytesTotal         $ fromIntegral     . cumulative_live_bytes
    update rmCopiedBytesTotal                 $ fromIntegral     . copied_bytes
    update rmParCopiedBytesTotal              $ fromIntegral     . par_copied_bytes
    update rmCumulativeParMaxCopiedBytesTotal $ fromIntegral     . cumulative_par_max_copied_bytes
    update rmMutatorCpuSecondsTotal           $ rtsTimeToSeconds . mutator_cpu_ns
    update rmMutatorElapsedSecondsTotal       $ rtsTimeToSeconds . mutator_elapsed_ns
    update rmGcCpuSecondsTotal                $ rtsTimeToSeconds . gc_cpu_ns
    update rmGcElapsedSecondsTotal            $ rtsTimeToSeconds . gc_elapsed_ns
    update rmCpuSecondsTotal                  $ rtsTimeToSeconds . cpu_ns
    update rmElapsedSecondsTotal              $ rtsTimeToSeconds . elapsed_ns
    update rmGcdetailsGen                     $ fromIntegral     . gcdetails_gen                  . gc
    update rmGcdetailsThreads                 $ fromIntegral     . gcdetails_threads              . gc
    update rmGcdetailsAllocatedBytes          $ fromIntegral     . gcdetails_allocated_bytes      . gc
    update rmGcdetailsLiveBytes               $ fromIntegral     . gcdetails_live_bytes           . gc
    update rmGcdetailsLargeObjectsBytes       $ fromIntegral     . gcdetails_large_objects_bytes  . gc
    update rmGcdetailsCompactBytes            $ fromIntegral     . gcdetails_compact_bytes        . gc
    update rmGcdetailsSlopBytes               $ fromIntegral     . gcdetails_slop_bytes           . gc
    update rmGcdetailsMemInUseBytes           $ fromIntegral     . gcdetails_mem_in_use_bytes     . gc
    update rmGcdetailsCopiedBytes             $ fromIntegral     . gcdetails_copied_bytes         . gc
    update rmGcdetailsParMaxCopiedBytes       $ fromIntegral     . gcdetails_par_max_copied_bytes . gc
    update rmGcdetailsSyncElapsedSeconds      $ rtsTimeToSeconds . gcdetails_sync_elapsed_ns      . gc
    update rmGcdetailsCpuSeconds              $ rtsTimeToSeconds . gcdetails_cpu_ns               . gc
    update rmGcdetailsElapsedSeconds          $ rtsTimeToSeconds . gcdetails_elapsed_ns           . gc

rtsTimeToSeconds :: RtsTime -> Double
rtsTimeToSeconds = (/ 1e9) . fromIntegral
