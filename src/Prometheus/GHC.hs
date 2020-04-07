{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Prometheus.GHC where

import           Prometheus

import           Control.Monad (when)
import           Data.Functor.Identity
import           GHC.Generics
import           GHC.Stats
import           Prelude



data GHCMetrics f = GHCMetrics
                      { _ghcmGcsTotal                         :: NoIdentity f Counter
                      , _ghcmMajorGcsTotal                    :: NoIdentity f Counter
                      , _ghcmAllocatedBytesTotal              :: NoIdentity f Counter
                      , _ghcmMaxLiveBytes                     :: NoIdentity f Gauge
                      , _ghcmMaxLargeObjectsBytes             :: NoIdentity f Gauge
                      , _ghcmMaxCompactBytes                  :: NoIdentity f Gauge
                      , _ghcmMaxSlopBytes                     :: NoIdentity f Gauge
                      , _ghcmMaxMemInUseBytes                 :: NoIdentity f Gauge
                      , _ghcmCumulativeLiveBytesTotal         :: NoIdentity f Counter
                      , _ghcmCopiedBytesTotal                 :: NoIdentity f Counter
                      , _ghcmParCopiedBytesTotal              :: NoIdentity f Counter
                      , _ghcmCumulativeParMaxCopiedBytesTotal :: NoIdentity f Counter
                      , _ghcmMutatorCpuSecondsTotal           :: NoIdentity f Counter
                      , _ghcmMutatorElapsedSecondsTotal       :: NoIdentity f Counter
                      , _ghcmGcCpuSecondsTotal                :: NoIdentity f Counter
                      , _ghcmGcElapsedSecondsTotal            :: NoIdentity f Counter
                      , _ghcmCpuSecondsTotal                  :: NoIdentity f Counter
                      , _ghcmElapsedSecondsTotal              :: NoIdentity f Counter
                      , _ghcmGcdetailsGen                     :: NoIdentity f Gauge
                      , _ghcmGcdetailsThreads                 :: NoIdentity f Gauge
                      , _ghcmGcdetailsAllocatedBytes          :: NoIdentity f Gauge
                      , _ghcmGcdetailsLiveBytes               :: NoIdentity f Gauge
                      , _ghcmGcdetailsLargeObjectsBytes       :: NoIdentity f Gauge
                      , _ghcmGcdetailsCompactBytes            :: NoIdentity f Gauge
                      , _ghcmGcdetailsSlopBytes               :: NoIdentity f Gauge
                      , _ghcmGcdetailsMemInUseBytes           :: NoIdentity f Counter
                      , _ghcmGcdetailsCopiedBytes             :: NoIdentity f Gauge
                      , _ghcmGcdetailsParMaxCopiedBytes       :: NoIdentity f Gauge
                      , _ghcmGcdetailsSyncElapsedSeconds      :: NoIdentity f Gauge
                      , _ghcmGcdetailsCpuSeconds              :: NoIdentity f Gauge
                      , _ghcmGcdetailsElapsedSeconds          :: NoIdentity f Gauge
                      }
                    deriving Generic



ghcMetrics :: GHCMetrics Metric
ghcMetrics =
  GHCMetrics
    (counter $ infoM "ghc_gcs_total" "Total number of GCs")
    (counter $ infoM "ghc_major_gcs_total" "Total number of major (oldest generation) GCs")
    (counter $ infoM "ghc_allocated_bytes_total" "Total bytes allocated")
    (gauge   $ infoM "ghc_max_live_bytes" "Maximum live data (including large objects + compact regions)")
    (gauge   $ infoM "ghc_max_large_objects_bytes" "Maximum live data in large objects")
    (gauge   $ infoM "ghc_max_compact_bytes" "Maximum live data in compact regions")
    (gauge   $ infoM "ghc_max_slop_bytes" "Maximum slop")
    (gauge   $ infoM "ghc_max_mem_in_use_bytes" "Maximum memory in use by the RTS")
    (counter $ infoM "ghc_cumulative_live_bytes_total" "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program.")
    (counter $ infoM "ghc_copied_bytes_total" "Sum of copied_bytes across all GCs")
    (counter $ infoM "ghc_par_copied_bytes_total" "Sum of copied_bytes across all parallel GCs")
    (counter $ infoM "ghc_cumulative_par_max_copied_bytes_total" "Sum of par_max_copied_bytes across all parallel GCs")
    (counter $ infoM "ghc_mutator_cpu_seconds_total" "Total CPU time used by the mutator")
    (counter $ infoM "ghc_mutator_elapsed_seconds_total" "Total elapsed time used by the mutator")
    (counter $ infoM "ghc_gc_cpu_seconds_total" "Total CPU time used by the GC")
    (counter $ infoM "ghc_gc_elapsed_seconds_total" "Total elapsed time used by the GC")
    (counter $ infoM "ghc_cpu_seconds_total" "Total CPU time (at the previous GC)")
    (counter $ infoM "ghc_elapsed_seconds_total" "Total elapsed time (at the previous GC)")
    (gauge   $ infoM "ghc_gcdetails_gen" "The generation number of this GC")
    (gauge   $ infoM "ghc_gcdetails_threads" "Number of threads used in this GC")
    (gauge   $ infoM "ghc_gcdetails_allocated_bytes" "Number of bytes allocated since the previous GC")
    (gauge   $ infoM "ghc_gcdetails_live_bytes" "Total amount of live data in the heap (including large + compact data)")
    (gauge   $ infoM "ghc_gcdetails_large_objects_bytes" "Total amount of live data in large objects")
    (gauge   $ infoM "ghc_gcdetails_compact_bytes" "Total amount of live data in compact regions")
    (gauge   $ infoM "ghc_gcdetails_slop_bytes" "Total amount of slop (wasted memory)")
    (counter $ infoM "ghc_gcdetails_mem_in_use_bytes" "Total amount of memory in use by the RTS")
    (gauge   $ infoM "ghc_gcdetails_copied_bytes" "Total amount of data copied during this GC")
    (gauge   $ infoM "ghc_gcdetails_par_max_copied_bytes" "In parallel GC, the max amount of data copied by any one thread")
    (gauge   $ infoM "ghc_gcdetails_sync_elapsed_seconds" "The time elapsed during synchronisation before GC")
    (gauge   $ infoM "ghc_gcdetails_cpu_seconds" "The CPU time used during GC itself")
    (gauge   $ infoM "ghc_gcdetails_elapsed_seconds" "The time elapsed during GC itself")



updateGHCMetrics :: GHCMetrics Identity -> IO ()
updateGHCMetrics metrics = do
  isEnabled <- getRTSStatsEnabled
  when isEnabled $ do
    rtsStats <- getRTSStats
    let updateCounter what from = (.=.) (what metrics) $ from rtsStats
        updateGauge   what from = (.=.) (what metrics) $ from rtsStats
    updateCounter _ghcmGcsTotal                         $ fromIntegral . gcs
    updateCounter _ghcmMajorGcsTotal                    $ fromIntegral . major_gcs
    updateCounter _ghcmAllocatedBytesTotal              $ fromIntegral . allocated_bytes
    updateGauge   _ghcmMaxLiveBytes                     $ fromIntegral . max_live_bytes
    updateGauge   _ghcmMaxLargeObjectsBytes             $ fromIntegral . max_large_objects_bytes
    updateGauge   _ghcmMaxCompactBytes                  $ fromIntegral . max_compact_bytes
    updateGauge   _ghcmMaxSlopBytes                     $ fromIntegral . max_slop_bytes
    updateGauge   _ghcmMaxMemInUseBytes                 $ fromIntegral . max_mem_in_use_bytes
    updateCounter _ghcmCumulativeLiveBytesTotal         $ fromIntegral . cumulative_live_bytes
    updateCounter _ghcmCopiedBytesTotal                 $ fromIntegral . copied_bytes
    updateCounter _ghcmParCopiedBytesTotal              $ fromIntegral . par_copied_bytes
    updateCounter _ghcmCumulativeParMaxCopiedBytesTotal $ fromIntegral . cumulative_par_max_copied_bytes
    updateCounter _ghcmMutatorCpuSecondsTotal           $ rtsTimeToSeconds . mutator_cpu_ns
    updateCounter _ghcmMutatorElapsedSecondsTotal       $ rtsTimeToSeconds . mutator_elapsed_ns
    updateCounter _ghcmGcCpuSecondsTotal                $ rtsTimeToSeconds . gc_cpu_ns
    updateCounter _ghcmGcElapsedSecondsTotal            $ rtsTimeToSeconds . gc_elapsed_ns
    updateCounter _ghcmCpuSecondsTotal                  $ rtsTimeToSeconds . cpu_ns
    updateCounter _ghcmElapsedSecondsTotal              $ rtsTimeToSeconds . elapsed_ns
    updateGauge   _ghcmGcdetailsGen                     $ fromIntegral . gcdetails_gen . gc
    updateGauge   _ghcmGcdetailsThreads                 $ fromIntegral . gcdetails_threads . gc
    updateGauge   _ghcmGcdetailsAllocatedBytes          $ fromIntegral . gcdetails_allocated_bytes . gc
    updateGauge   _ghcmGcdetailsLiveBytes               $ fromIntegral . gcdetails_live_bytes . gc
    updateGauge   _ghcmGcdetailsLargeObjectsBytes       $ fromIntegral . gcdetails_large_objects_bytes . gc
    updateGauge   _ghcmGcdetailsCompactBytes            $ fromIntegral . gcdetails_compact_bytes . gc
    updateGauge   _ghcmGcdetailsSlopBytes               $ fromIntegral . gcdetails_slop_bytes . gc
    updateCounter _ghcmGcdetailsMemInUseBytes           $ fromIntegral . gcdetails_mem_in_use_bytes . gc
    updateGauge   _ghcmGcdetailsCopiedBytes             $ fromIntegral . gcdetails_copied_bytes . gc
    updateGauge   _ghcmGcdetailsParMaxCopiedBytes       $ fromIntegral . gcdetails_par_max_copied_bytes . gc
    updateGauge   _ghcmGcdetailsSyncElapsedSeconds      $ rtsTimeToSeconds . gcdetails_sync_elapsed_ns . gc
    updateGauge   _ghcmGcdetailsCpuSeconds              $ rtsTimeToSeconds . gcdetails_cpu_ns . gc
    updateGauge   _ghcmGcdetailsElapsedSeconds          $ rtsTimeToSeconds . gcdetails_elapsed_ns . gc



-- | Convert from 'RtsTime' (nanoseconds) to seconds with nanosecond precision.
rtsTimeToSeconds :: RtsTime -> Double
rtsTimeToSeconds = (/ 1e9) . fromIntegral
