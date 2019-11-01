# prometheus-typehell

This library is yet another [prometheus client](https://prometheus.io/docs/instrumenting/writing_clientlibs)
implementation, with [prometheus](https://hackage.haskell.org/package/prometheus) being a bulky
mess of imports and [prometheus-client](https://hackage.haskell.org/package/prometheus) breaking
referential transparency (it's still probably a better choice for smaller-sized projects).



## Library usage

The interface defined in this package is an exact copy of that defined in
[prometheus-client](https://hackage.haskell.org/package/prometheus), except:

- Collection operations are all defined in typeclasses
    (e.g. Counters and Gauges use `increment` instead of their respective `incCounter` and `incGauge`);

- Metrics can be registered and exported in bulk if defined inside of a matching `Generic` datatype;

Therefore...

### Using metrics as usual

```haskell
-- Create metrics
cntr   <- register . counter   $ Info "counter"   "counter_help"
gg     <- register . gauge     $ Info "gauge"     "gauge_help"
hstgrm <- register . histogram ( Info "histogram" "histogram_help" ) def
smmr   <- register . summary   ( Info "summary"   "summary_help"   ) def

-- Collect some data with the metrics
increment cntr
cntr .+. 5

increment gg
gg .+. 5
decrement gg
gg .-. 5
gg .=. 5

hstgrm `observe` 2.5

smmr `observe` 2.5

-- Extract and export metrics
cntrCurrentValue <- extract cntr
ggExported <- export gg


-- In case of vectors everything is the same, but with additional functions
vcntr <- register . vector "label" . counter $ Info "counter"   "counter_help"

withLabel "this" vcntr increment
withlabel "this" vcntr (.+. 5)

vcntrExported <- export vcntr
```


### Packing metrics into datatypes

```haskell
{-# LANGUAGE DeriveGeneric #-}

-- Define a datatype that looks something like
data These f = These
                 { this :: NoIdentity f Counter
                 , that :: NoIdentity f (Vector1 Summary)
                 } deriving Generic
 
-- Initialize the said datatype
these :: These Metric -- You might need this if compiler cannot infer it
these = These
          (counter $ Info "this" "helpful comment")
          (vector "some" $ summary (Info "that" "helpful comment #2") def)
 
-- Register the metrics
registeredThese <- genericRegister these

-- Perform operations over the fields as usual
increment $ this registeredThese
withLabel "test" (that registeredThese) (`observe` 6.9)

-- Export metrics
bytestr <- genericExport registeredThese
```



## Library implementation

The underlying datatype representations look like:

- For basic metrics: `(Impure o Identity (Pure s))`, where
  - `s` is the inner __Pure__ representation (e.g. `Pure Double` in case of `Counter`);
  - `o` is additional arguments needed (e.g. buckets for `Histogram`s);

- For vectors: `Vector ((Impure (l, d) Identity (Pure (Map l) s))`, where
  - `l` is a list of label names it was constructed with;
  - `d` is the default singular basic metric (e.g. `Map` with all buckets set to 0 in case of a `Histogram`);
  - `Pure s` is now `Pure (Map l) s`, so it's a map of metrics.
