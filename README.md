# prometheus-porting

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
cntr   <- register . counter   $ mkInfo "counter"   "counter_help"
gg     <- register . gauge     $ mkInfo "gauge"     "gauge_help"
hstgrm <- register . histogram ( mkInfo "histogram" "histogram_help" ) defBuckets
smmr   <- register . summary   ( mkInfo "summary"   "summary_help"   ) defQuantiles

-- Collect some data with the metrics
increment cntr
plus 5 cntr

increment gg
plus 5 gg
decrement gg
minus 5 gg
set 5 gg

observe 2.5 hstgrm

observe 2.5 smmr

-- Extract and export metrics
cntrCurrentValue <- extract cntr
ggExported <- export gg


-- In case of vectors everything is the same, but with additional functions
vcntr <- register . vector "label" . counter $ mkInfo "counter"   "counter_help"

withLabel "this" vcntr increment
withlabel "this" vcntr $ plus 5

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
withLabel "test" (that registeredThese) $ observe 6.9

-- Export metrics
bytestr <- genericExport registeredThese
```

## Examples

There are a couple of example on how to plug this machinery in real applications under [example](./example) directory.


### Non HTTP Application
[Simple Example](./example/simple/Main.hs) that provides how to plug this in a non HTTP Application.

### HTTP/Wai Application
[Server Example](./example/server/Main.hs) that provides how to plug this in a Wai HTTP Application. In this case this middleware is going to give you for free metrics on latency and count status code for your HTTP endpoints
