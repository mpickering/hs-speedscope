# hs-speedscope

`hs-speedscope` is a simple executable for converting an eventlog into a format suitable to load into [speedscope](https://www.speedscope.app/).

WARNING: Only GHC 8.10 supports generating an eventlog with the correct events for this program to work.

## Usage

1. Create an eventlog which contains time profiling events by running your program with `program +RTS -p -l-au`.
2. Run `hs-speedscope` on the resulting eventlog `hs-speedscope program.eventlog`.
3. Load the resulting `program.eventlog.json` file into [speedscope](https://speedscope.app) to visualise the profile.

## Filtering an eventlog

It is sometimes useful to isolate a specific part of the sample, for example, when
I was profiling ghcide, I want to isolate a single hover request.

The `--start` and `--end` options can be used to indicate which parts of the
eventlog to keep. The filtering options look for messages inserted into the
eventlog by [`traceMarker`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Debug-Trace.html#v:traceMarker) events.

* No events before the first marker which matches the prefix given by `--start`
will be included in the result
* No events after the first marker which matches the prefix given by `--end` will
be included in the result.

For example, the following invocation will filter the profile between the START and END markers.

```
hs-speedscope File.eventlog --start START --end END
```

