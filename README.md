# hs-speedscope

`hs-speedscope` is a simple executable for converting an eventlog into a format suitable to load into [speedscope](https://www.speedscope.app/).

WARNING: Only GHC 8.10 supports generating an eventlog with the correct events for this program to work.

## Usage

1. Create an eventlog which contains time profiling events by running your program with `program +RTS -p -l-au`.
2. Run `hs-speedscope` on the resulting eventlog `hs-speedscope program.eventlog`.
3. Load the resulting `program.eventlog.json` file into [speedscope](https://speedscope.app) to visualise the profile.

