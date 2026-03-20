# Revision history for hs-speedscope

## 0.3.1 -- 2026-03-20
* Critical: fix event dropping logic [#30](https://github.com/mpickering/hs-speedscope/pull/30)
  This bug was introduced in 0.3.0.
* Fix cabal check warning [#27](https://github.com/mpickering/hs-speedscope/pull/27)
* Support GHC-9.14 [#28](https://github.com/mpickering/hs-speedscope/pull/28)
* Allow passing RTS options [#29](https://github.com/mpickering/hs-speedscope/pull/29)
* Fix logic for dropping system CCS entries [#32](https://github.com/mpickering/hs-speedscope/pull/32)

## 0.3.0 -- 2025-06-07
* Introduce Speedscope.Schema [#12](https://github.com/mpickering/hs-speedscope/pull/12)
* Allow more recent ghc-events [#13](https://github.com/mpickering/hs-speedscope/pull/13)
* Use CURRENT_PACKAGE_VERSION to get package version [#14](https://github.com/mpickering/hs-speedscope/pull/14)
* Use machines to decouple filtering and accumulation [#15](https://github.com/mpickering/hs-speedscope/pull/15)
* Abstract the conversion of events into samples [#17](https://github.com/mpickering/hs-speedscope/pull/17)
* Relax ghc-events bound [#19](https://github.com/mpickering/hs-speedscope/pull/19)
* Fix building with ghc-events-0.20 [#22](https://github.com/mpickering/hs-speedscope/pull/22)
* Add CI for recent GHCs and build bindists [#24](https://github.com/mpickering/hs-speedscope/pull/24)

## 0.2.1 -- 2020-12-19

* Support ghc-events 0.13, 0.14 and 0.15.

## 0.2 -- 2020-03-21

* Add --start and --end options which allow a profile to be filtered by
  eventlog UserMarker events

## 0.1.1 -- 2019-11-07

* Relax eventlog version check to allow 8.9

## 0.1 -- 2019-10-29

* Initial release
