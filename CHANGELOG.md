# Change Log

## Upcoming...

## v0.2.0.0 

- Make `timed` and `timed'` require a `MonadMask` constraint for bracketing.
- [#7](https://github.com/sellerlabs/monad-metrics/pull/7) Switch `Map` to `HashMap`; [~4x faster to look up](https://github.com/sellerlabs/monad-metrics/pull/8)
- Fix a potential race condition when registering new metrics.

## v0.1.0.2

Fix bug where timed metrics are reported in the negatives.

## ~~v0.1.0.0~~ v0.1.0.1

Initial Release

