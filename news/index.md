# Changelog

## censable 0.0.8

CRAN release: 2025-09-01

- Fixes issue where
  [`build_dec()`](https://christopherkenny.github.io/censable/reference/build_dec.md)
  would fail for blocks in 2010.
- Cleans up old pipe re-exports in favor of new base R pipe system.

## censable 0.0.7

- Add support for `match_*()` function to take multiple states at once.

## censable 0.0.6

- Fixes an error where
  [`build_dec()`](https://christopherkenny.github.io/censable/reference/build_dec.md)
  and
  [`build_acs()`](https://christopherkenny.github.io/censable/reference/build_acs.md)
  fail when `geography = 'state'`.
- Fixes an error where empty geometries are returned with
  `county = NULL` for some `geography` entries.

## censable 0.0.5

CRAN release: 2022-11-19

- Resolves an issue where block-level requests with geometry would fail
  for 2000 data.

## censable 0.0.4

- Internal changes to
  [`build_dec()`](https://christopherkenny.github.io/censable/reference/build_dec.md)
  and
  [`build_acs()`](https://christopherkenny.github.io/censable/reference/build_acs.md)
  to make variable fetching cleaner.
- Adds new `groups` to
  [`build_dec()`](https://christopherkenny.github.io/censable/reference/build_dec.md)
  for 7 category races with no Hispanic category: `'all7'`, `'pop7'`,
  and `'vap7'`.
- Allows any part race from
  [`build_dec()`](https://christopherkenny.github.io/censable/reference/build_dec.md)
  with `'ap:race'` for all race categories.
- Adds call to
  [`breakdown_geoid()`](https://christopherkenny.github.io/censable/reference/breakdown_geoid.md)
  in
  [`build_dec()`](https://christopherkenny.github.io/censable/reference/build_dec.md).

## censable 0.0.3

CRAN release: 2021-10-05

- Update year default to 2020 and allow for 2020 calls
- Added a `NEWS.md` file to track changes to the package.
