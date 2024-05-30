# censable 0.0.7

* Add support for `match_*()` function to take multiple states at once.

# censable 0.0.6

* Fixes an error where `build_dec()` and `build_acs()` fail when `geography = 'state'`.
* Fixes an error where empty geometries are returned with `county = NULL` for some `geography` entries.

# censable 0.0.5

* Resolves an issue where block-level requests with geometry would fail for 2000 data.


# censable 0.0.4

* Internal changes to `build_dec()` and `build_acs()` to make variable fetching cleaner.
* Adds new `groups` to `build_dec()` for 7 category races with no Hispanic category: `'all7'`, `'pop7'`, and `'vap7'`.
* Allows any part race from `build_dec()` with `'ap:race'` for all race categories.
* Adds call to `breakdown_geoid()` in `build_dec()`.

# censable 0.0.3

* Update year default to 2020 and allow for 2020 calls
* Added a `NEWS.md` file to track changes to the package.
