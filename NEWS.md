# censable 0.0.4
* Internal changes to `build_dec()` and `build_acs()` to make variable fetching cleaner.
* Adds new `groups` to `build_dec()` for 7 category races with no Hispanic category: `'all7'`, `'pop7'`, and `'vap7'`.
* Allows any part race from `build_dec()` with `'ap:race'` for all race categories.
* Adds call to `breakdown_geoid()` in `build_dec()`.

# censable 0.0.3

* Update year default to 2020 and allow for 2020 calls
* Added a `NEWS.md` file to track changes to the package.
