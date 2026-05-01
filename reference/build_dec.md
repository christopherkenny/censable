# Build Data from the Decennial Census

Creates a dataset, using the decennial census information, with the
standard variables used for redistricting. Creates a stable base for
getting data from `censusapi` for common calls in redistricting.

## Usage

``` r
build_dec(
  geography,
  state = NULL,
  county = NULL,
  geometry = TRUE,
  year = 2020,
  groups = "all"
)

mem_build_dec(
  geography,
  state = NULL,
  county = NULL,
  geometry = TRUE,
  year = 2020,
  groups = "all"
)
```

## Arguments

- geography:

  Required. The geography level to use.

- state:

  Required. Two letter state postal code.

- county:

  Optional. Name of county. If not provided, returns blocks for the
  entire state.

- geometry:

  Defaults to TRUE. Whether to return the geometry or not.

- year:

  year, must be 2000, 2010, or 2020 (after August 2021)

- groups:

  defaults to `'all'`, which gets pop and vap. If `'pop'`, only gets
  pop. If `'vap'`, only gets vap. Allows for analogous seven category
  race with `'all7'`, `'pop7'`, and `'vap7'`. For counts for any part by
  race, you can supply `ap:race`, where race is in
  `c('black', 'white', 'aian', 'other', 'asian', 'nhpi')`. Anything that
  can't be matched defaults to `'all'`, so you can pass `''` to get
  `'all'`.

## Value

tibble with observations for each observation of the geography in the
state or county. Data includes up to 2 sets of columns for each race or
ethnicity category: population (pop) and voting age population (vap)

## Default output columns are:

- GEOID: Geographic Identifier

- NAME: Name of County

- pop: total population

- pop_white: total population, Non-Hispanic White

- pop_black: total population, Non-Hispanic Black

- pop_hisp: total population, Hispanic

- pop_aian: total population, Non-Hispanic American Indian and Alaskan
  Native

- pop_asian: total population, Non-Hispanic Asian

- pop_nhpi: total population, Non-Hispanic Native Hawaiian and Pacific
  Islander

- pop_other: total population, Non-Hispanic Other

- pop_two: total population, Non-Hispanic Two Plus Races

- vap: voting age population

- vap_white: voting age population, Non-Hispanic White

- vap_black: voting age population, Non-Hispanic Black

- vap_hisp: voting age population, Hispanic

- vap_aian: voting age population, Non-Hispanic American Indian and
  Alaskan Native

- vap_asian: voting age population, Non-Hispanic Asian

- vap_nhpi: voting age population, Non-Hispanic Native Hawaiian and
  Pacific Islander

- vap_other: voting age population, Non-Hispanic Other

- vap_two: voting age population, Non-Hispanic Two Plus Races

- geometry: sf geometry

Arguments for `geography` are not checked, so will error if invalid.
This is by design, to avoid blocking usage that could become valid.

Currently valid options for `geography`:

- 'state'

- 'county'

- 'tract'

- 'block group'

- 'block'

- 'county subdivision'

- 'zcta'

- 'congressional district'

- 'state legislative district (upper chamber)'

- 'state legislative district (lower chamber)'

- 'school district (unified)'

- 'school district (elementary)'

- 'school district (secondary)'

- 'voting district' may also work, though seems to be less reliable

## Examples

``` r
if (FALSE) { # has_census_key()
# uses the Census API
tb <- build_dec(geography = 'block', state = 'NY', county = 'Rockland', geometry = TRUE)
}
```
