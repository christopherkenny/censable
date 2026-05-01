# Build Data from the American Community Survey

Creates a dataset, using the decennial census information, with the
standard variables used for redistricting. Creates a stable base for
getting data from `censusapi` for common calls in redistricting.

\#' \# Output columns are:

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
This is by design to avoid blocking usage that could become valid.

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

## Usage

``` r
build_acs(
  geography,
  state = NULL,
  county = NULL,
  geometry = TRUE,
  year = 2020,
  survey = "acs5",
  groups = "all"
)

mem_build_acs(
  geography,
  state = NULL,
  county = NULL,
  geometry = TRUE,
  year = 2020,
  survey = "acs5",
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

- survey:

  whether the get estimates from the 5-year ('acs5'), 3-year ('acs3'),
  or 1-year ('acs1') survey. Default is 'acs5'.

- groups:

  defaults to 'all', which gets pop and vap. If 'pop', only gets pop. If
  'vap', only gets vap. Any other strings default to 'all'.

## Value

tibble with observations for each observation of the geography in the
state or county. Data includes up to 3 sets of columns for each race or
ethnicity category: population (pop), voting age population (vap), and
citizen voting age population (cvap)

## Examples

``` r
if (FALSE) { # has_census_key()
# uses the Census API
tb <- build_acs(geography = 'tract', state = 'NY', county = 'Rockland', geometry = TRUE)
}
```
