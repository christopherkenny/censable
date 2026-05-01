# Create GEOID from Default Columns

Create GEOID from Default Columns

## Usage

``` r
construct_geoid(
  .data,
  area_type,
  state = "state",
  county = "county",
  tract = "tract",
  block_group = "block group",
  block = "block",
  cd = "cd",
  shd = "shd",
  ssd = "ssd",
  zcta = "zcta"
)
```

## Arguments

- .data:

  dataframe, tibble, or sf tibble

- area_type:

  Defaults to creating the smallest possible with 'spine' for states,
  counties, tracts, block groups, and blocks. You can also pass one of
  the on spine geographies to create that specific level. Other options
  are 'shd' for lower state legislative districts, 'ssd' for upper state
  legislative districts, 'cd' for congressional districts, or 'zcta' for
  zip code tabulation areas.

- state:

  name of column with state component

- county:

  name of column with county component

- tract:

  name of column with tract component

- block_group:

  name of column with block group component

- block:

  name of column with block component

- cd:

  name of column with cd component

- shd:

  name of column with shd component

- ssd:

  name of column with ssd component

- zcta:

  name of column with zcta component

## Value

.data with new column GEOID

## Examples

``` r
data(mt_county)
mt_county <- mt_county |> breakdown_geoid()
mt_county <- mt_county |> dplyr::select(-dplyr::all_of('GEOID'))
mt_county <- mt_county |> construct_geoid()
```
