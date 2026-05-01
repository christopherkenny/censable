# Breakdown Census GEOID into Components

Breakdown Census GEOID into Components

## Usage

``` r
breakdown_geoid(.data, GEOID = "GEOID", area_type = "spine")
```

## Arguments

- .data:

  dataframe, tibble, or sf tibble

- GEOID:

  Column in .data with Census GEOID

- area_type:

  String, default is 'spine' with type of GEOID. Options are 'spine' for
  states, counties, tracts, block groups, and blocks. 'shd' for lower
  state legislative districts, 'ssd' for upper state legislative
  districts, 'cd' for congressional districts, or 'zcta' for zip code
  tabulation areas.

## Value

.data with added identifying columns based on area_type

## Examples

``` r
data(mt_county)
mt_county <- mt_county |> breakdown_geoid()
```
