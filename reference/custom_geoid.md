# Create a GEOID from Columns

Create a GEOID from Columns

## Usage

``` r
custom_geoid(.data, ...)
```

## Arguments

- .data:

  dataframe, tibble, or sf tibble

- ...:

  columns of .data in the order you want to make the GEOID

## Value

.data with new column GEOID

## Examples

``` r
data(mt_county)
mt_county <- mt_county |> custom_geoid(GEOID)
```
