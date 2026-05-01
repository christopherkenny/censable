# Try to Match to State FIPS

Searches for an exact match and offers the best match if no exact match

## Usage

``` r
match_fips(state)
```

## Arguments

- state:

  character with state FIPS, Abbreviation, Name, or ANSI

## Value

FIPS code if a match is found or character(0) if no match is found

## Examples

``` r
match_fips('NY')
#> [1] "36"
match_fips('01')
#> [1] "01"
```
