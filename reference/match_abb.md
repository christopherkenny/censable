# Try to Match to State Abbreviation

Searches for an exact match and offers the best match if no exact match

## Usage

``` r
match_abb(state)
```

## Arguments

- state:

  character with state FIPS, Abbreviation, Name, or ANSI

## Value

Abbreviation if a match is found or character(0) if no match is found

## Examples

``` r
match_abb('NY')
#> [1] "NY"
match_abb('01')
#> [1] "AL"
```
