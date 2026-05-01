# Try to Match to State Name

Searches for an exact match and offers the best match if no exact match

## Usage

``` r
match_name(state)
```

## Arguments

- state:

  character with state FIPS, Abbreviation, Name, or ANSI

## Value

Name if a match is found or character(0) if no match is found

## Examples

``` r
match_name('NY')
#> [1] "New York"
match_name('01')
#> [1] "Alabama"
```
