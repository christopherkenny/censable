# Try to Match to State ANSI

Searches for an exact match and offers the best match if no exact match

## Usage

``` r
match_ansi(state)
```

## Arguments

- state:

  character with state FIPS, Abbreviation, Name, or ANSI

## Value

ANSI if a match is found or character(0) if no match is found

## Examples

``` r
match_ansi('NY')
#> [1] "1779796"
match_ansi('01')
#> [1] "1779775"
```
