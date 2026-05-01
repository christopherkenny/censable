# Collapse Full Race Categories into 5 Categories

Collapses Other, AIAN, NHPI, and Two+ into Other, by prefix.

## Usage

``` r
collapse5(.data, prefix)
```

## Arguments

- .data:

  tibble, data.frame, or sf tibble

- prefix:

  The prefix(es) for the race categories. Must be a character vector.

## Value

.data with columns collapsed

## Examples

``` r
data(mt_county)
mt_county <- mt_county |> collapse5(prefix = c('pop_', 'vap_'))
```
