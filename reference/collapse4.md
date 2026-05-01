# Collapse Full Race Categories into 4 Categories

Collapses Other, AIAN, Asian, NHPI, and Two+ into other, by prefix.

## Usage

``` r
collapse4(.data, prefix)
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
mt_county <- mt_county |> collapse4(prefix = c('pop_', 'vap_'))
```
