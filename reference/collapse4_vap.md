# Collapse Voting Age Population Race Categories into 4 Categories

Collapses Other, AIAN, Asian, NHPI, and Two+ into other.

## Usage

``` r
collapse4_vap(.data, prefix = "vap_")
```

## Arguments

- .data:

  tibble, data.frame, or sf tibble

- prefix:

  Default is `vap_`. The prefix for the race categories.

## Value

.data with columns collapsed

## Examples

``` r
data(mt_county)
mt_county <- mt_county |> collapse4_vap()
```
