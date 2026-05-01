# Collapse Voting Age Population Race Categories into 5 Categories

Collapses Other, AIAN, NHPI, and Two+ into other.

## Usage

``` r
collapse5_vap(.data, prefix = "vap_")
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
mt_county <- mt_county |> collapse5_vap()
```
