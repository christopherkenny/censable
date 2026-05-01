# Collapse Population Race Categories into 4 Categories

Collapses Other, AIAN, Asian, NHPI, and Two+ into other.

## Usage

``` r
collapse4_pop(.data, prefix = "pop_")
```

## Arguments

- .data:

  tibble, data.frame, or sf tibble

- prefix:

  Default is `pop_`. The prefix for the race categories.

## Value

.data with columns collapsed

## Examples

``` r
data(mt_county)
mt_county <- mt_county |> collapse4_pop()
```
