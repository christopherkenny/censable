
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censable <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/christopherkenny/censable/workflows/R-CMD-check/badge.svg)](https://github.com/christopherkenny/censable/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`censable` creates a common interface to `tidycensus`, with
[memoized](https://github.com/r-lib/memoise) data functions. With a
growing redistricting universe in R, it seems right to partition out
some use cases, to allow imports to be more targeted and to stop
packages from repeating each other.

## Installation

<!--
You can install the released version of censable from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("censable")
```
-->

You can download the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("christopherkenny/censable")
```

## Example

``` r
library(censable)
```

The primary data functions are `build_dec` and `build_acs`

``` r
de <- build_dec(geography = 'county', state = 'DE')
#> Getting data from the 2010 decennial Census
#> Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.
#> Using Census Summary File 1
```

The `build_*` functions take arguments on geography and state, at a
minimum. If you’re looking at a smaller piece of a state, you can use
`county` to subset it. Year defaults to 2010, but will be updated to
2020 around October 2021, following the API release of the 2020 Census
data.

The `build_*` functions are a convenient wrapper around `tidycensus`,
with an eye towards collecting data on total population and voting age
population by race. Total population variables begin with the prefix
`pop`, while voting age population variables begin with the prefix
`vap`.

Additionally, there are memoised versions of the `build_*` functions:

``` r
de <- mem_build_dec(geography = 'county', state = 'DE')
#> Getting data from the 2010 decennial Census
#> Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.
#> Using Census Summary File 1
```

These speed up repeated calls, allowing you to have code that is self
contained and reproducable, without taking the time hit of having to
redownload the data each time.
