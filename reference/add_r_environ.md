# Add Entry to Renviron

Adds a value to the Renvironment of the form `name=value`. Designed for
flexibly adding API keys for future sessions. Defaults are set up for
entering a Census API key to work with `tidycensus`. By default this key
will be configured to work with `tidycensus`. Package internally allows
this key to work with `censusapi` when used through `censable`.

## Usage

``` r
add_r_environ(
  value,
  name = "CENSUS_API_KEY",
  overwrite = FALSE,
  install = FALSE
)
```

## Arguments

- value:

  Character. Value to add.

- name:

  Defaults to CENSUS_API_KEY. Character. Name to give `value`.

- overwrite:

  Defaults to FALSE. Boolean. Should existing item with name `name` in
  Renviron be overwritten?

- install:

  Defaults to FALSE. Boolean. Should this be added '~/.Renviron' file?

## Value

value, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
add_r_environ('1234', 'SECRET_API_KEY')
} # }
```
