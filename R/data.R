#' stata (State Data)
#'
#' @name stata
#'
#' @description
#' tibble with columns:
#' - fips: Federal Information Processing Standards codes
#' - abb: two letter postal abbreviations
#' - name: title case state name
#' - ansi: American National Standards Institute codes
#' - region: Census Regions (for 50 states and D.C.)
#' - division: Census Divisions (for 50 states and D.C.)
#'
#' @return tibble with state identifying information
#'
#' @usage
#' data('stata')
#'
#' @examples
#' data('stata')
#'
#' @md
#' @concept data
NULL


#' Montana County Data
#'
#' @name mt_county
#'
#' @description
#' - GEOID: Geographic Identifier
#' - NAME: Name of County
#' - pop: total population
#' - pop_white: total population, Non-Hispanic White
#' - pop_black: total population, Non-Hispanic Black
#' - pop_hisp: total population, Hispanic
#' - pop_aian: total population, Non-Hispanic American Indian and Alaskan Native
#' - pop_asian: total population, Non-Hispanic Asian
#' - pop_nhpi: total population, Non-Hispanic Native Hawaiian and Pacific Islander
#' - pop_other: total population, Non-Hispanic Other
#' - pop_two: total population, Non-Hispanic Two Plus Races
#' - vap: voting age population
#' - vap_white: voting age population, Non-Hispanic White
#' - vap_black: voting age population, Non-Hispanic Black
#' - vap_hisp: voting age population, Hispanic
#' - vap_aian: voting age population, Non-Hispanic American Indian and Alaskan Native
#' - vap_asian: voting age population, Non-Hispanic Asian
#' - vap_nhpi: voting age population, Non-Hispanic Native Hawaiian and Pacific Islander
#' - vap_other: voting age population, Non-Hispanic Other
#' - vap_two: voting age population, Non-Hispanic Two Plus Races
#' - geometry: sf geometry
#'
#' @return sf tibble with one observation for each county in Montana
#'
#' @usage
#' data('mt_county')
#'
#' @examples
#' data('mt_county')
#'
#' @md
#' @concept data
NULL

#' Counties FIPS 2000
#'
#' @name fips_2000
#'
#' @description
#' Contains three columns:
#' - state: state FIPS
#' - county: county FIPS
#' - name: county name
#'
#' @return tibble
#'
#' @usage
#' data('fips_2000')
#'
#' @examples
#' data('fips_2000')
#'
#' @md
#' @concept data
NULL

#' Counties FIPS 2010
#'
#' @name fips_2010
#'
#' @description
#' Contains three columns:
#' - state: state FIPS
#' - county: county FIPS
#' - name: county name
#'
#' @return tibble
#'
#' @usage
#' data('fips_2010')
#'
#' @examples
#' data('fips_2010')
#'
#' @md
#' @concept data
NULL

#' Counties FIPS 2020
#'
#' @name fips_2020
#'
#' @description
#' Contains three columns:
#' - state: state FIPS
#' - county: county FIPS
#' - name: county name
#'
#' @return tibble
#'
#' @usage
#' data('fips_2020')
#'
#' @examples
#' data('fips_2020')
#'
#' @md
#' @concept data
NULL
