#' Build Data from the Decennial Census
#'
#'
#' @description
#' Creates a dataset, using the decennial census information, with the
#' standard variables used for redistricting. Creates a stable base for getting
#' data from tidycensus for common calls in redistricting.
#'
#' # Output columns are:
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
#'
#' Arguments for `geography` are not checked, so will fail with `tidycensus` errors if invalid.
#' This is by design to avoid blocking usage that could become valid, especially following
#' the 2020 Census data release.
#'
#' Currently valid options for `geography`:
#' - 'state'
#' - 'county'
#' - 'tract'
#' - 'block group'
#' - 'block'
#' - 'county subdivision'
#' - 'zcta'
#' - 'congressional district'
#' - 'state legislative district (upper chamber)'
#' - 'state legislative district (lower chamber)'
#' - 'school district (unified)'
#'
#' Full options for `geography` that may or may not be valid depending on year and geometry
#' are listed at:
#' [Kyle Walker's tidycensus site](https://walker-data.com/tidycensus/articles/basic-usage.html).
#'
#'
#' @param geography Required. The geography level to use.
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geometry Defaults to TRUE. Whether to return the geometry or not.
#' @param year year, must be 2000, 2010, or 2020 (after August 2021)
#' @param groups defaults to 'all', which gets pop and vap. If 'pop', only gets pop.
#' If 'vap', only gets vap. Any other strings default to 'all'.
#'
#' @return tibble with observations for each observation of the geography in the state
#' or county. Data includes 2 sets of columns for each race or ethnicity category:
#' population (pop) and voting age population (vap)
#'
#' @md
#' @export
#' @concept build
#' @examples
#' \dontrun{
#' # uses the Census API
#' tb <- build_dec(geography = 'block', state = 'NY', county = 'Rockland', geometry = TRUE)
#' }
build_dec <- function(geography, state, county = NULL, geometry = TRUE,
                      year = 2020, groups = 'all') {
  if (!isTRUE(groups[1] %in% c('all', 'pop', 'vap'))) {
    groups <- 'all'
  }

  if (year %% 10 != 0) {
    stop('Decennial data only available for years ending in 0.')
  }

  if (year < 2000) {
    stop('Decennial endpoint for years before 2000 is not available.')
  }

  if (year == 2020) {
    vars <- c(
      pop = 'P2_001N', pop_white = 'P2_005N', pop_black = 'P2_006N',
      pop_hisp = 'P2_002N', pop_aian = 'P2_007N', pop_asian = 'P2_008N',
      pop_nhpi = 'P2_009N', pop_other = 'P2_010N', pop_two = 'P2_011N',
      vap = 'P4_001N', vap_white = 'P4_005N', vap_black = 'P4_006N',
      vap_hisp = 'P4_002N', vap_aian = 'P4_007N', vap_asian = 'P4_008N',
      vap_nhpi = 'P4_009N', vap_other = 'P4_010N', vap_two = 'P4_011N'
    )

    vars7 <- c(
      pop7 = 'P1_001N', pop7_white = 'P1_003N', pop7_black = 'P1_004N',
      pop_aian = 'P1_005N', pop_asian = 'P1_006N',
      pop_nhpi = 'P1_007N', pop_other = 'P1_008N', pop_two = 'P1_009N',
      vap7 = 'P3_001N', vap7_white = 'P3_003N', vap7_black = 'P3_004N',
      vap7_aian = 'P3_005N', vap7_asian = 'P3_006N',
      vap7_nhpi = 'P3_007N', vap7_other = 'P3_008N', vap7_two = 'P3_009N',
    )

    if (groups == 'pop') {
      vars <- vars[stringr::str_detect(names(vars), 'pop')]
    } else if (groups == 'vap') {
      vars <- vars[stringr::str_detect(names(vars), 'vap')]
    }

    out <- tidycensus::get_decennial(
      geography = geography, state = state,
      year = year, county = county,
      geometry = geometry, keep_geo_vars = FALSE,
      variables = vars, output = 'wide'
    )
  } else if (year == 2010) {
    vars <- c(
      pop = 'P003001', pop_white = 'P005003', pop_black = 'P005004',
      pop_hisp = 'P004003', pop_aian = 'P005005', pop_asian = 'P005006',
      pop_nhpi = 'P005007', pop_other = 'P005008', pop_two = 'P005009',
      vap = 'P010001', vap_white = 'P011005', vap_black = 'P011006',
      vap_hisp = 'P011002', vap_aian = 'P011007', vap_asian = 'P011008',
      vap_nhpi = 'P011009', vap_other = 'P011010', vap_two = 'P011011'
    )

    vars7 <- c(
      pop7 = 'P003001', pop7_white = 'P003002', pop7_black = 'P003003',
      pop7_aian = 'P003004', pop7_asian = 'P003005',
      pop7_nhpi = 'P003006', pop7_other = 'P003007', pop7_two = 'P005008',
      vap7 = 'P010001', vap7_white = 'P010003', vap7_black = 'P010004',
      vap7_aian = 'P010005', vap7_asian = 'P010006',
      vap7_nhpi = 'P010007', vap7_other = 'P010008', vap7_two = 'P010009'
    )

    if (groups == 'pop') {
      vars <- vars[stringr::str_detect(names(vars), 'pop')]
    } else if (groups == 'vap') {
      vars <- vars[stringr::str_detect(names(vars), 'vap')]
    }

    out <- tidycensus::get_decennial(
      geography = geography, state = state,
      year = year, county = county,
      geometry = geometry, keep_geo_vars = FALSE,
      variables = vars, output = 'wide'
    )
  } else { # 2000
    vars_pop <- c(
      pop = 'P004001', pop_white = 'P004005', pop_black = 'P004006',
      pop_hisp = 'P004002', pop_aian = 'P004007', pop_asian = 'P004008',
      pop_nhpi = 'P004009', pop_other = 'P004010', pop_two = 'P004011'
    )

    vars_vap <- c(
      vap = 'P006001', vap_white = 'P006005', vap_black = 'P006006',
      vap_hisp = 'P006002', vap_aian = 'P006007', vap_asian = 'P006008',
      vap_nhpi = 'P006009', vap_other = 'P006010', vap_two = 'P006011'
    )

    vars_pop7 <- c(
      pop7 = 'P004001', pop7_white = 'P004005', pop7_black = 'P004006',
      pop7_aian = 'P004007', pop7_asian = 'P004008',
      pop7_nhpi = 'P004009', pop7_other = 'P004010', pop7_two = 'P004011'
    )


    if (!missing(county)) {
      if (groups[1] %in% c('pop', 'all')) {
        out_pop <- tidycensus::get_decennial(
          geography = geography, state = state,
          year = year,
          geometry = geometry, keep_geo_vars = FALSE,
          variables = vars_pop
        )
      }
      if (groups[1] %in% c('vap', 'all')) {
        out_vap <- tidycensus::get_decennial(
          geography = 'block', state = state, year = year,
          geometry = FALSE, keep_geo_vars = FALSE,
          variables = vars_vap, output = 'wide'
        )
      }
    } else {
      if (groups[1] %in% c('pop', 'all')) {
        out_pop <- tidycensus::get_decennial(
          geography = geography, state = state,
          year = year, county = county,
          geometry = geometry, keep_geo_vars = FALSE,
          variables = vars_pop
        )
      }
      if (groups[1] %in% c('vap', 'all')) {
        out_vap <- tidycensus::get_decennial(
          geography = geography, state = state,
          year = year, county = county,
          geometry = FALSE, keep_geo_vars = FALSE,
          variables = vars_vap, output = 'wide'
        )
      }
    }
    if (groups[1] == 'all') {
      out <- out_pop %>%
        dplyr::left_join(out_vap, by = 'GEOID') %>%
        sf::st_as_sf()
    }
  }
  out
}
