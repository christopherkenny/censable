#' Build Data from the Decennial Census
#'
#'
#' @description
#' Creates a dataset, using the decennial census information, with the
#' standard variables used for redistricting. Creates a stable base for getting
#' data from tidycensus for common calls in redistricting.
#'
#' # Default output columns are:
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
#' Depending on the day, weather, and other unrelated things, `'voting district'` may also work.
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
#' @param groups defaults to `'all'`, which gets pop and vap. If `'pop'`, only gets pop.
#' If `'vap'`, only gets vap. Allows for analogous seven category race with `'all7'`, `'pop7'`, and `'vap7'`.
#' Any other strings default to `'all'`.
#'
#' @return tibble with observations for each observation of the geography in the state
#' or county. Data includes up to 2 sets of columns for each race or ethnicity category:
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
  # if (!isTRUE(groups[1] %in% c('all', 'pop', 'vap', 'all7', 'pop7', 'vap7'))) {
  #   groups <- 'all'
  # }

  if (year %% 10 != 0) {
    stop('Decennial data only available for years ending in 0.')
  }

  if (year < 2000) {
    stop('Decennial endpoint for years before 2000 is not available.')
  }

  if (year %in% c(2020, 2010)) {
    vars <- fetch_api_vars(survey = 'dec', year = year, groups)

    out <- tidycensus::get_decennial(
      geography = geography, state = state,
      year = year, county = county,
      geometry = geometry, keep_geo_vars = FALSE,
      variables = vars, output = 'wide'
    )
  } else { # 2000

    to_get <- dplyr::case_when(
      groups == 'all' ~ c('pop', 'vap'),
      groups == 'all7' ~ c('pop7', 'vap7'),
      TRUE ~ groups
    ) %>% unique()

    vars <- lapply(to_get, function(x) fetch_api_vars('dec', year, x))

    out <- lapply(
      seq_along(vars), function(i) {
        tidycensus::get_decennial(
          geography = geography, state = state,
          year = year, county = county,
          geometry = (geometry & i <= 1), keep_geo_vars = FALSE,
          variables = vars[[i]], output = 'wide'
        )
      }
    )
  }

  if (all(class(out) == 'list')) {
    if (length(out) > 1) {
      out <- purrr::reduce(out, dplyr::left_join, by = 'GEOID') %>%
        sf::st_as_sf() %>%
        dplyr::rename_with(.fn = function(x) stringr::str_sub(x, end = -3), .cols = dplyr::ends_with('.x')) %>%
        dplyr::select(-dplyr::contains('.'))
    } else {
      out <- out[[1]]
    }
  }

  # if (any(stringr::str_detect(groups, 'ap:'))) {
  #   ap_groups <- groups[]
  # }

  out
}

build_dec_ap <- function() {
  NULL
}
