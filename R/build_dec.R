#' Build Data from the Decennial Census
#'
#'
#' @description
#' Creates a dataset, using the decennial census information, with the
#' standard variables used for redistricting. Creates a stable base for getting
#' data from `censusapi` for common calls in redistricting.
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
#' Arguments for `geography` are not checked, so will error if invalid.
#' This is by design, to avoid blocking usage that could become valid.
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
#' - 'school district (elementary)'
#' - 'school district (secondary)'
#' - 'voting district' may also work, though seems to be less reliable
#'
#'
#' @param geography Required. The geography level to use.
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geometry Defaults to TRUE. Whether to return the geometry or not.
#' @param year year, must be 2000, 2010, or 2020 (after August 2021)
#' @param groups defaults to `'all'`, which gets pop and vap. If `'pop'`, only gets pop.
#' If `'vap'`, only gets vap. Allows for analogous seven category race with `'all7'`, `'pop7'`, and `'vap7'`.
#' For counts for any part by race, you can supply `ap:race`, where race is in `c('black', 'white', 'aian', 'other', 'asian', 'nhpi')`.
#' Anything that can't be matched defaults to `'all'`, so you can pass `''` to get `'all'`.
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
  if (!any(groups %in% c('all', 'pop', 'vap', 'all7', 'pop7', 'vap7') | startsWith(groups, 'ap:'))) {
    groups <- 'all'
  }

  races <- stringr::str_sub(groups[stringr::str_detect(groups, 'ap:')], start = 4)
  ap_groups <- dplyr::case_when(
    'all' %in% groups ~ 'all',
    'all7' %in% groups ~ 'all',
    any(stringr::str_detect(groups, 'vap')) & !any(stringr::str_detect(groups, 'pop')) ~ 'vap',
    !any(stringr::str_detect(groups, 'vap')) & any(stringr::str_detect(groups, 'pop')) ~ 'pop',
    TRUE ~ 'all'
  )
  groups <- groups[!stringr::str_detect(groups, 'ap:')]
  if ('all' %in% groups) {
    groups <- setdiff(groups, c('pop', 'vap'))
  }
  if ('all7' %in% groups) {
    groups <- setdiff(groups, c('pop7', 'vap7'))
  }

  if (year %% 10 != 0) {
    stop('Decennial data only available for years ending in 0.')
  }

  if (year < 2000) {
    stop('Decennial endpoint for years before 2000 is not available.')
  }

  if (length(groups) > 0) {
    if (year %in% c(2020, 2010)) {
      vars <- lapply(groups, function(group) fetch_api_vars(survey = 'dec', year = year, group))
    } else {
      to_get <- dplyr::case_when(
        groups == 'all' ~ c('pop', 'vap'),
        groups == 'all7' ~ c('pop7', 'vap7'),
        TRUE ~ groups
      ) %>% unique()

      vars <- lapply(to_get, function(x) fetch_api_vars('dec', year, x))
    }

    out <- lapply(
      seq_along(vars), function(i) {
        get_census_api(
          geography = geography, state = state,
          year = year, county = county,
          variables = vars[[i]],
          tab = ifelse(year == 2020, 'dec/pl', 'dec/sf1')
        )
      })

    if (all(class(out) == 'list')) {
      if (length(out) > 1) {
        out <- purrr::reduce(out, dplyr::left_join, by = 'GEOID')
      } else {
        out <- out[[1]]
      }
    }

  } # end length(groups) > 0

  if (length(races) > 0) {
    ap_out <- build_dec_ap(
      geography = geography, state = state,
      county = county,
      geometry = length(groups) == 0,
      year = year, groups = ap_groups,
      races = races
    )

    if (length(groups) > 0) {
      out <- dplyr::left_join(out, ap_out, by = 'GEOID')
    } else {
      out <- ap_out
    }
  }

  if (geometry) {
    out <- out %>%
      dplyr::left_join(
        get_geometry(geography, year = year, state = state, county = county),
        by = 'GEOID'
      ) %>%
      dplyr::relocate('geometry', .after = dplyr::everything()) %>%
      sf::st_as_sf()
  } else {
    out <- tibble::as_tibble(out)
  }

  out %>%
    dplyr::rename_with(.fn = function(x) stringr::str_sub(x, end = -3), .cols = dplyr::ends_with('.x')) %>%
    dplyr::select(-dplyr::contains('.'))
}

build_dec_ap <- function(geography, state, county = NULL, geometry = FALSE,
                         year = 2020, groups = 'all', races = NULL) {
  if (is.null(races)) return(NULL)

  if (groups == 'all') {
    vars <- c(
      lapply(races, function(race) fetch_api_vars_ap(year, 'pop', race)),
      lapply(races, function(race) fetch_api_vars_ap(year, 'vap', race))
    )
  } else {
    vars <- lapply(races, function(race) fetch_api_vars_ap(year, groups, race))
  }

  noms <- paste0(
    rep({if (groups == 'all') {c('pop', 'vap')} else {groups}}, each = length(races)),
    '_ap_',
    rep(races, (groups == 'all') + 1)
  )
  names(vars) <- noms

  out <- lapply(
    seq_along(vars), function(i) {
      x <- noms[i]
      get_census_api(
        geography = geography, state = state,
        year = year, county = county,
        variables = vars[[i]],
        tab = 'dec/pl'
      ) %>%
        dplyr::transmute(
          GEOID = GEOID,
          {{ x }} := rowSums(dplyr::select(dplyr::as_tibble(.), dplyr::starts_with('P')), na.rm = TRUE)
        )
    }
  )

  if (all(class(out) == 'list')) {
    if (length(out) > 1) {
      out <- purrr::reduce(out, dplyr::left_join, by = 'GEOID')
    } else {
      out <- out[[1]]
    }
  }

  out
}
