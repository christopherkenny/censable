#' Build Data from the Decennial Census
#'
#' @description
#' Creates a dataset, using the decennial census information, with the
#' standard variables used for redistricting. Creates a stable base for getting
#' data from `censusapi` for common calls in redistricting.
#'
#' #' # Output columns are:
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
#' This is by design to avoid blocking usage that could become valid.
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
#'
#' @param geography Required. The geography level to use.
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geometry Defaults to TRUE. Whether to return the geometry or not.
#' @param year year, must be 2000, 2010, or 2020 (after August 2021)
#' @param survey whether the get estimates from the 5-year ('acs5'), 3-year ('acs3'),
#' or 1-year ('acs1') survey. Default is 'acs5'.
#' @param groups defaults to 'all', which gets pop and vap. If 'pop', only gets pop.
#' If 'vap', only gets vap. Any other strings default to 'all'.
#'
#' @return tibble with observations for each observation of the geography in the state
#' or county. Data includes up to 3 sets of columns for each race or ethnicity category:
#' population (pop), voting age population (vap), and citizen voting age population (cvap)
#'
#' @export
#' @concept build
#' @examples
#' \dontrun{
#' # uses the Census API
#' tb <- build_acs(geography = 'tract', state = 'NY', county = 'Rockland', geometry = TRUE)
#' }
build_acs <- function(geography, state, county = NULL, geometry = TRUE, year = 2020,
                      survey = 'acs5', groups = 'all') {
  if (!isTRUE(groups[1] %in% c('all', 'pop', 'vap'))) {
    groups <- 'all'
  }

  if (year < 2005) {
    stop('ACS endpoint for years before 2005 is not available.')
  }

  # totals + white + black + hisp / for total, vap, and cvap (by sex because acs...)
  vars <- c(
    pop = 'B03002_001',
    pop_white = 'B03002_003',
    pop_black = 'B03002_004',
    pop_hisp = 'B03002_012',
    pop_aian = 'B03002_005',
    pop_asian = 'B03002_006',
    pop_nhpi = 'B03002_007',
    pop_other = 'B03002_008',
    pop_two = 'B03002_009',
    m_vap = 'B05003_008', m_nvap = 'B05003_012',
    f_vap = 'B05003_019', f_nvap = 'B05003_023',
    m_vap_black = 'B05003B_008', m_nvap_black = 'B05003B_012',
    f_vap_black = 'B05003B_019', f_nvap_black = 'B05003B_023',
    m_vap_white = 'B05003H_008', m_nvap_white = 'B05003H_012',
    f_vap_white = 'B05003H_019', f_nvap_white = 'B05003H_023',
    m_vap_hisp = 'B05003I_008', m_nvap_hisp = 'B05003I_012',
    f_vap_hisp = 'B05003I_019', f_nvap_hisp = 'B05003I_023',
    m_vap_aian = 'B05003C_008', m_nvap_aian = 'B05003C_012',
    f_vap_aian = 'B05003C_019', f_nvap_aian = 'B05003C_023',
    m_vap_asian = 'B05003D_008', m_nvap_asian = 'B05003D_012',
    f_vap_asian = 'B05003D_019', f_nvap_asian = 'B05003D_023',
    m_vap_nhpi = 'B05003E_008', m_nvap_nhpi = 'B05003E_012',
    f_vap_nhpi = 'B05003E_019', f_nvap_nhpi = 'B05003E_023',
    m_vap_other = 'B05003F_008', m_nvap_other = 'B05003F_012',
    f_vap_other = 'B05003F_019', f_nvap_other = 'B05003F_023',
    m_vap_two = 'B05003G_008', m_nvap_two = 'B05003G_012',
    f_vap_two = 'B05003G_019', f_nvap_two = 'B05003G_023'
  )

  if (groups == 'pop') {
    vars <- vars[stringr::str_detect(names(vars), 'pop')]
  } else if (groups == 'vap') {
    vars <- vars[stringr::str_detect(names(vars), '_vap')]
  } else if (groups == 'cvap') {
    vars <- vars[stringr::str_detect(names(vars), 'vap')]
  }

  vars_duct_tape <- paste0(vars, 'E')
  names(vars_duct_tape) <- names(vars)

  out <- get_census_api(
    geography = geography, state = state,
    year = year, county = county,
    variables = vars_duct_tape,
    tab = paste0('acs/', survey)
  )

  out <- out %>%
    dplyr::select(-dplyr::ends_with('M')) %>%
    dplyr::rename_with(drop_E)

  if (groups[1] %in% c('cvap', 'vap', 'all')) {
    out <- out %>%
      dplyr::mutate(
        vap = .data$m_vap + .data$f_vap,
        vap_white = .data$m_vap_white + .data$f_vap_white,
        vap_black = .data$m_vap_black + .data$f_vap_black,
        vap_hisp = .data$m_vap_hisp + .data$f_vap_hisp,
        vap_aian = .data$m_vap_aian + .data$f_vap_aian,
        vap_asian = .data$m_vap_asian + .data$f_vap_asian,
        vap_nhpi = .data$m_vap_nhpi + .data$f_vap_nhpi,
        vap_other = .data$m_vap_other + .data$f_vap_other,
        vap_two = .data$m_vap_two + .data$f_vap_two
      )

    if (groups[1] %in% c('cvap', 'all')) {
      out <- out %>%
        dplyr::mutate(
          cvap = .data$vap - .data$m_nvap - .data$f_nvap,
          cvap_white = .data$vap_white - .data$m_nvap_white - .data$f_nvap_white,
          cvap_black = .data$vap_black - .data$m_nvap_black - .data$f_nvap_black,
          cvap_hisp = .data$vap_hisp - .data$m_nvap_hisp - .data$f_nvap_hisp,
          cvap_aian = .data$vap_aian - .data$m_nvap_aian - .data$f_nvap_aian,
          cvap_asian = .data$vap_asian - .data$m_nvap_asian - .data$f_nvap_asian,
          cvap_nhpi = .data$vap_nhpi - .data$m_nvap_nhpi - .data$f_nvap_nhpi,
          cvap_other = .data$vap_other - .data$m_nvap_other - .data$f_nvap_other,
          cvap_two = .data$vap_two - .data$m_nvap_two - .data$f_nvap_two
        ) %>%
        dplyr::select(-dplyr::starts_with(c('m', 'f')))
    } else {
      out <- out %>%
        dplyr::select(-dplyr::starts_with(c('m', 'f')))
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

  out
}
