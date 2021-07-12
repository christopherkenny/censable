#' Build Data from the Decennial Census
#'
#' Creates a dataset, using the decennial census information, with the
#' standard variables used for redistricting. Creates a stable base for getting
#' data from tidycensus for common calls in redistricting.
#'
#' @param geography Required. The geography level to use.
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geometry Defaults to TRUE. Whether to return the geometry or not.
#' @param year year, must be 2000, 2010, or 2020 (after August 2021)
#' @param survey whether the get estimates from the 5-year ('acs5'), 3-year ('acs3'),
#' or 1-year ('acs1') survey. Default is 'acs5'.
#'
#' @return tibble with observations for each observation of the geography in the state
#' or county. Data includes 2 sets of columns for each race or ethnicity category:
#' population (pop) and voting age population (vap)
#'
#' @export
#' @concept datatable
#' @examples
#' \dontrun{
#' # uses the Census API
#' tb <- build_dec(geography = 'block', state = 'NY', county = 'Rockland', geometry = TRUE)
#' }
build_acs <- function(geography, state, county, geometry = TRUE, year = 2010, survey = 'acs5'){

  if(year < 2009) {
    stop('ACS endpoint for years before 2009 is not available.')
  }

  # totals + white + black + hisp / for total, vap, and cvap (by sex because acs...)
  vars <- c(pop         = 'B03002_001',
            pop_white   = 'B03002_003',
            pop_black   = 'B03002_004',
            pop_hisp    = 'B03002_012',
            pop_aian    = 'B03002_005',
            pop_asian   = 'B03002_006',
            pop_nhpi    = 'B03002_007',
            pop_other   = 'B03002_008',
            pop_two     = 'B03002_009',
            m_vap       = 'B05003_008',  m_nvap        = 'B05003_012',
            f_vap       = 'B05003_019',  f_nvap        = 'B05003_023',
            m_vap_black = 'B05003B_008', m_nvap_black  = 'B05003B_012',
            f_vap_black = 'B05003B_019', f_nvap_black  = 'B05003B_023',
            m_vap_white = 'B05003H_008', m_nvap_white  = 'B05003H_012',
            f_vap_white = 'B05003H_019', f_nvap_white  = 'B05003H_023',
            m_vap_hisp  = 'B05003I_008', m_nvap_hisp   = 'B05003I_012',
            f_vap_hisp  = 'B05003I_019', f_nvap_hisp   = 'B05003I_023',
            m_vap_aian  = 'B05003C_008', m_nvap_aian   = 'B05003C_012',
            f_vap_aian  = 'B05003C_019', f_nvap_aian   = 'B05003C_023',
            m_vap_asian = 'B05003D_008', m_nvap_asian  = 'B05003D_012',
            f_vap_asian = 'B05003D_019', f_nvap_asian  = 'B05003D_023',
            m_vap_nhpi  = 'B05003E_008', m_nvap_nhpi   = 'B05003E_012',
            f_vap_nhpi  = 'B05003E_019', f_nvap_nhpi   = 'B05003E_023',
            m_vap_other = 'B05003F_008', m_nvap_other  = 'B05003F_012',
            f_vap_other = 'B05003F_019', f_nvap_other  = 'B05003F_023',
            m_vap_two   = 'B05003G_008', m_nvap_two    = 'B05003G_012',
            f_vap_two   = 'B05003G_019', f_nvap_two    = 'B05003G_023'
  )

  out <- tidycensus::get_acs(geography = geography, state = state, year = year,
                                   geometry = geometry, keep_geo_vars = FALSE,
                                   variables = vars, output = 'wide') %>%
    dplyr::select(-dplyr::ends_with('M')) %>%
    dplyr::rename_with(drop_E)

  out %>% dplyr::mutate(
    vap = .data$m_vap + .data$f_vap,
    vap_white = .data$m_vap_white + .data$f_vap_white,
    vap_black = .data$m_vap_black + .data$f_vap_black,
    vap_hisp  = .data$m_vap_hisp  + .data$f_vap_hisp,
    vap_aian  = .data$m_vap_aian  + .data$f_vap_aian,
    vap_asian = .data$m_vap_asian + .data$f_vap_asian,
    vap_nhpi  = .data$m_vap_nhpi  + .data$f_vap_nhpi,
    vap_other = .data$m_vap_other + .data$f_vap_other,
    vap_two   = .data$m_vap_two   + .data$f_vap_two
  ) %>% dplyr::mutate(
    cvap = .data$vap - .data$m_nvap - .data$f_nvap,
    cvap_white = .data$vap_white - .data$m_nvap_white - .data$f_nvap_white,
    cvap_black = .data$vap_black - .data$m_nvap_black - .data$f_nvap_black,
    cvap_hisp  = .data$vap_hisp  - .data$m_nvap_hisp  - .data$f_nvap_hisp,
    cvap_aian  = .data$vap_aian  - .data$m_nvap_aian  - .data$f_nvap_aian,
    cvap_asian = .data$vap_asian - .data$m_nvap_asian - .data$f_nvap_asian,
    cvap_nhpi  = .data$vap_nhpi  - .data$m_nvap_nhpi  - .data$f_nvap_nhpi,
    cvap_other = .data$vap_other - .data$m_nvap_other - .data$f_nvap_other,
    cvap_two   = .data$vap_two   - .data$m_nvap_two   - .data$f_nvap_two
  ) %>% dplyr::select(-dplyr::starts_with(c('m','f'))
  )

}
