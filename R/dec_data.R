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
build_dec <- function(geography, state, county, geometry = TRUE, year = 2010){

  if( year %% 10 != 0){
    stop('Decennial data only available for years ending in 0.')
  }

  if(year < 2000) {
    stop('Decennial endpoint for years before 2000 is not available.')
  }

  if(year >= 2010){

    vars <- c(pop      = 'P003001', pop_white = 'P005003', pop_black = 'P005004',
              pop_hisp = 'P004003', pop_aian  = 'P005005', pop_asian = 'P005006',
              pop_nhpi = 'P005007', pop_other = 'P005008', pop_two   = 'P005009',
              vap      = 'P010001', vap_white = 'P011005', vap_black = 'P011006',
              vap_hisp = 'P011002', vap_aian  = 'P011007', vap_asian = 'P011008',
              vap_nhpi = 'P011009', vap_other = 'P011010', vap_two   = 'P011011')

    out <- tidycensus::get_decennial(geography = geography, state = state, year = year,
                         geometry = geometry, keep_geo_vars = FALSE,
                         variables = vars, output = 'wide')

  } else { # 2000
    vars_pop <- c(pop      = 'P004001', pop_white = 'P004005', pop_black = 'P004006',
                  pop_hisp = 'P004002', pop_aian  = 'P004007', pop_asian = 'P004008',
                  pop_nhpi = 'P004009', pop_other = 'P004010', pop_two   = 'P004011')

    vars_vap <- c(vap      = 'P006001', vap_white = 'P006005', vap_black = 'P006006',
                  vap_hisp = 'P006002', vap_aian  = 'P006007', vap_asian = 'P006008',
                  vap_nhpi = 'P006009', vap_other = 'P006010', vap_two   = 'P006011')

    out_pop <- tidycensus::get_decennial(geography = 'block', state = state, year = year,
                             geometry = geometry, keep_geo_vars = FALSE,
                             variables = vars_pop)
    out_vap <- tidycensus::get_decennial(geography = 'block', state = state, year = year,
                             geometry = FALSE, keep_geo_vars = FALSE,
                             variables = vars_vap, output = 'wide')

    out <-out_pop %>% dplyr::left_join(out_vap, by = 'GEOID') %>% sf::st_as_sf()
  }

  out
}
