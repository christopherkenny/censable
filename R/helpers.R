#' drop E from name
#' @keywords internal
#' @noRd
drop_E <- function(x) {
  ends_in_E <- stringr::str_sub(x, -1L) == 'E' & x != 'NAME'

  dplyr::if_else(ends_in_E, stringr::str_sub(x, 1L, -2L), x)
}

#' Clean Estimate Names
#' @keywords internal
#' @noRd
clean_name_suffix <- function(.data) {
  .data %>% dplyr::rename_with(.fn = drop_E)
}

# # Get Character of geography that needs counties
# # @keywords internal
# get_by_county <- function() {
#   c('')
# }
#
# # Loop over Counties Decennial
# # @keywords internal
# loop_dec_counties <- function(.geography, .state, .year, .geometry, .vars) {
#
#   fips <- get(paste0('fips_', .year))
#   counties <- fips %>%
#     dplyr::filter(.data$state == match_fips(.state)) %>%
#     dplyr::pull(.data$county)
#
#   if (length(counties) == 0) {
#     stop('Entry `state` could not be matched to a valid FIPS code. See `match_fips()` for matching information.')
#   }
#
#   out <- lapply(seq_along(counties), function(ct, state = .state, vars = .vars, year = .year,
#                                               geography = .geography, geometry = .geometry){
#     cat('geography:', geography, '\n')
#     cat('state:', state, '\n')
#     cat('year:', year, '\n')
#     cat('geometry:', geometry, '\n')
#     cat('vars:', vars, '\n')
#     cat('ct:', counties[ct], '\n')
#     Sys.sleep(2)
#
#     x <- NULL
#     try({x <- tidycensus::get_decennial(
#       geography = geography, state = state,
#       year = year, county = counties[ct],
#       geometry = geometry, variables = vars,
#       keep_geo_vars = FALSE, output = 'wide'
#     )})
#     return(x)
#   })
#
#   do.call('rbind', out)
# }
#
#
# # Loop over Counties ACS
# # @keywords internal
# loop_acs_counties <- function(.geography, .state, .year, .geometry, .vars, .survey) {
#
#   .year <- .year - (.year %% 10)
#   fips <- get(paste0('fips_', .year))
#   counties <- fips %>%
#     dplyr::filter(.data$state == match_fips(.state)) %>%
#     dplyr::pull(.data$county)
#
#   if (length(counties) == 0) {
#     stop('Entry `state` could not be matched to a valid FIPS code. See `match_fips()` for matching information.')
#   }
#
#   out <- lapply(counties, function(ct){
#     x <- NULL
#     try({x <- tidycensus::get_acs(
#       geography = .geography, state = .state,
#       year = .year, county = ct,
#       geometry = .geometry, variables = .vars,
#       keep_geo_vars = FALSE, output = 'wide', survey = .survey
#     )})
#     return(x)
#   })
#
#   out <- do.call('rbind', out)
#   out
# }
