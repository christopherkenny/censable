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
