#' Collapse Population Race Categories into 4 Categories
#'
#' Collapses AIAN, Asian, NHPI, and Two+ into other.
#'
#' @param .data tibble, data.frame, or sf tibble
#' @param prefix Default is `pop_`. The prefix for the race categories.
#'
#' @return .data with columns collapsed
#' @export
#'
#' @concept collapse
#' @examples
#' data(mt_county)
#' mt_county <- mt_county %>% collapse_pop()
collapse_pop <- function(.data, prefix = 'pop_') {
  vars <- paste0(prefix, c('other', 'aian', 'asian', 'nhpi', 'two'))
  stopifnot(all(vars %in% names(.data)))
  new_var_oth <- vars[1]
  .data %>%
    dplyr::mutate({{ new_var_oth }}:= .data[[vars[1]]] + .data[[vars[2]]] + .data[[vars[3]]] +
                    .data[[vars[4]]] + .data[[vars[5]]]) %>%
    dplyr::select(!dplyr::all_of(vars[2:5]))
}


#' Collapse Voting Age Population Race Categories into 4 Categories
#'
#' Collapses AIAN, Asian, NHPI, and Two+ into other.
#'
#' @param .data tibble, data.frame, or sf tibble
#' @param prefix Default is `vap_`. The prefix for the race categories.
#'
#' @return .data with columns collapsed
#' @export
#'
#' @concept collapse
#' @examples
#' data(mt_county)
#' mt_county <- mt_county %>% collapse_vap()
collapse_vap <- function(.data, prefix = 'vap_') {
  collapse_pop(.data = .data, prefix = prefix)
}

#' Collapse Full Race Categories into 4 Categories
#'
#' Collapses AIAN, Asian, NHPI, and Two+ into other, by prefix.
#'
#' @param .data tibble, data.frame, or sf tibble
#' @param prefix The prefix(es) for the race categories. Must be a character vector.
#'
#' @return .data with columns collapsed
#' @export
#'
#' @concept collapse
#' @examples
#' data(mt_county)
#' mt_county <- mt_county %>% collapse(prefix = c('pop_', 'vap_'))
collapse <- function(.data, prefix) {

  if (!inherits(prefix, 'character')){
    stop('`prefix` must be a character vector.')
  }

  if (length(prefix) == 0) {
    stop('`prefix` must be a non-zero length character vector.')
  }

  lapply(seq_along(prefix), function(i) {
    vars <- paste0(prefix[i], c('other', 'aian', 'asian', 'nhpi', 'two'))
    new_var_oth <- vars[1]
    .data <- .data %>%
      dplyr::mutate({{ new_var_oth }} := .data[[vars[[1]]]] + .data[[vars[[2]]]] + .data[[vars[[3]]]] +
                      .data[[vars[[4]]]] + .data[[vars[[5]]]]) %>%
      dplyr::select(!dplyr::all_of(vars[2:5]))
  })
  .data
}
