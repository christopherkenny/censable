#' Collapse Population Race Categories into 4 Categories
#'
#' Collapses Other, AIAN, Asian, NHPI, and Two+ into other.
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
#' mt_county <- mt_county %>% collapse4_pop()
collapse4_pop <- function(.data, prefix = 'pop_') {
  collapse4(.data = .data, prefix = prefix)
}


#' Collapse Voting Age Population Race Categories into 4 Categories
#'
#' Collapses Other, AIAN, Asian, NHPI, and Two+ into other.
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
#' mt_county <- mt_county %>% collapse4_vap()
collapse4_vap <- function(.data, prefix = 'vap_') {
  collapse4(.data = .data, prefix = prefix)
}

#' Collapse Full Race Categories into 4 Categories
#'
#' Collapses Other, AIAN, Asian, NHPI, and Two+ into other, by prefix.
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
#' mt_county <- mt_county %>% collapse4(prefix = c('pop_', 'vap_'))
collapse4 <- function(.data, prefix) {

  if (!inherits(prefix, 'character')){
    stop('`prefix` must be a character vector.')
  }

  if (length(prefix) == 0) {
    stop('`prefix` must be a non-zero length character vector.')
  }

  for (i in seq_along(prefix)) {
    vars <- paste0(prefix[i], c('other', 'aian', 'asian', 'nhpi', 'two'))
    new_var_oth <- vars[1]
    .data <- .data %>%
      dplyr::mutate({{ new_var_oth }} := .data[[vars[[1]]]] + .data[[vars[[2]]]] + .data[[vars[[3]]]] +
                      .data[[vars[[4]]]] + .data[[vars[[5]]]]) %>%
      dplyr::select(!dplyr::all_of(vars[2:5]))
  }
  .data
}



#' Collapse Full Race Categories into 5 Categories
#'
#' Collapses Other, AIAN, NHPI, and Two+ into Other, by prefix.
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
#' mt_county <- mt_county %>% collapse5(prefix = c('pop_', 'vap_'))
collapse5 <- function(.data, prefix) {

  if (!inherits(prefix, 'character')){
    stop('`prefix` must be a character vector.')
  }

  if (length(prefix) == 0) {
    stop('`prefix` must be a non-zero length character vector.')
  }

  for (i in seq_along(prefix)) {
    vars <- paste0(prefix[i], c('other', 'aian', 'nhpi', 'two'))
    new_var_oth <- vars[1]
    .data <- .data %>%
      dplyr::mutate({{ new_var_oth }} := .data[[vars[[1]]]] + .data[[vars[[2]]]] +
                      .data[[vars[[3]]]] + .data[[vars[[4]]]]) %>%
      dplyr::select(!dplyr::all_of(vars[2:4]))
  }
  .data
}

#' Collapse Voting Age Population Race Categories into 5 Categories
#'
#' Collapses Other, AIAN, NHPI, and Two+ into other.
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
#' mt_county <- mt_county %>% collapse5_vap()
collapse5_vap <- function(.data, prefix = 'vap_') {
  collapse5(.data = .data, prefix = prefix)
}


#' Collapse Population Race Categories into 5 Categories
#'
#' Collapses Other, AIAN, NHPI, and Two+ into other.
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
#' mt_county <- mt_county %>% collapse5_pop()
collapse5_pop <- function(.data, prefix = 'pop_') {
  collapse5(.data = .data, prefix = prefix)
}
