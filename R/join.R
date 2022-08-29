#' Join Abb by Name
#'
#' Adds a column with state abbs joining by a column with state names
#'
#' @param .data data.frame or tibble
#' @param .name column with state name
#'
#' @return .data with column .name replaced with abbreviation
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_abb_name(name)
join_abb_name <- function(.data, .name) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$abb)
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join FIPS by Name
#'
#' Adds a column with state fips joining by a column with state name
#'
#' @param .data data.frame or tibble
#' @param .name column with state name
#'
#' @return .data with column .name replaced with fips
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_fips_name(name)
join_fips_name <- function(.data, .name) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$fips)
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join ANSI by Name
#'
#' Adds a column with state ansi joining by a column with state name
#'
#' @param .data data.frame or tibble
#' @param .name column with state name
#'
#' @return .data with column .name replaced with ansi
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_ansi_name(name)
join_ansi_name <- function(.data, .name) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$ansi)
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join Name by Abb
#'
#' Adds a column with state name joining by a column with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbreviation
#'
#' @return .data with column .abb replaced with state name
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_name_abb(abb)
join_name_abb <- function(.data, .abb) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$abb)
  by_char <- 'abb'
  names(by_char) <- rlang::as_name(rlang::enquo(.abb))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join FIPS by Abb
#'
#' Adds a column with state fips joining by a column with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbreviation
#'
#' @return .data with column .abb replaced with state name
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_fips_abb(abb)
join_fips_abb <- function(.data, .abb) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$fips, .data$abb)
  by_char <- 'abb'
  names(by_char) <- rlang::as_name(rlang::enquo(.abb))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join ANSI by Abb
#'
#' Adds a column with state ansi joining by a column with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbreviation
#'
#' @return .data with column .abb replaced with state ansi
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_ansi_abb(abb)
join_ansi_abb <- function(.data, .abb) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$ansi, .data$abb)
  by_char <- 'abb'
  names(by_char) <- rlang::as_name(rlang::enquo(.abb))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join Abb by ANSI
#'
#' Adds a column with state abbreviation joining by a column with state ansi
#'
#' @param .data data.frame or tibble
#' @param .ansi column with state ansi
#'
#' @return .data with column .ansi replaced with state abbreviation
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_abb_ansi(ansi)
join_abb_ansi <- function(.data, .ansi) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$ansi, .data$abb)
  by_char <- 'ansi'
  names(by_char) <- rlang::as_name(rlang::enquo(.ansi))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join Name by ANSI
#'
#' Adds a column with state name joining by a column with state ansi
#'
#' @param .data data.frame or tibble
#' @param .ansi column with state ansi
#'
#' @return .data with column .ansi replaced with state name
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_name_ansi(name)
join_name_ansi <- function(.data, .ansi) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$ansi)
  by_char <- 'ansi'
  names(by_char) <- rlang::as_name(rlang::enquo(.ansi))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join FIPS by ANSI
#'
#' Adds a column with state fips joining by a column with state ansi
#'
#' @param .data data.frame or tibble
#' @param .ansi column with state ansi
#'
#' @return .data with column .ansi replaced with state fips
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_fips_ansi(ansi)
join_fips_ansi <- function(.data, .ansi) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$fips, .data$ansi)
  by_char <- 'ansi'
  names(by_char) <- rlang::as_name(rlang::enquo(.ansi))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join ANSI by FIPS
#'
#' Adds a column with state ansi joining by a column with state fips
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state ansi
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_ansi_fips(fips)
join_ansi_fips <- function(.data, .fips) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$fips, .data$ansi)
  by_char <- 'fips'
  names(by_char) <- rlang::as_name(rlang::enquo(.fips))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join Abb by FIPS
#'
#' Adds a column with state abbreviation joining by a column with state fips
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state abb
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_abb_fips(fips)
join_abb_fips <- function(.data, .fips) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$fips, .data$abb)
  by_char <- 'fips'
  names(by_char) <- rlang::as_name(rlang::enquo(.fips))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join Name by FIPS
#'
#' Adds a column with state name joining by a column with state fips
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state name
#' @export
#'
#' @concept join
#' @examples
#' data('stata')
#' stata %>% join_name_fips(fips)
join_name_fips <- function(.data, .fips) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$fips)
  by_char <- 'fips'
  names(by_char) <- rlang::as_name(rlang::enquo(.fips))

  .data %>% dplyr::left_join(stata, by = by_char)
}


# #' Join new by old
# #'
# #' Adds a column with state new joining by a column with state old
# #'
# #' @param .data data.frame or tibble
# #' @param .old column with state old
# #'
# #' @return .data with column .old replaced with state new
# #' @export
# #'
# #' @concept join
# #' @examples
# #' data('stata')
# #' stata %>% join_new_old(old)
# join_new_old <- function(.data, .old) {
#   stata <- get('stata')
#   stata <- stata %>% dplyr::select(.data$new, .data$old)
#   by_char <- 'old'
#   names(by_char) <- rlang::as_name(rlang::enquo(.old))
#
#   .data %>% dplyr::left_join(stata, by = by_char)
# }
