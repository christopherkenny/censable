#' Join Abb by Name
#'
#' Adds a column with state abbs joining by a column with state names
#'
#' @param .data data.frame or tibble
#' @param .name column with state abbreviation
#'
#' @return .data with column .name replaced with abbreviation
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% join_abb_name(name)
join_abb_name <- function(.data, .name) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$abb)
  by_char <- 'abb'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join FIPS by Name
#'
#' Replaces state name with state fips
#'
#' @param .data data.frame or tibble
#' @param .name column with state fips
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
  by_char <- 'fips'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join ANSI by Name
#'
#' Replaces state name with state ansi
#'
#' @param .data data.frame or tibble
#' @param .name column with state ansi
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
  by_char <- 'ansi'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join Name by Abb
#'
#' Replaces state abbreviation with state name
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
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
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.abb))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join FIPS by Abb
#'
#' Replaces state abbreviation with state fips
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
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
  by_char <- 'fips'
  names(by_char) <- rlang::as_name(rlang::enquo(.abb))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join ANSI by Abb
#'
#' Replaces state abbreviation with state ansi
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
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
  by_char <- 'ansi'
  names(by_char) <- rlang::as_name(rlang::enquo(.abb))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join Abb by ANSI
#'
#' Replaces state ansi with state abbreviation
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
  by_char <- 'abb'
  names(by_char) <- rlang::as_name(rlang::enquo(.ansi))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join Name by ANSI
#'
#' Replaces state ansi with state name
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
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.ansi))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join FIPS by ANSI
#'
#' Replaces state ansi with state fips
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
  by_char <- 'fips'
  names(by_char) <- rlang::as_name(rlang::enquo(.ansi))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join ANSI by FIPS
#'
#' Replaces state fips with state ansi
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
  by_char <- 'ansi'
  names(by_char) <- rlang::as_name(rlang::enquo(.fips))

  .data %>% dplyr::left_join(stata, by = by_char)
}

#' Join Abb by FIPS
#'
#' Replaces state fips with state abb
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
  by_char <- 'abb'
  names(by_char) <- rlang::as_name(rlang::enquo(.fips))

  .data %>% dplyr::left_join(stata, by = by_char)
}


#' Join Name by FIPS
#'
#' Replaces state fips with state name
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
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.fips))

  .data %>% dplyr::left_join(stata, by = by_char)
}


# #' Join new by old
# #'
# #' Replaces state old with state new
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
#   stata <- stata %>% dplyr::select(.data$new, .data$abb)
#   by_char <- 'new'
#   names(by_char) <- rlang::as_name(rlang::enquo(.old))
#
#   .data %>% dplyr::left_join(stata, by = by_char)
# }
