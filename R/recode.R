#' Recode Abb by Name
#'
#' Replaces state name with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .name column with state name
#'
#' @return .data with column .name replaced with abbreviation
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_abb_name(name)
recode_abb_name <- function(.data, .name) {
  stata <- get('stata')
  rec <- stata$abb
  names(rec) <- stata$name

  .data %>% dplyr::mutate({{ .name }} := unname(!!rec))
}


#' Recode FIPS by Name
#'
#' Replaces state name with state fips
#'
#' @param .data data.frame or tibble
#' @param .name column with state name
#'
#' @return .data with column .name replaced with fips
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_fips_name(name)
recode_fips_name <- function(.data, .name) {
  stata <- get('stata')
  rec <- stata$fips
  names(rec) <- stata$name

  .data %>% dplyr::mutate({{ .name }} := unname(!!rec))
}

#' Recode ANSI by Name
#'
#' Replaces state name with state ansi
#'
#' @param .data data.frame or tibble
#' @param .name column with state name
#'
#' @return .data with column .name replaced with ansi
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_ansi_name(name)
recode_ansi_name <- function(.data, .name) {
  stata <- get('stata')
  rec <- stata$ansi
  names(rec) <- stata$name

  .data %>% dplyr::mutate({{ .name }} := unname(!!rec))
}

#' Recode Name by Abb
#'
#' Replaces state abbreviation with state name
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
#'
#' @return .data with column .abb replaced with state name
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_name_abb(abb)
recode_name_abb <- function(.data, .abb) {
  stata <- get('stata')
  rec <- stata$name
  names(rec) <- stata$abb

  .data %>% dplyr::mutate({{ .abb }} := unname(!!rec))
}


#' Recode FIPS by Abb
#'
#' Replaces state abbreviation with state fips
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
#'
#' @return .data with column .abb replaced with state name
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_fips_abb(abb)
recode_fips_abb <- function(.data, .abb) {
  stata <- get('stata')
  rec <- stata$fips
  names(rec) <- stata$abb

  .data %>% dplyr::mutate({{ .abb }} := unname(!!rec))
}

#' Recode ANSI by Abb
#'
#' Replaces state abbreviation with state ansi
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
#'
#' @return .data with column .abb replaced with state ansi
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_ansi_abb(abb)
recode_ansi_abb <- function(.data, .abb) {
  stata <- get('stata')
  rec <- stata$ansi
  names(rec) <- stata$abb

  .data %>% dplyr::mutate({{ .abb }} := unname(!!rec))
}


#' Recode Abb by ANSI
#'
#' Replaces state ansi with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .ansi column with state ansi
#'
#' @return .data with column .ansi replaced with state abbreviation
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_abb_ansi(ansi)
recode_abb_ansi <- function(.data, .ansi) {
  stata <- get('stata')
  rec <- stata$abb
  names(rec) <- stata$ansi

  .data %>% dplyr::mutate({{ .ansi }} := unname(!!rec))
}

#' Recode Name by ANSI
#'
#' Replaces state ansi with state name
#'
#' @param .data data.frame or tibble
#' @param .ansi column with state ansi
#'
#' @return .data with column .ansi replaced with state name
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_name_ansi(name)
recode_name_ansi <- function(.data, .ansi) {
  stata <- get('stata')
  rec <- stata$name
  names(rec) <- stata$ansi

  .data %>% dplyr::mutate({{ .ansi }} := unname(!!rec))
}

#' Recode FIPS by ANSI
#'
#' Replaces state ansi with state fips
#'
#' @param .data data.frame or tibble
#' @param .ansi column with state ansi
#'
#' @return .data with column .ansi replaced with state fips
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_fips_ansi(ansi)
recode_fips_ansi <- function(.data, .ansi) {
  stata <- get('stata')
  rec <- stata$fips
  names(rec) <- stata$ansi

  .data %>% dplyr::mutate({{ .ansi }} := unname(!!rec))
}


#' Recode ANSI by FIPS
#'
#' Replaces state fips with state ansi
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state ansi
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_ansi_fips(fips)
recode_ansi_fips <- function(.data, .fips) {
  stata <- get('stata')
  rec <- stata$ansi
  names(rec) <- stata$fips

  .data %>% dplyr::mutate({{ .fips }} := unname(!!rec))
}

#' Recode Abb by FIPS
#'
#' Replaces state fips with state abb
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state abb
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_abb_fips(fips)
recode_abb_fips <- function(.data, .fips) {
  stata <- get('stata')
  rec <- stata$abb
  names(rec) <- stata$fips

  .data %>% dplyr::mutate({{ .fips }} := unname(!!rec))
}


#' Recode Name by FIPS
#'
#' Replaces state fips with state name
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state name
#' @export
#'
#' @concept recode
#' @examples
#' data('stata')
#' stata %>% recode_name_fips(fips)
recode_name_fips <- function(.data, .fips) {
  stata <- get('stata')
  rec <- stata$name
  names(rec) <- stata$fips

  .data %>% dplyr::mutate({{ .fips }} := unname(!!rec))
}

# #' Recode new by old
# #'
# #' Replaces state old with state new
# #'
# #' @param .data data.frame or tibble
# #' @param .old column with state old
# #'
# #' @return .data with column .old replaced with state new
# #' @export
# #'
# #' @concept recode
# #' @examples
# #' data('stata')
# #' stata %>% recode_new_old(old)
# recode_new_old <- function(.data, .old) {
#   stata <- get('stata')
#   rec <- stata$new
#   names(rec) <- stata$old
#
#   .data %>% dplyr::mutate({{.old}} := unname(!!rec))
# }
