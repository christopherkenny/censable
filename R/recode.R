#' Recode Name as Abb
#'
#' Replaces state name with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .name column with state abbreviation
#'
#' @return .data with column .name replaced with abbreviation
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_name_abb(name)
recode_name_abb <- function(.data, name) {
  stata <- get('stata')
  rec <- stata$abb
  names(rec) <- stata$name
  
  .data %>% dplyr::mutate({{.name}} := !!rec)
}


#' Recode Name as FIPS
#'
#' Replaces state name with state fips
#'
#' @param .data data.frame or tibble
#' @param .name column with state fips
#'
#' @return .data with column .name replaced with fips
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_name_fips(name)
recode_name_fips <- function(.data, name) {
  stata <- get('stata')
  rec <- stata$fips
  names(rec) <- stata$name
  
  .data %>% dplyr::mutate({{.name}} := !!rec)
}

#' Recode Name as ANSI
#'
#' Replaces state name with state ansi
#'
#' @param .data data.frame or tibble
#' @param .name column with state ansi
#'
#' @return .data with column .name replaced with ansi
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_name_ansi(name)
recode_name_ansi <- function(.data, name) {
  stata <- get('stata')
  rec <- stata$ansi
  names(rec) <- stata$name
  
  .data %>% dplyr::mutate({{.name}} := !!rec)
}

#' Recode Abb as Name
#'
#' Replaces state abbreviation with state name
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
#'
#' @return .data with column .abb replaced with state name
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_abb_name(abb)
recode_abb_name <- function(.data, .abb) {
  stata <- get('stata')
  rec <- stata$name
  names(rec) <- stata$abb
  
  .data %>% dplyr::mutate({{.abb}} := !!rec)
}


#' Recode Abb as FIPS
#'
#' Replaces state abbreviation with state fips
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
#'
#' @return .data with column .abb replaced with state name
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_abb_fips(abb)
recode_abb_fips <- function(.data, .abb) {
  stata <- get('stata')
  rec <- stata$fips
  names(rec) <- stata$abb
  
  .data %>% dplyr::mutate({{.abb}} := !!rec)
}

#' Recode Abb as ANSI
#'
#' Replaces state abbreviation with state ansi
#'
#' @param .data data.frame or tibble
#' @param .abb column with state abbrevaition
#'
#' @return .data with column .abb replaced with state ansi
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_abb_ansi(abb)
recode_abb_ansi <- function(.data, .abb) {
  stata <- get('stata')
  rec <- stata$ansi
  names(rec) <- stata$abb
  
  .data %>% dplyr::mutate({{.abb}} := !!rec)
}


#' Recode ANSI as abb
#'
#' Replaces state ansi with state abbreviation
#'
#' @param .data data.frame or tibble
#' @param .abb column with state ansi
#'
#' @return .data with column .ansi replaced with state abbreviation
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_ansi_abb(ansi)
recode_ansi_abb <- function(.data, .ansi) {
  stata <- get('stata')
  rec <- stata$abb
  names(rec) <- stata$ansi
  
  .data %>% dplyr::mutate({{.ansi}} := !!rec)
}

#' Recode ANSI as Name
#'
#' Replaces state ansi with state name
#'
#' @param .data data.frame or tibble
#' @param .abb column with state ansi
#'
#' @return .data with column .ansi replaced with state name
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_ansi_name(name)
recode_ansi_name <- function(.data, .ansi) {
  stata <- get('stata')
  rec <- stata$name
  names(rec) <- stata$ansi
  
  .data %>% dplyr::mutate({{.ansi}} := !!rec)
}

#' Recode ANSI as FIPS
#'
#' Replaces state ansi with state fips
#'
#' @param .data data.frame or tibble
#' @param .abb column with state ansi
#'
#' @return .data with column .ansi replaced with state fips
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_ansi_fips(ansi)
recode_ansi_fips <- function(.data, .ansi) {
  stata <- get('stata')
  rec <- stata$fips
  names(rec) <- stata$ansi
  
  .data %>% dplyr::mutate({{.ansi}} := !!rec)
}


#' Recode FIPS as ANSI
#'
#' Replaces state fips with state ansi
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state ansi
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_fips_abb(fips)
recode_fips_ansi <- function(.data, .fips) {
  stata <- get('stata')
  rec <- stata$ansi
  names(rec) <- stata$fips
  
  .data %>% dplyr::mutate({{.fips}} := !!rec)
}

#' Recode FIPS as Abb
#'
#' Replaces state fips with state abb
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state abb
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_fips_abb(fips)
recode_fips_abb <- function(.data, .fips) {
  stata <- get('stata')
  rec <- stata$abb
  names(rec) <- stata$fips
  
  .data %>% dplyr::mutate({{.fips}} := !!rec)
}


#' Recode FIPS as Name
#'
#' Replaces state fips with state name
#'
#' @param .data data.frame or tibble
#' @param .fips column with state fips
#'
#' @return .data with column .fips replaced with state name
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% recode_fips_name(fips)
recode_fips_name <- function(.data, .fips) {
  stata <- get('stata')
  rec <- stata$name
  names(rec) <- stata$fips
  
  .data %>% dplyr::mutate({{.fips}} := !!rec)
}