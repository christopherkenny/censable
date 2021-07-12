#' Join Abb to Name
#'
#' Adds a column with state abbs joining to a column with state names
#'
#' @param .data data.frame or tibble
#' @param .name column with state abbreviation
#'
#' @return .data with column .name replaced with abbreviation
#' @export
#'
#' @examples
#' data('stata')
#' stata %>% join_name_abb(name)
join_name_abb <- function(.data, .name) {
  stata <- get('stata')
  stata <- stata %>% dplyr::select(.data$name, .data$abb)
  by_char <- 'name'
  names(by_char) <- rlang::as_name(rlang::enquo(.name))

  .data %>% dplyr::left_join(stata, by = by_char)
}


