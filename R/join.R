#' Join Name to Abb
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
  stata <- stata %>% dplyr::select(name, abb)
  by_char <- 'name'
  names(by_char) <- as.character(rlang::as_name(.name))
  
  .data %>% dplyr::left_join(stata, by = by_char)
}
