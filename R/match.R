#' Try to Match to State FIPS
#'
#' Searches for an exact match and offers the best match if no exact match
#'
#' @param state character with state FIPS, Abbreviation, or Name
#'
#' @return FIPS code if a match is found or character(0) if no match is found
#' @export
#'
#' @examples
#' match_fips('NY')
match_fips <- function(state) {
  stata <- censable::stata
  pos <- tolower(c(stata$fips, stata$abb, stata$name))
  state <- tolower(state)
  matched <- which(state == pos)

  if (length(matched) == 0) {
    matched <- agrep(pattern = state, x = pos)
  }

  matched <- (matched %% nrow(stata))
  matched <- ifelse(matched == 0, 57, matched)
  stata$fips[matched]
}

#' Try to Match to State Abbreviation
#'
#' Searches for an exact match and offers the best match if no exact match
#'
#' @param state character with state FIPS, Abbreviation, or Name
#'
#' @return Abbreviation if a match is found or character(0) if no match is found
#' @export
#'
#' @examples
#' match_abb('NY')
#' match_abb('01')
match_abb <- function(state) {
  stata <- censable::stata
  pos <- tolower(c(stata$fips, stata$abb, stata$name))
  state <- tolower(state)
  matched <- which(state == pos)

  if (length(matched) == 0) {
    matched <- agrep(pattern = state, x = pos)
  }

  matched <- (matched %% nrow(stata))
  matched <- ifelse(matched == 0, 57, matched)
  stata$abb[matched]
}
