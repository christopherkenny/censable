#' Try to Match to State FIPS
#'
#' Searches for an exact match and offers the best match if no exact match
#'
#' @param state character with state FIPS, Abbreviation, Name, or ANSI
#'
#' @return FIPS code if a match is found or character(0) if no match is found
#' @export
#'
#' @concept match
#' @examples
#' match_fips('NY')
#' match_fips('01')
match_fips <- function(state) {
  stata <- censable::stata
  pos <- tolower(c(stata$fips, stata$abb, stata$name, stata$ansi))
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
#' @param state character with state FIPS, Abbreviation, Name, or ANSI
#'
#' @return Abbreviation if a match is found or character(0) if no match is found
#' @export
#'
#' @concept match
#' @examples
#' match_abb('NY')
#' match_abb('01')
match_abb <- function(state) {
  stata <- censable::stata
  pos <- tolower(c(stata$fips, stata$abb, stata$name, stata$ansi))
  state <- tolower(state)
  matched <- which(state == pos)

  if (length(matched) == 0) {
    matched <- agrep(pattern = state, x = pos)
  }

  matched <- (matched %% nrow(stata))
  matched <- ifelse(matched == 0, 57, matched)
  stata$abb[matched]
}

#' Try to Match to State Name
#'
#' Searches for an exact match and offers the best match if no exact match
#'
#' @param state character with state FIPS, Abbreviation, Name, or ANSI
#'
#' @return Name if a match is found or character(0) if no match is found
#' @export
#'
#' @concept match
#' @examples
#' match_name('NY')
#' match_name('01')
match_name <- function(state) {
  stata <- censable::stata
  pos <- tolower(c(stata$fips, stata$abb, stata$name, stata$ansi))
  state <- tolower(state)
  matched <- which(state == pos)

  if (length(matched) == 0) {
    matched <- agrep(pattern = state, x = pos)
  }

  matched <- (matched %% nrow(stata))
  matched <- ifelse(matched == 0, 57, matched)
  stata$name[matched]
}


#' Try to Match to State ANSI
#'
#' Searches for an exact match and offers the best match if no exact match
#'
#' @param state character with state FIPS, Abbreviation, Name, or ANSI
#'
#' @return ANSI if a match is found or character(0) if no match is found
#' @export
#'
#' @concept match
#' @examples
#' match_ansi('NY')
#' match_ansi('01')
match_ansi <- function(state) {
  stata <- censable::stata
  pos <- tolower(c(stata$fips, stata$abb, stata$name, stata$ansi))
  state <- tolower(state)
  matched <- which(state == pos)

  if (length(matched) == 0) {
    matched <- agrep(pattern = state, x = pos)
  }

  matched <- (matched %% nrow(stata))
  matched <- ifelse(matched == 0, 57, matched)
  stata$ansi[matched]
}
