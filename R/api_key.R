#' Check or Get Census API Key
#'
#' @return logical if `has`, key if `get`
#' @export
#'
#' @name key
#'
#' @examples
#' has_census_key()
has_census_key <- function() {
  key <- Sys.getenv('CENSUS_API_KEY')
  if (key == '') {
    key <- Sys.getenv('CENSUS_KEY')
  }
  if (key == '') {
    FALSE
  } else {
    TRUE
  }
}

#' @param key Census API Key as a character
#' @rdname key
#' @export
get_census_key <- function(key = '') {
  if (key == '') {
    key <- Sys.getenv('CENSUS_API_KEY')
  }
  if (key == '') {
    key <- Sys.getenv('CENSUS_KEY')
  }
  if (key == '') {
    stop('Must either supply a key or have one set as `CENSUS_API_KEY` or `CENSUS_KEY`.')
  }
  key
}
