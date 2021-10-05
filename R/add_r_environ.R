#' Add Entry to Renviron
#'
#' Adds a value to the Renvironment of the form `name=value`. Designed for flexibly
#' adding API keys for future sessions. Defaults are set up for entering a Census API
#' key to work with tidycensus.
#'
#' @param value Character. Value to add.
#' @param name Defaults to CENSUS_API_KEY. Character. Name to give `value`.
#' @param overwrite Defaults to FALSE. Boolean. Should existing item with name `name` in Renviron be overwritten?
#' @param install Defaults to FALSE. Boolean. Should this be added '~/.Renviron' file?
#'
#' @return value, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' add_r_environ('1234', 'SECRET_API_KEY')
#' }
#'
add_r_environ <- function(value, name = 'CENSUS_API_KEY', overwrite = FALSE, install = FALSE) {
  if (missing(value)) {
    stop('Input `value` cannot be missing.')
  }

  value <- list(value)
  names(value) <- name

  if (install) {
    r_env <- file.path(Sys.getenv('HOME'), '.Renviron')

    if (!file.exists(r_env)) {
      file.create(r_env)
    }

    lines <- readLines(r_env)
    newline <- paste0(name, "='", value, "'")


    exists <- stringr::str_detect(lines, paste0(name, '='))

    if (any(exists)) {
      if (sum(exists) > 1) {
        stop('Multiple entries in .Renviron have name matching input `name`.\nEdit manually with `usethis::edit_r_environ()`.')
      }

      if (overwrite) {
        lines[exists] <- newline
        writeLines(lines, r_env)
        do.call(Sys.setenv, value)
      } else {
        message(paste0(name, ' already exists in .Renviron. \nEdit manually with `usethis::edit_r_environ() or set `overwrite = TRUE`.'))
      }
    } else {
      lines[length(lines) + 1] <- newline
      writeLines(lines, r_env)
      do.call(Sys.setenv, value)
    }
  } else {
    do.call(Sys.setenv, value)
  }

  invisible(value)
}
