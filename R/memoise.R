#' Build Data from the ACS (Memoised)
#' @rdname build_acs
#' @export
mem_build_acs <- build_acs

#' Build Data from the Decennial Census (Memoised)
#' @rdname build_dec
#' @export
mem_build_dec <- build_dec


# `<<-` used below to modify the package namespace and resolve
# import issue caught by command check.
# This does not modify the global environment.
# Follows the solution noted in https://github.com/r-lib/memoise/issues/76
.onLoad <- function(libname, pkgname) {
  mem_build_dec <<- memoise::memoise(build_dec)
  mem_build_acs <<- memoise::memoise(build_acs)
}
