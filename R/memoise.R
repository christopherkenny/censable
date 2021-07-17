#' Build Data from the ACS (Memoised)
#' @rdname build_acs
#' @export
mem_build_acs <- memoise::memoise(build_acs)

#' Build Data from the Decennial Census (Memoised)
#' @rdname build_dec
#' @export
mem_build_dec <- memoise::memoise(build_dec)
