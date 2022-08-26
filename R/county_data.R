match_county <- function(state, counties, decade) {
  st <- get(paste0('fips_', decade), envir = parent.env(parent.frame()))
  st <- st[st$state %in% state, ]

  pos <- tolower(c(st$county, st$name))
  counties <- tolower(counties)

  matched <- pmatch(counties, pos)

  if (length(matched) != length(counties) || any(is.na(matched))) {
    stop(c('{.arg counties} could not be matched to a county for every entry. \
           Please supply one of the FIPS code or full name.'))
  }
  matched <- (matched %% nrow(st))
  matched[matched == 0] <- nrow(st)
  st$county[matched]
}
