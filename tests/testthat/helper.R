skip_if_no_token <- function() {
  if (has_census_key()) {
    Sys.sleep(3)
  } else {
    skip("No Census Key")
  }
}
