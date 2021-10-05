test_that('breakdown_geoid', {
  ex <- mt_county %>% breakdown_geoid()
  expect_true(all(c('state', 'county') %in% names(ex)))
})


test_that('construct_geoid', {
  ex <- mt_county %>%
    breakdown_geoid() %>%
    construct_geoid()
  expect_true(all(c('state', 'county') %in% names(ex)))
  expect_identical(ex$GEOID, mt_county$GEOID)
})
