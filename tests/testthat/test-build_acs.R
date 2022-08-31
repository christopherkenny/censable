test_that("build_acs works with geometry", {
  skip_if_no_token()
  act <- build_acs(geography = 'tract', state = 'NY', county = '087',
                   geometry = FALSE, year = 2020)

  expect_s3_class(act, 'data.frame')
})

test_that("build_acs works with geometry", {
  skip_if_no_token()
  act <- build_acs(geography = 'tract', state = 'FL', county = '001',
                   geometry = TRUE, year = 2018)

  expect_s3_class(act, 'sf')
})
