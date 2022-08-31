test_that("build_dec works without geometry pop 2020", {1
  skip_if_no_token()
  act <- build_dec(geography = 'block', state = 'DE', groups = 'pop', year = 2020,
                   geometry = FALSE)

  expect_s3_class(act, 'data.frame')
})

test_that("build_dec works with geometry pop7 2020", {
  skip_if_no_token()
  act <- build_dec(geography = 'tract', state = 'DE', groups = 'pop7', year = 2020,
                   geometry = TRUE)

  expect_s3_class(act, 'sf')
})


test_that("build_dec works for counties vap 2010", {
  skip_if_no_token()
  act <- build_dec(geography = 'county', state = 'DE', groups = 'vap', year = 2010,
                   geometry = FALSE)

  expect_s3_class(act, 'data.frame')
})
