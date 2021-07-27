test_that("collapse4 works", {
  coll <- mt_county %>% collapse4(prefix = c('pop_', 'vap_'))

  expect_s3_class(coll, 'sf')
  expect_equal(ncol(coll), 13)
})

test_that("collapse4_pop", {
  coll <- mt_county %>% collapse4_pop()

  expect_s3_class(coll, 'sf')
  expect_equal(ncol(coll), 17)
})

test_that("collapse4_vap", {
  coll <- mt_county %>% collapse4_vap()

  expect_s3_class(coll, 'sf')
  expect_equal(ncol(coll), 17)
})

test_that("collapse5 works", {
  coll <- mt_county %>% collapse5(prefix = c('pop_', 'vap_'))

  expect_s3_class(coll, 'sf')
  expect_equal(ncol(coll), 15)
})

test_that("collapse5_pop", {
  coll <- mt_county %>% collapse5_pop()

  expect_s3_class(coll, 'sf')
  expect_equal(ncol(coll), 18)
})

test_that("collapse5_vap", {
  coll <- mt_county %>% collapse5_vap()

  expect_s3_class(coll, 'sf')
  expect_equal(ncol(coll), 18)
})
