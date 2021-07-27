test_that("match_fips", {
  a <- match_fips('NY')
  b <- match_fips('36')
  c <- match_fips('New York')
  d <- match_fips('1779796')

  expect_equal(a, '36')
  expect_equal(b, '36')
  expect_equal(c, '36')
  expect_equal(d, '36')
})

test_that("match_abb", {
  a <- match_abb('NY')
  b <- match_abb('36')
  c <- match_abb('New York')
  d <- match_abb('1779796')

  expect_equal(a, 'NY')
  expect_equal(b, 'NY')
  expect_equal(c, 'NY')
  expect_equal(d, 'NY')
})

test_that("match_name", {
  a <- match_name('NY')
  b <- match_name('36')
  c <- match_name('New York')
  d <- match_name('1779796')

  expect_equal(a, 'New York')
  expect_equal(b, 'New York')
  expect_equal(c, 'New York')
  expect_equal(d, 'New York')
})

test_that("match_ansi", {
  a <- match_ansi('NY')
  b <- match_ansi('36')
  c <- match_ansi('New York')
  d <- match_ansi('1779796')

  expect_equal(a, '1779796')
  expect_equal(b, '1779796')
  expect_equal(c, '1779796')
  expect_equal(d, '1779796')
})
