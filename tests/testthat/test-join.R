test_that('join_abb_name', {
  test <- stata %>% join_abb_name(name)
  expect_equal(stata$abb, test$abb.y)
})
test_that('join_fips_name', {
  test <- stata %>% join_fips_name(name)
  expect_equal(stata$fips, test$fips.y)
})
test_that('join_ansi_name', {
  test <- stata %>% join_ansi_name(name)
  expect_equal(stata$ansi, test$ansi.y)
})


test_that('join_name_abb', {
  test <- stata %>% join_name_abb(abb)
  expect_equal(stata$name, test$name.y)
})
test_that('join_fips_abb', {
  test <- stata %>% join_fips_abb(abb)
  expect_equal(stata$fips, test$fips.y)
})
test_that('join_ansi_abb', {
  test <- stata %>% join_ansi_abb(abb)
  expect_equal(stata$ansi, test$ansi.y)
})


test_that('join_abb_ansi', {
  test <- stata %>% join_abb_ansi(ansi)
  expect_equal(stata$abb, test$abb.y)
})
test_that('join_fips_ansi', {
  test <- stata %>% join_fips_ansi(ansi)
  expect_equal(stata$fips, test$fips.y)
})
test_that('join_name_ansi', {
  test <- stata %>% join_name_ansi(ansi)
  expect_equal(stata$name, test$name.y)
})


test_that('join_abb_fips', {
  test <- stata %>% join_abb_fips(fips)
  expect_equal(stata$abb, test$abb.y)
})
test_that('join_ansi_fips', {
  test <- stata %>% join_ansi_fips(fips)
  expect_equal(stata$ansi, test$ansi.y)
})
test_that('join_name_fips', {
  test <- stata %>% join_name_fips(fips)
  expect_equal(stata$name, test$name.y)
})
