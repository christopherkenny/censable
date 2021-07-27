test_that('recode_abb_name', {
  test <- stata %>% recode_abb_name(name)
  expect_equal(test$abb, test$name)
})
test_that('recode_fips_name', {
  test <- stata %>% recode_fips_name(name)
  expect_equal(test$fips, test$name)
})
test_that('recode_ansi_name', {
  test <- stata %>% recode_ansi_name(name)
  expect_equal(test$ansi, test$name)
})


test_that('recode_name_abb', {
  test <- stata %>% recode_name_abb(abb)
  expect_equal(test$name, test$abb)
})
test_that('recode_fips_abb', {
  test <- stata %>% recode_fips_abb(abb)
  expect_equal(test$fips, test$abb)
})
test_that('recode_ansi_abb', {
  test <- stata %>% recode_ansi_abb(abb)
  expect_equal(test$ansi, test$abb)
})


test_that('recode_abb_ansi', {
  test <- stata %>% recode_abb_ansi(ansi)
  expect_equal(test$abb, test$ansi)
})
test_that('recode_fips_ansi', {
  test <- stata %>% recode_fips_ansi(ansi)
  expect_equal(test$fips, test$ansi)
})
test_that('recode_name_ansi', {
  test <- stata %>% recode_name_ansi(ansi)
  expect_equal(test$name, test$ansi)
})

test_that('recode_abb_fips', {
  test <- stata %>% recode_abb_fips(fips)
  expect_equal(test$abb, test$fips)
})
test_that('recode_ansi_fips', {
  test <- stata %>% recode_ansi_fips(fips)
  expect_equal(test$ansi, test$fips)
})
test_that('recode_name_fips', {
  test <- stata %>% recode_name_fips(fips)
  expect_equal(test$name, test$fips)
})
