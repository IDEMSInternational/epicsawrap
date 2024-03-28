library(testthat)

# test definitions
test_that("Expect warning", {
  expect_warning(definitions("zm", "test_1", "no"))
})
