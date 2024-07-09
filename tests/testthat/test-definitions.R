library(testthat)

# test definitions
test_that("Expect warning", {
  expect_warning(definitions("internal_tests", "r_data_test_1", summaries = "no"))
})

# names(definitions_id_list) <- station_id