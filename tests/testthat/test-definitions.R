library(testthat)
gcs_auth_file(file = "testdata/epicsa_token.json")

# test definitions
test_that("Expect warning", {
  expect_warning(definitions("internal_tests", "r_data_test_1", summaries = "no"))
})

# names(definitions_id_list) <- station_id