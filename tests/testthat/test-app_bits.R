library(testthat)

# Test case 1
gcs_auth_file(file = "testdata/epicsa_token.json")

test_that("Correct summaries are calculated for mw_workshop", {
  expect_no_error(annual_rainfall_summaries("mw_workshops", "Nkhotakota"))
  expect_no_error(annual_rainfall_summaries("mw_workshops", "Kasungu"))
  
  expect_no_error(annual_temperature_summaries("mw_workshops", "Nkhotakota"))
  expect_no_error(annual_temperature_summaries("mw_workshops", "Kasungu"))
  
  expect_no_error(monthly_temperature_summaries("mw_workshops", "Nkhotakota"))
  expect_no_error(monthly_temperature_summaries("mw_workshops", "Kasungu"))
  
  expect_no_error(crop_success_probabilities("mw_workshops", "Nkhotakota"))
  expect_no_error(crop_success_probabilities("mw_workshops", "Kasungu"))
  
  expect_no_error(season_start_probabilities("mw_workshops", "Nkhotakota"))
  expect_no_error(season_start_probabilities("mw_workshops", "Kasungu"))
})

### zm_workshops
# zambia_eastern is giving all of the zambia stuff! Chipata etc etc.
# chipata is giving Nkhotakota
station_metadata("zm_workshop")

test_that("Correct summaries are calculated for zm_workshop", {
  #  expect_no_error(annual_rainfall_summaries("zm_workshops", "Chipata Met"))
  
  #  expect_no_error(annual_temperature_summaries("zm_workshops", "Chipata Met"))
  
  #  expect_no_error(monthly_temperature_summaries("zm_workshops", "Chipata Met"))
  
  #  expect_no_error(crop_success_probabilities("zm_workshops", "Chipata Met"))
  
  #  expect_no_error(season_start_probabilities("zm_workshops", "Chipata Met"))
})

### zm_test
station_metadata("zm_test")

test_that("Correct summaries are calculated for zm_test", {
  # all these stations are actually just the zambia stations from before, from what I can tell.
  expect_no_error(annual_rainfall_summaries("zm_test", "zambia_eastern"))
  expect_no_error(annual_rainfall_summaries("zm_test", "zambia_eastern_4"))
  expect_no_error(annual_rainfall_summaries("zm_test", "zmd_eastern_8"))
  
  expect_no_error(annual_temperature_summaries("zm_test", "zambia_eastern"))
  expect_no_error(annual_temperature_summaries("zm_test", "zambia_eastern_4"))
  expect_no_error(annual_temperature_summaries("zm_test", "zmd_eastern_8"))
  
  expect_no_error(monthly_temperature_summaries("zm_test", "zambia_eastern"))
  expect_no_error(monthly_temperature_summaries("zm_test", "zambia_eastern_4"))
  expect_no_error(monthly_temperature_summaries("zm_test", "zmd_eastern_8"))
  
  expect_no_error(crop_success_probabilities("zm_test", "zambia_eastern"))
  expect_no_error(crop_success_probabilities("zm_test", "zambia_eastern_4"))
  expect_no_error(crop_success_probabilities("zm_test", "zmd_eastern_8"))
  
  expect_no_error(season_start_probabilities("zm_test", "zambia_eastern"))
  expect_no_error(season_start_probabilities("zm_test", "zambia_eastern_4"))
  expect_no_error(season_start_probabilities("zm_test", "zmd_eastern_8"))
})

### ml_test
station_metadata("ml_test")
test_that("Correct summaries are calculated for ml_test", {
  expect_no_error(annual_rainfall_summaries("ml_test", "Kasungu"))
  expect_no_error(annual_temperature_summaries("ml_test", "Kasungu"))
  expect_no_error(monthly_temperature_summaries("ml_test", "Kasungu"))
  expect_no_error(crop_success_probabilities("ml_test", "Kasungu"))
})
season_start_probabilities("ml_test", "Kasungu")
