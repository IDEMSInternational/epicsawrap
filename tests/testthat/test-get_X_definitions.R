library(testthat)

# Test case 1
gcs_auth_file(file = "testdata/epicsa_token.json")

#zambia_by_year_example <- get_r_instat_definitions(data_book$get_calculations("ZambiaEastern_by_Station_Name_s_year1"))
#saveRDS(zambia_by_year_example, "testdata/zambia_by_year_example.rds")
ghana_by_year_example <- readRDS("testdata/data_by_year_example.rds")
zambia_by_year_example <- readRDS("testdata/zambia_by_year_example.rds")

# Test cases
test_that("get_start_rains_definitions returns correct structure with start_rains", {
  result <- get_start_rains_definitions(ghana_by_year_example$start_rains)
  expect_true("start_rains" %in% names(result))
  expect_true(all(c("start_day", "end_day", "threshold", "total_rainfall", 
                    "over_days", "amount_rain", "proportion", "prob_rain_day", 
                    "dry_spell", "spell_max_dry_days", "spell_interval", 
                    "dry_period", "max_rain", "period_interval", "period_max_dry_days") %in% names(result$start_rains)))
})

test_that("get_start_rains_definitions returns NA for missing variables", {
  result <- get_start_rains_definitions(NULL)
  expect_true("start_rains" %in% names(result))
  expect_true(all(is.na(unlist(result$start_rains))))
})

test_that("get_start_rains_definitions extracts correct values", {
  result <- get_start_rains_definitions(ghana_by_year_example$start_rain)
  expect_equal(result$start_rains$start_day, 1)
  expect_equal(result$start_rains$end_day, 366)
  expect_equal(result$start_rains$threshold, 0.85)
  expect_equal(result$start_rains$total_rainfall, TRUE)
  expect_equal(result$start_rains$amount_rain, 20)
  expect_equal(result$start_rains$over_days, 2)
  expect_equal(result$start_rains$proportion, FALSE)
})

########## end_rains

# Test cases
test_that("get_end_rains_definitions returns correct structure with end_rains", {
  result <- get_end_rains_definitions(ghana_by_year_example$end_rains)
  expect_true("end_rains" %in% names(result))
  expect_true(all(c("start_day", "end_day", "output", "min_rainfall", "interval_length") %in% names(result$end_rains)))
})

test_that("get_end_rains_definitions returns NA for missing variables", {
  result <- get_end_rains_definitions(NULL)
  expect_true("end_rains" %in% names(result))
  expect_true(all(is.na(unlist(result$end_rains))))
})

test_that("get_end_rains_definitions extracts correct values", {
  result <- get_end_rains_definitions(ghana_by_year_example$end_rains)
  expect_equal(result$end_rains$start_day, 1)
  expect_equal(result$end_rains$end_day, 366)
  expect_equal(result$end_rains$output, "both")
  expect_equal(result$end_rains$min_rainfall, 10)
  expect_equal(result$end_rains$interval_length, 1)
})

test_that("get_end_rains_definitions handles different structures correctly", {
  end_rains <- list(
    filter = list(
      "[[1]]" = "(roll_sum_rain > 10) | is.na(x=roll_sum_rain)",
      roll_sum_rain = list(
        ghana = "rainfall",
        "[[2]]" = "RcppRoll::roll_sumr(x=rainfall, n=3, fill=NA, na.rm=FALSE)"
      )
    ),
    filter_2 = "doy >= 1 & doy <= 366"
  )
  result <- get_end_rains_definitions(end_rains)
  expect_equal(result$end_rains$start_day, 1)
  expect_equal(result$end_rains$end_day, 366)
  expect_equal(result$end_rains$output, "both")
  expect_equal(result$end_rains$min_rainfall, 10)
  expect_equal(result$end_rains$interval_length, 3)
})

########## temperature summaries

# Test cases
test_that("get_end_rains_definitions returns correct structure with end_rains", {
  result <- get_temperature_summary_definitions(year = "year",
                                                month = "month",
                                                data_by_year = ghana_by_year_example,
                                                min_tmin_column = "min_min_temperature", mean_tmin_column = "mean_min_temperature",
                                                max_tmin_column = "max_min_temperature", min_tmax_column = "min_max_temperature",
                                                mean_tmax_column = "mean_max_temperature", max_tmax_column = "min_max_temperature")
  expect_true("mean_tmin" %in% names(result))
  expect_true("mean_tmax" %in% names(result))
  expect_true("min_tmin" %in% names(result))
  expect_true("min_tmax" %in% names(result))
  expect_true("max_tmin" %in% names(result))
  expect_true("max_tmax" %in% names(result))
  expect_true(all(c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop") %in% names(result$mean_tmin)))
  expect_true(all(c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop") %in% names(result$mean_tmax)))
  expect_true(all(c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop") %in% names(result$min_tmin)))
  expect_true(all(c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop") %in% names(result$min_tmax)))
  expect_true(all(c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop") %in% names(result$max_tmin)))
  expect_true(all(c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop") %in% names(result$max_tmax)))
  
  expect_equal(result$mean_tmin$to, "annual")
  expect_equal(result$mean_tmin$na_rm, NA)
  expect_equal(result$mean_tmin$na_n, NA)
  expect_equal(result$mean_tmin$na_n_non, NA)
  expect_equal(result$mean_tmin$na_consec, NA)
  expect_equal(result$mean_tmin$na_prop, NA)
})

### season length
test_that("get_season_length_definitions returns correct structure with length", {
  result <- get_season_length_definitions(length = ghana_by_year_example$length)
  expect_true("seasonal_length" %in% names(result))
  expect_true(all(c("end_type") %in% names(result$seasonal_length)))
  expect_equal(result$seasonal_length$end_type, "rains")
})


# end_season ------------------------------------------------------
# Test cases
test_that("get_end_season_definitions returns correct structure with end_season", {
  result <- get_end_season_definitions(zambia_by_year_example$end_season)
  expect_true("end_season" %in% names(result))
  expect_true(all(c("start_day", "end_day", "water_balance_max", "capacity", "evaporation", "evaporation_value") %in% names(result$end_season)))
})

test_that("get_end_season_definitions returns NA for missing variables", {
  result <- get_end_season_definitions(NULL)
  expect_true("end_season" %in% names(result))
  expect_true(all(is.na(unlist(result$end_season))))
})

test_that("get_end_season_definitions extracts correct values", {
  result <- get_end_season_definitions(zambia_by_year_example$end_season)
  expect_equal(result$end_season$start_day, 245)
  expect_equal(result$end_season$end_day, 366)
  expect_equal(result$end_season$water_balance_max, 0.5)
  expect_equal(result$end_season$capacity, 100)
  expect_equal(result$end_season$evaporation, "value")
  expect_equal(result$end_season$evaporation_value, 5)
})

test_that("get_end_rains_definitions throws error for end_season", {
  expect_error(get_end_rains_definitions(zambia_by_year_example$end_season))
})

# TO TEST:
#zambia_by_year_example$sum_rainfall
#zambia_by_year_example$sum_rainday


# TO TEST: 
# get_crop_success_
# get_probability_

# then test build_*
# then test collate
# then test get_r_instat_definitions