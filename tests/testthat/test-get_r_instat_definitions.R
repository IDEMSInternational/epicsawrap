# test-get_r_instat_definitions.R

library(testthat)

# Define a mock instat_calculation class for testing
instat_calculation <- function(type, function_exp = "", calculated_from = list(), manipulations = list(), result_name = "", sub_calculations = list()) {
  list(
    type = type,
    function_exp = function_exp,
    calculated_from = calculated_from,
    manipulations = manipulations,
    result_name = result_name,
    sub_calculations = sub_calculations
  )
}

# Sample calculations for testing
calc1 <- instat_calculation("summary", "summary_sum(x = rainday, na.rm = TRUE)", list(), list(), "sum_rainday")
calc2 <- instat_calculation("summary", "summary_sum(x = rainfall, na.rm = TRUE)", list(), list(), "sum_rainfall")
calc8 <- instat_calculation("calculation", "end_season - start_rain", list(), list(), "length")
calc9 <- instat_calculation("summary", "summary_mean(x = tmin, na.rm = TRUE)", list(), list(), "mean_tmin")
calc_by <- instat_calculation("by", calculated_from = list("station"))
calc_filter <- instat_calculation("filter", "x > 10", sub_calculations = list(calc1))
calc_filter_2 <- instat_calculation("filter", "x > 10")

# Test cases
test_that("get_r_instat_definitions handles summary type calculations", {
  calculations <- list(calc1, calc2)
  result <- get_r_instat_definitions(calculations)
  expect_true("sum_rainday" %in% names(result))
  expect_true("sum_rainfall" %in% names(result))
  expect_equal(result$sum_rainday$function_exp, "summary_sum(x = rainday, na.rm = TRUE)")
  expect_equal(result$sum_rainfall$function_exp, "summary_sum(x = rainfall, na.rm = TRUE)")
})

test_that("get_r_instat_definitions handles calculation type calculations", {
  calculations <- list(calc8)
  result <- get_r_instat_definitions(calculations)
  expect_true("length" %in% names(result))
  expect_equal(result$length, "end_season - start_rain")
})

test_that("get_r_instat_definitions handles nested calculations", {
  nested_calc <- instat_calculation("summary", "summary_sum(x = rainfall, na.rm = TRUE)", list(), list(calc1, calc2), "sum_rainfall_nested")
  calculations <- list(nested_calc)
  result <- get_r_instat_definitions(calculations)
  expect_true("sum_rainfall_nested" %in% names(result))
  expect_true("sum_rainday" %in% names(result$sum_rainfall_nested))
  expect_true("sum_rainfall" %in% names(result$sum_rainfall_nested))
})

test_that("get_r_instat_definitions handles empty calculations list", {
  calculations <- list()
  result <- get_r_instat_definitions(calculations)
  expect_equal(result, NULL)
})

test_that("get_r_instat_definitions handles complex calculation types", {
  complex_calc <- instat_calculation("summary", "summary_mean(x = tmin, na.rm = TRUE)", list(), list(calc8), "mean_tmin")
  calculations <- list(complex_calc)
  result <- get_r_instat_definitions(calculations)
  expect_true("mean_tmin" %in% names(result))
  expect_true("length" %in% names(result$mean_tmin))
})

test_that("get_r_instat_definitions handles 'by' type calculations", {
  calculations <- list(calc_by)
  result <- get_r_instat_definitions(calculations)
  expect_true("by_1" %in% names(result))
  expect_equal(result$by_1, list("station"))
})

test_that("get_r_instat_definitions handles 'filter' type calculations with sub_calculations", {
  calculations <- list(calc_filter)
  result <- get_r_instat_definitions(calculations)
  expect_true("filter" %in% names(result))
  expect_equal(result$filter[[1]], "x > 10")
  expect_equal(result$filter[[2]]$function_exp, "summary_sum(x = rainday, na.rm = TRUE)")
})

test_that("get_r_instat_definitions handles 'filter' type calculations without sub_calculations", {
  calculations <- list(calc_filter_2)
  result <- get_r_instat_definitions(calculations)
  expect_true("filter_2" %in% names(result))
  expect_equal(result$filter_2, "x > 10")
})

