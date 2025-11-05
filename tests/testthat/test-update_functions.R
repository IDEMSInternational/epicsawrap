library(databook)
library(rpicsa)
expect_has_cols <- function(df, cols) {
  expect_true(all(cols %in% names(df)),
              info = paste("Missing columns:", paste(setdiff(cols, names(df)), collapse = ", ")))
}

# test-epicsa-workflows.R
# ---- START RAINS -----------------------------------------------------------
test_that("update_start_rains populates start_rain outputs", {

  # 1. Let's set up our data book
  data_book <- DataBook$new()

  # 2. Importing in some data for testing (this is stored in the rpicsa package)
  data(daily_niger)
  daily_niger$var <- 5
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))

  # 3. Get the definitinos data
  defs <- jsonlite::read_json("testdata/test_json_2.json")

  # 4. Apply
  data_names <- list(date = "date",
                     rain = "rain",
                     year = "year",
                     doy = "doy",
                     station = "station_name",
                     evaporation_variable = "var")
  
  suppressWarnings(update_start_rains(data_frame = "daily_niger",
                     data_names = data_names,
                     definitions = defs,
                     data_book = data_book))

  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_has_cols(by, c("start_rain", "start_rain_date", "start_rain_status"))
})

# ----------------------------------------------------------------------
test_that("update_end_rains populates end_rains outputs", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  
  data_names <- list(
    date = "date",
    station = "station_name",
    year = "year",
    rain = "rain",
    doy = "doy"
  )
  
  suppressWarnings(
    update_end_rains("daily_niger", data_names, defs, data_book)
  )
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_has_cols(by, c("end_rains", "end_rains_date", "end_rains_status"))
})

# ----------------------------------------------------------------------
test_that("update_seasonal_rain creates seasonal totals and counts", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  # enable both totals & rainy-day counts in defs
  defs$annual_summaries$seasonal_rain$total_rain <- TRUE
  defs$annual_summaries$seasonal_rain$n_rain <- TRUE
  
  data_names <- list(
    date = "date",
    station = "station_name",
    year = "year",
    rain = "rain",
    doy = "doy"
  )
  
  summary_data_names <- list(start_date = NULL, end_date = NULL)
  
  suppressWarnings(
    update_seasonal_rain(
      data_frame = "daily_niger",
      data_names = data_names,
      NULL,
      summary_data_names,
      definitions = defs,
      data_book = data_book
    )
  )
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_has_cols(by, c("sum_rain"))
})

# ----------------------------------------------------------------------
test_that("seasonal_rain works on filtered slice", {
  data_book <- DataBook$new()
  dd <- rpicsa::daily_niger |>
    dplyr::filter(station_name == "Agades", year > 1945, year <= 1950) |>
    dplyr::mutate(year = as.numeric(year))
  data_book$import_data(list(daily_data = dd))
  
  suppressWarnings(
    seasonal_rain(
      summary_data = NULL,
      start_date   = 193,
      end_date     = 292,
      data         = "daily_data",
      date_time    = "date",
      year         = "year",
      doy          = "doy",
      station      = "station_name",
      rain         = "rain",
      data_book    = data_book
    )
  )
  
  by <- data_book$get_data_frame("daily_data_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_has_cols(by, c("sum_rain"))
})

# ----------------------------------------------------------------------
test_that("update_annual_temperature creates mean/min/max for tmin and tmax", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  
  data_names <- list(
    date = "date",
    tmin = "tmin",
    tmax = "tmax",
    year = "year",
    station = "station_name"
  )
  
  suppressWarnings(
    update_annual_temperature("daily_niger", data_names, defs, data_book)
  )
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_has_cols(by, c("mean_tmin","min_tmin","max_tmin","mean_tmax","min_tmax","max_tmax"))
})

# ----------------------------------------------------------------------
test_that("update_monthly_temperature creates year-month aggregates", {
  data_book <- DataBook$new()
  data(daily_niger)
  data_book$import_data(list(daily_niger = daily_niger))
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  # match your inst-script tweak: remove 'to' in monthly definitions
  for (k in c("max_tmin","min_tmin","mean_tmin","max_tmax","min_tmax","mean_tmax")) {
    if (!is.null(defs$monthly_temperature_summaries[[k]])) {
      defs$monthly_temperature_summaries[[k]]$to <- NULL
    }
  }
  
  data_names <- list(
    date = "date",
    tmin = "tmin",
    tmax = "tmax",
    year = "year",
    month = "month",
    station = "station_name"
  )
  
  suppressWarnings(
    update_monthly_temperature("daily_niger", data_names, defs, data_book)
  )
  
  bym <- data_book$get_data_frame("daily_niger_by_station_name_year_month")
  expect_s3_class(bym, "data.frame")
  expect_true(nrow(bym) > 0)
})

# ----------------------------------------------------------------------
test_that("update_end_season (evaporation = 'value') runs and populates outputs", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  data_book$add_key("daily_niger", c("date", "station_name"), "key")
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  defs$annual_summaries$end_season$start_day <- 1
  defs$annual_summaries$end_season$end_day <- 200
  defs$annual_summaries$end_season$capacity <- 50
  defs$annual_summaries$end_season$water_balance_max <- 100
  defs$annual_summaries$end_season$evaporation_value <- 5
  defs$annual_summaries$end_season$reducing_value <- 0.5
  defs$annual_summaries$end_season$evaporation <- "value"
  
  data_names <- list(
    date = "date",
    rain = "rain",
    year = "year",
    doy = "doy",
    station = "station_name"
  )
  
  suppressWarnings(
    update_end_season("daily_niger", data_names, defs, data_book)
  )
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_true(nrow(by) > 0)
  expect_equal(as.numeric(by$end_season),
              as.numeric(c(NA, 8, rep(1, 34))))
})

# ----------------------------------------------------------------------
test_that("update_end_season (evaporation = 'variable') runs with evap_var", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  daily_niger <- dplyr::mutate(daily_niger, evap_var = 5, year = as.numeric(year))
  data_book$import_data(list(daily_niger = daily_niger))
  data_book$add_key("daily_niger", c("date", "station_name"), "key")
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  defs$annual_summaries$end_season$evaporation <- "variable"
  defs$annual_summaries$end_season$evaporation_value <- NULL
  defs$annual_summaries$end_season$evaporation_variable <- "evap_var"
  defs$annual_summaries$end_season$capacity <- 100
  defs$annual_summaries$end_season$water_balance_max <- 0.5
  defs$annual_summaries$end_season$reducing_value <- 5
  
  data_names <- list(
    date = "date",
    rain = "rain",
    year = "year",
    doy = "doy",
    station = "station_name",
    evaporation_variable = "evap_var"
  )
  
  suppressWarnings(
    update_end_season("daily_niger", data_names, defs, data_book)
  )
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_true(nrow(by) > 0)
})

# ----------------------------------------------------------------------
test_that("update_crops_definitions creates crop_def and/or crop_prop when grids differ", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  # Ensure season bounds exist (used by crops)
  suppressWarnings({
    start_rains(
      data = "daily_niger", date_time = "date", station = "station_name",
      year = "year", rain = "rain", start_day = 121, end_day = 300,
      output = c("doy", "status"), data_book = data_book
    )
    end_rains(
      data = "daily_niger", date_time = "date", station = "station_name",
      year = "year", rain = "rain", start_day = 121, end_day = 300,
      output = c("doy", "status"), data_book = data_book
    )
  })
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  
  data_names <- list(
    date_time = "date",
    element = "rain",
    year = "year",
    doy = "doy",
    station = "station_name",
    rain = "rain"
  )
  seasonal_data_names <- list(
    station = "station_name",
    year = "year",
    start_day = "start_rain",
    end_day = "end_rains"
  )
  
  defs$crops_success$water_requirements <- c(370, 400, 500)
  defs$crops_success$planting_length <- c(110, 220)
  
  suppressWarnings(
    update_crops_definitions(
      data_frame = "daily_niger",
      data_names = data_names,
      seasonal_data_frame = "daily_niger_by_station_name_year",
      seasonal_data_names = seasonal_data_names,
      definitions = defs,
      data_book = data_book
    )
  )
  
  dns <- data_book$get_data_names()
  expect_true(any(c("crop_def", "crop_prop") %in% dns))
  expect_true(any(c("crop_def", "crop_prop") %in% dns))
  
  expect_equal(unique(data_book$get_data_frame("crop_prop")$plant_day), 150)
  expect_equal(unique(data_book$get_data_frame("crop_prop")$plant_length), c(110, 210))
  expect_equal(unique(data_book$get_data_frame("crop_prop")$rain_total), c(350, 400, 450, 500))
  
  expect_equal(unique(data_book$get_data_frame("crop_def")$plant_day), c(100, 150, 200))
  expect_equal(unique(data_book$get_data_frame("crop_def")$plant_length), c(110, 220))
  expect_equal(unique(data_book$get_data_frame("crop_def")$rain_total), c(370, 400, 500))
  
})

# ----------------------------------------------------------------------
test_that("update_crops_definitions collapses to single table when grids identical", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  suppressWarnings({
    start_rains(
      data = "daily_niger", date_time = "date", station = "station_name",
      year = "year", rain = "rain", start_day = 121, end_day = 300,
      output = c("doy", "status"), data_book = data_book
    )
    end_rains(
      data = "daily_niger", date_time = "date", station = "station_name",
      year = "year", rain = "rain", start_day = 121, end_day = 300,
      output = c("doy", "status"), data_book = data_book
    )
  })
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  # make the season_start_probabilities identical to crops_success
  defs$crops_success$water_requirements <- c(370, 400, 500)
  defs$crops_success$planting_length <- c(110, 220)
  defs$season_start_probabilities <- defs$crops_success
  defs$season_start_probabilities$definition_props <- TRUE
  defs$season_start_probabilities$return_crops_table <- FALSE
  
  data_names <- list(
    date_time = "date",
    element = "rain",
    year = "year",
    doy = "doy",
    station = "station_name",
    rain = "rain"
  )
  seasonal_data_names <- list(
    station = "station_name",
    year = "year",
    start_day = "start_rain",
    end_day = "end_rains"
  )
  
  suppressWarnings(
    update_crops_definitions(
      data_frame = "daily_niger",
      data_names = data_names,
      seasonal_data_frame = "daily_niger_by_station_name_year",
      seasonal_data_names = seasonal_data_names,
      definitions = defs,
      data_book = data_book
    )
  )
  
  dns <- data_book$get_data_names()
  expect_true(any(grepl("^crop_def", dns)))
  
  expect_equal(unique(data_book$get_data_frame("crop_prop")$plant_day), unique(data_book$get_data_frame("crop_def")$plant_day))
  expect_equal(unique(data_book$get_data_frame("crop_prop")$plant_length), unique(data_book$get_data_frame("crop_def")$plant_length))
  expect_equal(unique(data_book$get_data_frame("crop_prop")$rain_total), unique(data_book$get_data_frame("crop_def")$rain_total))
})

# ----------------------------------------------------------------------
test_that("update_get_extremes computes extreme_rain and extreme_tmax", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  
  rn <- list(date_time = "date", element = "rain", year = "year", station = "station_name")
  tm <- list(date_time = "date", element = "tmin", year = "year", station = "station_name")
  
  suppressWarnings(
    update_get_extremes("daily_niger", rn, defs, data_book, element = "extreme_rain")
  )
  suppressWarnings(
    update_get_extremes("daily_niger", tm, defs, data_book, element = "extreme_tmax")
  )
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_true(any(grepl("extreme_", names(by))))
})

# ----------------------------------------------------------------------
test_that("update_spells computes longest spells and writes spells_filter", {
  data_book <- DataBook$new()
  data(daily_niger)
  daily_niger <- daily_niger %>% dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_niger = daily_niger))
  
  defs <- jsonlite::read_json("testdata/test_json_2.json")
  
  # tmax > 30
  defs$annual_summaries$longest_tmax_spell$direction <- "greater"
  defs$annual_summaries$longest_tmax_spell$value <- 30
  defs$annual_summaries$longest_tmax_spell$end_day <- 366
  nm_tmax <- list(date_time = "date", element = "tmax", year = "year", doy = "doy", station = "station_name")
  
  suppressWarnings(
    update_spells("daily_niger", nm_tmax, "daily_niger", NULL, defs, data_book, element = "tmax")
  )
  
  spell_values <- data_book$get_data_frame("daily_niger_by_station_name_year")$spells
  expect_equal(as.numeric(spell_values), 
               c(NA, NA, NA, NA, 264, 155, 281, 174, 191, 163, 291, 199, 294, 304, NA,
                 259, 254, 292, 240, 273, NA, NA, 150, 212, 284, 250, 269, 279, 150,
                 265, NA, 242, 237, 246, 271, NA))
  
  # tmin < 20
  defs$annual_summaries$longest_tmin_spell$direction <- "less"
  defs$annual_summaries$longest_tmin_spell$value <- 20
  defs$annual_summaries$longest_tmin_spell$end_day <- 366
  nm_tmin <- list(date_time = "date", element = "tmin", year = "year", doy = "doy", station = "station_name")
  
  suppressWarnings(
    update_spells("daily_niger", nm_tmin, "daily_niger", NULL, defs, data_book, element = "tmin")
  )
  spell_values <- data_book$get_data_frame("daily_niger_by_station_name_year")$spells
  
  expect_equal(as.numeric(spell_values), 
               c(NA, NA, NA, NA, 91, 158, 141, 84, 120, 126, 116, 123, 127, 86, NA,
                 161, 115, 138, 74, 82, 72, 132, 95, 128, 122, 129,
                 108, 146, 61, 129, NA, 123, 135, 138, 115, NA))
  
  # rain >= 0.85 (wet-day spells)
  defs$annual_summaries$longest_rain_spell$direction <- "greater"
  defs$annual_summaries$longest_rain_spell$value <- 0.85
  defs$annual_summaries$longest_rain_spell$end_day <- 366
  defs$annual_summaries$longest_rain_spell$s_start_doy <- 1
  nm_rain <- list(date_time = "date", element = "rain", year = "year", doy = "doy", station = "station_name")
  
  suppressWarnings(
    update_spells("daily_niger", nm_rain, "daily_niger", NULL, defs, data_book, element = "rain")
  )
  
  spell_values <- data_book$get_data_frame("daily_niger_by_station_name_year")$spells
  
  expect_equal(as.numeric(spell_values), 
               c(NA, NA, 3, 2, 1, 2, 3, 3, 4, 2, 3, 2, 3, 5, NA, 3, 3, 3, 3, 5, 
                 2, 2, NA, NA, 2, 1, 2, 3, 3, NA, NA, 2, NA, 2, 2, NA))
  
  by <- data_book$get_data_frame("daily_niger_by_station_name_year")
  expect_s3_class(by, "data.frame")
  expect_true(nrow(by) > 0)
})
