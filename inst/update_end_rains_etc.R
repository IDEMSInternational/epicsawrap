################# START RAINS =========================================================

library(rpicsa)
library(databook)

# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
daily_niger$var <- 5
data_book$import_data(list(daily_niger = daily_niger))

# 3. Read in our definitions data
#definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")

# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   rain = "rain",
                   year = "year",
                   doy = "doy",
                   station = "station_name",
                   evaporation_variable = "var")


update_start_rains(data_frame = "daily_niger",
                   data_names = data_names,
                   definitions = definitions,
                   data_book = data_book)

############ END RAINS '#####################################################
# 
# library(rpicsa)
# library(databook)
# devtools::load_all()
# 
# # 1. Let's set up our data book
# data_book <- DataBook$new()
# 
# # 2. Importing in some data for testing (this is stored in the rpicsa package)
# data(daily_niger)
# data_book$import_data(list(daily_niger = daily_niger))

# 3. Read in our definitions data
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")

# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   station = "station_name",
                   year = "year",
                   rain = "rain",
                   doy = "doy")

daily_niger <- data_book$get_data_frame("daily_niger")

update_end_rains("daily_niger", data_names, definitions, data_book)

# suppressWarnings(end_rains(data = "daily_niger",
#                            date_time = "date",
#                            station = "station_name",
#                            rain = "rain",
#                            start_day = 121,
#                            end_day = 300,
#                            data_book = data_book))


### Seasonal Rain ====================================================================

library(rpicsa)
library(databook)
devtools::load_all()
# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
data_book$import_data(list(daily_niger = daily_niger))

# 3. Read in our definitions data
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")
definitions$annual_summaries$seasonal_rain$total_rain <- TRUE
definitions$annual_summaries$seasonal_rain$n_rain <- TRUE

# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   station = "station_name",
                   year = "year",
                   rain = "rain",
                   doy = "doy")

# 5. Defining the "summary_data_names" list
summary_data_names <- list(start_date = NULL,
                           end_date = NULL)

update_seasonal_rain(data_frame = "daily_niger",
                     data_names = data_names,
                     summary_data_frame = NULL,
                     summary_data_names = summary_data_names,
                     definitions = definitions,
                     data_book = data_book)

# Testing seasonal rain "raw"
library(databook)
data_book <- DataBook$new()
daily_data <- rpicsa::daily_niger |>
  dplyr::filter(station_name == "Agades", year > 1945, year <= 1950) |>
  dplyr::mutate(year = as.numeric(year))
data_book$import_data(list(daily_data = daily_data))

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

# Inspect results
data_book$get_data_frame("daily_data_by_station_name_year")



###############################################################################

# update_annual_temperature ####

library(rpicsa)
library(databook)
devtools::load_all()

# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
data_book$import_data(list(daily_niger = daily_niger))

# 3. Read in our definitions data
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")

# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   tmin = "tmin",
                   tmax = "tmax",
                   year = "year",
                   month = "month",
                   station = "station_name")

update_annual_temperature(data_frame = "daily_niger",
                          data_names = data_names,
                          definitions = definitions,
                          data_book = data_book)

data_book$get_data_frame("daily_niger_by_station_name_year")




# MONTHLY TEMPS =====================================================================

library(rpicsa)
library(databook)

# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
data_book$import_data(list(daily_niger = daily_niger))

# 3. Read in our definitions data
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")
definitions$monthly_temperature_summaries$max_tmin$to <- NULL
definitions$monthly_temperature_summaries$min_tmin$to <- NULL
definitions$monthly_temperature_summaries$mean_tmin$to <- NULL
definitions$monthly_temperature_summaries$max_tmax$to <- NULL
definitions$monthly_temperature_summaries$min_tmax$to <- NULL
definitions$monthly_temperature_summaries$mean_tmax$to <- NULL
# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   tmin = "tmin",
                   tmax = "tmax",
                   year = "year",
                   month = "month",
                   station = "station_name")

update_monthly_temperature(data_frame = "daily_niger",
                           data_names = data_names,
                           definitions = definitions,
                           data_book = data_book)

data_book$get_data_frame("daily_niger_by_station_name_year_month")

#############################################################

# UPDATE END SEASON =============================================================

library(rpicsa)
library(databook)

# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
data_book$import_data(list(daily_niger = daily_niger))
data_book$add_key("daily_niger", c("date", "station_name"), "key")

# 3. Read in our definitions data
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")
# 
definitions$annual_summaries$end_season$start_day <- 1
definitions$annual_summaries$end_season$end_day <- 200
definitions$annual_summaries$end_season$capacity <- 50
definitions$annual_summaries$end_season$water_balance_max <- 100
definitions$annual_summaries$end_season$evaporation_value <- 5
definitions$annual_summaries$end_season$reducing_value <- 0.5

# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   rain = "rain",
                   year = "year",
                   doy = "doy",
                   station = "station_name")
update_end_season(data_frame = "daily_niger",
                  data_names = data_names,
                  definitions = definitions,
                  data_book = data_book)
data_book$get_data_frame("daily_niger_by_station_name_year")


# Now with an evaporation variable ################################
# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
daily_niger <- daily_niger %>%
  dplyr::mutate(evap_var = 5,
                year = as.numeric(year))
data_book$import_data(list(daily_niger = daily_niger))
data_book$add_key("daily_niger", c("date", "station_name"), "key")

# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
definitions$annual_summaries$end_season$evaporation <- "variable"
definitions$annual_summaries$end_season$evaporation_value <- NULL
definitions$annual_summaries$end_season$evaporation_variable <- "evap_var"

data_names <- list(date = "date",
                   rain = "rain",
                   year = "year",
                   doy = "doy",
                   station = "station_name",
                   evaporation_variable = "evap_var")
update_end_season(data_frame = "daily_niger",
                  data_names = data_names,
                  definitions = definitions,
                  data_book = data_book)
data_book$get_data_frame("daily_niger_by_station_name_year")




### CROPS #############
# library(databook)
# data_book <- DataBook$new()
# definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
# data(daily_niger)
# data_book$import_data(list(daily_niger = daily_niger))

data_names <- list(date_time = "date",
                   element = "rain",
                   year = "year",
                   doy = "doy",
                   station = "station_name",
                   rain = "rain")

daily_niger_by_station_name_year <- (data_book$get_data_frame("daily_niger_by_station_name_year"))

seasonal_data_names <- list(station = "station_name",
                            year = "year",
                            start_day = "start_rain",
                            end_day = "end_rains")

# 1. With crop success != seasonal start
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
update_crops_definitions(data_frame = "daily_niger",
                         data_names = data_names,
                         seasonal_data_frame = "daily_niger_by_station_name_year",
                         seasonal_data_names = seasonal_data_names,
                         definitions = definitions,
                         data_book = data_book)
# TODO: crop_prop6 should run like 
# data_book$delete_dataframes(c("crop_def", "crop_def1", "crop_def2", "crop_def3",
#                               "crop_def4",  "crop_def5",  "crop_def6", "crop_def7",
#                               "crop_def8",  "crop_def9",
#                               "crop_prop", "crop_prop1", "crop_prop2", "crop_prop3",
#                               "crop_prop4", "crop_prop5", "crop_prop6", "crop_prop7"))
data_book$get_data_names() # crop_def3, crop_prop3
crop_def <- data_book$get_data_frame("crop_def")
unique(crop_def$plant_day)
unique(crop_def$plant_length)
unique(crop_def$rain_total)
crop_prop <- (data_book$get_data_frame("crop_prop"))
unique(crop_prop$plant_day)
unique(crop_prop$plant_length)
unique(crop_prop$rain_total)

# 2. With crop success == seasonal start
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
definitions$season_start_probabilities <- definitions$crops_success
definitions$season_start_probabilities$definition_props <- TRUE
definitions$season_start_probabilities$return_crops_table <- FALSE
update_crops_definitions(data_frame = "daily_niger",
                         data_names = data_names,
                         seasonal_data_frame = "daily_niger_by_station_name_year",
                         seasonal_data_names = seasonal_data_names,
                         definitions = definitions,
                         data_book = data_book)
data_book$get_data_names() # crop_def3, crop_prop3
crop_def <- data_book$get_data_frame("crop_def1")
unique(crop_def$plant_day)
unique(crop_def$plant_length)
unique(crop_def$rain_total)
crop_prop <- (data_book$get_data_frame("crop_prop1"))
unique(crop_prop$plant_day)
unique(crop_prop$plant_length)
unique(crop_prop$rain_total)

# 3. With crop success TRUE and = seasonal start FALSE
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
definitions$season_start_probabilities$definition_props <- FALSE
update_crops_definitions(data_frame = "daily_niger",
                         data_names = data_names,
                         seasonal_data_frame = "daily_niger_by_station_name_year",
                         seasonal_data_names = seasonal_data_names,
                         definitions = definitions,
                         data_book = data_book)
data_book$get_data_names() # crop_def3, crop_prop3
crop_def <- data_book$get_data_frame("crop_def2")
unique(crop_def$plant_day)
unique(crop_def$plant_length)
unique(crop_def$rain_total)

# 4. With crop success FALSE and = seasonal start TRUE
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
definitions$crops_success$return_crops_table <- FALSE
update_crops_definitions(data_frame = "daily_niger",
                         data_names = data_names,
                         seasonal_data_frame = "daily_niger_by_station_name_year",
                         seasonal_data_names = seasonal_data_names,
                         definitions = definitions,
                         data_book = data_book)
data_book$get_data_names() # crop_def3, crop_prop3
crop_prop <- (data_book$get_data_frame("crop_prop"))
unique(crop_prop$plant_day)
unique(crop_prop$plant_length)
unique(crop_prop$rain_total)

### GET EXTREMES ####################################################################
library(databook)
data_book <- DataBook$new()
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
data(daily_niger)
data_book$import_data(list(daily_niger = daily_niger))

data_names <- list(date_time = "date",
                   element = "rain",
                   year = "year",
                   station = "station_name")
update_get_extremes(data_frame = "daily_niger",
                    data_names = data_names,
                    definitions = definitions,
                    data_book = data_book,
                    element = "extreme_rain")
data_book$get_data_frame("daily_niger_by_station_name_year")

data_names <- list(date_time = "date",
                   element = "tmin",
                   year = "year",
                   station = "station_name")
update_get_extremes(data_frame = "daily_niger",
                    data_names = data_names,
                    definitions = definitions,
                    data_book = data_book,
                    element = "extreme_tmax")
data_book$get_data_frame("daily_niger_by_station_name_year")

update_get_extremes(data_frame = "daily_niger",
                    data_names = data_names,
                    definitions = definitions,
                    data_book = data_book,
                    element = "extreme_tmax")
data_book$get_data_frame("daily_niger_by_station_name_year")




### SPELLS ####################################################################
# Initialising R (e.g Loading R packages)
data_book <- DataBook$new()

data("daily_niger")

data_book$import_data(data_tables=list(daily_niger=daily_niger))

# 1. tmax > 30
data_names <- list(date_time = "date",
                   element = "tmax",
                   year = "year",
                   doy = "doy",
                   station = "station_name")

definitions$annual_summaries$longest_tmax_spell$direction <- "greater" 
definitions$annual_summaries$longest_tmax_spell$value <- 30
definitions$annual_summaries$longest_tmax_spell$end_day <- 366

update_spells(data_frame = "daily_niger",
              data_names = data_names,
              summary_data_frame = "daily_niger",
              seasonal_data_names = NULL,
              definitions = definitions,
              data_book = data_book,
              element = "longest_tmax_spell")
data_book$get_data_frame("daily_niger_by_station_name_year")


# 2. tmin < 20
data_names <- list(date_time = "date",
                   element = "tmin",
                   year = "year",
                   doy = "doy",
                   station = "station_name")

definitions$annual_summaries$longest_tmin_spell$direction <- "less" 
definitions$annual_summaries$longest_tmin_spell$value <- 20
definitions$annual_summaries$longest_tmin_spell$end_day <- 366

update_spells(data_frame = "daily_niger",
              data_names = data_names,
              summary_data_frame = "daily_niger",
              seasonal_data_names = NULL,
              definitions = definitions,
              data_book = data_book,
              element = "longest_tmin_spell")
data_book$get_data_frame("daily_niger_by_station_name_year")


# 3. rain between 10 and 30
data_names <- list(date_time = "date",
                   element = "rain",
                   year = "year",
                   doy = "doy",
                   station = "station_name")

definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_2.json")
definitions$annual_summaries$longest_rain_spell$direction <- "greater" 
definitions$annual_summaries$longest_rain_spell$value <- 0.85
definitions$annual_summaries$longest_rain_spell$end_day <- 366
definitions$annual_summaries$longest_rain_spell$s_start_doy <- 1

update_spells(data_frame = "daily_niger",
              data_names = data_names,
              summary_data_frame = "daily_niger",
              seasonal_data_names = NULL,
              definitions = definitions,
              data_book = data_book,
              element = "longest_rain_spell")
data_book$get_data_frame("daily_niger_by_station_name_year")