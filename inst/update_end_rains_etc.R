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
                   station = "station_name",
                   year = "year",
                   rain = "rain",
                   doy = "doy")

daily_niger <- data_book$get_data_frame("daily_niger")

rpicsa::end_rains(data = "daily_niger",
                    date_time = data_names$date,
                    station = data_names$station,
                    year = data_names$year,
                    rain = data_names$rain,
                    doy = data_names$doy,
                    s_start_month = 1,
                    drop = FALSE,
                    start_day = 121,
                    end_day = 300,
                    output = c("doy", "date", "status"),
                    interval_length = 1,
                    min_rainfall = 20,
                    data_book = data_book)

daily_niger <- data_book$get_data_frame("daily_niger")

suppressWarnings(end_rains(data = "daily_niger",
                           date_time = "date",
                           station = "station_name",
                           rain = "rain",
                           start_day = 121,
                           end_day = 300,
                           data_book = data_book))


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

data_book$get_data_frame("daily_niger_by_station_name_month")

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



################# START RAINS =========================================================

library(rpicsa)
library(databook)

# 1. Let's set up our data book
data_book <- DataBook$new()

# 2. Importing in some data for testing (this is stored in the rpicsa package)
data(daily_niger)
data_book$import_data(list(daily_niger = daily_niger))

# 3. Read in our definitions data
definitions <- jsonlite::fromJSON("C:/Users/lclem/Downloads/test_json_1.json")

# temp add random numbers in here. Fixed it in rpicsa that this is not needed after next sync. 
definitions$annual_summaries$start_rains$prob_rain_day <- 0.8
definitions$annual_summaries$start_rains$period_interval <- 3
definitions$annual_summaries$start_rains$period_max_dry_days <- 1
definitions$annual_summaries$start_rains$max_rain <- 3
# 4. Put in "data_names" the names of all the variables we're going to use from the daily_niger data.
# Looking at our rpicsa::annual_rain, this can be
# station, year, and rain
data_names <- list(date = "date",
                   rain = "rain",
                   year = "year",
                   doy = "doy",
                   station = "station_name")

update_start_rains(data_frame = "daily_niger",
                   data_names = data_names,
                   definitions = definitions,
                   data_book = data_book)




### CROPS #############
crop_success_definition <- definitions$crops_success




