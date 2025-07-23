## Extremes
library(databook)
library(tidyverse)
devtools::load_all()

data_book <- DataBook$new()

## Set up: Importing Mansa daily data already organised using R-Instat
mansa_daily <- readRDS("C:/Users/lclem/Downloads/mansa_daily.rds")
data_book$import_RDS(data_RDS = mansa_daily)
rm(mansa_daily)
data_book$get_data_names()

# read in EPICSA token to access google buckets
library(epicsawrap)
epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")


# Dialog: Transform
rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp = "rain >= 50.00", result_name="rain_day", calculated_from= list("data_RDS"="rain"))
transform_calculation <- instatCalculations::instat_calculation$new(type="calculation", function_exp="zoo::rollapply(data=rain_day, width=1, FUN=sum, align='right', fill=NA)", result_name="count", sub_calculations=list(rain_day), manipulations=list(), save=2, before=FALSE, adjacent_column="rain")
data_book$run_instat_calculation(calc=transform_calculation, display=FALSE)

rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp = "tmin <= 15.00", result_name="rain_day", calculated_from= list("data_RDS"="tmin"))
transform_calculation <- instatCalculations::instat_calculation$new(type="calculation", function_exp="zoo::rollapply(data=rain_day, width=1, FUN=sum, align='right', fill=NA)", result_name="tmin_count", sub_calculations=list(rain_day), manipulations=list(), save=2, before=FALSE, adjacent_column="rain")
data_book$run_instat_calculation(calc=transform_calculation, display=FALSE)

# TODO: how do we handle NAs here? And how do we want to handle NAs?
rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp = "tmax >= 30.00", result_name="rain_day", calculated_from= list("data_RDS"="tmax"))
transform_calculation <- instatCalculations::instat_calculation$new(type="calculation", function_exp="zoo::rollapply(data=rain_day, width=1, FUN=sum, align='right', fill=NA)", result_name="tmax_count", sub_calculations=list(rain_day), manipulations=list(), save=2, before=FALSE, adjacent_column="rain")
data_book$run_instat_calculation(calc=transform_calculation, display=FALSE)

# Dialog: Summarise
data_book$calculate_summary(data_name="data_RDS", columns_to_summarise=c("count", "tmin_count", "tmax_count"), store_results=TRUE, return_output=FALSE,
                            factors=c("s_year","station"), j=1, summaries=c("summary_sum"), silent=TRUE)

data_RDS_by_s_year_station <- data_book$get_data_frame("data_RDS_by_s_year_station")
annual_rain <- reformat_annual_summaries(data_RDS_by_s_year_station,
                                         station_col = "station", 
                                         year_col = "s_year",
                                         extreme_rain_days_col = "sum_count",
                                         extreme_tmin_days_col  = "sum_tmin_count",
                                         extreme_tmax_days_col = "sum_tmax_count")


#########
# Export to Buckets
export_r_instat_to_bucket(station="station",
                                      summaries=c("annual_rainfall"),
                                      data = "data_RDS",
                                      annual_rainfall_data = annual_rain,
                                      data_by_year="data_RDS_by_s_year_station",
                                      start_rains_column = "start_rains",
                                      rain="rain",
                                      year="s_year",
                                      extreme_rainfall_column="count",   # TODO: we add in extreme_tmin_count, extreme_tmax_count (to be from raw data, not summary data)
                                      extreme_tmin_column="tmin_count",
                                      extreme_tmax_column = "tmax_count",
                                      include_summary_data=TRUE,
                                      definitions_id="00",
                                      country="internal_tests")

head(data_book$get_data_frame("data_RDS"))
