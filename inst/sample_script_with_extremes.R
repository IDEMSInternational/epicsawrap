library(epicsawrap)
library(databook)

# Load R-Instat
setwd(dir="C:/Users/lclem/source/repos/RInstat/instat/bin/x64/Debug/static/InstatObject/R")

source(file="Rsetup.R")

data_book <- DataBook$new()


# Dialog: Import Dataset
new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/ghana_example_epicsa.RDS")
data_book$import_RDS(data_RDS=new_RDS)
rm(new_RDS)

# Dialog: Transform (Adding new col indicator which gives 1 if rainfall >= 40mm)
rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp="(rainfall >= 40)", result_name="rain_day", calculated_from= list("ghana"="rainfall"))
group_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("ghana"="station"))
transform_calculation <- instatCalculations::instat_calculation$new(type="calculation", function_exp="zoo::rollapply(data=rain_day, width=1, FUN=sum, align='right', fill=NA)", result_name="extreme_rain", sub_calculations=list(rain_day), manipulations=list(group_by_station), save=2, before=FALSE, adjacent_column="rainfall")
data_book$run_instat_calculation(calc=transform_calculation, display=FALSE)

rm(list=c("transform_calculation", "rain_day", "group_by_station"))

# Dialog: Climatic Summary (Summarise the new col indicator - count the 1's. i.e., count the # of evenets with >= 40mm rain)
data_book$calculate_summary(columns_to_summarise="extreme_rain", data_name="ghana", factors=c("station", "year"), j=1, summaries=c("summary_sum"), silent=TRUE)

# Then we export to the buckets:

# get the key (wherever you have it saved)
epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")


ghana_by_station_year <- data_book$get_data_frame(data_name="ghana_by_station_year")

# reformat_annual_summaries - only change for this one is that we now add in 
## extreme_rain_days_col = "sum_extreme_rain"
# or extreme_rain_days_col = NAME OF EXTREME VARIABLE
annual_rain <- reformat_annual_summaries(data=ghana_by_station_year,
                                         station_col="station",
                                         year_col="year",
                                         start_rains_doy_col="start_rain",
                                         end_rains_doy_col="end_rains",
                                         season_length_col="length",
                                         extreme_rain_days_col = "sum_extreme_rain")

# Then we can export:
# There are two new arguments, which are OPTIONAL. These are only needed if you 
# want to get the definitions for the extreme rainfall column. However, it requires
# needing to read in the raw data frame since we need to access the definition in it
# for what our rainy-days threshold is.
# We do NOT store the raw data or use it except to get this definition. 
# I've set it as optional for this reason in case they are hesitant to provide the 
# raw data name.
# New Arguments:
# 1. extreme_rainfall_column = "extreme_rain"
# 2. data = "ghana"
exported_data <- export_r_instat_to_bucket(data_by_year = "ghana_by_station_year",
                                           summaries=c("annual_rainfall"),
                                           station="station",
                                           annual_rainfall_data=annual_rain,
                                           start_rains_column="start_rain",
                                           end_rains_column="end_rains",
                                           rain="rainfall",
                                           year="year",
                                           seasonal_length_column="length",
                                           definitions_id="909",
                                           country="internal_tests",
                                           data = "ghana",
                                           extreme_rainfall_column = "extreme_rain")
