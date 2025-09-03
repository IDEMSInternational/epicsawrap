# Longest Dry Spell
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


## Dialog: Start of Rains

year_type <- data_book$get_column_data_types(data_name="data_RDS", columns="s_year")
data_book$convert_column_to_type(data_name="data_RDS", col_names="s_year", to_type="factor")
station_type <- data_book$get_column_data_types(data_name="data_RDS", columns="station")
data_book$convert_column_to_type(data_name="data_RDS", col_names="station", to_type="factor")
data_book$convert_linked_variable(from_data_frame="data_RDS", link_cols=c("s_year", "station"))
grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("data_RDS"="station"))
grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("data_RDS"="station"))
roll_sum_rain <- instatCalculations::instat_calculation$new(type="calculation", function_exp="RcppRoll::roll_sumr(x=rain, n=3, fill=NA, na.rm=FALSE)", result_name="roll_sum_rain", calculated_from=list("data_RDS"="rain"), manipulations=list(grouping_by_station))
rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp="rain >= 0.85", result_name="rain_day", calculated_from=list("data_RDS"="rain"))
dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="instatClimatic::spells(x=rain_day == 0)", result_name="dry_spell", sub_calculations=list(rain_day))
roll_max_dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="dplyr::lead(x=RcppRoll::roll_maxl(n=21, x=dry_spell, fill=NA))", result_name="roll_max_dry_spell", sub_calculations=list(dry_spell))
conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="((rain >= 0.85) & roll_sum_rain > 20 & roll_max_dry_spell <= 9) | is.na(x=rain) | is.na(x=roll_sum_rain) | is.na(x=roll_max_dry_spell)", sub_calculations=list(roll_sum_rain, roll_max_dry_spell))
grouping_by_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("data_RDS"="s_year"))
doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="s_doy >= 93 & s_doy <= 184", calculated_from=databook::calc_from_convert(x=list(data_RDS="s_doy")))
start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(test=is.na(x=dplyr::first(x=rain)) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), yes=NA, no=dplyr::first(x=s_doy, default=NA))", result_name="start_rain", save=2)
start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp="dplyr::if_else(condition=is.na(x=dplyr::first(x=rain)) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), true=as.Date(NA), false=dplyr::first(date, default=NA))", result_name="start_rain_date", save=2)
start_of_rains_status <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(dplyr::n() > 0, ifelse(dplyr::first(is.na(roll_sum_rain)), NA, TRUE), FALSE)", result_name="start_rain_status", save=2)
start_of_rains_combined <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(grouping_by_station, conditions_filter, grouping_by_year, doy_filter), sub_calculation=list(start_of_rains_doy, start_rain_date, start_of_rains_status))
data_book$run_instat_calculation(calc=start_of_rains_combined, display=FALSE, param_list=list(drop=FALSE))
linked_data_name <- data_book$get_linked_to_data_name("data_RDS", link_cols=c("s_year", "station"))
calculated_from_list <- c(setNames("start_rain_status", linked_data_name), setNames("start_rain", linked_data_name))
start_rain_status2 <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(!is.na(start_rain), TRUE, start_rain_status)", calculated_from=calculated_from_list, result_name="start_rain_status", save=2)
start_rain_combined_status_2 <- instatCalculations::instat_calculation$new(type="combination", sub_calculations=list(start_rain_status2))
data_book$run_instat_calculation(calc=start_rain_combined_status_2, display=FALSE, param_list=list(drop=FALSE))
data_book$convert_column_to_type(data_name="data_RDS", col_names="s_year", to_type=year_type)
data_book$remove_unused_station_year_combinations(data_name="data_RDS", year="s_year", station="station")
data_book$convert_linked_variable(from_data_frame="data_RDS", link_cols=c("s_year", "station"))
rm(list=c("start_of_rains_combined", "calculated_from_list", "linked_data_name", "station_type", "year_type", "start_of_rains_status", "start_rain_date", "start_of_rains_doy", "doy_filter", "grouping_by_year", "rain_day", "dry_spell", "roll_max_dry_spell", "roll_sum_rain", "conditions_filter", "grouping_by_station", "start_rain_status2", "start_rain_combined_status_2"))
#View(data_book$get_data_frame("data_RDS_by_station_s_year"))



# Dialog: Spells
spell_day <- instatCalculations::instat_calculation$new(calculated_from= list("data_RDS"="rain"), type="calculation", function_exp="(rain >= 0) & rain <= 0.85", result_name="spell_day", save=0)
spell_length <- instatCalculations::instat_calculation$new(type="calculation", result_name="spell_length", sub_calculations=list(spell_day), save=0, function_exp="instatClimatic::spells(x=spell_day)")
grouping <- instatCalculations::instat_calculation$new(calculated_from=list("data_RDS" = "station", "data_RDS"="s_year"), type="by")
spells <- instatCalculations::instat_calculation$new(type="summary", function_exp="max(x=spell_length)", result_name="longest_dry_spell", manipulations=list(spell_length, grouping), save=2)
data_book$run_instat_calculation(calc=spells, display=FALSE)

data_RDS_by_station_s_year <- data_book$get_data_frame("data_RDS_by_station_s_year")
# longest_dry_spell - this is a new column. 
annual_rain <- reformat_annual_summaries(data_RDS_by_station_s_year,
                                         station_col = "station", 
                                         year_col = "s_year",
                                         start_rains_doy_col = "start_rain",
                                         start_rains_date_col = "start_rain_date",
                                         longest_rain_spell_col = "longest_dry_spell",
                                         longest_tmin_spell_col  = NULL,
                                         longest_tmax_spell_col = NULL)


#########
# Export to Buckets
export_r_instat_to_bucket(station="station",
                          summaries=c("annual_rainfall"),
                          data = "data_RDS",
                          annual_rainfall_data = annual_rain,
                          data_by_year="data_RDS_by_station_s_year",
                          longest_rain_spell_col = "longest_dry_spell",
                          #longest_tmin_spell_col = "longest_dry_spell",
                          #longest_tmax_spell_col = "longest_dry_spell",
                          start_rains_column="start_rain",
                          rain="rain",
                          year="s_year",
                          include_summary_data=TRUE,
                          definitions_id="00",
                          country="internal_tests")
