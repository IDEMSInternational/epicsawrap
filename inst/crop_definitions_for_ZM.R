devtools::install_github("IDEMSInternational/databook")
devtools::install_github("IDEMSInternational/rpicsa")
devtools::install_github("IDEMSInternational/instatExtras")

library(databook)
library(tidyverse)

# Initialising R (e.g Loading R packages)

setwd(dir="C:/Users/lclem/source/repos/RInstat/instat/bin/x64/Debug/static/InstatObject/R")

source(file="Rsetup.R")

data_book <- DataBook$new()

## Set up: Getting the data from Climsoft

# Connect database connection.
data_book$database_connect(dbname="mariadb_climsoft_db_v4_zambia", host="35.187.47.59", port=3306, user="root")


# Dialog: Import From Climsoft
data_book$import_climsoft_data(table="observationfinal",
                               station_filter_column="stationName",
                               stations=c("CHIPATA MET","LUNDAZI MET","MFUWE MET","MSEKERA AGROMET","PETAUKE MET"),
                               element_filter_column="elementName",
                               elements=c("Temp daily mean","Temp  daily max","Temp  daily min","Temp  daily mean","Precip  daily"))


# Dialog: Unstack (Pivot Wider)
observations_data <- data_book$get_data_frame(data_name="observations_data")
observations_data_unstacked <- tidyr::pivot_wider(data=observations_data, names_from=element_abbrv, values_from=value)
data_book$import_data(data_tables=list(observations_data_unstacked=observations_data_unstacked))
rm(list=c("observations_data_unstacked", "observations_data"))

# Dialog: Use Date
data_book$split_date(data_name="observations_data_unstacked", col_name="date", year_val=TRUE, month_val=TRUE, day_in_year_366 =TRUE, s_start_month=7)

#data_book$delete_dataframes(c("observations_unstacked_data_by_station_id_s_year", "crop_def", "crop_prop"))

# Dialog: Start of Rains
#Start of rains: First day after 1st Oct with 20mm rain over 3 days
# with no dry spell exceeding 9 days in the following 21 days
year_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="s_year")
data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type="factor")
station_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="station_id")
data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="station_id", to_type="factor")
data_book$convert_linked_variable(from_data_frame="observations_unstacked_data", link_cols=c("s_year", "station_id"))
grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="station_id"))
grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="station_id"))
roll_sum_rain <- instatCalculations::instat_calculation$new(type="calculation", function_exp="RcppRoll::roll_sumr(x=PRECIP, n=3, fill=NA, na.rm=FALSE)", result_name="roll_sum_rain", calculated_from=list("observations_unstacked_data"="PRECIP"), manipulations=list(grouping_by_station))
rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp="PRECIP >= 0.85", result_name="rain_day", calculated_from=list("observations_unstacked_data"="PRECIP"))
dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="rpicsa::spells(x=rain_day == 0)", result_name="dry_spell", sub_calculations=list(rain_day))
roll_max_dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="dplyr::lead(x=RcppRoll::roll_maxl(n=21, x=dry_spell, fill=NA))", result_name="roll_max_dry_spell", sub_calculations=list(dry_spell))
conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="((PRECIP >= 0.85) & roll_sum_rain > 20 & roll_max_dry_spell <= 9) | is.na(x=PRECIP) | is.na(x=roll_sum_rain) | is.na(x=roll_max_dry_spell)", sub_calculations=list(roll_sum_rain, roll_max_dry_spell))
grouping_by_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="s_year"))
doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="s_doy >= 93 & s_doy <= 184", calculated_from=databook::calc_from_convert(x=list(observations_unstacked_data="s_doy")))
start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(test=is.na(x=dplyr::first(x=PRECIP)) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), yes=NA, no=dplyr::first(x=s_doy, default=NA))", result_name="start_rain", save=2)
start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp="dplyr::if_else(condition=is.na(x=dplyr::first(x=PRECIP)) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), true=as.Date(NA), false=dplyr::first(date, default=NA))", result_name="start_rain_date", save=2)
start_of_rains_status <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(dplyr::n() > 0, ifelse(dplyr::first(is.na(roll_sum_rain)), NA, TRUE), FALSE)", result_name="start_rain_status", save=2)
start_of_rains_combined <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(grouping_by_station, conditions_filter, grouping_by_year, doy_filter), sub_calculation=list(start_of_rains_doy, start_rain_date, start_of_rains_status))
data_book$run_instat_calculation(calc=start_of_rains_combined, display=FALSE, param_list=list(drop=FALSE))
linked_data_name <- data_book$get_linked_to_data_name("observations_unstacked_data", link_cols=c("s_year", "station_id"))
calculated_from_list <- c(setNames("start_rain_status", linked_data_name), setNames("start_rain", linked_data_name))
start_rain_status2 <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(!is.na(start_rain), TRUE, start_rain_status)", calculated_from=calculated_from_list, result_name="start_rain_status", save=2)
start_rain_combined_status_2 <- instatCalculations::instat_calculation$new(type="combination", sub_calculations=list(start_rain_status2))
data_book$run_instat_calculation(calc=start_rain_combined_status_2, display=FALSE, param_list=list(drop=FALSE))
data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type=year_type)
data_book$remove_unused_station_year_combinations(data_name="observations_unstacked_data", year="s_year", station="station_id")
data_book$convert_linked_variable(from_data_frame="observations_unstacked_data", link_cols=c("s_year", "station_id"))
rm(list=c("start_of_rains_combined", "calculated_from_list", "linked_data_name", "station_type", "year_type", "start_of_rains_status", "start_rain_date", "start_of_rains_doy", "doy_filter", "grouping_by_year", "rain_day", "dry_spell", "roll_max_dry_spell", "roll_sum_rain", "conditions_filter", "grouping_by_station", "start_rain_status2", "start_rain_combined_status_2"))

# Dialog: End of Rains/Season
#End of season: Capacity of 100mm on 1st March reduces 0.5 with evaporation rate of 5mm per d
year_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="s_year")
data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type="factor")
station_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="station_id")
data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="station_id", to_type="factor")
data_book$convert_linked_variable(from_data_frame="observations_unstacked_data", link_cols=c("s_year", "station_id"))
# 100mm on 1st March reduces 0.5 with evaporation rate of 5mm per day
rain_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(test=is.na(x=PRECIP), yes=0, no=PRECIP)", result_name="rain_min", calculated_from=list("observations_unstacked_data"="PRECIP"))
wb_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp="purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), 100), .x=tail(x=rain_min - 5, n=-1), .init=0)", result_name="wb_min", sub_calculations=list(rain_min))
rain_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(test=is.na(x=PRECIP), yes=100, no=PRECIP)", result_name="rain_max", calculated_from=list("observations_unstacked_data"="PRECIP"))
wb_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp="purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), 100), .x=tail(x=rain_max - 5, n=-1), .init=0)", result_name="wb_max", sub_calculations=list(rain_max))
wb <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(test=(wb_min != wb_max) | is.na(x=PRECIP), yes=NA, no=wb_min)", result_name="wb", sub_calculations=list(wb_min, wb_max))
conditions_check <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse((wb <= 0.5) | is.na(x=PRECIP), 1, 0)", result_name="conditions_check", sub_calculations=list(wb))
conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="conditions_check == 1", sub_calculations=list(conditions_check))
grouping_by_station_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="station_id","observations_unstacked_data"="s_year"))
doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="s_doy >= 245 & s_doy <= 366", calculated_from=databook::calc_from_convert(x=list(observations_unstacked_data="s_doy")))
end_season <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(test=is.na(x=dplyr::first(x=wb)), yes=NA, no=dplyr::first(x=s_doy))", result_name="end_season", calculated_from=list("observations_unstacked_data"="s_doy"), save=2)
end_season_date <- instatCalculations::instat_calculation$new(type="summary", function_exp="dplyr::if_else(condition=is.na(x=dplyr::first(x=wb)), true=as.Date(NA), false=dplyr::first(x=date))", result_name="end_season_date", save=2)
end_of_season_combined <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(conditions_filter, grouping_by_station_year, doy_filter), sub_calculations=list(end_season, end_season_date))
data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_of_season_combined)
conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="conditions_check == 1 | is.na(conditions_check) | conditions_check == 0", sub_calculations=list(conditions_check))
end_season_status <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(dplyr::n() == 0, NA, ifelse(all(conditions_check == 0, na.rm=TRUE), FALSE, NA))", calculated_from=list("observations_unstacked_data"="s_doy"), result_name="end_season_status", sub_calculations=list(conditions_check), save=2)
end_of_season_combined_status <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(conditions_filter, grouping_by_station_year, doy_filter), sub_calculations=list(end_season_status))
data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_of_season_combined_status)
linked_data_name <- data_book$get_linked_to_data_name("observations_unstacked_data", link_cols=c("s_year", "station_id"))
calculated_from_list <- c(setNames("end_season_status", linked_data_name), setNames("end_season", linked_data_name))
end_season_status_2 <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(!is.na(end_season), TRUE, end_season_status)", calculated_from=calculated_from_list, result_name="end_season_status", save=2)
end_season_combined_status_2 <- instatCalculations::instat_calculation$new(type="combination", sub_calculations=list(end_season_status_2))
data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_season_combined_status_2)
data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type=year_type)
data_book$convert_column_to_type(data_name=linked_data_name, col_names="s_year", to_type=year_type)
data_book$remove_unused_station_year_combinations(data_name="observations_unstacked_data", year="s_year", station="station_id")
rm(list=c("end_of_season_combined", "calculated_from_list", "linked_data_name", "end_of_season_combined_status", "end_season_status", "station_type", "year_type", "end_season_date", "end_season", "doy_filter", "grouping_by_station_year", "rain_max", "wb_max", "rain_min", "wb_min", "wb", "conditions_check", "conditions_filter", "end_season_status_2", "end_season_combined_status_2"))

# Dialog: PICSA Crops
data_book$crops_definitions(data_name="observations_unstacked_data", year="s_year",
                            station="station_id", rain="PRECIP", day="s_doy",
                            plant_days = seq(from = 0, to = 270, by = 5),
                            plant_lengths=seq(from = 45, to = 180, by = 5),
                            rain_totals=seq(from = 200, to = 1200, by = 25),
                            start_day="start_rain",
                            season_data_name="observations_unstacked_data_by_station_id_s_year",
                            start_check="both", end_day="end_season")
#saveRDS(file="C:/Users/lclem/OneDrive/Documents/Zambia_data.RDS", object=data_book)

# read in EPICSA token to access google buckets
epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")

# Prepare for buckets
crop_prop <- data_book$get_data_frame(data_name="crop_prop")
crop_prop <- epicsawrap::reformat_crop_success(data=crop_prop, station_col="station_id",
                                   total_rain_col="rain_total", plant_day_col="plant_day",
                                   plant_length_col="plant_length",
                                   prop_success_with_start_col = "prop_success_with_start", prop_success_no_start_col = "prop_success_no_start")
crop_def <- data_book$get_data_frame(data_name="crop_def")
crop_def <- epicsawrap::reformat_season_start(data=crop_def, station_col="station_id", year_col="year", plant_day_col="plant_day", plant_day_cond_col="plant_day_cond")
observations_unstacked_data_by_station_id_s_year <- data_book$get_data_frame(data_name="observations_unstacked_data_by_station_id_s_year")
annual_rain <- epicsawrap::reformat_annual_summaries(data=observations_unstacked_data_by_station_id_s_year, station_col="station_id", year_col="s_year", start_rains_doy_col="start_rain", start_rains_date_col="start_rain_date", end_season_doy_col="end_season", end_season_date_col="end_season_date")

# Export to Buckets
exported_data <- epicsawrap::export_r_instat_to_bucket(data_by_year = "observations_unstacked_data_by_station_id_s_year",
                                           summaries=c("crop_success"),#, "annual_rainfall"),
                                           station = "station_id",
                                           start_rains_column="start_rain",
                                           crop_data = "crop_def",
                                           #crop_data_name = "crop_def",
                                           end_rains_column="end_season",
                                           rain="PRECIP", year="s_year",
                                           crop_success_data=crop_prop,
                                           seasonal_length_column="length",
                                           include_summary_data=TRUE,
                                           definitions_id="444",
                                           country="zm_test")
rm(list=c("crop_prop", "crop_def"))

# Dialog: Save Data As
saveRDS(file="C:/Users/lclem/OneDrive/Documents/Zambia_data.RDS", object=data_book)


# TODO:
# fix code to work for annual_rainfall when we have end_season
# fix code to work for start_season for MW