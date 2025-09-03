library(databook)

data_book <- DataBook$new()

# Dialog: Import Dataset

new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/MW_data.RDS")
data_book$import_RDS(data_RDS=new_RDS)
rm(new_RDS)

data_book$get_data_names()

#data_book$delete_dataframes(c("observations_unstacked_data_by_station_id_s_year", "crop_prop", "crop_def"))

#View(data_book$get_data_frame("observations_unstacked_data"))

# Dialog: Start of Rains
#  first occasion (from 1st October) - s_doy 93
#  more than 25mm in a 3 day period
# no dry spell of 10 days or more within the following 30 days

year_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="s_year")

data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type="factor")
station_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="station_id")

data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="station_id", to_type="factor")
data_book$convert_linked_variable(from_data_frame="observations_unstacked_data", link_cols=c("s_year", "station_id"))
grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="station_id"))
grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="station_id"))
roll_sum_rain <- instatCalculations::instat_calculation$new(type="calculation", function_exp="RcppRoll::roll_sumr(x=PRECIP, n=3, fill=NA, na.rm=FALSE)", result_name="roll_sum_rain", calculated_from=list("observations_unstacked_data"="PRECIP"), manipulations=list(grouping_by_station))
rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp="PRECIP >= 0.5", result_name="rain_day", calculated_from=list("observations_unstacked_data"="PRECIP"))
dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="instatClimatic::spells(x=rain_day == 0)", result_name="dry_spell", sub_calculations=list(rain_day))
roll_max_dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="dplyr::lead(x=RcppRoll::roll_maxl(n=30, x=dry_spell, fill=NA))", result_name="roll_max_dry_spell", sub_calculations=list(dry_spell))
conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="((PRECIP >= 0.5) & roll_sum_rain > 25 & roll_max_dry_spell <= 10) | is.na(x=PRECIP) | is.na(x=roll_sum_rain) | is.na(x=roll_max_dry_spell)", sub_calculations=list(roll_sum_rain, roll_max_dry_spell))
grouping_by_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="s_year"))
doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="s_doy >= 93 & s_doy <= 184", calculated_from=databook::calc_from_convert(x=list(observations_unstacked_data="s_doy")))
start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(test=is.na(x=dplyr::first(x=PRECIP)) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), yes=NA, no=dplyr::first(x=s_doy, default=NA))", result_name="start_rain", save=2)
start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp="dplyr::if_else(condition=is.na(x=dplyr::first(x=PRECIP)) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), true=as.Date(NA), false=dplyr::first(date_time, default=NA))", result_name="start_rain_date", save=2)
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
# End of season is defined as the last day in the season (1st October - 30th April)
# more than 10mm of rainfall
year_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="s_year")

#View(data_book$get_data_frame("observations_unstacked_data"))

data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type="factor")

station_type <- data_book$get_column_data_types(data_name="observations_unstacked_data", columns="station_id")


data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="station_id", to_type="factor")

data_book$convert_linked_variable(from_data_frame="observations_unstacked_data", link_cols=c("s_year", "station_id"))


roll_sum_rain <- instatCalculations::instat_calculation$new(type="calculation", function_exp="RcppRoll::roll_sumr(x=PRECIP, n=1, fill=NA, na.rm=FALSE)", result_name="roll_sum_rain", calculated_from=list("observations_unstacked_data"="PRECIP"))
conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="(roll_sum_rain > 10) | is.na(x=roll_sum_rain)", sub_calculations=list(roll_sum_rain))
grouping_by_station_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("observations_unstacked_data"="station_id","observations_unstacked_data"="s_year"))
doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="s_doy >= 93 & s_doy <= 305", calculated_from=databook::calc_from_convert(x=list(observations_unstacked_data="s_doy")))
end_rains <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(test=is.na(x=dplyr::last(x=roll_sum_rain)), yes=NA, no=dplyr::last(x=s_doy))", result_name="end_rains", calculated_from=list("observations_unstacked_data"="s_doy"), save=2)
end_rains_date <- instatCalculations::instat_calculation$new(type="summary", function_exp="dplyr::if_else(condition=is.na(x=dplyr::last(x=roll_sum_rain)), true=as.Date(NA), false=dplyr::last(x=date))", result_name="end_rains_date", calculated_from=list("observations_unstacked_data"="date"), save=2)
end_rains_status <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(dplyr::n() > 0, yes=ifelse(is.na(x=dplyr::last(x=roll_sum_rain)), yes=NA, no=TRUE), no=FALSE)", result_name="end_rains_status", save=2)
end_of_rains_combined <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(conditions_filter, grouping_by_station_year, doy_filter), sub_calculations=list(end_rains, end_rains_date, end_rains_status))
data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_of_rains_combined)

linked_data_name <- data_book$get_linked_to_data_name("observations_unstacked_data", link_cols=c("s_year", "station_id"))


data_book$convert_column_to_type(data_name="observations_unstacked_data", col_names="s_year", to_type=year_type)

data_book$convert_column_to_type(data_name=linked_data_name, col_names="s_year", to_type=year_type)

data_book$remove_unused_station_year_combinations(data_name="observations_unstacked_data", year="s_year", station="station_id")

rm(list=c("end_of_rains_combined", "conditions_filter", "roll_sum_rain", "grouping_by_station_year", "doy_filter", "end_rains", "end_rains_date", "end_rains_status", "year_type", "station_type", "linked_data_name"))


saveRDS(file="C:/Users/lclem/OneDrive/Documents/MW_data.RDS", object=data_book)


#View(data_book$get_data_frame("observations_unstacked_data_by_station_id_s_year"))


# merging bits because we will now run the second half:
data_book$crops_definitions(data_name="observations_unstacked_data", year="s_year",
                            station="station_id", rain="PRECIP", day="s_doy",
                            # plant_days=seq(from = 100, to = 200, by = 10),
                            # plant_lengths=seq(from = 80, to = 180, by = 25),
                            # rain_totals=seq(from = 200, to = 1200, by = 100),
                            plant_days = seq(from = 0, to = 270, by = 5),
                            plant_lengths = seq(from = 45, to = 180, by = 5),
                            rain_totals = seq(from = 200, to = 1200, by = 25),
                            start_day="start_rain",
                            season_data_name="observations_unstacked_data_by_station_id_s_year",
                            start_check="both", end_day="end_rains")

saveRDS(file="C:/Users/lclem/OneDrive/Documents/MW_data.RDS", object=data_book)


epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")

observations_unstacked_data_by_station_id_s_year <- data_book$get_data_frame(data_name="observations_unstacked_data_by_station_id_s_year")

annual_rain <- epicsawrap::reformat_annual_summaries(data=observations_unstacked_data_by_station_id_s_year, station_col="station_id", year_col="s_year", start_rains_doy_col="start_rain", start_rains_date_col="start_rain_date", end_rains_doy_col="end_rains", end_rains_date_col="end_rains_date")
crop_prop <- data_book$get_data_frame(data_name="crop_prop")
crop_prop <- epicsawrap::reformat_crop_success(data=crop_prop, station_col="station_id", total_rain_col="rain_total", plant_day_col="plant_day", plant_length_col="plant_length",
                                               prop_success_with_start_col = "prop_success_with_start", prop_success_no_start_col = "prop_success_no_start")

epicsawrap::export_r_instat_to_bucket(summaries=c("annual_rainfall", "crop_success"),
                                      annual_rainfall_data=annual_rain,
                                      data_by_year="observations_unstacked_data_by_station_id_s_year",
                                      station="station_id",
                                      start_rains_column="start_rain",
                                      crop_data="crop_def",
                                      end_rains_column="end_rains",
                                      rain="PRECIP", year="s_year",
                                      crop_success_data=crop_prop,
                                      start_rains_status_column="start_rain_status",
                                      end_rains_status_column="end_rains_status",
                                      include_summary_data=TRUE, definitions_id="444",
                                      country="mw_test")


epicsawrap::get_definitions_data("mw_test", "12343000")

# crop prop
summaries_crop_success_probabilities_13331011_20250422222855 <- readRDS("C:/Users/lclem/Downloads/summaries_crop_success_probabilities_13331011_20250422222855.rds")

# check 444 is in the bucket OK