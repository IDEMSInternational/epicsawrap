library(databook)
devtools::load_all()

data_book <- DataBook$new()

# Dialog: Import Dataset
new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/ghana_example_epicsa.RDS")
calculation <- instatCalculations::calculation
get_data_book_output_object_names <- databook:::get_data_book_output_object_names
get_data_book_scalar_names <- databook:::get_data_book_scalar_names
overall_label="[Overall]"
data_book$import_RDS(data_RDS=new_RDS)

rm(new_RDS)

# Dialog: Use Date
data_book$split_date(data_name="ghana", col_name="date", month_name=TRUE, s_start_month=1)

# Dialog: Column Summaries
data_book$calculate_summary(data_name="ghana", columns_to_summarise=c("min_temperature","max_temperature"), factors="month_name", store_results=TRUE, na.rm=TRUE, return_output=FALSE, na_type=c("'n_non_miss'"), j=1, na_min_n=10, summaries=c("summary_mean", "summary_min", "summary_max"), silent=TRUE)

# collate_definitions_data(data_by_year = "ghana_by_station_year",
#                          data_by_year_month = "ghana_by_month_name",
#                          summaries = c("annual_temperature", "monthly_temperature"),
#                          min_tmin_column = "min_min_temperature" ,
#                          mean_tmin_column = "mean_min_temperature", 
#                          max_tmin_column = "max_min_temperature",
#                          min_tmax_column = "min_max_temperature" ,
#                          mean_tmax_column = "mean_max_temperature", 
#                          max_tmax_column = "max_max_temperature",
#                          min_monthly_tmin_column = "min_min_temperature",
#                          mean_monthly_tmin_column = "mean_min_temperature",
#                          max_monthly_tmin_column = "max_min_temperature",
#                          min_monthly_tmax_column = "min_max_temperature",
#                          mean_monthly_tmax_column = "mean_max_temperature",
#                          max_monthly_tmax_column = "max_max_temperature")
# # 

epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")

export_r_instat_to_bucket(data_by_year = "ghana_by_station_year",
                         data_by_year_month = "ghana_by_month_name",
                         station = "station",
                         summaries = c("annual_temperature", "monthly_temperature"),
                         min_tmin_column = "min_min_temperature" ,
                         mean_tmin_column = "mean_min_temperature", 
                         max_tmin_column = "max_min_temperature",
                         min_tmax_column = "min_max_temperature" ,
                         mean_tmax_column = "mean_max_temperature", 
                         max_tmax_column = "max_max_temperature",
                         min_monthly_tmin_column = "min_min_temperature",
                         mean_monthly_tmin_column = "mean_min_temperature",
                         max_monthly_tmin_column = "max_min_temperature",
                         min_monthly_tmax_column = "min_max_temperature",
                         mean_monthly_tmax_column = "mean_max_temperature",
                         max_monthly_tmax_column = "max_max_temperature",
                         country = "internal_tests",
                         definitions_id = 250924)

devtools::load_all()
annual_temperature_summaries("internal_tests", "r_data_test_1")
monthly_temperature_summaries("internal_tests", "r_data_test_1")

annual_temperature_summaries("internal_tests", "Tamale")
monthly_temperature_summaries("internal_tests", "Saltpond")


# 
# annual_temp <- get_r_instat_definitions(data_book$get_calculations("ghana_by_station_year"))
# definitions_year_month <- get_r_instat_definitions(data_book$get_calculations("ghana_by_month_name"))
# 
# data_by_year = annual_temp
# data_by_year_month = definitions_year_month
# 
# 
# 
# build_block(min_monthly_tmin_column,
#             mean_monthly_tmin_column,
#             max_monthly_tmin_column,
#             "tmin",
#             data_by_year_month,
#             "monthly")
# 
# #
# 
# 
# temperature_summaries <- build_total_temperature_summaries(data_by_year = annual_temp,
#                                                            data_by_year_month = definitions_year_month,
#                                                            min_tmin_column = "min_min_temperature_column", 
#                                                            mean_tmin_column = "mean_min_temperature_column", 
#                                                            max_tmin_column = "max_min_temperature_column",
#                                                            min_tmax_column = "min_max_temperature_column", 
#                                                            mean_tmax_column = "mean_max_temperature_column", 
#                                                            max_tmax_column = "max_max_temperature_column",
#                                                            min_monthly_tmin_column = "min_min_temperature_column", 
#                                                            mean_monthly_tmin_column = "mean_min_temperature_column", 
#                                                            max_monthly_tmin_column = "max_min_temperature_column",
#                                                            min_monthly_tmax_column = "min_max_temperature_column", 
#                                                            mean_monthly_tmax_column = "mean_max_temperature_column", 
#                                                            max_monthly_tmax_column = "max_max_temperature_column")
# 
# 
# 
# 
