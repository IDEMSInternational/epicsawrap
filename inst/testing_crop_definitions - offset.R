
data_book <- DataBook$new()
x <- readRDS(file="C:/Users/lclem/OneDrive/Documents/Zambia_data.RDS")
data_book$import_RDS(data_RDS = x)

epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")

observations_unstacked_data_by_station_id_s_year <- data_book$get_data_frame(data_name="observations_unstacked_data_by_station_id_s_year")

annual_rain <- epicsawrap::reformat_annual_summaries(data=observations_unstacked_data_by_station_id_s_year, station_col="station_id", year_col="s_year", start_rains_doy_col="start_rain", start_rains_date_col="start_rain_date", end_rains_doy_col="end_rains", end_rains_date_col="end_rains_date")
crop_prop <- data_book$get_data_frame(data_name="crop_prop")
crop_prop <- epicsawrap::reformat_crop_success(data=crop_prop, station_col="station_id", total_rain_col="rain_total", plant_day_col="plant_day", plant_length_col="plant_length",
                                               prop_success_with_start_col = "prop_success_with_start", prop_success_no_start_col = "prop_success_no_start")

x <- epicsawrap::export_r_instat_to_bucket(summaries=c("annual_rainfall", "crop_success"),
                                      annual_rainfall_data=annual_rain,
                                      data_by_year="observations_unstacked_data_by_station_id_s_year",
                                      station="station_id",
                                      start_rains_column="start_rain",
                                      crop_data="crop_def",
                                      end_rains_column="end_rains",
                                      rain="PRECIP",
                                      crop_success_data=crop_prop,
                                      start_rains_status_column="start_rain_status",
                                      end_rains_status_column="end_rains_status",
                                      include_summary_data=TRUE, definitions_id="444",
                                      country="mw_test")


epicsawrap::get_definitions_data("mw_test", "12343000")

# crop prop
summaries_crop_success_probabilities_13331011_20250422222855 <- readRDS("C:/Users/lclem/Downloads/summaries_crop_success_probabilities_13331011_20250422222855.rds")

# check 444 is in the bucket OK


