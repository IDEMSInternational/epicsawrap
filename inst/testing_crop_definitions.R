library(databook)

setwd("~/GitHub/epicsawrap_master")
devtools::load_all()
# Initialising R (e.g Loading R packages)

setwd(dir="C:/Users/lclem/source/repos/RInstat/instat/static/InstatObject/R")

source(file="Rsetup.R")

data_book <- DataBook$new()

# Setting display options (e.g Number of significant digits)
options(digits=4, show.signif.stars=FALSE, dplyr.summarise.inform=FALSE, R.commands.displayed.in.the.output.window=TRUE, Comments.from.dialogs.displayed.in.the.output.window=TRUE)

# Dialog: Import Dataset
new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/ghana_example_epicsa.RDS")
data_book$import_RDS(data_RDS=new_RDS)

rm(new_RDS)

# Dialog: Delete Data Frames
data_book$delete_dataframes(data_names=c("crop_prop","crop_def"))

# Dialog: PICSA Crops
groups <- dplyr::groups
group_by <- dplyr::group_by

data_book$crops_definitions(data_name="ghana", year="year", station="station", rain="rainfall", day="doy",
                            plant_days=c(100, 150, 200),
                            plant_lengths=c(110, 220),
                            rain_totals=c(370, 400, 500), start_day="start_rain", season_data_name="ghana_by_station_year", end_day="end_rains", start_check="both",
                            display_start_probabilities = TRUE)
#data_book$crops_definitions(data_name="ghana", year="year", station="station", rain="rainfall", day="doy", plant_days=c(100, 110, 120, 130, 140), plant_lengths=c(110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160), rain_totals=c(370, 380, 390, 400, 410, 420, 430, 440, 450, 460, 470, 480, 490, 500), start_day="start_rain", season_data_name="ghana_by_station_year", end_day="end_rains", start_check="both")
#data_book$crops_definitions(data_name="ghana", year="year", station="station", rain="rainfall", day="doy", plant_days=c(100, 110), plant_lengths=c(110, 150), rain_totals=c(370, 500), start_day="start_rain", season_data_name="ghana_by_station_year", end_day="end_rains", start_check="both")
#data_book$crops_definitions(data_name="ghana", year="year", station="station", rain="rainfall", day="doy", plant_days=c(100), plant_lengths=c(110), rain_totals=c(370), start_day="start_rain", season_data_name="ghana_by_station_year", end_day="end_rains", start_check="both")

data_book$get_data_names()

# View(data_book$get_data_frame("crop_def"))
# View(data_book$get_data_frame("crop_prop"))

epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")
crop_prop <- data_book$get_data_frame(data_name="crop_prop")
#crop_prop$hsj <- crop_prop$plant_day
#crop_prop$plant_day <- NULL
crop_prop <- reformat_crop_success(data=crop_prop, station_col="station", total_rain_col="rain_total", plant_day_col="plant_day", plant_length_col="plant_length", prop_success_with_start_col = "prop_success_with_start", prop_success_no_start_col = "prop_success_no_start")
crop_def <- data_book$get_data_frame(data_name="crop_def")
crop_def <- reformat_season_start(data=crop_def, station_col="station", year_col="year", plant_day_col="plant_day", plant_day_cond_col="plant_day_cond")
ghana_by_station_year <- data_book$get_data_frame(data_name="ghana_by_station_year")

# we should give crop_def for the season_start_probabilties. that needs station, year, plant day, and plant day condition. 

setwd("C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap_master")
devtools::load_all()
# exported_data <- export_r_instat_to_bucket(data_by_year = "ghana_by_station_year",
#   summaries=c("crop_success", "start_season"),
#   station = "station",
#   crop_data = "crop_def",
#   #crop_data_name = "crop_def",
#   end_rains_column="end_rains",
#   rain="rainfall", year="year",
#   crop_success_data = crop_prop,
#   season_start_data=crop_def,
#   include_summary_data=TRUE, definitions_id="000", country="internal_tests")
# rm(list=c("crop_prop", "crop_def"))

devtools::load_all()

# reading in season start data too:
export_r_instat_to_bucket(data_by_year = "ghana_by_station_year",
                          summaries=c("start_season"),
                          station="station", 
                          crop_data="crop_prop",
                          year="year",
                          season_start_data=crop_def,
                          definitions_id="88888",
                          country="internal_tests",
                          include_summary_data = TRUE)
# TODO: data isn't uploading for our season_start_data.
# TODO: if I upload both crop_success and start_season, then it works well :) 
