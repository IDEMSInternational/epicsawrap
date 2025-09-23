library(databook)
library(tidyverse)
devtools::load_all()
epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")

data_book <- DataBook$new()

# Dialog: Import Dataset
mansa_daily <- readRDS("C:/Users/lclem/Downloads/mansa_daily.rds")
data_book$import_RDS(data_RDS = mansa_daily)

data_book$get_data_names()

ghana2 <- data_book$get_data_frame(data_name="data_RDS")
update_station_metadata(metadata_data = ghana2,
                     station_var = "station",
                     country = "internal_test")

x <- get_station_metadata(country = "mw",
                 include_definitions = FALSE)

jsonlite::toJSON(x)





# Import Station Metadata from Bucket
x <- get_station_metadata(country = "mw",
                      include_definitions = FALSE)
