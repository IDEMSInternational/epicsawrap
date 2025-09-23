library(tidyverse)
devtools::load_all()

epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")
id_1 <- "250909.20250909172322"
id_2 <- "250909.20250917154932"
x <- get_definitions_data(country = "internal_tests",
                          station_id = c("MANSA MET"),
                          definitions_id = "250909",
                          file = id_1)
x$spells

# Call definitinos 
list_definition_versions("internal_tests", "250909")


get_definitions_data(country = "internal_tests",
                     station_id = c("MANSA MET"),
                     definitions_id = "250909",
                     file = id_1)


# Calling the data itself from the bucket

get_summaries_data("internal_tests", "MANSA MET", "annual_rainfall_summaries")
