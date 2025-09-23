library(databook)
library(instatExtras)

devtools::load_all()

data_book <- DataBook$new()

## Set up: Importing Mansa daily data already organised using R-Instat
mansa_daily <- readRDS("C:/Users/lclem/Downloads/mansa_daily.rds")
data_book$import_RDS(data_RDS = mansa_daily)
rm(mansa_daily)
data_book$get_data_names()

grouping <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("data_RDS"="s_year","data_RDS"="station"))
spells_calculation <- instatCalculations::instat_calculation$new(type="calculation", function_exp="instatClimatic::spells(x=(rain >= 10) & rain <= 50)", result_name="spell", calculated_from= list("data_RDS"="rain"), manipulations=list(grouping))
spells_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="dplyr::lead(c(NA,diff(spell)))<0", sub_calculations=list(spells_calculation), save=2, result_data_frame="spells")
data_book$run_instat_calculation(calc=spells_filter, display=FALSE)
rm(list=c("spells_filter", "spells_calculation", "grouping"))

spells <- data_book$get_data_frame("spells")

spells_data <- reformat_spells_data(spells, "station", "date", "spell")

epicsawrap::gcs_auth_file(filename="C:/Users/lclem/OneDrive/Documents/GitHub/epicsawrap1/tests/testthat/testdata/epicsa_token.json")

export_r_instat_to_bucket(station="station",
                          summaries=c("spells"),
                          spells_data = "spells",
                          spells_data_reformatted = spells_data,
                          include_summary_data=TRUE,
                          definitions_id="250909",
                          country="internal_tests")

head(data_book$get_data_frame("data_RDS"))