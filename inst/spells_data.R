library(databook)
library(instatExtras)

devtools::load_all()

data_book <- DataBook$new()

new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/ghana_example_epicsa.RDS")
data_book$import_RDS(data_RDS=new_RDS)

grouping <- instatCalculations::instat_calculation$new(type="by", calculated_from=list("ghana"="year","ghana"="station"))
spells_calculation <- instatCalculations::instat_calculation$new(type="calculation", function_exp="instatClimatic::spells(x=(rainfall >= 0) & rainfall <= 0.85)", result_name="spell", calculated_from= list("ghana"="rainfall"), manipulations=list(grouping))
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



