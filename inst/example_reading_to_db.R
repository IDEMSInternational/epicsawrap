# Collate Summaries
devtools::load_all()
library(databook)
data_book <- DataBook$new()

# Dialog: Import Dataset
#new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/GitHub/databook_master/data/summary_data_binded.RDS")

new_RDS <- readRDS(file="C:/Users/lclem/OneDrive/Documents/GitHub/databook_master/data/summary_data_binded.RDS")

#data_book$import_RDS(data_RDS=new_RDS)

# Export to Postgres Database:
summary_data <- new_RDS$summary
definition_data <- new_RDS$definition
summary_station_metadata <- new_RDS$summary_station_metadata
crop_data <- new_RDS$crop

# Write to database
export_to_database(con = con,
             #definition_data = definition_data,
             #summary_station_metadata = summary_station_metadata,
             #summary_data = summary_data)
             crop_data = crop_data)

# send up station metadata

# TODO: station_id in summary_station_metadata to be the station_id
Zambia_eastern_station_info <- rio::import(file="C:/Program Files/R-Instat/R-Instat 0.8.14.131/static/Library/Climatic/Zambia/Zambia_eastern_station_info.csv", stringsAsFactors=TRUE)
data_book$import_data(data_tables=list(Zambia_eastern_station_info=Zambia_eastern_station_info))

zambia_metadata <- data_book$get_data_frame("Zambia_eastern_station_info")
stat_met <- collate_station_metadata(data = zambia_metadata,
                                     station_id = "Station_ID",
                                     station_name = "Station_Name",
                                     longitude = "Longitude",
                                     latitude = "Latitude",
                                     elevation = "Elevation",
                                     district = "District")
stat_met

# TODO 4/4: Test the export function below
export_to_database(con = con,
                   station_metadata = stat_met)








# Error: Failed to fetch row : ERROR:  SAVEPOINT can only be used in transaction blocks



# 1. Nuclear reconnect
dbDisconnect(con)
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = "db.epicsa.idems.international",
  dbname   = "epicsa",
  user     = "idems_lily",
  password = "SocgYDCqU-JCxNvj4UFKj",
  port     = 5432
)
# 2. Check you're starting clean
dbGetInfo(con)  # just to confirm connection is live

# 3. Verify no transaction is already open
dbExecute(con, "ROLLBACK")  # harmless if nothing open, clears it if something is
#WARNING:  there is no transaction in progress
#[1] 0
# 4. Now try
dbBegin(con)
dbGetQuery(con, "SELECT pg_current_xact_id_if_assigned()")  
#pg_current_xact_id_if_assigned
#1                           <NA>





#

dbDisconnect(con)
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = "db.epicsa.idems.international",
  dbname   = "epicsa",
  user     = "idems_lily",
  password = "SocgYDCqU-JCxNvj4UFKj",
  port     = 5432
)
dbExecute(con, "BEGIN")

tryCatch({
  chunk_size <- 1000
  for (i in seq(1, nrow(summary_data), by = chunk_size)) {
    chunk <- summary_data[i:min(i + chunk_size - 1, nrow(summary_data)), ]
    dbWriteTable(con, "summary", chunk, append = TRUE, row.names = FALSE)
  }
  dbExecute(con, "COMMIT")
}, error = function(e) {
  dbExecute(con, "ROLLBACK")
  stop(e)
})
# WARNING:  there is already a transaction in progress
# 
# WARNING:  there is no transaction in progress

summary_data <- summary_data[1:10,]
dbWriteTable(
  conn      = con,
  name      = "summary",          # table name in postgres
  value     = summary_data,
  append    = TRUE,               # append to existing table
  row.names = FALSE
)

#############


# send up station
# 1. If it already exists, then we need to REPLACE? / or do we throw an ERROR? I don't know.
dbWriteTable(
  conn      = con,
  name      = "station",          # table name in postgres
  value     = station,
  append    = TRUE,               # append to existing table
  row.names = FALSE
)







x <- DBI::dbReadTable(con, "station")

df <- data.frame(station_id = "dodoma", station_name = "dodoma", latitude = 0, longitude = 0,
                 elevation = 0, district = "test", country_code = "TZ")
dbWriteTable(
  conn      = con,
  name      = "station",          # table name in postgres
  value     = df,
  append    = TRUE,               # append to existing table
  row.names = FALSE
)

# station metadata data frame

# 1. Calls the current station metadata from the place

# 2. If     station/station_id are in there already, it updates them to the new values given
#    Else   add a new column with that information in it
# station
# station_id		VARCHAR(255) primary key
# station_name		VARCHAR(255) 	
# latitude			NUMERIC 
# longitude		NUMERIC 
# elevation		NUMERIC 
# district			VARCHAR(255) 	
# country_code		VARCHAR(10) 	

# 3. Export to DB.



