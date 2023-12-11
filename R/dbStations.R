library(RPostgreSQL)

# Function to write data from CSV to PostgreSQL
writeCSVToPostgres <- function(filepath, con, table_name) {

  # Read data from CSV
  data <- read.csv(filepath)

  # Write data to PostgreSQL table
  DBI::dbWriteTable(conn = con, name = table_name, value = data, append = TRUE)

  # Close the database connection
  dbDisconnect(con)

  message("Data written to PostgreSQL table successfully!")
}

con = connectAQdb()


query_tables <- "SELECT table_name FROM information_schema.tables WHERE table_schema='public'"

tables <- dbGetQuery(con, query_tables)

print(tables)



table_name <- 'tbl_stations'

query_columns <- paste("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '", table_name, "'")

columns <- dbGetQuery(con, query_columns)

print(columns)
