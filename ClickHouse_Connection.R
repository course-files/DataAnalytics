library(RClickhouse)
library(DBI)

# Establish the connection
con <- DBI::dbConnect(
  RClickhouse::clickhouse(),
  host = "localhost",
  port = 9001, # The default port is 9000 - Set as 9001 for the Lab
  user = "student",
  password = "5trathm0re",
  db = "classicmodels"
)

# Example query
payment_dataset_from_ClickHouse <- dbGetQuery(con, "
  SELECT *
  FROM payments;
")

# Print results
View(payment_dataset_from_ClickHouse)

# Close connection
dbDisconnect(con)
