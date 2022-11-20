
#==== prepare sql ====

library(tidyverse)
#library(RSelenium)
library(dplyr)
library(lubridate)

library(DBI)
require(dbplyr)
require(RSQLite)

# create directory
dir.create("data/sql", showWarnings = FALSE)

# connect to database file
mydb <- dbConnect(RSQLite::SQLite(), "data/sql/covid19-dxy.db")

#==== create table case====
# demo table for SQL prepare, run only once!
file_path <- "data/sql/demo_case.rds"
df_case <- tbl_case %>%  head(1) 
write_rds(df_case, file_path)

#==== create table case_recent====
file_path <- "data/sql/demo_case_recent.rds"
df_case_recent <- tbl_case_recent %>%  head(1) 
write_rds(df_case_recent, file_path)

#==== create table risk====
# demo table for SQL prepare, run only once!
file_path <- "data/sql/demo_risk.rds"
df_risk <- tbl_risk %>%  head(1) 
write_rds(df_risk, file_path)

## create table
dbCreateTable(mydb, "case",df_case)
dbCreateTable(mydb, "case_recent",df_case_recent)
dbCreateTable(mydb, "area_risk",df_risk)
dbListTables(mydb)

# not run following, just for check
## add to my database
dbWriteTable(mydb, "area_risk", 
             tbl_risk,
             append=TRUE,overwite=FALSE)

## check it
check_out <- tbl(mydb, "case_recent") %>% 
  collect() %>%
  mutate_at(
    dplyr::vars(starts_with("cst_")),
    .funs = lubridate::as_datetime, 
    tz = "Asia/Shanghai"
  )

## clean check, remove table
dbRemoveTable(mydb, "case_recent")

# disconnect to my database
dbDisconnect(mydb)

