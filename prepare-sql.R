
#==== prepare sql ====

library(tidyverse)
library(RSelenium)
library(dplyr)
library(lubridate)

library(DBI)
require(dbplyr)
require(RSQLite)

# create directory
dir.create("data/sql", showWarnings = FALSE)

# connect to database file
mydb <- dbConnect(RSQLite::SQLite(), "data/sql/covid19-dxy.db")

#==== create table area_risk====
## df NA
## paste0("'",names(area_risk_scraped),"'", collapse = ", ")
names_tar<- c('index_full', 'i', 'j', 
              'index', 'rank_raw', 'time_raw',
              'province', 'city', 'unit', 
              'rank_type', 'rank_n', 'time_public', 
              'time_stamp')
df_na <- as_tibble(matrix(rep(NA,length(names_tar)),nrow = 1),
                   .name_repair = "unique") %>%
  rename_all(., ~all_of(names_tar))

## create table
dbCreateTable(mydb, "area_risk",df_na)


#==== create table area_case ====
## df NA
names_tar<- c('index', 'area_block', 'area_name', 
              'cases_newadd', 'cases_current', 
              'risk_area',
              'time_raw', 'time_stamp')
df_na <- as_tibble(matrix(rep(NA,length(names_tar)),nrow = 1),
                   .name_repair = "unique") %>%
  rename_all(., ~all_of(names_tar)) %>%
  mutate_at(vars(contains("cases_")), as.character)

## create table
dbCreateTable(mydb, "area_case",df_na)



#==== test and check====
timestamp <- lubridate::now(tzone = 'Asia/Shanghai')
tbl_tem <- area_cases_scraped %>%
  mutate(
    time_raw = "time raw text",
    time_stamp = timestamp)

## add to my database
dbWriteTable(mydb, "area_case", 
             tbl_tem,
             append=TRUE,overwite=FALSE)



# show me the database table
check_out<- tbl(mydb, "area_risk") %>% 
  as_tibble() %>%
  #tail() %>%
  mutate(time_public = lubridate::as_datetime(time_public,
                                              tz='Asia/Shanghai'),
         time_stamp = lubridate::as_datetime(time_stamp,
                                             tz='Asia/Shanghai'))

check_out<- dbReadTable(mydb, "area_case") %>%
  #tail() %>%
  mutate(#time_public = lubridate::as_datetime(time_public,
    #                                     tz='Asia/Shanghai'),
    time_stamp = lubridate::as_datetime(time_stamp,
                                        tz='Asia/Shanghai'))

## remove table
dbRemoveTable(mydb, "area_risk")

# list all tables
dbListTables(mydb)

# disconnect to my database
dbDisconnect(mydb)
