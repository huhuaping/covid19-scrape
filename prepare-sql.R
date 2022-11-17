
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

#==== create table area_risk====

## set date time
cst_now <- now(tzone = "Asia/Shanghai") 
gmt_now <- with_tz(cst_now, tzone = "UCT")
num_now <- as.numeric(gmt_now)
par_now <- floor(num_now/60) # 

## generate url
url_json <- paste0(
  "https://file1.dxycdn.com/2021/0202/196/1680100273140422643-135.json?t=",
  par_now)

## request
r <- httr::GET(url_json)
re_header <- httr::headers(r)

## important information
num8 <- as.numeric(str_extract(url_json,"(\\d{8}$)"))
cst_date <- re_header$date %>%
  str_replace_all(., "(.+\\, )|( GMT)", "") %>%
  lubridate::parse_date_time(., orders = " dbY HMS", tz = "UCT") %>%
  with_tz(., tzone =  "Asia/Shanghai")
cst_modify <- re_header$`last-modified` %>%
  str_replace_all(., "(.+\\, )|( GMT)", "") %>%
  lubridate::parse_date_time(., orders = " dbY HMS", tz = "UCT") %>%
  with_tz(., tzone =  "Asia/Shanghai")

## to html and text
docs <- read_html(r) %>%
  html_text(.) %>%
  # to json
  jsonlite::fromJSON(.) 

# to tibble
tbl_risk <- docs %>%
  .$data %>%
  unnest(dangerPros) %>%
  unnest(dangerAreas)  %>%
  # add identification information
  add_column(cst_date = cst_date, .before = "dangerCount") %>%
  add_column(cst_modify = cst_modify, .before = "dangerCount")%>%
  add_column(num8 = num8, .before = "dangerCount") %>%
  add_column(index = 1:nrow(.), .before = "dangerCount")

# demo table for SQL prepare, run only once!
file_path <- "data/sql/demo_risk.rds"
df_risk <- tbl_risk %>%  head(1) 
write_rds(df, file_path)

## create table
dbCreateTable(mydb, "area_risk",df_risk)
dbListTables(mydb)

# not run following, just for check
## add to my database
dbWriteTable(mydb, "area_risk", 
             tbl_risk,
             append=TRUE,overwite=FALSE)

## check it
check_out <- tbl(mydb, "area_risk") %>% 
  collect() %>%
  mutate_at(
    dplyr::vars(starts_with("cst_")),
    .funs = lubridate::as_datetime, 
    tz = "Asia/Shanghai"
  )

## clean check, remove table
dbRemoveTable(mydb, "area_risk")

#==== create table area_case ====
## df NA
names_tar<- c('index', 'area_block', 'area_name', 
              'cases_newadd',"cases_nonsense", 'cases_current', 
              'risk_area',
              'time_raw', 'time_stamp')
df_na <- as_tibble(matrix(rep(NA,length(names_tar)),nrow = 1),
                   .name_repair = "unique") %>%
  rename_all(., ~all_of(names_tar)) %>%
  mutate_at(vars(contains("cases_")), as.character)

## create table
dbCreateTable(mydb, "area_case_nonsense",df_na)



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
  collect() %>%
  mutate(cst_date = as_datetime(cst_date, tz = "Asia/Shanghai")) %>%
  mutate(cst_modify = as_datetime(cst_modify, tz = "Asia/Shanghai"))

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

#====daily inspection=====

# connect to database file
mydb <- dbConnect(RSQLite::SQLite(), "data/sql/covid19-dxy.db")

dbListTables(mydb)

check_out <- dbReadTable(mydb, "area_risk") %>%
  #tail() %>%
  mutate(#time_public = lubridate::as_datetime(time_public,
    #                                     tz='Asia/Shanghai'),
    time_stamp = lubridate::as_datetime(time_stamp,
                                        tz='Asia/Shanghai'))

# disconnect to my database
dbDisconnect(mydb)
