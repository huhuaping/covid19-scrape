# Scrapes covid19 risk area from DXY and turns them into structured data for use by the TrentonTracker API
cat("load required pkgs...")
library(tidyverse)
library(dplyr)
require(magrittr)
require(lubridate)
library(RSelenium)
#require(XML)
require(httr)
require(rvest)
require(jsonlite)
library(DBI)
require(dbplyr)
require(RSQLite)


#==== Selenium headless ====
# Selenium driver with headless 
#driver <- rsDriver(browser=c("firefox"))
#rd <- driver[["client"]]
cat("start Selenium headless...")
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
#))
#rD <- rsDriver(browser=c("chrome"), verbose = TRUE, 
#               chromever="107.0.5304.62", 
#               port=4446L, 
#               extraCapabilities = eCaps) 

#rD <- RSelenium::rsDriver(
#  browser = "firefox",
#  extraCapabilities = list(
#   "moz:firefoxOptions" = list(
#      args = list('--headless')
#    )
#  )
#)
rD <- rsDriver(browser=c("firefox"),port = 4445L)
Sys.sleep(2)
rd <- rD[["client"]]
rd$setTimeout(type = 'page load', milliseconds = 20000) 
rd$maxWindowSize()

# navigate home page
cat("navigate home page...")
url_tar <- "https://ncov.dxy.cn/ncovh5/view/pneumonia"
rd$navigate(url_tar)
Sys.sleep(2)

## response header
cat("obtain access and modify datetime...")
r <- httr::GET(url_tar)
re_header <- httr::headers(r)
cst_modify <- re_header$`last-modified` %>%
  str_replace_all(., "(.+\\, )|( GMT)", "") %>%
  lubridate::parse_date_time(., orders = " dbY HMS", tz = "UCT") %>%
  with_tz(., tzone =  "Asia/Shanghai")
cst_access <- re_header$`date` %>%
  str_replace_all(., "(.+\\, )|( GMT)", "") %>%
  lubridate::parse_date_time(., orders = " dbY HMS", tz = "UCT") %>%
  with_tz(., tzone =  "Asia/Shanghai")
rm(r)

# rendered html page source
cat("obtian rendered html page source...")
pg <- rd$getPageSource()[[1]]
Sys.sleep(2)
docx <- read_html(pg)

# get the public cst date time
cat("get the public cst date time...")
xpath_tar <- "//span[starts-with(@class, 'dateTips')]"
#xpath_tar <-"//*[@id='root']/div/div[3]/div[5]/div[1]/div[2]/span"
cst_pub <- docx %>%
  html_nodes(xpath = xpath_tar) %>%
  html_text() %>%
  str_extract(., "(?<=截至北京时间)(.+)") %>%
  lubridate::parse_date_time(., orders = " Ymd HM", tz = "Asia/Shanghai") %>%
  with_tz(., tzone =  "Asia/Shanghai")

# quit selenium and release process
cat("quit selenium and release process...")
rd$closeServer()
rd$close()
rm(rd)
rm(rD)
gc()

# ==== help function ====
json_2tbl <- function(docs = docx, script = script_tar){
  xpath_tar <- paste0("//*[@id='",script,"']")
  txt <- docs %>%
    html_nodes(xpath = xpath_tar) %>%
    html_text()
  
  trim_first <- paste0("^try \\{ window.",
                       script," = ")
  trim_end <- "\\}catch\\(e\\)\\{\\}$"
  
  txt <- str_replace(txt, trim_first, "")
  txt <- str_replace(txt, trim_end, "")
  
  # str_extract(txt, "^.{10}")
  
  # to json and then to tibble
  tbl <- jsonlite::fromJSON(txt) %>%
    as_tibble() 
  return(tbl)
}

# ==== json to table ====
cat("convert json to table...")

# get table `getAreaStat`
script_tar <- "getAreaStat"
tbl_areaStat <- json_2tbl(docs = docx, script = script_tar) %>%
  arrange(locationId)

# get table `fetchRecentStatV2`
## do not use `fetchRecentStat`
script_tar <- "fetchRecentStatV2"
tbl_RecentStatV2 <- json_2tbl(docs = docx, script = script_tar) %>%
  arrange(locationId)

# ==== tidy table ====

## area cases
cat("tidy all tables: area cases")
tbl_case <- tbl_areaStat %>%
  select(-dangerAreas) %>%
  rename_with(., .fn = ~paste0("province_", .),
              .cols = all_of(ends_with("Count"))) %>%
  rename("province" = "provinceName",
         "province_Name" = "provinceShortName",
         "province_locationId" = "locationId") %>%
  rename("city" = "cities") %>%
  unnest(cols = city, names_sep = "_") %>%
  rename("city_Name" = "city_cityName") %>%
  pivot_longer(cols = all_of(contains("_")),
               names_to = c("area", ".value"), 
               names_sep = "_",
               ) %>%
  unique() %>%
  add_column(cst_access=cst_access, .before = "province")%>%
  add_column(cst_modify=cst_modify, .before = "province")%>%
  add_column(cst_pub=cst_pub, .before = "province")


## area risk
cat("tidy all tables: area risk")
tbl_risk <- tbl_areaStat %>%
  select(provinceName, provinceShortName,
    all_of(contains("Danger")),
    locationId, dangerAreas) %>%
  unnest(dangerAreas) %>%
  select(provinceName, provinceShortName, 
         locationId, everything()) %>%
  rename("province" = "provinceName",
         "province_Name" = "provinceShortName",
         "province_locationId" = "locationId") %>%
  add_column(cst_access=cst_access, .before = "province")%>%
  add_column(cst_modify=cst_modify, .before = "province")%>%
  add_column(cst_pub=cst_pub, .before = "province")

## area recent statistics
cat("tidy all tables: area recent statistics")
tbl_case_recent <- tbl_RecentStatV2 %>%
  rename_with(., .fn = ~paste0("province_", .),
              .cols = all_of(ends_with("Count"))) %>%
  rename("province" = "provinceName",
         "province_Name" = "provinceShortName",
         #"provincelocationId" = "locationId",
         "province_dangerCountIncr" = "dangerCountIncr") %>%
  rename("city" = "cities") %>%
  unnest(cols = city, names_sep = "_") %>%
  rename("city_Name" = "city_cityName") %>%
  select(-city_provinceName, -city_provinceShortName) %>%
  pivot_longer(cols = all_of(contains("_")),
               names_to = c("area", ".value"), 
               names_sep = "_",
  ) %>%
  unique() %>%
  add_column(cst_access=cst_access, .before = "province")%>%
  add_column(cst_modify=cst_modify, .before = "province")%>%
  add_column(cst_pub=cst_pub, .before = "province")

# ==== add to my database ====
cat("connect to mySQL...")
mydb <- dbConnect(RSQLite::SQLite(), 
                  "data/sql/covid19-dxy.db",
                  synchronous = NULL)

cat("write table append to mySQL...")
dbWriteTable(mydb, "case", 
             tbl_case,
             append=TRUE,overwite=FALSE)
Sys.sleep(0.5)
dbWriteTable(mydb, "area_risk", 
             tbl_risk,
             append=TRUE,overwite=FALSE)
Sys.sleep(0.5)
dbWriteTable(mydb, "case_recent", 
             tbl_case_recent,
             append=TRUE,overwite=FALSE)
Sys.sleep(0.5)
cat("disconnect to mySQL...")
dbDisconnect(mydb)

