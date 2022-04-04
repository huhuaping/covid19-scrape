# Scrapes covid19 risk area from DXY and turns them into structured data for use by the TrentonTracker API

library(tidyverse)
library(RSelenium)
library(dplyr)
library(lubridate)

cat("Downloading covid19 risk area data...")


# We start the RSelenium environment
driver <- rsDriver(browser=c("firefox"),port = 4444L)
Sys.sleep(5)
rd <- driver[["client"]]
rd$maxWindowSize()

# send the url to the Firefox browser
url_area <- "https://ncov.dxy.cn/ncovh5/view/pneumonia_risks?from=dxy&link=&share=&source="
rd$navigate(url_area)
Sys.sleep(5)

# access risk zone
xpath_target <- "*//div[contains(@class,'riskZone___')]"
elms_zone <- rd$findElements(using =  'xpath', value = xpath_target)
n_zone <- length(elms_zone)

# loop risk zones
# i <- 1
tbl_risk <- NULL
for (i in 1:n_zone) {
  
  elm_zone <- elms_zone[[i]]
  
  # get subtitle
  xpath_target <- "*//div[contains(@class, 'subtitle___')]"
  subtitle <- elm_zone$findChildElement('xpath', xpath_target)$getElementText() %>%
    unlist()
  
  if (subtitle !='') {
    # get time
    xpath_target <- "*//div[contains(@class, 'time___')]"
    time_public <- elm_zone$findChildElement('xpath', xpath_target)$getElementText() %>%
      unlist()  
    
    # access block div
    xpath_target <- "div[contains(@class, 'block___')]"
    elms_block <- elm_zone$findChildElements(using = 'xpath', value = xpath_target)
    n_block <- length(elms_block)
    
    cat('i=',i,subtitle,'has', n_block,'provinces \n', sep = " ")
    
    
    # loop for blocks
    # j <- 2
    for (j in 1:n_block) {
      elm_block <- elms_block[[j]]
      
      # access province div
      xpath_target <- "*//div[contains(@class, 'center___')]" # do not use 'placeholder__'
      province <- elm_block$findChildElement('xpath', xpath_target)$getElementText() %>%
        unlist()
      
      
      
      # access blockline > city
      xpath_target <- "*//div[contains(@class, 'blockLine___')]//div[contains(@class, 'city__')]"
      elm_cities <- elm_block$findChildElements('xpath', xpath_target)
      cities <- sapply(elm_cities, function(x) x$getElementText() %>% unlist())
      
      # access blockline > units
      xpath_target <- "*//div[contains(@class, 'blockLine___')]//div[2]"
      elm_units <- elm_block$findChildElements('xpath', xpath_target)
      units <- sapply(elm_units, function(x) x$getElementText() %>% unlist())
      
      cat("j=",j," has units ", length(units),'\n', sep ="")
      
      
      # Construct a data frame
      tbl_raw <- tibble(
        i =i, j =j,
        rank_raw = subtitle,
        time_raw = time_public,
        province = province,
        city =cities,
        unit = units
      ) %>%
        add_column(index = 1:nrow(.), .before = "rank_raw") 
      
      
      tbl_risk <- bind_rows(tbl_risk, tbl_raw)
    } # end loop blocks
    
  } # end if
  
  
  
} # end loop zones


# Construct a data frame from the scraped table, parse the dates
tbl_clean <- tbl_risk %>%
  mutate(rank_type = str_extract(rank_raw, "(.+)(?=地区)"),
         rank_n = as.numeric(str_extract(rank_raw, "(?<=地区)(\\d{1,4})")),
         time_public = lubridate::ymd_hm(
           str_extract(time_raw, "(?<=截至北京时间 )(.+)"),
           tz = "GMT"
         )) %>%
  add_column(index_full = 1:nrow(.), .before = 'i')
  
timestamp <- str_replace_all(
  as.character(lubridate::now(tzone = 'Asia/Shanghai')),
  " |:",
  "_")



# Generate URL for full text download EO-77.pdf
baseurl <- "data/rds/area_risk_scraped_"
path_out  <- paste0(baseurl,timestamp,".rds")

# Write out the scraped data
write_rds(tbl_clean, path_out)


# quit and release process
rd$closeServer()
rd$close()
rm(rd)
#rm(driver)
#gc()