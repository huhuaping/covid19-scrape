# Scrapes covid19 risk area from DXY and turns them into structured data for use by the TrentonTracker API

library(tidyverse)
library(RSelenium)
library(dplyr)
library(lubridate)

cat("Downloading covid19 risk area data...")


#==== start the RSelenium environment====
driver <- rsDriver(browser=c("firefox"),port = 4444L)
Sys.sleep(5)
rd <- driver[["client"]]
rd$setTimeout(type = 'page load', milliseconds = 20000) 
rd$maxWindowSize()


#==== scrape risk area =====
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



#====part 2 scrape covid19 daily=====

# send the url to the Firefox browser
url_area <- "https://ncov.dxy.cn/ncovh5/view/pneumonia"
rd$navigate(url_area)
Sys.sleep(5)

# get time public
xpath_target <- "*//div[contains(@class, 'main')]//span[contains(@class, 'dateTips')]"
time_raw <- rd$findElement(using =  'xpath', value = xpath_target)$getElementText() %>%
  unlist() %>%
  str_extract(., "(?<=截至北京时间)(.+)") %>%
  str_trim(., side = "both")

# access expand blocks
xpath_target <- "*//div[contains(@class,'areaBox___')][1]//div[contains(@class, 'expandBlock___')]"

elms_expandblock <- rd$findElements(using =  'xpath', value = xpath_target)
n_expandblock <- length(elms_expandblock)

cat("we find totaly", n_expandblock, "provinces collapsed \n", sep = " ")

# click arrow img to expand all provinces
xpath_target <- "div[contains(@class, 'areaBlock')]//p[contains(@class, 'subBlock1___')]//img"

## helper function
click_childElm <- function(node , xpath=xpath_target){
  elm <- node$findChildElement(using = 'xpath', value = xpath)
  elm$clickElement()
}
## click all 
sapply(elms_expandblock, click_childElm)
Sys.sleep(0.5)
cat("we now expand", n_expandblock, "provinces \n", sep = " ")


# all nodes areaBlock
xpath_target <- "*//div[contains(@class,'areaBox___')][1]//div[contains(@class, 'expandBlock___')]//div[contains(@class,'areaBlock')]"

elms_areablock <- rd$findElements(using =  'xpath', value = xpath_target)
n_areablock <- length(elms_areablock)
cat("It shows totally", n_areablock, " rows (provinces/cities) \n", sep = " ")

## step 1: get all areaBlock class name
class_areablock<- unlist(sapply(elms_areablock, function(x){x$getElementAttribute('class')})) %>%
  str_extract(., "(areaBlock\\d{1})")
Sys.sleep(0.5)
cat("step 1: get all areaBlock class name. \n", sep = " ")


## helper function 
get_childText <- function(nodes, xpath =xpath_target){
  elems <- sapply(nodes, function(x) x$findChildElements(using = "xpath", value = xpath))
  out <- unlist(sapply(elems, function(x) unlist(x$getElementText())))
}

## step 2: get all province or city names
xpath_target <- "p[contains(@class, 'subBlock1')]"
area_name <- get_childText(nodes = elms_areablock,xpath = xpath_target)
Sys.sleep(0.5)
cat("step 2: get all province or city names. \n", sep = " ")


## step 3: get yesterday new cases numbers
xpath_target <- "p[contains(@class, 'subBlock2')]"
cases_newadd <- get_childText(nodes = elms_areablock,xpath = xpath_target)
Sys.sleep(0.5)
cat("step 3: get yesterday new cases numbers. \n", sep = " ")


## step 4: get current covid19 cases numbers
xpath_target <- "p[contains(@class, 'subBlock3')]"
cases_current <- get_childText(nodes = elms_areablock,xpath = xpath_target)
Sys.sleep(0.5)
cat("step 4: get current covid19 cases numbers. \n", sep = " ")


## step 5: get risk area numbers
xpath_target <- "p[contains(@class, 'subBlock4')]"
risk_area <- get_childText(nodes = elms_areablock,xpath = xpath_target)
Sys.sleep(0.5)
cat("step 5: get risk area numbers. \n", sep = " ")


# construct tibble
tbl_cases <- tibble(area_block = class_areablock,
                    area_name = area_name,
                    cases_newadd = cases_newadd,
                    cases_current = cases_current,
                    risk_area = risk_area
                    ) %>%
  add_column(index = 1:nrow(.), .before = "area_block")


timestamp <- str_replace_all(
  as.character(lubridate::now(tzone = 'Asia/Shanghai')),
  " |:",
  "_")


# Generate URL for full text download EO-77.pdf
baseurl <- "data/rds-cases/area_cases_scraped_"
path_out  <- paste0(baseurl,timestamp,".rds")

# Write out the scraped data
write_rds(tbl_cases, path_out)

cat("Finnaly,  write data table out. \n", sep = " ")


#==== quit and release process====
rd$closeServer()
rd$close()
rm(rd)
#rm(driver)
#gc()