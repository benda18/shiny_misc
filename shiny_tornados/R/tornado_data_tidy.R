library(dplyr)
library(readr)
library(ggmap)
library(ggplot2)
library(lubridate)
library(tigris)

# https://www.spc.noaa.gov/wcm/#data
rm(list=ls())

setwd("~/R/play/shiny_misc/shiny_tornados")

all.subdirs <- list.dirs( recursive = F) %>%
  gsub("\\.", getwd(), .)

# data_url----

url_allT_1950.2022    <- "https://www.spc.noaa.gov/wcm/data/1950-2022_all_tornadoes.csv"
url_actualT_1950.2022 <- "https://www.spc.noaa.gov/wcm/data/1950-2022_actual_tornadoes.csv"

# check if data is already downloaded----
setwd(grep("/data$", all.subdirs, ignore.case = F, value = T))
allT.dl <- "1950-2022_all_tornadoes.csv" %in% list.files()
actualT.dl <- "1950-2022_actual_tornadoes.csv" %in% list.files()

# if data is not already downloaded, download it----
if(!allT.dl){
  print("not DL yet")
  download.file(url = url_allT_1950.2022, 
                destfile = "1950-2022_all_tornadoes.csv")
  
}

if(!actualT.dl){
  download.file(url = url_actualT_1950.2022, 
                destfile = "1950-2022_actual_tornadoes.csv")
}

#  import tornado data----
from.yr <- 1990

allT    <- read_csv("1950-2022_all_tornadoes.csv") %>%
  .[.$yr >= from.yr,]
actualT <- read_csv("1950-2022_actual_tornadoes.csv") %>%
  .[.$yr >= from.yr,]


# import census data----
tigris_st_geo <- tigris::states(cb = T, year = 2021) %>%
  .[!.$STUSPS %in% c("PR", "VI", "HI", "AK", 
                     "AS", "GU", "MP"),] %>%
  sf::st_simplify() %>%
  .[!colnames(.) %in% c("ALAND", "AWATER", "LSAD", "AFFGEOID", "STATE_NAME")]
tigris_co_geo <- tigris::counties(cb = T) %>%
  .[!.$STUSPS %in% c("PR", "VI", "HI", "AK", 
                     "AS", "GU", "MP"),] %>%
  sf::st_simplify() %>%
  .[!colnames(.) %in% c("ALAND", "AWATER", "LSAD", "AFFGEOID", "STATE_NAME")]

# save(tigris_st_geo, 
#      tigris_co_geo, 
#      allT, 
#      actualT,
#      file = "tornado.RData")

# save to shiny dir
setwd("~/R/play/shiny_misc/shiny_tornados/shiny/shiny_tornadoes")
save(tigris_st_geo, 
     tigris_co_geo, 
     allT, 
     actualT,
     file = "tornado.RData")




# shiny outline----

# USA Map showing all tornadoes (mapped) by 
# * map        (plot) - SERVER - 
# * length     (size)  
# * width      (linewidth)
# * state      (filter)
# * county     (filter)
# * year       (filter)
# * magnitude  (color/fill / filter)
# * injuries   (color/fill / summary table)
# * fatalities (color/fill / summary table)
# * econ loss  (color/fill / summary table)
# * crop loss  (color/fill / summary table)