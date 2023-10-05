library(dplyr)
library(readr)
library(ggmap)
library(ggplot2)
library(lubridate)


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

# read csv----
allT    <- read_csv("1950-2022_all_tornadoes.csv")
actualT <- read_csv("1950-2022_actual_tornadoes.csv")


# tidy csv----
lower48 <- actualT[!actualT$st %in% c("HI", "AK"),] %>%
  .[.$yr >= 2008,] %>%
  .[.$elat != 0,] %>%
  .[.$elon != 0,]

bbox.states <- c("KS")
st.map <- ggmap::get_stamenmap(bbox = c(left   = min(c(lower48$elon[lower48$st %in% bbox.states], 
                                                       lower48$slon[lower48$st %in% bbox.states])), 
                                        bottom = min(c(lower48$elat[lower48$st %in% bbox.states], 
                                                       lower48$slat[lower48$st %in% bbox.states])), 
                                        right  = max(c(lower48$elon[lower48$st %in% bbox.states], 
                                                       lower48$slon[lower48$st %in% bbox.states])), 
                                        top    = max(c(lower48$elat[lower48$st %in% bbox.states], 
                                                       lower48$slat[lower48$st %in% bbox.states]))), 
                     zoom = 6, 
                     maptype = "terrain")

ggplot() + 
  geom_segment(data = lower48[!lower48$elon == 0 &
                             !lower48$elat == 0 & 
                               lower48$st %in% bbox.states,], 
             aes(x = slon, xend = elon, 
                 y = slat, yend = elat, 
                 linewidth = wid))+
  coord_quickmap()

allT[allT$slon == 0,]
