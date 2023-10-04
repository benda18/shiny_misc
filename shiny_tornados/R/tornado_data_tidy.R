library(dplyr)
library(readr)

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



