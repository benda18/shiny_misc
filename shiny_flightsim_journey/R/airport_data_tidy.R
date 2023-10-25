# airports


library(dplyr)
library(readr)

rm(list=ls());cat('\f');gc()

setwd("~/R/play/shiny_misc/shiny_flightsim_journey")


wd <- list(data =   "~/R/play/shiny_misc/shiny_flightsim_journey/data", 
           output = "~/R/play/shiny_misc/shiny_flightsim_journey/output", 
           R =      "~/R/play/shiny_misc/shiny_flightsim_journey/R", 
           shiny =  "~/R/play/shiny_misc/shiny_flightsim_journey/shiny")

setwd(wd$output)

# load data----
load("airport_data.RData")

# airport data from ourairports.com ----
# download airport data if not already in the .RData file
if(! "data_airports" %in% ls()){
  data_airports     <- read_csv("https://davidmegginson.github.io/ourairports-data/airports.csv")
  data_airport_freq <- read_csv("https://davidmegginson.github.io/ourairports-data/airport-frequencies.csv")
  data_runways      <- read_csv("https://davidmegginson.github.io/ourairports-data/runways.csv")
  data_navaids      <- read_csv("https://davidmegginson.github.io/ourairports-data/navaids.csv")
  data_countries    <- read_csv("https://davidmegginson.github.io/ourairports-data/countries.csv")
  data_regions      <- read_csv("https://davidmegginson.github.io/ourairports-data/regions.csv")
  
  
  
  # save
  save(list = ls(pattern = "^data_"), 
       file = "airport_data.RData")
}





