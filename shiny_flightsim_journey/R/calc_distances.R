
# calculating distances bw locations
# airports
library(ggplot2)
library(janitor)
library(dplyr)
library(readr)
library(ggrepel)

rm(list=ls());cat('\f');gc()

setwd("~/R/play/shiny_misc/shiny_flightsim_journey")


wd <- list(data =   "~/R/play/shiny_misc/shiny_flightsim_journey/data", 
           output = "~/R/play/shiny_misc/shiny_flightsim_journey/output", 
           R =      "~/R/play/shiny_misc/shiny_flightsim_journey/R", 
           shiny =  "~/R/play/shiny_misc/shiny_flightsim_journey/shiny")

setwd(wd$output)

# load data----
load("airport_data.RData")

# Define the function
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  require(geosphere)
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

# new crosswalks----
cw_cont.country <- data_countries %>%
  group_by(continent, 
           iso_country, 
           country_name) %>%
  summarise()
cw_cont.country$continent <- factor(cw_cont.country$continent, 
                                    levels = c("NA", "SA", "EU", "AF", "AS", "OC", "AN"))
cw_cont.country$iso_country <- factor(cw_cont.country$iso_country, 
                                      levels = unique(cw_cont.country$iso_country[order(cw_cont.country$country_name)]))
cw_cont.country$country_name <- factor(cw_cont.country$country_name)

# filters----

# pick_continent

# pick_country
sort(unique(data_airports$iso_country))
data_countries
# pick_region
# pick distance

# max elevation

# min elevation

# start_ap
# end_ap



data_airports

save(list = ls(pattern = "^data_|^cw_"), 
     file = "airport_data.RData")
