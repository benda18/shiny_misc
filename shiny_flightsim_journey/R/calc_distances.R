
# calculating distances bw locations
# airports

# read: https://r-spatial.org/r/2023/05/15/evolution4.html
# https://www.faa.gov/air_traffic/publications/atpubs/aim_html/chap1_section_1.html

library(ggplot2)
library(janitor)
library(dplyr)
library(readr)
library(ggrepel)
library(glue)
library(rnaturalearthdata)
library(sf)

rm(list=ls());cat('\f');gc()

setwd("~/R/play/shiny_misc/shiny_flightsim_journey")


wd <- list(data =   "~/R/play/shiny_misc/shiny_flightsim_journey/data", 
           output = "~/R/play/shiny_misc/shiny_flightsim_journey/output", 
           R =      "~/R/play/shiny_misc/shiny_flightsim_journey/R", 
           shiny =  "~/R/play/shiny_misc/shiny_flightsim_journey/shiny/flight_sim_cyoa")

setwd(wd$output)

# load data----
load("airport_data.RData")

# setwd(wd$shiny)
# save(cw_cont.country, 
#      data_airports, 
#      data_runways, 
#      file = "shinyData.RData")

# Define the function
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  require(geosphere)
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

# # new crosswalks----
# cw_cont.country <- data_countries %>%
#   group_by(continent, 
#            iso_country, 
#            country_name) %>%
#   summarise()
# cw_cont.country$continent <- factor(cw_cont.country$continent, 
#                                     levels = c("NA", "SA", "EU", "AF", "AS", "OC", "AN"))
# cw_cont.country$iso_country <- factor(cw_cont.country$iso_country, 
#                                       levels = unique(cw_cont.country$iso_country[order(cw_cont.country$country_name)]))
# cw_cont.country$country_name <- factor(cw_cont.country$country_name)

# filters----

# pick_continent

# pick_country

# pick_region
# pick distance

# max elevation

# min elevation

# start_ap
# end_ap

# runway types

# not closed
data_airports <- data_airports[!data_airports$type %in% "closed",]
# 4-dig codes
data_airports <- data_airports[nchar(data_airports$ident) == 4,]
data_airports[grepl(pattern = "\\D{4,4}", data_airports$ident),]

# select two airports----

data_airports$elevation_ft %>% range(., na.rm = T)
data_runways$length_ft %>% range(., na.rm = T)

table_surfaces <- data_runways$surface %>% tolower %>% table %>%
  as.matrix() %>%
  as.data.frame() 
table_surfaces$type <- rownames(table_surfaces)
table_surfaces <- as_tibble(table_surfaces)
colnames(table_surfaces) <- c("count", "type")

table_surfaces <- table_surfaces[order(table_surfaces$count,decreasing = T),]

table_surfaces[substr(table_surfaces$type, 1,1) == "a",]

table_surfaces$asph.concr <- table_surfaces$type %>% grepl("^asp|-asp|^con|-con", .)
table_surfaces[grepl("wat", table_surfaces$type),]


cw_surfaces <- data.frame(surface = NA, 
                          surftype = c("Paved", "Unpaved", "Water", "Helipad"))

table_surfaces$type <- table_surfaces$type %>%
  gsub("^\\'|\\'$", "", .) 



valid.apts <- data_runways[!is.na(data_runways$le_longitude_deg) & 
                             !is.na(data_runways$le_latitude_deg) & 
                             !is.na(data_runways$he_longitude_deg) & 
                             !is.na(data_runways$he_latitude_deg),] %>%
  group_by(airport_ident) %>%
  summarise() %>%
  .$airport_ident

valid.apts <- data_airports[data_airports$ident %in% valid.apts & 
                !is.na(data_airports$iata_code),]$ident

some.apts <- sample(valid.apts,size=2,replace=F)
#some.apts <- c("EGAC", "EIKY")


rw.metadata <- data_runways[data_runways$airport_ident %in% some.apts,] %>%
  group_by(airport_ident) %>%
  slice_max(., order_by = length_ft, n = 1) %>%
  group_by(ident = airport_ident, 
           surface, 
           length_ft, 
           elevation = le_elevation_ft) %>%
  summarise()



ggplot(data = data_airports[data_airports$ident %in% some.apts,]) + 
  geom_sf(data = sf::st_as_sf(rnaturalearthdata::countries110), 
          fill = "white", color = "grey")+
  # geom_sf(data = sf::st_as_sf(rnaturalearthdata::coastline110), 
  #         color = "grey")+
  geom_path(aes(x = longitude_deg, y = latitude_deg)) +
  geom_label_repel(min.segment.length = 0,
                   aes(x = longitude_deg, y = latitude_deg, 
                 label = ident))+
  geom_point(aes(x = longitude_deg, y = latitude_deg))+
  #coord_sf() +
  theme(panel.background = element_rect(fill = "powderblue"), 
        plot.background = element_rect(fill = "light grey"), 
        strip.background =  element_rect("red"), 
        axis.line = element_blank(), 
        panel.grid = element_line(color = "skyblue", linetype = 23))+
  labs(title = paste(data_airports[data_airports$ident %in% some.apts,]$name, 
                      collapse = " >>> "), 
  subtitle = glue("Elevation: {paste(scales::comma(rw.metadata$elevation), \"ft\",  collapse = \", \")}
  Runway Length: {paste(scales::comma(rw.metadata$length_ft),  \"ft\", collapse = \", \")}
  Runway Surface: {paste(rw.metadata$surface,collapse = \", \")}
  Flight Distance: {scales::comma(calculate_distance(lon1 = data_airports$longitude_deg[data_airports$ident == some.apts[1]], 
                   lat1 = data_airports$latitude_deg[data_airports$ident == some.apts[1]], 
                   lon2 = data_airports$longitude_deg[data_airports$ident == some.apts[2]], 
                   lat2 = data_airports$latitude_deg[data_airports$ident == some.apts[2]])/1609.34)} miles\n"))

