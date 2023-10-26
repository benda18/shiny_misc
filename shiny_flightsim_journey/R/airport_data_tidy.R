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

# airport data from ourairports.com ----
# download airport data if not already in the .RData file
if(! "data_airports" %in% ls()){
  data_airports     <- read_csv("https://davidmegginson.github.io/ourairports-data/airports.csv", na = "")
  data_airport_freq <- read_csv("https://davidmegginson.github.io/ourairports-data/airport-frequencies.csv", na = "")
  data_runways      <- read_csv("https://davidmegginson.github.io/ourairports-data/runways.csv", na = "")
  data_navaids      <- read_csv("https://davidmegginson.github.io/ourairports-data/navaids.csv", na = "")
  data_countries    <- read_csv("https://davidmegginson.github.io/ourairports-data/countries.csv", na = "")
  data_regions      <- read_csv("https://davidmegginson.github.io/ourairports-data/regions.csv", na = "")
  
  
  
  # save
  save(list = ls(pattern = "^data_|^cw_"), 
       file = "airport_data.RData")
}


# Explore----

data_airports <- data_airports[,c("id", "ident", "type", "name", "latitude_deg", "longitude_deg", 
                 "elevation_ft", "continent", "iso_country", "iso_region", 
                 "municipality", "scheduled_service", 
                 "iata_code", "local_code")]
data_countries <- data_countries[,c("id", "code", "name", 
                                    "continent")]
colnames(data_countries) <- c("id", "iso_country", "country_name", "continent")

data_regions   <- data_regions[,c("id", "code", "local_code", 
                                  "name", "continent", "iso_country")]
colnames(data_regions) <- c("id", "iso_region", "local_code", "region_name", 
                            "continent", "iso_country")

save(list = ls(pattern = "^data_|^cw_"), 
     file = "airport_data.RData")



# airport layout----
a.apt <- "KS64"

#View(data_runways[data_runways$airport_ident %in% a.apt,])

data_runways[grepl("^H", data_runways$le_ident) &
               !grepl("^\\d", data_runways$airport_ident) & 
               !is.na(data_runways$le_longitude_deg) & 
               !is.na(data_runways$le_ident),]$airport_ident %>%
  unique() %>%
  .[nchar(.) == 4]

data_airport_freq[data_airport_freq$airport_ident %in% a.apt,] %>%
  .[order(.$type,.$description),c("airport_ident", "type", "description", "frequency_mhz")] %>%
  mutate(., frequency_mhz = as.character(round(frequency_mhz,2)))


ggplot(data = data_runways[data_runways$airport_ident %in% a.apt,]) +
geom_segment(aes(x = le_longitude_deg, xend = he_longitude_deg, 
               y = le_latitude_deg, yend = he_latitude_deg, 
               color = closed), 
               linewidth = 4) +
  geom_label_repel(aes(x = le_longitude_deg, y = le_latitude_deg, 
                 label = le_ident), 
             fontface = "bold", color = "black", 
             min.segment.length = 0, direction = "x")+
  geom_label_repel(aes(x = he_longitude_deg, y = he_latitude_deg, 
                 label = he_ident), 
             fontface = "bold", color = "black", 
             min.segment.length = 0, direction = "y") +
  geom_point(aes(x =  he_longitude_deg, y = he_latitude_deg))+
  geom_point(aes(x =  le_longitude_deg, y = le_latitude_deg))+
  coord_quickmap()+
  labs(title = a.apt) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
         legend.position = "bottom") #+
  # geom_point(data = data_navaids[data_navaids$associated_airport %in% a.apt & 
  #                                  !is.na(data_navaids$associated_airport),], 
  #            aes(x = longitude_deg, y = latitude_deg, color = type), 
  #           size = 3)

cw_navaid.type <- data.frame(type = c("DME", "NDB", "NDB-DME", 
                                      "TACAN", "VOR", "VOR-DME", 
                                      "VORTAC"), 
                             navaid_name = c("Distance Measuring Equipment", 
                                             "Nondirectional Radio Beacon", 
                                             "Nondirectional Radio Beacon - Distance Measuring Equipment", 
                                             "Tactacial Air Navigation", 
                                             "VHF Omni-directional Range", 
                                             "VHF Omni-directional Range - Distance Measuring Equipment", 
                                             "VHF Omni-directional Range/Tactical Air Navigation"))


# cleanup columnnames


