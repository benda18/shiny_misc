library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)
library(tigris)
library(sf)


rm(list=ls());cat('\f')
gc()

# Vars----

some.states <- c("North Carolina")


# wd----
wd <- {list(home   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade", 
            data   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/data", 
            output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/output",
            R      = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/R", 
            shiny  = NA, 
            cw     = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/crosswalks")
}

setwd(wd$R)

# load data----
setwd(wd$data)

load(list.files(pattern = "\\.RData$"))


usa.states <- tigris::states(cb = T)

some.counties <- tigris::counties(state = some.states, cb = T) %>% 
  .[,c("STATE_NAME", "STUSPS", 
       #"NAMELSADCO", 
       "NAMELSAD", "NAME")] %>% 
  mutate(., type = "county") %>% 
  st_centroid()

some.cosubdivs <- tigris::county_subdivisions(state = some.states, 
                                              cb = T) %>% 
  .[,c("STATE_NAME", "STUSPS", 
       #"NAMELSADCO", 
       "NAMELSAD", "NAME")] %>% 
  mutate(., type = "place") %>% 
  st_centroid()

some.places <- tigris::places(some.states, cb = T) %>% 
  .[,c("STATE_NAME", "STUSPS", 
       #"NAMELSADCO", 
       "NAMELSAD", "NAME")] %>% 
  mutate(., type = "place") %>% 
  st_centroid()

some.pl_subs <- rbind(some.places, some.cosubdivs) %>%
  rbind(., some.counties)
rm(some.places, some.cosubdivs, some.counties)

the.coords <- sf::st_coordinates(some.pl_subs)

some.pl_subs$lon <- the.coords[,"X"]
some.pl_subs$lat <- the.coords[,"Y"]

some.pl_subs <- some.pl_subs %>%
  st_drop_geometry() %>%
  as_tibble()

some.pl_subs[grepl("wilmington", some.pl_subs$NAME, ignore.case = T),]


# filter by state----
cw_specific_places[cw_specific_places$specific_region_country_or_colony %in% some.states,]
