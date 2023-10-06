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

tigris_reg_geo <- tigris::regions(year = 2021) %>%
  sf::st_simplify() %>%
  .[!colnames(.) %in% c("ALAND", "AWATER", "LSAD", "AFFGEOID", "STATE_NAME")]

cw_magnitude <- data.frame(mag = c(-9,0:5), 
                           def = c(-1, 0:5))

list("ME", "VT", "RI", 
     "CT", "NY", "MA", 
     "PA", "NJ", "NH") %>%
  .[order(unlist(.))]

reg_st_list <- list("Northeast" = relist(flesh = sort(c("ME", "VT", "RI", 
                                                        "CT", "NY", "MA", 
                                                        "PA", "NJ", "NH")), 
                                         skeleton = list("ME", "VT", "RI", 
                                                         "CT", "NY", "MA", 
                                                         "PA", "NJ", "NH")), 
                    "West"      = relist(flesh = sort(c("WA", #"AK", "HI",
                                                        "OR", "CA", "NV", 
                                                        "ID", "MT", "WY", 
                                                        "UT", "CO", "AZ", 
                                                        "NM")),
                                         list("WA", #"AK", "HI",
                                              "OR", "CA", "NV", 
                                              "ID", "MT", "WY", 
                                              "UT", "CO", "AZ", 
                                              "NM")), 
                    "Midwest"   = relist(flesh = sort(c("OH", "MI", "IN", 
                                                        "IL", "MO", "NE", 
                                                        "IA", "WI", "MN",
                                                        "ND", "SD", "KS")),
                                         list("OH", "MI", "IN", 
                                              "IL", "MO", "NE", 
                                              "IA", "WI", "MN",
                                              "ND", "SD", "KS")), 
                    "South"     = relist(flesh = sort(c("TX", "OK", "AR", 
                                                        "LA", "MS", "AL", 
                                                        "GA", "FL", "MD",
                                                        "TN", "SC", "NC", 
                                                        "KY", "WV", "VA", 
                                                        "DE", "DC")),
                                         list("TX", "OK", "AR", 
                                              "LA", "MS", "AL", 
                                              "GA", "FL", "MD",
                                              "TN", "SC", "NC", 
                                              "KY", "WV", "VA", 
                                              "DE", "DC")))



st_co_df <- tigris_co_geo[,c("STUSPS", "NAME", "COUNTYFP")] %>%
  sf::st_drop_geometry() 
colnames(st_co_df)[2] <- "COUNTY"

st_co_df <- st_co_df %>%
  .[order(.$STUSPS, .$COUNTY),]
rownames(st_co_df) <- 1:nrow(st_co_df)

# save to shiny dir
setwd("~/R/play/shiny_misc/shiny_tornados/shiny/shiny_tornadoes")
save(tigris_st_geo, 
     tigris_co_geo, 
     #allT, 
     actualT,
     cw_magnitude,
     reg_st_list,
     st_co_df,
     file = "tornado.RData")



ggplot() + 
  geom_sf(data = left_join(tigris_st_geo[!tigris_st_geo$STUSPS %in% 
                          c("PR", "VI", "HI", "AK", 
                            "AS", "GU", "MP"),], 
          cw_tigris_reg, by = "STUSPS"), 
          aes(fill = NAME.y))

ggplot() + 
  geom_sf(data =tigris_st_geo[!tigris_st_geo$STUSPS %in% 
                                c("PR", "VI", "HI", "AK", 
                                  "AS", "GU", "MP"),])+
  geom_sf(data = tigris_reg_geo[4,],
          alpha = 0.2,
          color = NA, fill = "red")



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