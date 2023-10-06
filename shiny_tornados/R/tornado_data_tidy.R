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



cw_co_fips <- tigris_co_geo[,c("STUSPS", "NAME", "COUNTYFP")] %>%
  sf::st_drop_geometry() 
colnames(cw_co_fips)[2] <- "COUNTY"

cw_co_fips <- cw_co_fips %>%
  .[order(.$STUSPS, .$COUNTY),] %>%
  mutate(., 
         COUNTYFP = as.numeric(COUNTYFP))
rownames(cw_co_fips) <- 1:nrow(cw_co_fips)

# make a df of om <---> f1/f2/f3/f4

tor.st_co_yr.F1 <- actualT %>%
  group_by(om,yr,st,f1) %>%
  summarise() %>%
  ungroup() %>%
  left_join(., 
            cw_co_fips, 
            by = c("st" = "STUSPS", 
                   "f1" = "COUNTYFP")) %>%
  .[!is.na(.$COUNTY),]

tor.st_co_yr.F2 <- actualT %>%
  group_by(om,yr,st,f2) %>%
  summarise() %>%
  ungroup() %>%
  left_join(., 
            cw_co_fips, 
            by = c("st" = "STUSPS", 
                   "f2" = "COUNTYFP")) %>%
  .[!is.na(.$COUNTY),]

tor.st_co_yr.F3 <- actualT %>%
  group_by(om,yr,st,f3) %>%
  summarise() %>%
  ungroup() %>%
  left_join(., 
            cw_co_fips, 
            by = c("st" = "STUSPS", 
                   "f3" = "COUNTYFP")) %>%
  .[!is.na(.$COUNTY),]

tor.st_co_yr.F4 <- actualT %>%
  group_by(om,yr,st,f4) %>%
  summarise() %>%
  ungroup() %>%
  left_join(., 
            cw_co_fips, 
            by = c("st" = "STUSPS", 
                   "f4" = "COUNTYFP")) %>%
  .[!is.na(.$COUNTY),]

colnames(tor.st_co_yr.F1)[4] <- "co_fips"
colnames(tor.st_co_yr.F2)[4] <- "co_fips"
colnames(tor.st_co_yr.F3)[4] <- "co_fips"
colnames(tor.st_co_yr.F4)[4] <- "co_fips"

tor.st_co_yr <- rbind(tor.st_co_yr.F1, 
                      tor.st_co_yr.F2, 
                      tor.st_co_yr.F3, 
                      tor.st_co_yr.F4) %>%
  group_by_all() %>%
  summarise()



rm(tor.st_co_yr.F1, 
   tor.st_co_yr.F2, 
   tor.st_co_yr.F3, 
   tor.st_co_yr.F4)

# save to shiny dir
setwd("~/R/play/shiny_misc/shiny_tornados/shiny/shiny_tornadoes")
save(tigris_st_geo, 
     tigris_co_geo, 
     #allT, 
     actualT,
     cw_magnitude,
     reg_st_list,
     #st_co_df,
     tor.st_co_yr,
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