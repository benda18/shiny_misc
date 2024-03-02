library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)

renv::snapshot()
renv::status()
#renv::restore()

# http://cran.r-project.org/web/packages/swephR/index.html

# Details----


# swe_sol_eclipse_when_loc()
# Find the next solar eclipse for a given geographic position.
# 
# swe_sol_eclipse_when_glob()
# Find the next solar eclipse on earth.
# 
# swe_sol_eclipse_how()
# Compute the attributes of a solar eclipse for a given time.
?swe_sol_eclipse_how 

#cle.eclipse.time <- base::julian.POSIXt(ymd_hms("2024-04-05 14:30:17", tz = "America/New_York"))
cle.eclipse.time <- swephR::swe_julday(year = 2024, 
                   month = 4, 
                   day = 8, 
                   hour = 14.5+4, 
                   gregflag = 1)
cle.eclipse.xyz  <- c(x = -81.44067000, 
                      y = 41.24006000, 
                      z = 0)



# swe_sol_eclipse_how(jd_ut = 1234580.19960447,
#                     ephe_flag = SE$FLG_MOSEPH,
#                     geopos = c(0,50,10))


cle_eclipse <- swe_sol_eclipse_how(cle.eclipse.time, 
                                   4, 
                                   cle.eclipse.xyz)


attr.defs <- "attr[0]   fraction of solar diameter covered by moon; with total/annular eclipses, it results in magnitude acc. to IMCCE.

attr[1]   ratio of lunar diameter to solar one

attr[2]   fraction of solar disc covered by moon (obscuration)

attr[3]   diameter of core shadow in km

attr[4]   azimuth of sun at tjd

attr[5]   true altitude of sun above horizon at tjd

attr[6]   apparent altitude of sun above horizon at tjd

attr[7]   elongation of moon in degrees

attr[8]   magnitude acc. to NASA; = attr[0/1] for partial and attr[1/2] for annular and total eclipses

attr[9]   saros series number (if available; otherwise -99999999) 

attr[10]  saros series member number (if available; otherwise -99999999) */" %>%
  strsplit(., 
         "\n\n") %>%
  unlist() %>%
  gsub("^attr\\[\\d{1,2}\\] {1,}", "", .)

attr.df <- data.frame(attr = c(cle_eclipse$attr), 
                      def = c(attr.defs, 
                              rep(NA, each = length(cle_eclipse$attr) - length(attr.defs)))) %>%
  as_tibble()

attr.df$def

hudson.grid <- expand.grid(lon = seq(-125,-66,by=1), 
                           lat = seq(25, 50, by = 1),
                           # lon = seq(cle.eclipse.xyz["x"] - 15,
                           #                                    cle.eclipse.xyz["x"] + 15, 
                           #                                    by = .5), 
                           #                          lat = seq(cle.eclipse.xyz["y"] - 15,
                           #                                    cle.eclipse.xyz["y"] + 15, 
                           #                                    by = .5), 
                           year = 2024, 
                           month = 4, 
                           day = 8, 
                           time = 4 + (14.75 + c(-1:1))) %>%
  mutate(., 
         jtime = NA, 
         obs_area = NA, 
         obs_dia = NA) %>%
  as_tibble()

nrow(hudson.grid)
hudson.grid

for(i in unique(hudson.grid$time)){
  hudson.grid$jtime[hudson.grid$time == i] <- swephR::swe_julday(year     = hudson.grid$year[first(which(hudson.grid$time == i))], 
                                                                 month    = hudson.grid$month[first(which(hudson.grid$time == i))], 
                                                                 day      = hudson.grid$day[first(which(hudson.grid$time == i))], 
                                                                 hour     = hudson.grid$time[first(which(hudson.grid$time == i))], 
                                                                 gregflag = 1)
}

for(i in 1:nrow(hudson.grid)){
  # hudson.grid$jtime[i] <- swephR::swe_julday(year     = hudson.grid$year[i], 
  #                                            month    = hudson.grid$month[i], 
  #                                            day      = hudson.grid$day[i], 
  #                                            hour     = hudson.grid$time[i], 
  #                                            gregflag = 1)
  temp <- swe_sol_eclipse_how(jd_ut     = hudson.grid$jtime[i], 
                              ephe_flag = 4,
                              geopos    = c(x = hudson.grid$lon[i], 
                                            y = hudson.grid$lat[i], 
                                            z = 10))$attr
  hudson.grid$obs_area[i] <- temp[3]
  hudson.grid$obs_dia[i]  <- temp[1]
}

var_oa <- .85

ggplot() + 
  geom_point(data = hudson.grid[hudson.grid$obs_area >= var_oa | 
                                  hudson.grid$obs_dia >= var_oa,], 
             size = 2,
             aes(x = lon, y = lat, 
                 color = obs_area)) + 
  coord_quickmap()+
  scale_size_area(trans = "sqrt")+
  scale_color_viridis_c(option = "C")+
  facet_grid(jtime~.)
