library(dplyr)
library(readr)


rm(list=ls());cat('\f');gc()

setwd("~/R/play/shiny_misc/shiny_nc.coastal.hazards")

# wd management----

wd <- list(data = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/data", 
           data.flood = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/data/nc_flood_2023_csvtables",
           data.groundwater = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/data/nc_groundwater_2023_csvtables",
           data.shoreline = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/data/nc_shorelin_change_2023_csvtables",
           metadata = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/metadata", 
           R = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/R", 
           output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/output", 
           shiny = NA, 
           home = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards")


# load data----

setwd(wd$data.flood)
flood_df.list <- list()
  for(i in list.files()){
    flood_df.list[[i]] <- read_csv(i)
  }


setwd(wd$data.shoreline)
shoreline_df.list <- list()
for(i in list.files()){
  shoreline_df.list[[i]] <- read_csv(i)
}
rm(i)

setwd(wd$metadata)
assets_codes <- read_csv("assets_codes.csv")

setwd(wd$home)


# explore data----