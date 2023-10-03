library(dplyr)
library(readr)


rm(list=ls());cat('\f');gc()

setwd("~/R/play/shiny_misc/shiny_nc.coastal.hazards")

# wd management----

wd.list <- list(data = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/data", 
                metadata = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/metadata", 
                R = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/R", 
                output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_nc.coastal.hazards/output", 
                shiny = NA)
