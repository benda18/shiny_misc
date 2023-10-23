#isst_data_reshape

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)

#https://stackoverflow.com/questions/57552015/how-to-extract-column-variable-attributes-labels-from-r-to-csv-or-excel

rm(list=ls());cat('\f')
gc()

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


# explore----

iastsav <- iastsav %>% haven::as_factor()
nastsav <- nastsav %>% haven::as_factor()




class(iastsav$rig)
class(iastsav[,"rig"] %>% unlist())
identical(iastsav$rig, 
          unname(unlist(iastsav[,"rig"])))


select(iastsav, "rig")
?select
