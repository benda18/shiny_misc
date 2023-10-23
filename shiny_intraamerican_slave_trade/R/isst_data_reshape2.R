#isst_data_reshape

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)
library(purrr)

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

na.colname_defs <- nastsav %>%
  map_dfc(attr, "label") %>%
  t() %>%
  as.data.frame

na.colname_defs$def <- na.colname_defs$V1
na.colname_defs$colname <- rownames(na.colname_defs)
na.colname_defs <- na.colname_defs %>% as_tibble() %>%
  .[,c("colname", "def")]

ia.colname_defs <- iastsav %>%
  map_dfc(attr, "label") %>%
  t() %>%
  as.data.frame 

ia.colname_defs$def <- ia.colname_defs$V1
ia.colname_defs$colname <- rownames(ia.colname_defs)
ia.colname_defs <- as_tibble(ia.colname_defs) %>%
  .[,c("colname", "def")]

