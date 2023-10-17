#isst_data_reshape

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)

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

cw_master.col.labels <- cw_master.col.labels %>% lapply(., as_tibble)

cw_master.col.labels$iast_col.labels$col_cat <- NA
cw_master.col.labels$nast_col.labels$col_cat <- NA

cw_master.col.labels$iast_col.labels[grepl("date|^month |^year |^Day", cw_master.col.labels$iast_col.labels$col_def, ignore.case = T),]$col_cat <- "date"
cw_master.col.labels$nast_col.labels[grepl("date|^month |^year |^Day", cw_master.col.labels$nast_col.labels$col_def, ignore.case = T),]$col_cat <- "date"

cw_master.col.labels$iast_col.labels[grepl("place|port|location",cw_master.col.labels$iast_col.labels$col_def, 
                                           ignore.case = T) & 
                                       is.na(cw_master.col.labels$iast_col.labels$col_cat),] <- "place"
cw_master.col.labels$nast_col.labels[grepl("place|port|location",cw_master.col.labels$nast_col.labels$col_def, 
                                           ignore.case = T) & 
                                       is.na(cw_master.col.labels$nast_col.labels$col_cat),] <- "place"

cw_master.col.labels$iast_col.labels$imputed <- ifelse(grepl(pattern = "Imputed", cw_master.col.labels$iast_col.labels$col_def), T, F)
cw_master.col.labels$nast_col.labels$imputed <- ifelse(grepl(pattern = "Imputed", cw_master.col.labels$nast_col.labels$col_def), T, F)
