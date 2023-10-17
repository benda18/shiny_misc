library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)
#library(tigris)
#library(sf)


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

not.cn.ptrns <- c("^ADLT\\d", 
                  "^ADULT\\d{1,1}$", 
                  "^BOY\\d{1,1}$", 
                  "^BOYRAT\\d{1,}$", 
                  "^CHIL\\d{1,1}IMP$",
                  "^CHILD\\d{1,}$", 
                  "^CHILRAT\\d{1,}$",
                  "^CREW\\d{1,}$",
                  "^DATARR\\d{1,}$", 
                  "^DATELAND\\d{1,}$", 
                  "^DATELEFTAFR$", 
                  "^DDEP", 
                  "^DLSLA", "^EVGREEN$", 
                  "^FEMALE", 
                  "^FEML", 
                  "^GIRL\\d{1,}$", 
                  "^GIRLRAT", 
                  "^INFANT", 
                  "^MALE\\d{1,}$|^MALE\\d{1,}IMP$", 
                  "^MALRAT", 
                  "^MEN\\d{1,}$", 
                  "^MENRAT", 
                  "^NCAR", 
                  "^NPPRETRA$", 
                  "^NPPRIOR$", 
                  "^SAILD\\d{1,}$", 
                  "^SLAS\\d{1,}$", 
                  "^SLADAMER", 
                  "^SLAV.*\\d{1,}$", 
                  "^WOMEN\\d{1,}$")

place.colnames <- c("ADPSALE1", "ADPSALE2", "ARRPORT", "ARRPORT2", "EMBPORT", "EMBPORT2",
                    "MAJBUYPT", "MAJSELPT", "MJBYPTIMP", "MJSLPTIMP", "NATINIMP", 
                    "NATIONAL", "NPAFTTRA", 
                    "PLAC1TRA", "PLAC2TRA", "PLAC3TRA", 
                    "PLACCONS", "PLACREG", 
                    "PORTDEP", "PORTRET", 
                    "PTDEPIMP", "SLA1PORT", "SLAARRIV") %>% sort

as_tibble(cw_master.col.labels$nast_col.labels) %>%
  #.[grepl("place|port|location| at | in | to |landed|arrived|depart|arriv", .$col_def, ignore.case= T),] %>%
  .[grepl("place|port|location", .$col_def, ignore.case= T),] %>%
  .[order(.$col.name),] %>%
  .[!grepl(pattern = paste0(not.cn.ptrns, collapse = "|"), 
           x = .$col.name),] %>%
  .[!.$col.name %in% place.colnames,]

