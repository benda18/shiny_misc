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



place.colnames <- c("ADPSALE1", "ADPSALE2", "ARRPORT", "ARRPORT2", "EMBPORT", "EMBPORT2",
                    "MAJBUYPT", "MAJSELPT", "MJBYPTIMP", "MJSLPTIMP", 
                    #"NATIONAL", "NATINIMP", 
                    "NPAFTTRA", 
                    "PLAC1TRA", "PLAC2TRA", "PLAC3TRA", 
                    #"PLACCONS", "PLACREG", 
                    "PORTDEP", "PORTRET", 
                    "PTDEPIMP", "SLA1PORT", "SLAARRIV", "VOYAGEID") %>% sort

pl_nast <- nastsav[colnames(nastsav) %in% place.colnames]
pl_iast <- iastsav[colnames(iastsav) %in% tolower(place.colnames)]


length(unique(pl_nast$VOYAGEID))
length(unique(pl_iast$voyageid))

attributes(pl_nast$PTDEPIMP)[1]


?apply

pl_nast$count_places_identified <- apply(X = as_tibble(as.data.frame(is.na(pl_nast[,2:ncol(pl_nast)]))), 
      MARGIN = 1, 
      FUN = sum, na.rm = T)
pl_iast$count_places_identified <- apply(X = as_tibble(as.data.frame(is.na(pl_iast[,2:ncol(pl_iast)]))), 
                                         MARGIN = 1, 
                                         FUN = sum, na.rm = T)


mean(pl_iast$count_places_identified)
sd(pl_iast$count_places_identified)

mean(pl_nast$count_places_identified)
sd(pl_nast$count_places_identified)

library(ggplot2)

attr(pl_nast$MAJBUYPT,"label")
haven::print_labels(pl_nast$MAJBUYPT) %>% as.data.frame() %>% head()
haven::as_factor(pl_nast$MAJBUYPT)
