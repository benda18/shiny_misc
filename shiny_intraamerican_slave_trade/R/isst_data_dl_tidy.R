library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(networkD3)
library(janitor)
library(lubridate)

"https://www.slavevoyages.org/american"
"https://r-spatial.org/projects/"


rm(list=ls());cat('\f')
gc()

# funs----

# wd----
wd <- {list(home = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade", 
           data = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/data", 
           output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/output",
           R = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/R", 
           shiny = NA, 
           cw = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/crosswalks")
}
setwd(wd$R)

# vars----
setwd(wd$data)
#data_ia.csv <- "https://www.slavevoyages.org/documents/download/I-Am1.0.csv"
data_ia.sav <- "https://www.slavevoyages.org/documents/download/I-Am1.0.sav"
data_ta.sav <- "https://www.slavevoyages.org/documents/download/tastdb-exp-2020.sav"

# download csv if necessary
if(!"I-Am1.0.sav" %in% list.files()){
  download.file(url = data_ia.sav, destfile = "I-Am10.sav")
  rm(data_ia.sav)
}
#
# if(!"I-Am1.0.csv" %in% list.files()){
#   download.file(url = data_ia.csv, destfile = "I-Am1.0.csv")
#   rm(data_ia.csv)
# }

if(!"tastdb-exp-2020.sav" %in% list.files()){
  download.file(url = data_ta.sav, destfile = "tastdb-exp-2020.sav")
  rm(data_ta.sav)
}

# load data----
#iast <- read_csv("I-Am1.0.csv")
iastsav <- haven::read_sav("I-Am1.0.sav")         # only works when i download direct from website
nastsav <- haven::read_sav("tastdb-exp-2020.sav") # only works when i download direct from website


# other datasets----
slave.dates <- {data.frame(geography = c("USA", "UK", 
                                   "Spain", "Netherlands", 
                                   "France", 
                                   "Venezuela", 
                                   "Canada", "Haiti"), 
                          date = ymd(c(18060101, 18070101, 
                                       18110101, 18140101, 
                                       18150101, 18170101, 
                                       18190101, 18220101)), 
                          date_desc = c("date_atlantic_slave_trade_abolished"))}

# crosswalks----
setwd(wd$cw)


if(! "cw_specific_places.csv" %in% list.files()){
  download.file(url = "https://raw.githubusercontent.com/benda18/shiny_misc/087c931c7775131ed0a1a3a86aef705be04065f4/shiny_intraamerican_slave_trade/crosswalks/cw_specific_places.csv", 
                destfile = "cw_specific_places.csv")
}

cw_specific_places <- read_csv(file = "cw_specific_places.csv")

setwd(wd$data)

# RData everyting----

iast_col.labels <- lapply(iastsav, attr, "label") %>% 
  as.data.frame() %>%
  t() %>%
  as.data.frame()
colnames(iast_col.labels) <- "col_def"
iast_col.labels$col.name <- rownames(iast_col.labels)
rownames(iast_col.labels) <- 1:nrow(iast_col.labels)
iast_col.labels <- iast_col.labels[,c("col.name", "col_def")]
iast_col.labels$source <- "iast"

nast_col.labels <- lapply(nastsav, attr, "label") %>% 
  as.data.frame() %>%
  t() %>%
  as.data.frame()
colnames(nast_col.labels) <- "col_def"
nast_col.labels$col.name <- rownames(nast_col.labels)
rownames(nast_col.labels) <- 1:nrow(nast_col.labels)
nast_col.labels <- nast_col.labels[,c("col.name", "col_def")]
nast_col.labels$source <- "nast"

cw_master.col.labels <- list("iast_col.labels" = iast_col.labels, 
                             "nast_col.labels" = nast_col.labels)
rm(iast_col.labels, nast_col.labels)

setwd(wd$data)
save(cw_master.col.labels, 
     iastsav, 
     nastsav, 
     slave.dates, 
     cw_specific_places,
     file = "master_st.RData")

rm(list=ls());cat('\f')
load("master_st.RData")

# explore data----
