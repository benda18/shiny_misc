library(dplyr)
library(readr)
library(data.table)
library(lubridate)


rm(list=ls());cat('\f')

# Funs----




# working directory----


long.wd <- "C:\\Users\\bende\\Documents\\R\\play\\shiny_misc\\shiny_llc_shuffle"
short.wd <- "~/R/play/shiny_misc/shiny_llc_shuffle"
setwd(short.wd)

# ?path.expand()
# ?file.path()
# ?getwd()
# ?setwd()
# ?normalizePath()
# ?shortPathName()


# data source urls----
mc.delinquient.tax <- "https://go.mcohio.org/applications/treasurer/search/fdpopup.cfm?dtype=DQ"
mc.sales.annual    <- "https://go.mcohio.org/applications/treasurer/search/fdpopup.cfm?dtype=YS"


# download data---
# done manually


# Sales Data Tidy----
setwd("data")
files.res.sales <- list.files(pattern = "^SALES_\\d{4,4}_RES\\.csv$")

sales.res.2023 <- read_csv(file = grep(pattern = "2023", x = files.res.sales, value = T), 
                           #col_types = "c",
                           guess_max = min(c(10000, Inf)))

sales.res.2023$SALEDTE <- dmy(sales.res.2023$SALEDTE)
#sales.res.2023$CONVNUM <- as.numeric(sales.res.2023$CONVNUM)  #no NAs result but not ideal
sales.res.2023$PRICE   <- as.numeric(sales.res.2023$PRICE)
sales.res.2023$ACRES   <- as.numeric(sales.res.2023$ACRES)

sales.res.2023$SALETYPE %>% unique()
sales.res.2023$SALEVALIDITY %>% unique()

sales.res.2023$sale_valid <- grepl("^DESIGNATED|^VALID |^LAND CONTRACT|^LIQUIDATION|^PARTIAL|^RELATED|^Mobile |^SALE INVOLVING", 
                                   x = sales.res.2023$SALEVALIDITY)

sales.res.2023 %>%
  group_by(sale_valid,SALEVALIDITY ) %>%
  summarise()





grep(pattern = "OWN|NAME", 
     x = colnames(sales.res.2023), 
     value = T, 
     ignore.case = T)

grep(pattern = "deed|conv", 
     x = colnames(sales.res.2023), 
     value = T, 
     ignore.case = T)

grep(pattern = "sale", 
     x = colnames(sales.res.2023), 
     value = T, 
     ignore.case = T)

# Delinquent Tax Data Tidy----
files.deltax.all <- list.files(pattern = "^Delq_\\d{8,8}\\.csv$")

