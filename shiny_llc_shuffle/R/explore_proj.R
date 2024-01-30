library(dplyr)
library(readr)
library(data.table)


rm(list=ls());cat('\f')

# Funs----
fun_dl.re.sales <- function(sale.yr){
  require(data.table)
  require(glue)
  # data source for ref: 
  # https://go.mcohio.org/applications/treasurer/search/filedownloads.cfm
  cat("SOURCE: https://go.mcohio.org/applications/treasurer/search/filedownloads.cfm\n\n")
  # error handling----
  sale.yr <- as.character(sale.yr)
  # must be 4 dig &  must be between 2023 and 2011
  if(nchar(sale.yr) != 4 |  
     !data.table::between(as.numeric(sale.yr), 2011, 2023)){
    stop("ERROR: [sale.yr] must be between 2011 and 2023")
  }
  # generate url----
  return(glue("https://go.mcohio.org/applications/treasurer/search/data/Yearly/SALES_{sale.yr}.zip"))
}




# working directory----
long.wd <- "C:\\Users\\bende\\Documents\\R\\play\\shiny_misc\\shiny_llc_shuffle"
short.wd <- "~/R/play/shiny_misc/shiny_llc_shuffle"

# ?path.expand()
# ?file.path()
# ?getwd()
# ?setwd()
# ?normalizePath()
# ?shortPathName()


# data source urls----
mc.delinquient.tax <- "https://go.mcohio.org/applications/treasurer/search/fdpopup.cfm?dtype=DQ"
mc.sales.annual    <- "https://go.mcohio.org/applications/treasurer/search/fdpopup.cfm?dtype=YS"


# DOWNLOAD DATA----
# dl real estate sales






