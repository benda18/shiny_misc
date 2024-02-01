library(dplyr)
library(readr)
library(data.table)
library(lubridate)
library(igraph)
library(renv)
library(ggplot2)

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

# Renv----
# https://rstudio.github.io/renv/articles/renv.html

# initialize a new project
# renv::init()

# data source urls----
# mc.delinquient.tax <- "https://go.mcohio.org/applications/treasurer/search/fdpopup.cfm?dtype=DQ"
# mc.sales.annual    <- "https://go.mcohio.org/applications/treasurer/search/fdpopup.cfm?dtype=YS"


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


sales.res.2023$sale_valid <- grepl("^DESIGNATED|^VALID |^LAND CONTRACT|^LIQUIDATION|^PARTIAL|^RELATED|^Mobile |^SALE INVOLVING", 
                                   x = sales.res.2023$SALEVALIDITY)

temp.order.owners <-  sales.res.2023[,c("OLDOWN", 
                                        "OWNERNAME1")] %>% 
  t %>% 
  as.data.frame() %>% 
  as.list.data.frame 

sales.res.2023$own1 <- lapply(temp.order.owners, sort) %>% lapply(., first) %>% unlist()
sales.res.2023$own2 <- lapply(temp.order.owners, sort) %>% lapply(., last) %>% unlist()

master_sales.23 <- sales.res.2023[,c("own1", 
                                     "own2",
                                     "PARID",
                                     "SALEDTE",
                                     "ACRES", "PRICE", "CLS", "SALETYPE", "SALEVALIDITY",
                                     "sale_valid")] %>%
  group_by_all() %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  #.[.$sale_valid,] %>%
  .[order(.$n,decreasing = T),] %>%
  mutate(., pardate_id = paste(PARID,SALEDTE)) 
# .[,c("own1", "own2",
#      "ACRES",
#      "sale_valid", "PARID", "SALEDTE"),]

rm(temp.order.owners)


# ASSOC: OWNER <--> OWNER ----

M_own2own <- master_sales.23 %>%
  group_by(own1, own2) %>%
  summarise()


M_own2parid <- rbind(summarise(group_by(master_sales.23, owner = own1, PARID)),
                     summarise(group_by(master_sales.23, owner = own2, PARID))) %>%
  group_by_all() %>%
  summarise()

# build graph
GR_own2own       <- graph_from_data_frame(d = M_own2own, directed = F)

igraph::gsize(GR_own2own)  # number of edges of graph

clusters_own2own <- clusters(GR_own2own)
groups_own2own   <- groups(clusters_own2own)

clusters_own2own$membership # name = owner, value = cluster_id
clusters_own2own$csize      # cluster_id number of members
clusters_own2own$no         # total number of clusters

CW_owner.clusterid <- data.frame(owner      = names(clusters_own2own$membership), 
                                 cluster_id = unname(clusters_own2own$membership)) %>% as_tibble()

M_clusterid_size  <- data.frame(cluster_id = 1:length(clusters_own2own$csize), 
                                n_owners   = clusters_own2own$csize) %>% as_tibble()


# temp1 <- igraph::clusters(GR_own2own) %>% 
#   groups() %>% 
#   #length() # tells you how many cluster groups
#   summary() %>% 
#   as.data.table() %>% as.data.frame() %>% as_tibble()
# 
# temp1$V2 %>% table
# temp1$V2 %>% unique
# temp1$V1 %>% table %>% unname %>% table
# temp1$V1 %>% as.numeric() %>% is.na() %>% table()
# temp1$V1 %>% summary
# temp1$V1 %>% as.numeric %>% summary
# temp1$V1 %>% as.numeric %>% fivenum
# temp1$V1 %>% length
# 
# temp1$N %>% as.numeric() %>% is.na() %>% table(., useNA = "always")
# temp1$N %>% as.numeric() %>% is.na() %>% as.numeric() %>% plot(type = "l")
# 
# temp1$N %>% unique
# 
# temp1 <- temp1 %>%
#   as.data.table(.) %>%
#   dcast(.,V1 ~ V2) %>%
#   as.data.frame() %>%
#   as_tibble() %>%
#   mutate(., 
#          cluster_id = as.numeric(V1), 
#          n_owners   = as.numeric(Length)) %>%
#   .[order(.$cluster_id),
#     c("cluster")]
# 
# temp1


# ASSOC:  OWNER <--> PROPERTY ----