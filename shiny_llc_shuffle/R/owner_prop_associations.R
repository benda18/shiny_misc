library(dplyr)
library(readr)
library(data.table)
library(lubridate)
library(igraph)
library(renv)
library(ggplot2)

rm(list=ls());cat('\f')

renv::status()
# renv::snapshot()

# Funs----
?adist
?agrep


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

rm(files.res.sales)

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
  .[.$sale_valid,] %>%
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

M_own2pardate_id <- rbind(summarise(group_by(master_sales.23, owner = own1, pardate_id)),
                          summarise(group_by(master_sales.23, owner = own2, pardate_id))) %>%
  group_by_all() %>%
  summarise()

# build graph
GR_own2own       <- graph_from_data_frame(d = M_own2own, directed = F) %>% simplify()
GR_own2parid     <- graph_from_data_frame(d = M_own2parid, directed = F) %>% simplify()
GR_own2pardate_id <- graph_from_data_frame(d = M_own2pardate_id, directed = F) %>% simplify()

# igraph::gsize(GR_own2own)  # number of edges of graph
# igraph::gsize(GR_own2parid)
# igraph::gsize(GR_own2pardate_id)

clusters_own2own        <- clusters(GR_own2own)
clusters_own2parid      <- clusters(GR_own2parid)
clusters_own2pardate_id <- clusters(GR_own2pardate_id)


# clusters_own2own$membership # name = owner, value = cluster_id
# clusters_own2own$csize      # cluster_id number of members
# clusters_own2own$no         # total number of clusters

CW_owner.clusterid <- data.frame(owner      = names(clusters_own2own$membership), 
                                 cluster_id = as.integer(unname(clusters_own2own$membership))) %>% 
  as_tibble() %>%
  mutate(., 
         cluster_id =  paste("o2o", cluster_id, sep = "_"))

CW_parid.clusterid       <- data.frame(owner      = names(clusters_own2parid$membership), 
                                       cluster_id = as.integer(unname(clusters_own2parid$membership))) %>% 
  as_tibble() %>%
  mutate(., 
         cluster_id =  paste("o2p", cluster_id, sep = "_"))

CW_pardate_id.clusterid  <- data.frame(owner      = names(clusters_own2pardate_id$membership), 
                                       cluster_id = as.integer(unname(clusters_own2pardate_id$membership))) %>% 
  as_tibble() %>%
  mutate(., 
         cluster_id =  paste("o2pd", cluster_id, sep = "_"))

M_clusterid_size.o2o  <- data.frame(cluster_id = as.integer(1:length(clusters_own2own$csize)), 
                                    n_o2o   = clusters_own2own$csize) %>% 
  as_tibble()  %>%
  mutate(., 
         cluster_id =  paste("o2o", cluster_id, sep = "_")) %>% 
  as.data.table() %>%
  melt(., id.vars = "cluster_id") %>%
  as.data.frame() %>%
  as_tibble()

M_clusterid_size.o2p  <- data.frame(cluster_id = as.integer(1:length(clusters_own2parid$csize)), 
                                    n_o2p   = clusters_own2parid$csize) %>% 
  as_tibble() %>%
  mutate(., 
         cluster_id =  paste("o2p", cluster_id, sep = "_")) %>% 
  as.data.table() %>%
  melt(., id.vars = "cluster_id") %>%
  as.data.frame() %>%
  as_tibble()

M_clusterid_size.o2pd  <- data.frame(cluster_id = as.integer(1:length(clusters_own2pardate_id$csize)), 
                                     n_o2pd   = clusters_own2pardate_id$csize) %>% 
  as_tibble() %>%
  mutate(., 
         cluster_id =  paste("o2pd", cluster_id, sep = "_")) %>% 
  as.data.table() %>%
  melt(., id.vars = "cluster_id") %>%
  as.data.frame() %>%
  as_tibble()




#rm(clusters_own2own, clusters_own2parid, clusters_own2pardate_id)

M_clusterid_size <- rbind(M_clusterid_size.o2o, 
                          M_clusterid_size.o2p, 
                          M_clusterid_size.o2pd) 

rm(M_clusterid_size.o2o, 
   M_clusterid_size.o2p, 
   M_clusterid_size.o2pd)

# review
CW_owner.clusterid
CW_parid.clusterid
CW_pardate_id.clusterid

M_clusterid_size 

M_own2own
M_own2parid
M_own2pardate_id

#master_sales.23

# Find clusters with LLCs in them----
# set var_ptrn_llc ----
var_ptrn_llc <- "LLC$|LTD$| LLC AND"


LLC_clustersA <- CW_owner.clusterid %>%
  .[grepl(var_ptrn_llc, .$owner),] %>%
  .$cluster_id %>%
  unique() 

LLC_clustersB <- CW_parid.clusterid %>%
  .[grepl(var_ptrn_llc, .$owner),] %>%
  .$cluster_id %>%
  unique() 

LLC_clustersC <- CW_pardate_id.clusterid %>%
  .[grepl(var_ptrn_llc, .$owner),] %>%
  .$cluster_id %>%
  unique() 


LLC_clusters <- c(LLC_clustersA, LLC_clustersB, LLC_clustersC) %>% unique()
rm(LLC_clustersA, LLC_clustersB, LLC_clustersC)


# Find Clusters ----
# big random - o2o
ls()
grep("^\\w{1,3}_", ls(), ignore.case = T, value = T) 

M_clusterid_size


# SELECT DOWN TO OWNERS YOU WANT TO GRAPH----
# set var_topn ----
var_topn <- 3

var_explore.further <- c("LINKO PROPERTIES LLC", 
                         "AUTUMN GROVE INVESTMENTS LLC",
                         "LEWIS AND LEWIS PROPERTY MANAGEMENT LLC", 
                         "AFFORDABLE HOUSES LLC", 
                         "BEE INTENTIONAL LIMITED LLC", 
                         "MADINA PROPERTY MANAGEMENT LLC", 
                         "NW INVESTMENT PROPERTIES", 
                         "VAUGHN KEY HOMES LLC", 
                         "JAR LLC", 
                         "DEVIN CONSTRUCTION CO LLC", 
                         "DEVIN CONSTRUCTION", 
                         "DESIGN HOMES & DEVELOPMENT CO INC", 
                         "DEVIN CONSTRUCTION CO, LLC", 
                         "DEVIN CONSTRUCTION CO , LLC", 
                         "TRAILS PARTNERS LLC", 
                         "G A WHITE DEVELOPMENT CO LLC", 
                         "BERTRAM BUILDERS LLC AND", 
                         "BELMONT PROPERTY MANAGEMENT LLC")

var_explore.clusters <- c("o2p_144", "o2o_16")

outliers_clust_o2o <- M_clusterid_size %>%
  .[.$cluster_id %in% LLC_clusters,] %>%
  .[.$variable == "n_o2o",] %>%
  slice_sample(., n = var_topn)

outliers_clust_o2p <- M_clusterid_size %>%
  .[.$cluster_id %in% LLC_clusters,] %>%
    .[.$variable == "n_o2p",] %>%
    slice_sample(., n = var_topn)

# bigclust_o2o <- M_clusterid_size %>%
#   .[.$cluster_id %in% LLC_clusters,] %>%
#   .[.$variable == "n_o2o",] %>%
#   slice_max(., order_by = value, n = var_topn)
# bigclust_o2p <- M_clusterid_size %>%
#   .[.$cluster_id %in% LLC_clusters,] %>%
#   .[.$variable == "n_o2p",] %>%
#   slice_max(., order_by = value, n = var_topn)
# 
# smclust_o2o <- M_clusterid_size %>%
#   .[.$cluster_id %in% LLC_clusters,] %>%
#   .[.$variable == "n_o2o",] %>%
#   slice_min(., order_by = value, n = var_topn)
# smclust_o2p <- M_clusterid_size %>%
#   .[.$cluster_id %in% LLC_clusters,] %>%
#   .[.$variable == "n_o2p",] %>%
#   slice_min(., order_by = value, n = var_topn)
# 
# outliers_clust_o2o <- rbind(bigclust_o2o, smclust_o2o)
# outliers_clust_o2p <- rbind(bigclust_o2p, smclust_o2p)


outlier_members.o2o <- CW_owner.clusterid[CW_owner.clusterid$cluster_id %in% 
                                           outliers_clust_o2o$cluster_id,] %>%
  group_by(owner) %>%
  summarise() %>%
  .$owner


outlier_members.o2p <- CW_parid.clusterid[CW_parid.clusterid$cluster_id %in% 
                                           outliers_clust_o2p$cluster_id,] %>%
  group_by(owner) %>%
  summarise() %>%
  .$owner

GR_own2own_filter        <- graph_from_data_frame(d = M_own2own[M_own2own$own1 %in% outlier_members.o2o |
                                                                  M_own2own$own2 %in% outlier_members.o2o,], 
                                                  directed = F) %>% 
  simplify()

gsize(GR_own2own_filter)
plot(GR_own2own_filter); Sys.sleep(3)
neighborhood(GR_own2own_filter) %>% 
  lapply(., length) %>%
  unlist()

#igraph::cluster_walktrap()



GR_own2own_filter %>%
  neighborhood() %>%
  lapply(., length) %>%
  unlist() %>%
  unique() %>%
  sort() 

GR_own2parid_filter      <- graph_from_data_frame(d = M_own2parid[M_own2parid$owner %in% outlier_members.o2p | 
                                                                    M_own2parid$PARID %in% outlier_members.o2p ,], directed = F) %>% 
  simplify()

gsize(GR_own2parid_filter)
plot(GR_own2parid_filter)

#GR_own2pardate_id_filter <- graph_from_data_frame(d = M_own2pardate_id, directed = F) %>% simplify()


# FINAL STEP----
ls() %>%
  grep("var_ptrn_llc|^var_explore|^var_ptrn|own2own|^CW_owner.clusterid|o2o|M_clusterid_size|master_sales", ., value = T)

ls() %>%
  grep("^CW_", ., value = T)




var_explore.clusters
var_explore.further
var_ptrn_llc

# adist(x = "MENDENHALL PROPERTIES 4 LLC", 
#       y = "MENDENHALL PROPERTIES f LLC") %>% 
#   as.list() %>%
#   unlist 
# 
# agrep(pattern = "MENDENHALL PROPERTIES 4 LLC", 
#       x       = "MENDENHALL PROPERTIES fff LLC", 
#       value = T)
# 
# adist(x = c("tim 1"), 
#       y = c("tom 5", "tim", "timmy", "timothy"))
# 
# 
# df_similar_names <- data.frame(name1 = var_explore.further, 
#                                n_agrep_matches = NA, 
#                                list.names = NA) %>% as_tibble()
# 
# 
# for(i in 1:nrow(df_similar_names)){
#   df_similar_names$n_agrep_matches[i] <- 
#     agrep(pattern = df_similar_names$name1[i],
#           max.distance = c(1),#0.09,
#          x = CW_owner.clusterid$owner, value = T) %>%
#     length()
#   df_similar_names$list.names[i] <- 
#     list(agrep(pattern = df_similar_names$name1[i],
#           max.distance = c(1),
#           x = CW_owner.clusterid$owner, value = T))
# }
# 
# df_similar_names$list.names[1]
# df_similar_names$list.names[7]
# df_similar_names$list.names[9]
# df_similar_names$list.names[10]

master_sales.23$sale_valid %>% table()
master_sales.23


# ASSOC:  OWNER <--> PROPERTY ----