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

# backup original here
bu_sales.res.2023 <- sales.res.2023

sales.res.2023 %>%
  group_by(sale_valid,SALEVALIDITY ) %>%
  summarise()

# check if ACRES ever changes with PARID (it doesn't)
sales.res.2023 %>%
  group_by(PARID) %>%
  summarise(n = n(), 
            n_acres = n_distinct(ACRES)) %>%
  group_by(n,n_acres) %>%
  summarise(n2 = n())

# check if multiple records per transaction ( there can be)
sales.res.2023 %>%
  group_by(PARID, SALEDTE) %>%
  summarise(n_records = n()) %>%
  group_by(n_records) %>%
  summarise(n = n()) 

# # cleanup and create master owner name crosswalk
# cw_sales_owners <- rbind(data.frame(o_name = sales.res.2023$OLDOWN, 
#                                     srccol = "OLDOWN"), 
#                          data.frame(o_name = sales.res.2023$OWNERNAME1, 
#                                     srccol = "OWNERNAME1")) %>% 
#   as_tibble()
# 
# cw_sales_owners %>%
#   group_by(o_name) %>%
#   summarise(n_recs_with_name = n()) %>%
#   .[order(.$n_recs_with_name,decreasing = T),] %>%
#   group_by(n_recs_with_name) %>%
#   summarise(count = n()) 


temp.order.owners <-  sales.res.2023[,c("OLDOWN", 
                                        "OWNERNAME1")] %>% 
  t %>% 
  as.data.frame() %>% 
  as.list.data.frame 

sales.res.2023$own1 <- lapply(temp.order.owners, sort) %>% lapply(., first) %>% unlist()
sales.res.2023$own2 <- lapply(temp.order.owners, sort) %>% lapply(., last) %>% unlist()

rel_owners <- sales.res.2023[,c("own1", 
                                "own2", 
                                "SALEDTE",
                                "PARID", 
                                "sale_valid"#, 
                                #"DEEDREFERENCE"
)] %>%
  group_by_all() %>%
  summarise(n = n()) %>%
  ungroup() %>%
  .[.$sale_valid,] %>%
  .[order(.$n,decreasing = T),] %>%
  .[,c("own1", "own2"),]


# rel_owners %>% matrix(., ncol = 2, byrow = T) %>% as.list() %>% str
# replicate(1000,list(sample(letters, size = 2))) %>% 
#   lapply(., sort) #%>% 
# #lapply(., diff) %>% 
# #unlist() %>%
# #fivenum
gr2023 <- igraph::graph_from_data_frame(rel_owners, directed = F)

# ?igraph::groups()
# g <- make_graph("Zachary")
# plot(g)
# fgc <- cluster_fast_greedy(g)
# groups(fgc)
# 
# g2 <- make_ring(10) + make_full_graph(5)
# plot(g2)
# groups(components(g2))
# ?groups

# size of clusters----

igraph::clusters(gr2023) %>% 
  groups() %>%
  lapply(., length) %>%
  unname() %>%
  unlist() %>% unique() %>% sort() %>% plot

names(clusters(gr2023))


clusters(gr2023)$csize
clusters(gr2023)$membership %>% 
  unname() %>% table() %>%
  unname() %>% table

# convert membership (below) to a crosswalk table
clusters(gr2023)$membership
names(clusters(gr2023)$membership)  # owner names
unname(clusters(gr2023)$membership) # cluster id

cw_owners_clusterid <- data.frame(owner_name = names(clusters(gr2023)$membership), 
                                  cluster_id = unname(clusters(gr2023)$membership)) %>%
  as_tibble()

# error-check (ok if returns values 1:Inf)
cw_owners_clusterid %>%
  group_by(cluster_id) %>%
  summarise(n_onames = n_distinct(owner_name)) %>%
  .$n_onames %>% unique() %>% sort

# error-check (ok if returns value 1 only)
cw_owners_clusterid %>%
  group_by(owner_name) %>%
  summarise(n_cid= n_distinct(cluster_id)) %>%
  .$n_cid %>% unique() %>% sort

#join crosswalk----
sales.res.2023_out <- sales.res.2023[,c("own1", 
                                        "own2", 
                                        "SALEDTE",
                                        "PARID", 
                                        "sale_valid")] %>%
  group_by_all() %>%
  summarise(n = n()) %>%
  ungroup() %>%
  .[.$sale_valid,] %>%
  .[order(.$n,decreasing = T),] %>%
  left_join(., 
            cw_owners_clusterid, 
            by = c("own1" = "owner_name")) %>%
  left_join(., 
            cw_owners_clusterid, 
            by = c("own2" = "owner_name"), 
            suffix = c(".own1", ".own2"))


table(sales.res.2023_out$cluster_id.own1 == sales.res.2023_out$cluster_id.own2)


# now lets look at how often properties stick in clusters
cw_props_clusterid <- sales.res.2023_out %>%
  group_by(PARID, 
           #SALEDTE, 
           cluster_id = cluster_id.own1) %>%
  summarise(n = n(), 
            n_saledates = n_distinct(SALEDTE)) %>%
  .[order(.$n, decreasing = T),]

summary_props_clusters <- cw_props_clusterid %>%
  group_by(PARID) %>%
  summarise(sum_n = sum(n), 
            n = n(),
            n_clusterids = n_distinct(cluster_id)) %>%
  .[order(.$n, .$sum_n, decreasing = T),]


# ERROR FOUND
summary_props_clusters$pid_typo_chec <- F
summary_props_clusters[!grepl("^.{9,9} ", summary_props_clusters$PARID) |
                               !grepl("^.{3,3} ", summary_props_clusters$PARID),]$pid_typo_chec <- T


cw_pidtypocheck  <- summary_props_clusters %>%
  group_by(PARID, pid_typo_chec) %>%
  summarise()

bu_sales.res.2023 %>%
  left_join(., cw_pidtypocheck) %>%
  .[.$sale_valid,] %>% 
  #.[.$ACRES > 0,] %>%
  #group_by()
  group_by(PARCELLOCATION , 
           ACRES
           ) %>%
  summarise(n_parid = n_distinct(PARID), 
            n = n(), 
            pid.chec = any(pid_typo_chec)) %>%
  .[order(.$n_parid, decreasing = T),] %>%
  .[.$n_parid > 1,]

# component_distribution(graph = gr2023, 
#                        cumulative = F, 
#                        mul.size = F)
# 
# 
# # igraph::as_membership(gr2023) not appropriate implementation
# 
# #?igraph::communities(gr2023)
# #cluster_fast_greedy(gr2023)
# #cluster_edge_betweenness(gr2023)
# 
# cluster.distribution(gr2023)
# largest_component(gr2023)
# count_components(gr2023)
# data.frame(cluster_size =
#            1:length(component_distribution(gr2023, F,F)), 
#            distribution = component_distribution(gr2023, F,F)) %>%
#   as_tibble() %>%
#   .[.$distribution != 0,]  %>%
#   .[order(.$distribution, decreasing = T),] 
#   

var_organized.owners <- c("TRUSTEES", 
                          "CORPORATION", 
                          "CORPORATION", 
                          "ASSOCIATION", 
                          "REUTILIZATION"  , 
                          "CONSTRUCTION",
                          "DEVELOPMENT", "IMPROVEMENT", "PARTNERSHIP", 
                          "INVESTMENTS", "INCORPORATED", "NEIGHBORHOOD", 
                          "MANAGEMENT", "PROPERTIES", "CONSULTING", "COMMUNITY",
                          "TRUSTEES", "TRUSTEE", "LIMITED", "TRUST",
                          "CORP", "BANK", "LLC", "INC", "LTD", "TR", "LP", "NA") %>%
  paste(" ", ., "$", 
      sep = "") %>%
  paste(., collapse = "|")





  # Delinquent Tax Data Tidy----
files.deltax.all <- list.files(pattern = "^Delq_\\d{8,8}\\.csv$")

