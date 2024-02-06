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
get_distances <- function(v_name = "DARAKI GROUP LLC",
                          g = temp.gr){
  temp.dist <- distances(graph = temp.gr, 
                         v  = v_name,
                         to = ) %>% 
    t() %>%
    .[!is.infinite(.),] 
  
  df.dist = data.frame(d_names = names(temp.dist), 
                       d_dist  = unname(temp.dist), 
                       V       = v_name)
  return(df.dist)
}
# ?adist
# ?agrep


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
sales.res.2022 <- read_csv(file = grep(pattern = "2022", x = files.res.sales, value = T), 
                           #col_types = "c",
                           guess_max = min(c(22000, Inf)))
sales.res.2021 <- read_csv(file = grep(pattern = "2021", x = files.res.sales, value = T), 
                           #col_types = "c",
                           guess_max = min(c(10000, Inf)))
sales.res.2020 <- read_csv(file = grep(pattern = "2020", x = files.res.sales, value = T), 
                           #col_types = "c",
                           guess_max = min(c(10000, Inf)))
sales.res.2019 <- read_csv(file = grep(pattern = "2019", x = files.res.sales, value = T), 
                           #col_types = "c",
                           guess_max = min(c(10000, Inf)))
sales.res.2018 <- read_csv(file = grep(pattern = "2018", x = files.res.sales, value = T), 
                           #col_types = "c",
                           guess_max = min(c(15912, Inf))) %>%
  mutate(., 
         NBHD = NA)


sales.res.2023 <- rbind(sales.res.2023, 
                        sales.res.2022, 
                        sales.res.2021,
                        sales.res.2020, 
                        sales.res.2019, 
                        sales.res.2018)

rm(files.res.sales, sales.res.2022, sales.res.2021, sales.res.2020, 
   sales.res.2019, sales.res.2018)

sales.res.2023$SALEDTE <- dmy(sales.res.2023$SALEDTE)
#sales.res.2023$CONVNUM <- as.numeric(sales.res.2023$CONVNUM)  #no NAs result but not ideal
sales.res.2023$PRICE   <- as.numeric(sales.res.2023$PRICE)
sales.res.2023$ACRES   <- as.numeric(sales.res.2023$ACRES)

sales.res.2023$sale_valid <- grepl("^DESIGNATED|^VALID |^LAND CONTRACT|^LIQUIDATION|^PARTIAL|^RELATED|^Mobile |^SALE INVOLVING", 
                                   x = sales.res.2023$SALEVALIDITY)


# filter out invalid sales here----
sales.res.2023 <- sales.res.2023[sales.res.2023$sale_valid,]

temp.gr <- rbind(mutate(sales.res.2023[sales.res.2023$sale_valid,], 
                        from = PARID, to = OWNERNAME1)[,c("from", "to")],
                 mutate(sales.res.2023[sales.res.2023$sale_valid,], 
                        from = OLDOWN, to = PARID)[,c("from", "to")]) %>%
  graph_from_data_frame(., 
                        directed = T) %>% simplify()



# clusters----
igraph::no.clusters(graph = temp.gr)
igraph::gsize(temp.gr)
temp.clusters <- igraph::clusters(temp.gr)


# summarise clusters
summary_clust <- NULL
# loop through clusters
for(i in unique(temp.clusters$membership)){
  if(i %% 1000 == 0){
    print(i)
  }
  # cluster members - ppl
  temp_owner <- names(temp.clusters$membership[temp.clusters$membership == i]) %>%
    .[!grepl(" \\d{4,4}$", .)]
  # cluster members - propertie
  temp_parid <- names(temp.clusters$membership[temp.clusters$membership == i]) %>%
    .[grepl(" \\d{4,4}$", .)]
  # cluster size
  temp_c.size <- temp.clusters$csize[i]
  # number LLC
  temp_n.llc  <- sum(grepl(" LLC$", temp_owner))
  # number of properties
  temp_n.parid <- length(temp_parid)
  temp_n.owner <- length(temp_owner)
  
  # number of transactions
  # sales.res.2023[sales.res.2023$OLDOWN %in% temp_owner |
  #                  sales.res.2023$OWNERNAME1 %in% temp_owner | 
  #                  sales.res.2023$PADDR1 %in% temp_parid,]
  temp_n.trans <- sum(sales.res.2023$OLDOWN %in% temp_owner |
                        sales.res.2023$OWNERNAME1 %in% temp_owner | 
                        sales.res.2023$PADDR1 %in% temp_parid)
  
  summary_clust <- rbind(summary_clust, 
                         data.frame(cid = i, 
                                    csize = temp_c.size, 
                                    n_llc = temp_n.llc,
                                    n_parid = temp_n.parid, 
                                    n_owner = temp_n.owner, 
                                    n_trans = temp_n.trans))
  
}

head(summary_clust)

summary_clust %>% as_tibble() %>%
  .[.$n_llc > 0,] %>%
  .[.$n_parid <= 1000,] %>%
  ggplot(data = ., 
         aes(x = csize, 
             y = n_trans)) + 
  geom_bin2d() +
  geom_smooth() +
  geom_jitter()




# names(temp.clusters)
# temp_clus.membership <- temp.clusters$membership %>% 
#   as.data.frame() 
# temp_clus.membership$member <- rownames(temp_clus.membership)
# colnames(temp_clus.membership) <- c("cluster_id", "member")
# temp_clus.membership <- temp_clus.membership %>% as_tibble()
# 
# 
# temp_clus.membership$member %>%
#   grep("^.{3,3} .{5,5} .{4,4}$", ., value = T)
# 
# llc_depth <- temp_clus.membership %>%
#   group_by(cluster_id) %>%
#   summarise(n_members = n_distinct(member),
#             n_llc = sum(grepl("LLC$", member))) %>%
#   .[.$n_llc > 1,] %>% 
#   mutate(., 
#          llc_per_member = n_llc / n_members) %>%
#   .[order(.$llc_per_member, .$n_members, decreasing = T),]
# 
# # pick a cluster
# a.clust <- 13 #107
# a.clust.mbrs <- temp_clus.membership[temp_clus.membership$cluster_id == 
#                        a.clust,]$member
# 
# a.clust.gr <- rbind(mutate(sales.res.2023[sales.res.2023$sale_valid,], 
#                            from = PARID, to = OWNERNAME1)[,c("from", "to")],
#                     mutate(sales.res.2023[sales.res.2023$sale_valid,], 
#                            from = OLDOWN, to = PARID)[,c("from", "to")]) %>%
#   .[.$from %in% a.clust.mbrs | 
#       .$to %in% a.clust.mbrs,] %>%
#   graph_from_data_frame(., 
#                         directed = T)
# 
# plot(a.clust.gr)
# 
# communities(temp.clusters) %>% 
#   lapply(X = ., 
#          FUN = grep, 
#          pattern = "LLC$", 
#          value = T) 
# 
# # Distances----
# for(i in grep("LLC$", names(V(temp.gr)), value = T)){
#   print(i)
#   get_distances(i)
# }
# 
# 
# # FIND: Owners who have owned the same property 2 or more times----
# 
# 
# find_multowner <- sales.res.2023 %>%
#   .[.$sale_valid,] %>%
#   group_by(owner = OLDOWN, 
#            PARID) %>%
#   summarise(n = n(), 
#             n_saledates = n_distinct(SALEDTE)) %>%
#   # .[.$n > 1 |
#   #     .$n_saledates > 1,] %>%
#   .[order(.$n_saledates, decreasing = T),] %>%
#   mutate(., owner_type = "old_owner")
# 
# find_multowner1 <- sales.res.2023 %>%
#   .[.$sale_valid,] %>%
#   group_by(owner = OWNERNAME1, 
#            PARID) %>%
#   summarise(n = n(), 
#             n_saledates = n_distinct(SALEDTE)) %>%
#   # .[.$n > 1 |
#   #     .$n_saledates > 1,] %>%
#   .[order(.$n_saledates, decreasing = T),] %>%
#   mutate(., owner_type = "new_owner")
# 
# find_multowner <- rbind(find_multowner, 
#                         find_multowner1)
# rm(find_multowner1)
# 
# find_multowner %>%
#   group_by(owner, PARID) %>%
#   summarise(n = n(), 
#             sum_nsd = sum(n_saledates)) %>%
#   .[order(.$owner),] %>%
#   .[.$sum_nsd > 2,] %>%
#   mutate(., 
#          is_llc = grepl("LLC$", owner)) %>%
#   ggplot(data = ., 
#          aes(x = n, y = sum_nsd)) +
#   geom_point()
