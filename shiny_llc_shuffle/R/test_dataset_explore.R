# test data

library(dplyr)
library(renv)
library(igraph)
library(lubridate)
library(data.table)


rm(list=ls());cat('\f')

renv::status()
# renv::snapshot()

# property that's sold a number of times as you would predict ----
set.seed(19754)

build_transactions <- function(n_trans = 20, 
                               n_buyers.ratio = 1,
                               n_sellers.ratio = 1, 
                               n_prop.ratio    = 0.8){
  n_transactions <- n_trans
  n_prop         <- as.integer(n_transactions * n_prop.ratio)
  n_buyers       <- as.integer(n_transactions * n_buyers.ratio)
  n_sellers      <- as.integer(n_transactions * n_sellers.ratio)
  
  #rep(sample(100:500, size = 3), length.out = 4)
  
  prop_id   <- replicate(n = n_prop, 
                         paste("pid", 
                               paste(sample(0:9,3,replace=T), collapse = ""),
                               sep = "_")) %>%
    rep(., length.out = n_transactions) %>% 
    sample()
  
  seller_id <- replicate(n = n_sellers, 
                         paste("id", #sid", 
                               paste(sample(0:9,6,replace=T), collapse = ""),
                               sep = "_")) %>%
    rep(., length.out = n_transactions) %>% 
    sample()
  
  buyer_id  <- replicate(n = n_buyers, 
                         paste("id", #bid", 
                               paste(sample(0:9,6,replace=T), collapse = ""),
                               sep = "_")) %>%
    rep(., length.out = n_transactions) %>% 
    sample()
  
  date_id   <- replicate(n = n_transactions, 
                         #paste("date", 
                         paste(sample(0:9,5,replace=T), collapse = ""),
                         #      sep = "_")
  ) %>% 
    as.numeric() %>%
    sort() #%>%
  #paste("date", ., sep = "_")
  
  test_norm <- data.frame(prop.id   = prop_id, 
                          seller.id = seller_id, 
                          buyer.id  = buyer_id, 
                          date      = date_id) %>% as_tibble()
  return(test_norm)
}


# Buildem----

trans_1.1 <- build_transactions(20, .8, .8, .8)

# gr_1.1 <- graph_from_data_frame(trans_1.1[,c("seller.id", "buyer.id")], 
#                                 directed = F)
# plot(gr_1.1)

# try again

rbind(mutate(trans_1.1, 
             from = prop.id, 
             to   = buyer.id)[,c("from", "to")],
      mutate(trans_1.1, 
             from = seller.id, 
             to   = prop.id)[,c("from", "to")]) %>%
  graph_from_data_frame(., directed = T) %>% plot()




graph_from_data_frame(trans_1.1[,c("prop.id", "buyer.id")], 
                      directed = T)
graph_from_data_frame(trans_1.1[,c("seller.id", "prop.id")], 
                      directed = T)



trans_97.8 <- build_transactions(40, .5, .5)

trans_97.8$seller.id %>% unique

gr_97.8 <- graph_from_data_frame(trans_97.8[,c("seller.id", "buyer.id")], 
                                 directed = F)

plot(gr_97.8)



# consolidated ----
trans_all <- rbind(mutate(trans_97.8, name = "97.8"),
                   mutate(trans_1.1, name = "1.1"))

gr_all <- graph_from_data_frame(trans_all[,c("seller.id", "buyer.id")])

plot(gr_all)

# distances----
# https://r.igraph.org/reference/index.html#structural-properties
?distance_table
?mean_distance
?distances
?shortest_paths
?all_shortest_paths

# distance_table(gr_all, F) 
# mean_distance(gr_all, directed = F, unconnected = T)

dist_all <- distances(gr_all, algorithm = "unweighted", 
          mode = "all") %>%
  as.data.frame()

dist_all$to <- rownames(dist_all)
dist_all <- as.data.table(dist_all)

melt(dist_all, id.vars = "to", 
     variable.name = "from", 
     value.name = "dist")
