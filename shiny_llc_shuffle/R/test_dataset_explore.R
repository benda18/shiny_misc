# test data

library(dplyr)
library(renv)
library(igraph)
library(lubridate)

rm(list=ls());cat('\f')

renv::status()
# renv::snapshot()

# property that's sold a number of times as you would predict ----
set.seed(19754)

build_transactions <- function(n_trans = 40, 
                               n_buyers.ratio = 1,
                               n_sellers.ratio = 1){
  n_transactions <- n_trans
  n_buyers       <- as.integer(n_transactions * n_buyers.ratio)
  n_sellers      <- as.integer(n_transactions * n_sellers.ratio)
  
  #rep(sample(100:500, size = 3), length.out = 4)
  
  prop_id   <- replicate(n = 1, 
                         paste("pid", 
                               paste(sample(0:9,3,replace=T), collapse = ""),
                               sep = "_"))
  
  seller_id <- replicate(n = n_sellers, 
                         paste("sid", 
                               paste(sample(0:9,6,replace=T), collapse = ""),
                               sep = "_")) %>%
    rep(., length.out = n_transactions) %>% 
    sample()
  
  buyer_id  <- replicate(n = n_buyers, 
                         paste("bid", 
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

trans_1.1 <- build_transactions(40, 1, 1)

gr_1.1 <- graph_from_data_frame(trans_1.1[,c("seller.id", "buyer.id")], 
                                directed = F)

plot(gr_1.1)


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
