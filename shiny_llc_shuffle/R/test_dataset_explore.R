# test data

library(dplyr)
library(renv)
library(igraph)
library(lubridate)

rm(list=ls());cat('\f')

# property that's sold a number of times as you would predict ----
set.seed(19754)

n_transactions <- 40
n_buyers       <- as.integer(n_transactions * 0.94)
n_sellers      <- as.integer(n_transactions * 0.7)

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
# graph it

gr_owner <- graph_from_data_frame(test_norm[,c("seller.id", "buyer.id")], 
                                  directed = F)

plot(gr_owner)
