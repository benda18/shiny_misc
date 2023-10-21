library(igraph)
library(jsonlite)
library(curl)
library(readr)
library(glue)
library(dplyr)

setwd("~/R/play/shiny_misc/shiny_herbiehancock/data")
rm(list=ls());cat('\f');gc()
source(".env")
setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")

# # example----
# ex1 <- curl(url = "https://api.discogs.com/releases/249504")
# jsonlite::fromJSON(ex1)
# rm(ex1)


# vars----
var_artid <- 3865 # herbie hancock
var_artname <- "Herbie Hancock"

# documentation----
"https://www.discogs.com/developers/#page:home,header:home-quickstart"
"https://www.discogs.com/developers/#page:database,header:database-search"


# load data----
#load("hh_main_artist_ids.RData")

#hh_main_artist_ids


# loop thru----
#art_grp_members <- NULL
out_artG.list <- list()

#for(i in hh_main_artist_ids$id){
for(i in var_artid){
  curl_art <- curl(url = glue("https://api.discogs.com/artists/{i}"))
  out_art  <- fromJSON(txt = curl_art)
  #out_artM.list[[as.character(i)]] <- try(out_art$members) 
  out_artG.list[[as.character(i)]] <- try(out_art$groups) 
  
  
  # try(temp <- expand.grid(from = out_art$members$id, 
  #             to = out_art$members$id) %>%
  #   .[order(.$from,.$to,decreasing = F),] %>%
  #   .[.$to < .$from,] %>% 
  #   mutate(., i_in = i, 
  #          name = out_art$name))
  # try(art_grp_members <- rbind(art_grp_members, 
  #                          temp))
  # try(rm(temp))
}

out_artM.list <- list()
for(i in out_artG.list[[1]]$id){
  curl_art <- curl(url = glue("https://api.discogs.com/artists/{i}"))
  out_art  <- fromJSON(txt = curl_art)
  out_artM.list[[as.character(i)]] <- try(out_art$members)
  try(out_artM.list[[as.character(i)]]$DQ <- NA)
  try(out_artM.list[[as.character(i)]]$DQ <- out_art$data_quality)
  
  try(out_artM.list[[as.character(i)]]$grpname <- NA)
  try(out_artM.list[[as.character(i)]]$grpname <- out_art$name)
  
  #out_artG.list[[as.character(i)]] <- try(out_art$groups) 
  
}

out_artM.list
artist_groups <- out_artG.list[[1]][grepl("Donald Byrd|Joe Henderson|Buster Williams|Bobby Hutcherson|Rockit Band|Charles Tolliver|Herbie Hancock|Eric Dolphy|Headhunters$|Miles Davis|Hank Mobley|V\\.S\\.O\\.P\\.", 
                         x = out_artG.list[[1]]$name, ignore.case = T),]

artist_groups <- artist_groups[,c("id", "name")] 
colnames(artist_groups) <- c("group_id", "group_name")
artist_groups$artist_id <- var_artid
artist_groups$artist_name <- var_artname

out_artM.list <- out_artM.list[which(names(out_artM.list) %in% as.character(artist_groups$group_id))] 

group_members <- NULL
for(i in 1:length(out_artM.list)){
  out_artM.list[[i]]$group_id = names(out_artM.list[i])
  
  group_members <- rbind(group_members, 
                         out_artM.list[[i]]) %>% as_tibble()
  
}

colnames(group_members) <- c("artist_id", "artist_name", 
                             "resource_url", "active", 
                             "DQ", "group_name", "group_id")

group_members
artist_groups



# art_grp_members
# 
# 
# art_grp_members[!duplicated(art_grp_members),] %>%
#   .[,c("from", "to")] %>%
#   igraph::graph_from_data_frame() %>%
#   plot()
# 
# View(out_art)
# 
# 
# # # search for artist----
# # setwd("~/R/play/shiny_misc/shiny_herbiehancock/data")
# # 
# # search_artist <- "Herbie-Hancock"
# # get.page      <- 2
# # get.perpage   <- 500
# # get.country   <- "US"
# # 
# # curl_sa <- curl(url = glue("https://api.discogs.com/database/search?artist={search_artist}&token={API_TOKEN}&page={get.page}&per_page={get.perpage}&country={get.country}"))
# # df_artist <- fromJSON(curl_sa)
# # df_artist$pagination
# # df_artist <- df_artist$results %>% as_tibble()
# # 
# # df_artist %>% colnames() %>% sort
# # 
# # df_artist %>%
# #   .[.$master_id != 0,] %>%
# #   .[!is.na(.$year),] %>%
# #   .[!is.na(.$master_url),] %>%
# #   #.[.$country == "US",] %>%
# #   group_by(title = trimws(title)) %>%
# #   summarise(n = n(), 
# #             first_yr = min(as.numeric(year), na.rm = T), 
# #             med_yr = median(as.numeric(year), na.rm = T)) %>%
# #   .[order(.$first_yr,decreasing = F),] %>% as.data.frame()
# # 
# # df_artist$master_url
# # df_artist$uri %>% sort
# # df_artist$catno
# # df_artist$thumb
# # df_artist$resource_url
# # df_artist$format_quantity
# # 
# # df_artist[df_artist$format_quantity==0,]$title
# # 
# # df_artist$formats[[61]]
# # df_artist$title[61]
# # 
# # # get albums----
# # 
# # # get album personnel----
# 
