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


miles.davis <- 23755
herbie.hancock <- 3865

great.quintets <- c("Herbie Hancock", "John Coltrane", 
                    "Red Garland", "Paul Chambers (3)", "\"Philly\" Joe Jones", 
                    "Wayne Shorter", 
                    "Ron Carter", 
                    "Anthony Williams", 
                    "Miles Davis")


# vars----
var_artid <- miles.davis 
var_artname <- "Miles Davis"

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

library(janitor)

out_artG.list <- out_artG.list %>%
  as.data.frame.list() %>% clean_names()

colnames(out_artG.list) <- colnames(out_artG.list) %>%
  gsub("^x\\d{1,}_", "", .) %>%
  paste("group", ., sep = "_")

quint.membersurls <- out_artG.list[grepl("The Miles Davis Quintet", 
                    out_artG.list$group_name, ignore.case = T),]$group_resource_url %>%
  curl() %>%
  fromJSON() %>%
  .$members %>%
  .[.$name %in% 
      great.quintets,]

out.df <- NULL
for(i in quint.membersurls$resource_url){
  temp <- curl(i) %>%
    fromJSON()
  out.df <- c(out.df, 
              temp$data_quality)
}


out_artM.list

# artist_groups <- out_artG.list[[1]][grepl("Donald Byrd|Joe Henderson|Buster Williams|Bobby Hutcherson|Rockit Band|Charles Tolliver|Herbie Hancock|Eric Dolphy|Headhunters$|Miles Davis|Hank Mobley|V\\.S\\.O\\.P\\.", 
#                                           x = out_artG.list[[1]]$name, ignore.case = T),]
# 
# artist_groups <- artist_groups[,c("id", "name")] 
# colnames(artist_groups) <- c("group_id", "group_name")
# artist_groups$artist_id <- var_artid
# artist_groups$artist_name <- var_artname
# 
# out_artM.list <- out_artM.list[which(names(out_artM.list) %in% as.character(artist_groups$group_id))] 
# 
# group_members <- NULL
# for(i in 1:length(out_artM.list)){
#   out_artM.list[[i]]$group_id = names(out_artM.list[i])
#   
#   group_members <- rbind(group_members, 
#                          out_artM.list[[i]]) %>% as_tibble()
#   
# }
# 
# colnames(group_members) <- c("artist_id", "artist_name", 
#                              "resource_url", "active", 
#                              "DQ", "group_name", "group_id")
# 
# group_members
# artist_groups
# 
# 
# 
