library(jsonlite)
library(curl)
library(readr)
library(glue)
library(dplyr)

setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")
rm(list=ls());cat('\f');gc()
source(".env")

# example----
ex1 <- curl(url = "https://api.discogs.com/releases/249504")
jsonlite::fromJSON(ex1)

# documentation----
"https://www.discogs.com/developers/#page:home,header:home-quickstart"
"https://www.discogs.com/developers/#page:database,header:database-search"

# search for artist----
setwd("~/R/play/shiny_misc/shiny_herbiehancock/data")

search_artist <- "Herbie-Hancock"
get.page      <- 2
get.perpage   <- 500
get.country   <- "US"

curl_sa <- curl(url = glue("https://api.discogs.com/database/search?artist={search_artist}&token={API_TOKEN}&page={get.page}&per_page={get.perpage}&country={get.country}"))
df_artist <- fromJSON(curl_sa)
df_artist$pagination
df_artist <- df_artist$results %>% as_tibble()

df_artist %>% colnames() %>% sort

df_artist %>%
  .[.$master_id != 0,] %>%
  .[!is.na(.$year),] %>%
  .[!is.na(.$master_url),] %>%
  #.[.$country == "US",] %>%
  group_by(title = trimws(title)) %>%
  summarise(n = n(), 
            first_yr = min(as.numeric(year), na.rm = T), 
            med_yr = median(as.numeric(year), na.rm = T)) %>%
  .[order(.$first_yr,decreasing = F),] %>% as.data.frame()

df_artist$master_url
df_artist$uri %>% sort
df_artist$catno
df_artist$thumb
df_artist$resource_url
df_artist$format_quantity

df_artist[df_artist$format_quantity==0,]$title

df_artist$formats[[61]]
df_artist$title[61]

# get albums----

# get album personnel----

