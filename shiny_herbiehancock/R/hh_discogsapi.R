library(jsonlite)
library(curl)
library(readr)
library(glue)
library(dplyr)

setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")
rm(list=ls());cat('\f');gc()

# example----
ex1 <- curl(url = "https://api.discogs.com/releases/249504")
jsonlite::fromJSON(ex1)

# documentation----
"https://www.discogs.com/developers/#page:home,header:home-quickstart"
"https://www.discogs.com/developers/#page:database,header:database-search"

# search for artist----
setwd("~/R/play/shiny_misc/shiny_herbiehancock/data")
search_artist <- "herbie-hancock"
source(".env")

curl_sa <- curl(url = glue("https://api.discogs.com/database/search?artist={search_artist}&token={API_TOKEN}"))
df_artist <- fromJSON(curl_sa) %>% as_tibble()

# get albums----

# get album personnel----

