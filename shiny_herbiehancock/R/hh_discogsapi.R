library(jsonlite)
library(curl)
library(readr)
library(glue)


setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")
rm(list=ls());cat('\f');gc()

# example----
ex1 <- curl(url = "https://api.discogs.com/releases/249504")
jsonlite::fromJSON(ex1)

# documentation----
"https://www.discogs.com/developers/#page:home,header:home-quickstart"
"https://www.discogs.com/developers/#page:database,header:database-search"

# search for artist----
search_artist <- "herbiehancock"

curl_sa <- curl(url = glue("https://api.discogs.com/database/search?artist={search_artist}"))
fromJSON(curl_sa)

# get albums----

# get album personnel----

