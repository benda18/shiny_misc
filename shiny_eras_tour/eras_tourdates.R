library(dplyr)
library(readr)
library(rvest)
library(lubridate)
library(janitor)
library(ggplot2)
library(gganimate)
library(data.table)
library(tidycensus)
library(tigris)
library(maps)
library(ggmap)
# # install.packages("remotes")
# # remotes::install_github("chris-prener/censusxy", dependencies = T, force = F)
# library(censusxy)

setwd("~/R/play/eras_tour")

rm(list=ls());cat('\f')
gc()

# get TS.ERAS dates----

eras.url <- "https://en.wikipedia.org/wiki/The_Eras_Tour"

eras.html <- rvest::read_html(eras.url)

eras.table.23 <- rvest::html_element(eras.html, 
                                     xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[3]") %>%
  html_table()
eras.table.24 <- rvest::html_element(eras.html, 
                                     xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[4]") %>%
  html_table() %>%
  .[!.$City == "Total",]

rm(eras.html, eras.url)

# tidy----
eras.table.23 <- clean_names(eras.table.23)
eras.table.24 <- clean_names(eras.table.24)

# format as date
eras.table.23$date <- mdy(paste(eras.table.23$date_2023, ", 2023", sep = ""))
eras.table.24$date <- mdy(paste(eras.table.24$date_2024, ", 2024", sep = ""))

# remove text dates field
eras.table.23 <- eras.table.23[!colnames(eras.table.23) %in% c("date_2023")]
eras.table.24 <- eras.table.24[!colnames(eras.table.24) %in% c("date_2024")]

# rbind
eras.table <- rbind(eras.table.23,eras.table.24)
rm(eras.table.23, eras.table.24)

# opening_act crosswalk
cw_opening.act <- eras.table[,c("opening_acts")]

cw_opening.act$opening_acts2 <- cw_opening.act$opening_acts %>%
  gsub(" ", "_", .) %>%
  gsub("Beabadoobee", "Beabadoobee ", .) %>%
  gsub("Gayle", "Gayle ", .) %>%
  gsub("Girl_in_Red", "Girl_in_Red ", .) %>%
  gsub("Haim", "Haim ", .) %>%
  gsub("Muna", "Muna ", .) %>%
  gsub("Paramore", "Paramore ", .) %>%
  gsub("Bridgers", "Bridgers ", .) %>%
  gsub("Muna \\[c\\]", "Muna", .) %>%
  gsub("^.*\\[b\\]$", "opening_acts_cancelled_weather", .) %>%
  gsub("^[[:punct:]]{1,1}$", 
       "no_opening_acts_scheduled", .) %>%
  trimws()



cw_opening.act2 <- NULL
for(i in 1:nrow(cw_opening.act)){
  temp.lj <- cw_opening.act[i,]
  temp.rj <- data.frame(opening_acts = temp.lj$opening_acts,
                        opening_acst2 = temp.lj$opening_acts2,
                        oa2 = unlist(strsplit(temp.lj$opening_acts2, 
                                       split = " ")))
  cw_opening.act2 <- rbind(cw_opening.act2, 
                           temp.rj)
  rm(temp.lj, temp.rj)

}
cw_opening.act <- cw_opening.act2 %>% 
  as_tibble() %>%
  group_by_all() %>%
  summarise()

rm(cw_opening.act2)

eras.table <- full_join(eras.table, 
          cw_opening.act[,c("opening_acts", "oa2")])


# further cleanup 
eras.table$promo_city <- eras.table$city

eras.table$promo_city[grepl("\\[a", eras.table$promo_city)] <- "Las Vegas"
eras.table$promo_city[grepl("\\[d", eras.table$promo_city)] <- "Los Angeles"
eras.table$promo_city[grepl("\\[e", eras.table$promo_city)] <- "Paris"
eras.table$promo_city[grepl("\\[f", eras.table$promo_city)] <- "Lyon"
eras.table$promo_city[grepl("\\[g", eras.table$promo_city)] <- "Miami"

# remove footnotes
eras.table$city <- eras.table$city %>%
  gsub("\\[.*$", "", .) 

# cw_usa.city.state 
cw_usa.citystate <- eras.table %>%
  .[.$country == "United States",] %>%
  group_by(venue, 
           #city, 
           promo_city, 
           state = NA, 
           country) %>%
  summarise()

cw_usa.citystate$state <- c("TX", "PA", 
                            "nv", "la", 
                            "co", "mi", 
                            "mo", "ma", 
                            "fl", "ca", 
                            "pa", "in", 
                            "wa", "ga", 
                            "nj", "tx", 
                            "tn", "oh", 
                            "fl", "ca", 
                            "il", "az", 
                            "mn") %>%
  toupper()




# crosswalk city_lon.lat
cw_city.lonlat <- eras.table[,c("city", "country")] %>%
  mutate(.,
         lon = NA_real_, 
         lat = NA_real_) %>%
  .[!duplicated(.),]

data("world.cities")

# for(i in 1:nrow(cw_city.lonlat)){
#   cw_city.lonlat[i,]
#   
#   if(cw_city.lonlat$country[i] == "United States"){
#     if(nrow(world.cities[world.cities$name == cw_city.lonlat$city[i] & 
#                          world.cities$country.etc == "USA",]) > 0){
#       
#       stop(world.cities[world.cities$name == cw_city.lonlat$city[i] & 
#                           world.cities$country.etc == "USA",]$name)
#     }
#   }
# }

temp.world_lonlat <- left_join(cw_city.lonlat[,c("city", "country")], 
          world.cities, 
          by = c("city" = "name", 
                 "country" = "country.etc")) %>%
  .[complete.cases(.),]


data("us.cities")
head(us.cities)
head(cw_usa.citystate)

temp.usa.lonlat <- us.cities %>%
  mutate(., 
         name = gsub(" \\w{2,2}$", "", name)) %>%
  right_join(., 
             cw_usa.citystate, 
             by = c("name" = "promo_city", 
                    "country.etc" = "state")) %>%
  as_tibble()

temp.world_lonlat <- temp.world_lonlat[,c("city", "country", "long", "lat")]
temp.usa.lonlat   <- temp.usa.lonlat[,c("name", "country", "long", "lat")]

colnames(temp.usa.lonlat) <- c("city", "country", "long", "lat")

all.longlat <- rbind(temp.world_lonlat, temp.usa.lonlat)

eras.table <- left_join(eras.table, 
          all.longlat, 
          by = c("promo_city" = "city", 
                 "country"))

eras.table[!complete.cases(eras.table),] %>%
  group_by(city, promo_city, country) %>%
  summarise()


eras.table$promo_city[grepl("Paulo$", eras.table$city)] <- "Sao Paulo"
eras.table$promo_city[grepl("^Z.*rich$", eras.table$city)] <- "Zurich"

eras.table$lat[eras.table$promo_city == "Cardiff"] <- 51.483333
eras.table$long[eras.table$promo_city == "Cardiff"] <- -3.183333

eras.table$lat[eras.table$promo_city == "Lyon"] <- 45.76
eras.table$long[eras.table$promo_city == "Lyon"] <- 4.84

eras.table$lat[eras.table$promo_city == "East Rutherford"] <- 40.817097
eras.table$long[eras.table$promo_city == "East Rutherford"] <- -74.085024

eras.table$lat[eras.table$promo_city == "Edinburgh"] <- 55.953333
eras.table$long[eras.table$promo_city == "Edinburgh"] <- -3.189167

eras.table$lat[eras.table$promo_city == "Foxborough"] <- 42.065278
eras.table$long[eras.table$promo_city == "Foxborough"] <- -71.248333

eras.table$lat[eras.table$promo_city == "Liverpool"] <- 53.4075
eras.table$long[eras.table$promo_city == "Liverpool"] <- -2.991944

eras.table$lat[eras.table$promo_city == "London"] <- 51.507222
eras.table$long[eras.table$promo_city == "London"] <- -0.1275

eras.table$lat[eras.table$promo_city == "Paris"] <- 48.856667
eras.table$long[eras.table$promo_city == "Paris"] <- 2.352222

eras.table$lat[eras.table$promo_city == "Sao Paulo"] <- -23.55
eras.table$long[eras.table$promo_city == "Sao Paulo"] <- -46.633333

eras.table$lat[eras.table$promo_city == "Zurich"] <- 47.374444
eras.table$long[eras.table$promo_city == "Zurich"] <- 8.541111



# cast opening acts
eras.table %>%
  group_by(date, city, country, venue, oa2) %>%
  summarise() %>%
  ungroup()

colnames(eras.table)[colnames(eras.table) == "oa2"] <- "opening_acts2"
eras.table$tour_name <- "eras"
# save data----

# master data
setwd("~/R/play/eras_tour/data")
write_csv(eras.table,
          file = "eras_table.csv")



eras_date.geo <- eras.table %>%
  group_by(date, promo_city, country, venue, long, lat) %>%
  summarise()

write_csv(eras_date.geo, 
          "eras_date.geo.csv")

eras_date.oa  <- eras.table[order(eras.table$date),]
eras_date.oa$date_id <- factor(eras_date.oa$date) %>% as.numeric()

cw_date.date_id <- eras_date.oa %>%
  group_by(date_id, date) %>%
  summarise() %>%
  ungroup()

write_csv(cw_date.date_id, 
          "cw_date.date_id.csv")

eras_date.oa <- eras_date.oa %>%
  group_by(date_id, date, opening_acts2, promo_city, country) %>%
  summarise() %>%
  ungroup() %>%
  group_by(opening_acts2) %>%
  summarise(join_tour   = min(date), 
            leave_tour  = max(date),
            join_tour.did = min(date_id),
            leave_tour.did = max(date_id),
            min.date_id = min(date_id), 
            max.date_id = max(date_id), 
            n_dates_on.tour = max.date_id - min.date_id + 1, 
            n_dates_performing = n_distinct(date)) %>%
  ungroup()
write_csv(eras_date.oa, 
          "eras_date.oa.csv")



