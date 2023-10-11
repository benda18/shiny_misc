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

eras.url <- "https://en.wikipedia.org/wiki/Reputation_Stadium_Tour"

eras.html <- rvest::read_html(eras.url)

reputation.table.18 <- rvest::html_element(eras.html, 
                                     #xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[3]") %>%
                                     xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[4]") %>%
  html_table() %>%
  .[!.$City == "Total",]
# reputation.table.24 <- rvest::html_element(eras.html, 
#                                      xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[4]") %>%
#   html_table() %>%
#   .[!.$City == "Total",]

rm(eras.html, eras.url)

# tidy----
reputation.table.18 <- clean_names(reputation.table.18)
#reputation.table.24 <- clean_names(reputation.table.24)

# format as date
reputation.table.18$date <- mdy(paste(reputation.table.18$date_2018, ", 2018", sep = ""))
#reputation.table.24$date <- mdy(paste(reputation.table.24$date_2024, ", 2024", sep = ""))

# remove text dates field
reputation.table.18 <- reputation.table.18[!colnames(reputation.table.18) %in% c("date_2018")]
#reputation.table.24 <- reputation.table.24[!colnames(reputation.table.24) %in% c("date_2024")]

# rbind
reputation.table <- reputation.table.18
rm(reputation.table.18)

# opening_act crosswalk
cw_opening.act <- reputation.table[,c("opening_acts")]

cw_opening.act$opening_acts2 <- cw_opening.act$opening_acts %>%
  gsub(" ", "_", .) %>%
  gsub("Camila_Cabello", "Camila_Cabello ", .) %>%
  gsub("Charli_XCX\\[a\\]|Charli_XCX", "Charli_XCX ", .) %>%
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

reputation.table <- full_join(reputation.table, 
          cw_opening.act[,c("opening_acts", "oa2")])


# further cleanup 
reputation.table$promo_city <- reputation.table$city

# reputation.table$promo_city[grepl("\\[a", reputation.table$promo_city)] <- "Las Vegas"
# reputation.table$promo_city[grepl("\\[d", reputation.table$promo_city)] <- "Los Angeles"
# reputation.table$promo_city[grepl("\\[e", reputation.table$promo_city)] <- "Paris"
# reputation.table$promo_city[grepl("\\[f", reputation.table$promo_city)] <- "Lyon"
# reputation.table$promo_city[grepl("\\[g", reputation.table$promo_city)] <- "Miami"

# remove footnotes
reputation.table$city <- reputation.table$city %>%
  gsub("\\[.*$", "", .) 

# cw_usa.city.state 
cw_usa.citystate <- reputation.table %>%
  .[.$country == "United States",] %>%
  group_by(venue, 
           #city, 
           promo_city, 
           state = NA, 
           country) %>%
  summarise()

View(cw_usa.citystate)

cw_usa.citystate$state <- c("TX", "MO", 
                            "ky", 
                            "wa", "md", 
                            "oh", "mi", 
                            "ma", "fl", 
                            "pa", "ca", 
                            "pa", "in", 
                            "ga", "la", "nj", 
                            "tx", "tn", "oh", 
                            "fl", "ca", "il", 
                            "co", "mo", "mn", 
                            "az") %>%
  toupper()




# crosswalk city_lon.lat
cw_city.lonlat <- reputation.table[,c("city", "country")] %>%
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

all.longlat[is.na(all.longlat$long),]

reputation.table <- left_join(reputation.table, 
          all.longlat, 
          by = c("promo_city" = "city", 
                 "country"))

reputation.table[!complete.cases(reputation.table),] %>%
  group_by(city, promo_city, country, long, lat) %>%
  summarise()

reputation.table$lat[reputation.table$promo_city == "Landover"] <- 38.933333
reputation.table$long[reputation.table$promo_city == "Landover"] <- -76.9
reputation.table$lat[reputation.table$promo_city == "Manchester"] <- 53.479444
reputation.table$long[reputation.table$promo_city == "Manchester"] <- -2.245278
reputation.table$lat[reputation.table$promo_city == "Miami Gardens"] <- 25.941111
reputation.table$long[reputation.table$promo_city == "Miami Gardens"] <- -80.245278
reputation.table$lat[reputation.table$promo_city == "St. Louis"] <- 38.627222
reputation.table$long[reputation.table$promo_city == "St. Louis"] <- -90.197778


reputation.table$promo_city[grepl("Paulo$", reputation.table$city)] <- "Sao Paulo"
reputation.table$promo_city[grepl("^Z.*rich$", reputation.table$city)] <- "Zurich"

reputation.table$lat[reputation.table$promo_city == "Cardiff"] <- 51.483333
reputation.table$long[reputation.table$promo_city == "Cardiff"] <- -3.183333

reputation.table$lat[reputation.table$promo_city == "Lyon"] <- 45.76
reputation.table$long[reputation.table$promo_city == "Lyon"] <- 4.84

reputation.table$lat[reputation.table$promo_city == "East Rutherford"] <- 40.817097
reputation.table$long[reputation.table$promo_city == "East Rutherford"] <- -74.085024

reputation.table$lat[reputation.table$promo_city == "Edinburgh"] <- 55.953333
reputation.table$long[reputation.table$promo_city == "Edinburgh"] <- -3.189167

reputation.table$lat[reputation.table$promo_city == "Foxborough"] <- 42.065278
reputation.table$long[reputation.table$promo_city == "Foxborough"] <- -71.248333

reputation.table$lat[reputation.table$promo_city == "Liverpool"] <- 53.4075
reputation.table$long[reputation.table$promo_city == "Liverpool"] <- -2.991944

reputation.table$lat[reputation.table$promo_city == "London"] <- 51.507222
reputation.table$long[reputation.table$promo_city == "London"] <- -0.1275

reputation.table$lat[reputation.table$promo_city == "Paris"] <- 48.856667
reputation.table$long[reputation.table$promo_city == "Paris"] <- 2.352222

reputation.table$lat[reputation.table$promo_city == "Sao Paulo"] <- -23.55
reputation.table$long[reputation.table$promo_city == "Sao Paulo"] <- -46.633333

reputation.table$lat[reputation.table$promo_city == "Zurich"] <- 47.374444
reputation.table$long[reputation.table$promo_city == "Zurich"] <- 8.541111



# cast opening acts
reputation.table %>%
  group_by(date, city, country, venue, oa2) %>%
  summarise() %>%
  ungroup()

colnames(reputation.table)[colnames(reputation.table) == "oa2"] <- "opening_acts2"
reputation.table$tour_name <- "reputation"

# save data----

# master data
setwd("~/R/play/eras_tour/data")
write_csv(reputation.table,
          file = "reputation_table.csv")



reputation_date.geo <- reputation.table %>%
  group_by(date, promo_city, country, venue, long, lat) %>%
  summarise()

write_csv(reputation_date.geo, 
          "reputation_date.geo.csv")

reputation_date.oa  <- reputation.table[order(reputation.table$date),]
reputation_date.oa$date_id <- factor(reputation_date.oa$date) %>% as.numeric()

cw_date.date_id <- reputation_date.oa %>%
  group_by(date_id, date) %>%
  summarise() %>%
  ungroup()

write_csv(cw_date.date_id, 
          "reputation_date.date_id.csv")

reputation_date.oa <- reputation_date.oa %>%
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
write_csv(reputation_date.oa, 
          "reputation_date.oa.csv")



