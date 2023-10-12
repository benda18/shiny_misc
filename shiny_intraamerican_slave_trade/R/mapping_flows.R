library(sankey)
library(alluvial)
library(ggplot2)
library(data.table)
library(lubridate)


rm(list=ls());cat('\f');gc()

# Resources----
# https://www.analyticsvidhya.com/blog/2022/06/data-visualisation-alluvial-diagram-vs-sankey-diagram/
# https://r-graph-gallery.com/sankey-diagram.html
# https://r-spatial.org/projects/
# https://commons.wikimedia.org/wiki/File:African_Slave_Trade.png

# base-R group_by / summarise:
# https://stackoverflow.com/questions/56024442/base-r-instead-of-dplyr-group-and-summarise-the-data


# wd----

wd <- {list(home = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade", 
            data = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/data", 
            output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/output",
            R = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/R", 
            shiny = NA, 
            cw = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/crosswalks")
}

# load data----
setwd(wd$data)
iast <- read.csv(file = "I-Am1.0.csv", stringsAsFactors = F) |>
  as.data.table()

broad.reg <- data.frame(name = c("Africa", "Caribbean", "Brazil",  "Europe", "Spanish Mainland America", "Mainland North America"), 
                        x    = c(18.20682, -75.31008,   -53.05434, 11.87362, -99.26491,                  -103.3469), 
                        y    = c(6.450174,  20.35586,   -10.80677, 56.67009,  21.78775,                   45.70563), 
                        stringsAsFactors = F)

# load crosswalks---
setwd(wd$cw)
for(i in list.files(pattern = "^cw_.*\\.csv$")){
  print(i)
  assign(value     = read.csv(file = i, stringsAsFactors = F), 
         x = gsub("\\.csv", "", i))
}
rm(i)


# join places to iast----
cw_voyage.itinerary
cw_imp_yoyage.itinerary 
cw_voyage.dates 
cw_imp_voyage.dates 

iast2 <- as.data.frame(iast) %>% as_tibble()
iast2 <- iast2[colnames(iast2) %in% c(tolower(cw_voyage.itinerary$colnames), 
                             "IntraAmer", "voyageid", 
                             tolower(cw_imp_yoyage.itinerary$value))] %>%
  as_tibble()


iast2 <- iast2 %>%
  as.data.table() %>%
  melt(., id.vars = c("IntraAmer", "voyageid"), 
       variable.factor = F) %>% 
  as_tibble() %>%
  group_by_all() %>%
  summarise() %>% 
  ungroup() 

iast2 <- iast2[!iast2$variable %in% unique(iast2[grepl("00$", iast2$value),]$variable),]

iast2 <- left_join(iast2, 
          as_tibble(cw_specific_places[,c("place_value", 
                                          "place_port_or_location", 
                                          "specific_region_country_or_colony",
                                          "broad_region")]), 
          by = c("value" = "place_value"))

cw_voyage.itinerary2 <- cw_voyage.itinerary
colnames(cw_voyage.itinerary2) <- c("value", "def", "cat")

col_defs_voyage <- rbind(cw_imp_yoyage.itinerary, 
      cw_voyage.itinerary2)
col_defs_voyage$value <- tolower(col_defs_voyage$value)
colnames(col_defs_voyage) <- c("variable", "var_def", "var_table")

col_defs_voyage$var_table[col_defs_voyage$var_table == "voyage.itinerary"] <- "cw_voyage.itinerary"

iast2 <- left_join(iast2, 
          col_defs_voyage) 

iast3 <- iast2[!is.na(iast2$value),]

iast3[order(iast3$voyageid),]$var_def %>% unique %>% sort

?igraph::graph_from_literal()
graph_from_literal("tim--bob,bob--tracy") %>%plot
# test plot----
broad.reg

?sankey::make_sankey()
args(make_sankey)

?sankey::sankey()
args(sankey)

ed1 <- read.table(stringsAsFactors = FALSE, textConnection(
  "                get_deps          get_description
                 get_deps               parse_deps
                 get_deps                     %||%
                 get_deps            drop_internal
          get_description        pkg_from_filename
               parse_deps                 str_trim
                cran_file             get_pkg_type
                cran_file          r_minor_version
            download_urls split_pkg_names_versions
            download_urls                cran_file
             pkg_download               dir_exists
             pkg_download            download_urls
             pkg_download        filename_from_url
             pkg_download             try_download
                  restore             pkg_download
                  restore        drop_missing_deps
                  restore            install_order
                  restore                 get_deps
 split_pkg_names_versions               data_frame
"))
pkgsnap_sankey <- make_sankey(nodes = NULL, 
                              edges = ed1, 
                              y     = c("optimal"), 
                              break_edges = F, 
                              gravity = "center")
sankey(pkgsnap_sankey)

# test alluvial----

?alluvial

# Titanic data
tit <- as.data.frame(Titanic) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(., 
         survived = ifelse(survived == "Yes", 
                           "survived", 
                           "perished"))
tit

(rw1 <- runif(1,0,0.5))
(cw1 <- runif(1,0,1))

# 2d
tit2d <- aggregate(x = freq ~ class + survived, data=tit, sum)
alluvial( tit2d[,1:2], 
          freq=tit2d$freq, 
          #xw=0.0, 
          xw = rw1,
          cw = 0.1,
          #cw = cw1,
          alpha=0.8,
          gap.width=0.1, 
          col= c(rep("red",4), 
                 rep("steelblue",4)), 
          border="white",
          layer = tit2d$survived == "perished" )


