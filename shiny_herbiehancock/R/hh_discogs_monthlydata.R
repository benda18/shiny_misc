#library(tesseract)
library(dplyr)
library(xml2)
library(XML)
library(readr)

rm(list=ls());cat('\f')

setwd("~/R/play/shiny_misc/shiny_herbiehancock/data")

xml.files <- list.files(pattern="\\.xml$")

#artist <- xml2::read_xml(xml.files)
#XML::xmlToList(xml.files)

temp <- read_lines(file = xml.files)

which.hh <- which(grepl("<name>Herbie Hancock</name>", x = temp, ignore.case = T))

# hh.artist <- 6803:6808

hh_main_artist_ids <- paste0(temp[which.hh],collapse = "") %>%
  #gsub("^</profile>", "", .) %>%
  gsub("<", "\n<", .) %>%
  gsub("\n</", "</", .) %>%
  gsub("><", ">\n<", .) %>% 
  gsub("<artist>", "@@__@@<artist>", .) %>%
  strsplit(., split = "@@__@@") %>%
  unlist() %>%
  #paste0(., collaspe = "\n") %>% 
  gsub("<images>.*</images>", "", .) %>%
  gsub("<namevariations>.*</namevariations>", "", .) %>%
  gsub("<members>.*</members>", "", .) %>%
  gsub("<profile>.*</profile>", "", .) %>%
  gsub("<urls>.*</urls>", "", .) %>%
  gsub("<data_quality>.*</data_quality>", "", .) %>%
  gsub("<profile>American pianist, keyboardist, composer, band leader. &#13;", "", .) %>%
  gsub("<aliases>.*</aliases>", "", .) %>%
  gsub("<realname>.*</realname>", "", .) %>%
  gsub("<artist>|</artist>", "", .) %>%
  #gsub("\n{1,}</artist>", "\n</artist>", .) %>%
  gsub("\n", "", .) %>% 
  .[. != ""] %>%
  strsplit(., split = "</id><name>") %>%
  lapply(X = ., FUN = gsub, pattern = "<.*>", replacement = "") %>%
  unlist() %>%
  matrix(., ncol = 2, byrow = T) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(., 
         id = as.numeric(V1), 
         name = V2) %>%
  .[!colnames(.) %in% c("V1", "V2")]
  
# export to output----
setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")

save(list = "hh_main_artist_ids", 
     file = "hh_main_artist_ids.RData")

setwd("~/R/play/shiny_misc/shiny_herbiehancock/data")


paste0(1:9,"A", sep = "|", collapse = "")
hh_main_artist_ids$id



temp.members <- temp[grepl(pattern = paste0("<id>", hh_main_artist_ids$id, "</id>", 
                                            sep = "|", collapse = ""), 
                           x = temp)]
gc()