# transform images from "hownot2.com" to tabular via ocr
#https://www.phind.com/

library(dplyr)
library(tesseract)
library(readr)
library(magrittr)
library(magick)
library(stringi)
library(pdftools)
library(ggplot2)

#https://stackoverflow.com/questions/54000691/extracting-tables-from-jpeg-into-a-dataframe-in-r

rm(list=ls());cat('\f')
gc()

# wd
wd <- list(img    = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/data/images", 
           data   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/data", 
           R      = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/R", 
           output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/output", 
           shiny  = NA)

# Funs----
pd <- function(img.filename, 
               find.replace = data.frame(from = c(NA), to = c(NA)), 
               colnames.out = NA, 
               skip_n = 0
){
  require(readr)
  require(dplyr)
  file.copy(img.filename, "table.jpg", overwrite = T)
  # read image
  img <- image_read("table.jpg") %>% 
    image_ocr() 
  # preprocessing
  for(i in 1:nrow(find.replace)){
    img <- gsub(pattern = find.replace$from[i], 
                replacement = find.replace$to[i], 
                x = img)
  }
  data1 <- img %>%
    read_lines() %>%
    .[!. %in% c("")]
  # processing
  data.out <- gsub(pattern = "(?<=\\d) | (?=\\d)", 
                   replacement = "_", 
                   x = data1, perl = T) %>%
    #gsub(pattern = " \\(.*\\)$", "", .) %>%
    strsplit(., "_") %>%
    lapply(., paste0, collapse = ",") %>%
    unlist() %>%
    .[(1:length(.)) > skip_n] %>%
    paste0(., collapse = "\n") %>%
    read_csv(., col_names = F)
  # name columns
  colnames(data.out) <- colnames.out
  data.out$filename <- img.filename
  # return
  return(data.out)
}

# get image----

# process images
setwd(wd$img)


# cross loading carabiners----
df_fr <- data.frame(from = c("CAMP Nano 22", "\\|", " \\(3.+tries)"), 
                    to   = c("CAMP Nano22", "I", " "))
co_v <- c("name", "weight_g", "crossload_mbs_Kn", 
          "result_Kn", "pulled_against")
skip_rows <- 3
img.title <- "cross_loading_carabiners.jpg"



dfout_crossloadcarabiners <- pd(img.filename = img.title, 
                                find.replace = df_fr, 
                                colnames.out = co_v, 
                                skip_n = skip_rows)


# save as RData----

setwd(wd$output)
save(list = ls(pattern = "^dfout_"), 
     file = "hownot2.RData")
setwd(wd$img)

# remove files



