# transform images from "hownot2.com" to tabular via ocr
#https://www.phind.com/

library(dplyr)
library(tesseract)
library(readr)
library(magrittr)
library(magick)
library(stringi)
library(pdftools)

#https://stackoverflow.com/questions/54000691/extracting-tables-from-jpeg-into-a-dataframe-in-r

rm(list=ls());cat('\f')
gc()

# wd
wd <- list(img    = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/data/images", 
           data   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/data", 
           R      = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/R", 
           output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/output", 
           shiny  = NA)


# get image----
setwd(wd$img)


img.title <- "cross_loading_carabiners"
img.url   <- "https://static.wixstatic.com/media/c990e4_1f596631067f4cf38c9e79a6a72ba0de~mv2.jpg"

# # download file
# download.file(img.url, destfile = paste0(img.title,".jpg"))    # for archive
# file.copy(from = paste0(img.title,".jpg"), to   = "table.jpg", 
#           overwrite = ) # for processing

file.copy("cross_loading_carabiners.png", "table.png")

# convert to PDF


# preprocessing
img <- image_read("table.png") %>% 
  image_crop(geometry_area(y = 35))  %>% 
  image_transparent("white", fuzz=50) %>% 
  image_background("white") %>%
  image_negate() %>%
  image_morphology(method = "Thinning", kernel = "Rectangle:20x1+0+0^<") %>%
  image_negate()

img

#image_read("table.png") %>% image_ocr() %>% read_tsv()

temp.data <- img %>%
  image_ocr()

temp.data %>%
  stri_split(fixed = "\n") %>%
  purrr::map(~ stringi::stri_split(str = ., fixed = "‘")) %>%
  .[[1]] %>%
  purrr::map_df(~ tibble::tibble(name = .[1], 
                                 weight = .[2], 
                                 crossload_Kn = .[3],
                                 result_Kn = .[4], 
                                 pa = .[5])) %>%
  dplyr::glimpse()
