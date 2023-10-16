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


# get image----
setwd(wd$img)

df.text.fixes <- data.frame(from1 = c("CAMP Nano 22", "\\|"), 
                            to1   = c("CAMP Nano22", "I"))
colnames.out <- c("name", "weight_g", "crossload_mbs_Kn", 
              "result_Kn", "pulled_against")
skip_n <- 3
img.title <- "cross_loading_carabiners"
img.url   <- "https://static.wixstatic.com/media/c990e4_1f596631067f4cf38c9e79a6a72ba0de~mv2.jpg"

# # download file
# download.file(img.url, destfile = paste0(img.title,".jpg"))    # for archive
# file.copy(from = paste0(img.title,".jpg"), to   = "table.jpg", 
#           overwrite = ) # for processing

file.copy("cross_loading_carabiners.jpg", "table.jpg", overwrite = T)

# read image
img <- image_read("table.jpg") %>% 
  image_ocr() #%>%
  # image_crop(geometry_area(y = 35))  %>%
  # image_transparent("white", fuzz=50) %>%
  # image_background("white") %>%
  # image_negate() %>%
  # image_morphology(method = "Thinning", kernel = "Rectangle:20x1+0+0^<") %>%
  # image_negate()

# preprocessing

for(i in 1:nrow(df.text.fixes)){
  img <- gsub(pattern = df.text.fixes$from1[i], 
       replacement = df.text.fixes$to1[i], 
       x = img)
}


data1 <- img %>%
  read_lines() %>%
  .[!. %in% c("")]




data.out <- gsub(pattern = "(?<=\\d) | (?=\\d)", 
     replacement = "_", 
     x = data1, perl = T) %>%
  gsub(pattern = " \\(.*\\)$", "", .) %>%
  strsplit(., "_") %>%
  lapply(., paste0, collapse = ",") %>%
  unlist() %>%
  .[(1:length(.)) > skip_n] %>%
  paste0(., collapse = "\n") %>%
  read_csv(., col_names = F)

colnames(data.out) <- colnames.out

data.out

ggplot(data = data.out, 
       aes(x = crossload_mbs_Kn, y = result_Kn))+
  geom_point()+
  geom_function(fun = function(x) x , 
                linetype = 2, 
                aes(color = ("safe line")))+
  xlim(0,NA)+
  ylim(0,NA)

