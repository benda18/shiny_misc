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

# # Funs----
# pd <- function(img.filename, 
#                find.replace = data.frame(from = c(NA), to = c(NA)), 
#                colnames.out = NA, 
#                skip_n = 0){
#   require(readr)
#   require(dplyr)
#   file.copy(img.filename, "table.jpg", overwrite = T)
#   # read image
#   img <- image_read("table.jpg") %>% 
#     image_ocr() 
#   # preprocessing
#   for(i in 1:nrow(find.replace)){
#     img <- gsub(pattern = find.replace$from[i], 
#                 replacement = find.replace$to[i], 
#                 x = img)
#   }
#   data1 <- img %>%
#     read_lines() %>%
#     .[!. %in% c("")]
#   # processing
#   data.out <- gsub(pattern = "(?<=\\d) | (?=\\d)", 
#                    replacement = "_", 
#                    x = data1, perl = T) %>%
#     #gsub(pattern = " \\(.*\\)$", "", .) %>%
#     strsplit(., "_") %>%
#     lapply(., paste0, collapse = ",") %>%
#     unlist() %>%
#     .[(1:length(.)) > skip_n] %>%
#     paste0(., collapse = "\n") %>%
#     read_csv(., col_names = F)
#   # name columns
#   colnames(data.out) <- colnames.out
#   data.out$filename <- img.filename
#   # return
#   return(data.out)
# }
# 
# # get image----
# 
# # process images
# setwd(wd$img)
# 
# 
# # cross loading carabiners----
# df_fr <- data.frame(from = c("CAMP Nano 22", "\\|", " \\(3.+tries)", 
#                              "nla fresh"), 
#                     to   = c("CAMP Nano22", "I", " ", 
#                              "n/a fresh"))
# co_v <- c("name", "weight_g", "crossload_mbs_Kn", 
#           "result_Kn", "pulled_against")
# skip_rows <- 3
# img.title <- "cross_loading_carabiners.jpg"
# 
# 
# 
# dfout_crossloadcarabiners <- pd(img.filename = img.title, 
#                                 find.replace = df_fr, 
#                                 colnames.out = co_v, 
#                                 skip_n = skip_rows)
# 
# 
# # cowtails_drop_test----
# df_fr <- data.frame(from = c("CAMP Nano 22", "\\|", " \\(3.+tries)", 
#                              "nla fresh"), 
#                     to   = c("CAMP Nano22", "I", " ", 
#                              "n/a fresh"))
# co_v <- c(#"test", 
#           "material", "length", "knot", "kn", "notes")
# skip_rows <- 0
# img.title <- "cows_tail_drop_test_slowpull.jpg"
# 
# dfout_cowstail.slow <- pd(img.filename = img.title, 
#                                 find.replace = df_fr, 
#                                 colnames.out = co_v, 
#                                 skip_n = skip_rows)
# 
# 
# 
# dfout_cowstail.200lbs <- data.frame(material = c(NA), 
#                                     length   = c(NA), 
#                                     knot     = c(NA), 
#                                     kn       = c(NA), 
#                                     notes    = c(NA), 
#                                     filename = "cows_tail_drop_test_slowpull.jpg")
# 
# 
# dfout_cowstail.300lbs <- data.frame(material = c(rep("dynamic", 8), 
#                                                  rep("semi-static", 5), 
#                                                  rep("static", 3), 
#                                                  rep("petzl static sling", 2)), 
#                                     length = c(rep("short",3), rep("long",5), 
#                                                rep("short",3), rep("long", 2), 
#                                                rep("long", 5)), 
#                                     knot = c(1,2,1,1,2,2,1,1,
#                                              1,2,1,1,2,1,1,1, 
#                                              "n/a", "n/a"), 
#                                     kn = c(6.76, 10.43,  6.59,  9.44, 12.49, 12.87,  9.54,  8.41, 
#                                            10.21, 17.04,  8.92, 12.61, 17.94, 
#                                            13.37, 14.10, 
#                                            13.61, 12.38, 17.38),
#                                     notes = NA, 
#                                     filename = "cows_tail_drop_test_300lbs.jpg") %>%
#   as_tibble() %>%
#   mutate(., 
#          knot = ifelse(knot == "1", "fresh", knot), 
#          knot = ifelse(knot == "2", "rock hard", knot))
# 
# dfout_cowstail <- rbind(dfout_cowstail.300lbs,
#       dfout_cowstail.200lbs) %>%
#   rbind(., 
#         dfout_cowstail.slow)
# 
# rm(dfout_cowstail.200lbs, dfout_cowstail.300lbs, dfout_cowstail.slow)
# 

# ropes in sharp hangers----
# col_kn <- image_read(path = "table.jpg") %>%
#   image_ocr() %>% read_lines()
# col_hanger <- image_read(path = "table.jpg") %>%
#   image_ocr() %>% read_lines() %>% .[!. %in% ""]
# col_rope <- image_read(path = "table.jpg") %>%
#   image_ocr() %>% read_lines()
# col_config <- image_read(path = "table.jpg") %>%
#   image_ocr() %>% read_lines()
# col_notes <- image_read(path = "table.jpg") %>%
#   image_ocr() %>% read_lines() %>%
#   .[!. %in% c("Notes", "")]


(dfout_ropesinshaprhangers <- data.frame(hanger = col_hanger, 
                                        rope = col_rope,
                                        configuration = col_config,
                                        results.Kn = col_kn, 
                                        notes = col_notes) %>%
  as_tibble())


# climbing gym falls----
hardiseasy_640hz <- image_read(path = "table.jpg") %>%
  image_ocr() %>% 
  gsub(" ", ", ", .) 

hardiseasy_640hz <- "4.01, 3.56, 4.09, 3.95, 4.64, 2.39, 2.73, 3.22, 3.76, 3.93, 4.53, 3.92\n
1.53, 1.29, 1.66, 1.46, 1.87, 0.54, 0.85, 0.59, 1.32, 1.26, 0.83, 0.54\n
2.46, 2.35, 2.56, 2.34, 2.89, 1.41, 1.68, 2.11, 2.56, 2.75, 2.92, 2.36\n
3.99, 3.64, 4.22, 3.8, 4.76, 1.95, 2.53, 2.7, 3.88, 4.01, 3.75, 2.9\n
0.02, -0.08, 0.13, 0.15, -0.12, 0.44, 0.2, 0.52, -0.12, -0.08, 0.78, 1.02\n" %>% 
  read_csv(., col_names = F)

hardiseasy_640hz <- t(hardiseasy_640hz) %>% 
  as.data.frame()

colnames(hardiseasy_640hz) <- c('quickdraw', "belayer", "climber", 
                                "belayer_climber", "friction_difference")

dfout_hardiseasy_640hz <- hardiseasy_640hz

# save as RData----

setwd(wd$output)

load(file = "hownot2.RData")

save(list = ls(pattern = "^dfout_"), 
     file = "hownot2.RData")
setwd(wd$img)

# remove files
getwd()

remove.jpg <- list.files(pattern = "\\.jpg")

file.remove(remove.jpg)



