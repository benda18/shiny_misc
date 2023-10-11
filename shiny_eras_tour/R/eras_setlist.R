# eras setlists

library(dplyr)
library(lubridate)
library(readr)
library(glue)

# "https://www.setlist.fm/forum/setlistfm/setlistfm-api/sytnax-for-api-call-in-r-13d699c1"
# "https://api.setlist.fm/docs/1.0/index.html"

setwd("~/R/play/eras_tour/data")
rm(list=ls());cat('\f');gc()

# funs----
parse_pl <- function(txt, t.date){
  rm.lines <- c("\\(shortened\\)", 
                "\\(spoken intro\\)",
                "\\(extended intro; shortened\\)",
                "\\(extended intro; with keys; shortened\\)",
                "\\(extended outro\\)",
                "\\(extended intro\\)", 
                "\\(live debut, on guitar\\)", 
                "\\(tour debut; on guitar\\)",
                "\\(on piano; double repeat\\)",
                "\\(double repeat; on piano\\)", 
                "\\(spoken word poem; contains… more \\)", 
                "\\(contains elements of \"State… more \\)", 
                "\\(contains elements of State… more \\)",
                "\\(10 minute version; spoken intro\\)")
  
  txt2 <- txt %>%
    trimws() %>%
    gsub(paste(rm.lines, sep = "|", collapse = "|"), 
         "", .) %>%
    gsub("\n\n", "\n", .) %>%
    gsub("\nPlay Video", 
         "_s", 
         .) %>%
    strsplit(., "\n") %>%
    unlist() %>% 
    trimws()
  
  # catch anything that slipped through (with parens)
  txt2 <- txt2 %>%
    gsub("^\\(.*\\)_s$", "_s",.) %>%
    paste(., sep = "@", collapse = "@") %>%
    gsub("@_s", "_s", .) %>%
    strsplit(., "@") %>%
    unlist()
  
  # catch sons w/o videos
  
  df.txt <- data.frame(song = txt2, 
                       line_type = NA_character_, 
                       album = NA_character_) %>%
    as_tibble() %>%
    mutate(., 
           line_type = ifelse(grepl("_s$", song), 
                              "song", "album"))
  
  df.txt[df.txt$line_type == "album",]
  
  df.txt$song <- df.txt$song %>%
    gsub("_s$", "", .)
  
  for(i in 1:nrow(df.txt)){
    if(i == 1){
      df.txt$album[i] <- df.txt$song[i]
    }else{
      if(df.txt$line_type[i] == "song"){
        df.txt$album[i] <- df.txt$album[i-1]
      }else{
        df.txt$album[i] <- df.txt$song[i]
      }
    }
    
  }
  df.txt <- df.txt[!df.txt$line_type == "album",]
  df.txt$song_order <- 1:nrow(df.txt)
  df.txt$date <- t.date
  return(df.txt)
}

# save_sl----
# https://www.setlist.fm/search?artist=3bd6bc5c&query=tour:%28The+Eras+Tour%29

# vars2----
a.date <- 20230827
x.txt <- {" Lover
Miss Americana & the Heartbreak Prince
(shortened)
Play Video
Cruel Summer
Play Video
The Man
Play Video
You Need to Calm Down
(shortened)
Play Video
Lover
(spoken intro)
Play Video
The Archer
(extended outro)
Play Video
Fearless
Fearless
(shortened)
Play Video
You Belong With Me
Play Video
Love Story
Play Video
evermore
'tis the damn season
(shortened)
Play Video
willow
Play Video
marjorie
(shortened)
Play Video
champagne problems
(spoken intro)
Play Video
tolerate it
(extended intro; with keys; shortened)
Play Video
reputation
...Ready for It?
Play Video
Delicate
Play Video
Don't Blame Me
(shortened)
Play Video
Look What You Made Me Do
Play Video
Speak Now
Enchanted
(extended intro; shortened)
Play Video
Long Live
Play Video
Red
Red - Intro
(contains elements of State… more )
Play Video
22
Play Video
We Are Never Ever Getting Back Together
Play Video
I Knew You Were Trouble
(shortened)
Play Video
All Too Well
(10 minute version; spoken intro)
Play Video
folklore
seven
(spoken word poem; contains… more )
Play Video
the 1
Play Video
betty
(spoken intro)
Play Video
the last great american dynasty
Play Video
august
Play Video
illicit affairs
(shortened)
Play Video
my tears ricochet
Play Video
cardigan
Play Video
1989
Style
(shortened)
Play Video
Blank Space
Play Video
Shake It Off
Play Video
Wildest Dreams
(shortened)
Play Video
Bad Blood
(shortened)
Play Video
Surprise Songs
Afterglow
(live debut, on guitar)
Play Video
Maroon
(double repeat; on piano)
Play Video
Midnights
Lavender Haze
Play Video
Anti‐Hero
Play Video
Midnight Rain
Play Video
Vigilante Shit
Play Video
Bejeweled
Play Video
Mastermind
Play Video
Karma
(extended outro)
Play Video
"}

assign(glue("sl_out"), parse_pl(x.txt, ymd(a.date)))
write_csv(sl_out, glue("sl_{a.date}.csv"))
rm(x.txt,sl_out,a.date)
