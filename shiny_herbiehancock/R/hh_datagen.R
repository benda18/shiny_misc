#library("tidyverse")
library(lubridate)
library(ggplot2)
library(dplyr)


setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")
rm(list=ls());cat('\f');gc()

# Funs----
fun_artist <- function(title, year, artist, 
                       instrument, label, instrument_type = NA) {
  df <- data.frame(title = title, 
                   year = ymd(year), 
                   artist = artist, 
                   instrument = instrument,
                   instrument_type = instrument_type, 
                   label = label)
  return(df)
}


# manual data----
discog <- rbind(fun_artist("takin' off", 19620528, 
                           "Dexter Gordon", "tenor sax", "blue note"),
                fun_artist("takin' off", 19620528, 
                           "Herbie Hancock", "piano", "blue note"), 
                fun_artist("takin' off", 19620528,
                           "Billy Higgins", "drums", "blue note"),
                fun_artist("takin' off", 19620528,
                           "Freddie Hubbard", "trumpet", "blue note"),
                fun_artist("takin' off", 19620528,
                           "Freddie Hubbard", "flugelhorn", "blue note"),
                fun_artist("takin' off", 19620528,
                           "Butch Warren", "bass", "blue note"),
                fun_artist("my point of view", 19630319,
                           "Donald Byrd", "trumpet", "blue note"), 
                fun_artist("my point of view", 19630319,
                           "Grant Green", "guitar", "blue note"), 
                fun_artist("my point of view", 19630319,
                           "Herbie Hancock", "piano", "blue note"), 
                fun_artist("my point of view", 19630319,
                           "Herbie Hancock", "vocals", "blue note"),
                fun_artist("my point of view", 19630319,
                           "Chuck Israels", "bass", "blue note"), 
                fun_artist("my point of view", 19630319,
                           "Hank Mobley", "tenor sax", "blue note"),
                fun_artist("my point of view", 19630319,
                           "Grachan Moncur III", "trombone", "blue note"),
                fun_artist("my point of view", 19630319,
                           "Tony Williams", "drums", "blue note"),
                fun_artist("inventions & dimensions", year = 19630830, 
                           label = "blue note", artist = "Willie Bobo", 
                           instrument = "drums"), 
                fun_artist("inventions & dimensions", year = 19630830, 
                           label = "blue note", artist = "Paul Chambers",
                           instrument = "bass"),
                fun_artist("inventions & dimensions", year = 19630830, 
                           label = "blue note", artist = "Herbie Hancock", 
                           instrument = "piano"), 
                fun_artist("inventions & dimensions", year = 19630830, 
                           label = "blue note", artist = "Osvaldo Martinez", 
                           instrument = "percussion"),
                fun_artist(title = "empyriean isles", year = 19641201, 
                           label = "blue note", artist = "Ron Carter", 
                           instrument = "bass"),
                fun_artist(title = "empyriean isles", year = 19641201, 
                           label = "blue note", artist = "Herbie Hancock", 
                           instrument = "piano"),
                fun_artist(title = "empyriean isles", year = 19641201, 
                           label = "blue note", artist = "Freddie Hubbard", 
                           instrument = "cornet"),
                fun_artist(title = "empyriean isles", year = 19641201, 
                           label = "blue note", artist = "Freddie Hubbard", 
                           instrument = "flugelhorn"),
                fun_artist(title = "empyriean isles", year = 19641201, 
                           label = "blue note", artist = "Tony Williams", 
                           instrument = "drums"),
                fun_artist(title = "maiden voyage", year = 19650501, 
                           label = "blue note", artist = "Ron Carter", 
                           instrument = "bass"),
                fun_artist(title = "maiden voyage", year = 19650501, 
                           label = "blue note", artist = "George Coleman", 
                           instrument = "tenor sax"),
                fun_artist(title = "maiden voyage", year = 19650501, 
                           label = "blue note", artist = "Herbie Hancock", 
                           instrument = "piano"),
                fun_artist(title = "maiden voyage", year = 19650501, 
                           label = "blue note", artist = "Freddie Hubbard", 
                           instrument = "trumpet"),
                fun_artist(title = "maiden voyage", year = 19650501, 
                           label = "blue note", artist = "Tony Williams", 
                           instrument = "drums"),
                fun_artist(title = "speak like a child", year = 19680306, 
                           label = "blue note", artist = "Ron Carter", 
                           instrument = "double bass", instrument_type = "string"),
                fun_artist(title = "speak like a child", year = 19680306, 
                           label = "blue note", artist = "Jerry Dodgion", 
                           instrument = "alto flute", instrument_type = "woodwind"),
                fun_artist(title = "speak like a child", year = 19680306, 
                           label = "blue note", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = "keyboard"),
                fun_artist(title = "speak like a child", year = 19680306, 
                           label = "blue note", artist = "Thad Jones", 
                           instrument = "flugelhorn", instrument_type = ""),
                fun_artist(title = "speak like a child", year = 19680306, 
                           label = "blue note", artist = "Peter Phillips", 
                           instrument = "bass trombone", instrument_type = "brass"),
                fun_artist(title = "speak like a child", year = 19680306, 
                           label = "blue note", artist = "Mickey Roker", 
                           instrument = "drums", instrument_type = "percussion"),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Garnett Brown", 
                           instrument = "trombone", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Johnny Coles", 
                           instrument = "flugelhorn", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Johnny Coles", 
                           instrument = "trumpet", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = "keyboard"),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Herbie Hancock", 
                           instrument = "electric piano", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Albert Heath", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Joe Henderson", 
                           instrument = "flute", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Joe Henderson", 
                           instrument = "tenor sax", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Jack Jeffers", 
                           instrument = "trombone", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Hubert Laws", 
                           instrument = "flute", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Romeo Penque", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Jerome Richardson", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Jerome Richardson", 
                           instrument = "flute", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Tony Studd", 
                           instrument = "bass trombone", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Oren Waters", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "the prisoner", year = 19690401, 
                           label = "blue note", artist = "Buster Williams", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Garnett Brown", 
                           instrument = "trombone", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Johnny Coles", 
                           instrument = "flugelhorn", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Johnny Coles", 
                           instrument = "horn", instrument_type = "brass"),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Johnny Coles", 
                           instrument = "trumpet", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "keyboards", instrument_type = "keyboard"),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "electric piano", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Billy Hart", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Billy Hart", 
                           instrument = "percussion", instrument_type = "percussion"),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Albert Heath", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Joe Henderson", 
                           instrument = "alto flute", instrument_type = "woodwind"),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Joe Henderson", 
                           instrument = "tenor sax", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "fat albert rotunda", year = 19690601, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "electric bass", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Jose Chepito Areas", 
                           instrument = "percussion", instrument_type = "percussion"),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Leon Chancler", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Leon Chancler", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = ""),
                # fun_artist(title = "mwandishi", year = 19701201, 
                #            label = "warner bros", artist = "Joe Farrell", 
                #            instrument = "synthesizer", instrument_type = "keyboard"),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Billy Hart", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Billy Hart", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Eddie Henderson", 
                           instrument = "flugelhorn", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Eddie Henderson", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Eddie Henderson", 
                           instrument = "trumpet", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Joe Henderson", 
                           instrument = "alto flute", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "alto flute", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Ronnie Montrose", 
                           instrument = "guitar", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Julian Priester", 
                           instrument = "trombone", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "mwandishi", year = 19701201, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Scott Beach", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Scott Beach", 
                           instrument = "voices", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Victor Domagalski", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Patrick Gleeson", 
                           instrument = "synthesizer", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "mellotron", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Herbie Hancock", 
                           instrument = "electric piano", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Billy Hart", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Billy Hart", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Eddie Henderson", 
                           instrument = "flugelhorn", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Eddie Henderson", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Eddie Henderson", 
                           instrument = "trumpet", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Delta Horne", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Delta Horne", 
                           instrument = "voices", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Candy Love", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Candy Love", 
                           instrument = "voices", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "alto flute", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "piccolo", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Bennie Maupin", 
                           instrument = "soprano sax", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Victor Pantoja", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Julian Priester", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Julian Priester", 
                           instrument = "trombone", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Sandra Stevens", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Sandra Stevens", 
                           instrument = "voices", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "electric bass", instrument_type = ""),
                fun_artist(title = "crossings", year = 19720101, 
                           label = "warner bros", artist = "Buster Williams", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Scott Beach", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buck Clarke", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Victor Domagalski", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Patrick Gleeson", 
                           instrument = "synthesizer", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp echoplex", instrument_type = "keyboard"),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "hand clap", instrument_type = "percussion"),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "clavinet", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "fender rhodes", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "fuzz-wah", instrument_type = "keyboard"),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "mellotron", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "electric piano", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Billy Hart", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Billy Hart", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Eddie Henderson", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Eddie Henderson", 
                           instrument = "trumpet", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Delta Horne", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Candy Love", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "afuche", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "nose flute", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "piccolo", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "soprano sax", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Julian Priester", 
                           instrument = "cowbell", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Julian Priester", 
                           instrument = "trombone", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Julian Priester", 
                           instrument = "alto trombone", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Julian Priester", 
                           instrument = "bass trombone", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Julian Priester", 
                           instrument = "tenor trombone", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Sandra Stevens", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buster Williams", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buster Williams", 
                           instrument = "electric bass", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buster Williams", 
                           instrument = "fuzz bass", instrument_type = ""), 
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buster Williams", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buster Williams", 
                           instrument = "wah wah bass", instrument_type = ""),
                fun_artist(title = "sextant", year = 19730301, 
                           label = "columbia", artist = "Buster Williams", 
                           instrument = "wah wah guitar", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp odyssey", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp synthesizer", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "clavinet", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "fender rhodes", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "piano", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "electric piano", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "pipe", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "bass", instrument_type = ""), 
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "electric bass", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "marimba", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "marimbula", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "marimbula", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "electric bass", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Harvey Mason Jr", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "flute", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "alto flute", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "reeds", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "soprano sax", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "tenor sax", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "saxello", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "saxophone", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Oren Waters", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "agogo", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "balafon", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "bottle", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "cabasa", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "congas", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "drums", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "gankogui", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "hinedewho", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "log drums", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "percussion", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "shakere", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "surdo", instrument_type = "percussion"),
                fun_artist(title = "head hunters", year = 19731013, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "tambourine", instrument_type = "percussion"),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Mike Clark", 
                           instrument = "drums", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp 2600", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp odyssey", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp strings", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "arp synthesizer", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "clavinet", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "fender rhodes", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "electric piano", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "synthesizer", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "keyboards", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Herbie Hancock", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "bass", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Paul Jackson Jr", 
                           instrument = "electric bass", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "bass clarinet", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "alto flute", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "soprano sax", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "tenor sax", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "saxello", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bennie Maupin", 
                           instrument = "saxophone", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Bill Summers", 
                           instrument = "percussion", instrument_type = ""),
                fun_artist(title = "thrust", year = 19740901, 
                           label = "columbia", artist = "Oren Waters", 
                           instrument = "vocals", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Herbie Hancock", "piano", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Herbie Hancock", "keyboards", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bud Brisbois", "trumpet", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Garnett Brown", "trombone", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Dick Hyde", "trombone", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Dick Hyde", "tuba", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Wayne Shorter", "alto sax", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Wayne Shorter", "soprano sax", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "soprano sax", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "tenor sax", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "bass clarinet", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "alto flute", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "bass flute", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "saxello", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bennie Maupin", "percussion", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Jim Horn", "flute", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Jim Horn", "saxophone", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Dewayne McKnight", "guitar", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "David Walker", "guitar", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Wah Wah Watson", "guitar", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Wah Wah Watson", "synthesizer", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Wah Wah Watson", "voice bag", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Henry Davis", "electric bass", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Paul Jackson Jr", "electric bass", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Louis Johnson", "electric bass", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Mike Clark", "drums", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "James Gadson", "drums", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Harvey Mason Jr", "drums", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Stevie Wonder", "harmonica", instrument_type = ""),
                fun_artist(title = "man-child", year = 19750822, label = "columbia", 
                           "Bill Summers", "percussion", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "James Gadson", "drums", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "piano", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "fender rhodes", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "electric piano", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "arp odyssey", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "arp string ensemble", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "clavinet", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "micromoog", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "oberheim 4 voice", instrument_type = "vocal"),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Herbie Hancock", "echoplex", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Bennie Maupin", "soprano sax", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Bennie Maupin", "tenor sax", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Bennie Maupin,", "saxello", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Bennie Maupin", "lyricon", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Bennie Maupin", "bass clarinet", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Ray Parker Jr", "guitar", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Ray Parker Jr", "backing vocals", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Paul Jackson Jr", "bass", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "James Levi", "drums", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Kenneth Nash", "percussion", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Wah Wah Watson", "guitar", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Wah Wah Watson", "maestro universal synthesizer", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Wah Wah Watson", "voice bag", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Wah Wah Watson", "vocals", instrument_type = ""),
                fun_artist(title = "secrets", year = 19760801, label = "columbia", 
                           "Wah Wah Watson", "bass", instrument_type = ""),
                fun_artist(title = "herbie hancock trio", year = 19770921, label = "cbs/sony", 
                           "Herbie Hancock", "piano", instrument_type = ""),
                fun_artist(title = "herbie hancock trio", year = 19770921, label = "cbs/sony", 
                           "Ron Carter", "bass", instrument_type = ""),
                fun_artist(title = "herbie hancock trio", year = 19770921, label = "cbs/sony", 
                           "Tony Williams", "drums", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Herbie Hancock", "keyboards", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Herbie Hancock", "synthesizer", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Herbie Hancock", "vocoder", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Patrick Gleeson", "synthesizer", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Bennie Maupin", "soprano sax", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Wah Wah Watson", "guitar", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Ray Parker Jr", "guitar", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Byron Miller", "electric bass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Paul Jackson Jr", "electric bass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Jaco Pastorius", "electric bass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Leon Chancler", "drums", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "James Levi", "drums", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Harvey Mason Sr", "drums", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Tony Williams", "drums", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Raul Rekow" , "percussion", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Bill Summers", "percussion", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Baba Daru", "tabla", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Bobby Shew", "brass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Maurice Spears", "brass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Robert O'Bryant", "brass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Garnett Brown", "brass", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Ernest J Watts", "woodwinds", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Fred Jackson Jr", "woodwinds", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Jack Nimitz", "woodwinds", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "David Willard Riddles", "woodwinds", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Terry Adams", "strings", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Roy Malan", "strings", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Nathan Rubin", "strings", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Linda Wood", "strings", instrument_type = ""),
                fun_artist(title = "sunlight", year = 19780615, label = "columbia", 
                           "Emily VanValkenburgh", "strings", instrument_type = "")) %>% 
  as_tibble()

# tidy---
discog[discog$instrument_type == "" & 
         !is.na(discog$instrument_type),]


library(data.table)

temp <- discog %>%
  group_by(instrument, 
           it_good = ifelse(instrument_type == "" | is.na(instrument_type),
                            "BLANK", "INS.TYPE")) %>%
  summarise(n = n()) %>%
  as.data.table() %>%
  dcast(., 
        instrument ~ it_good, 
        fill = 0) %>% 
  as.data.frame() %>% as_tibble()


temp$blank2 <- temp$BLANK != 0
temp$ins.type2 <- temp$INS.TYPE != 0

temp %>%
  group_by(blank2, ins.type2) %>%
  summarise(n = n())

rm(temp)

discog$artist <- tolower(discog$artist)
#instrument type assignments----
#discog$instrument <- as.character(discog$instrument)
discog$instrument_type[discog$instrument %in% c("harmonica", "wind")]           <- "wind"
discog$instrument_type[discog$instrument %in% c("cornet", "trombone", 
                                                "flugelhorn", "brass",
                                                "trumpet", "bass trombone", 
                                                "tenor trombone",
                                                "alto trombone", "tuba")]       <- "brass"
discog$instrument_type[discog$instrument %in% c("tenor sax", "bass flute", 
                                                "flute", "alto flute", 
                                                "clarinet", "bass clarinet", 
                                                "nose flute", "piccolo", 
                                                "soprano sax", "saxello", 
                                                "saxophone", "reeds", 
                                                "alto sax", "woodwinds")]       <- "woodwind"
discog$instrument_type[discog$instrument %in% c("mellotron", "piano", 
                                                "electric piano", 
                                                "keyboards", "fender rhodes", 
                                                "synthesizer", 
                                                "clavinet", "arp odyssey", 
                                                "arp synthesizer",
                                                "arp 2600", "arp strings", 
                                                "micromoog", 
                                                "maestro universal synthesizer")]                   <- "keyboard"
discog$instrument_type[discog$instrument %in% c("vocals","voices", 
                                                "voice bag", 
                                                "oberheim 4 voice",
                                                "echoplex", "lyricon", 
                                                "backing vocals", "vocoder")]    <- "vocal"
discog$instrument_type[discog$instrument %in% c("guitar", "bass", 
                                                "electric bass", 
                                                "acoustic bass", "fuzz bass", 
                                                "wah wah bass", "strings",
                                                "wah wah guitar", 
                                                "arp string ensemble")]         <- "string"
discog$instrument_type[discog$instrument %in% c("percussion", "drums", "congo", 
                                                "bongo", "afuche", "cowbell",
                                                "tabla",
                                                "pipe", "marimba", "marimbula")] <- "percussion"

#synths <- discog

#instrument generalizations----
discog$instrument[discog$instrument != "drums" & discog$instrument_type == "percussion"] <- "percussion"
discog$yr <- lubridate::year(discog$year)
discog <- discog %>% group_by(title, year, artist, instrument, instrument_type, label, yr) %>% summarise(n = n())
discog$instrument[discog$instrument %in% c("voice bag", "oberheim 4 voice", "echoplex", "lyricon")] <- "voice augmentation"
discog$instrument[discog$instrument %in% c("wah wah guitar")] <- "guitar"
discog$instrument[discog$instrument %in% c("double bass")] <- "bass"
discog$instrument[discog$instrument %in% c("fender rhodes", "clavinet", "keyboards")] <- "electric piano"
discog$instrument[discog$instrument %in% c("wah wah bass", "fuzz bass")] <- "electric bass"
discog$instrument[discog$instrument %in% c("arp strings", "arp 2600", "arp synthesizer", 
                                           "arp odyssey", "fuzz-wah", "arp echoplex", 
                                           "mellotron", "micromoog", 
                                           "maestro universal synthesizer")] <- "synthesizer"
discog$instrument[discog$instrument %in% c("tenor trombone", "alto trombone", "bass trombone")] <- "trombone"
discog$instrument[discog$instrument %in% c("nose flute", "alto flute")] <- "flute"
discog$instrument[discog$instrument %in% c("saxello", "soprano sax", "tenor sax", "alto sax")] <- "saxophone"
#discog$instrument_type[discog$instrument == "synthesizer"] <- "keyboard - synthesizer"
#discog$instrument_type[discog$instrument != "synthesizer" & discog$instrument_type == "keyboard"] <- "keyboard - piano"

#electric vs acoustic----
discog$ea <- NA
discog$ea <- "acoustic"
discog$ea[discog$instrument %in% c("electric piano",
                                   "synthesizer", 
                                   "voice augmentation", 
                                   "electric bass", 
                                   "electric guitar", 
                                   "arp string ensemble")] <- "electric"

#favorite albums----
albums <- discog$title %>% unique() %>% as.character()
albums <- data.frame(title = albums, 
                     favorite = "not a favorite")
albums$title <- as.character(albums$title)
albums$favorite <- as.character(albums$favorite)
albums$favorite[albums$title %in% c("maiden voyage", "speak like a child",
                                    "crossings", "sextant", 
                                    "head hunters", "man-child", 
                                    "mr hands", 
                                    "perfect machine")] <- "love it"
#eras----
eras <- data.frame(era_name = c("blue note era", 
                                "mwandishi era",
                                "head hunters era",
                                "jazz funk fusion era",
                                "disco r&b era",
                                "electro funk & hip-hop era",
                                "concept era",
                                "tribute era"), 
                   era_abbr = c("bne", 
                                "mwe", 
                                "hhe",
                                "jfe",
                                "drbe",
                                "efhh",
                                "ce", 
                                "te"))
album.eras <- discog %>% group_by(title) %>% summarise()
album.eras$era_abbr <- NA
album.eras$era_abbr[album.eras$title %in% c("takin' off", "my point of view",
                                            "inventions & dimensions", 
                                            "empyriean isles", "maiden voyage", 
                                            "speak like a child", "the prisoner")] <- "bne"
album.eras$era_abbr[album.eras$title %in% c("fat albert rotunda", 
                                            "mwandishi", "crossings", 
                                            "sextant")] <- "mwe"
album.eras$era_abbr[album.eras$title %in% c("head hunters", "thrust", 
                                            "man-child")] <- "hhe"
album.eras$era_abbr[album.eras$title %in% c("secrets", "sunlight")] <- "jfe"
album.eras$era_abbr[album.eras$title %in% c("feets, don't fail me now",
                                            "monster", "magic windows",
                                            "lite me up")] <- "drbe"
album.eras$era_abbr[album.eras$title %in% c("future shock", "sound system", 
                                            "perfect machine", "dis is da drum")] <- "efhh"
album.eras$era_abbr[album.eras$title %in% c("the new standard", 
                                            "gershwin's world", "future2future",
                                            "possibilities", "river: the joni letters",
                                            "the imagine project")] <- "ce"
album.eras$era_abbr[album.eras$title %in% c("herbie hancock trio",
                                            "quartet", "a tribute to miles")] <- "te"
album.eras <- inner_join(album.eras, eras)

discog <- discog %>% inner_join(., album.eras)
discog$era_name_f <- factor(discog$era_name, levels = eras$era_name)

#deaths----
deaths <- rbind(data.frame(artist = "Dexter Gordon", died = ymd(19900425)),
                data.frame(artist = "Tony Williams", died = ymd(19970223)),
                data.frame(artist = "Donald Byrd", died = ymd(20130204)), 
                data.frame(artist = "Wayne Shorter", died = ymd(20230302)), 
                data.frame(artist = "Paul Jackson Jr", died = ymd(20210318)),
                data.frame(artist = "Wah Wah Watson", died = mdy("October 24, 2018")))

titleyr <- discog %>% group_by(title, year) %>% summarise()
titleyr$title <- titleyr$title %>% as.character()
titleyr$title <- gsub("\"", "", titleyr$title)
titleyr$title[3] <- "inventions & dimensions"
titleyr$title[6] <- "speak like a child"
titleyr$instrument_type <- "album"

#plot----
ggplot() + 
  scale_x_date(name = "Year", date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_discrete() + 
  geom_path(data = discog,  color = "black",
            aes(x = year, y = instrument, 
                group = instrument, linewidth = I(1.5))) +
  geom_point(data = discog,  color = "black", shape = 22,
             aes(x = year, y = instrument, 
                 #shape = I(22), 
                 fill = title, size = I(3))) +
  facet_grid(instrument_type~., space = "free", scales = "free" ) +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, h = 1))

ggplot() + 
  geom_bin2d(data = discog, 
             aes(x = title, y = instrument_type))+
  scale_fill_viridis_c(option = "C")

ggplot() + 
  scale_x_discrete(name = "Album") +
  scale_y_discrete(name = "Instrument Families") + 
  scale_size_area() +
  #scale_fill_gradient(low = "yellow", high = "red") +
  #scale_size_continuous() +
  geom_path(data = discog,  color = "black",
            aes(x = title, y = instrument_type, 
                group = instrument_type)) +
  geom_point(data = discog %>% group_by(instrument_type, title, label, era_name_f, ea) %>% summarise(n = n()),  color = "black", shape = 22,
             #fill = "orange",
             aes(x = title, y = instrument_type, 
                 size = n, fill = era_name_f)) +
  facet_grid(as.table = FALSE, ea~era_name_f, space = "free", scales = "free" ) +
  theme(strip.text.x = element_text(angle = 90),
        strip.text.y = element_text(angle = 0),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, h = 1)) 

synths <- inner_join(synths, album.eras)
synths$era_name_f <- factor(synths$era_name, 
                            levels = c("blue note era", 
                                       "mwandishi era", 
                                       "head hunters era", 
                                       "jazz funk fusion era", 
                                       "disco r&b era", 
                                       "electro funk & hip hop era", 
                                       "concept era", "tribute"
                            ))
# #plot-synths----
# ggplot() + 
#   scale_x_discrete(name = "Album") +
#   scale_y_discrete(name = "Instrument Families") + 
#   scale_size_area() +
#   scale_fill_gradient(low = "yellow", high = "red") +
#   #scale_size_continuous() +
#   geom_path(data = synths[synths$instrument_type == "keyboard",],  color = "black",
#             aes(x = title, y = instrument, 
#                 group = instrument)) +
#   geom_point(data = synths[synths$instrument_type == "keyboard",] %>% group_by(instrument, title, label, era_name_f) %>% summarise(n = n()),  color = "black", shape = 22,
#              #fill = "orange",
#              aes(x = title, y = instrument, 
#                  size = n, fill = n)) +
#   facet_grid(as.table = FALSE, ~era_name_f, space = "free", scales = "free" ) +
#   theme(strip.text.x = element_text(angle = 90),
#         strip.text.y = element_text(angle = 0),
#         legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, h = 1)) 

hhe.artists <- discog %>% group_by(artist, era_name) %>% summarise() %>%
  .[.$era_name == "head hunters era",] %>% .$artist %>%
  unique() %>% as.character()


ggplot() + 
  scale_x_date(name = "Year", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_discrete(name = "Personnel") + 
  geom_path(data = discog[discog$artist %in% hhe.artists,],  color = "black",
            aes(x = year, y = artist, 
                group = artist, size = I(1.5))) +
  geom_point(data = discog[discog$artist %in% hhe.artists,],  color = "black", shape = 22,
             aes(x = year, y = artist, 
                 #shape = I(22), 
                 fill = title, size = I(3))) +
  facet_grid(.~era_name_f, space = "free", scales = "free" ) +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, h = 1)) 


# geom_text(data = deaths, 
#            aes(x = year, y = artist, label = "RIP"))

# ggplot() + 
#   geom_bin2d(data = discog, 
#              aes(x = title, y = artist)) +
#   facet_grid(instrument_type~label, space = "free", scales = "free" ) +
#   theme(strip.text.y = element_text(angle = 0),
#         legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, h = 1)) 

ggplot() + 
  scale_fill_continuous(low = "green", high = "red")+
  geom_bin2d(data = discog, 
             aes(x = title, y = instrument_type)) +
  facet_grid(~label, space = "free", scales = "free" ) +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, h = 1),
        text = element_text(size = 10)) #+
#ggsave("resume.png", device = "png", width = 4, height = 1.5, units = "in", dpi = 300)

# ggplot() + 
#   geom_dotplot(data = discog ,
#             #position = "stack",
#             aes(x = yr, y = instrument_type))+
#   facet_grid(instrument_type~.)

ggplot() + 
  geom_histogram(data = discog, stat = "count",
                 aes(x = instrument_type))

# ggplot() + 
#   geom_tile(data = discog,
#             aes(x = title, y = instrument_type, fill = n))

artist.tenure <- discog %>% 
  group_by(artist,era_name_f,instrument_type
           #instrument_type, 
           #label
  ) %>% 
  summarise(start = min(year),
            end = max(year),
            n = n()) 
artist.tenure$duration <- artist.tenure$end - artist.tenure$start

ggplot() + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  geom_segment(data = artist.tenure[artist.tenure$n > 0,], 
               aes(x = start, xend = end, 
                   y = as.character(artist), yend = as.character(artist))) +
  geom_point(data = discog[discog$n > 0,], 
             aes(x = year, y = as.character(artist))) + 
  facet_grid(instrument_type~era_name_f, space = "free", scales = "free")+
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90))

artistXalbum <- discog %>%
  group_by(artist, title) %>%
  summarise() %>%
  group_by(title) %>%
  summarise(n = n())

ggplot() + 
  geom_col(data = artistXalbum, 
           aes(x = title, y = n)) +
  coord_flip() 

# save .RData to output
setwd("~/R/play/shiny_misc/shiny_herbiehancock/output")
save(list = ls(), 
     file = "herbiehancock.RData")

# set wd back to home
setwd("~/R/play")



album.eras
albums
artist.tenure
#artistXalbum

discog
deaths
eras
titleyr[order(titleyr$year),]

studio.albums <- tibble::tribble(
  ~Studio.albums,
  "Takin' Off (1962)",
  "My Point of View (1963)",
  "Inventions & Dimensions (1963)",
  "Empyrean Isles (1964)",
  "Maiden Voyage (1965)",
  "Speak Like a Child (1968)",
  "The Prisoner (1969)",
  "Fat Albert Rotunda (1969)",
  "Mwandishi (1971)",
  "Crossings (1972)",
  "Sextant (1973)",
  "Head Hunters (1973)",
  "Dedication (1974)",
  "Thrust (1974)",
  "Man-Child (1975)",
  "Secrets (1976)",
  "Third Plane (1977)",
  "Herbie Hancock Trio (1977)",
  "Sunlight (1978)",
  "Directstep (1979)",
  "The Piano (1979)",
  "Feets, Don't Fail Me Now (1979)",
  "Monster (1980)",
  "Mr. Hands (1980)",
  "Magic Windows (1981)",
  "Herbie Hancock Trio (1982)",
  "Quartet (1982)",
  "Lite Me Up (1982)",
  "Future Shock (1983)",
  "Sound-System (1984)",
  "Village Life (1985)",
  "Perfect Machine (1988)",
  "A Tribute to Miles (1994)",
  "Dis Is da Drum (1994)",
  "The New Standard (1996)",
  "1+1 (1997)",
  "Gershwin's World (1998)",
  "Future 2 Future (2001)",
  "Possibilities (2005)",
  "River: The Joni Letters (2007)",
  "The Imagine Project (2010)"
)

studio.albums <- studio.albums %>% as_tibble()
studio.albums$album <- studio.albums$Studio.albums %>%
  gsub("\\(.*$", "", .) %>%
  trimws()
studio.albums$year <- strsplit(studio.albums$Studio.albums, 
                               split = "\\(") %>%
  lapply(., last) %>%
  unlist() %>%
  trimws() %>%
  gsub("\\)", "", .) %>%
  as.numeric



ggplot() + 
  geom_point(data = studio.albums, 
             aes(x = year, y = "solo studio albums"))+
  scale_x_continuous(limits = c(year(ymd(19400412)),year(Sys.Date())))
