#  tornado data dict

library(dplyr)
library(readr)


# https://www.spc.noaa.gov/wcm/#data
rm(list=ls());cat('\f')

tornado.ddict <- data.frame(col_num = 1:29, 
                            col_name = c("om", "yr", "mo", "dy", "date", "time", 
                                         "tz", "st", "stf", "stn", "f|sz|mag", 
                                         "inj", "fat", "loss", "closs", 
                                         "slat", "slon", 
                                         "elat", "elon", 
                                         "len", "wid", 
                                         "ns", "sn", "sg", 
                                         "f1", "f2", "f3", "f4", 
                                         "fc"), 
                            col_desc = c("tornado number", "year", 
                                         "month", "day", "date", "time", 
                                         "timezone (CST unless otherwise noted)", 
                                         "state", "state fips number", 
                                         "state number - number of this tornado in this state, in this year", 
                                         "f-scale, or ef-scale after 2007", 
                                         "injuries", "fatalaties", "estimated property loss in millions of usd 1996+", 
                                         "estimated crop loss in millions of dollars", 
                                         "start latitude", "start longitude", "end latitude", "end longitude", 
                                         "length_miles", "width_yards",
                                         "number of states affected", "state number (see next table)", "segment number",
                                         "county1_fips", "county2_fips", "county3_fips", "county4_fips", "scale rating metadata"
                                         )) %>% as_tibble()

int22_24 <- expand.grid(ns = 1:3, 
                        sn = 0:1, 
                        sg = c(-9,1,2))
int22_24$def <- NA
int22_24$def[int22_24$ns == 1 & 
               int22_24$sn == 1 & 
               int22_24$sg == 1] <- "Entire record for the track of the tornado (unless all 4 fips codes are non-zero"
int22_24$def[int22_24$ns == 1 & 
               int22_24$sn == 0 & 
               int22_24$sg == -9] <- "Continuing county fips code information only from 1,1,1, record, above (same om)"
int22_24$def[int22_24$ns == 2 & 
               int22_24$sn == 0 & 
               int22_24$sg == 1] <- "A 2-state tornado (st = state of touchdown, other fields summarize entire track)"
int22_24$def[int22_24$ns == 2 & 
               int22_24$sn == 1 & 
               int22_24$sg == 2] <- "First state segment for a 2-state tornado (2,0,1) (same state as above, same om)"
int22_24$def[int22_24$ns == 2 & 
               int22_24$sn == 1 & 
               int22_24$sg == 2] <- "Second state segment for a 2-state tornado (2,0,1) (same tracked into, same om)"
int22_24$def[int22_24$ns == 2 & 
               int22_24$sn == 0 & 
               int22_24$sg == -9] <- "Continuing county fips for a (2,1,2) record that exceeds 4 counties (same om)"
int22_24$def[int22_24$ns == 3 & 
               int22_24$sn == 0 & 
               int22_24$sg == 1] <- "A 3-state tornado (st = touchdown, other fields summarise entire track)"
int22_24$def[int22_24$ns == 3 & 
               int22_24$sn == 1 & 
               int22_24$sg == 2] <- "First state segment for a 3-state (3,0,1) tornado (same state as 3,0,1, same om)"
int22_24$def[int22_24$ns == 3 & 
               int22_24$sn == 1 & 
               int22_24$sg == 2] <- "second state segment for a 3-state (3,0,1) tornado (2nd state tracked into, same om as 3,0,1 record)"
int22_24$def[int22_24$ns == 3 & 
               int22_24$sn == 1 & 
               int22_24$sg == 2] <- "thrid state segment for a 3-state (3,0,1) tornado (3rd state tracked into, same om as 3,0,1 record)"

int22_24 <- int22_24[complete.cases(int22_24),]






