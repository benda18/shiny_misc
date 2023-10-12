#runaways
#https://stackoverflow.com/questions/27347548/r-assign-variable-labels-of-data-frame-columns
#https://stackoverflow.com/questions/8196109/how-to-convert-a-string-in-a-function-into-an-object

library(data.table)
#library(mdsr)
library("haven")
#library("shapefiles")
#library("Hmisc")
library("dplyr")
library("lubridate")

library("ggplot2")
library("ggrepel")
#library("cluster")
#library("forcats")
#library(tidycensus)
#library(tigris)
#library(stringr)
library("ggmap")
library("readr")
#library(sf)
#library(rgdal)
library("spData")
library("spDataLarge")  #install.packages('spDataLarge',repos='https://nowosad.github.io/drat/', type='source')

#library(sp)
#library(sf)
#library(rgeos)
#library(mdsr)
library("gtools") #for permutations()
library("maps") #for data(world.cities)

rm(list = ls())
cat("\f")
setwd("~/R/play")
#vars----

slave.dates <- data.frame(type = c("USA", "UK", 
                                   "Spain", "Netherlands", 
                                   "France", 
                                   "Venezuela", 
                                   "Canada", "Haiti"), 
                          date = ymd(c(18060101, 18070101, 
                                       18110101, 18140101, 
                                       18150101, 18170101, 
                                       18190101, 18220101)))


broad.reg <- data.frame(name = c("Africa", "Caribbean", "Brazil",  "Europe", "Spanish Mainland America", "Mainland North America"), 
                        x    = c(18.20682, -75.31008,   -53.05434, 11.87362, -99.26491,                  -103.3469), 
                        y    = c(6.450174,  20.35586,   -10.80677, 56.67009,  21.78775,                   45.70563), 
                        stringsAsFactors = F)

crossings <- permutations(length(broad.reg$name), 2, broad.reg$name) %>%
  as.data.frame() %>%
  left_join(., broad.reg, by = c("V1" = "name")) %>%
  left_join(., broad.reg, by = c("V2" = "name"), suffix = c(".from", ".to"))
cross.pts <- crossings %>%
  group_by(V1, x.from, y.from) %>%
  summarise()

ggplot() + 
  geom_segment(data = crossings, 
               aes(x = x.from, y = y.from, xend = x.to, yend = y.to)) +
  coord_quickmap() +
  geom_point(data = cross.pts, size = 3,
             aes(x = x.from, y = y.from)) +
  geom_label_repel(data = cross.pts, size = 3,
                   aes(x = x.from, y = y.from, 
                       label = V1)) +
  labs(title = "Primary paths of crossing")



#transatlantic-data----


ta <- read_sav("tastdb-exp-2020.sav")
head(ta)

attributes(ta$ADULT1)

# build df of col name meanings----
col_defs <- NULL
for(i in 1:length(names(ta))){
  col_defs <- rbind(col_defs, 
                    data.frame(id = i, 
                               colname = names(ta)[i], 
                               def = attr(ta[,i][[1]],"label"), 
                               class = class(unname(unlist(ta[,i]))))) %>% as_tibble()
}


# tidy----
head(col_defs)

col_defs$def <- gsub("ofAdults", "of Adults", col_defs$def,ignore.case = T)
col_defs$def <- gsub("ofFemales", "of Females", col_defs$def)
col_defs$def <- gsub("ofMales", "of Males", col_defs$def)
col_defs$def <- gsub("ofChildren", "of Children", col_defs$def)
col_defs$def <- gsub("ImputedChildren", "Imputed Children", col_defs$def)
col_defs$def <- gsub("ImputedFemales", "Imputed Females", col_defs$def)

#cat('\f');grep(pattern = " of", col_defs$def, value = T)

#additional clarity
col_defs$kw_place    <- grepl("place", col_defs$def, ignore.case = T)
col_defs$kw_port     <- grepl("port", col_defs$def, ignore.case = T)
col_defs$kw_slave    <- grepl("slave|slaves", col_defs$def, ignore.case = T)
col_defs$kw_landing  <- grepl("landing", col_defs$def, ignore.case = T)
col_defs$kw_embark   <- grepl("embark", col_defs$def, ignore.case = T)
col_defs$kw_purchase <- grepl("purchase", col_defs$def, ignore.case = T)
col_defs$kw_imputed  <- grepl("imputed", col_defs$def, ignore.case = T)
col_defs$kw_voyage   <- grepl("voyage", col_defs$def, ignore.case = T)
col_defs$kw_died     <- grepl("died", col_defs$def, ignore.case = T)
col_defs$kw_deaths   <- grepl("deaths", col_defs$def, ignore.case = T)
col_defs$kw_region   <- grepl("region", col_defs$def, ignore.case = T)
col_defs$kw_middle   <- grepl("middle|passage", col_defs$def, ignore.case = T)
col_defs$kw_owner    <- grepl("owner", col_defs$def, ignore.case = T)
col_defs$kw_venture  <- grepl("venture", col_defs$def, ignore.case = T)
col_defs$kw_age      <- grepl(" age |^age | age$", col_defs$def, ignore.case = T)  
col_defs$kw_gender   <- grepl("gender", col_defs$def, ignore.case = T)
col_defs$kw_crew     <- grepl("crew", col_defs$def, ignore.case = T)
col_defs$kw_captain  <- grepl("captain", col_defs$def, ignore.case = T)
col_defs$kw_vessel   <- grepl("vessel", col_defs$def, ignore.case = T)
col_defs$kw_intended <- grepl("intended", col_defs$def, ignore.case = T)
col_defs$kw_date     <- grepl("date", col_defs$def, ignore.case = T)
col_defs$kw_year     <- grepl("year", col_defs$def, ignore.case = T)
col_defs$kw_outcome  <- grepl("outcome", col_defs$def, ignore.case = T)
col_defs$kw_source   <- grepl("^source", col_defs$colname, ignore.case = T)


# data frequency
col_defs <- melt(as.data.table(ta), 
                 id.vars = "VOYAGEID") %>% 
  .[!is.na(.$value),] %>%
  .$variable %>% 
  table() %>%
  as.data.frame() %>%
  left_join(col_defs, ., 
            by = c("colname"="."))

names(col_defs)
col_defs %>%
  group_by(kw_place, kw_embark, kw_died, 
           kw_owner, kw_crew, 
           kw_date, kw_port, kw_purchase, 
           kw_deaths, kw_venture, kw_captain, 
           kw_year, kw_slave, kw_imputed, 
           kw_region, kw_age, kw_vessel, 
           kw_outcome, kw_landing, kw_voyage, 
           kw_middle, kw_gender, kw_intended, 
           kw_source) %>%
  summarise(n = n(), 
            n_vid = n_distinct(colname), 
            t_freq = sum(Freq)) %>%
  .[order(.$n,decreasing = T),] 

# voyage fates----
voyage.fates <- ta[colnames(ta) %in% 
                     toupper(grep(pattern = "voyageid|fate.*$", 
                                  names(ta), ignore.case = T, 
                                  value = T))]

voyage.fates$fate_f <- as_factor(voyage.fates$FATE)
voyage.fates$fate2_f <- as_factor(voyage.fates$FATE2)
voyage.fates$fate3_f <- as_factor(voyage.fates$FATE3)
voyage.fates$fate4_f <- as_factor(voyage.fates$FATE4)

voyage.fates <- voyage.fates[colnames(voyage.fates) %in%
                               c("VOYAGEID", 
                                 grep(".*_f$", colnames(voyage.fates),value = T))]

unique(voyage.fates$fate_f)  # Particular outcome of voyage
unique(voyage.fates$fate2_f) # Outcome of voyage for slaves
unique(voyage.fates$fate3_f) # Outcome of voyage if vessel captured
unique(voyage.fates$fate3_f) # Outcome of voyage for owner

colnames(voyage.fates) <- c("VOYAGEID", "outcome_voyage", "outcome_slaves", "outcome_if_captured", "outcome_owner")

# captured T/F
voyage.fates$captured     <- grepl("captured|taken to|taken by|given_up|looted", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$shipwrecked  <- grepl("shipwrecked", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$condemned_aband_sold <- grepl("for salvage|salvaged|condemned|abandoned|abandoned and/or sold off|^sold prematurely|ship and slaves sold in|^sold in the americas|^sold$", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$lost         <- grepl("lost|destroyed", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$fate_unknown <- grepl("fate unknown|unknown outcome|no further record|^unknown$", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$pirates      <- grepl("pirates", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$by_slaves    <- grepl("by slaves", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$driven_off   <- grepl("driven", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$detained     <- grepl("detained|refused permission to disembark", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$mutiny       <- grepl("mutiny", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$acquittal       <- grepl("acquittal|restored", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$did_not_reach_dest <- grepl("did not reach", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$ship_surrendered <- grepl("given up|pressed into government", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$broken_up   <-grepl("laid up|broken up", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$voyage_completed   <-grepl("voyage completed", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$ship_returned_to   <-grepl("^returned direct|^returned to|^ship returned direct to", voyage.fates$outcome_voyage, ignore.case = T)
voyage.fates$slaves_sold_in_afr <-grepl("sold slaves in africa", voyage.fates$outcome_voyage, ignore.case = T)

idd_fates <- (voyage.fates$captured +
                voyage.fates$ship_returned_to +
                voyage.fates$voyage_completed +
                voyage.fates$slaves_sold_in_afr +
                voyage.fates$acquittal +
                voyage.fates$broken_up +
                voyage.fates$mutiny +
                voyage.fates$did_not_reach_dest +
                voyage.fates$ship_surrendered +
                voyage.fates$detained +
                voyage.fates$shipwrecked + 
                voyage.fates$condemned_aband_sold + 
                voyage.fates$lost + 
                voyage.fates$fate_unknown +
                voyage.fates$pirates +
                voyage.fates$by_slaves+
                voyage.fates$driven_off )

table(idd_fates)

unique(as.character(voyage.fates$outcome_voyage[idd_fates == 0]))

voyage.fates$fate_by_whom <- NA
voyage.fates$fate_by_whom[grepl("by spanish", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "spanish"

voyage.fates$fate_by_whom[grepl("by french|by the french", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "french"


voyage.fates$fate_by_whom[grepl("by british|by the british|by english", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "british"
voyage.fates$fate_by_whom[grepl("by slaves", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "slaves"
voyage.fates$fate_by_whom[grepl("by crew|by the crew", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "crew"
voyage.fates$fate_by_whom[grepl("by africans", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "africans"
voyage.fates$fate_by_whom[grepl("by algerian pirates|by pirates|by privateers|by pirates or privateers|by argentinian privateers", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "pirates"
voyage.fates$fate_by_whom[grepl("by united states", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "usa"
voyage.fates$fate_by_whom[grepl("by dutch|by the dutch", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "dutch"
voyage.fates$fate_by_whom[grepl("by Cie du Senegal", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "senegal"
voyage.fates$fate_by_whom[grepl("by portuguese", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "portuguese"
voyage.fates$fate_by_whom[grepl("by swedes", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "swedes"
voyage.fates$fate_by_whom[grepl("by haitian", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "haiti"
voyage.fates$fate_by_whom[grepl("by brazil", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "brazil"
voyage.fates$fate_by_whom[grepl("by turks", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "turks"
voyage.fates$fate_by_whom[grepl("by venezuelans", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "venezuela"
voyage.fates$fate_by_whom[grepl("by the barbary powers", 
                                voyage.fates$outcome_voyage, 
                                ignore.case = T)] <- "barbary powers"


voyage.fates <- voyage.fates[!colnames(voyage.fates) %in% "outcome_voyage"] %>% 
  as.data.table() %>%
  melt(., id.vars = c("VOYAGEID", "outcome_slaves", 
                      "outcome_if_captured", 
                      "outcome_owner", "fate_by_whom"), 
       variable.name = "outcome_voyage")

voyage.fates <- voyage.fates[voyage.fates$value,]
voyage.fates <- as.data.frame(voyage.fates) %>%
  .[!colnames(.) %in% "value"] %>%
  as.data.table()

voyage.fates %>%
  group_by(outcome_voyage, outcome_if_captured, outcome_slaves, outcome_owner) %>%
  summarise(n = n(), 
            n_vid = n_distinct(VOYAGEID))

voyage.fates %>%
  .[.$outcome_voyage == "pirates",] %>%
  group_by(outcome_owner, outcome_voyage,outcome_if_captured, outcome_slaves) %>%
  summarise(n_vid = n_distinct(VOYAGEID), 
            pct_vid = scales::percent(n_vid/length(unique(voyage.fates$VOYAGEID)),
                                      accuracy = 0.1)) %>%
  .[order(.$n_vid,decreasing = T),]


pirate.voyages <- ta[ta$VOYAGEID %in% 
                       unique(voyage.fates$VOYAGEID[voyage.fates$outcome_voyage == "pirates"]),]



# Show Generally When we have data for trips across Atlantic Ocean----
# col_defs[col_defs$kw_date,c("colname", "def")]
# col_defs[col_defs$kw_year,c("colname", "def")]
# col_defs[col_defs$kw_middle,c("colname", "def")]
# col_defs[col_defs$kw_imputed,c("colname", "def")]
# 
# col_defs[col_defs$kw_imputed & 
#            col_defs$kw_date,c("colname", "def")]

# ta %>%
#   group_by(YEARAF, YEARAM) %>%
#   summarise(dur = YEARAM - YEARAF, 
#             t_adult_died_middle = sum(ADLT2IMP, na.rm = T),
#             t_child_died_middle = sum(CHIL2IMP,na.rm = T),
#             n = n(), 
#             n_vid = n_distinct(VOYAGEID))

voyage.fates

middle_passage_stats <- ta %>%
  group_by(VOYAGEID, 
           year_left_africa = YEARAF, 
           year_arrive_americas = YEARAM,
           mp_duration_days = VOYAGE, 
           mp_dur_imp_days = VOY2IMP,
           slaves_emp = SLAXIMP,
           slaves_emb2 = TSLMTIMP, 
           slaves_disemb = SLAMIMP, 
           slaves_died_mp = VYMRTIMP,
           adults_died_mp = ADLT2IMP,
           children_before_mp = CHIL1IMP,
           children_died_mp = CHIL2IMP,
           children_after_mp = CHIL3IMP,
           males_died_mp = MALE2IMP,
           females_died_mp = FEML2IMP,
           slave_mortality_rate = VYMRTRAT) %>%
  summarise(n = n()) %>%
  left_join(., voyage.fates, by = "VOYAGEID")

which(middle_passage_stats$slaves_disemb <= 3)

table(middle_passage_stats$outcome_voyage)

ggplot(data = middle_passage_stats, 
       aes(x = year_arrive_americas, y = slaves_emp)) + 
  geom_point()+
  geom_smooth()+
  geom_vline(aes(xintercept = 1861), color = "red", linetype = 2232)+
  labs(title = "Number of Slaves on Board")+
  scale_x_continuous(breaks = seq(0,2000,by = 50))

ggplot(data = middle_passage_stats, 
       aes(x = year_arrive_americas, y = slaves_disemb)) + 
  geom_point()+
  geom_smooth()+
  geom_vline(aes(xintercept = 1861), color = "red", linetype = 2232)+
  labs(title = "Number of Slaves Disembark")+
  scale_x_continuous(breaks = seq(0,2000,by = 50))

ggplot(data = middle_passage_stats[!middle_passage_stats$outcome_slaves %in% c("Slaves disembarked Americas"),], 
       aes(x = year_left_africa, y = slaves_emp, color = outcome_slaves)) + 
  geom_point()+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  geom_vline(aes(xintercept = 1861), color = "red", linetype = 2232)+
  labs(title = "Number of Slaves on Board")+
  scale_x_continuous(breaks = seq(0,2000,by = 50))

middle_passage_stats %>%
  group_by(year_arrive_americas) %>%
  summarise(#n_vid = n_distinct(VOYAGEID),
    t_slaves = sum(slaves_emp, na.rm = T), 
    avg_smr = mean(slave_mortality_rate, na.rm = T)) %>% 
  ggplot(data = ., 
         aes(x = year_arrive_americas, 
             y = t_slaves)) + 
  geom_line()+
  geom_smooth()+
  #scale_y_log10(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom")+
  geom_vline(aes(xintercept = 1861), color = "red", linetype = 2232)+
  labs(title = "Total Slaves Embarked by Year")+
  scale_x_continuous(breaks = seq(0,2000,by = 50))

ggplot(data = middle_passage_stats, 
       aes(x = year_left_africa, y = mp_dur_imp_days)) + 
  geom_boxplot(aes(group = year_left_africa))+
  geom_smooth()+
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0,1000,by=50))+
  theme(plot.background = element_rect(fill = "yellow"))+
  labs(title ="Middle Passage Duration (in Days) by Year")

ggplot(data = middle_passage_stats, 
       aes(y = slave_mortality_rate, x = mp_dur_imp_days)) + 
  geom_point()+
  geom_smooth(se = F)+
  geom_smooth(se = F, method = "lm")+
  labs(title = "mortality rate vs. duration of middle passage")

middle_passage_stats$slave_mortality_rate %>% hist

ggplot(data = middle_passage_stats, 
       aes(y = slave_mortality_rate, x = mp_dur_imp_days)) + 
  geom_point()+
  geom_smooth(se=F, 
              aes(color = outcome_owner), 
              method = "auto")+
  scale_x_log10()+
  theme(plot.background = element_rect(fill = "yellow"))


middle_passage_stats %>%
  group_by(year_left_africa) %>%
  summarise(n_vid = n_distinct(VOYAGEID), 
            med_mpdays = median(mp_dur_imp_days, na.rm = T)) %>%
  .[complete.cases(.),] %>%
  ggplot(data = ., aes(x = year_left_africa, y = med_mpdays)) + 
  geom_point()+
  geom_smooth(se = F)+
  scale_y_continuous(limits = c(0, 320))+
  scale_x_continuous(limits = c(1500, 1865))+
  labs(title = "Median duration of Middle Passage by Year")+
  theme(plot.background = element_rect(fill = "yellow"))

middle_passage_stats %>%
  group_by(year_left_africa) %>%
  summarise(n_vid = n_distinct(VOYAGEID), 
            med_mort = median(slave_mortality_rate, na.rm = T)) %>%
  .[complete.cases(.),] %>%
  ggplot(data = ., aes(x = year_left_africa, y = med_mort)) + 
  geom_point()+
  geom_smooth(se = F)+
  scale_y_continuous(limits = c(0, NA))+
  scale_x_continuous(limits = c(1500, 1865))+
  labs(title = "Median Mortality Rate by Year")+
  theme(plot.background = element_rect(fill = "yellow"))

# ships and captains and crew----
col_defs$colname[col_defs$kw_captain] # 
col_defs$def[col_defs$kw_crew] # captain names


# crew numbers throughout 
ta %>%
  group_by(VOYAGEID, 
           crew_v_start = CREW1, 
           crew_mp_start = CREW2, 
           crew_died_mp = SAILD3, 
           crew_mp_end = CREW3, 
           crew_v_end = CREW5, 
           crew_died_v = CREWDIED, 
           crew_deserted_v = NDESERT) %>%
  summarise(n = n(), 
            na_score = is.na(crew_v_start) +
              is.na(crew_mp_start)+
              is.na(crew_died_mp)+
              is.na(crew_mp_end) + 
              is.na(crew_v_end) +
              is.na(crew_died_v) + 
              is.na(crew_deserted_v)) %>%
  as.data.table() %>%
  melt(., id.vars = c("VOYAGEID", "n", "na_score")) %>%
  .[!is.na(.$value),] %>%
  group_by(variable) %>%
  summarise(n = n()/36108, 
            n_vid = n_distinct(VOYAGEID))


ta %>%
  group_by(VOYAGEID, 
           crew_at_start = CREW1, 
           #crew_died = CREWDIED, 
           #crew_deserted = NDESERT
           ) %>%
  summarise() %>%
  .[!is.na(.$crew_at_start),] %>%
  left_join(., middle_passage_stats) %>%
  .$mp_dur_imp_days %>% is.na() %>% sum()

col_defs$def[col_defs$kw_imputed]


# SHIP TRENDS
col_defs[col_defs$kw_vessel,] %>%
  group_by(colname, def, Freq) %>%
  summarise(pct = scales::percent(Freq/36108)) %>%
  .[order(.$Freq,decreasing = T),]


# outcomes ended by human agency
ta %>%
  .[as_factor(.$FATE4) %in% "Original goal thwarted (human agency)",] %>%
  group_by(fate = as_factor(FATE2)) %>%
  summarise(n = n(), 
            n_vid = n_distinct(VOYAGEID))

unique(voyage.fates$outcome_voyage)


# monte carlo simulation to determine typical voyages----
vid_broadregland <- ta[,c("VOYAGEID", "MJSELIMP1")]
vid_broadregland$MJSELIMP1 <- as_factor(vid_broadregland$MJSELIMP1)


vid_decade <- ta[,c("VOYAGEID", "YEAR10")]
vid_decade$YEAR10 <- as_factor(vid_decade$YEAR10) %>%
  as.character() %>%
  gsub("years ", "", .) %>%
  gsub("-.*$","",.) %>%
  as.numeric() %>%
  as.factor()

vid_decade
mps <- middle_passage_stats %>%
  left_join(., vid_decade, by = "VOYAGEID") %>%
  left_join(., vid_broadregland, by = "VOYAGEID")

names(mps)

slice_sample(group_by(mps[!is.na(mps$slaves_emp) & 
                            !is.na(mps$slaves_disemb) & 
                            !is.na(mps$outcome_owner),],
                      YEAR10), n = 20) %>%
  group_by(decade = YEAR10) %>%
  summarise(n_vid = n_distinct(VOYAGEID), 
            avg_slaves_boarded = mean(slaves_emp), 
            avg_slaves_deboarded = mean(slaves_disemb), 
            n_delivered_slaves = sum(outcome_owner == "Delivered slaves for original owners", na.rm = F)/20) %>%
  ggplot(data = ., position = "stack", 
         aes(alpha = n_delivered_slaves)) + 
  geom_col(aes(x = decade, y = avg_slaves_deboarded))+
  geom_col(aes(x = decade, y = avg_slaves_boarded - avg_slaves_deboarded), fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), 
        legend.position = "bottom")

grep("landing", col_defs$def, ignore.case = T, value = T) %>%
  grep("region", ., ignore.case = T, value = T) %>%
  grep("imputed", ., ignore.case = T, value = T)

col_defs[col_defs$def == "Imputed broad region of landing",]$colname
