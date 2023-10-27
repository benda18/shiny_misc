#isst_data_reshape

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)
library(purrr)

#https://stackoverflow.com/questions/57552015/how-to-extract-column-variable-attributes-labels-from-r-to-csv-or-excel

rm(list=ls());cat('\f')
gc()

# wd----
wd <- {list(home   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade", 
            data   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/data", 
            output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/output",
            R      = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/R", 
            shiny  = NA, 
            cw     = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/crosswalks")
}

setwd(wd$R)

# load data----
setwd(wd$data)

load(list.files(pattern = "\\.RData$"))


# explore----

iastsav <- iastsav %>% haven::as_factor()
nastsav <- nastsav %>% haven::as_factor() %>% janitor::clean_names()

na.colname_defs <- nastsav %>%
  map_dfc(attr, "label") %>%
  t() %>%
  as.data.frame

na.colname_defs$def <- na.colname_defs$V1
na.colname_defs$colname <- rownames(na.colname_defs)
na.colname_defs <- na.colname_defs %>% as_tibble() %>%
  .[,c("colname", "def")]

ia.colname_defs <- iastsav %>%
  map_dfc(attr, "label") %>%
  t() %>%
  as.data.frame 

ia.colname_defs$def <- ia.colname_defs$V1
ia.colname_defs$colname <- rownames(ia.colname_defs)
ia.colname_defs <- as_tibble(ia.colname_defs) %>%
  .[,c("colname", "def")]


# look for 'middle passage' & just before & after
na.colname_defs$def %>%
  grep("slave", ., ignore.case = T, value = T)
na.colname_defs$colname[89:90]

ia.colname_defs$def %>%
  grep("middle passage", ., ignore.case = T, value = T)

na.colname_defs[na.colname_defs$def %in% c(na.colname_defs$def %>%
  grep("imputed|crew", ., ignore.case = T, value = T) %>%
  grep("middle passage|total slaves|crew", ., ignore.case = T, value = T)),] %>%
  as.data.table()



nastsav$voyageid
nastsav$adlt1imp
nastsav$adlt2imp
nastsav$adlt3imp

nastsav$adpsale1
nastsav$adpsale2
nastsav$adult1
nastsav$adult2
nastsav$adult3
nastsav$adult4
nastsav$adult5
nastsav$adult6
nastsav$adult7

nastsav$arrport
nastsav$arrport2

nastsav$boy1
nastsav$boy2
nastsav$boy3
nastsav$boy4
nastsav$boy5
nastsav$boy6
nastsav$boy7

nastsav$boyrat1
nastsav$boyrat3
nastsav$boyrat7

nastsav$chil1imp
nastsav$chil2imp
nastsav$chil3imp

nastsav$child1
nastsav$child2
nastsav$child3
nastsav$child4
nastsav$child5
nastsav$child6
nastsav$child7
nastsav$chilrat1
nastsav$chilrat3
nastsav$chilrat7

nastsav$constreg

nastsav$crew

nastsav$d1slatra
nastsav$d1slatrb
nastsav$d1slatrc

nastsav$datarr32
nastsav$datarr33
#...
nastsav$datarr45

nastsav$datebuy
nastsav$datedep
nastsav$datedepa

nastsav$dlslatrb

nastsav$datebuy
#....
nastsav$dateleftafr

nastsav$evgreen
nastsav$embport


cw_master.col.labels[[2]] %>% as_tibble()

cw_collabs <- rbind(cw_master.col.labels[[1]], 
                    cw_master.col.labels[[2]]) %>%
  as_tibble()

cw_collabs[order(cw_collabs$col.name,
                 cw_collabs$source),]

cw_collabs %>%
  mutate(., 
         slaves.ppl = F, 
         crew.ppl = F, 
         date = F, 
         loc = grepl("place|port|middle passage|landing"), 
         fate = F)





cw_collabs %>%
  mutate(., 
         col.name2 = tolower(col.name), 
         col_def2 = tolower(col_def)) %>%
  group_by(col.name2) %>%
  summarise(n_coldef = n_distinct(col_def2), 
            n_source = n_distinct(source)) %>%
  group_by(n_source, n_coldef) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(., 
         pct_n = n / sum(n))

cw_collabs %>%
  mutate(., 
         col.name2 = tolower(col.name)) %>%
  as.data.table() %>%
  dcast(., 
        col.name2 ~ source, 
        value.var = "col_def", 
        fun.aggregate = length) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(., n_ds = iast + nast) 

# FATE crosswalks----
nastsav %>%
  group_by(fate,fate2,
           fate3,fate4) %>%
  summarise()

nastsav$fate %>% levels %>% sort

fate1.df <- data.frame(fate_lvl = sort(levels(nastsav$fate))) %>%
  mutate(., 
         laid_up = grepl("laid up|disarmed", fate_lvl, ignore.case = T), 
         fate_unknown = NA, 
         ship_lost = NA, 
         captured_or_detained = grepl("detained|captured by|taken by", fate_lvl, ignore.case = T), 
         captured_by_slaves   = grepl("captured by slaves|by slaves|taken by slaves", fate_lvl, ignore.case = T),
         shipwrecked_or_destoyed = NA, 
         slaves_sold   = NA, 
         ship_taken_by = NA, 
         ship_looted   = NA, 
         pirates       = grepl("pirates", fate_lvl, ignore.case = T), 
         court_proceedings = grepl("Vice-Admiralty Court|french proceedings|court of mixed commission", fate_lvl, ignore.case = T), 
         laidup_brokenup   = NA, 
         driven_off    = NA, 
         slaves_transshipped = NA, 
         condemned = NA) %>% 
  as_tibble()

fate1.df

interesting.data <- nastsav[,c("voyageid", "voy2imp",  
           "slamimp", "slaximp", 
           #"vymrtimp", 
           "fate", "fate2", "fate3", "fate4",
           #"saild3",
           "tonnage",
           "crew1", "crew2", "crew3", "crew4", "crew5", "crewdied", "ndesert", 
           "vymrtimp", 
           "vymrtrat", 
           "tonnage", 
           "dateleftafr", "dateland1")]

interesting.data$dateleftafr # date voyage left last slaving port
interesting.data$dateland1  # date of landing at first slaving port

interesting.data$slaximp # slaves embarked
interesting.data$slamimp # slaves disembarked

interesting.data$calc_delta.slaves <- interesting.data$slamimp - interesting.data$slaximp

interesting.data$crewdied # crew deaths during complete voyage
interesting.data$ndesert  # total number of crew deserted

interesting.data$calc_min.crew <- apply(interesting.data[,c("crew1", "crew2", 
                                                            "crew3", "crew4", 
                                                            "crew5")], 
                                        1, min, na.rm = T) %>%
  ifelse(is.infinite(.), NA, .) # minimum crew size

interesting.data$calc_max.crew <- apply(interesting.data[,c("crew1", "crew2", 
                                                            "crew3", "crew4", 
                                                            "crew5")], 
      1, max, na.rm = T) %>%
  ifelse(is.infinite(.), NA, .) #maximum crew size

interesting.data$voy2imp # length middle passage

interesting.data$tonnage # vessel tonnage

interesting.data$fate    # voyage outcome
interesting.data$fate2   # outcome for slaves
interesting.data$fate3   # outcome if vessel captured
interesting.data$fate4   # outcome for owner


colnames(interesting.data)[colnames(interesting.data) == "tonnage"] <- c("tonnage", "del_tonnage")
 
na.colname_defs[grep("crew at|crew when|crew deaths", na.colname_defs$def, T, value = F),]

ggplot(data=interesting.data, 
       aes(y = slaximp, 
           x = slamimp)) +
  geom_point() +
  geom_smooth(method = "lm")+
  geom_function(fun = function(x) x * 1.1839 +0.8646, 
                color = "red")

interesting.data$crewdied
interesting.data$slaximp
interesting.data$slamimp
lm(slamimp ~ slaximp, interesting.data) %>%
  summary



interesting.data[!is.na(interesting.data$calc_min.crew) & 
                   !is.na(interesting.data$calc_max.crew) & 
                   !is.na(interesting.data$tonnage),] %>%
  .[,c(1:18,20:24)] %>%
  group_by(calc_min.crew, calc_max.crew, tonnage) %>%
  summarise(n = n()) %>%
  ggplot(data = .) +
  # geom_segment(aes(y = calc_min.crew, yend = calc_max.crew, 
  #                  x = tonnage, xend = tonnage))+
  geom_point(aes(x = tonnage, y = calc_min.crew)) + 
  #geom_point(aes(x = tonnage, y = calc_max.crew))+
  scale_x_log10() +
  scale_y_log10()+
  geom_smooth(aes(y = calc_min.crew, x = tonnage, color = "min"), 
              se = F)+
  geom_smooth(aes(y = calc_max.crew, x = tonnage, color = "max"), 
              se = F)

interesting.data[!is.na(interesting.data$calc_min.crew) & 
                   !is.na(interesting.data$calc_max.crew) & 
                   !is.na(interesting.data$tonnage),] %>%
  .[,c(1:18,20:24)] %>%
  group_by(calc_min.crew, calc_max.crew, tonnage) %>%
  summarise(n = n()) %>%
  ggplot(data = .) +
  geom_density(aes(x = tonnage))


## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, max)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

stopifnot( apply(x, 2, is.vector))

x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
(xsum <- rowsum(x, group))


x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colSums(x)
