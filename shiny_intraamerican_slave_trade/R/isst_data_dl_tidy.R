library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(networkD3)
library(janitor)
library(lubridate)

"https://www.slavevoyages.org/american"
"https://r-spatial.org/projects/"


rm(list=ls());cat('\f')
gc()

# funs----

# wd----
wd <- list(home = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade", 
           data = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/data", 
           output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/output",
           R = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/R", 
           shiny = NA, 
           cw = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/crosswalks")

setwd(wd$R)

# vars----
setwd(wd$data)
data.url <- "https://www.slavevoyages.org/documents/download/I-Am1.0.csv"

# download csv if necessary
if(!"I-Am1.0.csv" %in% list.files()){
  download.file(url = data.url, destfile = "I-Am1.0.csv")
  rm(data.url)
}

# load data----
iast <- read_csv("I-Am1.0.csv")

# other datasets----
slave.dates <- {data.frame(type = c("USA", "UK", 
                                   "Spain", "Netherlands", 
                                   "France", 
                                   "Venezuela", 
                                   "Canada", "Haiti"), 
                          date = ymd(c(18060101, 18070101, 
                                       18110101, 18140101, 
                                       18150101, 18170101, 
                                       18190101, 18220101)))}

# crosswalks----
setwd(wd$cw)

# build data if necessary ----
if(!"cw_fate.csv" %in% list.files()){
  
  fates.text <- "1 Voyage completed as intended
2 Shipwrecked or destroyed, before slaves embarked
3 Shipwrecked or destroyed, after embarkation of slaves or during slaving
4 Shipwrecked or destroyed, after disembarkation
5 Shipwrecked or destroyed - unspecified
6 Captured by pirates or privateers - before slaves embarked
7 Captured by pirates or privateers - after embarkation of slaves
8 Captured by pirates or privateers - after disembarkation
9 Captured by pirates or privateers - unspecified
10 Captured by British - before slaves embarked
11 Captured by British - after embarkation of slaves
12 Captured by British - after disembarkation
13 Captured by British - unspecified
14 Captured by Spanish - before slaves embarked
15 Captured by Spanish - after embarkation of slaves
16 Captured by Spanish - after disembarkation
17 Captured by Spanish - unspecified
18 Captured by Dutch - before slaves embarked
19 Captured by Dutch - after embarkation of slaves
20 Captured by Dutch - after disembarkation
21 Captured by Dutch - unspecified
22 Captured by Portuguese - before slaves embarked
23 Captured by Portuguese - after embarkation of slaves
24 Captured by Portuguese - Americas after disembarkation
25 Captured by Portuguese - unspecified
26 Captured - unspecified
27 Condemned - before slaves embarked
28 Condemned - after embarkation of slaves
29 Condemned - Americas after disembarkation
30 Condemned - unspecified
31 Captured by the Barbary Powers before embarking slaves
39 Destroyed - unspecified
40 Sold
41 Left coast with trading cargo intact
42 Taken by Africans
43 Captured by Cie du Senegal
44 Abandoned and/or sold off Africa
45 Captured unspecified before slaves embarked
46 Captured unspecified after embarkation of slaves
47 Captured unspecified after slaves disembarked
48 Captured by pirates - slaves sold in Americas from another ship
49 Sold slaves in Americas - subsequent fate unknown
50 Captured by the French - before slaves embarked
51 Captured by the French - after embarkation of slaves
52 Captured by the French - after slaves disembarked
53 Captured by the French - unspecified
54 Condemned in the Americas by British after slaves disembarked
55 Pressed into government service
56 Captured by slaves: ship did not reach the Americas
57 Captured by crew: fate unknown
58 Condemned in the Americas by British before slaves disembarked
59 Bought at least one slave in Africa - subsequent fate unknown
66 Destroyed, lost or sold as result of slave rebellion
67 Taken, retaken and salvaged before reaching African trade site
68 Sold in the Americas after disembarking slaves
69 Cut off by Africans from shore, ship did not reach the Americas
70 Sold in the Americas - not known whether ship brought slaves
71 Captured before disembarking slaves; vessel recaptured or released subsequently
72 Driven off the African Coast with slaves on board
73 Captured by slaves, unknown outcome
74 Captured by crew, did not land slaves in the Americas
75 Lost before reaching Americas: slaves reached port in other ships
76 Some slaves removed by pirates/privateers
77 Arrived in Africa, subsequent fate unknown
78 Captured and recaptured after disembarking slaves
79 Crew mutiny; slaves landed in the Americas
80 Cut off by Africans from shore. Recaptured and landed slaves in Americas
81 Captured by slaves, recaptured and landed slaves in the Americas
82 Captured by slaves, recaptured and landed slaves in the Americas, lost
85 Completed voyage after slaves removed for salvage
86 Ship returned direct to home port after selling slaves to another vessel
87 Taken by slaves, vessel recaptured, unknown outcome
88 Sold prematurely in Europe after disembarking slaves in the Americas
89 Captured by the crew, slaves sold in Africa
90 Captured by the crew, no slaves taken, crew sold gold in Europe
91 Condemned in Africa with slaves, slaves transshipped/sold
92 Returned direct to Africa after bringing slaves to the Americas
93 Returned to Europe or Americas without obtaining slaves
94 Laid up (disarmed) or broken up in Africa
95 Laid up (disarmed) or broken up in the Americas
96 Either shipwrecked or captured by pirates before slaves taken on board
97 Abandoned or condemned for un-seaworthiness in the Americas
98 Ship and slaves sold in Africa
99 Taken by slaves, recaptured, then ship lost
102 Vice-Admiralty Court, St. Helena, condemned
103 Vice-Admiralty Court, St. Helena, restored
104 Vice-Admiralty Court, British Guiana, condemned
105 Vice-Admiralty Court, British Guiana, restored
106 Vice-Admiralty Court, Cape of Good Hope, condemned
107 Vice-Admiralty Court, Cape of Good Hope, restored
108 Vice-Admiralty Court, Jamaica, condemned
109 Vice-Admiralty Court, Jamaica, restored
110 Vice-Admiralty Court, Sierra Leone, condemned
111 Vice-Admiralty Court, Sierra Leone, restored
112 Vice-Admiralty Court, Barbados, condemned
113 Vice-Admiralty Court, Barbados, restored
114 Vice-Admiralty Court, Mauritius, condemned
115 Vice-Admiralty Court, Mauritius, restored
118 High Court of Admiralty, condemned
119 High Court of Admiralty, restored
120 Court of Mixed Commission, Sierra Leone, condemned
121 Court of Mixed Commission, Sierra Leone, restored
122 Court of Mixed Commission, Havana, condemned
123 Court of Mixed Commission, Havana, restored
124 Court of Mixed Commission, Rio de Janeiro, condemned
125 Court of Mixed Commission, Rio de Janeiro, restored
126 Court of Mixed Commission, Luanda, condemned
127 Court of Mixed Commission, Luanda, restored
128 Court of Mixed Commission, Cape of Good Hope, condemned
129 Court of Mixed Commission, Cape of Good Hope, restored
130 Lagos, Vice-Admiralty Court, condemned
132 Vice-Admiralty Court, Antigua, condemned
134 Vice-Admiralty Court, Tortola, condemned
135 Vice-Admiralty Court, Tortola, restored
136 Vice-Admiralty Court, Halifax, condemned
138 Vice-Admiralty Court, unspecified, condemned
139 Vice-Admiralty Court, unspecified, restored
141 Given up to the United States government
142 Arrested by Brazil or given up to the Brazilian government
144 Vice Admiralty Court, Dominica, condemned
148 Vice-Admiralty Court, Bahamas, condemned
149 Vice-Admiralty Court, Bahamas, restored
153 Captured and released without court proceedings
154 Driven or run on shore in the Americas: no court proceedings
155 Given up to the Mexican Government, Veracruz
156 Taken to Genoa and given up to Sardinian authorities
157 Abandoned in Europe after completing voyage
159 Captured by Argentinian privateers after embarkation of slaves
160 Captured by United States before slaves embarked
161 Captured by United States with slaves
162 Captured by United States after slaves disembarked
163 Captured by United States, slave presence unspecified
164 French proceedings initiated in Africa, acquittal
165 French proceedings initiated in Africa, condemned
166 French proceedings initiated in Africa, unknown outcome
170 French proceedings, initiated in unknown location, acquittal
171 French proceedings, initiated in unknown location, condemned
172 French proceedings, initiated in unknown location, unknown outcome
173 French proceedings initiated in Guadeloupe, acquittal
174 French proceedings initiated in Guadeloupe, condemned
175 French proceedings initiated in Guadeloupe, unknown outcome
176 French proceedings initiated in Martinique, acquittal
177 French proceedings initiated in Martinique, condemned
178 French proceedings initiated in Martinique, unknown outcome
179 French proceedings initiated in Cayenne, acquittal
180 French proceedings initiated in Cayenne, condemned
181 French proceedings initiated in Cayenne, unknown outcome
182 French proceedings initiated in France, acquittal
183 French proceedings initiated in France, condemned
184 French proceedings initiated in France, unknown outcome
185 Detained and condemned in the United States after slaves disembarked
186 Captured by Dutch, slaves turned loose in Africa
187 Captured by Dutch, slaves turned loose in Brazil
188 Temporarily detained by the Dutch: Voyage allowed to continue with slaves
189 Slaves removed by Dutch and vessel allowed to continue, subsequent fate unknown
191 Captured by Portuguese and Dutch
192 Captured by Algerian pirates
193 Captured by Turks
194 Captured by British, released
195 Looted by Dutch at Elmina
196 Captured first by the British and then by the Dutch after slaves embarked
198 Vessel driven back into port of departure by British and voyage aborted
199 Arrived in Brazil and refused permission to disembark
200 Ship and cargo captured by Africans and ship released
201 Ship captured by Haitian navy and slaves released in Haiti
202 Condemned at Gorée by the British
203 Captured by the British, retaken by original crew, completed voyage
204 Captured by pirates after slaves embarked, ship and slaves burned
205 Captured by English, slaves turned loose on Spanish Main
206 Sold slaves in Africa
207 Sold slaves in Europe, subsequent fate unknown
208 Left home port, no further record
209 Captured by pirates and burned
210 Looted by pirates, voyage continued
211 Captured by Venezuelans before disembarkation of slaves
212 Captured by the Swedes
213 Captured by the Dominican Republic
304 Departed an American port with slaves, subsequent fate unknown
305 Returned to port of embarkation without having sold slaves
306 Privateer captured slaves at sea and delivered for sale in America
307 Prisoners of war stole slaves during escape and carried to port of sale
308 Contracted to carry slaves within the Americas, subsequent fate unknown
309 Captives seized from vessel by Spanish officials and sold
310 Captured by slaves, slaves returned to Africa as free people
311 Shipwrecked, slaves salvaged
312 Captured by slaves, slaves freed by British authorities
313 Shipwrecked, slaves freed by British authorities"
  
  fates.num <- gsub(" .*$", "", read_lines(fates.text)) %>% as.numeric()
  fates.lab <- gsub("^\\d{1,} ", "", read_lines(fates.text))
  
  cw_fate <- data.frame(FATE      = fates.num,
                        FATEdesc  = NA,
                        FATElabel = fates.lab)
  write_csv(x = cw_fate, 
            file = "cw_fate.csv")
  rm(cw_fate, fates.num, fates.lab, fates.text)
}
if(!"cw_fate2.csv" %in% list.files()){
  fates2.text <- "1 Slaves disembarked Americas
2 No slaves embarked
3 Slaves disembarked in Africa or Europe
4 Slaves perished with ship
5 Slaves embarked, transshipped or no further record
6 No information on slaves
7 Slaves embarked but not disembarked in Europe or America"
  
  fates2.num <- gsub(" .*$", "", read_lines(fates2.text)) %>% as.numeric()
  fates2.lab <- gsub("^\\d{1,} ", "", read_lines(fates2.text))
  
  cw_fate2 <- data.frame(FATE2      = fates2.num, 
                         FATE2desc  = NA,
                         FATE2label = fates2.lab)
  write_csv(x = cw_fate2, 
            file = "cw_fate2.csv")
  rm(fates2.lab, fates2.num, fates2.text)
}
if(!"cw_fate3.csv" %in% list.files()){
  ftxt <- "1 Natural hazard
2 Pirate/privateer action
3 British
4 Spanish
5 Dutch
6 Portuguese
8 French
9 US
10 African
11 Crew action
12 Brazil
13 Captor unspecified
14 Not captured
15 Unknown
16 Haitians
17 Venezuela
18 Sweden"
  
  fnum <- gsub(" .*$", "", read_lines(ftxt)) %>% as.numeric()
  flab <- gsub("^\\d{1,} ", "", read_lines(ftxt))
  
  cw_fate3 <- data.frame(FATE3      = fnum, 
                         FATE3desc  = "Outcome of Voyage if vessel captured",
                         FATE3label = flab)
  write_csv(x = cw_fate3, 
            file = "cw_fate3.csv")
  rm(ftxt, flab, fnum, cw_fate3)
}
if(!"cw_fate4.csv" %in% list.files()){
  ftxt <- "1 Delivered slaves for original owners
2 Original goal thwarted (natural hazard)
3 Original goal thwarted (human agency)
4 Unknown outcome"
  
  fnum <- gsub(" .*$", "", read_lines(ftxt)) %>% as.numeric()
  flab <- gsub("^\\d{1,} ", "", read_lines(ftxt))
  
  cw_fate <- data.frame(FATE4      = fnum, 
                        FATE4desc  = "Outcome of Voyage for owner",
                        FATE4label = flab)
  write_csv(x = cw_fate, 
            file = "cw_fate4.csv")
  rm(ftxt, flab, fnum, cw_fate)
}
if(!"cw_fate5.csv" %in% list.files()){
  ftxt <- "1 Slave Insurrection
2 Vessel attacked from shore
3 Vessel’s boats attacked from shore
4 “Cut-off” (meaning unclear)
5 Three of more slaves jumping overboard, missing, or escaped
6 Insurrection planned, but thwarted"
  
  fnum <- gsub(" .*$", "", read_lines(ftxt)) %>% as.numeric()
  flab <- gsub("^\\d{1,} ", "", read_lines(ftxt))
  
  cw_fate <- data.frame(FATE5      = fnum, 
                        FATE5desc  = "African Resistance",
                        FATE5label = flab)
  write_csv(x = cw_fate, 
            file = "cw_fate5.csv")
  rm(ftxt, flab, fnum, cw_fate)
}

# cw_voyage.itinerary----
cw_voyage.itinerary <- NULL
cw_voyage.itinerary$colnames <- {"PORTDEP Port of departure
Format: F5
EMBPORT First intended port of embarkation
Format: F5
EMBPORT2 Second intended port of embarkation
Format: F5
EMBREG First intended region of purchase of slaves
Format: F5
EMBREG2 Second intended region of purchase of slaves
Format: F5
ARRPORT First intended port of disembarkation
Format: F5
ARRPORT2 Second intended port of disembarkation
Format: F5
REGARR First intended region of slave landing
Format: F5
REGARR2 Second intended region of slave landing
Format: F5
NPPRETRA Number of ports of call prior to buying slaves
Format: F3
PLAC1TRA First place of slave purchase
Format: F5
PLAC2TRA Second place of slave purchase
Format: F5
PLAC3TRA Third place of slave purchase
Format: F5
REGEM1 First region of embarkation of slaves
Format: F5
REGEM2 Second region of embarkation of slaves
Format: F5
REGEM3 Third region of embarkation of slaves
Format: F5
NPAFTTRA Port of call before Atlantic crossing
Format: F3
NPPRIOR Number of ports of call in Americas prior to slave of slaves
Format: F1
SLA1PORT First place of slave landing
Format: F5
ADPSALE1 Second place of slave landing
Format: F5
ADPSALE2 Third place of slave landing
Format: F5
REGDIS1 First region of slave landing
Format: F5
REGDIS2 Second region of slave landing
Format: F5
REGDIS3 Third region of slave landing
Format: F5
PORTRET Place at which voyage ended
Format: F5
RETRNREG Region of return
Format: F5
RETRNREG1 Broad region of return
Format: F5
DEPTREGIMP Derived region where voyage began
Format: F5
DEPTREGIMP1 Derived broad region where voyage began
Format: F5
MAJBUYPT Principal place of slave purchase
Format: F5
MAJSELPT Principal port of slave disembarkation
Format: F5" %>% 
  read_lines() %>%
  .[!grepl("^Format: ", .)] %>%
  gsub(" .*$", "", .)}

cw_voyage.itinerary$val_def <- {"PORTDEP Port of departure
Format: F5
EMBPORT First intended port of embarkation
Format: F5
EMBPORT2 Second intended port of embarkation
Format: F5
EMBREG First intended region of purchase of slaves
Format: F5
EMBREG2 Second intended region of purchase of slaves
Format: F5
ARRPORT First intended port of disembarkation
Format: F5
ARRPORT2 Second intended port of disembarkation
Format: F5
REGARR First intended region of slave landing
Format: F5
REGARR2 Second intended region of slave landing
Format: F5
NPPRETRA Number of ports of call prior to buying slaves
Format: F3
PLAC1TRA First place of slave purchase
Format: F5
PLAC2TRA Second place of slave purchase
Format: F5
PLAC3TRA Third place of slave purchase
Format: F5
REGEM1 First region of embarkation of slaves
Format: F5
REGEM2 Second region of embarkation of slaves
Format: F5
REGEM3 Third region of embarkation of slaves
Format: F5
NPAFTTRA Port of call before Atlantic crossing
Format: F3
NPPRIOR Number of ports of call in Americas prior to slave of slaves
Format: F1
SLA1PORT First place of slave landing
Format: F5
ADPSALE1 Second place of slave landing
Format: F5
ADPSALE2 Third place of slave landing
Format: F5
REGDIS1 First region of slave landing
Format: F5
REGDIS2 Second region of slave landing
Format: F5
REGDIS3 Third region of slave landing
Format: F5
PORTRET Place at which voyage ended
Format: F5
RETRNREG Region of return
Format: F5
RETRNREG1 Broad region of return
Format: F5
DEPTREGIMP Derived region where voyage began
Format: F5
DEPTREGIMP1 Derived broad region where voyage began
Format: F5
MAJBUYPT Principal place of slave purchase
Format: F5
MAJSELPT Principal port of slave disembarkation
Format: F5" %>% 
  read_lines() %>%
  .[!grepl("^Format: ", .)] %>%
  gsub(pattern =  paste("^", cw_voyage.itinerary$colnames, sep = "|", collapse = "|"), 
       replacement = "", x = .) %>%
  trimws()}

cw_voyage.itinerary <- as.data.frame.list(cw_voyage.itinerary)
cw_voyage.itinerary$cw_cat <- "voyage.itinerary"

# cw_voyage.dates----
cw_voyage.dates <- NULL
cw_voyage.dates$colnames <- {
  "DATEDEPA Day that voyage began
Format: F2
DATEDEPB Month that voyage began
Format: F2
DATEDEPC Year that voyage began
Format: F4
D1SLATRA Day that slave purchase began
Format: F2
D1SLATRB Month that slave purchase began
Format: F2
D1SLATRC Year that slave purchase began
Format: F4
DLSLATRA Day that vessel left last slaving port
Format: F2
DLSLATRB Month that vessel left last slaving port
Format: F2
DLSLATRC Year that vessel left last slaving port
Format: F4
DATARR32 Day of first disembarkation of slaves
Format: F2
DATARR33 Month of first disembarkation of slaves
Format: F2
DATARR34 Year of first disembarkation of slaves
Format: F4
DATARR36 Day of arrival at second place of landing
Format: F2
DATARR37 Month of arrival at second place of landing
Format: F2
DATARR38 Year of arrival at second place of landing
Format: F4
DATARR39 Day of third disembarkation of slaves
Format: F2
DATARR40 Month of third disembarkation of slaves
Format: F2
DATARR41 Year of third disembarkation of slaves
Format: F4
DDEPAM Day of departure from last place of landing
Format: F2
DDEPAMB Month of departure from last place of landing
Format: F2
DDEPAMC Year of departure from last place of landing
Format: F4
DATARR43 Day on which voyage completed
Format: F2
DATARR44 Month in which voyage completed
Format: F2
DATARR45 Year in which voyage completed
Format: F4
DATE_DEP Date that voyage began
Format: Date10
DATE_BUY Date that slave purchase began
Format: Date10
DATE_LEFTAFR Date that vessel left last slaving port
Format: Date10
DATE_LAND1 Date that slaves landed at first place
Format: Date10
DATE_LAND2 Date that slaves landed at second place
Format: Date10
DATE_LAND3 Date that slaves landed at third place
Format: Date10
DATE_DEPAM Date ship left on return voyage
Format: Date10
DATE_END Date when voyage completed
Format: Date10
VOYAGE Length of Middle Passage in days
Format: F3
YEAR5 5-year period in which voyage occurred
Format: F3
YEAR10 Decade in which voyage occurred
Format: F3
YEAR25 Quarter-century in which voyage occurred
Format: F3
YEAR100 Century in which voyage occurred
Format: F4
VOY1IMP Voyage length from home port to disembarkation (days)
Format: F5
VOY2IMP Voyage length from leaving Africa to disembarkation (days)
Format: F5"  %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(" .*$", "", .)
}
cw_voyage.dates$val_def <- {
  "DATEDEPA Day that voyage began
Format: F2
DATEDEPB Month that voyage began
Format: F2
DATEDEPC Year that voyage began
Format: F4
D1SLATRA Day that slave purchase began
Format: F2
D1SLATRB Month that slave purchase began
Format: F2
D1SLATRC Year that slave purchase began
Format: F4
DLSLATRA Day that vessel left last slaving port
Format: F2
DLSLATRB Month that vessel left last slaving port
Format: F2
DLSLATRC Year that vessel left last slaving port
Format: F4
DATARR32 Day of first disembarkation of slaves
Format: F2
DATARR33 Month of first disembarkation of slaves
Format: F2
DATARR34 Year of first disembarkation of slaves
Format: F4
DATARR36 Day of arrival at second place of landing
Format: F2
DATARR37 Month of arrival at second place of landing
Format: F2
DATARR38 Year of arrival at second place of landing
Format: F4
DATARR39 Day of third disembarkation of slaves
Format: F2
DATARR40 Month of third disembarkation of slaves
Format: F2
DATARR41 Year of third disembarkation of slaves
Format: F4
DDEPAM Day of departure from last place of landing
Format: F2
DDEPAMB Month of departure from last place of landing
Format: F2
DDEPAMC Year of departure from last place of landing
Format: F4
DATARR43 Day on which voyage completed
Format: F2
DATARR44 Month in which voyage completed
Format: F2
DATARR45 Year in which voyage completed
Format: F4
DATE_DEP Date that voyage began
Format: Date10
DATE_BUY Date that slave purchase began
Format: Date10
DATE_LEFTAFR Date that vessel left last slaving port
Format: Date10
DATE_LAND1 Date that slaves landed at first place
Format: Date10
DATE_LAND2 Date that slaves landed at second place
Format: Date10
DATE_LAND3 Date that slaves landed at third place
Format: Date10
DATE_DEPAM Date ship left on return voyage
Format: Date10
DATE_END Date when voyage completed
Format: Date10
VOYAGE Length of Middle Passage in days
Format: F3
YEAR5 5-year period in which voyage occurred
Format: F3
YEAR10 Decade in which voyage occurred
Format: F3
YEAR25 Quarter-century in which voyage occurred
Format: F3
YEAR100 Century in which voyage occurred
Format: F4
VOY1IMP Voyage length from home port to disembarkation (days)
Format: F5
VOY2IMP Voyage length from leaving Africa to disembarkation (days)
Format: F5" %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(pattern =  paste("^", cw_voyage.dates$colnames, sep = "|", collapse = "|"), 
         replacement = "", x = .) %>%
    trimws()
}
cw_voyage.dates <- as.data.frame.list(cw_voyage.dates)
cw_voyage.dates$cw_cat <- "voyage.dates"


# cw_captain.crew----
cw_captain.crew <- NULL
cw_captain.crew$colnames <- {
  "CAPTAINA First captain’s name
Format: A60
CAPTAINB Second captain’s name
Format: A40
CAPTAINC Third captain’s name
Format: A40
CREW1 Crew at voyage outset
Format: F3
CREW2 Crew at departure from last port of slave purchase
Format: F3
CREW3 Crew at first landing of slaves
Format: F2
CREW4 Crew when return voyage began
Format: F2
CREW5 Crew at end of voyage
Format: F2
CREW Number of crew unspecified
Format: F3
SAILD1 Crew died before first place of trade in Africa
Format: F2
SAILD2 Crew died while ship was on Africa coast
Format: F2
SAILD3 Crew died during Middle Passage
Format: F2
SAILD4 Crew died in the Americas
Format: F2
SAILD5 Crew died on return voyage
Format: F2
CREWDIED Crew died during complete voyage
Format: F3
NDESERT Total number of crew deserted
Format: F2"   %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(" .*$", "", .)
}
cw_captain.crew$val_def <- {
  "CAPTAINA First captain’s name
Format: A60
CAPTAINB Second captain’s name
Format: A40
CAPTAINC Third captain’s name
Format: A40
CREW1 Crew at voyage outset
Format: F3
CREW2 Crew at departure from last port of slave purchase
Format: F3
CREW3 Crew at first landing of slaves
Format: F2
CREW4 Crew when return voyage began
Format: F2
CREW5 Crew at end of voyage
Format: F2
CREW Number of crew unspecified
Format: F3
SAILD1 Crew died before first place of trade in Africa
Format: F2
SAILD2 Crew died while ship was on Africa coast
Format: F2
SAILD3 Crew died during Middle Passage
Format: F2
SAILD4 Crew died in the Americas
Format: F2
SAILD5 Crew died on return voyage
Format: F2
CREWDIED Crew died during complete voyage
Format: F3
NDESERT Total number of crew deserted
Format: F2"   %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(pattern =  paste("^", cw_captain.crew$colnames, sep = "|", collapse = "|"), 
         replacement = "", x = .) %>%
    trimws()
}
cw_captain.crew <- as.data.frame.list(cw_captain.crew)
cw_captain.crew$cw_cat <- "captain.and.crew"


# cw_slaves.numbers----
cw_slaves.numbers <- NULL
cw_slaves.numbers$colnames <- {
  "SLINTEND Slaves intended from first port of purchase
Format: F4
SLINTEN2 Slaves intended from second port of purchase
Format: F4
NCAR13 Slaves carried from first port of purchase
Format: F4
NCAR15 Slaves carried from second port of purchase
Format: F4
NCAR17 Slaves carried from third port of purchase
Format: F4
TSLAVESP Total slaves purchased
Format: F4
TSLAVESD Total slaves on board at departure from last slaving port
Format: F4
SLAARRIV Total slaves arrived at first port of disembarkation
Format: F4
SLAS32 Slaves disembarked at first place
Format: F4
SLAS36 Slaves disembarked at second place
Format: F4
SLAS39 Slaves disembarked at third place
Format: F4" %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(" .*$", "", .)
}
cw_slaves.numbers$val_def <- {
  "SLINTEND Slaves intended from first port of purchase
Format: F4
SLINTEN2 Slaves intended from second port of purchase
Format: F4
NCAR13 Slaves carried from first port of purchase
Format: F4
NCAR15 Slaves carried from second port of purchase
Format: F4
NCAR17 Slaves carried from third port of purchase
Format: F4
TSLAVESP Total slaves purchased
Format: F4
TSLAVESD Total slaves on board at departure from last slaving port
Format: F4
SLAARRIV Total slaves arrived at first port of disembarkation
Format: F4
SLAS32 Slaves disembarked at first place
Format: F4
SLAS36 Slaves disembarked at second place
Format: F4
SLAS39 Slaves disembarked at third place
Format: F4" %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(pattern =  paste("^", cw_slaves.numbers$colnames, sep = "|", collapse = "|"), 
         replacement = "", x = .) %>%
    trimws()
}
cw_slaves.numbers <- as.data.frame.list(cw_slaves.numbers)
cw_slaves.numbers$cw_cat <- "Slaves.(numbers)"

# cw_slaves.chars ----
cw_slaves.chars <- NULL
cw_slaves.chars$colnames <- {
  "MEN1 Men embarked at first port of purchase
Format: F3
WOMEN1 Women embarked at first port of purchase
Format: F3
BOY1 Boys embarked at first port of purchase
Format: F3
GIRL1 Girls embarked at first port of purchase
Format: F3
ADULT1 Adults embarked at first port of purchase
Format: F3
CHILD1 Children embarked at first port of purchase
Format: F3
INFANT1 Infants embarked at first port of purchase
Format: F3
MALE1 Males embarked at first port of purchase
Format: F3
FEMALE1 Females embarked at first port of purchase
Format: F3
MEN2 Men who died on Middle Passage
Format: F3
WOMEN2 Women who died on Middle Passage
Format: F3
BOY2 Boys who died on Middle Passage
Format: F3
GIRL2 Girls who died on Middle Passage
Format: F3
ADULT2 Adults who died on Middle Passage
Format: F3
CHILD2 Children who died on Middle Passage
Format: F3
MALE2 Males who died on Middle Passage
Format: F3
FEMALE2 Females who died on Middle Passage
Format: F3
MEN3 Men disembarked at first place of landing
Format: F3
WOMEN3 Women disembarked at first place of landing
Format: F3
BOY3 Boys disembarked at first place of landing
Format: F3
GIRL3 Girls disembarked at first place of landing
Format: F3
ADULT3 Adults disembarked at first place of landing
Format: F3
CHILD3 Children disembarked at first place of landing
Format: F3
INFANT3 Infants disembarked at first place of landing
Format: F3
MALE3 Males disembarked at first place of landing
Format: F3
FEMALE3 Females disembarked at first place of landing
Format: F3
MEN4 Men embarked at second port of purchase
Format: F3
WOMEN4 Women embarked at second port of purchase
Format: F3
BOY4 Boys embarked at second port of purchase
Format: F3
GIRL4 Girl embarked at second port of purchase
Format: F3
ADULT4 Adults embarked at second port of purchase
Format: F3
CHILD4 Children embarked at second port of purchase
Format: F3
INFANT4 Infants embarked at second port of purchase
Format: F3
MALE4 Males embarked at second port of purchase
Format: F3
FEMALE4 Females embarked second port of purchase
Format: F3
MEN5 Men embarked at third port of purchase
Format: F3
WOMEN5 Women embarked at third port of purchase
Format: F3
BOY5 Boys embarked at third port of purchase
Format: F3
GIRL5 Girls embarked at third port of purchase
Format: F3
ADULT5 Adults embarked at third port of purchase
Format: F3
CHILD5 Children embarked at third port of purchase
Format: F3
MALE5 Males embarked at third port of purchase
Format: F3
FEMALE5 Females embarked at third port of purchase
Format: F3
MEN6 Men disembarked at second place of landing
Format: F3
WOMEN6 Women disembarked at second place of landing
Format: F3
BOY6 Boys disembarked at second place of landing
Format: F3
GIRL6 Girls disembarked at second place of landing
Format: F3
ADULT6 Adults disembarked at second place of landing
Format: F3
CHILD6 Children disembarked at second place of landing
Format: F3
MALE6 Males disembarked at second place of landing
Format: F3
FEMALE6 Females disembarked at second place of landing
Format: F3
SLADAFRI Slaves deaths before leaving Africa
Format: F3
SLADVOY Slaves deaths between African and the Americas
Format: F4
SLADAMER Slaves deaths between arrival and sale
Format: F3
ADLT1IMP Derived number of adult embarked
Format: F4
CHIL1IMP Derived number of children embarked
Format: F4
MALE1IMP Derived number of males embarked
Format: F4
FEML1IMP Derived number of females embarked
Format: F4
SLAVMAX1 Total slaves embarked with age and gender identified
Format: F4
SLAVEMA1 Total slaves embarked with age identified
Format: F4
SLAVEMX1 Total slaves embarked with gender identified
Format: F4
MENRAT1 Percentage of men among embarked slaves
Format: F8.5
WOMRAT1 Percentage of women among embarked slaves
Format: F8.5
BOYRAT1 Percentage of boys among embarked slaves
Format: F8.5
GIRLRAT1 Percentage of girls among embarked slaves
Format: F8.5
CHILRAT1 Child ratio among embarked slaves
Format: F8.5
MALRAT1 Male ratio among embarked slaves
Format: F8.5
ADLT2IMP Derived number of adults who died on Middle Passage
Format: F4
CHIL2IMP Derived number of children who died on Middle Passage
Format: F4
MALE2IMP Derived number of males who died on Middle Passage
Format: F4
FEML2IMP Derived number of females who died on Middle Passage
Format: F4
ADLT3IMP Derived number of adults landed
Format: F4
CHIL3IMP Derived number of children landed
Format: F4
MALE3IMP Derived number of males landed
Format: F4
FEML3IMP Derived number of females landed
Format: F4
SLAVMAX3 Total slaves identified by age and gender among landed slaves
Format: F4
SLAVEMA3 Total slaves identified by age among landed slaves
Format: F4
SLAVEMX3 Total slaves identified by gender among landed slaves
Format: F4
MENRAT3 Percentage of men among landed slaves
Format: F8.5
WOMRAT3 Percentage of women among landed slaves
Format: F8.5
BOYRAT3 Percentage of boys among landed slaves
Format: F8.5
GIRLRAT3 Percentage of girls among landed slaves
Format: F8.5
CHILRAT3 Child ratio among landed slaves
Format: F8.5
MALRAT3 Male ratio among landed slaves
Format: F8.5
MEN7 Derived number of men at departure or arrival
Format: F4
WOMEN7 Derived number of women at departure or arrival
Format: F4
BOY7 Derived number of boys at departure or arrival
Format: F4
GIRL7 Derived number of girls at departure or arrival
Format: F4
ADULT 7 Derived number of adults at departure or arrival
Format: F4
CHILD7 Derived number of children at departure or arrival
Format: F4
MALE7 Derived number of males at departure or arrival
Format: F4
FEMALE7 Derived number of females at departure or arrival
Format: F4
SLAVMAX7 Total slaves identified by age and gender at departure or arrival
Format: F4
SLAVEMA7 Total slaves identified by age at departure or arrival
Format: F4
SLAVEMX7 Total slaves identified by gender at departure or arrival
Format: F4
MENRAT7 Percentage of men at departure or arrival
Format: F8.5
WOMRAT7 Percentage of women at departure or arrival
Format: F8.5
BOYRAT7 Percentage of boys at departure or arrival
Format: F8.5
GIRLRAT7 Percentage of girls at departure or arrival
Format: F8.5
CHILRAT7 Child ratio at departure or arrival
Format: F8.5
MALRAT7 Male ratio at departure or arrival
Format: F8.5
TSLMTIMP Derived number of slaves embarked for mortality calculation
Format: F4
VYMRTIMP Derived slave deaths during Middle Passage
Format: F4
VYMRTRAT Slave mortality rate (slave deaths / slaves embarked)
Format: F8.5
JAMCASPR Average price of slaves standardized on sterling cash price of prime slaves sold in
Jamaica
Format: F8.2"   %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(" .*$", "", .)
}
cw_slaves.chars$val_def <- {
  "MEN1 Men embarked at first port of purchase
Format: F3
WOMEN1 Women embarked at first port of purchase
Format: F3
BOY1 Boys embarked at first port of purchase
Format: F3
GIRL1 Girls embarked at first port of purchase
Format: F3
ADULT1 Adults embarked at first port of purchase
Format: F3
CHILD1 Children embarked at first port of purchase
Format: F3
INFANT1 Infants embarked at first port of purchase
Format: F3
MALE1 Males embarked at first port of purchase
Format: F3
FEMALE1 Females embarked at first port of purchase
Format: F3
MEN2 Men who died on Middle Passage
Format: F3
WOMEN2 Women who died on Middle Passage
Format: F3
BOY2 Boys who died on Middle Passage
Format: F3
GIRL2 Girls who died on Middle Passage
Format: F3
ADULT2 Adults who died on Middle Passage
Format: F3
CHILD2 Children who died on Middle Passage
Format: F3
MALE2 Males who died on Middle Passage
Format: F3
FEMALE2 Females who died on Middle Passage
Format: F3
MEN3 Men disembarked at first place of landing
Format: F3
WOMEN3 Women disembarked at first place of landing
Format: F3
BOY3 Boys disembarked at first place of landing
Format: F3
GIRL3 Girls disembarked at first place of landing
Format: F3
ADULT3 Adults disembarked at first place of landing
Format: F3
CHILD3 Children disembarked at first place of landing
Format: F3
INFANT3 Infants disembarked at first place of landing
Format: F3
MALE3 Males disembarked at first place of landing
Format: F3
FEMALE3 Females disembarked at first place of landing
Format: F3
MEN4 Men embarked at second port of purchase
Format: F3
WOMEN4 Women embarked at second port of purchase
Format: F3
BOY4 Boys embarked at second port of purchase
Format: F3
GIRL4 Girl embarked at second port of purchase
Format: F3
ADULT4 Adults embarked at second port of purchase
Format: F3
CHILD4 Children embarked at second port of purchase
Format: F3
INFANT4 Infants embarked at second port of purchase
Format: F3
MALE4 Males embarked at second port of purchase
Format: F3
FEMALE4 Females embarked second port of purchase
Format: F3
MEN5 Men embarked at third port of purchase
Format: F3
WOMEN5 Women embarked at third port of purchase
Format: F3
BOY5 Boys embarked at third port of purchase
Format: F3
GIRL5 Girls embarked at third port of purchase
Format: F3
ADULT5 Adults embarked at third port of purchase
Format: F3
CHILD5 Children embarked at third port of purchase
Format: F3
MALE5 Males embarked at third port of purchase
Format: F3
FEMALE5 Females embarked at third port of purchase
Format: F3
MEN6 Men disembarked at second place of landing
Format: F3
WOMEN6 Women disembarked at second place of landing
Format: F3
BOY6 Boys disembarked at second place of landing
Format: F3
GIRL6 Girls disembarked at second place of landing
Format: F3
ADULT6 Adults disembarked at second place of landing
Format: F3
CHILD6 Children disembarked at second place of landing
Format: F3
MALE6 Males disembarked at second place of landing
Format: F3
FEMALE6 Females disembarked at second place of landing
Format: F3
SLADAFRI Slaves deaths before leaving Africa
Format: F3
SLADVOY Slaves deaths between African and the Americas
Format: F4
SLADAMER Slaves deaths between arrival and sale
Format: F3
ADLT1IMP Derived number of adult embarked
Format: F4
CHIL1IMP Derived number of children embarked
Format: F4
MALE1IMP Derived number of males embarked
Format: F4
FEML1IMP Derived number of females embarked
Format: F4
SLAVMAX1 Total slaves embarked with age and gender identified
Format: F4
SLAVEMA1 Total slaves embarked with age identified
Format: F4
SLAVEMX1 Total slaves embarked with gender identified
Format: F4
MENRAT1 Percentage of men among embarked slaves
Format: F8.5
WOMRAT1 Percentage of women among embarked slaves
Format: F8.5
BOYRAT1 Percentage of boys among embarked slaves
Format: F8.5
GIRLRAT1 Percentage of girls among embarked slaves
Format: F8.5
CHILRAT1 Child ratio among embarked slaves
Format: F8.5
MALRAT1 Male ratio among embarked slaves
Format: F8.5
ADLT2IMP Derived number of adults who died on Middle Passage
Format: F4
CHIL2IMP Derived number of children who died on Middle Passage
Format: F4
MALE2IMP Derived number of males who died on Middle Passage
Format: F4
FEML2IMP Derived number of females who died on Middle Passage
Format: F4
ADLT3IMP Derived number of adults landed
Format: F4
CHIL3IMP Derived number of children landed
Format: F4
MALE3IMP Derived number of males landed
Format: F4
FEML3IMP Derived number of females landed
Format: F4
SLAVMAX3 Total slaves identified by age and gender among landed slaves
Format: F4
SLAVEMA3 Total slaves identified by age among landed slaves
Format: F4
SLAVEMX3 Total slaves identified by gender among landed slaves
Format: F4
MENRAT3 Percentage of men among landed slaves
Format: F8.5
WOMRAT3 Percentage of women among landed slaves
Format: F8.5
BOYRAT3 Percentage of boys among landed slaves
Format: F8.5
GIRLRAT3 Percentage of girls among landed slaves
Format: F8.5
CHILRAT3 Child ratio among landed slaves
Format: F8.5
MALRAT3 Male ratio among landed slaves
Format: F8.5
MEN7 Derived number of men at departure or arrival
Format: F4
WOMEN7 Derived number of women at departure or arrival
Format: F4
BOY7 Derived number of boys at departure or arrival
Format: F4
GIRL7 Derived number of girls at departure or arrival
Format: F4
ADULT 7 Derived number of adults at departure or arrival
Format: F4
CHILD7 Derived number of children at departure or arrival
Format: F4
MALE7 Derived number of males at departure or arrival
Format: F4
FEMALE7 Derived number of females at departure or arrival
Format: F4
SLAVMAX7 Total slaves identified by age and gender at departure or arrival
Format: F4
SLAVEMA7 Total slaves identified by age at departure or arrival
Format: F4
SLAVEMX7 Total slaves identified by gender at departure or arrival
Format: F4
MENRAT7 Percentage of men at departure or arrival
Format: F8.5
WOMRAT7 Percentage of women at departure or arrival
Format: F8.5
BOYRAT7 Percentage of boys at departure or arrival
Format: F8.5
GIRLRAT7 Percentage of girls at departure or arrival
Format: F8.5
CHILRAT7 Child ratio at departure or arrival
Format: F8.5
MALRAT7 Male ratio at departure or arrival
Format: F8.5
TSLMTIMP Derived number of slaves embarked for mortality calculation
Format: F4
VYMRTIMP Derived slave deaths during Middle Passage
Format: F4
VYMRTRAT Slave mortality rate (slave deaths / slaves embarked)
Format: F8.5
JAMCASPR Average price of slaves standardized on sterling cash price of prime slaves sold in
Jamaica
Format: F8.2"  %>% 
    read_lines() %>%
    .[!grepl("^Format: ", .)] %>%
    gsub(pattern =  paste("^", cw_slaves.chars$colnames, sep = "|", collapse = "|"), 
         replacement = "", x = .) %>%
    trimws()
}
cw_slaves.chars <- as.data.frame.list(cw_slaves.numbers)
cw_slaves.chars$cw_cat <- "Slaves.(characteristics)"
 
#cw_source----
{"SOURCEA First source of information
Format: A60
SOURCEB Second source of information
Format: A50
SOURCEC Third source of information
Format: A50
SOURCED Fourth source of information
Format: A50
SOURCEE Fifth source of information
Format: A50
SOURCEF Sixth source of information
Format: A50
SOURCEG Seventh source of information
Format: A50
SOURCEH Eighth source of information
Format: A50
SOURCEI Ninth source of information
Format: A50
SOURCEJ Tenth source of information
Format: A50
SOURCEK Eleventh source of information
Format: A50
SOURCEL Twelfth source of information
Format: A40
SOURCEM Thirteenth source of information
Format: A40
SOURCEN Fourteenth source of information
Format: A40
SOURCEO Fifteenth source of information
Format: A40
SOURCEP Sixteenth source of information
Format: A40
SOURCEQ Seventeenth source of information
Format: A40
SOURCER Eighteenth source of information
Format: A40" }

# cw_county_ship_register----
cw_county_ship_register <- {data.frame(value = c(3,6,7,8,9,10,15,30), 
                                       val_def = c("Spain/Uruguay", 
                                                   "Portugal/Brazil", 
                                                   "Great Britain", 
                                                   "Netherlands",
                                                   "U.S.A.", 
                                                   "France",
                                                   "Denmark/Baltic", 
                                                   "Other"))}

#cw_imp_yoyage.itinerary----
cw_imp_yoyage.itinerary <- NULL

cw_imp_yoyage.itinerary$value <- "PTDEPIMP Imputed port where voyage began
Format: F5
MJBYPTIMP Imputed principal place of slave purchase
Format: F5
MAJBYIMP Imputed principal region of slave purchase
Format: F5
MAJBYIMP1 Imputed broad region of slave purchase
Format: F5
MJSLPTIMP Imputed principal port of slave disembarkation
Format: F5
MJSELIMP Imputed principal region of slave disembarkation
Format: F5
MJSELIMP1 Imputed broad region of slave disembarkation
Format: F5"  %>% read_lines %>%
  .[!grepl("^Format: ", .)] %>%
  gsub(" Imputed", "_Imputed", .) %>%
  strsplit(., "_") %>%
    lapply(., first) %>%
    unlist()

cw_imp_yoyage.itinerary$def <- "PTDEPIMP Imputed port where voyage began
Format: F5
MJBYPTIMP Imputed principal place of slave purchase
Format: F5
MAJBYIMP Imputed principal region of slave purchase
Format: F5
MAJBYIMP1 Imputed broad region of slave purchase
Format: F5
MJSLPTIMP Imputed principal port of slave disembarkation
Format: F5
MJSELIMP Imputed principal region of slave disembarkation
Format: F5
MJSELIMP1 Imputed broad region of slave disembarkation
Format: F5"  %>% read_lines %>%
  .[!grepl("^Format: ", .)] %>%
  gsub(" Imputed", "_Imputed", .) %>%
  strsplit(., "_") %>%
  lapply(., last) %>%
  unlist()
cw_imp_yoyage.itinerary <- as.data.frame.list(cw_imp_yoyage.itinerary)
cw_imp_yoyage.itinerary$cat <- "cw_imp_yoyage.itinerary"

# cw_imp_voyage.dates----
cw_imp_voyage.dates <- "YEARDEP Year voyage began (imputed)
Format: F4
YEARAF Year departed Africa (imputed)
Format: F4
YEARAM Year of arrival at port of disembarkation (imputed)
Format: F4" %>% read_lines %>%
  .[!grepl("^Format: ", .)] %>%
  gsub(" Year", "_Year", .) %>%
  strsplit(., "_") 

# cw_imp_slaves.numbers----
cw_imp_slaves.numbers <- data.frame(colname = c("SLAXIMP", "SLAMIMP"),
                                    def.imp = c("Imputed total slaves embarked", 
                                                "Imputed total slaves disembarked"))

# cw_specific_places----
cw_specific_places <- {"10101 Alicante
10102 Barcelona
10103 Bilbao
10104 Cadiz
10105 Figuera
10106 Gibraltar
10107 La Coruña
10110 Santander
10111 Seville
10112 San Lucar
10113 Vigo
10114 Malaga
10115 Puerto Santa Maria
10199 Spain, port unspecified
10202 Lagos
10203 Lisbon
10204 Oporto
10205 Ilho do Fayal
10206 Setubal
10299 Portugal, port unspecified
10399 Great Britain, port unspecified
10401 Barmouth
10402 Bideford
10403 Birkenhead
10404 Bristol
10405 Brixham
10406 Broadstairs
10407 Cawsand
10408 Chepstow
10409 Chester
10410 Colchester
10411 Cowes
10412 Dartmouth
10413 Deptford
10414 Dover
10415 Exeter
10416 Folkstone
10417 Frodsham
10418 Gainsborough
10419 Greenwich
10420 Guernsey
10421 Harwich
10422 Hull
10423 Ilfracombe
10424 Ipswich
10425 Isle of Man
10426 Isle of Wight
10427 Jersey
10428 Kendal
10429 King's Lynn
10430 Lancaster
10431 Lindale
10432 Liverpool
10433 London
10434 Lyme
10436 Maryport
10437 Milford Haven
10438 New Shoreham
10439 Newcastle upon Tyne
10440 Newnham
10441 North Shields
10442 Northam
10443 Norwich
10444 Padstowe
10445 Parkgate
10446 Piel of Foulney
10447 Plymouth
10448 Poole
10449 Portsery
10450 Portsmouth
10451 Poulton
10452 Preston
10453 Ramsgate
10454 Ravenglass
10455 River Thames
10456 Rochester
10457 Rotherhithe
10458 Rye
10459 Scarborough
10460 Sheerness
10461 Shields
10462 Shoreham
10463 Sidmouth
10464 Southampton
10466 Stockton
10467 Stockwithe
10468 Sunderland
10469 Teignmouth
10470 Topsham
10471 Torbay
10472 Wales
10473 Whitby
10474 Whitehaven
10476 Workington
10477 Yarmouth
10478 Youghal
10479 Conway
10480 Falmouth (England)
10481 Lymehouse (London)
10482 St Osyth
10483 Woodbridge
10484 Workington
10485 Lympstone
10499 England, port unspecified
10501 Airth
10502 Borrowstounness
10503 Dundee
10504 Glasgow
10505 Greenock
10506 Leith
10507 Lough Neagh
10508 Montrose
10509 Ross
10510 Saltcoats
10511 Largo, Fifeshire
10512 Dumfries
10513 Point Askaig
10514 Cambletown
10515 Portpatrick
10516 Erwin
10517 Aberdeen
10518 Stranraer
10519 Oban
10599 Scotland, port unspecified
10601 Cork
10602 Dublin
10603 Dungarvan
10604 Galway
10605 Kinsale
10606 Limerick
10607 Londonderry
10608 Ringsend
10609 Strangford
10610 Waterford
10611 Wexford
10612 Belfast
10613 Coleraine
10699 Ireland, port unspecified
10702 Bayonne
10703 Bordeaux
10704 Brest
10705 Calais
10706 Camaret
10707 Dieppe
10708 Dinard
10709 Dunkerque
10710 Honfleur
10711 Ile de Batz
10712 La Rochelle
10713 Le Havre
10714 Les Sables
10715 Lorient
10716 Marseille
10717 Mediterranean coast (France)
10718 Mindin
10719 Morlaix
10720 Nantes
10721 Paimboeuf
10722 Port Louis
10723 Quimper
10724 Rochefort
10725 Saint-Brieuc
10726 Saint-Malo
10727 Saint-Nazaire
10728 Saint-Tropez
10729 Sete
10730 Vannes
10799 France, port unspecified
10810 Amsterdam
10821 Enkhuizen
10822 Hoorn
10827 Zaandam
10828 Zuider Zee
10829 Texel
10830 Rotterdam
10831 Dordrecht
10832 Maze
10833 Moerdijk
10834 Hellevoetsluis
10840 Zeeland
10850 Middelburg
10851 Vlissingen
10852 Veere
10853 Zeeland or Middelburg
10860 Groningen
10861 Stad en Lande
10899 Netherlands, port unspecified
10901 København
10902 Husum
10903 Helsingør
10999 Denmark, port unspecified
11001 Pillau
11002 Elbe
11003 Emden
11004 Hamburg
11005 Hanover
11006 Lübeck
11007 Glückstadt
11099 Northern Germany, port unspecified
11101 Antwerp
11102 Flanders
11103 Brugge
11199 Belgium, port unspecified
11201 Tønsberg
11202 Bergen
11301 Göteborg
11399 Sweden, port unspecified
11401 Genova
11402 Napoli
11404 Trieste
11499 Italy, port unspecified
11501 Kronstadt
11502 Odessa
11599 Russia, port unspecified
11601 Sardinia
11699 Mediterranean, port unspecified
20101 Bristol (RI)
20102 Little Compton
20103 Newport
20104 North Kingston
20105 Providence
20106 Tiverton
20107 Warren
20108 Warwick, RI
20199 Rhode Island, port unspecified
20201 Bath
20202 Berwick
20203 Biddeford
20204 Calais (Maine)
20205 Camden
20207 Freeport
20208 Portland
20209 Prospect
20210 Richmond
20211 Robbinston
20212 Rockland
20213 Sheepscutt River
20214 Wiscasset
20215 Kittery Point
20216 Orrington
20217 Penobscot
20299 Maine, port unspecified
20301 Piscataqua
20302 Portsmouth (NH)
20303 North Hampton
20399 New Hampshire, port unspecified
20401 Massachusetts Bay
20402 Amesbury
20403 Arundel
20404 Boston
20405 Chatham
20406 Cohasset
20407 Dighton
20408 Duxbury
20409 Freetown (Mass.)
20410 Georgetown (Mass.)
20411 Hingham
20412 Kingston (Mass.)
20413 Marblehead
20414 Marshfield
20415 New Bedford
20416 Newbury
20417 Salem
20418 Salisbury
20419 Salfatudas
20420 Scituate
20421 Swansea
20422 Weymouth
20423 Charlestown
20424 Newburyport
20425 Nantucket
20426 Plymouth
20427 Dartmouth
20428 Somerset
20499 Massachusetts, port unspecified
20501 Bridgeport
20502 Guildford
20503 Haddam
20504 New Haven
20505 New London
20506 Middletown
20507 Hartford
20508 Fairfield
20509 Killingworth
20510 Weathersfield
20599 Connecticut, port unspecified
20601 Brookhaven
20602 Long Island
20603 Sag Harbor
20699 New York
20701 Wilmington (Del.)
20799 Delaware, port unspecified
20801 Delaware River
20802 Eastern New Jersey
20803 Milwell
20804 Perth Amboy
20805 Wharton
20806 Burlington
20807 Newark
20808 Cohanzie, NJ
20899 New Jersey, port unspecified
20901 Marcus Hook
20902 Philadelphia
20999 Pennsylvania, port unspecified
21001 Annapolis
21002 Baltimore
21003 North Potomac
21004 Oxford
21005 Pamunkey
21006 Patuxent
21007 Talbot County
21008 Wye River
21009 Cecil County
21010 Choptank
21011 Pocomoke
21012 St. Mary's River
21013 Londontowne
21099 Maryland, port unspecified
21101 Elizabeth River
21102 Hampton
21103 Lower James River
21104 Norfolk
21105 Northampton County
21106 Potomac river
21107 Rappahannock
21108 South Potomac
21109 Upper James River
21110 Williamsburg
21111 York River
21112 Vatt (location undetermined)
21113 Accomac
21114 Urbanna
21115 Richmond (VA)
21116 Hampton Roads
21199 Virginia, port unspecified
21201 Bertie County
21202 Chowan river
21203 New Bern
21204 Hyde County
21205 Roanoke
21206 Beaufort, NC
21207 Brunswick
21208 Cape Fear
21209 Edenton
21210 Wilmington
21299 North Carolina, port unspecified
21301 Beaufort
21302 Charleston
21304 Georgetown (SC)
21399 South Carolina, port unspecified
21401 Savannah
21402 Sunbury
21403 Tybee Island
21404 Cumberland Island
21405 Jekyll Island
21499 Georgia, port unspecified
21501 St. Augustine
21502 Pensacola
21503 Amelia Island
21504 Key West
21505 Indian River
21506 East Florida
21599 Florida, port unspecified
21601 New Orleans
21602 La Balise
21603 Louisiana, port unspecified
21604 Biloxi
21605 Mississippi, port unspecified
21606 Mobile
21607 Vicksburg (MS)
21699 Gulf Coast, port unspecified
21701 Quebec
21702 New Brunswick
21703 St. John
21704 Newfoundland
21705 Nova Scotia
21706 Halifax
21707 Mahone Bay
21708 Roseway, NS
21801 Smithfield
21802 Louisville
21899 Kentucky, port unspecified
21901 Newport and Boston
21902 Newport and Salem
21903 New England
21904 Carolinas
21905 Chesapeake
21906 Washington, DC
21999 USA, location unspecified
22001 Nashville
22099 Tennessee, port unspecified
22101 Woodville
22199 Texas, port unspecified
31102 Isabela
31103 Monte Christi
31104 Samana
31106 San(to) Domingo
31107 Ocoa
31108 Nizao
31109 Puerto de Plata
31110 Isla Saona
31199 Hispaniola, unspecified
31201 San Juan
31202 Ponce
31299 Puerto Rico, port unspecified
31301 Bahia Honda
31302 Banes
31303 Cabanas
31304 Caibarien
31305 Canasí
31306 Cardenas
31307 Cienfuegos
31308 Cuba, south coast
31309 Cuba, west coast
31310 Estuary of River Guane
31311 Guanimar
31312 Havana
31313 Isla de Pinas
31314 Magari, Manzanillo
31315 Mariel
31316 Matanzas
31317 Matanzas province, north
31318 Matanzas province, south
31319 Nuevitas
31320 Puerto Padre
31321 Sagua
31322 San Juan de los Remedios
31323 Santiago de Cuba
31324 Trinidad de Cuba
31326 Batabanó
31327 Baracoa
31328 Punta Macurijes
31329 South Keys
31330 Puerto Principe
31331 HolguÍn
31399 Cuba, port unspecified
31401 St. Andreas
31499 Spanish Caribbean, unspecified
32110 Curaçao
32120 Aruba
32130 Bonaire
32140 St. Maarten
32141 Cul de Sac (St. Maarten)
32150 St. Eustatius
32151 Saba
32199 Dutch Caribbean, colony unspecified
32201 Guiana
32202 Nova Zeelandia (Wild Coast)
32230 Essequibo
32240 Suriname
32241 Paramaribo
32250 Berbice
32299 Dutch Guianas, colony unspecified
32301 Bijsterpan (location undetermined)
32399 Other Dutch Americas, colony unspecified
33402 Barbuda
33499 Antigua, port unspecified
33699 Nevis, port unspecified
33802 St. Marc, Dominica
33899 Dominica, port unspecified
34202 Speightstown
34299 Barbados, port unspecified
34699 Tobago, port unspecified
35102 Antonia (a) Port Antonio
35103 Black River
35104 Falmouth (Jamaica)
35105 Lucea (a) St. Lucea
35106 Martha Brae
35107 Montego Bay
35108 Morant Bay
35109 Port Maria
35110 Port Royal
35111 Saint Ann's Bay
35112 Savanna la Mar
35113 Spanish Town
35114 Kingston (Jamaica)
35199 Jamaica, port unspecified
35201 New Providence
35202 Turk's Island
35203 Nassau
35204 Cat Island, Bahamas
35205 Exuma
35206 Heneague, Bahamas
35299 Bahamas, port unspecified
35301 Cumingsberg
35302 Stabroek
35304 Demerara
35305 Kingston (Demerera)
35399 British Guiana, port unspecified
35401 Mosquito Shore
35402 Trujillo
35499 Honduras, port unspecified
35502 Antigua or Dominica
35503 Barbados and Jamaica
35504 Dominica or Jamaica
35505 Jamaica or St. Kitts
35507 British Leewards
35508 Bermuda
35509 Hamilton (Bermuda)
35510 Becquia
35511 Grand Caymans
35512 Providence Island
35599 British Caribbean, colony unspecified
36101 Fort-Royale
36102 Saint-Pierre
36199 Martinique, port unspecified
36201 Basse-Terre
36202 Pointe-à-Pitre
36203 Marie-Galante
36204 Grande-Terre, port unspecified
36299 Guadeloupe, port unspecified
36301 Cayenne
36302 Oyapock
36399 Guyane française, port unspecified
36401 Tortuga (a) Saltatudas
36402 Arcahaie
36403 Cap Français
36404 Cayes (Les)
36405 Cul-de-Sac
36406 Fort Dauphin
36407 Jacmel
36408 Jérémie
36409 Léogane
36410 Môle Saint Nicolas
36411 Petit-Goâve
36412 Port-au-Prince
36413 Port-de-Paix
36414 Saint-Marc
36415 Île à Vache
36416 Cap Tiberon
36420 Gonaïve
36499 Saint-Domingue, port unspecified
37020 St. Thomas
37030 St. John (Virgin Islands)
37040 Spanish Town (Virgin Islands)
37099 Danish West Indies, colony unspecified
38101 Gustavia (St. Barthélemy)
38199 St. Barthélemy, port unspecified
39001 Caribbean (colony unspecified)
39002 West Indies (colony unspecified)
39003 Orici (location undetermined)
39004 Bottomaneau (location undetermined)
39005 Marcades (location undetermined)
41201 New Spain
41202 Campeche
41203 Veracruz
41204 Guatemala
41205 Santo Tomas
41206 Portobelo
41207 Cartagena
41208 Santa Marta
41210 Venezuela
41211 Borburata
41212 Caracas
41213 Cumana
41214 La Guaira
41215 Margarita
41216 Orinoko
41217 Rio de la Hacha
41218 Darien
41219 Nombre de Dios
41220 Rattan (a) Roatán
41221 Puerto Cabello
41222 Isla de Aves
41223 Coro
41224 Maracaibo
41225 Nueva Barcelona
41226 Guayana, Venezuela
41227 Caracas Coast and Tucacas
41228 Caye de San Juan
41252 Trujillo
41298 Honduras, port unspecified
41299 Spanish Circum-Caribbean, unspecified
42001 Buenos Aires
42002 Montevideo
42003 Colonia de Sacramento
42004 Maldonado
42005 Rio Negro
42006 Ensenada de Barragán
42007 Punta del Este
42099 Rio de la Plata, port unspecified
43099 Peru, port unspecified
44002 Talcahuano
50103 Pará
50104 Guajano
50105 Rio Amazona
50106 Ceará, port unspecified
50107 Macapá
50108 Belém
50201 Pôrto Seguro
50202 Rio Real
50203 Rio São Francisco
50204 Taipu
50299 Bahia, port unspecified
50301 Alagoas province
50302 Ilha de Itamaric
50303 Maceió
50304 Catuamo and Maria Farinha
50305 Porto de Galinhas
50306 Paraíba
50307 Rio Grande do Norte
50309 Fernando de Noronha
50311 Recife
50399 Pernambuco, port unspecified
50402 Baía de Botafogo
50403 Baía de Sepetiba
50404 Cabo de Búzios
50405 Cabo Frio
50406 Campos
50407 Cananéia
50408 Copacabana
50409 Dois Rios
50410 Ilha de Paquetá
50411 Ilha das Palmas
50412 Ilha Grande
50413 Ilha de Lobes
50414 Ilha de Marambaia
50416 Macaé
50417 Mangaratiba
50418 Maricá
50419 Paranaguá
50420 Parati
50421 Ponta Negro
50422 Rio de Janeiro
50423 Rio de Janeiro province
50424 Rio de Janeiro, Sao Paulo, Santa Catarina
50426 Rio Grande do Sul Province
50427 Rio São Jeso
50428 Santos
50430 São Sebastião
50431 Santa Catarina (a) St. Catherines
50433 Ubatuba
50434 Vitória
50435 Laguna, Santa Catarina
50436 Ilha does Porcos
50437 São Vicente
50438 Espírito Santo
50499 Southeast Brazil, port unspecified
60102 Arguim
60103 Bissagos
60104 Bissau
60105 Cacheu
60106 Casamance
60107 Galam
60108 Gambia
60109 Gorée
60110 James Fort
60111 Joal, or Saloum River
60112 Oibo
60113 Portudal
60114 Portuguese Guinea
60115 Rio Bolola
60116 Rio Bramiah
60117 Rio Grande
60118 Saint-Louis
60119 French Africa (Gorée or Senegal)
60126 Cape Verde Islands
60127 Santa Catarina do Fogo
60128 Madeira
60129 Azores
60130 Senegal
60141 Canary Islands
60142 Tenerife
60199 Senegambia and offshore Atlantic, port unspecified
60201 Banana Islands
60202 Bance Island (Ben's Island)
60203 Ben's Island
60204 Cacandia
60205 Côte de Malaguette (runs through to Cape Palmas on Windward Coast)
60206 Delagoa
60207 Freetown
60208 Gallinhas
60209 Garraway Roads
60210 Iles de Los
60211 Iles Plantain
60212 Rio Nunez
60213 Rio Pongo
60214 Scarcies
60216 Settra Kou
60217 Sherbro
60218 Siraboom
60219 Sugary (Siekere)
60220 Sierra Leone estuary
60241 Mano
60242 River Kissey
60299 Sierra Leone, port unspecified
60301 Barason
60302 Bassa
60303 Batoa
60304 Bercou
60305 Cap des Palmes
60306 Cape Mount (Cape Grand Mount)
60307 Cape Petit Mount
60308 Cape Grand Mount
60309 Cape Lahou
60310 Cess
60311 Grand Cess
60312 Petit Cess
60313 Dembia
60314 Drouin
60315 Grand Bassa
60316 Little Bassa
60317 Grand Bassam
60318 Grand Junk
60319 Little Junk
60320 Mano
60321 Grand Mesurado
60322 Petit Mesurado
60324 Rio Assini
60325 Rio Cavalie
60326 Rio Sinou
60327 River Kissey
60328 Sassandra
60329 Grand Sestos
60330 Rock Sestos
60331 St. Paul
60332 Tabou
60333 Trade Town
60334 Liberia
60335 Ivory Coast
60336 Young Sestos
60337 Little Sestos
60338 Kru Settra
60339 Cape Palmas
60340 Settra Kru
60399 Windward Coast, port unspecified
60402 Danish Gold Coast
60403 Acoda (Akka or Acquida)
60404 Accra
60405 Alampo
60407 Anomabu and Apam
60408 Anomabu
60409 Apammin
60410 Apollonia
60411 Axim
60412 Cape Coast Castle
60413 Chama
60414 Christiansborg
60415 Elmina
60416 Eva
60417 Kormantine
60418 Lagoo
60419 Mouree
60420 Ningo
60421 Rio Volta
60422 Sekondi
60423 Siekere
60424 Tantumquerry
60425 Wiamba
60426 Gold Coast from Kormatine to Accra
60427 Gold Coast east of Kormantine
60428 Pokesoe (Princes Town)
60429 Boutry
60430 Bercou
60431 Takoradi
60499 Gold Coast, port unspecified
60501 Benin
60502 Costa da Mina
60503 Aghway
60504 Agora
60505 Amokou
60506 Ardra
60507 Badagry
60508 Dixcove
60509 Epe
60510 Popo
60511 Little Popo
60512 Grand Popo
60513 Ife
60514 Jacquin
60515 Whydah
60516 Keta
60517 Lagos, Onim
60518 Lamo River
60519 Lay
60520 Legas
60521 Oerê
60522 Pataquery
60523 Porto Novo
60524 Rio Forcados
60525 Rio Nazareth
60526 Rio Nun
60599 Bight of Benin, port unspecified
60601 Andony
60602 Barabalemo
60603 Bilbay
60604 Bimbia
60605 Bonny
60607 Bundy
60608 Calabar
60609 Cameroons
60610 Cameroons River
60611 Cape Baxos
60612 Cap Lopez
60613 Cap Lopez And Cabo Santa Catarina
60614 Cape Lopez and Annobon
60615 Corisco
60616 Corisco and Princes Island
60617 Fockey
60618 Forke
60619 Formosa
60620 Gabon
60621 Liverpool River
60622 New Calabar
60623 Quaqua
60624 River Brass
60625 River del Rey
60626 Timbeys
60627 Rio Nazareth
60628 Calabary
60660 Gulf of Guinea islands
60661 Annobon
60662 Epee and Princes Islands
60663 Fernando Po
60664 Fernando Po and Cameroons
60665 Fernando Po and Princes Islands
60666 Juda and Princes Islands
60667 Juda, Epee, Princes Island, and Annobon
60668 Princes Island
60669 Princes Island and Annobon
60670 Princes Island and Cape Lopez
60671 Princes Island and São Tomé
60672 Principe, Cap Lopez, Annobon
60673 São Tomé
60674 São Tomé and Annobon
60675 São Tomé and Cape Lopez
60676 São Tomé or Princes Island
60677 Costa de Africa (mainland coast around S Tome/Principe)
60699 Bight of Biafra and Gulf of Guinea Islands, port unspecified
60701 Alecuba
60702 Ambona
60703 Ambriz
60704 Ambrizete
60705 Barra de Cariba, Luanda
60706 Bengo
60707 Benguela
60708 Benguela Velho
60709 Boary
60710 Bomara
60711 Cabinda
60712 Cabo Lopo Gonçalves
60713 Cape Mole
60714 Cape Padroon
60715 Coanza River
60716 Congo North
60717 Congo River
60718 Rio Zaire
60719 Grenada Point
60720 Iron Point
60721 Kacongo
60722 Kilongo
60723 Liverpool River
60724 Loango
60725 Malembo
60726 Mayumba
60727 Metrueba
60728 Mossulo, Luanda
60729 Mpinda
60730 Nova Redonda
60731 Penido
60732 Quicombo
60733 Salinas
60734 Luanda
60735 Rio Dande (N of Luanda)
60740 St. Helena
60741 Soyo
60742 Ponta da Lenha
60799 West Central Africa and St. Helena, port unspecified
60801 Anghoza River
60802 Cabo Delgado
60803 Cape of Good Hope
60805 Costa Leste Occidental
60806 Ibo
60807 Inhambane
60808 Quirimba
60809 Kilwa
60810 Lourenço Marques
60811 Madagascar
60812 Mauratan
60813 Morondova
60814 Port Dauphin
60815 St. Mary's Island
60816 Tulia
60817 Mocambo
60819 Momboza or Zanzibar
60820 Mozambique
60821 Pomba, Monfia
60822 Quilimane
60823 Sofala
60824 St. Lawrence
60826 Zanzibar
60831 Mascarene Islands
60832 Bourbon
60833 Ile de France
60834 Port-Louis
60836 Johanna Island
60837 Mauritius (Ile de France)
60838 Ascension, Assomption, and Bourbon
60899 Southeast Africa and Indian Ocean islands, port unspecified
60901 Princes Island and Elmina
60902 Angola (possibly New Calabar)
60903 Senegambia or Sierra Leone
60908 West of Cape Apolonia
60909 Angola to Ardra
60912 Windward + Ivory + Gold + Benin
60913 C.C. Castle and Windward
60914 Gold Coast + Bight of Benin + Bight of Biafra
60915 Bights
60916 Windward Coast (Nunez-Assini)
60917 Gold Coast, Fr. definition
60981 Causacoo (location undetermined)
60982 Cape St Martha (location undetermined)
60983 Casnasonis (locationundetermined)
60984 Massa (location undetermined)
60985 Touau-Toro (location undetermined)
60986 Cape Logas (location undetermined)
60999 Africa., port unspecified
80201 Guecka, location undetermined
80202 Rocus, location undetermined
80203 Kalas, location undetermined
80204 Imfors Bay, location undetermined
80205 Marrader (Morada?), location undetermined
80499 Spanish Americas, port unspecified
80501 Kingston, Savannah, Maryland
80599 British Americas, port unspecified
80601 Bengal
80602 Goa
80603 Calcutta
80604 Basse Indres
80703 British plantations
80704 Non-British
80705 Captured by WIC
80706 Captured interloper
80707 Prize (unknown place)
80708 Prize (taken from Dutch)
80709 Prize (taken from French)
80710 Prize (taken from Spanish)
80711 Sold at sea to another vessel
80712 Wreck
80713 Slaves rescued at sea from a shipwreck
80714 Prize (taken from Portuguese)
80715 Prize (taken from USA)
80716 Taken at sea from pirates
80717 Seized at sea by Spanish officials
80799 Other non-geographical" %>% 
  gsub("(?<=\\d) ", "\t", ., perl = T) %>%
  read_tsv(., 
           col_names = c("place_value", "place (port or location)")) %>%
  clean_names() %>% 
  mutate(., 
       broad_region_value = place_value %/% 10000 * 10000, 
       specific_region_value = place_value %/% 100 * 100)}
cw_broad.regions <- {"10000 Europe
20000 Mainland North America
30000 Caribbean
40000 Spanish American Mainland
50000 Brazil
60000 Africa
80000 Other" %>%  
  gsub("(?<=\\d) ", "\t", ., perl = T) %>%
  read_tsv(., 
           col_names = c("broad_region_value", "broad_region"))}
cw_specific_regions <- {"10100 Spain
10200 Portugal
10300 Great Britain
10400 England
10500 Scotland
10600 Ireland
10700 France
10800 Netherlands
10900 Denmark
11000 Northern Germany
11100 Belgium
11200 Norway
11300 Sweden
11400 Italy
11500 Russia
11600 Mediterranean
11700 Latvia
20100 Rhode Island
20200 Maine
20300 New Hampshire
20400 Massachusetts
20500 Connecticut
20600 New York
20700 Delaware
20800 New Jersey
20900 Pennsylvania
21000 Maryland
21100 Virginia
21200 North Carolina
21300 South Carolina
21400 Georgia
21500 Florida
21600 Gulf coast
21700 Canada
21800 Kentucky
21900 Other North America
22000 Tennessee
22100 Texas
31100 Hispaniola
31200 Puerto Rico
31300 Cuba
31400 Other Spanish Caribbean
32100 Dutch Caribbean
32200 Dutch Guianas
32300 Other Dutch Americas
33200 Tortola
33300 Anguilla
33400 Antigua
33500 St. Kitts
33600 Nevis
33700 Montserrat
33800 Dominica
34100 St. Lucia
34200 Barbados
34300 St. Vincent
34400 Grenada
34500 Trinidad
34600 Tobago
35100 Jamaica
35200 Bahamas
35300 British Guiana
35400 British Honduras
35500 Other British Caribbean
36100 Martinique
36200 Guadeloupe
36300 French Guiana
36400 Saint-Domingue
36500 Other French Caribbean
37000 Danish West Indies
38100 St. Barthélemy (Sweden)
39000 Other Caribbean
41200 Spanish Circum-Caribbean
42000 Rio de la Plata
43000 Peru
44000 Chile
45000 Ecuador
50100 Amazonia
50200 Bahia
50300 Pernambuco
50400 Southeast Brazil
50500 Other Brazil
60100 Senegambia and off shore Atlantic
60200 Sierra Leone
60300 Windward Coast
60400 Gold Coast
60500 Bight of Benin
60600 Bight of Biafra and Gulf of Guinea islands
60700 West Central Africa and St. Helena
60800 Southeast Africa and Indian Ocean islands
60900 Other Africa
80100 East Indies
80200 Americas
80300 Asia e Africa
80400 Spanish Americas
80500 British Americas
80600 India
80700 Non-geographical" %>%  
  gsub("(?<=\\d) ", "\t", ., perl = T) %>%
  read_tsv(., 
           col_names = c("specific_region_value", "specific_region_country_or_colony"))}
cw_specific_places2 <- {"80000 Other 80300 Asia e Africa 80399 Asia e Africa, port unspecified
80000 Other 80400 Spanish Americas 80401 Spanish American Mainland, port unspecified
80000 Other 80100 East Indies 80199 East Indies, port unspecified
80000 Other 80200 Americas 80299 Americas, port unspecified
50000 Brazil 50500 Other Brazil 50599 Brazil, region unspecified
60000 Africa 60100 Senegambia and offshore Atlantic 60101 Albreda
40000 Spanish Mainland Americas 45000 Ecuador 45099 Ecuador, port unspecified
50000 Brazil 50100 Amazonia 50102 Maranhão
40000 Spanish Mainland Americas 44000 Chile 44001 Valparaiso
40000 Spanish Mainland Americas 43000 Peru 43001 Lima
30000 Caribbean 36500 Other French Caribbean 36599 French Caribbean, colony unspecified
30000 Caribbean 37000 Danish West Indies 37010 St. Croix
30000 Caribbean 35100 Jamaica 35101 Annotto Bay
30000 Caribbean 34300 St. Vincent 34399 St. Vincent, port unspecified
30000 Caribbean 34400 Grenada 34499 Grenada, port unspecified
30000 Caribbean 34500 Trinidad 34599 Trinidad, port unspecified
30000 Caribbean 34600 Tobago 34601 Zion Hill
30000 Caribbean 34100 St. Lucia 34199 St. Lucia, port unspecified
30000 Caribbean 34200 Barbados 34201 King's Harbour
30000 Caribbean 33700 Montserrat 33799 Montserrat, port unspecified
30000 Caribbean 33800 Dominica 33801 Roseau
30000 Caribbean 33500 St. Kitts 33599 St. Kitts, port unspecified
30000 Caribbean 33600 Nevis 33601 Newcastle (Nevis)
30000 Caribbean 33200 Tortola 33299 Tortola, port unspecified
30000 Caribbean 33300 Anguilla 33399 Anguilla, port unspecified
30000 Caribbean 33400 Antigua 33401 Saint John (Antigua)" %>% 
  gsub("(?<=\\d) | (?=\\d)", "\t", ., perl = T) %>% 
  read_tsv(., col_names = c("broad_region_value", 
                            "broad_region", 
                            "specific_region_value", 
                            "specific_region (country or colony)", 
                            "place value", "place (port or location)")) %>% 
  clean_names()}

cw_specific_places <- left_join(cw_specific_places, cw_broad.regions) %>%
  left_join(cw_specific_regions) %>%
  rbind(., cw_specific_places2)

# write to cw----
setwd(wd$cw)
write_csv(x = cw_specific_places, 
          file = "cw_specific_places.csv")
setwd(wd$data)

rm(cw_specific_places2, cw_specific_regions, cw_broad.regions)


# explore data----
