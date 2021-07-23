# Data prep
# Preprocess data,

## Before: InterCatch extraction, data raised external to InterCatch, and ALK's
## After:  Discards rates 

# Data sources vary per stock: 
#       - InterCatch CATON with distribution: meg.27.7b-k8abd, sol.27.7fg, hke.27.3a46-8abd
#       - InterCatch CATON without distribution: mon.27.78abd 
#       - Raised outside InterCatch: cod.27.7e-k, had.27.7b-k, whg.27.7b-ce-k, 
#       - Supplied by NEP expert: nep.fu.16, nep.fu.17, nep.fu.19, nep.fu.2021, nep.fu.22, nep.out.7
# Notes: User must make sure that: factors are turned to characters and dim of data frame does not change shape during cleaning process

gc();rm(list = ls())
library(readxl)
library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)
library(icesSAG)

### not gonna edit and entire scritp everytime we want to update the years
YEARS <- c(2017:2019)

# 00 - Single species advice sheet values ####
# All intercatch canum should total to these values. 
# Notes available in each section to describe any differences. 
stock_list <- c("cod.27.7e-k", "had.27.7b-k" ,"whg.27.7b-ce-k" , "sol.27.7fg"  , "meg.27.7b-k8abd", "mon.27.78abd" ,"hke.27.3a46-8abd")

#Raised total of catchs, landings and discards, used in assessent, taken form advice sheet
advice_key <- icesSAG::findAssessmentKey( stock = stock_list, year = (max(YEARS)+1),  published = TRUE)
advice_sheet <- icesSAG::getSummaryTable(advice_key)
cod_advice <- as.data.frame(advice_sheet[1]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
had_advice <- as.data.frame(advice_sheet[2]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
whg_advice <- as.data.frame(advice_sheet[3]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
sol_advice <- as.data.frame(advice_sheet[4]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
meg_advice <- as.data.frame(advice_sheet[5]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
mon_advice <- as.data.frame(advice_sheet[6]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
hke_advice <- as.data.frame(advice_sheet[7]) %>% select(fishstock, Year, stockSizeUnits, landings, discards, catches) %>%gather(catch_cat, total, 4:6)
advice_sheet_values <- rbind(cod_advice, had_advice, whg_advice, sol_advice, meg_advice, mon_advice, hke_advice)
advice_sheet_values$source <- "advice sheet"
advice_sheet_values$catch_cat[advice_sheet_values$catch_cat == "catches"] <- "catch"
advice_sheet_values <- advice_sheet_values %>% select(Year, fishstock, source, stockSizeUnits,  catch_cat, total)
names(advice_sheet_values) <- c("year", "stock", "source",  "unit","catch_cat", "total")

# 01 - InterCatch CATON ####
# with distributions 
taf.unzip("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON Stocks with distributions all WG 2002  2019.zip", files="2020 06 22 WGMIXFISH CATON stocks with distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
#NB gitignore this file as it is too big
intercatch_caton <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")

# without distributions 
taf.unzip("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON stocks withOUT distributions all WG 2002 2019.zip", files="2020 06 22 WGMIXFISH CATON stocks withOUT distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
#NB gitignore this file as it is too big
intercatch_caton_no_dist <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON stocks withOUT distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
#This is wher eteh issue is Claire!
names(intercatch_caton_no_dist) <- names(intercatch_caton)
intercatch_caton <- rbind(intercatch_caton_no_dist, intercatch_caton)
#subset by stocks
intercatch_caton <- intercatch_caton %>% filter(Stock %in% c( "hke.27.3a46-8abd","meg.27.7b-k8abd", "mon.27.78abd", "sol.27.7fg"))

intercatch_caton_saftey_check <- intercatch_caton #save for sanity checking later

# ~ Area fix ####
area_spp_fix <- read.csv("bootstrap/data/supporting_files/Area_lookup.csv")
names(area_spp_fix) <- c("Area" ,"Standard","ICES_mix_correct", "ICES_FU","species_mix_FU" )
intercatch_caton <- left_join(intercatch_caton,area_spp_fix, by = "Area" )
dim(intercatch_caton)[1]-dim(intercatch_caton_saftey_check)[1] #safety check - dims should match
intercatch_caton$Area <- intercatch_caton$ICES_mix_correct
intercatch_caton <- intercatch_caton[-c(13,14,15,16)]

# ~ Country fix  ####
Country_Lookup <- read_xlsx("bootstrap/data/supporting_files/Country_lookup.xlsx")
intercatch_caton <- left_join(intercatch_caton,Country_Lookup)
dim(intercatch_caton)[1]-dim(intercatch_caton_saftey_check)[1] #safety check - dims should match
intercatch_caton$Country <- intercatch_caton$CorrectCountry
intercatch_caton <- intercatch_caton[-c(13)]

# ~ Species fix #### 
intercatch_caton$Species <- toupper(substr(intercatch_caton$Stock,1,3))

# ~ Metier level 4 fix ####
intercatch_caton$lvl4 <- substr(intercatch_caton$fleet,1,7)
lvl4_Lookup <- read_xlsx("bootstrap/data/supporting_files/Metier_lvl4_lookup.xlsx")
intercatch_caton <- left_join(intercatch_caton,lvl4_Lookup)
dim(intercatch_caton)[1]-dim(intercatch_caton_saftey_check)[1] #safety check - dims should match
intercatch_caton$lvl4_new <- ifelse(is.na(intercatch_caton$Correct_lvl4),intercatch_caton$lvl4, intercatch_caton$Correct_lvl4)
intercatch_caton$lvl4 <- intercatch_caton$lvl4_new 
intercatch_caton <- intercatch_caton[-c(15,16,17)]

# ~ Remove unwanted data ####  
intercatch_caton <-intercatch_caton [!intercatch_caton$CatchCat %in% c("BMS landing", "Logbook Registered Discard"),]
intercatch_caton<- intercatch_caton%>% select("DataYear" ,"Stock" ,"Country" ,"fleet" ,"CatchCat","Weight_Total_in_kg","lvl4", "Area","Species")
names(intercatch_caton) <-  c("Year", "Stock","Country" ,"Fleet" , "CatchCat", "CATON_in_kg", "lvl4", "Area" , "Species")

# ~ Convert to tonnes ####
intercatch_caton$CATON <- intercatch_caton$CATON_in_kg/1000

# ~ Split out stocks, compare with advice sheet and edit

intercatch_caton_summed <- intercatch_caton %>% filter(Year %in% YEARS) %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(IC_Landings = round(sum(CATON_in_kg, na.rm=T)/1000,2)) %>% data.frame()
#
intercatch_caton_summed$CatchCat <- tolower(intercatch_caton_summed$CatchCat)
#
advice_sheet_values_summed <-advice_sheet_values %>% filter(year %in% YEARS)  %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(Advice_catch = sum(total, na.rm=T))
names(advice_sheet_values_summed) <- c("Year","Stock", "CatchCat", "Advice_catch")

IC_AC <- full_join(advice_sheet_values_summed,intercatch_caton_summed)
dim(advice_sheet_values_summed)[1]-dim(IC_AC)[1]
##### if the dims do not match then there is unmatched data and you need to check
####  Look at the differnaces 
IC_AC <- IC_AC %>% mutate(Differance = Advice_catch-IC_Landings )
####
ggplot(filter(IC_AC,is.na(Differance)==F), aes(Year, Differance, group==CatchCat,fill = CatchCat)) + geom_bar(stat="identity", position="dodge") +facet_wrap(~Stock)+ scale_y_continuous(labels = scales::comma) +theme_classic()

# ~~ sol.27.7fg ####
advice_sheet_values[advice_sheet_values$stock %in% c("sol.27.7fg") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("sol.27.7fg") & intercatch_caton$Year %in% YEARS,] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - sol.27.7fg there is a good match
intercatch_caton_sol <- intercatch_caton[intercatch_caton$Stock == "sol.27.7fg" & intercatch_caton$Year %in% YEARS,]

# ~~ meg.27.7b-k8abd ####
advice_sheet_values[advice_sheet_values$stock %in% c("meg.27.7b-k8abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("meg.27.7b-k8abd") & intercatch_caton$Year %in% YEARS,] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - meg.27.7b-k8abd, there is no 2017 in interCatch caton, also in this years intercatch file
intercatch_caton_meg <- intercatch_caton[intercatch_caton$Stock == "meg.27.7b-k8abd" & intercatch_caton$Year %in% YEARS,]
intercatch_caton_meg <- intercatch_caton_meg[-c(11,12)]

# ~~ hke.27.3a46-8abd ####
advice_sheet_values[advice_sheet_values$stock %in% c("hke.27.3a46-8abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("hke.27.3a46-8abd") & intercatch_caton$Year %in% YEARS,] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - hke.27.3a46-8abd poor match for landings in last 2 years, and as you can see form plot below there is no problem with units
# Difference is minor but still needs to be raised to the total level of the fleet
ggplot(intercatch_caton[intercatch_caton$Stock %in% c("hke.27.3a46-8abd") & intercatch_caton$Year %in% YEARS,], aes(Year, CATON_in_kg/1000, colour = CatchCat)) + geom_bar(stat="identity") +facet_wrap(~Country)+ scale_y_continuous(labels = scales::comma) +theme_classic()
intercatch_caton_hke <- intercatch_caton[intercatch_caton$Stock == "hke.27.3a46-8abd"& intercatch_caton$Year %in% YEARS,]

hke_landings_2017 <- 104671
hke_landings_2018 <- 89671
hke_landings_2019 <- 82298

hke_discards_2017 <- 7386
hke_discards_2018 <- 7034
hke_discards_2019 <- 4940

intercatch_caton_hke$prop <- NaN
intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Discards"] <- intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Discards"]/ sum(intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Discards"], na.rm=T)
intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Discards"] <- intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Discards"]/ sum(intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Discards"], na.rm=T)
intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Discards"] <- intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[3]& intercatch_caton_hke$CatchCat == "Discards"]/ sum(intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Discards"], na.rm=T)

intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Landings"] <- intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Landings"]/ sum(intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Landings"], na.rm=T)
intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Landings"] <- intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Landings"]/ sum(intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Landings"], na.rm=T)
intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Landings"] <- intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[3]& intercatch_caton_hke$CatchCat == "Landings"]/ sum(intercatch_caton_hke$CATON[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Landings"], na.rm=T)

intercatch_caton_hke$CATON_new <- NaN
intercatch_caton_hke$CATON_new[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Discards"] <- intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Discards"]* hke_discards_2017
intercatch_caton_hke$CATON_new[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Discards"] <- intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Discards"]* hke_discards_2018
intercatch_caton_hke$CATON_new[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Discards"] <- intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Discards"]* hke_discards_2019

intercatch_caton_hke$CATON_new[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Landings"] <- intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[1] & intercatch_caton_hke$CatchCat == "Landings"]* hke_landings_2017
intercatch_caton_hke$CATON_new[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Landings"] <- intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[2] & intercatch_caton_hke$CatchCat == "Landings"]* hke_landings_2018
intercatch_caton_hke$CATON_new[intercatch_caton_hke$Year == YEARS[3] & intercatch_caton_hke$CatchCat == "Landings"] <- intercatch_caton_hke$prop[intercatch_caton_hke$Year == YEARS[3]& intercatch_caton_hke$CatchCat == "Landings"]* hke_landings_2019

advice_sheet_values[advice_sheet_values$stock %in% c("hke.27.3a46-8abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton_hke %>% select( Year,Stock, CatchCat, CATON_new) %>% group_by(Year,Stock, CatchCat) %>% summarise(CATON_new = sum(CATON_new, na.rm=T)) %>% data.frame()
# Notes - hke.27.3a46-8abd, now we have a match
intercatch_caton_hke$CATON <- intercatch_caton_hke$CATON_new
intercatch_caton_hke <- intercatch_caton_hke[-c(11,12)]

# ~~ mon.27.78abd ####
advice_sheet_values[advice_sheet_values$stock %in% c("mon.27.78abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("mon.27.78abd") & intercatch_caton$Year %in% YEARS,] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - mon.27.78abd very poor match for landings! There is no unit difference, just some general cumulative differences
ggplot(intercatch_caton[intercatch_caton$Stock %in% c("mon.27.78abd")& intercatch_caton$Year %in% YEARS,], aes(Year, CATON_in_kg/1000, colour = CatchCat)) + geom_bar(stat="identity") +facet_wrap(~Country)+ scale_y_continuous(labels = scales::comma) +theme_classic()

intercatch_caton[intercatch_caton$Stock %in% c("mon.27.78abd" ) & intercatch_caton$Year %in% YEARS & intercatch_caton$Country == "FRA" ,] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
intercatch_caton_mon <- intercatch_caton[intercatch_caton$Stock == "mon.27.78abd"& intercatch_caton$Year %in% YEARS,]

mon_landings_2017 <- 25634
mon_landings_2018 <- 22345
mon_landings_2019 <- 21266
mon_discards_2017 <- 2175
mon_discards_2018 <- 1250
mon_discards_2019 <- 1444

intercatch_caton_mon$prop <- NaN
intercatch_caton_mon$prop[ intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Discards"] <- intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Discards"]/ sum(intercatch_caton_mon$CATON[ intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Discards"], na.rm=T)
intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Discards"] <- intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Discards"]/ sum(intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Discards"], na.rm=T)
intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Discards"] <- intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[3]& intercatch_caton_mon$CatchCat == "Discards"]/ sum(intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Discards"], na.rm=T)

intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Landings"] <- intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Landings"]/ sum(intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Landings"], na.rm=T)
intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Landings"] <- intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Landings"]/ sum(intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Landings"], na.rm=T)
intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Landings"] <- intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Landings"]/ sum(intercatch_caton_mon$CATON[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Landings"], na.rm=T)

intercatch_caton_mon$CATON_new <- NaN
intercatch_caton_mon$CATON_new[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Discards"] <- intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Discards"]* mon_discards_2017
intercatch_caton_mon$CATON_new[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Discards"] <- intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Discards"]* mon_discards_2018
intercatch_caton_mon$CATON_new[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Discards"] <- intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Discards"]* mon_discards_2019

intercatch_caton_mon$CATON_new[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Landings"] <- intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[1] & intercatch_caton_mon$CatchCat == "Landings"]* mon_landings_2017
intercatch_caton_mon$CATON_new[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Landings"] <- intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[2] & intercatch_caton_mon$CatchCat == "Landings"]* mon_landings_2018
intercatch_caton_mon$CATON_new[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Landings"] <- intercatch_caton_mon$prop[intercatch_caton_mon$Year == YEARS[3] & intercatch_caton_mon$CatchCat == "Landings"]* mon_landings_2019

advice_sheet_values[advice_sheet_values$stock %in% c("mon.27.78abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton_mon %>% select( Year,Stock, CatchCat, CATON) %>% group_by(Year,Stock, CatchCat) %>% summarise(CATON = sum(CATON, na.rm=T)) %>% data.frame()
intercatch_caton_mon %>% select( Year,Stock, CatchCat, CATON_new) %>% group_by(Year,Stock, CatchCat) %>% summarise(CATON_new = sum(CATON_new, na.rm=T)) %>% data.frame()
# Notes - mon.27.78abd, now we have a match
intercatch_caton_mon$CATON <- intercatch_caton_mon$CATON_new
intercatch_caton_mon <- intercatch_caton_mon[-c(11,12)]

intercatch_caton <- rbind(intercatch_caton_sol, intercatch_caton_meg, intercatch_caton_hke, intercatch_caton_mon) 
intercatch_caton <- intercatch_caton %>% select(Year, Stock, Country, Area ,lvl4 , CatchCat, CATON) %>% group_by(Year, Stock, Country, Area ,lvl4 , CatchCat) %>% summarise(CATON = sum(CATON, na.rm = T)) %>% data.frame()

# ~ Calculating discard rates #### 
intercatch_caton<- intercatch_caton %>% select(Year,Stock, Country,Area,lvl4,CatchCat,CATON) %>%
  group_by(Year,Stock,Country,Area,lvl4,CatchCat) %>%  dplyr::summarise(CATON=sum(CATON, na.rm=TRUE))%>% 
  tidyr::spread(CatchCat,CATON)
intercatch_caton$Catch<-rowSums(intercatch_caton[c("Discards","Landings")],na.rm=T)
intercatch_caton$DR<-intercatch_caton$Discards/intercatch_caton$Catch

# 02 - WGCSE CATON raised outside InterCatch ####
caton_cod <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_COD_summary.csv")
caton_had <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_HAD_summary.csv")
caton_whg <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_WHG_summary.csv")

# ~ Fix and merge ####
caton_cod$Stock<-"cod.27.7e-k"
caton_had$Stock<-"had.27.7b-k"
caton_whg$Stock<-"whg.27.7b-ce-k"
caton_other<-rbind(caton_cod,caton_had,caton_whg)
caton_other_saftey_check <- caton_other #save for saftey checks later
names(caton_other)<-c("Year","Country","Area","lvl4","Landings","Discards","Stock")
caton_other$lvl4<-substr(caton_other$lvl4,1,7)

# ~ Country fix ####
caton_other <- left_join(caton_other,Country_Lookup)
dim(caton_other)[1]-dim(caton_other_saftey_check)[1] #safety check - dims should match
caton_other$Country <- caton_other$CorrectCountry
caton_other <- caton_other[-8]

# ~ Calculating discard rates #### 
caton_other$Catch <- rowSums(caton_other[c("Discards","Landings")],na.rm=T)
caton_other$DR <- caton_other$Discards/caton_other$Catch
caton_other <- caton_other %>% select(Year, Stock, Country, Area, lvl4, Discards, Landings, Catch, DR)

# ~Compare with advice sheet values for last 3 years ####
advice_sheet_values[advice_sheet_values$stock %in% c("cod.27.7e-k") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
caton_other[caton_other$Stock %in% c("cod.27.7e-k") & caton_other$Year %in% YEARS,] %>% select( Year,Stock, Landings, Discards) %>% gather(CatchCat, CATON, 3:4) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON, na.rm=T)) %>% data.frame()
# Notes - cod.27.7e-k, good match and already in tonnes

advice_sheet_values[advice_sheet_values$stock %in% c("had.27.7b-k") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
caton_other[caton_other$Stock %in% c("had.27.7b-k") & caton_other$Year %in% YEARS,] %>% select( Year,Stock, Landings, Discards) %>% gather(CatchCat, CATON, 3:4) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON, na.rm=T)) %>% data.frame()
# Notes - had.27.7b-k good match, already in tonnes

advice_sheet_values[advice_sheet_values$stock %in% c("whg.27.7b-ce-k") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
caton_other[caton_other$Stock %in% c("whg.27.7b-ce-k") & caton_other$Year %in% YEARS,] %>% select( Year,Stock, Landings, Discards) %>% gather(CatchCat, CATON, 3:4) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON, na.rm=T)) %>% data.frame()
# Notes - whg.27.7b-ce-k very poor match but ok, already in tonnes, this difference is due to the inclusion of rectangles from the south of 7a. Tbale in advice sheet assessmen summary was wrong

# 04 - Merge ####
caton_summary<-rbind(intercatch_caton,caton_other)
caton_summary <- caton_summary[caton_summary$Catch >0,] %>% data.frame()

# 05 - Summary of discard rates ####
# we need to understand teh implications of missing discard rates

caton_summary$rate <- NaN
caton_summary$rate[is.na(caton_summary$DR)]  <- "No rate" 
caton_summary$rate[!is.na(caton_summary$DR)]  <- "Rate available" 

ggplot(caton_summary, aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Stock) + theme_classic()
ggplot(caton_summary[caton_summary$Stock == "hke.27.3a46-8abd",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Year) + theme_classic()
ggplot(caton_summary[caton_summary$Stock == "hke.27.3a46-8abd",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Area) + theme_classic()
# The lack of discard rates in 7j and 6a is very worrying, I wonder are these fleets raised in intercatch at all? 
ggplot(caton_summary[caton_summary$Stock == "hke.27.3a46-8abd" & caton_summary$Area == "27.7.j",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()
# The issue is centered around FRA and ES - we need to email data submitters and find out why - also need to ask stock assessor if we can assume same discard rate in 7j as in 8.
ggplot(caton_summary[caton_summary$Stock == "hke.27.3a46-8abd" & caton_summary$Country == "FRA",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Area) + theme_classic()
ggplot(caton_summary[caton_summary$Stock == "hke.27.3a46-8abd" & caton_summary$Country == "ES",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Area) + theme_classic()

ggplot(caton_summary[caton_summary$Stock == "mon.27.78abd",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Year) + theme_classic()
ggplot(caton_summary[caton_summary$Stock == "mon.27.78abd",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Area) + theme_classic()
# The lack of discard rates in 7j  is very worrying, I wonder are these fleets raised in intercatch at all? 
ggplot(caton_summary[caton_summary$Stock == "mon.27.78abd" & caton_summary$Area == "27.7.j",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()


ggplot(caton_summary[caton_summary$Stock == "meg.27.7b-k8abd",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Year) + theme_classic()
ggplot(caton_summary[caton_summary$Stock == "meg.27.7b-k8abd",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Area) + theme_classic()
# The lack of discard rates in 7j  is very worrying, I wonder are these fleets raised in intercatch at all? 
ggplot(caton_summary[caton_summary$Stock == "meg.27.7b-k8abd" & caton_summary$Area == "27.7.j",], aes(rate, Landings)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()

# Missing Discard rates 
# CM - I think we should set to zero, as the total discards have been supplied for what Member states could sample, and what stock coordinators could fill in
# I am not happy with the quality/consistency in data available through WGBIE
# Perhaps I should have raised landings to the level of advice sheet and just added discards by replicating the discard rate where possible 

# CM - I have blocked out this code as this chunk of code is removing landings that we need!
# table(is.na(caton_summary$Discards))
# ##note the alternative way to deal with this is is to assign 0 to the NA values and 0 to the DR rate for NA
# ## however NA typically implies no data not a zero discard rate so there could be a major assumtion there that 
# ## is wrong
# NA_NaN_IC_DR <- caton_summary %>% filter(is.nan(DR)==T | is.na(Discards)==T) 
# caton_summary_fin <- caton_summary %>% filter(is.na(Discards)==F , is.nan(DR)==F) 
# dim(caton_summary)[1]-(dim(caton_summary_fin)[1]+dim(NA_NaN_IC_DR)[1])

write.taf(caton_summary,"results/clean_data/caton_summary.csv")

