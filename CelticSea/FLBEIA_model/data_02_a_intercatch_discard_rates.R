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

# 00 - Single species advice sheet values ####
# All intercatch canum should total to these values. 
# Notes available in each section to describe any differences. 
stock_list <- c("cod.27.7e-k", "had.27.7b-k" ,"whg.27.7b-ce-k" , "sol.27.7fg"  , "meg.27.7b-k8abd", "mon.27.78abd" ,"hke.27.3a46-8abd")

#Raised total of catchs, landings and discards, used in assessent, taken form advice sheet
advice_key <- icesSAG::findAssessmentKey( stock = stock_list, year = 2020,  published = TRUE)
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
intercatch_caton[intercatch_caton == 'sol.27.7fg',] %>% select(DataYear,CatchCat, Weight_Total_in_kg ) %>% group_by(DataYear,CatchCat) %>% summarise(Weight_Total_in_kg  = (sum(Weight_Total_in_kg, na.rm=T)/1000))
intercatch_caton[intercatch_caton == 'sol.27.7fg',] %>% select(DataYear,CatchCat, Weight_Total_in_kg ) %>% group_by(DataYear,CatchCat) %>% summarise(Weight_Total_in_kg  = (sum(Weight_Total_in_kg, na.rm=T)/1000))

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

# ~ M?tier level 4 fix ####
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

# ~Compare with advice sheet values for last 3 years ####
advice_sheet_values[advice_sheet_values$stock %in% c("meg.27.7b-k8abd") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("meg.27.7b-k8abd") & intercatch_caton$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - meg.27.7b-k8abd, there is no 2017 in interCatch caton

advice_sheet_values[advice_sheet_values$stock %in% c("sol.27.7fg") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("sol.27.7fg") & intercatch_caton$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - sol.27.7fg there is a good match

advice_sheet_values[advice_sheet_values$stock %in% c("hke.27.3a46-8abd") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("hke.27.3a46-8abd") & intercatch_caton$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - hke.27.3a46-8abd poor match for landings in last 2 years

advice_sheet_values[advice_sheet_values$stock %in% c("mon.27.78abd") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_caton[intercatch_caton$Stock %in% c("mon.27.78abd") & intercatch_caton$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, CatchCat, CATON_in_kg) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON_in_kg, na.rm=T)/1000) %>% data.frame()
# Notes - hke.27.3a46-8abd poor match for landings in last 2 years



# ~ Convert to tonnes ####
intercatch_caton$CATON <- intercatch_caton$CATON_in_kg/1000

# ~ Calculating discard rates #### 
intercatch_caton<- intercatch_caton %>% select(Year,Stock, Country,Area,lvl4,CatchCat,CATON_in_kg) %>%
  group_by(Year,Stock,Country,Area,lvl4,CatchCat) %>%  dplyr::summarise(CATON_in_kg=sum(CATON_in_kg, na.rm=TRUE))%>% 
  tidyr::spread(CatchCat,CATON_in_kg)
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
advice_sheet_values[advice_sheet_values$stock %in% c("cod.27.7e-k") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
caton_other[caton_other$Stock %in% c("cod.27.7e-k") & caton_other$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, Landings, Discards) %>% gather(CatchCat, CATON, 3:4) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON, na.rm=T)) %>% data.frame()
# Notes - cod.27.7e-k, good match and already in tonnes

advice_sheet_values[advice_sheet_values$stock %in% c("had.27.7b-k") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
caton_other[caton_other$Stock %in% c("had.27.7b-k") & caton_other$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, Landings, Discards) %>% gather(CatchCat, CATON, 3:4) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON, na.rm=T)) %>% data.frame()
# Notes - had.27.7b-k good match, already in tonnes

advice_sheet_values[advice_sheet_values$stock %in% c("whg.27.7b-ce-k") & advice_sheet_values$year %in% c(2017, 2018, 2019),] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
caton_other[caton_other$Stock %in% c("whg.27.7b-ce-k") & caton_other$Year %in% c(2017, 2018, 2019),] %>% select( Year,Stock, Landings, Discards) %>% gather(CatchCat, CATON, 3:4) %>% group_by(Year,Stock, CatchCat) %>% summarise(caton = sum(CATON, na.rm=T)) %>% data.frame()
# Notes - whg.27.7b-ce-k very poor match but ok, already in tonnes, this difference is due to the inclusion of rectangles from the south of 7a. Tbale in advice sheet assessmen summary was wrong

# 04 - Merge ####
caton_summary<-rbind(intercatch_caton,caton_other)
caton_summary <- caton_summary[caton_summary$Catch >0,]

# 05 - Summary of missing discard rates ####
# Summary of missing 

# ~ Check for no discard data ---------------------------------------------

# CM this chunk of code is removing landings that we need!
# table(is.na(caton_summary$Discards))
# ##note the alternative way to deal with this is is to assign 0 to the NA values and 0 to the DR rate for NA
# ## however NA typically implies no data not a zero discard rate so there could be a major assumtion there that 
# ## is wrong
# NA_NaN_IC_DR <- caton_summary %>% filter(is.nan(DR)==T | is.na(Discards)==T) 
# caton_summary_fin <- caton_summary %>% filter(is.na(Discards)==F , is.nan(DR)==F) 
# dim(caton_summary)[1]-(dim(caton_summary_fin)[1]+dim(NA_NaN_IC_DR)[1])

write.taf(caton_summary,"results/clean_data/caton_summary.csv")

