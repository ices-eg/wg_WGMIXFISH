# Data prep
# Preprocess data,

## Before: InterCatch extraction, data raised external to InterCatch, and ALK's
## After:  Age structure from InterCatch 

# Notes: User must make sure that:  
# 1 - factors are turned to characters, 
# 2 - dim of data frame does not change shape during cleaning process
# 3 - all data sources should be converted to tonnes
# 4 - subset data by stock and not area so that raising to advice sheet values can be achieved
# 5 - SOP corrections should eb applied in this file.

# Data sources vary per stock: 
#       - InterCatch CANUM with distribution: meg.27.7b-k8abd, sol.27.7fg, hke.27.3a46-8abd(lengths only)
#       - Raised outside InterCatch: cod.27.7e-k, had.27.7b-k, whg.27.7b-ce-k, mon.27.78abd 

gc()
rm(list = ls())
library(readxl)
library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)
library(FSA)
library(FLCore)
library(ggplotFL)
library(icesSAG)

### not gonna edit an entire script everytime we want to update the years
YEARS <- c(2017:2019)
####Note there are still some hard coded years as there are year specific fixes Q? how permenant are these 
#and cany the be moved to a lookup somehow (probably not)


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

# 01 - CANUM raised in InterCatch  ####
taf.unzip("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.zip", files="2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
intercatch_canum <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
#subset by stock (and not area)
intercatch_canum <- intercatch_canum[intercatch_canum$CatchCat %in% c("Discards", "Landings") & intercatch_canum$Stock %in% c("hke.27.3a46-8abd", "meg.27.7b-k8abd", "sol.27.7fg" ),]
# ~ Remove unwanted data ####  
intercatch_canum <- intercatch_canum[intercatch_canum$CANUM>0,] #  Note - HKE has a number of zeros, not sure why
intercatch_canum_saftey_check <- intercatch_canum #save for sanity checking later
intercatch_canum$samples_weight_kg <- (as.numeric(intercatch_canum$CANUM)*as.numeric(intercatch_canum$MeanWeight_in_g))/1000 # put in kg
intercatch_canum$MeanWeight_in_g <- as.numeric(intercatch_canum$MeanWeight_in_g)

# ~ Area fix ####
area_spp_fix <- read.csv("bootstrap/data/supporting_files/Area_lookup.csv")
names(area_spp_fix) <- c("Area" ,"Standard","ICES_mix_correct", "ICES_FU","species_mix_FU" )
intercatch_canum <- left_join(intercatch_canum,area_spp_fix, by = "Area" )
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$Area <- intercatch_canum$ICES_mix_correct
intercatch_canum <- intercatch_canum[-c(23,24,25,25)]

# ~ Country fix  ####
Country_Lookup <- read_xlsx("bootstrap/data/supporting_files/Country_lookup.xlsx")
intercatch_canum <- left_join(intercatch_canum,Country_Lookup)
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$Country <- intercatch_canum$CorrectCountry
intercatch_canum <- intercatch_canum[-c(25)]

# ~ M?tier level 4 fix #### 
intercatch_canum$lvl4 <- substr(intercatch_canum$Fleet,1,7)
lvl4_Lookup <- read_xlsx("bootstrap/data/supporting_files/Metier_lvl4_lookup.xlsx")
intercatch_canum <- left_join(intercatch_canum,lvl4_Lookup)
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$lvl4_new <- ifelse(is.na(intercatch_canum$Correct_lvl4),intercatch_canum$lvl4, intercatch_canum$Correct_lvl4)
intercatch_canum$lvl4 <- intercatch_canum$lvl4_new 
intercatch_canum <- intercatch_canum[-c(26,27,28)]

# ~ Convert units ####
intercatch_canum$CATON <- intercatch_canum$CATON_in_kg/1000
intercatch_canum$samples_weight_tonnes <- (intercatch_canum$MeanWeight_in_g/1000000)*intercatch_canum$CANUM


# ~ Split out stocks, compare with advice sheet and edit ####

# ~~ sol.27.7fg ####
advice_sheet_values[advice_sheet_values$stock %in% c("sol.27.7fg") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_canum[intercatch_canum$Stock %in% c("sol.27.7fg") & intercatch_canum$Datayear %in% YEARS,] %>% select( Datayear,Stock, CatchCat, samples_weight_tonnes) %>% group_by(Datayear,Stock, CatchCat) %>% summarise(caton = sum(samples_weight_tonnes))
# Notes - sol.27.7fg there is a good match
intercatch_canum_sol_checks <- intercatch_canum[intercatch_canum$Stock == "sol.27.7fg" & intercatch_canum$Datayear %in% c(2017, 2018, 2019),] %>%  select(Datayear,Country, Area, CatchCat, CATON, samples_weight_tonnes) %>% group_by(Datayear,Country, Area, CatchCat, CATON) %>% summarise(samples_weight_tonnes = sum(samples_weight_tonnes, na.rm=T)) %>% mutate(course_difference = (CATON -samples_weight_tonnes) , SOP = (samples_weight_tonnes/CATON)) %>% data.frame() 
intercatch_canum_sol_checks$Acceptable <- ifelse(intercatch_canum_sol_checks$SOP>0.94, "Acceptable", "Suspect")
intercatch_canum_sol_checks$Acceptable <- ifelse(intercatch_canum_sol_checks$SOP>1.05, "Suspect", intercatch_canum_sol_checks$Acceptable)
ggplot(intercatch_canum_sol_checks, aes(CATON, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("sol.27.7fg")
ggplot(intercatch_canum_sol_checks, aes(Acceptable, CATON)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("sol.27.7fg")+xlab("")

# ~~ hke.27.3a46-8abd ####
advice_sheet_values[advice_sheet_values$stock %in% c("hke.27.3a46-8abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_canum[intercatch_canum$Stock %in% c("hke.27.3a46-8abd") & intercatch_canum$Datayear %in% YEARS,] %>% select( Datayear,Stock, CatchCat, samples_weight_tonnes) %>% group_by(Datayear,Stock, CatchCat) %>% summarise(caton = sum(samples_weight_tonnes)) %>% data.frame()
# Notes - hke.27.3a46-8abd not a good match, sometimes too little landings and other time too many discards!
intercatch_canum_hke_checks <- intercatch_canum[intercatch_canum$Stock == "hke.27.3a46-8abd" & intercatch_canum$Datayear %in% c(2017, 2018, 2019),] %>%  select(Datayear,Country, Area, CatchCat, CATON, samples_weight_tonnes) %>% group_by(Datayear,Country, Area, CatchCat, CATON) %>% summarise(samples_weight_tonnes = sum(samples_weight_tonnes, na.rm=T)) %>% mutate(course_difference = (CATON -samples_weight_tonnes) , SOP = (samples_weight_tonnes/CATON)) %>% data.frame() 
intercatch_canum_hke_checks$Acceptable <- ifelse(intercatch_canum_hke_checks$SOP>0.94, "Acceptable", "Suspect")
intercatch_canum_hke_checks$Acceptable <- ifelse(intercatch_canum_hke_checks$SOP>1.05, "Suspect", intercatch_canum_hke_checks$Acceptable)
ggplot(intercatch_canum_hke_checks, aes(CATON, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("hke.27.3a46-8abd")
ggplot(intercatch_canum_hke_checks, aes(Acceptable, CATON)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("hke.27.3a46-8abd")+xlab("")

# ~~ meg.27.7b-k8abd ####
advice_sheet_values[advice_sheet_values$stock %in% c("meg.27.7b-k8abd") & advice_sheet_values$year %in% YEARS,] %>% select( year,stock, catch_cat, total) %>% group_by(year,stock, catch_cat) %>% summarise(caton = sum(total, na.rm=T))
intercatch_canum[intercatch_canum$Stock %in% c("meg.27.7b-k8abd") & intercatch_canum$Datayear %in% YEARS,] %>% select( Datayear,Stock, CatchCat, samples_weight_tonnes) %>% group_by(Datayear,Stock, CatchCat) %>% summarise(caton = sum(samples_weight_tonnes)) %>% data.frame()
# Notes - meg.27.7b-k8abd not a good match, sometimes too little landings and other time too many discards!
intercatch_canum_meg_checks <- intercatch_canum[intercatch_canum$Stock == "meg.27.7b-k8abd" & intercatch_canum$Datayear %in% c(2017, 2018, 2019),] %>%  select(Datayear,Country, Area, CatchCat, CATON, samples_weight_tonnes) %>% group_by(Datayear,Country, Area, CatchCat, CATON) %>% summarise(samples_weight_tonnes = sum(samples_weight_tonnes, na.rm=T)) %>% mutate(course_difference = (CATON -samples_weight_tonnes) , SOP = (samples_weight_tonnes/CATON)) %>% data.frame() 
intercatch_canum_meg_checks$Acceptable <- ifelse(intercatch_canum_meg_checks$SOP>0.94, "Acceptable", "Suspect")
intercatch_canum_meg_checks$Acceptable <- ifelse(intercatch_canum_meg_checks$SOP>1.05, "Suspect", intercatch_canum_meg_checks$Acceptable)
ggplot(intercatch_canum_meg_checks, aes(CATON, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("meg.27.7b-k8abd")
ggplot(intercatch_canum_meg_checks, aes(Acceptable, CATON)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("meg.27.7b-k8abd")+xlab("")

#~ Remove quarter ~ aggregate ####  
intercatch_canum<- intercatch_canum %>% select(Datayear ,Stock, Country ,CatchCat,lvl4, Area,CATON, CANUMType, ageorlength, CANUM, MeanWeight_in_g, samples_weight_tonnes) %>% group_by(Datayear, Stock,  Country, CatchCat, lvl4,  Area, CANUMType, ageorlength, CATON) %>% summarise(MeanWeight_in_g = weighted.mean(as.numeric(MeanWeight_in_g), CANUM), CANUM=sum(CANUM,na.rm = T),  samples_weight_tonnes=sum(samples_weight_tonnes,na.rm = T))%>% data.frame()
intercatch_canum <- intercatch_canum %>% select(Datayear, Country,Area , CatchCat,lvl4, ageorlength, CATON, CANUM, MeanWeight_in_g, samples_weight_tonnes,Stock)
names(intercatch_canum) <- c("Year","Country", "Area", "CatchCat", "lvl4", "Age", "CATON", "CANUM", "Mean_Weight_in_g","samples_weight_tonnes", "Stock")

intercatch_canum_hke <- intercatch_canum[intercatch_canum$Stock == "hke.27.3a46-8abd",]
intercatch_canum_sol <- intercatch_canum[intercatch_canum$Stock == "sol.27.7fg",]
intercatch_canum_meg <- intercatch_canum[intercatch_canum$Stock ==  "meg.27.7b-k8abd",]

# 02 - Convert length to age #####
#issue with mixed length units! https://shiny.marine.ie/igfs/ 
names(intercatch_canum_hke)[6] <- "Length"
intercatch_canum_hke$Length_new <- intercatch_canum_hke$Length
#ggplot(intercatch_canum_hke, aes(Length_new, Mean_Weight_in_g)) + geom_point() +theme_classic() 
#ggplot(intercatch_canum_hke, aes(Length_new, Mean_Weight_in_g)) + geom_line() +theme_classic() 
intercatch_canum_hke$Length_new <- ifelse(intercatch_canum_hke$Length_new <250 & intercatch_canum_hke$Mean_Weight_in_g>120, intercatch_canum_hke$Length_new*10, intercatch_canum_hke$Length_new)
intercatch_canum_hke <- intercatch_canum_hke[!intercatch_canum_hke$Length_new >750 & intercatch_canum_hke$Mean_Weight_in_g<2500,]
intercatch_canum_hke$Length_new <- ifelse(intercatch_canum_hke$Length_new <30 & intercatch_canum_hke$Mean_Weight_in_g>5, intercatch_canum_hke$Length_new*10, intercatch_canum_hke$Length_new)
#ggplot(intercatch_canum_hke, aes(Length_new, Mean_Weight_in_g)) + geom_point() +theme_classic() 
intercatch_canum_hke$Length <- intercatch_canum_hke$Length_new/10 #convert to cm
intercatch_canum_hke <- intercatch_canum_hke[!is.na(intercatch_canum_hke$Stock),]

load("bootstrap/data/ices_intercatch/ALK/hke/alk.RData")
dimnames(alk); dim(alk)
# trends in alk per cohort stara
# for (i in 1:24){
#   matrix.key <- prop.table(apply(alk[,,i],2,rev), margin=1)
#   alkPlot(matrix.key,"splines")
# }
# average across cohorts 
hke_alk_year <- apply(alk, c(1,2), FUN=mean)
hke_alk_year <- prop.table(apply(hke_alk_year,2,rev), margin=1)
alkPlot(hke_alk_year,"splines") # sanity check
#apply alk
brks <- c(1,seq(2,100,2),110,120,130)
intercatch_canum_hke <- FSA::alkIndivAge(hke_alk_year,~Length  ,data=intercatch_canum_hke, type="SR", breaks=brks)
intercatch_canum_hke

#plot new age data
ggplot(intercatch_canum_hke, aes(age, samples_weight_tonnes)) + geom_bar(stat="identity")+ theme_classic()
ggplot(intercatch_canum_hke, aes(age,samples_weight_tonnes)) + geom_bar(stat="identity")+ facet_wrap(~Year) + theme_classic()
ggplot(intercatch_canum_hke, aes(Datayear, CATON)) + geom_bar(stat="identity") + theme_classic()

#conclusion delete all sample data prior to 2011
intercatch_canum_hke <- intercatch_canum_hke[intercatch_canum_hke$Datayear>2010,]

# 03 - CANUM raised outside InterCatch - WGCSE ####
canum_cod <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_COD_summary.csv")
canum_had <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_HAD_summary.csv")
canum_whg <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_WHG_summary.csv")

# ~ Stock fix ####
canum_cod$Stock<-"cod.27.7e-k"
canum_had$Stock<-"had.27.7b-k"
canum_whg$Stock<-"whg.27.7b-ce-k" 

# ~ M?tier level 4 fix
canum_cod$lvl4 <- canum_cod$fleet 
canum_had$lvl4 <- canum_had$fleet 
canum_whg$lvl4 <- canum_whg$fleet 

# ~ Format fix ####
canum_cod <- canum_cod[canum_cod$year <(max(YEARS)+1),] # we only had access to this years file
canum_cod<- canum_cod %>% select(year, country, subArea  , catchCat   , lvl4, Age  , frequency1000  , meanWeightKg, Stock) %>% 
  group_by(year, country, subArea  , catchCat   , lvl4, Age, Stock ) %>% summarise(frequency1000 = sum(frequency1000, na.rm=T), Weight = weighted.mean(meanWeightKg,w=frequency1000,na.rm=T)) %>% data.frame() #CM needs to be changed to weighted mean when we get the right file
names(canum_cod) <- c("Year", "Country", "Area", "CatchCat", "lvl4", "Age", "Stock", "CANUM", "Mean_Weight_in_g")

canum_had <- canum_had %>% select(year, country, subArea, catchCat, lvl4, Age, frequency1000, meanWeightKg, Stock) %>% 
  group_by(year, country, subArea, catchCat, lvl4, Age, Stock) %>% summarise(frequency1000 = sum(frequency1000, na.rm=T), Weight = weighted.mean(meanWeightKg,w=frequency1000,na.rm=T)) %>% data.frame()
names(canum_had) <- c("Year", "Country", "Area", "CatchCat", "lvl4", "Age","Stock", "CANUM", "Mean_Weight_in_g")

canum_whg <- canum_whg %>% select(year, country, subArea, catchCat, lvl4, Age, frequency1000, meanWeightKg, Stock) %>% 
  group_by(year, country, subArea, catchCat, lvl4, Age, Stock) %>% summarise(frequency1000 = sum(frequency1000, na.rm=T), Weight = weighted.mean(meanWeightKg,w=frequency1000,na.rm=T)) %>% data.frame()
names(canum_whg) <- c("Year", "Country", "Area", "CatchCat", "lvl4", "Age","Stock", "CANUM", "Mean_Weight_in_g")

other_canum <- rbind(canum_cod, canum_whg, canum_had)
other_canum$Area <- as.character(other_canum$Area)
other_canum_saftey_check <- other_canum

# ~ Country fix  ####
other_canum <- left_join(other_canum,Country_Lookup)
dim(other_canum)[1]-dim(other_canum_saftey_check)[1] #safety check - dims should match
other_canum$Country <- other_canum$CorrectCountry
other_canum <- other_canum[-c(10)]

#~ SOP check of baseline data ####
other_caton <- read.csv("results/clean_data/caton_summary.csv")
other_caton <- other_caton[other_caton$Stock %in% c("cod.27.7e-k" ,"had.27.7b-k", "whg.27.7b-ce-k"),]
other_caton$Area <- "27.7"
other_caton <- other_caton%>% select(Year, Stock, Country, Area,lvl4, Discards, Landings) %>% 
  group_by(Year, Stock, Country, Area,lvl4) %>% 
  summarise(Discards =sum(Discards, na.rm=T), Landings = sum(Landings ,na.rm=T))
other_caton <- gather(other_caton, key = CatchCat, value = "CATON", 6:7)
other_canum <- left_join(other_canum, other_caton, by= c("Year" = "Year", "Stock" = "Stock", "Country" = "Country", "Area" = "Area", "lvl4" = "lvl4", "CatchCat"= "CatchCat"))
other_canum$samples_weight_kg <- (other_canum$CANUM*other_canum$Mean_Weight_in_g)/1000 # put in kg

other_canum_checks <- other_canum %>% 
  group_by(Year, Stock, Country, Area, CatchCat, CATON) %>% summarise(samples_weight_kg = sum(samples_weight_kg, na.rm=T)) %>% mutate(course_difference = (CATON -samples_weight_kg) , SOP = (samples_weight_kg/ CATON)) %>% data.frame() 
ggplot(other_canum_checks[other_canum_checks$Stock == "cod.27.7e-k",], aes(CATON, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic() +ggtitle("cod.27.7e-k")
ggplot(other_canum_checks[other_canum_checks$Stock == "had.27.7b-k",], aes(CATON, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("had.27.7b-k")
ggplot(other_canum_checks[other_canum_checks$Stock == "whg.27.7b-ce-k",], aes(CATON, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("whg.27.7b-ce-k")
# sanity check - assuming we consider SOP between 0.95 and 1.05 as acceptable
other_canum_checks$Acceptable <- ifelse(other_canum_checks$SOP>0.94, "Acceptable", "Suspect")
other_canum_checks$Acceptable <- ifelse(other_canum_checks$SOP>1.05, "Suspect", other_canum_checks$Acceptable)
ggplot(other_canum_checks[other_canum_checks$Stock == "cod.27.7e-k",], aes( Acceptable, CATON)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("cod.27.7e-k")+xlab("")
ggplot(other_canum_checks[other_canum_checks$Stock == "had.27.7b-k",], aes(Acceptable, CATON)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("had.27.7b-k")+xlab("")
ggplot(other_canum_checks[other_canum_checks$Stock == "whg.27.7b-ce-k",], aes(Acceptable, CATON)) + geom_bar(stat="identity")+ facet_wrap(~Country) + theme_classic()+ggtitle("whg.27.7b-ce-k")+xlab("")

# ~ Adjustment _ I am not sure this is required - CM - need to ask PD and JB
#And they are not so now we take a ratio of of the unique(SOP_SUM over the caton to give us a ratio to multiply the No_at_age at)
# intercatch_canum3 <- intercatch_canum3 %>% group_by_at(vars(-SOP,-MeanWeight_in_g,-Number_at_age,-Age)) %>%  mutate(diff_ratio=unique(SOP_SUM)/unique(CATON_in_kg)) %>% ungroup() %>% mutate(No_At_Age_ADJ=Number_at_age*diff_ratio)
WGCSE_canum <- other_canum %>% select(Year,Country, Area, CatchCat, lvl4, Age, CANUM, Mean_Weight_in_g,samples_weight_kg, Stock) 

# 04 - CANUM raised outside InterCatch - WGBIE ####
# mon.27.78abd
# Stock assesoror supplied anumbers at age and metier info was taken from Caton
caton <- read.csv("results/clean_data/caton_summary.csv")
caton <- caton[caton$Stock == "mon.27.78abd",] %>% select(Year, Country, Area, lvl4, Discards, Landings)%>% gather(CatchCat, caton, 5:6)
caton <- caton[!is.na(caton$caton),]
caton$caton <- caton$caton/1000 #convert into tonnes
caton_dis <- caton[caton$CatchCat== "Discards",]
caton_dis %>% select(Year,caton ) %>% group_by(Year) %>% summarise(total = sum(caton, na.rm=T) )
caton_lan <- caton[caton$CatchCat== "Landings",]
caton_lan%>% select(Year, caton) %>% group_by(Year) %>% summarise(caton = sum(caton, na.rm=T))

#these values dont match what is in the advice sheet
lan_total_2017 <- 25634
lan_total_2018 <- 22345
lan_total_2019 <- 21266

dis_total_2017 <- 2175
dis_total_2018 <- 1250
dis_total_2019 <- 1444

caton_dis$prop[caton_dis$Year == YEARS[1]] <- caton_dis$caton[caton_dis$Year == YEARS[1]]/sum(caton_dis$caton[caton_dis$Year == YEARS[1]], na.rm=T)
caton_dis$prop[caton_dis$Year == YEARS[2]] <- caton_dis$caton[caton_dis$Year == YEARS[2]]/sum(caton_dis$caton[caton_dis$Year == YEARS[2]], na.rm=T)
caton_dis$prop[caton_dis$Year == YEARS[3]] <- caton_dis$caton[caton_dis$Year == YEARS[3]]/sum(caton_dis$caton[caton_dis$Year == YEARS[3]], na.rm=T)

caton_dis$caton_corrected <- NaN
caton_dis$caton_corrected[caton_dis$Year == YEARS[1]] <- dis_total_2017*caton_dis$prop[caton_dis$Year == YEARS[1]]
caton_dis$caton_corrected[caton_dis$Year == YEARS[2]] <- dis_total_2018*caton_dis$prop[caton_dis$Year == YEARS[2]]
caton_dis$caton_corrected[caton_dis$Year == YEARS[3]] <- dis_total_2019*caton_dis$prop[caton_dis$Year == YEARS[3]]
caton_dis%>% select(Year, caton_corrected) %>% group_by(Year) %>% summarise(caton_corrected = sum(caton_corrected, na.rm=T))

caton_lan$prop[caton_lan$Year == YEARS[1]] <- caton_lan$caton[caton_lan$Year == YEARS[1]]/sum(caton_lan$caton[caton_lan$Year == YEARS[1]], na.rm=T)
caton_lan$prop[caton_lan$Year == YEARS[2]] <- caton_lan$caton[caton_lan$Year == YEARS[2]]/sum(caton_lan$caton[caton_lan$Year == YEARS[2]], na.rm=T)
caton_lan$prop[caton_lan$Year == YEARS[3]] <- caton_lan$caton[caton_lan$Year == YEARS[3]]/sum(caton_lan$caton[caton_lan$Year == YEARS[3]], na.rm=T)

caton_lan$caton_corrected <- NaN
caton_lan$caton_corrected[caton_lan$Year == YEARS[1]] <- lan_total_2017*caton_lan$prop[caton_lan$Year == YEARS[1]]
caton_lan$caton_corrected[caton_lan$Year == YEARS[2]] <- lan_total_2018*caton_lan$prop[caton_lan$Year == YEARS[2]]
caton_lan$caton_corrected[caton_lan$Year == YEARS[3]] <- lan_total_2019*caton_lan$prop[caton_lan$Year == YEARS[3]]
caton_lan%>% select(Year, caton_corrected) %>% group_by(Year) %>% summarise(caton_corrected = sum(caton_corrected, na.rm=T))
caton <- rbind(caton_lan, caton_dis)


#convert length to age
# load some functions
source('funcs/length_to_age_functions.R')

# VBGF parameters and length data
ages <- 0:9
Linf <- 171
K <- 0.1075
t0 <- 0
sd <-  seq(3,10,length=length(ages))
LWa <- 0.0000303
LWb <- 2.82
ln <- read.inputfile("bootstrap/data/ices_intercatch/ALK/mon/mon78_landings_n.csv")
lw <- read.inputfile("bootstrap/data/ices_intercatch/ALK/mon/mon78_landings_wt_.csv") #note that submission file says that these are in kg but they are in tonnes!
dn <- read.inputfile("bootstrap/data/ices_intercatch/ALK/mon/mon78_discards_n.csv")
dw <- read.inputfile("bootstrap/data/ices_intercatch/ALK/mon/mon78_discards_wt.csv")#note that submission file says that these are in kg but they are in tonnes!

# convert to FLQuant
lnq <- makeFLQuant(ln,units=1e-3) # thousands
lwq <- makeFLQuant(lw,units=1)    # kg
dnq <- makeFLQuant(dn,units=1e-3) 
dwq <- makeFLQuant(dw,units=1)

# fit cohorts along VBGC to make an ALK 
# combine landings and discards
cnq <- lnq+dnq  
cwq <- (lnq*lwq + dnq*dwq)/(lnq+dnq)

# fit cohorts along VBGC to make an ALK 
fit <- fitVbgcFlQuant(cnq)

# apply ALK to landings
# apply ALK to landings
landings <- applyAlkFlQuant(nlen=lnq,wlen=lwq,fit)
landings.n <- apply(landings[['nage']],1:2,sum)
landings.wt <- apply(landings[['wage']]*landings[['nage']],1:2,sum)/landings.n

landings.n <- as.data.frame(landings.n)
landings.n <- landings.n %>% select(year, age, data) %>% setNames(c("year", "age", "CANUM"))
landings.wt <- as.data.frame(landings.wt) 
landings.wt <- landings.wt %>% select(year, age, data) %>% setNames(c("year", "age", "Mean_weight_kg"))

landings_mon <- left_join(landings.n, landings.wt)
landings_mon <- landings_mon[landings_mon$CANUM>0,]
landings_mon$CatchCat <- "Landings"

# apply ALK to discards
discards <- applyAlkFlQuant(nlen=dnq,wlen=dwq,fit)
discards.n <- apply(discards[['nage']],1:2,sum)
discards.wt <- apply(discards[['wage']]*discards[['nage']],1:2,sum)/discards.n

discards.n <- as.data.frame(discards.n)
discards.n <- discards.n %>% select(year, age, data) %>% setNames(c("year", "age", "CANUM"))
discards.wt <- as.data.frame(discards.wt) 
discards.wt <- discards.wt %>% select(year, age, data) %>% setNames(c("year", "age", "Mean_weight_kg"))

discards_mon <- left_join(discards.n, discards.wt)
discards_mon <- discards_mon[discards_mon$CANUM>0,]
discards_mon$CatchCat <- "Discards"

mon_age <- rbind(discards_mon, landings_mon) %>% select(year, age, CatchCat, CANUM, Mean_weight_kg)%>% group_by(year, age, CatchCat, CANUM, Mean_weight_kg) %>% summarise(CANUM = sum(CANUM, na.rm=T), Mean_weight_kg = mean(Mean_weight_kg, na.rm=T)) %>% data.frame()
mon_age$sample_weight_kg <- mon_age$CANUM* mon_age$Mean_weight_kg
mon_age[mon_age$year>2016,] %>% select(year, CatchCat, sample_weight_kg) %>% group_by(year, CatchCat) %>% summarise(caton = sum(sample_weight_kg, na.rm = T))

#apply alk to metiers
#landings are close but discards are way out. CANUM supplied by stock assessor matches Advice sheet. 
# So we will just calculate a proportion of total landings and discards in caton and then apply ALK 

mon_new <- left_join(caton, mon_age, by= c("Year"= "year", "CatchCat" = "CatchCat" ))
mon_new$CANUM_new <- mon_new$CANUM *mon_new$prop
mon_new$sample_weight_kg <- mon_new$CANUM_new*mon_new$Mean_weight_kg
mon_new %>% select(Year, CatchCat, sample_weight_kg) %>% group_by(Year, CatchCat) %>% summarise(caton = sum(sample_weight_kg, na.rm=T))


# 05 _ Merge and write out final CANUM #####
canum_summary <- rbind(intercatch_canum_sol, intercatch_canum_meg, intercatch_canum_hke, WGCSE_canum)

write.taf(canum_summary, file.path("results/clean_data/canum_summary.csv"))

