# Data prep
# Preprocess data,

## Before: InterCatch extraction, data raised external to InterCatch, and ALK's
## After:  Age structure from InterCatch 

# Notes: User must make sure that:  
# 1 - factors are turned to characters, 
# 2 - dim of data frame does not change shape during cleaning process
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

# 01 - CANUM raised in InterCatch  ####
taf.unzip("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.zip", files="2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
intercatch_canum <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
#subset for case study area
intercatch_canum <- intercatch_canum[intercatch_canum$Area %in% c("27.7"  , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ) & intercatch_canum$CatchCat %in% c("Discards", "Landings") & intercatch_canum$Stock %in% c("hke.27.3a46-8abd", "meg.27.7b-k8abd", "sol.27.7fg" ),]
# ~ Remove unwanted data ####  
intercatch_canum <- intercatch_canum[intercatch_canum$CANUM>0,] #  CM -a number of hke with no canum, why?
intercatch_canum_saftey_check <- intercatch_canum #save for sanity checking later
intercatch_canum$samples_weight_kg <- (as.numeric(intercatch_canum$CANUM)*as.numeric(intercatch_canum$MeanWeight_in_g))/1000 # put in kg


# ~ Area fix ####
area_spp_fix <- read.csv("bootstrap/data/supporting_files/Area_lookup.csv")
names(area_spp_fix) <- c("Area" ,"Standard","ICES_mix_correct", "ICES_FU","species_mix_FU" )
intercatch_canum <- left_join(intercatch_canum,area_spp_fix, by = "Area" )
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$Area <- intercatch_canum$ICES_mix_correct
intercatch_canum <- intercatch_canum[-c(22,23,24,25)]

# ~ Country fix  ####
Country_Lookup <- read_xlsx("bootstrap/data/supporting_files/Country_lookup.xlsx")
intercatch_canum <- left_join(intercatch_canum,Country_Lookup)
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$Country <- intercatch_canum$CorrectCountry
intercatch_canum <- intercatch_canum[-c(25)]

# ~ Métier level 4 fix #### 
intercatch_canum$lvl4 <- substr(intercatch_canum$Fleet,1,7)
lvl4_Lookup <- read_xlsx("bootstrap/data/supporting_files/Metier_lvl4_lookup.xlsx")
intercatch_canum <- left_join(intercatch_canum,lvl4_Lookup)
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$lvl4_new <- ifelse(is.na(intercatch_canum$Correct_lvl4),intercatch_canum$lvl4, intercatch_canum$Correct_lvl4)
intercatch_canum$lvl4 <- intercatch_canum$lvl4_new 
intercatch_canum <- intercatch_canum[-c(25,26,27)]

#~ SOP check of baseline data ####
#needs to be done before you aggregate
intercatch_canum_checks <- intercatch_canum %>% 
  group_by(Datayear, Stock, Country, Area, CatchCat, CANUMType, CATON_in_kg ) %>% summarise(samples_weight_kg = sum(samples_weight_kg, na.rm=T)) %>% mutate(course_difference = (CATON_in_kg -samples_weight_kg) , SOP = (samples_weight_kg/ CATON_in_kg)) %>% data.frame() 
ggplot(intercatch_canum_checks[intercatch_canum_checks$Stock == "hke.27.3a46-8abd",], aes(CATON_in_kg, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic() +ggtitle("hke.27.3a46-8abd")
ggplot(intercatch_canum_checks[intercatch_canum_checks$Stock == "sol.27.7fg",], aes(CATON_in_kg, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("sol.27.7fg")
ggplot(intercatch_canum_checks[intercatch_canum_checks$Stock == "meg.27.7b-k8abd",], aes(CATON_in_kg, SOP)) + geom_point() + facet_wrap(~Country) + theme_classic()+ggtitle("meg.27.7b-k8abd")
# sanity check - assuming we consider SOP between 0.95 and 1.05 as acceptable
intercatch_canum_checks$Acceptable <- ifelse(intercatch_canum_checks$SOP>0.94, "Acceptable", "Suspect")
intercatch_canum_checks$Acceptable <- ifelse(intercatch_canum_checks$SOP>1.05, "Suspect", intercatch_canum_checks$Acceptable)
ggplot(intercatch_canum_checks[intercatch_canum_checks$Stock == "hke.27.3a46-8abd",], aes( Acceptable, CATON_in_kg)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("hke.27.3a46-8abd")+xlab("")
ggplot(intercatch_canum_checks[intercatch_canum_checks$Stock == "sol.27.7fg",], aes(Acceptable, CATON_in_kg)) + geom_bar(stat="identity") + facet_wrap(~Country) + theme_classic()+ggtitle("sol.27.7fg")+xlab("")
ggplot(intercatch_canum_checks[intercatch_canum_checks$Stock == "meg.27.7b-k8abd",], aes(Acceptable, CATON_in_kg)) + geom_bar(stat="identity")+ facet_wrap(~Country) + theme_classic()+ggtitle("meg.27.7b-k8abd")+xlab("")

# ~ Adjustment _ I am not sure this is required - CM - need to ask PD and JB
#And they are not so now we take a ratio of of the unique(SOP_SUM over the caton to give us a ratio to multiply the No_at_age at)
#intercatch_canum3 <- intercatch_canum3 %>% group_by_at(vars(-SOP,-MeanWeight_in_g,-Number_at_age,-Age)) %>%  mutate(diff_ratio=unique(SOP_SUM)/unique(CATON_in_kg)) %>% ungroup() %>% mutate(No_At_Age_ADJ=Number_at_age*diff_ratio)

#~ Remove quarter ~ aggregate ####  
intercatch_canum<- intercatch_canum %>% select("Datayear" ,"Stock" ,"Season","SeasonType","Country" ,"CatchCat","lvl4", "Area","CATON_in_kg","CANUMType", "ageorlength","CANUM","MeanWeight_in_g", "samples_weight_kg")
intercatch_canum_YEARS <- intercatch_canum %>% filter(SeasonType %in%c("Year")) %>% select(-Season,-SeasonType)
intercatch_canum_QUARTER <- intercatch_canum %>% filter(!SeasonType %in%c("Year")) %>% select(-Season,-SeasonType) #NB CATON_in_kg is replicated
intercatch_canum_QUARTER <- intercatch_canum_QUARTER %>% group_by(Datayear, Stock,  Country, CatchCat, lvl4,  Area, CANUMType, ageorlength, CATON_in_kg, samples_weight_kg) %>% summarise(MeanWeight_in_g = weighted.mean(as.numeric(MeanWeight_in_g), CANUM), CANUM=sum(CANUM,na.rm = T),  samples_weight_kg=sum(samples_weight_kg,na.rm = T))%>% data.frame()
#NOTE - warnings are due to no mean weights for HKE in 2009 for UKN, UKE and UKS.
intercatch_canum <- rbind(intercatch_canum_YEARS,intercatch_canum_QUARTER)
intercatch_canum <- intercatch_canum %>% select(Datayear, Country,Area , CatchCat,lvl4,CANUMType, ageorlength,CANUM, MeanWeight_in_g, samples_weight_kg,Stock)
names(intercatch_canum) 
intercatch_canum$MeanWeight_in_g <-  as.numeric(intercatch_canum$MeanWeight_in_g)

intercatch_canum_hke <- intercatch_canum[intercatch_canum$Stock == "hke.27.3a46-8abd", ]
intercatch_canum <- intercatch_canum[!intercatch_canum$Stock == "hke.27.3a46-8abd", ]

# 02 - Convert length to age #####
load("bootstrap/data/ices_intercatch/ALK/hke/alk.RData")
dimnames(alk)#arrays: [i,j,k] for the ith row, jth column, kth level
# dim(alk)
# # trends in alk per chort stara
# for (i in 1:24){
#   matrix.key <- prop.table(apply(alk[,,i],2,rev), margin=1)
#   alkPlot(matrix.key,"splines")
# }

# average across cohorts 
hke_alk_year <- apply(alk, c(1,2), FUN=mean)
hke_alk_year <- prop.table(apply(hke_alk_year,2,rev), margin=1)
alkPlot(hke_alk_year,"splines") # sanity check
names(intercatch_canum_hke)[15] <- "Length"

#plot length against weight by country
ggplot(intercatch_canum_hke, aes(Length, MeanWeight_in_g)) + geom_line() + facet_wrap(~Country) +theme_classic()
ggplot(intercatch_canum_hke, aes(Length, MeanWeight_in_g)) + geom_line() + facet_wrap(~Datayear) +theme_classic()
#issues with data submitted in 2013 - 2016
ggplot(intercatch_canum_hke[intercatch_canum_hke$Datayear %in% c(2013, 2014, 2015, 2016) & intercatch_canum_hke$Length<200,], aes(Length, MeanWeight_in_g)) + geom_line() + facet_wrap(~Datayear) +theme_classic()
ggplot(intercatch_canum_hke[intercatch_canum_hke$Datayear %in% c(2013, 2014, 2015, 2016) & intercatch_canum_hke$Length<200,], aes(Length, MeanWeight_in_g)) + geom_line() + facet_wrap(~Country) +theme_classic()
ggplot(intercatch_canum_hke, aes(Length, MeanWeight_in_g)) + geom_line() + facet_wrap(~DataUsedInAssessment) +theme_classic()

#issue with mixed length units! https://shiny.marine.ie/igfs/
intercatch_canum_hke$Length_new <- intercatch_canum_hke$Length

intercatch_canum_hke$Length_new <- ifelse(intercatch_canum_hke$Length_new[intercatch_canum_hke$Length_new >25 & intercatch_canum_hke$MeanWeight_in_g <100], intercatch_canum_hke$Length_new/100, intercatch_canum_hke$Length_new)
#intercatch_canum_hke$Length_new [intercatch_canum_hke$MeanWeight_in_g <100 & intercatch_canum_hke$Length_new >25 ]  <-  intercatch_canum_hke$Length_new [intercatch_canum_hke$MeanWeight_in_g <100 & intercatch_canum_hke$Length_new >25 ] /100 #convert from mm to cm


ggplot(intercatch_canum_hke, aes(Length_new, MeanWeight_in_g)) + geom_line() + facet_wrap(~Country) +theme_classic()

#intercatch_canum_hke$Length <- ifelse(intercatch_canum_hke$Length<1,1,intercatch_canum_hke$Length) #there are no fish in ALK less then 1
brks <- c(1,seq(2,100,2),110,120,130)
rb.len1 <- FSA::alkIndivAge(hke_alk_year,~Length  ,data=intercatch_canum_hke, type="SR", breaks=brks)

# 03 - CANUM raised outside InterCatch - WGCSE ####
canum_cod <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_COD_summary.csv")
canum_had <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_HAD_summary.csv")
canum_whg <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_WHG_summary.csv")

# ~ Stock fix ####
canum_cod$Stock<-"cod.27.7e-k"
canum_had$Stock<-"had.27.7b-k"
canum_whg$Stock<-"whg.27.7b-ce-k"

# ~ Métier level 4 fix
canum_cod$lvl4 <- canum_cod$fleet 
canum_had$lvl4 <- canum_had$fleet 
canum_whg$lvl4 <- canum_whg$fleet 

# ~ Format fix ####
canum_cod <- canum_cod[canum_cod$year <2020,] # we only had access to this years file
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
other_caton <- read.csv("results/clean_data/intercatch_caton_summary.csv")
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
#intercatch_canum3 <- intercatch_canum3 %>% group_by_at(vars(-SOP,-MeanWeight_in_g,-Number_at_age,-Age)) %>%  mutate(diff_ratio=unique(SOP_SUM)/unique(CATON_in_kg)) %>% ungroup() %>% mutate(No_At_Age_ADJ=Number_at_age*diff_ratio)

# 04 - CANUM raised outside InterCatch - WGBIE ####
#Mon

# 05 _ Merge and write out final CANUM #####
write.taf(intercatch_canum_fin,file.path("results/clean_data/intercatch_canum_summary.csv"))

