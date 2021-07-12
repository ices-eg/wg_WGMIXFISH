# Data prep
# Preprocess data,

## Before: InterCatch extraction, data raised external to InterCatch, and ALK's
## After:  Age structure from InterCatch 

# Notes: User must make sure that:  
# 1 - factors are turned to characters, 
# 2 - dim of data frame does not change shape during cleaning process
# Data sources vary per stock: 
#       - InterCatch CANUM with distribution: meg.27.7b-k8abd, sol.27.7fg
#       - Raised outside InterCatch: cod.27.7e-k, had.27.7b-k, whg.27.7b-ce-k, mon.27.78abd 

#gc()
#rm(list = ls())
library(readxl)
library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)

# 01 - InterCatch CANUM ####
taf.unzip("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.zip", files="2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
intercatch_canum <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
#subset for case study area
intercatch_canum <- intercatch_canum[intercatch_canum$Area %in% c("27.7"  , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ),]
intercatch_canum_saftey_check <- intercatch_canum #save for sanity checking later

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

# ~ Species fix #### 
intercatch_canum$Species <- toupper(substr(intercatch_canum$Stock,1,3))

# ~ Métier level 4 fix #### 
intercatch_canum$lvl4 <- substr(intercatch_canum$Fleet,1,7)
lvl4_Lookup <- read_xlsx("bootstrap/data/supporting_files/Metier_lvl4_lookup.xlsx")
intercatch_canum <- left_join(intercatch_canum,lvl4_Lookup)
dim(intercatch_canum)[1]-dim(intercatch_canum_saftey_check)[1] #safety check - dims should match
intercatch_canum$lvl4_new <- ifelse(is.na(intercatch_canum$Correct_lvl4),intercatch_canum$lvl4, intercatch_canum$Correct_lvl4)
intercatch_canum$lvl4 <- intercatch_canum$lvl4_new 
intercatch_canum <- intercatch_canum[-c(25,26,27)]

# ~ Remove unwanted data ####  
intercatch_canum <-intercatch_canum [intercatch_canum$CatchCat %in% c("Discards", "Landings"),]


#~ Remove quarter ~ aggregate ####  
intercatch_canum<- intercatch_canum %>% select("Datayear" ,"Stock" ,"Season","SeasonType","Country" ,"Fleet" ,"CatchCat","lvl4", "Area","Species","CATON_in_kg","CANUMType", "ageorlength","CANUM","MeanWeight_in_g")
intercatch_canum_YEARS <- intercatch_canum %>% filter(SeasonType %in%c("Year")) %>% select(-Season,-SeasonType)
intercatch_canum_QUARTER <- intercatch_canum %>% filter(!SeasonType %in%c("Year")) %>% select(-Season,-SeasonType) #NB CATON_in_kg is replicated
intercatch_canum_QUARTER <- intercatch_canum_QUARTER %>% group_by(Datayear, Stock,  Country, Fleet, CatchCat, lvl4,  Area,  Species, CANUMType, ageorlength, CATON_in_kg) %>% summarise(MeanWeight_in_g = weighted.mean(as.numeric(MeanWeight_in_g), CANUM), CANUM=sum(CANUM,na.rm = T))%>% data.frame()
#NOTE - warnings are due to no mean weights for HKE in 2009 for UKN, UKE and UKS.
intercatch_canum <- rbind(intercatch_canum_YEARS,intercatch_canum_QUARTER)

#sum(intercatch_canum_saftey_check$CANUM)-sum(intercatch_canum$CANUM) # This is not a valid sanity check as they are repeated values

# ~ SOP check ####
# first remove all zeros
intercatch_canum <- intercatch_canum[intercatch_canum$CANUM>0,]
intercatch_canum <- intercatch_canum[intercatch_canum$MeanWeight_in_g>0,] # why would there be fish with an age and no weight?
intercatch_canum <- intercatch_canum[!is.na(intercatch_canum$CATON_in_kg),] 

# Check the caton and numbers at age*meanweight matches, SOP is in grams
intercatch_canum$SOP <- (as.numeric(intercatch_canum$CANUM)*as.numeric(intercatch_canum$MeanWeight_in_g))/1000 # put in kg
### SOP and the caton_kg should be the same or within very small tolerances)
intercatch_canum$diff <- intercatch_canum$SOP - as.numeric(intercatch_canum$CATON_in_kg) 


#And they are not so now we take a ratio of of the unique(SOP_SUM over the caton to give us a ratio to multiply the No_at_age at) 
intercatch_canum3 <- intercatch_canum3 %>% group_by_at(vars(-SOP,-MeanWeight_in_g,-Number_at_age,-Age)) %>%  mutate(diff_ratio=unique(SOP_SUM)/unique(CATON_in_kg)) %>% ungroup() %>% mutate(No_At_Age_ADJ=Number_at_age*diff_ratio)



# 02 - CANUM raised outside InterCatch ####
canum_cod <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_COD_summary.csv")
canum_had <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_HAD_summary.csv")
canum_whg <-  read.csv("bootstrap/data/ices_intercatch/canum_WG_WHG_summary.csv")

#### alters canum_cod allow rbind
#?From the processing scripts, it seems that Frequency is equal to FrequencyUploaded / SOP ratio for each line. SOP ratio is equal to the sum of products of N@age*W@age/Caton.  (pers coms lionel)
##
names(canum_cod)
names(canum_cod) <- tolower(names(canum_cod))
names(canum_cod)[names(canum_cod)=="area"] <-"subArea"
names(canum_cod)[names(canum_cod)=="frequency"] <-"frequency1000"
names(canum_cod)[names(canum_cod)=="weight"] <-"meanWeightKg"
names(canum_cod)[names(canum_cod)=="catch.cat."] <-"catchCat"
names(canum_cod)[names(canum_cod)=="agelength"] <-"Age"
names(canum_cod)[names(canum_cod)=="fleet1"] <-"fleet"

### lvl4
canum_cod$fleet <- substring(canum_cod$fleet,1,7)

##selects coloumns and changes from grams to kg 
canum_cod <- canum_cod %>% select(year,country,subArea,catchCat,fleet,Age,frequency1000,meanWeightKg) %>% mutate(meanWeightKg=meanWeightKg/1000) %>% group_by_at(vars(-frequency1000)) %>% summarise(frequency1000=sum(frequency1000,na.rm = T)) 

##Weighted mean by freqencey of mean weights
canum_cod <- canum_cod %>% group_by_at(vars(-frequency1000,-meanWeightKg)) %>% summarise(meanWeightKg=weighted.mean(meanWeightKg,w=frequency1000,na.rm=T),frequency1000=sum(frequency1000,na.rm = T)) %>% ungroup()

# ~ Fix and merge ####
canum_cod$Stock<-"cod.27.7e-k"
canum_had$Stock<-"had.27.7b-k"
canum_whg$Stock<-"whg.27.7b-ce-k"
canum_other<-rbind(canum_had,canum_whg)
names(canum_other)<-c("Year", "Country","Area" , "CatchCat","lvl4","Age", "frequency1000","MeanWeight_in_g",  "Stock")
canum_other$lvl4<-substr(canum_other$lvl4,1,7)
names(canum_cod)<-c("Year", "Country","Area" , "CatchCat","lvl4","Age", "frequency1000","MeanWeight_in_g",  "Stock")
canum_cod$lvl4<-substr(canum_cod$lvl4,1,7)

# ~bind addtional data to main IC File----
#We need to remvoe any addtional coloumn and change the names of the ones we keep to match

#code for if this is true
# intercatch_canum_bind <- intercatch_canum3 %>% select(Year, Country,Area , CatchCat,lvl4,Age, frequency1000,MeanWeight_in_g,  Stock,No_At_Age_ADJ)
# names(intercatch_canum_bind)[names(intercatch_canum_bind)=="No_At_Age_ADJ"] <- "No_At_Age"
# names(canum_cod)[names(canum_cod)=="frequency1000"] <- "No_At_Age"
# Inter_canum<-rbind(intercatch_canum3,canum_cod)

###Reduce to final columns of interest
intercatch_canum_fin <- intercatch_canum3 %>% select(Year, Country,Area , CatchCat,lvl4,Age,MeanWeight_in_g,  Stock,No_At_Age_ADJ)
names(intercatch_canum_fin)[names(intercatch_canum_fin)=="No_At_Age_ADJ"] <- "No_At_Age"
# 07 _ Write out intercatch summary #####
## three untill we know if cod can be combined on to IC code above commented out
write.taf(intercatch_canum_fin,file.path("results/clean_data/intercatch_canum_summary.csv"))
write.taf(canum_other,file.path("results/clean_data/intercatch_canum_HAD_WHG.csv"))
write.taf(canum_cod,file.path("results/clean_data/intercatch_canum_COD.csv"))
