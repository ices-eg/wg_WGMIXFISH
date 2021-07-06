# Data prep
# Preprocess data,

## Before: Intercatch
## After:  Fillin and save out accessions_catch.csv (data)


# 00_Setup ####
gc()
rm(list = ls())

library(readxl)
library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)
Data_path <- "CelticSea/bootstrap"
Data_path_out <- "CelticSea/Results"

# taf.unzip("CelticSea/bootstrap/initial/ices_intercatch/2021 06 24 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2020.zip",
#           files="2021 06 24 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2020.csv",
#           exdir="CelticSea/bootstrap/data/ices_intercatch")

#NB gitignore this file as it is too big
intercatch_with_dist <-  read.csv(file = file.path(Data_path,"data/ices_intercatch/2021 06 24 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2020.csv"),fileEncoding = "UTF-8-BOM")
#intercatch_with_dist_0 <- intercatch_with_dist

intercatch_with_dist2 <- intercatch_with_dist %>% filter(Area %in% c("27.7"  , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ))

#check for missing areas
check <- intercatch_with_dist %>% filter(!Area %in% c("27.7"  , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ))
unique(check$Area)
rm(check)


# Apply universal fixed from Lookup tabel ---------------------------------
table(intercatch_with_dist2$Country)
###CHECK THIS EACH YEAR (you have been warned)
intercatch_with_dist <- intercatch_with_dist2

# Create species adn LVL4 metier ---------------------------------------------------------

intercatch_with_dist$Species <- toupper(substr(intercatch_with_dist$Stock,1,3))
#create an extra colum for lvl4
intercatch_with_dist$lvl4 <- substr(intercatch_with_dist$Fleet,1,7)

# Load in Lookups ---------------------------------------------------------

Country_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Country_lookup.xlsx")
lvl4_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Metier_lvl4_lookup.xlsx")
area_spp_fix <- read.csv("CelticSea/FLBEIA_model/lookup/Area_lookup.csv")


##Apply area fix
names(area_spp_fix) <- c("Area" ,"Standard","ICES_mix_correct", "ICES_FU","species_mix_FU" )
area_spp_fix$Area <- as.character(area_spp_fix$Area)

area_spp_fix <- area_spp_fix %>% filter(is.na(area_spp_fix$species_mix_FU)==F)

intercatch_with_dist$Area <- as.character(intercatch_with_dist$Area)



dim(intercatch_with_dist)

new_intercatch <- left_join(intercatch_with_dist,area_spp_fix, by = "Area" )
dim(intercatch_with_dist)[1]-dim(new_intercatch)[1]

##Check for NA values for the coloums added by area_spp_fix
table(is.na(new_intercatch$species_mix_FU[new_intercatch$Species =="NEP"]))
#### why just nep? becuse the area_spp_fix currently only targts neps

# #make columes for cleaning
new_intercatch$Area_keep <- new_intercatch$ICES_mix_correct
new_intercatch$Area_keep <- ifelse(new_intercatch$Species=="NEP",new_intercatch$Area_keep,new_intercatch$Area)

new_intercatch$species_mix_FU <- as.character(new_intercatch$species_mix_FU)
new_intercatch$Species_keep <- ifelse(new_intercatch$Species=="NEP",new_intercatch$species_mix_FU,new_intercatch$Species)

#### apply level 4 fix
names(new_intercatch)
names(lvl4_Lookup)
dim(new_intercatch)
new_intercatch2 <- left_join(new_intercatch,lvl4_Lookup)
dim(new_intercatch2)[1]-dim(new_intercatch)[1]

new_intercatch2$lvl4[is.na(new_intercatch2$Correct_lvl4)==F] <- new_intercatch2$Correct_lvl4[is.na(new_intercatch2$Correct_lvl4)==F]
new_intercatch2 <- new_intercatch2 %>% select(-Correct_lvl4)
table(new_intercatch2$lvl4)
##dims should match

table(new_intercatch2$lvl4)
dim(new_intercatch2)
new_intercatch3 <- left_join(new_intercatch2,Country_Lookup)
dim(new_intercatch3)-dim(new_intercatch3)
#dims should match

new_intercatch3$Country[is.na(new_intercatch3$CorrectCountry)==F] <- new_intercatch3$CorrectCountry[is.na(new_intercatch3$CorrectCountry)==F]
new_intercatch3 <- new_intercatch3 %>% select(-CorrectCountry)
table(new_intercatch3$Country)


# Save fixed file when done -----------------------------------------------
saveRDS(new_intercatch3,file=file.path(Data_path,"data/ices_intercatch/2021 06 24 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2020.Rdata"))




rm(list=ls()[!ls() %in% c("Data_path","Data_path_out")])
gc()
Data_path <- "CelticSea/bootstrap"
Data_path_out <- "CelticSea/Results"
#You should be able to run from here if you have done the above
intercatch_with_dist <- readRDS(file.path(Data_path,"data/ices_intercatch/2021 06 24 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2020.Rdata"))

# DISCARD RATES -----------------------------------------------------------
#names

intercatch_with_dist <-intercatch_with_dist %>%  filter(CANUMType =="Lngt")

intercatch_with_dist<- intercatch_with_dist%>% select("Datayear" ,"Stock" ,"Country" ,"Fleet" ,"CatchCat","CANUMType" ,"CATON_in_kg","Effort","UnitEffort","DataUsedInAssessment", "lvl4", "Area_keep"  ,"Species_keep")


names(intercatch_with_dist) <-  c("Year", "Stock","Country" ,"Fleet" , "CatchCat", "CANUMType" ,"CATON_in_kg","Effort" , "UnitEffort" , "DataUsedInAssessment","lvl4", "Area" , "Species")


#addtional data not provided as part of IC FILE 
IC_cod <-  read.csv(file.path(Data_path,"data/ices_intercatch/caton_WG_COD_summary.csv"))
IC_had <-  read.csv(file.path(Data_path,"data/ices_intercatch/caton_WG_HAD_summary.csv"))
IC_whg <-  read.csv(file.path(Data_path,"data/ices_intercatch/caton_WG_WHG_summary.csv"))


IC_cod$Stock<-"cod.27.7e-k"
IC_had$Stock<-"had.27.7b-k"
IC_whg$Stock<-"whg.27.7b-ce-k"
IC_rep<-rbind(IC_cod,IC_had,IC_whg)
IC_rep$em1<-NA
IC_rep$em2<-NA
colnames(IC_rep)<-c("Year","Country","Area","lvl4","Landings","Discards","Stock","BMS landing","Logbook Registered Discard")
IC_rep$Catch<-IC_rep$Landings+IC_rep$Discards
IC_rep$lvl4<-substr(IC_rep$lvl4,1,7)
IC_rep$Area<-substr(IC_rep$Area,1,6)


IC_sum<-aggregate(list(BMS=NA,Catch=IC_rep$Catch,Discards=IC_rep$Discards,Landings=IC_rep$Landings,REP=NA),by=list(Stock=IC_rep$Stock,Country=IC_rep$Country,lvl4=IC_rep$lvl4,Area=IC_rep$Area,Year=IC_rep$Year),sum,na.rm=T)
IC_sum$BMS<-NA
IC_sum$REP<-NA
colnames(IC_sum)[6]<-"BMS landing"
colnames(IC_sum)[10]<-"Logbook Registered Discard"
IC_sum$DR<-IC_sum$Discards/IC_sum$Catch

# Summarise the discard table
# Discard summary by lvl4
Inter_stock_summary<- intercatch_with_dist %>%  group_by(Year,Stock,Country,Area,lvl4,CatchCat) %>%
  dplyr::summarise(CATON_in_kg=sum(CATON_in_kg, na.rm=TRUE))%>% group_by(Stock,Country,lvl4,Area,Year,CatchCat) %>%
  dplyr::summarise(CATON_in_kg=sum(CATON_in_kg)) %>% ungroup() %>%
  tidyr::spread(CatchCat,CATON_in_kg)
Inter_stock_summary$Catch<-rowSums(Inter_stock_summary[c("Discards","Landings")],na.rm=T)
Inter_stock_summary$DR<-Inter_stock_summary$Discards/Inter_stock_summary$Catch

Inter_stock_summary<-rbind(Inter_stock_summary,IC_sum)


# Adjustments and corrections to DR  --------------------------------------
### currently blank but may fill up in time 


# Write out intercatch summary ####
write.taf(Inter_stock_summary,file.path(Data_path_out,"clean_data/intercatch_summary.csv"))


rm(list=ls()[!ls() %in% c("Data_path","Data_path_out")])
gc()
Data_path <- "CelticSea/bootstrap"
Data_path_out <- "CelticSea/Results"
# Just the age distribution -----------------------------------------------
intercatch_with_Age_dist <- readRDS(file.path(Data_path,"data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with AGE dist WG 2002 2019.Rdata"))

intercatch_with_Age_dist <- intercatch_with_Age_dist %>% filter(CANUMType=="Age")


#final usable dataset # pick teh columes that you want to keep 
intercatch_with_Age_dist2<- intercatch_with_Age_dist%>% select(Datayear ,Stock ,Country ,Fleet ,CatchCat ,
                                                     AgeOrLengthDistribution ,CATON_in_kg,Effort,UnitEffort,
                                                     DataUsedInAssessment, lvl4, Area_keep  ,Species_keep,ageorlength,CANUM,MeanWeight_in_g)



names(intercatch_with_Age_dist2) <-  c("Year", "Stock","Country" ,"Fleet" , "CatchCat", "CANUMType" ,"CATON_in_kg",
                                       "Effort" , "UnitEffort" , "DataUsedInAssessment","lvl4", "Area" , "species","Age","No_At_Age","MeanWeight_in_g")



# Check SOP of weight at age and other ------------------------------------
#yes i know you hate vars() Paul
intercatch_with_Age_dist2 <- intercatch_with_Age_dist2 %>% group_by_at(vars(-No_At_Age,-MeanWeight_in_g))%>% mutate(SOP=(as.numeric(No_At_Age)*as.numeric(MeanWeight_in_g)/1000)) %>% ungroup() 

#%>% mutate(diff=unique(CATON_in_kg)-SOP) %>% ungroup()
IC_AC_SOP_CHECK <- intercatch_with_Age_dist2 %>% group_by_at(vars(-SOP,-No_At_Age,-MeanWeight_in_g,-Age)) %>% mutate(SOP_SUM=sum(SOP)) %>% ungroup() %>% mutate(diff=CATON_in_kg-SOP_SUM)

table(intercatch_with_Age_dist2$UnitEffort)


#05 _ Get rid of ages and remove duplicates

Inter_stock_summary<-intercatch_with_Age_dist2

unique(Inter_stock_summary$Fleet)

# Scaleing the distirbution to Caton_kg -----------------------------------
Inter_stock_summary2 <- Inter_stock_summary %>%  group_by_at(vars(-SOP,-No_At_Age,-MeanWeight_in_g,-Age)) %>% mutate(SOP_SUM=sum(SOP)) %>% ungroup() %>% mutate(diff=CATON_in_kg/SOP_SUM)

## I am going to hazard a guess a say these are IC extraction artifacts 
Inter_stock_summary_INF_MISSING <- Inter_stock_summary2 %>% filter(diff%in% c("Inf","NaN"),No_At_Age == 0)
write.taf(Inter_stock_summary_INF_MISSING,file.path(Data_path_out,"Intermediate_products/Inf_NaN_Zeros_age.csv"))


Inter_stock_summary2 <- Inter_stock_summary2 %>% filter(!diff %in% c("Inf","NaN"),No_At_Age != 0)


Inter_stock_summary2 <- Inter_stock_summary2 %>% mutate(SOP2=SOP*diff,No_At_Age_ADJ=No_At_Age*diff) %>% mutate(SOP3=(No_At_Age_ADJ*as.numeric(MeanWeight_in_g))/1000)


Inter_stock_summary3 <- Inter_stock_summary2 %>% select(Year,Stock,Country,Fleet,CatchCat,lvl4,Area,species,Age,CATON_in_kg,No_At_Age_ADJ,MeanWeight_in_g,SOP3) %>%  group_by_at(vars(-No_At_Age_ADJ,-SOP3,-CATON_in_kg)) %>% summarise(CATON_in_kg=sum(CATON_in_kg,na.rm=T),No_At_Age_ADJ=sum(No_At_Age_ADJ,na.rm = T),SOP3=sum(SOP3,na.rm = T)) %>% mutate(prop_weight=SOP3/CATON_in_kg) %>% ungroup()



###Reduce to final coloumns of interest
names(Inter_stock_summary3)
Inter_stock_summary <- Inter_stock_summary3 %>% select(Year,Stock,Country,Fleet,CatchCat,lvl4,Area,species,Age,CATON_in_kg,No_At_Age_ADJ,MeanWeight_in_g,SOP3,prop_weight)


# 07 _ Write out intercatch summary ####
write.taf(Inter_stock_summary,file.path(Data_path_out,"clean_data/intercatch_summary_Age.csv"))

