# Data prep
# Preprocess data,

## Before: Intercatch
## After:  Fillin and save out accessions_catch.csv (data)


# 00_Setup ####
gc()
rm(list = ls())

library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)
Data_path <- "CelticSea/Data_processing/John/Data/bootstrap"

taf.unzip("CelticSea/Data_processing/John/Data/bootstrap/initial/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.zip",
          files="2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv",
          exdir="CelticSea/Data_processing/John/Data/bootstrap/data/ices_intercatch")

#NB gitignore this file as it is too big
intercatch_with_dist <-  read.csv(file = file.path(Data_path,"data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
#intercatch_with_dist_0 <- intercatch_with_dist

intercatch_with_Age_dist <- intercatch_with_dist %>% filter(CANUMType == "Age",Area %in% c("27.7"  ,  "27.7.a" , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ))

#check for missing areas
check <- intercatch_with_dist %>% filter(CANUMType == "Age", !Area %in% c("27.7"  ,  "27.7.a" , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ))
unique(check$Area)
rm(check)

saveRDS(intercatch_with_Age_dist,file=file.path(Data_path,"data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with AGE dist WG 2002 2019.Rdata"))
rm(intercatch_with_dist)


# Just the age distribution -----------------------------------------------
intercatch_with_Age_dist <- readRDS(file.path(Data_path,"data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with AGE dist WG 2002 2019.Rdata"))

intercatch_with_Age_dist$Species <- toupper(substr(intercatch_with_Age_dist$Stock,1,3))
 #create an extra colum for lvl4
intercatch_with_Age_dist$lvl4 <- substr(intercatch_with_Age_dist$Fleet,1,7)

# 03 _ Clean Area and Species ####
# #read in table of areas and species to change
area_spp_fix <- read.csv(file.path(Data_path,"initial/supporting_files/area_recoding.csv"))
names(area_spp_fix) <- c("Area" ,"ICES_mix_correct", "ICES_FU","species_mix_FU" )
area_spp_fix$Area <- as.character(area_spp_fix$Area)

area_spp_fix <- area_spp_fix %>% filter(is.na(area_spp_fix$species_mix_FU)==F)

intercatch_with_Age_dist$Area <- as.character(intercatch_with_Age_dist$Area)



dim(intercatch_with_Age_dist)

new_intercatch <- left_join(intercatch_with_Age_dist,area_spp_fix, by = "Area" )
dim(intercatch_with_Age_dist)[1]-dim(new_intercatch)[1]

##Check for NA values for the coloums added by area_spp_fix
table(is.na(new_intercatch$species_mix_FU[new_intercatch$Species =="NEP"]))
#### why just nep? becuse the area_spp_fix currently only targts neps

# #make columes for cleaning
new_intercatch$Area_keep <- new_intercatch$ICES_mix_correct
new_intercatch$Area_keep <- ifelse(new_intercatch$Species=="NEP",new_intercatch$Area_keep,new_intercatch$Area)

new_intercatch$species_mix_FU <- as.character(new_intercatch$species_mix_FU)
new_intercatch$Species_keep <- ifelse(new_intercatch$Species=="NEP",new_intercatch$species_mix_FU,new_intercatch$Species)


#final usable dataset # pick teh columes that you want to keep 
intercatch_with_Age_dist2<- new_intercatch%>% select(Datayear ,Stock ,Country ,Fleet ,CatchCat ,
                                                  AgeOrLengthDistribution ,CATON_in_kg,Effort,UnitEffort,
                                                  DataUsedInAssessment, lvl4, Area_keep  ,Species_keep,ageorlength,CANUM,MeanWeight_in_g)



names(intercatch_with_Age_dist2) <-  c("Year", "Stock","Country" ,"Fleet" , "CatchCat", "CANUMType" ,"CATON_in_kg",
                                  "Effort" , "UnitEffort" , "DataUsedInAssessment","lvl4", "Area" , "species","Age","No_At_Age","MeanWeight_in_g")



# 04 _ Subset for Celtic Seas Areas ####
intercatch_with_Age_dist2$Area <- as.character(intercatch_with_Age_dist2$Area)
intercatch_with_Age_dist2<- intercatch_with_Age_dist2[substr(intercatch_with_Age_dist2$Area, 1,4) %in% c("27.3", "27.4", "27.6", "27.7", "27.8"),]


#4_5 Check SOP of weight at age and other ------------------------------------
#yes i know you hate vars() Paul
intercatch_with_Age_dist2 <- intercatch_with_Age_dist2 %>% group_by_at(vars(-No_At_Age,-MeanWeight_in_g))%>% mutate(SOP=(as.numeric(No_At_Age)*as.numeric(MeanWeight_in_g)/1000)) %>% ungroup() 

#%>% mutate(diff=unique(CATON_in_kg)-SOP) %>% ungroup()
  IC_AC_SOP_CHECK <- intercatch_with_Age_dist2 %>% group_by_at(vars(-SOP,-No_At_Age,-MeanWeight_in_g,-Age)) %>% mutate(SOP_SUM=sum(SOP)) %>% ungroup() %>% mutate(diff=CATON_in_kg-SOP_SUM)

  table(intercatch_with_Age_dist2$UnitEffort)

#### oh you bastards u wish i had not looked
  ##also the effor is in different units (please god tell me that was picked up on)
  ## and how the fuck do you submit negative number to intercatch




#05 _ Get rid of ages and remove duplicates

Inter_stock_summary<-intercatch_with_Age_dist2


# 06 _ Fix country names ####
table(Inter_stock_summary$Country)

Inter_stock_summary$Country[Inter_stock_summary$Country=="Belgium"] <- "BE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Belgium"]<-"BE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Faroe Islands"]<-"FRO"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Denmark"]<-"DK"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Netherlands"]<-"NL"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Germany"]<-"DE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Ireland"]<-"IE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Norway"]<-"NOR"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Sweden"]<-"SE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Spain"]<-"ES"
Inter_stock_summary$Country[Inter_stock_summary$Country=="France"]<-"FRA"
Inter_stock_summary$Country[Inter_stock_summary$Country %in% c("UK (England)", "UK (Channel Island Guernsey)" ,"UK (Isle of Man)", "UK (Channel Island Jersey)")]<-"UKE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="UK(Northern Ireland)"]<-"UKN"
Inter_stock_summary$Country[Inter_stock_summary$Country=="UK(Scotland)"]<-"UKS"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Estonia"]<-"EE"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Lithuania"]<-"LT"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Poland"]<-"POL"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Finland"]<-"FIN"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Latvia"]<-"LT"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Russia"]<-"RUS"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Greenland"]<-"GRL"
Inter_stock_summary$Country[Inter_stock_summary$Country=="Portugal"]<-"PT"


# 06 _ Cleaning up the metiers
# THis list was calulated externally to this process in excel to match match the
# unique metiers in accessions landings and effort
table(Inter_stock_summary$lvl4)
###CHECK THIS EACH YEAR (you have been warned)

####I have reducded this section to only the actual fixes being applied 
# Misc --------------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 %in% c("C-Allge","MIS")] <- "MIS_MIS"

# dredge ------------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 == "DRB_all"] <- "DRB_MOL"
# Gillnests ---------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 %in% c("GNS", "GNS-DEF")] <- "GNS_DEF"
# Lines -------------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 == "LLS-DEF"] <- "LLS_DEF"
# trawls ------------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 %in% c("TBB")] <- "TBB_DEF"
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 == "PTB_all"] <- "PTB_DEF"
# Otters ------------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 %in% c("OTB")] <- "OTB_DEF"
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 == "OTT-DEF"] <- "OTT_DEF"
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 == "OTT-CRU"] <- "OTT_CRU"
# Sienes----------------------------------------------------------------------
Inter_stock_summary$lvl4[Inter_stock_summary$lvl4 == "SSC"] <- "SCC_DEF"


unique(Inter_stock_summary$Fleet)

# Scaleing the distirbution to Caton_kg -----------------------------------
Inter_stock_summary2 <- Inter_stock_summary %>%  group_by_at(vars(-SOP,-No_At_Age,-MeanWeight_in_g,-Age)) %>% mutate(SOP_SUM=sum(SOP)) %>% ungroup() %>% mutate(diff=CATON_in_kg/SOP_SUM)

## I am going to hazard a guess a say these are IC extraction artifacts 
Inter_stock_summary_INF_MISSING <- Inter_stock_summary2 %>% filter(diff%in% c("Inf","NaN"),No_At_Age == 0)

Inter_stock_summary2 <- Inter_stock_summary2 %>% filter(!diff %in% c("Inf","NaN"),No_At_Age != 0)


Inter_stock_summary2 <- Inter_stock_summary2 %>% mutate(SOP2=SOP*diff,No_At_Age_ADJ=No_At_Age*diff) %>% mutate(SOP3=(No_At_Age_ADJ*as.numeric(MeanWeight_in_g))/1000,prop_weight=SOP2/CATON_in_kg)

###Reduce to final coloumns of interest
names(Inter_stock_summary2)
Inter_stock_summary <- Inter_stock_summary2 %>% select(Year,Stock,Country,Fleet,CatchCat,lvl4,Area,species,Age,No_At_Age_ADJ,MeanWeight_in_g,prop_weight)


# 07 _ Write out intercatch summary ####
write.taf(Inter_stock_summary,file.path(Data_path,"results/clean_data/intercatch_summary_Age.csv"))





























