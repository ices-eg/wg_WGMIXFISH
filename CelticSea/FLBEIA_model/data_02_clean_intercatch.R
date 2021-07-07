# Data prep
# Preprocess data,

## Before: InterCatch extraction, data raised external to InterCatch, and ALK's
## After:  Discards rates and age structure 

# Notes: User must make sure that:  
# 1 - factors are turned to characters, 
# 2 - dim of data frame does not change shape during cleaning process


# Setup ####
gc()
rm(list = ls())
library(readxl)
library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)

# Calculating Discard Rates ####
# Data sources vary per stock: 
#       - InterCatch CATON with distribution: meg.27.7b-k8abd, sol.27.7fg
#       - InterCatch CATON without distribution: mon.27.78abd
#       - Raised outside InterCatch: cod.27.7e-k, had.27.7b-k, whg.27.7b-ce-k, 
#       - Supplied by NEP expert: nep.fu.16, nep.fu.17, nep.fu.19, nep.fu.2021, nep.fu.22, nep.out.7


# 01 - InterCatch CATON ####

# with distributions 
taf.unzip("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON Stocks with distributions all WG 2002  2019.zip", files="2020 06 22 WGMIXFISH CATON stocks with distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
#NB gitignore this file as it is too big
intercatch_caton <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")

# without distributions 
taf.unzip("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON stocks withOUT distributions all WG 2002 2019.zip", files="2020 06 22 WGMIXFISH CATON stocks withOUT distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
#NB gitignore this file as it is too big
intercatch_caton_no_dist <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2020 06 22 WGMIXFISH CATON stocks withOUT distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
names(intercatch_caton_no_dist) <- names(intercatch_caton)
intercatch_caton <- rbind(intercatch_caton_no_dist, intercatch_caton)

#subset for case study area
intercatch_caton <- intercatch_caton %>% filter(Area %in% c("27.7"  , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ))
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

# ~ Métier level 4 fix ####
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
intercatch_caton <- intercatch_caton[!intercatch_caton$Species %in% c("COD", "WHG", "HAD"),] #added in below!

# 02 - CATON raised outside InterCatch ####
caton_cod <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_COD_summary.csv")
caton_had <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_HAD_summary.csv")
caton_whg <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_WHG_summary.csv")

# ~ fix and merge ####
caton_cod$Stock<-"cod.27.7e-k"
caton_had$Stock<-"had.27.7b-k"
caton_whg$Stock<-"whg.27.7b-ce-k"
caton_other<-rbind(caton_cod,caton_had,caton_whg)
names(caton_other)<-c("Year","Country","Area","lvl4","Landings","Discards","Stock")
caton_other$lvl4<-substr(caton_other$lvl4,1,7)

# ~ calculating discard rates #### 
caton_other <- caton_other %>% group_by(Year, Country, Area, lvl4, Stock) %>% 
  summarise("Landings" = sum(Landings, na.rm=T), "Discards" = sum(Discards, na.rm=T))
caton_other$Catch <- caton_other$Discards +caton_other$Landings
caton_other$DR <- caton_other$Discards/caton_other$Catch

# Summarise the discard table
# Discard summary by lvl4
Inter_stock_summary<- intercatch_with_dist %>%  group_by(Year,Stock,Country,Area,lvl4,CatchCat) %>%
  dplyr::summarise(CATON_in_kg=sum(CATON_in_kg, na.rm=TRUE))%>% group_by(Stock,Country,lvl4,Area,Year,CatchCat) %>%
  dplyr::summarise(CATON_in_kg=sum(CATON_in_kg)) %>% ungroup() %>%
  tidyr::spread(CatchCat,CATON_in_kg)
Inter_stock_summary$Catch<-rowSums(Inter_stock_summary[c("Discards","Landings")],na.rm=T)
Inter_stock_summary$DR<-Inter_stock_summary$Discards/Inter_stock_summary$Catch

Inter_stock_summary<-rbind(Inter_stock_summary,IC_sum)

# 05   Creating Discard Rates ####
# Adjustments and corrections to DR  --------------------------------------
### currently blank but may fill up in time 


# Write out intercatch summary ####
write.taf(Inter_stock_summary,file.path(Data_path_out,"clean_data/intercatch_summary.csv"))


rm(list=ls()[!ls() %in% c("Data_path","Data_path_out")])
gc()




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

