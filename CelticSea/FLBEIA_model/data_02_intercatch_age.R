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

gc()
rm(list = ls())
library(readxl)
library(tidyr)
library(dplyr)
library(icesTAF)
library(ggplot2)

# 01 - InterCatch CANUM ####
taf.unzip("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.zip", files="2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv", exdir="bootstrap/data/ices_intercatch")
intercatch_canum <-  read.csv(file = file.path("bootstrap/data/ices_intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv"),fileEncoding = "UTF-8-BOM")
#subset for case study area
intercatch_canum <- intercatch_canum %>% filter(Area %in% c("27.7"  , "27.7.b" , "27.7.c","27.7.c.1","27.7.c.2" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j","27.7.j.1","27.7.j.2" , "27.7.k","27.7.k.1","27.7.k.2" ))
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
intercatch_canum <-intercatch_canum [!intercatch_canum$CatchCat %in% c("BMS landing", "Logbook Registered Discard"),]
intercatch_canum<- intercatch_canum%>% select("DataYear" ,"Stock" ,"Country" ,"fleet" ,"CatchCat","Weight_Total_in_kg","lvl4", "Area","Species")
names(intercatch_canum) <-  c("Year", "Stock","Country" ,"Fleet" , "CatchCat", "CATON_in_kg", "lvl4", "Area" , "Species")
intercatch_canum <- intercatch_canum[!intercatch_canum$Species %in% c("COD", "WHG", "HAD"),] #added in below!


# 02 - CATON raised outside InterCatch ####
caton_cod <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_COD_summary.csv")
caton_had <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_HAD_summary.csv")
caton_whg <-  read.csv("bootstrap/data/ices_intercatch/caton_WG_WHG_summary.csv")

# ~ Fix and merge ####
caton_cod$Stock<-"cod.27.7e-k"
caton_had$Stock<-"had.27.7b-k"
caton_whg$Stock<-"whg.27.7b-ce-k"
caton_other<-rbind(caton_cod,caton_had,caton_whg)
names(caton_other)<-c("Year","Country","Area","lvl4","Landings","Discards","Stock")
caton_other$lvl4<-substr(caton_other$lvl4,1,7)

# ~ Calculating discard rates #### 
caton_other <- caton_other %>% group_by(Year, Country, Area, lvl4, Stock) %>% 
  summarise("Landings" = sum(Landings, na.rm=T), "Discards" = sum(Discards, na.rm=T))
caton_other$Catch <- caton_other$Discards +caton_other$Landings
caton_other$DR <- caton_other$Discards/caton_other$Catch

# 03 - Merge data sources and write out

Inter_stock_summary<-rbind(Inter_stock_summary,IC_sum)

write.taf(Inter_stock_summary,file.path(Data_path_out,"clean_data/intercatch_summary.csv"))


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

# Scaling the distirbution to Caton_kg -----------------------------------
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

