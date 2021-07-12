gc()
#THERE IS AN RM HERE 
##
rm(list=ls())
## forgive me father for i have sinned and wiped the GE 
library(pander)
library(knitr)
library(tidyverse)
library(FLCore)
library(FLFleet)
library(Hmisc)

# Purpose -----------------------------------------------------------------
# This script is to process avalible effort, catch and logbook data from
# disparrate sources into something useable and format the data into 
# fleet object. Considered this section similar to DCF processing for 
# ICES submission 


# Datapaths ----------------------------------------------------------------
Data_path <- "results"
LookupPath <- "bootstrap"
BootstrapPath <- "bootstrap"
# #1.2 Set parameters -----------------------------------------------------
Yearwg<-2020
# MR change to last three Year
Year<-(Yearwg-3):(Yearwg-1)
PERCENTAGE <- "0.01"
FLEET_PERCENTAGE <- "0.01"
## NOTE: Have these read from Reproduce the advice output
# Nephrops setting.... T for including Nephrops, else F
nep <- TRUE
nep_latest <- TRUE


# # Tiered demersal spp --------------------------------------------------
tier_1 <- TRUE # cod, haddock, whiting
tier_2 <- TRUE # sole 7fg, monkfish, megrim
tier_3 <- FALSE # sole 7e, hake

# Landings obligation settings!! - do we want to forecast catch, no discards?
LO <- TRUE

# Making lists


# # 2020 shortlist --------------------------------------------------------
tier_1stk <- c("cod.27.7e-k", "had.27.7b-k", "whg.27.7b-ce-k")
tier_2stk <- c("sol.27.7fg", "mon.27.78abd", "meg.27.7b-k8abd")
tier_3stk <- c("sol.27.7e", "hke.27.3a46-8abd")
tier_IC_1 <- tier_1stk
tier_IC_2 <- tier_2stk
tier_IC_3 <- tier_3stk
tier_IC_nep<-c("NEP.FU.16","NEP.FU.17","NEP.FU.19","NEP.FU.2021" ,"NEP.FU.22", "NEP.OUT.7")



add_Nep<-nep

## For file names
options_dem <- ifelse(tier_1 & !tier_2 & !tier_3, "tier1",
                      ifelse(!tier_1 & tier_2 & tier_3, "tier2",
                             ifelse(!tier_1 & !tier_2 & tier_3, "tier3",
                                    ifelse(tier_1 & tier_2 & tier_3, "tier123",
                                           ifelse(tier_1 & tier_2 & !tier_3, "tier12",
                                                  ifelse(tier_1 & !tier_2 & tier_3, "tier13",
                                                         ifelse(!tier_1 & tier_2 & tier_3, "tier23",
                                                                "")))))))

options_nep <- ifelse(nep & nep_latest, "nepnew",
                      ifelse(nep & !nep_latest, "nepold",
                             ""))


# # Use SAM? replace the FLR STF with a SAM STF median values -------------
# Note, these are incompatible
UseSAM   <- TRUE
UseFwdF3 <- FALSE

options_lo  <- ifelse(LO, "LO", "")
options_SAM <- ifelse(UseSAM, "UsingSAM", ifelse(UseFwdF3, "UsingFwdF3",""))                   
options <- paste0(options_dem, options_nep, options_lo, options_SAM)
save(ver,options, file = "results/clean_data/tmp.RData")


sp.lst<-NULL

Sps<-NULL

ICs<-NULL


if (tier_1) {
  
  sp.lst 	<- c(sp.lst,paste(c("cod.27.7e-k", "had.27.7b-k", "whg.27.7b-ce-k")))
  Sps	<- c(Sps,c("COD","HAD","WHG"))
  #	sp.lst<-"cod.27.7e-k"
  #	SPS<-"COD"
  ICs	<- c(ICs,tier_IC_1)
}


if (tier_2) {
  
  sp.lst 	<- c(sp.lst,tier_2stk)
  Sps	<- c(Sps,c("SOL","MON","ANK","ANF","MEG","LEZ")) # 2020
  ICs	<- c(ICs,tier_IC_2)
}

if (tier_3) {
  
  sp.lst 	<- c(sp.lst,tier_3stk)
  Sps	<- c(Sps,c("SOL","HKE")) # this might need to be changed to this "sol.27.7e","sol.27.7fg"
  # to recode like for nephrops with SOL*area
  ICs	<- c(ICs,tier_IC_3)
}

if (add_Nep) {
  
  sp.lst 	<- c(sp.lst,"NEP.FU.16","NEP.FU.17","NEP.FU.19","NEP.FU.2021","NEP.FU.22", "NEP.OUT.7")
  Sps	<- c(Sps, "NEP.FU.16", "NEP.FU.17", "NEP.FU.19", "NEP.FU.2021", "NEP.FU.22", "NEP.OUT.7")
  ICs	<- c(ICs,tier_IC_nep)
}


# Print species selection -------------------------------------------------
# Are we okay with the selection of species ?

print(sp.lst)

print(Sps)

# Read in data ------------------------------------------------------------
#actual data
InterCatch <- read.csv(file.path(Data_path,"clean_data/intercatch_caton_summary.csv"))
InterCatch_age <- read.csv(file.path(Data_path,"clean_data/intercatch_canum_summary.csv"))
## Data that has been matched 
### data has also been filterede to Yearwg-3 and summarise so yes it is smaller 
## this is all done at the end of data_01
catch_start <-read.csv(file.path(Data_path,"clean_data/MATCHED_clean_accessions_landings.csv"))
effort_start <-read.csv(file.path(Data_path,"clean_data/MATCHED_clean_accessions_effort.csv"))
##Can also read in the data that could not be matched but need to decide wht to do with it


# # all data including unmatched
# catch_start <-read.csv(file.path(Data_path,"clean_data/clean_accessions_landings.csv"))
# effort_start <-read.csv(file.path(Data_path,"clean_data/clean_accessions_effort.csv"))
#stock data
Stock_lookup <- read.csv(file.path(LookupPath,"data/supporting_files/Stock_lookup.csv"))
nep_data <- read.csv(file.path(BootstrapPath,"data/submitted_stock_objects/WGCSE/nep.all/nep.stock.wgmixfish_2020.csv"))

#allows us to keep the orignal object in GE for specific checks 
catch <- catch_start
effort<- effort_start

dim(catch_start)
dim(catch_start)
catch<- catch[catch$Area %in% c("27.7"  ,   "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]
effort<- effort[effort$Area %in% c("27.7"  , "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]
dim(catch)
dim(catch)

# #1.4 Clean Country Names ------------------------------------------------
# select the last three Year
# effort<- subset(effort,Year %in% c(Yearwg-3,Yearwg-2,Yearwg-1))
# catch<- subset(catch,Year %in% c(Yearwg-3,Yearwg-2,Yearwg-1))

# Combine catch and effort ------------------------------------------------
catch2 <- catch
effort2 <- effort

catch2 <- catch2 %>% group_by_at(vars(-Landings,-Value)) %>% summarise(Landings=sum(Landings),Value=sum(Value)) %>% ungroup()
effort2 <- effort2 %>% group_by_at(vars(-kw_days,-Days_at_sea,-No_vessels)) %>% summarise(kw_days=sum(kw_days),Days_at_sea=sum(Days_at_sea),No_vessels=sum(No_vessels)) %>% ungroup()

catch2 <- catch2 %>% mutate(check=1:nrow(catch2))

# Processing catch data ---------------------------------------------------
Catch3 <- catch2
effort3 <- effort2

# #1.5 Clean species names ------------------------------------------------
Catch3 <-subset(Catch3,Species %in% Sps)

InterCatch <- InterCatch[InterCatch$Stock %in% tolower(sp.lst),]
InterCatch$Species <- toupper(substr(InterCatch$Stock, 1, 3))
InterCatch$Species <- ifelse(InterCatch$Species =="NEP", toupper(InterCatch$Stock),InterCatch$Species)

# ##Create discard ID in catch and effrot ---------------------------------
###all
###Remove area
###Remove metier keep area
### Just year country and species!!:
InterCatch$Discard_ID <- paste( InterCatch$Year, InterCatch$Country, InterCatch$Species, InterCatch$Area, InterCatch$lvl4, sep = "_")
InterCatch$Discard_ID_NO_AREA <- paste( InterCatch$Year, InterCatch$Country, InterCatch$Species,  InterCatch$lvl4, sep = "_")
InterCatch$Discard_ID_NO_METIER <- paste( InterCatch$Year, InterCatch$Country, InterCatch$Species,  InterCatch$Area, sep = "_")
InterCatch$Discard_ID_YEAR_COUNTRY <- paste( InterCatch$Year, InterCatch$Country,  sep = "_")

Catch3$Discard_ID <- paste(Catch3$Year, Catch3$Country, Catch3$Species, Catch3$Area, Catch3$Metier, sep = "_")
Catch3$Discard_ID_NO_AREA <- paste(Catch3$Year, Catch3$Country, Catch3$Species, Catch3$Metier, sep = "_")
Catch3$Discard_ID_NO_METIER <- paste(Catch3$Year, Catch3$Country, Catch3$Species,  Catch3$Area, sep = "_")
Catch3$Discard_ID_YEAR_COUNTRY <- paste(Catch3$Year, Catch3$Country,  sep = "_")
#selct dicard data
discard_dat <- InterCatch %>% select(Discard_ID, DR,Landings)
names(discard_dat)[names(discard_dat)=="Landings"] <- "IC_Landings"
#one for area
discard_dat_NO_AREA <- InterCatch %>% select(Discard_ID_NO_AREA, DR,Landings)
names(discard_dat_NO_AREA)[names(discard_dat_NO_AREA)=="Landings"] <- "IC_Landings"
###one for metier
discard_dat_NO_METIER <- InterCatch %>% select(Discard_ID_NO_METIER, DR,Landings)
names(discard_dat_NO_AREA)[names(discard_dat_NO_AREA)=="Landings"] <- "IC_Landings"
###one for Year adn country
discard_dat_YEAR_COUNTRY <- InterCatch %>% select(Discard_ID_YEAR_COUNTRY, DR,Landings)
names(discard_dat_YEAR_COUNTRY)[names(discard_dat_YEAR_COUNTRY)=="Landings"] <- "IC_Landings"
### so this line is takeing the maximum discard rate do we need this to be weighed?
discard_dat<- discard_dat %>% group_by(Discard_ID) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
#no araa
discard_dat_NO_AREA<- discard_dat_NO_AREA %>% group_by(Discard_ID_NO_AREA) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
##noMetier
#no araa
discard_dat_NO_METIER<- discard_dat_NO_METIER %>% group_by(Discard_ID_NO_METIER) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
#Year and country
discard_dat_YEAR_COUNTRY<- discard_dat_YEAR_COUNTRY %>% group_by(discard_dat_YEAR_COUNTRY) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
# Join discard and catch --------------------------------------------------
sum(Catch3$Landings)

dim(Catch3)
Catch4 <- left_join(Catch3, discard_dat, by = "Discard_ID") # anything that is here as NA is unknown
dim(Catch4)

sum(Catch4$Landings[is.na(Catch4$DR)==F])
sum(unique(Catch4$IC_Landings),na.rm = T)/1000
table(is.na(Catch4$DR))


Catch4_DR <- Catch4 %>% filter(is.na(DR)==F)
Catch4_DR_NA <-Catch4 %>% filter(is.na(DR)==T) 

# DR removeing area -------------------------------------------
Catch4_DR_NA <-Catch4_DR_NA %>%  select(-IC_Landings,-DR)

dim(Catch4_DR_NA)
Catch5<- left_join(Catch4_DR_NA,discard_dat_NO_AREA)
dim(Catch5)[1]-(dim(Catch4_DR_NA)[1])
##check na
table(is.na(Catch5$DR))

Catch5_DR <- Catch5 %>% filter(is.na(DR)==F)
Catch5_DR_NA <-Catch5 %>% filter(is.na(DR)==T) 
# DR removeing gear but keeping area -------------------------------------------
Catch5_DR_NA <-Catch5_DR_NA %>%  select(-IC_Landings,-DR)

dim(Catch5_DR_NA)
Catch6<- left_join(Catch5_DR_NA,discard_dat_NO_METIER)
dim(Catch6)[1]-(dim(Catch5_DR_NA)[1])
##check na
table(is.na(Catch6$DR))


Catch6_DR <- Catch6 %>% filter(is.na(DR)==F)
Catch6_DR_NA <-Catch6 %>% filter(is.na(DR)==T) 
# Put it all back together ------------------------------------------------
Catch6_DR_NA$DR <- 0  ##'*So this is a major assumption and needs to be checked*

names(Catch4_DR)
names(Catch5_DR)
names(Catch6_DR)
names(Catch6_DR_NA)

# Catch6_DR_NA <- Catch6_DR_NA mutate()

setdiff(names(Catch4_DR),names(Catch5_DR))
setdiff(names(Catch4_DR),names(Catch6_DR))

Catch4_DR <- select(Catch4_DR,-IC_Landings)
Catch5_DR <- select(Catch5_DR,-IC_Landings)

Catch7 <- rbind(Catch4_DR,Catch5_DR,Catch6_DR)

sum(Catch3$Landings)-(sum(Catch7$Landings)+sum(Catch6_DR_NA$Landings))


# 
# Catch_Check <- Catch3 %>% select(Discard_ID,Landings) %>% group_by(Discard_ID) %>% summarise(Landings=sum(Landings)) %>% ungroup()
# IC_Check <- InterCatch %>% select(Discard_ID,Country,Year,Landings,DR) %>% group_by(Discard_ID,Country,Year) %>% summarise(IC_Landings=sum(Landings),DR = max(DR,na.rm = T)) %>% ungroup()
# 
# Catch_IC_Check <- full_join(Catch_Check,IC_Check)
# 
# 
# Problems <- Catch_IC_Check %>% filter(is.na(Landings)==T)
# Catch_IC_Match <- Catch_IC_Check %>% filter(is.na(Landings)==F)
# dim(Catch_IC_Check)[1]-(dim(Catch_IC_Match)[1]+dim(Problems)[1])
# 
# Catch_Check2 <- Catch_Check %>% filter(!Discard_ID %in% Catch_IC_Match$Discard_ID)
# 
# 
# Problem <- discard_dat %>% filter(Discard_ID %in% c(Problems$Discard_ID))


#### ok so this is out untill we decide how to handle NA values and NaNs 
### which will be done in the processing scripts
#Catch3$DR[is.na(Catch3$DR)] <- 0 # this

# Join stock ID to Catch3 --------------------------------------------------
dim(Catch3)
Catch3 <- left_join(Catch3,Stock_Lookup, by = c("Area", "Species"))
dim(Catch3)

# #subset out lines without stock -----------------------------------------
Catch3<- Catch3[!is.na(Catch3$Stock),]


# Produce Catch3 by country from Catch3 df-------------------------------------------------------------
summa <- Catch3 %>% select(Landings,Country,Year,Stock) %>% group_by_at(vars(-Landings)) %>% summarise(Landings=sum(Landings,na.rm = T)) %>% ungroup()

summa<-summa[,c(3,2,1,4)]
summafleet<-aggregate(list(Landings=Catch3$Landings),by=list(Discard_ID=Catch3$Discard_ID, Year=Catch3$Year,   Stock=Catch3$Stock),sum)
write.csv(summa,file=file.path(Data_path,paste("/intermediate_products/catch_per_country_", options,".csv")))


Catch3<-mutate(Catch3,Discards=(Landings/(1-DR)-Landings)) %>%  select(Country,Year,Quarter,Metier,Vessel_length,Area,Species,Stock, DR,Landings,Discards,Value)


# #Assigning a fleet ------------------------------------------------------

Catch3$Potential_fleets <- substr(Catch3$Metier, 1,3)
Catch3$GeneralGrouping <- NA
Catch3$GeneralGrouping <- ifelse(Catch3$Potential_fleets %in% c("LLS", "LHP", "GNS", "FPO", "GTR", "GNC", "LHM", "LLD", "LTL", "GND", "GTN"), "Static", Catch3$GeneralGrouping)
Catch3$GeneralGrouping <- ifelse(Catch3$Potential_fleets %in% c("TBB"), "Beam", Catch3$GeneralGrouping)
Catch3$GeneralGrouping <- ifelse(Catch3$Potential_fleets %in% c("SSC", "PTM", "PTB", "SDN", "OTT", "OTM", "OTB"), "Otter", Catch3$GeneralGrouping)
Catch3$GeneralGrouping <- ifelse(Catch3$Potential_fleets %in% c("DRB", "PS" ,"PS_", "MIS", "HMD", "SPR"), "Other", Catch3$GeneralGrouping)

effort2$Potential_fleets <- substr(effort2$Metier, 1,3)
effort2$GeneralGrouping <- NA
effort2$GeneralGrouping <- ifelse(effort2$Potential_fleets %in% c("LLS", "LHP", "GNS", "FPO", "GTR", "GNC", "LHM", "LLD", "LTL", "GND", "GTN"), "Static", effort2$GeneralGrouping)
effort2$GeneralGrouping <- ifelse(effort2$Potential_fleets %in% c("TBB"), "Beam", effort2$GeneralGrouping)
effort2$GeneralGrouping <- ifelse(effort2$Potential_fleets %in% c("SSC", "PTM", "PTB", "SDN", "OTT", "OTM", "OTB"), "Otter", effort2$GeneralGrouping)
effort2$GeneralGrouping <- ifelse(effort2$Potential_fleets %in% c("DRB", "PS" ,"PS_", "MIS", "HMD", "SPR"), "Other", effort2$GeneralGrouping)


# #Creates country specific fleet -----------------------------------------
Catch3$Fleet<-paste(Catch3$Country,Catch3$GeneralGrouping,Catch3$Vessel_length,sep="_") 
Catch3$Metier <- paste(Catch3$Metier, Catch3$Area, sep= "_") 

effort2$Fleet<-paste(effort2$Country,effort2$GeneralGrouping,effort2$Vessel_length,sep="_") 
effort2$Metier <- paste(effort2$Metier, effort2$Area, sep= "_") 

# Now aggregate to make sure
Catch4<- Catch3 %>% group_by(Fleet,Metier, Vessel_length, Year,Area,Quarter,Species,Stock) %>%  
  summarise("Landings"=sum(Landings,na.rm=T),
            "Discards"=sum(Discards,na.rm=T),
            "Value" = sum(Value, na.rm=T)) %>% ungroup()

# Now aggregate to make sure
effort3 <- effort2 %>% group_by(Fleet,Metier, Vessel_length, Year,Area,Quarter) %>%dplyr::summarise("kw_days"=sum(kw_days,na.rm=T),"Days_at_sea"=sum(Days_at_sea,na.rm=T)) %>% ungroup()

# Section off the above for checking  -------------------------------------
Effort_FIN <- effort3

Catch_FIN <- Catch4

# year cons ---------------------------------------------------------------

yr.cons <- Yearwg -1 

ca_agg<- Catch_FIN %>% select(Stock,Year,Landings) %>% group_by_at(vars(-Landings)) %>% summarise(Landings=sum(Landings,na.rm = T)) %>% ungroup()
ca_agg <- ca_agg %>% filter(Year==yr.cons)

### ok so this is createing a value == to 1% of the landings for a fleet in a year.
ca_agg$pc<-as.numeric(as.numeric(FLEET_PERCENTAGE))*ca_agg$Landings


## by fleet

ca_agg_fl <- Catch_FIN %>% group_by(Fleet,Stock,Year) %>% summarise(Landings=sum(Landings,na.rm = T)) %>% ungroup()
ca_agg_fl <- ca_agg_fl %>% filter(Year==yr.cons)
ca_agg_fl$thres<-ca_agg$pc[match(ca_agg_fl$Stock,ca_agg$Stock)]

ca_agg_fl$keep<-ca_agg_fl$Landings>ca_agg_fl$thres # above threshold?
ca_agg_fl <- ca_agg_fl[!is.na(ca_agg_fl$Stock),]

rel<-ca_agg_fl %>% select(Fleet,keep) %>% filter(keep==TRUE) %>% unique()


# apply levels to catch effort --------------------------------------------
Catch_FIN$Fleet[!(Catch_FIN$Fleet %in% rel$Fleet)]<-"OTH_OTH" 
Effort_FIN$Fleet[!(Effort_FIN$Fleet %in% rel$Fleet)]<-"OTH_OTH"
# # summarise the 2019 landings for each fleet and Stock ------------------
ca2<- Catch_FIN %>% group_by(Fleet, Metier, Year)%>% summarise(Landings = sum(Landings, na.rm=T), Discards = sum(Discards, na.rm=T))


ca2 <- ca2 %>% group_by(Fleet,Year) %>% mutate(pc=Landings/sum(Landings,na.rm=T)) %>% ungroup() 


ca2$OTH[(ca2$pc<as.numeric(PERCENTAGE))]<-"OTH" # less than % of fleets catch 
ca2$OTH[(ca2$pc>=as.numeric(PERCENTAGE))]<-"NotOTH"

ca2 <- ca2 %>% group_by(Year,Fleet,Metier,OTH) %>% summarise_all(~.) %>% ungroup()

ca3 <- select(ca2, Year,Fleet,Metier,OTH)
dim(Catch_FIN)
Catch_FIN <- left_join(Catch_FIN,ca3)
dim(Catch_FIN)
dim(Effort_FIN)
Effort_FIN <- left_join(Effort_FIN,ca3)
dim(Effort_FIN)


Catch_FIN$Metier[Catch_FIN$OTH=="OTH"] <- "OTH_OTH"
Effort_FIN$Metier[Effort_FIN$OTH=="OTH"] <- "OTH_OTH"



# Final steps -------------------------------------------------------------
catch <- Catch_FIN %>%   ungroup() %>%    select(Discards,Landings,Value,Fleet,Metier,Year,Stock) %>%  group_by(Fleet,Metier,Year,Stock)%>%
  summarise(Discards=sum(Discards),Landings=sum(Landings),Value=sum(Value)) %>%  ungroup()

#effort<-aggregate(effort[c("kw_days")],by=list(Fleet=effort$Fleet,Metier=effort$Metier,Year=effort$Year),sum,na.rm=T)
effort <- Effort_FIN %>%ungroup() %>%  select(kw_days,Fleet,Metier,Year) %>%
  group_by(Fleet,Metier,Year) %>% 
  summarise(kw_days=sum(kw_days)) %>% 
  ungroup()

#sanity check
sort(unique(catch$Fleet))
sort(unique(effort$Fleet))

write.taf(catch,file = file.path(Data_path,"clean_data/Catch_4_Makefleets.csv"))
write.taf(effort,file = file.path(Data_path,"clean_data/Effort_4_Makefleets.csv"))

