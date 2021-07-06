# Data prep
# Preprocess data,

## Before: accessions landings from git
## After:  accession_landings.csv (data)


# 00_Setup ####

rm(list = ls())
gc()

library(tidyverse)
library(readxl)
#library(ggplot2)
library(icesTAF)
#library(dplyr)
#library(tidyr)

# 01 _ Notes ####
# This script is to clean the final year of the "old accessions" data
Data_path <- "CelticSea/bootstrap"
Data_path_out <- "CelticSea/Results"

# 02 _ Read in data ####
load(file.path(Data_path,"/initial/wgmixfish_accessions/catch_2020.Rdata"))
load(file.path(Data_path,"initial/wgmixfish_accessions/catchHAD.Rdata"))

# lookup tables -----------------------------------------------------------
area_spp_fix <- read.csv("CelticSea/FLBEIA_model/lookup/Area_lookup.csv")
lvl4_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Metier_lvl4_lookup.xlsx")
Vessel_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Vessel_length_lookup.xlsx")
Quarter_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Quarter_lookup.xlsx")
Stock_lookup <- read.csv("CelticSea/FLBEIA_model/lookup/Stock_lookup.csv")
#provision for adding fleets
#Fleet_lookup <- read.csv("CelticSea/FLBEIA_model/lookup/Stock_lookup.csv")


### Need detailed explanation on what is going on here
# Adding French CS had French fix for had
catch_new <- catch
catch_new$spp_FR <- paste0(catch_new$Species, catch_new$Country)
#sanity check
sum(catch_new$Landings[catch_new$spp_FR == "HADFRA"]) #74599.39
sum(catch_new$Landings[catch_new$spp_FR == "HADFRA"& catch_new$Year == 2009])# 5818.751
catch_new <- catch_new[!catch_new$spp_FR == "HADFRA",]
catch<- catch_new[-14]


catchHAD$Country <- as.character(catchHAD$Country)
catchHAD$Area <- as.character(catchHAD$Area)
catchHAD$Species <- as.character(catchHAD$Species)
catchHAD$ID <- as.character(catchHAD$ID)
catchHAD$Quarter <- as.character(catchHAD$Quarter)
catchHAD$Metier <- as.character(catchHAD$Metier)
catchHAD$Vessel_length <- as.character(catchHAD$Vessel_length)


accessions_landings<-rbind(catchHAD,catch)
#sanity check
sum(accessions_landings$Landings[accessions_landings$Species == "HAD" & accessions_landings$Country == "FRA"]) #71761.94 was previously 74599.39 - lost 5000 tonnes!!!
sum(accessions_landings$Landings[accessions_landings$Species == "HAD" & accessions_landings$Country == "FRA" & accessions_landings$Year == 2019])#  4547.736 - was previously 5818.751 lost 1000 tonnes!



# as cahracter ------------------------------------------------------------
accessions_landings$Country <- as.character(accessions_landings$Country)
accessions_landings$Metier <- as.character(accessions_landings$Metier)
accessions_landings$Vessel_length <- as.character(accessions_landings$Vessel_length)
accessions_landings$Area <- as.character(accessions_landings$Area)
accessions_landings$Species <- as.character(accessions_landings$Species)


# 03 _ Clean Area and Species ####
#read in table of areas and species to change
#this section is applying the areas and species fix
#from the lookup tables Does nep still need an adjustment?
##Apply area fix
names(area_spp_fix) <- c("Area" ,"Standard","ICES_mix_correct", "ICES_FU","species_mix_FU" )

area_spp_fix <- area_spp_fix %>% filter(is.na(area_spp_fix$species_mix_FU)==F)


dim(accessions_landings)
new_accession_landings <- left_join(accessions_landings,area_spp_fix, by = "Area" )
dim(new_accession_landings)

##Check for NA values for the coloums added by area_spp_fix
table(is.na(new_accession_landings$species_mix_FU[new_accession_landings$Species =="NEP"]))
#### why just nep? becuse the area_spp_fix currently only targts neps
# #make columes for cleaning
new_accession_landings$Area_keep <- new_accession_landings$ICES_mix_correct
new_accession_landings$Area_keep <- ifelse(new_accession_landings$Species=="NEP",new_accession_landings$Area_keep,new_accession_landings$Area)
new_accession_landings$species_mix_FU <- as.character(new_accession_landings$species_mix_FU)
new_accession_landings$Species_keep <- ifelse(new_accession_landings$Species=="NEP",new_accession_landings$species_mix_FU,new_accession_landings$Species)

#new_accession_landings$Species_keep <- ifelse(new_accession_landings$Species=="NEP", new_accession_landings$species_mix_FU,new_accession_landings$Species)

#final usable dataset
accessions_landings <- new_accession_landings%>% select( Country, Year, Quarter, Metier, Vessel_length,  Area_keep, Species_keep, Landings, Value)
names(accessions_landings) <-  c("Country", "Year", "Quarter", "Metier", "Vessel_length", "Area", "Species", "Landings", "Value")



# 04 _ Subset for Celtic Seas Areas ####
accessions_landings$Area <- as.character(accessions_landings$Area)
accessions_landings_new<- accessions_landings[substr(accessions_landings$Area, 1,4) %in% c("27.7"   , "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]
unique(accessions_landings_new$Area)

# back to accessions_landings -----------------------------------------

accessions_landings <- accessions_landings_new

# so many values with a zero
accessions_landings <- accessions_landings[!accessions_landings$Landings== 0,]
accessions_landings <- accessions_landings[!is.na(accessions_landings$Landings),]


# Why do we do this one ---------------------------------------------------
a<-which(accessions_landings$Country=="DK"&accessions_landings$Species=="NEP.FU.20-21")
exc<-1:nrow(accessions_landings)
accessions_landings<-accessions_landings[which(!(exc %in% a)),]


q <- ggplot(accessions_landings[accessions_landings$Year %in% c(2018) & accessions_landings$Species %in% c("COD", "HAD", "WHG"),], aes(Country, Landings)) + geom_bar(stat="identity") + facet_wrap(~Species)
q

q <- ggplot(accessions_landings[accessions_landings$Year  & accessions_landings$Species %in% c("COD", "HAD", "WHG"),], aes(Country, Landings)) + geom_bar(stat="identity") + facet_wrap(~Year)
q

# Grouped Species Assumptions #### LP to write text around this. not used in 2019
accessions_landings$Species[accessions_landings$Species %in% c("LEZ","LDB")]<- "MEG" # what are teh implications of doing this after the above caluclation
accessions_landings$Species[accessions_landings$Species %in% c("ANK","ANF", "MNZ")]<- "MON"
#


# Adjustments and corrections ---------------------------------------------


# Clean Metier Naming ####
names(lvl4_Lookup)[1] <- "Metier"

dim(accessions_landings)
accessions_landings2 <- left_join(accessions_landings,lvl4_Lookup)
dim(accessions_landings2)[1]-dim(accessions_landings)[1]

accessions_landings2$Metier[is.na(accessions_landings2$Correct_lvl4)==F] <- accessions_landings2$Correct_lvl4[is.na(accessions_landings2$Correct_lvl4)==F]
accessions_landings2 <- accessions_landings2 %>% select(-Correct_lvl4,-CHECK_please)
unique(accessions_landings2$Metier)


# Clean Quaters ####
Quarter_Lookup$Quarter <- as.character(Quarter_Lookup$Quarter)

dim(accessions_landings2)
accessions_landings3 <- left_join(accessions_landings2,Quarter_Lookup)
dim(accessions_landings3)[1]-dim(accessions_landings2)[1]

accessions_landings3$Quarter[is.na(accessions_landings3$Correct_Quarter)==F] <- accessions_landings3$Correct_Quarter[is.na(accessions_landings3$Correct_Quarter)==F]
accessions_landings3 <- accessions_landings3 %>% select(-Correct_Quarter)
unique(accessions_landings3$Quarter)


# Vessel Lengths ----------------------------------------------------------
accessions_landings3$Vessel_length
#clean FR vessel length 
table(accessions_landings$Vessel_length)
sort(unique(accessions_landings$Vessel_length))
accessions_landings$Vessel_length[accessions_landings$Country == "FRA" & accessions_landings$Vessel_length %in% c("", "10<24m", "24<40m")] <- "10<40m"
#clean UKS vessel length 
accessions_landings$Vessel_length[accessions_landings$Country == "UKS" & accessions_landings$Vessel_length == ""] <- "<10m"

dim(accessions_landings3)
accessions_landings4 <- left_join(accessions_landings3,Vessel_Lookup)
dim(accessions_landings4)[1]-dim(accessions_landings3)[1]

accessions_landings4$Vessel_length[is.na(accessions_landings4$Correct_Vessel_length)==F] <- accessions_landings4$Correct_Vessel_length [is.na(accessions_landings4$Correct_Vessel_length)==F]
accessions_landings4 <- accessions_landings4 %>% select(-Correct_Vessel_length)
unique(accessions_landings4$Vessel_length)

# SPECIFIC ISSUES  --------------------------------------------------------
#cannot be corrected with lookup
table(
accessions_landings4$Vessel_length[accessions_landings4$Country=="FRA"]
)

print(unique(accessions_landings4$Vessel_length[accessions_landings4$Country=="FRA"]))

table(
    accessions_landings4$Country[accessions_landings4$Country =="FRA" & accessions_landings4$Vessel_length ==""]
)

table(
  accessions_landings4$Metier[accessions_landings4$Country =="FRA" & accessions_landings4$Vessel_length ==""]
)
unique(accessions_landings4$Metier[accessions_landings4$Country =="FRA" & accessions_landings4$Vessel_length =="" &grepl("DRB|FPO|LLS|LHM|GN|PS|MIS",accessions_landings4$Metier)==T])

unique(accessions_landings4$Vessel_length[accessions_landings4$Vessel_length =="" &grepl("all",accessions_landings4$Metier)==T])

unique(accessions_landings4$Vessel_length[accessions_landings4$Country =="FRA" & accessions_landings4$Vessel_length =="" &grepl("DRB",accessions_landings4$Metier)==T])


#ES
accessions_landings4$Vessel_length[accessions_landings4$Country=="ES"& accessions_landings4$Year==2017 & accessions_landings4$metier=="LLS_DEF"] <- "all"

#UKS
accessions_landings4$Vessel_length[accessions_landings4$Country=="UKS"& accessions_landings4$Vessel_length==""] <- "all"
unique(accessions_landings4$Vessel_length[accessions_landings4$Country=="UKS"])


# some checks -------------------------------------------------------------
unique(accessions_landings4$Country)
unique(accessions_landings4$Quarter)
unique(accessions_landings4$Vessel_length)
unique(accessions_landings4$Species)



accessions_landings <- accessions_landings4 %>% group_by(Country, Year, Quarter, Metier, Vessel_length,
                                                        Area, Species) %>% dplyr::summarise("Landings" = sum(Landings, na.rm = TRUE),
                                                                                            "Value" = sum(Value, na.rm=TRUE)) %>% data.frame()

sum(accessions_landings4$Landings)/1000
sum(accessions_landings$Landings)/1000

q <- ggplot(accessions_landings[accessions_landings$Year %in% c(2019) & accessions_landings$Species %in% c("COD", "HAD", "WHG")& accessions_landings$Area %in% c("27.7.b" ,"27.7.c" ,  "27.7.d", "27.7.e","27.7.f" ,  "27.7.g" ,  "27.7.h" ,"27.7.j" ,  "27.7.k"),], aes(Country, Landings)) + geom_bar(stat="identity") + facet_wrap(~Species)
q



# Fix for odd nep issues
# accessions_landings$Species[ accessions_landings$Species == "NEP.FU.20.21"] <-  "NEP.FU.2021"
# accessions_landings$Species[ accessions_landings$Species == "NEP.FU20-21"] <-  "NEP.FU.2021"
accessions_landings$Species[ accessions_landings$Species == "NEP.FU.20-21"] <-  "NEP.FU.2021"
accessions_landings$Species[ accessions_landings$Species == "NEP.FU.23-24"] <-  "NEP.FU.2324"

# Fix for reallocation of NEP Irish landings in 2017.
# We need to move landings from FU17, 19, 2021, 22 and OUT.7 to FU16
# This process should add 1373.41 tonnes to NEP.FU.16 in 2017
aggregate(Landings ~ Species,
          subset(accessions_landings, Country == "IE" &
                   Year == 2017 &
                   Species %in% c("NEP.FU.16", "NEP.FU.17", "NEP.FU.19", "NEP.FU.2021", "NEP.FU.22", "NEP.OUT.7")),
          sum) # before doing anything we have 763.8, 804.59, 692.79, 1100.45, 2858.17, 477
nep_adjust <- as.data.frame(cbind("Year" = 2017,
                                  "Species" = c("NEP.FU.17", "NEP.FU.19", "NEP.FU.2021", "NEP.FU.22", "NEP.OUT.7"),
                                  "orig_lands" = as.numeric(c(803.681, 769.195, 2818.906, 1122.272, 546.09)),
                                  "adjusts" = as.numeric(c(-509.4388491, -377.5959672, -3.910233095, -9.455200352, -470.7602)),
                                  "new_species" = c("NEP.FU.17", "NEP.FU.19", "NEP.FU.2021", "NEP.FU.22", "NEP.OUT.7")))
nep_adjust$Species <- as.character(nep_adjust$Species)
nep_adjust$orig_lands <- as.numeric(as.character(nep_adjust$orig_lands))
nep_adjust$adjusts <- as.numeric(as.character(nep_adjust$adjusts))
nep_adjust$true_lands <- with(nep_adjust, orig_lands + adjusts)
nep_adjust$prop <- with(nep_adjust, true_lands / orig_lands)
nep_adjust_FU16 <- as.data.frame(cbind("Species" = nep_adjust$Species,
                                       "prop" = 1-nep_adjust$prop,
                                       "new_species" = "NEP.FU.16"))
nep_prop <- rbind(nep_adjust[, c("Species", "prop", "new_species")], nep_adjust_FU16)
nep_prop$prop <- as.numeric(nep_prop$prop)
# Subset Irish NEP data in 2017 from accessions 
nep_ire_rows <- with(accessions_landings, Country == "IE" & Year == 2017 & Species %in% c("NEP.FU.17", "NEP.FU.19", "NEP.FU.2021", "NEP.FU.22", "NEP.OUT.7"))
nep_ire <- accessions_landings[nep_ire_rows,]
# Remove these rows from the accessions_landings table, as we will create them again and rbind the new rows later
accessions_landings <- accessions_landings[!nep_ire_rows,]

# Merge Irish data from accessions_landings table with Adjusts for corrections.
# This duplicates each row in the catch table by the number of Areas for the FU in that row.
nep_ire_mer <- merge(nep_ire, nep_prop, all = T)
# Split the Landings and Values from accessions table by the proportions into FU16 and original FU
nep_ire_mer$Landings <- nep_ire_mer$Landings * nep_ire_mer$prop
nep_ire_mer$Value <- nep_ire_mer$Value * nep_ire_mer$prop
# Copy new_species names to correct column
nep_ire_mer$Species <- nep_ire_mer$new_species
# Allocate all the new FU16 landings to Area 27.7.k
nep_ire_mer[nep_ire_mer$Species == "NEP.FU.16", "Area"] <- "27.7.k"
# Keep only columns that we want
nep_ire_mer <- nep_ire_mer[,names(accessions_landings)]

# rbind original accessions data (without Irish 2017 NEPs) and newly created Irish NEPs
accessions_landings <- rbind(accessions_landings, nep_ire_mer)

aggregate(Landings ~ Species,
          subset(accessions_landings, Country == "IE" &
                   Year == 2017 &
                   Species %in% c("NEP.FU.16", "NEP.FU.17", "NEP.FU.19", "NEP.FU.2021", "NEP.FU.22", "NEP.OUT.7")),
          sum) # after the work we have 2051, 295, 353, 1099, 2834, 66. Not perfect yet, but much better



# add stock data ----------------------------------------------------------
dim(accessions_landings)
accessions_landings_Stock <- left_join(accessions_landings,Stock_lockup)
dim(accessions_landings)[1]-dim(accessions_landings_Stock)[1]


# #add fleets -------------------------------------------------------------

#provision for adding fleets 
# dim(accessions_landings)
# accessions_landings_fin <- left_join(accessions_landings_Stock,Fleet_lockup)
# dim(accessions_landings_Stock)[1]-dim(accessions_landings_fin)[1]

#  Write out clean data
write.taf(accessions_landings_fin, file = file.path(Data_path_out,"clean_data/clean_accessions_landings.csv"))

print(unique(accessions_landings$Vessel_length))

# effort section ----------------------------------------------------------
# Data prep
# Preprocess data,

## Before: accessions effort from git
## After:  accession_effort.csv (data)


# 00_Setup ####
gc()
rm(list = ls())

library(tidyverse)
library(readxl)
#library(ggplot2)
library(icesTAF)
#library(dplyr)
#library(tidyr)

Data_path <- "CelticSea/bootstrap"
Data_path_out <- "CelticSea/Results"

# 02 _ Read in data ####
load(file.path(Data_path,"initial/wgmixfish_accessions/effort_2020.Rdata"))
area_spp_fix <- read.csv("CelticSea/FLBEIA_model/lookup/Area_lookup.csv")
lvl4_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Metier_lvl4_lookup.xlsx")
Vessel_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Vessel_length_lookup.xlsx")
Quarter_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Quarter_lookup.xlsx")
Country_Lookup <- read_xlsx("CelticSea/FLBEIA_model/lookup/Country_lookup.xlsx")
accessions_effort <- effort
rm(effort)



# spanish data that needs to be substituted (ask someone why) -------------
load(file.path(Data_path,"initial/wgmixfish_accessions/effort_2019.RData"))

#remove offender from current data
accessions_effort_replece <- accessions_effort %>% filter(Country %in% c("ES-AZTI", "ES","ESP" )& Year %in% c(2018)) 
accessions_effort <- accessions_effort %>% filter(!Country %in% c("ES-AZTI", "ES","ESP" ) | !Year %in% c(2018)) 

effort_ES_2018 <- effort %>% filter(Country %in% c("ES-AZTI", "ES","ESP" )& Year %in% c(2018)) 

accessions_effort <- rbind(accessions_effort,effort_ES_2018)

rm(accessions_effort_replece,effort_ES_2018,effort)


# countrys ----------------------------------------------------------------


unique(accessions_effort$Country)

dim(accessions_effort)
accessions_effort_A <- left_join(accessions_effort,Country_Lookup)
dim(accessions_effort)[1]-dim(accessions_effort_A)[1]

accessions_effort_A$Country[is.na(accessions_effort_A$CorrectCountry)==F] <- accessions_effort_A$CorrectCountry[is.na(accessions_effort_A$CorrectCountry)==F]


accessions_effort_A <- accessions_effort_A %>% select(-CorrectCountry)
unique(accessions_effort_A$Country)




# 03 _ Clean Area ####
#read in table of areas and species to change

names(area_spp_fix) <- c("Area" ,"Standard","ICES_mix_correct", "ICES_FU","species_mix_FU" )

dim(accessions_effort_A)
accessions_effort <- left_join(accessions_effort_A,area_spp_fix, by = "Area" )
dim(accessions_effort)

accessions_effort$Area[is.na(accessions_effort$ICES_mix_correct)==F] <-accessions_effort$ICES_mix_correct[is.na(accessions_effort$ICES_mix_correct)==F]
unique(accessions_effort$Area)

class(accessions_effort$Area)
Bad_accessions_effort_areas <- accessions_effort %>% filter(Area %in%c("-1","NULL"))
write.taf(Bad_accessions_effort_areas,file = file.path(Data_path_out,"Intermediate_products/bad_effort_area.csv"))
#filter areas
accessions_effort <- accessions_effort %>% filter(!Area %in%c("-1","NULL"))
accessions_effort <- accessions_effort[substr(accessions_effort$Area, 1,4) %in% c("27.7"   , "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]

## remove exstra col
accessions_effort <- accessions_effort %>% select(-Standard,-ICES_mix_correct, -ICES_FU,-species_mix_FU)

# Vessel length -----------------------------------------------------------
unique(accessions_effort$Vessel_length)
unique(Vessel_Lookup$Vessel_length)

dim(accessions_effort)
accessions_effort2 <- left_join(accessions_effort,Vessel_Lookup)
dim(accessions_effort)[1]-dim(accessions_effort2)[1]

accessions_effort2$Vessel_length[is.na(accessions_effort2$Correct_Vessel_length)==F] <- accessions_effort2$Correct_Vessel_length[is.na(accessions_effort2$Correct_Vessel_length)==F]


accessions_effort2 <- accessions_effort2 %>% select(-Correct_Vessel_length)
unique(accessions_effort2$Vessel_length)

accessions_effort2[accessions_effort2$Vessel_length=="",]
#this is beig a pig to fix in the lookup table 
accessions_effort2$Vessel_length[(accessions_effort2$Vessel_length %in% c(""))]<-"all"
unique(accessions_effort2$Vessel_length)

# Clean Metier Naming ####
unique(accessions_effort2$Metier)
names(lvl4_Lookup)[1] <- "Metier"

dim(accessions_effort2)
accessions_effort3 <- left_join(accessions_effort2,lvl4_Lookup)
dim(accessions_effort2)[1]-dim(accessions_effort3)[1]

accessions_effort3$Metier[is.na(accessions_effort3$Correct_lvl4)==F] <- accessions_effort3$Correct_lvl4[is.na(accessions_effort3$Correct_lvl4)==F]
accessions_effort3 <- accessions_effort3 %>% select(-Correct_lvl4,-CHECK_please)
unique(accessions_effort3$Metier)



# Check quater  -----------------------------------------------------------
unique(accessions_effort3$Quarter)


Quarter_Lookup$Quarter <- as.character(Quarter_Lookup$Quarter)

dim(accessions_effort3)
accessions_effort4 <- left_join(accessions_effort3,Quarter_Lookup)
dim(accessions_effort3)[1]-dim(accessions_effort4)[1]

accessions_effort4$Quarter[is.na(accessions_effort4$Correct_Quarter)==F] <- accessions_effort4$Correct_Quarter[is.na(accessions_effort4$Correct_Quarter)==F]
accessions_effort4 <- accessions_effort4 %>% select(-Correct_Quarter)
unique(accessions_effort4$Quarter)


Bad_accessions_effort_Quarters <- accessions_effort4 %>% filter(!Quarter %in% c("Q1","Q2","Q3","Q4"))
accessions_effort5 <- accessions_effort4 %>% filter(Quarter %in% c("Q1","Q2","Q3","Q4"))

write.taf(Bad_accessions_effort_Quarters,file = file.path(Data_path_out,"Intermediate_products/Bad_accessions_effort_Quarters.csv"))

unique(accessions_effort5$Quarter)

unique(
  accessions_effort5$Country
)


# SPECIFIC fixes that cannot be done in a lookup --------------------------
## assume catch is correct

#PT
accessions_effort5$Area[accessions_effort5$Area =="27.7.j" & accessions_effort5$Country =="PT" & accessions_effort5$Year %in%c(2018,2019) & accessions_effort5$Quarter == "Q3"] <- "27.7.h"

#IE
accessions_effort5$Metier[accessions_effort5$Country=="IE"&  accessions_effort5$Metier =="PTM_LPF"] <- "PTM_SPF"

#PSAIN
accessions_effort5$Vessel_length[accessions_effort5$Country=="ES"& accessions_effort5$Year == 2018] <- "all"
accessions_effort5$Vessel_length[accessions_effort5$Country=="ES"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="GNS_DEF"] <- "all"
accessions_effort5$Vessel_length[accessions_effort5$Country=="ES"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="LLS_DEF" & accessions_effort5$Vessel_length == "-9" ] <- "all"

accessions_effort5$Metier[accessions_effort5$Country=="ES"& accessions_effort5$Year == 2018 & accessions_effort5$Metier %in%c("OTB_MPD","PTB_MBT")] <- "OTB_DEF" #OTB with pelagic doors
accessions_effort5$Metier[accessions_effort5$Country=="ES"& accessions_effort5$Year == 2018 & accessions_effort5$Metier %in%c("LTL_LPL","LHP_LPF")] <- "LLS_DEF" 
#France

accessions_effort5$Metier[accessions_effort5$Country=="FRA"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="DRB_all"] <- "DRB_MOL"
accessions_effort5$Metier[accessions_effort5$Country=="FRA"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="PS_SPF_"] <- "OTB_SPF"
accessions_effort5$Metier[accessions_effort5$Country=="FRA"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="OTT-DEF"] <- "OTT_DEF"

#scotland
accessions_effort5$Vessel_length[accessions_effort5$Country=="UKS"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="FPO_CRU"] <- "<10m"
accessions_effort5$Vessel_length[accessions_effort5$Country=="UKS"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="GNS_DEF"] <- "<10m"
accessions_effort5$Vessel_length[accessions_effort5$Country=="UKS"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="LLS_FIF"] <- "<10m"
accessions_effort5$Vessel_length[accessions_effort5$Country=="UKS"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="MIS_MIS"] <- "<10m"

#UKN
accessions_effort5$Metier[accessions_effort5$Country=="UKN"& accessions_effort5$Year == 2018 & accessions_effort5$Metier =="FPO_NA_"] <- "FPO_CRU"


#final usable dataset
# FDF removed as not used in Celtic Sea
accessions_effort <- accessions_effort5%>% select( Country, Year, Quarter, Metier, Vessel_length,
                                                  Area, kw_days, Days_at_sea, No_vessels)
names(accessions_effort) <-  c("Country", "Year", "Quarter", "Metier", "Vessel_length",  "Area",   "kw_days",
                               "Days_at_sea"  , "No_vessels")

accessions_effort$Country <- as.character(accessions_effort$Country)

accessions_effort$kw_days<-as.numeric(accessions_effort$kw_days)
accessions_effort$Days_at_sea<-as.numeric(accessions_effort$Days_at_sea)



accessions_effort<- accessions_effort %>% group_by(Country, Year, Quarter, Metier, Vessel_length,
                                                   Area) %>% dplyr::summarise("kw_days" = sum(kw_days, na.rm = TRUE),
                                                                              "Days_at_sea" = sum(Days_at_sea,na.rm=TRUE),
                                                                              "No_vessels" = sum(No_vessels, na.rm=TRUE) )

accessions_effort$Metier <- substr(accessions_effort$Metier,1,7)
accessions_effort_2020 <- as.data.frame(accessions_effort)

# 09 _ Write out clean data ####
write.taf(accessions_effort_2020, file.path(Data_path_out,"clean_data/clean_accessions_effort.csv"))

gc()
rm(list=ls())

Yearwg<-2020

Data_path <- "CelticSea/bootstrap"
Data_path_out <- "CelticSea/Results"

catch_start <-read.csv(file.path(Data_path_out,"clean_data/clean_accessions_landings.csv"))
effort_start <-read.csv(file.path(Data_path_out,"clean_data/clean_accessions_effort.csv"))

catch <- catch_start
effort <- effort_start

unique(catch$Area)

# some code ---------------------------------------------------------------

catch<- catch[catch$Area %in% c("27.7"   , "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]
effort<- effort[effort$Area %in% c("27.7"  , "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]

catch <- catch[substr(catch$Area, 1,4) %in% c( "27.7"),]
effort <- effort[substr(effort$Area, 1,4) %in% c( "27.7"),]




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


# Adjustments and corrections ---------------------------------------------
Catch3 <- catch2
# fix to exclude SOL 7.E in 2020
# currently filtering sol in 27.7.e 
Catch3[Catch3$Species =="SOL" & Catch3$Area == "27.7.e",]
Catch3 <- Catch3[Catch3$Species !="SOL" & Catch3$Area != "27.7.e",]

 


##### propotion of megrim based on split established by WG
Catch3$Landings[Catch3$Species=="LDB"]<-Catch3$Landings[Catch3$Species=="LDB"]*(1-0.052)
Catch3$Landings[Catch3$Species=="LEZ"]<-Catch3$Landings[Catch3$Species=="LEZ"]*(1-0.052)
Catch3$Landings[Catch3$Species=="MEG"]<-Catch3$Landings[Catch3$Species=="MEG"]*(1-0.052)
Catch3$Species[Catch3$Species %in% c("LEZ","LDB")]<- "MEG"

########### propotion of anglers and monk based on  known split
# we only care about 1 stock and thes especies are landined as spp so
#  a value is used to proportion what we want by stock (Expert knowledge?! you would hope...)
Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country %in% c("ES","ES-AZTI"))]<-Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country %in% c("ES","ES-AZTI"))]*0.57

Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country=="FRA")]<-Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country=="FRA")]*0.82

Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country=="IE")]<-Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country=="IE")]*0.78

Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country %in% c("IM","JE","UKE","UKN","UKS"))]<-Catch3$Landings[(Catch3$Species %in% c("ANK","ANF","MON"))&(Catch3$Country %in% c("IM","JE","UKE","UKN","UKS"))]*0.88
Catch3$Species[Catch3$Species %in% c("ANK","ANF","MNZ")]<- "MON"



# Processing catch data ---------------------------------------------------
Catch4 <- Catch3
effort3 <- effort2

Catch4$Metier <- substr(Catch4$Metier, 1,7)
effort3$Metier <- substr(effort3$Metier, 1,7)
effort3 <- effort3 %>% mutate(effort_check=1:nrow(effort3))
Catch4 <- Catch4 %>% mutate(catch_check=1:nrow(Catch4))


dim(Catch4)
Catch_effort <- left_join(Catch4,effort3)
dim(Catch_effort)


Catch_effort_NA <- filter(Catch_effort,is.na(effort_check)==T)
Catch_MATCH <-filter(Catch_effort,is.na(effort_check)==F)
dim(Catch_effort)[1]-(dim(Catch_effort_NA)[1]+dim(Catch_MATCH)[1])



Effort_Na <-effort3 %>% filter(!effort_check %in% Catch_effort$effort_check)
Effort_MATCH <-effort3 %>% filter(effort_check %in% Catch_effort$effort_check)

table(Catch_effort_NA$Country)
table(Effort_Na$Country)
table(Effort_Na$Metier)

write.taf(Catch_effort_NA,file.path(Data_path_out,"Intermediate_products/NA_Catch.csv"))
write.taf(Effort_Na,file.path(Data_path_out,"Intermediate_products/NA_Effort.csv"))

##
write.taf(Catch_MATCH,file.path(Data_path_out,"clean_data/Matched_clean_accessions_landings.csv"))
write.taf(Effort_MATCH,file.path(Data_path_out,"clean_data/Matched_clean_accessions_effort.csv"))



