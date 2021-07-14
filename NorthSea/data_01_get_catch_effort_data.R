#
# PROGRAM TO GET CATCH AND EFFORT DATA AS PROVIDED BY INDIVIDUAL COUNTRIES
#
# 01. Compile/standardise all national files for latest year into a single file for catch and for effort
# and add to previous compiled years.

# Version 4.10  09/10/2019
#
# Author: Clara Ulrich, clu@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# Youen Vermard , yvermard@ifremer.fr

## Before: bootstrap/wgmixfish_accessions/.Rdata
## After:  data/NS_Fleet database yyyy-mm-dd/.csv, /.Rdata, /.png

# Input data:
# this program reads the pre-processed accession files (accessed from github.com/ices-taf/wgmixfish_accessions)
# the program to process the accession files is located here (github.com/ices-taf/wgmixfish_accessions)

# This program:
# Checks and standardises the format of data provided in each column.
# The number of elements making up the metier are looked at - if these seem to be incorrect then an error or
# warning message is displayed.
# Country and year specific issues can be fixed just before this check once they are found.
# Functional units are assigned to Nephrops catches
# Stock codes are assigned to catches
#
# This object is saved in various formats to be used in subsequent scripts

# Data issues:
# Germany's landings value are calculated from a Price table from 2011 - we are hoping that subsequent submission
# will include this data and remove the need for this section
# for each effort and catch file submitted by MS some common corrections are applied (e.g. spaces in the metier).
# watch that the files do not have a space in the number!

# Consideraions:
# check how discard rate for nephrops is treated. Is this average as used for forecast?
# but should it not be or real historical time series??

# script will need updating each year - you can delete any year specific fixes once those fixes are saved to the compilation Rdata object (e.g. eff1_to2014)


# Developed with:
#   - R version 3.1
#
# v1. - New file started for May 2016 based on the version 2 last May (not including the CPUE stocks)
# v2. - October 2016 - change to only processing latest year of data rather than all years.
# v3. - Changes for May 2017 advice meeting to update all years submitted in 2017 (2006 - 2017). Norway data not submitted. Updates needed to codes (e.g. areas).
# v4. - June 2019 - Adapted to fit ICES TAF format



# 1. Initialise system ----------------------------------------------------

# Start with house cleaning
rm(list = ls(all.names=T)); gc(); graphics.off()

ver01 <- "NS_Fleet_database"

start.time <- proc.time()[3]
options(stringsAsFactors = FALSE, scipen=3)

cat(paste("\n",ver01,"\n",sep=""))

# set directories
res.path    <- file.path("data",ver01)
lookup.path <- "bootstrap/data/lookup_tables/"

# directory and filename for germany price check csv
gd.price.path <- paste0(lookup.path,"Prices_GER.csv")


# lookup tables
cc.lookup   <- read.csv(paste0(lookup.path,"AC_country_codes.csv"),stringsAsFactors = F)
area.lookup <- read.csv(paste0(lookup.path,"AC_areas.csv"),stringsAsFactors = F)


# 2. Settings ----------------------------------------------------------------

# set year
datayear       <- 2019 #last data year
datayear_range <-c(2009:datayear)


# Areas to be kept
Area.Names <- c("3AN", "4", "7D", "6A")


Recalc.Nep <- FALSE
export_    <- TRUE

# list of country to use
#ctry.codes <- c("GE","DK","EN","SC","NL","SW","NO","FR","BE") # 2 character country codes
ctry.codes <- c("GE","DK","EN","SC","NL","NO","BE","SW", "FR") # 2 character country codes

# 3. Load aggregated MIXFISH Accession data ----------------------------------

# Catch data
load("bootstrap/data/wgmixfish_accessions/catch_2020.RData")


#############################################################################################
#############################################################################################
#############################################################################################
################
################
################  FIX WHG 2020 for TBB_DEF_70-99_0_0_all NL
################          and GE TBB_CRU_16-31_0_0_all
################    that have a very high DR in IC and a factor 10 in the accession landings compared to IC
################  The fix is then to divide by 10 acession landings
################
#############################################################################################
#############################################################################################
#############################################################################################
  tmp <- which(catch$Year==2019 & catch$Species=="WHG" & catch$Metier=="TBB_CRU_16-31_0_0_all" &
                     catch$Country=="DE" & catch$Species=="WHG")
      catch[tmp,]
      catch[tmp,"Landings"] <- catch[tmp,"Landings"]/10

  tmp <- which(catch$Year==2019 & catch$Species=="WHG" & catch$Metier=="TBB_DEF_70-99_0_0_all" &
                     catch$Country=="NL" & catch$Species=="WHG" & substr(catch$Area,1,4)=="27.4")
      catch[tmp,]
      sum(catch[tmp,"Landings"])
      catch[tmp,"Landings"] <- catch[tmp,"Landings"]/10
#############################################################################################
###############   end fix
#############################################################################################




Ca <- catch

# Effort data
load("bootstrap/data/wgmixfish_accessions/effort_2020.RData")
Ef <- effort

#check for missing submissions (by country)
table(Ca$Country,Ca$Year)
table(Ef$Country,Ef$Year)

# 3.1. Fix issues highlighted in submission process (QC feedback forms) ==============

# BE - no issues in 2020
# DK - no issues for North Sea
# GE - check duplicate effort records. Adjust units for Baltic file records

ef.key <- apply(Ef[c("Country","Year","Quarter","Metier","Vessel_length","FDF","Area","kw_days","Days_at_sea","No_vessels")],1,paste,collapse="_")
tmp <- Ef[Ef$Country %in% "DE",]
key <- ef.key[Ef$Country %in% "DE"]
# sum(table(key)-1) # 233 duplicates. ID column indicates that the same data has been submitted for different ecoregions
# extract wgnssk ID labels
t1<-table(key)[table(key)>1]
key1<-names(t1) # keys that are repeated
idx1 <- which(key %in% key1) # index for matching duplicate keys
idx2 <- grep("wgnssk",tmp$ID[idx1]) # index for duplicate keys with wgnssk
idx<-idx1[-idx2]# index for tmp for duplicates to be removed
key.dup <- key[idx] #keys to be removed
ef.key <- apply(Ef[c("Country","Year","Quarter","Metier","Vessel_length","FDF","Area","kw_days","Days_at_sea","No_vessels")],1,paste,collapse="_")
Ef <- Ef[-which(ef.key %in% key.dup), ] # remove duplicates from Ef

# units in Baltic file catch file
fn <-"Resubmission_2009-2019 MIXFISH-ADVICE DEU BALTIC metier_catch_v2.csv"
Ca[Ca$file_name %in% fn,"Landings"] <- Ca[Ca$file_name %in% fn,"Landings"]/1000

# NL - catch file - Nep FU codes not quite correct
#unique(Ca$Species[Ca$Country %in% "NL"])
Ca$Species <- gsub("NEP.5","NEP.FU.5",Ca$Species)
Ca$Species <- gsub("NEP.6","NEP.FU.6",Ca$Species)
Ca$Species <- gsub("NEP.7","NEP.FU.7",Ca$Species)
Ca$Species <- gsub("NEP.32","NEP.FU.32",Ca$Species)
Ca$Species <- gsub("NEP.33","NEP.FU.33",Ca$Species)

# 3.2. Format data ==============

# Format catch data
Ca$Country <- as.character(Ca$Country)
Ca$Metier <- as.character(Ca$Metier)
Ca$FDF <- as.character(Ca$FDF)
Ca$Area <- as.character(Ca$Area)
Ca$Species <- as.character(Ca$Species)

Ca$Vessel_length <- as.character(Ca$Vessel_length)
Ca$Year <- as.numeric(as.character(Ca$Year))
Ca$Quarter <- as.character(Ca$Quarter)

# Format effort data
Ef$Country <- as.character(Ef$Country)
Ef$Year <- as.numeric(as.character(Ef$Year))
Ef$Metier <- as.character(Ef$Metier)
Ef$FDF <- as.character(Ef$FDF)
Ef$Area <- as.character(Ef$Area)
Ef$Vessel_length <- as.character(Ef$Vessel_length)

Ef$Quarter <- as.character(Ef$Quarter)
Ef$kw_days <- as.numeric(Ef$kw_days)
Ef$No_vessels <- as.numeric(Ef$No_vessels)
Ef$Days_at_sea <- as.numeric(Ef$Days_at_sea)

# 3.3. Edit and filter for country codes ==============
# change country names
repl <- which(Ef$Country %in% cc.lookup$orig)
Ef$Country[repl] <- cc.lookup$new[match(Ef$Country[repl], cc.lookup$orig)]
#check country
unique(Ef$Country)
# filter countries
Ef <- Ef[Ef$Country %in% ctry.codes,]

# ca
repl <- which(Ca$Country %in% cc.lookup$orig)
Ca$Country[repl] <- cc.lookup$new[match(Ca$Country[repl], cc.lookup$orig)]
#check country
unique(Ca$Country)
# filter countries
Ca <- Ca[Ca$Country %in% ctry.codes,]


# 3.4. Check for and remove duplicate records ==============
# Effort - check for duplicate records
key <- apply(Ef[,c("Country","Year","Quarter","Metier","Vessel_length","FDF","Area","kw_days",
                   "Days_at_sea","No_vessels")],1,paste,collapse="_")

unique(substr(key, 1,3))
if(max(table(key))>1){
  reps<-names(table(key)[table(key)>1])
  #isolate all duplicate records
  rep_sub <- Ef[which(key %in% reps),]
  sub_key <- key[which(key %in% reps)]
  # remove all duplicate records
  Ef <- Ef[-which(key %in% reps),]
  # add back 1 copy of duplicate records
  Ef <- rbind(Ef,rep_sub[match(reps,sub_key),])
}

# Catch - check for duplicate records
key <- apply(Ca[,c("Country","Year","Quarter","Metier","Vessel_length","FDF","Area","Species","Landings",
                   "Value","Discards")],1,paste,collapse="_")
if(max(table(key))>1){
  reps<-names(table(key)[table(key)>1])
  #isolate all duplicate records
  rep_sub <- Ca[which(key %in% reps),]
  sub_key <- key[which(key %in% reps)]
  # remove all duplicate records
  Ca <- Ca[-which(key %in% reps),]
  # add back 1 copy of duplicate records
  Ca <- rbind(Ca,rep_sub[match(reps,sub_key),])
}

# 3.5. other raw data edits which are needed ==============
# None

# 4. Process effort ----------------------------------------------------------

# 4.1. Filter countries, areas, and years=====================

#Apply metier fixes for common problems ***
Ef$Metier <- gsub(" ", "", Ef$Metier)
Ef$Metier <- gsub(pattern = "All", replacement = "all", x = Ef$Metier)
Ef$Metier <- gsub("Active", "active", Ef$Metier)
Ef$Metier <- gsub("Passive", "passive", Ef$Metier)

#check metiers
sort(unique(Ef$Metier))

# Filter years
Ef <- Ef[Ef$Year %in% datayear_range,]

pander(table(Ef$Country, Ef$Year))

# 4.2. Metier adjustments===========================
# standardize and filter area names
table(Ef$Area, Ef$Country)
Ef$Area <- toupper(Ef$Area)

#convert area codes
repl <- match(Ef$Area, area.lookup$orig)
Ef$Areanew <- area.lookup$standard[repl]
Ef$Areanew[is.na(Ef$Areanew)] <- Ef$Area[is.na(Ef$Areanew)]
Ef$Area<-Ef$Areanew

# check areas
sort(unique(Ef$Area))
Ef<-Ef[,-c(which(colnames(Ef) %in% "Areanew"))]

# Filter areas
Ef <- Ef[Ef$Area %in% Area.Names,]

# check FDF
Ef$FDF[is.na(Ef$FDF)] <- ""
sort(unique(Ef$FDF))

# Edit vessel length codes
Ef$Vessel_length[is.na(Ef$Vessel_length)] <- "all"
Ef$Vessel_length[Ef$Vessel_length %in% ""] <- "all"
Ef$Vessel_length[which(Ef$FDF=="FDF")] <- paste(Ef$Vessel_length[which(Ef$FDF=="FDF")],"FDF", sep="_")
# remove "m" for meter (reduces the number of unique levels)
Ef$Vessel_length <- gsub("m","",Ef$Vessel_length)
# check
sort(unique(Ef$Vessel_length))

# quick check
sort(unique(Ef$Country))
sort(unique(Ef$Year))
sort(unique(Ef$Quarter))
sort(unique(Ef$Metier))
sort(unique(Ef$Vessel_length))
sort(unique(Ef$FDF))
sort(unique(Ef$Area))


# 4.4. Export effort========================

if(export_) {
  write.csv(Ef,file=paste0(res.path,"/01a_ExportAllEffort_beforeAnyWorkingUp_",ver01,".csv"),row.names=FALSE)
  save(Ef,file = paste0(res.path,"/01a_ExportAllEffort_to_",datayear,"_beforeAnyWorkinUp_",ver01,".Rdata"))
}


# 5. Process catch -----------------------------------------------------------

# 5.1. Edit codes and Filter years============================

#Apply metier fixes for common problems ***
Ca$Metier <- gsub(" ", "", Ca$Metier)
Ca$Metier <- gsub(pattern = "All", replacement = "all", x = Ca$Metier)
Ca$Metier <- gsub("Active", "active", Ca$Metier)
Ca$Metier <- gsub("Passive", "passive", Ca$Metier)

# check metiers
sort(unique(Ca$Metier))

# get gear
Ca$gear <- unlist(lapply(strsplit(Ca$Metier,"_"),function(x) x[[1]]))

# check FDF
Ca$FDF[is.na(Ca$FDF)] <- ""
sort(unique(Ca$FDF))

# Add FDF tag to vessel length category
Ca$Vessel_length[is.na(Ca$Vessel_length)] <- "all"
Ca$Vessel_length[Ca$Vessel_length %in% ""] <- "all"
Ca$Vessel_length[which(Ca$FDF=="FDF")] <- paste(Ca$Vessel_length[which(Ca$FDF=="FDF")],"FDF",sep="_")
# remove "m" for meter (reduces the number of unique levels)
Ca$Vessel_length <- gsub("m","",Ca$Vessel_length)
# check
sort(unique(Ca$Vessel_length))

# Apply fixes for common problems to Landings, Discards and Value
Ca <- Ca[Ca$Landings>0,]
Ca$Value[Ca$Value<1 & !is.na(Ca$Value)] <- as.numeric(NA)
Ca$Discards <- as.numeric(NA)

# Filter years
Ca <- Ca[Ca$Year %in% datayear_range,]


# 5.2. Standardise area codes and set stock name========================

# remove areas 4.a-c for FR effort (cat 4 already includes these landings)
table(Ca$Country, Ca$Area)
#Ca <- Ca[-which(Ca$Country == "FR" & Ca$Area %in% c("27.4.a", "27.4.b", "27.4.c")),]

Ca$Area <- toupper(Ca$Area)
Ca$stock <- Ca$Species

# Convert area codes
repl<-match(Ca$Area, area.lookup$orig)
Ca$Areanew <- area.lookup$standard[repl]
Ca$Areanew[is.na(Ca$Areanew)] <- Ca$Area[is.na(Ca$Areanew)]
sort(unique(Ca$Areanew))

# set new area code
Ca$Area<-Ca$Areanew

#check areas for other stocks
sort(unique(Ca$Area))

# species
sort(unique(Ca$stock))

# species aggregations from data call
Ca[Ca$stock %in% c("RJC", "SKA", "RAJ", "RJA", "RJB", "RJC", "RJE", "RJF",
                   "RJH", "RJI", "RJM", "RJN", "RJO", "RJR", "SKA", "SKX",
                   "SRX"),"stock"] <- "RJA"
Ca[Ca$stock %in% c("DGS","DGH","DGX","DGZ","SDV"),"stock"] <- "SDV"
Ca[Ca$stock %in% c("OTHER"),"stock"] <- "OTH"

# species from data call
dc.spp <-c("ANF","ANK","BLL","CAA","COD","COE","DAB","FLE","GUG","GUR","HAD",
  "HAL","HER","HKE","HOM","JAX","LDB","LEM","LEZ","LIN","MAC",
  "MEG","MON","NOP","OTH","PLE","POK","POL","RJA","RJU","SDV",
  "SOL","SPR","TUR","WHB","WHG","WIT")

# aggregate all other species as OTH
idx1 <- which(!Ca$stock %in% dc.spp)
idx2 <- which(!grepl("NEP",Ca$stock))
idx <- idx2[idx2 %in% idx1]
Ca$stock[c(idx)] <- "OTH"

# Set stock names
# plaice and sole 7D & 4
Ca$stock[Ca$stock %in% c("SOL","PLE") & Ca$Area %in% c("4")] <- paste0(Ca$stock[Ca$stock %in% c("SOL","PLE") & Ca$Area %in% c("4")],"-NS")
Ca$stock[Ca$stock %in% c("PLE") & Ca$Area %in% c("3AN")] <- paste0(Ca$stock[Ca$stock %in% c("PLE") & Ca$Area %in% c("3AN")],"-NS")
Ca$stock[Ca$stock %in% c("SOL","PLE") & Ca$Area %in% c("7D")] <- paste0(Ca$stock[Ca$stock %in% c("SOL","PLE") & Ca$Area %in% c("7D")],"-EC")

#had whg cod in 6A and NS
Ca$stock[Ca$stock %in% c("COD","WHG") & Ca$Area %in% c("6A")] <- paste0(Ca$stock[Ca$stock %in% c("COD","WHG") & Ca$Area %in% c("6A")],"-WS")
Ca$stock[Ca$stock %in% c("COD") & Ca$Area %in% c("4","7D","3AN")] <- "COD-NS"
Ca$stock[Ca$stock %in% c("WHG") & Ca$Area %in% c("4","7D")] <- "WHG-NS"

Ca[Ca$stock %in% c("MNZ","ANG","MON","ANK"),"stock"] <- "ANF"
Ca[Ca$stock %in% c("MEG","LDB"),"stock"] <- "LEZ"

# nep FUs
Ca$stock <- gsub(".FU.","",Ca$stock)
Ca$stock[Ca$stock %in% "NEP" & Ca$Area %in% c("3AN","27.3.21")] <- "NEP3A"
Ca$stock[Ca$stock %in% "NEP.OUT.4"] <- "NEPOTH-NS"
Ca$stock[Ca$stock %in% c("NEP.OUT.6a","NEP.OUT.6")] <- "NEPOTH-WS"

#check
sort(unique(Ca$stock))

# 5.3 sort landings values ==================================

# 5.3.1 price check for germany ####

# germany, we do not have value included. therefore horrible proxy, we use 2009 price value
#provided once back in time.
#price 2009 :
gd.price <- read.csv(gd.price.path,colClasses='character', header=TRUE)
gd.price$PRICE <- as.numeric(gd.price$PRICE)
gd.price <- gd.price[,2:5]
gd.price <- aggregate(gd.price$PRICE*1000,list(price.spp=gd.price$SPECIES,price.cat=gd.price$GEAR),mean,na.rm=T)
#price now in euro/tonnes

# merging
for(i in seq(nrow(Ca))){
  if(Ca$Country[i] == "GE"){
    if(is.na(Ca$Value[i])){
    price.cat.i <- switch(
      EXPR = Ca$gear[i],
      "BEAM"="BEAM",
      "TBB" = "BEAM",
      "DEM_SEINE"="Otter + dem. Seine" ,
      "GILL" = "Gill + Trammel" ,
      "GNS" = "Gill + Trammel" ,
      "OTTER" = "Otter + dem. Seine",
      "TRAMMEL" = "Gill + Trammel",
      "PEL_TRAWL" = "Otter + dem. Seine",
      "MIS" = "Otter + dem. Seine",
      "OTB" = "Otter + dem. Seine",
      "SSC" = "Otter + dem. Seine",
      "DemHC" = "Otter + dem. Seine"
    )
    price.spp.i <- substr(Ca$Species[i],1,3)

    hit <- which(gd.price$price.spp == Ca$Species[i] & gd.price$price.cat == price.cat.i)
    if(length(hit)>0){Ca$Value[i] <- as.numeric(Ca$Landings[i])*gd.price$x[hit]}
    }
  }
}
#check
unique(Ca[Ca$Country=="GE" & is.na(Ca$Value), "Species"])

# 5.3.2 fill in the missing prices using the average prices ####
Ca <- data.table(Ca)
Ca[, Landings:=as.numeric(as.character(Landings))]
Ca[, Value:=as.numeric(as.character(Value))]

#calculate mean price
MeanPrice <- Ca[Landings>0 & Value>0, list(Price=weighted.mean(Value/Landings,Landings, na.rm=T)/1000), by=list(Species, Year)]
boxplot(MeanPrice$Price~MeanPrice$Species,las=2)

Ca <- merge(data.frame(Ca), MeanPrice, all.x=T)
Ca$Value[is.na(Ca$Value)] <- Ca$Landings[is.na(Ca$Value)] * 1000 * Ca$Price[is.na(Ca$Value)]

Ca$Landings <- as.numeric(Ca$Landings) #some negative values in the older years...
Ca$Discards <- as.numeric(Ca$Discards)
Ca$Value <- as.numeric(Ca$Value)

# 5.3.3 check of mean price ####
#quick check
round(tapply(Ca$Landings,list(Ca$Species,Ca$Year,Ca$Country),sum,na.rm=T))
round(tapply(Ca$Value,list(Ca$Species,Ca$Year,Ca$Country),sum,na.rm=T))

if(F){
  a<-round(tapply(Ca$Landings,list(Ca$Species,Ca$Year,Ca$Country),sum,na.rm=T))
  b<-round(tapply(Ca$Value,list(Ca$Species,Ca$Year,Ca$Country),sum,na.rm=T))

  windows()
  par(ask=T)
  for (i in 1:length(dimnames(a)[[3]])){
    barplot(t(a[,,i]),main=dimnames(a)[[3]][i],las=2,cex.names=0.7)
  }

  windows()
  par(ask=T)
  for (i in 1:length(dimnames(b)[[3]])){
    barplot(t(b[,,i]),main=dimnames(b)[[3]][i],las=2)
  }
}

#quick price check
Ca$price <- (Ca$Value/Ca$Landings)/1000
head(Ca[order(Ca$price,decreasing=T),],20)  #quite high for BEL???
boxplot(Ca$price)
mean_p <- aggregate(Ca$price,list(year=Ca$Year, country=Ca$Country),mean,na.rm=T)
tmp <- subset(Ca, Year==datayear)
head(tmp[order(tmp$price,decreasing=T),],20)
h <- hist(Ca$price, n=1000)
plot(h$mids, h$counts+1, t="l", log="xy")

print(subset(mean_p,year==datayear))
range(Ca$price, na.rm=TRUE)

xyplot(x~year|country,data=mean_p,type="b",ylab="mean price")

# plot mean price by year and country
mean_p <- aggregate(Ca$price,list(year=Ca$Year, country=Ca$Country,stock=Ca$stock),mean,na.rm=T)
print(subset(mean_p,year==datayear))
mean_p<-mean_p[mean_p$stock %in% c("COD-NS","HAD","PLE-EC","PLE-NS","POK","SOL-EC","SOL-NS","WHG-NS"),]

for(cc in c("BE", "DK", "EN", "FR", "GE", "NL", "SC", "SW")){
#for(cc in c("BE", "DK", "EN", "GE", "NL", "SC","SW")){
  png(filename=paste(res.path,"/01_Get_catch_effort_mean_price_by_country_and_stock_",cc,".png", sep=""), width = 9, height = 9, units="in", res=200)
  dat<-mean_p[mean_p$country %in% cc,]
  print(xyplot(x~year|stock,data=dat,type="b",ylab="mean price",main = cc))
  dev.off()
}


# 5.4 Select areas ==============================

Ca <- Ca[Ca$Area %in% Area.Names,]

#final check
print(tapply(Ca$Landings,list(stock=Ca$stock,year=Ca$Year,country=Ca$Country),sum,na.rm=T))

idx <- which(Ca$stock %in% c("COD-NS","COD-WS","HAD","WHG-NS","WHG-WS","POK","PLE-NS","PLE-EC","SOL-NS",
                             "SOL-EC","TUR",paste0("NEP",c(5:13,32:34,"3A","OTH-NS","OTH-WS"))))
print(tapply(Ca$Landings[idx],list(stock=Ca$stock[idx],year=Ca$Year[idx]),sum,na.rm=T))


# quick check
sort(unique(Ca$Country))
sort(unique(Ca$Year))
sort(unique(Ca$Quarter))
sort(unique(Ca$Metier))
sort(unique(Ca$Vessel_length))
sort(unique(Ca$FDF))
sort(unique(Ca$Area))
sort(unique(Ca$stock))

# 5.5 Export catches =========================

if(export_) {
  write.csv(Ca,file=paste0(res.path,"/01a_ExportAllCatches_beforeAnyWorkingUp_",ver01,".csv"),row.names=FALSE)
  save(Ca,file = paste0(res.path,"/01a_ExportAllCatches_to_",datayear,"_beforeAnyWorkinUp_",ver01,".Rdata"))
}


# 6. Save Catch and Effort data object -----------------------------------------------------------

save.list <- c("Area.Names",  "datayear",
               "Ef", "export_", "res.path", "Ca",
               "start.time", "ver01")

save(list=save.list, file=paste0(res.path,"/01_catch_effort_data.Rdata"))
