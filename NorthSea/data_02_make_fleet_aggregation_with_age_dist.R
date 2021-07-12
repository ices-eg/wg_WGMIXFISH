
## Before: bootstrap/wgmixfish_accessions/.Rdata
## After:  data/NS_Fleet database yyyy-mm-dd/.csv, /.Rdata, /.png

# 0. Guide to section level headers --------------------------------------------
# Repeated trailing symbols (-,=,#) of length >4
# add headers to document outline (in RStudio)

# Level 1 (# X. TITLE ---------------------------- )
# Level 2 (# X.X. TITLE ========================== )
# Level 3 (# X.X.X. TITLE ######################## )


# 1. Initialization -------------------------------------------------------

rm(list = ls(all.names=TRUE)); gc(); graphics.off()


# house cleaning
# ver <- "NS_Fleet database" #
# ver.datetime   <- "23/10/2019";
# cat(paste("\n",ver,"\n",sep=""))#;cat(paste(ver.datetime,"\n\n",sep=""))
start.time <- proc.time()[3]
options(stringsAsFactors = FALSE, scipen=3)

ver01    <- "NS_Fleet_database"
res.path <- paste0("data/",ver01)

# 2. Settings -------------------------------------------------------

# set year
datayear <- 2019 #last data year
datayear_range <-c(2009:datayear)


# define global variables
# StocksWithAgeDistribution <- c("cod.27.47d20","had.27.46a20","ple.27.420","ple.27.7d","pok.27.3a46","sol.27.7d","sol.27.4","whg.27.47d","tur.27.4")
# StocksWithAgeDistributionMIXFISH <- c("COD-NS","HAD","PLE-NS","PLE-EC","POK","SOL-EC","SOL-NS","WHG-NS","TUR")

## YV changed in 2019 as sol.7d not a cat1 stock in 2019
StocksWithAgeDistribution <- c("cod.27.47d20","had.27.46a20","ple.27.420","ple.27.7d","pok.27.3a46","sol.27.4","whg.27.47d","tur.27.4","wit.27.3a47d")
StocksWithAgeDistributionMIXFISH <- c("COD-NS","HAD","PLE-NS","PLE-EC","POK","SOL-NS","WHG-NS","TUR","WIT")


# Look up tables
#(translates code definitions  between Intercatch and Mixfish)
stockTab     <- read.csv(file = "bootstrap/data/lookup_tables/Stock_names.csv", header=T, colClasses="character", sep=";")
ICareaTab    <- read.csv(file = "bootstrap/data/lookup_tables/IC_areas.csv", header=T, colClasses="character")
ICcountryTab <- read.csv(file = "bootstrap/data/lookup_tables/IC_country_codes.csv", header=T, colClasses="character")
veslenTab    <- read.csv(file = "bootstrap/data/lookup_tables/Vessel_lengths.csv", header=T, colClasses="character")


# 3. Load data ---------------------------------------------------------------

# 3.1. catch and effort data from MIXFISH data call =====
load(file = file.path("data", ver01, "01_catch_effort_data.Rdata"))


eff1 <- Ef
sp1 <- Ca
rm(list = c("Ef", "Ca"))

# rename variables
tmp <- match(
  x = c("Country", "Year", "Quarter", "Metier", "Vessel_length", "Area", "kw_days", "Days_at_sea", "No_vessels"),
  table = names(eff1)
)
names(eff1)[tmp] <- c("country", "year", "quarter", "metier", "vessel_length", "area", "kwdays", "tot_days", "num_vessels")

tmp <- match(
  x = c("Country", "Year", "Quarter", "Metier", "Vessel_length", "Area", "Landings", "Discards", "Value"),
  table = names(sp1)
)
names(sp1)[tmp] <- c("country", "year", "quarter", "metier", "vessel_length", "area", "land", "disc", "value")


# make a backup
eff1_backup <- eff1
sp1_backup <- sp1

# 3.2. InterCatch file ==========================

# 3.2.1. North Sea CANUM and WECA #####
IC <- read.csv(file = "bootstrap/data/Intercatch/2019 06 22 WGMIXFISH CANUM WECA for stocks with distributions all WG 2002 2019.csv.gz", header=T, colClasses="character")

#check they are all final status
table(IC$Stock, IC$status)

#### SOME STOCKS in the past i.e. HAD have been exported with TRIAL AND FINAL STATUS!!! REMOVE TRIAL!!
dim(IC)
IC <- subset(IC, status=="Final")
dim(IC)


# 3.2.2. Renaming to fit Mixfish #####
#(consistency between accession/IC)
# any updates to stock table needed??
# rename stocks
IC$stock_IC <- IC$Stock
idx <- match(IC$Stock, stockTab$Stock_name)
IC$Stock <- stockTab$Fcube_stock_name[idx]
sort(unique(IC[is.na(IC$Stock),c("stock_IC")]))

# rename country - update lookup table if necessary
IC$country_IC <- IC$Country
idx <- match(IC$Country, ICcountryTab$Country_name)
IC$Country <- ICcountryTab$Country_code[idx]
sort(unique(IC[is.na(IC$Country),c("country_IC")]))

# rename areas - update lookuptable if necessary
IC$area_IC<-IC$Area
IC$Area <- gsub(" ","",IC$Area)
idx <- match(IC$Area, ICareaTab$IC_area)
IC$Area <- ICareaTab$Fcube_area[idx]
sort(unique(IC[is.na(IC$Area),c("area_IC")]))

# 4. Small Data corrections --------------------------------------------------------
# Intercatch data fixes, edits

# Rename Data year column
names(IC)[1]<-"Datayear"

# Some global corrections here to IC metiers
IC[which(IC$Fleet=="C-Allgears"),"Fleet"] <- "MIS_MIS_0_0_0_HC"
IC[IC$Fleet=="OTB-DEF" & IC$Country %in% "NO","Fleet"] <- "OTB_DEF>=120_0_0_all"
IC <- IC[!IC$Fleet=="FRATRB_IV",] ### remove data that were put for surveys

# make CATON numeric
IC$Weight_Total_kg <- as.numeric(IC$CATON_in_kg)


# 5. Data aggregation and adaptation  ----------------------------------------------------

# 5.1. Age-aggregated (aa) version =====
# aggregate landings/discards
# landings/discards are aggregated to get totals

IC_aa <- subset(IC, CatchCat %in% c("Landings", "Discards"))

# 5.1.1. Filter unique entries #####
# Only need one unique combination (year, season, country, area, fleet, stock)
# as 'Weight_Total_in_kg' is identical for further subcategories
# (age, sex)
lan.key <- apply(IC_aa[,c("Datayear","Stock","Country","Area","Fleet","Season","AgeOrLengthDistribution","CatchCat")], 1, paste, collapse="_")
idx <- match(unique(lan.key), lan.key)
IC_aa <- IC_aa[idx,]
IC_aa <- data.table(IC_aa)

# 5.1.2. Aggregate seasons for landings #####
IC_aa_landings <- data.frame(IC_aa[CatchCat=="Landings",
   list(landings=sum(Weight_Total_kg/1000, na.rm=T)),
   by=list(year=Datayear,
     stock=Stock,
     country=Country,
     area=Area,
     fleet=Fleet)])


test <- aggregate(landings ~ year + stock, data = IC_aa_landings, FUN=sum, na.rm=T)
test[test$year==datayear,]

# 5.1.3. Aggregate seasons for discards #####

IC_aa_discards <- data.frame(IC_aa[CatchCat=="Discards",
  list(discards=sum(Weight_Total_kg/1000, na.rm=T)),
  by=list(year=Datayear,
    stock=Stock,
    country=Country,
    area=Area,
    fleet=Fleet)])


test <- aggregate(discards ~ year + stock, data = IC_aa_discards, FUN=sum, na.rm=T)
test[test$year==datayear,]

# 5.1.4. Make table with Landings and Discards #####
IC_aa <- merge(IC_aa_discards, IC_aa_landings, all=T)

test <- aggregate(list(discards=IC_aa$discards, landings=IC_aa$landings), by=list(year =IC_aa$year, IC_aa$stock), FUN=sum, na.rm=T)
test[test$year==datayear,]



# 5.2. Age-disaggregated (ad) version ==========================

# 5.2.1. Filter to get only aged stocks #################
IC_ad <- subset(
  IC,
  subset = (
    CANUMType %in% "Age" &
      stock_IC %in% StocksWithAgeDistribution
  )
)

# rename ad variables
name.tmp <- names(IC_ad)
names(IC_ad)[which(name.tmp=="MeanWeight_in_g")] <- "MeanWeight_ad_g"
names(IC_ad)[which(name.tmp=="CANUM")] <- "CANUM_ad"

# class definitions
IC_ad$CANUM_ad <- as.numeric(IC_ad$CANUM_ad)
IC_ad$MeanWeight_ad_g <- as.numeric(IC_ad$MeanWeight_ad_g)


# 5.2.2. Aggregate season for landings  ################
IC_lan_ad <- subset(IC_ad, CatchCat=="Landings")

# aggregate CANUM (sum) and MeanWeight_g (weighted mean)
IC_lan_ad_bis <- data.table(IC_lan_ad)
tmp1 <- IC_lan_ad_bis[,
  list(
    MeanWeight_ad_kg = weighted.mean(x = MeanWeight_ad_g/1000, w = MeanWeight_ad_g*CANUM_ad, na.rm=T),
    CANUM_ad = sum(CANUM_ad, na.rm=T)
  ),
  list(
    year = Datayear,
    stock = stock_IC,
    country = Country,
    area = Area,
    metier = Fleet,
    age = ageorlength
  )
  ]

IC_lan_ad <- as.data.frame(tmp1)


# trim and remove NAs (necessary?)
IC_lan_ad <- IC_lan_ad[,c("year","stock", "country", "area", "metier", "age", "MeanWeight_ad_kg", "CANUM_ad")]
IC_lan_ad[is.na(IC_lan_ad)] <- 0


# adjust names to allow merging with discards later
names(IC_lan_ad)[which(names(IC_lan_ad)=="CANUM_ad")] <- "Lan_CANUM_ad"
names(IC_lan_ad)[which(names(IC_lan_ad)=="MeanWeight_ad_kg")] <- "Lan_MeanWeight_ad_kg"


# 5.2.3. Aggregate season for discards  #######################
IC_dis_ad <- subset(IC_ad, CatchCat=="Discards")

# aggregate CANUM (sum) and MeanWeight_in_g (weighted mean)
IC_dis_ad_bis <- data.table(IC_dis_ad)
tmp1 <- IC_dis_ad_bis[,
  list(
    MeanWeight_ad_kg = weighted.mean(x = MeanWeight_ad_g/1000, w = MeanWeight_ad_g*CANUM_ad, na.rm=T),
    CANUM_ad = sum(CANUM_ad,na.rm=T)
  ),
  list(
    year = Datayear,
    stock = stock_IC,
    country = Country,
    area = Area,
    metier = Fleet,
    age = ageorlength
  )
  ]


IC_dis_ad <- as.data.frame(tmp1)


# trim and remove NAs (necessary?)
IC_dis_ad <- IC_dis_ad[,c("year","stock", "country", "area", "metier", "age", "MeanWeight_ad_kg", "CANUM_ad")]
IC_dis_ad[is.na(IC_dis_ad)] <- 0


# adjust names to allow merging with discards later
names(IC_dis_ad)[which(names(IC_dis_ad)=="CANUM_ad")] <- "Dis_CANUM_ad"
names(IC_dis_ad)[which(names(IC_dis_ad)=="MeanWeight_ad_kg")] <- "Dis_MeanWeight_ad_kg"


# 5.2.4. Merge landings and discards #######################
IC_ad <- merge(IC_lan_ad, IC_dis_ad, all=T)
IC_ad[is.na(IC_ad)] <- 0

sum(IC_ad$Dis_CANUM_ad);sum(IC_dis_ad$Dis_CANUM_ad)
sum(IC_ad$Lan_CANUM_ad);sum(IC_lan_ad$Lan_CANUM_ad)


## 5.3 Ref table métiers --------------------------------------------------------


## 5.3.1 Coherence effort/landings Accession --------------------------------------------------------

AccessLan <- data.table(sp1)
AccessLan <- unique(AccessLan[,c('year', 'country', 'metier', 'area')])
AccessLan$Lan <- "x"

AccessEff <- data.table(eff1)
AccessEff <- unique(AccessEff[,c('year', 'country', 'metier', 'area')])
AccessEff$Eff <- "x"

Access <- merge(AccessEff, AccessLan, all=T)
write.table(Access, file= file.path("data", ver01, "diagnostics/testMetierAccession_Eff_Lan.csv"), row.names = F, sep=',')


#'########################################################################################################################
#'##########################################################################################################################
#'##### !!!!!!!
#'##### At this stage need to look at testMetierAccession_Eff_Lan.csv and create a look up table
######  "testMetierAccession_Eff_Lan_Corresp.csv"  to be used to make effort and catch files homogeneous
###### (i.e. find a suitable metier match for when there are landings but no effort)
######  Check the file from last year before moving to the next step
#'##########################################################################################################################
#'##########################################################################################################################




CorrespLanEff <- read.table(file=paste0("bootstrap/data/Fleet_aggregation_matchup_tables",
  "/testMetierAccession_Eff_Lan_Corresp.csv"), header=T, sep=',')
EffMatchto <- CorrespLanEff[is.na(CorrespLanEff$Eff) & CorrespLanEff$matchTo!='',]
ToBeRemovedLan <- CorrespLanEff[is.na(CorrespLanEff$Eff) & CorrespLanEff$matchTo=='',]

pander(ToBeRemovedLan)
pander(EffMatchto)

# remove id with landings without effort associated and which could not been matched
if(nrow(ToBeRemovedLan)>0){
  sp1 <- sp1[-which( paste(sp1$year, sp1$country, sp1$metier,sp1$area)
    %in% paste(ToBeRemovedLan$year ,ToBeRemovedLan$country ,ToBeRemovedLan$metier,ToBeRemovedLan$area)), ]
}


# rename métiers with landings without effort that have been merged
tmp <- eff1
idx <- match(paste(eff1$year, eff1$country, eff1$metier,eff1$area) , paste(EffMatchto$year ,EffMatchto$country ,EffMatchto$matchTo,EffMatchto$area))
eff1$metier[which(!is.na(idx))]  <- EffMatchto$metier[idx[which(!is.na(idx))]]

# check lan and IC métier
ICMet <- data.table(IC_aa_landings)
ICMet <- ICMet[,list(landIC=sum(landings)), by=list(country,metier=fleet,year)]
setorder(ICMet, country,year,-landIC,metier)
ICMet <- ICMet[, percLandIC := cumsum(landIC)/sum(landIC), by=c('country','year') ]
ICMet$year <- as.numeric(ICMet$year)


AccessMet <- data.table(sp1)
AccessMet <- AccessMet[,list(landAccess=sum(land)), by=list(country,metier,year)]
setorder(AccessMet, country,year,-landAccess,metier)
AccessMet <- AccessMet[, percLandAccess := cumsum(landAccess)/sum(landAccess), by=c('country','year') ]


testMet <- merge(ICMet, AccessMet, all=T)
setorder(testMet, country,year,metier,-landAccess)

# filter for years
testMet <- testMet[year %in% datayear_range,]

write.table(testMet, file = file.path("data", ver01, "/diagnostics/testMetierIC_Accession.csv"), row.names = F, sep=',')


#'##########################################################################################################################
#'##########################################################################################################################
#'##### !!!!!!!
#'##### At this stage need to look at testMetierIC_Accession.csv and create a look up table
#'#####  "testMetierIC_AccessionMatchTo.csv"  to be used to make effort and catch files homogeneous
#'#####  Check the file from last year before moving to the next step
#'#####
#'#####  Initial Idea is to match all accession métier to one IC métier that have age disaggregated and discard data
#'##########################################################################################################################
#'##########################################################################################################################

CorrespICLan <- read.table(file=paste0("bootstrap/data/Fleet_aggregation_matchup_tables",
  "/testMetierIC_AccessionMatchTo.csv"), header=T, sep=',')
ICMatchto <- CorrespICLan[is.na(CorrespICLan$landIC) & CorrespICLan$matchTo!='',]

pander(ICMatchto)


# rename métiers in IC to match accession métiers
idx <- match(paste(sp1$year, sp1$country, sp1$metier) , paste(ICMatchto$year ,ICMatchto$country ,ICMatchto$metier))
sp1$metier[which(!is.na(idx))]  <- ICMatchto$matchTo[idx[which(!is.na(idx))]]

idx <- match(paste(eff1$year, eff1$country, eff1$metier) , paste(ICMatchto$year ,ICMatchto$country ,ICMatchto$metier))
eff1$metier[which(!is.na(idx))]  <- ICMatchto$matchTo[idx[which(!is.na(idx))]]

# aggregate the metiers that have been renamed
toto <- sp1
sp1 <- data.table(sp1)
sp1 <- sp1[,
           list(
             land = sum(land,na.rm=T),
             value = sum(value,na.rm=T),
             price = mean(price, na.rm=T) #!# weighted mean?
           ),
           list(
             year = year,
             stock = stock,
             country = country,
             quarter = quarter,
             area = area,
             metier = metier,
             vessel_length = vessel_length,
             FDF = FDF
           )
           ]



toto <- eff1
eff1 <- data.table(eff1)
###!!!! num_vessels does not mean anything once aggregated!!
eff1 <- eff1[,
             list(
               kwdays =sum(kwdays,na.rm=T),
               tot_days = sum(tot_days,na.rm=T),
               num_vessels=max(num_vessels)
             ),
             list(
               year = year,
               country = country,
               quarter = quarter,
               area = area,
               metier = metier,
               vessel_length=vessel_length,
               FDF=FDF
             )
             ]



# 5.3.2. Remove intermediate objects to free up memory #####
rm(list=c("IC_aa_landings", "IC_aa_discards"))
#!# IC_lan_ad, IC_dis_ad, IC_lan_ad_bis, IC_dis_ad_bis could be removed here too?

# 5.3.3. Calculate ratio of discards to landings #####
IC_aa$DiscLandRatio <- IC_aa$discards/IC_aa$landings

# when landings are == 0 and discards > 0 then DR==Inf, set DR=0
if(length(IC_aa$DiscLandRatio=="Inf") > 0){
  IC_aa$DiscLandRatio[IC_aa$DiscLandRatio=="Inf"] <- 0
}

# 5.3.4. Tidy up ###################
# change year to numeric
IC_aa$year <- as.numeric(IC_aa$year)

# add MIXFISH ID string (stock/year/country/metier/area)
IC_aa$IDmatch <- paste(IC_aa$stock,IC_aa$year,IC_aa$country,IC_aa$metier,IC_aa$area,sep="/")

# class changes to variables
IC_aa$country <- as.vector(IC_aa$country)
IC_aa$metier <- as.vector(IC_aa$metier)
IC_aa$area <- as.vector(IC_aa$area)
IC_aa$stock <- as.vector(IC_aa$stock)

# do these IC totals make sense?
test <- aggregate(list(discards=IC_aa$discards, landings=IC_aa$landings), by=list(year=IC_aa$year, IC_aa$stock), FUN=sum, na.rm=T)
test[test$year==datayear,]

# 5.3.5. Renaming to fit Mixfish #####################

# rename stocks
IC_ad$stock_IC <- IC_ad$stock
idx <- match(IC_ad$stock, stockTab$Stock_name)
IC_ad$stock <- stockTab$Fcube_stock_name[idx]
sort(unique(IC_ad$stock)) # check they all got converted

# change year to numeric
IC_ad$year <- as.numeric(IC_ad$year)

# add new Mixfish identifier (stock/year/country/metier/area)
IC_ad$IDmatch <- paste(IC_ad$stock,IC_ad$year,IC_ad$country,IC_ad$metier,IC_ad$area,sep="/")


# class changes to variables
IC_ad$country <- as.vector(IC_ad$country)
IC_ad$metier <- as.vector(IC_ad$metier)
IC_ad$area <- as.vector(IC_ad$area)
IC_ad$stock <- as.vector(IC_ad$stock)


#!# WHat should we refer to here? The advice totals?
# do these IC totals make sense?
test <- aggregate(
  list(
    discards=IC_ad$Dis_MeanWeight_ad_kg * IC_ad$Dis_CANUM_ad/1000,
    landings=IC_ad$Lan_MeanWeight_ad_kg * IC_ad$Lan_CANUM_ad/1000),
  by=list(year=IC_ad$year, IC_ad$stock), FUN=sum, na.rm=T)
test[test$year==datayear,]



# 5.3.6. Aggregation to new Mixfish categories (remove quarter strata)#######

IC_ad_bis <- data.table(IC_ad)
tmp1 <- IC_ad_bis[,
  list(
    Lan_MeanWeight_ad_kg = weighted.mean(x = Lan_MeanWeight_ad_kg, w = Lan_MeanWeight_ad_kg*Lan_CANUM_ad, na.rm = T),
    Lan_CANUM_ad = sum(Lan_CANUM_ad,na.rm=T),
    Dis_MeanWeight_ad_kg = weighted.mean(x = Dis_MeanWeight_ad_kg, w = Dis_MeanWeight_ad_kg*Dis_CANUM_ad, na.rm = T),
    Dis_CANUM_ad = sum(Dis_CANUM_ad, na.rm=T)
  ),
  list(year, stock, country, area, metier, age, IDmatch)
  ]
IC_ad <- as.data.frame(tmp1)

IC_ad[is.na(IC_ad)] <- 0

sum(IC_ad$Dis_CANUM_ad);sum(IC_dis_ad$Dis_CANUM_ad)
sum(IC_ad$Lan_CANUM_ad);sum(IC_lan_ad$Lan_CANUM_ad)


dim(IC_ad); dim(IC_ad_bis); dim(IC_lan_ad)


# 5.3.7. Compute Age proportion and DR at age ######

IC_ad$Lan_ad_t <- IC_ad$Lan_CANUM_ad * IC_ad$Lan_MeanWeight_ad_kg / 1000
IC_ad$Dis_ad_t <- IC_ad$Dis_CANUM_ad * IC_ad$Dis_MeanWeight_ad_kg / 1000

IC_ad_bis <- data.table(IC_ad)
tmp1 <- IC_ad_bis[,
  Lan_aa_t := sum(Lan_ad_t, na.rm = T),
  list(year, stock, country, area, metier, IDmatch)]
#!
tmp1 <- tmp1[,
  Dis_aa_t := sum(Dis_ad_t, na.rm=T),
  list(year, stock, country, area, metier, IDmatch)]

IC_ad <- as.data.frame(tmp1)

IC_ad$pLan_ad_t <- IC_ad$Lan_ad_t / IC_ad$Lan_aa_t
IC_ad$pDis_ad_t <- IC_ad$Dis_ad_t / IC_ad$Dis_aa_t

IC_ad$DRatAge <- IC_ad$Dis_ad_t/IC_ad$Lan_ad_t
IC_ad[IC_ad$DRatAge=="Inf",]$DRatAge <- 0
head(IC_ad)



# 6. Merging Intercatch and Mixfish -------------------------------------------

# 6.1. Landings and Discards aggregation and standardization ===============
# 6.1.1. vessel lengths #####
# unique vessel lengths
sort(unique(sp1$vessel_length))

# lookup FCube vessel length codes
a <- sp1$vessel_length
idx <- match(sp1$vessel_length, veslenTab$Vessel_length)
sp1$vessel_length <- veslenTab$Fcube_vessel_length[idx]
sort(unique(sp1$vessel_length))

# produce report of matchups between IC and sp1
# A check: find which country_year combinations match up between MIXFISH and INTERCATCH submissions
if(T){ # logical - produce summary tables as .csv?
  # country_year combinations for MIXFISH submission
  sp1_comb <- sp1[,c("country", "year")]
  sp1_comb$country_year <- paste(sp1_comb$country, sp1_comb$year, sep="_")
  sp1_comb <- sp1_comb[match(unique(sp1_comb$country_year), sp1_comb$country_year),]

  # country_year combinations for INTERCATCH submission (age-aggregated)
  IC_aa_comb <- IC_aa[,c("country", "year")]
  IC_aa_comb$country_year <- paste(IC_aa_comb$country, IC_aa_comb$year, sep="_")
  IC_aa_comb <- IC_aa_comb[match(unique(IC_aa_comb$country_year), IC_aa_comb$country_year),]

  # country_year combinations for INTERCATCH submission (age-disaggregated)
  IC_ad_comb <- IC_ad[,c("country", "year")]
  IC_ad_comb$country_year <- paste(IC_ad_comb$country, IC_ad_comb$year, sep="_")
  IC_ad_comb <- IC_ad_comb[match(unique(IC_ad_comb$country_year), IC_ad_comb$country_year),]

  # logical: MIXFISH country_year combinations in IC?
  sp1_comb$in_IC_aa <- !is.na(match(sp1_comb$country_year, IC_aa_comb$country_year))
  sp1_comb$in_IC_ad <- !is.na(match(sp1_comb$country_year, IC_ad_comb$country_year))

  # logical: IC_aa country_year combinations in MIXFISH?
  IC_aa_comb$in_sp1 <- !is.na(match(IC_aa_comb$country_year, sp1_comb$country_year))

  # logical: IC_ad country_year combinations in MIXFISH?
  IC_ad_comb$in_sp1 <- !is.na(match(IC_ad_comb$country_year, sp1_comb$country_year))

  # produce .csv reports
  write.csv(sp1_comb,file = file.path("data", ver01,"/diagnostics/02_MIXFISH_submissions_in_IC.csv"))
  write.csv(IC_aa_comb,file = file.path("data", ver01,"/diagnostics/02_IC_aa_submissions_in_MIXFISH.csv"))
  write.csv(IC_ad_comb,file = file.path("data", ver01, "/diagnostics/02_IC_ad_submissions_in_MIXFISH.csv"))

  rm(list = c("sp1_comb", "IC_aa_comb", "IC_ad_comb"))
}

write.csv(IC_aa,file=file.path("data", ver01,"diagnostics/02_standardized IC submission.csv"))
write.csv(sp1, file=file.path("data", ver01, "diagnostics/02_standardized catch submission.csv"))


## 6.1.3. Raising discards in MIXFISH submission data #####
# raise discards according to the average discards/landings ratio for the intercatch strata

# IC_aa and IC_ad are at a yearly scale so aggregate sp1 over quarters +
# aggregate over new métiers created when matching metiers the step before
# KEEP AREA AS IC HAS AREA DEFINED AND DR CAN BE DIFFERENT BY AREAS!

sp1_bis <- data.table(sp1)
sp1_bis <- sp1_bis[,
  list(
   price = weighted.mean(price,land,na.rm=T),
   land=sum(land,na.rm=T),
   value=sum(value, na.rm=T)
  ),
  list(
   year=year,
   stock=stock,
   country=country,
   metier=metier,
   vessel_length=vessel_length,
   area = area#,
   #mesh_size=mesh_size # required???
  )]
sp1 <- as.data.frame(sp1_bis)

test2 <- sp1_bis[, list(land=sum(land)), by=list(year, Group.2=stock)]
(tmp <- merge(test, test2)) #!# What is being tested! IC vs Accession?  POK is so different bewcause Accession doesn't include NOR?
tmp2 <- subset(tmp, year == datayear)
plot(land ~ landings, tmp2, pch = "", log = "xy"); abline(0,1)
text(land ~ landings, tmp2, labels = tmp2$Group.2, cex=0.5)

names(IC_aa)[names(IC_aa)=="fleet"] <- "metier"

tmp <- IC_aa[,c("year", "stock", "country", "metier", "area", "DiscLandRatio")]
names(tmp)

sp1_ <- merge(sp1, tmp, all.x = TRUE)
head(sp1_)
head(sp1)


# calculate Discards for non-NA and > 0 ratios
sp1_[
  !is.na(sp1_$DiscLandRatio) & (sp1_$DiscLandRatio)>0,
  "disc"
  ] <- sp1_[
    !is.na(sp1_$DiscLandRatio) & (sp1_$DiscLandRatio)>0,"land"]*
  sp1_[!is.na(sp1_$DiscLandRatio) & (sp1_$DiscLandRatio)>0,"DiscLandRatio"]

#sp1 <- sp1_[,colnames(sp1)] #back to previous format
sp1 <- sp1_


#check
test2 <- data.table(sp1_)[, list(landAccession=sum(land,na.rm=T), discAccession=sum(disc, na.rm=T)), by=list(year, Group.2=stock)]
test2 <- merge(test, test2)
head(test2)
subset(test2, year == datayear)

#! I understand this difference to be mainly due to missing NOR in Accession
png(file = file.path("data", ver01,"diagnostics/02_CompareIC_Black_Accession_Red_Landings.png"), bg = "transparent")
p <- ggplot(data=test2, aes(x=as.numeric(year),y=landings)) +  geom_line() + facet_wrap(facets=vars(Group.2),nrow = 4,scales="free")+
  geom_line(data=test2, aes(x=as.numeric(year),y=landAccession),col='red',linetype=2) + facet_wrap(facets=vars(Group.2),nrow = 4,scales="free")
print(p)
dev.off()


png(file = file.path("data", ver01,"diagnostics/02_CompareIC_Black_Accession_Red_Discards.png"), bg = "transparent")
p <- ggplot(data=test2, aes(x=as.numeric(year),y=discards)) +  geom_line() + facet_wrap(facets=vars(Group.2),nrow = 4,scales="free")+
  geom_line(data=test2, aes(x=as.numeric(year),y=discAccession),col='red',linetype=2) + facet_wrap(facets=vars(Group.2),nrow=4,scales="free")
print(p)
dev.off()


if(export_) {
  #save(sp1,file=paste(res.path,"02_a_ExportRawLandings_AfterMergingInterCatch_",ver,".Rdata",sep=""))
  write.csv(sp1,file=file.path("data", ver01,"diagnostics/02a_ExportRawLandings_AfterMergingInterCatch.csv"),sep=",",row.names=FALSE)
}



# 6.1.4. Merge age-disaggregated data#####
# ## keep only prop and DRatAge


names(IC_ad)
IC_ad2 <- IC_ad[,c(
  "year", "stock", "country", "metier", "area", "age",
  "pLan_ad_t", "pDis_ad_t",
  "Lan_MeanWeight_ad_kg", "Dis_MeanWeight_ad_kg",
  "Lan_CANUM_ad", "Dis_CANUM_ad",
  "Lan_ad_t", "Lan_aa_t", "Dis_ad_t","DRatAge",
  "IDmatch"
)]

sp1_ad <- merge(sp1, IC_ad2, all.x=T)
dim(sp1_ad)
dim(IC_ad2)
head(sp1_ad)


### recompute the lan_ad_t that were duplicted due to vessel_length missing in IC
sp1_ad$Lan_ad_t <- sp1_ad$land * sp1_ad$pLan_ad_t
sp1_ad$Dis_ad_t <- sp1_ad$Lan_ad_t * sp1_ad$DRatAge

# test
data.table(sp1_ad)[stock=="HAD" & year==datayear,sum(Lan_ad_t,na.rm=T)]
data.table(sp1)[stock=="HAD" & year==datayear, sum(land, na.rm=T)] #!


# test the links and check for missing ages
testAge <- sp1_ad[which(sp1_ad$stock%in%StocksWithAgeDistributionMIXFISH),]
table(testAge$year)

testAgeNoAge <- testAge[is.na(testAge$age),]
testAgeWithAge <- testAge[!is.na(testAge$age),]
table(testAgeNoAge$year)
table(testAgeWithAge$year)


test <- merge(
  aggregate(
    list(NoAge=testAgeNoAge$land),
    by= list(year=testAgeNoAge$year, stock=testAgeNoAge$stock),
    FUN=sum,
    na.rm=T
  ),
  aggregate(
    list(Age=testAgeWithAge$Lan_ad_t),
    by=list(year=testAgeWithAge$year, stock=testAgeWithAge$stock),
    FUN=sum,
    na.rm=T),
  all=T
)
test$ratio <- test$NoAge/(test$NoAge+test$Age)

test[test$stock=="COD-NS",]
test[test$stock=="HAD",]
test[test$stock=="PLE-NS",]
test[test$stock=="SOL-NS",]
test[test$stock=="PLE-EC",]
test[test$stock=="SOL-EC",]
test[test$stock=="WHG-NS",]
test[test$stock=="WIT",]
test[test$stock=="POK",]



#### !!! if ratio != 0 then means that some fleets do not have age associated!!!
#### need for a fix!!!!

# # 6.1.5. Compute Discard ratio and age prop at year/stock scale to be applied to metiers without age comp #####


head(testAgeWithAge)
names(testAgeWithAge)
tmp <- data.table(testAgeWithAge)
tmp2 <- tmp[,
  list(
    Lan_CANUM_ad = sum(Lan_CANUM_ad, na.rm=T),
    Lan_MeanWeight_ad_kg = weighted.mean(Lan_MeanWeight_ad_kg, Lan_MeanWeight_ad_kg*Lan_CANUM_ad, na.rm=T),
    Dis_CANUM_ad = sum(Dis_CANUM_ad, na.rm=T),
    Dis_MeanWeight_ad_kg = weighted.mean(Dis_MeanWeight_ad_kg, Dis_MeanWeight_ad_kg*Dis_CANUM_ad, na.rm=T)
  ),
  by=list(year=year, stock=stock, age=age)
  ]
#
tmp2 <- tmp2[, Lan_ad_t := Lan_CANUM_ad*Lan_MeanWeight_ad_kg/1000]
tmp2 <- tmp2[, Dis_ad_t := Dis_CANUM_ad*Dis_MeanWeight_ad_kg/1000]
tmp2 <- tmp2[, Lan_aa_t := sum(Lan_ad_t, na.rm=T), by=list(year=year, stock=stock)]
tmp2 <- tmp2[, Dis_aa_t := sum(Dis_ad_t, na.rm=T), by=list(year=year, stock=stock)]
#

# # Discard ratio at age
tmp2 <- tmp2[, pLan_ad_t := Lan_ad_t/Lan_aa_t]
tmp2 <- tmp2[, pDis_ad_t := Dis_ad_t/Dis_aa_t]
tmp2$age <- as.numeric(as.character(tmp2$age))
#
tmp2[is.na(tmp2)] <- 0
tmp2 <- tmp2[, DRatAge := Dis_ad_t/Lan_ad_t]
tmp2 <- tmp2[DRatAge=="Inf", DRatAge:=0]


tmp2[is.na(tmp2)] <- 0
#


png(file = file.path("data", ver01,"diagnostics/02_CheckAgeDisagg_catchAtAge.png"), bg = "transparent")
  p <- xyplot(pLan_ad_t + pDis_ad_t ~ age | stock, groups=year, data = tmp2)
  print(p)
dev.off()

png(file = file.path("data", ver01,"diagnostics/02_CheckAgeDisagg_meanWtAtAge.png"), bg = "transparent")
  p <- xyplot(Dis_MeanWeight_ad_kg + Lan_MeanWeight_ad_kg ~ age | stock, groups=year, data = tmp2)
  print(p)
dev.off()

png(file = file.path("data", ver01,"diagnostics/02_CheckAgeDisagg_DRAtAge1.png"), bg = "transparent")
  p <- xyplot(DRatAge  ~ age | stock, groups=year, data = tmp2)
  print(p)
dev.off()

png(file = file.path("data", ver01,"diagnostics/02_CheckAgeDisagg_DRAtAge2.png"), bg = "transparent")
  p <- xyplot(DRatAge  ~ age | stock, groups=year, data = tmp2,ylim=c(0,2))
  print(p)
dev.off()

xyplot(Dis_MeanWeight_ad_kg + Lan_MeanWeight_ad_kg  ~ age | stock, groups=year, data = tmp2[stock=="HAD",])

#
tmp2 <- as.data.frame(tmp2[,c(
  "year","stock","age",
  "pLan_ad_t", "pDis_ad_t",
  "Dis_MeanWeight_ad_kg","Lan_MeanWeight_ad_kg",
  "DRatAge"
), with=F]) #!# What is "with" here?

#! Set upper limit to DR so that discards don't balloon ?

# # merge it with table without age distrib
testAgeNoAge <- merge(
  testAgeNoAge[,-which(colnames(testAgeNoAge)%in%
      c("age" ,"pLan_ad_t", "pDis_ad_t","Lan_MeanWeight_ad_kg", "Dis_MeanWeight_ad_kg","DRatAge"))],
  tmp2, all=T)

# # dim(testAgeNoAge)
testAgeNoAge <- testAgeNoAge[!is.na(testAgeNoAge$metier),]
testAgeNoAge <- data.table(testAgeNoAge)
## compute numbers and tonages at age
testAgeNoAge <- testAgeNoAge[,Lan_aa_t:=land]
testAgeNoAge <- testAgeNoAge[,Lan_ad_t:=Lan_aa_t*pLan_ad_t]
testAgeNoAge <- testAgeNoAge[,Lan_CANUM_ad:=Lan_ad_t/(Lan_MeanWeight_ad_kg/1000)]
# #
testAgeNoAge <- testAgeNoAge[,Dis_ad_t:=Lan_ad_t*DRatAge]
testAgeNoAge <- testAgeNoAge[,Dis_CANUM_ad:=Dis_ad_t/(Dis_MeanWeight_ad_kg/1000)]
testAgeNoAge <- data.frame(testAgeNoAge)
#
#
#
# ## compute DR at age for testAgeWithAge
testAgeWithAge <- data.table(testAgeWithAge)
testAgeWithAge <- testAgeWithAge[, DRatAge:=Dis_ad_t/Lan_ad_t]
testAgeWithAge <- testAgeWithAge[DRatAge=="Inf", DRatAge:=0]
#! NaN is OK for DRatAge?
#
#
#
#
# # Combine table with and table without age comp
test <- rbind(data.frame(testAgeWithAge), testAgeNoAge)
dim(test);dim(sp1_ad) #! test is larger than sp1_ad since ages have been added


#!# Check how much the addition of age information raised the
# landings at age

toto <- data.table(sp1_ad)
tata <- data.table(test)

toto[stock=="COD-NS",sum(Lan_ad_t,na.rm=T), by=list( year, stock)]
tata[stock=="COD-NS",sum(Lan_ad_t,na.rm=T), by=list( year, stock)]

toto[stock=="COD-NS",sum(Dis_ad_t,na.rm=T), by=list( year, stock)]
tata[stock=="COD-NS",sum(Dis_ad_t,na.rm=T), by=list( year, stock)]

# summarise for all stocks
tab1 <- toto[stock %in% StocksWithAgeDistributionMIXFISH,sum(Lan_ad_t,na.rm=T), by=list( year, stock)]
tab2 <- tata[stock %in% StocksWithAgeDistributionMIXFISH,sum(Lan_ad_t,na.rm=T), by=list( year, stock)]

setorder(tab1,year,stock)
setorder(tab2,year,stock)

tab3 <- cbind(tab1,tab2$V1)
tab3$ratio <- tab3$V1/tab3$V2
names(tab3)[3:4] <- c("unraised","raised")

tab3[tab3$stock %in% "COD-NS",]
tab3[tab3$stock %in% "POK",]
tab3[tab3$stock %in% "PLE-NS",]

#
#
# # ### compute discards and age comp
#
# ### keep only propLand and propDisc and DR at age to compute everything from sp1

#!#
# sp1_ad <- test[, c(
#   "year","stock","country","area","metier","vessel_length",
#   "age","IDmatch","pLan_ad_t", "pDis_ad_t", "DRatAge","Lan_MeanWeight_ad_kg",
#   "Dis_MeanWeight_ad_kg","Lan_ad_t","Dis_ad_t" )]
sp1_ad <- test

#***** a check *****#
tmpp <- subset(
  sp1_ad,
  subset = (
    year == datayear &
      country == "BE" &
      stock == "SOL-NS" &
      metier %in% c("TBB_DEF_>=120_0_0_all")
  )
)
tmpp
sum(tmpp$Lan_ad_t, na.rm=T); unique(tmpp$land); sum(tmpp$pLan_ad_t, na.rm=T)
sum(tmpp$Dis_ad_t, na.rm=T); unique(tmpp$disc); sum(tmpp$pDis_ad_t, na.rm=T)

#! both land and Lan_aa_t exist at this point
#! land is the one used, but Lan_aa could be updated and used going forward for clarity

sum(tmpp$Lan_ad_t, na.rm=T); unique(tmpp$land); unique(tmpp$Lan_aa_t);

# 6.1.6 merge with sp1 and compute age distrib  ==========================

#! this section is now irrelevant given the above merging
test4 <- data.table(sp1_ad)[, list(landAccessionAge=sum(Lan_ad_t,na.rm=T), discAccessionAge=sum(Dis_ad_t, na.rm=T)), by=list(year, Group.2=stock)]
test4 <- merge(test2, test4)
test4[test4$year==datayear,]



# 6.1.7 missing ages?  ==========================

png(file = file.path("data", ver01,"diagnostics/02_CompareIC_Black_Accession_Red_AccessionAge_Blue.png"), bg = "transparent")
  p <- ggplot(data=test4, aes(x=as.numeric(year),y=landings)) +  geom_line() + facet_wrap(facets=vars(Group.2),nrow=2,scales="free")+
    geom_line(data=test4, aes(x=as.numeric(year),y=landAccession),col='red',linetype=2) + facet_wrap(facets= vars(Group.2),nrow=2,scales="free")+
    geom_line(data=test4, aes(x=as.numeric(year),y=landAccessionAge),col='blue',linetype=4) + facet_wrap(facets=vars(Group.2),nrow=2,scales="free")
  print(p)
dev.off()

png(file = file.path("data", ver01,"diagnostics/02_CompareDISCARDS_IC_Black_Accession_Red_AccessionAge_Blue.png"), bg = "transparent")
  p <- ggplot(data=test4, aes(x=as.numeric(year),y=discards)) +  geom_line() + facet_wrap(facets=vars(Group.2),nrow=2,scales="free")+
    geom_line(data=test4, aes(x=as.numeric(year),y=discAccession),col='red',linetype=2) + facet_wrap(facets=vars(Group.2),nrow=2,scales="free")+
    geom_line(data=test4, aes(x=as.numeric(year),y=discAccessionAge),col='blue',linetype=4) + facet_wrap(facets=vars(Group.2),nrow=2,scales="free")+
    ylim(0,200000)
  print(p)
dev.off()

if(export_) {
  #save(sp1,file=paste(res.path,"/02_a_ExportRawLandings_AfterMergingInterCatch_",ver,".Rdata",sep=""))
  write.csv(sp1_ad,file=file.path("data", ver01,"diagnostics/02a_ExportRawLandingsAgeDist_AfterMergingInterCatch.csv"),sep=",",row.names=FALSE)
}

# 6.2. Effort (eff1) aggregation and standardisation ============
# AGGREGATION AND STANDARDISATION OF EFFORT BY FLEETS AND METIERS- USING THE TR1/TR2 TEMINOLOGY AS THE BASIS


# Define meshsize
im <- unique(unlist(lapply(strsplit(eff1$metier,"_"),length)))
ic <- data.frame(ictag=(eff1$metier))
ic$im <- NA
ic$im <- matrix(unlist(lapply(strsplit(eff1$metier,"_"),length)),byrow=T,ncol=1)
ic <- unique(ic)

eff1_backup <- eff1

eff1 <- subset(eff1, !metier %in% unique(ic[ic$im<5,])$ictag) #! subset of complete metier codes
eff1$mesh_size <- unlist(lapply(strsplit(eff1$metier,"_"),function(x) {
  ms <- paste(x[[3]],x[[4]],x[[5]],sep="_")
  return(ms)}))



sp1_ad_backup <- sp1_ad
sp1_backup <- sp1

# get number of "parts" of metier for each metier
imSp <- unique(unlist(lapply(strsplit(sp1_ad$metier,"_"),length)))
icSp <- data.frame(ictag=(sp1_ad$metier))
icSp$imSp <- NA
icSp$imSp <- matrix(unlist(lapply(strsplit(sp1_ad$metier,"_"),length)),byrow=T,ncol=1)
icSp <- unique(icSp)

## remove 'irrelevant' métier (hard to come back to IC etc at this step but should not represent huge landings)
sp1_ad <- subset(sp1_ad, !metier %in% unique(icSp[icSp$imSp<5,])$ictag)
data.table(sp1_ad)[year==datayear & Lan_ad_t>0, sum(Lan_ad_t, na.rm=T), by=list(year, stock)]
data.table(sp1_ad_backup)[year==datayear & Lan_ad_t>0, sum(Lan_ad_t, na.rm=T), by=list(year, stock)]

data.table(sp1_ad)[year==datayear & Dis_ad_t>0, sum(Dis_ad_t, na.rm=T), by=list(year, stock)]
data.table(sp1_ad_backup)[year==datayear & Dis_ad_t>0, sum(Dis_ad_t, na.rm=T), by=list(year, stock)]


sp1_ad$mesh_size <- unlist(lapply(strsplit(sp1_ad$metier,"_"),function(x) {
  ms <- paste(x[[3]],x[[4]],x[[5]],sep="_")
  return(ms)}))



## remove 'irrelevant' métier (hard to come back to IC etc at this step but should not represent huge landings)

#! this was repeated for sp1 as above for sp1_ad
imSp <- unique(unlist(lapply(strsplit(sp1$metier,"_"),length)))
icSp <- data.frame(ictag=(sp1$metier))
icSp$imSp <- NA
icSp$imSp <- matrix(unlist(lapply(strsplit(sp1$metier,"_"),length)),byrow=T,ncol=1)
icSp <- unique(icSp)

sp1 <- subset(sp1, !metier%in%unique(icSp[icSp$imSp<5,])$ictag)
data.table(sp1_backup)[year==datayear & land>0, sum(land, na.rm=T), by=list( stock)]
data.table(sp1)[year==datayear & land>0, sum(land, na.rm=T), by=list( stock)]



sp1$mesh_size <- unlist(lapply(strsplit(sp1$metier,"_"),function(x) {
  ms <- paste(x[[3]],x[[4]],x[[5]],sep="_")
  return(ms)}))


# 5.2.1. Assign basic fleet category #####
source("bootstrap/software/functions/FunctionDefineFleetCategories.r")

eff1 <- spfunction(eff1)
sp1 <- spfunction(sp1)
sp1_ad <- spfunction(sp1_ad)

unique(eff1$ID)
unique(eff1$ID)%in%unique(sp1$ID)
unique(eff1$ID)[!unique(eff1$ID)%in%unique(sp1$ID)] # effort no catch
unique(sp1$ID)[!unique(sp1$ID)%in%unique(eff1$ID)] # catch no effort



# 6.2.5. effort aggregation ------------------------------------------------------
 unique(sp1_ad$area); unique(eff1$area)
table(eff1$country, eff1$area)

# aggregate effort
eff1.eff <- aggregate(eff1[,c("kwdays","tot_days")], by = list(year=eff1$year,ID=eff1$ID),sum, na.rm=T) # max over sp

# aggregate capacity
# For BE and NL, max number of vessels is used. For others, sum number of vessels
#
# # # SUM
eff1.no_NL_BE <- subset(eff1, !country %in% c("BE", "NL"))
eff1.no_NL_BE.cap <- aggregate(
  eff1.no_NL_BE$num_vessels,
  by = list(year=eff1.no_NL_BE$year, fleet=eff1.no_NL_BE$fleet, ID=eff1.no_NL_BE$ID, quarter=eff1.no_NL_BE$quarter),
  sum, na.rm=T
)

# MAX
eff1.NL_BE <- subset(eff1, country %in% c("BE", "NL"))
eff1.NL_BE.cap <- aggregate(
  eff1.NL_BE$num_vessels,
  by = list(year=eff1.NL_BE$year, fleet=eff1.NL_BE$fleet, ID=eff1.NL_BE$ID, quarter=eff1.NL_BE$quarter),
  max, na.rm=T
)
#
eff1.cap <- rbind(eff1.no_NL_BE.cap, eff1.NL_BE.cap)
names(eff1.cap)[which(names(eff1.cap)=="x")] <- "num_vessels"

#Max across quarters gives yearly capacity
eff1.cap <- aggregate(eff1.cap$num_vessels, by = list(year=eff1.cap$year, fleet=eff1.cap$fleet), max, na.rm=T)
names(eff1.cap)[which(names(eff1.cap)=="x")] <- "num_vessels"
eff1.cap$year <- as.numeric(eff1.cap$year); eff1.cap$fleet <- as.character(eff1.cap$fleet)

# 5.3. Catch (sp1, sp1_ad) aggregation and standardization ==============
# AGGREGATION AND STANDARDISATION OF CATCH BY FLEETS AND METIERS- USING THE TR1/TR2 TEMINOLOGY AS THE BASIS

sp1_backup <- sp1
sp1_ad_backup <- sp1_ad
# sp1 <- sp1_backup ; sp1_ad <- sp1_ad_backup






# 5.3.2 Aggregation by fleet / metier #####

tmp <- data.table(sp1)
tmp <- tmp[,
           list(
             price = weighted.mean(price, price*land, na.rm=T),
             land = sum(land, na.rm=T),
             value = sum(value, na.rm=T),
             disc = sum(disc, na.rm=T)
           ),
           list(
             year = year,
             stock=stock,
             country=country,
             area=area,
             metier=metier,
             vessel_length=vessel_length,
             fleet=fleet, fl2=fl2, fl1=fl1, ID=ID, ID2=ID2
           )
           ]

tmp[,list(lan=sum(land), dis=sum(disc)), by=list(year, stock)]


dim(tmp); dim(sp1)
sp1 <- data.frame(tmp)

# Is this still correct?
# weighted.mean by Dis_MeanWeight_ad_kg*land ?
# I guess $land and $disc are proportional, so it shouldn't matter
data.table(sp1_ad)[,list(lan=sum(Lan_ad_t, na.rm=T), dis=sum(Dis_ad_t, na.rm=T)), by=list(year, stock)]

tmp <- data.table(sp1_ad)
tmp <- tmp[, land:=sum(Lan_ad_t, na.rm=T), by=list(year, stock, country, area, vessel_length, fleet, fl2, fl1, ID, ID2)]

tmp <- tmp[,
           list(
             Lan_MeanWeight_ad_kg = weighted.mean(Lan_MeanWeight_ad_kg, Lan_MeanWeight_ad_kg * land, na.rm=T),
             Dis_MeanWeight_ad_kg = weighted.mean(Dis_MeanWeight_ad_kg, Dis_MeanWeight_ad_kg * land, na.rm=T), ### land?
             Lan_ad_t = sum(Lan_ad_t, na.rm=T),
             Dis_ad_t = sum(Dis_ad_t, na.rm=T)
           ),
           list(
             year=year,
             stock=stock,
             country=country,
             area=area,metier=metier,
             vessel_length=vessel_length,
             age=age,
             fleet=fleet, fl2=fl2, fl1=fl1, ID=ID, ID2=ID2
           )
           ]


tmp[stock=="SOL-NS",list(lan=sum(Lan_ad_t), dis=sum(Dis_ad_t)), by=list(year, stock)] #!#
tmp[stock=="COD-NS",list(lan=sum(Lan_ad_t), dis=sum(Dis_ad_t)), by=list(year, stock)]

dim(tmp); dim(sp1_ad)
sp1_ad <- data.frame(tmp)


# recalculate CANUM_ad
sp1_ad$Lan_CANUM_ad <- sp1_ad$Lan_ad_t * 1000 / sp1_ad$Lan_MeanWeight_ad_kg
sp1_ad$Dis_CANUM_ad <- sp1_ad$Dis_ad_t * 1000 / sp1_ad$Dis_MeanWeight_ad_kg
head(sp1_ad)


#***** a check *****#
tmpp1 <- subset(
  IC_ad,
  subset = (
    year == 2018 &
      country == "BE" &
      stock == "SOL-NS" &
      metier %in% c("TBB_DEF_70-99_0_0_all")
  )
)
tmpp1


tmpp2 <- subset(
  sp1_ad,
  subset = (
    year == 2018 &
      country == "BE" &
      stock == "SOL-NS" &
      metier == "BT2"
  )
)
tmpp2
# compare landings at age
sum(tmpp1$Lan_ad_t)
sum(tmpp2$Lan_ad_t)

tmpp3 <- as.data.table(tmpp2)
tmpp3 <- tmpp3[,
               .(
                 Lan_MeanWeight_ad_kg = weighted.mean(x = Lan_MeanWeight_ad_kg, w = Lan_MeanWeight_ad_kg*Lan_CANUM_ad),
                 Dis_MeanWeight_ad_kg = weighted.mean(x = Dis_MeanWeight_ad_kg, w = Dis_MeanWeight_ad_kg*Dis_CANUM_ad)
               ),
               .(
                 metier=metier, age=age
               )
               ]
plot(Lan_MeanWeight_ad_kg~age, tmpp3)
points(Lan_MeanWeight_ad_kg~age, tmpp1, col=2, pch=20)

sum(tmpp1$Lan_ad_t)
sum(tmpp2$Lan_ad_t)

sum(tmpp1$Dis_ad_t, na.rm=T)
sum(tmpp2$Dis_ad_t, na.rm=T)



#...................................

#### age dist for sp1 - merge


### CHECK - THIS is new stuff from Youen ---------------------------------

# 5.4. Remove fleets without effort ======


# CHECK MATCH BETWEEN EFFORT AND CATCHES!
spid <- sort(unique(sp1$ID));length(spid)
sp_adid<- sort(unique(sp1_ad$ID));length(sp_adid)

efid <- sort(unique(eff1$ID)); length(efid)

#effort but no catches
efid[is.na(match(efid,spid))] #! I have no idea if these are issues (looks like a lot of "oth" metiers)
#catches but no effort
spid[is.na(match(spid, efid))] #! mainly other areas
#catches_ad but no effort
sp_adid[is.na(match(sp_adid, efid))]

# subset(sp1_ad, ID=="GE_Otter>=40.TR1.6A")

CatchesNoEffort <- spid[!spid %in% efid]
CatchesNoEffort

CatchesNoEffort_ad <- sp_adid[!sp_adid %in% efid]
CatchesNoEffort_ad

CatchesNoEffort[!CatchesNoEffort %in% CatchesNoEffort_ad]
CatchesNoEffort_ad[!CatchesNoEffort_ad %in% CatchesNoEffort]


tmp <- data.table(sp1[sp1$ID %in% unique(CatchesNoEffort, CatchesNoEffort_ad),])
tmp[year==datayear, sum(land, na.rm=TRUE), by=list(year, stock)]

#! Mostly an issue for otherstocks not dealt with -
#! Why don't we filter Accession for only relevent stocks at the outset?
#! We have carried them all the way through at this point...
#! I guess this is mainly done in script 3...

tmp1 <- tmp[year==datayear, sum(land, na.rm=TRUE), by=list(year, stock)]
tmp2 <- data.table(sp1)[year==datayear, sum(land, na.rm=TRUE), by=list(year, stock)]
names(tmp2)[3] <- "TOT"


tmp3 <- merge(
  x = tmp2,
  y = tmp1, all.x = TRUE)
tmp3$pCnoE <- tmp3$V1/tmp3$TOT
as.data.frame(tmp3)
#!


sp1 <- sp1[!sp1$ID %in% unique(CatchesNoEffort, CatchesNoEffort_ad),]
sp1_ad <- sp1_ad[!sp1_ad$ID %in% unique(CatchesNoEffort, CatchesNoEffort_ad),]


# CHECK MATCH BETWEEN EFFORT AND CATCHES!
#there need some more check there...
spid2<- sort(unique(sp1$ID2)) ;length(spid2)
spadid2<- sort(unique(sp1_ad$ID2)) ;length(spadid2)
efid2 <- sort(unique(eff1$ID2)) ;length(efid2)

xx <- efid2[is.na(match(efid2,spid2))];xx[grep(datayear,xx)]
xx_ad <- efid2[is.na(match(efid2,spadid2))]; xx_ad[grep(datayear,xx_ad)]
yy <- spid2[is.na(match(spid2,efid2))];yy[grep(datayear,yy)]
yy_ad <- spadid2[is.na(match(spadid2,efid2))];yy_ad[grep(datayear,yy_ad)]

CatchesNoEffort_id2 <- spid2[!spid2 %in% efid2]
CatchesNoEffort_ad_id2 <- spadid2[!spadid2 %in% efid2]

# check sum of landings for ID2 catch no effort
tmp <- data.table(sp1[sp1$ID2 %in% unique(CatchesNoEffort_id2, CatchesNoEffort_ad_id2),])
if(nrow(tmp)>0){
  write.csv(tmp, file = file.path("data", ver01,"diagnostics/02_CatchesNoEffort_id2_aa_and_ad.csv"))

  tmp2<-tmp[,sum(land), by=list(year, stock)]
  tmp2[order(-tmp2$V1),]

  spp. <- tmp2[order(-tmp2$V1),stock][1]

  tmp[stock %in% spp.]
}

# are the landings large enough that we should try to correct this mismatch?


# 5.5. Remove irrelevant / small metiers ----------------------------------

#...................................
#  PROCESSING - REMOVING IRRELEVANT / SMALL METIERS
#to do next year : check the danish TR1.3AN  ... and the other mismatches
#...................................


sp1.sp <- aggregate(
  sp1[,c("land", "disc","value")],
  by = list(year=sp1$year, ID=sp1$ID, stock=sp1$stock),
  sum, na.rm=T
)

tmp1 <- data.table(sp1_ad)
tmp1 <- tmp1[,
             list(
               Lan_MeanWeight_ad_kg = weighted.mean(x = Lan_MeanWeight_ad_kg, w = Lan_MeanWeight_ad_kg*Lan_CANUM_ad, na.rm = T),
               Lan_ad_t = sum(Lan_ad_t,na.rm=T),
               Dis_MeanWeight_ad_kg = weighted.mean(x = Dis_MeanWeight_ad_kg, w = Dis_MeanWeight_ad_kg*Dis_CANUM_ad, na.rm = T),
               Dis_ad_t = sum(Dis_ad_t, na.rm=T)
             ),
             list(year, ID, stock, age)
             ]
sp1_ad.sp <- as.data.frame(tmp1)

# recalculate CANUM
sp1_ad.sp$Lan_CANUM_ad <- sp1_ad.sp$Lan_ad_t / sp1_ad.sp$Lan_MeanWeight_ad_kg * 1000
sp1_ad.sp$Dis_CANUM_ad <- sp1_ad.sp$Dis_ad_t / sp1_ad.sp$Dis_MeanWeight_ad_kg * 1000



eff <- eff1.eff
cap <- eff1.cap
sp <- sp1.sp
sp_ad <- sp1_ad.sp

eff$fleet <- unlist(lapply(strsplit(eff$ID,split="\\."),function(x) x[1]))
effcap <- merge(eff,cap)



# 5.6. Remove 3a and 7d for stocks that don't extend there  ======

#
#...................................
#NEW SPECIES
#...................................
# keep only the relevant info in 3a and 7d for the stocks who extend there
# sp_ <- sp; sp_ad_ <- sp_ad # backup
# sp <- sp_; sp_ad <- sp_ad_ # reload

#
# filterAreaStock <- function(SP){
#   # lookup table
#   lut <- rbind(
#     # 4
#     expand.grid(area = "4", stock = unique(sp$stock)),
#     # 7D
#     expand.grid(area = "7D", stock = c("COD", "WHG", "PLE-EC", "SOL-EC", "BLL", "LEM", "MUR")),
#     # 3AN
#     expand.grid(area = "3AN",
#       stock = c("COD-NS", "HAD", "POK", "ANF", "PLE-NS", "BLL", "DAB", "FLE", "HKE", "LEM", "MUR")),
#     # 6A
#     expand.grid(area = "6A",
#       stock = c("HAD", "POK", "ANF", "HKE")
#     )
#   )
#   lut$cpue <- ifelse(lut$stock %in% c("ANF", "BLL", "DAB","HKE", "LEM", "MUR", "FLE"), TRUE, FALSE)
#
#   if(grepl("CPUE",ver)){
#     lut.tmp <- lut
#   } else {
#     lut.tmp <- subset(lut, subset = !cpue)
#   }
#
#   SP2 <- vector("list", nrow(lut.tmp))
#   for(i in seq(SP2)){
#     SP2[[i]] <- subset(SP, subset =
#       grepl(paste0("\\.", lut.tmp$area[i]), SP$ID) &
#       grepl(lut.tmp$stock[i], SP$stock)
#     )
#   }
#   SP2 <- do.call("rbind", SP2)
#   return(SP2)
#
# }
#
# sp <- filterAreaStock(sp)
# sp_ad <- filterAreaStock(sp_ad)
#
# # OLD #
# if(grepl("CPUE",ver)){
#   sp_1 <- sp[grep("\\.4",sp$ID),]
#
#   sp_2 <- sp[grep("\\.7D",sp$ID),]; sp_21 <- sp_2[grep("COD",sp_2$stock),]; sp_22 <- sp_2[grep("WHG",sp_2$stock),];
#   sp_23 <- sp_2[grep("PLE",sp_2$stock),]; sp_24 <- sp_2[grep("SOL",sp_2$stock),]; sp_25 <- sp_2[grep("BLL",sp_2$stock),];
#   sp_26 <- sp_2[grep("LEM",sp_2$stock),]; sp_27 <- sp_2[grep("MUR",sp_2$stock),]
#
#   sp_3 <- sp[grep("\\.3AN",sp$ID),];sp_31 <- sp_3[grep("COD",sp_3$stock),]; sp_32 <- sp_3[grep("HAD",sp_3$stock),];
#   sp_33 <- sp_3[grep("POK",sp_3$stock),]; sp_34 <- sp_3[grep("ANF",sp_3$stock),]; sp_35 <- sp_3[grep("PLE",sp_3$stock),];
#   sp_36 <- sp_3[grep("BLL",sp_3$stock),]; sp_37 <- sp_3[grep("DAB",sp_3$stock),]; sp_38 <- sp_3[grep("FLE",sp_3$stock),];
#   sp_39 <- sp_3[grep("HKE",sp_3$stock),]; sp_310 <- sp_3[grep("LEM",sp_3$stock),]; sp_311 <- sp_3[grep("MUR",sp_3$stock),] #; sp_36 <- sp_3[grep("NEP",sp_3$stock),]
#
#   sp_4 <- sp[grep("\\.6A",sp$ID),];sp_41<-sp_4[grep("ANF",sp_4$stock),];sp_42<-sp_4[grep("HAD",sp_4$stock),];
#   sp_43<-sp_4[grep("POK",sp_4$stock),];sp_44<-sp_4[grep("HKE",sp_4$stock),];
#
#   sp<-rbind(sp_1,sp_21,sp_22,sp_23,sp_24,sp_25,sp_26,sp_27,sp_31,sp_32,sp_33,sp_34,sp_35,sp_36,sp_37,sp_38,sp_39,sp_310,sp_311,sp_41,sp_42,sp_43,sp_44)
# }else{
#   sp_1 <- sp[grep("\\.4",sp$ID),]
#
#   sp_2 <- sp[grep("\\.7D",sp$ID),]; sp_21 <- sp_2[grep("COD",sp_2$stock),]; sp_22 <- sp_2[grep("WHG",sp_2$stock),];
#   sp_23 <- sp_2[grep("PLE",sp_2$stock),]; sp_24 <- sp_2[grep("SOL",sp_2$stock),];
#
#   sp_3 <- sp[grep("\\.3AN",sp$ID),];sp_31 <- sp_3[grep("COD",sp_3$stock),]; sp_32 <- sp_3[grep("HAD",sp_3$stock),];
#   sp_33 <- sp_3[grep("POK",sp_3$stock),]; sp_34 <- sp_3[grep("PLE",sp_3$stock),];
#
#   sp_4 <- sp[grep("\\.6A",sp$ID),];sp_41<-sp_4[grep("HAD",sp_4$stock),];
#   sp_42<-sp_4[grep("POK",sp_4$stock),];
#
#   sp<-rbind(sp_1,sp_21,sp_22,sp_23,sp_24,sp_31,sp_32,sp_33,sp_34,sp_41,sp_42)
#
# }


#! CHOICE OF AGGREGATION THRESHOLD - here we test several levels under which the metiers are considered as small and
# are aggregated as "OTH". The level is at least XX tons by year in average in at least on of the species.
# this is in aboslute terms, not relative, maybe not so smart, could be changed in the future?


#
#tot.land <- sum(sp$land) #sum all years all species
tot.land <- sum(sp[sp$year==datayear,"land"]) #sum all years all species

threshold.lev <- data.frame(cbind(threshold=0,n.unit=length(unique(sp$ID)), land=tot.land))


#for (min.land in c(seq(0,0.04,by=0.005),0.005)) { # test of several min.land. Final choice is 250 (5%)!!
min.land  <- 0.01 # test of several min.land.

print(min.land)
#min.land <- 300



# removing metiers with no catches

sp3_ <- tapply(sp$land, list(sp$ID,sp$year,sp$stock),FUN=sum, na.rm=T)
#sp3[is.na(sp3)]<-0
#sp3<-apply(sp3_,c(1,3),sum)

#! nota - by not using na.rm here we show ID which do not have data for all stocks in all years

# nb: IN 2012 WE CHNGE THIS AND USE THE METIER PRESENT IN 2011 AS THE CRITERIA
sp3 <- sp3_[,as.character(datayear),]

#sp.lst2 <- c("COD-NS","HAD","PLE-EC","PLE-NS","POK","SOL-EC","SOL-NS","WHG-NS")

#! A metier must catch at least 1% of a single stock to be counted as
#! a "large metier". Unfortunately, all stocks are assessed,
#! and only needs to qualify with a single stock
#! At this point many stocks are included that are not dealt with in the advice

List.ID.null <- NULL #list of ID that are not Null ;-)
List.ID.oth <- NULL #list of ID that are not oth ;-)
# for (S in sp.lst2) {
for (S in colnames(sp3)){
  x <- as.data.frame(sp3[!is.na(sp3[,S]) & sp3[,S]>0, S])    #used to remove irrelevant metiers
  names(x) <- S
  tot. <- sum(sp3[,S],na.rm=T)
  y <- as.data.frame(sp3[!is.na(sp3[,S]) & sp3[,S]>min.land*tot.,S])  #used to aggregate small metiers in others
  names(y) <- S
  ma.x <- match(row.names(x),dimnames(sp3)[[1]])
  ma.y <- match(row.names(y),dimnames(sp3)[[1]])
  List.ID.null<-c(List.ID.null,dimnames(sp3)[[1]][ma.x])
  List.ID.oth<-c(List.ID.oth,dimnames(sp3)[[1]][ma.y])
  #print(S)
  #print(dimnames(sp3)[[1]][ma.y])
}
List.ID.null<-sort(unique(List.ID.null))
List.ID.oth<-sort(unique(List.ID.oth))
length(List.ID.null)
length(List.ID.oth)

#manual removal - need further check whythis appears
#List.ID.null <- List.ID.null[!List.ID.null=="FR_Otter10-40.OTB32-69.6A"]
#List.ID.oth <- List.ID.oth[!List.ID.oth=="FR_Otter10-40.OTB32-69.6A"]

#summing the landings of irrelevant metiers
# TODO - CatchesNoEffort filter not necessary? Done above.
sp.irr <- sp[!sp$ID %in% List.ID.null | sp$ID %in% CatchesNoEffort,]
sp.irr <- aggregate(sp.irr[,c("land","disc","value")],list(year=sp.irr$year,stock=sp.irr$stock),sum,na.rm=T)
sp_ad.irr <- sp_ad[!sp_ad$ID %in% List.ID.null | sp_ad$ID %in% CatchesNoEffort,]
tmp <- data.table(sp_ad.irr)
tmp <- tmp[,
           list(
             Lan_MeanWeight_ad_kg = weighted.mean(x = Lan_MeanWeight_ad_kg, w = Lan_ad_t, na.rm=T),
             Lan_ad_t = sum(Lan_ad_t, na.rm=T),
             Dis_MeanWeight_ad_kg = weighted.mean(x = Dis_MeanWeight_ad_kg, w = Dis_ad_t, na.rm=T),
             Dis_ad_t = sum(Dis_ad_t, na.rm=T)
           ),
           list(year=year, stock=stock, age=age)
           ]
sp_ad.irr <- as.data.frame(tmp)
sp_ad.irr$Lan_CANUM_ad <- sp_ad.irr$Lan_ad_t * 1000 / sp_ad.irr$Lan_MeanWeight_ad_kg
sp_ad.irr$Dis_CANUM_ad <- sp_ad.irr$Dis_ad_t * 1000 / sp_ad.irr$Dis_MeanWeight_ad_kg
head(sp_ad.irr)

aggregate(land ~ stock + year, sp, subset = stock == "COD-NS", FUN = sum, na.rm=T)
aggregate(land ~ stock + year, sp.irr, subset = stock == "COD-NS", FUN = sum, na.rm=T)
aggregate(Lan_ad_t ~ stock + year, sp_ad.irr, subset = stock == "COD-NS", FUN = sum, na.rm=T)


#remove irrelevant metiers
sp2 <- sp[sp$ID %in% List.ID.null & !sp$ID %in% CatchesNoEffort,]
sp_ad2 <- sp_ad[sp_ad$ID %in% List.ID.null & !sp_ad$ID %in% CatchesNoEffort,]

effcap2 <- effcap[effcap$ID %in% List.ID.null & !effcap$ID %in% CatchesNoEffort,]



#EXPORT RAW DATA TO DISK FOR CHECKING - AFTER REMOVAL OF IRRELEVANT METIER

if(export_) {
  export. <- sp2
  export.$country <- substr(export.$ID,1,2)
  export.$fleet <-unlist(lapply(strsplit(export.$ID,split="\\."),function(x) x[1]))
  export.$metier <-unlist(lapply(strsplit(export.$ID,split="\\."),function(x) x[2]))
  export.$area <-unlist(lapply(strsplit(export.$ID,split="\\."),function(x) x[3]))
  #save(export.,file=paste(res.path,"02a_ExportRawLandings_ByCountry_AfterRemovalIrrelevant_",ver,".Rdata",sep=""))
  write.csv(export.,file=file.path("data", ver01,"diagnostics/02a_ExportRawLandings_ByCountry_AfterRemovalIrrelevant.csv"),row.names=FALSE)

  export. <- effcap2
  export.$country <- substr(export.$ID,1,2)
  export.$fleet <-unlist(lapply(strsplit(export.$ID,split="\\."),function(x) x[1]))
  export.$metier <-unlist(lapply(strsplit(export.$ID,split="\\."),function(x) x[2]))
  export.$area <-unlist(lapply(strsplit(export.$ID,split="\\."),function(x) x[3]))
  #save(export.,file=paste(res.path,"02b_ExportRawEffort_ByCountry_AfterRemovalIrrelevant_",ver,".Rdata",sep=""))
  write.csv(export.,file=file.path("data", ver01,"diagnostics/02b_ExportRawEffort_ByCountry_AfterRemovalIrrelevant.csv"),row.names=FALSE)

}


# Create country, fleet, metier, and area names from "ID"
sp2$country <- substr(sp2$ID,1,2)
sp2$fleet <- unlist(lapply(strsplit(sp2$ID,split="\\."),function(x) x[1]))
sp2$metier <- unlist(lapply(strsplit(sp2$ID,split="\\."),function(x) x[2]))
sp2$area <- unlist(lapply(strsplit(sp2$ID,split="\\."),function(x) x[3]))
col_names <- names(sp2)

sp_ad2$country <- substr(sp_ad2$ID,1,2)
sp_ad2$fleet <- unlist(lapply(strsplit(sp_ad2$ID,split="\\."),function(x) x[1]))
sp_ad2$metier <- unlist(lapply(strsplit(sp_ad2$ID,split="\\."),function(x) x[2]))
sp_ad2$area <- unlist(lapply(strsplit(sp_ad2$ID,split="\\."),function(x) x[3]))
col_names_ad <- names(sp_ad2)




# 5.7. Keep "large" metiers ----------------------------------------------------


#...................................
## AGGREGATION
#...................................

# here we keep all the "large" metiers . In each fleet, we have a OTH metiers
sp2$metier2 <- as.character(sp2$metier)
sp2[!(sp2$ID %in% List.ID.oth),"metier2"] <- "OTH"

sp_ad2$metier2 <- as.character(sp_ad2$metier)
sp_ad2[!(sp_ad2$ID %in% List.ID.oth),"metier2"] <- "OTH"

length(unique(subset(sp2, metier2!="OTH")$ID))
length(unique(subset(sp_ad2, metier2!="OTH")$ID))
setdiff(unique(subset(sp2, metier2!="OTH")$ID), unique(subset(sp_ad2, metier2!="OTH")$ID))
#print(unique(sp2$metier))

# add area to non-OTH metiers
#! add area to non-OTH metier names

sp2$metier2[!sp2$metier2=="OTH"] <- paste(sp2$metier2[!sp2$metier2=="OTH"],sp2$area[!sp2$metier2=="OTH"],sep=".")
sp_ad2$metier2[!sp_ad2$metier2=="OTH"] <- paste(sp_ad2$metier2[!sp_ad2$metier2=="OTH"],sp_ad2$area[!sp_ad2$metier2=="OTH"],sep=".")

# aggregate new metier definitions
sp3 <- aggregate(sp2[,c("land","disc","value")],
  by=list(fleet=sp2$fleet, metier=sp2$metier2, stock=sp2$stock, year=sp2$year), sum, na.rm=T)
sp3 <- sp3[ do.call(order, sp3) ,]

tmp <- data.table(sp_ad2)
tmp <- tmp[,
  list(
   Lan_MeanWeight_ad_kg = weighted.mean(x = Lan_MeanWeight_ad_kg, w = Lan_ad_t, na.rm=T),
   Lan_ad_t = sum(Lan_ad_t, na.rm=T),
   Dis_MeanWeight_ad_kg = weighted.mean(x = Dis_MeanWeight_ad_kg, w = Dis_ad_t, na.rm=T),
   Dis_ad_t = sum(Dis_ad_t, na.rm=T)
  ),
  list(fleet=fleet, metier=metier2, stock=stock, age=age, year=year)
]
sp_ad3 <- as.data.frame(tmp)
sp_ad3$Lan_CANUM_ad <- sp_ad3$Lan_ad_t * 1000 / sp_ad3$Lan_MeanWeight_ad_kg
sp_ad3$Dis_CANUM_ad <- sp_ad3$Dis_ad_t * 1000 / sp_ad3$Dis_MeanWeight_ad_kg
sp_ad3 <- sp_ad3[ do.call(order, sp_ad3) ,]
head(sp_ad3)

#!#
aggregate(Lan_ad_t ~ stock+year, sp_ad3, subset = stock=="COD-NS", sum, na.rm=TRUE)
aggregate(Lan_ad_t ~ stock+year, sp_ad2, subset = stock=="COD-NS", sum, na.rm=TRUE)
aggregate(Lan_ad_t ~ stock+year, sp1_ad, subset = stock=="COD-NS", sum, na.rm=TRUE)


aggregate(land ~ stock+year, sp3, subset = stock=="COD-NS", sum, na.rm=TRUE)
aggregate(land ~ stock+year, sp2, subset = stock=="COD-NS", sum, na.rm=TRUE)
aggregate(land ~ stock+year, sp1, subset = stock=="COD-NS", sum, na.rm=TRUE)


# now aggregating the fleets which have only the "OTH" metier
sp4 <- table(sp3[,c("fleet","metier")])
sp4.sum <- apply(sp4,1,sum)
List.ID.fleet <- rownames(sp4)[(sp4.sum-sp4[,"OTH"])>0]   #

sp3$fleet[!(sp3$fleet %in% List.ID.fleet)] <- "OTH_OTH"
sp_ad3$fleet[!(sp_ad3$fleet %in% List.ID.fleet)] <- "OTH_OTH"

#adding the landings from irrelevant metier
#sp3 <- rbind(sp3,cbind(sp.irr,fleet="OTH_OTH",metier="OTH")[names(sp3)])

# aa version
sp3 <- aggregate(sp3[,c("land","disc","value")],
                 by=list(
                   fleet=sp3$fleet, metier=sp3$metier,
                   stock=sp3$stock, year=sp3$year
                 ),
                 sum, na.rm=T
)

# ad version
tmp <- data.table(sp_ad3)
tmp <- tmp[,
           list(
             Lan_MeanWeight_ad_kg = weighted.mean(x = Lan_MeanWeight_ad_kg, w = Lan_ad_t, na.rm=T),
             Lan_ad_t = sum(Lan_ad_t, na.rm=T),
             Dis_MeanWeight_ad_kg = weighted.mean(x = Dis_MeanWeight_ad_kg, w = Dis_ad_t, na.rm=T),
             Dis_ad_t = sum(Dis_ad_t, na.rm=T)
           ),
           list(fleet=fleet, metier=metier, stock=stock, year=year, age=age)
           ]

#! aggregate landings and discards ? This could instead be done in subsequent step
#! with computeLandings() etc. As is, these values are wrong



sp_ad3 <- as.data.frame(tmp)
sp_ad3$Lan_CANUM_ad <- sp_ad3$Lan_ad_t * 1000 / sp_ad3$Lan_MeanWeight_ad_kg
sp_ad3$Dis_CANUM_ad <- sp_ad3$Dis_ad_t * 1000 / sp_ad3$Dis_MeanWeight_ad_kg
head(sp_ad3)

#re-calculate land and disc




sp.non.oth <- sp3[!sp3$metier=="OTH",]
sp.non.oth$ID <-  paste(sp.non.oth$fleet,sp.non.oth$metier,sep=".")
threshold.lev <- rbind(threshold.lev,data.frame(cbind(threshold=min.land, n.unit=length(unique(sp.non.oth$ID)), land=sum(sp.non.oth$land))))

#}   # end of loop on threshold
###


sp3 <- sp3[ do.call(order, sp3) ,]
sp_ad3$age <- as.numeric(sp_ad3$age)
sp_ad3 <- sp_ad3[ do.call(order, sp_ad3) ,]
head(sp_ad3, 20)


sp3$ID <- paste(sp3$fleet,sp3$metier, sep=".")
sp_ad3$ID <- paste(sp_ad3$fleet, sp_ad3$metier, sep=".")

sp4 <- round(tapply(sp3$land, list(sp3$ID, sp3$year,sp3$stock),sum,na.rm=T),digits=1)


#applying the same to effort and capacity
effcap2$fleet <- unlist(lapply(strsplit(effcap2$ID,split="\\."),function(x) x[1]))
effcap2$metier <- unlist(lapply(strsplit(effcap2$ID,split="\\."),function(x) x[2]))
effcap2$area <- unlist(lapply(strsplit(effcap2$ID,split="\\."),function(x) x[3]))
effcap2$metier2 <- as.character(effcap2$metier)
effcap2[!(effcap2$ID %in% List.ID.oth),"metier2"] <- "OTH"
#print(unique(effcap2$metier))

effcap2$metier2[!effcap2$metier2=="OTH"] <- paste(effcap2$metier2[!effcap2$metier2=="OTH"],effcap2$area[!effcap2$metier2=="OTH"],sep=".")


effcap2$fleet[!(effcap2$fleet %in% List.ID.fleet)] <- "OTH_OTH"


effcap3 <- aggregate(effcap2[,c("tot_days","kwdays")], by=list(fleet=effcap2$fleet,
                                                               metier=effcap2$metier2, year=effcap2$year), sum, na.rm=T)
num_vessels <- aggregate(effcap2[,"num_vessels"], by=list(fleet=effcap2$fleet,
                                                          year=effcap2$year), max, na.rm=T)
names(num_vessels)[which(names(num_vessels)=="x")] <- "num_vessels"

#decreasing KW units
effcap3$kwdays <- effcap3$kwdays/1000

effcap3<-merge(effcap3,num_vessels)


effcap3 <- effcap3[ do.call(order, effcap3) ,]

effcap3$ID <- paste(effcap3$fleet,effcap3$metier,sep=".")

ef4 <- round(tapply(effcap3$tot_days, list(effcap3$ID, effcap3$year),sum,na.rm=T),digits=1)

#verif
print("#=== this is the verification that the number of effort ID corresponds to catch ID=== \n")
print(nrow(ef4)==nrow(sp4))
if (!nrow(ef4)==nrow(sp4)) stop("please check!")


print(paste("threshold: ", min.land, " %, number of ID: ",nrow(sp4),sep=""))
print(rownames(sp4))

# test on capacity and effort
eff.<-aggregate(effcap3$tot_days,list(year=effcap3$year,fleet=effcap3$fleet),sum,na.rm=T)
names(eff.)[which(names(eff.)=="x")] <- "tot_days"
kw.<-aggregate(effcap3$kwdays,list(year=effcap3$year,fleet=effcap3$fleet),sum,na.rm=T)
names(kw.)[which(names(kw.)=="x")] <- "kwdays"


cap. <- aggregate(effcap3$num_vessels,list(year=effcap3$year,fleet=effcap3$fleet),max,na.rm=T)
names(cap.)[which(names(cap.)=="x")] <- "num_vessels"
test <- merge(eff., kw.)
test <- merge(test, cap.)
test$eff.ves <- round(test$tot_days/test$num_vessels)

catch <- sp3
catch_ad <- sp_ad3
effcap <- effcap3


test5 <- data.table(sp3)[, list(landAccession=sum(land,na.rm=T), discAccession=sum(disc, na.rm=T)), by=list(year, Group.2=stock)]
test5 <- merge(as.data.frame(test5), test2)
# ggplot(data=test5, aes(x=as.numeric(year),y=landings)) +  geom_line() + facet_grid( . ~ Group.2)+
#   geom_line(data=test4, aes(x=as.numeric(year),y=landAccession),col='red') + facet_grid( . ~ Group.2)

save(effcap, catch, catch_ad, file = file.path("data", ver01, "02_catch_eff.RData"))

write.csv(catch, file = file.path("data", ver01, "diagnostics/02_catch.csv"),row.names=FALSE)
write.csv(catch_ad, file = file.path("data", ver01, "diagnostics/02_catch_ad.csv"),row.names=FALSE)
write.csv(effcap, file = file.path("data", ver01, "diagnostics/02_effort.csv"),row.names=FALSE)





# X.X STOP HERE? THIS LOOKS OLD... --------------------------------------------------------------

if(0){

#...................................
#### merge age distribution with catch table
#...................................

# need to aggregate sp1_ad2 by = list(year=sp1$year, ID=sp1$ID, stock=sp1$stock),sum, na.rm=T)
#tmp1 <- aggregate(
#  sp1_ad2[,c("land","disc","value","landingsAtAge","discardsAtAge", "Lan_CANUM", "Dis_CANUM")],
#  by = list(
#    year = sp1_ad2$year,
#    ID = sp1_ad2$ID,
#    stock = sp1_ad2$stock,
#    age = sp1_ad2$age
#  ),
#  sum, na.rm=T
#)

# # recalculate WECA
#tmp1$Lan_WECA_in_kg <- tmp1$landingsAtAge / tmp1$Lan_CANUM
#tmp1$Dis_WECA_in_kg <- tmp1$discardsAtAge / tmp1$Dis_CANUM


# visual check
#tmpp <- subset(tmp1, ID %in% c("BE_Beam>=24.BT1.4", "BE_Beam>=24.BT2.4") & year == 2016 & stock == "COD-NS")
#tmpp

#tmpp <- subset(tmp1,fleet=="BE_Beam>=24" & metier == "BT1.4" & stock == "COD-NS" & year == 2016)
#tmpp
#sum(tmpp$landingsAtAge)
#tmpp$land


# get fleet and metier from ID
#tmp1 <- sp1_ad

#tmp1$fleet<-matrix(unlist(strsplit(tmp1$ID,"[.]")),byrow=T,ncol=3)[,1]
#x<-matrix(unlist(strsplit(tmp1$ID,"[.]")),byrow=T,ncol=3)[,2:3]
#tmp1$metier<-apply(x,1,paste,collapse=".")

# merge the two tables
# tmp2<-merge(catch,tmp1,all.x=T) # PROBLEM
#tmp2 <- merge(catch,tmp1, by=c("fleet","metier","stock","year"),all.x=T, all.y=T)


# recalculate landingsAtAge and discardsAtAge based on ratio .x / .y
#tmp2$landingsAtAge <- tmp2$landingsAtAge * (tmp2$land.x / tmp2$land.y)
#tmp2$discardsAtAge <- tmp2$discardsAtAge * (tmp2$disc.x / tmp2$disc.y)

# rename land.x, disc.x & value.x
#names(tmp2) <- gsub( pattern = ".x", replacement = "", x = names(tmp2) )

# remove land.y, disc.y, % value.y
#tmp2 <- tmp2[, -grep(x = names(tmp2), pattern = ".y")]

# recalculate CANUM
#tmp2$Lan_CANUM <- tmp2$landingsAtAge / tmp2$Lan_WECA_in_kg
#tmp2$Dis_CANUM <- tmp2$discardsAtAge / tmp2$Dis_WECA_in_kg

# visual test
#tmpp <- subset(tmp2,fleet=="BE_Beam>=24" & metier == "BT1.4" & stock == "COD-NS" & year == 2016)
#tmpp
#sum(tmpp$landingsAtAge)
#tmpp$land.y
#tmpp$land


#tmp2$
#tmp1$Lan_WECA_in_kg <- tmp1$landingsAtAge / tmp1$Lan_CANUM
#tmp1$Dis_WECA_in_kg <- tmp1$discardsAtAge / tmp1$Dis_CANUM



#catch_ad<-tmp2

# visual check
#tmpp <- subset(catch_ad, fleet=="BE_Beam>=24" & year == 2016 & stock == "COD-NS")
#tmpp
irr<-sp[!sp$ID %in% List.ID.null | sp$ID %in% CatchesNoEffort,]
# catch_ad <- sp1_ad[-which(sp1_ad$ID %in% irr$ID),]
catch_ad <- sp_ad[-which(sp_ad$ID %in% irr$ID),]



catch_ad[is.na(catch_ad$age) & catch_ad$stock%in%StocksWithAgeDistributionMIXFISH & catch_ad$year==2016,]


tmp <- matrix(unlist(strsplit(catch_ad$ID, split = ".", fixed = TRUE)),3)
catch_ad$fleet <- tmp[1,]
catch_ad$metier <- apply(tmp[2:3,],2,paste, collapse=".")
tmp <- substr(catch_ad$ID, start = 1, stop = 2)
catch_ad$country <- tmp
head(catch_ad)


tmp <- substr(catch$ID, start = 1, stop = 2)
catch$country <- tmp


unique(catch$metier)
unique(catch_ad$metier)
unique(catch_ad$country)
unique(catch_ad$stock)

catch_ad[which(catch_ad$country=="BE" & catch_ad$stock=="COD-NS" & catch_ad$year==2016 & is.na(catch_ad$age)),]

catch[which(catch$country=="BE" & catch$stock=="COD-NS" & catch$year==2016),]
subset(catch, country=="BE" & stock=="COD-NS" & year==2016)
subset(catch_ad, country=="BE" & stock=="COD-NS" & year==2016)

IC_ad[with(IC_ad, which(country=="BE" & stock=="COD-NS" & year==2016)),]
sum(unique(IC_ad[with(IC_ad, which(country=="BE" & stock=="COD-NS" & year==2016)),]$WLandingsTot))




save(effcap, catch, catch_ad, file=paste(res.path,"/02_catch_eff_",ver,".RData",sep=""))
write.csv(catch_ad,file=paste(res.path,"/diagnostics/02_catch_ageDist_",ver,".csv",sep=""),row.names=FALSE)


# some checks for catch_ad

# are irrelevant fleets removed?
irr<-sp[!sp$ID %in% List.ID.null | sp$ID %in% CatchesNoEffort,]
catch_ad[catch_ad$ID %in% irr$ID,] # should be empty

## check that aggregation of sp1_ad2 and merging happened correctly
# choose specific fleets/stocks/years
id<-"SC_Otter<24.TR1.4" # pick and ID to check
stk<-"HAD" # pick a stock to check
yr<-datayear # pick a year to check

tmp1<-catch_ad[grepl(id,catch_ad$ID) & catch_ad$stock %in% stk & catch_ad$year == yr ,]
tmp2<-sp1_ad2[grepl(id,sp1_ad2$ID) & sp1_ad2$stock %in% stk & sp1_ad2$year == yr ,]

# land value in tmp1 shoud equal sum of unique land values in tmp2
tmp1$land[1]==sum(unique(tmp2$land)) # should be TRUE
# disc value in tmp1 shoud equal sum of unique disc values in tmp2
tmp1$disc[1]==sum(unique(tmp2$disc)) # should be TRUE
# land at age value in tmp1 shoud equal sum of land at age values in tmp2
sum(tmp1$landingsAtAge)==sum((tmp2$landingsAtAge)) # should be TRUE
# disc at age value in tmp1 shoud equal sum of disc at age values in tmp2
sum(tmp1$discardsAtAge)==sum((tmp2$discardsAtAge)) # should be TRUE

#SOP check - sum of landings at age should equal land
tmp1<-tapply(catch_ad$land,list(catch_ad$ID,catch_ad$stock,catch_ad$year),function(x){sum(unique(x),na.rm=T)})
tmp2<-tapply(catch_ad$landingsAtAge,list(catch_ad$ID,catch_ad$stock,catch_ad$year),sum,na.rm=T)

# find ratio
tmp1[id,stk,as.character(yr)]/tmp2[id,stk,as.character(yr)] # should be ~1


#...................................
#### check contribution from OTH_OTH fleet
#...................................
subset(aggregate(catch[,c("land","disc")],list(stock=catch$stock,year=catch$year),sum,na.rm=T),year==datayear)

#check that OTH is small compared to landings
windows(height=10,width=12)
tot.land<-tapply(catch$land,catch$stock,sum)
oth.land<-tapply(catch$land[catch$fleet %in% "OTH_OTH"],catch$stock[catch$fleet %in% "OTH_OTH"],sum)
layout(rbind(1,2))
barplot(100*(oth.land/tot.land),las=2,cex.names =0.7,main="OTH_OTH landings as percentage of total landings",ylab="%",cex.main=0.9,cex.axis=0.8,cex=0.8)
title(main="All years",line=3)
barplot(rbind(tot.land-oth.land,oth.land),las=2,cex.names =0.7,main="Total landings (all years)",ylab="Landings (tonnes)",cex.axis=0.7,cex=0.8,cex.main=0.9,col=c("grey30","grey90"))
legend(x=1,y=600000,legend=c("light grey = OTH_OTH landings","dark grey = all other fleet landings"),fill=c("grey90","grey30"),cex=0.6)
savePlot(filename=paste0(res.path,"/make_fleet_agg_check_OTH_OTH_contribution_all_years_",ver),type="png")

# 2016
windows(height=10,width=12)
tot.land<-tapply(catch$land[catch$year ==2017],catch$stock[catch$year ==2017],sum)
oth.land<-tapply(catch$land[catch$fleet %in% "OTH_OTH" & catch$year ==2017],catch$stock[catch$fleet %in% "OTH_OTH" & catch$year ==2016],sum)
names(tot.land)[!names(tot.land) %in% names(oth.land)]
oth.land<-c(oth.land,c("NEP10"=NA,"NEP34"=NA))
oth.land<-oth.land[names(tot.land)]
layout(rbind(1,2))
barplot(100*(oth.land/tot.land),las=2,cex.names =0.7,main="OTH_OTH landings as percentage of total landings",ylab="%",cex.main=0.9,cex.axis=0.8,cex=0.8)
title(main="All years",line=3)
barplot(rbind(tot.land-oth.land,oth.land),las=2,cex.names =0.7,main="Total landings (all years)",ylab="Landings (tonnes)",cex.axis=0.7,cex=0.8,cex.main=0.9,col=c("grey30","grey90"))
legend(x=1,y=600000,legend=c("light grey = OTH_OTH landings","dark grey = all other fleet landings"),fill=c("grey90","grey30"),cex=0.6)


# just to check the numbers - do they make sense
tapply(catch$land,list(catch$year,catch$stock),sum)
tapply(sp1$land,list(sp1$year,sp1$stock),sum)

(tapply(sp1$land,list(sp1$country,sp1$year,sp1$stock),sum))[,,"HAD"]

colSums((tapply(sp1$land,list(sp1$country,sp1$year,sp1$stock),sum))[,,"HAD"])

#...................................
#### compare sp1 catch to IC catch totals
#...................................

#make comparison table to show degree of match with discards by country, stock, metier, fleet
tom<-aggregate(sp1[,c("land","disc")],by=list("country"=sp1$country,"year"=sp1$year,"stock"=sp1$stock),sum,na.rm=T)
har<-aggregate(IC_[,c("landings","discards")],by=list("country"=IC_$country,"year"=IC_$year,"stock"=IC_$stock),sum,na.rm=T)
names(har)[c(4,5)]<-c("IClandings","ICdiscards")
th<-merge(tom,har,all.x = T)
th$landpctdiff<-th$land/th$IClandings
th$discpctdiff<-th$disc/th$ICdiscards

write.csv(th,file=paste0(res.path,"/Comparison of landings and discards by country, year and stock_",ver,".csv"),row.names=F)

#...................................
#### compare sp catch at age to IC catch at age #
#...................................

#make comparison table to show degree of match with catch at age by country, stock, metier, fleet
tom<-aggregate(sp1_ad2[,c("landingsAtAge","discardsAtAge")],by=list("country"=sp1_ad2$country,"year"=sp1_ad2$year,"stock"=sp1_ad2$stock),sum,na.rm=T)
har<-aggregate(IC_ad[,c("Lan_Weight_at_CANUM_in_kg","Dis_Weight_at_CANUM_in_kg")]/1000,by=list("country"=IC_ad$country,"year"=IC_ad$year,"stock"=IC_ad$stock),sum,na.rm=T)
names(har)[c(4,5)]<-c("IClandAtAge","ICdiscAtAge")
th_ad<-merge(tom,har,all.x = T)
th_ad$landpctdiff<-th_ad$landingsAtAge/th_ad$IClandAtAge
th_ad$discpctdiff<-th_ad$discardsAtAge/th_ad$ICdiscAtAge

write.csv(th_ad,file=paste0(res.path,"/Comparison of landings and discards at age by country, year and stock_",ver,".csv"),row.names=F)

}
