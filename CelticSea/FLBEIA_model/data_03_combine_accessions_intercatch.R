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
library(icesTAF)
library(readxl)
library(icesSAG)
# Purpose -----------------------------------------------------------------
# This script is to process avalible effort, catch and logbook data from
# disparrate sources into something useable and format the data into 
# fleet object. Considered this section similar to DCF processing for 
# ICES submission 


# Datapaths ----------------------------------------------------------------
Data_path <- "results"
LookupPath <- "bootstrap"
BootstrapPath <- "bootstrap"
# #1.0 Set parameters -----------------------------------------------------
Yearwg<-2020
# MR change to last three Year
YEARS <-2009:2019
PERCENTAGE <- "0"
FLEET_PERCENTAGE <- "0"

# Making lists

# ~ 2020 shortlist --------------------------------------------------------
sp.lst <- c("cod.27.7e-k", "had.27.7b-k", "whg.27.7b-ce-k", "sol.27.7fg", 
             "mon.27.78abd", "meg.27.7b-k8abd", "nep.fu.16","nep.fu.17","nep.fu.19",
             "nep.fu.2021" ,"nep.fu.22", "nep.out.7")


# ~Print species selection -------------------------------------------------
print(sp.lst)


# 2.0 Read in data ------------------------------------------------------------
#actual data
InterCatch <- read.csv(file.path(Data_path,"clean_data/caton_summary.csv"))
InterCatch_age <- read.csv(file.path(Data_path,"clean_data/canum_summary.csv"))
## Data that has been matched 
### data has also been filterede to Yearwg-3 and summarise so yes it is smaller 
## this is all done at the end of data_01
#catch_start <-read.csv(file.path(Data_path,"clean_data/Matched_clean_accessions_landings.csv"))
#effort_start <-read.csv(file.path(Data_path,"clean_data/Matched_clean_accessions_landings.csv"))
##Can also read in the data that could not be matched but need to decide wht to do with it


# # all data including unmatched
catch_start <-read.csv(file.path(Data_path,"clean_data/Unmatched_clean_accessions_landings.csv"))
effort_start <-read.csv(file.path(Data_path,"clean_data/Unmatched_clean_accessions_effort.csv"))
#stock data
Stock_lookup <- read.csv(file.path(LookupPath,"data/supporting_files/Stock_lookup.csv"))
nep_data <- read.csv(file.path(BootstrapPath,"data/submitted_stock_objects/WGCSE/nep.all/nep.stock.wgmixfish_2020.csv"))


# Moniter for differences -------------------------------------------------
## so we get the IC file and the AC file and comapre for differances between the two
names(InterCatch)
check_IC <- InterCatch %>% select(Year,Stock,Country,Landings,Discards) %>% group_by(Year,Stock,Country) %>% summarise(Landing_IC=sum(Landings),Discard_IC=sum(Discards,na.rm = T),IC_Discards=sum(Discards,na.rm = T)) %>% ungroup()
names(catch_start)
check_AC <- catch_start %>% select(Year,Stock,Country,Landings) %>% group_by(Year,Stock,Country)%>% summarise(Landing_AC=sum(Landings)) %>% ungroup()

CHECK_IC_AC <- full_join(check_AC,check_IC)

#No stock no play
CHECK_IC_AC <- CHECK_IC_AC %>% filter(is.na(Stock)==F)

#now we need to get the stock objects which represemt what was actually in the single
#species advice
wg.stock <- FLStocks(lapply(list.files("results/clean_data/clean_stock_objects/"), function(x) {
  load(paste("results/clean_data/clean_stock_objects/","/",x,sep=""))
  res<-get("stock")
  name(res) <- x
  res
}))

##make a data frame
res.out <- as.data.frame(landings(
  wg.stock$`cod.27.7e-k.RData`
))
res.out <- res.out %>% mutate(stock="cod.27.7e-k")
for(i in 2:length(wg.stock)){
  
  stock <- wg.stock[[i]]
  res <- as.data.frame(landings(stock))
  res <- res %>% mutate(stock=gsub(".RData","",names(wg.stock)[i]))
  res.out <- rbind(res.out,res)
  
}

res.out <- res.out %>% select(stock,year,data)
names(res.out) <- c("Stock","Year","SObj_Landings")

#### discards
##make a data frame
res.out_disc <- as.data.frame(discards(
  wg.stock$`cod.27.7e-k.RData`
))
res.out_disc <- res.out_disc %>% mutate(stock="cod.27.7e-k")
for(i in 2:length(wg.stock)){
  
  stock <- wg.stock[[i]]
  res_disc <- as.data.frame(discards(stock))
  res_disc <- res_disc %>% mutate(stock=gsub(".RData","",names(wg.stock)[i]))
  res.out_disc <- rbind(res.out_disc,res_disc)
  
}

res.out_disc <- res.out_disc %>% select(stock,year,data)
names(res.out_disc) <- c("Stock","Year","SObj_Discards")

####add discard data to landings

res.out <- left_join(res.out,res.out_disc)

### we need to remove 8abd from the WGBIE stocks
Catch_by_country <- read.csv(file.path(BootstrapPath, "data", "ices_intercatch", "WGBIE_catch_by_country.csv")) 
Catch_by_country$CatchCategory[Catch_by_country$CatchCategory %in% c("discards","Logbook Registered Discard")] <- "Discards"
Catch_by_country <- Catch_by_country %>% select(-Country)%>%  filter(Year %in% YEARS,stock %in% c("HKE","MEG","MON"),CatchCategory %in% c("Landings","landings","Discards"), Area %in% c("8","8abd","27.8")) %>% select(-"Area") %>% group_by_at(vars(-tons))%>% summarise(tons=sum(tons)) %>% ungroup() %>% pivot_wider(names_from  = CatchCategory,values_from = tons)


names(res.out)
names(Catch_by_country)
names(Catch_by_country)[names(Catch_by_country)=="stock"] <- "Stock"
Catch_by_country$Stock[Catch_by_country$Stock=="HKE"] <- "hke.27.3a46-8abd"
Catch_by_country$Stock[Catch_by_country$Stock=="MEG"] <- "meg.27.7b-k8abd"
Catch_by_country$Stock[Catch_by_country$Stock=="MON"] <- "mon.27.78abd"

res.out <- left_join(res.out,Catch_by_country)

res.out <- res.out %>% group_by_at(vars(-SObj_Landings,-SObj_Discards,-Landings,-Discards)) %>% mutate(SObj_Landings=SObj_Landings-ifelse(is.na(Landings)==F,Landings,0),SObj_Discards=SObj_Discards-ifelse(is.na(Discards)==F,Discards,0)) %>% ungroup() %>% select(-Landings,-Discards)


##Now join everything together
CHECK_IC_AC_OBJ <- full_join(CHECK_IC_AC,res.out)
#sum and check
CHECK_IC_AC_OBJ_SUM <- CHECK_IC_AC_OBJ %>% group_by(Year,Stock) %>% summarise(Landing_AC=sum(Landing_AC,na.rm = T),Landing_IC=sum(Landing_IC,na.rm = T),Discard_IC=sum(Discard_IC,na.rm = T),SObj_Landings=unique(SObj_Landings),SObj_Discards=unique(SObj_Discards)) %>% ungroup()

#### now add advice 
Advice <- getSAG(stock = "hke.27.3a46-8abd",year = Yearwg)
Advice <-Advice %>%  select(Year,landings,discards,fishstock) %>% filter(Year %in% YEARS)

Stocks_list <- c(unique(check_IC$Stock))[unique(check_IC$Stock) != "hke.27.3a46-8abd"]

for(i in Stocks_list){
  Advice_out <- getSAG(stock = i,year = Yearwg)
  Advice_out <-Advice_out %>%  select(Year,landings,discards,fishstock) %>% filter(Year %in% YEARS)
  Advice<- rbind(Advice,Advice_out)
}

names(Advice)[names(Advice)=="discards"] <- "Advice_Discards"
names(Advice)[names(Advice)=="landings"] <- "Advice_Landings"
names(Advice)[names(Advice)=="fishstock"] <- "Stock"

CHECK_IC_AC_OBJ_SUM <- left_join(CHECK_IC_AC_OBJ_SUM,Advice)


#just the last 3 year
CHECK_IC_AC_OBJ_SUM <- CHECK_IC_AC_OBJ_SUM %>% filter(Year %in% YEARS) %>% 
  mutate(diff_AC=Landing_AC-SObj_Landings,diff_IC=Landing_IC-SObj_Landings,diff_Advice=Advice_Landings-SObj_Landings, diff_Advice_Disc=Advice_Discards-SObj_Discards )


# CHECK_IC_AC_OBJ_SUM <- CHECK_IC_AC_OBJ_SUM %>% group_by(Year,Stock) %>% mutate(AC_PROB=ifelse(diff_AC > ((
#   SObj_Landings/100)*10)|diff_AC < -((SObj_Landings/100)*10),"YES","NO"),IC_PROB=ifelse(diff_IC > ((SObj_Landings/100)*10)|diff_IC < -((SObj_Landings/100)*10),"YES","NO"),
#   ADVICE_PROB=ifelse(diff_Advice > ((SObj_Landings/100)*10)|diff_Advice < -((SObj_Landings/100)*10),"YES","NO"),
#   ADVICE_PROB_DISC=ifelse(diff_Advice_Disc > ((SObj_Discards/100)*10)|diff_Advice_Disc < -((SObj_Discards/100)*10),"YES","NO")) %>% ungroup()

CHECK_IC_AC_OBJ_SUM <- CHECK_IC_AC_OBJ_SUM %>% group_by(Year,Stock) %>% mutate(AC_PROB= round(Landing_AC/SObj_Landings,2),
                                                                               IC_PROB= round(Landing_IC/SObj_Landings,2),
                                                                               IC_PROB_DISC= round(Discard_IC/SObj_Discards,2),
                                                                               ADVICE_PROB=round(Advice_Landings/SObj_Landings,2),
                                                                               ADVICE_PROB_DISC=round(Advice_Discards/SObj_Discards,2)
                                                                                 ) %>% ungroup()


# 
# #now do some graphs, this is just counts of problems
# Graph_data <- CHECK_IC_AC_OBJ_SUM %>% select(Year,Stock,AC_PROB,IC_PROB,ADVICE_PROB) %>% pivot_longer(cols =c(AC_PROB,IC_PROB,ADVICE_PROB),names_to ="Prob")
# 
# ggplot(Graph_data,aes(x=value,group=Prob,fill=Prob))+geom_bar(position="dodge")+facet_wrap(~Stock)+theme_classic()
#   ggsave(file.path(Data_path,"intermediate_products","03_crosschecking_counts.png"))
### this is the actuall value we are out by
Graph_data2 <- CHECK_IC_AC_OBJ_SUM %>% select(Year,Stock,diff_AC,diff_IC,diff_Advice,diff_Advice_Disc) %>% pivot_longer(cols =c(diff_AC,diff_IC,diff_Advice,diff_Advice_Disc),names_to ="Diff")

ggplot(Graph_data2,aes(y=value,x=Year,group=Diff,fill=Diff))+geom_col(position="dodge")+facet_wrap(~Stock)+theme_classic()+   scale_y_continuous(breaks = seq(-5000,5000,500),limits =c(-5000,5000))+ggtitle("Differance from Stock Object")
  ggsave(file.path(Data_path,"\\intermediate_products\\03_crosschecking_landings.png"))

write.csv(CHECK_IC_AC_OBJ_SUM,file = file.path(Data_path,"\\intermediate_products\\03_crosschecking.csv"))

#  3.0 script starts -----------------------------------------------------------
#allows us to keep the orignal object in GE for specific checks 
catch <- catch_start
effort<- effort_start

dim(catch_start)
dim(catch_start)
catch<- catch[catch$Area %in% c("27.7"  ,   "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]
effort<- effort[effort$Area %in% c("27.7"  , "27.7.b" , "27.7.c" , "27.7.d",  "27.7.e",  "27.7.f" , "27.7.g" , "27.7.h",  "27.7.j" , "27.7.k" ),]
dim(catch)
dim(catch)

# 4.0 apply corrections  ------------------------------------------------
#Clean Country Names
# select the last three Year
# effort<- subset(effort,Year %in% c(Yearwg-3,Yearwg-2,Yearwg-1))
# catch<- subset(catch,Year %in% c(Yearwg-3,Yearwg-2,Yearwg-1))

# 5.0 Combine catch and effort ------------------------------------------------
catch2 <- catch
effort2 <- effort

catch2 <- catch2 %>% group_by_at(vars(-Landings,-Value)) %>% summarise(Landings=sum(Landings),Value=sum(Value)) %>% ungroup()
effort2 <- effort2 %>% group_by_at(vars(-kw_days,-Days_at_sea,-No_vessels)) %>% summarise(kw_days=sum(kw_days),Days_at_sea=sum(Days_at_sea),No_vessels=sum(No_vessels)) %>% ungroup()

catch2 <- catch2 %>% mutate(check=1:nrow(catch2))

# ~Processing catch data ---------------------------------------------------
Catch3 <- catch2
effort3 <- effort2

# ~adjust species names for intercatch ------------------------------------------------
Catch3 <-filter(Catch3,Stock %in% sp.lst)

InterCatch <- InterCatch[InterCatch$Stock %in% tolower(sp.lst),]
InterCatch$Species <- toupper(substr(InterCatch$Stock, 1, 3))
InterCatch$Species <- ifelse(InterCatch$Species =="NEP", toupper(InterCatch$Stock),InterCatch$Species)


# ~Prep the NEP data -----------------------------------------------------
#includes the fixes fro the nep names
nep_data <- nep_data %>% filter(!year %in% c(2020)) %>% select(fu,year,discard.rate.wgt)
names(nep_data)[names(nep_data) =="fu"] <-"Stock" 
names(nep_data)[names(nep_data) =="year"] <-"Year"
names(nep_data)[names(nep_data) =="discard.rate.wgt"] <-"DR"

nep_data$Stock <- paste("nep.",nep_data$Stock,sep="")
nep_data$Stock[nep_data$Stock=="nep.out7.fu"] <- "nep.out.7"

# 6.0Create discard ID in catch and effort ---------------------------------
###all
###Remove area
###Remove metier keep area
### Just year country and species!!:


# ~Intercatch discard id --------------------------------------------------
InterCatch$Discard_ID <- paste( InterCatch$Year, InterCatch$Country, InterCatch$Species, InterCatch$Area, InterCatch$lvl4, sep = "_")
InterCatch$Discard_ID_NO_AREA <- paste( InterCatch$Year, InterCatch$Country, InterCatch$Species,  InterCatch$lvl4, sep = "_")
InterCatch$Discard_ID_NO_METIER <- paste( InterCatch$Year, InterCatch$Country, InterCatch$Species,  InterCatch$Area, sep = "_")
InterCatch$Discard_ID_NO_COUNTRY <- paste( InterCatch$Year, InterCatch$Species, InterCatch$Area, InterCatch$lvl4, sep = "_")
InterCatch$Discard_ID_NO_COUNTRY_AREA <- paste( InterCatch$Year, InterCatch$Species,  InterCatch$lvl4, sep = "_")
# ~ Catch ID for discard_id's ---------------------------------------------
Catch3$Discard_ID <- paste(Catch3$Year, Catch3$Country, Catch3$Species, Catch3$Area, Catch3$Metier, sep = "_")
Catch3$Discard_ID_NO_AREA <- paste(Catch3$Year, Catch3$Country, Catch3$Species, Catch3$Metier, sep = "_")
Catch3$Discard_ID_NO_METIER <- paste(Catch3$Year, Catch3$Country, Catch3$Species,  Catch3$Area, sep = "_")
Catch3$Discard_ID_NO_COUNTRY <- paste(Catch3$Year,  Catch3$Species, Catch3$Area, Catch3$Metier, sep = "_")
Catch3$Discard_ID_NO_COUNTRY_AREA <- paste( Catch3$Year, Catch3$Species,  Catch3$Metier, sep = "_")


# ~ Create DR df to join to catch based on differen aggregations  ---------
#select discard data
discard_dat <- InterCatch %>% select(Discard_ID, DR,Landings)
names(discard_dat)[names(discard_dat)=="Landings"] <- "IC_Landings"
#one for area
discard_dat_NO_AREA <- InterCatch %>% select(Discard_ID_NO_AREA, DR,Landings)
names(discard_dat_NO_AREA)[names(discard_dat_NO_AREA)=="Landings"] <- "IC_Landings"
###one for metier
discard_dat_NO_METIER <- InterCatch %>% select(Discard_ID_NO_METIER, DR,Landings)
names(discard_dat_NO_METIER)[names(discard_dat_NO_METIER)=="Landings"] <- "IC_Landings"
### for no country
Discard_ID_NO_COUNTRY <- InterCatch %>% select(Discard_ID_NO_COUNTRY, DR,Landings)
names(Discard_ID_NO_COUNTRY)[names(Discard_ID_NO_COUNTRY)=="Landings"] <- "IC_Landings"
### so this line is takeing the maximum discard rate do we need this to be weighed?
discard_dat<- discard_dat %>% group_by(Discard_ID) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
#no araa
discard_dat_NO_AREA<- discard_dat_NO_AREA %>% group_by(Discard_ID_NO_AREA) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
##noMetier
#no araa
discard_dat_NO_METIER<- discard_dat_NO_METIER %>% group_by(Discard_ID_NO_METIER) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )
##no country but with everything elese
Discard_ID_NO_COUNTRY<- Discard_ID_NO_COUNTRY %>% group_by(Discard_ID_NO_COUNTRY) %>% dplyr::summarise( DR = max(DR,na.rm = T),IC_Landings=sum(IC_Landings,na.rm = T) )

# 7.0 Join discard and catch --------------------------------------------------
sum(Catch3$Landings)
table(grepl("NEP",Catch3$Species)==T)
Catch3_NEP <- Catch3 %>% filter(grepl("NEP",Catch3$Species)==T)
Catch3_NO_NEP <- Catch3 %>% filter(grepl("NEP",Catch3$Species)==F)

dim(Catch3_NO_NEP)
Catch4_NO_NEP <- left_join(Catch3_NO_NEP, discard_dat, by = "Discard_ID") # anything that is here as NA is unknown
dim(Catch4_NO_NEP)

# ~Nep section -------------------------------------------------------------
dim(Catch3_NEP)
Catch4_NEP <- left_join(Catch3_NEP, nep_data) # anything that is here as NA is unknown
dim(Catch4_NEP)

# ~bind data back together ------------------------------------------------
setdiff(names(Catch4_NO_NEP),names(Catch4_NEP))
Catch4_NEP <- Catch4_NEP %>% mutate(IC_Landings=NA)

Catch4 <- rbind(Catch4_NO_NEP,Catch4_NEP)

sum(Catch4$Landings[is.na(Catch4$DR)==F])
sum(unique(Catch4$IC_Landings),na.rm = T)/1000
table(is.na(Catch4$DR))

Catch4_DR <- Catch4 %>% filter(is.na(DR)==F)
Catch4_DR_NA <-Catch4 %>% filter(is.na(DR)==T) 

# ~DR removing area -------------------------------------------
Catch4_DR_NA <-Catch4_DR_NA %>%  select(-IC_Landings,-DR)

dim(Catch4_DR_NA)
Catch5<- left_join(Catch4_DR_NA,discard_dat_NO_AREA)
dim(Catch5)[1]-(dim(Catch4_DR_NA)[1])
##check na
table(is.na(Catch5$DR))

Catch5_DR <- Catch5 %>% filter(is.na(DR)==F)
Catch5_DR_NA <-Catch5 %>% filter(is.na(DR)==T) 
# ~DR removeing gear but keeping area -------------------------------------------
Catch5_DR_NA <-Catch5_DR_NA %>%  select(-IC_Landings,-DR)

dim(Catch5_DR_NA)
Catch6<- left_join(Catch5_DR_NA,discard_dat_NO_METIER)
dim(Catch6)[1]-(dim(Catch5_DR_NA)[1])
##check na
table(is.na(Catch6$DR))


Catch6_DR <- Catch6 %>% filter(is.na(DR)==F)
Catch6_DR_NA <-Catch6 %>% filter(is.na(DR)==T) 
# ~Remove country  --------------------------------------------------------
Catch6_DR_NA <-Catch6_DR_NA %>%  select(-IC_Landings,-DR)

dim(Catch5_DR_NA)
Catch7<- left_join(Catch6_DR_NA,Discard_ID_NO_COUNTRY)
dim(Catch7)[1]-(dim(Catch6_DR_NA)[1])

Catch7_DR <- Catch7 %>% filter(is.na(DR)==F)
Catch7_DR_NA <-Catch7 %>% filter(is.na(DR)==T) 
# ~Put it all back together ------------------------------------------------
## So this is some code to take the mean DR rate based off the matches we do have 
names(Catch4_DR)
names(Catch5_DR)
names(Catch6_DR)
names(Catch7_DR)
names(Catch7_DR_NA)

# Catch6_DR_NA <- Catch6_DR_NA mutate()

setdiff(names(Catch4_DR),names(Catch5_DR))
setdiff(names(Catch4_DR),names(Catch6_DR))

Catch4_DR <- select(Catch4_DR,-IC_Landings)
Catch5_DR <- select(Catch5_DR,-IC_Landings)
Catch6_DR <- select(Catch6_DR,-IC_Landings)
Catch7_DR <- select(Catch7_DR,-IC_Landings)

Catch7 <- rbind(Catch4_DR,Catch5_DR,Catch6_DR,Catch7_DR)

sum(Catch3$Landings)-(sum(Catch7$Landings)+sum(Catch7_DR_NA$Landings))

 Catch7_DR_NA$DR <- 0  ##'*So this is a major assumption and needs to be checked*
 
 Catch7_DR_NA <- select(Catch7_DR_NA,-IC_Landings)
 Catch8 <- rbind(Catch7,Catch7_DR_NA)
 
# ~Set Catch7 back to Catch3 and remove 4-7 --------------------------------
Catch3 <- Catch8
rm(Catch4,Catch4_DR,Catch4_DR_NA,Catch5,Catch5_DR,Catch5_DR_NA,Catch6,Catch6_DR,Catch6_DR_NA,Catch7,Catch7_DR_NA)
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

# # Join stock ID to Catch3 --------------------------------------------------
# dim(Catch3)
# Catch3 <- left_join(Catch3,Stock_lookup, by = c("Area", "Species"))
# dim(Catch3)

# ~subset out lines without stock -----------------------------------------
Catch3_NA_Stocks <- Catch3[is.na(Catch3$Stock)==T,] 
Catch3<- Catch3[is.na(Catch3$Stock)==F,]
# check residuals
table(Catch3_NA_Stocks$Species,Catch3_NA_Stocks$Area)
#check what we keep
table(Catch3$Species,Catch3$Area)
table(Catch3$Species,Catch3$Stock)

# 8.0 Catch per country -------------------------------------------------------------
summa <- Catch3 %>% select(Landings,Country,Year,Stock) %>% group_by_at(vars(-Landings)) %>% summarise(Landings=sum(Landings,na.rm = T)) %>% ungroup()

summa<-summa[,c(3,2,1,4)]
summafleet<-aggregate(list(Landings=Catch3$Landings),by=list(Discard_ID=Catch3$Discard_ID, Year=Catch3$Year,   Stock=Catch3$Stock),sum)
write.csv(summa,file=file.path(Data_path,paste0("/intermediate_products/catch_per_country", ".csv")))

# ~Calculate discards ------------------------------------------------------
#hashed out old code
## Question is this not simply landigns*discard rate?
# Catch3<-mutate(Catch3,Discards=(Landings/(1-DR)-Landings)) %>%  select(Country,Year,Quarter,Metier,Vessel_length,Area,Species,Stock, DR,Landings,Discards,Value)
Catch3<-mutate(Catch3,Discards=(Landings*DR)) %>%  select(Country,Year,Metier,Vessel_length,Area,Species,Stock, DR,Landings,Discards,Value)

# 9.0 Assigning a fleet ------------------------------------------------------

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


# ~Creates country specific fleet -----------------------------------------
Catch3$Fleet<-paste(Catch3$Country,Catch3$GeneralGrouping,Catch3$Vessel_length,sep="_") 
Catch3$Metier <- paste(Catch3$Metier, Catch3$Area, sep= "_") 

effort2$Fleet<-paste(effort2$Country,effort2$GeneralGrouping,effort2$Vessel_length,sep="_") 
effort2$Metier <- paste(effort2$Metier, effort2$Area, sep= "_") 

# Now aggregate to make sure
Catch4<- Catch3 %>% group_by(Fleet,Metier, Vessel_length, Year,Area,Species,Stock) %>%  
  summarise("Landings"=sum(Landings,na.rm=T),
            "Discards"=sum(Discards,na.rm=T),
            "Value" = sum(Value, na.rm=T)) %>% ungroup()

# Now aggregate to make sure
effort3 <- effort2 %>% group_by(Fleet,Metier, Vessel_length, Year,Area) %>%dplyr::summarise("kw_days"=sum(kw_days,na.rm=T),"Days_at_sea"=sum(Days_at_sea,na.rm=T)) %>% ungroup()

# 10.0 FIN df created and setting of fleet and metier aggregations  -------------------------------------
Effort_FIN <- effort3

Catch_FIN <- Catch4


# 11.0 Final steps -------------------------------------------------------------
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

# 12.0 Write out to clean_data --------------------------------------------
write.taf(catch,file = file.path(Data_path,"clean_data/Catch_4_Makefleets.csv"))
write.taf(effort,file = file.path(Data_path,"clean_data/Effort_4_Makefleets.csv"))
