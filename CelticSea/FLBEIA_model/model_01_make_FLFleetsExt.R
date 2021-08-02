##############################################################################
##
## Making the FLFleet object from the pre-compiled, cleaned accessions and 
## Intercatch data
##
## Author:  Paul Dolder
## Created: 30/07/2021
## Updated:
##
##############################################################################
## Packages
library(tidyverse)
library(FLBEIA)

## Paths
data_path  <- file.path("results", "clean_data")
stock_path <- file.path("results", "clean_data", "clean_stock_objects")

##############################################################################
## Load in data

stock.list <- c("cod.27.7e-k", "had.27.7b-k","whg.27.7b-ce-k",
		"meg.27.7b-k8abd", 
		"mon.27.78abd", "sol.27.7fg",
		"nep.fu.16","nep.fu.17", "nep.fu.19", "nep.fu.2021",
		"nep.fu.22", "nep.out.7")

## Effort
ef <- read.csv(file.path(data_path, "Effort_4_Makefleets.csv"))

## Catch (landings and discards CATON)
ca <- read.csv(file.path(data_path, "Catch_4_Makefleets.csv")) %>% 
	filter(Stock %in% stock.list)

## Currently some -Inf in discards, REMOVE WHEN JOHN FIXES!!
ca[!is.finite(ca$Discards),"Discards"] <- 0

## Age distribution (cod, had, whg, sol only)
ad <- read.csv(file.path(data_path, "canum_summary.csv")) %>%
	filter(Stock %in% stock.list)

## Correction in IC data
unique(grep("-", ad$lvl4, value = TRUE))
ad$lvl4 <- gsub("-", "_", ad$lvl4)

## stock objects
wg.stocks <- FLStocks(lapply(stock.list, function(s) {
				     print(s)
				     load(file.path(stock_path, paste0(s,".RData")))
				     res <- get("stock")
				     name(res) <- s
				     res
}))


## Want to extend any stock objects back to earliest year, even if filled with
## NAs

wg.stocks <- FLStocks(lapply(wg.stocks,window, start = 2009))

##############################################################################

## Data rationalisation --
# Here we summarise the accessions data to reduce the number of fleets and
# métier to only those meeting a threshold of a minimum landings of any of the
# stocks.

## We will keep the data, to match the Intercatch files and then summarise the 
## "Others" fleets and métiers later...

flt_threshold <- 0.01      ## Fleet must have at least 1% of any stock
met_threshold <- 0.01      ## Métier within a fleet must have at least 1% of any stock
yrs_threshold <- 2017:2019 ## Years on which thresholds applied.

# Fleets to keep--

flt_keep <- ca %>% filter(Year %in% yrs_threshold) %>% group_by(Fleet, Stock) %>% 
   	summarise(landings = sum(Landings)) %>% group_by(Stock) %>%
	mutate(frac = landings/sum(landings)) %>% mutate(above_thres = frac >= flt_threshold) %>%
	group_by(Fleet) %>% filter(any(above_thres)) %>% select(Fleet) %>% unique() %>% c() %>% unlist()

## Add to the effort and catch data. Note, there are some fleets without catch,
## so we reverse the argument to capture these.

ef <- ef %>% mutate(Fleet_keep = ifelse(Fleet %in% flt_keep, TRUE, FALSE))
ca <- ca %>% mutate(Fleet_keep = ifelse(Fleet %in% flt_keep, TRUE, FALSE))

## Check we have a match
## Should return TRUE if all match
all(sort(unique(ef$Fleet[ef$Fleet_keep])) == sort(unique(ca$Fleet[ca$Fleet_keep])))

# Métiers to keep --

mt_keep <- ca %>% filter(Year %in% yrs_threshold) %>% group_by(Fleet, Metier, Stock) %>% 
   	summarise(landings = sum(Landings)) %>% group_by(Fleet, Stock) %>%
	mutate(frac = landings/sum(landings)) %>% mutate(above_thres = frac >= met_threshold) %>%
	group_by(Fleet, Metier) %>% filter(any(above_thres)) %>% select(Fleet, Metier) %>% unique()

## Add to the effort and catch data.
ef <- ef %>% mutate(Met_keep = ifelse(paste0(Fleet,Metier) %in% paste0(mt_keep$Fleet, mt_keep$Metier), TRUE, FALSE))

ca <- ca %>% mutate(Met_keep = ifelse(paste0(Fleet,Metier) %in% paste0(mt_keep$Fleet, mt_keep$Metier), TRUE, FALSE))

## Summarise data in other fleets
ca %>% group_by(Fleet_keep) %>% summarise(Landings =sum(Landings), Discards = sum(Discards)) 

ca %>% group_by(Fleet_keep) %>% summarise(Landings =sum(Landings), Discards = sum(Discards)) %>%
	select(Landings, Discards) %>% as.matrix() %>% prop.table(margin = 2)

## So 8% of total landings and 4% of total discards ends up in the others fleet
## (excluding area 8)

## summarise data in other metier
ca %>% filter(!Fleet_keep) %>% group_by(Met_keep) %>% summarise(Landings =sum(Landings), Discards = sum(Discards)) 

ca %>% filter(Fleet_keep) %>% group_by(Met_keep) %>% summarise(Landings =sum(Landings), Discards = sum(Discards)) %>%	select(Landings, Discards) %>% as.matrix() %>% prop.table(margin = 2)
## So 2% of landings and 3% of discards in the others métier (excluding the
## others fleet)

## How much effort in other fleets

ef %>% group_by(Fleet_keep) %>% summarise(kw_days = sum(kw_days)) 
ef %>% group_by(Fleet_keep) %>% summarise(kw_days = sum(kw_days))  %>% select(kw_days) %>% prop.table()

## Keeping 73% of effort

######################################################################################
## catch-at-age generation
#################################################

# Here we want to disaggregate the landings in ca by the age disaggregated data
# in ad.

disaggregate_catch <- function(ac_dat = NULL, ic_dat = NULL, wg.stocks = NULL, year = NULL, stk = NULL, Fl = NULL, Met = NULL) {

print(paste0(Fl, "  ", Met, "  ", stk, "  ", year ))


#############
## AC data ##
#############

ac_catch <- filter(ac_dat, Fleet == Fl, Metier == Met, Stock == stk, Year == year)

#############
## IC data ##
#############

## Landings

## First look for a complete match
ic_land <- filter(ic_dat, CatchCat == "Landings",
	             Country == sapply(strsplit(Fl,"_"), "[[", 1),
		     lvl4 == substr(Met,1,7), 
		     Area == substr(Met, 9, 14), Stock == stk, Year == year)

L_lvl <- "Area_Metier" # record what level the match was made

## Next drop area
if(nrow(ic_land)==0) {
ic_land <- filter(ic_dat, CatchCat == "Landings",
	     Country == sapply(strsplit(Fl,"_"), "[[", 1),
	     lvl4 == substr(Met,1,7),
	     Area == "27.7",
	     Stock == stk, Year == year)
L_lvl <- "Metier"
}

## Get the ages within the stock
stk.ages <- as.numeric(dimnames(wg.stocks[[stk]])$age)

## Final fall-back is stock level if no ic match
## Multiply weights by 1000 to get in grams
if(nrow(ic_land)==0) {
ic_land <- data.frame(CatchCat = "Landings",
		 Age = dimnames(wg.stocks[[stk]])$age,
		 CANUM = c(wg.stocks[[stk]]@landings.n[,ac(year)]),
		 Mean_Weight_in_g = 1000 * c(wg.stocks[[stk]]@landings.wt[,ac(year)]))

L_lvl <- "Stock"
}


## Discards 

## First look for a complete match
ic_disc <- filter(ic_dat, CatchCat == "Discards",
	             Country == sapply(strsplit(Fl,"_"), "[[", 1),
		     lvl4 == substr(Met,1,7), 
		     Area == substr(Met, 9, 14), Stock == stk, Year == year)

D_lvl <- "Area_Metier" # record what level the match was made

## Next drop area
if(nrow(ic_disc)==0) {
ic_disc <- filter(ic_dat, CatchCat == "Discards",
	     Country == sapply(strsplit(Fl,"_"), "[[", 1),
	     lvl4 == substr(Met,1,7),
	     Area == "27.7",
	     Stock == stk, Year == year)
D_lvl <- "Metier"
}

## Get the ages within the stock
stk.ages <- as.numeric(dimnames(wg.stocks[[stk]])$age)

## Final fall-back is stock level if no ic match
## Multiply weights by 1000 to get in grams
if(nrow(ic_disc)==0) {
ic_disc <- data.frame(CatchCat = "Discards",
		 Age = dimnames(wg.stocks[[stk]])$age,
		 CANUM = c(wg.stocks[[stk]]@discards.n[,ac(year)]),
		 Mean_Weight_in_g = 1000 * c(wg.stocks[[stk]]@discards.wt[,ac(year)]))

D_lvl <- "Stock"
}

## Merge the two

ic_land <- ic_land %>% select(CatchCat, Age, CANUM, Mean_Weight_in_g) 
ic_disc <- ic_disc %>% select(CatchCat, Age, CANUM, Mean_Weight_in_g) 

ic <- rbind(ic_land, ic_disc)
ic$Age <- as.numeric(ic$Age)

## Need to aggregate across any plus groups
stk.plus <- range(wg.stocks[[stk]])[["plusgroup"]]

# Convert any above plus group to the plus group
ic$Age <- ifelse(ic$Age > stk.plus, stk.plus, ic$Age)

###############################################
## Re-aggregate the data with new mean weight
###############################################

ic <- ic %>% select(CatchCat, Age, CANUM, Mean_Weight_in_g) 

## Make sure we have each age for each category

## What to do about IC ages below the stock range? - Remove for now
ic <- ic[ic$Age %in% stk.ages,]

## Now add any missing ages
for(a in stk.ages) {

## Landings
if(!a %in% ic[ic$CatchCat == "Landings","Age"]) {
ic <- rbind(ic, data.frame(CatchCat = "Landings", Age = a, CANUM = 0, Mean_Weight_in_g = NA))
}
## Discards 
if(!a %in% ic[ic$CatchCat == "Discards","Age"]) {
ic <- rbind(ic, data.frame(CatchCat = "Discards", Age = a, CANUM = 0, Mean_Weight_in_g = NA))
}

}

ic <- ic %>%
	group_by(CatchCat, Age, .drop = FALSE) %>% 
	summarise(N = sum(CANUM),
	wt = weighted.mean(x = Mean_Weight_in_g, w = CANUM)) %>% 
	group_by(CatchCat) %>%
	mutate(SOP = sum(N*(wt/1e3),na.rm = TRUE)) %>% 
	as.data.frame()

## Fix for particular case of  Spain in 2018 for haddock. Need to check why landings and discards are
## the same for this case. There are no landings in IC, so we use stock level.

if(Fl == "ES_Otter_24<40m"  & year == 2018 & stk %in% c("had.27.7b-k","whg.27.7b-ce-k")) {
ic[ic$CatchCat =="Landings","N"]   <- c(wg.stocks[[stk]]@landings.n[,ac(year)])
ic[ic$CatchCat =="Landings","wt"]  <- 1000 * c(wg.stocks[[stk]]@landings.wt[,ac(year)])
ic[ic$CatchCat =="Landings","SOP"] <- c(wg.stocks[[stk]]@landings[,ac(year)]) 
ic$N[is.na(ic$N)] <- 0
}

## Now disaggregate the landings and discards tonnages according to these
## distributions

## What is the number of fish at age for each category for 1 tonne of fish
ic_stand <- ic %>% group_by(CatchCat, Age) %>%
	summarise(N = N/SOP, wt = wt) %>% as.data.frame()

## Add the tonnage on to the data
ic_stand$tonnes <- ifelse(ic_stand$CatchCat == "Landings", ac_catch$Landings, ac_catch$Discards)

## Multiply through, taking care of units
ic_stand$FinalN <- ic_stand$N * ic_stand$tonnes 

## Return a data frame with the right information for the fleet object
res <- data.frame(Fleet = Fl, Metier = Met, Year = year, Stock = stk, Age = stk.ages,
	   landings.n = ic_stand[ic_stand$CatchCat == "Landings","FinalN"]/1e3, # in thousands
	   landings.wt = ic_stand[ic_stand$CatchCat == "Landings","wt"]/1e3, # in kg
	   discards.n = ic_stand[ic_stand$CatchCat == "Discards","FinalN"]/1e3,  # in thousands
	   discards.wt = ic_stand[ic_stand$CatchCat == "Discards","wt"]/1e3, # in kg
	   price = ac_catch$Value / ac_catch$Landings, # in euro per tonne
	   match_level_land = L_lvl,
	   match_level_disc = D_lvl
	   )

## For Nephrops, we fill the results with the actual landings and discards and
## the weights with 1, as per a non-analytical stock in FLBEIA.
## We would also do this for other non-analytical stocks (i.e. non cat 1
## stocks)

if(grepl("nep",stk)) {
res$landings.n  <- ac_catch$Landings
res$discards.n  <- ac_catch$Discards
res$landings.wt <- 1
res$discards.wt <- 1
}

## FLBEIA does not like NAs or 0s for catch weights due to over-quota discard
## calculations, so where these exist we will fill with the stock level values
if(nrow(res[is.na(res$landings.wt) | res$landings.wt == 0,])!=0) {
zero.lw.ages <- res[is.na(res$landings.wt) | res$landings.wt == 0,"Age"]
res[res$Age %in% zero.lw.ages,"landings.wt"] <- c(wg.stocks[[stk]]@landings.wt[ac(zero.lw.ages),ac(year)])
}

if(nrow(res[is.na(res$discards.wt) | res$discards.wt == 0,])!=0) {
zero.dw.ages <- res[is.na(res$discards.wt) | res$discards.wt == 0,"Age"]
res[res$Age %in% zero.dw.ages,"discards.wt"] <- c(wg.stocks[[stk]]@discards.wt[ac(zero.dw.ages),ac(year)])
}


#################################
## Final check that SOP matches #
#################################

## Having a rounding issue here, not sure how to address

## Not nephrops, units in 1000s
if(!grepl("nep",stk)) {
if(!near(sum(1e3 * res$landings.n * res$landings.wt, na.rm = TRUE),ac_catch$Landings )) stop("SOP for landings does not match")
if(!near(sum(1e3 * res$discards.n * res$discards.wt, na.rm = TRUE), ac_catch$Discards )) stop("SOP for discards does not match")
}

if(grepl("nep",stk)) {
if(!near(sum(res$landings.n * res$landings.wt, na.rm = TRUE),ac_catch$Landings )) stop("SOP for landings does not match")
if(!near(sum(res$discards.n * res$discards.wt, na.rm = TRUE),ac_catch$Discards )) stop("SOP for discards does not match")
}

res[is.na(res)] <- 0

return(res)

}


## Apply the function across all rows of ca -- that's a lot!!
options(dplyr.summarise.inform = FALSE) ## <- this right here is the ticket.


fleet_data <- lapply(seq_len(nrow(ca)), function(i) {
			     print(i)
disaggregate_catch(ac_dat = ca, ic_dat = ad, wg.stocks = wg.stocks, 
		   year = ca[i,"Year"], 
		   stk = ca[i,"Stock"], 
		   Fl = ca[i,"Fleet"], 
		   Met = ca[i,"Metier"])
	   })


fleet_data <- bind_rows(fleet_data)

## What level have the data matched to...
## Summarise the data and compare to the stock objects

stock_land <- lapply(wg.stocks, function(x) cbind(stock = x@name, as.data.frame(x@landings))) 
stock_land <- do.call(rbind, stock_land)
colnames(stock_land)[c(1,3,8)] <- c("Stock", "Year", "landings")

stock_disc <- lapply(wg.stocks, function(x) cbind(stock = x@name, as.data.frame(x@discards))) 
stock_disc <- do.call(rbind, stock_disc)
colnames(stock_disc)[c(1,3,8)] <- c("Stock", "Year", "discards")


## Accessions
ac_catch <- ca %>% group_by(Year, Stock) %>%
	summarise(Landings = sum(Landings), Discards = sum(Discards))


theme_set(theme_bw())

## Landings
fleetSOP <- fleet_data %>% group_by(Stock, Year, match_level_land) %>%
	summarise(landings = 1e3 * sum(landings.n * landings.wt,na.rm = TRUE)) 

fleetSOP$landings <- ifelse(grepl("nep", fleetSOP$Stock), fleetSOP$landings/1e3, fleetSOP$landings)

fleetSOP  %>%
	ggplot(aes(x = Year, y = landings, group = Stock)) +
	       geom_bar(stat= "identity", aes(fill = match_level_land)) +
	       facet_wrap(~Stock, scale = "free_y") +
	       geom_point(data = stock_land, colour = "black") +
	       geom_point(aes(y = Landings), data = ac_catch, colour = "red", shape = 2) + 
	       ggtitle("Bars are SOP for landings.n x landings.wt", subtitle = "Black circles = stock object landings (tonnes), red triangles = accessions landings (tonnes)") + theme(axis.text.x = element_text(angle = -90)) + 
	       ylab("Landings (tonnes)")
ggsave(file = file.path("Figures", "Data_workup_fleets_landings.png"), width = 10, height = 7)

       ## The differences are entirely down to the accessions and intercatch
       ## differences. 

## Discards
fleetSOP_D <- fleet_data %>% group_by(Stock, Year, match_level_disc) %>%
	summarise(discards = 1e3 * sum(discards.n * discards.wt,na.rm = TRUE)) 

fleetSOP_D$discards <- ifelse(grepl("nep", fleetSOP_D$Stock), fleetSOP_D$discards/1e3, fleetSOP_D$discards)

fleetSOP_D %>% 
	ggplot(aes(x = Year, y = discards, group = Stock)) +
	       geom_bar(stat= "identity", aes(fill = match_level_disc)) +
	       facet_wrap(~Stock) +
	       geom_point(data = stock_disc)+
	       geom_point(aes(y = Discards), data = ac_catch, colour = "red", shape = 2) + 
	       ggtitle("Bars are SOP for discards.n x discards.wt", subtitle = "Black circles = stock object discards (tonnes), red triangles = accessions discards (tonnes)") + theme(axis.text.x = element_text(angle = -90)) + 
	       ylab("Discards (tonnes)")
ggsave(file = file.path("Figures", "Data_workup_fleets_discards.png"), width = 10, height = 7)


## The differences are down to the discard raising only...

## Now let's compare numbers-at-age

stock_landN <- lapply(wg.stocks, function(x) cbind(stock = x@name, as.data.frame(x@landings.n))) 
stock_landN <- do.call(rbind, stock_landN)
colnames(stock_landN)[c(1,2,3,8)] <- c("Stock","Age", "Year", "landings.n")

stock_discN <- lapply(wg.stocks, function(x) cbind(stock = x@name, as.data.frame(x@discards.n))) 
stock_discN <- do.call(rbind, stock_discN)
colnames(stock_discN)[c(1,2,3,8)] <- c("Stock","Age", "Year", "discards.n")


fleets_N <- fleet_data %>% group_by(Year, Stock, Age) %>% summarise(landings.n = sum(landings.n), discards.n = sum(discards.n))

## Something wrong with monkfish scale of landings??
fleets_N[fleets_N$Stock == "mon.27.78abd",c("landings.n", "discards.n")] <- fleets_N[fleets_N$Stock == "mon.27.78abd",c("landings.n", "discards.n")]/1000

neps <- unique(grep("nep", stock_landN$Stock, value = TRUE))


filter(stock_landN, !Stock %in% neps) %>% ggplot(aes(x = Age , y = landings.n)) + 
	facet_grid(Stock ~ Year, scale = "free") +
	geom_line() +
	geom_line(data = filter(fleets_N, !Stock %in% neps), colour = "red", aes(y = landings.n*1000)) + 
	ggtitle("Landings numbers-at-age in stock object (black) and fleets (red)")
ggsave(file = file.path("Figures", "Data_workup_fleets_landings_at_age.png"), width = 10, height = 7)


filter(stock_discN, !Stock %in% neps) %>% ggplot(aes(x = Age , y = discards.n)) + 
	facet_grid(Stock ~ Year, scale = "free") +
	geom_line() +
	geom_line(data = filter(fleets_N, !Stock %in% neps), colour = "red", aes(y = discards.n*1000)) + 
	ggtitle("Discards numbers-at-age in stock object (black) and fleets (red)")
ggsave(file = file.path("Figures", "Data_workup_fleets_discards_at_age.png"), width = 10, height = 7)


##########################################################################
## Aggregate the data according to the fleet keep and métier keep list...
##########################################################################

fleet_data_model <- fleet_data

## Remove any effort for fleets that do not catch these stocks
ef_mod <- filter(ef, Fleet %in% flt_keep)

## Relabel other fleets
fleet_data_model$Fleet  <- ifelse(fleet_data_model$Fleet %in% flt_keep, fleet_data_model$Fleet, "Other_fleets")
fleet_data_model$Metier <- ifelse(fleet_data_model$Fleet %in% flt_keep, fleet_data_model$Metier, "Other_Metier")

## Relabel other metier
ef_mod$Metier <- ifelse(paste0(ef_mod$Fleet, ef_mod$Metier) %in% paste0(mt_keep$Fleet, mt_keep$Metier), ef_mod$Metier, "Other_Metier")

fleet_data_model$Metier <- ifelse(paste0(fleet_data_model$Fleet, fleet_data_model$Metier) %in% paste0(mt_keep$Fleet, mt_keep$Metier), fleet_data_model$Metier, "Other_Metier")

## Now aggregate the data

## Effort
ef_mod <- ef_mod %>% group_by(Fleet, Metier, Year, .drop = FALSE) %>% summarise(kw_days = sum(kw_days)) %>%
	arrange(Fleet, Metier, Year)

## Catch

ca_mod <- fleet_data_model %>% group_by(Fleet, Metier, Year, Stock, Age, .drop = FALSE) %>%
	summarise(landingsN = sum(landings.n, na.rm = TRUE), landings.wt = weighted.mean(x = landings.wt, w = landings.n,na.rm = TRUE), discardsN = sum(discards.n, na.rm = TRUE), discards.wt = weighted.mean(x = discards.wt, w = discards.n, na.rm = TRUE),  price = weighted.mean(x = price, w = landings.n, na.rm = TRUE))


## Catch in thousands - except Nephrops
ca_mod[!grepl("nep", ca_mod$Stock),"landingsN"] <- 1e3 * ca_mod[!grepl("nep", ca_mod$Stock),"landingsN"]
ca_mod[!grepl("nep", ca_mod$Stock),"discardsN"] <- 1e3 * ca_mod[!grepl("nep", ca_mod$Stock),"discardsN"]


######################################################################
######################################################################
## Make the FLFleet Object
######################################################################
######################################################################

yr.range<- min(ca_mod$Year):max(ca_mod$Year)

fq<-FLQuant(dimnames=list(year=yr.range),quant="age") # blank quant with right dimensions...

fl.nam <- unique(ca_mod$Fleet)

fleets <- FLFleetsExt(lapply(fl.nam, function(Fl) {

#######################
## FLFleetExt
#######################

print(paste("#############",Fl,"###########",sep=" "))

## Metiers in the fleet with catches of the modelled stocks
met.nam <- unique(filter(ca_mod, Fleet == Fl)$Metier)

# Blank quants with same dims
eff <- fq
cap <- fq

## Fleets effort ##
fl_ef <- filter(ef_mod, Fleet == Fl, Metier %in% met.nam)

## Add to quant
fl_ef_sum <- fl_ef %>% group_by(Year) %>% summarise(kw_days = sum(kw_days)) %>% arrange(Year) %>%
	as.data.frame()

yrs <- fl_ef_sum$Year ## years in data
eff[,ac(yrs)] <- fl_ef_sum$kw_days  / 1e3

## Fleets capacity ##

## Maximum observed inter-annual change
maxD <- max(abs(1-c(eff)[-1]/c(eff)[-length(eff)]))

cap <- eff * (1+maxD) ## Assume effort overall will not increase by more than 20%. Note: need to explore assumption -
# this is also really for the conditioning step, historic data should be based
# on years observed values

units(eff) <- units(cap) <- "000 kwdays"

## Metiers in the catch data for the fleet
fl_ca <- filter(ca_mod, Fleet == Fl, Metier %in% met.nam)

## Metiers effort
mt_ef <- fl_ef %>% filter(Metier %in% met.nam) %>% 
	group_by(Year) %>% 
	mutate(effshare = kw_days / sum(kw_days))

####################
### FLMetierExt
####################

metiers <- FLMetiersExt(lapply(met.nam, function(met) {

print(met)

## Metier effort (and effort share)
met_E <- filter(mt_ef, Metier == met)  %>% arrange(Year)

## Years in metier effort
yrs <- met_E$Year 

effmet <- fq

effmet[,ac(yrs)] <- met_E$effshare

## Stocks caught by the metier

mt_ca <- filter(fl_ca, Metier == met)

stk.nam <- unique(mt_ca$Stock)

####################
## FLCatches
###################

catch <- FLCatchesExt(lapply(stk.nam, function(S) {

## Fl-Met-Stk catch

caa <- filter(mt_ca, Stock == S) %>% arrange(Year, Age) %>% as.data.frame()

print(S)

# create the objects with the right dimensions
la.age<- la.wt <- di.age <- di.wt <- pr. <- al <- be <- FLQuant(dimnames=list(age=range(wg.stocks[[ac(S)]])[["min"]]:range(wg.stocks[[ac(S)]])[["max"]],year=yr.range),quant="age")
 
## years with catch data
yrs <- unique(caa$Year)

## Landings numbers
la.age[,ac(yrs)] <- caa$landingsN
# landings weights
la.wt[,ac(yrs)]  <- caa$landings.wt
## Discards numbers
di.age[,ac(yrs)] <- caa$discardsN
# discards weights
di.wt[,ac(yrs)]  <- caa$discards.wt
# price (per kg)
pr.[,ac(yrs)] <- caa$price/1e3
# cobb-douglas alpha and beta
al[,ac(yrs)] <- 1
be[,ac(yrs)] <- 1

# range
rg. <-  range(wg.stocks[[S]])


## FLCatch object
res <- FLCatchExt(range=rg., name=ac(S), landings.n=la.age,discards.n=di.age,
                        landings.wt=la.wt,discards.wt=di.wt,price=pr.,alpha=al,beta=be)
  
## Add the units 
units(res@landings.n)  <- units(res@discards.n) <-  '1000'
units(res@landings.wt) <-  units(res@discards.wt) <- "kg"
units(res@landings)    <- units(res@discards) <- "tonnes"
units(res@price)       <- "euro per kg"

## Add the catch with inbuilt functions

res@landings <- computeLandings(res)
res@discards <- computeDiscards(res)


return(res)
})) ## end FLCatches
names(catch) <- catchNames(catch)

m <- FLMetierExt(effshare = effmet, catches = catch, name = ac(met))
return(m)
}))

names(metiers) <- met.nam

fl <- FLFleetExt(metiers = metiers, name = Fl, effort = eff, capacity = cap)

fl@range <-  range(wg.stocks)

return(fl)

	   }))

names(fleets) <- fl.nam


save(fleets, file = file.path("results", "FLBEIA_inputs", "FLFleets.RData"))


###################
## Some checks rough....
####################

sumCatches <- lapply(stock.list, function(x) apply(catchWStock(fleets, x),2,sum)) 

names(sumCatches) <- stock.list

sumCatches

apply(catchWStock(fleets, "cod.27.7e-k"),2,sum) /
wg.stocks[["cod.27.7e-k"]]@catch

apply(landWStock(fleets, "cod.27.7e-k"),2,sum)/
wg.stocks[["cod.27.7e-k"]]@landings

####################
