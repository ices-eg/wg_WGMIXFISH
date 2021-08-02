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

L_lvl <- "Perfect" # record what level the match was made

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

D_lvl <- "Perfect" # record what level the match was made

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

theme_set(theme_bw())

## Landings
fleet_data %>% group_by(Stock, Year, match_level_land) %>%
	summarise(landings = 1e3 * sum(landings.n * landings.wt,na.rm = TRUE)) %>% 
	reshape2::dcast(Stock + Year ~ match_level_land, value.var = "landings")

fleet_data %>% group_by(Stock, Year, match_level_land) %>%
	summarise(landings = 1e3 * sum(landings.n * landings.wt,na.rm = TRUE))  %>%
	ggplot(aes(x = Year, y = landings, group = Stock)) +
	       geom_bar(stat= "identity", aes(fill = match_level_land)) +
	       facet_wrap(~Stock)

## Discards
fleet_data %>% group_by(Stock, Year, match_level_disc) %>%
	summarise(discards = 1e3 * sum(discards.n * discards.wt,na.rm = TRUE)) %>% 
	reshape2::dcast(Stock + Year ~ match_level_disc, value.var = "discards")

fleet_data %>% group_by(Stock, Year, match_level_disc) %>%
	summarise(discards = 1e3 * sum(discards.n * discards.wt,na.rm = TRUE)) %>% 
	ggplot(aes(x = Year, y = discards, group = Stock)) +
	       geom_bar(stat= "identity", aes(fill = match_level_disc)) +
	       facet_wrap(~Stock)


## Summarise the totals and compare what's in the stock object...




## Aggregate the data according to the fleet keep and métier keep list...


