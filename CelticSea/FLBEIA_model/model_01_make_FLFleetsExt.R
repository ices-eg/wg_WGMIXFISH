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
stock_path <- file.path("results", "clean_stock_objects")

##############################################################################
## Load in data

stock.list <- c("cod.27.7e-k", "had.27.7b-k","meg.27.7b-k8abd", 
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
## So 1% of landings and 2% of discards in the others métier (excluding the
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

## First look for a complete match
ic <- filter(ic_dat, Country == sapply(strsplit(Fl,"_"), "[[", 1),
				 lvl4 == substr(Met,1,7), 
				 Area == substr(Met, 8, 13), Stock == stk, Year == year)
## Next drop area
if(nrow(ic)==0) {
ic <- filter(ic_dat, Country == sapply(strsplit(Fl,"_"), "[[", 1),
				 lvl4 == substr(Met,1,7),
				 Area == "27.7",
				 Stock == stk, Year == year)
}

## Get the ages within the stock
stk.ages <- as.numeric(dimnames(wg.stocks[[stk]])$age)

## Final fall-back is stock level if no ic match
## Multiply weights by 1000 to get in grams
if(nrow(ic)==0) {
ic <- data.frame(CatchCat = rep(c("Landings", "Discards"), each = length(stk.ages)),
		 Age = rep(dimnames(wg.stocks[[stk]])$age, times =  2),
		 CANUM = c(c(wg.stocks[[stk]]@landings.n[,ac(year)]),c(wg.stocks[[stk]]@discards.n[,ac(year)])),
		 Mean_Weight_in_g = 1000 * c(c(wg.stocks[[stk]]@landings.wt[,ac(year)]),c(wg.stocks[[stk]]@discards.wt[,ac(year)])))
}

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
	   discards.wt = ic_stand[ic_stand$CatchCat == "Discards","wt"]/1e3 # in kg
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

## Final check that SOP matches

## Not nephrops, units in 1000s
if(!grepl("nep",stk)) {
if(!round(sum(1e3 * res$landings.n * res$landings.wt, na.rm = TRUE),3) == round(ac_catch$Landings ,3)) stop("SOP for landings does not match")
if(!round(sum(1e3 * res$discards.n * res$discards.wt, na.rm = TRUE),3) == round(ac_catch$Discards ,3)) stop("SOP for discards does not match")
}

if(grepl("nep",stk)) {
if(!round(sum(res$landings.n * res$landings.wt, na.rm = TRUE),3) == round(ac_catch$Landings ,3)) stop("SOP for landings does not match")
if(!round(sum(res$discards.n * res$discards.wt, na.rm = TRUE),3) == round(ac_catch$Discards ,3)) stop("SOP for discards does not match")
}

return(res)

}


## Apply the function across all rows of ca -- that's a lot!!
options(dplyr.summarise.inform = FALSE) ## <- this right here is the ticket.


fleet_data <- lapply(seq_len(nrow(ca)), function(i) {
			     print(i)
disaggregate_catch(ac_dat = ca, ic_dat = ic_dat, wg.stocks = wg.stocks, year = ca[i,"Year"], stk = ca[i,"Stock"], Fl = ca[i,"Fleet"], Met = ca[i,"Metier"])
	   })


fleet_data <- bind_rows(fleet_data)

## Summarise the totals
