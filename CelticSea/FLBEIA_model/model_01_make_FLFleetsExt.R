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

# Here we want 


