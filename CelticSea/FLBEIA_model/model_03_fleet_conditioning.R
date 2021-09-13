##############################################################################
##
## Conditioning the stock object inputs 

## Author:  Paul Dolder
## Created: 05/08/2021
## Updated:
##
##############################################################################
## Packages
library(tidyverse)
library(FLBEIA)

Rcpp::sourceCpp(file.path("bootstrap", "software", "functions","cond_effort.cpp"))
source(file.path("bootstrap", "software", "functions", "calculate.q.sel.flrObjs.cpp.R"))

## Paths
data_path      <- file.path("results", "clean_data")
stock_path     <- file.path("results", "clean_data", "clean_stock_objects")
flbeia_in      <- file.path("results", "FLBEIA_inputs")
flbeia_precon  <- file.path(flbeia_in, "preconditioned")
flbeia_cond    <- file.path(flbeia_in, "conditioned")

# Load reference years
load(file.path(flbeia_in, "year_references.RData"))

## Load the biols
load(file.path(flbeia_cond, "FLBiols.RData"))

## Load in the fleets
load(file.path(flbeia_precon, "FLFleets.RData"))

## Load in the stock objects
stock.list <- c("cod.27.7e-k", "had.27.7b-k","whg.27.7b-ce-k",
		"meg.27.7b-k8abd", 
		"mon.27.78abd", "sol.27.7fg",
		"nep.fu.16","nep.fu.17", "nep.fu.19", "nep.fu.2021",
		"nep.fu.22", "nep.out.7")

wg.stocks <- FLStocks(lapply(stock.list, function(s) {
				     print(s)
				     load(file.path(stock_path, paste0(s,".RData")))
				     res <- get("stock")
				     name(res) <- s
				     res
}))

##############################################################################

fl.proj.avg.yrs <- 2017:2019 ## weights including landings.wt, discards.wt
sel.yrs         <- 2017:2019 ## the selection pattern including effort, effshare, catch.q, landings.sel, discards.sel


##########################
# Expand the FLFleetsExt
##
##
## Expand to simulation years, and condition simulation variables

Cond <- TRUE 

##########################

if(Cond) {

 fleets<-FLFleetsExt(lapply(fleets,window,start = data_yrs[1],end = sim_yrs[length(sim_yrs)])) # Note: keep as list, as slower to access FLFleetsExt 

 
 ## Now use the new Cpp function to set the effort values
 yrs  <- as.numeric(dimnames(fleets[[1]]@effort)$year)
 first_yr_sim <- which(yrs == sim_yrs[1]) - 1
 last_yr_sim  <- which(yrs == sim_yrs[length(sim_yrs)]) - 1
 first_avg_yr <- which(yrs == fl.proj.avg.yrs[1]) - 1
 last_avg_yr  <- which(yrs == fl.proj.avg.yrs[length(fl.proj.avg.yrs)]) - 1
 
 fleets <-  condition_fleet_effort(fleets, 
                                   dim = dim(fleets[[1]]@effort),
                                   sim_yrs = first_yr_sim:last_yr_sim,
                                   mean_yrs = first_avg_yr:last_avg_yr
 )
 
 

 } else {
load(file.path(flbeia_cond, "FLFleetsExt.RData"))

}

#########################
##
## fleets control
##
#########################

fls   <- names(fleets)
n.fls <-length(fleets) #number of the fleets

n.stks<-sum(sapply(sapply(fleets, catchNames), length)) # number of the fleet/stocks
n.flts.stks      <- sapply(lapply(fleets, catchNames), length) # number of stocks caught by each fleet.
flts.stksnames   <- NULL; for(f in 1:length(fleets))  flts.stksnames <- c(flts.stksnames, catchNames(fleets[[f]])) 


##############################
#### FLEET MODELS
##############################

##############################
##### Effort model and 
##### effort restriction #####
##############################

## Fixed effort, SMFB etc...
## SMFB, min equivilent to FCube min
effort.models    <- rep("fixedEffort", length = n.fls) ; names(effort.models)<-fls #

## using SMFB set the effort limitation by fleet, i.e. vector with n.fl values with min, max etc..
## using SMFB, set a restriction on 'catch' or 'landings' by fleet
restriction  <- rep("catch", length = n.fls) ; names(restriction) <-fls 

###########################
### CATCH MODELS
############################

## This is automatic
c.mod<-stack(lapply(fleets,catchNames))
c.mod$catch.mod<-sapply(c.mod$values,function(x) {
  if(x %in% grep("nep", stock.list, value = TRUE, invert = TRUE)) return("CobbDouglasAge") else  ## CobbDouglasAge
  return("CobbDouglasBio")
})
catch.models     <- c.mod$catch.mod ; names(catch.models)<-paste(c.mod$ind,c.mod$values,sep=".")
#############################

############################
### CAPTIAL MODELS
#############################
## Is fixed 
capital.models   <- rep("fixedCapital",n.fls)           ; names(capital.models)<-fls
#############################

#############################
### PRICE MODELS
## fixed
##############################
price.models     <- rep("fixedPrice",n.stks)            ; names(price.models)<-paste(c.mod$ind,c.mod$values,sep=".")

#############################
## Create the fleets ctrl
#############################

flq   <- FLQuant(dimnames = list(quant = 'all', year = data_yrs[1]:sim_yrs[length(sim_yrs)], season = 1), iter = 1)

fleets.ctrl      <- create.fleets.ctrl(fls = fls,n.fls.stks=n.flts.stks,fls.stksnames=flts.stksnames,
                                         effort.models= effort.models,catch.models=catch.models,
                                         capital.models=capital.models, price.models=price.models,flq=flq)

## Here if we want to add a default min or max SMFB restriction to $effort.restr

##############################################################################################
## Calculate the catchability for the projection years - uses either CobbDouglas or Baranov ##
##############################################################################################

## Fix for Nephrops where there are no landings or discard weights
nep <- grep("nep", names(wg.stocks), value = TRUE)

for(n in nep) {
  wg.stocks[[n]]@landings.wt[] <- 1
  wg.stocks[[n]]@discards.wt[] <- 1
  
}


if(Cond) {

fleets <- calculate.q.sel.flrObjs.cpp(biols, stocks = wg.stocks, fleets = fleets, BDs = NULL, fleets.ctrl, mean.yrs = sel.yrs, sim.yrs = sim_yrs)

}


## Original function
#fleets <- calculate.q.sel.flrObjs(biols, fleets = fleets, BDs = NULL, fleets.ctrl, mean.yrs = sel.yrs, sim.yrs = sim_yrs)


sapply(fleets, checkFleets)
validObject(fleets)

### Save the fleets and fleets_ctrl

save(fleets, file = file.path(flbeia_cond, "FLFleetsExt.RData"))
save(fleets.ctrl, file = file.path(flbeia_cond, "fleets_ctrl.RData"))

