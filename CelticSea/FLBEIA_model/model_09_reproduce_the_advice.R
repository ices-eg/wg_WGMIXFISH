# reproduce the advice

## Before: single species advice stock objects
## After:  forecasts 

library(FLCore)
library(FLBEIA)
library(tidyverse)
library(ggplot2)


# Set up ####
# We take the smallest and largest years from the biols objects to frame our year range
data_yrs <- c(range(biols)[["minyear"]],
              range(biols)[["maxyear"]])

first_yr_sim <- 2020 
last_yr_sim  <- 2022
proj_yrs     <- first_yr_sim:last_yr_sim

ResetRestriciton <- TRUE
ResetLO <- TRUE
Cond_Fleet <- TRUE

fl.proj.avg.yrs <- 2017:2019 ## weights including landings.wt, discards.wt
sel.yrs         <- 2017:2019 ## the selection pattern including effort, effshare, catch.q, landings.sel, discards.sel


# Read in stock objects ####
stk_path  <- file.path("results/clean_data/clean_stock_objects/")
stocks_list <- list.files(path = stk_path, pattern = ".RData")
stocks<-FLStocks(lapply(list.files(path = stk_path, pattern = ".RData"),function(x){
  load(file.path(stk_path,x))
  res<-get("stock")
  name(res)<-gsub('.RData',"",x)
  res}))

##~Some fixes ####
stocks[["cod.27.7e-k"]]@discards.wt[ac(4:7),ac(2019)]    <- stocks[["cod.27.7e-k"]]@discards.wt[ac(4:7),ac(2018)]
stocks[["cod.27.7e-k"]]@discards.wt[ac(7),ac(2017:2019)] <- stocks[["cod.27.7e-k"]]@discards.wt[ac(7),ac(2012)] ## last year of discard rate at age 7

for(st in stocks@names) {
  if(all(is.na(stocks[[st]]@discards.wt))) {
    stocks[[st]]@discards.wt <- stocks[[st]]@stock.wt
  }
}

## For individual ages/years
for(st in stocks@names) {
  if(any(is.na(stocks[[st]]@discards.wt))) {
    stocks[[st]]@discards.wt[is.na(stocks[[st]]@discards.wt)] <- stocks[[st]]@stock.wt[is.na(stocks[[st]]@discards.wt)]
  }
}

# Convert to FLBiols and FLFleet ####
source("funcs/reproduce_the_advice.R")

fleets <- FLFleetsExt()
biols <- FLBiols()

for(i in 1:length(stocks_list)){
print(i)
fleets_sub   <- FLFleetsExt(as(stocks[[i]], "FLFleetExt")); names(fleets)
biols_sub    <- FLBiols(as(stocks[[i]], "FLBiol"))
fleets <- rbind(fleets, fleets_sub)
biols <- rbind(biols, biols_sub)
rm(fleets_sub,biols_sub)
}

# Control object ####

LO <- FALSE 

fls   <- names(fleets)
n.fls <-length(fleets) #number of the fleets

n.stks<-sum(sapply(sapply(fleets, catchNames), length)) # number of the fleet/stocks
n.flts.stks      <- sapply(lapply(fleets, catchNames), length) # number of stocks caught by each fleet.
flts.stksnames   <- NULL; for(f in 1:length(fleets))  flts.stksnames <- c(flts.stksnames, catchNames(fleets[[f]])) 

# From dorleta
# dyn.stk <- c("MON", "MAC", "BSS", "SOL", "HKE", "HOM",  "MEG")#, "WHB")
# cnt.stk <-  c("ANK", "WHG", "SDV", "RJU", "RJC", "RJN", "NEP")
# 
# stks <- c(dyn.stk, cnt.stk)


flq   <- FLQuant(dimnames = list(quant = 'all', year = data_yrs[1]:last_yr_sim, season = 1), iter = 1)

fleets.ctrl      <- create.fleets.ctrl(fls = fls,n.fls.stks=n.flts.stks,fls.stksnames=flts.stksnames,
                                       effort.models= effort.models,catch.models=catch.models,
                                       capital.models=capital.models, price.models=price.models,flq=flq)

for(i in names(fleets)) {
  fleets.ctrl[[i]]$effort.restr <- restrictionTable[restrictionTable$fleet == i, "eff.restriction"]
}


#
# fleets.ctrl  -- see bim script 04
# advice       -- see bim script 05
# advice.ctrl  -- see bim script 05, create.advice.ctrl()
# biols.ctrl   -- see bim script 03, create.biols.ctrl() 
# obs.ctrl     -- see bim script 03, create.obs.ctrl()
# assess.ctrl  -- see bim script 03, create.assess.ctrl()
# SRs          -- see bim script 03, need FLSRsim
# main.ctrl --  ## main.ctrl <- list(sim.years = c("initial" = 2020, "final" = 2022))


# It might even be able to be done quite generically in a loop for each stock.

