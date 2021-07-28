# reproduce the advice

## Before: single species advice stock objects
## After:  forecasts 

library(FLCore)
library(FLBEIA)
library(ggplot2)


#source functions
source("funcs/reproduce_the_advice.R")

#read in stock objects
#

## Can now use to get both the FLFleet and FLBiols

#fleets   <- FLFleetsExt(as(stock, "FLFleetExt")); names(fleets)
#biols    <- FLBiols(as(stock, "FLBiol"))

## Need to add:

# fleets.ctrl  -- see bim script 04
# advice       -- see bim script 05
# advice.ctrl  -- see bim script 05, create.advice.ctrl()
# biols.ctrl   -- see bim script 03, create.biols.ctrl() 
# obs.ctrl     -- see bim script 03, create.obs.ctrl()
# assess.ctrl  -- see bim script 03, create.assess.ctrl()
# SRs          -- see bim script 03, need FLSRsim
# main.ctrl --  ## main.ctrl <- list(sim.years = c("initial" = 2020, "final" = 2022))


# Create the FLBiol          - as(stock, "FLBiol")
# Create the FLFLeetExt - as(stock, "FLFleetExt") 
# ## this is based on a single fleet catching everything, so like a single stock forecast method.
# 
# 
# It might even be able to be done quite generically in a loop for each stock.

