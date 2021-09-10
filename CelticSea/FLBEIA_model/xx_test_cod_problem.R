##############################################################################
##
## Running the MIXFISH scenarios 
##
## Author:  Paul Dolder
## Created: 11/08/2021
## Updated:
##
##############################################################################
## Packages
library(tidyverse)
library(FLBEIA)

## Paths
data_path      <- file.path("results", "clean_data")
stock_path     <- file.path("results", "clean_data", "clean_stock_objects")
flbeia_in      <- file.path("results", "FLBEIA_inputs")
flbeia_precon  <- file.path(flbeia_in, "preconditioned")
flbeia_cond    <- file.path(flbeia_in, "conditioned")

## Load in the FLBEIA objects
lapply(list.files(flbeia_cond, full.names = TRUE),
       function(x) load(x,envir = .GlobalEnv))


###############################
## SQ Effort intermediate year
###############################

main.ctrl                  <- list()
main.ctrl$sim.years        <- c(initial = 2020, final = 2021)
main.ctrl$SimultaneousMngt <- FALSE

for(f in names(fleets)) {
  fleets.ctrl[[f]]$effort.model <- "fixedEffort"
  fleets.ctrl[[f]]$restriction  <- "catch"
 }


### FIX COD POPULATION

biols.ctrl$'cod.27.7e-k'$growth.model <- "fixedPopulation"

biols[["cod.27.7e-k"]]@n[,ac(2020:2022)] <- biols[["cod.27.7e-k"]]@n[,ac(2019)]


hist <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = covars,
               indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
               covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)


out <- bioSum(hist)

filter(out, stock == "cod.27.7e-k", year == 2020)

