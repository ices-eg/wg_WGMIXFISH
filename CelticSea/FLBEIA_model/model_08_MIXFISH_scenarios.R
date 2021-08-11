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
 }

hist <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = covars,
               indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
               covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)
biols <- hist$biols
fleets <- hist$fleets
advice <- hist$advice
for(st in names(SRs)) SRs[[st]]@ssb[,ac(2019:2021)] <- ssb(biols[[st]])[,ac(2019:2021)]



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Run the scenarios to produce the advice.   ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
main.ctrl$sim.years[] <- c(2021,2022) 

## why this??
for(st in names(biols)) biols[[st]]@n[1, '2022'] <-  biols[[st]]@n[1, '2021'] 


sc_list <- "fixedEffort" ## name some

fleets.ctrl.list <- list()

fleets.ctrl.list[["fixedEffort"]] <- fleets.ctrl

## Make some more


for(sc in names(fleets.ctrl.list)){ 
  cat('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
  cat('         *** ', sc,  ' ***\n')
  cat('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
  output[[sc]] <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = NULL,
                         indices = NULL, advice = advice, main.ctrl = main.ctrl, 
                         biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.list[[sc]], 
                         covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, 
                         advice.ctrl = advice.ctrl) 
}

##save(output, file = './output/results.RData')

