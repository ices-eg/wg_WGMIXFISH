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


##########################
## Define scenarios
##########################

sc_list <- c("fixedEffort", "min", "max", "prev", "had.27.7b-k") ## name some

fleets.ctrl.list <- vector(mode = "list", length = length(sc_list))

fleets.ctrl.list[["fixedEffort"]] <- fleets.ctrl

fleets.ctrl.list[["min"]] <- fleets.ctrl

for(f in names(fleets)) {
  fleets.ctrl.list[["min"]][[f]]$effort.model <- "SMFB"
  fleets.ctrl.list[["min"]][[f]]$effort.restr <- "min"
  fleets.ctrl.list[["min"]][[f]]$restriction  <- "catch"
}

fleets.ctrl.list[["max"]] <- fleets.ctrl.list[["min"]]
for(f in names(fleets)) {
  fleets.ctrl.list[["max"]][[f]]$effort.restr <- "max"
}

fleets.ctrl.list[["prev"]] <- fleets.ctrl.list[["min"]]
for(f in names(fleets)) {
  fleets.ctrl.list[["prev"]][[f]]$effort.restr <- "prev"
}

fleets.ctrl.list[["had.27.7b-k"]] <- fleets.ctrl.list[["min"]]
for(f in names(fleets)) {
  fleets.ctrl.list[["had.27.7b-k"]][[f]]$effort.restr <- "had.27.7b-k"
}

##########################################
## 
## Run scenarios in parallel
##
##########################################

library(doParallel)

registerDoParallel(cores = parallel::detectCores()-1)

runs <- foreach(i = sc_list, .export = ls(.GlobalEnv)) %dopar% {
  
  library(FLBEIA)
  
  res <- FLBEIA(biols = biols,
                SRs = SRs, 
                BDs = NULL,
                fleets = fleets,
                covars = NULL,
                indices = NULL,
                advice = advice,
                main.ctrl = main.ctrl,
                biols.ctrl = biols.ctrl,
                fleets.ctrl = fleets.ctrl.list[[i]],
                covars.ctrl = NULL,
                obs.ctrl = obs.ctrl,
                assess.ctrl = assess.ctrl,
                advice.ctrl = advice.ctrl
  )
  
}

stopImplicitCluster()

names(runs) <- sc_list

## Summarise the results

bio <- rbind(bioSum(runs[["max"]], scenario = "max", years = ac(2019)),
             bioSum(runs[["min"]], scenario = "min", years = ac(2019)),
             bioSum(runs[["Esq"]], scenario = "Esq", years = ac(2019)),
             bioSum(runs[["COD"]], scenario = "COD", years = ac(2019)),
             bioSum(runs[["HAD"]], scenario = "HAD", years = ac(2019)),
             bioSum(runs[["MON"]], scenario = "MON", years = ac(2019)),
             bioSum(runs[["NHKE"]], scenario = "NHKE", years = ac(2019)),
             bioSum(runs[["NMEG"]], scenario = "NMEG", years = ac(2019)),
             bioSum(runs[["WHG"]], scenario = "WHG", years = ac(2019))            
)

## Save the outputs

save(runs, bio, file = file.path("results", "ScenarioResults.RData"))
