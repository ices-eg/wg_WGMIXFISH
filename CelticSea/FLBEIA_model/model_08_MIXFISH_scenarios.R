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

hist <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = covars,
               indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
               covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)
biols <- hist$biols
fleets <- hist$fleets
advice <- hist$advice
for(st in names(SRs)) SRs[[st]]@ssb[,ac(2019:2021)] <- ssb(biols[[st]])[,ac(2019:2021)]


out <- bioSum(hist)

theme_set(theme_bw())
ggplot(filter(out, year <2021),aes(x=year, y = f)) + 
  geom_point(colour = rep(c(rep("grey",8), rep("black",3), "red"), each = length(biols))) + geom_line() + 
  facet_wrap(~stock, scale = "free_y") + expand_limits(y = 0) + 
  geom_vline(xintercept =  2017, colour = "grey") +  geom_vline(xintercept =  2019, colour = "grey")
ggsave(file.path("figures", "Intermediate_year_diag.png"), width = 8, height = 6)



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Run the scenarios to produce the advice.   ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#main.ctrl$sim.years[] <- c(2021,2022) 
main.ctrl$sim.years <- 2021:2022 
## why this??
for(st in names(biols)) biols[[st]]@n[1, '2022'] <-  biols[[st]]@n[1, '2021'] 

##########################
## Define scenarios
##########################

sc_list <- c("fixedEffort", "min", "max", "prev", "had.27.7b-k") ## name some

fleets.ctrl.list <- vector(mode = "list", length = length(sc_list))

fleets.ctrl.list[["fixedEffort"]] <- fleets.ctrl

fleets.ctrl.list[["min"]] <- fleets.ctrl

## Non residual fleets
flt_list <- grep("_fleet", names(fleets), value = TRUE, invert = TRUE)

for(f in flt_list) {
  fleets.ctrl.list[["min"]][[f]]$effort.model <- "SMFB"
  fleets.ctrl.list[["min"]][[f]]$effort.restr <- "min"
  fleets.ctrl.list[["min"]][[f]]$restriction  <- "catch"
}

fleets.ctrl.list[["max"]] <- fleets.ctrl.list[["min"]]
for(f in flt_list) {
  fleets.ctrl.list[["max"]][[f]]$effort.restr <- "max"
}

fleets.ctrl.list[["prev"]] <- fleets.ctrl.list[["min"]]
for(f in flt_list) {
  fleets.ctrl.list[["prev"]][[f]]$effort.restr <- "prev"
}

fleets.ctrl.list[["had.27.7b-k"]] <- fleets.ctrl.list[["min"]]
for(f in flt_list) {
  fleets.ctrl.list[["had.27.7b-k"]][[f]]$effort.restr <- "had.27.7b-k"
}

fleets.ctrl.list[["cod.27.7e-k"]] <- fleets.ctrl.list[["min"]]
for(f in flt_list) {
  fleets.ctrl.list[["cod.27.7e-k"]][[f]]$effort.restr <- "cod.27.7e-k"
  fleets.ctrl.list[["min"]][[f]]$restriction  <- "catch"
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


out <- bioSum(res)

theme_set(theme_bw())
ggplot(out,aes(x=year, y = f)) + 
  geom_point() + geom_line() + 
  facet_wrap(~stock, scale = "free_y") + expand_limits(y = 0) + 
  geom_vline(xintercept =  2017, colour = "grey") +  geom_vline(xintercept =  2019, colour = "grey")

filter(out, year == 2020, stock == "cod.27.7e-k")


## Save the outputs

save(runs, file = file.path("results", "ScenarioResults.RData"))

