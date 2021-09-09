##############################################################################
##
## Making the advice object inputs 
##
## Author:  Paul Dolder
## Created: 05/08/2021
## Updated:
##
##############################################################################
## Packages
library(tidyverse)
library(FLBEIA)

## Source gm_mean function
source(file.path("bootstrap", "software", "functions", "funcs.R"))

## Paths
data_path      <- file.path("results", "clean_data")
stock_path     <- file.path("results", "clean_data", "clean_stock_objects")
flbeia_in      <- file.path("results", "FLBEIA_inputs")
flbeia_precon  <- file.path(flbeia_in, "preconditioned")
flbeia_cond    <- file.path(flbeia_in, "conditioned")

## Load the biols
load(file.path(flbeia_precon, "FLBiols.RData"))

## Load in the fleets
load(file.path(flbeia_precon, "FLFleets.RData"))

## Load in the reference years
load(file.path(flbeia_in, "year_references.RData"))

################
## advice ctrl
################

stks <- names(biols)

#################################
### Harvest Control Rules
##################################

## We use the actual TAC advice
advice.ctrl <- create.advice.ctrl(stksnames = stks, HCR.models = rep('fixedAdvice', length(stks)))

# advice based on catch, not landings
for(i in stks) {
advice.ctrl[[i]]$AdvCatch      <- TRUE 
}

advice <- list(TAC = FLQuant(NA, dimnames = list(stock = stks, year = data_yrs[1]:sim_yrs[length(sim_yrs)])),
               quota.share = lapply(stks, function(x) {
                 FLQuant(NA,
                         dimnames = list(fleets = names(fleets), year = data_yrs[1]:sim_yrs[length(sim_yrs)]))
               }))


## Is there a TAC slot for all stocks ?
all(catchNames(fleets) %in% dimnames(advice$TAC)$stock)

##############################
#### Setting the TACs ########
##############################

## TACs for TAC year (note, we may need to extend this to TAC yr + 1)
## This is the actual TAC, not the catches from the stf

## Probably want to replace this with an input file

nep7 <- grep("nep", names(biols), value = TRUE)

TACs <- matrix(NA, nrow = length(biols) - length(nep7) + 1,
	       ncol = length(sim_yrs),
	       dimnames = list(c(names(biols)[!names(biols) %in% nep7], "nep7"),
			       sim_yrs)
	       )

TACs['cod.27.7e-k', ac(2020:2022)] <- c(1055, rep(0, 2)) #advice, and not TAC!
TACs['had.27.7b-k', ac(2020:2022)] <- c(20274, rep(18382, 2))
TACs['whg.27.7b-ce-k', ac(2020:2022)] <- c(8772, rep(5261, 2))
TACs['meg.27.7b-k8abd', ac(2020:2022)] <- c(20350, rep(19184, 2))
TACs['mon.27.78abd', ac(2020:2022)] <- c(24343, rep(34579, 2))
TACs['sol.27.7fg', ac(2020:2022)] <- c(1652, rep(1413, 2))
TACs["nep7",]            <- c(16815 * 0.48, rep(20557,2) * 0.48)

###################
## Nephrops shares
###################

## Get the mean share over the data years
nep7.land.shares <- lapply(nep7, function(x)  {landStock(fleets, x)[,ac(2017:2019)]})
nep7.land.shares <- lapply(nep7.land.shares, function(x) mean(x/Reduce("+", nep7.land.shares)))
names(nep7.land.shares) <- nep7


for(i in stks) {
print(i)
  if(grepl("nep", i)) {   ## If Nephrops, we want to take the TAC sums per division and subdivide by landings share across all fleets
    
    ## The TAC sum divided by share of landings
    advice$TAC[i, ac(sim_yrs)] <- TACs["nep7",] * nep7.land.shares[[i]]
       
    } else {
advice$TAC[i, ac(sim_yrs)] <- TACs[i, ac(sim_yrs)] 
  }
}

advice$TAC[,ac(2020:2022)]

##############################
## Setting the quota shares
##############################

## Redefine function to better handle NAs!!
landWStock.f <- function (obj, stock) 
{
  aux <- 0
  res <- FLQuant()
  for (m in obj@metiers) {
    if (!(stock %in% catchNames(m))) 
      next
    if (aux == 0) {
      aux <- 1
      res <- m@catches[[stock]]@landings.n * m@catches[[stock]]@landings.wt
      res[is.na(res)] <- 0
      next
    }
    resf <- m@catches[[stock]]@landings.n * m@catches[[stock]]@landings.wt
    resf[is.na(resf)] <- 0
    res <- res + resf
  }
  return(res)
}

## Based on observed landings split

## Years over which to average the quota shares.
sel_yrs <- 2017:2019

names(advice$quota.share) <- stks

for(st in stks){
  for(fl in names(fleets)) {
    if(st %in% catchNames(fleets[[fl]])){
      
      advice$quota.share[[st]][fl,ac(2009:data_yrs[2])] <- quantSums(landWStock.f(fleets[[fl]], st))/
                                       quantSums(landWStock(fleets, st))

      advice$quota.share[[st]][fl, ac(sim_yrs)] <-
        yearMeans(advice$quota.share[[st]][fl, ac(sel_yrs)])  ## change to sel_yrs
      } else
      advice$quota.share[[st]][fl,] <- 0
      
    }
  }

apply(advice$quota.share$`had.27.7b-k`,2,sum)


#########################
## Save for model input
#########################

save(advice, file = file.path(flbeia_cond, "advice.RData"))
save(advice.ctrl, file = file.path(flbeia_cond, "advice_ctrl.RData"))

