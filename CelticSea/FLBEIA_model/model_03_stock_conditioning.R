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

## Source gm_mean function
source(file.path("bootstrap", "software", "functions", "funcs.R"))

## Paths
data_path      <- file.path("results", "clean_data")
stock_path     <- file.path("results", "clean_data", "clean_stock_objects")
flbeia_in      <- file.path("results", "FLBEIA_inputs")
flbeia_precon  <- file.path(flbeia_in, "preconditioned")
flbeia_cond    <- file.path(flbeia_in, "conditioned")

# Load reference years
load(file.path(flbeia_in, "year_references.RData"))

# Load the biols
load(file.path(flbeia_precon, "FLBiols.RData"))

###########################################################################
## Expand the FLBiols to the right dimension and condition the future years

biols<-FLBiols(lapply(biols,window,start = data_yrs[1],end = sim_yrs[length(sim_yrs)]))

# data_yrs[1] fill the slots in projection years for FLBiols
## Note, we will want to do specifically for each stock and this is a short-cut

stk.avg.yrs <- 2017:2019  ## Note, may want to be stock specific

biols<-FLBiols(lapply(names(biols),function(x) {
  print(x)
  s<-biols[[x]]
  s@m[,ac(sim_yrs)]<-yearMeans(s@m[,ac(stk.avg.yrs)])
  
  s@wt[,ac(sim_yrs)]<-yearMeans(s@wt[,ac(stk.avg.yrs)])
  mat(s)[,ac(sim_yrs)]<-yearMeans(s@mat$mat[,ac(stk.avg.yrs)])
  fec(s)[,ac(sim_yrs)]<-yearMeans(s@fec$fec[,ac(stk.avg.yrs)])
  s@spwn <- s@n
  s@spwn[] <- 0
  
  return(s)
}))


###################
## Biols control
###################

aspg  <- names(biols)[!grepl("nep", names(biols))] ## not nephrops
fixed <- names(biols)[grepl("nep", names(biols))] ## nephrops

growth.model        <- c(rep("ASPG", length(aspg)), rep("fixedPopulation", length(fixed)))
names(growth.model) <- c(aspg, fixed)
growth.model        <- growth.model[names(growth.model) %in% names(biols)]

growth.model <- growth.model[sort(names(growth.model))]

stks <- sort(names(biols))

biols.ctrl       <- create.biols.ctrl(stksnames=stks,growth.models=growth.model)


##############################################
## Obs ctrl - as a STF, we assume perfect obs
################################################

stkObs.models<-rep('perfectObs',length(stks)) ; names(stkObs.models)<-stks
obs.ctrl  <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models)

obs.ctrl <- obs.ctrl[sort(names(obs.ctrl))]

## assess ctrl - as a STF, we do not do an assessment 

assess.models<-rep('NoAssessment',length(stks)); names(assess.models)<-stks
assess.ctrl<-create.assess.ctrl(stksnames=stks,assess.models=assess.models)

assess.ctrl <- assess.ctrl[sort(names(assess.ctrl))]


#################################################
## Stock-recruit inputs
#################################################

## Empty FLQuant with the right dimensions
flq_dims <- FLQuant(1, dim = c(1, length(data_yrs[1]:sim_yrs[length(sim_yrs)])), dimnames = list(quant = 'all', year = data_yrs[1]:sim_yrs[length(sim_yrs)]))

# ICES recr forecast (set recruitment numbers - in thousands)
Recr <- FLPar(NA, dimnames=list(params=aspg, year=sim_yrs,iter=1))

     Recr["cod.27.7e-k",] <- rep(median(rec(biols[["cod.27.7e-k"]])[,ac(2005:c(data_yrs[2]-1))]),length(sim_yrs))
     Recr["had.27.7b-k",] <- rep(median(rec(biols[["had.27.7b-k"]])[,ac(1993:c(data_yrs[2]-1))]),length(sim_yrs))
     Recr["whg.27.7b-ce-k",] <- rep(median(rec(biols[["whg.27.7b-ce-k"]])[,ac(2010:c(data_yrs[2]-1))]),length(sim_yrs))
     Recr["mon.27.78abd",] <- rep(gm_mean(rec(biols[["mon.27.78abd"]])[,ac(1986:c(data_yrs[2]-3))]),length(sim_yrs))
     Recr["sol.27.7fg",] <- rep(median(rec(biols[["sol.27.7fg"]])[,ac(1972:c(data_yrs[2]-3))]),length(sim_yrs))
     Recr["meg.27.7b-k8abd",] <- rep(gm_mean(rec(biols[["meg.27.7b-k8abd"]])[,ac(1984:c(data_yrs[2]-3))]),length(sim_yrs))

SRs <- lapply(aspg, function(x) {
  print(x)
    sr <- FLSRsim(rec = biols[[x]]@n[1,], ssb = ssb(biols[[x]]), 
          uncertainty = flq_dims, proportion = flq_dims, model = 'geomean', name = x)
    sr@params[,ac(sim_yrs),,] <- c(Recr[x,])
    return(sr)
    })

names(SRs) <- aspg 

##########################################################
## fixed population stocks
##########################################################

# For fixedPopulation stocks fill the biols @n with a value

#for(i in fixed) {
#biols[[i]]@n[,ac(sim_yrs)]<-biols[[i]]@n[,ac(data_yrs[length(data_yrs)])]  # fill the population n with last
#}

## For Nephrops, we should input the UWTV biomass estimates (N * wt) as the
## abundance, ensuring we reflect biomass changes in catch rates per FU...
## This should be the total catch / hr

biols[["nep.fu.16"]]@n[,ac(sim_yrs)] <- 3290/0.062
biols[["nep.fu.17"]]@n[,ac(sim_yrs)] <- 508/0.062
biols[["nep.fu.19"]]@n[,ac(sim_yrs)] <- 595/0.069
biols[["nep.fu.2021"]]@n[,ac(sim_yrs)] <- 1710/0.06
biols[["nep.fu.22"]]@n[,ac(sim_yrs)] <- 1710/0.06
biols[["nep.out.7"]]@n[,ac(sim_yrs)] <- biols[["nep.out.7"]]@n[,ac(data_yrs[length(data_yrs)])] 

##############################################
## Save to model inputs
##############################################
sapply(biols, checkBiols)
checkObsctrl(obs.ctrl)
checkSRs(SRs)

save(biols, file = file.path(flbeia_cond, "FLBiols.RData"))
save(biols.ctrl, file = file.path(flbeia_cond, "biols_ctrl.RData"))
save(obs.ctrl, file = file.path(flbeia_cond, "obs_ctrl.RData"))
save(assess.ctrl, file = file.path(flbeia_cond, "assess_ctrl.RData"))
save(SRs,file=file.path(flbeia_cond,'SRs.RData'))
