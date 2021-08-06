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

 fleets<-lapply(fleets,window,data_yrs[1],sim_yrs[length(sim_yrs)]) # Note: keep as list, as slower to access FLFleetsExt 


fleets <- unclass(fleets)

nms.fls <- names(fleets)
l.fls   <- length(nms.fls)

for(i in 1:l.fls){
  print(paste0("-----", nms.fls[i], "-------"))
  
  nms.metiers <- names(fleets[[i]]@metiers)
  l.metiers   <- length(nms.metiers)
  
  fleets[[i]]@effort[, ac(sim_yrs)]    <- yearMeans(fleets[[i]]@effort[, ac(sel.yrs)])
  fleets[[i]]@fcost[, ac(sim_yrs)]     <- yearMeans(fleets[[i]]@fcost[, ac(fl.proj.avg.yrs)])
  fleets[[i]]@capacity[, ac(sim_yrs)]  <- yearMeans(fleets[[i]]@capacity[, ac(fl.proj.avg.yrs)])
  fleets[[i]]@crewshare[, ac(sim_yrs)] <- yearMeans(fleets[[i]]@crewshare[, ac(fl.proj.avg.yrs)])
  
  for( j in 1:l.metiers){
    print(paste0("--", fleets[[i]]@metiers[[j]]@name, "--"))
    fleets[[i]]@metiers[[j]]@effshare[, ac(sim_yrs)] <- yearMeans(fleets[[i]][[j]]@effshare[, ac(sel.yrs)])
    fleets[[i]]@metiers[[j]]@vcost[, ac(sim_yrs)]    <- yearMeans(fleets[[i]][[j]]@vcost[, ac(fl.proj.avg.yrs)])
    

    nms.stks <- names(fleets[[nms.fls[i]]]@metiers[[nms.metiers[j]]]@catches)
    l.stks <- length(nms.stks)
    
    for( k in 1:l.stks){
      print(fleets[[i]]@metiers[[j]]@catches[[k]]@name)
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[,ac(fl.proj.avg.yrs)])
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[,ac(fl.proj.avg.yrs)])

      
      ## If NAs
       if(any(is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)]))){
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)][is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)])] <- 0
      }
      
      if(any(is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)]))) {
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)][is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)])] <- 0
      }

      ## Even where there are zero catches at an age, we want to have a value
      if(any(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)][fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)] == 0])){
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)][fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)] == 0] <- 
        yearMeans(biols[[fleets[[i]]@metiers[[j]]@catches[[k]]@name]]@wt[,ac(sim_yrs)][fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[, ac(sim_yrs)] == 0])
      }
      
      if(any(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)][fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)] == 0])) {
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)][fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)] == 0] <- 
        yearMeans(biols[[fleets[[i]]@metiers[[j]]@catches[[k]]@name]]@wt[,ac(sim_yrs)][fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[, ac(sim_yrs)] == 0])
      }
      
      
      ## selection 
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(sim_yrs)] <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[,ac(sel.yrs)])
  
      # set any NAs in the proj year to 0 (in case of no catch)
      fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(sim_yrs)][is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(sim_yrs)])]<-0
      # discards selectivity as the inverse of the landings sel
      fleets[[i]]@metiers[[j]]@catches[[k]]@discards.sel[, ac(sim_yrs)] <- 1-fleets[[i]]@metiers[[j]]@catches[[k]]@landings.sel[, ac(sim_yrs)]
       fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[,ac(sim_yrs) ]     <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[,ac(sel.yrs)])
      fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[, ac(sim_yrs)][is.na(fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q[, ac(sim_yrs)])]<-0     
     
      # Catch prod values 
      fleets[[i]]@metiers[[j]]@catches[[k]]@alpha[, ac(sim_yrs)]        <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@alpha[,ac(fl.proj.avg.yrs)])
      fleets[[i]]@metiers[[j]]@catches[[k]]@beta[, ac(sim_yrs)]         <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@beta[,ac(fl.proj.avg.yrs)])
      

      fleets[[i]]@metiers[[j]]@catches[[k]]@price[, ac(sim_yrs)]  <- yearMeans(fleets[[i]]@metiers[[j]]@catches[[k]]@price[,ac(fl.proj.avg.yrs)])
      
    }
  }
}

fleets <- FLFleetsExt(fleets)
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

if(Cond) {
fleets <- calculate.q.sel.flrObjs(biols, fleets, BDs = NULL, fleets.ctrl, mean.yrs = sel.yrs, sim.yrs = sim_yrs)
}

Fl <- "BE_Beam_10<24m"
Mt <- "Other_Metier"
st <- "cod.27.7e-k"

fleets[[Fl]]@metiers[[Mt]]@catches[[st]]@beta
apply(biols[[st]]@n * biols[[st]]@wt,2,sum)

sapply(fleets, checkFleets)
validObject(fleets)

### Save the fleets and fleets_ctrl

save(fleets, file = file.path(flbeia_cond, "FLFleetsExt.RData"))

