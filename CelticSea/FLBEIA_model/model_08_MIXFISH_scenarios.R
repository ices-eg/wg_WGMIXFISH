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

hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.g"]]@catches[["cod.27.7e-k"]]@landings
hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.g"]]@catches[["cod.27.7e-k"]]@discards

options(scipen = 10)
hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.g"]]@catches[["cod.27.7e-k"]]@discards.sel

(hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.g"]]@catches[["cod.27.7e-k"]]@discards.n / 
  (hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.g"]]@catches[["cod.27.7e-k"]]@discards.n + 
     hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.g"]]@catches[["cod.27.7e-k"]]@landings.n))[,ac(2020)]


fltS <- fltStkSum(hist)

## Who has catch that has increased a lot ??
options(scipen = 10)
filter(fltS, stock == "cod.27.7e-k", year %in% 2019:2020, catch > 100) %>% as.data.frame()


`filter(fltS, stock == "cod.27.7e-k", year %in% 2016:2020, fleet == "FRA_Otter_10<40m")



cod_stk <- hist$stocks[["cod.27.7e-k"]]

cod_stk <- window(cod_stk, end = 2022)

cod_stk@stock.n <- hist$biols[["cod.27.7e-k"]]@n
cod_stk@catch.n <- catchStock(hist$fleets, "cod.27.7e-k")
cod_stk@landings.n <- landStock(hist$fleets, "cod.27.7e-k")
cod_stk@discards.n <- discStock(hist$fleets, "cod.27.7e-k")

cod_stk@catch.wt <-catchWStock(hist$fleets, "cod.27.7e-k")/catchStock(hist$fleets, "cod.27.7e-k")
cod_stk@landings.wt <-landWStock(hist$fleets, "cod.27.7e-k")/landStock(hist$fleets, "cod.27.7e-k")
cod_stk@discards.wt <-discWStock(hist$fleets, "cod.27.7e-k")/discStock(hist$fleets, "cod.27.7e-k")

cod_stk@mat[,ac(2020:2022)] <- cod_stk@mat[,ac(2019)]
cod_stk@m[,ac(2020:2022)] <- cod_stk@m[,ac(2019)]
cod_stk@m.spwn[,ac(2020:2022)] <- cod_stk@m.spwn[,ac(2019)]
cod_stk@harvest.spwn[,ac(2020:2022)] <- cod_stk@harvest.spwn[,ac(2019)]

FLash::computeHarvest(cod_stk)
FLash::computeHarvest(cod_stk) + cod_stk@m

apply(FLash::computeHarvest(cod_stk)[ac(2:5),],2,mean)

stk_fwd <- FLash::stf(stock)

ctrl <- FLash::fwdControl(data.frame(year = c(ac(2020:2021)), val = c(1,1), quantity = c("f", "f"), rel.year = c(ac(2019,2019))))

Recr <- median(rec(stock)[,ac(2005:2019)])

stk_proj <- FLash::fwd(stk_fwd,ctrl=ctrl,
           sr=list(model="mean",params=FLPar(c(Recr),dimnames=list(params="a",year=unique(ctrl@target[,"year"]),iter=1))))

fbar(stk_proj)
stk_proj@catch.n[,ac(2020)]

cod_stk@catch.n[,ac(2020)]/
stk_proj@catch.n[,ac(2020)]

## So catching predicted by Cobb Douglas are leading to far higher F than under Baranov....
## Catches are 1.2 - 2 x as high.



## Do the calcs by hand

st <- "cod.27.7e-k"

B <- biols[[st]]@n


catches <- lapply(fleets, function(fl) {
  
  E <- fl@effort
  
  if(st %in% catchNames(fl)) {
    
    mt_catch <- lapply(fl@metiers, function(mt) {
      
      mtshare <- mt@effshare
      
      Ef_mt <- E * mtshare
      
      if(st %in% catchNames(mt)) {
        
        
        W <- (mt@catches[[st]]@landings.sel*mt@catches[[st]]@landings.wt +
              (1-mt@catches[[st]]@landings.sel)*mt@catches[[st]]@landings.wt)
        
        ## Should be  ##   C.m <- q.m*(Ef)^alpha.m*(N*W)^beta.m # [mt,na,nu,it]
        ## But conditioned q only uses numbers??
        res  <- (mt@catches[[st]]@catch.q %*% Ef_mt) *(B * W)
     
        res[is.na(res)] <- 0
        
        return(res) ## pF
        
        #return(C)
        
      } else {
        return(FLQuant(0, dimnames = list(age = dimnames(B)$age, year = dimnames(B)$year))) 
        
      }
      
    })
    
    return(Reduce("+", mt_catch))
    
  } else {
    return(FLQuant(0, dimnames = list(age = dimnames(B)$age, year = dimnames(B)$year)))
    }
  
})


catches <- Reduce("+", catches)

units(catches) <- "1000"


catches[,ac(2020)]
B[,ac(2020)]


catches[,ac(2020)]/
B[,ac(2020)]


catches[,ac(2020)]
catchStock(fleets, st)[,ac(2020)]
## close enough. So why F of 2.0 ??
## Maybe that the NAs are causing the problem internally

F_flbeia(hist, "2020")




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


## Save the outputs

save(runs, file = file.path("results", "ScenarioResults.RData"))

