# 0. General info ---------------------------------------------------------

# author: Marc Taylor
# date: 2019-06-13
# description: imports conditioned biols and fleets. Adds further FLBEIA 
#   scenario control inputs

# 1. Initialise system ----------------------------------------------------

## Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()

## Scenario name
# scenario.name <- "ICES_MIXFISH_demo"

## Required packages 
# library(FLCore)  
# library(FLash)
# library(FLAssess)
# library(FLFleet)
# library(FLXSA)
library(FLBEIA)
# library(beepr)
# library(spict)


## Required functions
source("bootstrap/software/functions/nameChange.R")
source("bootstrap/software/functions/gm_mean.R")

source('bootstrap/software/functions/funcs.r')                                                   
# source('bootstrap/software/functions/remove_validity_FLFleet.r') # to reduce computing time      

# load previous STF settings
load(file= "model/model_settings.Rdata", verbose = TRUE)
ver <- "01_Reproduce_The_Advice_2020_keepNYrNow"
if (length(LO)>0) Run.name <- paste0(ver,"_LO") else Run.name <- ver
load(file = paste0("model","/", Run.name,".Rdata"), verbose = T)

# Single species advice
single.species.advice <- read.table("bootstrap/data/Reproduce_The_Advice/single_species_advice.csv",sep=",",header=T)
  

# Global settings
first.yr          <- 2014
proj.yr           <- an(yr.now) 
last.yr           <- an(yr.TACp1)
hist.yrs  <- ac(first.yr:(proj.yr-1))
proj.yrs  <- ac(proj.yr:last.yr)
ni <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$iter})) # number of iterations
ns <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$season}))  # number of seasons
# Year extent in objects
yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr, "last.yr" = last.yr))



### load previous objects
load( file = file.path("model", "02_condition_FLBEIA.Rdata"))


# 2. Reference points -----------------------------------------------------
BRPs <- as.data.frame(BRPs)
BRPs <- cbind(data.frame(stock_name = nameChange(rownames(BRPs), form = "new")), BRPs)

# for(i in seq(spicts)){
#   tmp <- data.frame(
#     stock_name = names(spicts)[i],
#     Blim = min(get.par("logB", spicts[[i]], exp = TRUE)[,2]),
#     Bpa = min(get.par("logB", spicts[[i]], exp = TRUE)[,2])*1.4,
#     Fmsy = spicts[[i]]$report$Fmsy,
#     Bmsy = spicts[[i]]$report$Bmsy
#   )
#   BRPs <- merge(BRPs, tmp, all = TRUE)
# }

Fmsy.nep <- data.frame(
  stock_name = c("NEP6","NEP7","NEP8","NEP9"),
  Fmsy = c(0.053,0.07,0.163,0.118))

BRPs <- merge(x = BRPs, Fmsy.nep, all = TRUE)
BRPs

# 3. Intermediate year TACs --------------------------------------------------

# # defines stocks under landing obligation
# LO <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-NS", "WHG-NS", "TUR", fixed_stocks)
# 
# WHGNS_TACratio  <- 0.777      # ratio from TAC in area 4 to the total catch    #  updated 2019 (VT)  "The human consumption fishery (HCF) catch split between Subarea 4 and Division 7.d in 2019 and 2020 is the same as the proportion of HCF catch between the areas in 2018: 77.7% from Subarea 4 and 22.3% from Division 7.d."
# PLEEC_TACratio  <- 0.762      # ratio from TAC in area 7d to the total catch   # updated 2019 (VT), calculated myself so needs checking (1-mean(7e landings 2003-2018/TAC 7d.e. 2003-2018)), value was 0.736 in 2018 : "Division 7.d proportion of the TAC assuming the same proportion of the TAC is taken from Division 7.e as during 2003–2018"
# PLEEC_VIIDratio <- 1-0.151  # ratio resident stock                             #  updated 2019 (VT) :  "All plaice in Division 7.d, including plaice originating from the North Sea and the western English Channel, according to a ratio calculated over the years 2003–2018: 15.1% of the plaice landed in Division 7.d is assumed to originate from the North Sea and the western English Channel, and this is added to the predicted values for the Division 7.d plaice stock."
# TURprop <- 0.711 # calculated myself, ratio official landings TUR 4 in official landings TUR and Brill in 4 and 2a for last 3 years (2016-2018)
# WITprop <- 0.3063  # Calculated by AK, ratio of official landings of witch to official landings witch and lemon sole
# 
# NEP6_IntYrSurvey <- 1163       # in year NEP6 TVSURVEY   # 2019 (VT)                   # not applicable for spring advice (TB 2018) Reopened October (YV)
# NEP7_IntYrSurvey <- 6129      # in year NEP7 TVSURVEY    # 2019 (VT)            
# NEP8_IntYrSurvey <- 865      # in year NEP8 TVSURVEY     # 2019 (VT)          
# NEP_IntYrSurvey  <- list("NEP6" = NEP6_IntYrSurvey,                
#                          "NEP7" = NEP7_IntYrSurvey,                
#                          "NEP8" = NEP8_IntYrSurvey)     
#                          
# #NEP_IntYrSurvey  <- list()                              # not applicable for spring advice (TB 2018) Reopened October (YV)
# #reopenNEP  <-  names( NEP_IntYrSurvey )
# reopenNEP  <-  c("NEP6", "NEP7","NEP8")  # 2019 (VT) if no reeopning (Spring advice), set NA, otherwise set the stocks concerned # reopened October (YV)
# 
# 
# ## Individual area TACs (found in Advice)
# TAC.now <- c(
#   "COD4"=29437,"COD3A"=4205,"COD7D"=1715,                     # updated 2019 (VT)
#   "HAD4"=28950,"HAD3A"=1780,"HAD6"=3226,                      # updated 2019 (VT)
#   #"SOL-EC"=2515,                                              # updated 2019 (VT)
#   "PLE-EC"=PLEEC_TACratio*PLEEC_VIIDratio* 10354 ,            # updated 2019 (VT)
#   "POK4"=93614  ,"POK6"=9713 ,                                # updated 2019 (VT)
#   "PLE4"   = 125435, "PLE3A"  = 16782 ,                       # updated 2019 (VT)
#   "SOL-NS" = 12555 ,                                          # updated 2019 (VT)
#   "WHG-NS"=17191/WHGNS_TACratio,                              # updated 2019 (VT) human consumption catch
#   "NEP-NS"= 22103 ,                                           # updated 2019 (VT) found in report section 11.2 
#   "TUR" = 8122*TURprop                                        # added 2019 (VT)
# )    
# ## and the combined area TACs
# TAC.now<-c(TAC.now,
#   "COD-NS"=sum(TAC.now["COD4"],TAC.now["COD3A"],TAC.now["COD7D"]),
#   "HAD"=sum(TAC.now["HAD4"],TAC.now["HAD3A"],TAC.now["HAD6"],32),
#   "POK"=sum(TAC.now["POK4"],TAC.now["POK6"]), # Added 32 tonnes industrial bycatch to HAD TAC for 2019 (even if not really a TAC) VT
#   "PLE-NS"=sum(TAC.now["PLE4"],TAC.now["PLE3A"]))
# 
# TAC.now
# 
# # add others (e.g. data poor without advice... SPiCT) 
# 
# #*** NEEDS updating ***
# 
# TAC.now["ANF"] <- 31690
# TAC.now["DAB"] <- 64452*0.1
# TAC.now["BLL"] <- 3170
# TAC.now["LIN"] <- 22000 # made up value
# TAC.now["LEM"] <- 7874 * (1 - WITprop) ## Slight change from hard coded 5484
# TAC.now["WIT"] <- 7874 * (WITprop) ## Slight change from hard coded 5484
# 
# 
# Fmsy.nep <-c("NEP6"=0.0812,"NEP7"=0.075,"NEP8"=0.163,"NEP9"=0.118)   # updated 2018 (TB), no change in 2019 (VT)
# 
# nep.ass.ADV <- vector("list", length(Fmsy.nep))
# names(nep.ass.ADV) <- names(Fmsy.nep)
# for(i in seq(Fmsy.nep)){
#   stk <- names(Fmsy.nep)[i]
#   nep.ass.ADV[[stk]] <- c(computeStock(stocks[[stk]])[,ac(proj.yr-1)] * Fmsy.nep[stk])
# }
# nep.ass.ADV <- unlist(nep.ass.ADV)
# 
# no.nep.ass.ADV  <-c("NEP5"=1074,"NEP10"=46,"NEP32"=389,"NEP33"=898,"NEP34"=552,"NEPOTH-NS"=525)  # wanted catch   # only updated NEP33 in 2019, no new advice for other species (only wanted catches in advice for NEP33, needs check)
# no.nep.ass.ADVC <-c("NEP5"=1637,"NEP10"=48,"NEP32"=397,"NEP33"=898,"NEP34"=590,"NEPOTH-NS"=525)  # total catch                    
# no.nep.ass.ADV[is.element(names(no.nep.ass.ADV),LO)] <- no.nep.ass.ADVC[is.element(names(no.nep.ass.ADV),LO)]
# 
# 
# # PolicyPaper.nep <-c("NEP5"=1334,"NEP6"=660,"NEP7"=6820,"NEP8"=1773,"NEP9"=1008,"NEP10"=38,"NEP32"=464,"NEP33"=1119,"NEP34"=459,"NEPOTH-NS"=376) #,"NEP3A"=5318)  YV all changed except NEPOTH-NS
# # PolicyPaper.nepC <-c("NEP5"=1391,"NEP6"=742,"NEP7"=6843,"NEP8"=2062,"NEP9"=1060,"NEP10"=40,"NEP32"=492,"NEP33"=1257,"NEP34"=492,"NEPOTH-NS"=376) #,"NEP3A"=5318)  YV all changed except NEPOTH-NS
# # 
# # PolicyPaper.nep[is.element(names(PolicyPaper.nep),LO)] <- PolicyPaper.nepC[is.element(names(PolicyPaper.nep),LO)]
# # 
# # names(PolicyPaper.nep) <- nameChange(names(PolicyPaper.nep))
# # PolicyPaper.nep
# # sum(PolicyPaper.nep)
# 
# 
# 
# TAC.now <- c(TAC.now, nep.ass.ADV, no.nep.ass.ADV)


# rename TAC.now
names(TAC.now) <- nameChange(names(TAC.now), form = "new")



# 
# 
# 
# # 4. re-define SRs for STF (if required) ---------------------------------------------------
# # This basically replaces any existing SR fit with a simple geometric mean function,
# # followed by specified values assumed by the advice
# 
# is.STF <- FALSE
# 
# if(is.STF){
# flq1 <- FLQuant(1, dim = c(1,length(first.yr:last.yr)), 
#   dimnames = list(quant = 'all', year = first.yr:last.yr))
# 
# stk_name <- "COD_dash_NS"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# # SRs[[stk_name]]@params[,ac(proj.yrs),,] <- median(rec(stocks[[stk_name]])[,ac(1998:2016)])
# SRs[[stk_name]]@params[,proj.yrs,,] <- 692194 # values taken from the october forecast
# 
# stk_name <- "HAD"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# SRs[[stk_name]]@params[,proj.yrs,,] <- 4235840 # updated 2017 TB : from assessment model forcast
# 
# stk_name <- "PLE_dash_EC"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# SRs[[stk_name]]@params[,proj.yrs,,] <- gm_mean(rec(stocks[[stk_name]])[,ac(1980:2014)])
# 
# stk_name <- "PLE_dash_NS"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# SRs[[stk_name]]@params[,proj.yrs,,] <- gm_mean(rec(stocks[[stk_name]])[,ac(1957:2014)])
# SRs[[stk_name]]@params[,proj.yrs[-1],,] <- 1055007 #  updated 2017 TB :R2017 from RCT3 updated 2017 (TB)
# 
# stk_name <- "POK"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# # SRs[[stk_name]]@params[,ac(proj.yrs),,] <- median(rec(stocks[[stk_name]])[,ac(2003:2015)])
# SRs[[stk_name]]@params[,proj.yrs,,] <- 110334 # updated 2017 TB      Median recruitment re-sampled from the years 2003-2016
# 
# stk_name <- "SOL_dash_EC"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# SRs[[stk_name]]@params[,proj.yrs,,] <- gm_mean(rec(stocks[[stk_name]])[,ac(1982:2013)])
# 
# stk_name <- "SOL_dash_NS"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,hist.yrs]
# SRs[[stk_name]]@params[,proj.yrs,,] <- gm_mean(rec(stocks[[stk_name]])[,ac(1957:2013)])
# 
# stk_name <- "WHG_dash_NS"
# SRs[[stk_name]] <- FLSRsim(
#   rec = biols[[stk_name]]@n[1,], ssb = ssb(biols[[stk_name]]), 
#   uncertainty = flq1, proportion = flq1, model = 'geomean', name = stk_name)
# SRs[[stk_name]]@params[,hist.yrs,,] <- rec(stocks[[stk_name]])[,ac(hist.yrs)]
# SRs[[stk_name]]@params[,proj.yrs,,] <- gm_mean(rec(stocks[[stk_name]])[,ac(1990:2013)]) # GM (excl. 2014-2016)
# SRs[[stk_name]]@params[,proj.yrs[-1],,] <- 3339689 # RCT3 value updated for 2017 (TB)
# 
# }

# 5. Build advice ---------------------------------------------------------------

# TAC, TAE
SSA <- single.species.advice
SSA$stock <- nameChange(SSA$stock, form = "new")
general_proj.ave.years <- proj.yr-1
Ltmp <- vector("list", length(biols))
names(Ltmp) <- names(biols)
for(i in seq(biols)){
  
  # obj.name <- load(paste0("data/stocks/", names(biols)[i], ".RData"), verbose = TRUE)
  # tmp1 <- get(obj.name)
  stk <- names(biols)[i]
  
  tmp2 <- expand.grid(
    stock=stk, year=first.yr:last.yr, unit="unique", season="all",
    area="unique", iter=seq(ni),
    data=NA
  )

  catch <- 0 * (fleets[[1]]@metiers[[1]]@catches[[1]]@landings + fleets[[1]]@metiers[[1]]@catches[[1]]@discards)
  for(fl in seq(fleets)){
    for(met in seq(fleets[[fl]]@metiers)){
      if(stk %in% names(fleets[[fl]]@metiers[[met]]@catches)){
        # lan <- lan + c(yearMeans(fleets[[fl]]@metiers[[met]]@catches[[stk]]@landings[, ac(general_proj.ave.years)]))
        catch <- catch + (fleets[[fl]]@metiers[[met]]@catches[[stk]]@landings + fleets[[fl]]@metiers[[met]]@catches[[stk]]@discards)
      }
      print(paste("fleet", fl, "; metier", met, "; stk", i, "is done"))
    }
  }
  
  # if(stk %in% names(TAC.now)){
  #   catch[,proj.yrs] <- TAC.now[stk]
  # } else {
  #   catch[,proj.yrs] <- catch[,ac(proj.yr-1)]
  # }
  
  # input TAC for proj.yrs
  SSAsub <- subset(SSA, stock == stk)
  if(nrow(SSAsub)>0){
    for(y in proj.yrs){
      SSAsub.y <- subset(SSAsub, year == an(y))
      if(nrow(SSAsub.y)>0){
        if(!is.na(SSAsub.y$catch)){
          catch[,y] <- SSAsub.y$catch
        } else {
          catch[,y] <- catch[,ac(an(y)-1)]
        }
      } else {
        catch[,y] <- catch[,ac(an(y)-1)]
      }
    }
  } else {
    for(y in proj.yrs){
      catch[,y] <- catch[,ac(an(y)-1)]
    }
  }
  


  ### Assign TAC
  tmp3 <- tmp2
  tmp3$data <- c(catch)
  assign(paste0(names(biols)[i], "_advice.TAC.flq"), iter(as.FLQuant(tmp3), ni) )

  ### Assign TAE
  # tmp3 <- tmp2
  # tmp3$data <- eff
  # assign(paste0(names(biols)[i], "_advice.TAE.flq"), iter(as.FLQuant(tmp3), ni) )

  # ### Assign advice average years
  assign(paste0(names(biols)[i], "_advice.avg.yrs"), c(general_proj.ave.years))
  
  ### Add names of created objects
  Ltmp[[i]] <- paste0(stk, c("_advice.TAC.flq", "_advice.avg.yrs") )
  Ltmp[[i]] <- paste0(stk, c("_advice.TAC.flq") )
  # Ltmp[[i]] <- paste0(stk, c("_advice.avg.yrs") )

}


stks <- names(biols)    

advice   <- create.advice.data(
  yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr,"last.yr" = last.yr)),
  ns = ns,
  ni = ni,
  stks.data = Ltmp,
  fleets = fleets
)
# advice$TAE <- replace(advice$TAE, !is.na(advice$TAE), NA)


### loop to replace advice$quota.share NA with 0 (doesn't seem to matter)
# for(stk in seq(advice$quota.share)){
#   NAs <- which(is.na(advice$quota.share[[stk]]))
#   if(length(NAs)>0){
#     advice$quota.share[[stk]]@.Data[NAs] <- 0
#   }
# }


# 6. Build main.ctrl ------------------------------------------------------------

main.ctrl           <- list()
main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)


# 7. Build biols.ctrl -----------------------------------------------------

growth.model     <- seq(biols)*NA
for(i in seq(biols)){
  if(names(biols)[i] %in% ad_stocks){
    growth.model[i] <- "ASPG"
  }
  if(names(biols)[i] %in% aa_stocks){
    if(names(biols)[i] %in% spict_stocks){
      growth.model[i] <- "BDPG" 
    }
    if(names(biols)[i] %in% fixed_stocks){
      growth.model[i] <- "fixedPopulation" 
    }
  }
}

biols.ctrl       <- create.biols.ctrl(stksnames=names(biols), growth.model=growth.model)


# 8. Build fleets.ctrl ----------------------------------------------------

# 8.1. n.fls.stks ---------------------------------------------------------
# number of stocks fished by each fleet
n.fls.stks <- NaN*seq(fleets)
for(i in seq(fleets)){
  n.fls.stks[i] <- length(unique(unlist(lapply(fleets[[i]]@metiers, FUN = function(x){names(x@catches)}))))
}


# 8.2. fls.stksnames ------------------------------------------------------
# stocks fished by each fleet (in one long vector)
fls.stksnames <- vector(mode = "list", length(fleets))
for(i in seq(fleets)){
  stksnames <- c()
  for(j in seq(fleets[[i]]@metiers)){
    stksnames <- c(stksnames, names(fleets[[i]]@metiers[[j]]@catches))
  }
  fls.stksnames[[i]] <- unique(stksnames)
}
fls.stksnames <- do.call("c", fls.stksnames)

# compare
sum(n.fls.stks); length(fls.stksnames)
sum(do.call("c", lapply(fleets, FUN = function(x){length(x@metiers)})))


# 8.3. effort.models ------------------------------------------------------

effort.models    <- rep('SMFB', length(fleets)) # SMFB: Simple Mixed Fisheries Behavior
# effort.models    <- rep('fixedEffort', length(fleets)) # fixedEffort: Fixed Effort model


# 8.4. catch.models -------------------------------------------------------
catch.models <- ifelse(fls.stksnames %in% ad_stocks, 'CobbDouglasAge', 'CobbDouglasBio')


# 8.5. capital.models -----------------------------------------------------
capital.models   <- rep('fixedCapital', length(fleets))


# 8.6. price.models -------------------------------------------------------
price.models     <- NULL                            


Ltmp <- vector("list", length(biols))
names(Ltmp) <- names(biols)
stks <- names(biols)
for(i in seq(biols)){
  Ltmp[[i]] <- paste0(stks[i], ".unit")
  DIMS <- dims(biols[[i]])
  assign(paste0(stks[i], ".unit"), DIMS$unit)
}


# 8.7. create ------------------------------------------------------------------

flq <- FLBEIA:::create.list.stks.flq(
  stks = stks,
  yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr,"last.yr" = last.yr)),
  ni = ni,
  ns = ns,
  list.stks.unit = Ltmp
)[[1]][,,1,]

# flq              <- create.list.stks.flq()[[1]][,,1,]

fls <- names(fleets)
stknames <- names(biols)
fleets.ctrl      <- create.fleets.ctrl(
  fls=fls, n.fls.stks=n.fls.stks, fls.stksnames=fls.stksnames,
  effort.models=effort.models, catch.models=catch.models,
  capital.models=capital.models, price.models=price.models, flq=flq
)


# 8.8. restrictions -----------------------------------------
current_stocks.restr <- c(ad_stocks, c("NEP6", "NEP7", "NEP8", "NEP9"))

for(fl in names(fleets)){
  fleets.ctrl[[fl]]$restriction <- "catch"
  fleets.ctrl[[fl]]$effort.restr <- "min"
  # tmp <- vector("list", length(fleets[[fl]]@metiers))
  fleets.ctrl[[fl]]$stocks.restr <- unique(do.call("c", lapply(fleets[[fl]]@metiers, 
    FUN = function(x){names(x@catches)})))
  fleets.ctrl[[fl]]$stocks.restr <- fleets.ctrl[[fl]]$stocks.restr[
    fleets.ctrl[[fl]]$stocks.restr %in% current_stocks.restr]
}




# 9. Build advice.ctrl ----------------------------------------------------------


# 9.1. HCR.models ---------------------------------------------------------

HCR.models    <- ifelse(names(biols) %in% fixed_stocks, 'fixedAdvice', 'IcesHCR')


# 9.2. create --------------------------------------------------------------

advice.ctrl <- create.advice.ctrl(
  stksnames = names(biols), 
  HCR.models = HCR.models, 
  first.yr = first.yr, 
  last.yr = last.yr, 
  iter = ni
)


# 9.3. adjust -------------------------------------------------------------

for(i in seq(biols)){
  advice.ctrl[[names(biols[i])]]$nyears <- 3
  advice.ctrl[[names(biols[i])]]$wts.nyears <- 1
  advice.ctrl[[names(biols[i])]]$fbar.nyears <- 1
  AdvCatch <- as.logical(seq(first.yr:last.yr)*TRUE)
  names(AdvCatch) <- first.yr:last.yr
  advice.ctrl[[names(biols[i])]]$AdvCatch <- AdvCatch # TAC is given in terms of catch, if TRUE, or landings, if FALSE
  if(names(biols)[i] %in% ad_stocks & 
      !is.null(advice.ctrl[[names(biols[i])]]$ref.pts)){
    advice.ctrl[[names(biols[i])]]$ref.pts["Blim",] <- 0 #subset(BRPs, stock_name == names(biols[i]))$Blim
    advice.ctrl[[names(biols[i])]]$ref.pts["Btrigger",] <- subset(BRPs, stock_name == names(biols[i]))$Btrigger
    advice.ctrl[[names(biols[i])]]$ref.pts["Fmsy",] <- subset(BRPs, stock_name == names(biols[i]))$Fmsy
    advice.ctrl[[names(biols[i])]]$intermediate.year <- "Fsq"
  }
  if(names(biols)[i] %in% spict_stocks & 
      !is.null(advice.ctrl[[names(biols[i])]]$ref.pts)){
    advice.ctrl[[names(biols[i])]]$ref.pts["Blim",] <- 0 #subset(BRPs, stock_name == names(biols[i]))$Blim
    advice.ctrl[[names(biols[i])]]$ref.pts["Btrigger",] <- subset(BRPs, stock_name == names(biols[i]))$Bpa
    advice.ctrl[[names(biols[i])]]$ref.pts["Fmsy",] <- subset(BRPs, stock_name == names(biols[i]))$Fmsy
 }
}

# advice.ctrl$ANF
# advice.ctrl$BLL
# advice.ctrl$DAB
advice.ctrl$COD_dash_NS
# advice.ctrl$SOL_dash_EC



# 10. Build assess.ctrl ---------------------------------------------------

assess.models    <- rep('NoAssessment',length(biols))    
assess.ctrl      <- create.assess.ctrl(stksnames = names(biols), assess.models = assess.models)



# 11. Build obs.ctrl ------------------------------------------------------

stkObs.models    <- rep('perfectObs', length(biols)) # use NoObsStock?
obs.ctrl         <- create.obs.ctrl(stksnames = names(biols),  stkObs.models = stkObs.models)



# 12. Build covars.ctrl ---------------------------------------------------

covars.ctrl      <- NULL


# 13. Build BDs -----------------------------------------------------------

incl <- which(names(biols) %in% spict_stocks)
stks.data <- vector("list", length(incl))
names(stks.data) <- names(biols)[incl]
stks <- names(biols)[incl]
for(i in seq(stks)){
    stk <- stks[i]
    DIMS <- dims(biols[[stk]])
    RANGE <- range(biols[[stk]])
    
    stks.data[[stk]] <- paste0(
      stk,
      c(
        ".unit",
        ".age.min",
        ".age.max",
        "_bd.model",
        "_params.name",
        "_params.array",
        "_biomass.flq",
        "_catch.flq",
        "_range.plusgroup",
        "_range.minyear",
        "_alpha"
      )
    )

    fit <- spicts[[stk]]
    tab1 <- sumspict.parest(fit)
    tab5 <- sumspict.predictions(fit)
    varcov <- fit$cov.fixed
    colnames(varcov) # old?: c("logm", "logK", "logq", "logn", "logsdb", "logsdf", "logsdi", "logsdc")
    coorr  <- cov2cor(varcov)
    pars <- fit$par.fixed
    
    4*exp(pars["logm"])/exp(pars["logK"])

    4*(pars["logm"])/(pars["logK"])
    get.par("logr", fit, exp=TRUE)
    get.par("r", fit)
    get.par("logK", fit, exp=TRUE)
    tab1
    RandPar_SPict_log <- as.data.frame(mvrnorm(10, pars, varcov))
    if(!"logn" %in% colnames(RandPar_SPict_log)){RandPar_SPict_log$logn <- get.par("logn", fit)[2]}
    RandPar_SPict <- exp(RandPar_SPict_log)

    RandPar_flbeia <- matrix(NA, 10,3, dimnames = list(iter = 1:10, c('r', 'K', 'p')))
    RandPar_flbeia[,1] <- (RandPar_SPict[,'logm']*RandPar_SPict[,'logn']^(RandPar_SPict[,'logn']/(RandPar_SPict[,'logn']-1)))/RandPar_SPict[,'logK']
    RandPar_flbeia[,2] <- RandPar_SPict[,'logK']
    RandPar_flbeia[,3] <- RandPar_SPict[,'logn'] - 1

    r.stk <- (get.par("logm", fit, exp=T)[2]*get.par("logn", fit, exp=T)[2]^(get.par("logn", fit, exp=T)[2]/(get.par("logn", fit, exp=T)[2]-1)))/get.par("logK", fit, exp=T)[2] 
    K.stk <- get.par("logK", fit, exp=T)[2]
    p.stk <- get.par("logn", fit, exp=T)[2] - 1
    
    assign(paste0(stk, ".unit"), DIMS$unit)
    assign(paste0(stk, ".age.min"), RANGE["min"])
    assign(paste0(stk, ".age.max"), RANGE["max"])
    assign(paste0(stk, "_bd.model"), "PellaTom")
    assign(paste0(stk, "_params.name"), c("K", "p", "r"))
    par.array.tmp <- array(c(K.stk, p.stk, r.stk), dim = c(3, length(first.yr:(last.yr)),ns,ni), dimnames = list(param = c("K", "p", "r"), year = first.yr:last.yr, season=1, iter=1) )

    assign(paste0(stk, "_params.array"), 
      par.array.tmp
    )
    
    assign(paste0(stk, "_alpha"),
      # get.par("logalpha", fit, exp=T)[2] 
      array( (par.array.tmp['p',,,]/par.array.tmp['r',,,]+1)^(1/par.array.tmp['p',,,]) ) 
    )
    
    
    
    assign(paste0(stk, "_biomass.flq"),
     computeStock( biols[[stk]])[,ac(first.yr:(proj.yr-1))]
    )

    assign(paste0(stk, "_catch.flq"),
     window(catch(stocks[[stk]]), start=first.yr, end=last.yr)[,ac(first.yr:(proj.yr-1))]
    )    

    assign(paste0(stk, "_params.name"), c("K", "p", "r"))

    assign(paste0(stk, "_range.plusgroup"), RANGE["plusgroup"] )
    assign(paste0(stk, "_range.minyear"), RANGE["minyear"] )
    
    
    stks.data[[stk]] <- c(
      stks.data[[stk]],
      paste0(stk, "_gB.flq")
    )
        
    B.tmp <- computeStock(biols[[stk]])
    B.tmp[,ac(proj.yr)] <- tab5[1,1] # biomass projection year
    C.tmp <- catch(window(stocks[[stk]], start=first.yr, end=last.yr))
    C.tmp[,ac(proj.yr)] <- tab5[5,1]
    tmp <- 
      B.tmp[,ac((first.yr+1):(proj.yr))] - 
      B.tmp[,ac((first.yr):(proj.yr-1))] + 
      C.tmp[,ac((first.yr):(proj.yr-1))]
    
    dimnames(tmp)$age <- 1
    dimnames(tmp)$year <- hist.yrs
    # tmp

    assign(paste0(stks[i], "_gB.flq"),
      tmp
    )
}

BDs <- create.BDs.data(
  yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr,"last.yr" = last.yr)),
  ns = ns,
  ni = ni,
  stks.data = stks.data
)


# 14. Build covars --------------------------------------------------------

covars <- NULL

# 15. Build indices -------------------------------------------------------

indices <- NULL


# 16. Save objects --------------------------------------

save(
  stocks, biols, spicts, 
  SRs, BDs, fleets, covars, BRPs,
  indices, advice, 
  main.ctrl, 
  biols.ctrl, fleets.ctrl, 
  covars.ctrl, obs.ctrl, 
  assess.ctrl, advice.ctrl,
  file = file.path("model", "03_input_objects.Rdata")
)

