#######################################################################################
##
##
##  main code to run the optimizer
##  F to optimize: Ranges of MSY values
##
########################################################################################



rm(list=ls())
gc(reset=T)

### define FCube path and scripts
#functionPath <- "F:\\D\\Expertise\\WGMIXFISH\\2016\\WGMIXFISH-METH\\optim\\prgr\\"
#source("North_Sea/optim/prgr/02_Projection TAC year_function_v1_2016.r")




### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning

gc(); graphics.off()

#ver1 <- "03_Fcube_Projection"
#ver.datetime   <- "12/10/2016";
#ver1 <- "03_Fcube_SQIntYr_CMT"
# ver.datetime   <- "29/10/2019";

options(stringsAsFactors = FALSE, scipen=100)



#source("North_Sea/optim/prgr/02_Projection TAC year_function_v1_2016.r")

# define output directories
resPath <- "output/"
plot.path <- "output/04_Model/plots/"
mkdir(plot.path)

results.path <- "output/04_Model/results/"
mkdir(results.path)


##-----------------------------------------------------------------------------
##  GET INPUTS
##-----------------------------------------------------------------------------
res.path <- "model"

load("model/model_settings.Rdata")
load(paste0(res.path,"/01_Reproduce_The_Advice_2020_keepNYrNow_LO.Rdata"))
load(paste0(res.path,"/02_Conditioning_Fleets_LO.Rdata"))
load("data/FIDES_out/Fides_output.RData")
##-----------------------------------------------------------------------------
##  GET INPUTS
##-----------------------------------------------------------------------------





source('bootstrap/software/functions/remove_validity_FLFleet.R') # to reduce computing time      
source('bootstrap/software/functions/FLFcube_FLCore_withFIDES.R')
source('model_04_00_Functions.r')
source('bootstrap/software/functions/rbgaAmoi.r')                                      
source('bootstrap/software/functions/funcs.R')  


if (length(LO)>0) Run.nameFC <- paste0(ver1,"_LO") else Run.nameFC <- ver1 

#dir.create(file.path("North_Sea/reopening2016/plots", Run.nameFC))
#dir.create(file.path("North_Sea/plots", Run.nameFC))
#plot.path <- file.path("North_Sea/plots",Run.nameFC) 

#unallocated_cod <- FALSE


# results dataframe

#res.effort <- data.frame()

# output historical effort 
#ef.mt <- slot.fleet(fleets,"effshare")
#ef.mt <- ef.mt[ef.mt$year==yr.assess,]
#ef.fl <- data.frame(scenario="baseline",year=yr.assess,fleet=fleet.names,effort=sapply(fleets,function(x) effort(x)[,yr.assess]))
#ef.mt <- merge(ef.mt,ef.fl)
#ef.mt$effort <- as.numeric(ef.mt$effort)
#ef.mt$efmet <- ef.mt$effort*ef.mt$effshare
#res.effort.met <- ef.mt[, c("year","scenario","fleet","metier","effshare","effort","efmet")]


#output historic relative stability
#rs <- slot.fleet(fleets,"landings")
#rs <- rs[rs$year==yr.assess,]
#rs$country <- substr(rs$fleet,1,2)
#names(rs) <- gsub("qname","stock",names(rs))
#rs1 <- aggregate(rs$landings,list(stock=rs$stock,country=rs$country),sum,na.rm=T)
#names(rs1)[which(names(rs1)=="x")] <- "landings"
#rs2 <- aggregate(rs1$landings,list(stock=rs1$stock),FUN="sum",na.rm=T)
#names(rs2)[which(names(rs2)=="x")] <- "land.tot"
#rs <- merge(rs1,rs2)
#rs$share <- rs$landings / rs$land.tot
#rs$year <- as.numeric(yr.assess)
#rs$scenario <- "baseline"
#rs <- rs[,c("year","stock","country","scenario","share")]
#rm(rs1,rs2)


# a new data.frame for storing all catch info, distinguishing between legal landings (<=TAC),
# over TAC landings, unallocated and discards (from histrorical discards ratio)

#TACnow <- TACplusuplift[sort(names(TACplusuplift))]
TACnow <- TAC.now[sort(names(TAC.now))]
TACnow <- TACnow[!duplicated(TACnow)]

# All.landings <-c(sapply(dem.st.fwd,function(x) landings(x)[,yr.now]),sapply(nep.st.fwd,function(x) landings(x)[,yr.now])) 
# All.landings <-All.landings[sort(names(All.landings))]
# TACnow <- TACnow[names(All.landings)]
# Legal.landings <-pmin(All.landings,TACnow)
# OverTAC.landings <- All.landings-TACnow
# OverTAC.landings[OverTAC.landings<0] <- 0 
# 
# disc <- c(sapply(dem.st.fwd,function(x) discards(x)[,yr.now]),sapply(nep.st.fwd,function(x) discards(x)[,yr.now]))
# disc <- disc[sort(names(disc))]
# 
# tonnes <- round(c(TACnow,Legal.landings,disc,OverTAC.landings),2)
# allcatch.dataframe <- data.frame(year=yr.now,sc="baseline",stock=rep(st.names,4),
#                                  catch=rep(c("TAC","legal landings","discards","overTAC landings"),each=length(st.names)),
#                                  tonnes =as.numeric(tonnes))




F3ProjectionYear <- as.numeric(yr.TAC) #number of projection years


#! choice of scenarios
# 
# scenarios <- c("max","min","sq_E","val","cod-ns")
# Fc.silent <- FALSE #(if true, not all intermediate results are given)

# effort.stock <- list()
# tot.nep.f3.landings <- matrix(nrow=length(nep.lst),ncol=length(now:F3ProjectionYear),dimnames=list(nep.names,now:F3ProjectionYear))


# Management Plans for yrTAC   <-This must be as in reproduce the advice, but only for the TAC year! 

# ctrl.MP.yrTAC <- lapply(ctrl.STF,function(x) fwdControl(x@target[-1,]))



##-----------------------------------------------------------------------------
## RUNNING SCENARIOS
##-----------------------------------------------------------------------------
# NepYes <- F
# if (NepYes ==F) f3.ctrl@target.F <- FLPar(Ftarg.dem.now,params=dem.names) else
#   f3.ctrl@target.F <- FLPar(c(Ftarg.dem.now,Ftarg.nep.now),params=c(dem.names,nep.names))







# source("model_04_03_rbgaAmoi.r")
#an<-function(x) {as.numeric(x)}

# 
#   BRPs<-matrix(NA,nrow=length(dem.names),ncol=9,dimnames=list(dem.names,
#                                                               c("Flim","Fpa","Fmsy", "FmsyL","FmsyH","Fmgt","Blim","Bpa", "Btrigger"))) #yv add , Fmgt 
#   BRPs["COD-NS",] <- c(Flim=0.54  ,  Fpa=0.39 ,  Fmsy=0.31  ,FmsyL=0.2  ,FmsyH=0.4,  Fmgt=NA    , Blim=107000   , Bpa=150000, Btrigger=150000)
#   BRPs["HAD",]    <- c(Flim=0.384 ,  Fpa=0.274,  Fmsy=0.19  ,FmsyL=0.167    ,FmsyH=0.194,    Fmgt=NA    , Blim=94000    , Bpa=132000, Btrigger=NA)   # updated 2017 TB
#   BRPs["PLE-EC",] <- c(Flim=0.5   ,  Fpa=0.36 ,  Fmsy=0.25  ,FmsyL=0.175  ,FmsyH=0.34,  Fmgt=NA    , Blim=18448    , Bpa=25826, Btrigger=NA)    # updated 2017 TB
#   BRPs["PLE-NS",] <- c(Flim=0.516 ,  Fpa=0.369,  Fmsy=0.21  ,FmsyL=0.146  ,FmsyH=0.30,  Fmgt=0.30  , Blim=207288   , Bpa=290203, Btrigger=NA)   # updated 2017 TB
#   BRPs["POK",]    <- c(Flim=0.564 ,  Fpa=0.403,  Fmsy=0.358 ,FmsyL=0.21  ,FmsyH=0.49,  Fmgt=NA    , Blim=107000   , Bpa=150000, Btrigger=NA)   # updated 2017 TB
#   BRPs["SOL-EC",] <- c(Flim=0.359  ,  Fpa=0.256  ,  Fmsy=0.256   ,FmsyL=0.195  ,FmsyH=0.256,  Fmgt=NA    , Blim=13751       , Bpa=19251, Btrigger=19251)
#   BRPs["SOL-NS",] <- c(Flim=0.63  ,  Fpa=0.44 ,  Fmsy=0.2   ,FmsyL=0.113  ,FmsyH=0.37,  Fmgt=0.2   , Blim=26300    , Bpa=37000, Btrigger=NA)
#   BRPs["WHG-NS",] <- c(Flim=0.39  ,  Fpa=0.28 ,  Fmsy=0.172  ,FmsyL=0.158    ,FmsyH=0.172,    Fmgt=NA    , Blim=172741   , Bpa=241837 , Btrigger=NA)
#   #BRPs["TUR",]    <- c(Flim=NA    ,  Fpa=NA   ,  Fmsy=0.27  ,Fmgt=NA    , Blim=2070     , Bpa=2900) #YV
  

  
  
  

# BRPs <- BRPs[-which(rownames(BRPs)=="WHG-NS"),] 
dem.st.fwd <- dem.st.fwd[rownames(BRPs)]  
  
  
### defining MSY ranges for all species
#### stock order: COD-NS, HAD-NS, PLE-NS, POK, SOL-NS, WHG-NS, TUR
FMSY_min <- data.frame(BRPs)$FmsyL#*MSY_AR
FMSY_max <- data.frame(BRPs)$FmsyH#*MSY_AR
FMSY     <- data.frame(BRPs)$Fmsy#*MSY_AR

#FMSY_min <- c(.2, .2,.2,.2,.2,.2,.2)
#FMSY_max <- c(.5, .5,.5,.5,.5,.5,.5)


### fix for Sole EC where SSB < SSB Btrigger

# FMSY_min[rownames(BRPs)=="SOL-EC"] <-FMSY_min[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]
# FMSY_max[rownames(BRPs)=="SOL-EC"] <-FMSY[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]
# FMSY[rownames(BRPs)=="SOL-EC"] <-FMSY[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]


### fix for Sole COD-NS where SSB < SSB Btrigger

  FMSY_min[rownames(BRPs)=="COD-NS"] <-FMSY_min[rownames(BRPs)=="COD-NS"] * as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,"2021",,,])/BRPs[rownames(BRPs)=="COD-NS","Btrigger"]
  FMSY_max[rownames(BRPs)=="COD-NS"] <-FMSY[rownames(BRPs)=="COD-NS"] * as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,"2021",,,])/BRPs[rownames(BRPs)=="COD-NS","Btrigger"]
  FMSY[rownames(BRPs)=="COD-NS"] <-FMSY[rownames(BRPs)=="COD-NS"] * as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,"2021",,,])/BRPs[rownames(BRPs)=="COD-NS","Btrigger"]

#FMSY_min[rownames(BRPs)=="COD-NS"] <-.17
#FMSY_max[rownames(BRPs)=="COD-NS"] <-.17
#FMSY[rownames(BRPs)=="COD-NS"] <-.17

nStocks <- length(FMSY_min)   ##
lengthPop <- 15 #30  ## length of the first population to be used in LHS to optimize it
Iters <- 30 #15 ## number of iterations


run_the_optim<-T


### without Had
#dem.stock <- subset(dem.stock, names(dem.stock)!="HAD")

#source("North_Sea/optim/prgr/02_Projection TAC year_function_v1_2016.r")

if (run_the_optim)
  {
  init <- (optimumLHS(k=nStocks, n=lengthPop))
  for (i in 1:nStocks){
    init[,i] <- init[,i] * (FMSY_max[i]-FMSY_min[i]) + FMSY_min[i] ## optimumLHS sample in a uniform distribution, transforme it to be bounded by MSY ranges
  }
  # hist(init[,1])

  Fc.silent <- FALSE #(if true, not all intermediate results are given)

  monitor <- function(obj) {
       # plot the population
       xlim = c(min(FMSY_min), max(FMSY_max));
       ylim = c(min(FMSY_min), max(FMSY_max));
       plot(obj$population, xlim=xlim, ylim=ylim, xlab="pi", ylab="sqrt(50)");
  }

  deb <- date()	 
  
  # call GA
  rbga.res = rbgaAmoi(stringMin=FMSY_min, 
	  stringMax=FMSY_max,
	  suggestions=init,
	  popSize=lengthPop, iters=Iters,
	  mutationChance=NA,
	  elitism=NA,
	  monitorFunc=NULL, evalFunc=FcubeDiffLandings2Scenarios,
	  showSettings=FALSE, verbose=TRUE)
  fin <- date()

  colnames(rbga.res$suggestions) <- colnames(rbga.res$population) <- dem.names
  

  dif<-"diff2max-min2" 
  save(rbga.res, file=file.path("model",paste0("04_Optim_",ver1,dif,".Rdata",sep="")))
}



