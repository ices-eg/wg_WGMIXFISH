        ###################################################################################
        ##     FLFCUBE SCRIPT FOR RUNNING FCUBE RUNS ON VARIOUS TAC SCENARIOS            ##
        ##                                                                               ##
        ##                         North Sea                                             ##
        ##                                                                               ##
        ##    Progr.1 - REPRODUCE THE ADVICE FROM SINGLE-STOCK SETTINGS                  ##
        ##                                                                               ##
        ##                                                                               ##
        ## Initial Author : Clara Ulrich, DTU Aqua <clu@aqua.dtu.dk>                     ##
        ## Further Contributors: Youen Vermard, Paul J. Dolder,...                       ##
        ## And ICES WGMIXFISH colleagues                                                 ##
        ##                                                                               ##
        ## update for 2018 WGMIXFISH (May meeting) : Thomas Brunel                       ##
        ##                                                                               ##
        ## R version 3.4.2 (2017-09-28)                                                  ##
        ## FLFleet_2.6.0   FLAssess_2.6.1 FLash_2.5.8     FLCore_2.6.6                   ##
        ##                                                                               ##
        ###################################################################################



##-----------------------------------------------------------------------------
##  GETTING STARTED
##-----------------------------------------------------------------------------

# library(icesTAF)
# library(ggplot2)
# library(lattice)
# library(pander)
# library(data.table)
# library(FLCore)
# library(FLFleet)
# library(FLash)
# # library(FLAssess)
# library(grid)


rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
gc(); graphics.off()
ver <- "01_Reproduce_The_Advice_2020_keepNYrNow"
# ver.datetime   <- "23/10/19"
options(stringsAsFactors = FALSE, scipen=100)



#stock.path  <- "data/NS_FLStocks_01_Reproduce_The_Advice_2020"
stock.path  <- "data/FLStocks_out"
input.path  <- "bootstrap/data/Reproduce_The_Advice"              ###
model.path  <- "model"                       ###  model storage

# plot.path   <- "output"                      ###


#results folders
res.path  <- "output/01_Reproduce_The_Advice/results"
plot.path <- "output/01_Reproduce_The_Advice/plots"


source('bootstrap/software/functions/FLFcube_FLCore_R31.R')
source('bootstrap/software/functions/fwdF3 function.r')       ### replacement FLR fwd()  that does not overwrite survivors in the stf objects if any (eg for SAM stocks)
source('bootstrap/software/functions/funcs.R')
source('bootstrap/software/functions/remove_validity_FLFleet.R') # to reduce computing time


## Set to TRUE if you want to produce the diagnostics plots
## Must have the single species advice in a file to compare against
plotdiag<-TRUE
if (plotdiag==TRUE) print("warning: you must have a file called 'single_species_advice.csv' in your input folder for this to work")




##-----------------------------------------------------------------------------
## GET INPUTS
##-----------------------------------------------------------------------------
t1<-Sys.time()

load("model/model_settings.Rdata")

wg.path <-  paste(stock.path)

stock.names <- unlist(strsplit(list.files(wg.path, pattern=".RData$"), "\\."))
stock.names <- stock.names[stock.names !="RData"]
stock.names <- stock.names[!stock.names %in% c("RData", "SOL-EC")]  ## sol7D not cat1 in 2019 same in 2020

 dem.names <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK",  "SOL-NS", "WHG-NS", "TUR", "WIT")  #YV changed 2019 Sol 7D not cat1 in 2019
#dem.names <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-NS", "WHG-NS", "TUR")

nep.names <- stock.names[stock.names %in% grep("NEP",stock.names,value=T)]

if (is.element("NEP",LO)) LO <- c(LO,nep.names)
st.lst  <- as.list(stock.names) ; names(st.lst)  <- stock.names
dem.lst <- as.list(dem.names)   ; names(dem.lst) <- dem.names; n.dem <-length(dem.names)
nep.lst <- as.list(nep.names)   ; names(nep.lst) <- nep.names; n.nep <-length(nep.names)


# reading in stock data
wg.stock <- FLStocks(lapply(st.lst, function(x) {
        load(paste(wg.path,"/",x,".RData",sep=""))
        res<-get("stock")
        name(res) <- x
        units(harvest(res))<-"f"
        res}))

nep.stock <- FLStocks(lapply(nep.lst,function(x) wg.stock[[x]]))


# adjust stock objects  so that all end in the last catch data year,
# but keep current year estimates for stocks that have in year estimates
#
# reduce range for whg because intermediate year was included (VT)
whgNYrNow <- stock.n(wg.stock[["WHG-NS"]])[,yr.now]
wg.stock[["WHG-NS"]]<-window(wg.stock$`WHG-NS`,end=an(yr.assess))


# save maturity for cod and then remove 2019
codMatYrNow       <- mat(wg.stock[["COD-NS"]])[, yr.now]
codMat3YrMean   <- apply(mat(wg.stock[["COD-NS"]])[, ac((an(yr.now)-3):(an(yr.now)))],1,mean)   # updated 2019 (VT) : Maturity for the TAC year onwards is the average of final four years of assessment data (not average between last 3)
codNYrNow<-stock.n(wg.stock[["COD-NS"]])[, yr.now]
wg.stock[["COD-NS"]]<-window(wg.stock$`COD-NS`,end=an(yr.assess))
### YV fix for plus group not defined
range(wg.stock$`COD-NS`)[3] <- 6





# BRP
BRPs<-matrix(NA,nrow=length(dem.names),ncol=9,
             dimnames=list(dem.names,c("Flim","Fpa","Fmsy", "FmsyL","FmsyH", "Fmgt","Blim","Bpa", "Btrigger")))
BRPs["COD-NS",] <- c(Flim=0.54 ,  Fpa=0.39 ,  Fmsy=0.31 ,  FmsyL=0.198,  FmsyH=0.46 ,  Fmgt=NA,  Blim=107000 ,  Bpa=150000, Btrigger=150000)  # OK      for 2020
BRPs["HAD",]    <- c(Flim=0.384,  Fpa=0.274,  Fmsy=0.194,  FmsyL=0.167,  FmsyH=0.194,  Fmgt=NA,  Blim=94000  ,  Bpa=132000, Btrigger=132000) # OK       for 2020
BRPs["PLE-EC",] <- c(Flim=0.5  ,  Fpa=0.36 ,  Fmsy=0.25 ,  FmsyL=0.175,  FmsyH=0.344,  Fmgt=NA,  Blim=18447  ,  Bpa=25826 , Btrigger=25826 )  # OK      for 2020
BRPs["PLE-NS",] <- c(Flim=0.516,  Fpa=0.369,  Fmsy=0.21 ,  FmsyL=0.146,  FmsyH=0.30 ,  Fmgt=NA,  Blim=207288 ,  Bpa=290203, Btrigger=564599) # OK       for 2020
BRPs["POK",]    <- c(Flim=0.620,  Fpa=0.446,  Fmsy=0.363,  FmsyL=0.21 ,  FmsyH=0.536,  Fmgt=NA,  Blim=107297 ,  Bpa=149098, Btrigger=149098)  # OK      for 2020
#BRPs["SOL-EC",] <- c(Flim=0.359,  Fpa=0.256, Fmsy=0.256,  FmsyL=0.195,  FmsyH=0.322,  Fmgt=NA,  Blim=13751  ,  Bpa=19251 , Btrigger=19251 ) # need check
BRPs["SOL-NS",] <- c(Flim=0.42 ,  Fpa=0.302 , Fmsy=0.207,  FmsyL=0.123,  FmsyH=0.341,  Fmgt=NA,  Blim=30828  ,  Bpa=42838 , Btrigger=42838 ) # OK      for 2020
BRPs["WHG-NS",] <- c(Flim=0.46 ,  Fpa=0.330,  Fmsy=0.172,  FmsyL=0.158,  FmsyH=0.172,  Fmgt=NA,  Blim=119970 ,  Bpa=166708, Btrigger=166708) # OK      for 2020
BRPs["TUR",]    <- c(Flim=0.61 ,  Fpa=0.47 ,  Fmsy=0.36 ,  FmsyL=0.25 ,  FmsyH=0.48 ,  Fmgt=NA,  Blim=2974   ,  Bpa=4163  , Btrigger=6353  ) # OK      for 2020
BRPs["WIT",] <- c(Flim=0.30 ,  Fpa=0.20 ,  Fmsy=0.154,  FmsyL=0.108 ,  FmsyH=0.21 ,  Fmgt=NA,  Blim=3069   ,  Bpa=4745  , Btrigger=4745  ) # added in 2020 (TB)

# specify the years
range<- t(sapply(wg.stock, function (x) x@range))
#now <- max(range[,"maxyear"])+1



##-----------------------------------------------------------------------------
## SINGLE STOCK FORECAST CONDITIONNING: TACs, RECR, HCR etc
#
##-----------------------------------------------------------------------------
if (length(LO)>0) Run.name <- paste0(ver,"_LO") else Run.name <- ver


### 1) TACs

## TAC ratios when total catch is not exactly for species and subarea 4

WHGNS_TACratio  <- 0.777   # updated 2020 : ratio from TAC in area 4 to the total catch    #  updated 2019 (VT)  "The human consumption fishery (HCF) catch split between Subarea 4 and Division 7.d in 2019 and 2020 is the same as the proportion of HCF catch between the areas in 2018: 77.7% from Subarea 4 and 22.3% from Division 7.d."
PLEEC_TACratio  <- 0.732   # updated 2020   # ratio from TAC in area 7d to the total catch   # updated 2019 (VT), calculated myself so needs checking (1-mean(7e landings 2003-2018/TAC 7d.e. 2003-2018)), value was 0.736 in 2018 : "Division 7.d proportion of the TAC assuming the same proportion of the TAC is taken from Division 7.e as during 2003–2018"
PLEEC_VIIDratio <- 1-0.1563 # updated 2020 update  # ratio resident stock                             #  updated 2019 (VT) :  "All plaice in Division 7.d, including plaice originating from the North Sea and the western English Channel, according to a ratio calculated over the years 2003–2018: 15.1% of the plaice landed in Division 7.d is assumed to originate from the North Sea and the western English Channel, and this is added to the predicted values for the Division 7.d plaice stock."
TURprop <- 0.712           # updated 2020 (TB)   # calculated myself, ratio official landings TUR 4 in official landings TUR and Brill in 4 and 2a for last 3 years (2017-2019)
WITprop <- 0.470           # added 2020 (TB), based on last 3 years ratio


NEP6_IntYrSurvey <- 1102       # in year NEP6 TVSURVEY   # 2020 (KS)                   # not applicable for spring advice (TB 2018) Reopened October (YV)
NEP7_IntYrSurvey <- 4589       # in year NEP7 TVSURVEY   # 2020 (KS)
NEP8_IntYrSurvey <- 1119       # in year NEP8 TVSURVEY   # 2020 (KS)
NEP_IntYrSurvey  <- list("NEP6" = NEP6_IntYrSurvey,
                         "NEP7" = NEP7_IntYrSurvey,
                         "NEP8" = NEP8_IntYrSurvey)

#NEP_IntYrSurvey  <- list()                              # not applicable for spring advice (TB 2018) Reopened October (YV)
#reopenNEP  <-  names( NEP_IntYrSurvey )
reopenNEP  <-  c("NEP6", "NEP7","NEP8")  # 2019 (VT) if no reeopning (Spring advice), set NA, otherwise set the stocks concerned # reopened October (YV)




## Individual area TACs (found in Advice )
TAC.now <- c(
  "COD4"=14718,"COD3A"=2103,"COD7D"=858,                      # updated 2020 (TB)
  "HAD4"=35653,"HAD3A"=2193,"HAD6"=3973,                      # updated 2020 (TB)
  #"SOL-EC"=2515,                                             # updated 2019 (VT)
  "PLE-EC"=PLEEC_TACratio*PLEEC_VIIDratio* 9154 ,             # updated 2020 (TB)         # TAC 7de * prop of this TAC in 7d * prop of 7d stock in 7d TAC
  "POK4"=79813  ,"POK6"=8280 ,                                # updated 2020 (TB)
  "PLE4"   = 146852, "PLE3A"  = 19647 ,                       # updated 2020 (TB)
  "SOL-NS" = 17545 ,                                          # updated 2020 (TB)
  "WHG-NS"=17158/WHGNS_TACratio,                              # updated 2020 (TB) agreed tAC in 4
  "NEP-NS"= 22077 ,                                           # updated 2020 (KS) found in https://ec.europa.eu/commission/presscorner/detail/en/IP_19_6151
  "TUR" = 6498*TURprop ,                                      # added 2020 (TB)
  "WIT" = 6785*WITprop                                     # added 2020 (TB)
)
## and the combined area TACs
TAC.now<-c(TAC.now,
  "COD-NS"=sum(TAC.now["COD4"],TAC.now["COD3A"],TAC.now["COD7D"]),
  "HAD"=sum(TAC.now["HAD4"],TAC.now["HAD3A"],TAC.now["HAD6"]),"POK"=sum(TAC.now["POK4"],TAC.now["POK6"]), # Added 32 tonnes industrial bycatch to HAD TAC for 2019 (even if not really a TAC) VT
  "PLE-NS"=sum(TAC.now["PLE4"],TAC.now["PLE3A"]))

TAC.now


#
#if (LO==TRUE) { #TAC uplift
#  for (s. in c("COD-NS","HAD","PLE-NS","PLE-EC","WHG-NS")) TACplusuplift[s.] <- TAC.now[s.] + c(discards(wg.stock[[s.]])[,yr.assess])
#  TACplusuplift["NEP-NS"] <- TAC.now["NEP-NS"] + sum(sapply(nep.stock,function(x) discards(x)[,yr.assess]))
#}


# 2) recruitment assumption in STF
Recr <- FLPar(NA, dimnames=list(dem_stocks=dem.names, year=YearsProj,iter=1))

Rcod <- 176577   # use value from advice sheet  # median(rec(wg.stock[["COD-NS"]])[,ac((now-21):(now-1))])  # "Median recruitment resampled from the years 1998–2018, in thousands" (VT)
Recr["COD-NS",] <- c(268197,rep(Rcod,length(YearsProj)-1))        # updated 2020 TB

Recr["HAD",] <- c(5406360, rep(5406360,length(YearsProj)-1))  # updated 2020 TB

Rpleec <- gm_mean(stock.n(wg.stock[["PLE-EC"]])[1,ac((now-6):(now-3))])    # updated 2019 VT "Geometric mean 2013–2016, in thousands"
Recr["PLE-EC",] <- rep(Rpleec,length(YearsProj))

Rplens  <-  gm_mean(stock.n(wg.stock[["PLE-NS"]])[1,ac(2007:(now-4))])    # updated 2020 TB : Geometric mean (GM, 2007-2016),
Recr["PLE-NS",] <- c(Rplens,rep(Rplens,length(YearsProj)-1))    # updated 2020 TB


Rpok2020 <- 78287  ; Rpok2021 <- 77918                          # updated 2020 TB : advice sheet suggest to use recent GM
Recr["POK",] <- c(Rpok2020,rep(Rpok2021,2) )                    #
#                                                                #
#Rpok<-90078                                                      #  alternatively we could use the median recruitment (same value for the 3 forecast years)
#Recr["POK",] <- rep(Rpok,length(YearsProj))                      #  from the SAM stochastic simulations, available on the WGNSSK sharpoint
#

##
#Rsolec   <-   gm_mean(stock.n(wg.stock[["SOL-EC"]])[1,ac(1982:(now-4))])    # updated 2019 VT : Geometric mean (GM, 1982-2015),
#Recr["SOL-EC",] <- c(28964  ,rep(Rsolec,length(YearsProj)-1))             # updated 2019 VT : R2019 from RCT3

Rsolns <- gm_mean(stock.n(wg.stock[["SOL-NS"]])[1,ac(1957:(now-4))])        # updated 2020TB : Geometric mean (GM, 1957-2016),
Recr["SOL-NS",] <- rep(Rsolns, length(YearsProj))                           # in case no reopening 2020(TB)
#Recr["SOL-NS",] <- c(476477,rep(Rsolns, length(YearsProj)-1))              # updated after reopening 2019(VT)

Rwhgns   <-   gm_mean(stock.n(wg.stock[["WHG-NS"]])[1,ac(2002:(now-1))]) # updated 2020 TB :GM (excl. 2002-2018)
Recr["WHG-NS",] <-rep( Rwhgns , length(YearsProj))
#Recr["WHG-NS",] <-c(7550000, rep( Rwhgns , 2 ))                                     # updated 2018 YV reopening R2018 from RCT3

Rtur <- gm_mean(stock.n(wg.stock[["TUR"]])[1,ac(1981:(now-1))]) # updated 2020 TB :GM (excl. 1981-2019)
Recr["TUR",] <-rep( Rtur , length(YearsProj) )

Rwit <- 24699                                                   # added 2020 (TB) : Median of the resampled recruitment estimates of 2017-2019. not able to reproduce that here
Recr["WIT",] <-rep( Rwit , length(YearsProj) )


# 3)  ICES MSY ADVICE RULE AND STF SETUP
dem.stock <- FLStocks()
dem.st.fwd <- FLStocks()
ctrl.STF <- list()

# for the stocks which are under MSY Btrigger
 ssb_TACYr <- c("COD-NS" =  78300)   # updated 2020 (TB)
# ssb_TACYr <- c("WHG-NS" = 156590) # update 2019 (VT) with SSB 2020
slidingslope <- vector()
# slidingslope[["WHG-NS"]] <- ssb_TACYr["WHG-NS"]/BRPs["WHG-NS","Btrigger"]
 slidingslope[["COD-NS"]] <- ssb_TACYr["COD-NS"]/BRPs["COD-NS","Btrigger"]
# slidingslope[["SOL-EC"]] <- ssb_TACYr["SOL-EC"]/BRPs["SOL-EC","Bpa"]


# forecast control
ctrl.STF[["COD-NS"]]  <- fwdControl(          # updated 2019 (YV) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! VERY BAD FIX!!!!!!!!!!!!!!!!!!!!!!!
  data.frame(
    year=c(yr.now,yr.TAC),
   # val=c(.29,.162), # TAC constraint in 2020 and MSY HCR in 2021
   # quantity=c("f","f")
    val=c(TAC.now["COD-NS"],slidingslope["COD-NS"]*BRPs["COD-NS","Fmsy"]),
    quantity=c("catch","f")
  )
)



ctrl.STF[["HAD"]] <- fwdControl(              # update 2018 (TB), no change in 2020 (TB)
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(TAC.now["HAD"]+77,BRPs["HAD","Fmsy"]),
    quantity=c("catch","f")
  )
)

ctrl.STF[["PLE-EC"]] <- fwdControl(           # update 2018 (TB), no change in 2020 (TB)
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(1,BRPs["PLE-EC","Fmsy"]),
    quantity=c("f","f"),
    rel.year=c(yr.assess,NA)
  )
)


ctrl.STF[["PLE-NS"]] <- fwdControl(           # update 2018 (TB), no change in 2020 (TB)
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(1,BRPs["PLE-NS","Fmsy"]),
    quantity=c("f","f"),
    rel.year=c(yr.assess,NA)
  )
)

ctrl.STF[["POK"]] <- fwdControl(              # no change 2020 (TB) update 2018 (TB) ### original version replaced because conditionning based on Fsq now  fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(TAC.now["POK"],BRPs["POK","Fmsy"]), quantity=c("landings","f")))
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(1 ,BRPs["POK","Fmsy"]),
    quantity=c("f","f"),
     rel.year=c(yr.assess,NA)
  )
)

# ctrl.STF[["SOL-EC"]]  <- fwdControl(        # updated 2019 (VT): STF in 2019, FMSY in 2020, no MSY Btrigger approach this year
#   data.frame(
#     year=c(yr.now,yr.TAC),
#     val=c(1,BRPs["SOL-EC","Fmsy"]),
#     quantity=c("f","f"),
#     rel.year=c(yr.now,NA)
#   )
# )

ctrl.STF[["SOL-NS"]] <- fwdControl(         # changed in 2020 (TB) TAC constraint replaces status quo F in im year
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(TAC.now["SOL-NS"],BRPs["SOL-NS","Fmsy"]),
    quantity=c("catch","f")
  )
)

ctrl.STF[["WHG-NS"]] <- fwdControl(         # updated 2020 TB     no longer on HCR sliding slope
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(1,BRPs["WHG-NS","Fmsy"]),
    quantity=c("f","f"),
    rel.year=c(yr.assess,NA)
  )
)

ctrl.STF[["TUR"]] <- fwdControl(         # updated 2020 TB : stq F and then Fmsy instead of Fpa# added in 2019 (VT): STF in 2019, Fpa in 2020
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(1,BRPs["TUR","Fmsy"]),
    quantity=c("f","f"),
    rel.year=c(yr.assess,NA)
  )
)

ctrl.STF[["WIT"]] <- fwdControl(         # updated 2020 TB : stq F and then Fmsy instead of Fpa# added in 2019 (VT): STF in 2019, Fpa in 2020
  data.frame(
    year=c(yr.now,yr.TAC),
    val=c(1,BRPs["WIT","Fmsy"]),
    quantity=c("f","f"),
    rel.year=c(yr.assess,NA)
  )
) ### ! For turbot single species assessment, catch raised up for 14% discard rate but no discards in FLStock so discrepency between our catch and single species catch


###
#! NEPHROPS ADVICE
dead.discard.rate <- c("NEP6"=0.187,  "NEP7"=0.024,"NEP8"= 0.165,  "NEP9"=0.014 )    # updated 2020 (KS)
# information required to compute the TAC applying the de minimis equation
  # define which stocks are under the deminimis
nep.deminimis <- c()                            # none in the 2019 advice (VT)
  # survival rate for undersize nephrops discarded
Surv.nep          <- c( "NEP6"=0.15,  "NEP7"=0.25,"NEP8"=0.25,  "NEP9"=0.25)       # no change in 2020 (KS)                        ## TB : added october 2016 to implement the de minimis equation

# information that was necessary to do the deminimis computation in 2017, not the case in the 2018 advice
  # discard rate specific to the Nephrops above the MCRS
 #DR.unwanted.catch <- c("NEP5"=0.155,"NEP6"=0.1426,"NEP7"=0,   "NEP8"=0.111, "NEP9"=0.10,  "NEP10"=0.069,"NEP32"=0,  "NEP33"=0.225,"NEP34"=0.116,  "NEPOTH-NS"=0)  ## TB : added october 2016 to implement the de minimis equation
  # mean weight of the discards above MCRS
 #wt.unwanted.catch <- c("NEP5"=21.6, "NEP6"=10.88, "NEP7"=NA,  "NEP8"=10.75, "NEP9"=11.1,  "NEP10"=14.37,"NEP32"=0,  "NEP33"=17.19,"NEP34"=16.13,  "NEPOTH-NS"=0)     ## TB : added october 2016 to implement the de minimis equation


Fmsy.nep <-c("NEP6"=0.0812,"NEP7"=0.075,"NEP8"=0.163,"NEP9"=0.118)   # updated 2018 (TB), no change in 2019 (VT) and 2020 (KS)

no.nep.ass.ADV  <-c("NEP5"=1031,"NEP10"=46,"NEP32"=379,"NEP33"=956,"NEP34"=530,"NEPOTH-NS"=301)  # wanted catch  # updated 2020 (KS) # only updated NEP33 in 2019, no new advice for other species (only wanted catches in advice for NEP33, needs check)
no.nep.ass.ADVC <-c("NEP5"=1570,"NEP10"=46,"NEP32"=381,"NEP33"=956,"NEP34"=566,"NEPOTH-NS"=301)  # total catch   # updated 2020 (KS) "NEPOTH-NS" (2020) no total catch advice
no.nep.ass.ADV[is.element(names(no.nep.ass.ADV),LO)] <- no.nep.ass.ADVC[is.element(names(no.nep.ass.ADV),LO)]





## 4) biology and fisheries selectivity in the STF

# stf arguments:
# nyears=3 for 3 year-forecast
# wts.years= number of years to calculate averages for M, wt, mat, etc.
# fbar.years = number of years to calculate average F
# f.rescale = TRUE if Fbar scaled to correspond to a specific F (e.g. last year) but given another selectivity (e.g. average 3 years)
# disc.nyears = number of years to calculate average discard fraction

#cod
dem.stock[[1]] <- stf(wg.stock[["COD-NS"]], nyears=3, wts.nyears=3, fbar.nyears=3, f.rescale=F,disc.nyears=1) # updated 2020 (TB)   # updated 2019 (VT) YV changed rescale to F
names(dem.stock)[1] <- "COD-NS"
mat(dem.stock[["COD-NS"]])[, yr.now]   <- codMatYrNow   # updated 2019 (VT) : Maturity for the TAC year onwards is the average of final four years of assessment data 2016-2019 (not average between last 3)
mat(dem.stock[["COD-NS"]])[, ac((yr.TAC):yr.TACp1)]   <- codMat3YrMean   # updated 2019 (VT) : Maturity for the TAC year onwards is the average of final four years of assessment data 2016-2019 (not average between last 3)
stock.n(dem.stock[["COD-NS"]])[, yr.now]  <-  codNYrNow


#had
# First adjust the discards so that they also take into account the IBC
discards.n(wg.stock[["HAD"]]) <-  catch.n(wg.stock[["HAD"]]) - landings.n(wg.stock[["HAD"]])

dem.stock[[2]] <- stf(wg.stock[["HAD"]], nyears=3, wts.nyears=3, fbar.nyears=3, f.rescale=F,disc.nyears=3)     # updated 2020 (TB) # F based on TAC 2019 (VT)
names(dem.stock)[2] <- "HAD"

# based on Jarowski(2011) forecast of cohort-based weight data are given manually (found in WGNSSK report)
landings.wt(dem.stock[["HAD"]])[, yr.now]       <- c(0.000,	0.481,	0.451,	0.495,	0.549,	0.708,	0.661,	1.237,	1.571)    # updated 2020 (TB)
landings.wt(dem.stock[["HAD"]])[, yr.TAC]       <- c(0.000,	0.481,	0.451,	0.495,	0.654,	0.612,	0.791,	0.724,	1.390)
landings.wt(dem.stock[["HAD"]])[, yr.TACp1]     <- c(0.000,	0.481,	0.451,	0.495,	0.654,	0.858,	0.674,	0.875,	1.184)

discards.wt(dem.stock[["HAD"]])[, yr.now]       <- c(0.042,	0.140,	0.230,	0.304,	0.446,	0.431,	0.467,	0.699,	0.780)    # updated 2020 (TB)
discards.wt(dem.stock[["HAD"]])[, yr.TAC]       <- c(0.042,	0.140,	0.230,	0.314,	0.391,	0.545,	0.504,	0.531,	0.790)
discards.wt(dem.stock[["HAD"]])[, yr.TACp1 ]    <- c(0.042,	0.140,	0.230,	0.314,	0.389,	0.478,	0.645,	0.577, 0.660)

#catch.wt(dem.stock[["HAD"]])[, ac(yr.now:yr.TACp1)] <- computeCatch(dem.stock[["HAD"]],slot="wt")[, ac(yr.now:yr.TACp1)]

catch.wt(dem.stock[["HAD"]])[, yr.now]       <- c(0.042,0.146,0.335,0.433,0.616,0.795,0.745,1.301,1.634)   # updated 2020 (TB)
catch.wt(dem.stock[["HAD"]])[, yr.TAC]       <- c(0.042,0.146,0.335,0.471,0.568,0.762,0.950,0.857,1.508)
catch.wt(dem.stock[["HAD"]])[, yr.TACp1 ]    <- c(0.042,0.146,0.335,0.471,0.646,0.704,0.907,1.105,1.336 )


stock.wt(dem.stock[["HAD"]])[, ac(yr.now)]      <- c(0.042,0.146,0.335,0.433,0.616,0.795,0.745,1.301,1.634)    # updated 2020 (TB)
stock.wt(dem.stock[["HAD"]])[, ac(yr.TAC)]      <- c(0.042,0.146,0.335,0.471,0.568,0.762,0.95,0.857,1.508)
stock.wt(dem.stock[["HAD"]])[, ac(yr.TACp1)]    <- c(0.042,0.146,0.335,0.471,0.646,0.704,0.907,1.105,1.336)

m(dem.stock[["HAD"]])[, ac(yr.now)]      <- c(0.981,	1.258,	0.577,	0.288,	0.263,	0.255,	0.24,	0.267,	0.376)    # updated 2020 (TB)
m(dem.stock[["HAD"]])[, ac(yr.TAC)]      <- c(0.981,	1.258,	0.577,	0.288,	0.263,	0.255,	0.24,	0.267,	0.376)    #  the  +grp value is different from 3 years average...
m(dem.stock[["HAD"]])[, ac(yr.TACp1)]    <- c(0.981,	1.258,	0.577,	0.288,	0.263,	0.255,	0.24,	0.267,	0.376)

# use the selectivity output from TSA (in WGNSSK report Sel+DSel)
#harvest(dem.stock[["HAD"]])[, ac(yr.now:yr.TACp1)]<- c(0.002,0.026,0.137,0.240,0.238,0.227,0.116,0.076,0.076)   # updated 2020 (TB)

#
# abundances at the start of the intermediate year is actually produced by the assessment
# (altough not included in the stock object)
stock.n(dem.stock[["HAD"]])[, ac(yr.now)] <-  c(5406360,	4728130,	224700,	68970,	90010,	28070,	62400,	3300,	32110)


#plaice 7d
dem.stock[[3]] <- stf(wg.stock[["PLE-EC"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=T,disc.nyears=3)       # updated 2020 (TB)
names(dem.stock)[3]          <-   "PLE-EC"
stock.n(dem.stock[["PLE-EC"]])[1,yr.assess]   <-  Rpleec                                                       # added 2020 (TB) : "The model estimate has been replaced by the geometric mean 2014-2017 in the forecast due to large uncertainty."

#plaice
dem.stock[[4]] <- stf(wg.stock[["PLE-NS"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=T,disc.nyears=3)       # updated 2020 (TB)
names(dem.stock)[4]          <-   "PLE-NS"

#saithe
dem.stock[[5]] <- stf(wg.stock[["POK"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=T,disc.nyears=3)          #updated 2020 (TB)
names(dem.stock)[5]            <-  "POK"

# #sole 7d
# dem.stock[[6]] <- stf(wg.stock[["SOL-EC"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=T,disc.nyears=3)              # ok update 2019 (VT)
# names(dem.stock)[6] <- "SOL-EC"


##sole north sea
dem.stock[[6]] <- stf(wg.stock[["SOL-NS"]],nyears=3,wts.nyears=5,fbar.nyears=5 ,f.rescale=F,disc.nyears=3)   #updated 2020 (TB)  : apparently now mean over the last 5 years is used for biol and sel   # ok update 2019 (VT)
names(dem.stock)[6] <- "SOL-NS"   ## YV updated once removing SOL-EC


##whiting
dem.stock[[7]] <- stf(wg.stock[["WHG-NS"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=T,disc.nyears=3)      # updated 2020 (TB)   ## YV updated once removing SOL-EC           # updated 2019 (VT) : mean over the last 3 years is taken for the weights
names(dem.stock)[7] <-"WHG-NS"   ## YV updated once removing SOL-EC
stock.n(dem.stock[["WHG-NS"]])[, yr.now]  <-  whgNYrNow
  # discards ratio done manually because  this doesn't correspond to mean of the last 3 years
dem.stock[["WHG-NS"]]@landings.n[,ac(now:(now+2))]  <- c(0.01551,	0.07081,	0.25621,	0.49011,	0.69910,	0.76268,	0.77388,	0.76766,	0.86298)
dem.stock[["WHG-NS"]]@discards.n[,ac(now:(now+2))]  <- 1-dem.stock[["WHG-NS"]]@landings.n[,ac(now:(now+2))]



##turbot
dem.stock[[8]] <- stf(wg.stock[["TUR"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=F,disc.nyears=3)    # updated 2020 (TB)  ## YV updated once removing SOL-EC
catch(dem.stock[[8]]) <- landings(dem.stock[[8]]) ### YV no catch in the stock object and cantches are needed latter on to compute cacthabilities (F is based on landings)
names(dem.stock)[8] <-"TUR"
#
#  Witch  - NS
dem.stock[[9]] <- stf(wg.stock[["WIT"]],nyears=3,wts.nyears=3,fbar.nyears=3,f.rescale=F,disc.nyears=3)     # added 2020 (TB)
names(dem.stock)[9] <-"WIT"




#

#case of no discards
dem.stock <- lapply(dem.stock, function(x) {
  if(all(is.na(landings.n(x)[,ac(yr.now:yr.TACp1)]))) landings.n(x)[,ac(yr.now:yr.TACp1)] <- 1
  if(all(is.na(discards.n(x)[,ac(yr.now:yr.TACp1)]))) discards.n(x)[,ac(yr.now:yr.TACp1)] <- 0

  # Landings Obligation
  if (is.element(name(x),LO)){
      #if (name(x)!="HAD") {
      #landings.n(x)[,ac(yr.now:yr.TACp1)] <- 1
      #discards.n(x)[,ac(yr.now:yr.TACp1)] <- 0
      #landings.n(x)[1:3,ac(yr.now:yr.TACp1)] <- 0.94
      #discards.n(x)[1:3,ac(yr.now:yr.TACp1)] <- 0.06 # de minimis for the first 3 ages - default for everybody... CLU proxy 25/05/2017
      #landings.wt(x)[,ac(yr.now:yr.TACp1)] <- catch.wt(x)[,ac(yr.now:yr.TACp1)]
      #landings.wt(x)[,ac(yr.now:yr.TACp1)] <- ((landings.n(x)[,ac(yr.now:yr.TACp1)]* landings.wt(x)[,ac(yr.now:yr.TACp1)])+
       #                                       (discards.n(x)[,ac(yr.now:yr.TACp1)]* discards.wt(x)[,ac(yr.now:yr.TACp1)]))/
        #                                      (landings.n(x)[,ac(yr.now:yr.TACp1)]+ discards.n(x)[,ac(yr.now:yr.TACp1)])
  #  } else {
      landings.n(x)[,ac(yr.TAC:yr.TACp1)] <- 1
      discards.n(x)[,ac(yr.TAC:yr.TACp1)] <- 0
      #landings.n(x)[1:3,ac(yr.now:yr.TACp1)] <- 0.94
      #discards.n(x)[1:3,ac(yr.now:yr.TACp1)] <- 0.06 # de minimis for the first 3 ages - default for everybody... CLU proxy 25/05/2017
      landings.wt(x)[,ac(yr.TAC:yr.TACp1)] <- catch.wt(x)[,ac(yr.TAC:yr.TACp1)]
      #landings.wt(x)[,ac(yr.now:yr.TACp1)] <- ((landings.n(x)[,ac(yr.now:yr.TACp1)]* landings.wt(x)[,ac(yr.now:yr.TACp1)])+
#                                             (discards.n(x)[,ac(yr.now:yr.TACp1)]* discards.wt(x)[,ac(yr.now:yr.TACp1)]))/
#                                             (landings.n(x)[,ac(yr.now:yr.TACp1)]+ discards.n(x)[,ac(yr.now:yr.TACp1)])
#

    #}
 }
return(x)
})


## FORWARD PROJECTIONS
#
##-----------------------------------------------------------------------------

# 1) DEMERSAL STOCKS

dem.st.fwd <- lapply(dem.stock, function(x) {
  n. <- name(x)
  ctrl. <- ctrl.STF[[n.]]
  yr. <- unique(ctrl.@target[,"year"])
  srPar<-FLPar(c(Recr[n.,yr.]),dimnames=list(params="a",year=yr.,iter=1))

  #### !!!!! YV fix for control on ssb, not working because in FLash number of iter == 50 and is not enough to converge
  if(ctrl.@target$quantity[2]=="ssb"){
    x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
    x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))

  } else {
#    x <- fwdF3(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
    x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
  }
  #### !!!!! YV fix for control on ssb, not working because in FLash number of iter == 50 and is not enough to converge
  #  x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))

})
dem.names2 <- names(dem.st.fwd)

for(i in 1:length(dem.names)){
  name(dem.st.fwd[[i]]) <- dem.names2[i]
}



# 2) NEPHROPS STOCKS

nep.stock <- FLStocks(lapply(nep.lst,function(x) wg.stock[[x]]))
nep.stock <- FLStocks(lapply(nep.stock, function(x) {

x <- stf(x,nyears=2, wts.nyears=3, fbar.nyears=1)
#if(name(x) != "NEP7")  {                                                       # removed (TB) 2020 : NEP7 biol is average of last 3 years
#  x <- stf(x,nyears=2, wts.nyears=3, fbar.nyears=1) } else {
#  x <- stf(x,nyears=2, wts.nyears=length(max(2000,range(x)["minyear"]):an(yr.assess)), fbar.nyears=1)    }                   # ok 2019 (VT) but what about for NEP33? added 2018 : use mean weight starting from 2000
#
 # if (is.element(name(x),LO)) {
#    DR  <-  c(discards.n(x)[,ac(yr.now)])     # current discard rate
#    landings.n(x)[,ac(yr.now:yr.TAC)] <- 0.94     # new discard rate under the de minimis of 6%
#    discards.n(x)[,ac(yr.now:yr.TAC)] <- 0.06
#    landings.wt(x)[,ac(yr.now:yr.TAC)] <-    ((1-DR) * landings.wt(x)[,ac(yr.now:yr.TAC)]   + DR * discards.wt(x)[,ac(yr.now:yr.TAC)]   - 0.06 * discards.wt(x)[,ac(yr.now:yr.TAC)] )/  0.94   # recompute the new landings.wt based on the new 6% discards and assuming discards.wt is unchanged
#    rm(DR)
#  }

  return(x)
}))



Fbar.nep   <- sapply(nep.stock,function(x) harvest(x)[,yr.assess])
nep.ass    <- names(Fbar.nep)[which(!is.na(Fbar.nep))]   #nep stocks with an assessment, not the ass of the nep ;-)
no.nep.ass <- names(Fbar.nep)[which(!names(Fbar.nep) %in% nep.ass)]

for (i in nep.names) {
  if (!is.element(i,LO)) {
    tac <- TAC.now["NEP-NS"]*landings(nep.stock[[i]])[,yr.assess]/Sums(lapply(nep.stock,landings))[,yr.assess]} else {
    tac <- TAC.now["NEP-NS"]*catch(nep.stock[[i]])[,yr.assess]/Sums(lapply(nep.stock,catch))[,yr.assess]
  }
  TAC.now <- c(TAC.now, i=tac)
}

names(TAC.now)[grep("i",names(TAC.now))] <- nep.names


# DO THE FORECAST

nep.st.fwd <- lapply(nep.stock, function(x){
  n. <- name(x)
  cat(n.)
  # NEP  in year monitoring
  if(is.element(n.,reopenNEP)){  ####  in case any advice hs been reopened based on the june surveys
    stock.n(x)[,ac(yr.now:yr.TAC)] <- NEP_IntYrSurvey[[n.]]
  }else{
    stock.n(x)[,ac(yr.now:yr.TAC)] <- stock.n(x)[,yr.assess]
  }

 # DO THE FORECAST FOR NON  DE MINIMIS STOCKS

  if(!is.element (n.,nep.deminimis)){
    # yr.now : HR based on TAC.now
    landings(x)[,yr.now]   <- TAC.now[n.]
    landings.n(x)[,yr.now] <- landings(x)[,yr.now]/landings.wt(x)[,yr.now]
    catch.n(x)[,yr.now]    <- landings.n(x)[,yr.now]/(1-discards.n(x)[,yr.now])
    harvest(x)[,yr.now]    <- catch.n(x)[,yr.now]/stock.n(x)[,yr.now]

    # yr.TAC : HR based on target
    harvest(x)[,yr.TAC] <- Fmsy.nep[n.]

    if(is.element(n.,LO)){
      landings.n(x)[,yr.TAC] <- 1
      discards.n(x)[,yr.TAC] <- 0
      landings.wt(x)[,yr.TAC] <- catch.wt(x)[,yr.TAC]
    }

    # this part has been added in 2018 to reproduce the procedure used at WGNSSK in year
    DR.dead<-dead.discard.rate[n.] ; surv <- Surv.nep[n.]
    landings.n(x)[,yr.TAC] <- stock.n(x)[,yr.TAC] * harvest(x)[,yr.TAC]     - stock.n(x)[,yr.TAC]*harvest(x)[,yr.TAC]*DR.dead
    discards.n(x)[,yr.TAC] <- stock.n(x)[,yr.TAC] * harvest(x)[,yr.TAC]*DR.dead  + stock.n(x)[,yr.TAC]*harvest(x)[,yr.TAC]*DR.dead*(surv/(1-surv))
    catch.n(x)[,yr.TAC]    <- landings.n(x)[,yr.TAC] + discards.n(x)[,yr.TAC]
    landings(x)[,yr.TAC]   <- landings.n(x)[,yr.TAC] * landings.wt(x)[,yr.TAC]
    discards(x)[,yr.TAC]   <- discards.n(x)[,yr.TAC] * discards.wt(x)[,yr.TAC]
    catch(x)[,yr.TAC]      <- landings(x)[,yr.TAC] + discards(x)[,yr.TAC]

    if(n. %in% no.nep.ass)
    {
    landings(x)[,yr.TAC] <- no.nep.ass.ADV[n.]
    catch(x)[,yr.TAC]    <- no.nep.ass.ADVC[n.]
    }
  }

  # TB october 2016  : the part bellow is remplacing the ### part above for stock under the de minimis rule (no the case for any stock in NSSK2018)
  if (is.element (n.,nep.deminimis)){
    # yr.now : HR based on TAC.now
    DR  <-  yearMeans(discards.n(x)[,ac((an(yr.now)-3):(an(yr.now)-1))] / catch.n(x)[,ac((an(yr.now)-3):(an(yr.now)-1))]) # current discard rate
    landings(x)[,yr.now] <- TAC.now[n.]
    landings.n(x)[,yr.now] <- landings(x)[,yr.now]/landings.wt(x)[,yr.now]
    catch.n(x)[,yr.now] <- landings.n(x)[,yr.now]/(1-discards.n(x)[,yr.now])
    catch(x)[,yr.now]  <-  catch.n(x)[,yr.now]*catch.wt(x)[,yr.now]
    harvest(x)[,yr.now] <- catch.n(x)[,yr.now]/stock.n(x)[,yr.now]

    if(!is.element(name(x),LO)){
      # implementation of the LO
      DR.unwanted.catch[n.] <-0
    }else{
      # if the stock is not under LO, there is no unwanted catch, therefore the corresponding discard rate is 0
      # if the stock is under LO with a de minimis exception of Discards under MCRS = 6 % of total catch, then :
      # - the discard rate for the unwanted catch is unchanged compare to previous year
      # - the discard rate for the discards below MCRS is 6%
      DR  <- 0.06 + DR.unwanted.catch[n.]
    }

  DR.undersized <-  DR  - DR.unwanted.catch[n.]

  #yr.TAC : HR based on target
  harvest(x)[,yr.TAC]  <- Fmsy.nep[n.]

  if (!is.element(n.,no.nep.ass)){
    catch.n(x)[,yr.TAC]  <- (1 / (Surv.nep[name(x)]*(DR-DR.unwanted.catch[n.])+1))* stock.n(x)[,yr.TAC]*harvest(x)[,yr.TAC]
    catch(x)[,yr.TAC]    <- catch.n(x)[,yr.TAC]*catch.wt(x)[,yr.TAC]
  }
  if (is.element(n.,no.nep.ass)){
    catch(x)[,yr.TAC]      <- no.nep.ass.ADV[n.]
    catch.n(x)[,yr.TAC]    <- catch(x)[,yr.TAC]/catch.wt(x)[,yr.TAC]
  }

  landings.n(x)[,yr.TAC]      <- catch.n(x)[,yr.TAC]*(1-DR.undersized)
  landings(x)[,yr.TAC]        <- landings.n(x)[,yr.TAC]*landings.wt(x)[,yr.TAC]
  discards.n(x)[,ac(yr.TAC)]  <- catch.n(x)[,ac(yr.TAC)]*DR.undersized
  discards(x)[,ac(yr.TAC)]    <- discards.n(x)[,ac(yr.TAC)]*discards.wt(x)[,ac(yr.TAC)]
  }

  return(x)
})





##----------------------------------------------
# Single-species results
##----------------------------------------------
results <- data.frame()
#########
results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.now,yr.TAC),n.dem)),
    stock=rep(dem.names2,2),
    value="landings",
    data=c(round(sapply(dem.st.fwd, function(x) c(computeLandings(x)[,yr.now]))),round(sapply(dem.st.fwd, function(x) computeLandings(x)[,yr.TAC])))
  )
)

results <- rbind(
  results,
  cbind(
    sc="baseline",year=sort(rep(c(yr.now,yr.TAC),n.dem)),
    stock=rep(dem.names2,2),
    value="catch",
    data=c(round(sapply(dem.st.fwd, function(x) computeCatch(x)[,yr.now])),round(sapply(dem.st.fwd, function(x) computeCatch(x)[,yr.TAC])))
  )
)

results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.now,yr.TAC),n.nep)),
    stock=rep(nep.names,2),
    value="landings",
    data=c(round(sapply(nep.st.fwd, function(x) landings(x)[,yr.now])),round(sapply(nep.st.fwd, function(x) landings(x)[,yr.TAC])))
  )
)

results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.now,yr.TAC),n.nep)),
    stock=rep(nep.names,2),
    value="catch",
    data=c(round(sapply(nep.st.fwd, function(x) catch(x)[,yr.now])),round(sapply(nep.st.fwd, function(x) catch(x)[,yr.TAC])))
  )
)



results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.now,yr.TAC),n.dem)),
    stock=rep(dem.names2,2),
    value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),
    data=c(round(sapply(dem.st.fwd, function(x) fbar(x)[,yr.now]/fbar(x)[,yr.assess]),3),round(sapply(dem.st.fwd, function(x) fbar(x)[,yr.TAC]/fbar(x)[,yr.assess]),3))
  )
)

results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.now,yr.TAC),n.nep)),
    stock=rep(nep.names,2),value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),
    data=c(round(sapply(nep.st.fwd, function(x) fbar(x)[,yr.now]/fbar(x)[,yr.assess]),3),round(sapply(nep.st.fwd, function(x) fbar(x)[,yr.TAC]/fbar(x)[,yr.assess]),3))
  )
)


results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.assess,yr.now,yr.TAC),n.dem)),
    stock=rep(dem.names2,3),
    value="Fbar",
    data=c(round(sapply(dem.st.fwd, function(x) fbar(x)[,yr.assess]),3),round(sapply(dem.st.fwd, function(x) fbar(x)[,yr.now]),3),round(sapply(dem.st.fwd, function(x) fbar(x)[,yr.TAC]),3))
  )
)

results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.assess,yr.now,yr.TAC),n.nep)),
    stock=rep(nep.names,3),
    value="Fbar",
    data=c(round(sapply(nep.st.fwd, function(x) fbar(x)[,yr.assess]),3),round(sapply(nep.st.fwd, function(x) fbar(x)[,yr.now]),3),round(sapply(nep.st.fwd, function(x) fbar(x)[,yr.TAC]),3))
  )
)

results <- rbind(
  results,
  cbind(
    sc="baseline",
    year=sort(rep(c(yr.now,yr.TAC,yr.TACp1),n.dem)),
    stock=rep(dem.names2,3),
    value="ssb",
    data=c(round(sapply(dem.st.fwd, function(x) ssb(x)[,yr.now])),round(sapply(dem.st.fwd, function(x) ssb(x)[,yr.TAC])),round(sapply(dem.st.fwd, function(x) ssb(x)[,yr.TACp1])))
  )
)



# finding the landings target corresponding to this - in year now
Ftarg.dem.now <- sapply(dem.st.fwd,function(x){
  n. <- x@name
  r. <- range[n., "minfbar"]:range[n., "maxfbar"]
  #F.land <- harvest(x)[,yr.now]*landings.n(x)[,yr.now]/catch.n(x)[,yr.now]
  #apply(F.land[as.character(r.)],2:6,mean,na.rm=T)
  fbar(x)[,yr.now]*landings(x)[,yr.now]/catch(x)[,yr.now]
})

Ftarg.nep.now <- sapply(nep.st.fwd,function(x){
  n. <- x@name
  r. <- range[n., "minfbar"]:range[n., "maxfbar"]
  F.land <- harvest(x)[,yr.now]*landings.n(x)[,yr.now]/(landings.n(x)[,yr.now]+discards.n(x)[,yr.now])
})

# saving the results
#BRPs, ctrl.STF, Ftarg.now, LastProjectionYear, LO, now, range, Recr, Run.name, st.fwd, results,
#stock, st.lst, names, stock.names, TAC.now, wg.stock, YearsProj, yr.assess, yr.now, yr.TAC,
#yr.TACp1
# save.image(file=paste0(model.path,"/",Run.name,".Rdata"))
save(range, dem.st.fwd, nep.st.fwd, wg.stock, ctrl.STF,Ftarg.dem.now,Ftarg.nep.now,TAC.now,
  dem.stock, nep.stock,Recr,reopenNEP,NEP_IntYrSurvey,nep.ass,no.nep.ass,results,BRPs, file = paste0(model.path,"/",Run.name,".Rdata"))

print(ftable(tapply(as.numeric(results$data),list(results$year,results$value,results$stock),sum)))
#write.table(results,'single_species_results_ for_comparison script LO LOon.csv',sep=",",row.names=F)
# plot Fsq by stock compared to Fmsy
fsq <- sapply(dem.stock,function(x) fbar(x)[,yr.assess])
Fratio <- as.data.frame(
  rbind(
    cbind(stock=dem.names,type="Fmsy/Fsq",val=BRPs[,"Fmsy"]/fsq),
    cbind(stock=dem.names,type="FmsyH/Fsq",val=BRPs[,"FmsyH"]/fsq),
    cbind(stock=dem.names,type="FmsyL/Fsq",val=BRPs[,"FmsyL"]/fsq)
  ),
  row.names = FALSE
)


Fratio$val <- as.numeric(Fratio$val)

png(file=file.path(plot.path,paste0(Run.name, "_Fsq_to_MSY.png")), width=7, height=6, units="in", res=400)
  p0 <- ggplot(Fratio,aes(stock,val)) +
    geom_point(aes(shape=factor(type))) +
    ylab("ratio") +
    theme(legend.title=element_blank(),
      axis.text=element_text(size=8),
      legend.text=element_text(size=10)) +
    geom_hline(yintercept=1,linetype="dashed")
  print(p0)
dev.off()

#####################
# Diagnostic plots ##
#####################
#if (plotdiag==TRUE) {

   library(ggplot2) ;  library(plyr) ; library(reshape2) ; library(gridExtra)

  ## Ratio of landings to discards ##
  # landings - discards in weights (last 2 years)
  rat_weight <- rbind(
    as.data.frame(lapply(wg.stock,function(x) landings(x)[,ac(max(range(wg.stock[[1]])))]/catch(x)[,ac(max(range(wg.stock[[1]])))])),
    as.data.frame(lapply(wg.stock,function(x) landings(x)[,ac(max(range(wg.stock[[1]])-1))]/catch(x)[,ac(max(range(wg.stock[[1]])-1))]))
  )

  # in number wrt Fbar (last 2 years)
  rat_nbr <- rbind(
    as.data.frame(lapply(wg.stock,function(x){
      r. <- ac(range(x)["minfbar"]:range(x)["maxfbar"])
      mean(harvest(x)[r.,ac(max(range(wg.stock[[1]])))]*landings.n(x)[r.,ac(max(range(wg.stock[[1]])))]/catch.n(x)[r.,ac(max(range(wg.stock[[1]])))])/fbar(x)[,ac(max(range(wg.stock[[1]])))]
    })),
    as.data.frame(lapply(wg.stock,function(x){
      r. <- ac(range(x)["minfbar"]:range(x)["maxfbar"])
      mean(harvest(x)[r.,ac(max(range(wg.stock[[1]])-1))]*landings.n(x)[r.,ac(max(range(wg.stock[[1]])-1))]/catch.n(x)[r.,ac(max(range(wg.stock[[1]]))-1)])/fbar(x)[,ac(max(range(wg.stock[[1]])-1))]
    }))
  )

  # now comparison of single species advice and baseline FCube run ##

  ## First read in the single species advice sheet values
  single.species.advice <- read.table(file.path(input.path,'single_species_advice.csv'),sep=",",header=T)
  spp.advice <- melt(single.species.advice,id=c("stock","year"))
  spp.advice <- spp.advice[c("year","stock","variable","value")]; colnames(spp.advice)<-c("year","stock","value","data")
  results <- results[!names(results)=='iter']

  # combine the results with single species advice
  mult <- paste("FmultVsF",substr(paste(yr.assess),3,4),sep="") # remove the Fmult values
  plots.comp <- rbind(results[(results$value != mult),],cbind(sc="SingleSpp.Advice",spp.advice))
  plots.comp$data <- as.numeric(plots.comp$data)
  plots.comp <- plots.comp[plots.comp$year>=now,]


  # also want to plot them as relative
  plots.comp.rel <- dcast(plots.comp,year + stock + value ~ sc,value.var="data")
  colnames(plots.comp.rel) <- c("year","stock","value","FCube.baseline","Single.Spp.Advice")
  plots.comp.rel$diff <- round(100*(plots.comp.rel$FCube.baseline-plots.comp.rel$Single.Spp.Advice)/plots.comp.rel$Single.Spp.Advice,1)

  ## dataframe of last 3 years values
  stf_check <- rbind(
    cbind(type="fbar",as.data.frame(lapply(dem.st.fwd,function(x) fbar(x)[,ac(yr.assess:yr.TACp1)]))),
    cbind(type="landings",as.data.frame(lapply(dem.st.fwd,function(x) landings(x)[,ac(yr.assess:yr.TACp1)]))),
    cbind(type="ssb",as.data.frame(lapply(dem.st.fwd,function(x) ssb(x)[,ac(yr.assess:yr.TACp1)])))
  )


  ## PLOTS ##
  pdf(file.path(plot.path, paste0(Run.name, '_diagnostics_plots_recreate_the_advice.pdf')))

  stf_check$qname  <-  as.character(stf_check$qname)

  p0 <- ggplot(
    stf_check[(stf_check$year != as.numeric(yr.TACp1)),],
    aes(x=(year),y=data,colour=factor(year))
  )
  p0 <- p0 +
    scale_x_continuous("", breaks=as.numeric(c(yr.assess, yr.now, yr.TAC))) +
    geom_point(size=2) +
    geom_line(colour="black") +
    facet_grid(type ~ qname,scale="free_y") +
    theme_bw() +
    geom_hline(
      data = stf_check[(stf_check$year==yr.assess),],
      aes(yintercept=data), lty=2
    ) +
    theme(axis.text.x=element_text(angle=-90,vjust=0),strip.text.x=element_text(size=6)) +
    ggtitle("FCube baseline output \n(dashed line = Sq values from assessment year)") +
    xlab("")
   print(p0)

  ## plot of discard ratio by weight and fbar
  plot.ratios<-rbind(cbind(type="weight",rat_weight),cbind(type="fbar",rat_nbr))

  p <- ggplot(plot.ratios,aes(x=factor(qname),y=data))
  print(p + geom_point(aes(colour=type,shape=type),size=3) + theme_bw() + ylab("landings proportion in catch") +
          xlab("stock") + theme(axis.text.x=element_text(angle=-90,hjust=0,face="bold",size=6)) + geom_line(aes(group=qname)) +
          ggtitle("Difference between the proportion of \n landings:catch by weight and by fbar") +
          facet_wrap(~year,nrow=2))



  ## plot of Reproduce the advice compared to the single species advice

  # Analytical

  p1 <- ggplot(plots.comp[(plots.comp$stock %in% dem.names),],aes(x=stock,y=data))
  print(p1 + geom_point(aes(colour=sc,shape=sc),size=3) + facet_grid(value~year,scale="free_y") + theme_bw() + scale_shape(solid = FALSE) +
          theme(legend.position="top",axis.text.x=element_text(angle=-90,hjust=0,face="bold",size=8)) + ggtitle("Reproduce the advice diagnostic plot Analytical stocks. \nValues are absolute ouput from single species and FCube baseline run"))



  # Non analytical
  p2<-ggplot(plots.comp[(plots.comp$stock %in% nep.names & plots.comp$value !="ssb" & plots.comp$year != yr.TACp1),],aes(x=factor(stock),y=data))
  print(p2 + geom_point(aes(colour=sc,shape=sc),size=3) + facet_grid(value~year,scale="free_y") + theme_bw() + scale_shape(solid = FALSE) +
         theme(legend.position="top",axis.text.x=element_text(angle=-90,hjust=0,face="bold",size=8)) + ggtitle("Reproduce the advice diagnostic plot Non Analytical stocks. \nValues are absolute ouput from single species and FCube baseline run"))



  ## as a percentage - analytical
  p3<-ggplot(plots.comp.rel[(plots.comp.rel$stock %in% dem.names),],aes(x=value,y=diff))
  print(p3 + geom_point(aes(colour=year,shape=year),size=3) + facet_wrap(~stock) + theme_bw() + scale_shape(solid = FALSE) +
          theme(legend.position="top",axis.text.x=element_text(angle=-90,hjust=0,face="bold",size=8)) + ggtitle("Reproduce the advice diagnostic plot Analytical stocks. \nValues are percentage deviation of FCube baseline run from single species output"))



  ## as a percentage - Non analytical
  p4 <- ggplot(
    data = plots.comp.rel[(plots.comp.rel$stock %in% nep.names & plots.comp.rel$value !="ssb" & plots.comp.rel$year != yr.TACp1),],
    aes(x=value,y=diff)
  )
  print(p4 + geom_point(aes(colour=year,shape=year),size=3) + facet_wrap(~stock) + theme_bw() + scale_shape(solid = FALSE) +
         theme(legend.position="top",axis.text.x=element_text(angle=-90,hjust=0,face="bold",size=8)) + ggtitle("Reproduce the advice diagnostic plot Non Analytical stocks. \nValues are percentage deviation of FCube baseline run from single species output"))



  ## table of values
  plots.comp.rel <- plots.comp.rel[!(plots.comp.rel$value %in% c("Fbar","landings") & plots.comp.rel$year==max(plots.comp.rel$year)),]

  for (i in dem.names) {
    grid.newpage()
    grid.table(plots.comp.rel[which(plots.comp.rel$stock ==i),]) #,h.even.alpha=1, h.odd.alpha=1,  v.even.alpha=0.5, v.odd.alpha=1,row.just="left")
  }

  for (i in nep.names) {
   grid.newpage()
   grid.table(plots.comp.rel[which(plots.comp.rel$stock ==i & plots.comp.rel$value !="ssb"),])  #,h.even.alpha=1, h.odd.alpha=1,  v.even.alpha=0.5, v.odd.alpha=1,row.just="left")
  }


  tmp <- reshape(plots.comp.rel, idvar=c("year","value","stock"), varying=list(4:6), direction="long")
  tmp[tmp$time==1,]$time <- "FCube_single_spp"
  tmp[tmp$time==2,]$time <- "ICES_advice"
  tmp[tmp$time==3,]$time <- "difference (%)"
  tmp2 <- reshape(tmp, idvar=c("year","value","time"), timevar="stock", direction="wide")
  tmp2 <- tmp2[order(tmp2$year,tmp2$value,tmp2$time),]
  colnames(tmp2)[colnames(tmp2)=="time"] <- "type"
  colnames(tmp2)[grepl("FCube.baseline.",colnames(tmp2))] <- sapply(strsplit(colnames(tmp2)[grepl("FCube.baseline.",colnames(tmp2))],"FCube.baseline."),function(x)return(x[2]))

  grid.newpage()
  grid.table(tmp2)  #,h.even.alpha=1, h.odd.alpha=1,  v.even.alpha=0.5, v.odd.alpha=1,row.just="left")


  dev.off()
# }

write.csv(tmp2, file=file.path(res.path, paste0(Run.name, '_diagnostics_recreate_the_advice.csv')),row.names = FALSE)
 # end of batch
print(Sys.time()-t1)
#dev.off()
