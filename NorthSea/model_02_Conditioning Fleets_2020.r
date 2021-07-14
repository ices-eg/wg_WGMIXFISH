###################################################################################
##     FLFCUBE SCRIPT FOR RUNNING FCUBE RUNS ON VARIOUS TAC SCENARIOS            ##
##                                                                               ##
##                         North Sea                                             ##
##                                                                               ##
##                                                                               ##
##      Progr.2 - FLEET CONDITIONING FOR ADVICE 2 YEARS AHEAD                    ##
##                                                                               ##
## Initial Author : Clara Ulrich, DTU Aqua <clu@aqua.dtu.dk>                     ##
## Further Contributors: Youen Vermard, Paul J. Dolder,...
# And ICES WGMIXFISH colleagues
##                                                                               ##
## Runs with R 3.1                                                               ##
##                                                                               ##
##                                                                               ##
###################################################################################


### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
gc(); graphics.off()
ver <- "02_Conditioning_Fleets"
ver.datetime   <- "27/10/2020";
options(stringsAsFactors = FALSE, scipen=100)



source('bootstrap/software/functions/FLFcube_FLCore_R31.R')
source('bootstrap/software/functions/remove_validity_FLFleet.R') # to reduce computing time

##-----------------------------------------------------------------------------
## I. GET INPUTS
##-----------------------------------------------------------------------------

# wg.path    <- "data/NS_FLStocks_01_Reproduce_The_Advice_2019" YV 2020 not used latter in the code
fleet.path <- "data/NS_Fleet_database"

model.path <- "model"

#results folders
res.path  <- "output/02_Conditioning/results"
plot.path <- "output/02_Conditioning/plots"

##-----------------------------------------------------------------------------
## II. CHOICE OF THE RUN
#
##-----------------------------------------------------------------------------
Run.nameFC <- ver

load("model/model_settings.Rdata")
load(file.path(model.path,"01_Reproduce_The_Advice_2020_keepNYrNow_LO.Rdata"))
load(file.path(fleet.path,"03_NS_Fleet_database_AA_KW.RData")) #fleets data without catch at age information

if (length(LO)>0)  Run.nameFC <- paste0(Run.nameFC,"_LO") else Run.nameFC <-Run.nameFC

NepYes <- TRUE # if TRUE Nephrops is included
if (NepYes==T) st.names <- stock.names else st.names <- dem.names

fleet.names   <- names(fleets)
fl.lst        <- as.list(fleet.names)
names(fl.lst) <- fleet.names

#! choice of Fcube management year and numbers of years used to estimate mean landings

f3.ctrl <- new("FLFcube.control")
avg.yrs<-1

f3.ctrl@data.yrs <- (max(range[,"maxyear"])-avg.yrs+1):max(range[,"maxyear"])

yr.rge <-ac(f3.ctrl@data.yrs)


  #!specific case no Nephrops or no turbot
## keep only the FLCatches from the same stocks

  fleets <- FLFleets(lapply(fleets, function(x) {
    x@metiers <-lapply(x@metiers,function(x1) {

      ma <- match(st.names,names(x1@catches)) #turbot removed from stock.names
      ma <- ma[!is.na(ma)]
      flc<-FLCatches()
      for (sp in seq(length(ma))) {
        flc[[sp]] <- x1@catches[[ma[sp]]]
      }
      names(flc)<-names(x1@catches)[ma]
      x1@catches <- flc
      return(x1) })
    return(x)}))





printplot<- FALSE

if (printplot==T) {


  #Forecasting the fleets -  simple average with analysis of catchability
  #plotting catchability
  qq. <- slot.fleet(fleets,"catch.q")
  qq.$logq <- log(qq.$catch.q)
  qq.2 <- aggregate(qq.$logq,list(qname=qq.$qname,fleet=qq.$fleet,metier=qq.$metier),FUN="mean")
  qq. <- merge(qq.,qq.2)
  qq.$dev <- qq.$logq/qq.$x
  qq. <- qq.[!is.nan(qq.$dev) & !(qq.$dev)==0,]


# PLOTS CATCHABILITY ALL METIERS AT ONCE
  par(ask=F)
  for (st in dem.names) {
  x11()
  print(xyplot(dev ~ year|fleet,groups=metier,data=subset(qq., !fleet=="OTH_OTH" & qname==st), pch=c(19,21:25),type="b",col="black",#panel=panel.effshare, #type="b",fill=T,col="black",
               scales=list(x=list(cex=0.7,tick.number=4),y=list(cex=0.7)), par.strip.text=list(cex=0.7),
               ylab=list(label="relative (log.q)",cex=0.8),xlab=list(cex=0.8),
               main=paste('log(q) by fleet and metier, ',st,sep="")))
  savePlot(filename=paste(plot.path,"/catchability_",st,".jpeg",sep=""),'jpeg')
  }


  # PLOTS RELATIVE STABILITY
  ld <- slot.fleet(fleets,"landings")
  ld$country <- substring(ld$fleet,1,2)
  ld <- aggregate(ld$landings,list(year=ld$year, stock=ld$qname,country=ld$country),FUN="sum",na.rm=T)
  names(ld)[which(names(ld)=="x")] <- "landings"
  ld. <- aggregate(ld$landings,list(year=ld$year, stock=ld$stock),FUN="sum",na.rm=T)
  names(ld.)[which(names(ld.)=="x")] <- "land.tot"
  ld <- merge(ld,ld.)
  ld$share <- (ld$landings/ld$land.tot)*100

  x11()
        print(xyplot(share ~year|country,groups=stock, data=subset(ld, stock %in% c("COD-NS","POK","WHG-NS","HAD","SOL-NS","PLE-NS","PLE-EC")),
              scales=list(x=list(cex=0.7,tick.number=4),y=list(cex=0.7)),main="landings share by country,dem fish",
                  type="b",auto.key=list(columns=3,points=FALSE,lines=TRUE,type="b",cex=0.8),
                  par.strip.text=list(cex=0.7), ylab=list(label="percentage",cex=0.8),
                  par.settings=list(superpose.symbol=list(pch=0:5))))
    savePlot(filename=paste(plot.path,"/RelStabCountry_Dem.jpeg",sep=""),'jpeg')

  if (NepYes==T) {
    x11()
        print(xyplot(share ~year|country,groups=stock, data=ld[grep("NEP",ld$stock),],
              scales=list(x=list(cex=0.7,tick.number=4),y=list(cex=0.7)),main="landings share by country,Nephrops FU",
                  type="b",auto.key=list(columns=3,points=FALSE,lines=TRUE,type="b",cex=0.8),
                  par.strip.text=list(cex=0.7), ylab=list(label="percentage",cex=0.8),
                  par.settings=list(superpose.symbol=list(pch=0:5))))
        savePlot(filename=paste(plot.path,"/RelStabCountry_Nep.jpeg",sep=""),'jpeg')

  }

}

#change in L-D ratio over time
ratio <- lapply(dem.st.fwd,function(x) landings(x)/catch(x))
ratio <- lapply(ratio, function(x) sweep(x[,ac(now:LastProjectionYear)],c(1,3:6),x[,ac(yr.assess)],FUN="/"))


#Forecasting the fleets - very long!
gc()
i <- 0
print(system.time(fl.pred <- lapply(fleets, function(x) {
              #print(name(x))
            x@metiers <- as(lapply(x@metiers, function(met) {
              #print(name(met))
                          met@catches <- as(lapply(met@catches, function(st) {
                          #print(name(st))
                                          x. <- dims(catch.q(st))$minyear:dims(catch.q(st))$maxyear
                                          y. <- c(catch.q(st))

                                          st <- window(st,end=LastProjectionYear)

                                          # reg analysis of the catchability
                                          #if (all(!is.na(y.)) && all(y.>0)) { #print("CheckOK")
                                          if (all(!is.na(y.)) && all(y.>0) && all(is.finite(y.))) { #print("CheckOK")

                                           if (summary(lm(log(y.)~x.))$coefficient["x.",4] >0.05) # to be improved - log q trends
                                          catch.q(st)[,ac(now:LastProjectionYear)] <- yearMeans(catch.q(st)[,yr.rge])  else {
                                          #print(paste("qtrend",n.x, n.m, name(st)))
                                          catch.q(st)[,ac(now:LastProjectionYear)] <- catch.q(st)[,yr.rge[length(yr.rge)]] }
                                          } else catch.q(st)[,ac(now:LastProjectionYear)] <- yearMeans(catch.q(st)[,yr.rge])


                                          landings.sel(st)[,ac(now:LastProjectionYear)] <- yearMeans(landings.sel(st)[,yr.rge])
                                          if(name(st)%in%names(ratio)) landings.sel(st)[,ac(now:LastProjectionYear)] <- landings.sel(st)[,ac(now:LastProjectionYear)]*ratio[[name(st)]]
                                          #discards.sel(st)[,ac(now:LastProjectionYear)] <- yearMeans(discards.sel(st)[,yr.rge])
                                          discards.sel(st)[,ac(now:LastProjectionYear)] <- 1-landings.sel(st)[,ac(now:LastProjectionYear)]

                                          if(is.element(name(st),LO)) {

                                            #if(is.element(name(st),nep.names)) {
                                             # landings.sel(st)[,ac((now):LastProjectionYear)] <- 0.94
                                              #discards.sel(st)[,ac((now):LastProjectionYear)] <- 0.06
                                            #} else {
                                            landings.sel(st)[,ac((now+1):LastProjectionYear)] <- 0.94
                                            discards.sel(st)[,ac((now+1):LastProjectionYear)] <- 0.06 #we keep demininis for other stocks too
                                          }#}

                                          #LO 2017
                                          #not implemented now - as not accounted for in SS advice
#                                           if(is.element(name(st),names(LO_2017)) {
#
#                                             if(is.element(unlist(strsplit(met,"[.]"))[1],LO_2017[[st]])) {
#
#                                               landings.sel(st)[,ac((now):LastProjectionYear)] <- 0.94
#                                               discards.sel(st)[,ac((now):LastProjectionYear)] <- 0.06
#                                             }}
#
                                          price(st)[,ac(now:LastProjectionYear)] <- yearMeans(price(st)[,yr.rge])
                                          return(st)}) ,"FLCatches")
                                          met <- window(met,end=LastProjectionYear)
                                          met@effshare[,ac(now:LastProjectionYear)] <-  yearMeans(met@effshare[,yr.rge])
                          return(met)}) ,"FLMetiers")
                      x <- window(x,end=LastProjectionYear)
          x@capacity[,ac(now:LastProjectionYear)] <- yearMeans(x@capacity[,yr.rge])
          x@crewshare[,ac(now:LastProjectionYear)] <- yearMeans(x@crewshare[,yr.rge])
          print(name(x))

          return(x)})
 ))
fl.pred <- FLFleets(fl.pred)

#res.path <- "North_Sea/results"
save(f3.ctrl,fl.lst, fl.pred, fleet.names, fleets, Run.nameFC, NepYes, yr.rge, st.names, file=paste0(model.path,"/",Run.nameFC,".Rdata"))
