# 1. Initialization --------------------------------------------------------------
# Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()
ver01 <- "NS_Fleet_database"

# ver <- "NS_Fleet database" #
# ver.datetime   <- "2019-10-23";
# cat(paste("\n",ver,"\n",sep=""))# ;cat(paste(ver.datetime,"\n\n",sep=""))
options(stringsAsFactors = FALSE, scipen=3)

datayear <- 2019

# 1.1. Required packages ============================

# 1.2. Required Methods ========================

source("bootstrap/software/functions/FLFcube_FLCore_R31.R")  #slot.fleet needed



# 1.3. Required paths ========================

# fleet.path <- file.path(paste0("data/",ver,"/"))
# res.path <- file.path(paste0("data/",ver,"/"))

fleet.path <- file.path("data", ver01)
res.path   <- file.path("output")
stock.path <- file.path("data/FLStocks_out")


# 1.4. Other options ============================
# choice of Age-disaggregated data for analytical stocks or not
AgeDisaggregated <- FALSE

# Stocks to include
AD.Stocks <- c(
  "COD-NS","HAD","PLE-EC",
  "PLE-NS","POK",
  "SOL-NS","WHG-NS","TUR","WIT"
)


# YV changed in 2019 Sol 7D not cat1 stock in 2019, still valid in 2020 even if an XSA is run might include SOL-EC?
# AD.Stocks <- c(
#   "COD-NS","HAD","PLE-EC",
#   "PLE-NS","POK", "SOL-EC",
#   "SOL-NS","WHG-NS","TUR"
# )

AA.Stocks <- c(
  #"ANF", "BLL", "DAB", "LEM", "LIN",# "USK",
  "NEP10","NEP32","NEP33",
  "NEP34","NEP5","NEP6",
  "NEP7","NEP8","NEP9",
  "NEPOTH-NS"
)

# year ranges
year_range <- 2012:datayear

# version string
if(AgeDisaggregated==TRUE){
  ver03 <- paste("AD",sep="_")
} else {
  ver03 <- paste("AA",sep="_")
  AA.Stocks <- sort(c(AA.Stocks,AD.Stocks))
  AD.Stocks <- vector()
}

print(ver03)



# 2. Load data -----------------------------------------------------------

# 2.1. Load catch and effort data ==================================

load(file.path(fleet.path, paste0("02_catch_eff", ".RData"))) # "catch", "catch_ad", "effcap"



# subset based on year_range
catch <- subset(catch, year %in% year_range)
effcap <- subset(effcap, year %in% year_range)
catch_ad <- subset(catch_ad, year %in% year_range)
unique(catch$stock)
unique(catch_ad$stock)

# remove NAs from landings etc
catch$land[is.na(catch$land)] <- 0
catch$disc[is.na(catch$disc)] <- 0

# catch_ad$Lan_MeanWeight_ad_kg[is.na(catch_ad$Lan_MeanWeight_ad_kg)] <- 0
# catch_ad$Dis_MeanWeight_ad_kg[is.na(catch_ad$Dis_MeanWeight_ad_kg)] <- 0
catch_ad$Lan_CANUM_ad[is.na(catch_ad$Lan_CANUM_ad)] <- 0
catch_ad$Dis_CANUM_ad[is.na(catch_ad$Dis_CANUM_ad)] <- 0

# convert numbers to '000s
catch_ad$Lan_CANUM_ad <- catch_ad$Lan_CANUM_ad / 10^3
catch_ad$Dis_CANUM_ad <- catch_ad$Dis_CANUM_ad / 10^3


head(catch)
head(catch_ad)



years <- as.numeric(sort(unique(catch$year)))
years



# correct TUR (discards should be set to zero)
head(catch)
rmv <- which(catch$stock == "TUR")
catch$disc[rmv] <- 0

head(catch_ad)
rmv <- which(catch_ad$stock == "TUR")
catch_ad$Dis_MeanWeight_ad_kg[rmv] <- 0
catch_ad$Dis_CANUM_ad[rmv] <- 0
catch_ad$Dis_ad_t[rmv] <- 0



# 2.X. Load inflation rates and correct values (LATER) ===================

# correct monetary values by inflation rate up to 2010
# make sure to get the eurostat rates and "clean" the
# countries not required and recode. Also make sure to use the relevant
# year for referencing. In this case it's 2010.
# TODO - CLU

inflation_correction <- FALSE

if(inflation_correction) {
  infRates.orig <- read.csv("C:\\Users\\clul\\Documents\\GitHub\\2012-WKBEM\\wgmixfish\\data\\economics\\infRate_update.csv", row.names=1)
  names(infRates.orig) <- substr(names(infRates.orig), 2,5)
  infRates <- infRates.orig[,as.character(2001:2010)]
  infRates <- apply(1+infRates[,ncol(infRates):1]/100, 1, cumprod)
  infRates <- rbind(infRates.orig[, "2011"], infRates)
  #2011 inflation rate will be 1, the reference year
  infRates[1,] <- 1
  infRates <- rbind(1/(1+infRates.orig[, "2012"]/100), infRates)
  rownames(infRates) <- 2012:2001
  infRat <- c(infRates)
  infRates <- as.data.frame(expand.grid(dimnames(infRates)))
  infRates$infRat <- infRat
  names(infRates) <- c('year', 'ctr', 'infRat')

  # merge with catch dataset, use transform and reformat
  catch$ctr <- unlist(lapply(strsplit(catch$fleet, "_"), "[", 1))
  catch <- merge(catch, infRates, all.x=T)
  catch <- transform(catch, value=value*infRat)
}



# 2.2. Load stocks ============================

# Stock.Names <- sort(unique(catch$stock))
Stock.Names <-unlist(strsplit(list.files(stock.path), ".RData")) #2020 YV reduce the stocks to the ones used in advice
#Stock.Names <-Stock.Names[Stock.Names!="SOL-EC"]#2020 YV reduce the stocks to the ones used in advice


list.files(stock.path)
#Stock.Names <- Stock.Names[!Stock.Names=="NEPOTH-NS"]

Stock.Names <- Stock.Names[which(Stock.Names %in% c(AD.Stocks, AA.Stocks))]

#reducing the effort and catch datasets
catch <- subset(catch, stock %in% Stock.Names)
unique(catch$stock)
catch_ad <- subset(catch_ad, stock %in% Stock.Names)
effcap <- subset(effcap,ID %in% unique(catch$ID))

#specific fix CLU - remove ANF for now, 26/05
#FIXME? effcap <- effcap[!(effcap$fleet=="FR_Otter>=40" & effcap$metier=="OTH"),]


#FIXME?
#identifying the stocks with no discards
# nodiscards_stocks <- vector()
# di.lst <- lapply(wg.stock,discards)
# for (s in st.lst) if(all(di.lst[[s]]==0)) nodiscards_stocks <- c(nodiscards_stocks,s)
#  #!! WATCH IT

st.lst <- as.list(Stock.Names)
names(st.lst) <- Stock.Names

stocks <- lapply(st.lst, function(x) {
  tmp <- load(paste0(stock.path,"/",x,".RData"))
  res <- get(tmp)
  name(res) <- x
  print(x)
  return(res)
})


range <- lapply(stocks, function (x) x@range)
plusgroup <- unlist(lapply(range,function(x) x["plusgroup"]))
names(plusgroup) <- st.lst





# 2.3. Compare Stock and Intercatch derived land and disc --------

#!quick check - all stocks
fleet.sum <- aggregate(catch[,c("land","disc")],list(stock=catch$stock,year=catch$year),sum,na.rm=T)
fleet.sum.ad <- aggregate(catch_ad[,c("Lan_ad_t","Dis_ad_t")],
  list(stock=catch_ad$stock, year=catch_ad$year),sum,na.rm=T)

merge(fleet.sum, fleet.sum.ad, all.x = TRUE)
#fleet.sum <- fleet.sum[fleet.sum$stock %in% Analytical.Stocks,]

stock.land <- lapply(stocks, function(x) {
  if (range(x)["minyear"]>years[1]) yrs <-range(x)["minyear"]:range(x)["maxyear"] else yrs <- years
  res <- as.data.frame(landings(x)[,ac(yrs)])[,c("year","data")]
  res$stock <- name(x)
  return(res)
})

stock.land <- eval(parse(text=paste('rbind(stock.land[[',paste(seq(length(stock.land)),collapse=']],stock.land[['),']])',sep='')))
names(stock.land)[which(names(stock.land)=="data")]<-"WG.land"

stock.disc <- lapply(stocks, function(x) {
  if (range(x)["minyear"]>years[1]) yrs <-range(x)["minyear"]:range(x)["maxyear"] else yrs <- years
  res<-as.data.frame(discards(x)[,ac(yrs)])[,c("year","data")]
  res$stock <- name(x)
  return(res)})
stock.disc <- eval(parse(text=paste('rbind(stock.disc[[',paste(seq(length(stock.disc)),collapse=']],stock.disc[['),']])',sep='')))
names(stock.disc)[which(names(stock.disc)=="data")]<-"WG.disc"

comp. <- merge(stock.land, stock.disc)
comp <- merge(comp.,fleet.sum)
comp$ratio.l <- round(comp$land/comp$WG.land,2)
comp$ratio.d <- round(comp$disc/comp$WG.disc,2)
comp$diffland <- round(comp$land-comp$WG.land,2)
comp$rel.land <- round(comp$diffland / comp$WG.land,3)* 100

#print(comp[order(comp$stock,comp$year),c("year","stock","WG.land","WG.disc","ratio.l","ratio.d","land","disc","diffland")])

subset(comp[order(comp$stock,comp$year),c("year","stock","WG.land","WG.disc","ratio.l","ratio.d","land","disc","diffland", "rel.land")],year==years[length(years)])
#this is really not good for WoS

##test for whiting
#whg. <-   catch[catch$stock=="WHG",]
#whg.$country <- substr(whg.$fleet,1,2)
#whg. <- aggregate(whg.[,c("land","disc")],list(country=whg.$country,year=whg.$year),sum,na.rm=T)
#whg. <- tapply(whg.$land,list(country=whg.$country,year=whg.$year),sum,na.rm=T)
#
#WoS cod is very poor so we raise the data


fname <- paste0(res.path,"/diagnostics/03a_comparison_ICES_FleetData_",ver03,".csv")
write.csv(comp[order(comp$stock,comp$year),c("year","stock","WG.land","WG.disc","ratio.l","ratio.d","land","disc","diffland")],
      file=fname,row.names=FALSE)
print(fname)

#rewritten not as a function!
ef.type <- "KW"





# 3. FLFleets construction ------------------------------------------------

# 3.1. From prepared aggregated data ==========================
print("Analytical stocks first... ")
  fleet.names <- as.list(unique(effcap$fleet))
  names(fleet.names)  <- unlist(fleet.names)
  stock.names <- as.list(toupper(names(stocks)))
  names(stock.names)  <- unlist(stock.names)
  yr.range    <- as.character(unique(effcap$year))
  rge.fl  <- as.numeric(range(yr.range))


# make empty slots
fq <- FLQuant(0,dimnames=list(year=yr.range))
dat <- FLQuant(0,dimnames=list(year=yr.range),units="tonnes",quant="age")

fqage_stock <- FLQuants(lapply(st.lst, function(x) {
  FLQuant(0,dimnames=list(age=range[[x]]["min"]:range[[x]]["max"],year=yr.range))
}))



print("making the fleets...")

# fl.names<-fleet.names[!fleet.names=="OTH_OTH"]
fl.nam <- fleet.names

fleets <- FLFleets(lapply(fl.nam, function(fl){ #for (fl in fleet.names){
  print(fl)

  # effort and capacity (number of boats)
  subfl <- subset(effcap,fleet==fl)
  eff <- fq
  cap <- fq
  if (ef.type=="KW") agg <-aggregate(subfl$kwdays, by=list(subfl$year),FUN=sum) else
    if (ef.type=="DS") agg <-aggregate(subfl$tot_days, by=list(subfl$year),FUN=sum)
  eff[,as.character(agg$Group.1)] <- agg$x
  eff[is.na(eff)] <- 0
  if (ef.type=="KW") units(eff) <- "000 kWdays" else
    if (ef.type=="DS") units(eff) <-"Vesseldays"

  agg <-aggregate(subfl$num_vessels, by=list(subfl$year),FUN=max, na.rm = TRUE)
  cap[,as.character(agg$Group.1)] <- agg$x
  cap[is.na(cap)]<-0
  units(cap) <- "number of vessels"

  #  flq.fl  <-FLQuant(dimnames=dimnames(eff),units="NA")
  met.nam <- as.list(unique(subfl$metier))
  names(met.nam) <- unlist(met.nam)

  metiers <- FLMetiers(lapply(met.nam, function(met){ # for(met in met.nam){
    print(paste(fl, met, sep=" | "))
    submet <- subset(catch,fleet==fl & metier==met)
    effmet <- fq

    if (ef.type=="KW") agg <-aggregate(subset(subfl,metier==met)$kwdays, by=list(subset(subfl,metier==met)$year),FUN=sum) else
      if (ef.type=="DS") agg <-aggregate(subset(subfl,metier==met)$tot_days, by=list(subset(subfl,metier==met)$year),FUN=sum)

    # effort share
    effmet[,as.character(agg$Group.1)] <- agg$x
    effmet[is.na(effmet)]<-0
    effsh <- effmet/eff

    stk.nam <- as.list(unique(submet$stock))
    names(stk.nam) <- unlist(stk.nam)

    if(length(stk.nam)>0) {

      catches  <- FLCatches(lapply(stk.nam,function(stk){  # for (stk  in stk.nam){
        print(paste(fl, met, stk, sep=" | "))
        substk <- subset(submet, stock==stk)
        if(stk %in% AD.Stocks){
          substk_ad <- subset(catch_ad, fleet==fl & metier==met & stock==stk)
        }

        rgy <- sort(unique(substk$year))
        rgy <- ac(rgy)

        # aa & ad
        la  <- dat; units(la) <- "t"
        di  <- dat; units(di) <- "t"

        la[,rgy]  <- substk$land
        di[,rgy]  <- substk$disc
        la[is.na(la)]<-0
        di[is.na(di)]<-0

        #FIXME - is this needed here? Similar fix not done for di.n
        # if (stk %in% nodiscards_stocks) di[]<-0

        ca <- la + di

        # ad only
        if (stk %in% AD.Stocks){
          # age-based
          substk.ages <- sort(as.numeric(substk_ad$age))
          rga <- seq(from=range[[stk]]['min'],to=(range[[stk]]['plusgroup']-1))
          rga <- rga[which(rga %in% unique(substk.ages))]
          rga <- ac(rga)
          pg <- as.numeric(range[[stk]]['plusgroup'])

          la.n    <- fqage_stock[[stk]]; units(la.n) <- "10^3"
          di.n    <- fqage_stock[[stk]]; units(di.n) <- "10^3"
          ca.n    <- fqage_stock[[stk]]; units(ca.n) <- "10^3"
          la.wt    <- fqage_stock[[stk]]; units(la.wt) <- "kg"
          di.wt    <- fqage_stock[[stk]]; units(di.wt) <- "kg"
          ca.wt    <- fqage_stock[[stk]]; units(ca.wt) <- "kg"
          catchability    <- fqage_stock[[stk]]; units(catchability) <- ""

          lanum <- tapply(substk_ad$Lan_CANUM_ad, list(age=as.numeric(substk_ad$age), year=substk_ad$year), mean, na.rm=T)
          lanum <- replace(lanum, is.na(lanum), 0) # replace NaN with zero
          dinum <- tapply(substk_ad$Dis_CANUM_ad, list(age=as.numeric(substk_ad$age), year=substk_ad$year), mean, na.rm=T)
          dinum <- replace(dinum, is.na(dinum), 0) # replace NaN with zero

          wela <- tapply(substk_ad$Lan_MeanWeight_ad_kg, list(age=as.numeric(substk_ad$age), year=substk_ad$year), mean, na.rm=T)
          wela <- replace(wela, wela == 0 | is.na(wela), NaN) # important that zeros are removed for projecting mean weights forward
          lasop <- tapply(substk_ad$Lan_ad_t, list(age=as.numeric(substk_ad$age), year=substk_ad$year), mean, na.rm=T)
          lasop <- replace(lasop, is.na(lasop), 0) # replace NaN with zero
          # lanum * wela

          wedi <- tapply(substk_ad$Dis_MeanWeight_ad_kg, list(age=as.numeric(substk_ad$age), year=substk_ad$year), mean, na.rm=T)
          wedi <- replace(wedi, wedi == 0 | is.na(wedi), NaN) # important that zeros are removed for projecting mean weights forward
          disop <- tapply(substk_ad$Dis_ad_t, list(age=as.numeric(substk_ad$age), year=substk_ad$year), mean, na.rm=T)
          disop <- replace(disop, is.na(disop), 0) # replace NaN with zero
          # dinum * wedi

          # substitute non-plusgroup ages
          la.n[rga,rgy] <- lanum[rga,rgy]
          di.n[rga,rgy] <- dinum[rga,rgy]
          la.wt[rga,rgy] <- wela[rga,rgy]
          di.wt[rga,rgy] <- wedi[rga,rgy]

          # calculate plusgroup values (weighted mean, or sop/n)
          #! Can this be simplified with 'drop'?
          if(length(rgy)>1 & max(substk.ages)!=pg) {
            la.n[ac(pg),rgy] <- apply(lanum[!rownames(lanum) %in% rga,rgy], 2, sum, na.rm=T)
            di.n[ac(pg),rgy] <- apply(dinum[!rownames(dinum) %in% rga,rgy], 2, sum, na.rm=T)

            la.wt[ac(pg),rgy] <- apply(lasop[!rownames(lasop) %in% rga,rgy], 2, sum, na.rm=T)/
              la.n[ac(pg),rgy]
            di.wt[ac(pg),rgy] <- apply(disop[!rownames(disop) %in% rga,rgy], 2, sum, na.rm=T)/
              di.n[ac(pg),rgy]

          } else {
            if(length(rgy)==1 & max(substk.ages)!=pg) {
              la.n[ac(pg),rgy] <- sum(lanum[!rownames(lanum) %in% rga,rgy],na.rm=T)
              di.n[ac(pg),rgy] <- sum(dinum[!rownames(dinum) %in% rga,rgy],na.rm=T)

              la.wt[ac(pg),rgy] <- sum(lasop[!rownames(lasop) %in% rga,rgy],na.rm=T)/
                la.n[ac(pg),rgy]
              di.wt[ac(pg),rgy] <- sum(disop[!rownames(disop) %in% rga,rgy],na.rm=T)/
                di.n[ac(pg),rgy]

            } else {
              if(length(rgy)>1 & max(substk.ages)==pg) {
                la.n[ac(pg),rgy] <- lanum[rownames(lanum) %in% ac(pg),rgy]
                di.n[ac(pg),rgy] <- dinum[rownames(dinum) %in% ac(pg),rgy]

                la.wt[ac(pg),rgy] <- lasop[rownames(lasop) %in% ac(pg),rgy]/la.n[ac(pg),rgy]
                di.wt[ac(pg),rgy] <- disop[rownames(disop) %in% ac(pg),rgy]/di.n[ac(pg),rgy]
              }
            }
          }

          # replace NAs and calculate catch
          la.n[is.na(la.n)] <- 0
          di.n[is.na(di.n)] <- 0
          ca.n <- la.n + di.n

          # replace zeros in mean weights slots
          la.wt[is.na(la.wt)] <- 0
          di.wt[is.na(di.wt)] <- 0

          # catchability/selectivity
          # Ftot*(Ci/Ctot)/f? why not plusyear? (f*q=F)
          # FIXME? I have added the plusgroup to catchability calculation
          # tmp <- harvest(wg.stock[[S]])[rga,rgy]*(ca.n[rga,rgy]/catch.n(wg.stock[[S]])[rga,rgy])
          # catchability[rga,rgy] <- tmp%/%effmet[,rgy]
          tmp <- harvest(stocks[[stk]])[,rgy]*(ca.n[,rgy]/catch.n(stocks[[stk]])[,rgy])
          catchability[,rgy] <- tmp%/%effmet[,rgy]

          ## C/(f*N) (FLBEIA)
          # catchability[,rgy] <- (ca.n[,rgy])/(effmet[,rgy]%*%stock.n(wg.stock[[S]])[,rgy])
          # catchability2 <- catchability
          # plot(catchability, catchability2, col=jetPal(dim(wg.stock[[S]])[1]), pch=20); abline(0,1)
          # legend("bottomright", legend=seq(dim(wg.stock[[S]])[1]), col=jetPal(dim(wg.stock[[S]])[1]), pch=20, title="age group")
          # plot(1:6, catchability[,1], t="n", ylim=c(0, max(catchability, na.rm=T)))
          # for(i in seq(ncol(catchability))){lines(1:6, catchability[,i], col=jetPal(6)[i])}
          # for(i in seq(ncol(catchability2))){lines(1:6, catchability2[,i], lty=2, col=jetPal(6)[i])}

          #if effmet=0 one year, q is inf
          catchability[!is.finite(catchability)] <- NA
          #ct.sel <-dat
          #ct.sel[] <- 1
          #ld.sel <- la/catc
          la.sel <- la.n/ca.n

          #di.sel <- di/catc
          di.sel <- di.n/ca.n
          la.sel[is.na(la.sel)]<-0
          di.sel[is.na(di.sel)]<-0

          #price
          pr <- fqage_stock[[stk]]
          # pr[,as.character(subst$year)]  <- (subst$value/subst$land)/1000
          pr[,as.character(substk$year)]  <- rep(substk$value/substk$land,each=dim(pr)[1])  #price in euros/tons
          units(pr) <- "Euros/tonnes"

      } else { #non age-disaggregated data

        #catchability/selectivity
        catchability <- ca*(fbar(stocks[[stk]])[,yr.range]/effmet)/catch(stocks[[stk]])[,yr.range]
        #fbar(wg.stock[[S]])[,yr.range]*(catc/wg.stock[[S]]@catch[,yr.range])/effmet
        #if effmet=0 one year, q is inf
        catchability[!is.finite(catchability)] <- NA
        ct.sel <- dat
        ct.sel[] <- 1
        la.sel <- la/ca

        di.sel <- di/ca
        la.sel[is.na(la.sel)]<-0
        di.sel[is.na(di.sel)]<-0

        #price
        pr <- dat
        # pr[,as.character(subst$year)]  <- (subst$value/subst$land)/1000
        pr[,as.character(substk$year)]  <- substk$value/substk$land  #price in euros/tons
        units(pr) <- "Euros/tonnes"
      }

      #range
      rg. <- range[[stk]]
      rg.["minyear"] <- years[1]
      rg.["maxyear"] <- years[length(years)]

      units(la.sel) <- "NA"
      units(di.sel) <- "NA"
      units(catchability) <- "NA"


      # FLCatch object
      if (stk %in% AD.Stocks) {
        res <- FLCatch(range=rg., name=stk, landings=la, discards=di,
          landings.n = la.n, landings.wt=la.wt, discards.n=di.n, discards.wt=di.wt,
          catch.q=catchability, landings.sel=la.sel, discards.sel=di.sel, price=pr)
        #! re-compute landings, discards (this may eventually be fixed in program 2)
        res@landings <- computeLandings(res)
        res@discards <- computeDiscards(res)

      } else {
        res <- FLFleet::FLCatch(range=rg., name=stk, landings=la, discards=di,
          catch.q=catchability, landings.sel=la.sel, discards.sel=di.sel, price=pr)
      }

      return(res)
      })) # end of FLCatches

    } else { #FIXME - Is this really used? Would seem to be overwritten by next fl / met that contains no coinciding species in catches
      catches <- FLCatch(landings=dat, name="PleaseRemoveMeLater")  #NA Catch
    }


    metier <- FLMetier(catches=catches, name=met)
    quant(metier@vcost) <- "quant"
    quant(metier@effshare) <- "quant"

    units(effsh) <- "proportion"
    metier@effshare <- effsh

    return(metier)
    })) # end of FLMetiers

  fleet <- FLFleet(metiers=metiers, name=fl, effort=eff, capacity=cap)
  #fl <- new("FLFleet", metiers=metiers, name=Fl, effort=eff, capacity=cap,fcost=fq,crewshare=fq)
  fleet@range[c("minyear","maxyear")]  <- rge.fl
  return(fleet)
}))

fleets.int <- fleets



# mis
landings(stocks[["TUR"]])[,"2018"]
discards(stocks[["TUR"]])[,"2018"]

tmp_ad <- aggregate(Lan_ad_t ~ fleet, data = catch_ad,
  subset = year == "2018" & stock == "TUR", FUN = sum, na.rm = TRUE)
sum(tmp_ad$Lan_ad_t)

tmp_ad <- aggregate(Dis_ad_t ~ fleet, data = catch_ad,
  subset = year == "2018" & stock == "TUR", FUN = sum, na.rm = TRUE)
sum(tmp_ad$Dis_ad_t)

tmp_aa <- aggregate(land ~ fleet, data = catch,
  subset = year == "2018" & stock == "TUR", FUN = sum, na.rm = TRUE)
sum(tmp_aa$land)

tmp_aa <- aggregate(disc ~ fleet, data = catch,
  subset = year == "2018" & stock == "TUR", FUN = sum, na.rm = TRUE)
sum(tmp_aa$disc)




source("bootstrap/software/functions/fleetsSum.R")
DF <- fleetsSum(fleets = fleets, yrs = ac(datayear))
head(DF)

(agg1 <- aggregate(data ~ year + stock, data = DF, sum, na.rm=TRUE))


# stk <- "TUR"
# (agg2 <- aggregate(data ~ year + fleet, data = DF, subset = stock==stk,
#   FUN = sum, na.rm=TRUE))
# sum(agg2$data)
# stocks[[stk]]@catch[,ac(2018)]
#
# stk <- "COD-NS"
# (agg2 <- aggregate(data ~ year + fleet, data = DF, subset = stock==stk,
#   FUN = sum, na.rm=TRUE))
# sum(agg2$data)
# stocks[[stk]]@catch[,ac(2018)]
#
# stk <- "LEM"
# (agg2 <- aggregate(data ~ year + fleet, data = DF, subset = stock==stk,
#   FUN = sum, na.rm=TRUE))
# sum(agg2$data)
# stocks[[stk]]@catch[,ac(2018)]
#
# stk <- "LIN"
# (agg2 <- aggregate(data ~ year + fleet, data = DF, subset = stock==stk,
#   FUN = sum, na.rm=TRUE))
# sum(agg2$data)
# stocks[[stk]]@catch[,ac(2018)]
#
# stk <- "ANF"
# (agg2 <- aggregate(data ~ year + fleet, data = DF, subset = stock==stk,
#   FUN = sum, na.rm=TRUE))
# sum(agg2$data)
# stocks[[stk]]@catch[,ac(2018)]
#



# 3.2. Add "Other" Fleet and top up when MIXFISH catches < ICES stock total ==============================

print("OTH fleet ...")

if("OTH_OTH" %in% names(fleets)){
  effort(fleets[["OTH_OTH"]])[] <- 1000
  fleets[["OTH_OTH"]]@metiers[["OTH"]]@effshare[] <- 1

  oth.catches <- FLCatches(lapply(names(stocks), function(stk) {
    print(stk)

    flcatch <- fleets[["OTH_OTH"]]@metiers[["OTH"]]@catches[[stk]]
    landings(flcatch)[is.na(landings(flcatch))] <- 0
    discards(flcatch)[is.na(discards(flcatch))] <- 0

    #landings
    wg. <- landings(stocks[[stk]])[,yr.range]
    wg.[is.na(wg.)] <- 0
    fl. <- fq
    for(fl in seq(fleets)){
      for(met in seq(fleets[[fl]]@metiers)){
        if(stk %in% names(fleets[[fl]]@metiers[[met]]@catches)){
          la <- landings(fleets[[fl]]@metiers[[met]], catch=stk)
          la[is.na(la)] <- 0
          fl. <- fl. + la
          rm(la)
        }
      }
    }

    units(fl.) <- "t"
    res <- wg. %-% fl. # ICES minus FLEETS
    res[res<0] <- 0 # if ICES is greater, then replace with zero
    landings(flcatch) <- landings(flcatch) + res
    units(landings(flcatch)) <- "t"

    #discards
    wg. <- discards(stocks[[stk]])[,yr.range]
    wg.[is.na(wg.)] <- 0
    fl. <- fq
    for(fl in seq(fleets)){
      for(met in seq(fleets[[fl]]@metiers)){
        if(stk %in% names(fleets[[fl]]@metiers[[met]]@catches)){
          di <- discards(fleets[[fl]]@metiers[[met]], catch=stk)
          di[is.na(di)] <- 0
          fl. <- fl. + di
          rm(di)
        }
      }
    }
    units(fl.) <- "t"
    res <- wg. %-% fl.
    res[res<0] <- 0
    discards(flcatch) <- discards(flcatch) + res
    units(discards(flcatch)) <- "t"

    if (stk %in% AD.Stocks){
      # assume mean weights from stock object
      landings.wt(flcatch)[,yr.range] <- landings.wt(stocks[[stk]])[,yr.range]
      discards.wt(flcatch)[,yr.range] <- discards.wt(stocks[[stk]])[,yr.range]

      landingsFrac.ad <- apply(landings.n(stocks[[stk]]) *
        landings.wt(stocks[[stk]]), MARGIN = 2:6,
        FUN = function(x){x/sum(x, na.rm=TRUE)})
      discardsFrac.ad <- apply(discards.n(stocks[[stk]]) *
        discards.wt(stocks[[stk]]), MARGIN = 2:6,
        FUN = function(x){x/sum(x, na.rm=TRUE)})

      # calculate numbers
      landings.n(flcatch)[,yr.range] <- landings(flcatch)[,yr.range] %*% landingsFrac.ad[,yr.range] %/% landings.wt(flcatch)[,yr.range]
      discards.n(flcatch)[,yr.range] <- discards(flcatch)[,yr.range] %*% discardsFrac.ad[,yr.range] %/% discards.wt(flcatch)[,yr.range]

      # remove NAS in landings.n and discards.n
      landings.n(flcatch)[is.na(landings.n(flcatch))] <- 0 # replace NAs with zero
      discards.n(flcatch)[is.na(discards.n(flcatch))] <- 0 # replace NAs with zero

      landings(flcatch); computeLandings(flcatch)
      discards(flcatch); computeDiscards(flcatch)

      # #landings.n
      # wg. <- landings.n(stocks[[stk]])[,yr.range]
      # wg.[is.na(wg.)] <- 0
      # fl. <- fqage_stock[[stk]]
      # for(fl in seq(fleets)){
      #   for(met in seq(fleets[[fl]]@metiers)){
      #     if(stk %in% names(fleets[[fl]]@metiers[[met]]@catches)){
      #       la.n <- landings.n(fleets[[fl]]@metiers[[met]], catch=stk)
      #       la.n[is.na(la.n)] <- 0
      #       fl. <- fl. + la.n
      #     }
      #   }
      # }
      # units(fl.) <- "10^3"
      # res <- wg. %-% fl.
      # res[res<0] <- 0
      # landings.n(flcatch) <- landings.n(flcatch) + res
      # units(landings.n(flcatch)) <- "10^3"
      #
      # #discards.n
      # wg. <- discards.n(stocks[[stk]])[,yr.range]
      # wg.[is.na(wg.)] <- 0
      # fl. <- fqage_stock[[stk]]
      # for(fl in seq(fleets)){
      #   for(met in seq(fleets[[fl]]@metiers)){
      #     if(stk %in% names(fleets[[fl]]@metiers[[met]]@catches)){
      #       di.n <- discards.n(fleets[[fl]]@metiers[[met]], catch=stk)
      #       di.n[is.na(di.n)] <- 0
      #       fl. <- fl. + di.n
      #     }
      #   }
      # }
      # units(fl.) <- "10^3"
      # res<-wg. %-% fl.
      # res[res<0] <- 0
      # discards.n(flcatch) <- discards.n(flcatch) + res
      # units(discards.n(flcatch)) <- "10^3"


      # catchability/selectivity
      tmp <- harvest(stocks[[stk]])[,yr.range]*((landings.n(flcatch) + discards.n(flcatch))/
              catch.n(stocks[[stk]])[,yr.range])
      catch.q(flcatch) <- tmp %/% effort(fleets[["OTH_OTH"]])
      catch.q(flcatch)[!is.finite(catch.q(flcatch))] <- NA

      landings.sel(flcatch) <- landings.n(flcatch)/(landings.n(flcatch) + discards.n(flcatch))
      discards.sel(flcatch) <- discards.n(flcatch)/(landings.n(flcatch) + discards.n(flcatch))

      landings.sel(flcatch)[is.na(landings.sel(flcatch))] <- 0
      discards.sel(flcatch)[is.na(discards.sel(flcatch))] <- 0

      landings(flcatch)
      discards(flcatch)
      computeLandings(flcatch)
      computeDiscards(flcatch)


    } else {  #non age disagrregated
      catches <- discards(flcatch) + landings(flcatch)
      #catchability/selectivity
      catch.q(flcatch) <- catches*(fbar(stocks[[stk]])[,yr.range]/effort(fleets[["OTH_OTH"]]))/catch(stocks[[stk]])[,yr.range]
        #fbar(wg.stock[[st]])[,yr.range]*(catches/catch(wg.stock[[st]])[,yr.range])/effort(fleets[["OTH_OTH"]])
      #ct.sel <-dat
      #ct.sel[] <- 1
      landings.sel(flcatch) <- landings(flcatch)/catches
      discards.sel(flcatch) <- discards(flcatch)/catches

    }

    return(flcatch)
  }))

  fleets[["OTH_OTH"]]@metiers[["OTH"]]@catches <- oth.catches

}



# removing the fake catch for those fleets without analytical stock
fleets <- lapply(fleets, function(x) {
  print(name(x))
  x@metiers <- as(lapply(x@metiers,function(x1) {
    print(x1@name)
    ma <- which(!names(x1@catches)=="PleaseRemoveMeLater")
    ma <- ma[!is.na(ma)]
    flc<-FLCatches()
    for (sp in seq(length(ma))) {
      flc[[sp]] <- x1@catches[[ma[sp]]]
      #name(flc[[sp]])<-tolower(name(flc[[sp]]))
    }
    names(flc)<-names(x1@catches)[ma]
    x1@catches <- flc
    return(x1) }),"FLMetiers")
  return(x)})


fleets.int2 <- fleets


print("F check...")
Fbar <- lapply(stocks,fbar)


#check
F.check<-lapply(st.lst, function(st) {

if (AgeDisaggregated==TRUE) wg. <- harvest(stocks[[st]])[,yr.range] else wg. <- fbar(stocks[[st]])[,yr.range]

  flq <- FLQuant(0,dimnames=dimnames(wg.))

  fl. <-lapply(fleets, function(x) {
    if(st %in% spp(x)) {
      e. <- effort(x)
      mt. <- lapply(x@metiers,function(x1) {
        if (st %in% names(x1@catches)) {
          eff <- e. * x1@effshare
          Q   <- catch.q(x1)[[st]]
          #sel <- landings.sel(x1)[[st]]
          sel <- catch.sel(x1)[[st]]
          #harv <- sweep(sweep(sel,2:6,eff,FUN="*"),2:6,Q,FUN="*")
          harv <- sweep(sel,2:6,eff,FUN="*") %*% Q
        } else  harv<-flq  #})
        harv[is.na(harv)] <- 0
        return(harv)})
      mt. <- Sums(mt.)
    } else mt. <- flq
  })
  fl. <- Sums(as(fl.,"FLQuants"))

  res <- wg. %-% fl. #sweep(wg.,1:6,fl.,FUN="-")
  print(st)
  units(res) <- "NA"
  return(res)
})


print(F.check)

# create diagnostic plot of F.check !!!!




# a final comparison ------------------------------------------------------

DF <- fleetsSum(fleets = fleets, yrs = ac(datayear))
head(DF)

comp2 <- subset(comp, year == datayear)
comp2 <- comp2[,c("year", "stock", "WG.land", "WG.disc", "land", "disc")]

(agg1 <- aggregate(data ~ year + stock + variable , data = DF, sum, na.rm=TRUE))
tmp <- reshape2::dcast(agg1, formula = year + stock ~ variable, value.var = "data")

comp2 <- merge(x = comp2, y = tmp)

comp2$ratio.l <- round(comp2$landings/comp2$WG.land,2)
comp2$ratio.d <- round(comp2$discards/comp2$WG.disc,2)
comp2$diffland <- round(comp2$landings-comp2$WG.land,2)
comp2$rel.land <- round(comp2$diffland / comp2$WG.land,3)* 100
comp2

subset(comp2[order(comp$stock,comp$year),c("year","stock","WG.land","WG.disc","ratio.l","ratio.d","landings","discards","diffland", "rel.land")],year==years[length(years)])





# 3.3. Save FLFleets object =================================

save(fleets, file=file.path(fleet.path, paste0("03_",ver01,"_",ver03,"_",ef.type, ".RData",sep="")) )


#into Data as well
# save(fleets, file=paste0("./North_Sea/data/fleets/03_",ver03,"_",ef.type, ".RData"))



#}  #effort type

export <- FALSE

if(export==TRUE) {

### EXPORT FILE FOR PAULS FIGURE HAVING ALL CATCH INFO IN
land <- slot.fleet(fleets,"landings")
names(land)[which(names(land)=="qname")] <- "stock"
disc <- slot.fleet(fleets,"discards")
names(disc)[which(names(disc)=="qname")] <- "stock"
export <- merge(land,disc)
export$land <- export$landings
export$disc <- export$discards
export$ID <- seq(nrow(export))

#revenue
rev. <-lapply(fleets, function(x) {
  mt. <- lapply(x@metiers,function(x1) {
    st. <- lapply(x1@catches,function(x2) {
    res<-as.data.frame(revenue(x2))
    res$fleet <-name(x)
    res$metier <- x1@name
    res$stock <- x2@name
    #if (!is.na(x1@gear)) res$metier <- x1@gear else res$metier <- x1@name
    res})
  st. <- eval(parse(text=paste('rbind(st.[[',paste(seq(length(st.)),collapse=']],st.[['),']])',sep='')))
  })
  mt. <- eval(parse(text=paste('rbind(mt.[[',paste(seq(length(mt.)),collapse=']],mt.[['),']])',sep='')))
})
rev. <- eval(parse(text=paste('rbind(rev.[[',paste(seq(length(rev.)),collapse=']],rev.[['),']])',sep='')))
names(rev.)[which(names(rev.)=="data")] <- "value"

export <- merge(export,rev.)
export <- export[,c("fleet","metier","stock","year","land","disc","value","ID")]
write.csv(export,file=paste(fleet.path,"/03b_Export_AllCatchesIncludingOTHOTHStockLevel_",ver03,".csv",sep=""))

}
