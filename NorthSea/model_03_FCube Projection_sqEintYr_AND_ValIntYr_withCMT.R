###################################################################################
##     FLFCUBE SCRIPT FOR RUNNING FCUBE RUNS ON VARIOUS TAC SCENARIOS            ##
##                                                                               ##
##                         North Sea                                             ##
##                                                                               ##
##      Progr.3 - Fcube projection on several deterministic years                ##
##                                                                               ##
##  Running with fleets and stocks data up to 2017, Advice 2019                  ##
##                                                                               ##
# Initial Author : Clara Ulrich, DTU Aqua <clu@aqua.dtu.dk>                     ##
## Further Contributors: Youen Vermard, Paul J. Dolder,...
# And ICES WGMIXFISH colleagues
##                                                                               ##
## Runs with R 5.1                                                               ##
##                                                                               ##
###################################################################################

#October 2018: Test for inclusion of actual quotas from FIDES, reproducing and including
#NWWAC's Choke Mitigation Tool (CMT) and


### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
gc(); graphics.off()

#ver1 <- "03_Fcube_Projection"
#ver.datetime   <- "12/10/2016";

#(choke type 1 sensus NWWAC) in the calculation of min scenario
ver.datetime   <- "27/10/2020";

options(stringsAsFactors = FALSE, scipen=100)

# library(icesTAF)
# taf.library(FLCore)
# library(FLash)
# # library(FLAssess)
# taf.library(FLFleet)
# library(ggplotFL)



source('bootstrap/software/functions/FLFcube_FLCore_withFIDES.R')
source('bootstrap/software/functions/FLFcube_FLCore_R31.R')
source('bootstrap/software/functions/remove_validity_FLFleet.R')
# source('bootstrap/software/functions/fwdF3 function.r')  # if fixed, FLash can be removed



##-----------------------------------------------------------------------------
##  GET INPUTS
##-----------------------------------------------------------------------------
res.path <- "model"

ver_A <- "03_Fcube_ValIntY"        #in this run we explore the use of removing the non-limiting quota [CMT]  and Value intermediate Year
ver_B <- "03_Fcube_sqEIntY"        #in this run we explore the use of removing the non-limiting quota [CMT]  and sqE intermediate Year

versions <- c(ver_A,ver_B)

for(ver1 in versions){
  print(ver1)

  load("model/model_settings.Rdata")
  load(paste0(res.path,"/01_Reproduce_The_Advice_2020_keepNYrNow_LO.Rdata"))
  load(paste0(res.path,"/02_Conditioning_Fleets_LO.Rdata"))
  load("data/FIDES_out/Fides_output.RData")

  if (length(LO)>0) Run.nameFC <- paste0(ver1,"_LO") else Run.nameFC <- ver1
  
  #FIDES <- "FALSE"
  
  if (FIDES) Run.nameFC <- paste0(Run.nameFC,"_FIDES")

  print(Run.nameFC)

  #dir.create(file.path("North_Sea/reopening2016/plots", Run.nameFC))
  mkdir(file.path("output/03_FCubeProj/plots", Run.nameFC))       # replaced dir.create by mkdir
  plot.path <- file.path("output/03_FCubeProj/plots",Run.nameFC)

  #unallocated_cod <- FALSE

  # results dataframe

  res.effort <- data.frame()

  # output historical effort
  ef.mt <- slot.fleet(fleets,"effshare")
  ef.mt <- ef.mt[ef.mt$year==yr.assess,]
  ef.fl <- data.frame(scenario="baseline",year=yr.assess,fleet=fleet.names,effort=sapply(fleets,function(x) effort(x)[,yr.assess]))
  ef.mt <- merge(ef.mt,ef.fl)
  ef.mt$effort <- as.numeric(ef.mt$effort)
  ef.mt$efmet <- ef.mt$effort*ef.mt$effshare
  res.effort.met <- ef.mt[, c("year","scenario","fleet","metier","effshare","effort","efmet")]


  #output historic relative stability
  rs_fleet <- slot.fleet(fleets,"landings")
  disc_fleet <- slot.fleet(fleets,"discards")
  rs_fleet <- merge(rs_fleet,disc_fleet)
  rs_fleet <- rs_fleet[rs_fleet$year==yr.assess,]
  rs_fleet$country <- substr(rs_fleet$fleet,1,2)
  names(rs_fleet) <- gsub("qname","stock",names(rs_fleet))

  #corrections
  rs_fleet[rs_fleet$country %in% c("EN","SC"),"country"] <- "UK"
  rs_fleet$stock <- as.character(rs_fleet$stock)
  #rs_fleet <- rs_fleet[-which(rs_fleet$stock=="WHG-NS" & rs_fleet$metier %in%
  #                              c("BT2.7D","GN1.7D","GT1.7D","TR1.7D","TR1.7D")),]
  rs_fleet_nep <- subset(rs_fleet,stock %in% c("NEP10", "NEP33", "NEP34", "NEP5","NEP6","NEP7","NEP8","NEP9","NEPOTH-NS"))
  rs_fleet_nep$stock <- "NEP-NS"
  rs_fleet <- rbind(rs_fleet,rs_fleet_nep)
  rs1 <- aggregate(list(landings=rs_fleet$landings, discards=rs_fleet$discards),list(stock=rs_fleet$stock,country=rs_fleet$country),sum,na.rm=T)
  #names(rs1)[which(names(rs1)=="x")] <- "landings"
  rs2 <- aggregate(list(land.tot=rs1$landings, disc.tot=rs1$discards),list(stock=rs1$stock),FUN="sum",na.rm=T)
  #names(rs2)[which(names(rs2)=="x")] <- "land.tot"
  rs <- merge(rs1,rs2)
  rs$catch <- rs$landings+rs$discards
  rs$lshare <- rs$landings / rs$land.tot
  rs$year <- as.numeric(yr.assess)
  rs$scenario <- "baseline"
  rs_ <- rs
  rs <- rs[,c("year","stock","country","scenario","lshare")]
  rs <- rs[rs$stock!="NEP-NS",]
  rm(rs1,rs2,rs_fleet_nep,disc_fleet)


  ### NB NB here the TAC are considered as landings TAC - unclear if the uplift/topup is already included or not. To check

  ##CMT calculations
  disc_stock <- as.data.frame(cbind(sapply(wg.stock,function(x) discards(x)[,yr.assess]),names(wg.stock)))
  disc_stock[,1] <- as.numeric(as.character(disc_stock[,1]))
  names(disc_stock) <- c("discardsStock","stock")
  land_stock <- as.data.frame(cbind(sapply(wg.stock,function(x) landings(x)[,yr.assess]),names(wg.stock)))
  land_stock[,1] <- as.numeric(as.character(land_stock[,1]))
  names(land_stock) <- c("landingsStock","stock")
  disc_stock <- merge(disc_stock,land_stock)
  rm(land_stock)
  disc_stock <- rbind(disc_stock,c("NEP-NS",sum(disc_stock[disc_stock$stock %in% c("NEP10", "NEP33", "NEP34", "NEP5","NEP6","NEP7","NEP8","NEP9","NEPOTH-NS"),
                                               "discardsStock"],na.rm=T),
                                            sum(disc_stock[disc_stock$stock %in% c("NEP10", "NEP33", "NEP34", "NEP5","NEP6","NEP7","NEP8","NEP9","NEPOTH-NS"),
                                                   "landingsStock"],na.rm=T)))
  disc_stock <-subset(disc_stock, !stock %in% c("NEP10", "NEP33", "NEP34", "NEP5","NEP6","NEP7","NEP8","NEP9","NEPOTH-NS", "NEP32"))
  disc_stock[,2] <- as.numeric(disc_stock[,2])
  disc_stock[,3] <- as.numeric(disc_stock[,3])
  disc_stock$DiscardStockPercent <- disc_stock$discardsStock*100/(disc_stock$discardsStock+disc_stock$landingsStock)
  #discards_ <- tapply(discards_[,1],discards_[,2],sum,na.rm=T)

  #quantifying the discards NOT yet included in the uplift in 2017

  disc_stock$UpliftAlreadyInTAC <- disc_stock$discardsStock
  disc_stock[disc_stock$stock=="COD-NS","UpliftAlreadyInTAC"] <- sum(subset(rs_fleet,stock=="COD-NS" & metier%in%
                                                                          c("TR1.4","TR1.3AN",
                                                                            "GN1.3AN","GN1.4",
                                                                            "GT1.3AN","GT1.4"))$discards,na.rm=T)
  disc_stock[disc_stock$stock=="WHG-NS","UpliftAlreadyInTAC"] <- sum(subset(rs_fleet,stock=="WHG-NS" & metier%in%
                                                                              c("TR1.4","TR1.3AN","TR1.7D",
                                                                                "TR2.7D",
                                                                                "GN1.3AN","GN1.4","GN1.7D",
                                                                                "GT1.3AN","GT1.4","GT1.7D",
                                                                                "BT1.3AN", "BT1.4",
                                                                                "OTB32-69.3AN","OTB32-69.4"))$discards,na.rm=T)
  disc_stock[disc_stock$stock=="PLE-NS","UpliftAlreadyInTAC"] <- sum(subset(rs_fleet,stock=="PLE-NS" & metier%in%
                                                                              c("TR1.4","TR1.3AN",
                                                                                "BT1.3AN", "BT1.4",
                                                                                "OTB32-69.3AN","OTB32-69.4"))$discards,na.rm=T)
  disc_stock[disc_stock$stock=="PLE-EC","UpliftAlreadyInTAC"] <- 0


  UnionTAC <- merge(UnionTAC,disc_stock) #all discards uplift goes to EU, not Norway
  UnionTAC$TACplusUplift <- UnionTAC$initialTAC + UnionTAC$discardsStock-UnionTAC$UpliftAlreadyInTAC
  rm(disc_stock)
  #names(UnionTAC)[2:4] <- c("EEC","TACinitial","TACfinal")

  fi <- merge(fi,UnionTAC,all.x=T)
  fi$qshareInitial <- fi$initialQuota/fi$initialTAC
  fi$qshareFinal <- fi$finalQuota/fi$initialTAC

  rs_$DiscardPercent <- rs_$discards*100/(rs_$landings + rs_$discards)
  rs_ <- rs_[,!names(rs_) %in% c("land.tot","disc.tot")]

  rs_ <- merge(rs_,fi)
  rs_$QuotaPlusUplift <- rs_$TACplusUplift*rs_$qshareInitial

  ggplot(rs_,aes(x=country,y=landings)) +
    geom_point(size=3,pch=2)+facet_wrap(.~stock,scales="free")+
    geom_point(data=rs_,aes(x=country,y=initialQuota),col="red",size=3)+
    geom_point(data=rs_,aes(x=country,y=finalQuota),size=3) +
    ggtitle(paste("landings (triangles) and quotas (red=initial, black=final)",yr.assess))

  ggplot(rs_,aes(x=country,y=lshare)) +
    geom_point(size=3,pch=2)+facet_wrap(.~stock,scales="free")+
    geom_point(data=rs_,aes(x=country,y=qshareInitial),col="red",size=3)+
    geom_point(data=rs_,aes(x=country,y=qshareFinal),size=3) +
    ggtitle(paste("landings (triangles) and quotas (red=initial, black=final) shares",yr.assess))

  ## CMT outcomes

  rs_$LandOnInitial <- rs_$landings/rs_$initialQuota
  rs_$LandOnFinal <- rs_$landings/rs_$finalQuota
  rs_$CatchOnInitial <- rs_$catch/rs_$initialQuota
  rs_$CatchOnFinal <- rs_$catch/rs_$finalQuota
  rs_$SurplusRatio <- rs_$catch/rs_$QuotaPlusUplift
  rs_$SurplusTonnage <- rs_$QuotaPlusUplift-rs_$catch

  ggplot(rs_,aes(x=country,y=SurplusTonnage)) + geom_hline(yintercept=0)+
    geom_col()+facet_wrap(.~stock,scales="free")+
    ggtitle(paste("Choke Mitigation Tool (CMT), Tonnage difference between Quota initial plus uplift and catch by stock",yr.assess))

  ggplot(rs_,aes(x=stock,y=SurplusTonnage)) + geom_hline(yintercept=0)+
    geom_col()+facet_wrap(.~country,scales="free")+
    ggtitle(paste("Choke Mitigation Tool (CMT), Tonnage difference between Quota initial plus uplift and catch by country",yr.assess))

      # a new data.frame for storing all catch info, distinguishing between legal landings (<=TAC),
      # over TAC landings, unallocated and discards (from histrorical discards ratio)

  #TACnow <- TACplusuplift[sort(names(TACplusuplift))]
  TACnow <- TAC.now[sort(names(TAC.now))]
  TACnow <- TACnow[!duplicated(TACnow)]

  All.landings <-c(sapply(dem.st.fwd,function(x) landings(x)[,yr.now]),sapply(nep.st.fwd,function(x) landings(x)[,yr.now]))
  All.landings <-All.landings[sort(names(All.landings))]
  TACnow <- TACnow[names(All.landings)]
  Legal.landings <-pmin(All.landings,TACnow)
  OverTAC.landings <- All.landings-TACnow
  OverTAC.landings[OverTAC.landings<0] <- 0

  disc <- c(sapply(dem.st.fwd,function(x) discards(x)[,yr.now]),sapply(nep.st.fwd,function(x) discards(x)[,yr.now]))
  disc <- disc[sort(names(disc))]

  tonnes <- round(c(TACnow,Legal.landings,disc,OverTAC.landings),2)
  allcatch.dataframe <- data.frame(year=yr.now,sc="baseline",stock=rep(st.names,4),
                                   catch=rep(c("TAC","legal landings","discards","overTAC landings"),each=length(st.names)),
                                   tonnes =as.numeric(tonnes))




  F3ProjectionYear <- as.numeric(yr.TAC) #number of projection years


    #! choice of scenarios

  #scenarios <- c("max","min","cod-ns","sq_E","val")
  scenarios <- c("max","min", "cod-ns", "sq_E","val")
   #scenarios <- c("min")


  Fc.silent <- FALSE #(if true, not all intermediate results are given)

  effort.stock <- list()
  tot.nep.f3.landings <- matrix(nrow=length(nep.lst),ncol=length(now:F3ProjectionYear),dimnames=list(nep.names,now:F3ProjectionYear))


  # Management Plans for yrTAC   <-This must be as in reproduce the advice, but only for the TAC year!

  ctrl.MP.yrTAC <- lapply(ctrl.STF,function(x) fwdControl(x@target[-1,]))



  ##-----------------------------------------------------------------------------
  ## RUNNING SCENARIOS
  ##-----------------------------------------------------------------------------

  if (NepYes ==F) f3.ctrl@target.F <- FLPar(Ftarg.dem.now,params=dem.names) else
                  f3.ctrl@target.F <- FLPar(c(Ftarg.dem.now,Ftarg.nep.now),params=c(dem.names,nep.names))


  for (sc in scenarios) {
    #sc <- "cod"
    #sc <- "min"

    print(paste("scenario:",sc))
    t1<-Sys.time()

    #initialising the projection objects from the original objects
    fl.f3 <- fl.pred
    dem.st.f3 <- dem.stock
    nep.st.f3 <- nep.stock
    f3.ctrl.YY <- f3.ctrl

    for (YY in now:F3ProjectionYear){

    YY <- ac(YY)

        ##-----------------------------------------------------------------------------
        ## 1). INTERMEDIATE YEAR
        ##-----------------------------------------------------------------------------

    cat("----FCUBE YR", YY, "----", "\n")

    if (as.numeric(YY)==now) {

      if (ver1==ver_A) {
            print("Val in Int Yr")
            # fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
            #   effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
            #   return(x)}))
            f3.ctrl.YY@mgmt.year <- as.numeric(YY)
            f3.ctrl.YY@effort.rule <- array("val")

            res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
            if (Fc.silent) fl.f3<-res else {
              fl.f3<- res[[1]]
              effort.stock[[sc]][[YY]] <- res[[2]]}

      } else {
        print("sqE in Int Yr")
        print("status quo in Int Yr")
        fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
          effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
          return(x)}))

      }



    } else {  # Fcube scenarios for TAC Yr

    #scenario status quo compared to last data year
    if (sc=="sq_E") {
    fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
              effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
              return(x)}))


    } else {

    # 1. run Fcube
    f3.ctrl.YY@mgmt.year <- as.numeric(YY)
    f3.ctrl.YY@effort.rule <- array(sc)

    if (FIDES) res <- FLFcube_FIDES(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent,fides=rs_) else
    res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
    if (Fc.silent) fl.f3<-res else {
            fl.f3<- res[[1]]
            effort.stock[[sc]][[YY]] <- res[[2]]
            if (FIDES==TRUE & sc=="min") ChokeCategory1 <- res[[4]]}
    }

  } # end of if on TAC Yr

  ef.fl <- data.frame(scenario=sc,year=YY,fleet=fleet.names,effort=sapply(fl.f3,function(x) effort(x)[,YY]))

  res.effort <- rbind(res.effort,ef.fl)

  # effort.by.metier
  ef.mt <- slot.fleet(fl.f3,"effshare")
  ef.mt <- ef.mt[ef.mt$year==YY,]
  ef.mt <- merge(ef.mt,ef.fl)
  ef.mt$effort <- as.numeric(ef.mt$effort)
  ef.mt$efmet <- ef.mt$effort*ef.mt$effshare
  res.effort.met <- rbind(res.effort.met, ef.mt[, c("year","scenario","fleet","metier","effshare","effort","efmet")])


  # 2. calculate resulting F from new effort

  #table of partial landings F

  # partial landings F by stock and fleet - a list of list of vectors
  partF.land <- lapply(st.lst,function(st) {
  #print(st)
      fl. <-lapply(fl.f3, function(x) {
           if (st %in% unique(unlist(lapply(x@metiers, function(x1) names(x1@catches))))) {
              e.<-effort(x)[,YY]
              mt. <- sapply(x@metiers,function(x1) {
              if (st %in% names(x1@catches)) {
              eff <- e. * x1@effshare[,YY]
              Q   <- catch.q(x1)[[st]][,YY]
              sel <- landings.sel(x1)[[st]][,YY]
              harv <- sweep(sweep(sel,2:6,eff,FUN="*"),2:6,Q,FUN="*")
              #harv <- sweep(eff,2:6,Q,FUN="*")
              } else  harv<-0
              harv[is.na(harv)] <- 0
              return(harv)})
              } else mt. <- 0
              })
              })

  #predicted landings F by stock
  F.land.pred <- sapply(partF.land,function(x) sum(sapply(x,sum)))


  # partial F by stock and fleet - a list of list of vectors
  partF_all <- lapply(st.lst,function(st) {
    #print(st)
    fl. <-lapply(fl.f3, function(x) {
      if (st %in% unique(unlist(lapply(x@metiers, function(x1) names(x1@catches))))) {
        e.<-effort(x)[,YY]
        mt. <- sapply(x@metiers,function(x1) {
          if (st %in% names(x1@catches)) {
            eff <- e. * x1@effshare[,YY]
            Q   <- catch.q(x1)[[st]][,YY]
            sel <- landings.sel(x1)[[st]][,YY]
            #harv <- sweep(sweep(sel,2:6,eff,FUN="*"),2:6,Q,FUN="*")
            harv <- sweep(eff,2:6,Q,FUN="*")
          } else  harv<-0
          harv[is.na(harv)] <- 0
          return(harv)})
      } else mt. <- 0
    })
  })

  #predicted F by stock
  F.pred_all <- sapply(partF_all,function(x) sum(sapply(x,sum)))



  # 3. include this new fcube F into fwd for calculating TAC

  #demersal stocks
  dem.st.f3 <- lapply(dem.st.f3, function(x) {   # for (x in stf.stock) {
                  n. <- name(x)
                  r. <- as.character(range[n., "minfbar"]:range[n., "maxfbar"])


                  #in dem.stock, which is a stf stock, landings.n is only a proportion[0-1] of landings.n vs. catch.n
                  #fmult.land <- F.land.pred[n.]/apply(harvest(x)[r.,YY]*landings.n(x)[r.,YY],2:6,mean,na.rm=T)
                  fmult.land <- F.pred_all[n.]/fbar(x)[,YY]

                  ctrl <- fwdControl(data.frame(year=YY,val=c(fmult.land),quantity="f", rel.year=YY))
                  srPar<-FLPar(c(Recr[n.,YY]),dimnames=list(params="a",iter=1))
                  x <- fwd(x,ctrl=ctrl,sr=list(model="mean",params=srPar))
                  name(x) <- n.
                  return(x)

  })



  if (NepYes==T) {

  nep.st.f3 <- lapply(nep.st.f3, function(x) {
          n. <- name(x)
          #print(n.)
          # NEP  in year monitoring
          if (n. %in% reopenNEP) stock.n(x)[,YY] <- NEP_IntYrSurvey[n.][[1]] else
          stock.n(x)[,YY] <- stock.n(x)[,yr.assess]

                          #in dem.stock, which is a stf stock, landings.n is only a proportion of landings.n vs. catch.n
            fmult.land <- F.land.pred[n.]/(harvest(x)[,YY]*landings.n(x)[,YY])

          harvest(x)[,YY] <- harvest(x)[,YY]*fmult.land
          catch.n(x)[,YY] <- stock.n(x)[,YY]*harvest(x)[,YY]
          landings.n(x)[,YY] <- catch.n(x)[,YY]*landings.n(x)[,YY]
          landings(x)[,YY] <- landings.n(x)[,YY]*landings.wt(x)[,YY]
          discards.n(x)[,YY] <- catch.n(x)[,YY]-landings.n(x)[,YY]
          discards(x)[,YY] <- discards.n(x)[,YY]*discards.wt(x)[,YY]
          return(x)})

  # the estimated change (ratio) in landings between Fcube and the baseline for the NEP stocks with UWTV
  # is applied to the other stocks in order to apply the same rule to these

  tot.nep.f3.landings[,YY] <-sapply(nep.lst, function(x) landings(nep.st.f3[[x]])[,YY])

  Ratio <- sum(tot.nep.f3.landings[nep.ass,YY])/
           sum(sapply(nep.lst, function(x) landings(nep.st.fwd[[x]])[,YY])[nep.ass])

  tot.nep.f3.landings[no.nep.ass,YY] <- sapply(nep.lst, function(x) landings(nep.st.fwd[[x]])[,YY])[no.nep.ass]*Ratio
  }


  #Relative stability by country
  rel.share <- lapply(partF.land,function(x) {
                  fl. <- sapply(x,sum)
                  fl. <- data.frame(country=substr(names(fl.),1,2),F=fl.)
                  fl.[fl.$country %in% c("EN","SC"),"country"] <- "UK"
                  fl. <-cbind(aggregate(fl.$F,list(country=fl.$country),sum,na.rm=T),tot=sum(aggregate(fl.$F,list(country=fl.$country),sum,na.rm=T)$x))
                  fl.$lshare <- fl.$x / fl.$tot
                  return(fl.) })



  rel.share <- cbind(eval(parse(text=paste('rbind(rel.share[[', paste(seq(length(rel.share)),
  			collapse=']] ,rel.share[['), ']])', sep=''))),stock=rep(names(partF.land),each=max(sapply(rel.share,nrow))), year=YY,scenario=sc)


  rs<- rbind(rs,rel.share[,names(rs)])


  ## Feeding in the total catch data.frame for storing all catch info

  All.landings <-c(sapply(dem.st.f3,function(x) landings(x)[,YY]),tot.nep.f3.landings[,YY])
  All.landings <-All.landings[sort(names(All.landings))]
  Legal.landings <- OverTAC.landings <- All.landings
  for (s. in names(Legal.landings)) {
    Legal.landings[s.] <-min(All.landings[s.],TACnow[s.]) ##TO CHANGE
    OverTAC.landings[s.] <- All.landings[s.]-TACnow[s.]
  }
  OverTAC.landings[OverTAC.landings<0] <- 0

  disc <- c(sapply(dem.st.f3,function(x) discards(x)[,YY]),sapply(nep.st.f3,function(x) discards(x)[,YY]))
  disc <- disc[sort(names(disc))]

  tonnes <- round(c(Legal.landings,disc,OverTAC.landings),2)

  allcatch.dataframe <- rbind(allcatch.dataframe,
                              data.frame(year=YY,sc=sc,stock=rep(st.names,3),
                                         catch=rep(c("legal landings","discards","overTAC landings"),each=length(st.names)),
                                         tonnes =as.numeric(tonnes))
                            )



  #************
  #! returning landings and discards into the fleets and metiers
  #first we need to trick the no.nep.ass for calculating the basic new landings in yr.now before applying the
  #fcube ratio, for the baseline

  BaselineNepNoAss <- sapply(nep.lst, function(x) landings(nep.st.fwd[[x]])[,yr.assess])[no.nep.ass]/sapply(nep.lst, function(x) landings(nep.st.fwd[[x]])[,YY])[no.nep.ass]


      fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
              n. <- name(x)
              #print(n.)
              x@metiers <- lapply(x@metiers, function(met) {
                            m. <- name(met)
                            met@catches <- lapply(met@catches, function(st) {
                                           s. <- name(st)
                                           if (!s. %in% no.nep.ass) {
                                           ratio.f.land <- partF.land[[s.]][[n.]][m.]/F.land.pred[[s.]]
                                           landings(st)[,YY] <-All.landings[[s.]]*ratio.f.land
                                           }  else { #nep
                                           baseline.land <-landings(st)[,yr.assess]*BaselineNepNoAss[s.]
                                           landings(st)[,YY] <- baseline.land*Ratio
                                           }

                                           discards(st)[,YY] <- landings(st)[,YY]*discards.sel(st)[,YY]/landings.sel(st)[,YY]
                                           return(st)})
                            return(met) })
              return(x) }))




      ##-----------------------------------------------------------------------------
      ## 2). RECALCULATE MANAGEMENT PLANS
      ##-----------------------------------------------------------------------------

  YYp1 <- as.numeric(YY)+1
  if (YYp1 <= F3ProjectionYear) {
  YYp1 <- ac(YYp1)
  print("----MANAGEMENT PLAN----")
    #FORWARD
   dem.st.MP <- lapply(dem.st.f3, function(x) {
          n. <- name(x)
          ctrl. <- ctrl.MP.yrTAC[[n.]]
          srPar<-FLPar(c(Recr[n.,YYp1]),dimnames=list(params="a",iter=1))
          x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
          name(x) <- n.
          return(x)
          })

    # for NEP - the advice is unchanged since we do not know how results above influence biomass. Keep


    #finding the landings target corresponding to this
  Ftarg.dem.yrTAC <- sapply(dem.st.MP,function(x) {
                  n. <- x@name
                  r. <- range[n., "minfbar"]:range[n., "maxfbar"]
                  #F.land <- harvest(x)[,YYp1]*landings.n(x)[,YYp1]/catch.n(x)[,YYp1]
                  #apply(F.land[as.character(r.)],2:6,mean,na.rm=T)
                  fbar(x)[,YYp1]*landings(x)[,YYp1]/catch(x)[,YYp1]
                  })

  Ftarg.nep.yrTAC <- sapply(nep.st.fwd,function(x) {
                  n. <- x@name
                  r. <- range[n., "minfbar"]:range[n., "maxfbar"]
                  F.land <- harvest(x)[,YYp1]*landings.n(x)[,YYp1]/(landings.n(x)[,YYp1]+discards.n(x)[,YYp1])
                  })



  if (NepYes ==F) f3.ctrl.YY@target.F <- FLPar(Ftarg.dem.yrTAC,params=dem.names) else
                  f3.ctrl.YY@target.F <- FLPar(c(Ftarg.dem.yrTAC,Ftarg.nep.yrTAC),params=c(dem.names,nep.names))


  }

  } #### end of year loop

  #------------RESULTS--------------------
  print("----WRITE RESULTS----")

  years <- now:F3ProjectionYear
  #landings

  results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                               stock=rep(dem.names,each=length(years)),value="landings",
                               data=unlist(lapply(dem.st.f3, function(x) round(computeLandings(x)[,ac(years)])))
  ))

  results<-rbind(results,cbind(sc=sc,year=rep(years,each=n.nep), #watch this, here it reads species first and year after. Different from the FLStocks objects
                               stock=rep(nep.names,length(years)),value="landings",
                               data=c(tot.nep.f3.landings)
  ))

  #discards
  results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                               stock=rep(dem.names,each=length(years)),value="discards",
                               data=unlist(lapply(dem.st.f3, function(x) round(computeDiscards(x)[,ac(years)])))
  ))



  #TAC advice according to MP

  results<-rbind(results,cbind(sc=sc,year=rep(years[-1],n.dem),
                               stock=rep(dem.names,each=length(years)-1),value="Ld_MgtPlan",
                               data=unlist(lapply(dem.st.MP, function(x) round(computeLandings(x)[,ac(years[-1])])))
  ))

  results<-rbind(results,cbind(sc=sc,year=rep(years[-1],n.nep),
                               stock=rep(nep.names,each=length(years)-1),value="Ld_MgtPlan",
                               data=unlist(lapply(nep.st.fwd, function(x) round(landings(x)[,ac(years[-1])])))
  ))

  # Fmult

  results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                               stock=rep(dem.names,each=length(years)),value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),
                               data=unlist(lapply(dem.st.f3, function(x) round(sweep(fbar(x)[,ac(years)],c(1,3:6),fbar(x)[,yr.assess],FUN="/"),2)))
  ))

  results<-rbind(results,cbind(sc=sc,year=rep(years,n.nep),
                               stock=rep(nep.names,each=length(years)),value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),
                               data=unlist(lapply(nep.st.f3, function(x) round(sweep(fbar(x)[,ac(years)],c(1,3:6),fbar(x)[,yr.assess],FUN="/"),2)))
  ))

  # Fbar

  results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                               stock=rep(dem.names,each=length(years)),value="Fbar",
                               data=unlist(lapply(dem.st.f3, function(x) fbar(x)[,ac(years)]))
  ))

  results<-rbind(results,cbind(sc=sc,year=rep(years,n.nep),
                               stock=rep(nep.names,each=length(years)),value="Fbar",
                               data=unlist(lapply(nep.st.f3, function(x) fbar(x)[,ac(years)]))
  ))



  #ssb

  years_ssb <- c(years,YYp1)
  results<-rbind(results,cbind(sc=sc,year=rep(years_ssb,n.dem),
                               stock=rep(dem.names,each=length(years_ssb)),value="ssb",
                               data=unlist(lapply(dem.st.f3, function(x) round(ssb(x)[,ac(years_ssb)])))
  ))

  results<-rbind(results,cbind(sc=sc,year=rep(years_ssb,n.dem),
                               stock=rep(dem.names,each=length(years_ssb)),value="ssb_MgtPlan",
                               data=unlist(lapply(dem.st.MP, function(x) round(ssb(x)[,ac(years_ssb)])))
  ))




  print(Sys.time()-t1)


  } #end of scenarios


  ##-----------------------------------------------------------------------------
  ## III. OUTPUTS
  ##-----------------------------------------------------------------------------


  results$year <- as.numeric(results$year)
  results$data <- as.numeric(results$data)

  res.effort$year <- as.numeric(res.effort$year)
  res.effort$effort <- as.numeric(res.effort$effort)


  # output table
  res <-ftable(tapply(results$data,list(results$value,results$year,results$sc,results$stock),mean,na.rm=T))

  save(results, res.effort, file = paste0(res.path, '/', Run.nameFC, '_Catch_EffortByScenario.Rdata'))
  save(allcatch.dataframe, file = paste0(res.path, '/', Run.nameFC, '_AllCatches_Dataframe.Rdata') )


  # output table
  Print_Results <- TRUE

  if (Print_Results) {
  res <- subset(results,value=="landings")
  print("--- Landings ---")
  print(round(ftable(tapply(res$data,list(res$year,res$sc,res$stock),mean,na.rm=T)),0))


  res <- subset(results,value==paste("FmultVsF",substr(yr.assess,3,4),sep=""))
  print(paste("--- FmultVsF",substr(yr.assess,3,4),"---"))
  print(round(ftable(tapply(res$data,list(res$year,res$sc,res$stock),mean,na.rm=T)),2))

  res <- subset(results,value=="Ld_MgtPlan")
  print("--- Ld_MgtPlan ---")
  print(round(ftable(tapply(res$data,list(res$year,res$sc,res$stock),mean,na.rm=T))))
  #

  res <- subset(results,value=="ssb")
  print("--- ssb ---")
  print(round(ftable(tapply(res$data,list(res$year,res$sc,res$stock),mean,na.rm=T)),0))

  # relative stability plot
  #par(mfrow=c(2,3))

  for (st in dem.names) {
    png(paste0(plot.path, "/", Run.nameFC, '_RelStab_Scenarios_',st,'.png'),
      height=5, width=7, units = "in", res = 200)

      p <- ggplot(data = subset(rs, stock==st)) +
        aes(x = scenario, y = lshare, shape=factor(year), col=factor(year)) +
        facet_wrap("country") +
      	geom_point() +
        labs(title = st) +
      	ylab("Share") +
      	xlab("Scenarios") +
      	theme(text = element_text(size=12), axis.text.x = element_text(angle=90, hjust=0.5))
     print(p)

    dev.off()
  }

  #changes in effort by metier
  res.effort.met$gear <- unlist(lapply(strsplit(res.effort.met$metier,split="\\."),function(x) x[1]))
  res.effort.met[res.effort.met$gear %in% c("OTH","otter"),"gear"] <- "OTH"
  eff.gear <- aggregate(res.effort.met$efmet,list(year=res.effort.met$year, scenario=res.effort.met$scenario,
                                                   gear=res.effort.met$gear),sum,na.rm=T)

  print("--- Effort by metier ---")
  res <- round(ftable(tapply(eff.gear$x,list(eff.gear$year,eff.gear$sc,eff.gear$gear),mean,na.rm=T)),0)
  print(res)
  }

  ##-----------------------------------------------------------------------------
  ## DIAGNOSTICS
  ##-----------------------------------------------------------------------------

  if(!Fc.silent) {

    chokespecies <- matrix(nrow=length(st.names),ncol=2,dimnames=list(st.names,ac(now:F3ProjectionYear)))
    unchokespecies <- matrix(nrow=length(st.names),ncol=2,dimnames=list(st.names,ac(now:F3ProjectionYear)))


   # for (YY in F3ProjectionYear){
    #  print(YY)

      eff.st <- effort.stock[["max"]][[yr.TAC]]


      #calculating the choke species!
      zz_min <- vector()
      zz_max <- vector()
          max_ <- apply(eff.st,2,max,na.rm=T)
          min_ <- apply(eff.st,2,min,na.rm=T)
          if(FIDES) {
            for (i in names(min_)) {
              for (j in seq(dim(eff.st)[[3]])) {
                if (isTRUE(any(ChokeCategory1[,i,j])) & !is.na(min(eff.st[which(ChokeCategory1[,i,j]),i,j]))) {
                  min_[i] <- min(eff.st[which(ChokeCategory1[,i,j]),i,j],na.rm=T)
                } else {
                  min_[i] <- NA
                }
          }}}



      for (i in dimnames(eff.st)[[2]]) {
        if(!i=="unalloc") {
          x <- eff.st[,i,1]
          if (!is.na(min_[i])){
            zz_min <- c(zz_min,which(x==min_[i]))
          } else {
            zz_min <- c(zz_min,NA)
          }
          zz_max <- c(zz_max,which(x==max_[i]))
        }}
        names(zz_min)[names(zz_min)==""] <- "NONE"
        zz_min[names(zz_min)=="NONE"] <- 1


      effort_fl_yrassess <- sapply(fl.f3,function(x) effort(x)[,yr.assess])

      chokespecies[,ac(YY)] <- sapply(dimnames(eff.st)[[1]],function(x) {sum(effort_fl_yrassess[which(names(zz_min)==x)],na.rm=T)})
      unchokespecies[,ac(YY)] <- sapply(dimnames(eff.st)[[1]],function(x) {sum(effort_fl_yrassess[which(names(zz_max)==x)],na.rm=T)})



      print(paste("#-- choke species - % of",yr.assess, "effort limited by each species in", YY))
      print(round(chokespecies[,ac(YY)]/sum(chokespecies[,ac(YY)],na.rm=T),3))
      print ("nbr of fleets")
      nflchoke <- table(names(zz_min))
      #names(nflchoke) <- stock.names[as.numeric( names(nflchoke))]
      print(nflchoke)

   capture.output(paste("#-- choke species - % of",yr.assess, "effort limited by each species in", YY), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"))
   capture.output(round(chokespecies[,ac(YY)]/sum(chokespecies[,ac(YY)],na.rm=T),3), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(paste("nbr of fleets"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(nflchoke, file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)




      print(paste("#-- least limiting species - % of",yr.assess, "effort unlimited by each species in", YY))
      print(round(unchokespecies[,ac(YY)]/sum(unchokespecies[,ac(YY)],na.rm=T),2))
      print ("nbr of fleets")
      nflunchoke <- table(zz_max)
      names(nflunchoke) <- stock.names[as.numeric( names(nflunchoke))]
      print(nflunchoke)

   capture.output(paste("#-- least limiting species - % of",yr.assess, "effort unlimited by each species in", YY), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(round(unchokespecies[,ac(YY)]/sum(unchokespecies[,ac(YY)],na.rm=T),2), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(paste("nbr of fleets"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(nflunchoke, file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)


   zz_min <- dimnames(eff.st)[[1]][zz_min]
   zz_max <- dimnames(eff.st)[[1]][zz_max]
   names(zz_min) <- dimnames(eff.st)[[2]]
   names(zz_max) <- dimnames(eff.st)[[2]]

   print("#-- stock most limiting each fleet")
   print(zz_min)
   print("#-- stock least limiting each fleet")
   print(zz_max)


   capture.output(paste("#-- stock most limiting each fleet"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(zz_min, file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(paste("#-- stock least limiting each fleet"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(zz_max, file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)



      print("#--  % of effort reduction between sq and min")
      reduction <- (sum(subset(res.effort,year==YY & scenario=="sq_E")$effort) - sum(subset(res.effort,year==YY & scenario=="min")$effort))/sum(subset(res.effort,year==YY & scenario=="sq_E")$effort)
      print(-round(reduction*100,2))

   capture.output(paste("#--  % of effort reduction between sq and min"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(-round(reduction*100,2), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)



      print("#--  % of effort increase between sq and max")
      increase <- (sum(subset(res.effort,year==YY & scenario=="sq_E")$effort) - sum(subset(res.effort,year==YY & scenario=="max")$effort))/sum(subset(res.effort,year==YY & scenario=="sq_E")$effort)
      print(-round(increase*100,2))


   capture.output(paste("#--  % of effort increase between sq and max"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(-round(increase*100,2), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)


   capture.output(paste("#--  effort by stock in TAC year"), file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)
   capture.output(eff.st, file=paste0(res.path,"/", Run.nameFC,"_outputsFCubeProj.txt"), append=T)

      #save(eff.st,file=paste(res.path,'/', Run.nameFC,'_FleetEffortByStock.Rdata',sep=""))

    }


  save.image(file=paste(res.path,'/', Run.nameFC,'_FcubeAllObjects.Rdata',sep=""))

  write.csv(allcatch.dataframe,file=paste(res.path,'/', Run.nameFC,'_AllCatchDataframe.csv',sep=""))

  ##-----------------------------------------------------------------------------
  ## STANDARD PLOT CHECKING THAT IT IS ALL FINE
  ##-----------------------------------------------------------------------------


  #  plot with dem and NEP6-9 pulled
  res. <- subset(results,value=="landings" & year==yr.TAC)
  res. <- tapply(res.$data,list(res.$sc,res.$stock),sum,na.rm=T)

  res. <- cbind(res.,apply(res.[,paste("NEP",6:9,sep="")],1,sum,na.rm=T))
  colnames(res.)[which(colnames(res.)=="")]<-"NEP6-9"
  res. <- res.[,c(dem.names,"NEP6-9")]
  #res.1 <- res.[-(1:2),]
  #res. <- res.landings[-(1:2),dem.names]

  pal <- rainbow(length(dem.names)+1)
  baseline <- res.[1,]
  res. <- res.[-1,]


  png(paste0(plot.path, "/", 'pred_landings_', yr.TAC,'.png'),
      height=6, width=7.5, units = "in", res = 200)
    op <- par(mar = c(4,4,1,1), mgp=c(2,0.5,0), ps = 12)
    barplot(t(res.[scenarios,]), col=pal, beside = TRUE,
      width = 1, legend = FALSE,
      ylim=c(0, max(res.)*1.15),
      ylab=paste("predicted landings",yr.TAC),
      xlab="Fcube scenarios")
    legend("top", legend = colnames(res.), ncol=5, bty = "n", col=pal, fill = pal)

    abline(h=baseline[colnames(res.)[1]], col=pal[1], lwd=1.5)
    abline(h=baseline[colnames(res.)[2]], col=pal[2], lwd=1.5)
    abline(h=baseline[colnames(res.)[3]], col=pal[3], lwd=1.5)
    abline(h=baseline[colnames(res.)[4]], col=pal[4], lwd=1.5)
    abline(h=baseline[colnames(res.)[5]], col=pal[5], lwd=1.5)
    abline(h=baseline[colnames(res.)[6]], col=pal[6], lwd=1.5)
    abline(h=baseline[colnames(res.)[7]], col=pal[7], lwd=1.5)
    abline(h=baseline[colnames(res.)[8]], col=pal[8], lwd=1.5)
    abline(h=baseline[colnames(res.)[9]], col=pal[9], lwd=1.5)
    par(op)
    dev.off()
}
