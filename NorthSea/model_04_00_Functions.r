###################################################################################
##     FLFCUBE SCRIPT FOR RUNNING FCUBE RUNS ON VARIOUS TAC SCENARIOS            ##
##                                                                               ##
##                         North Sea                                             ##
##                                                                               ##
##      Progr.3 - Fcube projection on several deterministic years                ##
##                                                                               ##
##  Running with fleets and stocks data up to 2012, Advice 2014                  ##
##                                                                               ##
# Initial Author : Clara Ulrich, DTU Aqua <clu@aqua.dtu.dk>                      ## 
## Further Contributors: Youen Vermard, Paul J. Dolder,...                       ## 
# And ICES WGMIXFISH colleagues                                                  ## 
##                                                                               ##
## Runs with R 3.1                                                               ##
##                                                                               ##
###################################################################################



### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
gc(); graphics.off()

#ver1 <- "03_Fcube_Projection"
#ver.datetime   <- "12/10/2016";
# ver.datetime   <- "24/05/2018";

options(stringsAsFactors = FALSE, scipen=100)


source('bootstrap/software/functions/FLFcube_FLCore_withFIDES.R')         
source('bootstrap/software/functions/FLFcube_FLCore_R31.R')
source('bootstrap/software/functions/funcs.R')                                                   
source('bootstrap/software/functions/remove_validity_FLFleet.R') # to reduce computing time      


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

if(FIDES){
ver1 <- "04_Fcube_SQIntYr__OPTIM_FIDES"
}else{
ver1 <-"04_Fcube_SQIntYr__OPTIM"  
}


if (length(LO)>0) Run.nameFC <- paste0(ver1,"_LO") else Run.nameFC <- ver1
if (FIDES) Run.nameFC <- paste0(Run.nameFC,"_FIDES")

print(Run.nameFC)



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


## FIDES data for the last year
#source("model_03_ReadFIDES.r")
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

## CMT outcomes

rs_$LandOnInitial <- rs_$landings/rs_$initialQuota
rs_$LandOnFinal <- rs_$landings/rs_$finalQuota
rs_$CatchOnInitial <- rs_$catch/rs_$initialQuota
rs_$CatchOnFinal <- rs_$catch/rs_$finalQuota
rs_$SurplusRatio <- rs_$catch/rs_$QuotaPlusUplift
rs_$SurplusTonnage <- rs_$QuotaPlusUplift-rs_$catch
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

scenarios <- c("sq_E","min")

Fc.silent <- FALSE #(if true, not all intermediate results are given)


effort.stock <- list()
tot.nep.f3.landings <- matrix(nrow=length(nep.lst),ncol=length(now:F3ProjectionYear),dimnames=list(nep.names,now:F3ProjectionYear))

saveCatchFleet <- list()

# Management Plans for yrTAC   <-This must be as in reproduce the advice, but only for the TAC year! 

# ctrl.MP.yrTAC <- lapply(ctrl.STF,function(x) fwdControl(x@target[-1,]))
FcubeDiffLandings2Scenarios <- function(TargetByStock=c())
{
  names(TargetByStock) <- dem.names
  print(TargetByStock)

    
        ctrl.MP.yrTAC <- list()
        
        ctrl.MP.yrTAC <- lapply(dem.lst, function(x){
          
          
          return(fwdControl(data.frame(year=yr.TAC,val=TargetByStock[x],quantity="f")))
          
          #return(fwdControl(data.frame(year=rep(yr.TAC,2),val=c(TargetByStock[x],NA),quantity=c("f","landings"),
          #                        min=c(NA,(1-TACcap)*TAC.now[x]),max=c(NA,(1+TACcap)*TAC.now[x]))))
          #})
          
        })         
        
        ##-----------------------------------------------------------------------------
        ## RUNNING SCENARIOS
        ##-----------------------------------------------------------------------------
        
        if (NepYes ==F) f3.ctrl@target.F <- FLPar(Ftarg.dem.now,params=dem.names) else
                        f3.ctrl@target.F <- FLPar(c(Ftarg.dem.now,Ftarg.nep.now),params=c(dem.names,nep.names))
        
        
        
        ### FCUBE
        test.landings <- list()
        test.SSB <- list()
        
        
    for (sc in scenarios) {
        #sc <- "cod"
        
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
          
            print("status quo in Int Yr")
          fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
            effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
            return(x)}))
          
            
           
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
        
        
        ## yv add fides stuff
        if (FIDES) res <- FLFcube_FIDES(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent,fides=rs_) else
          res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
        if (Fc.silent) fl.f3<-res else {
          fl.f3<- res[[1]]
          effort.stock[[sc]][[YY]] <- res[[2]]
          if (FIDES==TRUE & sc=="min") ChokeCategory1 <- res[[4]]}
        
        
        # res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
        #   if (Fc.silent) {
        #     fl.f3<-res 
        #   }else {
        #           fl.f3<- res[[1]]
        #           effort.stock[[sc]][[YY]] <- res[[2]]
        #   }
          
        
        
        }
        
        } # end of if on TAC Yr
        
        # ef.fl <- data.frame(scenario=sc,year=YY,fleet=fleet.names,effort=sapply(fl.f3,function(x) effort(x)[,YY]))
        # 
        # res.effort <- rbind(res.effort,ef.fl)
        # 
        # # effort.by.metier 
        # ef.mt <- slot.fleet(fl.f3,"effshare")
        # ef.mt <- ef.mt[ef.mt$year==YY,]
        # ef.mt <- merge(ef.mt,ef.fl)
        # ef.mt$effort <- as.numeric(ef.mt$effort)
        # ef.mt$efmet <- ef.mt$effort*ef.mt$effshare
        # res.effort.met <- rbind(res.effort.met, ef.mt[, c("year","scenario","fleet","metier","effshare","effort","efmet")])
        
        
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
        
                        
                        # fmult.land <- F.pred_all[n.]/fbar(x)[,YY]   YV commented this line and changed by the one below OCT 18
                        fmult.land <- F.land.pred[n.]/apply(harvest(x)[r.,YY]*landings.n(x)[r.,YY],2:6,mean,na.rm=T) #YV OCT 18 changed this line
                        
                        
                        ctrl <- fwdControl(data.frame(year=YY,val=c(fmult.land),quantity="f", rel.year=YY))
                        srPar<-FLPar(c(Recr[n.,YY]),dimnames=list(params="a",iter=1))
                        x <- fwd(x,ctrl=ctrl,sr=list(model="mean",params=srPar))
                        name(x) <- n.
                        return(x)
        
        })                            
        
         
        
        if (NepYes==T) {
        
        nep.st.f3 <- lapply(nep.st.f3, function(x) {
                n. <- name(x)
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
        
        
        # #Relative stability by country
        # rel.share <- lapply(partF.land,function(x) {
        #                 fl. <- sapply(x,sum)
        #                 fl. <- data.frame(country=substr(names(fl.),1,2),F=fl.)
        #                 fl. <-cbind(aggregate(fl.$F,list(country=fl.$country),sum,na.rm=T),tot=sum(aggregate(fl.$F,list(country=fl.$country),sum,na.rm=T)$x))
        #                 fl.$share <- fl.$x / fl.$tot
        #                 return(fl.) })
          
        ## YV add fides stuff
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


        # rs<- rbind(rs,rel.share[,names(rs)])


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

        # allcatch.dataframe <- rbind(allcatch.dataframe,
        #                             data.frame(year=YY,sc=sc,stock=rep(st.names,3),
        #                                        catch=rep(c("legal landings","discards","overTAC landings"),each=length(st.names)),
        #                                        tonnes =as.numeric(tonnes))
        #                           )
        # 
        # 
        
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
                                     
            saveCatchFleet[[sc]] <- fl.f3
        
        
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
        
 
        
        
        test.landings[[sc]] <- sapply(dem.st.f3,function(x) landings(x)[,YY])
        test.SSB[[sc]]      <- sapply(dem.st.f3,function(x) ssb(x)[,ac(as.numeric(YY)+1)])
        
        print(test.landings)
        
        
        
        } #end of scenarios
        
        
    
    Difference  <- sum(test.landings[[1]]) - sum(test.landings[[2]])
    Difference2 <- sum(    (test.landings[[1]] - test.landings[[2]])^2)
    
    Difference3 <- sum(    (test.landings[[1]] - test.landings[[2]])/test.landings[[1]])
    
    return(Difference3)
    }      







#################################################################################
######## function to ru after the OPTIM to extract once the best individual is identify and run the "other" sc with the F values associated



# Management Plans for yrTAC   <-This must be as in reproduce the advice, but only for the TAC year! 

# ctrl.MP.yrTAC <- lapply(ctrl.STF,function(x) fwdControl(x@target[-1,]))
FcubeDiffLandings2Scenarios_Best <- function(TargetByStock=c(),   scenarios = scenarios)
{
  names(TargetByStock) <- dem.names
  print(TargetByStock)
  

  resu<-data.frame(sc=NA,year=NA,stock=NA,value=NA,data=NA)
  
  ctrl.MP.yrTAC <- list()
  
  ctrl.MP.yrTAC <- lapply(dem.lst, function(x){
    
    
    return(fwdControl(data.frame(year=yr.TAC,val=TargetByStock[x],quantity="f")))
    
    #return(fwdControl(data.frame(year=rep(yr.TAC,2),val=c(TargetByStock[x],NA),quantity=c("f","landings"),
    #                        min=c(NA,(1-TACcap)*TAC.now[x]),max=c(NA,(1+TACcap)*TAC.now[x]))))
    #})
    
  })         
  # single species forecast
  
  dem.st.ss <- lapply(dem.stock, function(x) {
    n. <- name(x)
    #ctrl. <- ctrl.MP.yrTAC[[n.]]
    ctrl. <-  fwdControl(         # updated 2020 TB : stq F and then Fmsy instead of Fpa# added in 2019 (VT): STF in 2019, Fpa in 2020
      data.frame(
        year=c(yr.now,yr.TAC),
        val=c(1,TargetByStock[n.]),
        quantity=c("f","f"),
        rel.year=c(yr.assess,NA)
      )
    )
    yr. <- unique(ctrl.@target[,"year"]) 
    srPar<-FLPar(c(Recr[n.,yr.]),dimnames=list(params="a",year=yr.,iter=1))
    x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
  })
  
  dem.names2<-names(dem.st.fwd)
  for (i in 1:length(dem.names)) {
    name(dem.st.ss[[i]])<-dem.names2[i] }
  
  # go and get the values of the SS TAC to be used on the barplot figure 
  TACs<-unlist(lapply(dem.st.ss,function(x) landings(x)[,ac(yr.TAC)]+discards(x)[,ac(yr.TAC)]))
  # go and get the SSB of the SS TAC to be used on the barplot figure 
  SSBs<-unlist(lapply(dem.st.ss,function(x) ssb(x)[,ac(as.numeric(yr.TAC)+1)]))
  
  ##-----------------------------------------------------------------------------
  ## RUNNING SCENARIOS
  ##-----------------------------------------------------------------------------
  
  if (NepYes ==F) f3.ctrl@target.F <- FLPar(Ftarg.dem.now,params=dem.names) else
    f3.ctrl@target.F <- FLPar(c(Ftarg.dem.now,Ftarg.nep.now),params=c(dem.names,nep.names))
  
  
  
  ### FCUBE
  test.landings <- list()
  test.SSB <- list()   
  saveResFcube <- list()
  
  for (sc in scenarios) {
    #sc <- "cod"
    
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
        
        print("status quo in Int Yr")
        fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
          effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
          return(x)}))
        
        
        
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
          
          
          ## YV 2019 add fides stuff
          if (FIDES) res <- FLFcube_FIDES(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent,fides=rs_) else
            res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
          if (Fc.silent) fl.f3<-res else {
            fl.f3<- res[[1]]
            effort.stock[[sc]][[YY]] <- res[[2]]
            if (FIDES & sc=="min") ChokeCategory1 <- res[[4]]
            saveResFcube[[sc]] <- res}
          # res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
          #   if (Fc.silent) {
          #     fl.f3<-res 
          #   }else {
          #           fl.f3<- res[[1]]
          #           effort.stock[[sc]][[YY]] <- res[[2]]
          #   }
          
          
          
        }
        
      } # end of if on TAC Yr
      
      # ef.fl <- data.frame(scenario=sc,year=YY,fleet=fleet.names,effort=sapply(fl.f3,function(x) effort(x)[,YY]))
      # 
      # res.effort <- rbind(res.effort,ef.fl)
      # 
      # # effort.by.metier 
      # ef.mt <- slot.fleet(fl.f3,"effshare")
      # ef.mt <- ef.mt[ef.mt$year==YY,]
      # ef.mt <- merge(ef.mt,ef.fl)
      # ef.mt$effort <- as.numeric(ef.mt$effort)
      # ef.mt$efmet <- ef.mt$effort*ef.mt$effshare
      # res.effort.met <- rbind(res.effort.met, ef.mt[, c("year","scenario","fleet","metier","effshare","effort","efmet")])
      
      
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
        
        
        # fmult.land <- F.pred_all[n.]/fbar(x)[,YY]   YV commented this line and changed by the one below OCT 18
        fmult.land <- F.land.pred[n.]/apply(harvest(x)[r.,YY]*landings.n(x)[r.,YY],2:6,mean,na.rm=T) #YV OCT 18 changed this line
        
        
        ctrl <- fwdControl(data.frame(year=YY,val=c(fmult.land),quantity="f", rel.year=YY))
        srPar<-FLPar(c(Recr[n.,YY]),dimnames=list(params="a",iter=1))
        x <- fwd(x,ctrl=ctrl,sr=list(model="mean",params=srPar))
        name(x) <- n.
        return(x)
        
      })                            
      
      
      
      if (NepYes==T) {
        
        nep.st.f3 <- lapply(nep.st.f3, function(x) {
          n. <- name(x)
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
        fl. <-cbind(aggregate(fl.$F,list(country=fl.$country),sum,na.rm=T),tot=sum(aggregate(fl.$F,list(country=fl.$country),sum,na.rm=T)$x))
        fl.$share <- fl.$x / fl.$tot
        return(fl.) })
      
      
      
      rel.share <- cbind(eval(parse(text=paste('rbind(rel.share[[', paste(seq(length(rel.share)),
                                                                          collapse=']] ,rel.share[['), ']])', sep=''))),stock=rep(names(partF.land),each=max(sapply(rel.share,nrow))), year=YY,scenario=sc)
      
      
      # rs<- rbind(rs,rel.share[,names(rs)])
      
      
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
      
      # allcatch.dataframe <- rbind(allcatch.dataframe,
      #                             data.frame(year=YY,sc=sc,stock=rep(st.names,3),
      #                                        catch=rep(c("legal landings","discards","overTAC landings"),each=length(st.names)),
      #                                        tonnes =as.numeric(tonnes))
      #                           )
      # 
      # 
      
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
        # go and get the values of the SS TAC to be used on the barplot figure 
        #TACs<-unlist(lapply(dem.st.MP,function(x) landings(x)[,ac(yr.TAC)]+discards(x)[,ac(yr.TAC)]))
        # go and get the SSB of the SS TAC to be used on the barplot figure 
        #SSBs<-unlist(lapply(dem.st.MP,function(x) ssb(x)[,ac(as.numeric(yr.TAC)+1)]))
        
        
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
    
    
    
    
    test.landings[[sc]] <- sapply(dem.st.MP,function(x) landings(x)[,YY])
    test.SSB[[sc]]      <- sapply(dem.st.MP,function(x) ssb(x)[,ac(as.numeric(YY)+1)])
    
    resu<-rbind(resu,data.frame(sc=sc,year=rep(YY,n.dem),
                                stock=rep(dem.names,each=length(YY)),value="landings",
                                data=unlist(lapply(dem.st.f3, function(x) round(computeLandings(x)[,ac(YY)])))
    ))
    
    
    resu<-rbind(resu,data.frame(sc=sc,year=rep(YY,n.dem),
                                stock=rep(dem.names,each=length(YY)),value="discards",
                                data=unlist(lapply(dem.st.f3, function(x) round(computeDiscards(x)[,ac(YY)])))
    ))
    
    
    resu<-rbind(resu,data.frame(sc=sc,year=rep(YY,n.dem),
                                stock=rep(dem.names,each=length(YY)),value="SSB",
                                data=unlist(lapply(dem.st.f3, function(x) round(ssb(x)[,ac(YY)])))
    ))
    
    
    resu<-rbind(resu,data.frame(sc=sc,year=rep(c(an(YY)+1),n.dem),
                                stock=rep(dem.names,each=length(YY)),value="SSB",
                                data=unlist(lapply(dem.st.f3, function(x) round(ssb(x)[,ac(an(YY)+1)])))
    ))
    
    resu<-rbind(resu,data.frame(sc=sc,year=rep(c(an(YY)-1),n.dem),
                                stock=rep(dem.names,each=length(YY)),value="SSB",
                                data=unlist(lapply(dem.st.f3, function(x) round(ssb(x)[,ac(an(YY)-1)])))
    ))
    
    
    print(sc)        
    
    
  } #end of scenarios
  
  
  
  return(list(TACs,resu[-1,], SSBs,saveResFcube))
}      






























