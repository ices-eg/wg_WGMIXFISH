################################################
#
#  MAKING A BIG FUNCTION FOR RUNNING THE MAX-MIN OPTIMISER...
##
#
# WGMIXFISH 2020

require(stockassessment)

UseSAM <- TRUE ## True in the final results, corrects for any bias 
SkipIntermedYr <- TRUE


FcubeDiffLandings2Scenarios_Best <- function(TargetByStock=c())
#TargetByStock=TargetByStock;TACcap=TACcap;scenarios=scenarios;
#  fl.f3=fl.pred; dem.st.f3=dem.st.fwd;f3.ctrl.YY=f3.ctrl;YY=yr.TAC
{

scenarios <- c("min","max","sq_E","val", "cod.27.7e-k","had.27.7b-k","whg.27.7b-ce-k")
 resu<-data.frame()

#TACcap=TACcap
scenarios=scenarios
fl.pred=FLFleets(fl.pred)
dem.st.fwd=dem.st.fwd
f3.ctrl.YY=f3.ctrl
YY=yr.TAC

names(TargetByStock) <- dem.names
print(TargetByStock)

# step 1 - fwd control with target


ctrl.yrTAC <- list()

ctrl.yrTAC <- lapply(dem.lst, function(x){


    return(fwdControl(data.frame(year=yr.TAC,val=TargetByStock[x],quantity="f")))

                        #return(fwdControl(data.frame(year=rep(yr.TAC,2),val=c(TargetByStock[x],NA),quantity=c("f","landings"),
#                        min=c(NA,(1-TACcap)*TAC.now[x]),max=c(NA,(1+TACcap)*TAC.now[x]))))
#})

})

# single species forecast

dem.st.ss <- lapply(dem.st.fwd, function(x) {
  n. <- name(x)
  ctrl. <- ctrl.yrTAC[[n.]]
  #yr. <- unique(ctrl.@target[,"year"])
  srPar<-FLPar(c(Recr[n.,yr.TAC]),dimnames=list(params="a",year=yr.TAC,iter=1))
  x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
})

dem.names2<-names(dem.st.fwd)
for (i in 1:length(dem.names)) {
  name(dem.st.ss[[i]])<-dem.names2[i] }

# go and get the values of the SS TAC to be used on the barplot figure
TACs<-unlist(lapply(dem.st.ss,function(x) landings(x)[,ac(yr.TAC)]+discards(x)[,ac(yr.TAC)]))

#finding the landings target corresponding to this
Ftarg.dem.yrTAC <- sapply(dem.st.ss,function(x) {
  n. <- x@name
  r. <- range[n., "minfbar"]:range[n., "maxfbar"]
  F.land <- harvest(x)[,yr.TAC]*landings.n(x)[,yr.TAC]/catch.n(x)[,yr.TAC]
  apply(F.land[as.character(r.)],2:6,mean,na.rm=T)})

f3.ctrl.YY@target.F <- if(nep==FALSE) { FLPar(Ftarg.dem.yrTAC,params=dem.names) } else {
FLPar(c(Ftarg.dem.yrTAC, Ftarg.nep.now), params = c(dem.names, nep.names))
}
yr.rge <- ac(f3.ctrl.YY@data.yrs)


### FCUBE
All.landings <- list()
All.SSB <- list()

for (sc in scenarios) {
 
print(sc)


    print(paste("scenario:",sc))
  t1<-Sys.time()


    #initialising the projection objects from the original objects
    fl.f3 <- fl.pred #FLfleet objects
    dem.st.f3 <- dem.stock # FLStock objects
    if(nep) {
    nep.st.f3 <- nep.stock  #nep.stock<- NaN
    }

    for (YY in now:LastProjectionYear) { # YY = 2019
        YY <- ac(YY)

############################################################

        ##-----------------------------------------------------------------------------
        ## 1). INTERMEDIATE YEAR
        ##-----------------------------------------------------------------------------

        if (!SkipIntermedYr | an(YY) !=now) {   # Can choose to skip intermediate year
            cat("----FCUBE YR", YY, "----", "\n")

            #status quo in Intermediate year for all scenarios
            if (as.numeric(YY)==now) {
                print("SQ in Int Yr")
                fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
                            effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
                            return(x)}))
            } else {  # Fcube scenarios for TAC Yr
                #scenario status quo compared to last data year
                if (sc=="sq_E") {
                    fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
                                effort(x)[,YY]<-yearMeans(effort(x)[,yr.rge[length(yr.rge)]])
                                return(x)}))
                } else if (sc=="Ef_Mgt") {
                          # Scenario mostly North Sea related, although there is a flatfish plan in VIIe, not accounted for here
                          yr.ref <- as.character(as.numeric(YY)-1) # last data year

                          fl.f3 <- FLFleets(lapply(fl.f3, function(x) {  # issue here with eff.redYrAssesstoNow
                            # set for testing x <- fl.f3[[1]]
                                      eff <- effort(x)[,yr.ref]   #effort function works on FLstock/FLQuant
                                      if (as.numeric(YY)==now) eff.red <- eff.redYrAssesstoNow else # These lines refere to the North Sea.
                                         if(as.numeric(YY)==(now+1)) eff.red <- eff.redNowtoTACYr else # but where are these set????
                                            eff.red <- 0
                                                m.lst <- names(metiers(x))[!is.na(match(names(metiers(x)),names(eff.red)))]
                                                # function was previously metiers
                                                for (m in names(metiers(x))) {
                                                    if (m %in% m.lst) {
                                                        effshare(x@metiers[[m]])[,YY] <- eff*effshare(x@metiers[[m]])[,yr.ref]*(1-eff.red[m]/100)
                                                    } else effshare(x@metiers[[m]])[,YY] <- eff*effshare(x@metiers[[m]])[,yr.ref]
                                                }
                                                effort(x)[,YY] <- as.numeric(Sums(lapply(x@metiers,function(x1) effshare(x1)[,YY])))
                                                # recalculating new effshare then
                                                for (m in names(metiers(x))) {
                                                    effshare(x@metiers[[m]])[,YY] <- effshare(x@metiers[[m]])[,YY]/eff
                                                }
                                      return(x)}))
                       } else {

                          # 1. run Fcube
                          f3.ctrl.YY@mgmt.year <- as.numeric(YY)
                          f3.ctrl.YY@effort.rule <- array(sc)
                          res <- FLFcube(fleets=fl.f3, stocks=wg.stock, control=f3.ctrl.YY,silent=Fc.silent)
                          if (Fc.silent) fl.f3<-res else {
                                  fl.f3<- res[[1]]
                                  effort.stock[[sc]][[YY]] <- res[[2]]
                          }
                      }
            } # end of if on TAC Yr
	

            ef.fl <- data.frame(scenario=sc,year=YY,fleet=fleet.names,effort=sapply(fl.f3,function(x) effort(x)[,YY]))

#            res.effort <- rbind(res.effort,ef.fl)

            # effort.by.metier
#            ef.mt <- slot.fleet(fl.f3,"effshare")
#            ef.mt <- ef.mt[ef.mt$year==YY,]
##            ef.mt <- merge(ef.mt,ef.fl)
 #           ef.mt$effort <- as.numeric(ef.mt$effort)
 #           ef.mt$efmet <- ef.mt$effort*ef.mt$effshare
  #          res.effort.met <- rbind(res.effort.met, ef.mt[, c("year","scenario","fleet","metier","effshare","effort","efmet")])


            # 2. calculate resulting F from new effort

            # 2019 fix!!!
            # st.lst - at this point is a nested S4 object, if we turn to a list, then the code works
            st.lst <- names(wg.stock)
            #table of partial landings F
            # partial landings F by stock and fleet - a list of list of vectors
            partF.land <- lapply(st.lst,function(st) { #there is a list here that I think should be just 1 dimension
              # to test use st = "cod.27.7e-k"
                                        fl. <-lapply(fl.f3, function(x) {
                                          # to test use x = fl.f3[[1]]
                                     if (st %in% unique(unlist(lapply(x@metiers, function(x1) names(x1@catches))))) {
                                       #to test use x1 = x[[1]]
                                        e.<-effort(x)[,YY]
                                        mt. <- sapply(x@metiers,function(x1) {
                                        if (st %in% names(x1@catches)) {
                                            eff <- e. * effshare(x1)[,YY]
                                            Q   <- catch.q(x1)[[st]][,YY]
                                            sel <- landings.sel(x1)[[st]][,YY]
                                            harv <- sweep(sweep(sel,2:6,eff,FUN="*"),2:6,Q,FUN="*")
                                        } else  harv<-0
                                        harv[is.na(harv)] <- 0
                                        return(harv)})
                                     } else mt. <- 0
                                     })
                            })
            names(partF.land) <- st.lst #2019 add
            #predicted landings F by stock
            F.land.pred <- sapply(partF.land,function(x) sum(sapply(x,sum)))

            # 3. include this new fcube F into fwd for calculating TAC

            #demersal stocks
            dem.st.f3 <- lapply(dem.st.f3, function(x) {   
                            n. <- name(x)
                            r. <- as.character(range[n., "minfbar"]:range[n., "maxfbar"])

                            #in dem.stock, which is a stf stock, landings.n is only a proportion[0-1] of landings.n vs. catch.n
                            fmult.land <- F.land.pred[n.]/apply(harvest(x)[r.,YY]*landings.n(x)[r.,YY],2:6,mean,na.rm=T)
                            fmult.land[is.na(fmult.land)] <- 0 ## catch where stock is zero catch
                            ctrl <- fwdControl(data.frame(year=YY,val=c(fmult.land),quantity="f", rel.year=YY))
                            srPar<-FLPar(c(Recr[n.,YY]),dimnames=list(params="a",iter=1))
                            x <- fwd(x,ctrl=ctrl,sr=list(model="mean",params=srPar))
                            name(x) <- n.
                            return(x)
                            })

	    if(UseSAM) {

		   ## Get the intermediate and TAC year targets in fbar to go to SAM forecast
		 if(YY == now) {
		Ftarg_int <- sapply(dem.st.f3, function(x) fbar(x)[,YY]) } else {
			
			if(SkipIntermedYr) {
			Ftarg_int <- sapply(sam_stks, function(x) sam_settings[[x]][["fval"]][2])
			}
			Ftarg_tac <- sapply(dem.st.f3, function(x) fbar(x)[,YY])
		 	}
	 
	    ## Adjust the target F in the SAM forecast
	    for(i in sam_stks) {
		    if(YY == now) {
		    sam_settings[[i]][["fval"]][2] <- Ftarg_int[[i]]
		    if(!is.null(sam_settings[[i]][["fscale"]])) {
		    sam_settings[[i]][["fscale"]][[2]] <- NA
		    }
		    if(!is.null(sam_settings[[i]][["fscale"]])) {
		    sam_settings[[i]][["catchval.exact"]][[2]] <- NA
		    }
		    if(!is.null(sam_settings[[i]][["catchval"]])) {
		    sam_settings[[i]][["catchval"]][[2]] <- NA
		    }
		    } else {
		    sam_settings[[i]][["fval"]][2] <- Ftarg_int[[i]]
		    sam_settings[[i]][["fval"]][3] <- Ftarg_tac[[i]]
		    
		    if(!is.null(sam_settings[[i]][["fscale"]])) {
		    sam_settings[[i]][["fscale"]][[2]] <- NA
		    sam_settings[[i]][["fscale"]][[3]] <- NA
		    }
		    if(!is.null(sam_settings[[i]][["catchval.exact"]]) & !SkipIntermedYr) {
		    sam_settings[[i]][["catchval.exact"]][[2]] <- NA
		    sam_settings[[i]][["catchval.exact"]][[3]] <- NA
		    }
		    if(!is.null(sam_settings[[i]][["catchval"]]) & !SkipIntermedYr) {
		    sam_settings[[i]][["catchval"]][[2]] <- NA
		    sam_settings[[i]][["catchval"]][[3]] <- NA
		    }
		    }
	    }
	    
	    ## cod sam forecast
	    set.seed(sam_settings[["cod.27.7e-k"]][["seed"]])
	    cod_sam_forecast <- forecast(SAM_fits[["cod.27.7e-k_sam"]], 
	    			     fval = sam_settings[["cod.27.7e-k"]][["fval"]],
				     catchval = sam_settings[["cod.27.7e-k"]][["catchval"]],
	    			     ave.years = sam_settings[["cod.27.7e-k"]][["ave.years"]],
	    			     rec.years = sam_settings[["cod.27.7e-k"]][["rec.years"]],
	    			     splitLD = sam_settings[["cod.27.7e-k"]][["splitLD"]],
	    			     nosim = sam_settings[["cod.27.7e-k"]][["nosim"]]
	    )
	    
	    ## had sam forecast
	    set.seed(sam_settings[["had.27.7b-k"]][["seed"]])
	    had_sam_forecast <- forecast(SAM_fits[["had.27.7b-k_sam"]], 
	    			     fval = sam_settings[["had.27.7b-k"]][["fval"]],
	    			     ave.years = sam_settings[["had.27.7b-k"]][["ave.years"]],
	    			     rec.years = sam_settings[["had.27.7b-k"]][["rec.years"]],
	    			     splitLD = sam_settings[["had.27.7b-k"]][["splitLD"]],
	    			     nosim = sam_settings[["had.27.7b-k"]][["nosim"]]
	    )
	    
	    ## whg sam forecast
	    set.seed(sam_settings[["whg.27.7b-ce-k"]][["seed"]])
	    whg_sam_forecast <- forecast(SAM_fits[["whg.27.7b-ce-k_sam"]], 
	    			     fval = sam_settings[["whg.27.7b-ce-k"]][["fval"]],
	    			     ave.years = sam_settings[["whg.27.7b-ce-k"]][["ave.years"]],
	    			     rec.years = sam_settings[["whg.27.7b-ce-k"]][["rec.years"]],
	    			     splitLD = sam_settings[["whg.27.7b-ce-k"]][["splitLD"]],
	    			     nosim = sam_settings[["whg.27.7b-ce-k"]][["nosim"]]
	    )
	    
	    ## sole 7fg sam forecast
	    
	    set.seed(sam_settings[["sol.27.7fg"]][["seed"]])
	    sol_sam_forecast <- forecast(SAM_fits[["sol.27.7fg_sam"]],
	    			     catchval.exact = sam_settings[["sol.27.7fg"]][["catchval.exact"]],
	    			     fscale = sam_settings[["sol.27.7fg"]][["fscale"]],
	    			     year.base = sam_settings[["sol.27.7fg"]][["year.base"]],
	    			     fval = sam_settings[["sol.27.7fg"]][["fval"]],
	    			     ave.years = sam_settings[["sol.27.7fg"]][["ave.years"]],
	    			     rec.years = sam_settings[["sol.27.7fg"]][["rec.years"]],
	    			     splitLD = sam_settings[["sol.27.7fg"]][["splitLD"]],
	    			     nosim = sam_settings[["sol.27.7fg"]][["nosim"]]
	    )

	    ## Extract the SAM results

	    sam_results <- expand.grid(stock = sam_stks, year = c((now-1):(now+2)), fbar = NA, catch = NA, landings = NA, discards = NA,   ssb = NA)

	    for(i in sam_stks) {
		    	ii <- sapply(strsplit(i, ".", fixed = TRUE), "[[", 1)

	    	tab <- attributes(get(paste(ii, "sam_forecast", sep = "_")))$shorttab

			sam_results[sam_results$stock == i,"fbar"]     <- tab["fbar",]
			sam_results[sam_results$stock == i,"catch"]    <- tab["catch",]
			sam_results[sam_results$stock == i,"landings"] <- tab["Land",]
			sam_results[sam_results$stock == i,"discards"] <- tab["Discard",]
			sam_results[sam_results$stock == i,"ssb"]      <- tab["ssb",]
	    }

	    }

if(nep) {

nep.st.f3 <- lapply(nep.st.f3, function(x) {
			   n. <- x@name
			   x@stock.n[,YY] <- x@stock.n[,yr.now]

			   # But we also need to fill the landings etc..
			   x@landings.wt[,YY] <- x@landings.wt[,yr.assess]
			   x@discards.wt[,YY] <- x@discards.wt[,yr.assess]
			   #x@landings.n[,YY]  <- x@landings.n[,yr.assess]
			   #x@discards.n[,YY]  <- x@discards.n[,yr.assess]
			   x@catch.wt[,YY]  <- x@catch.wt[,yr.assess]

			   # case of landing obligation
			   if(LO) {
			   x@landings.n[,YY] <- 1
			   x@discards.n[,YY] <- 0
			   x@landings.wt[,YY] <- x@catch.wt[,YY]
			   x@discards.wt[,YY] <- 0
			   }

			   fmult.land <- F.land.pred[n.]/(x@harvest[,YY] * x@landings.n[,YY])

			   x@harvest[,YY]    <- x@harvest[,YY] * fmult.land
			   x@catch.n[,YY]    <- x@stock.n[,YY] * x@harvest[,YY]
			   x@landings.n[,YY] <- x@catch.n[,YY] * x@landings.n[,YY]
			   x@landings[,YY]   <- x@landings.n[,YY] * x@landings.wt[,YY]
			   x@discards.n[,YY] <- x@catch.n[,YY] - x@landings.n[,YY]
			   x@discards[,YY]   <- x@discards.n[,YY] * x@discards.wt[,YY]

return(x)})

# the estimated change in landings between FCube and the baseline for NEP stocks with UWTV is applied to the other stocks

tot.nep.f3.landings[,YY] <- sapply(nep.lst, function(x) nep.st.f3[[x]]@landings[,YY])

Ratio <- sum(tot.nep.f3.landings[nep.ass,YY]) /
	sum(sapply(nep.lst, function(x) nep.st.fwd[[x]]@landings[,YY])[nep.ass])

tot.nep.f3.landings[no.nep.ass,YY] <- sapply(nep.lst, function(x) nep.st.fwd[[x]]@landings[,YY])[no.nep.ass] * Ratio


			    } # if nep = T



   ## For any top up stocks, we need to raise the discards by DR
             if(AddDis == T) {
		     for(i in names(DR)) {
    if(i %in% names(dem.st.f3)) {
		       if(LO == T) {
    dem.st.f3[[i]]@landings.n[,YY]  <- dem.st.f3[[i]]@landings.n[,YY]/(1-DR[i])
    dem.st.f3[[i]]@catch.n[,YY]  <- dem.st.f3[[i]]@landings.n[,YY]
    }

    if(LO == F) {
      ## Include a discard.wt
      dem.st.f3[[i]]@discards.wt[,YY] <-   dem.st.f3[[i]]@catch.wt[,YY]

      # Raise catch.n
          dem.st.f3[[i]]@catch.n[,YY]  <-     
          dem.st.f3[[i]]@landings.n[,YY]/(1-DR[i])

      # fill discard.n  
    dem.st.f3[[i]]@discards.n[,YY]  <- (dem.st.f3[[i]]@catch.n[,YY] -
      dem.st.f3[[i]]@landings.n[,YY])

    # calc the new landings, discards and catch
    dem.st.f3[[i]]@landings[,YY]  <- computeLandings(dem.st.f3[[i]][,YY])
    dem.st.f3[[i]]@discards[,YY]  <- computeDiscards(dem.st.f3[[i]][,YY])
    dem.st.f3[[i]]@catch[,YY]     <- computeCatch(dem.st.f3[[i]][,YY])

    }
      }
             }
	     }    

            ## Feeding in the total catch data.frame for storing all catch info

	    if(nep) {
		    All.landings <-  c(sapply(dem.st.f3,function(x) landings(x)[,ac(YY)]), tot.nep.f3.landings[,ac(YY)])
	    }
	    if(!nep) {
		    All.landings <- c(sapply(dem.st.f3,function(x) landings(x)[,ac(YY)]))
	    }

	    if(UseSAM) {
	    for(i in sam_stks) {
	    All.landings[[i]] <- sam_results[sam_results$stock == i & sam_results$year == YY,"landings"]
	    }
	    }

	    All.landings <-All.landings[sort(names(All.landings))]

            # added to make the allcatchdataframwork...
            TACnow<-as.numeric(results$data[(results$sc == Run.name & results$year==YY & results$value=="landings")])
            names(TACnow)<-results$stock[(results$sc == Run.name & results$year==YY & results$value=="landings")]
            TACnow<-TACnow[sort(names(TACnow))]

            Legal.landings <-pmin(All.landings,TACnow) ##TO CHANGE - why is this note here?
            OverTAC.landings <- All.landings-TACnow
            OverTAC.landings[OverTAC.landings<0] <- 0

	    if(nep) {
  	disc <-  c(sapply(dem.st.f3, function(x) discards(x)[,yr.now]), sapply(nep.st.fwd, function(x) discards(x) [,yr.now]))
  	}

  	if(!nep) {
 	 disc <- c(sapply(dem.st.f3,function(x) discards(x)[,yr.now]))
  	}

	if(UseSAM) {
	    for(i in sam_stks) {
	    disc[[i]] <- sam_results[sam_results$stock == i & sam_results$year == YY,"discards"]
	    }
	    }

            disc <- disc[sort(names(disc))]

            tonnes <- round(c(Legal.landings,disc,OverTAC.landings),2)


        #************
        #! returning landings and discards into the fleets and metiers
        #first we need to trick the no.nep.ass for calculating the basic new landings in yr.now before applying the
        #fcube ratio, for the baseline

	    if(!nep) {
	fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
	                      # to test x <- fl.f3[[1]]
                        n. <- name(x)
                        x@metiers <- lapply(x@metiers, function(met) {
                                      #to test met <- x@metiers[[1]]
                                      m. <- name(met)
                                      met@catches <- lapply(met@catches, function(st) {
                                                      # to test st <-met@catches[[1]]
                                                     s. <- name(st)#toupper(name(st))
                                                     #there is a problem with this line -  F.land.pred[[s.]
                                                     ratio.f.land <- partF.land[[s.]][[n.]][m.]/F.land.pred[[s.]]
                                                     landings(st)[,YY] <-All.landings[[s.]]*ratio.f.land
                                                     discards(st)[,YY] <- landings(st)[,YY]*discards.sel(st)[,YY]/landings.sel(st)[,YY]
                                                     return(st)})
                                      return(met) })
                        return(x) }))

	    }


	    if(nep) {
        BaselineNepNoAss <- sapply(nep.lst, function(x) landings(nep.st.fwd[[x]])[,yr.assess])[no.nep.ass]/sapply(nep.lst, function(x) landings(nep.st.fwd[[x]])[,YY])[no.nep.ass]

            fl.f3 <- FLFleets(lapply(fl.f3, function(x) {
                        n. <- name(x)
                        x@metiers <- lapply(x@metiers, function(met) {
                                      m. <- name(met)
                                      met@catches <- lapply(met@catches, function(st) {
                                                     s. <- name(st)
                                                     if (!s. %in% no.nep.ass) {
                                                     ratio.f.land <- partF.land[[s.]][[n.]][m.]/F.land.pred[[s.]]
                                                     landings(st)[,YY] <-All.landings[[s.]]*ratio.f.land
						     } else {
						     baseline.land <- st@landings[,yr.assess] * BaselineNepNoAss[s.]
						     st@landings[,YY] <- baseline.land * Ratio

						     }
                                                     discards(st)[,YY] <- landings(st)[,YY]*discards.sel(st)[,YY]/landings.sel(st)[,YY]
                                                     return(st)})
                                      return(met) })
                        return(x) }))


	    }



            ##-----------------------------------------------------------------------------
            ## 2). RECALCULATE MANAGEMENT PLANS
            ##-----------------------------------------------------------------------------

            YYp1 <- as.numeric(YY)+1
            if (YYp1 <= LastProjectionYear) {
                YYp1 <- ac(YYp1)
                print("----MANAGEMENT PLAN----")
                #FORWARD
                dem.st.MP <- lapply(dem.st.f3, function(x) {
                                n. <- name(x)
                                  ctrl. <- ctrl.MP[[n.]]
                                srPar<-FLPar(c(Recr[n.,YYp1]),dimnames=list(params="a",iter=1))
                                x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
                                name(x) <- n.
                                return(x)
                                })

		# For NEP the advice is unchanged as we don't know how the results influence biomass

                #finding the landings target corresponding to this
                Ftarg.dem.yrTAC <- sapply(dem.st.MP,function(x) {
                                      n. <- x@name
                                      r. <- range[n., "minfbar"]:range[n., "maxfbar"]
                                      F.land <- harvest(x)[,YYp1]*landings.n(x)[,YYp1]/catch.n(x)[,YYp1]
                                      F.land[is.na(F.land)] <- 0 ## catch where stock is zero catch
                                      apply(F.land[as.character(r.)],2:6,mean,na.rm=T)})

		if(nep) {
		Ftarg.nep.yrTAC <- sapply(nep.st.fwd, function(x) {
				      n. <- x@name
				      r. <- range[n., "minfbar"]:range[n., "maxfbar"]
				      F.land <- x@harvest[,YYp1] * x@landings.n[,YYp1] / (x@landings.n[,YYp1] + x@discards.n[,YYp1])
				      })

		}
	 	
	    }
	
        } # end "!SkipIntermedYr | an(YY) !=now"

        ## Only TAC year - I DON'T THINK THIS IS NECESSARY FOR NEPHROPS AS WE AREN'T PROJECTING FORWARD THE STOCK
        if(SkipIntermedYr & an(YY) == now) {
            print("Skipping Intermediate year, calculate stock F")
            dem.st.f3 <- lapply(dem.st.f3, function(x) {  
                            n. <- name(x)
		              IntF <- ctrl.MP[[n.]]@target[,"val"][1]
			    ctrl.type <- ctrl.MP[[n.]]@target[,"quantity"][1]
                            ctrl. <- fwdControl(data.frame(year=YY,val=IntF,quantity=ctrl.type, rel.year=NA))
                            srPar<-FLPar(c(Recr[n.,YY]),dimnames=list(params="a",iter=1))
                            x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
                            })

            for (i in dem.names) {
                 dem.st.f3[[i]]@name <-i
                }

            YYp1 <- as.numeric(YY)+1
            if (YYp1 <= LastProjectionYear) {
                YYp1 <- ac(YYp1)
                print("----MANAGEMENT PLAN----")
                  #FORWARD
                dem.st.MP <- lapply(dem.st.f3, function(x) {
                                n. <- name(x)
                                  ctrl. <- ctrl.MP[[n.]]
                                srPar<-FLPar(c(Recr[n.,YYp1]),dimnames=list(params="a",iter=1))
                                x <- fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
                                name(x) <- n.
                                return(x)
                                })

                #finding the landings target corresponding to this
                Ftarg.dem.yrTAC <- sapply(dem.st.MP,function(x) {
                                      n. <- x@name
                                      r. <- range[n., "minfbar"]:range[n., "maxfbar"]
                                      F.land <- harvest(x)[,YYp1]*landings.n(x)[,YYp1]/catch.n(x)[,YYp1]
                                      F.land[is.na(F.land)] <- 0 ## catch where stock is zero catch
                                      apply(F.land[as.character(r.)],2:6,mean,na.rm=T)})

		if(nep) {
		Ftarg.nep.yrTAC <- sapply(nep.st.fwd, function(x) {
				      n. <- x@name
				      r. <- range[n., "minfbar"]:range[n., "maxfbar"]
				      F.land <- x@harvest[,YYp1] * x@landings.n[,YYp1] / (x@landings.n[,YYp1] + x@discards.n[,YYp1])
				      })
		}


            }
        } # end "SkipIntermedYr & an(YY) == now"

  } #### end of year loop


    #------------RESULTS--------------------
    print("----WRITE RESULTS----")
    years <- now:LastProjectionYear

    #landings
    results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                                 stock=rep(dem.names,each=length(years)),value="landings",
                                 data=unlist(lapply(dem.st.f3, function(x) round(computeLandings(x)[,ac(years)])))
    ))
    if(nep) {
    results <- rbind(results, cbind(sc = sc, year = rep(years, each = n.nep),
				    stock = rep(nep.names, length(years)), value = "landings",
				    data = c(tot.nep.f3.landings)
				    ))
    }

    #discards
    results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                                 stock=rep(dem.names,each=length(years)),value="discards",
                                 data=unlist(lapply(dem.st.f3, function(x) round(computeDiscards(x)[,ac(years)])))
    ))


    #TAC advice according to MP
#    results<-rbind(results,cbind(sc=sc,year=rep(years[-1],n.dem),
#                                 stock=rep(dem.names,each=length(years)-1),value="Ld_MgtPlan",
#                                 data=unlist(lapply(dem.st.MP, function(x) round(computeLandings(x)[,ac(years[-1])])))
 #   ))

#    if(nep) {
#    results <- rbind(results, cbind(sc = sc, year = rep(years[-1], n.nep),
#				    stock = rep(nep.names, each = length(years)-1), value = "Ld_MgtPlan",
#				    data = unlist(lapply(nep.st.fwd, function(x) round(x@landings[,ac(years[-1])])))
#				    ))

 #   }

    # Fmult
    results<-rbind(results,cbind(sc=sc,year=rep(years,n.dem),
                                 stock=rep(dem.names,each=length(years)),value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),
                                 data=unlist(lapply(dem.st.f3, function(x) round(sweep(fbar(x)[,ac(years)],c(1,3:6),fbar(x)[,yr.assess],FUN="/"),2)))
    ))

    if(nep) {
    results <- rbind(results, cbind(sc = sc, year = rep(years, n.nep),
				    stock = rep(nep.names, each = length(years)), value = paste("FmultVsF", substr(yr.assess,3,4), sep=""),
				    data = unlist(lapply(nep.st.f3, function(x) round(sweep(fbar(x)[,ac(years)],c(1,3:6), fbar(x)[,yr.assess],FUN="/"),2)))
				    ))
    }

    ## Fbar
    results <- rbind(results, cbind(sc = sc, year = rep(years, n.dem),
				    stock = rep(dem.names, each = length(years)), value = "Fbar",
				    data = unlist(lapply(dem.st.f3, function(x) fbar(x)[,ac(years)]))
				    ))

    if(nep) {
     results <- rbind(results, cbind(sc = sc, year = rep(years, n.nep),
				    stock = rep(nep.names, each = length(years)), value = "Fbar",
				    data = unlist(lapply(nep.st.f3, function(x) fbar(x)[,ac(years)]))
				    ))
    }


    #ssb
    years_ssb <- c(years,YYp1)
    results<-rbind(results,cbind(sc=sc,year=rep(years_ssb,n.dem),
                                 stock=rep(dem.names,each=length(years_ssb)),value="ssb",
                                 data=unlist(lapply(dem.st.f3, function(x) round(ssb(x)[,ac(years_ssb)])))
    ))

#    results<-rbind(results,cbind(sc=sc,year=rep(years_ssb,n.dem),
 #                                stock=rep(dem.names,each=length(years_ssb)),value="ssb_MgtPlan",
  #                               data=unlist(lapply(dem.st.MP, function(x) round(ssb(x)[,ac(years_ssb)])))
   # ))

    if(UseSAM) {
    
	    for(i in sam_stks) {
	    # landings
		    results[results$stock == i & results$value == "landings" & results$sc == sc & results$year %in% ac(years),"data"] <- sam_results[sam_results$stock == i & sam_results$year %in% ac(years), "landings"]
	    # discards 
		    results[results$stock == i & results$value == "discards" & results$sc == sc & results$year %in% ac(years),"data"] <- sam_results[sam_results$stock == i & sam_results$year %in% ac(years), "discards"]
	  # catch 
		    results[results$stock == i & results$value == "catch" & results$sc == sc & results$year %in% ac(years),"data"] <- sam_results[sam_results$stock == i & sam_results$year %in% ac(years), "catch"]
	  # fbar 
		    results[results$stock == i & results$value == "Fbar" & results$sc == sc & results$year %in% ac(years),"data"] <- sam_results[sam_results$stock == i & sam_results$year %in% ac(years), "fbar"]
	  # Fmult 
		    results[results$stock == i & results$value == paste0("FmultVsF",substr(now-1,3,4)) & results$sc == sc & results$year %in% ac(years),"data"] <- c(fbar(dem.stock[[i]])[,ac(now-1)]) *
			    sam_results[sam_results$stock == i & sam_results$year %in% ac(years), "fbar"]

	  # ssb 
		    results[results$stock == i & results$value == "ssb" & results$sc == sc & results$year %in% ac(years_ssb),"data"] <- sam_results[sam_results$stock == i & sam_results$year %in% ac(years_ssb), "ssb"]
	    }
    
    }



    print(Sys.time()-t1)  
  
  
} #end of scenarios


return(list(TACs,results))
}




