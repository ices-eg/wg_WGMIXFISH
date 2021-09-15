#############################################################################################################
## FLFCube - Reproduce the advice
## Celtic Sea exploratory analysis
## Running on R3.1.2 32bits
## library : FLCore 2.5.0 , FLash, FLAssess 2.5.0, FLFleet
## Update for WGMIXFISH 2020 - June 22 2020 (No new change since 2019)
#############################################################################################################
require(stockassessment)

Run.name <- "Reproduce_the_advice_Optim_2020"

ReproduceAdviceonOptim <- function(Ftarg) {

  
  wg.path <- file.path("results/clean_data/clean_stock_objects") # old path: results/clean_data/wgcse_stock_objects

  LO       <- TRUE 
  nep_LO   <- FALSE
  UseSAM   <- TRUE 
 
  assessment_type <- c("cod.27.7e-k" = "SAM",
		    "had.27.7b-k" = "SAM", 
		    "whg.27.7b-ce-k" = "SAM",
		    "hke.27.3a46-8abd" = "SS3", 
		    "meg.27.7b-k8abd" = "bayesian SCA",
		    "mon.27.78abd" = "a4a",
		    "sol.27.7e" = "XSA", 
		    "sol.27.7fg" = "SAM")

  load(file.path(wg.path, "SAM_fits", "sam_fits.RData"))
  
  
  
     ctrl.MP[["cod.27.7e-k"]] <- fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(1854,Ftarg["cod.27.7e-k"]),quantity=c("catch","f"),rel.year=c(NA,NA)))
     ctrl.MP[["had.27.7b-k"]] <- fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(1,Ftarg["had.27.7b-k"]),quantity=c("f","f"),rel.year=c(yr.now,NA)))
     ctrl.MP[["whg.27.7b-ce-k"]] <- fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(8772,Ftarg["whg.27.7b-ce-k"]),quantity=c("catch","f"),rel.year=c(NA,NA)))
     ctrl.MP[["mon.27.78abd"]] <- fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(1, Ftarg["mon.27.78abd"]),quantity=c("f","f"),rel.year=c(yr.now,NA)))
     ctrl.MP[["sol.27.7fg"]] <- fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(TAC.now["sol.27.7fg"],Ftarg["sol.27.7fg"]),quantity=c("catch","f"),rel.year=c(NA,NA)))
     ctrl.MP[["meg.27.7b-k8abd"]] <- fwdControl(data.frame(year=c(yr.now,yr.TAC),val=c(1, Ftarg["meg.27.7b-k8abd"]),quantity=c("f","f"),rel.year=c(yr.now,NA)))


   

###########################################################################
#### III. Forward projections 	                                       ####
###########################################################################
# Demersal 
  dem.st.fwd <- lapply(dem.stock, function(x) {
         	n. <- name(x)
          ctrl. <- ctrl.MP[[n.]] 
          yr. <- unique(ctrl.@target[,"year"])
          srPar<-FLPar(c(Recr[n.,yr.]),dimnames=list(params="a",year=yr.,iter=1))
	  if(assessment_type[[n.]] == "SAM" & UseFwdF3) {
	  x <- fwdF3(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
	  } else {
          x <- FLash::fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
	  }
	  return(x)
          })
  
  dem.names2<-names(dem.st.fwd)

  for (i in 1:length(dem.names)) {
    name(dem.st.fwd[[i]])<-dem.names2[i] }


  ## We need to rerun with FwdF3

  if(UseFwdF3) { 
dem.st.fwd <- lapply(dem.st.fwd, function(x) {
         	n. <- name(x)
          ctrl. <- ctrl.MP[[n.]] 
          yr. <- unique(ctrl.@target[,"year"])
          srPar<-FLPar(c(Recr[n.,yr.]),dimnames=list(params="a",year=yr.,iter=1))
	  if(assessment_type[[n.]] == "SAM" & UseFwdF3) {
	  x <- fwdF3(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
	  } else {
          x <- FLash::fwd(x,ctrl=ctrl.,sr=list(model="mean",params=srPar))
	  }
	  return(x)
          })
  
  }


  
  ## Adding discards to the cod forecast (both intermediate year and TAC year - updated in 2018)
  
  # to test use "cod.27.7e-k"
  
  if(AddDis == T) {
	  
	  for(i in names(DR)) {
		  if(i %in% names(dem.st.fwd)) {

    if(LO == T) {
	dem.st.fwd[[i]]@landings.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <- dem.st.fwd[[i]]@landings.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]/(1-DR[i])
     	dem.st.fwd[[i]]@catch.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <- dem.st.fwd[[i]]@landings.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]
	    }
    
    if(LO == F) {
      ## Include a discard.wt
      dem.st.fwd[[i]]@discards.wt[,ac((as.numeric(yr.TAC)-1):yr.TAC)] <-
        dem.st.fwd[[i]]@catch.wt[,ac((as.numeric(yr.TAC)-1):yr.TAC)]
      
      # Raise catch.n
          dem.st.fwd[[i]]@catch.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <- dem.st.fwd[[i]]@landings.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]/(1-DR[i])
          
      # fill discard.n  
    dem.st.fwd[[i]]@discards.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <- (dem.st.fwd[[i]]@catch.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)] -
      dem.st.fwd[[i]]@landings.n[,ac((as.numeric(yr.TAC)-1):yr.TAC)])

    # calc the new landings, discards and catch
    dem.st.fwd[[i]]@landings[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <-
      computeLandings(dem.st.fwd[[i]][,ac((as.numeric(yr.TAC)-1):yr.TAC)])
    dem.st.fwd[[i]]@discards[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <-
      computeDiscards(dem.st.fwd[[i]][,ac((as.numeric(yr.TAC)-1):yr.TAC)])
    dem.st.fwd[[i]]@catch[,ac((as.numeric(yr.TAC)-1):yr.TAC)]  <-
      computeCatch(dem.st.fwd[[i]][,ac((as.numeric(yr.TAC)-1):yr.TAC)])
    
    }


    }
    }
  }
    
 
  for (i in 1:length(dem.names)) {
    name(dem.st.fwd[[i]])<-dem.names2[i] }

#######################
## New: SAM stocks
#######################

if(UseSAM) {

## We run a SAM forecast, with the same code as the single stock
## and replace the FLStock forecast outputs with those from SAM

## Stocks with SAM forecasts

sam_stks <- names(assessment_type[assessment_type == "SAM"])

## SAM settings for STF
sam_settings <- list()

sam_settings[["cod.27.7e-k"]] <- list("fval" =   c(c(fbar(wg.stock[["cod.27.7e-k"]])[,ac(now-1)]), ## last year
					0.477, # int year
					rep(Ftarg[["cod.27.7e-k"]],2)), ## TAC year. TAC year + 1
			  "ave.years" = c((now-3):(now-1)), ## average bio params
			  "rec.years" = c(2015:(now-1)), 
			  "splitLD" = TRUE, 
			  "nosim" = 50001,
			  "seed" = 12345)

sam_settings[["had.27.7b-k"]] <- list("fval" = c(c(fbar(wg.stock[["had.27.7b-k"]])[,ac(now-1)]), # last year
						 c(fbar(wg.stock[["had.27.7b-k"]])[,ac(now-1)]), # int year = Fsq
						 c(rep(Ftarg["had.27.7b-k"],2))),
				      "ave.years" = c((now-3):(now-1)), ## average bio params
				      "rec.years" = c(1993:(now-1)), 
				      "splitLD" = TRUE,
				      "nosim" = 10001)

sam_settings[["whg.27.7b-ce-k"]] <-list("fval" = c(c(fbar(wg.stock[["whg.27.7b-ce-k"]])[,ac(now-1)]), # last year
						 0.495, # int year, = Fmix
					rep(Ftarg[["whg.27.7b-ce-k"]],2)), ## TAC year. TAC year + 1
				      "ave.years" = c((now-3):(now-1)), ## average bio params
				      "rec.years" = c(2010:(now-1)), 
				      "splitLD" = TRUE,
				      "nosim" = 50001)

sam_settings[["sol.27.7fg"]] <- list("catchval.exact" = c(NA,
							  1652,  ## int year catch constraint
							  NA, NA),
				     "fscale" = c(NA, NA, NA, 1),
				     "fval" = c(c(fbar(wg.stock[["sol.27.7fg"]])[,ac(now-1)]),
						NA, 
						Ftarg["sol.27.7fg"],
						NA),
				     "year.base" = 2019,
				     "ave.years" = c((now-3):(now-1)),
				     "rec.years" = c(1971:(now-3)),
				     "splitLD" = TRUE,
				     "nosim" = 10000,
				     "seed" = 123)


### If we're using LO, we need to overwrite the landFrac in the data. This is not ideal, and I have discussed with Anders how to improve for future.

if(LO) {
for(i in sam_stks) {
SAM_fits[[paste(i,"sam", sep = "_")]]$data$landFrac[ac(sam_settings[[i]][["ave.years"]]),] <- 1
SAM_fits[[paste(i,"sam", sep = "_")]]$data$disMeanWeight[ac(sam_settings[[i]][["ave.years"]]),] <- NA
SAM_fits[[paste(i,"sam", sep = "_")]]$data$landMeanWeight[ac(sam_settings[[i]][["ave.years"]]),] <- SAM_fits[[paste(i,"sam", sep = "_")]]$data$catchMeanWeight[ac(sam_settings[[i]][["ave.years"]]),]  
}
}

## cod sam forecast
set.seed(sam_settings[["cod.27.7e-k"]][["seed"]])
cod_sam_forecast <- forecast(SAM_fits[["cod.27.7e-k_sam"]], 
			     fval = sam_settings[["cod.27.7e-k"]][["fval"]],
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

####################
# NEPHROPS STOCKS
####################

  if(nep) {
    nep.stock <- FLStocks(lapply(nep.lst,function(x) wg.stock[[x]]))
 

  # If we dont have the last year harvest then.... 
    # Need to make an assumption about last year harvest - assume same landings.wt and discards.wt, 
    # then calc landings.n and discards.n based on tonnages landed
    nep.stock <- FLStocks(lapply(nep.stock, function(x) {

      if (name(x) == "nep.fu.17") {table2.nyears <- length(2008:yr.assess)+1} # FU17 uses data series since 2008. +1 because present year is empty
      if (name(x) %in% c("nep.fu.16", "nep.fu.19", "nep.fu.2021", "nep.fu.22")) {table2.nyears <- 3+1} # +1 because present year is empty
      if (name(x) == "nep.out.7") {table2.nyears <- 3+1} # +1 because present year is empty
      
      
                    x <- stf(x, nyears = 3, wts.nyears = table2.nyears, disc.nyears = 3+1, fbar.nyears = 1+1)
                    
                   
                    # These are necesary as the Stock objects are already extended to the Int Yr, with the survey data
                    x@stock.n[,ac(yr.now:yr.TAC)] <- x@stock.n[,yr.now]  ### yr.now as have surveys # Mikel: Do we need a stock.n for next year?
		                x@harvest[,ac(yr.now:yr.TAC)] <- x@harvest[,yr.assess]  # Mikel: yr.TAC is already setup by stf()...right?
		                x@landings.n[,ac(yr.now)] <- x@landings.n[,ac(yr.TAC)]
		                x@discards.n[,ac(yr.now)] <- x@discards.n[,ac(yr.TAC)]
		  ##
                    
                    return(x)
                    }))
    
    
    

    Fbar.nep <- sapply(nep.stock,function(x) harvest(x)[,yr.assess])
    nep.ass <- names(Fbar.nep)[which(!is.na(Fbar.nep))] #stocks with an assessment, not the ass of the nep ;-)
    no.advice.nep <- names(Fbar.nep)[which(!names(Fbar.nep) %in% nep.ass & !names(Fbar.nep) %in% names(PA_Advice.nep))]
    no.nep.ass <- names(Fbar.nep)[which(!names(Fbar.nep) %in% nep.ass)]
  
    # Split the TAC among FUs based on last year's landings (we already excluded TAC corresponding to 7a previously in '5. Setup year references and TACs').
    tac_per <- NULL # table to know the percentages of landings of previous year
    for (i in nep.names) {
      tac_per <- rbind(tac_per, cbind(i, as.numeric(landings(nep.stock[[i]])[,yr.assess]/Sums(lapply(nep.stock, function(x) x@landings[,ac(yr.assess)]))[,yr.assess])))
      tac <- TAC.now["NEP-CS"]*landings(nep.stock[[i]])[,yr.assess]/Sums(lapply(nep.stock, function(x) x@landings[,ac(yr.assess)]))[,yr.assess]
      TAC.now <- c(TAC.now, i=as.numeric(tac))
      }
    names(TAC.now)[grep("i",names(TAC.now))] <- nep.names
  
    nep.st.fwd <- lapply(nep.stock, function(x) {
                    n. <- name(x)
                    

	
        # yr.now assumption: HR based on TAC.now
          x@landings[,yr.now] <- TAC.now[n.]
          x@landings.wt[,yr.now] <- x@landings.wt[,yr.TAC]
          # catch.n = Landings + Dead & Surviving discards:
          x@catch.n[,yr.now] <- x@landings.n[,yr.now]/(1-x@discards.n[,yr.now]) # discards proportion is an average of previous 3 years
          # x@catch.n[,yr.now] <- x@landings.n[,yr.now]/(1-(x@discards.n[,yr.now] * (1-as.numeric(nep.surv[n.])))) # NO
          # Harvest rate = (Landings + Dead discards) / Abundance
          x@harvest[,yr.now] <- x@landings.n[,yr.now]/(1-(x@discards.n[,yr.now] * (1-as.numeric(nep.surv[n.]))))/x@stock.n[,yr.now]


        
        # yr.TAC ADVICE: HR based on target
          x@harvest[,yr.TAC] <- Fmsy.nep[n.]
        
  		    # case of Landings obligation
  		    if(nep_LO == T) { # Here there is no survival rate
  		    # landings.n(x)[,ac(yr.TAC:yr.TACp1)] <- 1
  		    # discards.n(x)[,ac(yr.TAC:yr.TACp1)] <- 0
  		    # landings.wt(x)[,ac(yr.TAC:yr.TACp1)] <- catch.wt(x)[,ac(yr.TAC:yr.TACp1)]
  		    # ADVICE
  		     # Discards by number = Abundance * Fmsy * Recent Discard rate
  		     x@discards.n[,yr.TAC] <- x@stock.n[,yr.TAC] * x@harvest[,yr.TAC] * x@discards.n[,yr.TAC]
  		     x@discards.n[is.na(x@discards.n)] <- 0
           # Landings by number = Abundance * Fmsy * 1-Recend Discard rate
  		     x@landings.n[,yr.TAC] <- x@stock.n[,yr.TAC] * x@harvest[,yr.TAC] * x@landings.n[,yr.TAC]
           # Landings and discard by weight
           x@landings[,yr.TAC] <- x@landings.n[,yr.TAC] * x@landings.wt[,yr.TAC]
  		     x@discards[,yr.TAC] <- x@discards.n[,yr.TAC] * x@discards.wt[,yr.TAC]
  		     # Catch = Landings + Discards
           x@catch.n[,yr.TAC] <- x@landings.n[,yr.TAC] + x@discards.n[,yr.TAC]
  		     x@catch[,yr.TAC] <- x@landings[,yr.TAC] + x@discards[,yr.TAC]
  		     
  		           } else if (nep_LO == F) {
  
          
           # Landings by number = (Abundance * Fmsy) - Dead discards
           x@landings.n[,yr.TAC] <- (x@stock.n[,yr.TAC] * x@harvest[,yr.TAC]) - (x@stock.n[,yr.TAC] * x@harvest[,yr.TAC] * nep.dead.dr[n.])
           # Discards by number = Dead discards + Surviving discards
           x@discards.n[,yr.TAC] <- (x@stock.n[,yr.TAC] * x@harvest[,yr.TAC] * nep.dead.dr[n.]) + (x@stock.n[,yr.TAC] * x@harvest[,yr.TAC] * nep.dead.dr[n.] * (nep.surv[n.]/(1-nep.surv[n.])))
           x@discards.n[is.na(x@discards.n)] <- 0
           x@discards.wt[is.na(x@discards.wt)] <- 0
           # Landings and discard by weight
  		     x@landings[,yr.TAC] <- x@landings.n[,yr.TAC] * x@landings.wt[,yr.TAC]
  		     x@discards[,yr.TAC] <- x@discards.n[,yr.TAC] * x@discards.wt[,yr.TAC]
           # Catch
           x@catch.n[,yr.TAC] <- x@landings.n[,yr.TAC] + x@discards.n[,yr.TAC] # Catch = Landings + Dead & Surviving discards
  		     # x@catch.n[,yr.TAC] <- x@landings.n[,yr.TAC] + (x@discards.n[,yr.TAC] * (1-as.numeric(nep.surv[n.]))) # NO Catch = Landings + Dead discards
           x@catch[,yr.TAC] <- x@landings[,yr.TAC] + x@discards[,yr.TAC] # Catch = Landings + Dead & Surviving discards
           # x@catch[,yr.TAC] <- x@landings[,yr.TAC] + (x@discards[,yr.TAC] * (1-as.numeric(nep.surv[n.]))) # Catch = Landings + Dead discards
  
  		     
  		           }

        if(n. %in% no.nep.ass) x@catch[,yr.TAC] <- x@landings[,yr.TAC] <- PA_Advice.nep[n.]
		    
 		    x@landings[is.na(x@landings)] <- 0
		    x@discards[is.na(x@discards)] <- 0

		    x@catch <- landings(x) + discards(x) ## Add in catches
     return(x)
                    })
    }

  results <- data.frame()
    results<- rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.dem)),stock=rep(dem.names2,2),value="landings",data=c(sapply(dem.st.fwd, function(x) round(computeLandings(x)[,yr.now])),sapply(dem.st.fwd, function(x) round(computeLandings(x)[,yr.TAC])))))
  
  if( nep == T ) {results <- rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                                 stock=rep(nep.names ,2),value="landings",
                                                 data=c(sapply(nep.st.fwd, function(x) round(landings(x)[,yr.now])),
                                                        sapply(nep.st.fwd, function(x) round(landings(x)[,yr.TAC])))))
  }
  
  results<-rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.dem)),stock=rep(dem.names2,2),value="discards",data=c(sapply(dem.st.fwd, function(x) round(computeDiscards(x)[,yr.now])),sapply(dem.st.fwd, function(x) round(computeDiscards(x)[,yr.TAC])))))
  
  if( nep == T ) {results <- rbind(results,
                                   cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                         stock=rep(nep.names ,2),value="discards",
                                         data=c(sapply(nep.st.fwd, function(x) round(computeDiscards(x)[,yr.now])),
                                                sapply(nep.st.fwd, function(x) round(computeDiscards(x)[,yr.TAC])))))
    if(nep_LO == F) {results <- rbind(results,
                                      cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                         stock=rep(nep.names ,2),value="discards.dead",
                                         data=c(sapply(nep.st.fwd, function(x) round(computeDiscards(x)[,yr.now] * (1-nep.surv[name(x)]))),
                                                sapply(nep.st.fwd, function(x) round(computeDiscards(x)[,yr.TAC] * (1-nep.surv[name(x)]))))),
                                      cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                         stock=rep(nep.names ,2),value="discards.surviving",
                                         data=c(sapply(nep.st.fwd, function(x) round(computeDiscards(x)[,yr.now] * (nep.surv[name(x)]))),
                                                sapply(nep.st.fwd, function(x) round(computeDiscards(x)[,yr.TAC] * (nep.surv[name(x)]))))))
    }}
  
  results<-rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.dem)),stock=rep(dem.names2,2),value="catch",data=c(sapply(dem.st.fwd, function(x) round(computeCatch(x)[,yr.now])),sapply(dem.st.fwd, function(x) round(computeCatch(x)[,yr.TAC])))))
  
  if( nep == T ) {results <- rbind(results, cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                                  stock=rep(nep.names ,2),value="catch",
                                                  data=c(
                                                    # Mikel: catch.wt are not reliable, so I will don't use computeCatch()
                                                    # How to fix this?
                                                      # sapply(nep.st.fwd, function(x) round(computeCatch(x)[,yr.now])),
                                                      # sapply(nep.st.fwd, function(x) round(computeCatch(x)[,yr.TAC])))))
                                                    # For the moment:
                                                    # Instead, I will use just the Catch(x) calculated as Landings + Dead & Surviving discards
                                                    sapply(nep.st.fwd, function(x) round(catch(x)[,yr.now])),
                                                    sapply(nep.st.fwd, function(x) round(catch(x)[,yr.TAC])))))
  }
  
  results<-rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.dem)),stock=rep(dem.names2,2),value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),data=c(sapply(dem.st.fwd, function(x) round(fbar(x)[,yr.now]/fbar(x)[,yr.assess],2)),sapply(dem.st.fwd, function(x) round(fbar(x)[,yr.TAC]/fbar(x)[,yr.assess],2)))))
  
  if( nep == T ) {results <- rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                                 stock=rep(nep.names ,2),value=paste("FmultVsF",substr(yr.assess,3,4),sep=""),
                                                 data=c(sapply(nep.st.fwd, function(x) round(fbar(x)[,yr.now]/fbar(x)[,yr.assess],3)),
                                                        sapply(nep.st.fwd, function(x) round(fbar(x)[,yr.TAC]/fbar(x)[,yr.assess],3)))))
  }
  
  results<-rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.dem)),stock=rep(dem.names2,2),value="Fbar",data=c(sapply(dem.st.fwd, function(x) round(fbar(x)[,yr.now],2)),sapply(dem.st.fwd, function(x) round(fbar(x)[,yr.TAC],2)))))
  
  if( nep == T ) {results <- rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC),n.nep)),
                                                 stock=rep(nep.names ,2),value="Fbar",
                                                 data=c(sapply(nep.st.fwd, function(x) round(fbar(x)[,yr.now],3)),
                                                        sapply(nep.st.fwd, function(x) round(fbar(x)[,yr.TAC],3)))))
  }
  
  results<-rbind(results,cbind(sc=Run.name,year=sort(rep(c(yr.now,yr.TAC,yr.TACp1),n.dem)),stock=rep(dem.names2,3),value="ssb",data=c(sapply(dem.st.fwd, function(x) round(ssb(x)[,yr.now])),sapply(dem.st.fwd, function(x) round(ssb(x)[,yr.TAC])),sapply(dem.st.fwd, function(x) round(ssb(x)[,yr.TACp1])))))

  ### If using SAM, overwrite the FLR results

  if(UseSAM) {
  sam_results$stock <- factor(as.character(sam_results$stock))
  sam_results      <- sam_results[order(sam_results$year, sam_results$stock),]

  results[results$value == "landings" & results$stock %in% sam_stks, "data"] <- sam_results[sam_results$year %in% c(now:(now+1)),"landings"]
  results[results$value == "catch" & results$stock %in% sam_stks, "data"] <- sam_results[sam_results$year %in% c(now:(now+1)),"catch"]
  results[results$value == "discards" & results$stock %in% sam_stks, "data"] <- sam_results[sam_results$year %in% c(now:(now+1)),"discards"]
  results[results$value == "ssb" & results$stock %in% sam_stks, "data"] <- sam_results[sam_results$year %in% c(now:(now+2)),"ssb"]
  results[results$value == "Fbar" & results$stock %in% sam_stks, "data"] <- sam_results[sam_results$year %in% c(now:(now+1)),"fbar"]

  ## Need to add on Fmult..

  }


return(results)

}
