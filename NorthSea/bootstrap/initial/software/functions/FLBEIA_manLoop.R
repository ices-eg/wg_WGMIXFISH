FLBEIA_manLoop <- function(biols, SRs = NULL, BDs = NULL, fleets, covars = NULL, indices = NULL, advice = NULL, biols.ctrl, 
        fleets.ctrl, covars.ctrl, obs.ctrl, assess.ctrl,  advice.ctrl, assessmentYr = NULL){
  
    if(is.null(assessmentYr)){stop("Must define assessmentYr (numeric year value")}
    
    # define main control c(assessmentYr, assessmentYr+1)
    main.ctrl <- list(sim.years = c(initial = assessmentYr, final = assessmentYr+1))

    # Extract the common dimensions [year, season, it] from the 1st Biol.
    ny <- dim(biols[[1]]@n)[2]
    ns <- dim(biols[[1]]@n)[4]
    it <- dim(biols[[1]]@n)[6]
    minyear <- ac(dims(biols[[1]])$minyear)
    maxyear <- ac(dims(biols[[1]])$maxyear)
    seasons <- 1:ns
    
    # Stock names
    stnms <- names(biols)
    
    # If SimultaneousMngt argument missing in main.ctrl => set it to FALSE, the original FLBEIA configuration.
    main.ctrl$SimultaneousMngt <- ifelse(is.null(main.ctrl$SimultaneousMngt), FALSE, ifelse(is.na(main.ctrl$SimultaneousMngt), FALSE, main.ctrl$SimultaneousMngt))
   
    # Check that all FLQuants have the rigth [ny,ns,it] dimensions. 
    chckdim0 <- FLBEIA:::checkDims(biols,  minyear, maxyear, ns, it)
    chckdim1 <- FLBEIA:::checkDims(fleets, minyear, maxyear, ns, it)
    if(!is.null(covars)) chckdim2 <- FLBEIA:::checkDims(covars, minyear, maxyear, ns, it)
   
    # Extract years, check and convert into positions.
    sim.years <- as.numeric(main.ctrl$sim.years)
    if(!(sim.years[1] %in% as.numeric(minyear):as.numeric(maxyear))) stop('First simulation year is outside year range in the objects')
    if(!(sim.years[length(sim.years)] %in% as.numeric(minyear):as.numeric(maxyear))) stop('Last simulation year is outside year range in the objects')
    # convert sim.years in positon is the FLR objects.
    sim.years <- which(sim.years[1] == as.numeric(minyear):as.numeric(maxyear)):which(sim.years[2] == as.numeric(minyear):as.numeric(maxyear))
    
    # Check if the argument LandObl is missing for any fleet in fleets.ctrl. 
    # No Landing Obligation if the argument is missing.
    for (flnm in names(fleets)){ 
      if(is.null(fleets.ctrl[[flnm]]$LandObl)) fleets.ctrl[[flnm]]$LandObl<-FALSE
    }
    # Check if the argument AdvCatch is missing for any stock in advice.ctrl. 
    # AdvCatch = FALSE (TAC in terms of landings for each year) in case AdvCatch is missing.
    for (st in names(biols)){ 
      if(is.null(advice.ctrl[[st]]$AdvCatch)){
         advice.ctrl[[st]]$AdvCatch <- rep(FALSE,ny)
         names(advice.ctrl[[st]]$AdvCatch) <- c(as.numeric(minyear):as.numeric(maxyear))
      }
    }
    
    stocks         <- vector('list', length(stnms)) 
    names(stocks) <- stnms
    
    if(main.ctrl$SimultaneousMngt == FALSE) {
      
      # Define advice conditions:
      adv.yr <- adv.ss <- vector('list', length(stnms))
      names(adv.yr) <- names(adv.ss) <- stnms
      for (st in stnms) {
        
        # Advice years
        adv.yr[[st]] <- advice.ctrl[[st]][['adv.year']] # advice years
        if (is.null(adv.yr[[st]])) { # no value, then advice yearly
          adv.yr[[st]] <- sim.years
        } else if (adv.yr[[st]]=='all' | is.na(adv.yr[[st]])) {
          adv.yr[[st]] <- sim.years
        } else { # convert advice years into positions
          adv.yr[[st]] <- as.numeric(adv.yr[[st]])
          if(sum(!(adv.yr[[st]] %in% as.numeric(minyear):as.numeric(maxyear)))>0) # check
            stop("Advice years for: '", st, "' outside year range in the objects")
          # convert adv.yr[[st]] in positon of the FLR objects
          for (i in 1:length(adv.yr[[st]])) adv.yr[[st]][i] <- which(adv.yr[[st]][i] == as.numeric(minyear):as.numeric(maxyear))
        }
        
        # Advice seasons
        adv.ss[[st]] <- advice.ctrl[[st]][['adv.season']]
        if (is.null(adv.ss[[st]])) { 
          adv.ss[[st]] <- advice.ctrl[[st]][['adv.season']] <- ns 
        } else if (is.na(adv.ss[[st]])) { 
          adv.ss[[st]] <- advice.ctrl[[st]][['adv.season']] <- ns 
        }
        # For SSFB required advice season (if different to last one) for calculating season shares appropriately
        for (fl in names(fleets)) if (fleets.ctrl[[fl]]$effort.model == "SSFB")
          fleets.ctrl[[fl]][[st]]$adv.season <- adv.ss[[st]]
        if (!(adv.ss[[st]] %in% seasons)) stop("Advice season for: '", st, "' outside season range in the objects")
        
        # Advice year assessment necessary?
        # assess.ctrl[[st]]$ass.curryr=TRUE if assessment estimates also for advice year are needed
        if (is.null(assess.ctrl[[st]]$ass.curryr)) { assess.ctrl[[st]]$ass.curryr <- F } else if (is.na(assess.ctrl[[st]]$ass.curryr)) { assess.ctrl[[st]]$ass.curryr <- F }
        if (assess.ctrl[[st]]$ass.curryr==TRUE) obs.ctrl[[st]]$obs.curryr <- T
        # Advice year observations necessary?
        # obs.ctrl[[st]]$obs.curryr=TRUE if observations also for advice year are needed
        if (is.null(obs.ctrl[[st]]$obs.curryr)) { obs.ctrl[[st]]$obs.curryr <- F } else if (is.na(obs.ctrl[[st]]$obs.curryr)) { obs.ctrl[[st]]$obs.curryr <- F }
        
      }
      
    } else { # if main.ctrl$SimultaneousMngt == TRUE:
      
      # Advice years NOT to be defined (are assumed to be all years)
      if (!is.null(advice.ctrl[[st]][['adv.year']]))
        stop("Advice years for: '", st, "' should not be defined if  main.ctrl$SimultaneousMngt == TRUE.
              See advice.ctrl[['", st, "']][['adv.year']].")
      
      # Advice seasons NOT to be defined (are assumed to be only once, in the last season)
      if (!is.null(advice.ctrl[[st]][['adv.season']]))
        stop("Advice seasons for: '", st, "' should not be defined if main.ctrl$SimultaneousMngt == TRUE. 
              See advice.ctrl[['", st, "']][['adv.season']].")
      
      # No possibility of doing the assessment in current year
      obs.ctrl <- lapply(obs.ctrl, function(x){
        x[['obs.curryr']] <- FALSE
        return(x)})
      assess.ctrl <- lapply(assess.ctrl, function(x){
        x[['ass.curryr']] <- FALSE
        return(x)})
      
    }
    
    for(yr in sim.years){
      for(ss in seasons){
        
        
        cat('############################################################\n')
        cat('-                   Year: ', as.numeric(minyear) + yr -1, ', Season: ',ss, '\n')
        cat('############################################################\n')
        
        
        # #~~~~~~~~~~~~~~~~ OPERATING MODELS (seasonal) ~~~~~~~~~~~~~~~~~~~~~#
        # 
        # cat('************ OPERATING MODEL***************************\n')
        # 
        # cat('------------ BIOLOGICAL OM ------------\n')
        # # - Biologic OM.
        # res   <- FLBEIA:::biols.om (biols = biols, fleets = fleets, SRs = SRs, BDs = BDs, covars = covars, biols.ctrl = biols.ctrl, year = yr, season = ss)
        # biols <- res$biols
        # SRs   <- res$SRs
        # BDs   <- res$BDs
        # 
        # cat('------------ FLEETS OM ------------\n')
        # # - Fleets OM.
        # res        <- FLBEIA:::fleets.om(fleets = fleets, biols = biols, BDs = BDs, covars = covars, advice = advice, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl, 
        #                         assess.ctrl=assess.ctrl, advice.ctrl = advice.ctrl, year = yr, season = ss)
        # fleets     <- res$fleets
        # fleets.ctrl <- res$fleets.ctrl
        # covars     <- res$covars
        # 
        # cat('------------ COVARS OM ------------\n')
        # # - Covariables OM. (the covariables can affect the covariables themselfs but also the biols and fleets, biols.ctrl and fleets.ctrl)
        # res    <- FLBEIA:::covars.om(fleets = fleets, biols = biols, SRs = SRs, covars = covars, advice = advice, covars.ctrl = covars.ctrl, year = yr, season = ss)
        # covars <- res$covars
        # biols  <- res$biols
        # fleets <- res$fleets
        # SRs    <- res$SRs
        
        
        # In last year of the simulation, if last season, there is no assessment => go to the end.
        if(yr == sim.years[length(sim.years)] & ss == ns) next    
         
        if(main.ctrl$SimultaneousMngt == FALSE){   
          for (st in stnms) {
          
            if (yr %in% adv.yr[[st]] & ss == adv.ss[[st]]) {
              
              yr.man <- ifelse( adv.ss[[st]]==ns, yr, yr+1)
        
              #~~~~~~~~~~~~~~~~ MANAGEMENT PROCEDURE.  (>=annual) ~~~~~~~~~~~~~~~#
              cat('************ MANAGEMENT PROCEDURE ****************************\n')
          
              # - Observation.
              cat('----------- OBSERVATION MODEL ------------\n')
              res          <- FLBEIA:::observation.mp(biols = biols, fleets = fleets, covars = covars, indices = indices, 
                                  advice = advice, obs.ctrl = obs.ctrl, year = yr.man, season=ss, stknm=st)
              stocks[[st]] <- res$stock
              fleets.obs   <- res$fleets.obs
              indices      <- res$indices
                  
              # - Assessment.
              cat('------------ ASSESSMENT MODEL ------------\n')
              datayr <- dimnames(biols[[1]]@n)[[2]][yr.man-1]
            
              res <- FLBEIA:::assessment.mp(stocks = stocks, fleets.obs = fleets.obs, indices = indices, covars=covars, 
                                      assess.ctrl = assess.ctrl, datayr = datayr, season=ss, stknm=st)  
              stocks <- res$stocks
              covars <- res$covars
  
              # - Advice. 
              cat('----------------- ADVICE -----------------\n')
              advice.ctrl[[st]][['tac.lag']] <- ifelse (advice.ctrl[[st]][['adv.season']] == ns, 0, 1)
              
              advice <- FLBEIA:::advice.mp(stocks = stocks, fleets.obs = fleets.obs, indices = indices, covars = covars, 
                                  advice = advice, advice.ctrl = advice.ctrl, year = yr, season = ss, stknm=st)
      
        }}}}
      
        if(main.ctrl$SimultaneousMngt == TRUE & yr < sim.years[length(sim.years)]){  # Simultaneous and Yearly management. 
        
        #~~~~~~~~~~~~~~~~ MANAGEMENT PROCEDURE.  (>=annual) ~~~~~~~~~~~~~~~#
        cat('************ MANAGEMENT PROCEDURE ****************************\n')
        
        stocks <- vector('list', length(stnms))
        names(stocks) <- stnms
        #indices <- vector('list', length(stnms))
        #names(indices) <- stnms
        
        # - Observation.
        cat('----------- OBSERVATION MODEL ------------\n')
        for(st in stnms){

          res          <- FLBEIA:::observation.mp(biols = biols, fleets = fleets, covars = covars, indices = indices, 
                                       advice = advice, obs.ctrl = obs.ctrl, year = yr, season=ns, stknm=st)
          stocks[[st]] <- res$stock
          fleets.obs   <- res$fleets.obs
          indices      <- res$indices
        }
        
        # - Assessment.
        cat('------------ ASSESSMENT MODEL ------------\n')
        for(st in stnms){
        
          res <- FLBEIA:::assessment.mp(stocks = stocks, fleets.obs = fleets.obs, indices = indices, covars = covars, 
                               assess.ctrl = assess.ctrl, datayr = yr-1, season=ns, stknm=st)    
          stocks[[st]] <- res$stocks[[st]]
          covars <- res$covars
          }
        
        
        # - Advice. 
        cat('----------------- ADVICE -----------------\n')
        for(st in stnms){
          advice <- FLBEIA:::advice.mp(stocks = stocks, fleets.obs = fleets.obs, indices = indices, covars = covars, 
                            advice = advice, advice.ctrl = advice.ctrl, year = yr, season = ns, stknm=st)
          }
        }

    }
    
    if(!exists('stocks'))  stocks <- NULL
    
    return(list(biols = biols, fleets = fleets, covars = covars,  advice = advice, stocks = stocks, indices = indices, BDs = BDs, SRs = SRs, fleets.ctrl = fleets.ctrl,
                      pkgs.versions = installed.packages(fields = 'Packaged')[,c('Built', 'Version', 'Packaged')]))
}
