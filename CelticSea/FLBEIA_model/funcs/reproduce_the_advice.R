setAs('FLStock', 'FLFleetExt',
      function(from)
      {
        dms <-  dimnames(from@stock.n)
        
        ## Fleet level
        
        eff   <- cap <- FLQuant(dimnames=list(year=dms$year),quant="all")
        eff[] <- 1
        cap[] <- 5
        
        ## metier level
        effsh   <- FLQuant(dimnames=list(year=dms$year),quant="all")
        effsh[] <- 1
        
        ## catch level
        pr   <- al <- be <- FLQuant(dimnames=list(year=dms$year,
                                    age = dms$age), quant=ifelse(dms$age[1] == "all","all", "age"))
        
        pr[] <- 1
        al[] <- 1
        be[] <- 1
        
        stk. <- FLCatchExt(landings = from@landings,
                           landings.n = from@landings.n,
                           discards = from@discards,
                           discards.n = from@discards.n,
                           landings.wt = from@landings.wt,
                           discards.wt = from@discards.wt,
                           alpha = al,
                           beta = be,
                           price = pr,
                           )
        
        for(i in slotNames(stk.)[!slotNames(stk.) %in% 
                  c("name", "desc","range","landings.sel","discards.sel", "catch.q", "price", "alpha", "beta")]){
        units(slot(stk.,i)) <- units(slot(from,i))
        }
        
        range(stk.)[["min"]]       <-range(from)[["min"]]
        range(stk.)[["max"]]       <-range(from)[["max"]]
        range(stk.)[["plusgroup"]] <-range(from)[["plusgroup"]]
        
        ## Bring it all together
        ca. <- FLCatchesExt(stk.)
        names(ca.) <- from@name
        mt. <- FLMetiersExt(FLMetierExt(name = from@name, effshare = effsh, catches = ca.))
        names(mt.) <- from@name
        
        fl. <- FLFleetExt(name = from@name,
                          effort = eff,
                          capacity = cap,
                          metiers = mt.)
        fl.@range <- from@range[c("min","max", "minyear","maxyear")]
        
        return(fl.)
      }
)

## Can now use to get both the FLFleet and FLBiols

#fleets   <- FLFleetsExt(as(stock, "FLFleetExt")); names(fleets)
#biols    <- FLBiols(as(stock, "FLBiol"))

## Need to add:

# fleets.ctrl  -- see bim script 04
# advice       -- see bim script 05
# advice.ctrl  -- see bim script 05, create.advice.ctrl()
# biols.ctrl   -- see bim script 03, create.biols.ctrl() 
# obs.ctrl     -- see bim script 03, create.obs.ctrl()
# assess.ctrl  -- see bim script 03, create.assess.ctrl()
# SRs          -- see bim script 03, need FLSRsim
# main.ctrl --  ## main.ctrl <- list(sim.years = c("initial" = 2020, "final" = 2022))

## Need to run FLBEIA::calculate.q.sel.flrObjs() on the fleets object to fill 
## the landings.sel, discards.sel and catch.q

## Also need to extend all objects to simulation years, and fill in the values

#FLBEIA(biols = biols, 
#       SRs = SRs,
#       BDs = NULL,
#       fleets = fleets,
#       indices = NULL,
#       advice = advice,
#       covars = NULL,
#       main.ctrl = main.ctrl,
#       biols.ctrl = biols.ctrl,
#       fleets.ctrl = fleets.ctrl,
#       covars.ctrl = NULL,
#       obs.ctrl = obs.ctrl,
#       assess.ctrl = assess.ctrl,
#       advice.ctrl = advice.ctrl)


