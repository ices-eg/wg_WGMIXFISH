#' @title Make FLFleets function
#'
#' @description This function takes the FLStock objects and a simple creates an FLBiols object
#' needed as an input to FLBEIA.

#' @param data_path is the path where the data files are stored (i.e. one upper
#' level from the FLStocks 
#'
#' @return is an FLFleets object containing a list of FLFleet objects for each
#' stock

#' @examples make_simple_FLFleet(stk_path = \path\to\data)

#' @export

# data_path <- file.path("results", "clean_data", "clean_stock_objects")

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
                           name = from@name
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

make_simple_FLFleet <- function(data_path = NULL) {

    stk_path  <- file.path(data_path)
    
    ## Load the stock objects from the path
    stocks<-FLStocks(lapply(list.files(path = stk_path, pattern = ".RData"),function(x){
      load(file.path(stk_path,x))
      res<-get("stock")
      name(res)<-gsub('.RData',"",x)
      res}))
    
    # Convert from FLStock to FLBiol
    fleets <- FLFleetsExt(lapply(stocks, function(x) {
      as(x, "FLFleetExt")
    }))
    
  ## Set the plus group
    ## Set maturity for Nephrops
    
    
    ## Return the FLBiols list
    return(fleets)
    
  }
