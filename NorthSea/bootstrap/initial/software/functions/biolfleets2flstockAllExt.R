biolfleets2flstockAllExt <- function(obj, verbose = TRUE){
  stocks2 <- obj$stocks
  for(i in seq(stocks2)){
    stk <- names(stocks2)[i]
    
    # make extended FLStock
    # stocks2[[stk]] <- biolfleets2flstock( biol=obj$biols[[stk]], fleets=obj$fleets)
    
    # extend with stf
    # stocks2[[stk]] <- FLCore::window(stocks2[[stk]], end = range(obj$biols)["maxyear"])
    tmp <- FLBEIA:::perfectObs(biol = obj$biols[[stk]], fleets = obj$fleets, 
      year = dim(obj$biols[[stk]]@n)[2]+1)
    tmp@harvest[,dim(tmp@harvest)[2]] <- FLash::computeHarvest(tmp[,dim(tmp@harvest)[2]])
    stocks2[[stk]] <- tmp
    
    if(verbose) print(paste(stk, "is finished"))
  }
  return(stocks2)
}

