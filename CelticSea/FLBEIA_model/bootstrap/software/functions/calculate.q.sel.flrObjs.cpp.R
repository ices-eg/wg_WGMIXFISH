
Rcpp::sourceCpp(file.path("bootstrap", "software", "functions","cond_flcatches.cpp"))


calculate.q.sel.flrObjs.cpp <- function(biols, fleets, BDs, fleets.ctrl, mean.yrs, sim.yrs){
  
  for(st in names(biols)){
    
    na <- dim(biols[[st]]@n)[1]
    
    ## Year references
    yrs  <- as.numeric(dimnames(fleets[[1]]@effort)$year)
    first_yr_sim <- which(yrs == sim.yrs[1]) - 1
    last_yr_sim  <- which(yrs == sim.yrs[length(sim.yrs)]) - 1
    first_avg_yr <- which(yrs == mean.yrs[1]) - 1
    last_avg_yr  <- which(yrs == mean.yrs[length(mean.yrs)]) - 1
  
    if(na != 1){  # 'Biomass' in numbers because the catch is in numbers, in the middle of the season.
      B <- biols[[st]]@n*exp(-biols[[st]]@m/2)#*biols[[st]]@wt  
    }else{ # 'Biomass' in weight because the growth is in weight => later we use the catch in weight.
      
      if(is.null(BDs[[st]])) gB <- 0
      else gB <- BDs[[st]]@gB
      
      B <- biols[[st]]@n*biols[[st]]@wt + gB
    }
    
    fleets <- condition_flcatches(fl =fleets, 
                        B = as.vector(B), 
                        st, 
                        mean_yrs = first_avg_yr:last_avg_yr, 
                        sim_yrs = first_yr_sim:last_yr_sim)
      }
  return(fleets)
}


