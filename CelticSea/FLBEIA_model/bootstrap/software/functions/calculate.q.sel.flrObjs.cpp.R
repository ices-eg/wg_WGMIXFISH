
Rcpp::sourceCpp(file.path("bootstrap", "software", "functions","cond_flcatches.cpp"))
#Rcpp::sourceCpp(file.path("bootstrap", "software", "functions","cond_flcatches_weights.cpp"))


calculate.q.sel.flrObjs.cpp <- function(biols, stocks, fleets, BDs, fleets.ctrl, mean.yrs.q, mean.yrs.wts, mean.yrs.sel, sim.yrs, LO = FALSE, UseLWt = FALSE){
  
  for(st in names(biols)){
  
    na <- dim(biols[[st]]@n)[1]
    
    ## Year references
    yrs  <- as.numeric(dimnames(fleets[[1]]@effort)$year)
    first_yr_sim <- which(yrs == sim.yrs[1]) - 1
    last_yr_sim  <- which(yrs == sim.yrs[length(sim.yrs)]) - 1

    ## years in  numerics
    first_avg_yr_q <- which(yrs == mean.yrs.q[1]) - 1
    last_avg_yr_q  <- which(yrs == mean.yrs.q[length(mean.yrs.q)]) - 1
    
    first_avg_yr_wts <- which(yrs == mean.yrs.wts[1]) - 1
    last_avg_yr_wts  <- which(yrs == mean.yrs.wts[length(mean.yrs.wts)]) - 1

    first_avg_yr_sel <- which(yrs == mean.yrs.sel[1]) - 1
    last_avg_yr_sel  <- which(yrs == mean.yrs.sel[length(mean.yrs.sel)]) - 1

  
    if(na != 1){  # 'Biomass' in numbers because the catch is in numbers, in the middle of the season.
      B <- biols[[st]]@n*exp(-biols[[st]]@m/2)  
    }else{ # 'Biomass' in weight because the growth is in weight => later we use the catch in weight.
      
      if(is.null(BDs[[st]])) gB <- 0
      else gB <- BDs[[st]]@gB
      
      B <- biols[[st]]@n*biols[[st]]@wt + gB
    }
    
    SLwt <- window(stocks[[st]]@landings.wt, start = yrs[1], end = yrs[length(yrs)])
    SDwt <- window(stocks[[st]]@discards.wt, start = yrs[1], end = yrs[length(yrs)])
    
    fleets <- condition_flcatches(fl =fleets,
                                  SLwt = as.vector(SLwt),
                                  SDwt = as.vector(SDwt),
                        B = as.vector(B), 
                        st = st, 
                        mean_yrs_q = first_avg_yr_q:last_avg_yr_q, 
			mean_yrs_wts = first_avg_yr_wts:last_avg_yr_wts,
			mean_yrs_sel = first_avg_yr_sel:last_avg_yr_sel,
                        sim_yrs = first_yr_sim:last_yr_sim,
			LO = LO,
			UseLWt = UseLWt)
    
  }
      
  return(fleets)
}


