####
#### a remplement to the FLR function fdw() so that, if numbers at age are specified at the start of the first
#    projection year, those do not get overwritten
#    this is important for assessments that produce estimates in the current year which are used in the single stock advice STF
#    such as North Sea cod and whiting
#    Thomas Brunel based on code from Iago Mosqueira, WGMIXFISH METHODS 2020


fwdF3 <- function(x, ctrl, sr) {
  
  ages <- dimnames(m(x))$age
  year <- an(ctrl@target$year[1])
  
  # are there values for the stock abundances in the ImY?
  surv<-apply(!is.na(stock.n(x)[,ac(year)] ),2:6,any)
  
  # COPY stock
  y <- x
  if(surv)
  {
    # SET stock.n[1:last-1, year-1] to stock.n[2:last, year]
    stock.n(y)[ages[-length(ages)], ac(year-1)] <-
      stock.n(y)[ages[-1], ac(year)]
    # SET stock.n[last, year-1] to zero, all in age last-1
    stock.n(y)[ages[length(ages)], ac(year-1)] <- 0
    # SET m and F to almost nothing
    m(y)[, ac(year-1)] <- 1e-32
    harvest(y)[, ac(year-1)] <- 1e-32
  }
  # FWD
  # if Stq F : need to take F value from current year (which is equal to Fsq, given how stf works) because F previous year has just been set to 0
  if (!is.na(ctrl@target[1,'rel.year'])) ctrl@target[1,'rel.year'] <- ac(an(ctrl@target[1,'rel.year']) +1) 
  z <-  fwd(y,ctrl=ctrl,sr=sr)
  # REASSEMBLE stock
  x[, ac(year:(year+2))] <- z[, ac(year:(year+2))]
  return(x)
}

