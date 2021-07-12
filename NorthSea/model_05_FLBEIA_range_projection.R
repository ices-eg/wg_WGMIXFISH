
# Initialise system --------------------------------------------------------------
# Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()

# Scenario name -----------------------------------------------------------
# scenario.name <- "ICES_MIXFISH_demo"


# Required packages -------------------------------------------------------

library(FLCore) 
library(FLash) 
library(FLAssess)
library(FLFleet)
library(FLXSA)
library(FLBEIA) 
library(beepr)
library(ggplotFL)

library("GA")
library("lhs")
library("parallel")


# load previous STF settings
load(file= "model/model_settings.Rdata", verbose = TRUE)

source("bootstrap/software/functions/FLBEIA_manLoop.R")
source("bootstrap/software/functions/nameChange.R")
source("bootstrap/software/functions/IcesRangeHCR.R")


# Global settings
first.yr          <- 2014
proj.yr           <- an(yr.now) 
last.yr           <- an(yr.TACp1)
hist.yrs  <- ac(first.yr:(proj.yr-1))
proj.yrs  <- ac(proj.yr:last.yr)
ni <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$iter})) # number of iterations
ns <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$season}))  # number of seasons
# Year extent in objects
yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr, "last.yr" = last.yr))


# input objects
load(file = file.path("model", "03_input_objects.Rdata"))


# create different fleet.ctrl objects -------------------------------

# fleet effort.model 'min' 
fleets.ctrl.min <- fleets.ctrl
fleets.ctrl.min[["FR_Otter_gthan__equal_40"]]$effort.model
fleets.ctrl.min[["FR_Otter_gthan__equal_40"]]$effort.restr

# fleet effort.model 'max' 
fleets.ctrl.max <- fleets.ctrl
flt.names <- names(fleets)
for(i in seq(flt.names)){
  fleets.ctrl.max[[flt.names[i]]]$effort.restr <- "max"
}
fleets.ctrl.max[["FR_Otter_gthan__equal_40"]]$effort.model
fleets.ctrl.max[["FR_Otter_gthan__equal_40"]]$effort.restr

# fleet effort.model 'fixedEffort' 
fleets.ctrl.fixedEffort <- fleets.ctrl
flt.names <- names(fleets)
for(i in seq(flt.names)){
  fleets.ctrl.fixedEffort[[flt.names[i]]]$effort.model <- "fixedEffort"
}
fleets.ctrl.fixedEffort[["FR_Otter_gthan__equal_40"]]$effort.model



# create main.ctrl objects -----------------------------------------------

# intermediate year
main.ctrl.IntYr <- main.ctrl
main.ctrl.IntYr$sim.years["final"] <- main.ctrl.IntYr$sim.years["initial"]
main.ctrl.IntYr

# advice year
main.ctrl.AdvYr <- main.ctrl
main.ctrl.AdvYr$sim.years["initial"] <- main.ctrl.AdvYr$sim.years["initial"] + 1
main.ctrl.AdvYr$sim.years["final"] <- main.ctrl.AdvYr$sim.years["initial"]
main.ctrl.AdvYr

# advice year + 1
main.ctrl.AdvYrP1 <- main.ctrl
main.ctrl.AdvYrP1$sim.years["initial"] <- main.ctrl.AdvYrP1$sim.years["initial"] + 2
main.ctrl.AdvYrP1$sim.years["final"] <- main.ctrl.AdvYrP1$sim.years["initial"]
main.ctrl.AdvYrP1




# add BRPS to advice.ctrl
BRPs
BRPs$FmsyH <- ifelse(is.na(BRPs$FmsyH), BRPs$Fmsy*1.0001, BRPs$FmsyH)
BRPs$FmsyL <- ifelse(is.na(BRPs$FmsyL), BRPs$Fmsy*0.9999, BRPs$FmsyL)
BRPs$Ftarg <- BRPs$Fmsy
BRPs

for(i in seq(advice.ctrl)){
  stk <- names(advice.ctrl)[i]
  if(advice.ctrl[[stk]]$HCR.model == "IcesHCR"){
    advice.ctrl[[stk]]$HCR.model <- "IcesRangeHCR"
    advice.ctrl[[stk]]$ref.pts <- rbind(advice.ctrl[[stk]]$ref.pts, 
      t(as.matrix(subset(BRPs, stock_name == stk)[c("FmsyL", "FmsyH", "Ftarg")])))
  }
}


# extract Fmsy ranges
Fmsy_lw <- ICES_HCR_stocks <- unlist(lapply(advice.ctrl, function(stck, range.ref.pts){
  if(stck$HCR.model == "IcesRangeHCR"){
    ref.pts <- stck$ref.pts[rownames(stck$ref.pts) == "FmsyL",ncol(stck$ref.pts)]
    return(ref.pts)
  }
}))
Fmsy_up <- ICES_HCR_stocks <- unlist(lapply(advice.ctrl, function(stck, range.ref.pts){
  if(stck$HCR.model == "IcesRangeHCR"){
    ref.pts <- stck$ref.pts[rownames(stck$ref.pts) == "FmsyH",ncol(stck$ref.pts)]
    return(ref.pts)
  }
}))
Fmsy <- ICES_HCR_stocks <- unlist(lapply(advice.ctrl, function(stck, range.ref.pts){
  if(stck$HCR.model == "IcesRangeHCR"){
    ref.pts <- stck$ref.pts[rownames(stck$ref.pts) == "Fmsy",ncol(stck$ref.pts)]
    return(ref.pts)
  }
}))






# 0. Set-up range scenario ----------------

## GA settings
n.cores = 10 # specify number of cores
pop_size      <- n.cores
maxit         <- 3 # number of generations to run
run 					<- maxit
parallel_mode <- TRUE
parallel_mode <- pop_size
# if(parallel_mode) parallel_mode <- min(detectCores(), pop_size)
local_search  <- FALSE

# max. estimated runtime (hours)
yrProjTime <- 6.75/60 # one year FLBEIA projection (hours)
(maxit*2*yrProjTime + yrProjTime) # estimated runtime (hours per year)
2*yrProjTime + (maxit*2*yrProjTime + yrProjTime) # total estimated runtime (hours)



## stock names with ICES_HCR
ICES.HCR.stk.names <- names(Fmsy_up)

# ----------------------------------------------------------- #
## fitness function (for optimizing between min & max-ranges)
# ----------------------------------------------------------- #

# set.seed(1); tmp <- cbind(Fmsy_lw, Fmsy_up); x <- apply(tmp, 1, FUN = function(x){runif(1, min=x[1], max = x[2])}); x


# 0.1 range function ------------------------------------------------------

rangeFun <- function(x){
  names(x) <- ICES.HCR.stk.names
  
  # change the advice control object with the range values
  advice.ctrl.range <- advice.ctrl
  for(i in seq_along(advice.ctrl.range)){
    if(advice.ctrl.range[[i]]$HCR.model == "IcesRangeHCR"){
      advice.ctrl.range[[i]]$ref.pts[rownames(advice.ctrl.range[[i]]$ref.pts) == "Ftarg"] <- 
        x[names(x) == names(advice.ctrl.range)[i]]
    }
  }
  
  # 1. re-run management procedure from assessment year using new Ftarget
  # to derive TAC in advice/projection year
  # 
  # NOTE: This management loop may ultimately need to happen inside of
  # FLash::stf for consistency with FCube (i.e. TAC based on 
  # single-species advice assumption for int. year)
  #
  # ***************************************
  min.run <- FLBEIA_manLoop(
    biols = res.range$biols, # updated
    SRs = res.range$SRs, # updated
    BDs = res.range$BDs, # updated
    fleets = res.range$fleets, # updated
    covars = res.range$covars, # updated
    indices = indices,
    advice = res.range$advice, # updated
    biols.ctrl = biols.ctrl,
    fleets.ctrl = fleets.ctrl.min, #updated
    covars.ctrl = covars.ctrl,
    obs.ctrl = obs.ctrl,
    assess.ctrl = assess.ctrl,
    advice.ctrl = advice.ctrl.range, # updated
    assessmentYr = unname(main.ctrl.AdvYr$sim.years["initial"]-1) # *** updated ***
  )
  # ***************************************
  
  
  # 2. run "min" scenario for advice/projection year
  min.run <- FLBEIA(
    biols = min.run$biols, # updated 
    SRs = min.run$SRs, # updated 
    BDs = min.run$BDs, # updated 
    fleets = min.run$fleets, # updated
    covars = min.run$covars, # updated
    indices = indices, 
    advice = min.run$advice, # updated
    main.ctrl = main.ctrl.AdvYr, # updated
    biols.ctrl = biols.ctrl, 
    fleets.ctrl = fleets.ctrl.min, #updated
    covars.ctrl = covars.ctrl, 
    obs.ctrl = obs.ctrl, 
    assess.ctrl = assess.ctrl, 
    advice.ctrl = advice.ctrl.range # updated
  )

	# 3. run "max" scenario for advice/projection year
	max.run <- FLBEIA(
	  biols = min.run$biols, # updated 
	  SRs = min.run$SRs, # updated 
	  BDs = min.run$BDs, # updated 
	  fleets = min.run$fleets, # updated
	  covars = min.run$covars, # updated
	  indices = indices, 
	  advice = min.run$advice, # updated
	  main.ctrl = main.ctrl.AdvYr, # updated
	  biols.ctrl = biols.ctrl, 
	  fleets.ctrl = fleets.ctrl.max, #updated
	  covars.ctrl = covars.ctrl, 
	  obs.ctrl = obs.ctrl, 
	  assess.ctrl = assess.ctrl, 
	  advice.ctrl = advice.ctrl.range # updated
	)
		
	# calculate fitness
	min.run.out <- bioSum(min.run, scenario = "min", years = an(main.ctrl.AdvYr$sim.years["initial"]))
	ca.min <- sum(min.run.out$catch, na.rm = TRUE)
	
	max.run.out <- bioSum(max.run, scenario = "max", years = an(main.ctrl.AdvYr$sim.years["initial"]))
	ca.max <- sum(max.run.out$catch, na.rm = TRUE)

	return(-abs(ca.max - ca.min))  # ga maximizes (but we aim for the lowest difference)

}






# run Range ---------------------------------------------------------------

t1 <- Sys.time()


# 1. intermediate year at sq_E -------------

res.range <- FLBEIA(
  biols = biols, 
  SRs = SRs, 
  BDs = BDs, 
  fleets = fleets, 
  covars = covars, 
  indices = indices, 
  advice = advice,
  main.ctrl = main.ctrl.IntYr, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.fixedEffort, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
) 


# 2. advice year with range ------------
seed <- 1
set.seed(seed)


# 2.1. create an initial population with candidate solutions ---------
init_pop  <- optimumLHS(k = length(Fmsy_lw), n = pop_size)
for (i in 1:ncol(init_pop)){
  init_pop[,i] <- init_pop[,i] * (Fmsy_up[i]-Fmsy_lw[i]) + Fmsy_lw[i]
}
init_pop[1,] <- BRPs$Fmsy[match(ICES.HCR.stk.names, BRPs$stock_name)] # add Fmsy to 1st ind


# 2.2. run GA search for best Ftarget combination -----------
res.ga <- ga(type = "real-valued", 
  fitness = rangeFun,
  lower = Fmsy_lw, 
  upper = Fmsy_up, 
  popSize = pop_size, 
  maxiter = maxit, 
  run = run,
  monitor = TRUE,
  seed = seed + 12345,
  suggestions = init_pop,
  #optim = local_search,
  #optimArgs = list(control = list(trace = 2, maxit = 2)),
  parallel = parallel_mode)
  
plot(res.ga)


# 2.3. run advice year again with solution ----------
x <- res.ga@solution
names(x) <- ICES.HCR.stk.names

# change the advice control object with the range values
advice.ctrl.range <- advice.ctrl
for(i in seq_along(advice.ctrl.range)){
 if(advice.ctrl.range[[i]]$HCR.model == "IcesRangeHCR"){
   advice.ctrl.range[[i]]$ref.pts[rownames(advice.ctrl.range[[i]]$ref.pts) == "Ftarg"] <-
     x[names(x) == names(advice.ctrl.range)[i]]
 }
}

##******************************************
# re-run management loop to derive TAC in advice year
# (again, may need to be ultimately done with FLash::stf)
res.range <- FLBEIA_manLoop(
  biols = res.range$biols, # updated
  SRs = res.range$SRs, # updated
  BDs = res.range$BDs, # updated
  fleets = res.range$fleets, # updated
  covars = res.range$covars, # updated
  indices = indices,
  advice = res.range$advice, # updated
  biols.ctrl = biols.ctrl,
  fleets.ctrl = fleets.ctrl.min, #updated
  covars.ctrl = covars.ctrl,
  obs.ctrl = obs.ctrl,
  assess.ctrl = assess.ctrl,
  advice.ctrl = advice.ctrl.range, # updated
  assessmentYr = unname(main.ctrl.AdvYr$sim.years["initial"]-1) # *** updated ***
)
# ***************************************



# run "min" scenario for advice/projection year
res.range <- FLBEIA(
  biols = res.range$biols, # updated 
  SRs = res.range$SRs, # updated 
  BDs = res.range$BDs, # updated 
  fleets = res.range$fleets, # updated
  covars = res.range$covars, # updated
  indices = indices, 
  advice = res.range$advice, # updated
  main.ctrl = main.ctrl.AdvYr, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.min, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl.range # updated
)

  




# 3. advice year +1 -------------------------------------------------------

# this year is not important, but we run it in order to get SSB at start of year

res.range <- FLBEIA(
  biols = res.range$biols, # updated 
  SRs = res.range$SRs, # updated 
  BDs = res.range$BDs, # updated 
  fleets = res.range$fleets, # updated
  covars = res.range$covars, # updated
  indices = indices, 
  advice = res.range$advice, # updated
  main.ctrl = main.ctrl.AdvYrP1, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.min, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl.range # updated
)







t2 <- Sys.time()
t2-t1




# retrieve actual F-advice values -----------------------------------------
fname <- tempfile(fileext = ".txt")
sink(file = fname)
tmp <- FLBEIA_manLoop(
  biols = res.range$biols, # updated
  SRs = res.range$SRs, # updated
  BDs = res.range$BDs, # updated
  fleets = res.range$fleets, # updated
  covars = res.range$covars, # updated
  indices = indices,
  advice = res.range$advice, # updated
  biols.ctrl = biols.ctrl,
  fleets.ctrl = fleets.ctrl.min, #updated
  covars.ctrl = covars.ctrl,
  obs.ctrl = obs.ctrl,
  assess.ctrl = assess.ctrl,
  advice.ctrl = advice.ctrl.range, # updated
  assessmentYr = unname(main.ctrl.AdvYr$sim.years["initial"]-1) # *** updated ***
)
sink()

tmp2 <- scan(file = fname, what = "character", sep = "\n") # scans FLBEIA output
tmp2 <- gsub("-", "", tmp2) # remove several non-informative characters
tmp2 <- gsub("#", "", tmp2)
tmp2 <- gsub("*", "", tmp2)
tmp2 <- gsub("[1]", "", tmp2, fixed = T)
tmp2 <- gsub(" ", "", tmp2)
# tmp2
hit <- match(names(biols), tmp2) # identify lines where stock names appear
hit <- hit[which(!is.na(an(tmp2[hit+1])))] # identify lines thereafter which are numeric (F-advice values)
rangeFadv <- data.frame(stock = tmp2[hit], Fadv = tmp2[hit+1]) # summary table of F-advice



# ----------------------------- #
# save object and write to disk
# ----------------------------- #

save(res.range, res.ga, rangeFadv, file = "model/05_range_run.Rdata")
# load(file = "model/05_range_run.Rdata")




