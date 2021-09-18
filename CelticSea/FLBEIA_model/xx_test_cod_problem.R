##############################################################################
##
## Running the MIXFISH scenarios 
##
## Author:  Paul Dolder
## Created: 11/08/2021
## Updated:
##
##############################################################################
## Packages
library(tidyverse)
library(FLBEIA)

## Paths
data_path      <- file.path("results", "clean_data")
stock_path     <- file.path("results", "clean_data", "clean_stock_objects")
flbeia_in      <- file.path("results", "FLBEIA_inputs")
flbeia_precon  <- file.path(flbeia_in, "preconditioned")
flbeia_cond    <- file.path(flbeia_in, "conditioned")

## Load in the FLBEIA objects
lapply(list.files(flbeia_cond, full.names = TRUE),
       function(x) load(x,envir = .GlobalEnv))


###############################
## SQ Effort intermediate year
###############################

main.ctrl                  <- list()
main.ctrl$sim.years        <- c(initial = 2020, final = 2021)
main.ctrl$SimultaneousMngt <- FALSE

for(f in names(fleets)) {
  fleets.ctrl[[f]]$effort.model <- "fixedEffort"
  fleets.ctrl[[f]]$restriction  <- "catch"
}


### FIX COD POPULATION

#biols.ctrl$'cod.27.7e-k'$growth.model <- "fixedPopulation"

#biols[["cod.27.7e-k"]]@n[,ac(2020:2022)] <- biols[["cod.27.7e-k"]]@n[,ac(2019)]

## Artificially inflate the cod quota
#advice$TAC["cod.27.7e-k","2020"] <- 10000

## Make the catch weights equal to the stock weights

#for(f in names(fleets)) {

#	mts <- fleets[[f]]@metiers@names

#	for(mt in mts) {

#	if(!"cod.27.7e-k" %in% catchNames(fleets[[f]]@metiers[[mt]])) next 

#	fleets[[f]]@metiers[[mt]]@catches[["cod.27.7e-k"]]@landings.wt[,ac(2009:2022)] <- biols[["cod.27.7e-k"]]@wt[,ac(2009:2022)]
#	fleets[[f]]@metiers[[mt]]@catches[["cod.27.7e-k"]]@discards.wt[,ac(2009:2022)] <- biols[["cod.27.7e-k"]]@wt[,ac(2009:2022)]
#
#	}

#}


## recalulate catchability

## Load in the stock objects
#stock.list <- c("cod.27.7e-k", "had.27.7b-k","whg.27.7b-ce-k",
#                "meg.27.7b-k8abd", 
#                "mon.27.78abd", "sol.27.7fg",
#                "nep.fu.16","nep.fu.17", "nep.fu.19", "nep.fu.2021",
#                "nep.fu.22", "nep.out.7")

#wg.stocks <- FLStocks(lapply(stock.list, function(s) {
#  print(s)
#  load(file.path(stock_path, paste0(s,".RData")))
#  res <- get("stock")
#  name(res) <- s
#  res
#}))

#source(file.path("bootstrap", "software", "functions", "calculate.q.sel.flrObjs.cpp.R"))
#fleets <- calculate.q.sel.flrObjs.cpp(biols, stocks = wg.stocks, fleets = fleets, BDs = NULL, fleets.ctrl, mean.yrs = 2017:2019, sim.yrs = 2020:2022)


## Artificially inflate TACs so not limiting
advice$TAC[,"2020"] <- advice$TAC[,"2020"] * 10

hist <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = covars,
               indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
               covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)

out <- bioSum(hist)

filter(out, stock == "cod.27.7e-k", year %in% 2017:2020) %>% as.data.frame() %>% print()

for(i in grep("nep",unique(out$stock), invert = TRUE, value = TRUE)){
   filter(out, stock == i, year %in% 2017:2020) %>% as.data.frame() %>% print()
   }

## Compare the catch weight to the stock weight
for(i in grep("nep",unique(names(hist$biols)), invert=TRUE, value = TRUE)) {
  print(i)
  print(window(catchWStock(hist$fleets, i)/catchStock(hist$fleets, i),start=2009, end=2020)/
          window(hist$biols[[i]]@wt,start=2009, end=2020))
}


## Plot the intermediate year
theme_set(theme_bw())
ggplot(filter(out, year < 2021), aes(x = year, y = f)) + geom_point(colour = rep(c(rep("black", 8),rep("blue", 3), "red"), each= length(unique(out$stock)))) +facet_wrap(~stock, scale = "free_y") + theme(legend.position = "none") + 	expand_limits(y = 0) + geom_line()
ggsave(file.path("figures", "Intermediate_yr_FINAL.png"))


flt <- fltStkSum(hist)
filter(flt, stock == "cod.27.7e-k", year %in% 2017:2020, discRat > 0.5) %>% as.data.frame()

fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@landings.sel[,ac(2017:2020)]
fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@landings.wt[,ac(2017:2020)]
fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@discards.wt[,ac(2017:2020)]
fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@catch.q[,ac(2017:2020)]

hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@landings.sel[,ac(2017:2020)]
hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@discards.sel[,ac(2017:2020)]
hist$fleets[["FRA_Otter_10<40m"]]@metiers[["OTB_DEF_27.7.fg"]]@catches[["cod.27.7e-k"]]@discards.n[,ac(2017:2020)]


## Try with effort at 0.5

for(f in names(fleets)) {
fleets[[f]]@effort[,ac(2020:2022)] <- fleets[[f]]@effort[,ac(2020:2022)]  * 0.5
}

hist <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = covars,
               indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
               covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)

out <- bioSum(hist)

for(i in grep("nep",unique(out$stock), invert = TRUE, value = TRUE)){
   filter(out, stock == i, year %in% 2017:2020) %>% as.data.frame() %>% print()
   }


### Dorleta's test

cod_flmt <- str_split(names(which(FLBEIA:::stock.fleetInfo(fleets)[1,]==1)), "&&")

fleetsw <- fleets
for(s in cod_flmt){ 
   #  if(s[1] != "FRA_Otter_10<40m") next
   (print(s))
   cc <- fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]
   
#   fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@landings.wt <- fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@discards.wt <- biols[["cod.27.7e-k"]]@wt

## FLEET CATCH WEIGHTS

fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@landings.wt[,ac(2020:2022)] <- fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@discards.wt[,ac(2020:2022)] <- 
	(fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@landings.wt[,ac(2020:2022)] * fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@landings.sel[,ac(2020:2022)]) + (fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@discards.wt[,ac(2020:2022)] * fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@discards.sel[,ac(2020:2022)]) 



#   fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@catch.q <- (cc@landings.n+ cc@discards.n)/sweep(biols[["cod.27.7e-k"]]@n * exp(-biols[["cod.27.7e-k"]]@m/2), 1, fleetsw[[s[1]]]@metiers[[s[2]]]@effshare*fleetsw[[s[1]]]@effort,"*")
   
#   if(!(s[1] %in% c("FRA_Otter_10<40m","IE_Otter_10<24m","IE_Otter_24<40m"))){
	   
#	   fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@catch.q[, ac(2020:2022)] <- yearMeans(fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@catch.q[, ac(2017:2019)], na.rm = TRUE)}
#   fleetsw[[s[1]]]@metiers[[s[2]]]@catches[["cod.27.7e-k"]]@catch.q[, ac(2020:2022)] <- 0}
   
}

## compare the catchabilities 
fleets[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@catch.q
fleetsw[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@catch.q

fleetsw[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@catch.q-
fleets[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@catch.q

## Check same year/age -- really simple!!

a <- 5
y <- 2016

Caa <- fleets[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@landings.n[ac(a), ac(y)] +
fleets[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@discards.n[ac(a), ac(y)] 

B <- biols[["cod.27.7e-k"]]@n[ac(a),ac(y)] * exp(-biols[["cod.27.7e-k"]]@m[ac(a),ac(y)]/2)

E <- fleets[["FRA_Otter_10<40m"]]@effort[,ac(y)] * fleets[["FRA_Otter_10<40m"]]@metiers[[2]]@effshare[,ac(y)]

Caa/(E*B)

fleets[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@catch.q[ac(a),ac(y)]
fleetsw[["FRA_Otter_10<40m"]]@metiers[[2]]@catches[["cod.27.7e-k"]]@catch.q[ac(a),ac(y)]


histw <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleetsw, covars = covars,
                indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
                covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)
outw <- bioSum(histw)
outfw <- fltSum(histw)
outfsw <- fltStkSum(histw)
outmsw <- mtStkSum(histw)

filter(outw, stock == "cod.27.7e-k", year %in% 2019:2020)

## Plot the intermediate year
theme_set(theme_bw())
ggplot(filter(outw, year < 2021), aes(x = year, y = f)) + geom_point(colour = rep(c(rep("black", 8),rep("blue", 3), "red"), each= length(unique(out$stock)))) +facet_wrap(~stock, scale = "free_y") + theme(legend.position = "none") + 	expand_limits(y = 0) + geom_line()




############################################
## Some runs looking at fractions of effort
############################################

eff_frac <- seq(0.1,1.5,0.2)

library(doParallel)

registerDoParallel(cores = parallel::detectCores()-1)

runs <- foreach(i = eff_frac, .export = ls(.GlobalEnv)) %dopar% {

  library(FLBEIA)

 for(f in names(fleets)) {
 fleets[[f]]@effort[,ac(2020:2022)] <- fleets[[f]]@effort[,ac(2020:2022)]  * i
 }

hist <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, covars = covars,
               indices = NULL, advice = advice, main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl,
               covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)

out <- bioSum(hist)


return(out)
 
}


stopImplicitCluster()


## Plot the cod F

results <- expand.grid(effort_mult = eff_frac, stock = unique(runs[[1]]$stock, value = NA))

for(i in 1:length(eff_frac)) {
	for(s in unique(results$stock)) {
results$value[results$stock==s & results$effort_mult== eff_frac[i]] <- c(filter(runs[[i]], stock == s, year == 2020)$f)
	}
}

mean_f <- data.frame(stock = unique(runs[[1]]$stock), value = NA)

for(s in unique(runs[[1]]$stock)) {
mean_f$value[mean_f$stock == s] <- mean(c(filter(runs[[1]], stock == s, year %in% 2019)$f))
}

theme_set(theme_bw())
ggplot(results, aes(x = effort_mult, y = value)) + 
	geom_point() + geom_line() + 
	geom_point(data = mean_f, aes(x = 1, y = value), colour = "red") + 
	facet_wrap(~stock, scale = "free_y") + ylab("Fishing mortality (F year^-1)")+
	xlab("Effort multiplier (x 2019 effort)")
ggsave(file.path("figures", "effort_f_relationships_FINAL.png"))
