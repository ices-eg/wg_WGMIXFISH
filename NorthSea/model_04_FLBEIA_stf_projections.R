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

# load previous STF settings
load(file= "model/model_settings.Rdata", verbose = TRUE)

source("bootstrap/software/functions/FLBEIA_manLoop.R")
source("bootstrap/software/functions/nameChange.R")


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
main.ctrl.AdvYr







# 1. Intermediate year projections -------------------------------------------------------


# 1.1. sqE ---------------------------------------------------------------------


t1 <- Sys.time()
res_iSQE <- FLBEIA(
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
beep()
t2 <- Sys.time()
t2-t1



# 1.2. min ---------------------------------------------------------------------

t1 <- Sys.time()
res_iMIN <- FLBEIA(
  biols = biols, 
  SRs = SRs, 
  BDs = BDs, 
  fleets = fleets, 
  covars = covars, 
  indices = indices, 
  advice = advice,
  main.ctrl = main.ctrl.IntYr, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.min, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
) 
beep()
t2 <- Sys.time()
t2-t1




# 2. single species advice projections -------------------------------------

# doesn't run management loop in proj.yr-1, and thus previously defined TAC
# for proj.yr is used

# 2.1. sqE, then min ----------------------------------------------------------------

FLBEIA.obj <- res_iSQE
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.min

res_iSQE_pSSMIN <- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 2.2. sqE, then max ----------------------------------------------------------------


FLBEIA.obj <- res_iSQE
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.max # updated

res_iSQE_pSSMAX<- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 2.3. sqE, then sqE ----------------------------------------------------------------


FLBEIA.obj <- res_iSQE
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.fixedEffort # updated

res_iSQE_pSSSQE<- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)



# 2.4. min, then min ----------------------------------------------------------------

FLBEIA.obj <- res_iMIN
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.min

res_iMIN_pSSMIN <- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 2.5. min, then max ----------------------------------------------------------------


FLBEIA.obj <- res_iMIN
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.max # updated

res_iMIN_pSSMAX<- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)





# 3. ICES HCR advice projections ----------------------------------------

# runs management loop in proj.yr-1, and overwrites the defined TAC
# used for proj.yr

# run management loop to derive TAC in advice/projection year

FLBEIA.obj <- res_iSQE
fleets.ctrl.obj <- fleets.ctrl.min # updated (not important)

res_iSQE_pHCR <- FLBEIA_manLoop(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated 
  covars = FLBEIA.obj$covars, # updated 
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl,
  assessmentYr = unname(main.ctrl.AdvYr$sim.years["initial"]-1) # *** updated ***
)
 

FLBEIA.obj <- res_iMIN
fleets.ctrl.obj <- fleets.ctrl.min # updated (not important)

res_iMIN_pHCR <- FLBEIA_manLoop(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated 
  covars = FLBEIA.obj$covars, # updated 
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl,
  assessmentYr = unname(main.ctrl.AdvYr$sim.years["initial"]-1) # *** updated ***
)
 


# 3.1. sqE, then min ----------------------------------------------------------------

FLBEIA.obj <- res_iSQE_pHCR
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.min

res_iSQE_pHCRMIN <- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 3.2. sqE, then max ----------------------------------------------------------------

FLBEIA.obj <- res_iSQE_pHCR
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.max

res_iSQE_pHCRMAX <- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 3.3. min, then min ----------------------------------------------------------------

FLBEIA.obj <- res_iMIN_pHCR
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.min

res_iMIN_pHCRMIN <- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 3.4. min, then max ----------------------------------------------------------------

FLBEIA.obj <- res_iMIN_pHCR
main.ctrl.obj <- main.ctrl.AdvYr
fleets.ctrl.obj <- fleets.ctrl.max

res_iMIN_pHCRMAX <- FLBEIA(
  biols = FLBEIA.obj$biols, # updated 
  SRs = FLBEIA.obj$SRs, # updated 
  BDs = FLBEIA.obj$BDs, # updated 
  fleets = FLBEIA.obj$fleets, # updated
  covars = FLBEIA.obj$covars, # updated
  indices = indices, 
  advice = FLBEIA.obj$advice, # updated
  main.ctrl = main.ctrl.obj, # updated
  biols.ctrl = biols.ctrl, 
  fleets.ctrl = fleets.ctrl.obj, #updated
  covars.ctrl = covars.ctrl, 
  obs.ctrl = obs.ctrl, 
  assess.ctrl = assess.ctrl, 
  advice.ctrl = advice.ctrl
)


# 4. save objects --------------------------------------------------------------------


save(
  res_iSQE, res_iMIN, 
  res_iSQE_pSSMIN, res_iSQE_pSSMAX,
  res_iSQE_pSSSQE,
  res_iMIN_pSSMIN, res_iMIN_pSSMAX,
  res_iSQE_pHCRMIN, res_iSQE_pHCRMAX,
  res_iMIN_pHCRMIN, res_iMIN_pHCRMAX,
  file = "model/04_runs.Rdata"
)

# load(file = "model/04_runs.Rdata")









# 5. plot -------------------------------------------------------------------------
library(ggplotFL)


stk <- "COD_dash_NS" # "COD_dash_NS"

scen <- c(
  "res_iSQE_pSSMIN", 
  "res_iSQE_pSSMAX",
  "res_iSQE_pSSSQE", 
  "res_iMIN_pSSMIN", 
  "res_iMIN_pSSMAX", 
  "res_iSQE_pHCRMIN",
  "res_iSQE_pHCRMAX",
  "res_iMIN_pHCRMIN",
  "res_iMIN_pHCRMAX"
)

# BRPs.tmp <- BRPs
# BRPs.tmp$stock <- BRPs.tmp$stock_name
# BRPs.tmp$iter <- 1
# BRPs.tmp$Btarget <- NaN
# BRPs.tmp$Ftarget <- BRPs.tmp$Fmsy

L <- vector("list", length(scen))
for(i in seq(L)){
  tmp <- bioSum(obj = get(scen[i]), years = an(proj.yrs), scenario = scen[i], long = TRUE)
  L[[i]] <- tmp
  print(i)
}
L <- do.call("rbind", L)

head(L)



L$iFltCtrl <- c("sq_E", "min")[match(substr(L$scenario, 5, 8), c("iSQE", "iMIN"))]
L$pHCR <- c("singleSpecies", "ICEShcr")[match(substr(L$scenario, 11, 12), c("SS", "HCR"))]
L$pFltCtrl <- c("sq_E", "min", "max")[match(substr(L$scenario, 13, 15), c("SQE", "MIN", "MAX"))]
  

dfsub <- subset(L, stock == stk & indicator %in% c("f","rec","ssb","catch") & pHCR == "singleSpecies")
pal <- pals::brewer.paired
COL <- pal(length(unique(dfsub$scenario)))
p <- ggplot(data = dfsub) + 
  aes(x = year, y = value, group = scenario, color = pFltCtrl, linetype = iFltCtrl) + 
  facet_wrap(~ indicator, ncol = 2, scales = "free") +
  geom_line(size = 0.5) #+
  # scale_color_manual(values = COL) + 
  # scale_linetype_manual(values = c(1:4,1:4))
print(p)
    
png(file.path("tmp", paste0(stk, "_biol~scen.png")), width = 6, height = 4, units = "in", res = 400)
  plot(p)
dev.off()




tmp <- fltStkSum(min.run, years = c(hist.yrs, proj.yrs), verbose = T)
tmp$stock <- nameChange(tmp$stock, form = "old")
tmp$fleet <- nameChange(tmp$fleet, form = "old")
dim(tmp)
head(tmp)

agg1 <- aggregate(quota ~ stock + year, tmp, sum)
agg2 <- aggregate(catch ~ stock + year, tmp, sum)
agg <- merge(x = agg1, y = agg2, all = TRUE)
agg$quotaUpt <- agg$catch / agg$quota
agg$fleet <- "all"

tmp <- merge(x = tmp, y = agg, all = TRUE)
head(tmp)

tmp2 <- subset(tmp, year %in% an(proj.yrs))
head(tmp2)
aggregate(quota ~ stock + year, data = tmp2, FUN = sum) #****** NEP



aggsub <- subset(tmp, year %in% an(proj.yrs) &
    stock %in% c(
      "NEP6", "NEP7", "NEP8", "NEP9",
      "COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", 
      #"SOL-EC", 
      "SOL-NS", "TUR", "WHG-NS") & fleet != "all")

subset(aggsub, fleet == "DK_Beam" & year == proj.yr)

pal <- pals::brewer.paired
COL <- pal(length(unique(aggsub$stock)))


  p <- ggplot(data = aggsub) + 
    aes(x=year, y=quotaUpt, color=stock, group = stock, shape = stock) +
    theme_set(theme_gray(base_size = 7)) +
    facet_wrap(~ fleet, ncol = 7) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = which(unique(aggsub$year)==ac(proj.yr-1)), lty=3) +
    geom_hline(yintercept = 1, lty=3) +

    ylim(c(0,1.1)) +
    # scale_x_discrete(expand = c(0.05,0.05)) +
    scale_color_manual(values=COL) +
    scale_shape_manual(values = rep(15:18, length.out = length(unique(aggsub$stock)))) +
    ylab("Choke prob.") +
    theme(axis.text.x=element_text(angle = 90, hjust=1, vjust = 0.5),
      legend.position="bottom") +
    guides(col = guide_legend(nrow = 2))
# print(p)

png("tmp/quotaUpt~stock+year.png", width = 7, height = 7, units = "in", res = 400)
  print(p)
dev.off()


