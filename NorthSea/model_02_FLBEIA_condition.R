# 0. General info ---------------------------------------------------------

# author: Marc Taylor
# date: 2019-06-13
# description: takes post-conditioned FLFleets and FLStocks from WGMIXFISH 
#   and produces FLBiols and FLFleetsExt for use in FLBEIA

# 1. Initialise system ----------------------------------------------------

## Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()

# ## Scenario name
# scenario.name <- "ICES_MIXFISH_demo"

## Required packages 
library(FLCore)  
library(FLash)
# library(FLAssess)
library(FLFleet)
# library(FLXSA)
library(FLBEIA)
# library(beepr)
# library(spict)

## create scenario folders (if needed)
# dir.create(file.path("data", "scenarios"), showWarnings = FALSE)
# dir.create(file.path("data", "scenarios", scenario.name), showWarnings = FALSE)
# dir.create(file.path("data", "scenarios", scenario.name,"results"), showWarnings = FALSE)

# dir.create(file.path("output", "figs"), showWarnings = FALSE)
# dir.create(file.path("output", "sr"), showWarnings = FALSE)

## Required functions
source("bootstrap/software/functions/nameChange.R")
source("bootstrap/software/functions/gm_mean.R")



# load previous STF settings
load(file= "model/model_settings.Rdata", verbose = TRUE)
ver <- "01_Reproduce_The_Advice_2020_keepNYrNow"
if (length(LO)>0) Run.name <- paste0(ver,"_LO") else Run.name <- ver
load(file = paste0("model","/", Run.name,".Rdata"), verbose = T)




# Global settings
first.yr          <- 2014
proj.yr           <- an(yr.now) # 
last.yr           <- an(yr.TACp1)
hist.yrs  <- ac(first.yr:(proj.yr-1))
proj.yrs  <- ac(proj.yr:last.yr)
ni <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$iter})) # number of iterations
ns <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$season}))  # number of seasons
# Year extent in objects
yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr, "last.yr" = last.yr))





# 2. Load WGMIXFISH data -----------------------------------------------------

# 2.1. Load FLStock objs and convert to FLBiol objs --------------------------------------------

## load FLFleets and prepare list of (unique) stocks caught by all fleet/metier combinations
load("data/NS_Fleet_database/03_NS_Fleet_database_AD_KW.RData")
stk_names <- c()
for(i in seq(fleets)){
  for(j in seq(fleets[[i]]@metiers)){
    stk_names <- c(stk_names, names(fleets[[i]]@metiers[[j]]@catches))
    print(paste("i =", i, "| j =", j))
  }
}
stk_names <- unique(stk_names)
stk_names

## define stock types
## ad = "age-disaggregated", aa = "age-aggregated", fixed=fixed dynamics, spict - biomass dynamics
ad_stocks <- unlist(dem.lst)
aa_stocks <- unlist(nep.lst)
# c("ANF", "BLL", "DAB", "LEM", "LIN", 
#   "NEPOTH-NS", "NEP33", "NEP5", "NEP32", "NEP6", "NEP7", 
#   "NEP34", "NEP8", "NEP10", "NEP9")
fixed_stocks <- aa_stocks[grep(pattern = "NEP", aa_stocks)]
spict_stocks <- aa_stocks[-grep(pattern = "NEP", aa_stocks)]

## restrict to some
# stk_names <- stk_names[which(stk_names %in% c(ad_stocks, spict_stocks))]
stk_names <- stk_names[which(stk_names %in% c(ad_stocks, aa_stocks))]

## load and rename FLStock objects
for(i in seq(stk_names)){
  obj.name <- load(paste0("data/FLStocks_out/", stk_names[i], ".RData"), verbose = TRUE)
  tmp1 <- get(obj.name)
  assign(paste0(stk_names[i], "_stock"), tmp1)
  rm(list=c("obj.name", "tmp1"))
}

## pack these together in FLStock(s) object
stocks <- FLStocks(sapply(paste0(stk_names, "_stock"), FUN = get))
names(stocks) <- stk_names
rm(list=paste0(stk_names, "_stock"))

# double chack that all plus groups are defined
if(!all(!is.na(unlist(lapply(stocks, function(x){range(x)["plusgroup"]}))))) stop("plusgroup not defined for one or more stocks")

# ## change harvest units
# for(i in seq(length(stocks))){
#   units(harvest(stocks[[i]])) <- "f"
#   if(names(stocks)[i] %in% c(fixed_stocks)){
#     units(harvest(stocks[[i]])) <- "hr"
#   }
# }

## change names for compatibility conditioning functions
names(stocks) <- nameChange(names(stocks), form = "new")
ad_stocks <- nameChange(ad_stocks, form = "new")
aa_stocks <- nameChange(aa_stocks, form = "new")
fixed_stocks <- nameChange(fixed_stocks, form = "new")
spict_stocks <- nameChange(spict_stocks, form = "new")



# 2.2. Make adjustments to FLStocks (if needed) ---------------------------

## FLBEIA requires info in numbers and mean weight for the FLBiols
## Add missing slots for Nephrops 

## give arbitrary value for NEPOTH landings.wt, discards.wt and catch.wt 
## and assign numbers slots
# if("NEPOTH_dash_NS" %in% names(stocks)){
#   landings.wt(stocks[["NEPOTH_dash_NS"]]) <- 40
#   landings.n(stocks[["NEPOTH_dash_NS"]]) <- c(landings(stocks[["NEPOTH_dash_NS"]])) / c(landings.wt(stocks[["NEPOTH_dash_NS"]]))
#   landings(stocks[["NEPOTH_dash_NS"]]) <- computeLandings(stocks[["NEPOTH_dash_NS"]])
#   discards.wt(stocks[["NEPOTH_dash_NS"]]) <- 40
#   discards.n(stocks[["NEPOTH_dash_NS"]]) <- c(discards(stocks[["NEPOTH_dash_NS"]])) / c(discards.wt(stocks[["NEPOTH_dash_NS"]]))
#   discards(stocks[["NEPOTH_dash_NS"]]) <- computeDiscards(stocks[["NEPOTH_dash_NS"]])
#   catch(stocks[["NEPOTH_dash_NS"]]) <- landings(stocks[["NEPOTH_dash_NS"]]) + discards(stocks[["NEPOTH_dash_NS"]]) 
#   catch.wt(stocks[["NEPOTH_dash_NS"]]) <- 40
#   catch.n(stocks[["NEPOTH_dash_NS"]]) <- c(catch(stocks[["NEPOTH_dash_NS"]])) / c(catch.wt(stocks[["NEPOTH_dash_NS"]]))
# }
# 
# 

## stock.wt - based on catch.wt for all Nephrops stocks
## stock.n - Nephrops with TV survey (NEP 6,7,8,9) have this slot.  
## For the rest, use catch.n multiplied by a large value (e.g. 10) to prevent
##  catches from exceeding stock size. 
for(i in seq(stocks)){
  if(names(stocks)[i] %in% fixed_stocks){
    stocks[[i]]@stock.wt <- stocks[[i]]@catch.wt
    stocks[[i]]@mat[] <- 1
  }
  if(names(stocks)[i] %in% c("NEPOTH_dash_NS", "NEP33", "NEP5", "NEP32", "NEP34", "NEP10")){
    stocks[[i]]@stock.n <- stocks[[i]]@catch.n * 10
    units(stocks[[i]]) <- units(stocks[["NEP6"]])
  }
}



# 2.3. Load SPiCT objects -------------------------------------------------

##  for later par extraction
incl <- which(names(stocks) %in% spict_stocks)
spicts <- vector(mode = "list", length(incl))
names(spicts) <- names(stocks)[incl]
for(i in seq(stocks)){
  if(names(stocks)[i] %in% spict_stocks){
    obj.name <- load(paste0("data/spict_stocks/spictFits/", names(stocks)[i], "_fit_spict.Rdata"), verbose = TRUE)
    spicts[[names(stocks)[i]]] <- get(obj.name)
  }
}

# summary(spicts[["ANF"]])



# 3 Build FLBiols -------------------------------------------------------
biol.proj.avg.yrs <- (proj.yr-3):(proj.yr-1) # hist.yrs

stks.data <- vector("list", length(stocks))
names(stks.data) <- names(stocks)
for(i in seq(stks.data)){
    stk <- names(stks.data)[i]
    DIMS <- dims(stocks[[stk]])
    RANGE <- range(stocks[[stk]])
    BIOL <- as(stocks[[stk]][,hist.yrs], 'FLBiol')
    
    stks.data[[stk]] <- paste0(
      stk,
      c(
        ".unit", 
        ".age.min",
        ".age.max",
        "_n.flq",
        "_wt.flq",
        "_m.flq",
        "_fec.flq",
        "_mat.flq",
        "_spwn.flq",
        "_range.plusgroup",
        "_range.minyear",
        "_range.maxyear",
        "_range.minfbar",
        "_range.maxfbar",
        "_biol.proj.avg.yrs"
      )
    )

    assign(paste0(stk, ".unit"), DIMS$unit)
    assign(paste0(stk, ".age.min"), RANGE["min"])
    assign(paste0(stk, ".age.max"), RANGE["max"])
    assign(paste0(stk, "_n.flq"), stocks[[stk]]@stock.n[,hist.yrs])
    assign(paste0(stk, "_wt.flq"), stocks[[stk]]@stock.wt[,hist.yrs])
    assign(paste0(stk, "_m.flq"), stocks[[stk]]@m[,hist.yrs])
    assign(paste0(stk, "_fec.flq"), fec(BIOL))
    assign(paste0(stk, "_mat.flq"), mat(BIOL))
    assign(paste0(stk, "_spwn.flq"), mat(BIOL)*0)
    assign(paste0(stk, "_range.plusgroup"), RANGE["plusgroup"])
    assign(paste0(stk, "_range.minyear"), range(BIOL)["minyear"])
    assign(paste0(stk, "_range.maxyear"), range(BIOL)["maxyear"])
    assign(paste0(stk, "_range.minfbar"), RANGE["minfbar"])
    assign(paste0(stk, "_range.maxfbar"), RANGE["maxfbar"])
    assign(paste0(stk, "_biol.proj.avg.yrs"), biol.proj.avg.yrs)
}

biols <- create.biols.data(
  yrs = yrs, ns = ns, ni = ni,
  stks.data = stks.data
)


## adjust projection years for fixed_stocks
biol.proj.avg.yrs <- proj.yr-1
for(i in seq(biols)){
  if(names(biols)[i] %in% fixed_stocks){
      biols[[i]]@n[,proj.yrs] <- biols[[i]]@n[,ac(biol.proj.avg.yrs)]
      biols[[i]]@wt[,proj.yrs] <- yearMeans(biols[[i]]@wt[,ac(biol.proj.avg.yrs)])
      rec(biols[[i]])[,proj.yrs] <- biols[[i]]@n[,ac(biol.proj.avg.yrs)]
  }
}


# further adjustment of projected values (if required; e.g. mean weights)
wt(biols[["HAD"]])[, ac(yr.now)]      <- c(0.042,0.146,0.335,0.433,0.616,0.795,0.745,1.301,1.634)    # updated 2020 (TB)
wt(biols[["HAD"]])[, ac(yr.TAC)]      <- c(0.042,0.146,0.335,0.471,0.568,0.762,0.95,0.857,1.508)
wt(biols[["HAD"]])[, ac(yr.TACp1)]    <- c(0.042,0.146,0.335,0.471,0.646,0.704,0.907,1.105,1.336)

m(biols[["HAD"]])[, ac(yr.now)]      <- c(0.981,	1.258,	0.577,	0.288,	0.263,	0.255,	0.24,	0.267,	0.376)    # updated 2020 (TB)
m(biols[["HAD"]])[, ac(yr.TAC)]      <- c(0.981,	1.258,	0.577,	0.288,	0.263,	0.255,	0.24,	0.267,	0.376)    #  the  +grp value is different from 3 years average...
m(biols[["HAD"]])[, ac(yr.TACp1)]    <- c(0.981,	1.258,	0.577,	0.288,	0.263,	0.255,	0.24,	0.267,	0.376)








# 4. Build SRRs ---------------------------------------------------------------
srr_years <- an(hist.yrs)
stks <- names(stocks)

stks.data <- vector("list", length(ad_stocks))
names(stks.data) <- ad_stocks
for(i in seq(stocks)){
  
  if(stks[i] %in% ad_stocks){
    
    DIMS <- dims(biols[[i]])
    RANGE <- range(biols[[i]])
    
    stks.data[[stks[i]]] <- paste0(
      stks[i],
      c(
        ".unit",
        ".age.min",
        ".age.max",   
        "_sr.model",
        "_params.n",
        "_params.name",
        "_params.array",        
        "_rec.flq",
        "_ssb.flq",
        "_proportion.flq",
        "_prop.avg.yrs",
        "_timelag.matrix",        
        "_range.plusgroup",
        "_range.minyear",
        "_uncertainty.flq"
      )
    )

    assign(paste0(stks[i], ".unit"), DIMS$unit)
    assign(paste0(stks[i], ".age.min"), RANGE["min"])
    assign(paste0(stks[i], ".age.max"), RANGE["max"])
    
    sr_type <- 'geomean'
    assign(paste0(stks[i], "_sr.model"), sr_type)

    # params
    tmp1 <- stocks[[i]][,ac(srr_years)]
    tmp2 <- as.FLSR(tmp1)
    model(tmp2) <- sr_type
    tmp2 <- fmle(tmp2)
    # plot(tmp2)
    
    tmp3 <- as.data.frame(tmp2@params)
    tmpdf <- expand.grid(
      param = tmp3$params,
      year = seq(first.yr, last.yr),
      unit = "unique",
      season = seq(dims(tmp1)$season),
      area = "unique",
      iter = 1
    )
    tmpdf$data <- tmp3$data[match(tmpdf$param, tmp3$params)]
    # write.csv(tmpdf, file=paste0(file.path("output", "sr"), "/", stks[i], "_params.csv"))
    
    # stk1_params.array
    tmp4 <- xtabs2(
      data~param+year+season+iter,
      data=tmpdf,
      exclude=NULL,
      na.action=na.pass
    )
    
    # substitute Recr assumptions
    mat <- match(stks[i], nameChange(rownames(Recr), form = "new"))
    tmp4[, proj.yrs,,] <- Recr[mat, proj.yrs]
    
    # stk1_params.n
    assign(
      paste0(stks[i], "_params.n"),
      length(tmp3$params)
    )
    
    # stk1_params.name
    assign(
      paste0(stks[i], "_params.name"),
      as.character(tmp3$params)
    )
    
    assign(
      paste0(stks[i], "_params.array"),
      tmp4
    )

    # stk1_rec.flq
    assign(
      paste0(stks[i], "_rec.flq"),
      rec(tmp1)[,ac(first.yr:(proj.yr-1))]
    )
    
    # stk1_ssb.flq
    assign(
      paste0(stks[i], "_ssb.flq"),
      ssb(tmp1)[,ac(first.yr:(proj.yr-1))]
    )
    
    # stk1_proportion.flq
    assign(
      paste0(stks[i], "_proportion.flq"),
      FLQuant(1, dimnames=list(age="all", year=first.yr:last.yr, unit=1, season=1))
    )
    
    # stk_prop.avg.yrs
    assign(paste0(stks[i], "_prop.avg.yrs"), hist.yrs)
    
    
    # stk1_timelag.matrix
    assign(
      paste0(stks[i], "_timelag.matrix"),
      matrix(c(ifelse(dims(tmp1)$min==0, 1, dims(tmp1)$min),1), nrow=2, ncol=ns, dimnames = list(c('year', 'season'),'all'))
    )
    
    assign(paste0(stks[i], "_range.plusgroup"), RANGE["plusgroup"])
    assign(paste0(stks[i], "_range.minyear"), RANGE["minyear"])

    # stk1_uncertainty.flq
    assign(
      paste0(stks[i], "_uncertainty.flq"),
      FLQuant(1, dimnames=list(age="all", year=first.yr:last.yr, unit=1, season=1))
    )
    
    
    rm(list = c("tmp1", "tmp2", "tmp3", "tmp4"))
      
  }
}

## build SRs
SRs <- FLBEIA:::create.SRs.data(
  yrs = yrs,
  ns = ns,
  ni = ni,
  stks.data = stks.data
)
class(SRs)

SRs[["COD_dash_NS"]]@params
# Recr["COD_dash_NS",]

save(SRs, file = file.path("model", "SRs.Rdata"))
# load(file = file.path("model", "SRs.Rdata"))


# 5. Build FLfleetsExt --------------------------------------------

# 5.1. Rename FCube FLFleets to use in conditioning function --------------

FCUBEfleets <- fleets
names(FCUBEfleets) <- nameChange(names(FCUBEfleets))
for(fl in seq(FCUBEfleets)){
  names(FCUBEfleets[[fl]]@metiers) <- nameChange(names(FCUBEfleets[[fl]]@metiers))
  for(met in seq(FCUBEfleets[[fl]]@metiers)){
    names(FCUBEfleets[[fl]]@metiers[[met]]@catches) <- nameChange(names(FCUBEfleets[[fl]]@metiers[[met]]@catches))
    print(paste("fl", fl, "; met", met, "is finished"))
  }
}
# beepr::beep()

# some stats on fleets object
range(FCUBEfleets)
class(FCUBEfleets)
length(FCUBEfleets) # number of fleets
sum(unlist(lapply(FCUBEfleets, FUN = function(x){length(x@metiers)}))) # number of metiers

# trim to years range to accommodate catch at age data
FCUBEfleets <- window(FCUBEfleets, start=first.yr, end=proj.yr-1)
# beepr::beep()

### save data
save(FCUBEfleets, file=file.path("model", "FCUBEfleets_renamed.Rdata"))
# load(file=file.path("model", "FCUBEfleets_renamed.Rdata"))

rm(fleets)


# 5.2 Extract slots for FLFleetsExt creation (landings.n, discards.n etc ---------
### create individual objects for each fleet/metier/stock statistic where stock matches biols

general_proj.ave.years <- proj.yr-1 # years to average  for forecast years

fls.data <- vector("list", length(FCUBEfleets))
names(fls.data) <- names(FCUBEfleets)
fls.data
fls <- names(fls.data)
for( i in seq(fls) ){ ### for each fleet

  ### Grab stuff from FCUBEfleets (temporary - should use raw data at some point rather than rely on this object)
  # effort and capacity
  assign(paste0(fls[i], "_effort.flq"), FCUBEfleets[[fls[i]]]@effort)
  assign(paste0(fls[i], "_capacity.flq"), FCUBEfleets[[fls[i]]]@effort*5) #FCUBEfleets[[fls[i]]]@capacity)
  fls.data[[fls[i]]] <- c(
    fls.data[[fls[i]]],
    paste0(fls[i], "_effort.flq"),
    paste0(fls[i], "_capacity.flq")
  )
  
  if(all(!is.na(FCUBEfleets[[fls[i]]]@fcost))){ # if @fcost slot not empty, make object
    assign(paste0(fls[i], "_fcost.flq"), FCUBEfleets[[fls[i]]]@fcost)
    fls.data[[fls[i]]] <- c(
      fls.data[[fls[i]]],
      paste0(fls[i], "_fcost.flq")
    )
  }
  
  # projection years
  assign(paste0(fls[i], "_proj.avg.yrs"), general_proj.ave.years)
  fls.data[[fls[i]]] <- c(
    fls.data[[fls[i]]],
    paste0(fls[i], "_proj.avg.yrs")
  )
  
  mets <- names(FCUBEfleets[[fls[i]]]@metiers)
  
  # check for at least one species of overlap between fleets and biols
  tmp <- c()
  for(m in seq(mets)){
    tmp[m] <- 
      sum(names(FCUBEfleets[[fls[i]]]@metiers[[m]]@catches) %in% names(biols)) > 0 &
      sum(unlist(lapply(FCUBEfleets[[fls[i]]]@metiers[[m]]@catches, FUN = function(stk){
        lan <- landings(stk)[,ac(general_proj.ave.years)]
        dis <- discards(stk)[,ac(general_proj.ave.years)]
        return(c(lan + dis))}))) > 0
  }
  
  mets <- mets[which(tmp)]
  
  assign(paste0(fls[i], ".mets"), mets)
  fls.data[[fls[i]]] <- c(
    fls.data[[fls[i]]],
    paste0(fls[i], ".mets")
  )  
  for( j in seq(mets) ){ ### for each metier
    
    # assign effshare of fl/met
    assign(paste0(fls[i], ".", mets[j], "_effshare.flq"), FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@effshare)
    fls.data[[fls[i]]] <- c(
      fls.data[[fls[i]]],
      paste0(fls[i], ".", mets[j], "_effshare.flq")
    )
    
    # projection years
    assign(paste0(fls[i], ".", mets[j], "_proj.avg.yrs"), general_proj.ave.years)
    fls.data[[fls[i]]] <- c(
      fls.data[[fls[i]]],
      paste0(fls[i], ".", mets[j], "_proj.avg.yrs")
    )
    
    stks <- names(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches)
    stks <- stks[which(stks %in% names(biols))] # trim to match biols
    
    # check that all species should be included
    stk.incl <- rep(1, length(stks))
    for(k in seq(stks)){# i=24;j=1;k=4
      # causes for removal
      if(stks[k] %in% ad_stocks){
        comp.lan <- computeLandings(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]])[,ac(general_proj.ave.years)]
        comp.dis <- computeDiscards(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]])[,ac(general_proj.ave.years)]
        lan <- landings(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]])[,ac(general_proj.ave.years)]
        dis <- discards(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]])[,ac(general_proj.ave.years)]
        
        if( # if no landings or discards
          all(lan == 0) & all(dis == 0)
        ){stk.incl[k] <- 0}
        
        if( # if landings exist but none when computating: sum(landings.n * landings.wt)
          any(
            is.na(comp.lan) |
            is.na(comp.dis) |
            is.na(lan) |
            is.na(dis)
          )
        ){stk.incl[k] <- 0}
        
      }
      
      if(stks[k] %in% aa_stocks){
        lan <- landings(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]])[,ac(general_proj.ave.years)]
        dis <- discards(FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]])[,ac(general_proj.ave.years)]
        
        if( # if no landings or discards
          all(lan == 0) & all(dis == 0)
        ){stk.incl[k] <- 0}
        
        if( # if landings exist but none when computating: sum(landings.n * landings.wt)
          any(
            is.na(lan) |
            is.na(dis)
          )
        ){stk.incl[k] <- 0}
        
      }
    }
    
    # remove stks
    stks <- stks[which(stk.incl == 1)]
    
    # if(length(stks) == 0){
    #   
    # }
 
    assign(paste0(fls[i], ".", mets[j], ".stks"), stks)
    fls.data[[fls[i]]] <- c(
      fls.data[[fls[i]]],
      paste0(fls[i], ".", mets[j], ".stks")
    )    
    
    for( k in seq(stks) ){ ### for each stock

      if(stks[k] %in% ad_stocks){ ### for each age-disaggregated stock
        
        catch <- FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]]
        
        # get rid of .wt zeros or NAs (substitute from biols)
        lan.wt.idx <- which(catch@landings.wt == 0 | is.na(catch@landings.wt))
        if(length(lan.wt.idx)>0){as(catch@landings.wt, 'array')[lan.wt.idx] <- biols[[stks[k]]]@wt[,hist.yrs][lan.wt.idx, drop=TRUE]}
        
        dis.wt.idx <- which(catch@discards.wt == 0 | is.na(catch@discards.wt))
        if(length(dis.wt.idx)>0){as(catch@discards.wt, 'array')[dis.wt.idx] <- biols[[stks[k]]]@wt[,hist.yrs][dis.wt.idx, drop=TRUE]}
        
        # make alpha and beta
        ones <- catch@catch.q*0 + 1
        units(ones) <- "NA"
        
        ### assign data
        # landings.n
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.n.flq"), catch@landings.n )
        # landings.wt
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.wt.flq"), catch@landings.wt )
        # discards.n
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.n.flq"), catch@discards.n )
        # discards.wt
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.wt.flq"), catch@discards.wt )
        # price
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_price.flq"), catch@price )
        # catch.q
        # assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_catch.q.flq"), catch@catch.q )
        # alpha
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_alpha.flq"), ones )
        # beta
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_beta.flq"), ones )
        
        
        
        # projection years
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_proj.avg.yrs"), general_proj.ave.years )
         
        # remove temporary objects
        rm(list = c("catch"))
        
        # add slot names to fls.data
        fls.data[[fls[i]]] <- c(
          fls.data[[fls[i]]],
          paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.n.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.wt.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.n.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.wt.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_price.flq"),
          # paste0(fls[i], ".", mets[j], ".", stks[k], "_catch.q.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_alpha.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_beta.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_proj.avg.yrs")
        )

      } ### end of procedure for age-based stocks
      
      if(stks[k] %in% aa_stocks){
        # landings.n
        tmp <- FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]]@landings / 
          stocks[[stks[k]]]@landings.wt[,hist.yrs]
        # tmp <- FCUBEfleets[[i]]@metiers[[mets[j]]]@catches[[stks[k]]]@landings
        if(dimnames(tmp)$age == "all"){dimnames(tmp)$age <- 1}
        # tmp <- tmp / biols[[stks[k]]]@wt[,hist.yrs] # convert to numbers
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.n.flq"), tmp )
        rm(tmp)
        
        # landings.wt
        tmp <- stocks[[stks[k]]]@landings.wt[,hist.yrs]
        # tmp <- biols[[stks[k]]]@wt[,hist.yrs] # FCUBEfleets[[i]]@metiers[[j]]@catches[[stks[k]]]@landings
        if(dimnames(tmp)$age == "all"){dimnames(tmp)$age <- 1}
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.wt.flq"), tmp )
        rm(tmp)
        
        # discards.n
        tmp <- FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]]@discards / 
          stocks[[stks[k]]]@discards.wt[,hist.yrs]
        # tmp <- FCUBEfleets[[i]]@metiers[[mets[j]]]@catches[[stks[k]]]@discards
        if(dimnames(tmp)$age == "all"){dimnames(tmp)$age <- 1}
        # tmp <- tmp / biols[[stks[k]]]@wt[,hist.yrs] # convert to numbers
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.n.flq"), tmp )
        rm(tmp)
        
        # discards.wt
        tmp <- stocks[[stks[k]]]@discards.wt[,hist.yrs]
        # tmp <- biols[[stks[k]]]@wt[,hist.yrs] # FCUBEfleets[[i]]@metiers[[j]]@catches[[stks[k]]]@discards
        if(dimnames(tmp)$age == "all"){dimnames(tmp)$age <- 1}
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.wt.flq"), tmp )
        rm(tmp)
        
        # price
        tmp <- FCUBEfleets[[fls[i]]]@metiers[[mets[j]]]@catches[[stks[k]]]@price
        if(dimnames(tmp)$age == "all"){dimnames(tmp)$age <- 1}
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_price.flq"), tmp )
        rm(tmp)
        
        # catch.q (not provided for aa stocks)
 
        # projection years
        assign(paste0(fls[i], ".", mets[j], ".", stks[k], "_proj.avg.yrs"), general_proj.ave.years )
        
        # add slot names to fls.data
        fls.data[[fls[i]]] <- c(
          fls.data[[fls[i]]],
          paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.n.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_landings.wt.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.n.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_discards.wt.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_price.flq"),
          paste0(fls[i], ".", mets[j], ".", stks[k], "_proj.avg.yrs")
        )
      }
      
      print(paste("###", "fl", i, "|", "met", j, "|", "stk", k, "###"))
    
    } ### end of stks loop
  } ### end of mets loop
} ### end of fls loop
# beepr::beep()






stks.data <- vector("list", length(biols))
names(stks.data) <- names(biols)
stks <- names(biols)
for(i in seq(biols)){

  DIMS <- dims(biols[[i]])
  RANGE <- range(biols[[i]])
  
  stks.data[[stks[i]]] <- paste0(
    stks[i],
    c(
      ".unit",
      ".age.min",
      ".age.max",
      "_n.flq"
    )
  )

  assign(paste0(stks[i], ".unit"), DIMS$unit)
  assign(paste0(stks[i], ".age.min"), RANGE["min"])
  assign(paste0(stks[i], ".age.max"), RANGE["max"])
  assign(paste0(stks[i], "_n.flq"), biols[[i]]@n[,hist.yrs])
  
  if(stks[i] %in% ad_stocks){
    stks.data[[stks[i]]] <- c(stks.data[[stks[i]]], paste0(stks[i], "_m.flq"))
    assign(paste0(stks[i], "_m.flq"), biols[[i]]@m[,hist.yrs])
  }
  
  if(stks[i] %in% aa_stocks){
    stks.data[[stks[i]]] <- c(
      stks.data[[stks[i]]],
      paste0(stks[i], "_gB.flq")
    )
    if(stks[i] %in% fixed_stocks){
      assign(
        paste0(stks[i], "_gB.flq"), 
        FLQuant(
          array(0, dim = c(1, length(hist.yrs), DIMS$unit, ns, 1, ni)), 
          dimnames=list(age=1, year=hist.yrs)
        )
      )
    } else {
      if(stks[i] %in% spict_stocks){
        fit <- spicts[[stks[i]]]
        tab1 <- sumspict.parest(fit)
        tab5 <- sumspict.predictions(fit)
        B.tmp <- computeStock(biols[[stks[i]]])
        B.tmp[,ac(proj.yr)] <- tab5[1,1] # biomass projection year
        C.tmp <- catch(window(stocks[[stks[i]]], start=first.yr, end=last.yr))
        C.tmp[,ac(proj.yr)] <- tab5[5,1]
        tmp <- B.tmp[,ac((first.yr+1):(proj.yr))] - B.tmp[,ac((first.yr):(proj.yr-1))] + C.tmp[,ac((first.yr):(proj.yr-1))]
        dimnames(tmp)$age <- 1
        dimnames(tmp)$year <- hist.yrs
        assign(paste0(stks[i], "_gB.flq"),
          tmp
        )
      }
    }
  }
}



# 5.3. Unify slots for FLFleetsExt object ---------------------

### WARNING: takes a while... 30 min ###
source("bootstrap/software/functions/create.fleets.data_tmp.R")
t1 <- Sys.time()
stks <- names(biols)
# fleets <- FLBEIA::create.fleets.data(
fleets <- create.fleets.data_tmp(
  yrs = yrs,
  ns = ns,
  ni = ni,
  fls.data = fls.data,
  stks.data = stks.data
)
t2 <- Sys.time(); t2-t1
# beepr::beep()


save(fleets, file = file.path("model", "fleets.Rdata"))
# load(file = file.path("model", "fleets.Rdata"))


# 5.4. Add economic parameters -------------------------------

addEcon = FALSE
year_ave <- proj.yr-1
year_ave_else <- proj.yr-1
if(addEcon){
  
  load("data/STECF_econ/df.stecf2.Rdata")
  # names(df.stecf)
  vcostVars <- c("Energy_costs_euro", "Value_of_unpaid_labour_euro", 
    "Repair_&_maintenance_costs_euro", "Other_variable_costs_euro")
  fcostVars <- c("Other_non-variable_costs_euro", "Consumption_of_fixed_capital_euro")
  
  frac_fcost <- vector("list", length(fleets))
  frac_vcost <- vector("list", length(fleets))
  for(i in 1:length(fleets)){
    fl <- names(fleets)[i]
    df.stecf.fl <- subset(df.stecf, fleet == fl)
    
    # determine metier effshare for weighting stecf fcosts (based on mean of year_ave, unless all are zero, then proj_yr-1)
    eff <- fleets[[fl]]@effort
    nonZero <- which(!eff==0 | is.na(eff))
    nonZeroYear <- as.numeric(dimnames(eff)[[2]][nonZero])
    if(all(year_ave %in% nonZeroYear)){year_use <- year_ave}else{year_use <- year_ave_else}
    lu <- data.frame(effshare = unlist(lapply(fleets[[fl]]@metiers, function(x){mean(c(x@effshare[,ac(year_use)]))})))
    lu$effshare <- lu$effshare/sum(lu$effshare)
    lu$metier <- rownames(lu)
    df.stecf.fl <- merge(df.stecf.fl, lu)
    
    # calculate total revenue for fleet
    rev <- revenue_flbeia(fleets[[fl]])
    units(rev) <- "NA"
    
    # crewshare (proportion of landings value) - weighted by metier effshare
    fleets[[fl]]@crewshare[] <- weighted.mean(x = df.stecf.fl$Crewshare_frac, w = df.stecf.fl$effshare)
    units(fleets[[fl]]@crewshare) <- "NA"    
    
    # fixed costs (independent of effort) - weighted by metier effshare
    fleets[[fl]]@fcost[] <- mean(c((rev * weighted.mean(x = df.stecf.fl$fcost_rev_ratio, w = df.stecf.fl$effshare))[,ac(year_ave)]), na.rm = TRUE)
    units(fleets[[fl]]@fcost) <- "NA"
    
    # record fractional fixed costs
    frac_fcost.fl <- as.data.frame(t(apply(df.stecf.fl[,fcostVars], 2, # weighted mean values
      FUN = function(x){weighted.mean(x = x, w = df.stecf.fl$effshare, na.rm = TRUE)})))
    frac_fcost.fl <- as.data.frame(t(apply(frac_fcost.fl[,fcostVars], 1, # fractional costs
      FUN = function(x){x/sum(x, na.rm = TRUE)})))
    frac_fcost.fl <- cbind(fleet = df.stecf.fl[1,c("fleet")], frac_fcost.fl) # add fleet id
    frac_fcost[[i]] <- frac_fcost.fl
    
    # variable costs (assumes same variable costs per metier using fleet's cost/rev ratio)
    # revenue by metier is less reliable given that the landings of a given metier may not contain all landed species (e.g. Crangon)
    frac_vcost.fl <- vector("list", length(fleets[[i]]@metiers))
    vcost.fl <- (mean(c(rev[,ac(year_use)]), na.rm = TRUE) * mean(x = df.stecf.fl$vcost_rev_ratio, na.rm = TRUE)) /
      mean(fleets[[fl]]@effort[,ac(year_use)], na.rm = TRUE)
    # variable costs (per unit effort)
    for(j in seq(fleets[[i]]@metiers)){
      met <- names(fleets[[i]]@metiers)[j]
      df.stecf.fl.met <- subset(df.stecf.fl, fleet == fl & metier == met)
      
      # # calculate value of landings for metier
      # rev.mt <- Reduce("+", lapply(fleets[[fl]]@metiers[[met]]@catches, FUN = function(x) seasonSums(unitSums(quantSums(x@landings.n * 
      #   x@landings.wt * x@price)))))
      # units(rev.mt) <- "NA"
      # fleets[[fl]]@metiers[[met]]@vcost[] <- mean(c(((rev.mt * mean(x = df.stecf.fl.met$vcost_rev_ratio)) / 
      #     (fleets[[fl]]@effort * fleets[[fl]]@metiers[[met]]@effshare))[,ac(year_ave)]), na.rm = TRUE) # rev/eff * cost/rev = cost/eff

      fleets[[fl]]@metiers[[met]]@vcost[] <- vcost.fl
      units(fleets[[fl]]@metiers[[met]]@vcost) <- "NA"
      
      # record fractional fixed costs
      frac_vcost.fl.met <- as.data.frame(t(apply(df.stecf.fl.met[,vcostVars], 2, # mean values
        FUN = function(x){mean(x = x, na.rm = TRUE)})))
      frac_vcost.fl.met <- as.data.frame(t(apply(frac_vcost.fl.met, 1, # fractional costs
        FUN = function(x){x/sum(x, na.rm = TRUE)})))
      frac_vcost.fl.met <- cbind(df.stecf.fl.met[1,c("fleet", "metier")], frac_vcost.fl.met) # add fleet id
      frac_vcost.fl[[j]] <- frac_vcost.fl.met
    }
    frac_vcost.fl <- do.call("rbind", frac_vcost.fl) # collapse list
    frac_vcost[[i]] <- frac_vcost.fl

    print(paste(round(i/length(fleets)*100), "% is finished"))
  }
  beepr::beep()
  
  frac_fcost <- do.call("rbind", frac_fcost) # collapse list
  frac_vcost <- do.call("rbind", frac_vcost) # collapse list
  
  save(frac_fcost, file = file.path("output", "frac_fcost.Rdata"))
  save(frac_vcost, file = file.path("output", "frac_vcost.Rdata"))

  save(fleets, file = file.path("output", "fleets_w_econ.Rdata"))
  # load(file = file.path("output", "fleets_w_econ.Rdata"))
}


# 6. Save objects --------------------------------------

save(
  biols, spicts, stocks, fleets, SRs, 
  aa_stocks, ad_stocks, spict_stocks, fixed_stocks,
  file = file.path("model", "02_condition_FLBEIA.Rdata")
)


