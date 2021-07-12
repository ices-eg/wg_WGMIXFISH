# totvcost_flbeia2 <- function (fleet){
#   mts <- names(fleet@metiers)
#   res <- FLQuant(0, dimnames = dimnames(fleet@effort))
#   for (mt in mts) {
#     res <- res + fleet@metiers[[mt]]@vcost * fleet@effort *
#       fleet@metiers[[mt]]@effshare
#   }
#   Rev <- revenue_flbeia(fleet) # * fleet@crewshare
#   units(res) <- units(Rev)
#   remainder <- Rev - res
#   crewsh <- remainder * fleet@crewshare
#   units(crewsh) <- units(Rev)
#   res <- res + crewsh
#   return(res)
# }

totvcost_flbeia2 <- function (fleet){
  mts <- names(fleet@metiers)
  res <- FLQuant(0, dimnames = dimnames(fleet@effort))
  for (mt in mts) {
    res <- res + fleet@metiers[[mt]]@vcost * fleet@effort * 
      fleet@metiers[[mt]]@effshare
  }
  Rev <- revenue_flbeia(fleet) * fleet@crewshare
  units(res) <- units(Rev)
  res <- res + Rev
  return(res)
}

totfcost_flbeia2 <- function (fleet, covars, flnm = NULL){
  if (is.null(flnm)) 
    flnm <- 1
  return(fleet@fcost * covars[["NumbVessels"]][flnm, ])
}


costs_flbeia2 <- function (fleet, covars, flnm = NULL){
  res <- totvcost_flbeia2(fleet) + totfcost_flbeia2(fleet, covars, flnm)
  return(res)
}



fltSum2 <- function (obj, flnms = "all", years = dimnames(obj$biols[[1]]@n)$year, 
  byyear = TRUE, long = FALSE, InterestRate = 0.03, scenario = "bc") 
{
  grossValue <- costs <- salaries <- grossSurplus <- nVessels <- fep <- NULL
  fleets <- obj$fleets
  covars <- obj$covars
  warning("Due to a problem with the units attribute in some off the slots, sometimes this function crashes. In case it fails, we recommend \n          removing the units using the setUnitsNA function")
  if (flnms[1] == "all") 
    flnms <- names(fleets)
  Dim <- dim(fleets[[1]]@effort[, years, ])[c(2, 4, 6)]
  Dimnm <- dimnames(fleets[[1]]@effort[, years, ])
  FLQ0.dimfleets <- FLQuant(0, dimnames = c(fleet = list(names(fleets)), ### 0.0 as default for additive factors
    dimnames(fleets[[1]]@effort[, , ])[2:6]))
  FLQ1.dimfleets <- FLQuant(1, dimnames = c(fleet = list(names(fleets)), ### 1.0 as default for multiplicative factors
    dimnames(fleets[[1]]@effort[, , ])[2:6]))
  if (is.null(covars$Depreciation)) 
    covars$Depreciation <- FLQ0.dimfleets
  if (is.null(covars$Salaries)) 
    covars$Salaries <- FLQ0.dimfleets
  if (is.null(covars$MaxDays)) 
    covars$MaxDays <- FLQuant(365/dim(fleets[[1]]@effort[, 
      years, ])[4], dimnames = dimnames(FLQ0.dimfleets))
  if (is.null(covars$NumbVessels)) 
    covars$NumbVessels <- FLQ1.dimfleets # changed to 1.0 multiplier (multiplicative in totfcost_flbeia)
  if (is.null(covars$CapitalCost)) 
    covars$CapitalCost <- FLQ0.dimfleets
  if (byyear == F) {
    res <- NULL
    res.fl <- NULL
    year = rep(years, prod(Dim[2:3]))
    season = rep(rep(Dimnm[[4]], each = Dim[1]), Dim[3])
    iter = rep(rep(1:Dim[3], each = prod(Dim[1:2])), 1)
    for (f in flnms) {
      fl <- fleets[[f]]
      mts <- names(fl@metiers)
      fleet <- rep(f, each = prod(Dim))
      temp.catch <- lapply(catchNames(fl), function(x) quantSums(unitSums(catchWStock.f(fl, x))))
      temp.landings <- lapply(catchNames(fl), function(x) quantSums(unitSums(landWStock.f(fl, x))))
      temp.discards <- lapply(catchNames(fl), function(x) quantSums(unitSums(discWStock.f(fl, x))))
      res.fl <- bind_cols(year = year, season = season, 
        fleet = fleet, iter = iter, 
        catch = c(Reduce("+", temp.catch)[, years]), 
        landings = c(Reduce("+", temp.landings)[, years]), 
        discards = c(Reduce("+", temp.discards)[, years]), 
        capacity = c(fl@capacity[, years, ]), 
        effort = c(fl@effort[, years, ]), 
        fcosts = c(totfcost_flbeia2(fl, covars, f)[, years, ]), 
        vcosts = c(totvcost_flbeia2(fl)[, years, ]), 
        costs = c(costs_flbeia2(fl, covars, f)[, years, ]), 
        grossValue = c(revenue_flbeia(fl)[, years, ]), 
        nVessels = c(covars[["NumbVessels"]][f, years])
      ) %>% mutate(
        discRat = discards/catch, 
        grossSurplus = grossValue - costs, 
        price = grossValue/landings, 
        salaries = c(fl@crewshare[, years, ]) * grossValue + c(covars[["Salaries"]][f, years]), 
        gva = grossValue - costs + salaries,
        profitability = grossSurplus/grossValue, 
        fep = grossSurplus - c(covars[["Depreciation"]][f, years]) * nVessels, 
        netProfit = fep - c(covars[["CapitalCost"]][f, years]) * InterestRate * nVessels
      )
      temp <- lapply(catchNames(fl), function(x) quantSums(unitSums(catchWStock.f(fl, x))))
      temp <- Reduce("+", temp)[, years]
      totTAC <- Reduce("+", lapply(names(obj$advice$quota.share), 
        function(x) obj$advice$quota.share[[x]][f, years] * 
          obj$advice$TAC[x, years]))
      if (dim(temp)[4] > 1) {
        res.fl <- res.fl %>% mutate(quotaUpt = c(sweep(temp, 
          c(1:3, 5:6), totTAC/dim(temp)[4], "/")))
      }
      else {
        res.fl <- res.fl %>% mutate(quotaUpt = c(temp/totTAC))
      }
      res <- bind_rows(res, res.fl)
    }
  } else {
    res <- NULL
    res.fl <- NULL
    year = rep(years, Dim[3])
    iter = rep(rep(1:Dim[3], each = Dim[1]), 1)
    for (f in flnms) {
      fl <- fleets[[f]]
      mts <- names(fl@metiers)
      fleet <- rep(f, each = prod(Dim[-2]))
      temp.catch <- lapply(catchNames(fl), function(x) seasonSums(quantSums(unitSums(catchWStock.f(fl, x)))))
      temp.landings <- lapply(catchNames(fl), function(x) seasonSums(quantSums(unitSums(landWStock.f(fl, x)))))
      temp.discards <- lapply(catchNames(fl), function(x) seasonSums(quantSums(unitSums(discWStock.f(fl, x)))))
      res.fl <- bind_cols(
        year = year, fleet = fleet, 
        iter = iter, catch = c(Reduce("+", temp.catch)[, years]), 
        landings = c(Reduce("+", temp.landings)[, years]), 
        discards = c(Reduce("+", temp.discards)[, years]), 
        capacity = c(seasonSums(fl@capacity[, years, ])), 
        effort = c(seasonSums(fl@effort[, years, ])), 
        fcosts = c(seasonSums(totfcost_flbeia2(fl, covars, f)[, years, ])), 
        vcosts = c(seasonSums(totvcost_flbeia2(fl)[, years, ])), 
        costs = c(seasonSums(costs_flbeia2(fl, covars, f)[, years, ])), 
        grossValue = c(seasonSums(revenue_flbeia(fl)[, years, ])), 
        nVessels = c(seasonMeans(covars[["NumbVessels"]][f, years]))
      ) %>% mutate(
        discRat = discards/catch, 
        grossSurplus = grossValue - costs, price = grossValue/landings, 
        salaries = c(seasonSums(fl@crewshare[, years, ])) * grossValue + 
          c(seasonSums(covars[["Salaries"]][f, years])), 
        gva = grossValue - costs + salaries, 
        profitability = grossSurplus/grossValue, 
        fep = grossSurplus - c(seasonSums(covars[["Depreciation"]][f, years])) * nVessels, 
        netProfit = fep - c(seasonSums(covars[["CapitalCost"]][f, years])) * InterestRate * nVessels
      )
      temp <- lapply(catchNames(fl), function(x) seasonSums(quantSums(unitSums(catchWStock.f(fl, x)))))
      temp <- Reduce("+", temp)[, years]
      totTAC <- Reduce("+", lapply(names(obj$advice$quota.share), 
        function(x) obj$advice$quota.share[[x]][f, years] * 
          obj$advice$TAC[x, years]))
      res.fl <- res.fl %>% mutate(quotaUpt = c(temp/totTAC))
      res <- bind_rows(res, res.fl)
    }
  }
  if (long == TRUE) {
    ind <- if_else(byyear == TRUE, 3, 4)
    indicator.nms <- names(res)[-c(1:ind)]
    res <- res %>% gather(key = "indicator", value = "value", 
      indicator.nms)
  }
  res <- res %>% mutate(scenario = scenario) %>% select(scenario, 
    everything())
  return(res)
}
