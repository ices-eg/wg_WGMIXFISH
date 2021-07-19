# ~ Converting SAM to FLStock ####
# Simon Fisher
sam_to_FLStock <- function(object, ### sam object
                           uncertainty = FALSE, ### confidence intervals?
                           conf_level = 95, ### confidence interval level in %
                           catch_estimate = FALSE, ### use catch input or
                           ### model estimates
                           correct_catch = FALSE, ### "correct" catch with
                           ### estimated multiplier
                           ...) {
  
  ### check class type of input
  if (!isTRUE("sam" %in% class(object))) stop("object has to be class sam")
  
  ### calculate SD multiplier for uncertainty range
  if (isTRUE(uncertainty)) SD_mult <- stats::qnorm(0.5 + conf_level/200)
  
  ### get dimensions
  years <- object$data$years
  ages <- object$conf$minAge:object$conf$maxAge
  
  ### create FLQuant dummy
  qnt <- FLQuant(NA, dimnames = list(age = ages, year = years))
  ### create FLStock
  stk <- FLStock(qnt)
  
  ### stock numbers @ age
  stock.n <- exp(object$pl$logN)
  n_ages <- dim(stock.n)[1]
  n_yrs <- dim(stock.n)[2]
  stock.n(stk)[1:n_ages, 1:n_yrs] <- stock.n
  
  ### add range
  if (isTRUE(uncertainty)) {
    stock.n_sd <- qnt
    stock.n_sd[1:n_ages, 1:n_yrs] <- object$plsd$logN
    attr(stock.n(stk), "low") <- stock.n(stk) - stock.n_sd * SD_mult
    attr(stock.n(stk), "high") <- stock.n(stk) + stock.n_sd * SD_mult
  }
  
  ### harvest @ age
  harvest <- rbind(NA, exp(object$pl$logF))
  ### duplicate linked ages
  harvest <- harvest[object$conf$keyLogFsta[1,] + 2, ]
  n_ages <- dim(harvest)[1]
  n_yrs <- dim(harvest)[2]
  harvest(stk)[1:n_ages, 1:n_yrs] <- harvest
  units(harvest(stk)) <- "f"
  
  ### add range
  if (isTRUE(uncertainty)) {
    tmp <- rbind(NA, object$plsd$logF)
    tmp <- tmp[object$conf$keyLogFsta[1,] + 2, ]
    harvest_sd <- qnt
    harvest_sd[1:n_ages,1:n_yrs] <- tmp
    attr(harvest(stk), "low") <- harvest(stk) - harvest_sd * SD_mult
    attr(harvest(stk), "high") <- harvest(stk) + harvest_sd * SD_mult
  }
  
  #input data
  
  # stock
  # stock weight @ age
  stock.wt <- t(object$data$stockMeanWeight)
  n_ages <- dim(stock.wt)[1]
  n_yrs <- dim(stock.wt)[2]
  stock.wt(stk)[1:n_ages, 1:n_yrs] <- stock.wt
  # stock biomass
  stock <- quantSums(qnt)
  stock[, 1:n_yrs] <- exp(object$sdrep$value[names(object$sdrep$value) == "logtsb"])
  stock(stk) <- stock
  
  # add range
  if (isTRUE(uncertainty)) {
    stock_sd <- quantSums(qnt)
    stock_sd[, 1:n_yrs] <- object$sdrep$sd[names(object$sdrep$value) == "logtsb"]
    attr(stock(stk), "low") <- exp(log(stock(stk)) - stock_sd * SD_mult)
    attr(stock(stk), "high") <- exp(log(stock(stk)) + stock_sd * SD_mult)
  }
  
  
  # catch
  # catch weight @ age
  if (isTRUE(length(dim(object$data$catchMeanWeight)) == 3)) {
    catch.wt <- t(object$data$catchMeanWeight[,,, drop = TRUE])
  } else {
    catch.wt <- t(object$data$catchMeanWeight)
  }
  n_ages <- dim(catch.wt)[1]
  n_yrs <- dim(catch.wt)[2]
  catch.wt(stk)[1:n_ages, 1:n_yrs] <- catch.wt
  ### catch numbers @ age
  catch_fleets <- which(object$data$fleetTypes == 0)
  ### extract observations & estimates
  dat_catch <- cbind(object$data$aux, value = exp(object$data$logobs),
                     estimate = exp(object$rep$predObs))
  dat_catch <- dat_catch[dat_catch[, "fleet"] == catch_fleets, ]
  ### sum up catch numbers over all commercial fleets
  dat_catch <- stats::aggregate(cbind(value, estimate) ~ age + year, dat_catch,
                                FUN = sum)
  ### add missing ages/years
  dat_full <- expand.grid(age = unique(dat_catch$age),
                          year = unique(dat_catch$year))
  dat_catch <- merge(x = dat_catch, y = dat_full, all = TRUE)
  dat_catch <- dat_catch[order(dat_catch$year, dat_catch$age), ] ### sort
  
  ### insert catch: estimates or input values
  if (!isTRUE(catch_estimate)) {
    
    ### use input data
    catch.n(stk)[ac(unique(dat_catch$age)), ac(unique(dat_catch$year))] <-
      dat_catch$value
    
  } else {
    
    ### use value estimated by SAM
    catch.n(stk)[ac(unique(dat_catch$age)), ac(unique(dat_catch$year))] <-
      dat_catch$estimate
    
  }
  
  ### correct catch numbers if a catch multiplier was estimated
  if (isTRUE(correct_catch)) {
    
    ### warn if no catch multiplier available
    if (!(length(object$pl$logScale) > 1)) {
      warning("catch correction requested but no catch multiplier estimated")
    }
    
    ### get catch multiplier dimensions
    ages_mult <- object$conf$minAge:object$conf$maxAge
    yrs_mult <- object$conf$keyScaledYears
    ### create catch multiplier FLQuant
    catch_mult_data <- FLQuant(
      matrix(data = object$pl$logScale[(object$conf$keyParScaledYA + 1)],
             ncol = object$conf$noScaledYears,
             nrow = length(object$conf$minAge:object$conf$maxAge),
             byrow = TRUE),
      dimnames = list(year = object$conf$keyScaledYears,
                      age = object$conf$minAge:object$conf$maxAge))
    ### model values in log scale, exponentiate
    catch_mult_data <- exp(catch_mult_data)
    ### for simpler calculations, expand dimensions for stock
    catch_mult <- catch.n(stk) %=% 1
    catch_mult[ac(ages_mult), ac(yrs_mult)] <- catch_mult_data
    
    ### correct catch.n
    catch.n(stk) <- catch.n(stk) * catch_mult
    # ### split into landings and discards, based on landing fraction
    # ### done later
    # land_frac <- landings.n(stk) / catch.n(stk)
    # landings.n(stk) <- catch.n(stk) * land_frac
    # discards.n(stk) <- catch.n(stk) * (1 - land_frac)
    # ### update stock
    # catch(stk) <- computeCatch(stk)
    # landings(stk) <- computeLandings(stk)
    # discards(stk)<- computeDiscards(stk)
    
    # ### catch biomass
    # catch <- quantSums(qnt)
    # catch[, 1:n_yrs] <- exp(object$sdrep$value[names(object$sdrep$value) ==
    #                                              "logCatch"])
    # catch(stk) <- catch
    
  }
  
  ### total catch
  catch(stk) <- computeCatch(stk)
  
  ### catch range
  if (isTRUE(uncertainty) & isTRUE(catch_estimate)) {
    catch_sd <- quantSums(qnt)
    catch_sd[, 1:n_yrs] <- object$sdrep$sd[names(object$sdrep$value) == "logCatch"]
    attr(catch(stk), "low") <- exp(log(catch(stk)) - catch_sd * SD_mult)
    attr(catch(stk), "high") <- exp(log(catch(stk)) + catch_sd * SD_mult)
  }
  
  ### landings
  ### calculate with landings fraction of total catch
  if (isTRUE(length(dim(object$data$landFrac)) == 3)) {
    lfrac <- t(object$data$landFrac[,,, drop = TRUE])
  } else {
    lfrac <- t(object$data$landFrac)
  }
  n_ages <- dim(lfrac)[1]
  n_yrs <- dim(lfrac)[2]
  lfrac_qnt <- qnt ### FLQuant template
  lfrac_qnt[1:n_ages, 1:n_yrs] <- lfrac
  ### calculate landings numbers @ age
  landings.n(stk) <- catch.n(stk) * lfrac_qnt
  ### landings weights @ age
  if (isTRUE(length(dim(object$data$landMeanWeight)) == 3)) {
    landings.wt <- t(object$data$landMeanWeight[,,, drop = TRUE])
  } else {
    landings.wt <- t(object$data$landMeanWeight)
  }
  n_ages <- dim(landings.wt)[1]
  n_yrs <- dim(landings.wt)[2]
  landings.wt(stk)[1:n_ages, 1:n_yrs] <- landings.wt
  ### total landings
  landings(stk) <- computeLandings(stk)
  ### landings range
  if (isTRUE(uncertainty)) {
    ### landing fraction of total catch
    lfrac_qnt_sum <- landings(stk) / catch(stk)
    ### replace NAs with 1
    lfrac_qnt_sum[is.na(lfrac_qnt_sum)] <- 1
    ### calculate range
    ### assume that landing fraction is the same for estimate, low and high
    ### -> split catch into landings and discards
    attr(landings(stk), "low") <- attr(catch(stk), "low") * lfrac_qnt_sum
    attr(landings(stk), "high") <- attr(catch(stk), "high") * lfrac_qnt_sum
  }
  
  ### discards
  ### calculate discards number @ age
  discards.n(stk) <- catch.n(stk) * (1 - lfrac_qnt)
  ### discards weights @ age
  if (isTRUE(length(dim(object$data$disMeanWeight)) == 3)) {
    discards.wt <- t(object$data$disMeanWeight[,,, drop = TRUE])
  } else {
    discards.wt <- t(object$data$disMeanWeight)
  }
  n_ages <- dim(discards.wt)[1]
  n_yrs <- dim(discards.wt)[2]
  discards.wt(stk)[1:n_ages, 1:n_yrs] <- discards.wt
  ### total discards
  discards(stk) <- computeDiscards(stk)
  ### discard range
  if (isTRUE(uncertainty)) {
    ### calculate range
    ### assume that landing fraction is the same for estimate, low and high
    attr(discards(stk), "low") <- attr(catch(stk), "low") * (1 - lfrac_qnt_sum)
    attr(discards(stk), "high") <- attr(catch(stk), "high") * (1 - lfrac_qnt_sum)
  }
  
  ### natural mortality
  m <- t(object$data$natMor)
  n_ages <- dim(m)[1]
  n_yrs <- dim(m)[2]
  m(stk)[1:n_ages, 1:n_yrs] <- m
  
  ### maturity ogive
  mat <- t(object$data$propMat)
  n_ages <- dim(mat)[1]
  n_yrs <- dim(mat)[2]
  mat(stk)[1:n_ages, 1:n_yrs] <- mat
  
  ### proportion of F before spawning
  if (isTRUE(length(dim(object$data$propF)) == 3)) {
    harvest.spwn <- t(object$data$propF[,,, drop = TRUE])
  } else {
    harvest.spwn <- t(object$data$propF)
  }
  n_ages <- dim(harvest.spwn)[1]
  n_yrs <- dim(harvest.spwn)[2]
  harvest.spwn(stk)[1:n_ages, 1:n_yrs] <- harvest.spwn
  
  ### proportion of M before spawning
  m.spwn <- t(object$data$propM)
  n_ages <- dim(m.spwn)[1]
  n_yrs <- dim(m.spwn)[2]
  m.spwn(stk)[1:n_ages, 1:n_yrs] <- m.spwn
  
  ### set description
  desc(stk) <- "FLStock created from SAM model fit"
  
  ### set range
  ### plusgroup?
  range(stk)["plusgroup"] <- ifelse(isTRUE(object$conf$maxAgePlusGroup == 1),
                                    object$conf$maxAge, NA)
  ### fbar range
  range(stk)[c("minfbar", "maxfbar")] <- object$conf$fbarRange
  
  ### return FLStock
  return(stk)
  
}