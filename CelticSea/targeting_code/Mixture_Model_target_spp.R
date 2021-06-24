################################################################
##
## Mixture model - identify if a species is a target from logbook data
##
## A test of methods - no validation data available...
## though maybe could use the FDF or EFF data?
##
## 28/05/2020
##
################################################################

library(tidyverse)
library(mixtools)
library(rstan)
#########
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
#############

load("IFISH_2018.RData")

#########

spp <- c("COD","HAD", "WHG", "PLE", "POK", "HKE", "NEP", "SOL", "ANF")

areas <- unique(grep("27.7", IFISH$FAO_FISHING_AREA_CODE, value = TRUE))

## By fishery  
dat <- filter(IFISH, FAO_FISHING_AREA_CODE %in% areas, 
              GEAR_CODE == "OTB", SPECIES_CODE %in% spp)

# select and reformat
dat <- dat %>% select(ACTIVITY_DATE, VOYAGE_ID, RSS_NO, FAO_FISHING_AREA_CODE, SPECIES_CODE, LANDED_WEIGHT) %>% 
  reshape2::dcast(ACTIVITY_DATE + RSS_NO + VOYAGE_ID + FAO_FISHING_AREA_CODE ~ SPECIES_CODE, value.var = "LANDED_WEIGHT", 
                  fun.aggregate = sum)

par(mfrow=c(3,3))
for(i in spp) {
hist(log(dat[,i]), main = i)
}

res <- lapply(spp, function(x) { normalmixEM(log(dat[,x][dat[,x] != 0]), lambda = 0.05, mu = c(2,5), sigma = 5, k = 3) })
names(res) <- spp

lapply(res, summary)

par(mfrow=c(3,3))
lapply(spp, function(i) { plot(res[[i]], whichplots = 2, density = TRUE, main2 = i) })

## the mean landed value non and targeted
lapply(spp, function(i) {exp(res[[i]]$mu)})

# bootstrapped estimates of the mean landings for each component 
#targ_boot <- boot.se(targ, B = 100)
#rbind(range(exp(targ_boot$mu[1,])), range(exp(targ_boot$mu[2,])))

## Add back on to the data
#dat$target <- ifelse(targ$posterior[,2] > 0.95, TRUE, FALSE)

dat$pCOD <- ifelse(dat$COD > 0, res[["COD"]]$posterior[,2], 0)
dat$pHAD <- ifelse(dat$HAD > 0, res[["HAD"]]$posterior[,2], 0)
dat$pWHG <- ifelse(dat$WHG > 0, res[["WHG"]]$posterior[,2], 0)
dat$pPLE <- ifelse(dat$PLE > 0, res[["PLE"]]$posterior[,2], 0)
dat$pPOK <- ifelse(dat$POK > 0, res[["POK"]]$posterior[,2], 0)
dat$pHKE <- ifelse(dat$HKE > 0, res[["HKE"]]$posterior[,2], 0)
dat$pNEP <- ifelse(dat$NEP > 0, res[["NEP"]]$posterior[,2], 0)
dat$pSOL <- ifelse(dat$SOL > 0, res[["SOL"]]$posterior[,2], 0)
dat$pANF <- ifelse(dat$ANF > 0, res[["ANF"]]$posterior[,2], 0)

## Can we summarise?

# For a single trip
n <- 34 

par(mfrow=c(3,3))
for(i in spp) {
plot(res[[i]], whichplots = 2, main2 = i)
  points(log(dat[n,i]), 0.1, pch = 4, col = "blue", cex = 2)
  text(0, 0.1, label = paste("targ prob = ",round(dat[n,paste0("p",i)],2)))
  }


## Bayesian method 
## Just implement a Stan version ??


cod_data <- list(N = length(dat$COD[dat$COD>0]), y = log(dat$COD[dat$COD >0]),
                 K  = 2)

cod_fit <- stan(file = "mixture_model.stan", data = cod_data, iter = 1000, chains = 4)

plot(cod_fit)
pairs(cod_fit)

hist(log(dat$COD[dat$COD>0]), prob = TRUE, ylim = c(0,0.45))

data_range <- seq(min(log(dat$COD[dat$COD>0])), 
                  max(log(dat$COD[dat$COD>0])), 0.001)

lines(data_range, dnorm(data_range, 
      summary(cod_fit)$summary[3,"mean"], 
      summary(cod_fit)$summary[5,"mean"]), col = "red")

lines(data_range, dnorm(data_range, 
      summary(cod_fit)$summary[4,"mean"], 
      summary(cod_fit)$summary[6,"mean"]), col = "green")


mu     <- extract(cod_fit,"mu")$mu
sigma  <- extract(cod_fit, "sigma")$sigma


## Work out how to scale the density properly!!

hist(log(dat$COD[dat$COD>0]), prob = TRUE, ylim = c(0,0.45))
for(i in 1:nrow(mu)) {
 lines(data_range, dnorm(data_range, 
      mu[i,1], 
      sigma[i,1]), col = "red")
 lines(data_range, dnorm(data_range, 
      mu[i,2], 
      sigma[i,2]), col = "green")
}

