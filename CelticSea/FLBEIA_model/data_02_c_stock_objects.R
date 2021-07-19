  ### stock object prep
  ### sanity check contents and sort issues
  
  # Set-up ####
  #install.packages(c("FLCore", "FLXSA","FLAssess", "FLash"), repos="http://flr-project.org/R")
  #, INSTALL_opts = "--no-multiarch")
  # devtools::install_github("fishfollower/SAM/stockassessment")
  
  rm(list=ls())
  gc()
  library(icesTAF)
  library(stockassessment) # Needed for SAM to FLR conversion
  library(FLCore)
  library(FLAssess)
  library(FLash)
  library(FLXSA)
  library(FLa4a)
  library(ggplot2)
  
  # Load other functions
    # Loading sam_to_FLStock function to Convert SAM to FLStock
    # Author: Simon Fisher
    sourceTAF("bootstrap/software/functions/sam_to_FLStock.R")
    # Loading QC_FLStock function to check the stock objects
    # Author: Paul Dolder
    sourceTAF("bootstrap/software/functions/QC_FLStock.R")
    
  
  
  
  # According ot the request sent to the stock assessors teh object should contain ----
  # Each fish stock must contain:
  # stock.n(x) # Numbers at age
  # harvest(x) # fishing moretality
  # catch.wt(x) # weights at age
  # landings.n(x) #landings numbers
  # discards.n(x) # discard numbers
  # range(stock) # units for f and plus group range(stock)[c("minfbar","maxfbar")]
  # Note fish stock obbjects go as far as asessment year -1, but NEP tsock objects go as far as assessment year!!!
  # But the other slots must be filled also
  
  # CS Tier 1 SPP:  cod, had and whg ----
  # ~ cod.27.7e-k ####
  load("bootstrap/data/submitted_stock_objects/WGCSE/cod.27.7e-k/Cod_7ek_WGCSE2020/run/model.RData")
  stock <- sam_to_FLStock(fit)
  plot(stock)
  
  stock@name <- "cod.27.7e-k"
  stock@desc <- "cod.27.7e-k, WGCSE, 2020"
  stock@landings.n # some missing values in landings.n Data were re uploaded back to 2004 only, so before discards are not known. During the benchmark we have decided to let SAM recompute the historical discards
  stock@catch.n
  stock@discards.n
  # The values in numbers <2004 are missing for age 1 and 2 because....
  # No need to compute anything as it will not effect total catch/landings/discards
  #units(stock@stock.n) <- "000"
  stock@range[['plusgroup']][] <- 7
  plot(stock@stock)
  plot(computeStock(stock))
  
  #forecast
  # load("bootstrap/data/submitted_stock_objects/WGCSE/cod.27.7e-k/Cod_7ek_WGCSE2020/run/forecast.RData")
  # forecast <- sam2flr(FC)
  
  # QC_FLStock(stock)
  
  #save out
  save(stock, file="results/clean_data/clean_stock_objects/cod.27.7e-k.RData")
  rm(fit, stock)
  
  # ~ whg.27.7b-ce-k ####
  load("bootstrap/data/submitted_stock_objects/WGCSE/whg.27.7b-ce-k/whg.7b-ce-k_WGCSE_II/run/model.RData")
  stock <- sam_to_FLStock(fit)
  plot(stock)
  
  stock@name <- "whg.27.7b-ce-k"
  stock@desc <- "whg.27.7b-ce-k, WGCSE, 2020"
  stock@range[['plusgroup']][] <- 7
  plot(computeHarvest(stock))
  plot(stock@harvest)
  
  plot(stock@stock)
  plot(computeStock(stock))
  
  plot(stock@catch)
  plot(computeCatch(stock))
  
  #forecast
  #load("../WGCSE/whg.27.7b-ce-k/whg.7b-ce-k_WGCSE_II/run/forecast.RData")
  #forcast <- sam2flr(FC)
  
  QC_FLStock(stock)
  
  #save out
  save(stock, file="results/clean_data/clean_stock_objects/whg.27.7b-ce-k.RData")
  rm(fit, stock)
  
  # ~ had.27.7b-k #####
  load("bootstrap/data/submitted_stock_objects/WGCSE/had.27.b-k/HAD7bk_2020_Assessment/run/model.RData")
  stock <- sam_to_FLStock(fit)
  plot(stock)
  
  stock@name <- "had.27.7b-k"
  stock@desc <- "had.27.7b-k, WGCSE, 2020"
  stock@range[['plusgroup']][] <- 8
  
  
  # I have no idea why harvest at age 0 is NA, I have messaged the stock coordinator.
  # For now I will just set to 0
  stock@harvest[1] <- 0 # I think this should be a value?
  
  plot(stock@harvest)
  plot(computeHarvest(stock))
  
  plot(stock@stock)
  plot(computeStock(stock))
  
  plot(stock@catch)
  plot(computeCatch(stock))
  
  #forecast
  #load("../WGCSE/had.27.b-k/HAD7bk_2020_Benchmark_II/run/forecast.RData")
  #forcast <- sam2flr(FC)
  
  # QC_FLStock(stock)
  
  #save out
  save(stock, file="results/clean_data/clean_stock_objects/had.27.7b-k.RData")
  rm(fit, stock)
  
  ## CS Tier 2 SPP:  hake, monkfish and megrim ----
  
  # ~ meg.27.7b-k8abd ####
  #Stock coordinator - Ane Iriondo <airiondo@azti.es>
  #  Recieved by email
  
  
  # Model outputs and stf
  load("bootstrap/data/submitted_stock_objects/WGBIE/meg.27.7b-k8abd/MEG_wg2020.RData")
  
  stock <- meg
  # units not set
  units(stock@harvest) <- "f"
  plot(stock)
  
  stock@name <- "meg.27.7b-k8abd"
  stock@desc <- "meg.27.7b-k8abd, WGBIE, 2020"
  
  slot.names <- slotNames(stock)
  slot.names <- slot.names[!slot.names %in% c("name", "desc", "range")]
  
  
  
  for(i in slot.names) {
    print(i)
    if(dim(slot(stock, i))[6] != 1) {
      slot(stock, i) <- apply(slot(stock, i), c(1,2), median)
    }
  }
  
  QC_FLStock(stock)
  
  #forcast
  # meg.stf
  #save out
  save(stock, file="results/clean_data/clean_stock_objects/meg.27.7b-k8abd.RData")
  rm(Fsq,i,meg,meg.stf,slot.names,stock)
  
  
  # ~ hke.27.3a46-8abd ####
  # Stock coordinator - Dorleta Garcia <dgarcia@azti.es>
  #   Recieved by email
  
  # Model outputs and stf
  load("bootstrap/data/submitted_stock_objects/WGBIE/hke.27.3a46-8abd/nhke_FLStock.RData")
  
  stock <- stk_norepl
  
  #stock slot not filled:
  stock@stock <- computeStock(stock)
  
  plot(stock)
  
  stock@name <- "hke.27.3a46-8abd"
  stock@desc <- "hke.27.3a46-8abd, WGBIE, 2020"
  
  #Warning messages:
  # 1: In QC_FLStock(stock) : The fishing mortality rates in @harvest do not match those from computeHarvest(FLStock),... is this expected? There appears to be a very slight difference. Need to ask stock coordinator is if this is normal
  plot(stock@harvest)
  plot(computeHarvest(stock))
  
  QC_FLStock(stock)
  
  save(stock, file="results/clean_data/clean_stock_objects/hke.27.3a46-8abd.RData")
  rm(stk,stk_norepl,stock)
  
  
  # ~  mon.27.78abd  ####
  # Model outputs and stf
  load("bootstrap/data/submitted_stock_objects/WGBIE/mon.27.78abd/mon78_a4a.Rdata")
  
  stock <- stock+fit1
  
  plot(stock)
  stock@discards.wt[is.na(stock@discards.wt)] <- 0
  stock@discards.n[is.na(stock@discards.n)] <- 0
  stock@discards <- computeDiscards(stock)
  
  stock@landings.wt[is.na(stock@landings.wt)] <- 0
  stock@landings.n[is.na(stock@landings.n)] <- 0
  stock@landings <- computeLandings(stock)
  
  stock@name <- "mon.27.78abd"
  stock@desc <- "mon.27.78abd, WGBIE, 2020"
  
  QC_FLStock(stock)
  
  save(stock, file="results/clean_data/clean_stock_objects/mon.27.78abd.RData")
  rm(fit1,stock,tun.sel)
  
  
  ## CS Tier 3 SPP: sole 7e, sole 7fg ----
  # ~ sol.27.7e ####
  # Model outputs and stf
  load("bootstrap/data/submitted_stock_objects/WGCSE/sol.27.7e/2020_sol.27.7e_assessment-master/model/stock.RData")
  stock <- stk_new
  plot(stock)
  stock@stock <- computeStock(stock)
  
  
  stock@name <- "sol.27.7e"
  stock@desc <- "sol.27.7e, WGCSE, 2020"
  # 1: In QC_FLStock(stock) : The fishing mortality rates in @harvest do not match those from computeHarvest(FLStock),... is this expected?
  plot(stock@harvest)
  plot(computeHarvest(stock))
  # 2: In QC_FLStock(stock) :the 'landings' slot does not match outputs of ComputeLandings(),... is this expected?
  plot(stock@landings)
  plot(computeLandings(stock))
  
  QC_FLStock(stock)
  
  save(stock, file="results/clean_data/clean_stock_objects/sol.27.7e.RData")
  rm(stk_new,stock)
  
  
  # ~ sol.27.7.fg ####
  # Model outputs and stf
  load("bootstrap/data/submitted_stock_objects/WGCSE/sol.27.7fg/WGCSE2020_Sol.27.7fg_Assessment/OUTPUT/SAM_fit_sol_7fg.RData")
  stock <- sam_to_FLStock(SAM_fit_sol_7fg)
  plot(stock)
  
  stock@name <- "sol.27.7fg"
  stock@desc <- "sol.27.7fg, WGCSE, 2020"
  plot(stock@stock)
  plot(computeStock(stock))
  
  
  plot(stock@catch)
  plot(computeCatch(stock))
  
  stock@range[['plusgroup']][] <- 10
  
  # QC_FLStock(stock)
  
  save(stock, file="results/clean_data/clean_stock_objects/sol.27.7fg.RData")
  rm(SAM_fit_sol_7fg,stock)
  
  
  ## CS Tier 4 SPP: nep.fu.16, nep.fu.17, nep.fu.19, nep.fu.2021, nep.fu.22, nep.out.fu ----
  
  # Used North Sea FU6 as example to mimic the stock objects
  # First we work up the .csv file for all nep.all
  nep.wide <- read.csv("bootstrap/data/submitted_stock_objects/WGCSE/nep.all/nep.stock.wgmixfish_2020.csv")
  # nep.wide <- read.csv("bootstrap/data/submitted_stock_objects/WGCSE/nep.all/nep.stock.wgmixfish_2019.csv")
  nep.all <- nep.wide
  nep.all$harvest.rate <- nep.all$harvest.rate/100 # hr is coming as percentage
  
  # a fix for building the fleet object
  #nep.all$harvest.rate[is.na(nep.all$harvest.rate)]<-0.00000000000000001
  
  # Change names to match slots:
  names(nep.all) <- c("fu", "year", "stock.n", "ci", "landings.n", "discards.n", "removals.n",
                      "harvest", "landings", "discards", "discard.rate.n",
                      "dead.disc.rate.n", "landings.wt", "discards.wt", "discard.rate.wgt",
                      "prop.removal.ret.n", "survival.rate")
  
  # Create new slots for catch (mimic calculations as in FU6)
  nep.all$discards_0 <- nep.all$discards # to avoid NAs in the catch when we sum in next line
  nep.all$discards_0[is.na(nep.all$discards_0)] <- 0
  
  # LP 20/10/20: Why do we have landings with NA ????
  nep.all$landings_0 <- nep.all$landings
  nep.all$landings_0[is.na(nep.all$landings_0)] <- 0
  nep.all$catch <- nep.all$landings_0 + nep.all$discards_0 # Catch = Landings + Dead & Surviving discards
  # nep.all$catch <- nep.all$landings_0 + (nep.all$discards_0 * (1 - nep.all$survival.rate)) # NO Landings + Dead discards (Removals in tonnes)
  
  nep.all$discards.n_0 <- nep.all$discards.n # to avoid NAs in the catch when we sum in next line
  nep.all$discards.n_0[is.na(nep.all$discards.n_0)] <- 0
  
  nep.all$landings.n_0 <- nep.all$landings.n # to avoid NAs in the catch when we sum in next line
  nep.all$landings.n_0[is.na(nep.all$landings.n_0)] <- 0
  
  nep.all$catch.n <- nep.all$landings.n_0 + nep.all$discards.n_0 # Catch = Landings + Dead & Surviving discards
  # NO Check that catch.n are Removals in numbers  = Landings + Dead discards:
  # nep.all$catch.n - (nep.all$landings.n + (nep.all$discards_0 * (1 - nep.all$survival.rate)) # NO
  
  nep.all$discards.wt_0 <- nep.all$discards.wt # to avoid NAs in the catch when we sum in next line
  nep.all$discards.wt_0[is.na(nep.all$discards.wt_0)] <- 0
  
  nep.all$landings.wt_0 <- nep.all$landings.wt # to avoid NAs in the catch when we sum in next line
  nep.all$landings.wt_0[is.na(nep.all$landings.wt_0)] <- 0
  nep.all$catch.wt <- with(nep.all, (landings.wt_0*landings.n/(landings.n_0+discards.n_0)) + (discards.wt_0*discards.n_0/(landings.n_0+discards.n_0)))
  
  # LP
  # lw<- with(nep.all, (landings.wt_0*landings.n_0/(landings.n_0+discards.n_0)))
  # dw<- with(nep.all, (discards.wt_0*discards.n_0/(landings.n_0+discards.n_0)))
  # lw[is.na(lw)]<-0
  # dw[is.na(dw)]<-0
  # nep.all$catch.wt<-lw+dw
  
  # nep.all$catch.wt <- with(nep.all, (landings.wt*landings/(landings+discards_0)) + (discards.wt_0*discards_0/(landings+discards_0)))
  # nep.all$catch.wt2 <- with(nep.all, (landings.wt*landings.n/(removals.)) + (discards.wt_0*discards.n_0/(landings.n+discards.n_0)))
  # nep.all$catch.wt <- with(nep.all, (landings.wt*landings/(landings+discards_0)) + (discards.wt_0*discards_0/(landings+discards_0)))
  
  
  # Special case for FU16, where the stock assessment uses present year's landings mean weights. NOT IN 2020
  # So here I copy the landings.wt to catch.wt (only for FU16, when catch.wt is NA and landings.wt is not NA)
  # nep.all[nep.all$fu == "fu.16" & !is.na(nep.all$landings.wt) & is.na(nep.all$catch.wt),]$catch.wt <- nep.all[nep.all$fu == "fu.16" & !is.na(nep.all$landings.wt) & is.na(nep.all$catch.wt),]$landings.wt
  
  # Dead discard rate and Survival rates will be setup when running the script: model_01_Reproduce_the_advice_Celtic_Sea_2019.Rmd
  # Save them in data folder for later
  nep.dead.disc.surv.rate <- cbind(nep.all[,c("fu", "year", "discard.rate.n", "discard.rate.wgt", "dead.disc.rate.n", "survival.rate")])
  write.csv(nep.dead.disc.surv.rate, "bootstrap/data/submitted_stock_objects/WGCSE/nep.all/other.dicard.rates.csv", row.names = F)
  
  
  # Select columns for stock object
  nep.all <- nep.all[,c("fu", "year", "stock.n", "landings.n", "discards.n", "catch.n", "harvest", "landings",
                        "discards", "landings.wt", "discards.wt", "catch", "catch.wt")]
  
  # From wide to long
  nep.all.melt <- reshape2::melt(nep.all, id.vars = c("fu", "year"))
  names(nep.all.melt) <- c("fu", "year", "slot", "data")
  # age column:
  nep.all.melt$age <- 1
  # units column:
  nep.all.melt$units <- nep.all.melt$slot
  levels(nep.all.melt$units)
  levels(nep.all.melt$units) <- list(millions = c("stock.n", "landings.n", "discards.n", "catch.n"),
                                     tonnes = c("stock", "landings", "discards", "catch"),
                                     g = c("stock.wt", "landings.wt", "discards.wt", "catch.wt"),
                                     hr = "harvest")
  levels(nep.all.melt$units)
  
  
  # ~ nep.fu.16 ----
  # Create the stock object from .csv file
  FU <- "fu.16"
  nep.data <- subset(nep.all.melt, fu == FU) # subset for FU
  nep.data <- nep.data[,names(nep.data) != "fu"] # remove fu column
  
  stock <- as.FLStock(nep.data)
  
  stock@name <- paste0("nep.", FU)
  stock@desc <- paste0(stock@name, " WGCSE, 2020")
  
  # LP: another fix for NAs
  # stock@discards[is.na(stock@discards)]<-0
  # stock@discards.n[is.na(stock@discards.n)]<-0
  # stock@discards.wt[is.na(stock@discards.wt)]<-0
  # stock@landings[is.na(stock@landings)]<-0
  # stock@landings.n[is.na(stock@landings.n)]<-0
  # stock@landings.wt[is.na(stock@discards.wt)]<-0
  
  # Just checks to remind how are some things calculated:
  discards(stock) # Dead + surving discards
  discards.n(stock) # Dead + surving discards
  catch(stock) == landings(stock) + discards(stock) # Landings + Dead & Surviving discards
  # catch(stock) == landings(stock) + (discards(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate)) # NO Landings + Dead discards
  catch.n(stock) == landings.n(stock) + discards.n(stock) # Landings + Dead & Surviving discards
  # catch.n(stock) - (landings.n(stock) + (discards.n(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate))) # NO Landings + Dead discards
  catch.wt(stock) - ((landings.wt(stock) * landings.n(stock)/(landings.n(stock)+discards.n(stock))) + (discards.wt(stock) * discards.n(stock)/(landings.n(stock)+discards.n(stock))))
  
  save(stock, file=paste0("results/clean_data/clean_stock_objects/", stock@name, ".RData"))
  rm(stock, nep.data)
  
  
  # ~ nep.fu.17 ----
  # Create the stock object from .csv file
  FU <- "fu.17"
  nep.data <- subset(nep.all.melt, fu == FU) # subset for FU
  nep.data <- nep.data[,names(nep.data) != "fu"] # remove fu column
  
  stock <- as.FLStock(nep.data)
  
  stock@name <- paste0("nep.", FU)
  stock@desc <- paste0(stock@name, " WGCSE, 2020")
  
  # LP: another fix for NAs
  # stock@discards[is.na(stock@discards)]<-0
  # stock@discards.n[is.na(stock@discards.n)]<-0
  # stock@discards.wt[is.na(stock@discards.wt)]<-0
  # stock@landings[is.na(stock@landings)]<-0
  # stock@landings.n[is.na(stock@landings.n)]<-0
  # stock@landings.wt[is.na(stock@discards.wt)]<-0
  
  # Just checks to remind how are some things calculated:
  discards(stock) # Dead + surving discards
  discards.n(stock) # Dead + surving discards
  catch(stock) == landings(stock) + discards(stock) # Landings + Dead & Surviving discards
  # catch(stock) == landings(stock) + (discards(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate)) # NO Landings + Dead discards
  catch.n(stock) == landings.n(stock) + discards.n(stock) # Landings + Dead & Surviving discards
  # catch.n(stock) - (landings.n(stock) + (discards.n(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate))) # NO Landings + Dead discards
  catch.wt(stock) - ((landings.wt(stock) * landings.n(stock)/(landings.n(stock)+discards.n(stock))) + (discards.wt(stock) * discards.n(stock)/(landings.n(stock)+discards.n(stock))))
  
  save(stock, file=paste0("results/clean_data/clean_stock_objects/", stock@name, ".RData"))
  rm(stock, nep.data)
  
  
  # ~ nep.fu.19 ----
  # Create the stock object from .csv file
  FU <- "fu.19"
  nep.data <- subset(nep.all.melt, fu == FU) # subset for FU
  nep.data <- nep.data[,names(nep.data) != "fu"] # remove fu column
  
  stock <- as.FLStock(nep.data)
  
  stock@name <- paste0("nep.", FU)
  stock@desc <- paste0(stock@name, " WGCSE, 2020")
  
  # LP: another fix for NAs
  # stock@discards[is.na(stock@discards)]<-0
  # stock@discards.n[is.na(stock@discards.n)]<-0
  # stock@discards.wt[is.na(stock@discards.wt)]<-0
  # stock@landings[is.na(stock@landings)]<-0
  # stock@landings.n[is.na(stock@landings.n)]<-0
  # stock@landings.wt[is.na(stock@discards.wt)]<-0
  
  # Just checks to remind how are some things calculated:
  discards(stock) # Dead + surving discards
  discards.n(stock) # Dead + surving discards
  catch(stock) == landings(stock) + discards(stock) # Landings + Dead & Surviving discards
  # catch(stock) == landings(stock) + (discards(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate)) # NO Landings + Dead discards
  catch.n(stock) == landings.n(stock) + discards.n(stock) # Landings + Dead & Surviving discards
  # catch.n(stock) - (landings.n(stock) + (discards.n(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate))) # NO Landings + Dead discards
  catch.wt(stock) - ((landings.wt(stock) * landings.n(stock)/(landings.n(stock)+discards.n(stock))) + (discards.wt(stock) * discards.n(stock)/(landings.n(stock)+discards.n(stock))))
  
  save(stock, file=paste0("results/clean_data/clean_stock_objects/", stock@name, ".RData"))
  rm(stock, nep.data)
  
  
  # ~ nep.fu.2021 ----
  # Create the stock object from .csv file
  FU <- "fu.2021"
  nep.data <- subset(nep.all.melt, fu == FU) # subset for FU
  nep.data <- nep.data[,names(nep.data) != "fu"] # remove fu column
  
  stock <- as.FLStock(nep.data)
  
  stock@name <- paste0("nep.", FU)
  stock@desc <- paste0(stock@name, " WGCSE, 2020")
  
  # LP: another fix for NAs
  # stock@discards[is.na(stock@discards)]<-0
  # stock@discards.n[is.na(stock@discards.n)]<-0
  # stock@discards.wt[is.na(stock@discards.wt)]<-0
  # stock@landings[is.na(stock@landings)]<-0
  # stock@landings.n[is.na(stock@landings.n)]<-0
  # stock@landings.wt[is.na(stock@discards.wt)]<-0
  
  # Just checks to remind how are some things calculated:
  discards(stock) # Dead + surving discards
  discards.n(stock) # Dead + surving discards
  catch(stock) == landings(stock) + discards(stock) # Landings + Dead & Surviving discards
  # catch(stock) == landings(stock) + (discards(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate)) # NO Landings + Dead discards
  catch.n(stock) == landings.n(stock) + discards.n(stock) # Landings + Dead & Surviving discards
  # catch.n(stock) - (landings.n(stock) + (discards.n(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate))) # NO Landings + Dead discards
  catch.wt(stock) - ((landings.wt(stock) * landings.n(stock)/(landings.n(stock)+discards.n(stock))) + (discards.wt(stock) * discards.n(stock)/(landings.n(stock)+discards.n(stock))))
  
  save(stock, file=paste0("results/clean_data/clean_stock_objects/", stock@name, ".RData"))
  rm(stock, nep.data)
  
  
  # ~ nep.fu.22 ----
  # Create the stock object from .csv file
  FU <- "fu.22"
  nep.data <- subset(nep.all.melt, fu == FU) # subset for FU
  nep.data <- nep.data[,names(nep.data) != "fu"] # remove fu column
  
  stock <- as.FLStock(nep.data)
  
  stock@name <- paste0("nep.", FU)
  stock@desc <- paste0(stock@name, " WGCSE, 2020")
  
  # LP: another fix for NAs
  # stock@discards[is.na(stock@discards)]<-0
  # stock@discards.n[is.na(stock@discards.n)]<-0
  # stock@discards.wt[is.na(stock@discards.wt)]<-0
  # stock@landings[is.na(stock@landings)]<-0
  # stock@landings.n[is.na(stock@landings.n)]<-0
  # stock@landings.wt[is.na(stock@discards.wt)]<-0
  
  # Just checks to remind how are some things calculated:
  discards(stock) # Dead + surving discards
  discards.n(stock) # Dead + surving discards
  catch(stock) == landings(stock) + discards(stock) # Landings + Dead & Surviving discards
  # catch(stock) == landings(stock) + (discards(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate)) # NO Landings + Dead discards
  catch.n(stock) == landings.n(stock) + discards.n(stock) # Landings + Dead & Surviving discards
  # catch.n(stock) - (landings.n(stock) + (discards.n(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate))) # NO Landings + Dead discards
  catch.wt(stock) - ((landings.wt(stock) * landings.n(stock)/(landings.n(stock)+discards.n(stock))) + (discards.wt(stock) * discards.n(stock)/(landings.n(stock)+discards.n(stock))))
  
  save(stock, file=paste0("results/clean_data/clean_stock_objects/", stock@name, ".RData"))
  rm(stock, nep.data)
  
  
  # ~ nep.out.fu ----
  # Create the stock object from .csv file
  FU <- "out.7"
  nep.data <- subset(nep.all.melt, fu == "out7.fu") # subset for FU
  nep.data <- nep.data[,names(nep.data) != "fu"] # remove fu column
  
  stock <- as.FLStock(nep.data)
  
  stock@name <- paste0("nep.", FU)
  stock@desc <- paste0(stock@name, " WGCSE, 2020")
  
  
  # LP: another fix for NAs
  stock@discards[is.na(stock@discards)]<-0
  stock@discards.n[is.na(stock@discards.n)]<-0
  stock@discards.wt[is.na(stock@discards.wt)]<-0
  stock@landings[is.na(stock@landings)]<-0
  stock@landings.n[is.na(stock@landings.n)]<-0
  stock@landings.wt[is.na(stock@discards.wt)]<-0
  
  # Just checks to remind how are some things calculated:
  discards(stock) # Dead + surviving discards
  discards.n(stock) # Dead + surviving discards
  catch(stock) == landings(stock) + discards(stock) # Landings + Dead & Surviving discards
  # catch(stock) == landings(stock) + (discards(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate)) # NO Landings + Dead discards
  catch.n(stock) == landings.n(stock) + discards.n(stock) # Landings + Dead & Surviving discards
  # catch.n(stock) - (landings.n(stock) + (discards.n(stock) * (1-subset(nep.dead.disc.surv.rate, fu == FU)$survival.rate))) # NO Landings + Dead discards
  catch.wt(stock) - ((landings.wt(stock) * landings.n(stock)/(landings.n(stock)+discards.n(stock))) + (discards.wt(stock) * discards.n(stock)/(landings.n(stock)+discards.n(stock))))
  
  save(stock, file=paste0("results/clean_data/clean_stock_objects/", stock@name, ".RData"))
  rm(stock, nep.data)
  
