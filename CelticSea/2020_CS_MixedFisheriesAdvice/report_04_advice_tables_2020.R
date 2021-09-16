### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning

rm(list=ls())
gc(); graphics.off()



load(file.path('results',"01_Reproduce_the_advice_Celtic_Sea_2020tier12nepnewLOUsingSAM.Rdata"))
res.path <- "results/"

res<-read.csv(file=file.path(res.path,"BigTable.csv"))
names(res) <- c("value", "year",  "sc" , stock.names)

# Read single species advice from here to make everything easier ------------------------------------
  single_advice <- read.csv("bootstrap/data/supporting_files/single_species_advice.csv")
            
  #*update annually*#
  year_advice<-2021
  
  ## Catch in advice year
  ICES_advice<-NULL
  for (i in stock.names){ICES_advice[[i]] <- subset(single_advice, year == year_advice & stock == i)$catch}
  
  ## SSB in 2022 from the advice
  ICES_ssb_yplusone<-NULL
  ssb.stock.names <- stock.names[stock.names %in% c('cod.27.7e-k', 'whg.27.7b-ce-k', 'meg.27.7b-k8abd', 'mon.27.78abd', 'sol.27.7fg', 'had.27.7b-k')]
  for (i in ssb.stock.names){ICES_ssb_yplusone[[i]] <- subset(single_advice, year == year_advice+1 & stock == i)$ssb}
  
  ## Fishing mortality 2021
  fishing_mort<-NULL
  for (i in stock.names){fishing_mort[[i]] <- subset(single_advice, year == year_advice & stock == i)$Fbar}

  
# summ abs section --------------------------------------------------------

summ_abs<-NULL

# summ_abs loop for all stocks

for (i in 1:length(ICES_advice)) {
  
  summ_abs <- rbind(summ_abs,
                    data.frame(Stock = names(ICES_advice[i]),
                               Single_Stock_Advice = ICES_advice[[i]],
                               MAX_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="max", names(ICES_advice[i])],
                               MIN_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="min", names(ICES_advice[i])],
                               sqE_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="sq_E", names(ICES_advice[i])],
                               COD_fmsy_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="cod_fmsy",names(ICES_advice[i])],
                               # MR line added
                               val = res[res$value=="landings"&res$year==year_advice&res$sc=="val", names(ICES_advice[i])],
                               # COD_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="cod.27.7e-k",names(ICES_advice[i])],
                               HAD_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="had.27.7b-k", names(ICES_advice[i])],
                               WHG_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="whg.27.7b-ce-k", names(ICES_advice[i])]))#,
  # MEG_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="meg.27.7b-k8abd"],
  # MON_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="mon.27.78abd"],
  # SOL_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="sol.27.7fg"]))#,
  #  optimQ_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="optimQ"]))

}


# summ_rel section --------------------------------------------------------

summ_rel<-NULL

# summ_rel loop for all stocks

for (i in 1:length(ICES_advice)) {
  
  summ_rel <- rbind(summ_rel,
                    data.frame(Stock = names(ICES_advice[i]),
                               Single_Stock_Advice = ICES_advice[[i]],
                               MAX_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="max", names(ICES_advice[i])]/ICES_advice[[i]],
                               MIN_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="min", names(ICES_advice[i])]/ICES_advice[[i]],
                               sqE_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="sq_E", names(ICES_advice[i])]/ICES_advice[[i]],
                               COD_fmsy_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="cod_fmsy", names(ICES_advice[i])]/ICES_advice[[i]],
                               # MR line added
                               val = res[res$value=="landings"&res$year==year_advice&res$sc=="val", names(ICES_advice[i])]/ICES_advice[[i]],
                               # COD_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="cod.27.7e-k",names(ICES_advice[i])]/ICES_advice[[i]],
                               HAD_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="had.27.7b-k", names(ICES_advice[i])]/ICES_advice[[i]],
                               WHG_sc = res[res$value=="landings"&res$year==year_advice&res$sc=="whg.27.7b-ce-k", names(ICES_advice[i])]/ICES_advice[[i]]))#,
  # MEG_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="meg.27.7b-k8abd"]/ICES_advice[[i]],
  # MON_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="mon.27.78abd"]/ICES_advice[[i]],
  # SOL_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="sol.27.7fg"]/ICES_advice[[i]]))#,
  #  optimQ_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="optimQ"]))
  
}

  
# SSB section -------------------------------------------------------------

ssb <-NULL

# ssb loop for all stocks

for (i in 1:length(ICES_ssb_yplusone)) {
  
  ssb <- rbind(ssb,
               data.frame(Stock = names(ICES_ssb_yplusone[i]),
                          SSB_2022 = as.numeric(ICES_ssb_yplusone[names(ICES_ssb_yplusone) == names(ICES_ssb_yplusone)[i]]),
                          MAX_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="max", names(ICES_advice[i])],
                          MIN_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="min", names(ICES_advice[i])],
                          sqE_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="sq_E", names(ICES_advice[i])],
                          COD_fmsy_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="cod_fmsy", names(ICES_advice[i])],
                          # MR line added
                          val = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="val",names(ICES_advice[i])],
                          # COD_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="cod.27.7e-k",names(ICES_advice[i])],
                          HAD_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="had.27.7b-k", names(ICES_advice[i])],
                          WHG_sc = res[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="whg.27.7b-ce-k", names(ICES_advice[i])]))#,
  # MEG_sc = res$'cod.27.7e-k'[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="meg.27.7b-k8abd"],
  # MON_sc = res$'cod.27.7e-k'[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="mon.27.78abd"],
  # SOL_sc = res$'cod.27.7e-k'[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="sol.27.7fg"]))#,
#optimQ_sc = res$'cod.27.7e-k'[res$value=="ssb"&(res$year==year_advice+1)&res$sc=="optimQ"]))
  
}


#  fishing mort section ---------------------------------------------------

summ_f<-NULL

# summ_f loop for all stocks

for (i in 1:length(fishing_mort)) {
  
  summ_f <- rbind(summ_f,
                  data.frame(Stock = names(fishing_mort[i]),
                             Single_Stock_Advice = fishing_mort[[i]],
                             MAX_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="max", names(ICES_advice[i])],
                             MIN_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="min", names(ICES_advice[i])],
                             sqE_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="sq_E", names(ICES_advice[i])],
                             COD_fmsy_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="cod_fmsy", names(ICES_advice[i])],
                             # MR line added
                             val = res[res$value=="Fbar"&res$year==year_advice&res$sc=="val", names(ICES_advice[i])],
                             # COD_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="cod.27.7e-k",names(ICES_advice[i])],
                             HAD_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="had.27.7b-k", names(ICES_advice[i])],
                             WHG_sc = res[res$value=="Fbar"&res$year==year_advice&res$sc=="whg.27.7b-ce-k", names(ICES_advice[i])]))#,
  # MEG_sc = res$'cod.27.7e-k'[res$value=="Fbar"&res$year==year_advice&res$sc=="meg.27.7b-k8abd"],
  # MON_sc = res$'cod.27.7e-k'[res$value=="Fbar"&res$year==year_advice&res$sc=="mon.27.78abd"],
  # SOL_sc = res$'cod.27.7e-k'[res$value=="Fbar"&res$year==year_advice&res$sc=="sol.27.7fg"]))#,
  #  optimQ_sc = res$'cod.27.7e-k'[res$value=="landings"&res$year==year_advice&res$sc=="optimQ"]))
  
}

  
# Write the four tables -----------------------------------------------------------------
write.csv(ssb,file = file.path (res.path,"SSB advice table.csv"))
write.csv(summ_rel,file = file.path (res.path,"relative advice table.csv"))
write.csv(summ_abs,file = file.path (res.path,"absolute advice table.csv"))
write.csv(summ_f,file = file.path (res.path,"f advice table.csv"))
