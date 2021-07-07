#' ---
#' title: "Celtic Sea mixed fisheries 2021 - tables and figures"
#' author: "MIXFISH Celtic Sea subgroup"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#' 
#' Summary
#' =======
#' 
#' This script generates the figures and tables (together with the associated captions) required by the mixed fisheries advice and reporting documents for the Celtic seas ecoregion sub-group.
#' 
#' Load required packages
#' ======================
#' 

library(tidyverse)

## Load functions
source("funcs/figures_tables_functions.R")

#' Read and process data
#' =====================
 
## Load csv outputs from FLBEIA
summary_fleet      <- read.csv("1_Data/00_fleet_summary.csv")
summary_fleetstock <- read.csv("1_Data/01_fleet_stock_summary.csv")
summary_metier     <- read.csv("1_Data/02_metier_summary.csv")
summary_metierstock<- read.csv("1_Data/03_metier_stock_summary.csv")
summary_biological <- read.csv("1_Data/04_biological_summary.csv")
summary_advice     <- read.csv("1_Data/05_advice_summary.csv")

## Load csv single-stock advice
summary_ssa <- read.csv("1_Data/single_species_advice_GA.csv")

## Define assessment year
ay <- 2021

## Define stocks
stockVector <- c("cod.27.7e-k", "had.27.7b-k", "whg.27.7b-ce-k", "hke.27.3a46-8abd", 
                 "meg.27.7b-k8abd", "mon.27.78abd", "sol.27.7e", "sol.27.7fg") ## FOR TESTING!!! 
stockLabels <- stockVector

## Define plotting colours
pal_all_region <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                    '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')

#' Generate figures
#' ----------------
#' 
#' ### Mixed fisheries projections by stock and scenario
png("figures/figProj.png", width = 7, height = 6, units = "in", res = 400)
underOverPlot(inclYear  = ay+1, 
              inclStock = stockVector, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice %>% filter(scenario %in% c("min","max")), 
              stockLabels =  paste0(1:length(stockLabels),":",stockLabels),
              pal = pal_all_region[1:length(stockLabels)])
dev.off()

png("figures/figProj_gg.png", width = 7, height = 6, units = "in", res = 400)
ggunderOverPlot(inclYear  = ay+1, 
              inclStock = stockVector, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice %>% filter(scenario %in% c("min","max")), 
              stockLabels =  paste0(1:length(stockLabels),":",stockLabels),
              pal = pal_all_region[1:length(stockLabels)])
dev.off()

#' ### Mixed fisheries projections for a subset of stocks by scenario
#' 
#' NOTE: I'm automatically selecting 3 stock with the lowest catches according to the "min" scenario

stockVectorSubset <- summary_advice %>%
  filter(year == ay + 1) %>%
  filter(catch > 0) %>%
  select(stock, scenario, catch) %>%
  spread(scenario, catch) %>%
  arrange(max) %>%
  head(3) %>%
  select(stock) %>%
  unlist()

colVectorSubset   <- pal_all_region[which(stockVector %in% stockVectorSubset)]

png("figures/figProjLow.png", width = 7, height = 6, units = "in", res = 400)
underOverPlot(inclYear  = ay+1, 
              inclStock = stockVectorSubset, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice %>% filter(scenario %in% c("min","max")), 
              stockLabels =  paste0(1:length(stockLabels),":",stockLabels),
              pal = colVectorSubset)
dev.off()

png("figures/figProjLow_gg.png", width = 7, height = 6, units = "in", res = 400)
ggunderOverPlot(inclYear  = ay+1, 
              inclStock = stockVectorSubset, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice %>% filter(scenario %in% c("min","max")), 
              stockLabels =  paste0(1:length(stockLabels),":",stockLabels),
              pal = colVectorSubset)
dev.off()

#' ### Mixed fisheries estimates of effort needed to reach single stock advice (bar plot) 

png("figures/figMixFishBar.png", width = 7, height = 8, units = "in", res = 300)
plotEffortLim(ay = ay,
              fleetObj    = summary_fleet,
              fleetStkObj = summary_fleetstock,
              ssaObj      = summary_ssa)
dev.off()

#' ### Estimated SSB expressed as ratio to single-stock advice forecast (bar plot) 

png("figures/figMixFishSSB.png", width = 7, height = 8, units = "in", res = 300)
plotScenSSB(ay = ay, 
            bioObj = summary_biological, 
            ssaObj = summary_ssa,
            inclStock   = NULL,
            inclScen = scenarioVector,
            stockLabels =  paste0(1:length(stockLabels),":",stockLabels),
            pal         = pal_all_region[1:length(stockLabels)])
dev.off()

#' ### Landings distribution of species by metier

png("figures/figMetierLandings.png", width = 9, height = 7, units = "in", res = 300)
plotMetierLandings(ay = ay, 
            metObj = summary_metierstock, 
            stockLabels = paste0(1:length(stockLabels),":",stockLabels),
            pal         = pal_all_region[1:length(stockLabels)])
dev.off()

#' ### Total landings by stock

png("figures/figPieLandings.png", width = 7, height = 4, units = "in", res = 300)
plotPieLandings(ay = ay,
                bioObj = summary_biological,
                stockLabels = paste0(1:length(stockLabels),":",stockLabels))
dev.off()

#' Generate tables
#' ---------------
#' 
#' ### SSB results from single-stock advice and different mixed-fisheries scenarios

tabMixFishSSB <- tableSSB(ay = ay,
                          inclScen = scenarioVector,
                          bioObj = summary_biological, 
                          ssaObj = summary_ssa)
write.csv(tabMixFishSSB, 
          "tables/tabMixFishSSB.csv", 
          row.names = FALSE)

#' ### Catch per mixed-fisheries scenario in absolute values.

tabMixFishCatch <- tableCatch(ay = ay, 
                              inclScen = scenarioVector,
                              resObj = summary_advice, 
                              ssaObj = summary_ssa)
write.csv(tabMixFishCatch, 
          "tables/tabMixFishCatch.csv", 
          row.names = FALSE)

#' ### Ftotal (Fbar) resulting from single-stock advice and different mixed-fisheries scenarios

tabMixFishFbar <- tableFbar(ay = ay,
                            inclScen = scenarioVector,
                            bioObj = summary_biological,
                            ssaObj = summary_ssa)
write.csv(tabMixFishFbar, 
          "tables/tabMixFishFbar.csv", 
          row.names = FALSE)

#' ### Relative Catch in assessment year

tabRelCatch <- tableRelCatch(ay = ay,
                             inclScen = scenarioVector,
                             resObj = summary_advice,
                             ssaObj = summary_ssa)
write.csv(tabRelCatch, 
          "tables/tabRelCatch.csv", 
          row.names = FALSE)

#' ### Single Stock Advice Table

# tabAdvice <- tableStockAdvice()
# write.csv()

