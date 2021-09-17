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
library(FLBEIA)

## Load functions
source("funcs/figures_tables_functions.R")

#' Read and process data
#' =====================

## Produce the summary result files
load("results/ScenarioResults.RData")

summary_fleet <- lapply(names(runs), function(x) {
  fltSum(runs[[x]], scenario = x)
}) %>% bind_rows()

summary_fleetstock <- lapply(names(runs), function(x) {
  fltStkSum(runs[[x]], scenario = x)
}) %>% bind_rows()

summary_metier <- lapply(names(runs), function(x) {
  mtSum(runs[[x]], scenario = x)
}) %>% bind_rows()

summary_metierstock <- lapply(names(runs), function(x) {
  mtStkSum(runs[[x]], scenario = x)
}) %>% bind_rows()

summary_biological <-lapply(names(runs), function(x) {
  bioSum(runs[[x]], scenario = x)
}) %>% bind_rows()

summary_advice     <- lapply(names(runs), function(x) {
  advSum(runs[[x]], scenario = x)
}) %>% bind_rows()


## Load csv outputs from FLBEIA
#summary_fleet      <- read.csv("1_Data/00_fleet_summary.csv")
#summary_fleetstock <- read.csv("1_Data/01_fleet_stock_summary.csv")
#summary_metier     <- read.csv("1_Data/02_metier_summary.csv")
#summary_metierstock<- read.csv("1_Data/03_metier_stock_summary.csv")
#summary_biological <- read.csv("1_Data/04_biological_summary.csv")
#summary_advice     <- read.csv("1_Data/05_advice_summary.csv")

## Load csv single-stock advice
summary_ssa <- read.csv("results/single_species_results/single_species_advice_GA.csv")

## Define assessment year
ay <- 2020

## Define stocks
#stockVector <- c("cod.27.7e-k", "had.27.7b-k", "whg.27.7b-ce-k", "hke.27.3a46-8abd", 
 #                "meg.27.7b-k8abd", "mon.27.78abd", "sol.27.7fg"), "sol.27.7e") ## FOR TESTING!!! 

stockVector <- unique(summary_biological$stock)
stockLabels <- stockVector

## Scenarios
scenarioVector <- unique(summary_advice$scenario)

## Define plotting colours
pal_all_region <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                    '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')

write.csv(summary_advice,"results/summary_advice.csv")
write.csv(summary_biological,"results/summary_biological.csv")
write.csv(summary_fleet,"results/summary_fleet.csv")
write.csv(summary_fleetstock,"results/summary_fleetstock.csv")
write.csv(summary_metier,"results/summary_metier.csv")
write.csv(summary_metierstock,"results/summary_metierstock.csv")
write.csv(summary_ssa,"results/summary_ssa.csv")

#' Generate figures
#' ----------------
#' 
#' ### Mixed fisheries projections by stock and scenario
png("figures/figProj.png", width = 7, height = 6, units = "in", res = 400)
underOverPlot(inclYear  = ay+1, 
              inclStock = stockVector, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice, 
              stockLabels =  paste0(1:length(stockLabels),":",stockLabels),
              pal = pal_all_region[1:length(stockLabels)])
dev.off()

png("figures/figProj_gg.png", width = 7, height = 6, units = "in", res = 400)
ggunderOverPlot(inclYear  = ay+1, 
              inclStock = stockVector, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice, 
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
  tidyr::spread(scenario, catch) %>%
  arrange(max) %>%
  head(3) %>%
  ungroup() %>%
  select(stock) %>%
  unlist() 

colVectorSubset   <- pal_all_region[which(stockVector %in% stockVectorSubset)]

png("figures/figProjLow.png", width = 7, height = 6, units = "in", res = 400)
underOverPlot(inclYear  = ay+1, 
              inclStock = stockVectorSubset, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice, 
              stockLabels =  paste0(1:length(stockVectorSubset),":",stockVectorSubset),
              pal = colVectorSubset)
dev.off()

png("figures/figProjLow_gg.png", width = 7, height = 6, units = "in", res = 400)
ggunderOverPlot(inclYear  = ay+1, 
              inclStock = stockVectorSubset, 
              inclScen  = scenarioVector, 
              var = "catch", 
              resObj = summary_advice, 
              stockLabels =  paste0(1:length(stockVectorSubset),":",stockVectorSubset),
              pal = colVectorSubset)
dev.off()

#' ### Mixed fisheries estimates of effort needed to reach single stock advice (bar plot) 

flt_list <- grep("_fleet", unique(summary_fleet$fleet), value = TRUE, invert = TRUE)

png("figures/figMixFishBar.png", width = 7, height = 8, units = "in", res = 300)
plotEffortLim(ay = ay,
              fleetObj    = summary_fleet %>% filter(fleet %in% flt_list),
              fleetStkObj = summary_fleetstock %>% filter(fleet %in% flt_list),
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

