## Preprocess data, write TAF data tables


## Before: bootstrap/
## After:  data/

library(icesTAF)
library(ggplot2)
library(lattice)
library(pander)
library(data.table)

### FLR libraries should be on the TAF server now
library(FLCore)
library(FLFleet)

ver01 <- "NS_Fleet_database"

# create directories
mkdir("data")
mkdir(path = "data/FLStocks_out")                             ## output data_00 stock objects
mkdir(path = paste0("data/",ver01))                           ## output data_01 fleet data
mkdir(path = paste0("data/",ver01,"/diagnostics"))            ## output data_02 fleet data diagnostics
mkdir("output")                                               ## output data_03
mkdir("output/diagnostics")                                   ## output data_03
mkdir(path = "data/FIDES_out")                                ## output data_04 FIDES output

source("data_00_Standardising_FLStocks.R")

source("data_01_get_catch_effort_data.R")

source("data_02_make_fleet_aggregation_with_age_dist.R")

source("data_03_make_new_fleets_aa_ad.R")

source("data_04_ReadFides.R")
