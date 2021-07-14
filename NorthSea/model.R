## Run analysis, write model results

## Before: data/NS_FLStocks_01_Reproduce_The_Advice_2019
## After: model/ , output/

# load libraries
# compatible taf versions of FLCore, FLAssess and FLFleet are loaded from the taf library
library(icesTAF)
taf.library(FLCore)
library(FLash)
# taf.library(FLAssess)
taf.library(FLFleet)
library(ggplot2)
library(grid)
library(reshape2)
library(gridExtra)
# library(genalg)
library(lhs)




mkdir("model")
mkdir("output/01_Reproduce_The_Advice/results")
mkdir("output/01_Reproduce_The_Advice/plots")
mkdir("output/02_Conditioning/results")
mkdir("output/02_Conditioning/plots")

source("model_00_settings.R")

source("model_01_ReproduceTheAdvice_2020 modif for SAM stocks.r")

source("model_02_Conditioning Fleets_2020.r")

#source("model_03_FCube Projection_ValIntYr_withCMT.R")
#source("model_03_FCube Projection_SqEIntYr_withCMT.R")
source("model_03_FCube Projection_sqEintYr_AND_ValIntYr_withCMT.R")



#source("model_04_01_FCube Projection_SQuoIntYrTest.r")
source("model_04_01_Optim_CMT.R")
source("model_04_02_ExtractResults.R")

#source("model_04_02_Projection TAC year_function_ReturnBest2.R")
#source("model_04_02_Projection TAC year_function_ReturnBest2_CMT.R")

# source("model_04_03_rbgaAmoi.r")   # move this function to the software/functions file
#source("model_04_03_Optim.R")
