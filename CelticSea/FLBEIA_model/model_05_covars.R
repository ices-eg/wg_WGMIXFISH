##############################################################################
##
## Making the covars object inputs 
##
## Author:  Paul Dolder
## Created: 05/08/2021
## Updated:
##
##############################################################################

## Paths
data_path      <- file.path("results", "clean_data")
stock_path     <- file.path("results", "clean_data", "clean_stock_objects")
flbeia_in      <- file.path("results", "FLBEIA_inputs")
flbeia_precon  <- file.path(flbeia_in, "preconditioned")
flbeia_cond    <- file.path(flbeia_in, "conditioned")

## This is empty, as no covars are used.
## It may be we want to include something in future.

covars <- NULL

save(covars, file = file.path(flbeia_cond, "covars.RData"))
