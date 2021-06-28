#' @title QC function for FLStock object
#'
#' @description \code{QC_FLStock} checks over an FLStock object and looks for
#' any missing data or units, to save us time down the road... 

#' @param FLStock is the FLStock object to check.

#' @return is either a stop message if something is not right, a warning if
#' something is not quite right, or a confirmation the FLStock object is A-OK
#' with some messages of support..

#' @examples
#' None

#' @export
QC_FLStock <- function(FLStock = NULL) {

require(FLCore)
require(FLash)

###################
## Descriptions
####################

## Check name slot
if(isTRUE(FLStock@name == "") | isTRUE(length(FLStock@name) == 0)) stop("The stock name slot should be filled with x@name <- ''")

## Check description slot
if(isTRUE(FLStock@desc== "") | isTRUE(length(FLStock@desc) == 0)) warning("The stock has no description, can be filled with x@desc <- ''")

## fbar etc..
if(isTRUE(is.na(FLStock@range[["minfbar"]])) | isTRUE(is.na(FLStock@range[["maxfbar"]]))) stop("fbar ranges are not set, this can be done with x@range[['minfbar']][] <- n & x@range[['maxfbar']] <- n")

if(isTRUE(is.na(FLStock@range[["plusgroup"]]))) warning("There is no plusgroup set set, this can be done with x@range[['plusgroup']][] <- n")


###################
## Catch slots   ##
###################
slot.names <- c("landings", "discards","catch", "stock", paste(rep(c("landings", "discards","catch", "stock"),2), rep(c("n","wt"),each = 4), sep = "."))

## data
lapply(slot.names, function(x) { 
	       
	       if(!x %in% c("discards.wt", "discards.n")) 

	       if(any(is.na(slot(FLStock, x)))) stop(paste("Missing data in the", x, "slot, maybe you can fill with ComputeCatch(), ComputeLandings, ComputeDiscards or ComputeStock()? Note, these functions take arguements slot = 'n' and slot = 'wt'"))})

## units
lapply(slot.names, function(x) {
	       if(is.na(units(slot(FLStock, x)))) stop(paste("Please fill in the unit of", x, "with, e.g. units(x@catch.wt)[] <- 'kg'"))})

###################
## harvest slots ##
###################
if(is.na(units(slot(FLStock, "harvest")))) stop("Please fill the harvest units, either 'f' or 'hr'")
if(any(is.na(slot(FLStock, "harvest")))) stop("Missing data in the harvest slot, please fill")
if(any(round(slot(FLStock, "harvest"),2) != round(computeHarvest(FLStock), 2))) warning("The fishing mortality rates in @harvest do not match those from computeHarvest(FLStock),... is this expected?")
if(any(is.na(slot(FLStock, "harvest.spwn")))) stop("Missing data in the harvest.spwn slot, please fill")

###################
## Other biological params
###################
slot.names <- c("m.spwn", "mat", "m")
lapply(slot.names, function(x) {
	if(any(is.na(slot(FLStock, x)))) stop(paste("Missing data in the", x, "slot, please fill"), sep = " ")
	       })

####################
## SOP checks 
####################
if(any(slot(FLStock, "stock") != computeStock(FLStock))) warning("the 'stock' slot does not match outputs of ComputeStock(),... is this expected?")

if(any(slot(FLStock, "catch") != computeCatch(FLStock))) warning("the 'catch' slot does not match outputs of ComputeCatch(),... is this expected?")
	
if(any(slot(FLStock, "landings") != computeLandings(FLStock))) warning("the 'landings' slot does not match outputs of ComputeLandings(),... is this expected?")
	      	       
if(any(slot(FLStock, "discards") != computeDiscards(FLStock)) | is.na(any(slot(FLStock, "discards") != computeDiscards(FLStock)) )) warning("the 'discards' slot does not match outputs of ComputeDiscards(),... is this expected?")
	
# outputs 
cat("Stock name:", FLStock@name, sep = "\t\t"); cat("\n")
cat("Stock description is:", FLStock@desc, sep = "\t"); cat("\n")
cat("fbar range is:", paste0(FLStock@range[["minfbar"]],":",FLStock@range[["maxfbar"]]), sep = "\t\t"); cat("\n")
cat("Plusgroup is:", FLStock@range[["plusgroup"]], sep = "\t\t"); cat("\n")

slot.names <- c("landings", "discards","catch", "stock","harvest", paste(rep(c("landings", "discards","catch", "stock"),2), rep(c("n","wt"),each = 4), sep = "."))

lapply(slot.names, function(x) {
cat(paste(paste(x, "units are:", sep = " "), units(slot(FLStock, x)), sep = "\t")); cat("\n")
	       })

return("Please check any error or warnings below. And Thank You from MIXFISH!")

}
