#' @title Make FLBiols function
#'
#' @description This function takes the FLStock objects and creates an FLBiols object
#' needed as an input to FLBEIA.

#' @param data_path is the path where the data files are stored (i.e. one upper
#' level from the FLStocks 
#'
#' @return is an FLBiols object containing a list of FLBiol objects for each
#' stock

#' @examples make_FLBiols(stk_path = \path\to\data)

#' @export

make_FLBiols <- function(data_path = NULL) {

stk_path  <- file.path(data_path)

## Load the stock objects from the path
stocks<-FLStocks(lapply(list.files(path = stk_path, pattern = ".RData"),function(x){
                            load(file.path(stk_path,x))
                        res<-get("stock")
                        name(res)<-gsub('.RData',"",x)
                        res}))

# Convert from FLStock to FLBiol
biols <- FLBiols(lapply(stocks, function(x) {
  as(x, "FLBiol")
}))

## Set the plus group
## Set maturity for Nephrops
for(i in names(biols)) {
  biols[[i]]@range <- stocks[[i]]@range
  biols[[i]]@range[["plusgroup"]] <-  stocks[[i]]@range[["plusgroup"]]
  biols[[i]]@mat$mat[is.na(biols[[i]]@mat$mat)] <- 1
}

## Return the FLBiols list
return(biols)

}
