

costs_flbeia2 <- function(fleet, covars, flnm = NULL){
    
    res <- totvcost_flbeia2(fleet) + totfcost_flbeia(fleet, covars, flnm)
    
    return(res)               
}

totvcost_flbeia2 <- function(fleet){
    
    mts <- names(fleet@metiers)
    
    res <- FLQuant(0, dimnames = dimnames(fleet@effort))
    
    for(mt in mts){
        res <- res + fleet@metiers[[mt]]@vcost*fleet@effort*fleet@metiers[[mt]]@effshare
    }
    Rev <- suppressWarnings(revenue_flbeia(fleet)*fleet@crewshare)
    units(Rev) <- "NA"
    units(res) <- "NA"
    
    res <- res + Rev
    
    return(res)               
}
