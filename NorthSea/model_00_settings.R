# model settings
rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
# ac
ac <- function(x, ...)
  as.character(x, ...)

# an
an <- function(x, ...)
  as.numeric(x, ...)

# define year indices
now                <- 2020
yr.now             <- ac(now)
yr.TAC             <- ac(now+1)
yr.TACp1           <- ac(now+2)
yr.assess          <- ac(now-1)
LastProjectionYear <- now+2 #number of projection years
YearsProj          <- now:LastProjectionYear

# logical flag
FIDES           <- FALSE

# stock names
stock.names  <- c("COD-NS","HAD","NEP10","NEP32","NEP33","NEP34","NEP5","NEP6","NEP7","NEP8","NEP9",
                  "NEPOTH-NS","PLE-EC","PLE-NS","POK","SOL-NS","TUR","WHG-NS","WIT")
dem.names    <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK",  "SOL-NS", "WHG-NS", "TUR", "WIT")
nep.names    <- c("NEP10","NEP32","NEP33","NEP34","NEP5","NEP6","NEP7","NEP8","NEP9","NEPOTH-NS")

st.lst  <- as.list(stock.names) ; names(st.lst)  <- stock.names
dem.lst <- as.list(dem.names)   ; names(dem.lst) <- dem.names; n.dem <-length(dem.names)
nep.lst <- as.list(nep.names)   ; names(nep.lst) <- nep.names; n.nep <-length(nep.names)


# landing obligation
LO <- c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-NS","SOL-EC", "WHG-NS", "TUR","WIT")         # added NS turbot (NS) in 2019 (VT)
LO <- NA
if (is.element("NEP",LO)) LO <- c(LO,nep.names)

st.lst  <- as.list(stock.names) ; names(st.lst)  <- stock.names
dem.lst <- as.list(dem.names)   ; names(dem.lst) <- dem.names; n.dem <-length(dem.names)
nep.lst <- as.list(nep.names)   ; names(nep.lst) <- nep.names; n.nep <-length(nep.names)

save.image(file= "model/model_settings.Rdata")
