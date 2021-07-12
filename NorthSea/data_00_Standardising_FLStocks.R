## MIXFISH FLSTOCKS

# READING IN AND CHECKING ------------------------------------------




rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
gc(); graphics.off()

options(stringsAsFactors = FALSE, scipen=100)

in.path  <- "bootstrap/data/WGNSSK_StockObjects/"
out.path <- "data/FLStocks_out"

stockslist <- list.files(in.path)


# cod ------------------------------------------
load( paste0(in.path, "cod47d20_stock_object20.Rdata") ) ## yv changed 23/10 to take into account reopening
#cod_iter1 <- cod
#cod_iter1@stock <- iter(cod@stock,1)
#cod_iter1@stock.n <- iter(cod@stock.n,1)
#cod_iter1@harvest <- iter(cod@harvest,1)
#stock <- cod_iter1
stock <- stk
range(stock)["plusgroup"] <- range(stock)["max"]
plot(stock)
save(stock, file=paste0(out.path,"/", "COD-NS.RData") )
rm(stock)
#rm(cod)
rm(stk)

#turbot ------------------------------------------

### 2020 assessment needs FLSAM see https://flr-project.org/FLSAM/ for installation
objs <- load( paste0(in.path, "TUR.27.4_Final__WGNSSK_2020_assessment.RData") , verbose = T)
stock <- TUR

#### yv fix the stock object does not have discards nor catches which is a problem when running make fleet / same in 2020
catch(stock) <- landings(stock)
discards(stock) <- landings(stock)-catch(stock)

#! add numbers
catch.n(stock)
landings.n(stock)
discards.n(stock)[] <- 0

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)[is.na(discards.wt(stock))] <- 0

# add stock numbers
stock.n(stock) <- TUR.sam@stock.n
stock@stock.wt
stock(stock) <- computeStock(stock)

# add harvest
harvest(stock) <- TUR.sam@harvest

plot(stock)

save(stock, file=paste0(out.path,"/", "TUR.RData"))
rm(stock)
rm(list = c(objs))


# haddock ------------------------------------------
#load(stockslist[i])
load( paste0(in.path, "had_27_46a20_FLStock objects 2020.Rdata") )
stock <- x.pg

# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)

plot(stock)
save(stock, file=paste0(out.path,"/", "HAD.RData") )
rm(x.pg)
rm(stock)
rm(x.bms.pg)
rm(x.ibc.pg)


# ple-ec ----------------------
load( paste0(in.path, "2020stockobject_mixfish_ple.27.7d.RData") )


stock <- ass.stock
# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)

units(harvest(stock)) <- "f" # added 2020 YV

plot(stock)


save(stock, file=paste0(out.path,"/", "PLE-EC.RData") )
rm(stock)
rm(ass.stock)


# ple-ns ---------------------------------
load( paste0(in.path, "ple.27.420_assessment_result_2020.Rdata") )
stock <- ass.stockOrigDisc

# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)

plot(stock)
save(stock, file=paste0(out.path,"/", "PLE-NS.RData") )
rm(ass.stockOrigDisc)
rm(stock)
rm(resultsOrigDisc)

# saithe ----------------------------------------
load( paste0(in.path, "pok.27.3a46.RData") )
stock <- stk # added 2020 YV


# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)
range(stock)["plusgroup"] <- range(stock)["max"] # added plusgroup

plot(stock)
save(stock, file=paste(out.path, "POK.RData", sep='/') )
rm(stock)
rm(stk)


# sole-ns --------------------------------------
load( paste0(in.path, "sol.27.4_WGNSSK_2020.Rdata") )
stock <- sol.27.4

# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)


plot(stock)
save(stock, file=paste0(out.path,"/", "SOL-NS.RData") )
rm(stock)
rm(sol.27.4)


# sole-ec --------------------------
load( paste0(in.path, "sol7D_xsastock.Rdata"), verbose = TRUE )
stock <- xsa.stock

# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)


plot(stock)
save(stock, file=paste0(out.path,"/", "SOL-EC.RData") )
rm(xsa.stock)
rm(stock)

# whiting -----------------------
#load("whg47d.Rdata")
#stock <- x.stock
#save(stock, file="../RData/WHG-NS.RData")


## test WHG-NS
load( paste0(in.path, "whg.27.47d_FLStock_2020.Rdata"), verbose = TRUE )
stock <- stk

# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)
range(stock)["plusgroup"] <- range(stock)["max"] # added plusgroup


plot(stock)
save(stock, file=paste0(out.path,"/", "WHG-NS.RData") )
rm(stk)
rm(stock)


# witch wit.27.3a47d - WIT ---------------------

nms <- load( paste0(in.path, "wit.27.3a47d.RData"))
assign("stock", get(nms[1]))

# Check values
catch.n(stock)
landings.n(stock)
discards.n(stock)

catch.wt(stock)
landings.wt(stock)
discards.wt(stock)

all.equal(c(catch(stock)), c(computeCatch(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))
all.equal(c(discards(stock)), c(computeDiscards(stock)))

harvest(stock)

plot(stock)
save(stock, file=paste0(out.path,"/", "WIT.RData") )
rm(list = c(nms, "nms"))
rm(stock)






# # SPICT stocks ------------------------------------------------------------
#
#
# # anf --------------------------
# tmp <- load( paste0(in.path, "ANF.RData"), verbose = TRUE )
# stock <- get(tmp)
#
# # Check values
# catch.n(stock)
# landings.n(stock)
# discards.n(stock)
#
# catch.wt(stock)
# landings.wt(stock)
# discards.wt(stock)
#
# all.equal(c(catch(stock)), c(computeCatch(stock)))
# all.equal(c(landings(stock)), c(computeLandings(stock)))
# all.equal(c(discards(stock)), c(computeDiscards(stock)))
#
# harvest(stock)
#
# plot(stock)
# save(stock, file=paste0(out.path,"/", "ANF.RData") )
# rm(stock)
#
#
# # bll --------------------------
# tmp <- load( paste0(in.path, "BLL.RData"), verbose = TRUE )
# stock <- get(tmp)
#
# # Check values
# catch.n(stock)
# landings.n(stock)
# discards.n(stock)
#
# catch.wt(stock)
# landings.wt(stock)
# discards.wt(stock)
#
# all.equal(c(catch(stock)), c(computeCatch(stock)))
# all.equal(c(landings(stock)), c(computeLandings(stock)))
# all.equal(c(discards(stock)), c(computeDiscards(stock)))
#
# harvest(stock)
#
# plot(stock)
#
# save(stock, file=paste0(out.path,"/", "BLL.RData") )
# rm(stock)
#
#
# # dab --------------------------
# tmp <- load( paste0(in.path, "DAB.RData"), verbose = TRUE )
# stock <- get(tmp)
#
# # Check values
# catch.n(stock)
# landings.n(stock)
# discards.n(stock)
#
# catch.wt(stock)
# landings.wt(stock)
# discards.wt(stock)
#
# all.equal(c(catch(stock)), c(computeCatch(stock)))
# all.equal(c(landings(stock)), c(computeLandings(stock)))
# all.equal(c(discards(stock)), c(computeDiscards(stock)))
#
# harvest(stock)
#
# plot(stock)
#
# save(stock, file=paste0(out.path,"/", "DAB.RData") )
# rm(stock)
#
#
# # lem --------------------------
# tmp <- load( paste0(in.path, "LEM.RData"), verbose = TRUE )
# stock <- get(tmp)
#
# # Check values
# catch.n(stock)
# landings.n(stock)
# discards.n(stock)
#
# catch.wt(stock)
# landings.wt(stock)
# discards.wt(stock)
#
# all.equal(c(catch(stock)), c(computeCatch(stock)))
# all.equal(c(landings(stock)), c(computeLandings(stock)))
# all.equal(c(discards(stock)), c(computeDiscards(stock)))
#
# harvest(stock)
#
# plot(stock)
#
# save(stock, file=paste0(out.path,"/", "LEM.RData") )
# rm(stock)
#
#
# # lin --------------------------
# tmp <- load( paste0(in.path, "LIN.RData"), verbose = TRUE )
# stock <- get(tmp)
#
# # Check values
# catch.n(stock)
# landings.n(stock)
# discards.n(stock)
#
# catch.wt(stock)
# landings.wt(stock)
# discards.wt(stock)
#
# all.equal(c(catch(stock)), c(computeCatch(stock)))
# all.equal(c(landings(stock)), c(computeLandings(stock)))
# all.equal(c(discards(stock)), c(computeDiscards(stock)))
#
# harvest(stock)
#
# plot(stock)
#
# save(stock, file=paste0(out.path,"/", "LIN.RData") )
# rm(stock)
#
#
# # usk --------------------------
# tmp <- load( paste0(in.path, "USK.RData"), verbose = TRUE )
# stock <- get(tmp)
#
# # Check values
# catch.n(stock)
# landings.n(stock)
# discards.n(stock)
#
# catch.wt(stock)
# landings.wt(stock)
# discards.wt(stock)
#
# all.equal(c(catch(stock)), c(computeCatch(stock)))
# all.equal(c(landings(stock)), c(computeLandings(stock)))
# all.equal(c(discards(stock)), c(computeDiscards(stock)))
#
# harvest(stock)
#
# plot(stock)
#
# save(stock, file=paste0(out.path,"/", "USK.RData") )
# rm(stock)
#
#
#
#



## Nephrops stock objects -------------------
# should first be copied over from last year's objects,
# then add new year of data
# should at least include catch, landings and discards


load( paste0(in.path, "NEP5.RData") )

stock <- window(stock, end = 2019)
plot(stock)
stock@catch[,ac(2019)] <- 2154
stock@landings[,ac(2019)] <- 1172
stock@discards[,ac(2019)] <- stock@catch[,ac(2019)] - stock@landings[,ac(2019)]
plot(stock)



save(stock, file=paste0(out.path,"/", "NEP5.RData") )
rm(stock)




load( paste0(in.path, "NEP6.RData") )

stock <- window(stock, end = 2019)
plot(stock)
stock@landings[,ac(2019)] <- 4359
# below : there was some discrepancies in the weight in the past, updated in 2020 (TB)
stock@landings.wt[,ac(2003:2019)] <- c(21.89199351,23.13593788,23.58207934,22.53175214,24.94593943,26.62902866,24.4495075,25.17674078,27.05215025,27.3418533,27.60281179,29.92886304,29.38729639,27.96700978,29.03124774,28.91596234,28.33792865)
stock@landings.n[,ac(2019)] <- stock@landings[,ac(2019)] / stock@landings.wt[,ac(2019)]


discardRate <- 0.2039
stock@catch[,ac(2019)] <- stock@landings[,ac(2019)] / (1-discardRate)

stock@discards[,ac(2019)] <- stock@catch[,ac(2019)] - stock@landings[,ac(2019)]
# below : there was some discrepancies in the weight in the past, updated in 2020 (TB)
stock@discards.wt[,ac(2003:2019)] <- c(9.564748174,9.21917995,10.3246532,10.58458192,10.88983247,10.97137138,10.53669501,11.73692885,11.02049164,10.15920299,9.786682284,13.5894069,9.992537381,10.22851674,10.28251004,11.21825119,11.55324304)
stock@discards.n[,ac(2019)] <- stock@discards[,ac(2019)] / stock@discards.wt[,ac(2019)]

all.equal(c(discards(stock)), c(computeDiscards(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))

stock@stock.n[,ac(2019)] <- 1163   # overwritten by autumn TV survey
stock@harvest[,ac(2019)] <- 0.166

plot(stock)

save(stock, file=paste0(out.path,"/", "NEP6.RData") )
rm(stock)
rm(discardRate)




load( paste0(in.path, "NEP7.RData") )

plot(stock)
stock <- window(stock, end = 2019)
stock@stock.n[,ac(2019)] <- 6129

stock@landings.n[,ac(2019)] <- 338
stock@discards.n[,ac(2019)] <- 8
stock@catch.n[,ac(2019)] <- 344

stock@landings[,ac(2019)] <- 9032
stock@discards[,ac(2019)] <- 100

stock@landings.wt[,ac(2019)] <- 28.31
stock@discards.wt[,ac(2019)] <- 13.32

stock@catch[,ac(2019)] <- stock@landings[,ac(2019)] + stock@discards[,ac(2019)]

stock@harvest[,ac(2019)] <- 0.056

stock@stock.n[,ac(2019)] <- 6129   # overwritten by autumn TV survey
all.equal(c(discards(stock)), c(computeDiscards(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))

plot(stock)
save(stock, file=paste0(out.path,"/", "NEP7.RData") )
rm(stock)




load( paste0(in.path, "NEP8.RData") )

plot(stock)
stock <- window(stock, end = 2019)
stock@landings.wt[,ac(2019)] <- 21.81
stock@landings.n[,ac(2019)] <- 127

stock@discards.wt[,ac(2019)] <- 9.76
stock@discards.n[,ac(2019)] <- 42
stock@catch.n[,ac(2019)] <- 158

stock@landings[,ac(2019)] <- 2684
stock@discards[,ac(2019)] <- 411

stock@catch[,ac(2019)] <- stock@landings[,ac(2019)] + stock@discards[,ac(2019)]
stock@catch.wt[,ac(2019)] <- stock@catch[,ac(2019)] / stock@catch.n[,ac(2019)]

stock@stock.n[,ac(2019)] <- 865   # overwritten by autumn TV survey

all.equal(c(discards(stock)), c(computeDiscards(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))

stock@harvest[,ac(2019)] <- 0.183
plot(stock)

save(stock, file=paste0(out.path,"/", "NEP8.RData") )
rm(stock)





load( paste0(in.path, "NEP9.RData") )

plot(stock)
stock <- window(stock, end = 2019)

stock@landings.wt[,ac(2019)] <- 25.13
stock@landings.n[,ac(2019)] <- 55

stock@discards.wt[,ac(2019)] <- 9.84
stock@discards.n[,ac(2019)] <- 1
stock@catch.n[,ac(2019)] <- 56

stock@landings[,ac(2019)] <- 1395
stock@discards[,ac(2019)] <- 10

stock@catch[,ac(2019)] <- stock@landings[,ac(2019)] + stock@discards[,ac(2019)]
stock@catch.wt[,ac(2019)] <- stock@catch[,ac(2019)] / stock@catch.n[,ac(2019)]

all.equal(c(discards(stock)), c(computeDiscards(stock)))
all.equal(c(landings(stock)), c(computeLandings(stock)))

stock@harvest[,ac(2019)] <- 0.148
stock@stock.n[,ac(2019)] <- 376

plot(stock)

save(stock, file=paste0(out.path,"/", "NEP9.RData") )
rm(stock)





load( paste0(in.path, "NEP10.RData") )

plot(stock)
stock <- window(stock, end = 2019)

stock@landings[,ac(2019)] <- 21
stock@landings.wt[,ac(2019)] <- 33.01

stock@catch[,ac(2019)] <- stock@landings[,ac(2019)]
plot(stock)
save(stock, file=paste0(out.path,"/", "NEP10.RData") )
rm(stock)






load( paste0(in.path, "NEP32.RData") )
plot(stock)
stock <- window(stock, end = 2019)
stock@landings[,ac(2019)] <- 191
stock@catch[,ac(2019)] <- stock@landings[,ac(2019)]

plot(stock)
save(stock, file=paste0(out.path,"/", "NEP32.RData") )
rm(stock)




load( paste0(in.path, "NEP33.RData") )
plot(stock)
stock <- window(stock, end = 2019)
stock@landings[,ac(2019)] <- 1612
stock@catch[,ac(2019)] <- stock@landings[,ac(2019)]

plot(stock)
save(stock, file=paste0(out.path,"/", "NEP33.RData") )
rm(stock)




load( paste0(in.path, "NEP34.RData") )
plot(stock)
stock <- window(stock, end = 2019)
stock@landings[,ac(2019)] <- 1186
stock@landings.wt[,ac(2019)] <- 35.83
stock@catch[,ac(2019)] <- stock@landings[,ac(2019)]

plot(stock)
save(stock, file=paste0(out.path,"/", "NEP34.RData") )
rm(stock)




load( paste0(in.path, "NEPOTH-NS.RData") )
plot(stock)
stock <- window(stock, end = 2019)
stock@landings[,ac(2019)] <- 723.8
stock@discards[,ac(2019)] <- 567.1
stock@catch[,ac(2019)] <- stock@landings[,ac(2019)] + stock@discards[,ac(2019)]

plot(stock)
save(stock, file=paste0(out.path,"/", "NEPOTH-NS.RData") )
rm(stock)
