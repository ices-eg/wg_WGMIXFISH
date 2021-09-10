
library(FLBEIA)
library(ggplot2)
library(patchwork)

source("R/nameChange.R")



intermYr <- 2020
adviceYr <- 2021



# load data ---------------------------------------------------------------
# file.copy(from = "../2021_NrS_MixedFisheriesMethods/model/03_input_objects.Rdata", to = "data")
# file.copy(from = "../2021_NrS_MixedFisheriesMethods/model/04_runs.Rdata", to = "data")

load(file = "data/03_input_objects.Rdata", verbose = T)
load(file = "data/04_runs.Rdata", verbose = T)

tmp <- load(file = "data/dfBioSum_FCUBE.rdata", verbose = T)
assign(x = "bioSumFcube", value = get(tmp))
str(bioSumFcube)
hit <- which(bioSumFcube$scenario != "baseline")
bioSumFcube$scenario[hit] <- paste0("fcube_", bioSumFcube$scenario[hit])
subset(bioSumFcube, stock == "COD-NS")

tmp <- load(file = "data/dfFltStkSum_FCUBE.rdata", verbose = T)
assign(x = "fltStkSumFcube", value = get(tmp))
hit <- which(fltStkSumFcube$scenario != "baseline")
fltStkSumFcube$scenario[hit] <- paste0("fcube_", fltStkSumFcube$scenario[hit])



# make summary objects for FLBEIA -----------------------------------------

fltSumFlbeia <- fltSum(res_iSQE_pSSMIN, scenario = "flbeia_min")
fltSumFlbeia$fleet <- nameChange(fltSumFlbeia$fleet, "old")
head(fltSumFlbeia)

fltStkSumFlbeia <- fltStkSum(res_iSQE_pSSMIN, scenario = "flbeia_min")
fltStkSumFlbeia$stock <- nameChange(fltStkSumFlbeia$stock, "old")
fltStkSumFlbeia$fleet <- nameChange(fltStkSumFlbeia$fleet, "old")
head(fltStkSumFlbeia)

bioSumFlbeia <- bioSum(res_iSQE_pSSMIN, scenario = "flbeia_min")
bioSumFlbeia$stock <- nameChange(bioSumFlbeia$stock, "old")
head(bioSumFlbeia)




# merge FLBEIA and FCube summaries ----------------------------------------

bioSumAll <- merge(x = bioSumFcube, y = bioSumFlbeia, all = TRUE)
head(bioSumAll)
str(bioSumAll)
unique(bioSumAll$scenario)

fltStkSumAll <- merge(x = fltStkSumFcube, y = fltStkSumFlbeia, all = TRUE)
head(fltStkSumAll)
str(fltStkSumAll)
unique(fltStkSumAll$scenario)





# bioSum comparison ----------------------------------------------------------


dfsub <- subset(bioSumAll, 
  scenario %in% c("baseline", "fcube_min", "flbeia_min") & 
  year %in% (intermYr-1):(intermYr+2) &
  stock %in% c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-NS", "TUR", "WHG-NS", "WIT"))
dfUnique <- unique(dfsub[,c("scenario", "year", "stock")])
dfUnique$ssb <- 0
dfUnique$rec <- 0
dfUnique$f <- 0
dfUnique$catch <- 0



# ssb
XLIM <- c(intermYr, intermYr+2)
p1 <- ggplot(data = dfsub) + aes(x = year, y = ssb, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap(~ stock, scales = "free_y") +
  geom_line() + 
  geom_point() + 
  geom_blank(data = dfUnique) + 
  xlab("") +
  xlim(XLIM[1], XLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p1)


# catch
XLIM <- c(intermYr-1, intermYr+1)
p2 <- ggplot(data = dfsub) + aes(x = year, y = catch, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap(~ stock, scales = "free_y") +
  geom_line() + 
  geom_point() + 
  geom_blank(data = dfUnique) + 
  xlab("") +
  xlim(XLIM[1], XLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p2)


# f
XLIM <- c(intermYr-1, intermYr+1)
p3 <- ggplot(data = dfsub) + aes(x = year, y = f, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap(~ stock, scales = "free_y") +
  geom_line() + 
  geom_point() + 
  geom_blank(data = dfUnique) + 
  xlab("") +
  xlim(XLIM[1], XLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p3)


# rec
XLIM <- c(intermYr, intermYr+2)
p4 <- ggplot(data = dfsub) + aes(x = year, y = rec, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap(~ stock, scales = "free_y") +
  geom_line() + 
  geom_point() + 
  geom_blank(data = dfUnique) + 
  xlab("") +
  xlim(XLIM[1], XLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p4)



# all
dfsub2 <- reshape2::melt(dfsub, id.vars = c("year", "scenario", "stock"), measure.vars = c("ssb", "rec", "f", "catch"), variable.name = "var")
dfUnique2 <- unique(dfsub2[,c("scenario", "year", "stock")])
dfUnique2$value <- 0

p <- ggplot(data = dfsub2) + aes(x = year, y = value, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_grid(var ~ stock, scales = "free_y") +
  geom_line() + 
  geom_point() + 
  geom_blank(data = dfUnique2) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p)




# all alt
p1a <- p1 + facet_wrap(~ stock, scales = "free_y", ncol = length(unique(dfsub$stock)))
p2a <- p2 + facet_wrap(~ stock, scales = "free_y", ncol = length(unique(dfsub$stock)))
p3a <- p3 + facet_wrap(~ stock, scales = "free_y", ncol = length(unique(dfsub$stock)))
p4a <- p4 + facet_wrap(~ stock, scales = "free_y", ncol = length(unique(dfsub$stock)))



p <- p1a / p2a / p3a / p4a
print(p)

png("output/bioSum_compare.png", width = 15, height = 7.5, units = "in", res = 400)
  print(p)
dev.off()




# fltStkSum comparison ----------------------------------------------------------


dfsub <- subset(fltStkSumAll, 
  scenario %in% c("baseline", "fcube_min", "flbeia_min") & 
  year %in% (intermYr-1):(intermYr+2)# &
  # stock %in% c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-NS", "TUR", "WHG-NS", "WIT")
  )
head(dfsub)

dfUnique <- unique(dfsub[,c("scenario", "year", "fleet", "stock")])
dfUnique$quotaUpt <- 0
dfUnique$catch <- 0


# choking stock
subset(dfsub, !scenario=="baseline" & fleet == "GE_Beam<24" & year == intermYr+1)

YLIM <- c(0, 1.05)
p <- ggplot(data = subset(dfsub, !scenario=="baseline" & year == intermYr+1)) + 
  aes(x = stock, y = quotaUpt, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap( ~ fleet, scales = "free_y", ncol = 5) +
  # geom_line() +
  # geom_hline(yintercept = 0, lty = 1) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_point() +
  geom_blank(data = dfUnique) + 
  xlab("") +
  ylim(YLIM[1], YLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p)

png("output/quotaUpt_compare.png", width = 15, height = 10, units = "in", res = 400)
  print(p)
dev.off()


# catches by fleet ~ scenario --------------



# selection pattern compare ----------------
stocks1 <- lapply(stocks, function(x){
  as.data.frame(x)})
for(i in seq(stocks1)){
  stocks1[[i]]$stock <- names(stocks)[i]
}
stocks1 <- do.call("rbind", stocks1)
rownames(stocks1) <- seq(nrow(stocks1))
stocks1$scenario <- "WGNSSK"
head(stocks1)


res <- res_iSQE_pSSMIN
stocks2 <- vector("list", length(res$biols))
for(i in seq(stocks2)){
  stocks2[[i]] <- as.data.frame(biolfleets2flstock(
    biol = res$biols[[i]], 
    fleets = res$fleets))
  stocks2[[i]]$stock <- names(res$biols)[i]
}
stocks2 <- do.call("rbind", stocks2)
rownames(stocks2) <- seq(nrow(stocks2))
stocks2$scenario <- "WGMIXFISH"
head(stocks2)

stocks3 <- merge(x = stocks1, y = stocks2, all = TRUE)
stocks3$stock <- nameChange(x = stocks3$stock, form = "old")
str(stocks3)


dfsub <- subset(stocks3, 
  year %in% (intermYr-1) &
  stock %in% c("COD-NS", "HAD", "PLE-EC", "PLE-NS", "POK", "SOL-NS", "TUR", "WHG-NS", "WIT"))
head(dfsub)

dfUnique <- unique(dfsub[,c("scenario", "year", "stock", "age")])
dfUnique$data <- 0



p <- ggplot(data = subset(dfsub, slot == "harvest")) + 
  aes(x = as.numeric(age), y = data, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap( ~ stock, scales = "free_y") +
  # geom_() + 
  geom_line() +
  geom_point() +
  geom_blank(data = dfUnique) + 
  xlab("") +
  ylab("harvest (F)") +
  # xlim(XLIM[1], XLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p)


png("output/Fpattern_compare.png", width = 8, height = 6, units = "in", res = 400)
  print(p)
dev.off()



p <- ggplot(data = subset(dfsub, slot == "catch.n")) + 
  aes(x = as.numeric(age), y = data, group = scenario, color = scenario, lty = scenario, shape = scenario) + 
  facet_wrap( ~ stock, scales = "free_y") +
  # geom_() + 
  geom_line() +
  geom_point() +
  geom_blank(data = dfUnique) + 
  xlab("") +
  ylab("catch (numbers)") +
  # xlim(XLIM[1], XLIM[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
print(p)


png("output/catch.n_compare.png", width = 8, height = 6, units = "in", res = 400)
  print(p)
dev.off()


## Aggregate catch SSA and FLBEIA
tmp <- subset(dfsub, slot == "catch")
tmp2 <- tmp[,which(colnames(tmp) %in% c("data", "stock", "scenario"))]
table_catch_comp <- as.data.frame(tidyr::pivot_wider(tmp2, names_from=scenario, values_from = data))
table_catch_comp <- table_catch_comp[order(table_catch_comp$stock),]
table_catch_comp <- cbind(table_catch_comp, "Diff"=table_catch_comp$WGMIXFISH/table_catch_comp$WGNSSK-1)

## Fbar SSA and FLBEIA
tmp <- subset(dfsub, slot == "harvest")
name_sp <- unique(dfsub$stock)[order(unique(dfsub$stock))]
fbar_range <- matrix(ncol=2, nrow=length(biols))
for (k in 1:length(biols)){
  fbar_range[k,] <- range(biols[[k]])[which(names(range(biols[[k]])) %in% c("minfbar", "maxfbar"))]
}
rownames(fbar_range) <- attr(biols,"names")
rownames(fbar_range) <- sub("_dash_", replacement = "-", rownames(fbar_range))
table_fbar <- table_catch_comp
for (i in 1:nrow(table_fbar)){
  tmp2 <- subset(tmp, stock==table_fbar$stock[i])
  table_fbar$WGNSSK[i] <- mean(subset(tmp2, scenario=="WGNSSK" & age %in% fbar_range[table_fbar$stock[i],1]:fbar_range[table_fbar$stock[i],2])$data)
  table_fbar$WGMIXFISH[i] <- mean(subset(tmp2, scenario=="WGMIXFISH" & age %in% fbar_range[table_fbar$stock[i],1]:fbar_range[table_fbar$stock[i],2])$data)
}
table_fbar$Diff <- table_fbar$WGMIXFISH/table_fbar$WGNSSK-1
