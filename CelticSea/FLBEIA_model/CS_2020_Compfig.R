
library(tidyverse)
library(FLBEIA)


## Load functions
source("funcs/figures_tables_functions.R")



# underoover plot with fixed axis -----------------------------------------


underOverPlot <- function(inclYear  = NULL, 
                          inclStock = NULL, 
                          inclScen  = NULL,
                          var       = "catch",
                          resObj    = NULL,
                          stockLabels = NULL,
                          pal       = NULL){
  
  # ---------------
  # some checks to ensure correct arguments
  # ---------------
  
  ## Stop if no year specified
  if(is.null(inclYear)) {
    stop("is.null(inclYear): Year must be defined")
  }
  
  ## Stop if specific stock is not in data object 
  if(!is.null(inclStock) & (sum(!(inclStock %in% unique(resObj$stock))) > 0)) {
    stop("inclStock: Specified stock(s) do not match resObj$stock")
  }
  
  ## Stop if specified scenarios are not in data object
  if(!is.null(inclScen) & (sum(!(inclScen %in% unique(resObj$scenario))) > 0)) {
    stop("inclScen: Specified scenario(s) do not match resObj$scenario")
  }
  
  ## Stop if scenarios for a given stock have different TACs
  if(sum(sapply(unique(resObj$stock), function(x) {
    ifelse(length(unique(resObj$tac[resObj$year == inclYear & resObj$stock == x])) > 1,
           TRUE,
           FALSE)
  })) > 0){
    stop("resObj: Scenarios for each stock have different TACs")
  }
  
  # ----------------
  # Calculate stuff
  # ----------------
  
  if(is.null(pal)){
    palvector <- colorRampPalette(c("grey10", "grey90"))
    pal = palvector(length(inclStock))
  }
  
  sa <- resObj %>%
    ## filter to retain specified year, stocks and scenarios
    filter(year == inclYear, 
           stock %in% inclStock, 
           scenario %in% c("baseline",inclScen)) %>%
    group_by(stock, scenario) %>%
    summarise_all(sum) %>%
    ## calculate landings from catch and discards data
    mutate(landings = catch - discards) %>%
    gather(variable, value, catch, landings, discards) %>%
    ## select catch or landings
    filter(variable == var) %>%
    group_by(stock, scenario) %>%
    mutate(overshoot  = min(tac, value),
           undershoot = ifelse((value - tac) > 0 , 0, value - tac))
  
  ## calculate tac matrix
  sa_tac   <- sa %>%
    group_by(stock) %>%
    summarise(tac = unique(tac)) %>%
    arrange(stock) %>%
    column_to_rownames("stock") %>%
    as.matrix()
  
  ## calculate catch matrix
  sa_catch <- sa %>%
    select(stock, scenario, value) %>%
    tidyr::spread(scenario, value) %>%
    arrange(stock) %>%
    column_to_rownames("stock") %>%
    as.matrix()
  
  ## calculate overshoot matrix
  sa_overshoot <- sa %>% 
    select(stock, scenario, overshoot) %>%
    tidyr::spread(scenario, overshoot) %>%
    arrange(stock) %>%
    column_to_rownames("stock") %>%
    as.matrix()
  
  ## calculate undershoot matrix
  sa_undershoot <- sa %>%
    select(stock, scenario, undershoot) %>%
    tidyr::spread(scenario, undershoot) %>%
    arrange(stock) %>%
    column_to_rownames("stock") %>%
    as.matrix()
  
  # --------------
  # PLOT
  # --------------
  
  ## Define stock labels if none available
  if(is.null(stockLabels)) {
    stockLabels =  paste0(1:length(stockLabels),":",stockLabels)
  }
  
  ## Define layout of plotting area
  layout(matrix(c(1,1,2,3), 2, 2),
         widths=c(4,1), heights=c(1,2))
  par(mar=c(2.1,4.1,2.1,1.1))
  scenarios <- c(inclScen) 
  
  mp <- barplot(sa_catch[,scenarios], angle = 15, density = 20, col="black", 
                beside = TRUE, legend = F, 
                ylim = c(min(sa_undershoot)*1.15, 60000), 
                ylab = paste("predicted", ifelse(var=="catch", "catch", "landings"), inclYear), 
                names.arg = rep('', ncol(sa_catch[,scenarios])))
  mp <- barplot(sa_overshoot[,scenarios], col=pal, beside = TRUE, legend = F, add=T, names.arg = rep('', ncol(sa_catch[,scenarios])),ylim = c(min(sa_undershoot)*1.15, 60000))
  mp <- barplot(sa_undershoot[,scenarios], col=pal, beside = TRUE, legend = F, add=T, names.arg = rep('', ncol(sa_catch[,scenarios])))
  
  mtext(side = 1, at = mp, line = 0.01, 
        text = rep(formatC(c(1:dim(sa_catch)[1])),5), col = "black", cex = 0.8)
  
  mtext(text = "Scenarios: ", side = 3, line = 0.5, adj = 0)
  
  # text(mp[1,1], max(res.[,scenarios])*1.1, "Scenarios: ", pos=4)
  for (i in 1:length(scenarios)){
    text(x = mean(mp[,i]), y = 60000,
         labels = scenarios[i], pos=1)
  }
  
  mtext(text = "undershoot", side = 2, line = par()$mgp[2]+1, adj = 0, cex = 0.8)
  
  abline(h=0, col="black")
  
  for(i in 1:dim(sa_catch)[1]){
    #abline(h=res.[i, paste("TAC", inclYear,sep='')], col="black", lty=2)
    abline(h=sa_tac[i,], col=pal[i], lty=2)
  }
  
  for(i in 1:dim(sa_catch)[1]){
    mtext(side=4, line = 0.5, at=sa_tac[i,], col=1, text=i, cex=0.7, las=2)
  }
  
  for(i in 1:length(scenarios)){
    rect((i-1)*(dim(sa_catch)[1]+1)+0.7, min(sa_undershoot)*1.1, 
         i*(dim(sa_catch)[1]+1)+.2, 60000, 
         lty=1)
  }
  
  par(mar=c(2,0,0,1))
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  legend("bottomleft", fill=pal, cex=1, bty='n',
         legend= stockLabels, ncol=1)
  
}



## Define assessment year
ay <- 2020


summary_advice <- read.csv("results/summary_advice.csv")
summary_advice$landings <- as.numeric(summary_advice$landings)

#summarise nep
summary_advice$stock[grep("nep.",summary_advice$stock)] <- "nep.27.7bk"

summary_advice <- summary_advice %>% select(stock,year,scenario,catch,discards,landings,tac)%>%  group_by_at(vars(-catch,-discards,-landings,-tac)) %>% summarise(catch=sum(catch,na.rm = T),landings=sum(landings,na.rm = T),discards=sum(discards,na.rm = T),tac=sum(tac,na.rm = T)) %>% ungroup()


stockVector <- unique(summary_advice$stock)
stockLabels <- stockVector

## Scenarios
scenarioVector <- unique(summary_advice$scenario)


## Define plotting colours
pal_all_region <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                    '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')




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



###CS 2020
##wrong fromat need to swing out
AdviceResults <- read.csv("../2020_CS_MixedFisheriesAdvice/results/data4advicefig.csv")


AdviceResults2 <- AdviceResults %>% select(-X) %>% pivot_wider(names_from = value,values_from = data)
names(AdviceResults2)[names(AdviceResults2)=="sc"] <- c("scenario")
AdviceResults2 <- AdviceResults2 %>% select(scenario,year,stock,catch,discards,landings) %>% filter(scenario %in% c("min","max","sq_E","had.27.7b-k" ))

AdviceResults2$discards[is.na(AdviceResults2$discards)==T] <- 0
AdviceResults2$landings[is.na(AdviceResults2$landings)==T] <- 0

for(i in 1:length(AdviceResults2$catch)){
if(is.na(AdviceResults2$catch[i])==TRUE){
  AdviceResults2$catch[i] <- AdviceResults2$landings[i]+AdviceResults2$discards[i]
}
}

##maybe this needs to be set to 1
# AdviceResults2$landings <- 1
# AdviceResults2$discards <- 0
#need to add tacs

  #code from CS2020    
# assign("TAC_cod.27.7e-k",0)
# assign("TAC_had.27.7b-k", 18382)
# assign("TAC_whg.27.7b-ce-k", 5261)            
# assign("TAC_meg.27.7b-k8abd", 19184)
# assign("TAC_mon.27.78abd", 34579)
# assign("TAC_sol.27.7fg", 1413)
# `TAC_nep.27.7bk` <-  7813

  AdviceResults2$tac <-NA 
  AdviceResults2$tac[AdviceResults2$stock =="cod.27.7e-k"] <- 0
  AdviceResults2$tac[AdviceResults2$stock =="had.27.7b-k"] <- 18382
  AdviceResults2$tac[AdviceResults2$stock =="whg.27.7b-ce-k"] <- 5261
  AdviceResults2$tac[AdviceResults2$stock =="meg.27.7b-k8abd"] <- 19184
  AdviceResults2$tac[AdviceResults2$stock =="mon.27.78abd"] <- 34579
  AdviceResults2$tac[AdviceResults2$stock =="sol.27.7fg"] <- 1413
  AdviceResults2$tac[AdviceResults2$stock =="nep.27.7bk"] <- 7813
  


AdvicestockVector <- unique(AdviceResults2$stock)
AdvicestockLabels <- AdvicestockVector

## Scenarios
AdvicescenarioVector <- c("min", "max", "sq_E","had.27.7b-k")

# AdviceResults2 <- AdviceResults2 %>%
#   arrange(match(scenario, c("min", "max", "sq_E","had.27.7b-k")))

#' ### Mixed fisheries projections by stock and scenario CS2020 ADVICE
png("figures/figProj_ADVICE_2020.png", width = 7, height = 6, units = "in", res = 400)
underOverPlot(inclYear  = ay+1, 
              inclStock = AdvicestockVector, 
              inclScen  = AdvicescenarioVector, 
              var = "catch", 
              resObj = AdviceResults2, 
              stockLabels =  paste0(1:length(AdvicestockLabels),":",AdvicestockLabels),
              pal = pal_all_region[1:length(AdvicestockLabels)])
dev.off()


#####Tables
summary_advice2 <- summary_advice %>% filter(year %in% c(2020:2022)) %>% select(stock,year,scenario,tac,catch) %>% mutate(catch=round(catch,0),tac=round(tac,0))
names(summary_advice2)
AdviceResults3 <- AdviceResults2 %>% select(stock,year,scenario,catch)%>% mutate(catch=round(catch,0))
names(AdviceResults3)
names(AdviceResults3)[names(AdviceResults3)=="catch"] <- "CS2020 catch" 

summary_advice2$scenario[summary_advice2$scenario == "fixedEffort"] <- "fixedEffort (sq_E)"
AdviceResults3$scenario[AdviceResults3$scenario == "sq_E"] <- "fixedEffort (sq_E)"

FLBEI_CS2020 <- full_join(summary_advice2,AdviceResults3)
write.csv(FLBEI_CS2020,"results/FLBEI_CS2020_effort_table.csv")



# Now do one for the effort_all plots -------------------------------------
#need this to exstract the data in the right format

plotEffortLim <- function(ay,
                          fleetObj,
                          fleetStkObj,
                          ssaObj,
                          inclStock   = NULL, 
                          stockLabels = NULL,
                          pal         = NULL) {
  
  # Define unspecified properties
  # -----------------------------
  #browser()
  ## define stocks to be plotted
  if (is.null(inclStock)) {
    inclStock <- unique(fleetStkObj$stock)
  }
  
  ## define colour palette
  if (is.null(pal)) {
    pal <- pals::brewer.paired(12)[1:length(inclStock)]
  }
  
  ## define stock labels
  if (is.null(stockLabels)) {
    stockLabels <- paste0(1:length(inclStock),":",inclStock[order(inclStock)])
  }
  
  # Some validation checks
  # -----------------------
  
  ## Check if stock labels and stocks to include match
  if (length(stockLabels) != length(inclStock)) {
    stop("stockLabels and inclStock lengths differ!")
  }
  
  fleetStkObj <- fleetStkObj %>% 
    filter(year == ay+1) %>% 
    select(scenario, fleet, stock, catch, tacshare)
  
  if(sum(duplicated(fleetStkObj[,c("scenario","fleet","stock")])) > 0) {
    stop("fleetStkObj: duplicated combinations of scenario, fleet and stock")
  }
  
  fleetObj <- fleetObj %>% 
    filter(year == ay+1) %>% 
    select(scenario, fleet, effort)
  
  if(sum(duplicated(fleetObj[,c("scenario","fleet")])) > 0) {
    stop("fleetStkObj: duplicated combinations of scenario and fleet")
  }
  
  ssaObj <- ssaObj %>%
    filter(year == ay+1) %>%
    select(stock, catch) %>%
    rename(catch_ssa = catch)
  
  # combine data
  # ------------
  
  scaledEffort <- fleetStkObj %>% 
    left_join(fleetObj, by = c("scenario", "fleet")) %>%
    left_join(ssaObj, by = "stock") %>%
    filter(scenario == "fixedEffort") %>%
    mutate(tac_ssa = tacshare * catch_ssa,
           effort_ssa = effort * tac_ssa /catch)
  
  # NOTE: This calculation gives concordant results across most scenarios 
  # (which suggests that the model uses a fixed CPUE to calculate catch given some estimation of effort) - 
  # except for "MaxProfitTAC2020" where very different results are obtained. Why is this??
  
  scaledEffort <- scaledEffort %>%
    group_by(fleet) %>%
    mutate(limitation = "intermediate",
           limitation = ifelse(effort_ssa == min(effort_ssa), "choke", limitation),
           limitation = ifelse(effort_ssa == max(effort_ssa), "least", limitation))
  write.csv(scaledEffort,"results/scaledEffort4MixfishBar.csv")
  
  scaledEffort %>%
    ggplot(aes(x = stock, y = effort_ssa, colour = limitation, fill = stock)) +
    geom_bar(stat = "identity", size = 1, alpha = 0.7) +
    geom_hline(aes(yintercept = effort), linetype = 2) +
    facet_wrap(fleet ~ ., scales = "free_y", ncol = 3) +
    scale_color_manual(values = c('red', NA, 'green')) +
    xlab("Stock") +
    ylab('KW days (000)') +
    labs(fill = 'Effort stock', 
         color = 'Limiting Stock') +
    theme_bw() + 
    theme( axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3, size = 7),
           panel.grid = element_blank(),
           text = element_text(size = 9), 
           strip.text = element_text(size = 9)) +
    scale_colour_manual(name = "Effort stock", 
                        values = pal,
                        aesthetics = c("fill"))
}



# -------------------------------------------------------------------------
#Not actually that usefull
# summary_fleet <- read.csv("results/summary_fleet.csv")
# summary_fleetstock <- read.csv("results/summary_fleetstock.csv")
# summary_ssa  <- read.csv("results/summary_ssa.csv")
# 
# flt_list <- grep("_fleet", unique(summary_fleet$fleet), value = TRUE, invert = TRUE)
# 
# plotEffortLim(ay = ay,
#               fleetObj    = summary_fleet %>% filter(fleet %in% flt_list),
#               fleetStkObj = summary_fleetstock %>% filter(fleet %in% flt_list),
#               ssaObj      = summary_ssa)
# 
# 
# 
# eff <- read.csv("../2020_CS_MixedFisheriesAdvice/results/report_effort_all_table.csv")
# #so this is taken from the scaled effort plot function witha a write.csv stuck in it
# scaledEffort <- read.csv("results/scaledEffort4MixfishBar.csv")
# 
# 
# names(eff)
# names(scaledEffort)
# 
# scaledEffort2 <- scaledEffort %>% select(stock,fleet,scenario,effort,limitation) %>% filter(scenario =="fixedEffort")
# eff2 <- eff %>%  select(stock,fleet,value,choke)
# 
# names(eff2)[names(eff2)=="value"] <- "CS2020_effort"
# names(eff2)[names(eff2)=="choke"] <- "CS2020_choke"
# 
# effort_comp_table <- full_join(scaledEffort2,eff2)
# write.csv(effort_comp_table,"results/effort_comp_table.csv")
