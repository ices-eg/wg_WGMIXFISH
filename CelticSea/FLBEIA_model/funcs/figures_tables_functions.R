#' ---
#' title: "Celtic Sea mixed fisheries - tables and figures"
#' author: "MIXFISH Celtic Sea subgroup"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#' 
#' Summary
#' =======
#' 
#' This script generates the figures and tables (together with the associated captions) required by the mixed fisheries advice and reporting documents for the Celtic seas ecoregion sub-group.
#' 
#' The functions heavily depend on code by Vanessa Trijoulet & Paul Dolder.
#' 
#' Load required packages
#' ======================
#' 

require(tidyverse)

#' Define plotting functions
#' =========================
#' 
#' Function: underOverPlot()
#' -------------------------
#' 
#' This is a minimally recoded version of the function (?author) to plot estimates of potential catches of stock given mixed fishery scenarios.
#' 
#' Function takes as arguments:
#' 
#'  - @param inclYear  - (int) the year for which estimated catches given mixed fishery scenarios are to be plotted
#'  - @param inclStock - (chr vector) the stocks for which estimated catches given mixed fishery scenarios are to be plotted
#'  - @param inclScen  - (chr vector) the mixed fishery scenarios for which estimated catches are to be plotted
#'  - @param var       - (chr) values can be "catch" or "landings". Defines the data to be plotted (NOT OPERATIONAL)
#'  - @param resObj    - (dataframe) FLBEIA  model advice summary table
#'  - @param stockLabels - (chr vector) **Optional** vector of stock labels associated with specified stocks to be plotted
#'  - @param pal         - (chr vector) **Optional** vector of colours associated with specified stocks to be plotted
#'  
#' Function returns:
#' 
#'  - Base plot bar graph

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
    spread(scenario, value) %>%
    arrange(stock) %>%
    column_to_rownames("stock") %>%
    as.matrix()
  
  ## calculate overshoot matrix
  sa_overshoot <- sa %>% 
    select(stock, scenario, overshoot) %>%
    spread(scenario, overshoot) %>%
    arrange(stock) %>%
    column_to_rownames("stock") %>%
    as.matrix()
  
  ## calculate undershoot matrix
  sa_undershoot <- sa %>%
    select(stock, scenario, undershoot) %>%
    spread(scenario, undershoot) %>%
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
                ylim = c(min(sa_undershoot)*1.15, max(sa_catch*1.25)), 
                ylab = paste("predicted", ifelse(var=="catch", "catch", "landings"), inclYear), 
                names.arg = rep('', ncol(sa_catch[,scenarios])))
  mp <- barplot(sa_overshoot[,scenarios], col=pal, beside = TRUE, legend = F, add=T, names.arg = rep('', ncol(sa_catch[,scenarios])))
  mp <- barplot(sa_undershoot[,scenarios], col=pal, beside = TRUE, legend = F, add=T, names.arg = rep('', ncol(sa_catch[,scenarios])))
  
  mtext(side = 1, at = mp, line = 0.01, 
        text = rep(formatC(c(1:dim(sa_catch)[1])),5), col = "black", cex = 0.8)
  
  mtext(text = "Scenarios: ", side = 3, line = 0.5, adj = 0)
  
  # text(mp[1,1], max(res.[,scenarios])*1.1, "Scenarios: ", pos=4)
  for (i in 1:length(scenarios)){
    text(x = mean(mp[,i]), y = max(sa_catch[,scenarios])*1.1,
         labels = scenarios[i], pos=3)
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
         i*(dim(sa_catch)[1]+1)+.2, max(sa_catch[,scenarios])*1.2, 
         lty=1)
  }
  
  par(mar=c(2,0,0,1))
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  legend("bottomleft", fill=pal, cex=1, bty='n',
         legend= stockLabels, ncol=1)
  
}

#' Function: ggunderOverPlot()
#' -------------------------
#' 
#' This is a minimally recoded version of the function underOverPlot (?author) to plot estimates of potential catches of stock given mixed fishery scenarios using the ggplot suite.
#' 
#' Function takes as arguments:
#' 
#'  - @param inclYear  - (int) the year for which estimated catches given mixed fishery scenarios are to be plotted
#'  - @param inclStock - (chr vector) the stocks for which estimated catches given mixed fishery scenarios are to be plotted
#'  - @param inclScen  - (chr vector) the mixed fishery scenarios for which estimated catches are to be plotted
#'  - @param var       - (chr) values can be "catch" or "landings". Defines the data to be plotted (NOT OPERATIONAL)
#'  - @param resObj    - (dataframe) FLBEIA  model advice summary table
#'  - @param stockLabels - (chr vector) **Optional** vector of stock labels associated with specified stocks to be plotted
#'  - @param pal         - (chr vector) **Optional** vector of colours associated with specified stocks to be plotted
#'  
#' Function returns:
#' 
#'  - ggplot bar graph

ggunderOverPlot <- function(inclYear  = NULL, 
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
  
  ## define facet labels
  lineLabels <- sa %>% 
    group_by(stock, tac) %>%
    summarise() %>%
    ungroup() %>%
    mutate(stockLab = 1:n())
  
  ## duplicate for each facet
  lineLabels <- bind_rows(lapply(1:length(inclScen), function(x){
    lineLabels %>%
      mutate(scenario = inclScen[x])
  }))
  
  lineLabels$stockLab[lineLabels$scenario != inclScen[length(inclScen)]] <- ""
  
  # --------------
  # PLOT
  # --------------
  
  ## Define stock labels if none available
  if(is.null(stockLabels)) {
    stockLabels =  paste0(1:length(stockLabels),":",stockLabels)
  }
  
  sa %>%
    ggplot() +
    geom_bar(aes(x = stock, y = value), 
             colour = "black",
             fill   = "gray80",
             stat = "identity") +
    geom_bar(aes(x = stock, y = overshoot, fill = stock), 
             colour = "black",
             stat = "identity") +
    geom_bar(aes(x = stock, y = undershoot, fill = stock), 
             colour = "black",
             stat = "identity") +
    geom_hline(aes(yintercept = tac, colour = stock), 
               linetype = 2, 
               show.legend = FALSE) +
    labs(tag = "Scenarios:") +
    geom_text(aes(y = tac,
                  label = stockLab),
              x = length(stockLabels)+0.9,
              data = lineLabels) +
    facet_grid(~factor(scenario, levels = inclScen)) +
    scale_colour_manual(values = pal) +
    scale_fill_manual("",
                      values = pal,
                      labels = stockLabels) +
    scale_x_discrete("", labels = 1:length(stockLabels)) +
    scale_y_continuous(paste0("predicted catch ",inclYear)) +
    theme(legend.position = "right",
          plot.tag.position = c(0.1, 1),
          panel.border = element_rect(colour = "black",
                                      fill = NA),
          panel.background = element_blank(),
          strip.background = element_blank()) +
    coord_cartesian(xlim = c(1, length(stockLabels)), # This focuses the x-axis on the range of interest
                    clip = 'off')
  
  
  
}

#' Function: plotEffortLim()
#' ---------------------
#' 
#' Function calculates and plots the effort required by fleets to reach single-stock advice.
#' 
#' Function requires as arguments:
#' 
#'  - @param fleetObj A dataframe containing the following variables:
#'    - X
#'    - X
#'    - X
#'    
#' The function returns:
#' 
#'  - A ggplot figure.

## I need to somehow sensibly merge the stock data with the fleet data .... which fleets fish which stocks...???
## Park this for now --- some issue with deriving effort for all fleets...

plotEffortLim <- function(ay,
                          fleetObj,
                          fleetStkObj,
                          ssaObj,
                          inclStock   = NULL, 
                          stockLabels = NULL,
                          pal         = NULL) {
  
  # Define unspecified properties
  # -----------------------------
  
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
  
  scaledEffort %>%
    group_by(fleet) %>%
    mutate(limitation = "intermediate",
           limitation = ifelse(effort_ssa == min(effort_ssa), "choke", limitation),
           limitation = ifelse(effort_ssa == max(effort_ssa), "least", limitation)) %>%
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

#' Function: plotScenSSB()
#' ---------------------
#' 
#' Function (author: ???) to plot estimated scenario SSB expressed as ratio to single-stock advice forecast.
#' 
#' Function requires as arguments:
#' 
#' - @param resObj a dataframe containing the following variables
#'   - X
#'   - X
#'   - X
#'   
#' - @param inclStock       (chr vector) (optional) stocks to be selected 
#' - @param ay         (int) assessment year (YYYY)
#' - @param scBaseline (chr) name of baseline scenario

plotScenSSB <- function(ay, 
                        bioObj, 
                        ssaObj,
                        inclStock = NULL,
                        inclScen  = NULL,
                        stockLabels = NULL,
                        pal         = NULL){
  
  ## define stocks to be plotted
  if (is.null(inclStock)) {
    inclStock <- unique(bioObj$stock)
  }
  
  ## define scenarios to be plotted
  if (is.null(inclScen)) {
    inclScen <- unique(bioObj$scenario)
  }
  
  ## define colour palette
  if (is.null(pal)) {
    pal <- pals::brewer.paired(12)[1:length(inclStock)]
  }

  ## define stock labels
  if (is.null(stockLabels)) {
    stockLabels <- paste0(1:length(inclStock),":",inclStock[order(inclStock)])
  }
  
  ## Check if stock labels and stocks to include match
  if (length(stockLabels) != length(inclStock)) {
    stop("stockLabels and inclStock lengths differ!")
  }
  
  ## remove scientific notations in y axis
  options(scipen = 50, digits = 5) 
  
  ## future year for SSB
  fy <- ay +2
  
  ## Subset for stocks of interest
  bioObj <- bioObj %>%
    filter(stock    %in% inclStock,
           scenario %in% inclScen,
           year == fy)
  
  ## calculate predicted SSB relative to baseline
  ssaObjBaseline <- ssaObj %>%
    filter(year == fy) %>%
    select(stock, year, ssb) %>%
    rename(ssbBaseline = ssb)
  
  ## check that object is ok for joining
  if(sum(duplicated(ssaObjBaseline[,1:2])) > 0) stop("Duplicated stocks in ssaObj")
  
  ## Calculate relative ssb
  bioObjRel <- bioObj %>%
    left_join(ssaObjBaseline, by = c("stock", "year")) %>%
    select(stock, scenario, ssb, ssbBaseline) %>%
    mutate(ssbRel = ssb/ssbBaseline) %>%
    arrange(stock)
  
  # --------------
  # PLOT
  # --------------
  
  bioObjRel %>%
    ggplot() +
    geom_bar(aes(x = stock, y = ssbRel, fill = stock), 
             colour = "black",
             stat = "identity") +
    geom_hline(yintercept = 1) +
    facet_grid(~scenario) +
    scale_fill_manual("",
                      values = pal,
                      labels = stockLabels) +
    scale_x_discrete("", labels = 1:length(stockLabels)) +
    scale_y_continuous(paste0("predicted SSB ",fy," relative to single stock advice"), limits = c(0,NA)) +
    theme(legend.position = "top",
          panel.border = element_rect(colour = "black",
                                      fill = NA),
          panel.background = element_blank(),
          strip.background = element_blank())
  
}

#' Function: plotMetierLandings()
#' ---------------------
#' 
#' Function to plot the landings distribution of species by metier.
#' 
#' Function requires as arguments:
#' 
#' - @param metObj a dataframe containing the following variables
#'   - X
#'   - X
#'   - X
#' - @param ay         (int) assessment year (YYYY)

plotMetierLandings <- function(ay, 
                               metObj,
                               stockLabels = NULL,
                               pal = NULL) {
  
  ## define stocks to be plotted
  stks <- unique(metObj$stock)
  
  ## define stock labels
  if(is.null(stockLabels)) {
    stockLabels <- paste0(1:length(stks),":",stks)
  }
  
  ## define colour palette
  if (is.null(pal)) {
    pal <- pals::brewer.paired(12)[1:length(stks)]
  }
  
  metObj %>% 
    filter(year == ay-1) %>%
    select(fleet, metier, stock, landings, discards) %>%
    group_by(fleet, metier, stock) %>% 
    summarise(landings = unique(landings), 
              discards = unique(discards)) %>%
    mutate(metier = substr(metier,1,7)) %>%
    mutate(metier = ifelse(str_detect(metier,"Others"),"OTH", metier)) %>%
    group_by(metier, stock) %>%
    summarise(landings = sum(landings),
              discards = sum(discards)) %>%
    ggplot() +
    geom_bar(aes(x = metier, y = landings/1000, fill = stock), colour = "black", stat = "identity") +
    scale_y_continuous("Landings ('000 tonnes)") +
    scale_x_discrete("Metiers used by mixed-fisheries model") +
    scale_fill_manual(values = pal,
                      labels = stockLabels) +
    theme_bw() +
    theme(legend.position = c(0.9,0.7),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.title.align = 0.5,
          axis.text.x = element_text(angle = 90))
  
}

#' Function: plotPieLandings()
#' ---------------------
#' 
#' Function to plot the total landings by stock for the last data year in the form of a pie chart.
#' 
#' Function requires as arguments:
#' 
#' - @param bioObj a dataframe containing the following variables
#'   - X
#'   - X
#'   - X
#' - @param ay         (int) assessment year (YYYY)

plotPieLandings <- function(ay, 
                        bioObj,
                        stockLabels = NULL,
                        pal = NULL){
  
  ## define stocks to be plotted
  stks <- unique(bioObj$stock)
  
  ## define stock labels
  if(is.null(stockLabels)) {
    stockLabels <- paste0(1:length(stks),":",stks)
  }

  ## define colour palette
  if (is.null(pal)) {
    pal <- pals::brewer.paired(12)[1:length(stks)]
  }
  
  ## calculate percentage landings
  bioObj <- bioObj %>%
    filter(year == ay-1) %>%
    select(stock, scenario, landings) %>%
    arrange(stock) %>%
    group_by(stock) %>%
    summarise(landings = unique(landings)) %>%
    mutate(percLandings = landings/sum(landings)*100)
  
  # --------
  # plotting
  # --------
  
  layout(matrix(c(1,2),nrow = 1, ncol = 2), widths = c(2.5,1))
  par(mar=c(1,1,3,0))
  pie(bioObj$percLandings, labels = NA, col = pal, main = "Total landings by Stock")
  par(mar=c(3,0,2,2))
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  legend(1, 0.4, fill=pal, cex=0.7, legend= stockLabels, ncol=1, title = "Stock")
  
  
}


#' Define tabulation functions
#' =========================
#' 
#' Function: tableNomfun
#' ---------------------
#' 
#' Function to generate table on nomenclature that is used in the advice and reporting documents.

tableNomfun <- function(){
  
  return()
}

#' Function: tableSSB
#' ------------------
#' 
#' Function to generate table for SSB from single stock advice and mixed fishery scenarios.
#' 
#' Function takes as arguments:
#' 
#' - @param ay     single element vector for assessment year (YYYY)
#' - @param inclStock (optional) character vector of stocks to be subsetted for
#' - @param bioObj dataframe containing the following variables:
#'   - stock:    (chr) name of stock
#'   - scenario: (chr) simulation scenario associated with ssb
#'   - year:     (int) YYYY
#'   - ssb:      (num) Spawning stock biomass 

tableSSB <- function(ay, 
                     bioObj, 
                     ssaObj,
                     inclStock = NULL,
                     inclScen  = NULL) {
  
  ## calculate forecast year
  fy <- ay + 2
  
  # --------
  # tidy arguments and some checks
  # --------
  
  ## If subsetting of stocks is not specified
  if (is.null(inclStock)) {
    inclStock <- unique(bioObj$stock)
  }
  
  ## If subsetting of scenarios is not specified
  if (is.null(inclScen)) {
    inclScen <- unique(bioObj$scenario)
  }
  
  ## Check that all requested stocks are in data
  if(sum(!(inclStock %in% bioObj$stock)) > 0) {
    stop("inclStock: Not all specified stocks are represented in bioObj")
  }
  
  ## Check that all requested scenarios are in data
  if(sum(!(inclScen %in% bioObj$scenario)) > 0) {
    stop("inclScen: Not all specified scenarios are represented in bioObj")
  }
  
  ## process single stock advice
  ssaObjBaseline <- ssaObj %>%
    filter(year == fy, 
           stock %in% inclStock) %>%
    select(stock, ssb) %>%
    rename(`single stock advice` = ssb)
  
  ## check that object is ok for joining
  if(sum(duplicated(ssaObjBaseline[,1:2])) > 0) stop("Duplicated stocks in ssaObj")
  
  ## generate table
  bioObjTable <- bioObj %>%
        filter(year == fy, 
               stock    %in% inclStock,
               scenario %in% inclScen) %>%
        select(scenario, stock, ssb) %>%
        spread(scenario, ssb) %>%
    left_join(ssaObjBaseline, by = "stock")
  
  ## Re-order sequence of columns
  bioObjTable <- bioObjTable[,c("stock",
                                names(ssaObjBaseline)[-1],
                                inclScen)]
  
  
  return(bioObjTable)
}

#' Function: tableCatch
#' ------------------
#' 
#' Function to generate table for catch from single stock advice and mixed fishery scenarios.
#' 
#' Function takes as arguments:
#' 
#' - @param ay single element vector for assessment year (YYYY)
#' - @param resObj dataframe containing the following variables:
#'   - stock:    (chr) name of stock
#'   - scenario: (chr) simulation scenario associated with ssb
#'   - year:     (int) YYYY
#'   - catch:    (num) catch in ???
#' - @param inclStock (chr vector) (optional) stocks to be subsetted for
#' - @param inclScen  (chr vector) (optional) scenarios to be subsetted for

tableCatch <- function(ay, 
                       resObj, 
                       ssaObj,
                       inclStock = NULL,
                       inclScen  = NULL){

  ## calculate forecast year
  fy <- ay + 1
  
  ## If subsetting of stocks is not specified
  if (is.null(inclStock)) {
    inclStock <- unique(resObj$stock)
  }
  
  ## If subsetting of scenarios is not specified
  if (is.null(inclScen)) {
    inclScen <- unique(resObj$scenario)
  }
  
  ## Check that all requested stocks are in data
  if(sum(!(inclStock %in% resObj$stock)) > 0) {
    stop("inclStock: Not all specified stocks are represented in resObj")
  }
  
  ## Check that all requested scenarios are in data
  if(sum(!(inclScen %in% resObj$scenario)) > 0) {
    stop("inclScen: Not all specified scenarios are represented in resObj")
  }
  
  ## process single stock advice
  ssaObjBaseline <- ssaObj %>%
    filter(year == fy, 
           stock %in% inclStock) %>%
    select(stock, catch) %>%
    rename(`single stock advice` = catch)
  
  ## check that object is ok for joining
  if(sum(duplicated(ssaObjBaseline[,1:2])) > 0) stop("Duplicated stocks in ssaObj")
    
  ## generate table
  resObjTable <- resObj %>%
      filter(year == fy, 
             stock    %in% inclStock,
             scenario %in% inclScen) %>%
      select(scenario, stock, catch) %>%
      spread(scenario, catch) %>%
    left_join(ssaObjBaseline, by = "stock")
  
  ## Re-order sequence of columns
  resObjTable <- resObjTable[,c("stock",
                                names(ssaObjBaseline)[-1],
                                inclScen)]
  
  return(resObjTable)
}

#' Function: tableFbar
#' ------------------
#' 
#' Function to generate table for mean fishing mortality from single stock advice and mixed fishery scenarios.
#' 
#' Function takes as arguments:
#' 
#' - @param ay single element vector for assessment year (YYYY)
#' - @param bioObj dataframe containing the following variables:
#'   - stock:    (chr) name of stock
#'   - scenario: (chr) simulation scenario associated with ssb
#'   - year:     (int) YYYY
#'   - f:        (num) fbar
#' - @param inclStock (optional) character vector of stocks to be subsetted for
#' - @param inclScen  (optional) character vector of scenarios to be subsetted for

tableFbar <- function(ay, 
                      bioObj,
                      ssaObj,
                      inclStock = NULL,
                      inclScen  = NULL){
  
  ## calculate forecast year
  fy <- ay + 1
  
  ## If subsetting of stocks is not specified
  if (is.null(inclStock)) {
    inclStock <- unique(bioObj$stock)
  }
  
  ## If subsetting of scenarios is not specified
  if (is.null(inclScen)) {
    inclScen <- unique(bioObj$scenario)
  }
  
  ## Check that all requested stocks are in data
  if(sum(!(inclStock %in% bioObj$stock)) > 0) {
    stop("inclStock: Not all specified stocks are represented in bioObj")
  }
  
  ## Check that all requested scenarios are in data
  if(sum(!(inclScen %in% bioObj$scenario)) > 0) {
    stop("inclScen: Not all specified scenarios are represented in bioObj")
  }
  
  ## process single stock advice
  ssaObjBaseline <- ssaObj %>%
    filter(year == fy, 
           stock %in% inclStock) %>%
    select(stock, Fbar) %>%
    rename(`single stock advice` = Fbar)
  
  ## check that object is ok for joining
  if(sum(duplicated(ssaObjBaseline[,1:2])) > 0) stop("Duplicated stocks in ssaObj")
    
  ## generate table
  bioObjTable <- bioObj %>%
      filter(year == fy, 
             stock    %in% inclStock,
             scenario %in% inclScen) %>%
      select(scenario, stock, f) %>%
      spread(scenario, f) %>%
    left_join(ssaObjBaseline, by = "stock")
  
  ## Re-order sequence of columns
  bioObjTable <- bioObjTable[,c("stock",
                                names(ssaObjBaseline)[-1],
                                inclScen)]

  return(bioObjTable)
  
}

#' Function: tableRelCatch
#' -----------------------
#' 
#' Function to generate table showing estimated catch in each scenario relative to single stock advice.
#' 
#' Function takes as arguments:
#' 
#' - @param ay single element vector for assessment year (YYYY)
#' - @param resObj dataframe containing the following variables:
#'   - stock:    (chr) name of stock
#'   - scenario: (chr) simulation scenario associated with ssb
#'   - year:     (int) YYYY
#'   - catch:    (num) catch in ???
#' - @param ssaObj dataframe for single stock advice containing the following variables:
#'   - stock: (chr)
#'   - year:  (int) YYYY
#'   - catch: (num) catch in ???
#' - @param inclStock (optional) character vector of stocks to be subsetted for
#' - @param inclScen  (optional) character vector of scenarios to be subsetted for

tableRelCatch <- function(ay, 
                          resObj,
                          ssaObj,
                          inclStock = NULL,
                          inclScen  = NULL){
  
  ## NOTE... I NEED SINGLE STOCK ADVICE FOR THIS TO WORK!!!
  
  ## calculate forecast year
  fy <- ay + 1
  
  ## If subsetting of stocks is not specified
  if (is.null(inclStock)) {
    inclStock <- unique(resObj$stock)
  }
  
  ## If subsetting of scenarios is not specified
  if (is.null(inclScen)) {
    inclScen <- unique(resObj$scenario)
  }
  
  ## Check that all requested stocks are in data
  if(sum(!(inclStock %in% resObj$stock)) > 0) {
    stop("inclStock: Not all specified stocks are represented in resObj")
  }
  
  ## Check that all requested scenarios are in data
  if(sum(!(inclScen %in% resObj$scenario)) > 0) {
    stop("inclScen: Not all specified scenarios are represented in resObj")
  }
  
  ## process single stock advice
  ssaObjBaseline <- ssaObj %>%
    filter(year == fy, 
           stock %in% inclStock) %>%
    select(stock, catch) %>%
    rename(catchSSA = catch)
  
  ## check that object is ok for joining
  if(sum(duplicated(ssaObjBaseline[,1:2])) > 0) stop("Duplicated stocks in ssaObj")
  
  ## generate table
  resObjTable <- resObj %>%
      filter(year == fy, 
             stock %in% inclStock,
             scenario %in% inclScen) %>%
    left_join(ssaObjBaseline, by = "stock") %>%
      mutate(catchRel = catch/catchSSA) %>%
      select(scenario, stock, catchRel) %>%
      spread(scenario, catchRel)
  
  ## Re-order sequence of columns
  resObjTable <- resObjTable[,c("stock",
                                inclScen)]
  
  return(resObjTable)
}


#' Function: tableStockAdvice
#' --------------------------
#' 
#' Function to generate table
#' 
#' Function takes as arguments:
#' 
#' - @param ay single element vector for assessment year (YYYY)
#' - @param bioObj dataframe containing the following variavles:
#'   - 
#' - @param ssaObj dataframe containing the following variables:
#'   - stock
#'   - SSB

tableStockAdvice <- function(ay, 
                             bioObj, 
                             ssaObj, 
                             scenCoding = c("max"    = 1,
                                            "min"    = 2,
                                            "cod-ns" = 3,
                                            "sq_E"   = 4,
                                            "val"    = 5)){
  
  ## define years to use
  yearTAC <- ay + 1
  yearSSB <- ay + 2

  ## plot catches or landings?
  
  ## subset single stock advice for SSB and stock name
  
  ## Ensure aggregation across scenario, year, stock, catch category
  
  ## Extract catch data for TAC year
  
  ## Extract Fbar data for TAC year

  ## Extract SSB data for SSB year
  
  ## Calculate percentage difference in SSB from single stock advice
  mutate(percVarSSB = round(( tot$SSB - tot$SSBssa)/tot$SSBssa*100))
  
  ## update coding of scenarios for table
  for (ii in 1:length(scenCoding)){
    tot$test[tot$sc == names(scenCoding)[ii]] <- scenCoding[ii]
  }
  tot <- tot[order(tot$stock,tot$test),]
  
  ## Update coding of stocks
  tot$Basis <- rep(c('A','B','C','D', 'E'),length(unique(tot$stock)))

  ## select columns for table
  tot <- tot[,c("stock","catch", "Basis", "test", "Fbar",  "SSB", "percVarSSB")]
  
  return(tot)
  
}