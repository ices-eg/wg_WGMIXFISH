#################################################################################
#################################################################################

# Radar plots of max / min effort by fleet
# Paul Dolder
# 26/05/16

#################################################################################
#################################################################################

rm(list=ls())
library(FLBEIA)
library(ggplot2) ; library(dplyr)

load("results/01_Reproduce_the_advice_Celtic_Sea_2020tier12nepnewLOUsingSAM.Rdata")

#################################################################################
# Read from all outputs for the effort.stock fule

res <- "results/04_FCube_Forecasts_Celtic_Sea_2020tier12nepnewLOUsingSAMSQ_E_FcubeAllObjects.Rdata"
yearNow <- 2020
##################################################################################


tableREf <- data.frame(stock= c(dem.names,nep.names),
                       order = c(1:6, paste(7,1:6, sep = "_")))



plotRose <- function(res, yearNow){
  
  # Radar plots of max / min effort by fleet
  # Paul Dolder
  # 26/05/16
  
  library(ggplot2) ; library(dplyr)
  
  
  ###
  # Read from all outputs for the effort.stock fule
  #res <- load('../results/03_Fcube_Projection_LO_FcubeAllObjects.Rdata')
  res <- load(paste0(res))
  
  #load("03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  ###
  
  n.stk  <- length(rownames(effort.stock[[1]][[1]][,,1]))
  n.fl   <- length(colnames(effort.stock[[1]][[1]][,,1]))
  yr.now <- as.character(yearNow)
  
  names(effort.stock)
  
  effort <- data.frame(fleet = rep(colnames(effort.stock[[1]][[1]][,,1]),each = n.stk), 
                       stock = rep(rownames(effort.stock[[1]][[1]][,,1]), times = n.fl),
                       data = as.vector(effort.stock[[1]][[1]][,,1]))
  
  sqE    <- res.effort[(res.effort$scenario=="sq_E" & res.effort$year==yr.now),]
  
  effort$sq <- sqE$effort[match(effort$fleet,sqE$fleet)]
  
  effort$rel <- effort$data/effort$sq
  
  ## Use max effort
  # max effort by fleet
  maxE <- filter(res.effort,scenario=="max", year==as.character(as.numeric(yr.now)+1))
  effort$max <- maxE$effort[match(effort$fleet,maxE$fleet)]
  
  effort$rel <- effort$data/effort$max
  
  ## Identify choke per fleet
  chokes<-data.frame(fleet=names(sapply(by(effort$rel,effort$fleet,FUN=min, na.rm=TRUE),"[")),
                     data=sapply(by(effort$rel,effort$fleet,FUN=min, na.rm=TRUE),"["))
  
  ## Identify least per fleet
  least<-data.frame(fleet=names(sapply(by(effort$rel,effort$fleet,FUN=max, na.rm=TRUE),"[")),
                    data=sapply(by(effort$rel,effort$fleet,FUN=max, na.rm=TRUE),"["))
  
  chokes$choke <- "choke"
  least$choke <- "least"
  
  effort$choke <- chokes$choke[match(paste(effort$fleet,effort$rel),
                                     paste(chokes$fleet,chokes$data))]
  
  effort$least <- least$choke[match(paste(effort$fleet,effort$rel),
                                    paste(least$fleet,least$data))]
  
  effort$rel[is.na(effort$rel)] <- NA
  
  effort$Limitation <- "interm."
  effort$Limitation[which(!is.na(effort$choke))] <- "choke"
  effort$Limitation[which(!is.na(effort$least))] <- "least"
  effort$Limitation <- factor(effort$Limitation)
  levels(effort$Limitation)
  
  effort$fleet <- factor(effort$fleet)
  
  
  ###
  ## The plots
  ###
  
  
  
  # plots
  
  effort2 <- merge(effort, tableREf)
  #effort2$order <- as.numeric(as.character(effort2$order))
  effort2 <- effort2[order(effort2$order),]
  
  print(ggplot(filter(effort2, fleet != "OTH_OTH"), aes(x=order, y = rel, col=Limitation)) +
    facet_wrap(~fleet, ncol=6) +
    scale_color_manual(values=c("red", "grey80", "green"), guide = guide_legend(
      direction = "horizontal",
      title.position = "top")) + 
    geom_hline(yintercept = 1, lty=1, col=8) +
    geom_bar(fill=8,stat="identity", lwd=0.25) +
    theme_bw() + 
    coord_polar() +
    xlab("") +
    ylab("") +
    theme(strip.text.x = element_text(size = rel(0.8)), 
      axis.text.x = element_text(
        size = rel(0.7), hjust=1,
        angle = 360/(2*pi)*rev( pi/2 + seq( pi/n.stk, 2*pi-pi/n.stk, len=n.stk))),
      legend.position="bottom",
      legend.margin=margin(c(1,1,1,1)),
      axis.text.y = element_blank()))
    ggsave(file=file.path(plot.path,"Effort_radar.png"),width = 7, height = 9)
 
  
  return(effort2)
}


plotRose(res, yearNow)

##################################
## 
## Dorleta's bar plot 
##
##################################

load("results/04_FCube_Forecasts_Celtic_Sea_2020tier12nepnewLOUsingSAMSQ_E_FcubeAllObjects.Rdata")


is.choke <- function(x){
  ma <- mi <- rep(FALSE, length(x))
  
  ma[which.max(x)] <- TRUE
  mi[which.min(x)] <- TRUE
  
  ma <- ifelse(ma, 'least', ma)
  mi <- ifelse(mi, 'choke', mi)
  
  x <- ifelse(ma == 'least', ma, ifelse(mi == 'choke', mi,  'intermediate'))
  return(x)
}

# identify the min and max for each stock and replace SP by ES.
# eff <- eff %>% group_by(fleet, year) %>% filter(substr(fleet,1,2) != 'OT') %>% 
#   mutate(choke = is.choke(value)) %>% mutate(fleet = gsub('SP', 'ES', fleet ))


eff <- reshape2::melt(eff.st) %>% group_by(Var2) %>%  mutate(choke = is.choke(value)) 
colnames(eff) <- c("stock", "fleet", "id", "value", "choke")

eff$stock <- factor(eff$stock, levels = c(unique(sort(grep("nep",eff$stock, value = TRUE, invert = TRUE))),
                              unique(sort(grep("nep",eff$stock, value = TRUE)))))

## status quo effort

sqE <- sapply(fleets, function(x) mean(x@effort[,ac(2017:2019)]))

## merge
eff$sqE <- sqE[match(eff$fleet, names(sqE))]
write.csv(eff,"results/report_effort_all_table.csv")

pal <- pals::brewer.paired(12)


ggplot(eff, aes(x = stock, y = value, fill = stock, color = choke, group = fleet)) + 
  facet_wrap(fleet~., scales = 'free', ncol = 4) + geom_bar(stat = 'identity',  size = 1, alpha = 0.7) + 
  geom_line(aes(y=sqE), colour = "black", linetype = "dashed") + 
  scale_fill_manual(values= pal) +
  scale_color_manual(values = c('red', NA, 'green')) + 
  ylab('KW days (000)') +  
  labs(fill = 'Effort stock', color = 'Limiting Stock') + 
  theme_bw() + theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3),
                     panel.grid = element_blank())

ggsave(paste('plots/report_effort_', 'all', '.png', sep = ""), width = 35, height = 35, units = 'cm')


