################################################################
#### Compare SSB TAC year from single stock advice and MIXFISH scenarios
#library(tidyverse)
library(dplyr)
rm(list=ls())

## load inputs
load("results/01_Reproduce_the_advice_Celtic_Sea_2020tier12nepnewLOUsingSAM.Rdata")

load("results/04_FCube_Forecasts_Celtic_Sea_2020tier12nepnewLOUsingSAMSkipIntYr_FcubeAllObjects.Rdata")

### TO BE CHANGED!!!!!
yr.now<-2020
yr.now <- yr.now + 2 ## This ref is wrong, should be ssb following scenario

stocksToBePlotted <- c("cod.27.7e-k","had.27.7b-k","meg.27.7b-k8abd","mon.27.78abd","sol.27.7fg", "whg.27.7b-ce-k")
Legend <- c("1:cod.27.7e-k","2:had.27.7b-k","3:meg.27.7b-k8abd","4:mon.27.78abd","5:sol.27.7fg", "6:whg.27.7b-ce-k")

stock_order <- as.data.frame(matrix(c(stocksToBePlotted,c(1:length(stocksToBePlotted))),ncol=2))
colnames(stock_order) <- c("stock","order")
####################################################

resSSB<-subset(results,value=="ssb" & year==yr.now)
unique(resSSB$stock)
unique(resSSB$sc)
resSSB<-resSSB[,c("sc","stock","data")]
resSSB2<-reshape(resSSB,direction='wide',idvar="stock",timevar="sc")
names(resSSB2) <- c("stock","01_Reproduce_the_advice_Celtic_Sea_2020","min" ,"max","sq_E" ,"val",
 "had.27.7b-k","whg.27.7b-ce-k", "cod_FARMSY")


##### order table to fit with stock orders in the other tables
resSSB2<- left_join(resSSB2,stock_order, by = "stock")
resSSB2 <- resSSB2[order(resSSB2$order),]

resSSB3<-as.matrix(resSSB2[,-c(1:2,dim(resSSB2)[2])])/resSSB2[,2]
#colnames(resSSB3)<-unlist(strsplit(as.vector(colnames(resSSB3)),split="data."))[c((1:length(colnames(resSSB3)))*2)]

#pal <- c("#D62618", "darkslategray3", "darkolivegreen1", "antiquewhite3", "coral1", "aquamarine") #gray(c((1:(length(row.names(resSSB3))))/(length(row.names(resSSB3)))))


pal <- pals::brewer.paired(12)[1:length(stocksToBePlotted)]

png(filename=file.path(plot.path,"Fig_XXX_SSB.png"), width = 280, height = 180, units='mm', res = 300)
mp<-barplot(resSSB3, col=pal, beside = TRUE,legend = F, ylim=c(0,max(resSSB3)+0.5),ylab=paste("predicted SSB",yr.now,"relative to baseline"), cex.names = 1) #, cex.axis = 0.2)

legend("top",fill=pal,cex=1.2,bty='n',
        legend = Legend,ncol=4)
abline(h=1, col="black")

mtext(side = 1, at =mp, line = 0.01,text = rep(formatC(c(1:length(stocksToBePlotted))),5), col = "black",cex=1)

dev.off()

