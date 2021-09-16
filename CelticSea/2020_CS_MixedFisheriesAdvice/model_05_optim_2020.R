#######################################################################################
##
##
##  main code to run the optimizer
##  F to optimize: Ranges of MSY values
##
########################################################################################

rm(list=ls())
gc(reset=TRUE)

### libraries

library(genalg); library(lhs)
library(FLCore); library(FLash)
library(FLAssess); library(FLFleet)
library(ggplot2)

## Load the Intermediate year

source(file.path('funcs/02_Projection TAC year_function_2020.R'))
source(file.path("funcs/rbgaAmoi.R"))
  an<-function(x) {as.numeric(x)}
  Fc.silent = TRUE

## Do you want to run with a fixed zero catch for cod?
## Do you want to cap F upper for cod at reduced Fmsy ?
zeroCod <- FALSE
capped <- FALSE

### defining MSY ranges for all species
#### stock order: cod.27.7e-k, had.27.7b-k, whg.27.7b-ce-k

## Note, cod and whiting and capped because of SSB_TACyr
## haddock, megrim, monk and sole are not 

## Cod uncapped reduced
cod_low <- SSB_TACyr[["cod.27.7e-k"]]/BRPs[["cod.27.7e-k","Bmsytrigger"]] * BRPs["cod.27.7e-k","Fmsy_lower"]
cod_upp <- SSB_TACyr[["cod.27.7e-k"]]/BRPs[["cod.27.7e-k","Bmsytrigger"]] * BRPs["cod.27.7e-k","Fmsy_upper"]
cod_msy <- SSB_TACyr[["cod.27.7e-k"]]/BRPs[["cod.27.7e-k","Bmsytrigger"]] * BRPs["cod.27.7e-k","Fmsy"]

## whg
whg_low <- SSB_TACyr[["whg.27.7b-ce-k"]]/BRPs[["whg.27.7b-ce-k","Bmsytrigger"]] * BRPs["whg.27.7b-ce-k","Fmsy_lower"]
whg_upp <- SSB_TACyr[["whg.27.7b-ce-k"]]/BRPs[["whg.27.7b-ce-k","Bmsytrigger"]] * BRPs["whg.27.7b-ce-k","Fmsy_upper"]
whg_msy <- SSB_TACyr[["whg.27.7b-ce-k"]]/BRPs[["whg.27.7b-ce-k","Bmsytrigger"]] * BRPs["whg.27.7b-ce-k","Fmsy"]

FMSY<- c(cod_msy,
	 BRPs["had.27.7b-k","Fmsy"], 
	 BRPs["meg.27.7b-k8abd","Fmsy"],
	 BRPs["mon.27.78abd","Fmsy"],
	 BRPs["sol.27.7fg","Fmsy"],
	 whg_msy)

FMSY_min <- c(cod_low,
	      BRPs["had.27.7b-k","Fmsy_lower"],
	      BRPs["meg.27.7b-k8abd","Fmsy_lower"],
              BRPs["mon.27.78abd","Fmsy_lower"],
              BRPs["sol.27.7fg","Fmsy_lower"],
	      whg_low)

FMSY_max <- c(cod_upp,
	      BRPs["had.27.7b-k","Fmsy_upper"],
	      BRPs["meg.27.7b-k8abd","Fmsy_upper"],
              BRPs["mon.27.78abd","Fmsy_upper"],
              BRPs["sol.27.7fg","Fmsy_upper"],
	      whg_upp)

names(FMSY) <- names(FMSY_min) <- names(FMSY_max) <- dem.names

print("FMSY: ")
print(FMSY)

print("Ranges used: ")
print(rbind(FMSY_min, FMSY_max))

nStocks <- length(FMSY_min)   ##
lengthPop <- 50   ## 20 ## length of the first population to be used in LHS to optimize it
Iters <-  100 ## 100 number of iterations

run_the_optim<- FALSE 

if (run_the_optim)
{
init <- (optimumLHS(k=nStocks, n=lengthPop))
for (i in 1:nStocks){
init[,i] <- init[,i] * (FMSY_max[i]-FMSY_min[i]) + FMSY_min[i] ## optimumLHS sample in a uniform distribution, transforme it to be bounded by MSY ranges
}
hist(init[,1])


     monitor <- function(obj) {
         # plot the population
         xlim = c(min(FMSY_min), max(FMSY_max));
         ylim = c(min(FMSY_min), max(FMSY_max));
         plot(obj$population, xlim=xlim, ylim=ylim, xlab="pi", ylab="sqrt(50)");
     }

deb <- date()
rbga.res = rbgaAmoi(stringMin=FMSY_min,
	stringMax=FMSY_max,
	suggestions=init,
	popSize=lengthPop, iters=Iters,
	mutationChance=NA,
	elitism=NA,
	monitorFunc=NULL, evalFunc=FcubeDiffLandings2Scenarios,
	showSettings=FALSE, verbose=TRUE)
fin <- date()

dif<- ifelse(zeroCod, "diff2max-min_Cod0", "diff2max-min")
dif <- ifelse(capped, paste(dif, "capped", sep = "_"), paste(dif, "uncapped", sep = "_"))

save(rbga.res, file=paste("results/",paste0(dif, "iter_plus",".Rdata",sep=""),sep=""))
}




###################################################################################################################################################################################################
#  processing results and plotting
###################################################################################################################################################################################################

load("results/diff2max-min_uncappediter_plus.Rdata")
load("results/Advice_sheet_resultsdiff2max-min_uncapped.Rdata")

dif<- ifelse(zeroCod, "diff2max-min_Cod0", "diff2max-min")
dif <- ifelse(capped, paste(dif, "capped", sep = "_"), paste(dif, "uncapped", sep = "_"))


#### plot diff between min and max scenario for best indiv for each generation
png(filename=file.path("plots",paste0("optim convergence_",dif,".png")))
plot(rbga.res$best,main=paste("progress in the minimisation for",dif),xlab="generations" , ylab="diff max - min scenarios")
dev.off()


#### plot value of last population
png(filename=file.path("plots",paste0("optim_lastPop_",dif,".png")), width = 1600, height = 800)
boxplot(rbga.res$listPop[[length(rbga.res$listPop)]],names=names(dem.stock))
points(rbga.res$population[which.min(rbga.res$evaluations),], col='red',cex=2)
dev.off()
#
# last estimated fishing mortality
Flastyear <- sapply(dem.st.fwd,function(x) fbar(x)[,yr.assess])[dem.names]
FAdvice <- FMSY
## For cod its zero
FAdvice[["cod.27.7e-k"]] <- 0

png(filename=file.path('plots',paste0("optim in Fmsy range_",dif,".png",sep="")), width = 800, height = 600)
 plot(rbga.res$population[which.min(rbga.res$evaluations),],xaxt="n",col='red',cex=0,ylim=c(0,1.2),xlab="",ylab="F applied in Advice Year")
 axis(1,at=1:length(names(dem.stock)),labels=names(dem.stock))
 for (i in 1:6) segments(i,FMSY_min[i],i,FMSY_max[i],col="grey")
 points(FMSY_min,col="grey",pch="-",cex=5)
 points(FMSY_max,col="grey",pch="-",cex=5)
 points(rbga.res$population[which.min(rbga.res$evaluations),],col="green", pch=19, cex=2)
 points(FMSY,col="blue",pch=18,cex=2.5)
 points(Flastyear,col="darkorchid4",pch="+",cex=2)
 points(FAdvice, col="orange",pch="*",cex=4)
 legend(x="topright",pch=c(45,an(18),an(19),43,42),cex=1, pt.cex = 2,
        col=c("grey","blue","green","darkorchid4","orange"),
        c("Fmsy range","Fmsy","F range","current F","Fadvice2021") )

dev.off()

 ## Now get the single species advice catches corresponding to these Fs

 # --- --------    optim Ftarget within the ranges
 Ftargs  <-  c(rbga.res$population[which.min(rbga.res$evaluations),])
 names(Ftargs) <- dem.names

 print("Advised Range scenario F targets are: ")
 print(Ftargs)

 # source(file.path("funcs", '03_Projection TAC year_function_v1_returnBest_2020.R'))
 # STF     <-  FcubeDiffLandings2Scenarios_Best(TargetByStock=Ftargs)
 # results<-STF[[2]]
 # TACs<-STF[[1]]
 # 
 # save(STF, file=file.path("results",paste0("Range_STF",dif,".Rdata",sep="")))

 ## Now we calculate the single species catches with a stf with the Ftargs
 source(file.path("funcs", '04_ReproduceTheAdvice_Celtic_Sea_Optim_2020.R'))

 OptimAdvice <-  ReproduceAdviceonOptim(Ftargs)
 save(OptimAdvice, file=file.path("results",paste0("Advice_sheet_results",dif,".Rdata",sep="")))

 ## Advice sheet landings, catch, F and SSB
 print(OptimAdvice)

 OptimAdvice[OptimAdvice$year == yr.TAC & OptimAdvice$value == "landings",]
 OptimAdvice[OptimAdvice$year == yr.TAC & OptimAdvice$value == "discards",]
 OptimAdvice[OptimAdvice$year == yr.TAC & OptimAdvice$value == "catch",]
 OptimAdvice[OptimAdvice$year == yr.TAC & OptimAdvice$value == "Fbar",]
 OptimAdvice[OptimAdvice$year == yr.TACp1 & OptimAdvice$value == "ssb",]

 # remove nephrops from these plots
 stock.names <- stock.names[!stock.names == grep("nep", stock.names, value = T)]
 
 inds <- c("Landings","Discards","Catch","F","SSB")
 ss_results <- matrix(NA, nrow = length(stock.names), ncol = length(inds),
                      dimnames=list(stock.names, inds))

 singlespp <- read.csv(file.path('bootstrap','data','supporting_files','single_species_advice.csv'))
 singlespp <- subset(singlespp, stock %in% stock.names)
 singlespp$stock <- factor(singlespp$stock, levels = stock.names) # reorder
 singlespp <- singlespp[order(singlespp$stock),] # reorder

 # landings
 ss_results[,1] <- singlespp$landings[singlespp$year == yr.TAC]
 ss_results[,2] <- singlespp$discards[singlespp$year == yr.TAC]
 ss_results[,3] <- singlespp$catch[singlespp$year == yr.TAC]
 ss_results[,4] <- singlespp$Fbar[singlespp$year == yr.TAC]
 ss_results[,5] <- c(singlespp$ssb[singlespp$year == yr.TACp1]) 

 # The landings and SSB
 # OptimAdvice and ss_results

 Catch <- data.frame(sc = c(rep("single species advice", length(stock.names)),
                            rep("Optim advice", length(stock.names))),
                     stock = rep(stock.names,2),
                     catch = c(ss_results[,'Catch'],OptimAdvice[OptimAdvice$year==yr.TAC & OptimAdvice$stock %in% stock.names & OptimAdvice$value=='catch','data']),
                     ssb = c(ss_results[,'SSB'], OptimAdvice[OptimAdvice$year==yr.TACp1 & OptimAdvice$stock %in% stock.names & OptimAdvice$value=='ssb','data']))

 ## For cod, we need to uprate the landings by the same % as the single stock advice
 ## Need to do this in 02, 03 and 04 - now done PJD 18/06/2018

 # catch_ord <- Catch[order(Catch$stock),]
 catch_ord <- Catch
 catch_ord$stock <- factor(catch_ord$stock, levels = stock.names) # reorder
 catch_ord$catch <- as.numeric(catch_ord$catch)
 catch_ord$ssb <- as.numeric(catch_ord$ssb)
 
 catch_ord$sc[catch_ord$sc == "Optim advice"] <- "Range advice"
 catch_ord$sc[catch_ord$sc == "single species advice"] <- "Single species advice"
 catch_ord$sc <- factor(catch_ord$sc, levels = c("Single species advice", "Range advice")) # reorder
 
 catch_ord$Bmsytrigger <- NA
 for (i in row.names(BRPs)) {
   catch_ord[catch_ord$stock==i, "Bmsytrigger"] <- BRPs[i, "Bmsytrigger"]
 }
 
 ## Catch comparison plot

 ggplot(catch_ord, aes(stock, catch, fill = sc)) +
   geom_bar(stat = "identity", position = "dodge2", color = "black") +
   scale_fill_manual(values = c("blue", "green")) +
   theme_bw() +
   theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 10, vjust=0.9), axis.title.x = element_blank(),
         legend.title = element_blank(), legend.position = c(.8, .91),
         legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
   labs(y = "Catch advice (tonnes) in 2021", title = "Catch advice comparison")
 
 ggsave(filename=file.path('plots',paste0("catch_Comparison_",dif,".png",sep="")), width = 5, height = 5)


 ## SSB comarison plot
 
 ggplot(catch_ord) +
   geom_bar(aes(stock, ssb, fill = sc), stat = "identity", position = "dodge2", color = "black") +
   scale_fill_manual(values = c("blue", "green")) +
   theme_bw() +
   theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 10, vjust=0.9), axis.title.x = element_blank(),
         legend.title = element_blank(), legend.position = c(.8, .86),
         legend.background = element_blank(), legend.box.background = element_rect(colour = "black"),
         legend.spacing.y = unit(0.1, 'cm')) +
   labs(y = "Resultant SSB (tonnes) in 2022", title = "SSB advice comparison") +
   geom_errorbar(aes(x=stock, ymax=Bmsytrigger, ymin=Bmsytrigger, colour="MSY Btrigger"), lty = 2)
   
 ggsave(filename=file.path('plots',paste0("SSB_comparison_",dif,".png",sep="")), width = 5, height = 5)
 




 ## To extratc F values for advice sheet, the order is
 names(rbga.res[[2]])
 rbga.res$population[which.min(rbga.res$evaluations),]
 
