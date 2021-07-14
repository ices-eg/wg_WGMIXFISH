
###################################################################################################################################################################################################
#  processing results and plotting
###################################################################################################################################################################################################



rm(list=ls())
gc(reset=T)


source('model_04_00_Functions.r')


# resPath <- "output/"

ver1 <- "04_Fcube_SQIntYr__OPTIM"
dif <-"diff2max-min2" 

load(file=file.path("model",paste0("04_Optim_",ver1,dif,".Rdata",sep="")))


#####
# redefine plot.path (overwirtten while sourcing scirpts)
plot.path <- "output/04_Model/plots/"

# res.path <- file.path("North_Sea\\optim\\data\\")
# load(paste0(res.path,paste0(ver,".Rdata")))

# BRPs <- BRPs[-which(rownames(BRPs)=="WHG-NS"),] 
dem.st.fwd <- dem.st.fwd[rownames(BRPs)]  


#FMSY_min <- c(.2, .2,.2,.2,.2,.2,.2)
#FMSY_max <- c(.5, .5,.5,.5,.5,.5,.5)

# BRPs<-matrix(NA,nrow=length(dem.names),ncol=9,dimnames=list(dem.names,
#                                                             c("Flim","Fpa","Fmsy", "FmsyL","FmsyH","Fmgt","Blim","Bpa", "Btrigger"))) #yv add , Fmgt 
# BRPs["COD-NS",] <- c(Flim=0.54  ,  Fpa=0.39 ,  Fmsy=0.31  ,FmsyL=0.2  ,FmsyH=0.4,  Fmgt=NA    , Blim=107000   , Bpa=150000, Btrigger=150000)
# BRPs["HAD",]    <- c(Flim=0.384 ,  Fpa=0.274,  Fmsy=0.19  ,FmsyL=0.167    ,FmsyH=0.194,    Fmgt=NA    , Blim=94000    , Bpa=132000, Btrigger=NA)   # updated 2017 TB
# BRPs["PLE-EC",] <- c(Flim=0.5   ,  Fpa=0.36 ,  Fmsy=0.25  ,FmsyL=0.175  ,FmsyH=0.34,  Fmgt=NA    , Blim=18448    , Bpa=25826, Btrigger=NA)    # updated 2017 TB
# BRPs["PLE-NS",] <- c(Flim=0.516 ,  Fpa=0.369,  Fmsy=0.21  ,FmsyL=0.146  ,FmsyH=0.30,  Fmgt=0.30  , Blim=207288   , Bpa=290203, Btrigger=NA)   # updated 2017 TB
# BRPs["POK",]    <- c(Flim=0.564 ,  Fpa=0.403,  Fmsy=0.358 ,FmsyL=0.21  ,FmsyH=0.49,  Fmgt=NA    , Blim=107000   , Bpa=150000, Btrigger=NA)   # updated 2017 TB
# BRPs["SOL-EC",] <- c(Flim=0.359  ,  Fpa=0.256  ,  Fmsy=0.256   ,FmsyL=0.195  ,FmsyH=0.256,  Fmgt=NA    , Blim=13751       , Bpa=19251, Btrigger=19251)
# BRPs["SOL-NS",] <- c(Flim=0.63  ,  Fpa=0.44 ,  Fmsy=0.2   ,FmsyL=0.113  ,FmsyH=0.37,  Fmgt=0.2   , Blim=26300    , Bpa=37000, Btrigger=NA)
# BRPs["WHG-NS",] <- c(Flim=0.39  ,  Fpa=0.28 ,  Fmsy=0.172  ,FmsyL=0.158    ,FmsyH=0.172,    Fmgt=NA    , Blim=172741   , Bpa=241837 , Btrigger=NA)
# #BRPs["TUR",]    <- c(Flim=NA    ,  Fpa=NA   ,  Fmsy=0.27  ,Fmgt=NA    , Blim=2070     , Bpa=2900) #YV

### defining MSY ranges for all species
#### stock order: COD-NS, HAD-NS, PLE-NS, POK, SOL-NS, WHG-NS, TUR
FMSY_min <- data.frame(BRPs)$FmsyL#*MSY_AR
FMSY_max <- data.frame(BRPs)$FmsyH#*MSY_AR
FMSY <- data.frame(BRPs)$Fmsy#*MSY_AR

### fix for Sole EC where SSB < SSB Btrigger

# FMSY_min[rownames(BRPs)=="SOL-EC"] <-FMSY_min[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]
# # FMSY_max[rownames(BRPs)=="SOL-EC"] <-FMSY_max[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]
# FMSY_max[rownames(BRPs)=="SOL-EC"] <-FMSY[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]
# FMSY[rownames(BRPs)=="SOL-EC"] <-FMSY[rownames(BRPs)=="SOL-EC"] * as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2019",,,])/BRPs[rownames(BRPs)=="SOL-EC","Btrigger"]


FMSY_min[rownames(BRPs)=="COD-NS"] <-FMSY_min[rownames(BRPs)=="COD-NS"] * as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,"2021",,,])/BRPs[rownames(BRPs)=="COD-NS","Btrigger"]
FMSY_max[rownames(BRPs)=="COD-NS"] <-FMSY[rownames(BRPs)=="COD-NS"] * as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,"2021",,,])/BRPs[rownames(BRPs)=="COD-NS","Btrigger"]
FMSY[rownames(BRPs)=="COD-NS"] <-FMSY[rownames(BRPs)=="COD-NS"] * as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,"2021",,,])/BRPs[rownames(BRPs)=="COD-NS","Btrigger"]



#### plot diff between min and max scenario for best indiv for each generation
png(file=paste(plot.path,"optim convergence.png",sep=""))  
plot(rbga.res$best,main=paste("progress in the minimisation for",dif),xlab="generations" , ylab="diff max - min scenarios")
dev.off()

#### plot value of last population
#boxplot(rbga.res$listPop[[length(rbga.res$listPop)]],names=names(dem.stock),)	
#points(rbga.res$population[which.min(rbga.res$evaluations),], col='red',cex=2)
#


# FMSY_min <- data.frame(BRPs)$FmsyL   #*MSY_AR
# FMSY_max <- data.frame(BRPs)$FmsyH   #*MSY_AR


# last estimated fishing mortality
Flastyear <- sapply(dem.st.fwd,function(x) fbar(x)[,yr.assess])[dem.names]

# F to be used in the ballanced scenario
MSY_ballance <- BRPs[,"Fmsy"]
MSY_ballance[which(Flastyear>BRPs[,"FmsyH"])] <- BRPs[which(Flastyear>BRPs[,"FmsyH"]),"FmsyH"]
MSY_ballance[which(Flastyear<BRPs[,"FmsyL"])] <- BRPs[which(Flastyear<BRPs[,"FmsyL"]),"FmsyL"]

# namesStock <- c("cod 27.47d20","had 27.46a20","ple 27.7d","ple 27.420","pok 27.3a46","sol 27.4",
#                 "whg 27.47d","tur", "wit")


colnames(rbga.res$population)[colnames(rbga.res$population)=="COD-NS"] <- "cod 27.47d20"
colnames(rbga.res$population)[colnames(rbga.res$population)=="HAD"] <- "had 27.46a20"
colnames(rbga.res$population)[colnames(rbga.res$population)=="PLE-EC"] <- "ple 27.7d"
colnames(rbga.res$population)[colnames(rbga.res$population)=="PLE-NS"] <- "ple 27.420"
colnames(rbga.res$population)[colnames(rbga.res$population)=="POK"] <- "pok 27.3a46"
colnames(rbga.res$population)[colnames(rbga.res$population)=="SOL-NS"] <- "sol 27.4"
colnames(rbga.res$population)[colnames(rbga.res$population)=="WHG-NS"] <- "whg 27.47d"
colnames(rbga.res$population)[colnames(rbga.res$population)=="TUR"] <- "tur.27.4"
colnames(rbga.res$population)[colnames(rbga.res$population)=="WIT"] <- "wit.27.3a47d"


## Add advice year F to the plot
advice_data <- read.csv("bootstrap/data/Reproduce_The_Advice/single_species_advice.csv")
F_advice_yrTAC <- advice_data$Fbar[which(advice_data$year==yr.TAC)]
names(F_advice_yrTAC) <- advice_data$stock[which(advice_data$year==yr.TAC)]
F_advice_yrTAC <- F_advice_yrTAC[which(names(F_advice_yrTAC) %in% names(Flastyear))]
F_advice_yrTAC <- F_advice_yrTAC[names(Flastyear)] # same order than the rest


png(file=paste(plot.path,"optim in Fmsy range.png",sep=""))  
par(las=2, mar=c(7,5,4,2)) 

plot(rbga.res$population[which.min(rbga.res$evaluations),],xaxt="n",col='red',cex=0,ylim=c(.1,0.7),xlab="",ylab="F applied in Advice Year")	
# 
# plot(rbga.res$population[2,],xaxt="n",col='red',cex=0,ylim=c(0.1,0.7),xlab="",ylab="F applied in Advice Year")	

axis(1,at=1:length(names(dem.stock)),labels=colnames(rbga.res$population))
for (i in 1:9) segments(i,FMSY_min[i],i,FMSY_max[i],col="green")
points(FMSY_min,col="green",pch="-",cex=5)
points(FMSY_max,col="green",pch="-",cex=5)
points(rbga.res$population[which.min(rbga.res$evaluations),],col="red", pch=19, cex=2)
# points(rbga.res$population[which.min(rbga.res$evaluations),],col="red", pch=19, cex=2)

points(FMSY,col="blue",pch=18,cex=2.5)
points(Flastyear,col="darkorchid4",pch="+",cex=2)
#points(MSY_ballance, col="orange",pch="*",cex=4)

points(F_advice_yrTAC,col="orange", pch="*", cex=2)
legend(x="topright",pch=c(45,an(18),an(19),43,42),cex=1, pt.cex = 2,
       col=c("green","blue","red","darkorchid4", "orange"), 
       c("Fmsy range","Fmsy","F range","current F", paste0("advice ", yr.TAC)) )

dev.off()


### run the final Fcube for 
#     1) the Ftargets correspding to the results of the optimisation
#     2) the Ftargets corresponding to the ICES MSY approach


# --- --------    optim Ftarget within the ranges
 Ftargs  <-  c(rbga.res$population[which.min(rbga.res$evaluations),])
#Ftargs  <- c(rbga.res$population[2,])
STF     <- FcubeDiffLandings2Scenarios_Best(TargetByStock=Ftargs, c("max","min","COD-NS","sq_E"))
results <- STF[[2]]
TACs    <- STF[[1]]
SSBs    <- STF[[3]]


res.landings2 <- results[which(results$value=="landings" & results$year==now+1),]
res.landings2 <- res.landings2[,c("sc","stock","data")]
res.landings2 <- reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
rownames(res.landings2) <- res.landings2$stock
res.landings3 <- as.matrix(res.landings2[,-(1)])
colnames(res.landings3) <- unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]

res.SSB2 <- results[which(results$value=="SSB" & results$year==now+2),]
res.SSB2 <- res.SSB2[,c("sc","stock","data")]
res.SSB2 <- reshape(res.SSB2,direction='wide',idvar="stock",timevar="sc")
rownames(res.SSB2) <- res.SSB2$stock
res.SSB3 <- as.matrix(res.SSB2[,-(1)])
colnames(res.SSB3)<-unlist(strsplit(as.vector(colnames(res.SSB3)),split="data."))[c((1:length(colnames(res.SSB3)))*2)]

# plotname<-"optim"
# source(paste0("North_Sea/programs/prgrTablesAndFigures/",'FigAdvice_NS.r'))

# source("./North_Sea/programs/prgrTablesAndFigures/FigAdvice_NS.r")
# figNS_Advice(year=(now+1),TUR=FALSE, LO=TRUE, SSA=SSA_file, res=resFile )
# 
# dev.off()
# ResOpt  <-  res.      
# ResultsOpt <- STF[[2]]



catch_MSY <- c(as.numeric(landings(dem.st.fwd[["COD-NS"]])[,ac(now+1),,,]),
               as.numeric(landings(dem.st.fwd[["HAD"]])[,ac(now+1),,,]),
               as.numeric(landings(dem.st.fwd[["PLE-EC"]])[,ac(now+1),,,]),
               as.numeric(landings(dem.st.fwd[["PLE-NS"]])[,ac(now+1),,,]),
               as.numeric(landings(dem.st.fwd[["POK"]])[,"2019",,,]),
               # as.numeric(landings(dem.st.fwd[["SOL-EC"]])[,"2019",,,]),
               as.numeric(landings(dem.st.fwd[["SOL-NS"]])[,ac(now+1),,,]),
               as.numeric(landings(dem.st.fwd[["WHG-NS"]])[,ac(now+1),,,]),
               as.numeric(landings(dem.st.fwd[["TUR"]])[,ac(now+1),,,]), 
               as.numeric(landings(dem.st.fwd[["WIT"]])[,ac(now+1),,,])
)


#namesStock <- c("cod 27.47d20","had 27.46a20","ple 27.7d","ple 27.420","pok 27.3a46","sol 27.4",
#                "whg 27.47d","tur 27.4")
CatchComp <- rbind(catch_MSY,TACs) 
#colnames(CatchComp) <-  namesStock


colnames(CatchComp)[colnames(CatchComp)=="COD-NS"] <- "cod 27.47d20"
colnames(CatchComp)[colnames(CatchComp)=="HAD"] <- "had 27.46a20"
colnames(CatchComp)[colnames(CatchComp)=="PLE-EC"] <- "ple 27.7d"
colnames(CatchComp)[colnames(CatchComp)=="PLE-NS"] <- "ple 27.420"
colnames(CatchComp)[colnames(CatchComp)=="POK"] <- "pok 27.3a46"
colnames(CatchComp)[colnames(CatchComp)=="SOL-NS"] <- "sol 27.4"
colnames(CatchComp)[colnames(CatchComp)=="WHG-NS"] <- "whg 27.47d"
colnames(CatchComp)[colnames(CatchComp)=="TUR"] <- "tur.27.4"
colnames(CatchComp)[colnames(CatchComp)=="WIT"] <- "wit.27.3a47d"



x11()

par(las=3, mar=c(7,5,4,2))
barplot(CatchComp, beside=TRUE, col=c('blue', 'red'),
        ylab= paste0("Catches ",now+1),
        legend.text = c("Single species advice", "'range' advice"),
        args.legend = list(x = "topright", cex=1))
savePlot(filename=paste(plot.path,"optim Compare SSA Catches",sep=""),type="png")  
dev.off()

results.path <- "output/04_Model/results/"
write.csv(CatchComp,paste0(results.path,"resultsCatches_Optim.csv"))

SSB_MSY <- c(as.numeric(ssb(dem.st.fwd[["COD-NS"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["HAD"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["PLE-EC"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["PLE-NS"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["POK"]])[,ac(now+2),,,]),
             # as.numeric(ssb(dem.st.fwd[["SOL-EC"]])[,"2020",,,]),
             as.numeric(ssb(dem.st.fwd[["SOL-NS"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["WHG-NS"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["TUR"]])[,ac(now+2),,,]),
             as.numeric(ssb(dem.st.fwd[["WIT"]])[,ac(now+2),,,])
)


SSBComp <- rbind(SSB_MSY,SSBs) 
#colnames(SSBComp) <-  namesStock

colnames(SSBComp)[colnames(SSBComp)=="COD-NS"] <- "cod 27.47d20"
colnames(SSBComp)[colnames(SSBComp)=="HAD"] <- "had 27.46a20"
colnames(SSBComp)[colnames(SSBComp)=="PLE-EC"] <- "ple 27.7d"
colnames(SSBComp)[colnames(SSBComp)=="PLE-NS"] <- "ple 27.420"
colnames(SSBComp)[colnames(SSBComp)=="POK"] <- "pok 27.3a46"
colnames(SSBComp)[colnames(SSBComp)=="SOL-NS"] <- "sol 27.4"
colnames(SSBComp)[colnames(SSBComp)=="WHG-NS"] <- "whg 27.47d"
colnames(SSBComp)[colnames(SSBComp)=="TUR"] <- "tur.27.4"
colnames(SSBComp)[colnames(SSBComp)=="WIT"] <- "wit.27.3a47d"



x11()

par(las=3, mar=c(7,5,4,2))
barplot(SSBComp, beside=TRUE, col=c('blue', 'red'),
        ylab= paste0("SSB " , now+2),
        legend.text = c("Single species advice", "'range' advice"),
        args.legend = list(x = "topright", cex=1.5))
savePlot(filename=paste(plot.path,"optim Compare SSA SSB",sep=""),type="png")  
dev.off()

write.csv(SSBComp,paste0(results.path,"resultsSSB_Optim.csv"))



Year <- 2018
# test <- ReproduceAdviceonOptim(targs)


# 
# 
# 
# # -------------------      MSY approach
# source("./utilities_report_figures_funct.r")
# SSA_file = "bootstrap/data/Reproduce_The_Advice/single_species_advice.csv"
# 
# results.path <- "model/03_Fcube_SQIntYr_LO_Catch_EffortByScenario.Rdata"
# figNS_Advice(year=(now+1),TUR=FALSE, LO=TRUE, SSA=SSA_file, res=results.path )
# 
# dev.off()
# ResMSY  <-  res.      
# ResultsOpt <- STF[[2]]
# 
# 
# # plotname<-"ICES MSY approach"
# # source('../optim/prgr/FigAdvice_NS.r')
# # SSA_file = "data/single_species_advice.csv"
# # 
# # setwd('North_Sea/programs/')
# # figNS_Advice(year=2018,TUR=FALSE, LO=TRUE, SSA=SSA_file, res=results )
# # 
# # dev.off()
# # ResMSY  <-  res.      # get the catches corresponding to this Ftargets
# # ResultsMSY <- STF[[2]]
# # 
# 
# 
# # -------------------      ballanced scenario
# 
# # Ftargs  <-  MSY_ballance*MSY_AR
# # STF     <-  FcubeDiffLandings2Scenarios_Best(TargetByStock=Ftargs)
# # results<-STF[[2]]
# # TACs<-STF[[1]]
# 
# 
# # plotname<-"balanced"
# # source('D:/MIXFISH/sims/no whiting/FigAdvice_NS.r')
# # dev.off()
# # ResBal  <-  res.      # get the catches corresponding to this Ftargets
# # ResultsBal <- STF[[2]]
# 
# 
# ### THIS SEEMS NOT TO WORK ANYMORE
# # ################################
# 
# # # look at the differences between the MSY approach management and the optim management
# #  diffRes<- ResMSY["TAC2020",colnames(ResMSY) %in% c("COD-NS","HAD","PLE-EC","PLE-NS","POK","SOL-NS","WHG-NS","TUR")]-
# #   TACs[names(TACs) %in% c("COD-NS","HAD","PLE-EC","PLE-NS","POK","SOL-NS","WHG-NS","TUR")]
# #  
# # barplot(diffRes)
# # # 1) differences in the TAC
# # 	mp <- barplot(t(diffRes[dim(diffRes)[1],]), col=pal, beside = TRUE,legend = F, 
# # 	              ylim=c(min(res3)+min(res3)*0.25,(max(res.+max(res.)*.25))),ylab=paste("difference TAC set for",yr.now))
# # 	abline(h=0)
# #   savePlot(filename=paste(plot.path,"diff in TAC MSY_RangeOptim",sep=""),type="png")  
# # dev.off()
# 
# 
# # # 2) differences in the catches for the different Fcube scenarios
# # plotname<-"catches difference MSY_Optim"
# # source('D:/MIXFISH/sims/plotDiffMSY_Optim.r')
# # 
# # # 3) differences in SSB trajectories
# # 
# # ResultsMSY$manag  <-  "MSY AR"
# # ResultsOpt$manag  <-  "Range Optim"
# # ResultsBal$manag  <-  "Balanced"
# # ResComp <-  rbind(ResultsMSY,ResultsOpt)
# # ResComp <-  rbind(ResComp,ResultsBal)
# # ResComp <- ResComp[ResComp$value=="SSB", ]
# # ResComp$year<-an(ResComp$year)
# # ResComp<-ResComp[order(ResComp$year),]
# # library(lattice)
# # 
# # 
# # 
# # ResComp<-ResComp[is.element(ResComp$sc,c("min","sq")),]
# # 
# # xyplot(data~factor(year)|factor(paste(ResComp$sc,ResComp$stock)),layout=c(5,2)
# #         ,groups=factor(ResComp$manag)
# #         ,col=c("darkolivegreen4","red","blue2" )
# #         ,data=ResComp,type="b"
# #         ,xlab="years",ylab="SSB (t)"
# #         ,scales = list(relation="free")
# #         , pch= c("+","o","*")
# #         ,cex=2
# #         ,par.settings = list(strip.background=list(col="beige"))
# #         ,key=simpleKey( column=3,
# #                         paste(levels(factor(ResComp$manag)),c("+","o","*"),sep="     "), 
# #                         points = F,
# #                         rectangles = FALSE,
# #                         lines = FALSE,
# #                         col=c("darkolivegreen4","red","blue2" ), 
# #                         pch= c("+","o","*"),
# #                         cex=1 )
# #                     )
# #         
# #         
# # savePlot(filename=paste(plot.path,"diff in SSB_scMAXand MIN_MSY_RangeOptim",sep=""),type="png")  
# # dev.off()
# # 
# #  