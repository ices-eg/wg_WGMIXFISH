







# Radar plots -------------------------------------------------------------
# max / min effort by fleet
# Paul Dolder
# 26/05/16

# Inputs:
# 

plotRose2 <- function(res, yearNow){
  
  
  library(ggplot2) ; library(dplyr)
  
  
  # Read from all outputs for the effort.stock fule
  res <- load(paste0(res))
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
  
  
  # new part 
  
  ## Bar width determined by landings - based on recent data year
  land<-slot.fleet(fleets,"landings")
  land <- filter(land,year==2015) %>% group_by(fleet,qname) %>% 
    summarise(land=sum(landings,na.rm=T))
  
  totLand <- group_by(land,fleet) %>% summarise(land = sum(land))
  
  land$totLand <- totLand$land[match(land$fleet,totLand$fleet)]
  
  land$totRel <- (land$land / land$totLand) 
  
  effort$width <- land$totRel[match(paste(effort$fleet,effort$stock),
                                    paste(land$fleet,land$qname))]
  
  

  
  
  
  ####
  ## The plots
  ####
  
  
  
  # plots 
  
  
  
  
  print(ggplot(effort, aes(x=stock, y = rel, col=Limitation)) +
          facet_wrap(~fleet, ncol=7) +
          scale_color_manual(values=c("red", "grey80", "green")) + 
          geom_bar(fill=8,stat="identity", lwd=0.25, aes(width=width)) +
          theme_bw() + 
          coord_polar() + 
          theme(strip.text.x = element_text(size = rel(0.8)), 
                axis.text.x = element_text(
                  size = rel(0.5),
                  angle = 360/(2*pi)*rev( pi/2 + seq( pi/n.stk, 2*pi-pi/n.stk, len=n.stk))
                ),
                axis.text.y = element_blank() #,
          )#axis.text = element_text(colour = "white"))
  )
}

























##############################################################################################################"
##############################################################################################################"
##############################################################################################################"
##############################################################################################################"
##############################################################################################################"
##############################################################################################################"
##############################################################################################################"
##############################################################################################################"






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
  effort2$order <- as.numeric(as.character(effort2$order))
  effort2 <- effort2[order(effort2$order),]
  
  effort2$ref <- factor(effort2$ref, levels=unique(effort2$ref))
  
  # print(ggplot(effort2, aes(x=ref, y = rel, col=Limitation)) +
  #   facet_wrap(~fleet, ncol=7) +
  #   scale_color_manual(values=c("red", "grey80", "green"), guide = guide_legend(
  #     direction = "horizontal",
  #     title.position = "top")) + 
  #   geom_hline(yintercept = 1, lty=1, col=8) +
  #   geom_bar(fill=8,stat="identity", lwd=0.25) +
  #   theme_bw() + 
  #   coord_polar() +
  #   xlab("") +
  #   ylab("") +
  #   theme(strip.text.x = element_text(size = rel(0.8)), 
  #     axis.text.x = element_text(
  #       size = rel(0.7), hjust=1,
  #       angle = 360/(2*pi)*rev( pi/2 + seq( pi/n.stk, 2*pi-pi/n.stk, len=n.stk))),
  #     legend.position="bottom",
  #     legend.margin=margin(c(1,1,1,1)),
  #     axis.text.y = element_blank())
  # )
  
  return(effort2)
}




# Other -------------------------------------------------------------------



figNS_Advice <- function(year, TUR, LO, SSA, res, only7d = FALSE, pal = grey.colors ){

  
  options(scipen = 50, digits = 5) ## remove scientific notations in y axis
  
  # should be true for LO (all catches are considered as landings in this sc)
  	
  if(LO==TRUE) plotTotCatch <- TRUE 
  
  ### load outputs
  
  single.species.advice <- read.csv(SSA)
  load(res)
  
  results <<- results
  
  if(plotTotCatch == FALSE){ 	
  	res.landings2<-results[which(results$value=="landings" & results$year==year),]
  	res.landings2<-res.landings2[,c("sc","stock","data")]
  	res.landings2<-reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
  	rownames(res.landings2)<-res.landings2$stock
  	res.landings3<-as.matrix(res.landings2[,-(1)])
  	colnames(res.landings3)<-unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]
  }else{ 	
  	results <- results[results$value%in%c("landings", "discards"),]
  	results$value <- "landings"
  	results <- aggregate(list(data= results$data), by=list(sc=results$sc, year=results$year, stock=results$stock, value=results$value), FUN=sum, na.rm=T)
  	
  	res.landings2<-results[which(results$value=="landings" & results$year==year),]
  	res.landings2<-res.landings2[,c("sc","stock","data")]
  	res.landings2<-reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
  	rownames(res.landings2)<-res.landings2$stock
  	res.landings3<-as.matrix(res.landings2[,-(1)])
  	colnames(res.landings3)<-unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]
  }
  
  
  res.landings4<-t(res.landings3)
  
  dem.names <- colnames(res.landings4)[!colnames(res.landings4) %in% 
      grep("NEP",colnames(res.landings4),value=T)]
  yr.now     <- ac(year)
  Advice_Approach<-""

  #  plot with dem and NEP6-9 pulled
  res. <- cbind(res.landings4, 
    apply(res.landings4[,paste("NEP",6:9,sep="")], 1, sum, na.rm=T))
  colnames(res.)[length(colnames(res.))]<-"NEP6-9"
  res. <- res.[,c(dem.names,"NEP6-9")]

	# ADD TAC  order == COD HAD PleVIId PLEIV POK SolVIId SOLIV TUR WHG  NEP 6 to 9    !!!! 
  TAC_COD_NS <- single.species.advice$landings[single.species.advice$stock=="COD-NS" & single.species.advice$year==year]; TAC_COD_NS_Catch <- single.species.advice$catch[single.species.advice$stock=="COD-NS" & single.species.advice$year==year]
  TAC_HAD <- single.species.advice$landings[single.species.advice$stock=="HAD" & single.species.advice$year==year]; TAC_HAD_Catch <-single.species.advice$catch[single.species.advice$stock=="HAD" & single.species.advice$year==year]
  TAC_PLE_NS <- single.species.advice$landings[single.species.advice$stock=="PLE-NS" & single.species.advice$year==year]; TAC_PLE_NS_Catch <-single.species.advice$catch[single.species.advice$stock=="PLE-NS" & single.species.advice$year==year]
  TAC_POK <- single.species.advice$landings[single.species.advice$stock=="POK" & single.species.advice$year==year]; TAC_POK_Catch <-single.species.advice$catch[single.species.advice$stock=="POK" & single.species.advice$year==year]
  TAC_SOL_NS <- single.species.advice$landings[single.species.advice$stock=="SOL-NS" & single.species.advice$year==year]; TAC_SOL_NS_Catch <-single.species.advice$catch[single.species.advice$stock=="SOL-NS" & single.species.advice$year==year]
  TAC_WHG_NS <- single.species.advice$landings[single.species.advice$stock=="WHG-NS" & single.species.advice$year==year]; TAC_WHG_NS_Catch <-single.species.advice$catch[single.species.advice$stock=="WHG-NS" & single.species.advice$year==year]
  TAC_NEP6_9 <- single.species.advice$landings[single.species.advice$stock=="NEP6" & single.species.advice$year==year] +
    single.species.advice$landings[single.species.advice$stock=="NEP7" & single.species.advice$year==year] +
    single.species.advice$landings[single.species.advice$stock=="NEP8" & single.species.advice$year==year] +
    single.species.advice$landings[single.species.advice$stock=="NEP9" & single.species.advice$year==year] 
  TAC_NEP6_9_Catch <- single.species.advice$catch[single.species.advice$stock=="NEP6" & single.species.advice$year==year] +
    single.species.advice$catch[single.species.advice$stock=="NEP7" & single.species.advice$year==year] +
    single.species.advice$catch[single.species.advice$stock=="NEP8" & single.species.advice$year==year] +
    single.species.advice$catch[single.species.advice$stock=="NEP9" & single.species.advice$year==year] 
  TAC_PLE_EC <- single.species.advice$landings[single.species.advice$stock=="PLE-EC" & single.species.advice$year==year]; TAC_PLE_EC_Catch <-single.species.advice$catch[single.species.advice$stock=="PLE-EC" & single.species.advice$year==year]
  TAC_SOL_EC <- single.species.advice$landings[single.species.advice$stock=="SOL-EC" & single.species.advice$year==year]; TAC_SOL_EC_Catch <-single.species.advice$catch[single.species.advice$stock=="SOL-EC" & single.species.advice$year==year]
  TAC_TUR <- single.species.advice$landings[single.species.advice$stock=="TUR" & single.species.advice$year==year]; TAC_TUR_Catch <-single.species.advice$catch[single.species.advice$stock=="TUR" & single.species.advice$year==year]

  if (plotTotCatch==FALSE){ # plot catches
  	if(TUR==""){
  		res.<-rbind(res.,c(TAC_COD_NS,TAC_HAD,TAC_PLE_EC,TAC_PLE_NS,TAC_POK,TAC_SOL_EC,TAC_SOL_NS,TAC_TUR,TAC_WHG_NS,TAC_NEP6_9))
  		rownames(res.)[dim(res.)[1]]<-paste("TAC",year,sep='')
  	} else {
  		# res.<-rbind(res.,c(TAC_COD_NS,TAC_HAD,TAC_PLE_EC,TAC_PLE_NS,TAC_POK,TAC_SOL_EC,TAC_SOL_NS,TAC_WHG_NS,TAC_NEP6_9))
  		# rownames(res.)[dim(res.)[1]]<-paste("TAC",year,sep='')
  		res.<-rbind(res.,c(TAC_COD_NS,TAC_HAD,TAC_PLE_EC,TAC_PLE_NS,TAC_POK,TAC_SOL_NS,TAC_WHG_NS,TAC_NEP6_9))
  		rownames(res.)[dim(res.)[1]]<-paste("TAC",year,sep='')
  	}
  }else{ # plot landings only
  	if(TUR==""){
  		res.<-rbind(res.,c(TAC_COD_NS_Catch,TAC_HAD_Catch,TAC_PLE_EC_Catch,TAC_PLE_NS_Catch,TAC_POK_Catch,TAC_SOL_EC_Catch,TAC_SOL_NS_Catch,
  			TAC_TUR_Catch,TAC_WHG_NS_Catch,TAC_NEP6_9_Catch))
  		rownames(res.)[dim(res.)[1]]<-paste("TAC",year,sep='')
  	}else{
  	  res.<-rbind(res.,c(TAC_COD_NS_Catch,TAC_HAD_Catch,TAC_PLE_EC_Catch,TAC_PLE_NS_Catch,TAC_POK_Catch,TAC_SOL_NS_Catch,TAC_TUR_Catch,
  	                     TAC_WHG_NS_Catch,TAC_NEP6_9_Catch))
  	  rownames(res.)[dim(res.)[1]]<-paste("TAC",year,sep='')
  	}
  }
  
  res2 <- res.
  for (i in 1:dim(res2)[1]){
  	for (j in 1:dim(res2)[2]) {
  		res2[i,j]<-min(res2[i,j],res2[paste("TAC",year,sep=''),j])
  	}
  }


  ##### test
  res3 <- res.
  for (i in 2:dim(res3)[1]){
  	for (j in 1:dim(res3)[2]) {
  		res3[i,j]<-res3[i,j]-res3[paste("TAC",year,sep=''),j]
  	}
  }
  res3[res3>0]<-0
  ####


  scenarios<-c("max","min","cod-ns","sq_E","val")
  #scenarios<-c("max","min","pok","had","sq_E","val")
    
  if(!only7d){
  
    if(TUR==""){
      stocksToBePlotted <- c("COD-NS","HAD","PLE-NS","POK","SOL-NS","WHG-NS","NEP6-9","TUR","PLE-EC","SOL-EC")
      Legend <- c("1:Cod","2:Haddock","3:Plaice IV","4:Saithe","5:Sole IV","6:Whiting","7:Nephrops 6-9","8:Turbot","9:Plaice VIId","10:Sole VIId")
    }else{
      stocksToBePlotted <- c("COD-NS","HAD","PLE-NS","POK","SOL-NS","WHG-NS","NEP6-9","PLE-EC","TUR")
      Legend <- c("1:cod 27.47d20","2:had 27.46a20","3:ple 27.420","4:pok 27.3a46","5:sol  27.4","6:whg 27.47d","7:nep fu6-9","8:ple 27.7d","9:tur ")
      #Legend <- c("1:cod","2:Haddock","3:Plaice IV","4:Saithe","5:Sole IV","6:Whiting","7:Nephrops 6-9","9:Plaice VIId","10:Sole VIId")
      # stocksToBePlotted <- c("COD-NS","HAD","PLE-NS","POK","SOL-NS","WHG-NS","NEP6-9","PLE-EC","SOL-EC")
      # Legend <- c("1:cod 27.47d20","2:had 27.46a20","3:ple 27.420","4:pok 27.3a46","5:sol  27.4","6:whg 27.47d","7:nep fu6-9","8:ple 27.7d","9:sol 27.7d")
    }
  
    layout(matrix(c(1,1,2,3), 2, 2),
       widths=c(4,1), heights=c(1,2))
  
    par(mar=c(5.1,4.1,0, 2.1))
  
  	col <- pal(length(stocksToBePlotted))
  
  	res. <- res.[,stocksToBePlotted]
  	res2 <- res2[,stocksToBePlotted]
  	res3 <- res3[,stocksToBePlotted]
  
  
    #	mp<-barplot(t(res.[scenarios,]),angle = 15, density = 20, col="black", beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,max(res.[scenarios,]+max(res.[scenarios,])*.25)),ylab=paste("predicted landings",yr.now),
    #									xlab=paste("Fcube scenarios", Advice_Approach))
    #	mp<-barplot(t(res2[scenarios,]), col=pal, beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,(max(res.[scenarios,]+max(res.[scenarios,])*.25))),ylab=paste("predicted landings",yr.now),
    #									xlab=paste("Fcube scenarios", Advice_Approach),add=T)
    #	mp<-barplot(t(res3[scenarios,]), col=pal, beside = TRUE,legend = F, ylim=c(min(res3),0),ylab=paste("predicted landings",yr.now),
    #									xlab=paste("Fcube scenarios", Advice_Approach),add=T)
  
    
    	
    if(plotTotCatch==FALSE){
    	mp<-barplot(t(res.[scenarios,]),angle = 15, density = 20, col="black", beside = TRUE,legend = F,ylim=c(min(res3)+min(res3)*0.25,max(res.[scenarios,]+max(res.[scenarios,])*.25)),ylab=paste("predicted landings",yr.now), names.arg=rep('', length(scenarios)))
    	mp<-barplot(t(res2[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,(max(res.[scenarios,]+max(res.[scenarios,])*.25))),ylab=paste("predicted landings",yr.now), names.arg=rep('', length(scenarios)),add=T)
    	mp<-barplot(t(res3[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3),0),ylab=paste("predicted landings",yr.now), names.arg=rep('', length(scenarios)),add=T)
    }else{
    	mp<-barplot(t(res.[scenarios,]),angle = 15, density = 20, col="black", beside = TRUE,legend = F,ylim=c(min(res3)+min(res3)*0.25,max(res.[scenarios,]+max(res.[scenarios,])*.25)),ylab=paste("predicted catches",yr.now), names.arg=rep('', length(scenarios)))
    	mp<-barplot(t(res2[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,(max(res.[scenarios,]+max(res.[scenarios,])*.25))),ylab=paste("predicted catches",yr.now), names.arg=rep('', length(scenarios)),add=T)
    	mp<-barplot(t(res3[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3),0),ylab=paste("predicted catches",yr.now), names.arg=rep('', length(scenarios)),add=T)
    }
  									
  	mtext(side = 1, at =mp, line = 0.01,text = rep(formatC(c(1:dim(res.)[2])),5), col = "black",cex=0.8)
  	text(0,max(res.[scenarios,]+max(res.[scenarios,])*.2), "Scenarios: ", pos=4)
  	for (i in 1:length(scenarios)){
  		text((i-1)*(dim(res.)[2]+1),max(res.[scenarios,]+max(res.[scenarios,])*.15), scenarios[i], pos=4)
  	}
  										   
    mtext(side = 2, at =min(res3)*.8, line = 3,text = "undershoot", col = "black",cex=1)   
  
    #main =paste(yr.now,"Fcube landings estimates, dem stocks"), )
  
    #main =paste(yr.now,"Fcube landings estimates, dem stocks"), )
  
  	abline(h=0, col="black")
    for(i in 1:dim(res.)[2]){
    		abline(h=res.[paste("TAC",year,sep=''),colnames(res.)[i]], col="black",lty=2)
    }
    
    for(i in 1:dim(res.)[2]){
    	mtext(side=4,at=res.[paste("TAC",year,sep=''),i],text=i,cex=0.7,las=2)
    }
    
    for(i in 1:length(scenarios)){
    	rect((i-1)*(dim(res.)[2]+1)+0.7,min(res3)+min(res3)*0.25,i*(dim(res.)[2]+1)+.2,max(res.[scenarios,])+max(res.[scenarios,])*.25, lty=1)
    }
  
    par(mar=c(3,1,0, 0))
    plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
    plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
    legend("bottom",fill=col,cex=0.8,bty='n',
       legend= Legend,ncol=1)
       #legend = c("1:Cod","2:Haddock","3:Plaice IV","4:Saithe","5:Sole IV","6:Whiting","7:Nephrops 6-9","8:Turbot IV","9:Plaice VIId","10:Sole VIId"),ncol=1)  
    
    # print((t(res.)))
    res. <<- res.
  }

  ##### Only VIId
  if(only7d){
    
    if(TUR==""){
    stocksToBePlotted <- c("TUR","PLE-EC","SOL-EC")
    Legend <- c("8:Turbot","9:Plaice VIId","10:Sole VIId")
    }else{
      stocksToBePlotted <- c("TUR","PLE-EC")
      Legend <- c("8:Turbot","9:Plaice VIId")
    # stocksToBePlotted <- c("PLE-EC","SOL-EC")
    # Legend <- c("8:ple 27.7d","9:sol 27.7d")
      # stocksToBePlotted <- c("TUR","PLE-EC","SOL-EC")
      # Legend <- c("8:Turbot","9:Plaice VIId","10:Sole VIId")
      
    }
    
    layout(matrix(c(1,1,2,3), 2, 2),
       widths=c(4,1), heights=c(1,2))
    
    par(mar=c(5.1,4.1,0, 2.1))
    
    	col <- pal(length(stocksToBePlotted))
    
    	res. <- res.[,stocksToBePlotted]
    	res2 <- res2[,stocksToBePlotted]
    	res3 <- res3[,stocksToBePlotted]
    
    
    #	mp<-barplot(t(res.[scenarios,]),angle = 15, density = 20, col="black", beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,max(res.[scenarios,]+max(res.[scenarios,])*.25)),ylab=paste("predicted landings",yr.now),
    #									xlab=paste("Fcube scenarios", Advice_Approach))
    #	mp<-barplot(t(res2[scenarios,]), col=pal, beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,(max(res.[scenarios,]+max(res.[scenarios,])*.25))),ylab=paste("predicted landings",yr.now),
    #									xlab=paste("Fcube scenarios", Advice_Approach),add=T)
    #	mp<-barplot(t(res3[scenarios,]), col=pal, beside = TRUE,legend = F, ylim=c(min(res3),0),ylab=paste("predicted landings",yr.now),
    #									xlab=paste("Fcube scenarios", Advice_Approach),add=T)
    
    if(plotTotCatch==FALSE){
    	mp<-barplot(t(res.[scenarios,]),angle = 15, density = 20, col="black", beside = TRUE,legend = F,ylim=c(min(res3)+min(res3)*0.25,max(res.[scenarios,]+max(res.[scenarios,])*.25)),ylab=paste("predicted landings",yr.now), names.arg=rep('', length(scenarios)))
    	mp<-barplot(t(res2[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,(max(res.[scenarios,]+max(res.[scenarios,])*.25))),ylab=paste("predicted landings",yr.now), names.arg=rep('', length(scenarios)),add=T)
    	mp<-barplot(t(res3[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3),0),ylab=paste("predicted landings",yr.now), names.arg=rep('', length(scenarios)),add=T)
    }else{
    	mp<-barplot(t(res.[scenarios,]),angle = 15, density = 20, col="black", beside = TRUE,legend = F,ylim=c(min(res3)+min(res3)*0.25,max(res.[scenarios,]+max(res.[scenarios,])*.25)),ylab=paste("predicted catches",yr.now), names.arg=rep('', length(scenarios)))
    	mp<-barplot(t(res2[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3)+min(res3)*0.25,(max(res.[scenarios,]+max(res.[scenarios,])*.25))),ylab=paste("predicted catches",yr.now), names.arg=rep('', length(scenarios)),add=T)
    	mp<-barplot(t(res3[scenarios,]), col=col, beside = TRUE,legend = F, ylim=c(min(res3),0),ylab=paste("predicted catches",yr.now), names.arg=rep('', length(scenarios)),add=T)
    }									
    
    if (TUR==""){									
    	mtext(side = 1, at =mp, line = 0.01,text = rep(formatC(8:10),6), col = "black",cex=0.8)
    }else{
    	mtext(side = 1, at =mp, line = 0.01,text = rep(formatC(8:9),6), col = "black",cex=0.8)
    }
    
    	text(0.7,max(res.[scenarios,]+max(res.[scenarios,])*.2), "Scenarios: ", pos=4)
    	for (i in 1:length(scenarios)){
    		text((i-1)*(dim(res.)[2]+1)+0.7,max(res.[scenarios,]+max(res.[scenarios,])*.15), scenarios[i], pos=4)
    	}
    										   
    mtext(side = 2, at =min(res3)*.8, line = 3,text = "undershoot", col = "black",cex=1)   
    
            #main =paste(yr.now,"Fcube landings estimates, dem stocks"), )
    
            #main =paste(yr.now,"Fcube landings estimates, dem stocks"), )
    		abline(h=0, col="black")
    for(i in 1:dim(res.)[2]){
    		abline(h=res.[paste("TAC",year,sep=''),colnames(res.)[i]], col="black",lty=2)
    }
    
    
    for(i in 1:dim(res.)[2]){
    if(TUR==""){
    	mtext(side=4,at=res.[paste("TAC",year,sep=''),i],text=i+7,cex=0.7,las=2)
    }else{
    	mtext(side=4,at=res.[paste("TAC",year,sep=''),i],text=i+8,cex=0.7,las=2)
    }
    }
    
    for(i in 1:length(scenarios)){
    	rect((i-1)*(dim(res.)[2]+1)+0.7,min(res3)+min(res3)*0.25,i*(dim(res.)[2]+1)+.2,max(res.[scenarios,])+max(res.[scenarios,])*.25, lty=1)
    }
    
    par(mar=c(3,1,0, 0))
    plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
    plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
    legend("bottom",fill=col,cex=0.8,bty='n',
       legend= Legend,ncol=1)
       #legend = c("1:Cod","2:Haddock","3:Plaice IV","4:Saithe","5:Sole IV","6:Whiting","7:Nephrops 6-9","8:Turbot IV","9:Plaice VIId","10:Sole VIId"),ncol=1)  
  }
  
}



#### Compare SSB TAC year from single stock advice and MIXFISH scenarios


figNS_Advice_SSB <- function(yearNow, stks, LO, res, pal=grey.colors ){
  
  library(lattice)
  
  
  options(scipen = 50, digits = 5) ## remove scientific notations in y axis
  
  Year <- yearNow +2 # for SSB
  
  ### load outputs
  
  plotTotCatch <- TRUE # should be true for LO (all catches are considered as landings in this sc)
  if(LO==TRUE) plotTotCatch <- TRUE

  #load("../results/03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  load(res)

  stocksToBePlotted <- stks
  stockLabels <- tableREf$Advice_name[match(stks, tableREf$stock)]
  Legend <- paste0(1:length(stockLabels),":",stockLabels)

  # if(TUR==""){
  #   stocksToBePlotted <- c("COD-NS","HAD","PLE-NS","POK","SOL-NS","WHG-NS","TUR","PLE-EC","SOL-EC")
  #   Legend <- c("1:Cod","2:Haddock","3:Plaice IV","4:Saithe","5:Sole IV","6:Whiting","8:Turbot","9:Plaice VIId","10:Sole VIId")
  # }else{
  #   stocksToBePlotted <- c("COD-NS","HAD","PLE-NS","POK","SOL-NS","WHG-NS","TUR","PLE-EC")
  #   Legend <- c("1:cod 27.47d20","2:had 27.46a20","3:ple 27.420","4:pok 27.3a46","5:sol 27.4","6:whg 27.47d", "7:tur 27.4", "8:ple 27.7d")
  # }


  stock_order <- as.data.frame(matrix(c(stocksToBePlotted,c(1:length(stocksToBePlotted))),ncol=2))
  colnames(stock_order) <- c("stock","order")
  ####################################################"
  
  
  
  
  resSSB<-subset(results,value=="ssb" & year==Year)
  resSSB<-resSSB[,c("sc","stock","data")]
  resSSB2 <- reshape(resSSB, direction='wide', idvar="stock", timevar="sc")
  colnames(resSSB2) <- unlist(lapply(strsplit(colnames(resSSB2), split = ".", fixed = TRUE), FUN = function(x){
    if(length(x)==1){x[1]} else {x[2]}
  }))
  
  
  ##### order table to fit with stock orders in the other tables
  resSSB2 <- resSSB2[match(stock_order$stock, resSSB2$stock),]
  
  resSSB3 <- resSSB2[,unique(resSSB$sc)]/resSSB2$baseline
  resSSB3 <- resSSB3[,-which(colnames(resSSB3)=="baseline")]
  # resSSB3 <- (resSSB2[,-c(1:2,dim(resSSB2)[2])])/resSSB2[,2]
  # colnames(resSSB3)<-unlist(strsplit(as.vector(colnames(resSSB3)),split="data."))[c((1:length(colnames(resSSB3)))*2)+1]
  
  
  col <- pal(length(row.names(resSSB3)))
  mp <- barplot(as.matrix(resSSB3), col=col, beside = TRUE, legend = F, 
    ylim=c(0,max(resSSB3)+0.5), ylab = paste("predicted SSB",Year,"relative to baseline"),
    names.arg = rep("", ncol(resSSB3)))
  legend("top",fill=col,cex=0.8,bty='n', legend = Legend,ncol=4)   
  abline(h=1, col="black")																
  
  # mtext(side = 1, at =mp, line = 0.01,text = rep(formatC(c(1:length(stocksToBePlotted))),5), col = "black",cex=0.8)
  mtext(side = 1, at =mp, line = 0.01,text = rep(unlist(strsplit(Legend, ":"))[c(0:(length(Legend)-1))*2+1],5), col = "black",cex=0.8)
  scenarios<- colnames(resSSB3) # c("max","min","cod-ns","sq_E","val")
  
  for(i in 1:length(scenarios)){
    rect((i-1)*(dim(resSSB3)[1]+1)+0.7,0,i*(dim(resSSB3)[1]+1)+.2,max(resSSB3)*1.12, lty=1)
  }
  
  text(0.7,max(resSSB3)*1.1, "Scenarios: ", pos=4)
  for (i in 1:length(scenarios)){
    text((i-1)*(dim(resSSB3)[1]+1)+0.7,max(resSSB3)*1.05, scenarios[i], pos=4)
  }

}




TableSSB <- function(yearNow, LO, SSA, res ){
  
  #yearNow<-2018
  #TUR <- FALSE
  #LO <- TRUE
  
  Year<-yearNow + 2
  #TUR <- TUR
  LO <- LO
  
  
  
  plotTotCatch <- TRUE # should be true for LO (all catches are considered as landings in this sc)
  if(LO==TRUE) plotTotCatch <- TRUE
  
  
  #single.species.advice<-read.table("../plots/single_species_advice_reopening2016.csv",sep=",",header=T)
  single.species.advice <- read.csv(SSA, header=T)
  ssa <- single.species.advice[single.species.advice$year==Year, c('ssb', 'stock')]
  colnames(ssa) <- c('data.Single-stock-advice','stock' )
  
  ### load outputs
  
  #load("../results/03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  load(res)
  
  
  res.landings2<-results[which(results$value=="ssb" & results$year==Year),]
  res.landings2<-res.landings2[,c("sc","stock","data")]
  res.landings2<-reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
  res.landings2 <- merge(res.landings2, ssa)
  rownames(res.landings2)<-res.landings2$stock
  res.landings3<-as.matrix(res.landings2[,-(1)])
  colnames(res.landings3)<-unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]
  return(res.landings3)
}



TableFbar <- function(yearNow, LO, SSA, res ){
  
  #yearNow<-2018
  #TUR <- FALSE
  #LO <- TRUE
  
  Year<-yearNow + 1

  plotTotCatch <- TRUE # should be true for LO (all catches are considered as landings in this sc)
  if(LO==TRUE) plotTotCatch <- TRUE
  
  
  #single.species.advice<-read.table("../plots/single_species_advice_reopening2016.csv",sep=",",header=T)
  single.species.advice <- read.csv(SSA, header=T)
  ssa <- single.species.advice[single.species.advice$year==Year, c('Fbar', 'stock')]
  colnames(ssa) <- c('data.Single-stock-advice','stock' )
  
  ### load outputs
  
  #load("../results/03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  load(res)
  
  
  res.landings2<-results[which(results$value=="Fbar" & results$year==Year),]
  res.landings2<-res.landings2[,c("sc","stock","data")]
  res.landings2<-reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
  res.landings2 <- merge(res.landings2, ssa)
  rownames(res.landings2)<-res.landings2$stock
  res.landings3<-as.matrix(res.landings2[,-(1)])
  colnames(res.landings3)<-unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]
  return(res.landings3)
}


TableCatch <- function(yearNow, LO, SSA, res ){
  
  #year<-2017
  #TUR <- FALSE
  #LO <- TRUE
  
  Year<-yearNow + 1
  #TUR <- TUR
  LO <- LO
  
  
  
  plotTotCatch <- TRUE # should be true for LO (all catches are considered as landings in this sc)
  if(LO==TRUE) plotTotCatch <- TRUE
  
  
  #single.species.advice<-read.table("../plots/single_species_advice_reopening2016.csv",sep=",",header=T)
  single.species.advice<-read.csv(SSA)
  ssa <- single.species.advice[single.species.advice$year==Year, c('catch', 'stock')]
  colnames(ssa) <- c('data.Single-stock-advice','stock' )
  
  ### load outputs
  
  #load("../results/03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  load(res)
  
  
  results <- results[results$value%in%c("landings", "discards"),]
  results$value <- "landings"
  results <- aggregate(list(data= results$data), by=list(sc=results$sc, year=results$year, stock=results$stock, value=results$value), FUN=sum, na.rm=T)
  
  res.landings2<-results[which(results$value=="landings" & results$year==Year),]
  res.landings2<-res.landings2[,c("sc","stock","data")]
  res.landings2<-reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
  res.landings2 <- merge(res.landings2, ssa)
  rownames(res.landings2)<-res.landings2$stock
  res.landings3<-as.matrix(res.landings2[,-(1)])
  colnames(res.landings3)<-unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]
  
  
  
 return(res.landings3)
}



TableRelativeCatch <- function(yearNow, LO, SSA, res ){
  
  #year<-2017
  #TUR <- FALSE
  #LO <- TRUE
  
  Year<-yearNow + 1
  #TUR <- TUR
  LO <- LO
  
  
  
  plotTotCatch <- TRUE # should be true for LO (all catches are considered as landings in this sc)
  if(LO==TRUE) plotTotCatch <- TRUE
  
  #single.species.advice<-read.table("../plots/single_species_advice_reopening2016.csv",sep=",",header=T)
  single.species.advice<-read.csv(SSA)
  ssa <- single.species.advice[single.species.advice$year==Year, c('catch', 'stock')]
  colnames(ssa) <- c('data.Single-stock-advice','stock' )
  
  ### load outputs
  
  #load("../results/03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  load(res)
  
  results <- results[results$value%in%c("landings", "discards"),]
  results$value <- "landings"
  results <- aggregate(list(data= results$data), by=list(sc=results$sc, year=results$year, stock=results$stock, value=results$value), FUN=sum, na.rm=T)
  
  res.landings2<-results[which(results$value=="landings" & results$year==Year),]
  res.landings2<-res.landings2[,c("sc","stock","data")]
  res.landings2<-reshape(res.landings2,direction='wide',idvar="stock",timevar="sc")
  res.landings2 <- merge(res.landings2, ssa)
  rownames(res.landings2)<-res.landings2$stock
  res.landings3<-as.matrix(res.landings2[,-(1)])

  colnames(res.landings3)<-unlist(strsplit(as.vector(colnames(res.landings3)),split="data."))[c((1:length(colnames(res.landings3)))*2)]
  
  res.landings3[,c(1:dim(res.landings3)[2]-1)]<-  res.landings3[,c(1:dim(res.landings3)[2]-1)]/  res.landings3[,dim(res.landings3)[2]]
  
  
  
 return(res.landings3)
}


exportSSATables <- function(yearNow, LO, SSA, res ){
  
  #year<-2017
  #TUR <- FALSE
  #LO <- TRUE
  
  YearTAC<-yearNow + 1
  YearSSB <- yearNow + 2
  #TUR <- TUR
  LO <- LO
  
  
  
  plotTotCatch <- TRUE # should be true for LO (all catches are considered as landings in this sc)
  if(LO==TRUE) plotTotCatch <- TRUE
  
  
  #single.species.advice<-read.table("../plots/single_species_advice_reopening2016.csv",sep=",",header=T)
  single.species.advice<-read.csv(SSA)
  ssa <- single.species.advice[single.species.advice$year==YearTAC, c('ssb', 'stock')]
  colnames(ssa) <- c('SSBssa','stock' )
  
  ### load outputs
  
  #load("../results/03_Fcube_Projection_LO_Catch_EffortByScenario.Rdata")
  load(res)
  
  
  catch <- results[results$value%in%c("landings", "discards"),]
  catch$value <- "landings"
  catch <- aggregate(list(data= catch$data), by=list(sc=catch$sc, year=catch$year, stock=catch$stock, value=catch$value), FUN=sum, na.rm=T)
  
  catch<-catch[which(catch$value=="landings" & catch$year==YearTAC),]
  catch<-catch[,c("sc","stock","data")]
  colnames(catch)<-c("sc","stock","catch")
  
  Fbar<-results[which(results$value=="Fbar" & results$year==YearTAC),]
  Fbar<-Fbar[,c("sc","stock","data")]
  colnames(Fbar)<-c("sc","stock","Fbar")  
  
  SSB<-results[which(results$value=="ssb" & results$year==YearSSB),]
  SSB<-SSB[,c("sc","stock","data")]
  colnames(SSB)<-c("sc","stock","SSB")  
  
  
  tot <- merge(catch, Fbar)
  tot <- merge(tot, SSB)
  tot <- merge(tot, ssa)
  
  tot$percVarSSB <- round(( tot$SSB - tot$SSBssa)/tot$SSBssa*100)
  
  scen <- c("max", "min", "cod-ns", "sq_E", "val")
  
  tot <- tot[which(tot$sc%in%scen),]
  
  tot$test[tot$sc=="max"] <- 1
  tot$test[tot$sc=="min"] <- 2
  tot$test[tot$sc=="cod-ns"] <- 3
  #  tot$test[tot$sc=="pok"] <- 4
  tot$test[tot$sc=="sq_E"]<- 4
  tot$test[tot$sc=="val"] <- 5
  
  tot <- tot[order(tot$stock,tot$test),]
  tot <- subset(tot, select=which(colnames(tot)!="test"))
  #tot$UnwantedCatch <-"" 
  #tot$WantedCatch <-"" 
  tot$Basis <- rep(c('A','B','C','D', 'E'),length(unique(tot$stock)))
  #tot$FUnwantedCatch <-"" 
  #tot$FwantedCatch <-"" 
  #tot$PercChangeTAC <-"" 
  
  #tot <- tot[,c("stock","catch", "UnwantedCatch", "WantedCatch", "Basis", "Fbar", "FUnwantedCatch", "FwantedCatch", "SSB", "percVarSSB", "PercChangeTAC")]
  
  tot <- tot[,c("stock","catch", "Basis", "sc", "Fbar",  "SSB", "percVarSSB")]
  
  
  return( tot  )
  
}


plotEffort <- function(year,fleet ){

  options(scipen = 50, digits = 5) ## remove scientific notations in y axis
  
  ## advice year
  
  #year<-2017
  #TUR <- FALSE
  #LO <- TRUE
  
  year<-year
  
  load(fleet)
  
}



underOverPlot <- function(
  inclYear = now+1, 
  inclStock = c("COD-NS", "HAD", "PLE-NS", "POK", "SOL-NS", "WHG-NS"), 
  inclScen = c("max", "min", "cod-ns", "sq_E", "val"),
  var = "catch",
  ssaObj = ssa,
  resObj = results,
  stockLabels = c("COD-NS", "HAD", "PLE-NS", "POK", "SOL-NS", "WHG-NS"),
  col = NULL){

  if(is.null(col)){
    pal <- colorRampPalette(c("grey10", "grey90"))
    col = pal(length(inclStock))
  }
  
  # options(scipen = 50, digits = 5) ## remove scientific notations in y axis
  
  ### load outputs
  ssaObj <- reshape2::melt(data = ssaObj, id.vars = c("stock", "year"))
  
  if(var == "catch"){
    resObj <- subset(resObj, 
      value %in% c("discards", "landings") & 
      year == inclYear & 
      stock %in% inclStock & 
      sc %in% c("baseline", inclScen))
    ssaObj <- subset(ssaObj,
      variable %in% c("catch") & 
      year == inclYear & 
      stock %in% inclStock)
  } else {
    resObj <- subset(resObj, 
      value %in% c("landings") & 
      year == inclYear & 
      stock %in% inclStock & 
      sc %in% c("baseline", inclScen))
    ssaObj <- subset(ssaObj,
      variable %in% c("landings") & 
      year == inclYear & 
      stock %in% inclStock)
  }
  # head(resObj)
  
  resAgg <- aggregate(data ~ stock + sc, data = resObj, FUN = sum, na.rm=TRUE)
  resObj2 <- reshape2::dcast(data = resAgg, formula = stock ~ sc, fun.aggregate = sum, na.rm=TRUE, value.var = "data")
  
  ssaAgg <- aggregate(value ~ stock, data = ssaObj, FUN = sum, na.rm=TRUE)
  names(ssaAgg) <- c("stock", paste("TAC", inclYear,sep=''))
  
  # hatched
  res. <- merge(resObj2, ssaAgg)
  rownames(res.) <- res.$stock
  res. <- res.[,-which(names(res.)=="stock")]
  res. <- res.[match(inclStock, rownames(res.)),]
  res. <- as.matrix(res.)
  
  # to fill (reveals overshoot in res.)
  res2 <- res.
  for (i in 1:dim(res2)[1]){
  	for (j in 1:dim(res2)[2]) {
  		res2[i,j] <- min(res2[i,j], res2[i,paste("TAC", inclYear,sep='')])
  	}
  }
  
  # undershoot
  res3 <- res.
  for (i in 1:dim(res3)[1]){
  	for (j in 1:dim(res3)[2]) {
  		res3[i,j]<-res3[i,j]-res3[i, paste("TAC", inclYear,sep='')]
  	}
  }
  res3[res3>0]<-0

  
  # PLOT
  layout(matrix(c(1,1,2,3), 2, 2),
    widths=c(4,1), heights=c(1,2))
  par(mar=c(2.1,4.1,2.1,1.1))
  scenarios <- c(inclScen) 

	mp <- barplot(res.[,scenarios], angle = 15, density = 20, col="black", 
	  beside = TRUE, legend = F, 
	  ylim = c(min(res3)*1.15, max(res.*1.25)), 
	  ylab = paste("predicted", ifelse(var=="catch", "catch", "landings"), inclYear), 
	  names.arg = rep('', ncol(res.[,scenarios])))
	mp <- barplot(res2[,scenarios], col=col, beside = TRUE, legend = F, add=T, names.arg = rep('', ncol(res.[,scenarios])))
	mp <- barplot(res3[,scenarios], col=col, beside = TRUE, legend = F, add=T, names.arg = rep('', ncol(res.[,scenarios])))

	mtext(side = 1, at = mp, line = 0.01, 
	  text = rep(formatC(c(1:dim(res.)[1])),5), col = "black", cex = 0.8)
	
	mtext(text = "Scenarios: ", side = 3, line = 0.5, adj = 0)

	# text(mp[1,1], max(res.[,scenarios])*1.1, "Scenarios: ", pos=4)
	for (i in 1:length(scenarios)){
		text(x = mean(mp[,i]), y = max(res.[,scenarios])*1.1,
		  labels = scenarios[i], pos=3)
	}
  
	mtext(text = "undershoot", side = 2, line = par()$mgp[2]+1, adj = 0, cex = 0.8)

	abline(h=0, col="black")

  for(i in 1:dim(res.)[1]){
		#abline(h=res.[i, paste("TAC", inclYear,sep='')], col="black", lty=2)
    abline(h=res.[i, paste("TAC", inclYear,sep='')], col=col[i], lty=2)
  }
    
  for(i in 1:dim(res.)[1]){
  	mtext(side=4, line = 0.5, at=res.[i,paste("TAC", inclYear,sep='')], col=1, text=i, cex=0.7, las=2)
  }
    
  for(i in 1:length(scenarios)){
  	rect((i-1)*(dim(res.)[1]+1)+0.7, min(res3)*1.1, 
  	  i*(dim(res.)[1]+1)+.2, max(res.[,scenarios])*1.2, 
  	  lty=1)
  }
  
  par(mar=c(2,0,0,1))
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  plot(c(0,0), col="white",  axes=F, xlab="", ylab="")
  legend("bottomleft", fill=col, cex=1, bty='n',
     legend= stockLabels, ncol=1)
     
  res. <<- res.

}





