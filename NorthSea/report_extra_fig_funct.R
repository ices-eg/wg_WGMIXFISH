#################################################################################
## Vanessa Trijoulet
## Extra figure functions for report
#################################################################################

preparedata <- function(catch){

df <- catch
df<-df[(df$stock !="NEP3A"),] # remove NEP3A
df$catch<-df$land+df$disc # create a catch field
# data.yr<-max(df$year)

# rename the columns
colnames(df)<-c("Fleet","Metier","Stock","Year","Landings","Discards","Value","ID","Catch")

# put in a better order
df<-df[c("Stock","Year","Fleet","Metier","Landings","Discards","Catch","Value")]
#df<-rbind(df,yy)
# remove the areas from the metiers

df$Metier<-sapply(df$Metier,function(x) {
  strsplit(as.character(x),"\\.")[[1]][1]
})

# convert Stock, Year, Country, Fleet, Metier, Area to factor
df$Stock<-as.factor(df$Stock)
df$Year<-as.factor(df$Year)
df$Fleet<-as.factor(df$Fleet)
df$Metier<-as.factor(df$Metier)

# rename the Nephrops FUs to bring togther
NEP<-grep("NEP",unique(df$Stock),value=T)
levels(df$Stock)[levels(df$Stock) %in% NEP]<-"NEP"

## removing non-allocated landings/catch from OTH landings/catch

orig.name <- c(unique(catch_ad$stock), "NEP")
name.stock <- name_stocks$Stock_name[match(orig.name, name_stocks$Fcube_stock_name)]
name.stock[length(name.stock)] <- "Nephrops"
name.stock <- paste0(1:length(name.stock),":",name.stock)

# name.stock=c("1:cod 27.47d20","2:had 27.46a20","3:ple 27.420","4:pok 27.3a46", 
#              "5:sol 27.4","6:whg 27.47d","7:Nephrops",
#              "8:Turbot 27.4",
#              "9:ple 27.7d")
# orig.name=c("COD-NS","HAD", "PLE-NS", "POK", "SOL-NS", "WHG-NS", "NEP", "TUR", "PLE-EC")
# check
cbind(orig.name, name.stock)
for (i in 1:length(name.stock))
  levels(df$Stock)[levels(df$Stock) == orig.name[i]]<-name.stock[i]


# rename the odd metiers to reduce the number of bars
levels(df$Metier)[levels(df$Metier) == "tr2_grid"]<-"TR2"
levels(df$Metier)[levels(df$Metier) == "GN1_FDF"] <-"GN1"

# aggregate it all up
df2<-aggregate(df[c("Landings","Discards","Catch","Value")],by=list(Year=df$Year,Metier = df$Metier,Stock = df$Stock),sum)

##remove turbot
#idx<-which(df$Stock %in% "8:Turbot IV")
#df<-df[-idx,]

## removing landings from other.stock.areas (non-allocated) from OTH
#yy<-c('2:Haddock','4:Saithe','6:Whiting','7:Nephrops')
#df$Landings[df$Metier=='OTH'&df$Stock %in% yy &df$Year==data.yr]<-df$Landings[df$Metier=='OTH'&df$Stock %in% yy &df$Year==data.yr]-df$Landings[df$Metier=='non-allocated'&df$Stock %in% yy &df$Year==data.yr]
#df$Catch[df$Metier=='OTH'&df$Stock %in% yy &df$Year==data.yr]<-df$Catch[df$Metier=='OTH'&df$Stock %in% yy &df$Year==data.yr]-df$Catch[df$Metier=='non-allocated'&df$Stock %in% yy &df$Year==data.yr]
#df$Landings[df$Landings<0]<-NA # ;if(df$Catch<0<-NA)
# melt
df3<-melt(df2,id=c("Year","Metier","Stock"))


# reorder the Stocks and Metiers in the dataframe to something "sensible"
df3$Stock<-factor(df3$Stock,levels = name.stock)

df3$Metier<-factor(df3$Metier,levels = c("TR1","TR2","BT1","BT2","GN1",'GN1_FDF',"GT1",
                                       "LL1","beam_oth","pots","OTH",'non-allocated'))

# order the dataframe
df4<-df3[(order(df3$Year,df3$Metier,df3$Stock)),]



## there is no unallocated past 2005, so this can be removed
df5<-df4[!is.na(df4$Metier),]
df6 <- df5[!is.na(df5$Stock),]


return(df6)

}




landbystockmetier <- function(df6){
 
  data.yr <- max(levels(df6$Year))

  p<-ggplot(df6[(df6$variable=="Landings" & df6$Year==data.yr),],aes(factor(Metier),value/1000,fill=Stock))
  
  
return(p)

}



landbystock <- function(df6){

  data.yr<-max(levels(df6$Year))
    
  
  df7<-df6[(df6$variable=="Landings" & df6$Year==data.yr),]
  
  df7<-aggregate(df7["value"],by=list(Stock = df7$Stock),sum,na.rm=T)
  
  p2<- ggplot(df7,aes(x="",y=value,fill=Stock))

return(p2)

}



effortbyfleet <- function(){
  
  ### Only keep coutries with effort in days at sea
  
  ## fleets with effort in KW : "FR_Otter","FR_Static","BE_Beam"
  tmp<-subset(as.data.frame(lapply(fleets,effort)), !qname %in% c("OTH_OTH","unalloc"))
  tmp$data[which(tmp$data==0)]<-NA
  
  return(tmp)
  
}


relative.effort <- function(year.ref){
  
  rel.eff <-lapply(fleets,function(x) sweep(effort(x),c(1,3:6),apply(effort(x),c(1,3:6),mean,na.rm=T),FUN="/"))
  rel.eff <-lapply(fleets,function(x) sweep(effort(x),c(1,3:6),effort(x)[,as.character(year.ref)],FUN="/"))

return(rel.eff)
}


effshare.fn <- function(){
  
  efshare <- lapply(fleets,function(x) {
    res <- as.data.frame(FLQuants(effshare(x)))
    # !!! Yve If only one m?tier then res$qname==NA need to get the m?tier's name from x
    res$fleet <- name(x)
    res$qname[is.na(res$qname) | res$qname=="v1"] <- x@metiers@names  ## yve
    # res$qname[is.na(res$qname)] <- "OTH"   yve
    return(res)})
  efshare <- eval(parse(text=paste('rbind(efshare[[', paste(seq(length(efshare)),
                                                            collapse=']],efshare[['), ']])', sep='')))
  
  return(efshare)
}


landingsbyfleet <- function(){
  
  flq <- FLQuant(0,dimnames=dimnames(Sums(landings(fleets[[1]],catch="COD-NS"))))
  
  tot.ld <- lapply(st.lst,function(st) {
    res <- lapply(fleets,function(x1) {
      if(st %in% spp(x1)) z<-Sums(landings(x1,catch=st)) else z<-flq
      return(z) })
    res <-Sums(res)
  })
  
  #rel.stab <- lapply(st.lst,function(st) {
  # pdf(outplot.st)
  rel.stab <-data.frame()
  rel.rel <- data.frame()
  
  fl_landAllsp<- data.frame()     # yve
  
  for (st in st.lst) {
    fl.ld <- lapply(fleets,function(x) {
      if(st %in% spp(x)) z<-Sums(landings(x,catch=st)) else z<-flq
      return(z) })
    fl.share <- lapply(fl.ld, function(x) x/tot.ld[[st]])
    rel.share <- lapply(fl.share, function(x) sweep(x,c(1,3:6),apply(x,c(1,3:6),mean,na.rm=T),FUN="/"))
    
    fl.share <- cbind(as.data.frame(fl.share),stock=st)
    rel.share <- cbind(as.data.frame(rel.share),stock=st)
    
    tmp<-cbind(as.data.frame(fl.ld),stock=st)   #yve
    fl_landAllsp<-rbind(fl_landAllsp,tmp)       #yve
    
    rel.stab <- rbind(rel.stab,fl.share)
    rel.rel <- rbind(rel.rel,rel.share)
  }
  rel.stab$stock <- as.character(rel.stab$stock)
  rel.rel$stock <- as.character(rel.rel$stock)
  
  fl_landAllspM_NEP<-fl_landAllsp[-which(substr(fl_landAllsp$stock,1,3)==c("NEP")),]
  fl_landAllspM_NEP$stock<-factor(fl_landAllspM_NEP$stock)
  
  # Get the most important catches to order the fleets
  #test<-unique(fl_landAllspM_NEP$qname[order(fl_landAllspM_NEP$data,decreasing=T)])[-which(unique(fl_landAllspM_NEP$qname[order(fl_landAllspM_NEP$data,decreasing=T)])=="OTH_OTH")]
  year.toto <- max(fl_landAllspM_NEP$year)
  toto <- fl_landAllspM_NEP[fl_landAllspM_NEP$year==year.toto & fl_landAllspM_NEP$qname!="OTH_OTH",]
  toto <- aggregate(toto$data,by=list(qname=toto$qname),FUN='sum',na.rm=T)
  test <- toto$qname[order(toto$x,decreasing=T)]
  
  Order_fleet<-data.frame(matrix(nrow=length(test),ncol=2))
  colnames(Order_fleet)<-c("qname","order")
  Order_fleet$qname<-test
  Order_fleet$order<-1:length(test)
  Order_fleet$order<-as.numeric(Order_fleet$order)
  
  
  fl_landAllspM_NEP_ordered<-merge(fl_landAllspM_NEP,Order_fleet)
  fl_landAllspM_NEP_ordered<-fl_landAllspM_NEP_ordered[order(fl_landAllspM_NEP_ordered$order,fl_landAllspM_NEP_ordered$stock,fl_landAllspM_NEP_ordered$year),]
  
  fl_landAllspM_NEP_ordered$data[which(fl_landAllspM_NEP_ordered$data==0)]<-NA
  
  fl_landAllspM_NEP_ordered$qname <- as.factor(fl_landAllspM_NEP_ordered$qname)
  #levels(fl_landAllspM_NEP_ordered$qname) <- test
  
  res <- list()
  res[[1]] <- fl_landAllspM_NEP_ordered
  res[[2]] <- test
  
  return(res)
}



repro <- function(){
  
  
  single.species.advice <- read.table(file.path(input.path,'single_species_advice.csv'),sep=",",header=T)
  spp.advice <- melt(single.species.advice,id=c("stock","year"))
  spp.advice <- spp.advice[c("year","stock","variable","value")]; colnames(spp.advice)<-c("year","stock","value","data")
  results <- results[!names(results)=='iter']
  
  # combine the results with single species advice
  mult <- paste("FmultVsF",substr(paste(yr.assess),3,4),sep="") # remove the Fmult values
  plots.comp <- rbind(results[(results$value != mult),],cbind(sc="SingleSpp.Advice",spp.advice))
  plots.comp$data <- as.numeric(plots.comp$data)
  plots.comp <- plots.comp[plots.comp$year>=now,]
  
  
  # also want to plot them as relative
  plots.comp.rel <- dcast(plots.comp,year + stock + value ~ sc,value.var="data")
  colnames(plots.comp.rel) <- c("year","stock","value","FCube.baseline","Single.Spp.Advice")
  plots.comp.rel$diff <- round(100*(plots.comp.rel$FCube.baseline-plots.comp.rel$Single.Spp.Advice)/plots.comp.rel$Single.Spp.Advice,1)
  
  ## dataframe of last 3 years values
  stf_check <- rbind(
    cbind(type="fbar",as.data.frame(lapply(dem.st.fwd,function(x) fbar(x)[,ac(yr.assess:yr.TACp1)]))),
    cbind(type="landings",as.data.frame(lapply(dem.st.fwd,function(x) landings(x)[,ac(yr.assess:yr.TACp1)]))),
    cbind(type="ssb",as.data.frame(lapply(dem.st.fwd,function(x) ssb(x)[,ac(yr.assess:yr.TACp1)])))
  )
  
  
  res <- list()
  res[[1]] <- stf_check
  res[[2]] <- plots.comp.rel
  
  return(res)
  
}
