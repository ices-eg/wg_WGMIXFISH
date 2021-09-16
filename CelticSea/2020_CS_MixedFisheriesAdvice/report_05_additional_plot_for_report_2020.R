#####################################################
#####################################################
## Celtic Sea Report plots ##########################
#####################################################
#####################################################


library(ggplot2); library(FLCore); library(FLFleet)
library(FLash)
options(scipen=10000)


source("bootstrap/software/functions/FLFcube_FLCore_R31.R")

load(file.path("results/01_Reproduce_the_advice_Celtic_Sea_2020tier12nepnewLOUsingSAM.Rdata"))

load(file.path("results/02_Making_Fleets_Celtic_Sea_2020tier12nepnewLOUsingSAM_KW.RData"))

######################################################
plot.path <- "plots"
##  # plot outputs
pdf(file= file.path(plot.path, "fleet_diagnostics_catch.pdf"))

#############################
### landings and discards ###
#############################

la<-slot.fleet(fleets,"landings")
di<-slot.fleet(fleets,"discards")

la$country<-substr(la$fleet,1,2)
di$country<-substr(di$fleet,1,2)

colnames(la)[7]<-"data"
colnames(di)[7]<-"data"

ca<-rbind(cbind(la,variable="landings"),cbind(di,variable="discards"))

# Remove area tag
ca$metier<-substr(ca$metier,1,7)

# Reaggregate
ca<-aggregate(ca["data"],by=list(age=ca$age,year=ca$year,unit=ca$unit,season=ca$season,area=ca$area,iter=ca$iter,
                                qname=ca$qname,fleet=ca$fleet,metier=ca$metier,country=ca$country,variable=ca$variable),sum,na.rm=T)

ca2 <- ca[ca$variable %in% c('landings'), ]
ca2 <- ca2[ca2$year %in% c('2017','2018','2019'), ]
ca3<-aggregate(ca2$data, list(year=ca2$year,stock=ca2$qname, fleet=ca2$fleet ),FUN=sum)
print(

  ggplot(ca3,aes(x=year,y=x)) + geom_point(aes(shape=stock)) + geom_line(aes(linetype=stock)) +
        facet_wrap(~fleet, ncol = 3, scales="free_y") + theme_bw() + ggtitle("Landings by fleet") +scale_x_continuous(breaks=c(2017,2018,2019))+
        theme(axis.text.x=element_text(angle=-90, vjust=0.3))#new plot for report
  )


c<-unique(ca$country)

for (i in 1:length(c)) {
    cty<-subset(ca,country==c[i])
    sp<-unique(cty$qname)
    for (s in 1:length(sp)) {
        print(ggplot(cty[(cty$qname==sp[s]),],aes(x=year,y=data)) +
                geom_bar(stat="identity",position="stack",aes(fill=variable),colour="black") + facet_grid(fleet~metier) + theme_bw() +
                ggtitle(paste("catch",c[i],sp[s])) + scale_fill_grey() + theme_bw() + theme(axis.text.x=element_text(angle=-90)))
    }
}

dev.off()

###################################
### Effort and metier share #######
###################################

pdf(file=file.path (plot.path, "fleet_diagnostics_effort.pdf"))

ef<-as.data.frame(lapply(fleets,effort))
ef$country<-substr(ef$qname,1,2)

met<-slot.fleet(fleets,"effshare")
met$country<-substr(met$fleet,1,2)

# keep this for q
met_with_area<-met

met$metier<-substr(met$metier,1,7)
met_with_area<-met

met<-aggregate(met[c("effshare")],by=list(age=met$age,year=met$year,season=met$season,area=met$area,
                                          iter=met$iter,fleet=met$fleet,metier=met$metier,country=met$country),sum,na.rm=T)

print(ggplot(met,aes(x=year,y=effshare)) + geom_point(aes(shape=metier)) + geom_line(aes(linetype=metier)) + scale_x_continuous(breaks=c(2010,2012,2014,2016,2018,2020))+
        facet_wrap(~fleet, ncol = 4) + theme_bw() + ggtitle("Metier effort share") +
        theme(axis.text.x=element_text(angle=-90, vjust=0.3))) # new plot for report

for (i in 1:length(c)) {
    print(ggplot(ef[(ef$country==c[i]),],aes(x=year,y=data)) + geom_point() + geom_line(group=1) +
            facet_wrap(~qname) + theme_bw() + ggtitle(paste("Effort",c[i])) + theme(axis.text.x=element_text(angle=-90)))

    print(ggplot(met[(met$country==c[i]),],aes(x=year,y=effshare)) + geom_point(aes(shape=metier)) + geom_line(aes(linetype=metier)) +
            facet_grid(fleet~metier) + theme_bw() + ggtitle(paste("Metier effort share",c[i])) +
            theme(axis.text.x=element_text(angle=-90)))


}

dev.off()

##########################
###### Catchability ######
##########################

pdf(file=file.path (plot.path,"fleet_diagnostics_q.pdf"))

q<-slot.fleet(fleets,"catch.q")
q$country<-substr(q$fleet,1,2)

# across areas want to calc the q weighted by effort

for (i in 1:length(c)) {
    cty<-subset(q,country==c[i])
    cty<-cty[!(is.na(cty$catch.q)),]
    sp<-unique(cty$qname)
    for (s in 1:length(sp)) {
        print(ggplot(cty[(cty$qname==sp[s]),],aes(x=year,y=catch.q)) +
                geom_point() + geom_smooth(method="loess") + facet_grid(fleet~metier) + theme_bw() +
                ggtitle(paste("catchability",c[i],sp[s])) +  theme_bw() + theme(axis.text.x=element_text(angle=-90)))
  }
}

dev.off()

## Plots of effort by scenario

load("results/04_FCube_Forecasts_Celtic_Sea_2020tier12nepnewLOUsingSAMSkipIntYr_FcubeAllObjects.Rdata")
res.effort$effort[res.effort$effort == Inf] <- 0 # 2019 fix

print(ggplot(res.effort[(res.effort$year==2021 & res.effort$fleet !="OTH_OTH"),],aes(x=scenario,y=effort)) +
        geom_bar(stat="identity") + facet_wrap(~fleet,scale="free_y") + theme_bw() +
        theme(axis.text.x=element_text(angle=-90)) + ylab("effort ('000 kw days)"))
ggsave(file=file.path(plot.path,'Effort_by_scenario.png'),width=8,height=8)
