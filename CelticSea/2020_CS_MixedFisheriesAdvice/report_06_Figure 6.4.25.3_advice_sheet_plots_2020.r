#################################################################################
################################## MIXFISH PLOTS ################################
#################################################################################

## Paul Dolder

## 28/05/2015
## Celtic Sea

## This script runs on the extract 'ca.csv'

##
## 31/05/2018 - PJD rewritten from fleets object
##

## It produces the landings by metier plot (for advice sheet annex)
## and the total landings pie plot (for advice sheet catch box)
## and outputs the figures for landings, discards, discard percentage
## and the landings by fleet (for advice sheet catch box)
## Includes stocks COD, HAD, WHG
## NEP3A currently excluded
## If the Species or Metiers change, the script will need to be changed to reflect

#################################################################################

rm(list=ls())
res.path<-file.path("results/")
plot.path<-file.path("plots/")

library(reshape2) ; library(ggplot2) ;library(grid); library(data.table)

library(FLCore); library(FLFleet)
library(tidyr); library(dplyr)

source("bootstrap/software/functions/FLFcube_FLCore_R31.R")
source("bootstrap/software/functions/remove_validity_FLFleet.R")
source("bootstrap/software/functions/funcs.R")


load("results/02_Making_Fleets_Celtic_Sea_2020tier12nepnewLOUsingSAM_KW.RData")

df <- slot.fleet(fleets, "landings")
dfD <-slot.fleet(fleets, "discards")
dfC <-slot.fleet(fleets, "catch")

# merge catch categories
df <- merge(df, dfD, all = T)
df <- merge(df, dfC, all = T)
rm(dfD, dfC)

df$area <- substr(df$metier, 9, 14)
df$metier<-substr(df$metier,1,7)

colnames(df)[colnames(df)=="qname"] <-"stock"

levels(df$stock)[levels(df$stock) %in% grep("nep", unique(df$stock), value = TRUE)]<-"nep.27.7bk"

# aggregate it all up
dfa<-aggregate(df[c("landings","discards","catch")],by=list(Year=df$year,Area= df$area,Stock = df$stock),sum, na.rm = T) # #for area
df<-aggregate(df[c("landings","discards","catch")],by=list(Year=df$year,Metier = df$metier,Stock = df$stock),sum, na.rm = T)

# melt
df <-reshape2::melt(df,id=c("Year","Metier","Stock"))
dfa<-reshape2::melt(dfa,id=c("Year","Area","Stock"))

# order the dataframe
df  <-df[(order(df$Year,df$Metier,df$Stock)),]
dfa <-dfa[(order(dfa$Year,dfa$Area,dfa$Stock)),]

plot.path <- "plots"

###### PLOT CODE #####
none <- element_blank()

################################
## LANDINGS BARPLOT BY METIER ##
################################

## Make sure the stocks are labelled with numbers

levels(df$Stock)[levels(df$Stock) == "cod.27.7e-k"] <-"1:cod.27.7e-k"
levels(df$Stock)[levels(df$Stock) == "had.27.7b-k"] <- "2:had.27.7b-k"
levels(df$Stock)[levels(df$Stock) == "meg.27.7b-k8abd"] <-"3:meg.27.7b-k8abd"
levels(df$Stock)[levels(df$Stock) == "mon.27.78abd"] <- "4:mon.27.78abd"
levels(df$Stock)[levels(df$Stock) == "sol.27.7fg"] <-"5:sol.27.7fg"
levels(df$Stock)[levels(df$Stock) == "whg.27.7b-ce-k"] <- "6:whg.27.7b-ce-k"
levels(df$Stock)[levels(df$Stock) == "nep.27.7bk"] <- "7:nep.27.7bk"

df$Stock <- factor(df$Stock, levels = sort(levels(df$Stock)))

levels(dfa$Stock)[levels(dfa$Stock) == "cod.27.7e-k"] <-"1:cod.27.7e-k"
levels(dfa$Stock)[levels(dfa$Stock) == "had.27.7b-k"] <- "2:had.27.7b-k"
levels(dfa$Stock)[levels(dfa$Stock) == "meg.27.7b-k8abd"] <-"3:meg.27.7b-k8abd"
levels(dfa$Stock)[levels(dfa$Stock) == "mon.27.78abd"] <- "4:mon.27.78abd"
levels(dfa$Stock)[levels(dfa$Stock) == "sol.27.7fg"] <-"5:sol.27.7fg"
levels(dfa$Stock)[levels(dfa$Stock) == "whg.27.7b-ce-k"] <- "6:whg.27.7b-ce-k"
levels(dfa$Stock)[levels(dfa$Stock) == "nep.27.7bk"] <- "7:nep.27.7bk"

dfa$Stock <- factor(dfa$Stock, levels = sort(levels(dfa$Stock)))

## For other ares

dfa$Area[dfa$Area %in% c("","27.7.a", "27.7.d")] <- "OTH"

dfa <- dfa %>% group_by(Year, Area, Stock, variable) %>%
  summarise(value = sum(value))

pal <- pals::brewer.paired(12)[1:length(unique(df$Stock))]

data.yr<-2019

p<-ggplot(df[(df$variable=="landings" & df$Year==data.yr),],aes(factor(Metier),value/1000,fill=Stock))

p + geom_bar(stat="identity",position = "stack") +# ylim(0,13)+
  theme(panel.grid.major = none, panel.grid.minor = none) +  theme(panel.background = none) +
 # scale_fill_grey(start=0,end=1)+
  scale_fill_manual(values = pal) + 
  theme(panel.border = none) + theme(axis.line = element_line(colour = "grey50")) +
  theme_bw() +
  xlab("Metiers used by mixed-fisheries model") + ylab("Landings ('000 tonnes)")  +
  theme(axis.text.x = element_text(angle = 90,size = 8))  +
  theme(legend.key = element_rect(colour = "black"), legend.position=c(0.9,0.7),legend.key.size=unit(0.5,"cm"),
        legend.background = element_rect(colour="black", size=.5),
        legend.text=element_text(face="bold",size=8),
        legend.title=element_text(face="bold",size=8),
        legend.title.align=0.5) +
    theme(axis.text = element_text(lineheight=0.8, size=8,face="bold")) +
    theme(axis.title = element_text(size=12,face="bold")) +
   geom_bar(stat="identity",position = "stack",colour="black",show_guide=FALSE) +
  annotate("text", label=" ",x=5.5,y=18,fontface="bold",size=6) #label="Landings by species / metier")

ggsave(file= "plots/Celtic Sea Figure 6.4.25.3_advice_sheet_landing_by_metier_plot.png",width=8,height=4.8)

###########################
## By area
###########################

pa<-ggplot(dfa[(dfa$variable=="landings" & dfa$Year==data.yr),],aes(factor(Area),value/1000,fill=Stock))

pa + geom_bar(stat="identity",position = "stack") +# ylim(0,13)+
  theme(panel.grid.major = none, panel.grid.minor = none) +  theme(panel.background = none) +
 # scale_fill_grey(start=0,end=1)+
  scale_fill_manual(values = pal) + 
  theme(panel.border = none) + theme(axis.line = element_line(colour = "grey50")) +
  theme_bw() +
  xlab("Areas used by mixed-fisheries model") + ylab("Landings ('000 tonnes)")  +
  theme(axis.text.x = element_text(angle = 90,size = 8))  +
  theme(legend.key = element_rect(colour = "black"), legend.position=c(0.1,0.7),legend.key.size=unit(0.5,"cm"),
        legend.background = element_rect(colour="black", size=.5),
        legend.text=element_text(face="bold",size=8),
        legend.title=element_text(face="bold",size=8),
        legend.title.align=0.5) +
    theme(axis.text = element_text(lineheight=0.8, size=8,face="bold")) +
    theme(axis.title = element_text(size=12,face="bold")) +
   geom_bar(stat="identity",position = "stack",colour="black",show_guide=FALSE) +
  annotate("text", label=" ",x=5.5,y=18,fontface="bold",size=6) #label="Landings by species / metier")

ggsave(file= "plots/Celtic Sea Figure 6.4.25.3_advice_sheet_landing_by_area_plot.png",width=8,height=4.8)





###################
#pie plot landings#
###################

df2<-df[(df$variable=="landings" & df$Year==data.yr),]
df2<-aggregate(df2["value"],by=list(Stock = df2$Stock),sum,na.rm=T)

p<- ggplot(df2,aes(x="",y=value,fill=Stock))

 p + geom_bar(width = 1,stat="identity") +
  scale_fill_manual(values = pal)+
  coord_polar("y", start=0) +
  xlab("")+ylab("")+
 theme(axis.text.x = element_blank(),
  panel.border= element_blank(),
  panel.background = element_blank()) +
  geom_bar(width = 1,stat="identity",colour="black",show_guide=FALSE) +
  theme(legend.key.size=unit(1,"cm"),
        legend.text=element_text(face="bold",size=10),
        legend.title=element_text(face="bold",size=10),
        legend.title.align=0.5,
        legend.background = element_rect(colour="black", size=.5)) +
  annotate("text",x=1.8,y=1,label="Total Landings by Stock",fontface="bold",size=10)

ggsave(file= "plots/Celtic Sea Catch_distribution_figure_advice_sheet.png",width=11.7,height=8.3)

################################################
## Landings, Discards totals for advice sheet ##
################################################

land<-df[(df$variable=="landings" & df$Year==data.yr),]
disc<-df[(df$variable=="discards" & df$Year==data.yr),]

print(paste("-----Landings =",round(sum(land$value,na.rm=T),0),paste("t -------")))
print(paste("-----Discards =",round(sum(disc$value,na.rm=T),0),paste("t -------")))
print(paste("-----Discard  =",round(100*sum(disc$value,na.rm=T)/sum(sum(land$value,na.rm=T),sum(disc$value,na.rm=T)),0),paste("% -------")))


Fleet.summary.func<-function(x) {
  if (x %in% c("OTB_CRU","OTT_CRU","OTT_DEF","OTB_DEF","SSC_DEF","OTM_DEF"))
    return("Otter trawls and seines")
  if (x %in% c("TBB_DEF"))
    return("Beam trawls")
  if(x %in% c("GNS_DEF","GTR_DEF"))
    return("Gill and trammel nets")
  if(x %in% c("LLS_FIF"))
    return("Longlines")
  if (x %in% c("OTH","MIS_MIS"))
    return("Other gears")
    else
    return("THIS SHOULDN'T BE HERE!!")
}

## and apply the function (assigning metiers to a fleet...)
land$Fleet<-sapply(land$Metier,Fleet.summary.func)

unique(land$Fleet) ## check that all metiers allocated to a fleet

## Fleet summary
print(paste("-----Otter trawls and seines =",round(sum(land$value[(land$Fleet=="Otter trawls and seines")]),0),paste("t -------"),round(100*sum(land$value[(land$Fleet=="Otter trawls and seines")],na.rm=T)/sum(land$value,na.rm=T),0),paste("%")))
print(paste("-----Beam trawls =",round(sum(land$value[(land$Fleet=="Beam trawls")]),0),paste("t -------"),round(100*sum(land$value[(land$Fleet=="Beam trawls")],na.rm=T)/sum(land$value,na.rm=T),0),paste("%")))
print(paste("-----Gill and trammel nets =",round(sum(land$value[(land$Fleet=="Gill and trammel nets")]),0),paste("t -------"),round(100*sum(land$value[(land$Fleet=="Gill and trammel nets")],na.rm=T)/sum(land$value,na.rm=T),0),paste("%")))
print(paste("-----Longlines =",round(sum(land$value[(land$Fleet=="Longlines")]),0),paste("t -------"),round(100*sum(land$value[(land$Fleet=="Longlines")],na.rm=T)/sum(land$value,na.rm=T),0),paste("%")))
print(paste("-----Other gears =",round(sum(land$value[(land$Fleet=="Other gears")],na.rm=T),0),paste("t -------"),round(100*sum(land$value[(land$Fleet=="Other gears")],na.rm=T)/sum(land$value,na.rm=T),0),paste("%")))

