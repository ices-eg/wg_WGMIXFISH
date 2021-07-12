

# 5.3.1. Renaming of fleets / metiers #####
spfunction <- function(tab=tab){
    
	tab$gear <- substr(tab$metier,1,3)
    
    tab$gear <- toupper(tab$gear)
    tab$fleet <- tab$gear
    sort(unique(tab$fleet))
	
    
    #Assign basic fleet
    tab[tab$fleet %in% c("FPO", "GNS", "GTR","GND","GTN", "LLS","LHM"),"fleet"] <- "Static"
    tab[tab$fleet %in% c("OTM", "PS_","PTM"),"fleet"] <- "Pelagic"   #OTH??
    tab[tab$fleet %in% c("SDN","SSC"),"fleet"] <- "DSeine"
    tab[tab$fleet %in% c("OTB","OTT","PTB","OTG"),"fleet"] <- "Otter"
    tab[tab$fleet %in% c("TBB"),"fleet"] <- "Beam"  #waht do we do with "SMALL BEAM"?
    tab[tab$fleet %in% c("DRB"),"fleet"] <- "OTH"  #  OTH??
    tab$fleet[tab$fleet %in% c("MIS","AG_","NK_","OFG","FPN","LTL","HMD", "LNB", "DRH","RG_","GNC","LHP","SPR")] <- "OTH"
    
    #check
    unique(tab$fleet)
    
    tab$metier <- tab$gear
    #Beam
    # for checking a metier :sort(unique(sp1$mesh_size[sp1$gear %in% c("BEAM","SMALL_BEAM","TBB")]) )
    tab$metier[tab$gear %in% c("TBB") & tab$mesh_size %in% c(">=120_0_0","-1_0_0")] <- "BT1" # "-1_0_0 added: SCO submission - must be BT1
    tab$metier[tab$gear %in% c("TBB") & tab$mesh_size %in% c("90-99_0_0", "100-119_0_0","70-99_0_0","70-89_0_0")] <- "BT2"
    #Otter
    # for checking a metier :sort(unique(eff1$mesh_size[eff1$gear %in% c("OTB","OTT","PTB","SDN","SSC","SCC")]))
    tab$metier[tab$gear %in% c("OTB","OTT","PTB","SDN","SSC","SCC","OTG") & tab$mesh_size %in% c(">=120_0_0","100-119_0_0")] <- "TR1"
    tab$metier[tab$gear %in% c("OTB","OTT","PTB","SDN","SSC","SCC","OTG") & tab$mesh_size %in% c("70-99_0_0","90-119_0_0",">=70_0_0")] <- "TR2"
    tab$metier[tab$gear %in% c("OTB","OTT","PTB","SDN","SSC","SCC","OTG") & tab$mesh_size %in% c("70-89_2_35")] <- "TR2_grid"
    tab$metier[tab$gear %in% c("OTB","OTT","PTB","SDN","SSC","SCC","OTG") & tab$mesh_size %in% c("32-69_0_0","32-69_2_22")] <- "OTB32-69"
    tab$metier[tab$gear %in% c("OTB","OTT","PTB","SDN","SSC","SCC","OTG") & tab$mesh_size %in% c("16-31_0_0")] <- "TR3"
    #Gillnets/trammelnets/longlines
    tab$metier[tab$gear %in% c("GNS","GND")] <- "GN1"
    tab$metier[tab$gear %in% c("GTR","GTN")] <- "GT1"
    tab$metier[tab$gear %in% c("LLS","LHM")] <- "LL1"
    
    sort(unique(tab$metier))
    
    
    tab$metier[!tab$metier %in% c("BT1","BT2","TR1","TR2","TR3","GN1","GT1","LL1","OTB32-69","TR2_grid")] <- tolower(tab$metier)[!tab$metier %in% c("BT1","BT2","TR1","TR2","TR3","GN1","GT1","LL1","OTB32-69","TR2_grid")]
    sort(unique(tab$metier))
    tab$metier[tab$metier %in% c("mis","oth","drb","ag_","nk_","ltl","ofg","fpn","hmd","lnb", "drh","rg_","gnc","lhp","spr")] <- "OTH"
    tab$metier[tab$metier %in% c("tbb")] <- "beam_oth"
    tab$metier[tab$metier %in% c("otm", "ps_","ptm")] <- "pelagic"
    tab$metier[tab$metier %in% c("otb","ssc","otg","ott","sdn", "ptb")] <- "otter_oth"
    tab$metier[tab$metier %in% c("fpo")] <- "pots"
    
    sort(unique(tab$metier))
    
    #FDF static gears
    ma <- which(tab$fleet=="Static" & tab$vessel_length %in% grep("FDF",unique(tab$vessel_length),value=T)) 
    tab$metier[ma] <- paste(tab$metier[ma],"FDF",sep="_")
    
    #check
    sort(unique(tab$metier))

	# 5.2.3. Assign basic vessel category #####
	tab$vessel_length <- gsub("m","",tab$vessel_length)
	sort(unique(tab$vessel_length))
	a<-tab$vessel_length
	idx<-match(tab$vessel_length, veslenTab$Vessel_length)
	tab$vessel_length<- veslenTab$Fcube_vessel_length[idx]

	sort(unique(tab$vessel_length))


    
    # vessel length already standardized
    
    tab$fl2 <- paste(tab$fleet, tab$vessel_length,sep="")
    tab$fl1 <- paste(tab$country,paste(tab$fleet, tab$vessel_length,sep=""),sep="_")
    
    # choice of the fleets based on FOI - and further aggregation after discussion
    
    # one OTH per country
    ma <- which(tab$fleet %in% c("OTH"))
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
    
    
#Sweden aggregation
    ma <- which(tab$country %in% c("SW"))    #"BE", "FR", "GE",
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
table(tab$fl1[tab$country=="SW"],tab$metier[tab$country=="SW"])
    
# belgium aggregation
    ma <- which(tab$country %in% c("BE") & tab$fleet %in% c("Otter","DSeine", "Static"))    
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
    
    ma <- which(tab$fl1 %in% c("BE_Beam>=40","BE_Beam24-40"))    
    tab$fl1[ma] <- "BE_Beam>=24"
    
    ma <- which(tab$fl1 %in% c("BE_Beam10-24"))    
    tab$fl1[ma] <- "BE_Beam<24"
    
    #removing the shrimpers
    ma <- which(tab$country %in% c("BE") & tab$metier %in% c("beam") & tab$mesh_size %in% c("16-31_0_0") )    
    tab$fl1[ma] <- "BE_OTH"  
 table(tab$fl1[tab$country=="BE"],tab$metier[tab$country=="BE"])
   
# Denmark aggregation
    
    ma <- which(tab$country %in% c("DK") & tab$vessel_length %in% c("<10") & tab$fleet %in% c("Otter","DSeine"))    
    tab$fl1[ma] <- "DK_<10towed"
    
    tab$fl1[tab$fl1 %in% c("DK_DSeine10-24","DK_DSeine24-40","DK_DSeineall","DK_SeineNA","DK_DSeineNA")] <- "DK_Seine"
    tab$fl1[tab$fl1 %in% c("DK_Otter10-24","DK_Otterall","DK_OtterNA")] <- "DK_Otter<24" #PLEASE CHECK 0TTERALL NEXT TIME!
    tab$fl1[tab$fl1 %in% c("DK_Beam>=40","DK_Beam10-24","DK_Beam24-40")] <- "DK_Beam"   
    tab$fl1[tab$fl1 %in% c("DK_Pelagic>=40","DK_Pelagic10-24","DK_Pelagic24-40","DK_Pelagicall")] <- "DK_Pelagic"
    tab$fl1[tab$fl1 %in% c("DK_Static10-24","DK_Static24-40","DK_Staticall","DK_Static>=40","DK_Static<10", "DK_StaticFDF","DK_StaticNA" )] <- "DK_Static"    
    tab$fl1[tab$fl1 %in% c("DK_Otter24-40","DK_Otter>=40" )] <- "DK_Otter>=24"  #clu 31/05/2017 
    
    ma  <- which(tab$country %in% c("DK") & tab$vessel_length=="FDF" & !tab$fleet=="Static") #,   
    tab$fl1[ma] <- paste(tab$country[ma], "FDF",sep="_")
  table(tab$fl1[tab$country=="DK"],tab$metier[tab$country=="DK"])
  
 #england aggregation
    
    ma <- which(tab$country %in% c("EN") & tab$vessel_length %in% c("<10") )    
    tab$fl1[ma] <- "EN_<10"
    
    tab$fl1[tab$fl1 %in% c("EN_Otter24-40","EN_DSeine24-40")] <- "EN_Otter24-40"
    tab$fl1[tab$fl1 %in% c("EN_Otter>=40","EN_DSeine>=40")] <- "EN_Otter>=40"
    tab$fl1[tab$fl1 %in% c("EN_Otter10-24","EN_DSeine10-24")] <- "EN_Otter<24"
    tab$fl1[tab$fl1 %in% c("EN_Beam>=40","EN_Beam10-24","EN_Beam24-40")] <- "EN_Beam"   
    tab$fl1[tab$fl1 %in% c("EN_Pelagic>=40","EN_Pelagic10-24")] <- "EN_Pelagic"
    tab$fl1[tab$fl1 %in% c("EN_Static10-24","EN_Static24-40","EN_Static>=40","EN_StaticFDF","EN_StaticNA","EN_Staticall")] <- "EN_Static"    
    
    ma  <- which(tab$country %in% c("EN")  & tab$vessel_length=="FDF" & !tab$fleet=="Static") #,        
    tab$fl1[ma] <- paste(tab$country[ma], "FDF",sep="_")
 table(tab$fl1[tab$country=="EN"],tab$metier[tab$country=="EN"])
       
    #
    # scotland aggregation
    
    ma <- which(tab$country %in% c("SC") & tab$vessel_length %in% c("<10") & tab$fleet %in% c("Otter","DSeine"))    
    tab$fl1[ma] <- "SC_Otter<10"
    
    tab$fl1[tab$fl1 %in% c("SC_Otter24-40","SC_Otter>=40","SC_DSeine>=40","SC_DSeine24-40")] <- "SC_Otter>=24"
    tab$fl1[tab$fl1 %in% c("SC_Otter10-24","SC_DSeine10-24")] <- "SC_Otter<24"
    tab$fl1[tab$fl1 %in% c("SC_Beam>=40","SC_Beam10-24","SC_Beam24-40","SC_Beam<10")] <- "SC_Beam"   
    tab$fl1[tab$fl1 %in% c("SC_OTH>=40","SC_OTH10-24","SC_OTH24-40","SC_OTHall")] <- "SC_OTH"      
    tab$fl1[tab$fl1 %in% c("SC_Pelagic>=40","SC_Pelagic10-24","SC_Pelagic24-40")] <- "SC_Pelagic"
    tab$fl1[tab$fl1 %in% c("SC_Static10-24","SC_Static24-40","SC_Staticall")] <- "SC_Static"    
    
    #
    ma  <- which(tab$country %in% c("SC") & tab$fl2 %in% grep("FDF",unique(tab$fl2),value=T)) #,        
    tab$fl1[ma] <- paste(tab$country[ma], "FDF",sep="_")
 table(tab$fl1[tab$country=="SC"],tab$metier[tab$country=="SC"])
   
    #
    
# dutch aggregation
    ma  <- which(tab$country %in% c("NL") & !tab$fl2 %in% c("Beam>=40","Beam24-40"))
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
    tab$fl1[tab$fl1 =="NL_Beam"] <- "NL_Beam<24"
    
    ma <- which(tab$fl1 %in% c("NL_DSeine","NL_Otter"))    
    tab$fl1[ma] <- "NL_Otter"
 table(tab$fl1[tab$country=="NL"],tab$metier[tab$country=="NL"])
   
    
# Norway aggregation 
    tab$fl1[tab$fl1 %in% c("NO_Beam>=40","NO_Beam10-24","NO_Beam24-40","NO_Beamall")] <- "NO_Beam"
    tab$fl1[tab$fl1 %in% c("NO_Static<24","NO_Static>=40","NO_Static24-40","NO_Staticall","NO_Static10-24","NO_StaticNA","NO_Static_")] <- "NO_Static"
    tab$fl1[tab$fl1 %in% c("NO_Pelagic<24","NO_PEL<24","NO_PEL>=40","NO_PEL24-40","NO_Pelagic>=40","NO_Pelagic10-24","NO_Pelagic24-40")] <- "NO_Pelagic"
    
    # WG2017 NO did not submit a data refresh ubmission.  We have used IC dta to make a catch and effort submission file.  This means 
    # that we have no information on vessel lengths.  All Otter and DSeine to NO_Otter
    tab$fl1[tab$fl1 %in% c("NO_DSeineall","NO_Otterall")] <- "NO_Otter"
    #sp1$fl1[sp1$fl1 %in% c("NO_DSeine","NO_DSeine<24","NO_DSeineall","NO_Otter<24","NO_Otter0012","NO_Otter<10","NO_Otter10-24","NO_Otter12<18",
    #                       "NO_Otter18<24","NO_Otter24-40","NO_Otterall","NO_DSeine10-24","NO_DSeine24-40","NO_DSeineNA","NO_Otter<40","NO_OtterNA")] <- "NO_Otter<40"
    #sp1$fl1[sp1$fl1 %in% c("NO_Otter>=40","NO_DSeine>=40")] <- "NO_Otter>=40"
  table(tab$fl1[tab$country=="NO"],tab$metier[tab$country=="NO"])
   
    
    
 #french aggregation
    
    ma <- which(tab$fl1 %in% c("FR_Otter15") & tab$metier=="TR1")
    tab$fl1[ma] <- "FR_Otter>=40"
    
    ma <- which(tab$fl1 %in% c("FR_Otter15") & tab$metier=="TR2")
    tab$fl1[ma] <- "FR_Otter10-40"
    
    ma <- which(tab$fl1 %in% c("FR_Otter15") & tab$metier %in% c("OTB32-69","TR3","otter_oth"))
    tab$fl1[ma] <- "FR_OTH"
    
    ma <- which(tab$fl1 %in% c("FR_Beam10-24","FR_Beam15","FR_Beamall","FR_Beam>=40","FR_Beam24-40"))    
    tab$fl1[ma] <- "FR_Beam"
    
    ma <- which(tab$fl1 %in% c("FR_Otter24-40","FR_Otter10-24","FR_Otterall","FR_OtterNA"))    
    tab$fl1[ma] <- "FR_Otter10-40"
    
    ma <- which(tab$country %in% c("FR") & tab$fleet %in% c("DSeine", "Pelagic","Static"))
    tab$fl1[ma] <- "FR_OTH"
    
    ma <- which(tab$country %in% c("FR") & tab$metier %in% c("otter"))    
    tab$fl1[ma] <- "FR_OTH" 
    
    ma <- which(tab$country %in% c("FR") & tab$metier %in% c("GT1","GN1"))    
    tab$fl1[ma] <- "FR_Nets"
    
    ma <- which(tab$country %in% c("FR") & tab$vessel_length %in% c("<10"))    
    tab$fl1[ma] <- "FR_<10"
    
table(tab$fl1[tab$country=="FR"],tab$metier[tab$country=="FR"])
table(tab$fl1[tab$country=="FR" & tab$year==2017],tab$metier[tab$country=="FR" & tab$year==2017], tab$area[tab$country=="FR" & tab$year==2017])
table(tab$fl1[tab$country=="FR" & tab$year%in%c(2016,2017)],tab$metier[tab$country=="FR"& tab$year%in%c(2016,2017)],
      tab$area[tab$country=="FR"& tab$year%in%c(2016,2017)], tab$year[tab$country=="FR"& tab$year%in%c(2016,2017)])
    
    #german aggregation
    
    ma <- which(tab$fl1 %in% c("GE_Beam10-24","GE_Beam<10"))    
    tab$fl1[ma] <- "GE_Beam<24"
    
    ma <- which(tab$fl1 %in% c("GE_Beam>=40","GE_Beam24-40"))    
    tab$fl1[ma] <- "GE_Beam>=24"
    
    ma <- which(tab$country %in% c("GE") & tab$fleet %in% c("Static"))    
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
    
    ma <- which(tab$country %in% c("GE") & tab$fleet %in% c("Pelagic"))
    tab$fl1[ma] <- "GE_OTH"
    
    ma <- which(tab$fl1 %in% c("GE_Otter10-24","GE_DSeine10-24"))    
    tab$fl1[ma] <- "GE_Otter<24"
    
    ma <- which(tab$fl1 %in% c("GE_DSeine24-40","GE_Otter24-40"))    
    tab$fl1[ma] <- "GE_Otter24-40"
    
    
    ma  <- which(tab$country %in% c("GE") & tab$fl2 %in% grep("FDF",unique(tab$fl2),value=T)) #,   
    tab$fl1[ma] <- paste(tab$country[ma], "FDF",sep="_")
    
    #removing the shrimpers
    ma <- which(tab$country %in% c("GE") & tab$metier %in% c("beam") & tab$mesh_size %in% c("16-31_0_0") )    
    tab$fl1[ma] <- "GE_OTH"
 
table(tab$fl1[tab$country=="GE"],tab$metier[tab$country=="GE"])


 
#Northern Ireland
    ma <- which(tab$country %in% c("NI") & tab$fleet %in% c("Otter"))    
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
    
    ma <- which(tab$country %in% c("NI") & !tab$fleet %in% c("Otter"))    
    tab$fl1[ma] <- "NI_OTH"
    
# Ireland
    
    ma <- which(tab$country %in% c("IE"))    #"BE", "FR", "GE",
    tab$fl1[ma] <- paste(tab$country[ma], tab$fleet[ma],sep="_")
    
    #
    
    tab$fl2 <- tab$fleet
    tab$fleet <- tab$fl1
    
    sort(unique(tab$fleet))
    
    tab$ID<-paste(tab$fleet,tab$metier,tab$area,sep=".")
    tab$ID2<-paste(tab$fleet,tab$metier,tab$area,tab$year,sep=".")

    return(tab)    
}  