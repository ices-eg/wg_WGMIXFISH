## recalculate quota after swaps based on FIDES data

rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
gc(); graphics.off()

options(stringsAsFactors = FALSE, scipen=100)


in.path  <- "bootstrap/data/FIDES"
out.path <- "data/FIDES_out"

year <- 2019

# fides <- read.csv(paste0("bootstrap/initial/data/FIDES/QUOTA ",year," from FIDES via email.csv"), sep=";")
# fides <- read.xlsx2(paste0("bootstrap/data/FIDES/QUOTA-",year,"-ALLE-LS.xlsx"), sheetIndex=1)

fides1 <- read.csv(paste0("bootstrap/initial/data/FIDES/QUOTA ",year," from FIDES via email.csv"), sep=";")
fides2 <- read.csv(paste0("bootstrap/initial/data/FIDES/QUOTA ",year," from FIDES via email.csv"), sep=",")

if(any("Initial.Quantity"%in% colnames(fides1))){fides <- fides1}
if(any("Initial.Quantity"%in% colnames(fides2))){fides <- fides2}

# change commas to points
fides$Initial.Quantity    <- as.numeric(as.character(gsub(",",".",fides$Initial.Quantity)))
fides$Adapted.Quota       <- as.numeric(as.character(gsub(",",".",fides$Adapted.Quota)))



# northSea TAC stocks
cod_ns <- c("COD_4; Union waters of 2a; that part of 3a not covered by the Skagerrak and the Kattegat",
            "COD_4; Union waters of 2a; that part of 3a not covered by the Skagerrak and Kattegat",
            "COD_Skagerrak",
            "COD_7d")

had    <- c("HAD_4; Union waters of 2a",
            "HAD_3a",
            "HAD_Union and international waters of 5b and 6a")

whg_ns <- c("WHG_4; Union waters of 2a")

sol_ns <- c("SOL_Union waters of 2a and 4")  # do we also need "SOL_Union waters of 4"

sol_ec <- c("SOL_7d")

ple_ns <- c("PLE_4; Union waters of 2a; that part of 3a not covered by the Skagerrak and the Kattegat",
            "PLE_Skagerrak")

ple_ec <- c("PLE_7d and 7e")

pok    <- c("POK_3a and 4; Union waters of 2a",
            "POK_Union waters of 4; 3a",
            "POK_6 and Union and international waters of 5b, 12 and 14, north of 56° 30' N",
            "POK_6; Union and international waters of 5b; international waters of 12 and 14") # added tis one

nep    <- c("NEP_Union waters of 2a and 4")

wit    <- c("L/W_Union waters of 2a and 4")


#####
fides$Species.Code_Area.Description <- paste(fides$Species.Code,fides$Area.Description,sep="_")

fi <- subset(fides, Species.Code_Area.Description %in% c(cod_ns,had,whg_ns,sol_ns,sol_ec,ple_ns,ple_ec,pok,nep,wit))
fi[fi$Species.Code_Area.Description=="COD_4; Union waters of 2a; that part of 3a not covered by the Skagerrak and the Kattegat","Stock"] <- "COD-NS"
fi[fi$Species.Code_Area.Description=="COD_Skagerrak","Stock"] <- "COD-NS"
fi[fi$Species.Code_Area.Description=="COD_7d","Stock"] <- "COD-NS"
fi[fi$Species.Code_Area.Description=="COD_4; Union waters of 2a; that part of 3a not covered by the Skagerrak and Kattegat","Stock"] <- "COD-NS"

fi[fi$Species.Code_Area.Description=="HAD_4; Union waters of 2a","Stock"] <- "HAD"
fi[fi$Species.Code_Area.Description=="HAD_3a","Stock"] <- "HAD"
fi[fi$Species.Code_Area.Description=="HAD_Union and international waters of 5b and 6a","Stock"] <- "HAD"

fi[fi$Species.Code_Area.Description=="WHG_4; Union waters of 2a","Stock"] <- "WHG-NS"

fi[fi$Species.Code_Area.Description=="SOL_Union waters of 2a and 4","Stock"] <- "SOL-NS"

fi[fi$Species.Code_Area.Description=="SOL_7d","Stock"] <- "SOL-EC"

fi[fi$Species.Code_Area.Description=="PLE_4; Union waters of 2a; that part of 3a not covered by the Skagerrak and the Kattegat","Stock"] <- "PLE-NS"
fi[fi$Species.Code_Area.Description=="PLE_Skagerrak","Stock"] <- "PLE-NS"

fi[fi$Species.Code_Area.Description=="PLE_7d and 7e","Stock"] <- "PLE-EC"

fi[fi$Species.Code_Area.Description=="POK_3a and 4; Union waters of 2a","Stock"] <- "POK"
fi[fi$Species.Code_Area.Description=="POK_Union waters of 4; 3a","Stock"] <- "POK"
fi[fi$Species.Code_Area.Description=="POK_6 and Union and international waters of 5b, 12 and 14, north of 56° 30' N","Stock"] <- "POK"
fi[fi$Species.Code_Area.Description=="POK_6; Union and international waters of 5b; international waters of 12 and 14","Stock"] <- "POK"

fi[fi$Species.Code_Area.Description=="NEP_Union waters of 2a and 4","Stock"] <- "NEP-NS"

fi[fi$Species.Code_Area.Description=="L/W_Union waters of 2a and 4","Stock"] <- "WIT"


head(fi)
fi <- fi[,c("Species.Code", "Area.Code", "Stock","Level.Code", "Initial.Quantity", "Adapted.Quota")]
fi$Species.Code   <- as.character(fi$Species.Code)
fi$Area.Code      <- as.character(fi$Area.Code)
fi$Level.Code     <- as.character(fi$Level.Code)


fi <- aggregate(list(initialQuota=fi$Initial.Quantity,finalQuota=fi$Adapted.Quota),
                list(stock=fi$Stock, country=fi$Level.Code),sum,na.rm=T)


#lookup country
ctry.codes <- unique(fi$country) # 2 character country codes

cc.lookup <- as.data.frame(matrix(
  c(
    "BEL", "BE",
    "DEU", "GE",
    "GBR", "UK",
    "NLD", "NL",
    "DNK", "DK",
    "FRA", "FR",
    "SWE", "SW",
    "NOR", "NO"
  ), ncol=2, byrow = TRUE
))
names(cc.lookup) <- c("orig", "new")
cc.lookup$new  <- as.character(cc.lookup$new)
cc.lookup$orig <- as.character(cc.lookup$orig)


# change country names
repl <- which(fi$country %in% cc.lookup$orig)
fi$country[repl] <- cc.lookup$new[match(fi$country[repl], cc.lookup$orig)]


TotalTAC <- subset(fi,country =="TAC")[-c(2,4)]
UnionTAC <- subset(fi,country =="EEC")[-c(2,4)]

names(TotalTAC)[c(2)] <- c("initialTAC")
names(UnionTAC)[c(2)] <- c("initialTAC")

save(fi, TotalTAC, UnionTAC, file = paste0(out.path,"/Fides_output.RData"))

################################
