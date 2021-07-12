
fleetsSum <- function(fleets, yrs){
  L <- D <- vector(mode = "list", length(fleets))
  for(i in seq(length(fleets))){
    for(j in seq(length(fleets[[i]]@metiers))){
      for(k in seq(length(fleets[[i]]@metiers[[j]]@catches))){
        tmpL <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@landings[,yrs])
        tmpD <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@discards[,yrs])
        tmpL$fleet <- tmpD$fleet <- name(fleets[[i]])
        tmpL$metier <- tmpD$metier <- name(fleets[[i]]@metiers[[j]])
        tmpL$stock <- tmpD$stock <- name(fleets[[i]]@metiers[[j]]@catches[[k]])
        if(j == 1 & k == 1){
          dfL <- tmpL
          dfD <- tmpD
        } else {
          dfL <- rbind(dfL, tmpL)
          dfD <- rbind(dfD, tmpD)
        }
        # print(paste("| fl =", i, "| met =", j, "| stk =", k, "|"))
      }
    }
    L[[i]] <- dfL
    D[[i]] <- dfD
  
  }
  L <- do.call("rbind", L)
  D <- do.call("rbind", D)
  L$variable <- "landings"
  D$variable <- "discards"
  dim(L); dim(D)
  DF <- rbind(L, D)
  return(DF)
  
}





catchSum <- function(fleets, yrs){
  RES <- vector(mode = "list", length(fleets))
  for(i in seq(length(fleets))){
    for(j in seq(length(fleets[[i]]@metiers))){
      for(k in seq(length(fleets[[i]]@metiers[[j]]@catches))){
        tmpL.n <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.n[,yrs])
        tmpL.wt <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@landings.wt[,yrs])
        tmpD.n <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.n[,yrs])
        tmpD.wt <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@discards.wt[,yrs])
        
        tmpFpart <- as.data.frame(fleets[[i]]@metiers[[j]]@catches[[k]]@catch.q %*% (fleets[[i]]@effort * fleets[[i]]@metiers[[j]]@effshare))
        
        tmpL.n$fleet <- tmpL.wt$fleet <- tmpD.n$fleet <- tmpD.wt$fleet <- tmpFpart$fleet <- name(fleets[[i]])
        tmpL.n$metier <- tmpL.wt$metier <- tmpD.n$metier <- tmpD.wt$metier <- tmpFpart$metier <- name(fleets[[i]]@metiers[[j]])
        tmpL.n$stock <- tmpL.wt$stock <- tmpD.n$stock <- tmpD.wt$stock <- tmpFpart$stock <- name(fleets[[i]]@metiers[[j]]@catches[[k]])
        
        # rename and merge
        names(tmpL.n)[7] <- "landings.n"
        names(tmpL.wt)[7] <- "landings.wt"
        names(tmpD.n)[7] <- "discards.n"
        names(tmpD.wt)[7] <- "discards.wt"
        names(tmpFpart)[7] <- "harvest"
        
        res.k <- merge(x = tmpL.n, y = tmpL.wt)
        res.k <- merge(x = res.k, y = tmpD.n)
        res.k <- merge(x = res.k, y = tmpD.wt)
        res.k <- merge(x = res.k, y = tmpFpart)
        
        if(j == 1 & k == 1){
          res.j <- res.k
        } else {
          res.j <- rbind(res.j, res.k)
        }
        print(paste("| fl =", i, "| met =", j, "| stk =", k, "|"))
      }
    }
    RES[[i]] <- res.j
  }
  RES <- do.call("rbind", RES)
  
  RES$catch.n <- RES$landings.n + RES$discards.n
  RES$catch.wt <- ((RES$landings.n*RES$landings.wt) + (RES$discards.n*RES$discards.wt)) /  (RES$landings.n + RES$discards.n)

  return(RES)
}




