
read.inputfile <- function(file,nrowheader=2,matrix=T) {
  # file is a csv file
  # cell A1 has the name of the data (e.g catch.n)
  # cell A2 has a description (e.g. catch numbers of L pis in area 78abde)
  # Skip row 3
  # header information:
  # cell A4 is 'year' and the rest of the row are years
  # cell A5 is 'quarter' and the rest of the rows are quarters
  # the next rows can have other header information like effort, number of samples etc.
  # Skip next row
  # optional matrix with length in column A and observations in the other columns, matching header, names in first row 
  name <- read.table(file,sep=',',nrows=1,as.is=T)$V1
  desc <- read.table(file,sep=',',skip=1,nrows=1,as.is=T)$V1
  time0 <- read.table(file,sep=',',skip=3,nrows=nrowheader)
  time <- data.frame(t(time0[,-1]))
  names(time) <- time0$V1
  rownames(time) <- 1:nrow(time)
  if(matrix){
    x0 <- read.table(file,sep=',',skip=5+nrowheader)
    x <- x0[,-1]
    colnames(x) <- paste(time$year,time$quarter,sep='-')
    rownames(x) <- x0[,1]
    lengths <- c(x0[,1])
  } else {x <- NULL; lengths <- NULL}
  x[is.na(x)] <- 0
  out <- list(name=name,desc=desc,time=time,lengths=lengths,x=x)
  return(out)
}

makeFLQuant <- function(x,units=1){
  # function to make an FLQuant object from x
  # x: list from read.inputfile 
  
  lengths <- x$lengths
  years <- seq(min(x$time$year),max(x$time$year))
  quarters <- unique(x$time$quarter)
  seasons <- quarters/4 - 1/8
  dimnames <- list(length=lengths,year=years,season=seasons)
  out <- FLQuant(NA,dimnames=dimnames,quant='length')
  for(y in years){
    for(q in quarters){
      i <- which(x$time$year==y & x$time$quarter==q)
      j <- which(quarters==q)
      if(length(i)>0) out[,as.character(y),,j] <- x$x[,i] * units
    }
  }
  
  return(out)
}


SSQ <- function(theta,len,freq,meanlen,sd){
  # function to minimise in fitVbgc
  # this is only used as a helper function to fitVbgc
  # theta: starting values
  # len: length classses
  # freq: numbers-at-length
  # meanlen: mean length-at-age
  # sd: stdev-at-age
  mn <- apply(array(1:length(meanlen)),1,function(x) dnorm(len,meanlen[x],sd[x]))
  f <- rowSums(t(t(mn)*abs(theta)))
  ssq <- sum((f-freq)^2)
  return(ssq)
}

fitVbgc <- function(len,freq,season,Linf,K,t0,sd,ages){
  # function to fit normal distributions along a vbgc to a length frequency distribution
  # this is only used as a helper function to fitVbgcFlQuant
  # len: length classses
  # freq: numbers-at-length
  # season: proportion of the year that has passed (assuming 1-jan birth date)
  # Linf,K,t0: Von Bert paramters
  # sd: stdev-at-age
  # ages: ages to estimate

  meanlen <- Linf * (1 - exp(-K * (ages - t0 + season))) # mean length at age, offset for season
  theta <- sum(freq) * (0.5^(1:length(ages)))/sum(0.5^(1:length(ages)))
#  opt <- nlm(f=SSQ, p=theta, len=len, freq=freq, meanlen=meanlen, sd=sd)
#  cat(opt$code)
#  par <- abs(opt$estimate)
#  opt <- optim(theta, fn=SSQ, len=len, freq=freq, meanlen=meanlen, sd=sd, method = "BFGS")
  opt <- optim(theta, fn=SSQ, len=len, freq=freq/sum(freq,na.rm=T), meanlen=meanlen, sd=sd, method = "L-BFGS-B",lower=0,upper=1)
  conv <- opt$convergence
  cat(conv)
  prop <- abs(opt$par)
  out <- data.frame(age=ages,prop=prop,convergence=conv,meanlen,sd)
  return(out)
  }

fitVbgcFlQuant <- function(x){
  # function to fit normal distributions along a vbgc to a length frequency distribution in the form of an FLQuant
  # x: FLQuant
  
  lengths <- as.numeric(dimnames(x)$length)
  years <- as.numeric(dimnames(x)$year)
  seasons <- as.numeric(dimnames(x)$season)
  
  cat('0 indicates successful convergence\n')
  out <- NULL
  for(y in years){
    for(s in seasons){
      freq <- x[,as.character(y),,as.character(s)]
      if(sum(freq,na.rm=T)==0) next()
      fit <- fitVbgc(len=lengths,freq,season=s,Linf,K,t0,sd,ages)
      out <- rbind(out,data.frame(year=y,season=s,fit))
    }
  }
  out$prop[out$prop<1e-6] <- 0
  return(out)
}



applyAlk <- function(len,freq,wt=NULL,meanlen,sd,prop){
  # function to convert the fitted proportions-at-age from fitVbgcFlQuant to an ALK and apply this to a LFD
  # helper function to applyAlkFlQuant
  # len: length classses
  # freq: numbers-at-length
  # meanlen: mean length-at-age
  # sd: stdev-at-age  
  # prop: proportions-at-age from fitVbgc

  mn <- apply(array(1:length(meanlen)),1,function(x) dnorm(len,meanlen[x],sd[x]))
  alm <- t(t(mn)*prop)
  alk <- alm/rowSums(alm)
  alk[is.na(alk)] <- 0

  nage <- colSums(c(freq) * alk, na.rm=T)
  if(!is.null(wt)) {
      wage <- colSums(c(freq) * c(wt) * alk, na.rm=T)/colSums(c(freq) * alk, na.rm=T)
      wage[is.na(wage)] <- 0
  } else wage <- rep(0,length(nage))
  out <- data.frame(nage,wage)
  return(out)
}

applyAlkFlQuant <- function(nlen,wlen=NULL,fit){
  # function to convert the fitted proportions-at-age from fitVbgcFlQuant to an ALK and apply this to an FLQuant LFD
  
  # nlen: FLQuant
  # wlen: FLQuant
  
  lengths <- as.numeric(dimnames(nlen)$length)
  years <- as.numeric(dimnames(nlen)$year)
  seasons <- as.numeric(dimnames(nlen)$season)
  ages <- unique(fit$age)
  
  dimnames <- list(age=ages,year=years,season=seasons)
  nage <- FLQuant(NA,dimnames=dimnames,quant='age')
  wage <- FLQuant(NA,dimnames=dimnames,quant='age')

  for(y in years){
    for(s in seasons){
      freq <- nlen[,as.character(y),,as.character(s)]
      if(sum(freq,na.rm=T)==0) next()
      if(!is.null(wlen)) wt <- wlen[,as.character(y),,as.character(s)] else wt <- NULL
      fit1 <- subset(fit,year==y & season==s)
      meanlen <- fit1$meanlen
      sd <- fit1$sd
      prop <- fit1$prop
      a <- applyAlk(len=lengths,freq=freq,wt=wt,meanlen=meanlen,sd=sd,prop=prop)
      nage[,as.character(y),,as.character(s)] <- a$nage
      wage[,as.character(y),,as.character(s)] <- a$wage
    }
  }
  return(list(nage=nage,wage=wage))
}


plotFit <- function(nlen,fit,xlim=c(10,100),residuals=F){
  # function to plot the LFD plus the fitted distribution, it can also return the residuals
  
  # nlen: FLQuant
  # fit: data frame from fitVbgcFlQuant
  # residuals: should the residuals be returned?

  lengths <- as.numeric(dimnames(nlen)$length)
  years <- as.numeric(dimnames(nlen)$year)
  seasons <- as.numeric(dimnames(nlen)$season)
  ages <- unique(fit$age)

  out <- NULL  
  for(y in years){
    for(s in seasons){
      freq <- nlen[,as.character(y),,as.character(s)]
      if(sum(freq,na.rm=T)==0) next
      fit1 <- subset(fit,year==y & season==s)
      meanlen <- fit1$meanlen
      sd <- fit1$sd
      prop <- fit1$prop
      mn <- apply(array(1:length(meanlen)),1,function(x) dnorm(lengths,meanlen[x],sd[x]))
      mna <- t(t(mn)*prop)
      mna <- mna/sum(mna,na.rm=T)
      
      plot(lengths,freq/sum(freq,na.rm=T),type='h',ylab='frequency',xlim=xlim,main=y+s)
      for(i in 1:ncol(mna)) lines(lengths,mna[,i],col='blue')
      lines(lengths,rowSums(mna),col='purple')
      
      residual <- c(freq/sum(freq,na.rm=T))-rowSums(mna)
      out <- rbind(out,data.frame(year=y,season=s,length=lengths,residual))
    }
  }
  if(residuals) return(out)
}

sop <- function (stock, slot = "catch") 
{
  return(quantSums(slot(stock, paste(slot, ".n", sep = "")) * 
                     slot(stock, paste(slot, ".wt", sep = "")))/slot(stock, 
                                                                     slot))
}


