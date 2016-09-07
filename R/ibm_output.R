EvolutionOutput <- function(tmax = 1000, 
                            nPop = 6, 
                            oneLoop = T, 
                            run = 0, 
                            java.out = "Evolution.txt", 
                            carying.cap = c(6,8,9,9,17,10)){
  library(colorRamps)
  library(grDevices)
  library(reshape2)
  library(ggplot2)
  if(oneLoop){
    x <- read.csv(java.out)
  }else{
    u <- read.csv(java.out)
    x <- u[u[,"run"]==run,]
  }
  patch.names <- "Pop 1"
  for(i in 2:nPop){
    patch.names <- cbind(patch.names, paste("Pop",i))
  }
  maxAge <- max(x[,"age"])
  age.names <- "y2"
  for(i in 3:maxAge){
    age.names <- cbind(age.names, paste0("y",i))
  }
  K <- rep(carying.cap[1],tmax)
  for(i in 2:nPop){
    K <- rbind(K, rep(carying.cap[i],tmax))
  }
  PerPatch <- matrix(ncol=nPop,nrow=tmax)
  PerAge <- matrix(ncol=maxAge-1,nrow=tmax)
  PerPatchSex <- list("patch1" = matrix(ncol=2,nrow=tmax))
  for(i in 2:nPop){
    PerPatchSex[[paste0("patch",i)]] <- matrix(ncol=2,nrow=tmax)
  }
  for(i in 0:(tmax-1)){
    y<-x[x[,"timestep"]==i,]
    for(j in 0:(nPop-1)){
      PerPatch[i+1,j+1]<-sum(y[,"patch"]==j)
      f<-y[y[,"patch"]==j,]
      PerPatchSex[[(j+1)]][i+1,1] <- sum(f[,"sex"]=="vrouw")
      PerPatchSex[[(j+1)]][i+1,2] <- sum(f[,"sex"]=="man")
    }
    for(k in 2:maxAge){
      PerAge[i+1,k-1]<-sum(y[,"age"]==k)
    }
  }
  colnames(PerPatch) <- patch.names
  PerPatchM <- melt(PerPatch)
  colnames(PerAge)<- age.names
  PerAgeM <- melt(PerAge)
  par(mfrow=c(1,1))
  plot(PerPatch[,1],type="l",col=1, xlim = c(0,tmax+tmax/10),ylim = c(0,(max(carying.cap)+nPop*5)), main="Evolution of population size per patch", ylab = "Number of individuals", xlab = "t")
  l1 <- "Pop1 "
  for(i in 2:nPop){
    lines(PerPatch[,i],col=i)
    l1 <- c(l1,paste0("Pop",i," "))
  }
  legend("topright", legend = l1, fill = c(1:nPop), cex=0.8, bty = "n")
  print(ggplot(PerPatchM, aes(x = Var1, y=value, fill=Var2))
        + geom_bar(stat="identity")
        + ggtitle("Total number of individuals per patch, per year")
        + labs(x="t",y="Individuals"))
  print(ggplot(PerAgeM, aes(x = Var1, y=value, fill=Var2))
        + geom_bar(stat="identity")
        + ggtitle("Age division per year, sum of all patches")
        + labs(x="t", y="Individuals"))
  par(mfrow=c(nPop/2,2))
  for(i in 1:nPop){
    barplot(t(PerPatchSex[[i]]), legend.text = c("vrouw","man"), col=rainbow(2), main = paste("Patch",as.character(i)),  border=NA, args.legend = list(x= "topright",bty="n"))
    lines(K[(i),])
  }
  par(mfrow=c(1,1))
}

ExtinctionOutput <- function(nrep = 500, tmax = 1000, java.out = "ExtinctionTimes.txt", lines=F, lines.col=2){
  x <- read.csv(java.out)  
  runs <- rep(0,nrep)
  for(i in 0:(nrep-1)){
    if(nrow(x[x[,1]==i,])!=0){
      runs[i+1]<-min(x[x[,1]==i,][2])
    }else{
      runs[i+1]<-tmax
    }
  }
  extinction <- rep(0,tmax)
  for(i in 1:tmax){
    extinction[i]<-sum(runs[]==i)
  }
  extCDF<-rep(0,tmax)
  for(i in 1:tmax){
    extCDF[i] <- sum(extinction[1:i])/nrep
  }
  if(lines){
    lines(extCDF, col=lines.col)
  }else{
    plot(extCDF, type="l", col=1, xlab = "t", ylab="extinction CDF")
  }
}