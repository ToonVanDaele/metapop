EvolutionOutput <- function(tmax = 1000, nPop = 6, oneLoop = T, run = 0, java.out = "Evolution.txt", carying.cap = c(6,8,9,9,17,10)){
  library(colorRamps)
  library(grDevices)
  if(oneLoop){
    x <- read.csv(java.out)
  }else{
    u <- read.csv(java.out)
    x <- u[u[,"run"]==run,]
  }
  maxAge <- max(x[,"age"])
  K <- rep(carying.cap[1],tmax)
  for(i in 2:nPop){
    K <- rbind(K, rep(carying.cap[i],tmax))
  }
  PerPatch <- matrix(ncol=nPop,nrow=tmax)
  PerAge <- matrix(ncol=maxAge,nrow=tmax)
  PerPatchSex <- list("patch1" = matrix(ncol=2,nrow=tmax),"patch2" = matrix(ncol=2,nrow=tmax),"patch3" = matrix(ncol=2,nrow=tmax),
                      "patch4" = matrix(ncol=2,nrow=tmax),"patch5" = matrix(ncol=2,nrow=tmax),"patch6" = matrix(ncol=2,nrow=tmax))
  for(i in 0:(tmax-1)){
    y<-x[x[,"timestep"]==i,]
    for(j in 0:(nPop-1)){
      PerPatch[i+1,j+1]<-sum(y[,"patch"]==j)
      f<-y[y[,"patch"]==j,]
      PerPatchSex[[(j+1)]][i+1,1] <- sum(f[,"sex"]=="vrouw")
      PerPatchSex[[(j+1)]][i+1,2] <- sum(f[,"sex"]=="man")
    }
    for(k in 1:maxAge){
      PerAge[i+1,k]<-sum(y[,"age"]==k)
    }
  }
  par(mfrow=c(1,1))
  plot(PerPatch[,1],type="l",col=1, xlim = c(0,tmax+tmax/10),ylim = c(0,(max(carying.cap)+nPop*5)), main="Evolution of population size per patch", ylab = "Number of individuals", xlab = "t")
  l1 <- "Pop1 "
  for(i in 2:nPop){
    lines(PerPatch[,i],col=i)
    l1 <- c(l1,paste0("Pop",i," "))
  }
  legend("topright", legend = l1, fill = c(1:nPop), cex=0.8, bty = "n")
  barplot(t(PerPatch), col=primary.colors(6), xlim = c(0,tmax+tmax/5), main = "Total number of individuals per patch, per year", border=NA)
  legend(x=(tmax+tmax/5),y=10*nPop,legend=l1, cex=0.8, fill=primary.colors(nPop), bty="n")
  barplot(t(PerAge), col=primary.colors(maxAge), xlim = c(0,tmax+2*tmax/5), main = "Age division per year, sum of all patches", border=NA)
  legend(x=tmax+tmax/5,y=maxAge*nPop,title = "Age", ncol=2, bty="n",legend=c(1:maxAge),cex=0.8, fill=primary.colors(maxAge))
  par(mfrow=c(nPop/2,2))
  for(i in 1:nPop){
    barplot(t(PerPatchSex[[i]]), legend.text = c("vrouw","man"), col=rainbow(2), main = paste("Patch",as.character(i)),  border=NA, args.legend = list(x= "topright",bty="n"))
    lines(K[(i),])
  }
  #legend(0,25,legend=c("vrouw","man"),cex=0.8,fill=rainbow(2), bty="n")
  par(mfrow=c(1,1))
}

ExtinctionOutput <- function(nrep = 500, tmax = 1000, java.output = "ExtinctionTimes.txt", lines=F, lines.col=2){
  x <- read.csv(java.output)  
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