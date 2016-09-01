setwd("/Users/Aranka/NetBeansProjects/MetaPopulationSim")
n<-500
x.corr1<-read.csv("extinctionTime_corr1.txt")
runs.corr1 <- rep(0,n)
for(i in 0:(n-1)){
  if(nrow(x.corr1[x.corr1[,1]==i,])!=0){
    runs.corr1[i+1]<-min(x.corr1[x.corr1[,1]==i,][2])
  }else{
    runs.corr1[i+1]<-1000
  }
}
extinction.corr1 <- rep(0,1000)
for(i in 1:1000){
  extinction.corr1[i]<-sum(runs.corr1[]==i)
}
extCDF.corr1<-rep(0,1000)
for(i in 1:1000){
  extCDF.corr1[i] <- sum(extinction.corr1[1:i])/n
}

x.corr2<-read.csv("extinctionTime_corr2.txt")
runs.corr2 <- rep(0,n)
for(i in 0:(n-1)){
  if(nrow(x.corr2[x.corr2[,1]==i,])!=0){
    runs.corr2[i+1]<-min(x.corr2[x.corr2[,1]==i,][2])
  }else{
    runs.corr2[i+1]<-1000
  }
}
extinction.corr2 <- rep(0,1000)
for(i in 1:1000){
  extinction.corr2[i]<-sum(runs.corr2[]==i)
}
extCDF.corr2<-rep(0,1000)
for(i in 1:1000){
  extCDF.corr2[i] <- sum(extinction.corr2[1:i])/n
}

x.nocorr<-read.csv("extinctionTime_noCorr.txt")
runs.nocorr <- rep(0,n)
for(i in 0:(n-1)){
  if(nrow(x.nocorr[x.nocorr[,1]==i,])!=0){
    runs.nocorr[i+1]<-min(x.nocorr[x.nocorr[,1]==i,][2])
  }else{
    runs.nocorr[i+1]<-1000
  }
}
extinction.nocorr <- rep(0,1000)
for(i in 1:1000){
  extinction.nocorr[i]<-sum(runs.nocorr[]==i)
}
extCDF.nocorr<-rep(0,1000)
for(i in 1:1000){
  extCDF.nocorr[i] <- sum(extinction.nocorr[1:i])/n
}
plot(extCDF.corr1, type="l", col=1,xlab = "t", ylab="extinction CDF", main = "Nest=4, JuvSurv=0.5, AdSurv=0.8, Reprod=0.5, var=0.01")
lines(extCDF.corr2, col=2)
lines(extCDF.nocorr, col=3)
legend("bottomright", legend=c("Juvenile correlation ", "Full correlation ", "No correlation ") , fill=1:3, bty = "n", y.intersp = 1.2)
