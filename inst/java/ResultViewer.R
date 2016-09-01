setwd("/Users/Aranka/NetBeansProjects/MetaPopulationSim")
opar <- par()
library(colorRamps)
library(grDevices)
t<-1000
u <- read.csv("output.txt")
x <- u[u[,"run"]==29,]
maxAge <- max(x[,"age"])
K <- rbind(rep(6,t),rep(8,t),rep(9,t), rep(9,t), rep(17,t), rep(10,t))
PerPatch <- matrix(ncol=6,nrow=t)
PerPatchAdult <- matrix(ncol=6,nrow=t)
PerPatchJuv <- matrix(ncol=6,nrow=t)
PerAge <- matrix(ncol=maxAge,nrow=t)
PerAgeAdult <- matrix(ncol=maxAge,nrow=t)
PerPatchSex <- list("patch1" = matrix(ncol=2,nrow=t),"patch2" = matrix(ncol=2,nrow=t),"patch3" = matrix(ncol=2,nrow=t),
                    "patch4" = matrix(ncol=2,nrow=t),"patch5" = matrix(ncol=2,nrow=t),"patch6" = matrix(ncol=2,nrow=t))
for(i in 0:(t-1)){
  y<-x[x[,"timestep"]==i,]
  z<-y[y[,"age"]>=2,]
  w<-y[y[,"age"]<2,]
  for(j in 0:5){
    PerPatchAdult[i+1,j+1]<-sum(z[,"patch"]==j)
    PerPatchJuv[i+1,j+1]<-sum(w[,"patch"]==j)
    PerPatch[i+1,j+1]<-sum(y[,"patch"]==j)
    f<-y[y[,"patch"]==j,]
    PerPatchSex[[(j+1)]][i+1,1] <- sum(f[,"sex"]=="vrouw")
    PerPatchSex[[(j+1)]][i+1,2] <- sum(f[,"sex"]=="man")
  }
  for(k in 1:maxAge){
    PerAge[i+1,k]<-sum(y[,"age"]==k)
    PerAgeAdult[i+1,k]<-sum(z[,"age"]==k)
  }
  
}
plot(PerPatch[,1],type="l",col=1, xlim = c(0,t), ylim=c(0,65))
lines(PerPatch[,2],col=2)
lines(PerPatch[,3],col=3)
lines(PerPatch[,4],col=4)
lines(PerPatch[,5],col=5)
lines(PerPatch[,6],col=6)
par(xpd=T, mar=par()$mar+c(0,0,0,4))
barplot(t(PerPatch), col=primary.colors(6), main = "Total number of individuals per patch, per year")
legend((t+21),40,legend=c("P 1  ", "P 2  ", "P 3  ", "P 4  ", "P 5  ", "P 6  "), cex=0.8, fill=primary.colors(6))
barplot(t(PerPatchAdult), col=primary.colors(6), main = "Total number of adult individuals per patch, per year")
legend((t+21),40,legend=c("P 1  ", "P 2  ", "P 3  ", "P 4  ", "P 5  ", "P 6  "), cex=0.8, fill=primary.colors(6))
barplot(t(PerPatchJuv), col=primary.colors(6), main = "Total number of juvenile individuals per patch, per year")
legend((t+21),40,legend=c("P 1  ", "P 2  ", "P 3  ", "P 4  ", "P 5  ", "P 6  "), cex=0.8, fill=primary.colors(6))
#barplot(t(PerAge), col=primary.colors(maxAge), main = "Age division per year, sum of all patches")
#legend((t+21),40,legend=c(1:maxAge),cex=0.8, fill=primary.colors(maxAge))
barplot(t(PerAgeAdult), col=primary.colors(maxAge), main = "Age division of adults per year, sum of all patches")
legend((t+21),40,legend=c(1:maxAge),cex=0.8, fill =primary.colors(maxAge))
par(opar)
par(mfrow=c(3,2))
for(i in 1:6){
  barplot(t(PerPatchSex[[i]]), col=rainbow(2), main = paste("Patch",as.character(i)))
  lines(K[(i),])
}
legend(0,25,legend=c("vrouw","man"),cex=0.8,fill=rainbow(2), bty="n")
par(mfrow=c(1,1))
par(opar) 
