#### Generate matrix with stochastic values for survival and reproduction
library(popbio)
setwd("/Users/Aranka/NetBeansProjects/MetaPopulation/MetaPopulation")
set.seed(420)

### Initial information + output
tmax <- 1000
numpop <- 6
stages <- 2
stochasticity <- c(T,F,F)
names(stochasticity) <- c("Juvenile Survival", "Adult Survival", "Reproduction")
printinfo <- c(tmax,numpop, stages)
names(printinfo) <- c("tmax","numpop","stages")
printinfo <- c(printinfo, stochasticity)
write.table(printinfo, "stochSurvival.txt",col.names = FALSE)

### Generate matrices
np <- 3*numpop
vrs <- matrix(c(2,4,6),np,tmax)
vrmeans <- rep(c(0.5,0.8,0.5),np)
#vrmeans <- rep(c(0.7,0.9,0.8),np)
vrvars <- rep(c(0.01,0,0),np)
#vrvars <- rep(c(0.05,0,0),np)
parabetas <- matrix(0,99,np)
##### Ongecorreleerd
corrmatrix <- diag(1,np,np)
##### Patches ongecorreleerd, parameters binnen patches perfect gecorreleerd
#ones <- matrix(1,3,3)
#corrmatrix <- cbind(ones,matrix(0,3,3*(numpop-1)))
#for(i in 2:numpop-1){
#  corrmatrix <- rbind(corrmatrix,cbind(matrix(0,3,3*i),ones,matrix(0,3,3*(numpop-i-1))))
#}
##### parameters tussen en binnen patches perfect gecorreleerd
#corrmatrix <- matrix(1,np,np)
##### Enkel Juvenile survival gecorreleerd over alle patches
#corrmatrix <- diag(1,np,np)
#corrmatrix[seq(1,np,by=3),seq(1,np,by=3)] <- 1
Eigenv <- eigen(corrmatrix)
W<-Eigenv$vectors
D<-diag(Eigenv$values)
M12 <- W%*%(sqrt(abs(D)))%*%t(W)

for(j in 1:np){
    for(fx99 in 1:99){
        parabetas[fx99,j] <- betaval(vrmeans[j],sqrt(vrvars[j]),fx99/100)
    }
}

for(t in 1:tmax){
  m<-rnorm(np) #uncorrelated random normal values
  yrxy<-M12%*%m  #correlated random normal values
  for(y in 1:np){
    index <- round(100*pnorm(yrxy[y])) # we hebben maar 100 waarden bijgehouden dus we moeten afronden
    if(index==0){index<-1}
    if(index==100){index<-99}
    vrs[y,t]<-parabetas[index,y]
  }
}

output <- list("JuvSurvival"= vrs[(3*(1:numpop)-2),],"AdultSurvival" = vrs[(3*(1:numpop)-1),],"Reproduction" = vrs[(3*(1:numpop)),])

### Print out matrices
if(stochasticity[1]){
  write.table(output$JuvSurvival, "stochSurvival.txt",row.names = FALSE, col.names = FALSE, append = TRUE)
}
if(stochasticity[2]){
  write.table(output$AdultSurvival, "stochSurvival.txt",row.names = FALSE, col.names = FALSE, append = TRUE)
}
if(stochasticity[3]){
  write.table(output$Reproduction, "stochSurvival.txt",row.names = FALSE, col.names = FALSE, append = TRUE)
}
