outputPatch<-function(seed=420,mapsize=1000,dist_min=200,areaM=2.5,areaSD=0.8,Npatch=6,disp=500, plotG=FALSE, stay = 100, output.patch = "patches.txt", output.migration = "markov_transition.txt")
{
  library(MetaLandSim)
  library(dplyr)
  set.seed(seed)
  patches.tmp <- rland.graph(mapsize = mapsize, dist_m = dist_min, 
                             areaM = areaM, areaSD = areaSD, Npatch = Npatch, 
                             disp = disp, plotG = plotG)
  patches.out <- patches.tmp$nodes.characteristics %>%
    select(ID, areas)
  write.csv(patches.out, file=output.patch, eol=",\n", row.names = FALSE, col.names = FALSE)
  patches <- patches.tmp$nodes.characteristics %>%
    select(ID, areas, x, y)
  distM <- dist(patches[,c("x","y")], diag = TRUE, upper = TRUE)
  probM <- as.matrix(distM)
  diag(probM) <- stay  # hoe groter hoe sneller een individu uit de huidige patch migreert
  probM <- 1 / (probM + 1) ^ 2
  sumprob <- rowSums(probM, na.rm = TRUE)
  probM <- probM %*% diag(1/sumprob)
  write.csv(probM, output.migration ,eol=",\n", row.names = FALSE, col.names = FALSE)
}

corrMatrix <- function(npar, npop, correlation = F, stochasticity = NA){
  dim = npar*npop
  if(!correlation){
    M <- diag(1,dim,dim)
  }else{
    if(is.na(stochasticity[1])){
      M <- matrix(1,dim,dim)
    }else{
      m <- (1*stochasticity)%*%t(1*stochasticity)
      mu <- m
      for(i in 2:npop){
        mu <- cbind(mu,m)  
      }
      M <- mu
      for(j in 2:npop){
        M <- rbind(M,mu)
      }
      diag(M)<-1
    }
  }
  return(M)
}

stochMatrix <- function(seed = 420, tmax = 1000, Npop = 6, stages = 2, stages.names = c("Juvenile","Adult"), stochasticity = c(T,F,F), survivalM = c(0.5,0.8), survivalVAR = c(0.05,0), reproductionM = 0.5, reproductionVAR = 0, corrMat = diag(1,18,18), output.stoch = "stochSurvival.txt"){
  library(popbio)
  set.seed(seed)
  names <- paste0(stages.names[1],"_Survival")
  for(i in 2:stages){
    names <- c(names,paste0(stages.names[i],"_Survival"))
  }
  names <- c(names,"Reproduction")
  names(stochasticity) <- names
  printinfo <- c(tmax,numpop, stages)
  names(printinfo) <- c("tmax","numpop","stages")
  printinfo <- c(printinfo, stochasticity)
  write.table(printinfo, output.stoch,col.names = FALSE)
  
  np <- (stages + 1)*Npop
  vrs <- matrix(0,np,tmax)
  vrmeans <- rep(c(survivalM,reproductionM),np)
  vrvars <- rep(c(survivalVAR,reproductionVAR), np)
  parabetas <- matrix(0,99,np)
  Eigenv <- eigen(corrMat)
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
  
  for(i in 1:(stages+1)){
    if(stochasticity[i]){
      write.table(vrs[((stages+1)*(1:Npop)-(stages-i+1)),], output.stoch,row.names = FALSE, col.names = FALSE, append = TRUE)
    }
  }
}
