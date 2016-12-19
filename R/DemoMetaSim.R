####### Inputdata #####

library(popbio)
# setwd("/Users/Aranka/Documents/Stage_INBO/metapop/R")
source("cory_functions.R")
inputfile <- "./extdata/coryrates.txt"  #gebaseerd op data uit de paper
vrates <- as.matrix(read.table(inputfile, header = TRUE, stringsAsFactors = TRUE))
vrtypes <- c(rep(1,times = 18),3) #1:beta, 2:stretched beta, 3:lognormal
vrmins <- rep(0,times = 19)  # geen streched betas dus alles is hier gewoon 0
vrmaxs <- rep(0,times = 19)
n0 <- c(720, 26, 13, 24, 920, 6, 10, 23, 980, 12, 20, 31)#initiele populatieverdeling
#n0 <- c(0,0,0,0,920,6,10,23,980,12,20,31)
Ncap <- c(100, 100, 100)          #maximale populatiegrootte
Ne <- c(20, 20, 20)               # quasi-extinctiegrens
tmax <- 100                       # aantal simulatiejaren
np <- 19                          # aantal vital rates in vrates
dims <- 12                        # dimensie van G
runs <- 500                       # aantal simulaties
popnum <- 3                       # aantal verschillende populaties
dimpop <- 4                       # aantal stages

##### Initialisatie #####
vrmeans <- colMeans(vrates)       # komt overeen met tabel 11.2 (Morris en Doak) OK
vrvars <- diag(var(vrates))
corrmatrix <- cor(vrates)
corrmatrix[, 19] <- 0             # fertility heeft hier geen varantie
corrmatrix[19,] <- 0
corrmatrix[19, 19] <- 1
popNstart <- c(1:popnum)
for (pp in 1:popnum) {
  popNstart[pp] <- sum(n0[(2 + dimpop*(pp - 1)):(dimpop*pp)]) # initiele populatiegrootte (alles behalve zaden)
}

# randn('state',sum(100*clock)) rand('state',sum(150*clock)) nog niet geimplementeerd. Ik weet ook niet of dit echt wel nodig is...
#set.seed(sum(100*proc.time())) #dit is wss niet nodig
Eigenv <- eigen(corrmatrix)
W <- Eigenv$vectors
D <- diag(Eigenv$values)
M12 <- W %*% (sqrt(abs(D))) %*% t(W) # correlation matrix over the multiple observation years

## Stuk code van Vitalsim (make or retrieve sets of beta and stretched beta values to use in simulations)
parabetas <- matrix(0, 99, np)
for (j in 1:np) {
  if (vrtypes[j] != 3) {
    for (fx99 in 1:99) {
      if (vrtypes[j] == 1) {
        parabetas[fx99, j] <- betaval(vrmeans[j],sqrt(vrvars[j]),fx99/100) # Functie uit morris & doak geimplementeerd in popbio
      }
      if (vrtypes[j] == 2) {
        parabetas[fx99,j] <- stretchbetaval(vrmeans[j], sqrt(vrvars[j]), vrmins[j], vrmax[j], fx99/100)
      }
    }
  }
}

### Eigenwaarden zoeken
vrs <- vrmeans
matrixlijst <- makemx(vrmeans,dimpop,popnum)
mx1 <- matrixlijst$mx1
mx2 <- matrixlijst$mx2
mx3 <- matrixlijst$mx3
mx <- matrixlijst$mx
lam1a <- eigen.analysis(mx1)$lambda1
lam1b <- eigen.analysis(mx2)$lambda1
lam1c <- eigen.analysis(mx3)$lambda1
lam0 <- eigen.analysis(mx)$lambda1

### Aanmaken opslagplaats voor resultaten
PrExt <- matrix(0,tmax,1)            # Extinctiekans (extinction time tracker)
logLam <- matrix(0,runs,popnum)      # log-lambda values
stochLam <- matrix(0,runs,popnum)    # stochastic lambda values

for (xx in 1:runs) {
  xx
  nt <- n0                          # populatiegroottes initialiseren
  extinct <- matrix(0,1,popnum)     # houdt extincties bij
  for (tt in 1:tmax) {
    m <- rnorm(np) #uncorrelated random normal values
    yrxy <- M12 %*% m  #correlated random normal values
    for (yy in 1:np) {
      if (vrtypes[yy] != 3) {# een random parabeta-waarde kiezen uit de vooraf opgestelde lijst
        index <- round(100*pnorm(yrxy[yy])) # we hebben maar 100 waarden bijgehouden dus we moeten afronden
        if (index == 0) {index <- 1}
        if (index == 100) {index <- 99}
        vrs[yy] <- parabetas[index,yy]
      }else{
        vrs[yy] <- lnorms2(yrxy[yy],vrmeans[yy],vrvars[yy]) #De lnorms uit popbio berekent zelf nog een random waarde, maar die hebben we hier al
      }
    }
    matrixlijst <- makemx(vrs,dimpop,popnum)   # nieuwe matrix
    mx <- matrixlijst$mx
    nt <- mx %*% nt                                # matrixvermenigvuldiging met populatievector
    popNs <- vector("numeric",popnum)
    for (pp in 1:popnum) {                       # cap op de populatie
      popNs[pp] <- sum(nt[(2 + dimpop*(pp - 1)):(dimpop*pp)])
      if (popNs[pp] > Ncap[pp]) {
        nt[(2 + dimpop*(pp - 1)):(dimpop*pp)] <- nt[(2 + dimpop*(pp - 1)):(dimpop*pp)]/Ncap[pp]
      }
    }
    if (sum(extinct) < popnum) {             # bijhouden op welk tijdstip in een run de populatie uitsterft
      for (nn in 1:popnum) {
        if (popNs[nn] <= Ne[nn]) {extinct[nn] <- 1}
      }
      if (sum(extinct) == popnum) {PrExt[tt] <- PrExt[tt] + 1}
    }
  }
  logLam[xx,] <- (1/tmax)*log(popNs/popNstart)
  stochLam[xx,] <- (popNs/popNstart) ^ (1/tmax)
}
CDFExt <- cumsum(PrExt/runs)

### Output
#deterministic lambda values
lam0
lam1a
lam1b
lam1c
#mean stochastic lambda
exp(mean(logLam))
exp(colMeans(logLam))
# mean and SD of log lambda
mean(logLam)
colMeans(logLam)
sqrt(diag(var(logLam)))
sd(logLam)
#Histogram of logLams
hist(logLam)
#extinction time CDF
plot(CDFExt, type = "l")
