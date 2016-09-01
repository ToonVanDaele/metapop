pop.projection.WB <- function(A1,A2,cap,n0,iterations=20){
  x <- length(n0)
  t <- iterations
  stage <- matrix(numeric(x * t), nrow = x)
  pop <- numeric(t)
  change <- numeric(t - 1)
  TN<-numeric(t)
  TS<-numeric(t)
  Iter <- numeric(t)
  n<-n0
  for (i in 1:t) {
    Tn <- sum(n[2])
    TN[i]<-Tn
    TS[i]<-sum(n[4])
    if (Tn<cap) {
      stage[, i] <- n
      pop[i] <- sum(n[1:x-1])
      if (i > 1) {
        change[i - 1] <- pop[i]/pop[i - 1]
      }
      Iter[i]<-1
      n <- A1 %*% n
    }else{
      n[5]<-cap
    stage[, i] <- n
    pop[i] <- sum(n[1:x-1])
    if (i > 1) {
      change[i - 1] <- pop[i]/pop[i - 1]
    }
    Iter[i]<-2
    n <- A2 %*% n
    }
  }
  rownames(stage) <- rownames(A1)
  colnames(stage) <- 0:(t - 1)
  w <- stage[, t]
  pop.proj <- list(lambda = pop[t]/pop[t - 1], stable.stage = w/sum(w), 
                   stage.vectors = stage, pop.sizes = pop, pop.changes = change, North = TN, South = TS, MUsage = Iter)
  pop.proj
  
}