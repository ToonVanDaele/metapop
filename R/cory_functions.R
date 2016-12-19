# 

makemx <- function(vrates,dimpop,popnum){
  return <- list()
  for (i in 1:3) {
    return <- c(return,
                list(rbind(c(0, 0, 0, vrates[19]*vrates[(4 + 6*(i - 1))]),
                           c(vrates[(1 + 6*(i - 1))],
                             vrates[(2 + 6*(i - 1))]*(1 - vrates[(5 + 6*(i - 1))]), 0, 0),
                           c(0, vrates[(2 + 6*(i - 1))]*vrates[(5 + 6*(i - 1))],
                             vrates[(3 + 6*(i - 1))]*(1 - vrates[(6 + 6*(i - 1))]), 0),
                           c(0, 0, vrates[(3 + 6*(i - 1))]*vrates[(6 + 6*(i - 1))],
                             vrates[4 + 6*(i - 1)]))))
  }
  zero <- matrix(0, dimpop, dimpop)
  G <- rbind(cbind(return[[1]], zero, zero),
             cbind(zero, return[[2]], zero),
             cbind(zero, zero, return[[3]]))
  return <- c(return, list(G))
  namen <- c("mx1", "mx2", "mx3", "mx")
  names(return) <- namen
  return
}

#

stnormfx <- function(xx){
  ci <- 0.196854
  cii <- 0.115194
  ciii <- 0.000344
  civ <- 0.019527
  if (xx >= 0) {
    z <- xx
  }else{
    z <- -xx
  }
  a <- 1 + (ci*z) + (cii*z*z)
  b <- (ciii*z*z*z) + (civ*z*z*z*z)
  w < -a + b
  if (xx >= 0) {
    ff <- 1 - (1/(2*w*w*w*w))
  }else{
    ff < -1 - (1 - 1/(2*w*w*w*w))
  }
  ff
}

#

lnorms2 <- function(n, mean = 2, var = 1) 
{
  nmeans <- log(mean) - 0.5 * log(var/mean^2 + 1)
  nvars <- log(var/mean^2 + 1)
  normals <- n * sqrt(nvars) + nmeans
  lns <- exp(normals)
  lns
}