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
