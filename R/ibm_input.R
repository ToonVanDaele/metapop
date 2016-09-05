outputPatch<-function(seed=420,mapsize=1000,dist_min=200,areaM=2.5,areaSD=0.8,Npatch=6,disp=500, plotG=FALSE, output.patch = "patches.txt")
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
}