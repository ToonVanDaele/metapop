# Wootton & Bell  -> Dit is volledig omget in Markdown (script met dezelfde naam)
# Nog even nakijken en dan mag dit bestand verwijderd.

library(popbio)
library(Rramas)

#### Geen ruimtelijke component

init_1 <- c(55,90,0)
init_2 <- c(55,90,50)
classes <- c("F","N","I")
b <- 0.69
y <- 0.36
r <- 0.72
s <- 0.77
A <- matrix(data = c(0,y*b,y/2,r,s,0,0,0,1), nrow = 3, ncol = 3, byrow = TRUE)
dimnames(A)<- list(classes,classes)
A
p.p_1<-pop.projection(A,init_1,iterations=500)
p.p_2<-pop.projection(A,init_2,iterations=500)
p.p_2$pop.sizes <- p.p_2$pop.sizes-50

sensitivity(A[1:2,1:2])
elasticity(A[1:2,1:2])

plot(p.p_1$pop.sizes,col=3, ylim=c(0,200))
points(p.p_2$pop.sizes,col=4)


##### Rramas
v0 <- init_1[1:2]
mat <- A[1:2,1:2]
I <- 50
management <- c(y*I/2,0)
p.p_ramas <- projectn(v0,mat,time=500, management = management)
summary(p.p_ramas)
plot(p.p_ramas)
matT <- as.tmatrix(mat)
matT
summary(matT)

##### Metapopulatie

#Parameters
meta_classes <- c("Fn","Nn","Fs","Ns","I")
bn<-0.71
bs<-0.53
yn<-ys<-0.36
rn<-rs<-0.72
#sn<-ss<-0.77
sn<-0.91
ss<-0.77
m<-0.27
f<-0
c<-1

#Opbouw Matrix
An <- matrix(data = c(0,yn*bn,rn,sn), nrow = 2, ncol = 2, byrow = TRUE)
As <- matrix(data = c(0,ys*bs,rs,ss), nrow = 2, ncol = 2, byrow = TRUE)
An[2,1]<-An[2,1]*(1-m)
As[2,1]<-As[2,1]*(1-m)
Dsn <-  matrix(data = c(0,0,rn*m*c,0), nrow = 2, ncol = 2, byrow = TRUE)
Dns <-  matrix(data = c(0,0,rs*m*c,0), nrow = 2, ncol = 2, byrow = TRUE)

Fledg <- c(f*yn/2,0,(1-f)*ys/2,0)

G <- rbind(cbind(An,Dsn),cbind(Dns,As)) 
G <- rbind(cbind(G,Fledg),c(0,0,0,0,1))
dimnames(G)<- list(meta_classes,meta_classes)
G

#Sensitiviteitsanalyse (geeft andere resultaten dan in de paper)

lambda <- eigen.analysis(G[1:4,1:4])$lambda1
x <- c(yn,bn,rn,sn,ys,bs,rs,ss)
xdivlam <- x/lambda

S<-sensitivity(G[1:4,1:4])

sens <- c(syn=S[1,2]*bn,sbn=S[1,2]*yn,srn=S[2,1]*(1-m)+S[2,3]*m*c,ssn=S[2,2],sys=S[3,4]*bs, sbs=S[3,4]*ys, srs=S[4,1]*m*c+S[4,3]*(1-m), sss = S[4,4])
sens
elast <- xdivlam*sens
elast

E<-elasticity(G[1:4,1:4])

eigen.analysis(G[1:4,1:4])

VitalRates <- list(bn=0.71,bs=0.53, yn=0.36, ys=0.36, rn=0.72, rs=0.72, sn = 0.77, ss=0.77, m=0.27)
matrix <- expression(
  0, yn*bn, 0, 0,
  rn*(1-m), sn, rn*m, 0,
  0,0,0,ys*bs,
  rs*m,0,rs*(1-m), ss)

vitalsens(matrix, VitalRates)

# Sensitiviteit via simulatie uit de paper

dx = c(bn=1.1-0.71, bs= 1.02-0.53, yn=0.56-0.36, ys=0.69-0.36, rn=1.11-0.72, rs=1.38-0.72, sn=0.85-0.77, ss=0.88-0.77, m=-0.18-0.27)
dx
dR = 1-0.9438
sens<-dR/dx
sens

x<-c(bn,bs,yn,ys,rn,rs,sn,ss,m)
R=0.9438
elast<-(x/R)*sens
elast


#Simulaties

init_1_met <- c(19,60,20,29,0)
init_2_met <- c(19,60,20,29,50)

p.p_1<-pop.projection(G,init_1_met,iterations=100)
p.p_2<-pop.projection(G,init_2_met,iterations=100)
p.p_2$pop.sizes <- p.p_2$pop.sizes-50

plot(p.p_1$pop.sizes,col=3)
points(p.p_2$pop.sizes,col=4)

stage.vector.plot(p.p_2$stage.vectors[1:4,], proportions = TRUE)

# Rramas
Gt <- as.tmatrix(G[1:4,1:4])
summary(Gt)
plot(Gt)
meta_rramas1 <- projectn(init_2_met[1:4], Gt, time=100)
meta_rramas2 <- projectn(init_1_met[1:4], Gt, time=100, management=Fledg)
plot(meta_rramas1, ylim=c(0,10000))
plot(meta_rramas2, ylim=c(0,10000))

###### Met Density Dependence

#Opbouw tweede matrix na maximum capaciteit bereikt te hebben
cap<-100
An2 <- matrix(data=c(sn*(1-m), yn*bn,0,sn),nrow=2,ncol=2, byrow = TRUE)
Dsn2 <-  matrix(data = c(sn*m*c,0,0,0), nrow = 2, ncol = 2, byrow = TRUE)

float <- c(-(1-sn), (1-sn),0,0)

G2 <- rbind(cbind(An2,Dsn2),cbind(Dns,As)) 
G2 <- rbind(cbind(G2,float),c(0,0,0,0,1))
dimnames(G2)<- list(meta_classes,meta_classes)
G2

setwd("/Users/Aranka/Documents/Stage_INBO/metapop/R")
source("pop.projection.WB.R")

#Eigen projectiemethode die beide matrices kan gebruiken
pp_dens <- pop.projection.WB(G,G2,100,init_1_met,iterations=100)
plot(pp_dens$pop.sizes,col=3)

stage.vector.plot(pp_dens$stage.vectors[1:4,], proportions = FALSE)

NS_division <- rbind(colSums(pp_dens$stage.vectors[1:2,]),colSums(pp_dens$stage.vectors[3:4,]))
stage.vector.plot(NS_division,proportions = FALSE)

### Figure 6

Local_ext <- pp_dens$stage.vectors[,100]
Local_ext[5]<-0
Local_ext[1]<-0
Local_ext[2]<-0

pp_dens2 <- pop.projection.WB(G,G2,100,Local_ext,iterations = 100)
toPlot <-cbind(pp_dens$stage.vectors[1:4,], pp_dens2$stage.vectors[1:4,])
dimnames(toPlot) <- list(meta_classes[1:4], c(0:199))
stage.vector.plot(toPlot, proportions = FALSE)

#NS_division2 <- rbind(colSums(toPlot[1:2,]),colSums(toPlot[3:4,]))
NS_division2 <- rbind(toPlot[2,], toPlot[4,])
stage.vector.plot(NS_division2,proportions = FALSE)

### Figure 7(a)

An <- matrix(data = c(0,yn*bn,rn,sn), nrow = 2, ncol = 2, byrow = TRUE)
As <- matrix(data = c(0,ys*bs,rs,ss), nrow = 2, ncol = 2, byrow = TRUE)
Zero <- matrix(0L, nrow=2, ncol=2)
G3 <- rbind(cbind(An,Zero),cbind(Zero,As)) 
G3 <- rbind(cbind(G3,Fledg),c(0,0,0,0,1))
dimnames(G3)<- list(meta_classes,meta_classes)
G3

An2 <- matrix(data=c(sn, yn*bn,0,sn),nrow=2,ncol=2, byrow = TRUE)
G4 <- rbind(cbind(An2,Zero),cbind(Zero,As)) 
G4 <- rbind(cbind(G4,float),c(0,0,0,0,1))
dimnames(G4)<- list(meta_classes,meta_classes)
G4

pp_dens3 <- pop.projection.WB(G3,G4,100,init_1_met,iterations=100)

plot(pp_dens$North, col=1, ylim=c(0,200))
points(pp_dens$South, col=2)
points(pp_dens3$North, col=3)
points(pp_dens3$South, col=4)
legend("top",legend=c("North, migration", "South, migration", "North, no migration", "South, no migration"), col =1:4, lty=5)

### Figure 7(b)

plot(pp_dens3$North, col=2, ylim=c(0,300))
points(pp_dens$North+pp_dens$South, col=3)
points(pp_dens3$North+pp_dens3$South, col=4)

legend("top",legend=c("No sink", "Migration", "No Migration"), col =2:4, lty=5)
