#READ DATA
setwd("C:/Users/Oscar/git-repos/tdg-mates/R")
library(ncdf4)

x1.nc=nc_open("Buoy data/NDBC_41041_201101_D1_v00.nc")
x1=ncvar_get(x1.nc,"wave_sensor_1/significant_wave_height")
x2.nc=nc_open("Buoy data/NDBC_41041_201102_D1_v00.nc")
x2=ncvar_get(x2.nc,"wave_sensor_1/significant_wave_height")
x3.nc=nc_open("Buoy data/NDBC_41041_201103_D1_v00.nc")
x3=ncvar_get(x3.nc,"wave_sensor_1/significant_wave_height")
x4.nc=nc_open("Buoy data/NDBC_41041_201104_D1_v00.nc")
x4=ncvar_get(x4.nc,"wave_sensor_1/significant_wave_height")
x5.nc=nc_open("Buoy data/NDBC_41041_201105_D1_v00.nc")
x5=ncvar_get(x5.nc,"wave_sensor_1/significant_wave_height")

data=sort(c(x1,x2,x3,x4,x5))
data=data[data<10]
pdf("buoy-data.pdf",width=10,height = 10)
plot(data,xlab="Índex",ylab="Alçada significant",type = "n")
grid()
points(data)
dev.off()

#VILLASEÑOR
library(evir);library(gPdtest);
lmax=10;
xprev=0;
x=data;
for (i in seq(1,length(x)-16,length.out=lmax)) {
  i=as.integer(i);
  xmin=x[i];
  if(xmin==xprev) next;
  x.e=x[x>xmin]-xmin;
  test=gpd.test(x.e,J=450);
  pneg=test$p.values[1];
  ppos=test$p.values[2];
  cat(sprintf("%5d %6.5f %6.5f %6.5f\n",i,xmin,ppos,pneg))
  xprev=xmin;
}


#CV PLOT
library(evir); source("MLE/Castillo20014.R")
pdf("buoy-cv.pdf",width = 10,height = 8)
cvPlot(data)
dev.off()

#THRESHOLD
source("MLE/[34]eGPD.R")
u=gpd.findthresh(data,"CvM",p=0.05)
gpd.findthresh(data,"KS",p=0.01)
gpd.findthresh(data,"AD",p=0.01)
out=gpd(data,u)

#PLOT
pdf("buoy-fit.pdf",width=10,height=8)
plot(out,col="red",labels=FALSE)
1
0
mtext(side=1,text="Altura significant de les ones (m)",line=2.5)
mtext(side=2,text=expression(F[u](x-u)),line=2.5)
title(main=paste("Distribució d'excés per l'ajust amb u=",round(u,4)))
dev.off()


#FIT
eGPD(x[x>u]-u)
