library(gPdtest);
library(evir);
library(hydroGOF)
source("[34]eGPD.R");
source("tictoc.R");

tic();

NSIM=300;
NREPS=100;

vPsi=vector(mode="numeric",length=NSIM);
vK=vector(mode="numeric",length=NSIM);
cPsi=vector(mode="numeric",length=NSIM);
cK=vector(mode="numeric",length=NSIM);
oK=vector(mode="numeric",length=NSIM);

avPsi=vector(mode="numeric",length=NREPS);
avK=vector(mode="numeric",length=NREPS);
acPsi=vector(mode="numeric",length=NREPS);
acK=vector(mode="numeric",length=NREPS);

for (i in 1:NSIM) {
  oK[i]=0.95-i*0.021;
  
  for (j in 1:NREPS) {
    dades=rgpd(n=1000,xi=-oK[i]);
    if (-oK[i]<0) {
      metode="combined";
    } else {
      metode="amle"
    }
    vfit=gpd.fit(dades,metode);
    cfit=eGPD(dades);
    avPsi[j]=vfit[2];
    avK[j]=-vfit[1];
    acPsi[j]=cfit$psi;
    acK[j]=cfit$k;
  }
  
  vPsi[i]=mean(avPsi);
  vK[i]=mean(avK);
  cPsi[i]=mean(acPsi);
  cK[i]=mean(acK);

}

par(las=1,mar=c(5.1,4.1,2.1,2.1))
#plot kappa
pdf(file="vK-cK.pdf",width=10,height=8)
ymin=min(oK,vK,cK);
ymax=max(oK,vK,cK);
plot(1:NSIM,oK,type="l",lty=3,xlab=expression(Simulació),ylab=expression(kappa),
     ylim=c(ymin,ymax))
grid()
lines(1:NSIM,cK,col="red")
lines(1:NSIM,vK,col="green")
legend("bottomleft",inset=0.02,c("Teòric","MLE","AMLE"),lty=c(3,1,1),
       col=c("black","red","green"))
dev.off()

#plot biaix kappa
pdf(file="biaix-k.pdf",width=10,height=8);
cKb=abs(oK-cK);
vKb=abs(oK-vK);
ymin=min(cKb,vKb);
ymax=max(cKb,vKb);
plot(oK,vKb,type="l",col="green",xlab=expression(kappa ~ Simulació),ylab=expression("Biaix de" ~ kappa),
     ylim=c(ymin,ymax))
grid()
lines(oK,cKb,col="red")
legend("topleft",inset=0.02,c("MLE","AMLE"),col=c("red","green"),lty=c(1,1))
dev.off()

# #plot MSE kappa
# ckMSE=vector(mode="numeric",length=NSIM);
# vkMSE=vector(mode="numeric",length=NSIM);
# for (i in 1:NSIM) {
#   od=rgpd(n=1000,xi=-oK[i]);
#   cd=rgpd(n=1000,xi=-cK[i]);
#   vd=rgpd(n=1000,xi=-vK[i]);
#   ckMSE[i]=mse(sort(od),sort(cd));
#   vkMSE[i]=mse(sort(od),sort(vd));
# }
# ymin=min(ckMSE,vkMSE);
# ymax=max(ckMSE,vkMSE);
# plot(oK,ckMSE,type="l",col="red",xlab=expression(kappa ~ Simulació),ylab=expression("MSE de" ~ kappa),
#      ylim=c(ymin,500),panel.first=grid())
# lines(oK,vkMSE,col="green")
# legend("topleft",inset=0.02,c("MLE","AMLE"),col=c("red","green"),lty=c(1,1))

#plot psi
pdf(file="vPsi-cPsi.pdf",width=10,heigh=8)
ymin=min(vPsi,cPsi,1)
ymax=max(vPsi,cPsi,1)
plot(oK,rep(1,NSIM),type="l",lty=3,xlab=expression(Simulació),ylab=expression(psi),
     ylim=c(ymin,ymax))
grid()
lines(oK,cPsi,col="red")
lines(oK,vPsi,col="green")
legend("topright",inset=0.02,c("MLE","AMLE"),col=c("red","green"),lty=c(1,1))
dev.off()


#plot psi biaix
pdf(file="biaix-psi.pdf",width=10,heigh=8)
cPb=abs(1-cPsi);
vPb=abs(1-vPsi);
ymin=min(vPb,cPb)
ymax=max(vPb,cPb)
plot(oK,vPb,type="l",col="green",xlab=expression(kappa ~ Simulació),ylab=expression("Biaix de" ~ psi),
     ylim=c(ymin,ymax))
grid()
lines(oK,cPb,col="red")
legend("topright",inset=0.02,c("MLE","AMLE"),col=c("red","green"),lty=c(1,1))
dev.off()

toc()