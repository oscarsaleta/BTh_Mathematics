library(gPdtest);
library(evir);
source("[34]eGPD.R");

NSIM=100;

vPsi=vector(mode="numeric",length=NSIM);
vK=vector(mode="numeric",length=NSIM);
cPsi=vector(mode="numeric",length=NSIM);
cK=vector(mode="numeric",length=NSIM);
oK=vector(mode="numeric",length=NSIM)
for (i in 1:NSIM) {
  oK[i]=0.7-i*0.0091;
  dades=rgpd(n=10000,xi=-oK[i]);
  if (-oK[i]<0) {
    metode="combined";
  } else {
    metode="amle"
  }
  vfit=gpd.fit(dades,metode);
  cfit=eGPD(dades);
  vPsi[i]=vfit[2];
  vK[i]=-vfit[1];
  cPsi[i]=cfit$psi;
  cK[i]=cfit$k;
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
legend("bottomleft",inset=0.02,c("Teòric","MLE","Villaseñor"),lty=c(3,1,1),
       col=c("black","red","green"))
dev.off()

#plot biaix kappa
pdf(file="biaix-k.pdf",width=10,height=8);
cKb=abs(oK-cK);
vKb=abs(oK-vK);
ymin=min(cKb,vKb);
ymax=max(cKb,vKb);
plot(oK,vKb,type="l",col="green",xlab=expression(kappa),ylab=expression("Biaix de" ~ kappa),
     ylim=c(ymin,ymax))
grid()
lines(oK,cKb,col="red")
legend("topright",inset=0.02,c("MLE","Villaseñor"),col=c("red","green"),lty=c(1,1))
dev.off()

#plot psi
pdf(file="vPsi-cPsi.pdf",width=10,heigh=8)
ymin=min(vPsi,cPsi,1)
ymax=max(vPsi,cPsi,1)
plot(1:NSIM,rep(1,NSIM),type="l",lty=3,xlab=expression(Simulació),ylab=expression(psi),
     ylim=c(ymin,ymax))
grid()
lines(1:NSIM,cPsi,col="red")
lines(1:NSIM,vPsi,col="green")
legend("bottomleft",inset=0.02,c("MLE","Villaseñor"),col=c("red","green"),lty=c(1,1))
dev.off()
