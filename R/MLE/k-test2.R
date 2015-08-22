library(gPdtest)
source("MLE/[34]eGPD.R");

x=read.table("Power law/k-blackouts.txt")$V1;

kpos=vector(mode="numeric",length=length(x));
kneg=vector(mode="numeric",length=length(x));
for (i in seq(1,length(x),length.out=10)) {
  xmin=x[i];
  x.excess=x[x>xmin]-xmin;
  test=gpd.test(x.excess);
  kpos[i]=test$p.values[1];
  kneg[i]=test$p.values[2];
}
if (max(kpos)<max(kneg)) {
  xmin=x[which.max(kneg)]
} else {
  xmin=x[which.max(kpos)]
}


x=sort(x);
# xmin=230*1000;
alpha=2.3;
x.excess=x[x>xmin]-xmin;

fit=eGPD(x.excess)

x.ecdf=1-ecdf(x.excess)(x.excess)
x.fgpd=1-FGPD(x.excess,fit$k,fit$psi)
x.pl=1-PL(x,xmin,alpha)

# pdf("k-PLvsGPD.pdf",width=10,height=8)

plot(x.excess+xmin,x.ecdf,log="xy",xlab="Severitat d'atacs terroristes (morts)",ylab="CDF dades",
     panel.first=grid(equilogs=FALSE))
lines(x.excess+xmin,x.fgpd,col="red")
lines(x,x.pl,col="blue")
legend("bottomleft",c("Power-Law","GPD"),col=c("blue","red"),lty=c(1,1),inset = 0.02)

# dev.off()


