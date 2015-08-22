library(gPdtest)
source("MLE/[34]eGPD.R")

# Llegir dades
x=read.table("Power law/g-terrorism.txt")$V1;
x=sort(x);

# Triar xmin
kpos=vector(mode="numeric",length=length(x));
kneg=vector(mode="numeric",length=length(x));
x.indexes=vector(mode="integer",length=10);
x.indexes[1]=1;
for (i in 2:10) {
  x.indexes[i]=x.indexes[i-1]+150*i;
}
# x.indexes=length(x)-2^seq(1,20,2);
# x.indexes=sort(x.indexes[x.indexes>0]);
for (i in x.indexes) {
  xmin=x[i];
  x.excess=x[x>xmin]-xmin;
  if (length(x.excess)==0) break;
  test=gpd.test(x.excess);
  kneg[i]=test$p.values[1];
  kpos[i]=test$p.values[2];
}
if (max(kpos,na.rm=TRUE)<max(kneg,na.rm=TRUE)) {
  xmin=x[which.max(kneg)]
} else {
  xmin=x[which.max(kpos)]
}

# Fer fit
x.excess=x[x>xmin]-xmin;
fit=eGPD(x.excess);

# Fer plot
x.ecdf=1-ecdf(x.excess)(x.excess)
x.fgpd=1-FGPD(x.excess,fit$k,fit$psi)
alpha=2.4
x.pl=1-PL(x,xmin,alpha)
# pdf("g-PLvsGPD.pdf",width=10,height=8)
plot(x.excess+xmin,x.ecdf,log="xy",xlab="Severitat d'atacs terroristes (morts)",ylab="CDF dades",
     panel.first=grid(equilogs=FALSE))
lines(x.excess+xmin,x.fgpd,col="red")
lines(x,x.pl,col="blue")
legend("topright",c("Power-Law","GPD"),col=c("blue","red"),lty=c(1,1),inset = 0.02)
# dev.off()