source("MLE/[34]eGPD.R");

x=read.table("Power law/a-words.txt")$V1;
x=sort(x);
xmin=7;
alpha=1.95;

# Triar xmin
kpos=vector(mode="numeric",length=length(x));
kneg=vector(mode="numeric",length=length(x));
x.indexes=vector(mode="integer",length=10);
x.indexes[1]=1;
for (i in 2:10) {
  x.indexes[i]=x.indexes[i-1]+300*i;
}
# x.indexes=length(x)-2^seq(1,20,2);
# x.indexes=sort(x.indexes[x.indexes>0]);
for (i in x.indexes) {
  print(i)
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




x.excess=x[x>xmin]-xmin;

fit=eGPD(x.excess)

x.ecdf=1-ecdf(x.excess)(x.excess)
x.fgpd=1-FGPD(x.excess,fit$k,fit$psi)
x.pl=1-PL(x,xmin,alpha)

# pdf("a-PLvsGPD.pdf",width=10,height=8)

plot(x.excess+xmin,x.ecdf,log="xy",xlab="Freqüència d'ocurrència de paraules",ylab="CDF dades",
     panel.first=grid(equilogs=FALSE))
lines(x.excess+xmin,x.fgpd,col="red")
lines(x,x.pl,col="blue")
legend("bottomright",c("Power-Law","GPD"),col=c("blue","red"),lty=c(1,1),inset = 0.02)

# dev.off()


