source("MLE/[34]eGPD.R");

x=read.table("Power law/g-terrorism.txt")$V1;
x=sort(x);
xmin=12;
x.excess=x[x>xmin]-xmin;

fit=eGPD(x.excess)

x.ecdf=1-ecdf(x.excess)(x.excess)
x.fgpd=1-FGPD(x.excess,fit$k,fit$psi)
x.pl=1-PL(x,12,2.4)

pdf("g-PLvsGPD.pdf",width=10,height=8)

plot(x.excess,x.ecdf,log="xy",xlab="Severitat d'atacs terroristes (morts)",ylab="CDF dades",
     panel.first=grid(equilogs=FALSE))
lines(x.excess,x.fgpd,col="red")
lines(x-12,x.pl,col="blue")
legend("bottomright",c("Power-Law","GPD"),col=c("blue","red"),lty=c(1,1),inset = 0.02)

dev.off()


