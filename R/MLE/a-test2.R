source("MLE/[34]eGPD.R");

x=read.table("Power law/a-words.txt")$V1;
x=sort(x);
xmin=7;
alpha=1.95;
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


