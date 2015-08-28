library(evir); source("MLE/Castillo20014.R")
# x=sort(read.table("Power law/a-words.txt")$V1);pdf("cv-a.pdf",width=10,height = 8)
# x=sort(read.table("Power law/g-terrorism.txt")$V1);pdf("cv-g.pdf",width=10,height = 8)
# x=sort(read.table("Power law/k-blackouts.txt")$V1);pdf("cv-k.pdf",width=10,height = 8)
# x=sort(read.table("Power law/m-cities.txt")$V1);pdf("cv-m.pdf",width=10,height = 8)


pdf("cvplots.pdf",height=10,width=10);
par(mfrow=c(2,2));
x=sort(read.table("Power law/a-words.txt")$V1);
x=x[(length(x)-500):length(x)];
cvPlot(x)
x=sort(read.table("Power law/g-terrorism.txt")$V1);
x=x[(length(x)-500):length(x)];
cvPlot(x)
x=sort(read.table("Power law/k-blackouts.txt")$V1);cvPlot(x)
x=sort(read.table("Power law/m-cities.txt")$V1);
x=x[(length(x)-500):length(x)];
cvPlot(x)
dev.off()