library("evir")

x=read.table("/home/slenderman/git/tdg-mates/R/Power law/k-blackouts.txt")$V1;
P=ecdf(x);
data=1+1e-4-P(x);
plot(data~x,log="xy")