source("MLE/[34]eGPD.R")
PVAL=0.05

x=sort(read.table("Power law/a-words.txt")$V1);
xmin=ks.xmin(x,p=PVAL)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.2)


x=sort(read.table("Power law/g-terrorism.txt")$V1);
xmin=ks.xmin(x,p=PVAL)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.2)


x=sort(read.table("Power law/k-blackouts.txt")$V1);
xmin=ks.xmin(x,p=PVAL)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.2)


x=sort(read.table("Power law/m-cities.txt")$V1);
xmin=ks.xmin(x,m=1000,M=60000,dx=100,p=PVAL)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.2)
