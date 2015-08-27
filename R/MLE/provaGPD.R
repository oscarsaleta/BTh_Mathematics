library(evir)
source("MLE/[34]eGPD.R")

data("nidd.thresh")
nidd.thresh=sort(nidd.thresh)

imins=gpd.xmin(nidd.thresh)
xmin=nidd.thresh[imins[3]]

out=gpd(nidd.thresh,xmin)
tailplot(out,col="red",labels=FALSE)


x=sort(read.table("Power law/g-terrorism.txt")$V1)
imins=gpd.xmin.v(x)
xmin=x[imins]
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)


xmin=cvm.xmin(x)[1]
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.1)

