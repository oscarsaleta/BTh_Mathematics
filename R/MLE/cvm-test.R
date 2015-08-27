source("MLE/[34]eGPD.R")

x=sort(read.table("Power law/a-words.txt")$V1);
xmin=cvm.xmin(x,p=0.25)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.3)


x=sort(read.table("Power law/g-terrorism.txt")$V1);
xmin=cvm.xmin(x,m=7,M=20,p=0.25)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.3)


x=sort(read.table("Power law/k-blackouts.txt")$V1);
xmin=cvm.xmin(x,.25)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.3)


x=sort(read.table("Power law/m-cities.txt")$V1);
xmin=cvm.xmin(x,m=1000,M=6000,dx=100,p=.25)
out=gpd(x,xmin)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="x (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.3)



# cvm.xmin = function(x) {
#   critic = qCvM(0.1,n = length(x))
#   for (i in 1:(length(x))) {
#     xmin = i * x[1];
#     x.e = x[x > xmin] - xmin;
#     fit = eGPD(x.e);
#     pval = 0;
#     om = 0;
#     for (j in 1:100) {
#       data = rgpd(length(x),xi = fit$k,beta = fit$psi);
#       test = cvm.test(data,null = x.ecdf);
#       pval = pval + test$p.value;
#       om = om + test$statistic[["omega2"]];
#     }
#     om = om / 100;
#     pval = pval / 100;
#     if (pval < critic) {
#       return(c(xmin,pval));
#     }
#   }
#   return(-1);
# }