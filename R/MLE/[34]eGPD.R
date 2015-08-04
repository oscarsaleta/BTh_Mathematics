library(stats);
library(evir);

#to estimate the maximum likelihood (MLE) of a sample x by GPD(k,psi)
eGPD=function(x){
  fk=function(sigma)
    -mean(log(1-x/sigma));
  fp=function(sigma)
    length(x)*(-log(fk(sigma)*sigma)+fk(sigma)-1);
  int=c(-100*max(x),100*max(x));
  sigma=optimize(fp,interval=int,maximum=T)$maximum;
  list(k=fk(sigma),psi=fk(sigma)*sigma)
}

r=rgpd(n = 10000, xi = 0.5, beta = 1);
write(r,file="/home/slenderman/UniversitatDB/TFG Mates/gss/dades.dat",ncolumns = 1,sep=" ");
eGPD(r)
warnings()
