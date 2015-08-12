library(stats);
library(evir);

# CDF of the GDP
FGPD=function(x,k,psi){
  return(1-(1-k*x/psi)^(1/k))
}

#to estimate the maximum likelihood (MLE) of a sample x by GPD(k,psi)
eGPD=function(x){
  fk=function(sigma)
    -mean(log(1-x/sigma));
  fp=function(sigma)
    length(x)*(-log(fk(sigma)*sigma)+fk(sigma)-1);
  int=c(-100*max(x),100*max(x));
  lol=optimize(fp,interval=int,maximum=T);
  sigma = lol$maximum;
  list(lol,k=fk(sigma),psi=fk(sigma)*sigma)
}

<<<<<<< HEAD
# Cramer-von Mises statistic W²
W2f=function(z){
  n=length(z);
  v=(z-(2*seq(1,n))/(2*n))^2;
  return(W2=sum(v)+1/(12*n))
}

# Anderson-Darling statistic A²
A2f=function(z){
  n=length(z);
  zz=sort(z,decreasing = TRUE);
  v=(2*seq(1,n)-1)*(log(z)+log(1-zz));
  return(A2=-n-mean(v))
}
=======
#r=rgpd(n = 10000, xi = 0.5, beta = 1);
#write(r,file="/home/slenderman/UniversitatDB/TFG Mates/gss/dades.dat",ncolumns = 1,sep=" ");
#eGPD(r)
#warnings()
>>>>>>> fef983a072d51119f2e4a94caa66581b0a29a6b3
