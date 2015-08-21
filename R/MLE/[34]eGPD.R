library(stats);
library(evir);

# CDF of the GDP
FGPD=function(x,k,psi){
  return(1-(1-k*x/psi)^(1/k))
}

#CDF of the Power-Law
PL=function(x,xm,a){
  return(1-(x/xm)^(1-a))
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
