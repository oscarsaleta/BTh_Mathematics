library(goftest)
source("MLE/[34]eGPD.R");
x = sort(
  c(
    1.7,2.2,14.4,1.1,0.4,20.6,5.3,0.7,1.9,13,12,9.3,1.4,18.7,8.5,25.5,11.6,14.1,22.1,1.1,2.5,14.4,1.7,37.6,0.6,2.2,39,.3,15,11,7.3,22.9,1.7,.1,1.1,.6,9,1.7,7,20.1,.4,2.8,14.1,9.9,10.4,10.7,30,3.6,5.6,30.8,13.3,4.2,25.5,3.4,11.9,21.5,27.6,36.4,2.7,64,1.5,2.5,27.4,1,27.1,20.2,16.8,5.3,9.7,27.5,2.5,27
  )
)
x.ecdf=ecdf(x)
xmin.index = gpd.xmin(x);
xmin=27.5;
x.e=x[x>xmin]-xmin;
fit=eGPD(x.e)
x.f=FGPD(x,fit$k,fit$psi)
data=rgpd(length(x),xi=fit$k,beta=fit$psi)
plot(x,1-x.ecdf(x),log="xy")
lines(x,1-x.f,col="red")
cvm.test(data,null=x.ecdf)

critic=qCvM(0.1,n=length(x))
for(i in 1:(length(x)-1)) {
  xmin=i*x[1];
  x.e=x[x>xmin]-xmin;
  fit=eGPD(x.e);
  pval=0;
  om=0;
  for (j in 1:100) {
    data=rgpd(1000,fit$k,fit$psi);
    test=cvm.test(data,null=x.ecdf);
    pval=pval+test$p.value;
    om=om+test$statistic[["omega2"]];
  }
  om=om/100;
  pval=pval/100;
  print(c(xmin,om,pval))
  if (pval>critic) {
    print(xmin)
    break;
  }
}

xmin=0
