source("MLE/[34]eGPD.R")

boot.semiparam=function(x,u,k,psi) {
  ntail=length(x[x>xmin]);
  probtail=ntail/length(x);
  result=vector("numeric",length(x));
  for (i in 1:length(x)) {
    r=runif(1);
    if(r<probtail) {
      result[i]=rgpd(1,-k,psi);
    } else {
      result[i]=sample(x,1,replace=FALSE);
    }
  }
  return(result)
}

x=sort(read.table("Power law/a-words.txt")$V1);
# xmin=gpd.findthresh(x,method="CvM");
# fit=eGPD(x[x>xmin]-xmin);
# k=fit[[1]];
# psi=fit[[2]];
# dades=boot.semiparam(x,xmin,k,psi)
# plot(sort(dades),type="l",log="y")
# lines(x,col="red")
# cvm.test(dades,null=ecdf(x))
# ad.test(dades,null=ecdf(x))
# ks.test(dades,x)


gpd.goftest = function(x,u,metode = "CvM",J = 100) {
  x.e = x[x > u] - u;
  fit = eGPD(x.e);
  k = fit[[1]]; psi = fit[[2]];
  data = sort(rgpd(length(x.e),-k,psi));
  if (metode == "CvM") {
    ost = cvm.test(data,null = ecdf(x.e))$statistic[[1]];
  }
  else if (metode == "AD") {
    ost = cvm.test(data,null = ecdf(x.e))$statistic[[1]];
  }
  else if (metode == "KS") {
    ost = ks.test(data,x.e)$statistic[[1]];
  }
  else {
    stop("Metode no valid.");
  }
  count = 0;
  for (i in 1:J) {
    bdata = boot.semiparam(x,u,k,psi);
    bdata = sort(bdata);
    bdata.e = bdata[bdata > u] - u;
    bfit = eGPD(bdata.e);
    bk = bfit[[1]]; bpsi = bfit[[2]];
    bdataf = rgpd(length(bdata.e),-bk,bpsi);
    if (metode == "CvM") {
      bst = cvm.test(bdataf,null = ecdf(bdata.e))$statistic[[1]];
    }
    else if (metode == "AD") {
      bst = cvm.test(bdataf,null = ecdf(bdata.e))$statistic[[1]];
    }
    else if (metode == "KS") {
      bst = ks.test(bdataf,bdata.e)$statistic[[1]];
    }
    else {
      stop("Metode no valid.");
    }
    print(c(ost,bst))
    if (bst < ost) {
      count = count + 1;
    }
  }
  pval = count / J;
  return(pval);
  
}