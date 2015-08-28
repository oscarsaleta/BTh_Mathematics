library(stats);
library(evir);
library(gPdtest);
library(goftest);

# CDF of the GDP
FGPD = function(x,k,psi) {
  return(1 - (1 - k * (x) / psi) ^ (1 / k))
}

#CDF of the Power-Law
PL = function(x,xm,a) {
  return(1 - (x / xm) ^ (1 - a))
}

#to estimate the maximum likelihood (MLE) of a sample x by GPD(k,psi)
eGPD = function(x) {
  fk = function(sigma)
    - mean(log(1 - x / sigma));
  fp = function(sigma)
    length(x) * (-log(fk(sigma) * sigma) + fk(sigma) - 1);
  int = c(-100 * max(x),100 * max(x));
  lol = optimize(fp,interval = int,maximum = T);
  sigma = lol$maximum;
  list(k = fk(sigma),psi = fk(sigma) * sigma)
}

# Cramer-von Mises statistic W²
W2f = function(z) {
  n = length(z);
  v = (z - (2 * seq(1,n) - 1) / (2 * n)) ^ 2;
  return(W2 = sum(v) + 1 / (12 * n))
}

W2ff = function(x,k,psi) {
  FP = ecdf(x);
  v = FGPD(x,k,psi) - FP(x);
  return(sum(v ^ 2))
}

W2i = function(x,k,psi) {
  fgpd = function(x) {
    FP = ecdf(x);
    return (FGPD(x,k,psi) - FP(x));
  }
  res = integrate(fgpd,min(x),max(x))[1]$value;
  return(res * length(x));
}

# Anderson-Darling statistic A²
A2f = function(z) {
  n = length(z);
  zz = sort(z,decreasing = TRUE);
  v = (2 * seq(1,n) - 1) * (log(z) + log(1 - zz));
  return(A2 = -n - mean(v))
}

A2ff = function(x,k,psi) {
  FP = ecdf(x)(x);
  FG = FGPD(x,k,psi);
  v = (FG - FP) ^ 2;
  vv = FG * (1 - FG);
  return(sum(v / vv))
}

# Kolmogorov-Smirnov statistic KS
KSf = function(x,k,psi) {
  FP = ecdf(x);
  return(max(abs(FGPD(x,k,psi) - FP(x))))
}

# Algorithm for finding xmin
gpd.xmin = function(x, m = x[1], M = x[length(x)], dx = 1,p = 0.1) {
  # usem KS, CM i AD
  ks = ks.xmin(x,m,M,dx,p);
  cvm = cvm.xmin(x,m,M,dx,p);
  ad = ad.xmin(x,m,M,dx,p);
  return(c(ks,cvm,ad))
}

# ymin = min(c(ks,ws,as),na.rm = TRUE)
# ymax = max(c(ks,ws,as),na.rm = TRUE)
# plot(x[-length(x)],ks,ylim = c(ymin,ymax),type = "l")
# lines(x[-length(x)],ws,col = "green")
# lines(x[-length(x)],as,col = "red")


gpd.xmin.v = function(x,J = 999) {
  kpos = vector(mode = "numeric",length = length(x));
  kneg = vector(mode = "numeric",length = length(x));
  x.indexes = vector(mode = "integer",length = 10);
  x.indexes[1] = 1;
  jump = length(x) / 2;
  for (i in 2:10) {
    x.indexes[i] = as.integer(x.indexes[i - 1] + jump);
    jump = jump / 2;
  }
  for (i in x.indexes) {
    # print(i)
    xmin = x[i];
    x.excess = x[x > xmin] - xmin;
    if (length(x.excess) == 0)
      break;
    test = gpd.test(x.excess);
    # gpd.test usa gamma=-kappa
    kpos[i] = test$p.values[1];
    kneg[i] = test$p.values[2];
  }
  if (max(kpos,na.rm = TRUE) < max(kneg,na.rm = TRUE)) {
    return(which.max(kneg));
  } else {
    return(which.max(kpos));
  }
  
}

# Trobar llindar usant KS/CvM/AD
gpd.findthresh = function(x, method = "CvM",m = x[1], M = x[length(x)], dx = x[1],p = 0.1,nsim = 100) {
  x.ecdf = ecdf(x);
  xmin = m;
  maxloop = as.integer((M - m) / dx);
  for (i in 1:maxloop) {
    x.e = x[x > xmin] - xmin;
    fit = eGPD(x.e);
    x.ecdf = ecdf(x.e);
    pval = 0;
    for (j in 1:nsim) {
      data = rgpd(length(x.e),xi = -fit$k,beta = fit$psi);
      if (method == "KS") {
        test = ks.test(data,x.ecdf);
      } else if (method == "CvM") {
        test = cvm.test(data,x.ecdf);
      } else if (method == "AD") {
        test = ad.test(data,x.ecdf);
      } else {
        stop("Mètode no vàlid.")
      }
      pval = pval + test$p.value;
    }
    pval =  pval / nsim;
    if (pval > p) {
      return(xmin);
    }
    xmin = xmin + dx;
  }
  stop("No s'ha trobat un llindar adient");
}

# GOF test usant Cramer von Mises
cvm.xmin = function(x, m = x[1], M = x[length(x)], dx = 1,p = 0.1,nsim = 100) {
  # x.ecdf = ecdf(x);
  xmin = m;
  maxloop = as.integer((M - m) / dx);
  for (i in 1:maxloop) {
    x.e = x[x > xmin] - xmin;
    fit = eGPD(x.e);
    x.ecdf = ecdf(x.e);
    pval = 0;
    for (j in 1:nsim) {
      data = rgpd(length(x.e),xi = -fit$k,beta = fit$psi);
      test = cvm.test(data,null = x.ecdf);
      pval = pval + test$p.value;
    }
    pval =  pval / nsim;
    if (pval > p) {
      return(xmin);
    }
    xmin = xmin + dx;
  }
  stop("No s'ha trobat un llindar adient");
}

# GOF test usant Anderson Darling
ad.xmin = function(x,m = x[1],M = x[length(x)],dx = 1,p = 0.1,nsim = 100) {
  x.ecdf = ecdf(x);
  xmin = m;
  maxloop = as.integer((M - m) / dx);
  for (i in 1:(length(x))) {
    x.e = x[x > xmin] - xmin;
    fit = eGPD(x.e);
    x.ecdf = ecdf(x.e);
    pval = 0;
    for (j in 1:nsim) {
      data = rgpd(length(x.e),xi = -fit$k,beta = fit$psi);
      test = ad.test(data,null = x.ecdf);
      pval = pval + test$p.value;
    }
    pval = pval / nsim;
    if (pval > p) {
      return(xmin);
    }
    xmin = xmin + dx;
  }
  stop("No s'ha trobat un llindar adient");
}






