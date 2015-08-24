#Test Choulakian-Stephens 2012
source("MLE/[34]eGPD.R")

x = c(
  1.7,2.2,14.4,1.1,0.4,20.6,5.3,0.7,1.9,13,12,9.3,1.4,18.7,8.5,25.5,11.6,14.1,22.1,1.1,2.5,14.4,1.7,37.6,0.6,2.2,39,.3,15,11,7.3,22.9,1.7,.1,1.1,.6,9,1.7,7,20.1,.4,2.8,14.1,9.9,10.4,10.7,30,3.6,5.6,30.8,13.3,4.2,25.5,3.4,11.9,21.5,27.6,36.4,2.7,64,1.5,2.5,27.4,1,27.1,20.2,16.8,5.3,9.7,27.5,2.5,27
)
x = sort(x);
# threshold=27.5;
# x.excess=x[x>threshold]-threshold
# MLE=eGPD(x.excess)
# z=FGPD(x,MLE$k,MLE$psi)
# W2f(z)
# A2f(z)
#
# plot(x)
# points(x.excess,z)
# plot(x,z)

xmins = gpd.xmin(x)
x[xmins]
imin = gpd.xmin.v(x)


kpos = vector(mode = "numeric",length = length(x));
kneg = vector(mode = "numeric",length = length(x));
for (i in 1:(length(x) - 5)) {
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
  xmin = x[which.max(kneg)]
} else {
  xmin = x[which.max(kpos)]
}
