library(gPdtest)
library(sfsmisc)
source("MLE/[34]eGPD.R");

x = read.table("Power law/m-cities.txt")$V1;
x = sort(x);
xminPL = 52.46 * 1000;
alpha = 2.38;

# Triar xmin
kpos = vector(mode = "numeric",length = length(x));
kneg = vector(mode = "numeric",length = length(x));
x.indexes = vector(mode = "integer",length = 10);
x.indexes[1] = 1;
for (i in 2:10) {
  x.indexes[i] = x.indexes[i - 1] + 350 * i;
}
# x.indexes=length(x)-2^seq(1,20,2);
# x.indexes=sort(x.indexes[x.indexes>0]);
for (i in x.indexes) {
  print(i)
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
  xmin = x[which.max(kneg)];
} else {
  xmin = x[which.max(kpos)];
}

x.excess = x[x > xmin] - xmin;
fit = eGPD(x.excess);

x.ecdf = 1 - ecdf(x.excess)(x.excess)
x.fgpd = 1 - FGPD(x.excess,fit$k,fit$psi)
x.pl = 1 - PL(x[x > xminPL],xminPL,alpha)

pdf("m-PLvsGPD.pdf",width = 10,height = 8)

par(
  pty = "m",plt = c(0.1, 1, 0.1, 1), omd = c(0.1,0.9,0.1,0.9)
)
plot(
  x.excess + xmin,x.ecdf,log = "xy",xlab = "",ylab = "",panel.first = grid(equilogs =
                                                                             FALSE),
  xaxt = "n",yaxt = "n"
)
eaxis(1, padj = -0.5, cex.axis = 0.8)
mtext(side = 1, text = "Població ciutats EEUU", line = 2.5)
eaxis(2,padj = -0.5,cex.axis = 0.8)
mtext(
  side = 2,text = "CDF dades",line = 2.5,outer = TRUE
)
lines(x.excess + xmin,x.fgpd,col = "red")
lines(x[x > xminPL],x.pl,col = "blue")
legend(
  "topright",c("Power-Law","GPD"),col = c("blue","red"),lty = c(1,1),inset = 0.02
)

dev.off()
