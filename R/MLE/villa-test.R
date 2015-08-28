library(evir);library(gPdtest);
lmax=200;

# x=sort(read.table("Power law/a-words.txt")$V1);
# x=sort(read.table("Power law/g-terrorism.txt")$V1);
# x=sort(read.table("Power law/k-blackouts.txt")$V1);
x=sort(read.table("Power law/m-cities.txt")$V1);
xprev=0;
for (i in seq(1,length(x)-16,length.out=lmax)) {
  i=as.integer(i);
  xmin=x[i];
  if(xmin==xprev) next;
  x.e=x[x>xmin]-xmin;
  test=gpd.test(x.e,J=450);
  pneg=test$p.values[1];
  ppos=test$p.values[2];
  cat(sprintf("%5d %5d %6.5f %6.5f\n",i,xmin,ppos,pneg))
  xprev=xmin;
}




