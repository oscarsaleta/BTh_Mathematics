source("/home/slenderman/git/tdg-mates/R/MLE/[34]eGPD.R")

# Llegir dades
x=read.table("/home/slenderman/git/tdg-mates/R/Power law/g-terrorism.txt")$V1;

##metode1
#P=ecdf(x)
#data=P(x)
#plot(data~x,log="yx")
##metode2
#x.freqs=as.data.frame(table(x))
#x.freqs$Relative=as.numeric(prop.table(table(x)))
#x.freqs$g=as.numeric(x.freqs$x)
#plot(Relative~x,data=x.freqs,log="xy")
##metode3
#P=ecdf(x)
#data=1+1e-4-P(x)
#plot(data~x,log="xy")


for(j in 1:5) {
  # Triem xmin
  #xm=data[j];
  xm=x[j];
  # Tallem les dades
  #data.cut=data[-j];
  remove=c(seq(1,j));
  x.cut=x[-remove];
  # Recoloquem
  #data.cut=data.cut-data.cut[1];
  x.cut=x.cut-x.cut[1];
  #eGPD(data.cut);
  print(c(j,xm,eGPD(x.cut)))
  plot()
}