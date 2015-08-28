source("MLE/[34]eGPD.R")
methods=c("KS","CvM","AD");
xmin=vector("numeric",3);
out=vector("list",3);


x=sort(read.table("Power law/a-words.txt")$V1);
pdf("a-llindar.pdf",9,10)
par(mfrow=c(3,1))
for (i in 1:3) {
  xmin[i]=gpd.findthresh(x,method=methods[i]);
  out[[i]]=gpd(x,xmin[i]);
  tailplot(out[[i]],col="red",labels=FALSE)
  if (i==1) {
    title(main="Paraules a Moby Dick")
  }
  mtext(side=1,text=paste("Dades, u=",xmin[i]," (",methods[i],")"),line=2.5)
  mtext(side=2,text="1-F(x)",line=2.2)
}
dev.off()
c(eGPD(x[x>xmin[1]]-xmin[1]),eGPD(x[x>xmin[2]]-xmin[2]),eGPD(x[x>xmin[3]]-xmin[3]))


x=sort(read.table("Power law/g-terrorism.txt")$V1);
pdf("g-llindar.pdf",9,10)
par(mfrow=c(3,1))
for (i in 1:3) {
  xmin[i]=gpd.findthresh(x,method=methods[i]);
  out[[i]]=gpd(x,xmin[i]);
  tailplot(out[[i]],col="red",labels=FALSE)
  if (i==1) {
    title(main="Severitat d'atacs terroristes")
  }
  mtext(side=1,text=paste("Dades, u=",xmin[i]," (",methods[i],")"),line=2.5)
  mtext(side=2,text="1-F(x)",line=2.2)
}
dev.off()
c(eGPD(x[x>xmin[1]]-xmin[1]),eGPD(x[x>xmin[2]]-xmin[2]),eGPD(x[x>xmin[3]]-xmin[3]))


x=sort(read.table("Power law/k-blackouts.txt")$V1);
pdf("k-llindar.pdf",9,10)
par(mfrow=c(3,1))
for (i in 1:3) {
  xmin[i]=gpd.findthresh(x,method=methods[i],nsim=200);
  out[[i]]=gpd(x,xmin[i]);
  tailplot(out[[i]],col="red",labels=FALSE)
  if (i==1) {
    title(main="Tallades de subministre el\u{E8}ctric")
  }
  mtext(side=1,text=paste("Dades, u=",xmin[i]," (",methods[i],")"),line=2.5)
  mtext(side=2,text="1-F(x)",line=2.2)
}
dev.off()
c(eGPD(x[x>xmin[1]]-xmin[1]),eGPD(x[x>xmin[2]]-xmin[2]),eGPD(x[x>xmin[3]]-xmin[3]))


x=sort(read.table("Power law/m-cities.txt")$V1);
pdf("m-llindar.pdf",9,10)
par(mfrow=c(3,1))
for (i in 1:3) {
  xmin[i]=gpd.findthresh(x,method=methods[i],m=1000,M=60000,dx=100);
  out[[i]]=gpd(x,xmin[i]);
  tailplot(out[[i]],col="red",labels=FALSE)
  if (i==1) {
    title(main="Poblaci\u{F3} de ciutats d'EEUU")
  }
  mtext(side=1,text=paste("Dades, u=",xmin[i]," (",methods[i],")"),line=2.5)
  mtext(side=2,text="1-F(x)",line=2.2)
}
dev.off()
c(eGPD(x[x>xmin[1]]-xmin[1]),eGPD(x[x>xmin[2]]-xmin[2]),eGPD(x[x>xmin[3]]-xmin[3]))