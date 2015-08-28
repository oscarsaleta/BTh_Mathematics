source("MLE/[34]eGPD.R")
x=sort(read.table("Power law/g-terrorism.txt")$V1);
xmin=12;
eGPD(x[x>xmin]-xmin)
out=gpd(x,xmin)

pdf("g-GPD12.pdf",width=10,height=7)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="Severitat d'atemptats terroristes",line=2.5)
mtext(side=2,text="1-F(x)",line=2.3)
legend("topright",c("Ajust GPD"),lty=1,inset = 0.01)
dev.off()

pdf("g-kk.pdf",width=10,height = 7)
shape(x,models=30,start=15,end=350,labels=FALSE,reverse=TRUE)
mtext(side=1,text="Dades sobre el llindar",line=2.5)
mtext(side=3,text="Llindars",line=2.5)
mtext(side=2,text=expression("Paràmetre de forma"~kappa~"(CI 95%)"),line=2.3)
dev.off()



