source("MLE/[34]eGPD.R")
x=sort(read.table("Power law/g-terrorism.txt")$V1);
xmin=12;
eGPD(x[x>xmin]-xmin)
out=gpd(x,xmin)

pdf("g-GPD12.pdf",width=10,height=7)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="Freq. paraules (escala log)",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.3)
legend("topright",c("Ajust GPD"),lty=1,inset = 0.01)
dev.off()

pdf("g-kk.pdf",width=10,height = 7)
shape(x,models=30,start=5,end=35,labels=FALSE,reverse=FALSE)
mtext(side=3,text="Dades sobre el llindar",line=2.5)
mtext(side=1,text="Llindars",line=2.5)
mtext(side=2,text=expression("Paràmetre de forma"~kappa~"(CI 95%)"),line=2.3)
dev.off()


x.xmin=gpd.xmin(x)
