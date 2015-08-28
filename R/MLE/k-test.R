source("MLE/[34]eGPD.R")
x=sort(read.table("Power law/k-blackouts.txt")$V1);
xmin=230000;
eGPD(x[x>xmin]-xmin)
out=gpd(x,xmin)

pdf("k-GPD230000.pdf",width=10,height=7)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="Afectats per tallades de subministre",line=2.5)
mtext(side=2,text="1-F(x)",line=2.3)
legend("topright",c("Ajust GPD"),lty=1,inset = 0.01)
dev.off()

pdf("k-kk.pdf",width=10,height = 7)
shape(x,models=30,start=15,end=200,labels=FALSE,reverse=TRUE)
mtext(side=1,text="Dades sobre el llindar",line=2.5)
mtext(side=3,text="Llindars",line=2.5)
mtext(side=2,text=expression("Paràmetre de forma"~kappa~"(CI 95%)"),line=2.3)
dev.off()