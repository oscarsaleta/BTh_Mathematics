source("MLE/[34]eGPD.R")
x=sort(read.table("Power law/m-cities.txt")$V1);
xmin=52460;
eGPD(x[x>xmin]-xmin)
out=gpd(x,xmin)

pdf("m-GPD52460.pdf",width=10,height=7)
tailplot(out,col="red",labels=FALSE)
mtext(side=1,text="Poblaci\u{F3} de ciutats d'EEUU",line=2.5)
mtext(side=2,text="1-F(x) (escala log)",line=2.3)
legend("topright",c("Ajust GPD"),lty=1,inset = 0.01)
dev.off()

pdf("m-kk.pdf",width=10,height = 7)
shape(x,models=30,start=15,end=350,labels=FALSE,reverse=TRUE)
mtext(side=1,text="Dades sobre el llindar",line=2.5)
mtext(side=3,text="Llindars",line=2.5)
mtext(side=2,text=expression("Paràmetre de forma"~kappa~"(CI 95%)"),line=2.3)
dev.off()