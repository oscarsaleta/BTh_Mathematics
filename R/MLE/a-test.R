source("MLE/[34]eGPD.R")
x = sort(read.table("Power law/a-words.txt")$V1);
xmin = 7;
out = gpd(x,xmin)
eGPD(x[x > xmin] - xmin)

pdf("a-GPD7.pdf",width = 10,height = 7)
tailplot(out,col = "red",labels = FALSE)
mtext(side = 1,text = "Freq\u{FC}\u{E8}ncia paraules Moby Dick",line = 2.5)
mtext(side = 2,text = "1-F(x) (escala log)",line = 2.3)
legend("topright",c("Ajust GPD"),lty = 1,inset = 0.01)
dev.off()

pdf("a-kk.pdf",width = 10,height = 7)
shape(
  x,models = 30,start = 15,end = 350,labels = FALSE,reverse = TRUE
)
mtext(side = 1,text = "Dades sobre el llindar",line = 2.5)
mtext(side = 3,text = "Llindars",line = 2.5)
mtext(
  side = 2,text = expression("Paràmetre de forma" ~ kappa ~ "(CI 95%)"),line =
    2.3
)
dev.off()


# usem CM i AD de Choulakian-Stephens
ws = vector(mode = "numeric",length = length(x) - 1);
pws = vector(mode = "numeric",length = length(x) - 1);
xprev = 0;
# critic=qCvM(0.1,n=length(x));
p = 0.1
for (i in 1:(length(x) - 1)) {
  xmin = x[i];
  if (xmin == xprev) {
    ws[i] = ws[i - 1];
    next;
  }
  x.excess = sort(x[x > xmin] - xmin);
  if (length(x.excess) == 0)
    break;
  fit = eGPD(x.excess);
  z = FGPD(x.excess,fit$k,fit$psi)
  ws[i] = W2f(z);
  pws[i] = pCvM(ws[i],n = length(x));
  print(c(i,x[i],ws[i],pws[i]))
  if (pws[i] < p) {
    wsbo = ws[i];
    ibo = i;
    break;
  }
  xprev = xmin;
}
wi = which.min(ws);
result = c(wi,pCvM(wi,n = length(x)));
