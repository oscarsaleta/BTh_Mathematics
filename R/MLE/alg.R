library("evir")

# Llegir dades
g=read.table("/home/slenderman/git/tdg-mates/R/Power law/g-terrorism.txt")$V1
#metode1
P=ecdf(g)
data=P(g)
plot(data~g,log="yx")
#metode2
g.freqs=as.data.frame(table(g))
g.freqs$Relative=as.numeric(prop.table(table(g)))
g.freqs$g=as.numeric(g.freqs$g)
plot(Relative~g,data=g.freqs,log="xy")
#metode3
data2=1+1e-4-P(g)
plot(data2~g,log="xy")

hist(g,probability = TRUE,breaks="FD")
lines(density(g))
plot(density(g),log="xy")

# Triem xmin
jmin=1
xmin=data[jmin]
# Tallem les dades
data.cut=data[data>xmin]
data.cut
