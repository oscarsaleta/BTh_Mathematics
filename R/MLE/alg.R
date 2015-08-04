library("evir")


# Llegir dades
g=read.table("/home/slenderman/git/tdg-mates/R/Power law/g-terrorism.txt")$V1
P=ecdf(g)
data=P(g)
plot(data~g,log="yx")

g.freqs=as.data.frame(table(g))
g.freqs$Relative=as.numeric(prop.table(table(g)))
g.freqs$g=as.numeric(g.freqs$g)
plot(Relative~g,data=g.freqs,log="xy")

hist(g,probability = TRUE,breaks="FD")
lines(density(g))
plot(density(g),log="xy")

# Triem xmin
jmin=1
xmin=data[jmin]
# Tallem les dades
data.cut=data[data>xmin]
data.cut
