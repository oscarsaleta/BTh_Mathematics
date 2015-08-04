library(stats);
library(evir);

g=read.table("/home/slenderman/git/tdg-mates/R/Power law/g-terrorism.txt");
g=g$V1;
g=sort(g,decreasing=TRUE)

plot(g)
hist(g[g>50 & g<2000],breaks=length(g[g>50 & g<2000]))
plot(log(g[g>50 & g<2000]),log="xy")


freq=seq(1/length(g),1,by=1/length(g))
plot(log(g),log(10*freq),log="xy")

log(g)
log(10*freq)
freq
