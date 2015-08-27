source("MLE/[34]eGPD.R");
x = sort(
  c(
    1.7,2.2,14.4,1.1,0.4,20.6,5.3,0.7,1.9,13,12,9.3,1.4,18.7,8.5,25.5,11.6,14.1,22.1,1.1,2.5,14.4,1.7,37.6,0.6,2.2,39,.3,15,11,7.3,22.9,1.7,.1,1.1,.6,9,1.7,7,20.1,.4,2.8,14.1,9.9,10.4,10.7,30,3.6,5.6,30.8,13.3,4.2,25.5,3.4,11.9,21.5,27.6,36.4,2.7,64,1.5,2.5,27.4,1,27.1,20.2,16.8,5.3,9.7,27.5,2.5,27
  )
)
# x=sort(read.table("Power law/k-blackouts.txt")$V1)
# x = sort(read.table("Power law/a-words.txt")$V1)
# x=sort(read.table("Power law/g-terrorism.txt")$V1)

xmin.index = gpd.xmin(x);

plot(x,ecdf(x)(x),pch = "*");

xmin = x[xmin.index][1];
x.e = x[x >= xmin] - xmin;
fit = eGPD(x.e);
f = function(x) {
  return(FGPD(x,fit$k,fit$psi))
}
lines(x, f(x),col = "green")
# print(c(
#   "KS:",xmin,KSf(x,fit$k,fit$psi),W2ff(x,fit$k,fit$psi),A2ff(x,fit$k,fit$psi)
# ))
print(c(
  "KS:",xmin,KSf(x,fit$k,fit$psi),W2f(f(x)),A2f(f(x))
))


xmin = x[xmin.index][2];
x.e = x[x >= xmin] - xmin;
fit = eGPD(x.e);
lines(x,f(x),col = "red")
# print(c(
#   "W^2 (sum):",xmin,KSf(x,fit$k,fit$psi),W2ff(x,fit$k,fit$psi),A2ff(x,fit$k,fit$psi)
# ))
print(c(
  "W^2:",xmin,KSf(x,fit$k,fit$psi),W2f(f(x)),A2f(f(x))
))

xmin = x[xmin.index][3];
x.e = x[x >= xmin] - xmin;
fit = eGPD(x.e);
lines(x,f(x),col = "blue")
# print(c(
#   "A^2 (sum):",xmin,KSf(x,fit$k,fit$psi),W2ff(x,fit$k,fit$psi),A2ff(x,fit$k,fit$psi)
# ))
print(c(
  "A^2:",xmin,KSf(x,fit$k,fit$psi),W2f(f(x)),A2f(f(x))
))

xmin = gpd.xmin.v(x,J = 500);
x.e = x[x >= xmin] - xmin;
fit = eGPD(x.e);
lines(x,f(x),type = "l",col = "red")
# print(c(
#   "Villaseñor:",xmin,KSf(x,fit$k,fit$psi),W2ff(x,fit$k,fit$psi),A2ff(x,fit$k,fit$psi)
# ))
print(c(
  "Villaseñor:",xmin,KSf(x,fit$k,fit$psi),W2f(f(x)),A2f(f(x))
))

xmin = 7;
x.e = x[x >= xmin] - xmin;
fit = eGPD(x.e);
lines(x,f(x),type = "l",col = "yellow")
# print(c(
#   "Power-Law:",xmin,KSf(x,fit$k,fit$psi),W2ff(x,fit$k,fit$psi),A2ff(x,fit$k,fit$psi)
# ))
print(c(
  "Power-Law:",xmin,KSf(x,fit$k,fit$psi),W2f(f(x)),A2f(f(x))
))
