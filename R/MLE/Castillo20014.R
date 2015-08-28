# S2: Code in R for a CV-plot

# Description:
# cvdat(arg1) is a function that calculates the residual coefficient
# of variation of a non-negative data set (the last 16 observations are omitted).
# The argument (arg1) is a data set.
# expcv(arg1,arg2) is a function that simulate the quantile of the coefficient
# of variation of a sample of an exponential distribution. The arguments are the
# sample size (arg1) and the nominal probability (arg2).

# Usage:
# cvMatrix<-cvdat(dataSet) makes a two columns matrix that can be
# plotted with plot(cvMatrix).


#The function that calculates the residual coefficient of variation.
cvdat <- function(x) {
  x <- sort(x);nx <- length(x)
  if (min(x) < 0)
    print("min<0?");dl <- 16;CV <- numeric(nx - dl)
    for (k in 1:(nx - dl)) {
      u <- x[k];xk <- subset(x,x > u) - u
      CV[k] <- (sd(xk) / mean(xk))
    }
    Sample <- seq(1:(nx - dl));cbind(Sample,CV)
}

#Data set: Sample of absolute values of a Student’s t4.
# data <- abs(rt(1000,4))

#CV-plot (non-negative data set.
cvPlot = function(data) {
  cvdata <- cvdat(data)
  
  #Asimptotic Confidence Intervals (Ho: Exponential).
  #To be draw on the active chart.
  qn <-
    qnorm(0.95);ns <- length(data);dl <- 16;sample <- seq(1:(ns - dl))
  uk <- numeric(ns - dl);lk <- numeric(ns - dl)
  for (k in 1:ns - dl) {
    lk[k] <- 1 - qn / (sqrt(ns - k));
    uk[k] <- 1 + qn / (sqrt(ns - k))
  }
  pmin=min(min(cvdata[,2]),min(lk));
  pmax=max(max(cvdata[,2]),max(uk));
  plot(
    cvdata,type = "l",main = "CV-plot",ylab = "Coeficient de variaci\u{F3}",
    ylim = c(pmin,pmax),#c(0.65,1.45),
    col = "blue",
    xlab="Mostra rebutjada"
  )
  lines(sample,lk,type = "l",col = "red")
  lines(sample,uk,type = "l",col = "red")

  #Confidence Intervals by Simualtion (Ho: Exponential)
  #The function that simulate the quantiles.
#   expcv <- function(n,pr) {
#     rep <- 1000;tcv <- numeric(rep);
#     for (k in 1:rep) {
#       te <- rexp(n);tcv[k] <- sd(te) / mean(te)
#     };
#     quantile(tcv,probs = pr)
#   };
#   print("function defined")
#   
#   #Drawing confidence intervals on the active chart.
#   ns <- length(data);
#   inc = 100;
#   step <- floor(ns / inc);
#   pr = 0.95;
#   pk <- numeric(step);
#   for (k in 1:step) {
#     pk[k] <- 1 + inc * (k - 1)
#   };
#   low <- numeric(step);
#   for (k in 1:step) {
#     low[k] <- expcv(ns - (k - 1) * inc,(1 - pr))
#   };
#   upp <- numeric(step);
#   for (k in 1:step) {
#     upp[k] <- expcv(ns - (k - 1) * inc,pr)
#   };
#   print("points done")
#   
#   points(pk,upp,col = "green"); points(pk,low,col = "green");
#   print("plot 3 done")
}