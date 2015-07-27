#include <math.h>
#include <float.h>
#include "eGDP.h"
#include "gss.h"

int eGDP (int N, double *x, double *k, double *psi, double tol) {
    double max=-DBL_MAX,min;
    double optSigma;
    int i;

    for (i=0; i<N; i++) {
        if (x[i]>max)
            max = x[i];
    }
    max=100*max;
    min=-100*min;

    gss(&min,&max,N,x,fp,tol,&optSigma);

    *k = fk(N,x,optSigma);
    *psi = fk(N,x,optSigma)*optSigma; 

    return 0;

}

double fk (int N, double *x, double sigma) {
    int i;
    double sum=0;
    for (i=0; i<N; i++)
        sum+=log(1-x[i]/sigma);
    return -sum/N;
}

double fp (int N, double *x, double sigma) {
    return N*(-log(fk(N,x,sigma)*sigma)+fk(N,x,sigma)-1);
}
