#include <math.h>

double localmin(double a, double b, double eps, double t, int N, double *data, double (*f)(int N, double *data, double sigma), double x) {
    double c,d,e,m,p,q,r,tol,t2,u,v,w,fu,fv,fw,fx;
    
    c = 0.5*(3.0-sqrt(5.0));

    v = w = x = a+c*(b-a);
    e = 0;
    fv = fw = fx = f(N,data,x);


}
