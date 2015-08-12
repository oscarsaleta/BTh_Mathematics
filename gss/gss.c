#include <math.h>
#include "gss.h"

double fmin_brent(double ax, double bx, int N, double *data, double (*f)(int N, double *data, double sigma), double tol) {
    double a,b,c,d,e,eps,xm,p,q,r,tol1,tol2,u,v,w;
    double fu,fv,fw,fx,x;

    // 1/sqrt(golden ratio)
    c = 0.5*(3.-sqrt(5.));

    // sqrt de la precisio de maquina
    eps = machinePrec();

    // inicialitzacio
    a = ax;
    b = bx;
    v = a + c*(b-a);
    w = v;
    x = v;
    e = 0.;
    d = 0.;
    fx = f(N,data,x);
    fv = fx;
    fw = fx;

    // bucle principal
    while (fabs(x-xm) < (tol2-0.5*(b-a))) {

        xm = 0.5*(a+b);
        tol1 = eps*fabs(x)+tol/3.;
        tol2 = 2.*tol1;
        
        // mirar si fem gss o spi
        if (fabs(e) > tol1) {
            // fem spi
            r = (x-w)*(fx-fv);
            q = (x-v)*(fx-fw);
            p = (x-v)*q-(x-w)*r;
            q = 2.*(q-r);
            if (q > 0.)
                p = -p;
            else // collita propia
                q = fabs(q);
            r = e;
            e = d;
        } else if (fabs(p) < fabs(0.5*q*r)
                    || p > q*(a-x)
                    || p < q*(b-x)) {
            // parabolic interpolation step
            d = p/q;
            u = x+d;
            // no avaluar f massa a prop de ax o bx
            if ((u-a) < tol2 || (b-u) < tol2)
                d = dsign(tol1,xm-x);
        } else {
            // golden section step
            if (x >= xm)
                e = a-x;
            else
                e = b-x;
            d = c*e;
        }

        // no avaluar f massa a prop de x
        if (fabs(d) >= tol1)
            u = x+d;
        else
            u = x+dsign(tol1,d);
        fu = f(N,data,u);

        // actualitzar a, b, v, w i x
        if (fu <= fx) {
            if (u < x)
                b = x;
            else
                a = x;
            v = w;
            fv = fw;
            w = x;
            fw = fx;
            x = u;
            fx = fu;
        } else {
            if (u < x)
                a = u;
            else
                b = u;
            if (fu < fw || w == x) {
                v = w;
                fv = fw;
                w = u;
                fw = fu;
            } else if (fu <= fv || v == x || v == w) {
                v = u;
                fv = fu;
            } else {
            }
        }
    }
    // fi del bucle principal
    return x;
}

double machinePrec() {
    double eps,tol1;
    eps = 1.;
    tol1=1.+eps;
    while (tol1 > 1.) {
        eps /= 2.;
        tol1 = 1.+eps;
    }
    return sqrt(eps);
}

double dsign(double a, double b) {
    return (b >= 0 ? fabs(a) : -fabs(a));
}
