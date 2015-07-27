#include <math.h>
#include "gss.h"
#define MAXIT 100000
#define PHI 0.61803398875 //0.5*(-1+sqrt(5.))

inline double SIGN (const double &a, const double &b) {
    return ( b>=0 ? (a >= 0 ? a : -a) : (a >= 0 ? -a : a) );
}

int gss (double *a, double *b, int N, double *data, double (*f)(int N, double *data, double x), double tol, double *result) {
    double aa=*a,bb=*b,c,d;
    double fc, fd;
    double err;
    int i;
    err = fabs(*b-*a);
    c=bb+PHI*(aa-bb);
    d=aa+PHI*(bb-aa);
    fc=(*f)(N,data,c);
    fd=(*f)(N,data,d);
    for (i=0; i<MAXIT; i++) {
        if (fc<fd) {
            bb=d;
            //fb=fd;
            d=c;
            fd=fc;
            c=bb+PHI*(aa-bb);
            fc=(*f)(N,data,c);
        } else {
            aa=c;
            //fa=fc;
            c=d;
            fc=fd;
            d=aa+PHI*(bb-aa);
            fd=(*f)(N,data,d);
        }
        err = fabs(bb-aa);
        if (err < tol) {
            *a=aa;
            *b=bb;
            *result = (aa+bb)/2.;
            return 0;
        }
    }
    return 1;
}


#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
/* Donada f i un triplet ax<bx<cx i f(bx)<f(ax), f(bx)<f(cx), aquesta funció usa el mètode de Brent (gss+parabolic)
 * per aïllar el mínim de f, i el retorna en la variable min
 * Source: Numerical Recipes
 */
double brent (double ax, double bx, double cx, double (*f)(int N, double *data, double sigma), double tol, double *min) {
    int i;
    double a,b,d;
    double p,q,r;
    double tol1,tol2;
    double u,v,w,x;
    double fu,fv,fw,fx;
    double xm;
    double etemp,e=0.0; // distància moguda en el penúltim pas

    /* Construim l'interval (a,b)*/
    a=(ax<cx ? ax : cx);
    b=(ax>cx ? ax : cx);
    x=v=w=bx;
    fx=fv=fw=f(x);

    for (i=0; i<MAXIT; i++) {
        xm = 0.5*(a+b);
        tol1 = tol*fabs(x)+1e-10;
        tol2 = 2*tol1;

        /* Test per veure si hem acabat */
        if ( fabs(x-xm) <= tol2-0.5*(b-a) ) {
            *xmin = x;
            return fx;
        }

        /* Intentem un ajust parabòlic */
        if ( fabs(e) > tol1) {
            r = (x-w)*(fx-fw);
            q=(x-v)*(fx-fw);
            p=(x-v)*q-(x-w)*r;
            q=2.0*(q-r);
            if (q > 0.0)
                p = -p;
            q=fabs(q);
            etemp=e;
            e=d;
            if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x)) {
                e = (x >= xm ? a-x : b-x);
                d=(1-PHI)*e;
            } else {
                d=p/q;u=x+d;
                if (u-a < tol2 || b-u < tol2)
                    d=SIGN(tol1,xm-x);
            }
        } else {
            e = (x >= xm ? a-x : b-x);
            d=(1-PHI)*e;
        }
        u=(fabs(d) >= tol1 ? x+d : x+SIGN(tol1,d));
        fu=f(u);
        if (fu <= fx) {
            if (u >= x)
                a=x;
            else 
                b=x;
            SHFT(v,w,x,u);
            SHFT(fv,fw,fx,fu);
        } else {
            if (u < x)
                a=u;
            else
                b=u;
            if (fu <= fw || w == x) {
                v=w;
                w=u;
                fv=fw;
                fw=fu;
            } else if (fu <= fv || v == x || v == w) {
                v=u;
                fv=fu;
            }
        }
    }
    fprintf(stderr,"brent:: Massa iteracions\n");
    *xmin=x;
    return fx;
}
