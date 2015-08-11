#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <assert.h>
#include "brent.h"

/* Variables globals (calen dins funcions amb prototipus fixat */
int N;
double *x;

/* Funcions */
int test_GPD (void);
double fk(double);
double fp(double);

/* Funció principal */
int main(int argc, char* argv[]) {
    int i;

    /* Llegim paràmetres */
    if (argc != 2
            || sscanf(argv[1],"%d",&N)!=1) {
        fprintf(stderr,"%s N\n",argv[0]);
        return -1;
    }

    /* Assignem espai per x */
    x=(double *)malloc(N*sizeof(double)); assert(x!=NULL);

    /* Llegim dades per stdin */
    for(i=0;i<N;i++)
        fscanf(stdin,"%lf",&x[i]);

    /* Cridem test_GPD per trobar el MLE de la GPD */
    test_GPD();

    return 0;

}

/* Funció que troba el MLE de la GPD cridant al mètode de Brent */
int test_GPD (void) {
    double a,b,eps,t;
    double sigma,fsigma;
    double k,psi;
    double max=-DBL_MAX;
    int i;

    /* Busquem el màxim valor de les dades per crear un interval de cerca */
    for(i=0;i<N;i++) {
        if(x[i]>max)
            max=x[i];
    }
    a=100*max;
    b=-100*max;
    fprintf(stderr,"(a,b)=(%g,%g)\n",a,b);

    /* Definim una precissió basada en l'epsilon màquina */
    eps = 10.0*sqrt(r8_epsilon());
    t=eps;
    fprintf(stderr,"eps=%g\n",eps);

    /* Minimitzem la funció */
    fsigma=local_min(a,b,eps,t,fp,&sigma);

    /* Calculem k i psi a partir de sigma */
    k=fk(sigma);
    psi=k*sigma;

    /* Imprimim els resultats per stdout */
    fprintf(stdout,"k=%g, psi=%g\n",k,psi);
    return 0;
}

/* Funció (5), Castillo (2014) */
double fk (double sigma) {
    int i;
    double sum=0;
    for (i=0; i<N; i++)
        sum+=log(1-x[i]/sigma);
    return -sum/N;
}

/* Funció (6), Castillo (2014), canviada de signe per trobar màxims */
double fp (double sigma) {
    return -N*(-log(fk(sigma)*sigma)+fk(sigma)-1);
}   
