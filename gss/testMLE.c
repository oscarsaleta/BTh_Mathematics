#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "eGDP.h"


int main (int argc, char *argv[]) {
    int N,i;
    double *data,k,psi,tol;

    /* Lectura de la llargada del vector de dades */
    if (argc != 3
            || sscanf(argv[1],"%d",&N)!=1
            || sscanf(argv[2],"%lf",&tol)!=1) {
        fprintf(stderr,"%s N tol\n",argv[0]);
        return -1;
    }

    /* Assignació de memòria per les dades */
    data = (double*)malloc(N*sizeof(double)); assert(data!=NULL);

    /* Lectura de les dades (directament de stdin) */
    for (i=0; i<N; i++) {
        fscanf(stdin,"%lf",&data[i]);
    }

    /* Ja tenim tot el que fa falta per cridar eGDP */
    eGDP(N,data,&k,&psi,tol);

    /* Imprimir els resultats */
    printf("k=%g,  psi=%g\n",k,psi);

    return 0;
}
