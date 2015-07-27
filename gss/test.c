#include <stdio.h>
#include "gss.h"

double f (int N, double *data, double x) {
    return 3*x*x+2*x-1;
}

int main() {
    double a=-1;
    double b=3;
    double result;

    printf("[a,b]=[%g,%g]\n",a,b);
    gss(&a,&b,0,NULL,f,1e-10,&result);
    printf("[a,b]=[%g,%g]\n",a,b);
    printf("minim a %g\n",result);

    return 0;
    
        
}
