#include <R.h> 
#include <Rmath.h> 
#include <math.h>
#include <stdio.h>

void kernel_weights(double *X, int *p, int *n, double *phi, double *w) {
    int i, j, k, l;
    double sos;
    k = 0;
    for (i=0; i<*n-1; i++) {
        for (j=i+1; j<*n; j++) {
            sos = 0.;
            for (l=0; l<*p; l++) {
                sos += pow(X[l + (*p) * i] - X[l + (*p) * j],2.);
            }
            w[k] = exp(-(*phi) * sos);
            k += 1;
        }
    }
}

/*
void matrix_square(double *X, int *n, double *w) {
    int i, j, k;
    double matrix[*n][*n];
    double sum = 0;
    for (i=0; i<*n; i++)
        for (j=0; j<*n; j++)
            matrix[i][j] = X[*n * i + j];

    for (i=0; i<*n; i++) {
        for (j=0; j<*n; j++) {
            for (k=0; k<*n; k++) {
                sum = sum + matrix[i][k] * matrix[k][j];
            }
            w[*n * i + j] = sum;
            sum = 0;
        }
    }  
}
*/
