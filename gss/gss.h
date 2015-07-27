inline double SIGN (const double &a, const double &b);
int gss (double *a, double *b, int N, double *data, double (*f)(int N, double *data, double x), double tol, double *result);
double brent (double ax, double bx, double cx, double (*f)(int N, double *data, double sigma), double tol, double *min);
