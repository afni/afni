#include "converted_from_fortran.h"

#ifdef KR_headers
double exp();
double r_exp(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_exp(real *x)
#endif
{
return( exp(*x) );
}
