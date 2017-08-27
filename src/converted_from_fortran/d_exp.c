#include "converted_from_fortran.h"

#ifdef KR_headers
double exp();
double d_exp(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_exp(doublereal *x)
#endif
{
return( exp(*x) );
}
