#include "converted_from_fortran.h"

#ifdef KR_headers
double cosh();
double d_cosh(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_cosh(doublereal *x)
#endif
{
return( cosh(*x) );
}
