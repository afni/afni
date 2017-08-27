#include "converted_from_fortran.h"

#ifdef KR_headers
double asin();
double d_asin(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_asin(doublereal *x)
#endif
{
return( asin(*x) );
}
