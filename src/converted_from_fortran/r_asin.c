#include "converted_from_fortran.h"

#ifdef KR_headers
double asin();
double r_asin(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_asin(real *x)
#endif
{
return( asin(*x) );
}
