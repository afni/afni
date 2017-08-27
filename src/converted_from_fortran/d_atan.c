#include "converted_from_fortran.h"

#ifdef KR_headers
double atan();
double d_atan(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_atan(doublereal *x)
#endif
{
return( atan(*x) );
}
