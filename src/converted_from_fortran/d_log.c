#include "converted_from_fortran.h"

#ifdef KR_headers
double log();
double d_log(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_log(doublereal *x)
#endif
{
return( log(*x) );
}
