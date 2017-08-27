#include "converted_from_fortran.h"

#ifdef KR_headers
double sqrt();
double d_sqrt(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_sqrt(doublereal *x)
#endif
{
return( sqrt(*x) );
}
