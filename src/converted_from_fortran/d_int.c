#include "converted_from_fortran.h"

#ifdef KR_headers
double floor();
double d_int(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_int(doublereal *x)
#endif
{
return( (*x>0) ? floor(*x) : -floor(- *x) );
}
