#include "converted_from_fortran.h"

#ifdef KR_headers
double tan();
double d_tan(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_tan(doublereal *x)
#endif
{
return( tan(*x) );
}
