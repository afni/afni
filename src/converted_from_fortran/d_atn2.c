#include "converted_from_fortran.h"

#ifdef KR_headers
double atan2();
double d_atn2(x,y) doublereal *x, *y;
#else
#undef abs
#include "mathh.h"
double d_atn2(doublereal *x, doublereal *y)
#endif
{
return( atan2(*x,*y) );
}
