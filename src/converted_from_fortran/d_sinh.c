#include "converted_from_fortran.h"

#ifdef KR_headers
double sinh();
double d_sinh(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_sinh(doublereal *x)
#endif
{
return( sinh(*x) );
}
