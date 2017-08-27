#include "converted_from_fortran.h"

#ifdef KR_headers
double acos();
double r_acos(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_acos(real *x)
#endif
{
return( acos(*x) );
}
