#include "converted_from_fortran.h"

#ifdef KR_headers
double log();
double r_log(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_log(real *x)
#endif
{
return( log(*x) );
}
