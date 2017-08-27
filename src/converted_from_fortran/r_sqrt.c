#include "converted_from_fortran.h"

#ifdef KR_headers
double sqrt();
double r_sqrt(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_sqrt(real *x)
#endif
{
return( sqrt(*x) );
}
