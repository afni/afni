#include "converted_from_fortran.h"

#ifdef KR_headers
double tan();
double r_tan(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_tan(real *x)
#endif
{
return( tan(*x) );
}
