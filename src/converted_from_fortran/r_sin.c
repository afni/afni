#include "converted_from_fortran.h"

#ifdef KR_headers
double sin();
double r_sin(x) real *x;
#else
#undef abs
#include "mathh.h"
double r_sin(real *x)
#endif
{
return( sin(*x) );
}
