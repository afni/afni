#include "converted_from_fortran.h"

#ifdef KR_headers
double erfc();
double erfc_(x) real *x;
#else
extern double erfc(double);
double erfc_(real *x)
#endif
{
return( erfc(*x) );
}
