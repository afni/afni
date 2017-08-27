#include "converted_from_fortran.h"

#ifdef KR_headers
double tanh();
double d_tanh(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
double d_tanh(doublereal *x)
#endif
{
return( tanh(*x) );
}
