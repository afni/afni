#include "converted_from_fortran.h"

#ifdef KR_headers
double floor();
shortint h_dnnt(x) doublereal *x;
#else
#undef abs
#include "mathh.h"
shortint h_dnnt(doublereal *x)
#endif
{
return( (*x)>=0 ?
	floor(*x + .5) : -floor(.5 - *x) );
}
