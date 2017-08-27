#include "converted_from_fortran.h"

#ifdef KR_headers
ftnint iargc_()
#else
ftnint iargc_(void)
#endif
{
extern int xargc;
return ( xargc - 1 );
}
