#include "converted_from_fortran.h"

#ifdef KR_headers
integer i_mod(a,b) integer *a, *b;
#else
integer i_mod(integer *a, integer *b)
#endif
{
return( *a % *b);
}
