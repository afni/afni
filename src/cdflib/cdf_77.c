#include "cdflib.h"
long fifidint(double a)
/************************************************************************
FIFIDINT:
Truncates a double precision number to a long integer
************************************************************************/
/* a - number to be truncated */
{
  if (a < 1.0) return (long) 0;
  else return (long) a;
} /* END */
