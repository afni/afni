#include "cdflib.h"
double fifdint(double a)
/************************************************************************
FIFDINT:
Truncates a double precision number to an integer and returns the
value in a double.
************************************************************************/
/* a     -     number to be truncated */
{
  return (double) ((int) a);
} /* END */
