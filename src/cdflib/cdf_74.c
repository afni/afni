#include "cdflib.h"
double fifdmax1(double a,double b)
/************************************************************************
FIFDMAX1:
returns the maximum of two numbers a and b
************************************************************************/
/* a     -      first number */
/* b     -      second number */
{
  if (a < b) return b;
  else return a;
} /* END */
