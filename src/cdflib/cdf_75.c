#include "cdflib.h"
double fifdmin1(double a,double b)
/************************************************************************
FIFDMIN1:
returns the minimum of two numbers a and b
************************************************************************/
/* a     -     first number */
/* b     -     second number */
{
  if (a < b) return a;
  else return b;
} /* END */
