#include "cdflib.h"
double fifdsign(double mag,double sign)
/************************************************************************
FIFDSIGN:
transfers the sign of the variable "sign" to the variable "mag"
************************************************************************/
/* mag     -     magnitude */
/* sign    -     sign to be transfered */
{
  if (mag < 0) mag = -mag;
  if (sign < 0) mag = -mag;
  return mag;

} /* END */
