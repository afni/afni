#include "cdflib.h"
void ftnstop(char* msg)
/************************************************************************
FTNSTOP:
Prints msg to standard error and then exits
************************************************************************/
/* msg - error message */
{
  if (msg != NULL) fprintf(stderr,"%s\n",msg);
  exit(1);
} /* END */
