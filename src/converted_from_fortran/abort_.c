#include "stdio.h"
#include "converted_from_fortran.h"

#ifdef KR_headers
extern VOID sig_die();

int abort_()
#else
extern void sig_die(char*,int);

int abort_(void)
#endif
{
sig_die("Fortran abort routine called", 1);
return 0;
}
