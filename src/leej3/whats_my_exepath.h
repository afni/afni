#ifndef _WHATS_MY_EXEPATH_H_
#define _WHATS_MY_EXEPATH_H_
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <libgen.h>

#ifdef DARWIN
#   include <string.h>
#   include <errno.h>
#   include <libproc.h>
#   include <sys/syslimits.h>
#else
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <limits.h>
#endif

int whats_my_exepath(char output[],int len_array);
#endif /* _WHATS_MY_EXEPATH_H_ */
