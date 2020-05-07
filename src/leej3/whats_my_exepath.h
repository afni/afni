#ifndef _WHATSMYEXEPATH_
#define _WHATSMYEXEPATH_
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
#endif