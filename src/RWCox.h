#ifndef RWCOX_INCLUDE
#define RWCOX_INCLUDE

/*** setup macros for conditional compilation, a la Cox ***/

#ifdef RWCOX
#ifdef linux
#define RWCOX_LINUX      1    /* use Cox stuff, for Linux system */
#else
#define RWCOX_NOT_LINUX  1    /* use Cox stuff, for generic Unix */
#endif
#else
#define NOT_RWCOX        1    /* don't use Cox stuff at all */
#endif

#endif
