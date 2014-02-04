/* libsrc/ncconfig.h.  Generated automatically by configure.  */
/* libsrc/ncconfig.in.  Generated automatically from configure.in by autoheader.  */
#ifndef _NCCONFIG_H_
#define _NCCONFIG_H_

/* Define if you're on an HP-UX system. */
/* #undef _HPUX_SOURCE */

/* Define if type char is unsigned and you are not using gcc.  */
#ifndef __CHAR_UNSIGNED__
/* #undef __CHAR_UNSIGNED__ */
#endif

/* Define if your struct stat has st_blksize.  */
#define HAVE_ST_BLKSIZE 1

/* Define to `long' if <sys/types.h> doesn't define.  */
/* #undef off_t */

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if you have the ANSI C header files.  */
/* #undef STDC_HEADERS */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* Define if you don't have the <stdlib.h>.  */
/* #undef NO_STDLIB_H */

/* Define if you don't have the <sys/types.h>.  */
/* #undef NO_SYS_TYPES_H */

/* Define if you have the ftruncate function  */
/* #undef HAVE_FTRUNCATE */

/* Define if you have alloca, as a function or macro.  */
/* #undef HAVE_ALLOCA */

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
/* #undef HAVE_ALLOCA_H */

/* Define if you don't have the strerror function  */
#define NO_STRERROR 1

/* The number of bytes in a size_t */
#define SIZEOF_SIZE_T 0

/* The number of bytes in a off_t */
#define SIZEOF_OFF_T 0

/* Define to `int' if system doesn't define.  */
/* #undef ssize_t */

/* Define to `int' if system doesn't define.  */
#define ptrdiff_t int

/* Define to `unsigned char' if system doesn't define.  */
#define uchar unsigned char

/* Define if the system does not use IEEE floating point representation */
#define NO_IEEE_FLOAT 1

/* The number of bytes in a double.  */
#define SIZEOF_DOUBLE 0

/* The number of bytes in a float.  */
#define SIZEOF_FLOAT 0

/* The number of bytes in a int.  */
#define SIZEOF_INT 0

/* The number of bytes in a long.  */
#define SIZEOF_LONG 0

/* The number of bytes in a short.  */
#define SIZEOF_SHORT 0

#endif /* !_NCCONFIG_H_ */
