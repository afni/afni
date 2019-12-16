#ifdef KR_headers
extern FILE *fdopen();
#else
#ifdef MSDOS
#include "io.h"
#define close _close
#define creat _creat
#define open _open
#define read _read
#define write _write
#endif

#ifndef __THROW
# if defined __cplusplus && (__GNUC__ >= 3 || __GNUC_MINOR__ >= 8) && !defined(DARWIN)
#  define __THROW       throw ()
# else
#  define __THROW
# endif
# define KILL__THROW
#endif

#ifdef __cplusplus
extern "C" {
#endif
#ifndef MSDOS
#ifdef OPEN_DECL
extern int creat(const char*,int), open(const char*,int);
#endif
extern int close(int) __THROW ;
#ifndef READ_WRITE_64      /* for some 64-bit machines  03 Aug 2004 [rickr] */
#include <sys/types.h>
extern ssize_t read(int,void*,size_t) __THROW ;
extern ssize_t write(int,const void*,size_t) __THROW ;
#endif
extern int unlink(const char*) __THROW ;
#ifndef _POSIX_SOURCE
#ifndef NON_UNIX_STDIO
extern FILE *fdopen(int, const char*);
#endif
#endif
#endif

extern char *mktemp(char*);

#ifdef __cplusplus
	}
#endif
#endif

#include "fcntl.h"

#ifndef O_WRONLY
#define O_RDONLY 0
#define O_WRONLY 1
#endif

#ifdef KILL__THROW
#undef __THROW
#endif
