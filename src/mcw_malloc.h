#ifndef _MCW_MALLOC_HEADER_
#define _MCW_MALLOC_HEADER_

#include <stdlib.h>
#include <stdio.h>
#include <X11/Intrinsic.h>

#define myXtFree(xp)  (XtFree((char *)(xp)) , (xp)=NULL)
#define myXtNew(type) ((type *) XtCalloc(1,(Cardinal) sizeof(type)))
#define myfree(xp)    (free((xp)) , (xp)=NULL)

#include "machdep.h"

#ifdef DONT_USE_MCW_MALLOC

#define MCW_MALLOC_enabled 0

#else

#define USING_MCW_MALLOC

#undef malloc
#undef realloc
#undef calloc
#undef free

#define malloc(a)     mcw_malloc((a),__FILE__,__LINE__)
#define realloc(a,b)  mcw_realloc((a),(b),__FILE__,__LINE__)
#define calloc(a,b)   mcw_calloc((a),(b),__FILE__,__LINE__)
#define free          mcw_free

extern void   enable_mcw_malloc() ;
extern void * mcw_malloc( size_t , char * , int ) ;
extern void * mcw_realloc( void * , size_t , char * , int ) ;
extern void * mcw_calloc( size_t , size_t , char * , int ) ;
extern void   mcw_free( void * ) ;

extern char * mcw_malloc_status(void) ;
extern void   mcw_malloc_dump(void) ;
extern int    mcw_malloc_enabled(void) ;

#define MCW_MALLOC_enabled mcw_malloc_enabled()

#undef XtMalloc
#undef XtRealloc
#undef XtFree
#undef XtCalloc

#define XtMalloc(a)     mcw_XtMalloc((a),__FILE__,__LINE__)
#define XtRealloc(a,b)  mcw_XtRealloc((a),(b),__FILE__,__LINE__)
#define XtCalloc(a,b)   mcw_XtCalloc((a),(b),__FILE__,__LINE__)
#define XtFree          mcw_XtFree

extern char * mcw_XtMalloc( Cardinal , char * ,  int ) ;
extern char * mcw_XtRealloc( char * , Cardinal , char * ,  int ) ;
extern char * mcw_XtCalloc( Cardinal , Cardinal , char * ,  int ) ;
extern void   mcw_XtFree( char * ) ;

#endif /* DONT_USE_MCW_MALLOC */
#endif /* _MCW_MALLOC_HEADER_ */
