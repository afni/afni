/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _MCW_MALLOC_HEADER_
#define _MCW_MALLOC_HEADER_

/*----- 24 Jan 2001: modified slightly to add some comments, and
                     to fit in with the hashtable-ized mcw_malloc.c -----*/

#include <stdlib.h>
#include <stdio.h>

#ifdef __BUILDING_QUICKLOOK_PLUGIN__
  #include "IntrinsicQuickLook.h"
#else
  #include <X11/Intrinsic.h>
#endif

#include "machdep.h"

#ifdef  __cplusplus
extern "C" {
#endif

/*---------------------------------------------------------------------------*/
#ifdef DONT_USE_MCW_MALLOC

#define MCW_MALLOC_enabled 0

#define MCW_MALLOC_status  NULL
#define MCW_MALLOC_total   0

#undef  mcw_malloc
#define mcw_malloc  malloc

#undef  mcw_realloc
#define mcw_realloc realloc

#undef  mcw_calloc
#define mcw_calloc  calloc

#undef  mcw_free
#define mcw_free    free

/*---------------------------------------------------------------------------*/
#else

#define USING_MCW_MALLOC

/*-- define macros to replace the source code's use of malloc(), etc. --*/

#undef malloc
#undef realloc
#undef calloc
#undef free

#define malloc(a)     mcw_malloc((a),__FILE__,__LINE__)
#define realloc(a,b)  mcw_realloc((a),(b),__FILE__,__LINE__)
#define calloc(a,b)   mcw_calloc((a),(b),__FILE__,__LINE__)
#define free          mcw_free

/*-- prototypes for interface functions --*/

extern void   enable_mcw_malloc() ;
extern void * mcw_malloc( size_t , char * , int ) ;
extern void * mcw_realloc( void * , size_t , char * , int ) ;
extern void * mcw_calloc( size_t , size_t , char * , int ) ;
extern void   mcw_free( void * ) ;

extern char * mcw_malloc_status(const char *,int) ;
extern void   mcw_malloc_dump(void) ;
extern int    mcw_malloc_enabled(void) ;
extern void   pause_mcw_malloc(void);
extern void   resume_mcw_malloc(void);
extern int    mcw_malloc_paused(void);

extern long long mcw_malloc_total(void) ; /* 01 Feb 2007 */

/*-- how to check if the tracking routines are working --*/

#define MCW_MALLOC_enabled mcw_malloc_enabled()

#define MCW_MALLOC_status  mcw_malloc_status(__FILE__,__LINE__)
#define MCW_MALLOC_total   mcw_malloc_total()

/*-- do the same macro thing for the Xt library functions --*/

#undef XtMalloc
#undef XtRealloc
#undef XtFree
#undef XtCalloc

#define XtMalloc(a)     mcw_XtMalloc((a),__FILE__,__LINE__)
#define XtRealloc(a,b)  mcw_XtRealloc((char *)(a),(b),__FILE__,__LINE__)
#define XtCalloc(a,b)   mcw_XtCalloc((a),(b),__FILE__,__LINE__)
#define XtFree(a)       mcw_XtFree((char *)(a))

extern char * mcw_XtMalloc( Cardinal , char * ,  int ) ;
extern char * mcw_XtRealloc( char * , Cardinal , char * ,  int ) ;
extern char * mcw_XtCalloc( Cardinal , Cardinal , char * ,  int ) ;
extern void   mcw_XtFree( char * ) ;

#endif /* DONT_USE_MCW_MALLOC */
/*---------------------------------------------------------------------------*/

/*-- some macros used in various AFNI places --*/

#define myXtFree(xp)  (XtFree((char *)(xp)) , (xp)=NULL)
#define myXtNew(type) ((type *) XtCalloc(1,(Cardinal) sizeof(type)))
#define myfree(xp)    (free((xp)) , (xp)=NULL)

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_MALLOC_HEADER_ */
