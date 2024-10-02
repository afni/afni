#ifndef _REPLACE_Xt_HEADER_
#define _REPLACE_Xt_HEADER_

/*---- Attempt to finesse old use of Xt in non-graphics code [09 Nov 2018] ---*/
/*---- Method: change all 'Xt' to 'Rwc' and then define/typedef them away  ---*/

# ifndef offsetof            /* should not be needed */
#  define offsetof(st, m) ((size_t)&(((st *)0)->m))
# endif

/*---- If REPLACE_XT is defined as a compile flag X11 memory allocation is
     swapped out for C's malloc. This allows libmri.so to be compiled and
     linked without X11 as a dependency ---*/
#ifdef  REPLACE_XT           /* this is the finesse */

typedef   void*           RwcPointer ;
# define  RwcFree         free
# define  RwcCalloc       calloc
# define  RwcOffsetOf     offsetof
# define  RwcRealloc      realloc
# define  RwcMalloc       malloc
# define  RwcNew(t)       ((t *)calloc(1,sizeof(t)))
# define  RwcNewString(s) (strcpy(malloc(1+strlen(s)),s))

/* Xt is using char for boolean     [2 Oct 2024 rickr] */
typedef char              RwcBoolean ;  /* 07 Jul 2020 */
#undef  True
#undef  False
#define True  1
#define False 0

typedef unsigned int RwcCardinal ;      /* 08 Jul 2020 */
typedef void *       RwcWidget ;        /* 10 Jul 2020 */

#else                        /* this is the crudesse */

# include <X11/Intrinsic.h>
# define  RwcPointer      XtPointer
# define  RwcBoolean      Boolean
# define  RwcFree         XtFree
# define  RwcCalloc       XtCalloc
# define  RwcOffsetOf     XtOffsetOf
# define  RwcRealloc      XtRealloc
# define  RwcMalloc       XtMalloc
# define  RwcNew          XtNew
# define  RwcNewString    XtNewString
# define  RwcWidget       Widget
# define  RwcCardinal     Cardinal

#endif

#undef  myRwcFree
#undef  myRwcNew

/*! Macro to free a pointer and NULL-ize it as well. */
#define myRwcFree(xp) (RwcFree((char *)(xp)) , (xp)=NULL)

/*! Macro to allocate memory and zero-ize it. */
#define myRwcNew(type) ((type *) RwcCalloc(1,(unsigned) sizeof(type)))

#endif
