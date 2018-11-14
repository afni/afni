#ifndef _REPLACE_Xt_HEADER_
#define _REPLACE_Xt_HEADER_

/*---- Attempt to finesse old use of Xt in non-graphics code [09 Nov 2018] ---*/
/*---- Method: change all 'Xt' to 'Rwc' and then define/typedef them away  ---*/

# ifndef offsetof            /* should not be needed */
#  define offsetof(st, m) ((size_t)&(((st *)0)->m))
# endif

#undef  REPLACE_XT
#ifdef  REPLACE_XT           /* this is the finesse */

typedef   void*           RwcPointer ;
# define  RwcFree         free
# define  RwcCalloc       calloc
# define  RwcOffsetOf     offsetof
# define  RwcRealloc      realloc
# define  RwcMalloc       malloc
# define  RwcNew(t)       ((t *)calloc(1,sizeof(t)))
# define  RwcNewString(s) (strcpy(malloc(1+strlen(s)),str))

#else                        /* this is the crudesse */

# include <X11/Intrinsic.h>
# define  RwcPointer      XtPointer
# define  RwcFree         XtFree
# define  RwcCalloc       XtCalloc
# define  RwcOffsetOf     XtOffsetOf
# define  RwcRealloc      XtRealloc
# define  RwcMalloc       XtMalloc
# define  RwcNew          XtNew
# define  RwcNewString    XtNewString

#endif

#undef  myRwcFree
#undef  myRwcNew

/*! Macro to free a pointer and NULL-ize it as well. */
#define myRwcFree(xp) (RwcFree((char *)(xp)) , (xp)=NULL)

/*! Macro to allocate memory and zero-ize it. */
#define myRwcNew(type) ((type *) RwcCalloc(1,(unsigned) sizeof(type)))


#endif
