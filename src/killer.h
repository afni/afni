#ifndef _MCW_KILLER_H_
#define _MCW_KILLER_H_

/******
   Header file to manipulate "kill lists":
    A way to maintain a list of data that should be deleted when necessary.
    The pointers should be from XtMalloc, not malloc.
*******/

/** to get XtMalloc stuff **/

#ifndef USE_GNU_MALLOC
#  include <X11/Intrinsic.h>
#endif

/** structure to hold kill list **/

typedef struct {
   int     num , nalloc ;
   char ** kill ;
} KILL_list ;

#define KILL_INC 32

/** initialize a new kill list **/

#define INIT_KILL(kl) ((kl).num=0, (kl).nalloc=0, (kl).kill=NULL)

/** add a pointer to be killed **/

#define ADDTO_KILL(kl,p)                                           \
 {  if( (kl).num == (kl).nalloc ){                                 \
       (kl).nalloc += KILL_INC ;                                   \
       (kl).kill    = (char **) XtRealloc( (char *) (kl).kill ,    \
                                  sizeof(char *) * (kl).nalloc ) ; \
    }                                                              \
    (kl).kill[(kl).num++] = (char *) p ;                           \
 }

/** kill the data in a single pointer in the kill list,
    but keep the rest of the data in the list intact   **/

#define SINGLE_KILL(kl,p)                                   \
   {  char * cp = (char *) (p) ;                            \
      int qwer ;                                            \
      for( qwer=0 ; qwer < (kl).num ; qwer++ )              \
         if( cp == (kl).kill[qwer] ){                       \
            myXtFree(cp) ; (kl).kill[qwer] = NULL ; break ; \
         }                                                  \
   }

/** remove a pointer from the kill list, without deleting its data **/

#define REMOVEFROM_KILL(kl,p)                  \
   {  char * cp = (char *) (p) ;               \
      int qwer ;                               \
      for( qwer=0 ; qwer < (kl).num ; qwer++ ) \
         if( cp == (kl).kill[qwer] ){          \
            (kl).kill[qwer] = NULL ; break ;   \
         }                                     \
   }

/** replace a pointer in the kill list (perhaps it was XtRealloc-ed).
    If it doesn't exist, add it to the kill list.                     **/

#define REPLACE_KILL(kl,pold,pnew)                   \
   {  char * cpold = (char *) (pold) ;               \
      char * cpnew = (char *) (pnew) ;               \
      int qwer ;                                     \
      for( qwer=0 ; qwer < (kl).num ; qwer++ )       \
         if( cpold == (kl).kill[qwer] ){             \
            (kl).kill[qwer] = cpnew ; break ;        \
         }                                           \
      if( qwer == (kl).num ) ADDTO_KILL((kl),pnew) ; \
   }

/** kill all the data in the kill list! **/

#define KILL_KILL(kl) {                                                    \
      int qwer ;                                                           \
      for( qwer=0 ; qwer < (kl).num ; qwer++ ) myXtFree((kl).kill[qwer]) ; \
      myXtFree( (kl).kill) ;                                               \
      INIT_KILL(kl) ;                                                      \
   }

#endif /* _MCW_KILLER_H_ */
