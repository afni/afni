#include "3ddata.h"
#include "thd.h"


/**********************************************************************
  Routine to return a (hopefully) unique ID code to be used to identify
  a dataset to other datasets.  This is to prevent the former strategy
  of relying on the user input "self_name" being unique, since that
  turns out not to be a good idea (users -- they make software so
  much harder to write).  The method used was dreamed up while the
  author (RWCox) had the flu, so it is probably pitiful.
***********************************************************************/

#include <sys/utsname.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>

static char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

MCW_idcode MCW_new_idcode(void)
{
   struct utsname ubuf ;
   struct timeval tv ;
   struct timezone tz ;
   time_t tnow ;
   long   tt ;
   int    nn , tq , iq , ic ;
   MCW_idcode newid ;

   nn = uname( &ubuf ) ;
   if( nn == -1 ) strcpy( ubuf.nodename , "A" ) ;

   nn = gettimeofday( &tv , NULL ) ;
   if( nn == -1 ){ tv.tv_sec = lrand48()%9999999 ; tv.tv_usec = lrand48()%999999 ; }
   tt = tv.tv_sec + 13*tv.tv_usec + lrand48() % 9999999 ;

   nn = strlen( ubuf.nodename ) ; iq = 0 ;

   strcpy(newid.str,MCW_IDPREFIX) ; ic = strlen(newid.str) ;

   for( ; ic < MCW_IDSIZE-1 ; ic++ ){
     tq  = (tt % 26) + (int) ubuf.nodename[iq] ; tq  = tq % 26 ;
     iq  = (iq+1) % nn ; tt /= 5 ;
     newid.str[ic] = alphabet[tq] ;
   }
   newid.str[ic] = '\0' ;

   tnow = time(NULL) ;
   MCW_strncpy( newid.date , ctime(&tnow) , MCW_IDDATE ) ;
   nn = strlen(newid.date) ;
   if( nn > 0 && newid.date[nn-1] == '\n' ) newid.date[nn-1] = '\0' ;

   return newid ;
}
