#if !defined(NO_FRIVOLITIES) && defined(DARWIN)

#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

void AFNI_speak( char *string )
{
   char *buf ; pid_t ppp ;

   if( string == NULL || *string == '\0' ) return ;
   buf = getenv("AFNI_SPEECH") ;
   if( buf != NULL && toupper(*buf) == 'N' ) return ;

   ppp = fork() ; if( ppp != (pid_t)0 ) return ;

   buf = (char *)malloc(strlen(string)+32) ;
   sprintf(buf,"say -vCellos '%s'",string) ;
   system(buf) ;
   _exit(0) ;
}

#else

void AFNI_speak( char *string ){ return; }

#endif
