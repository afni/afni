#if !defined(NO_FRIVOLITIES) && defined(DARWIN)

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/wait.h>
#include <sys/types.h>

/*! Speak a string using Apple's say command:
    - string = Apple text-to-speech code
    - nofork = 1 if you want to wait for the speech to finish;
             = 0 if you want the function to return immediately,
                 before speech finishes (or perhaps even starts). */

void AFNI_speak( char *string , int nofork )
{
   char *buf ; pid_t ppp ;

   if( string == NULL || *string == '\0' ) return ;
   buf = getenv("AFNI_SPEECH") ;
   if( buf != NULL && toupper(*buf) == 'N' ) return ;

   if( !nofork ){
     ppp = fork() ;
     if( ppp < 0 ) return ; /* fork failed */

     /* parent: wait for child to exit (almost instantly) */

     if( ppp > 0 ){ waitpid(ppp,NULL,0); return; }

     /* child forks again immediately, then exits;
        this is to prevent zombie processes from hanging around */

     ppp = fork() ; if( ppp != 0 ) _exit(0) ; /* child exits now */

     /* grandchild continues on to actually do something */
   }

   /* Run the say program using system() */

   buf = (char *)malloc(strlen(string)+32) ;
   sprintf(buf,"say -vCellos '%s'",string) ;
   system(buf) ; free(buf) ;

   if( !nofork ) _exit(0) ;  /* grandchild exits */
   return ;                  /* no forking ==> return to caller */
}

#else

void AFNI_speak( char *string , int nofork ){ return; }  /* dummy function */

#endif
