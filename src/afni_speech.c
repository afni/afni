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

static int have_say = -1 ;

void AFNI_speak( char *string , int nofork )
{
   char *buf ; pid_t ppp ;

   /* bad input ==> quit */

   if( string == NULL || *string == '\0' ) return ;

   /* user says "don't talk" ==> quit */

   buf = getenv("AFNI_SPEECH") ;
   if( buf != NULL && toupper(*buf) == 'N' ) return ;

   /* don't have "say" program ==> quit */

   if( have_say == -1 ) have_say = (THD_find_executable("say") != NULL) ;
   if( have_say == 0 ) return ;

   /* if want speech to run in a separate process ... */

   if( !nofork ){
     ppp = fork() ;
     if( ppp < 0 ) return ; /* fork failed */

     /* parent: wait for child to exit (happens almost instantly) */

     if( ppp > 0 ){ waitpid(ppp,NULL,0); return; }

     /* child: fork again immediately, then this child exits;
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
