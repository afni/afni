#include <unistd.h>
#include <stdio.h>

/*----------------------------------------------------------------------------
   Become a daemon: fork, die, setsid, fork, die, disconnect 
------------------------------------------------------------------------------*/

void daemonize(void)
{
   pid_t pid ;

   pid = fork() ;
   if( pid < 0 )  exit(1) ;   /* fork failed */
   if( pid > 0 ) _exit(0) ;   /* parent exits */

   setsid() ;

   pid = fork() ;
   if( pid < 0 )  exit(1) ;   /* fork failed */
   if( pid > 0 ) _exit(0) ;   /* parent exits */

   chdir("/") ;
   freopen("/dev/null","r",stdin)  ;
   freopen("/dev/null","w",stdout) ;
   freopen("/dev/null","w",stderr) ;
}
