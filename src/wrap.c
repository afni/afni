#include "iochan.h"

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

#define NBUF  2048
#define DELAY 1

int main( int argc , char * argv[] )
{
   pid_t child_pid ;

   child_pid = fork() ;
   if( child_pid == (pid_t)(-1) ){fprintf(stderr,"Cannot fork!\n");exit(1);}

   if( child_pid == 0 ){  /* I'm the child */
      FILE * fp ;
      char buf[NBUF] = "\0" ;
      int nbuf=0 , jj ;
      IOCHAN * ioc ;

      fp = popen("rsh 3T60 -l scan ./wrap" , "r") ;
      if( fp == NULL ){fprintf(stderr,"Cannot popen!\n");exit(1);}

      while( fgets(buf+nbuf,NBUF-nbuf,fp) != NULL ){
         nbuf = strlen(buf) ;
      }
      pclose(fp) ; fprintf(stderr,"Child has data\n") ;

      ioc = iochan_init( "shm:fred:4K" , "w" ) ;
      if( ioc == NULL ){fprintf(stderr,"Child cannot iochan_init!\n");exit(1);}

      while(1){
         jj = iochan_writecheck(ioc,DELAY) ;  /* check if ready */
         if( jj > 0 ) break ;
         if( jj < 0 ){fprintf(stderr,"Child IOCHAN is now bad!\n");exit(1);}
         fprintf(stderr,".") ; fflush(stderr) ;
      }
      iochan_sendall( ioc , buf , nbuf ) ;
      while( ! iochan_clearcheck(ioc,DELAY) ){fprintf(stderr,"c"); fflush(stderr);}
      fprintf(stderr,"Child exiting\n") ;
      IOCHAN_CLOSE(ioc) ;

   } else {               /* I'm the parent */
      IOCHAN * ioc ;
      int jj , nbuf ;
      char buf[NBUF] = "\0" ;
      double ct ;

      ct = COX_clock_time() ;
      fprintf(stderr,"*** Parent waiting ***\n") ;
      ioc = iochan_init( "shm:fred:4K" , "r" ) ;
      if( ioc == NULL ){fprintf(stderr,"Parent cannot iochan_init!\n");exit(1);}

      while(1){
         jj = iochan_readcheck(ioc,DELAY) ;  /* check if ready */
         if( jj > 0 ) break ;
         if( jj < 0 ){fprintf(stderr,"Parent IOCHAN is now bad!\n");exit(1);}
      }
      fprintf(stderr,"Parent receiving\n") ;
      jj = iochan_recv( ioc , buf , NBUF ) ;
      IOCHAN_CLOSE(ioc) ;
      ct = COX_clock_time() - ct ;
      fprintf(stderr,"Parent got %d bytes in %6.2f seconds:\n%s",jj,ct,buf) ;
      (void) wait(NULL) ;
   }

   exit(0) ;
}
