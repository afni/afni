#include "mrilib.h"

#define WDEL 1
#define RDEL 1

int main( int argc , char * argv[] )
{
   IOCHAN * ioc ;
   int writer , ii , jj , kk , nbytes,nreps ;
   char * buf ;
   double time1,time2,rate ;

   if( argc < 5 ){
      printf("Usage: iotest iochan_spec mode_spec nbytes nreps\n");
      printf("where iochan_spec = shm:name:size or tcp:host:port\n");
      printf("      mode        = w or r\n") ;
      exit(0);
   }

   writer = (strcmp(argv[2],"create") == 0 || strcmp(argv[2],"w") == 0) ;

   nbytes = strtol( argv[3] , NULL , 10 ) ;
   if( nbytes < 2 ){fprintf(stderr,"nbytes is bad!\n");exit(1);}
   nreps  = strtol( argv[4] , NULL , 10 ) ;
   if( nreps  < 1 ){fprintf(stderr,"nreps is bad!\n");exit(1);}
   buf = (char *) malloc(nbytes) ;

   ioc = iochan_init( argv[1] , argv[2] ) ;
   if( ioc == NULL ){ printf("Error on creation of IOCHAN\n") ; exit(1) ; }

   printf("iochan_init returns ioc->bad = %d\n",ioc->bad) ;

   while( ! iochan_goodcheck(ioc,1000) ){
      printf(".") ; fflush(stdout) ;
   }
   printf("\nConnected\n") ;

   time1 = COX_clock_time() ;
   if( writer ){
      for( kk=0 ; kk < nreps ; kk++ ){
         while(1){
            jj = iochan_writecheck(ioc,WDEL) ;  /* check if ready */
            if( jj > 0 ) break ;
            if( jj < 0 ){ printf("\nIOCHAN is now bad!\n"); goto done;}
            printf(".") ; fflush(stdout) ;
         }
         iochan_sendall( ioc , buf , nbytes ) ;
         if( kk%10 == 0 ){printf("!") ; fflush(stdout) ;}
      }
      while( ! iochan_clearcheck(ioc,WDEL) ){ printf("c"); fflush(stdout); }
   } else {
      time1 = COX_clock_time() ;
      for( kk=0 ; kk < nreps ; kk++ ){
         while(1){
            jj = iochan_readcheck(ioc,RDEL) ;  /* check if ready */
            if( jj > 0 ) break ;
            if( jj < 0 ){ printf("\nIOCHAN is now bad!\n"); goto done; }
            printf(".") ; fflush(stdout) ;
         }
         ii = iochan_recvall( ioc , buf , nbytes ) ;
         if( kk%10 == 0 ){printf("!") ; fflush(stdout) ;}
      }
   }

done:
   time2 = COX_clock_time() - time1 ;
   rate  = 0.001 * nbytes * nreps / time2 ;
   printf("\n%f K/sec\n",rate) ;
   IOCHAN_CLOSE(ioc) ;
   exit(0) ;
}
