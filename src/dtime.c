#include "mrilib.h"
#include <unistd.h>

#define FNAME "Fred.Mertz"

int main( int argc , char * argv[] )
{
   int nmeg , nrep , ii , jj ;
   char * bbb ;
   FILE * fp ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: dtime meg rep\n"
             "Write 'meg' Mbytes of data to a file 'rep' times,\n"
             "and prints out the time that it takes.\n"
            ) ;
      exit(0) ;
   }

   nmeg = strtol( argv[1] , NULL , 10 ) * 1024 * 1024 ;
   nrep = strtol( argv[2] , NULL , 10 ) ;

   if( nmeg <= 0  || nrep <= 0 ){
      fprintf(stderr,"*** What are you, stupid?\n");
      exit(1);
   }

   if( THD_is_file(FNAME) ){
     fprintf(stderr,"Can't execute: temporary file %s exists!\n",FNAME) ;
     exit(1) ;
   }

   fp = fopen( FNAME , "wb" ) ;

   bbb = (char *) malloc( nmeg ) ;
   for( ii=0 ; ii < nmeg ; ii++ ) bbb[ii] = ii % 128 ;

   fprintf(stderr,"=== Initialization complete\n") ;

   (void) COX_clock_time() ;

   for( jj=0 ; jj < nrep ; jj++ ){
      fwrite( bbb , 1 , nmeg , fp ) ; fflush(fp) ; fsync(fileno(fp)) ; sync() ;
      fprintf(stderr,"#%03d: clock=%g\n", jj+1 , COX_clock_time() ) ;
   }

   fprintf(stderr,"=== %g Mbytes/second average\n",
           nmeg*nrep/(1024.*1024.*COX_clock_time()) ) ;

   fclose(fp) ; remove(FNAME) ; exit(0) ;
}
