#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int nfl , ii , ct ;
   char *sspec ;
   NI_element *nel ;
   NI_stream ns ;
   float *flar ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nimel N streamspec\n") ; exit(0) ;
   }

   nfl = strtol( argv[1] , NULL , 10) ;
   if( nfl < 1 ) nfl = 100000 ;
   sspec = argv[2] ;
   ns = NI_stream_open( sspec , "w" ) ;
   if( ns == NULL ){
     fprintf(stderr,"NI_stream_open fails\n"); exit(1);
   }

   nel = NI_new_data_element( "Tester" , nfl ) ;
   flar = (float *)malloc(sizeof(float)*nfl) ;
   for( ii=0 ; ii < nfl ; ii++ ) flar[ii] = (float)ii ;
   NI_add_column( nel , NI_FLOAT , flar ) ;
   free((void *)flar) ;

   while(1){
     ii = NI_stream_writecheck( ns , 666 ) ;
     if( ii == 1 ){ fprintf(stderr,"!\n") ; break ; }
     if( ii <  0 ){ fprintf(stderr,"BAD writecheck\n"); exit(1); }
     fprintf(stderr,".") ;
   }

   ct = NI_clock_time() ;
   NI_write_element( ns , nel , NI_BINARY_MODE ) ;
   NI_stream_closenow( ns ) ;
   ct = NI_clock_time() - ct ;
   fprintf(stderr,"Wrote %d floats in %d ms\n",nfl,ct) ;
   exit(0) ;
}
