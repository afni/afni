#include "niml.h"

/*--- Debug printout of a NIML element. ---*/

void NIML_to_stderr( void *nini )
{
   NI_stream ns_err ;
   ns_err = NI_stream_open( "stderr:" , "w" ) ;
   if( ns_err != NULL ){
     NI_write_element( ns_err , nini , NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) ;
     NI_stream_close( ns_err ) ;
   }
}

/*--- Open a NIML stream, read elements from it, print them ---*/

int main( int argc , char *argv[] )
{
   NI_stream ns ;
   void *nini ;
   int nn ;

   if( argc < 2 ){
      fprintf(stderr,"Usage: niccc streamspec\n");exit(0);
   }

   ns = NI_stream_open( argv[1], "r" ) ;
   if( ns == NULL ){
      fprintf(stderr,"*** niccc: NI_stream_open fails\n") ; exit(1) ;
   }
   while(1){
     nn = NI_stream_goodcheck( ns , 1 ) ;
     if( nn < 0 ){
       fprintf(stderr,"\n*** niccc: Stream fails\n"); exit(1);
     }
     if( nn == 0 ){ NI_sleep(5); continue; }  /* waiting for Godot */

     nn = NI_stream_readcheck( ns , 1 ) ;     /* check for data */

     if( nn > 0 ){
       nini = NI_read_element( ns , 2 ) ;
       if( nini == NULL ){
         fprintf(stderr,"*** niccc: read returns NULL\n");
       } else {
         NIML_to_stderr( nini ) ;
         NI_free_element( nini ) ;
       }
     }
   }
}
