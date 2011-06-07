#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   NI_group *ngr ;
   NI_stream ns ;
   int ct , ii ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nidset tcp:host:%d dataset\n",
            get_port_named("AFNI_DEFAULT_LISTEN_NIML")); exit(0);
   }

   ns = NI_stream_open( argv[1] , "w" ) ;
   if( ns == NULL ){
     fprintf(stderr,"** Can't open stream '%s'\n",argv[1]); exit(1);
   }

   dset = THD_open_dataset( argv[2] ) ;
   if( dset == NULL ){
     fprintf(stderr,"** Can't open dataset '%s'\n",argv[2]); exit(1);
   }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
     fprintf(stderr,"** Can't load dataset '%s'\n",argv[2]); exit(1);
   }
   ngr = THD_dataset_to_niml(dset) ;
   if( ngr == NULL ){
     fprintf(stderr,"** Can't convert dataset to NIML\n"); exit(1);
   }

   while(1){
     ii = NI_stream_writecheck( ns , 666 ) ;
     if( ii == 1 ){ fprintf(stderr,"!READY!\n") ; break ; }
     if( ii <  0 ){ fprintf(stderr,"BAD writecheck\n"); exit(1); }
     fprintf(stderr,".") ;
   }

   ct = NI_clock_time() ;
   NI_write_element( ns , ngr , NI_BINARY_MODE ) ;
   NI_stream_close( ns ) ;
   ct = NI_clock_time() - ct ;
   fprintf(stderr,"++ Wrote element in %d ms\n",ct) ;
   exit(0) ;
}
