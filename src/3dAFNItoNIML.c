
     /***** A quickie, I hope -- RWCox - 09 Mar 2005 *****/

#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   NI_group *ngr ;
   NI_stream ns_out ;
   int iarg=1 ;

   /*-- help me if you can --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoNIML dset\n"
             " Dumps AFNI dataset header information to stdout in NIML format.\n"
             " Mostly for debugging and testing purposes!\n"
             "-- RWCox - 09 Mar 2005\n"
      ) ;
      exit(0) ;
   }

   /*-- read command line options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){
     fprintf(stderr,"** Illegal option: %s\n",argv[iarg]); exit(1);
   }

   if( iarg >= argc ){
     fprintf(stderr,"** Not enough arguments on command line!\n"); exit(1);
   }

   /*-- read dataset --*/

   dset = THD_open_dataset( argv[iarg++] ) ;
   if( dset == NULL ){
     fprintf(stderr,"** Can't open dataset %s\n",argv[iarg-1]) ;
     exit(1) ;
   }

   /*-- convert attributes to NIML --*/

   ngr = THD_nimlize_dsetatr( dset ) ;
   if( ngr == NULL ){
     fprintf(stderr,"** Can't create NIML element!?\n"); exit(1);
   }
   NI_rename_group( ngr , "AFNI_dataset" ) ;

   /*-- open stream to stdout, write element, close stream --*/

   ns_out = NI_stream_open( "stdout:" , "w" ) ;
   if( ns_out == NULL ){
     fprintf(stderr,"** Can't create NIML stream!?\n"); exit(1);
   }

   NI_write_element( ns_out , ngr , NI_TEXT_MODE ) ;
   NI_stream_closenow( ns_out ) ;

   /*-- Ciao baby --*/

   exit(0) ;
}
