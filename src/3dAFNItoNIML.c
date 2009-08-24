
     /***** A quickie, I hope -- RWCox - 09 Mar 2005 *****/

#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   NI_group *ngr ;
   NI_stream ns_out ;
   int iarg=1 , dodata=0 , nout=0 , doascii=0 ;
   char strname[256] = "stdout:" ;

   /*-- help me if you can --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dAFNItoNIML [options] dset\n"
      " Dumps AFNI dataset header information to stdout in NIML format.\n"
      " Mostly for debugging and testing purposes!\n"
      "\n"
      " OPTIONS:\n"
      "  -data          == Also put the data into the output (will be huge).\n"
      "  -ascii         == Format in ASCII, not binary (even huger).\n"
      "  -tcp:host:port == Instead of stdout, send the dataset to a socket.\n"
      "                    (implies '-data' as well)\n"
      "\n"
      "-- RWCox - Mar 2005\n"
     ) ;
     PRINT_COMPILE_DATE; exit(0) ;
   }

   mainENTRY("3dAFNItoNIML main"); machdep(); PRINT_VERSION("3dAFNItoNIML");
   if( PRINT_TRACING ){ STATUS("Enable mcw_malloc()"); enable_mcw_malloc(); }

   /*-- read command line options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-data") == 0 ){
       dodata++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-ascii") == 0 ){
       doascii = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-tcp:",5) == 0 ){
       strcpy(strname,argv[iarg]+1) ; dodata++ ; iarg++ ; continue ;
     }

     fprintf(stderr,"** Illegal option: %s\n",argv[iarg]); exit(1);
   }

   if( iarg >= argc ){
     fprintf(stderr,"** Not enough arguments on command line!\n"); exit(1);
   }

   /*-- read dataset --*/

   dset = THD_open_dataset( argv[iarg++] ); CHECK_OPEN_ERROR(dset,argv[iarg-1]);

   /*-- convert attributes to NIML --*/

   if( dodata ){
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
   }

   switch( dodata ){
     default:
     case 1:
       ngr = THD_dataset_to_niml( dset ) ; /* header and data together */
       DSET_unload(dset) ;
       if( ngr == NULL ){
         fprintf(stderr,"** Can't do THD_dataset_to_niml()\n"); exit(1);
       }
     break ;

     case 0:
     case 2:
       ngr = THD_nimlize_dsetatr( dset ) ;      /* header only for now */
       if( ngr == NULL ){
         fprintf(stderr,"** Can't do THD_nimlize_dsetatr()\n"); exit(1);
       }
     break ;

     case 3:
       ngr = NULL ;           /* will send brick data only, not header */
     break ;
   }

   /*-- open output stream --*/

   ns_out = NI_stream_open( strname , "w" ) ;
   if( ns_out == NULL ){
     fprintf(stderr,"** Can't create NIML stream!?\n"); exit(1);
   }

   /*-- if have a tcp: stream, must wait for it to connect --*/

   if( strcmp(strname,"stdout:") != 0 ){
     int nn , nchk ;
     for( nchk=0 ; nchk < 99 ; nchk++ ){
       nn = NI_stream_writecheck( ns_out , 777 ) ;
       if( nn == 1 ){ if(nchk>0)fprintf(stderr,"\n"); break; }
       if( nn <  0 ){ fprintf(stderr,"** NIML stream failure!?\n"); exit(1); }
       if( nchk==0 ){ fprintf(stderr,"Waiting"); }
       fprintf(stderr,".") ;
     }
     nn = NI_stream_writecheck( ns_out , 1 ) ;
     if( nn <= 0 ){ fprintf(stderr,"** Can't connect!?\n"); exit(1); }

     if( dodata > 1 ) nout += NI_write_procins( ns_out , "keep_reading" ) ;
   }

   /*-- if have group element from above, send it now --*/

   if( ngr != NULL ){
     NI_rename_group( ngr , "AFNI_dataset" ) ;
     NI_set_attribute( ngr , "self_prefix" , DSET_PREFIX(dset) ) ;
     if( doascii ) ngr->outmode = NI_TEXT_MODE ;
     nout += NI_write_element( ns_out , ngr , NI_TEXT_MODE ) ;
     NI_free_element( ngr ) ;
   }

   /*-- if sending data sub-bricks separately, do that now --*/

   if( dodata > 1 ){
     int iv ; NI_element *nel ;
     for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
       nel = THD_subbrick_to_niml( dset , iv , SBFLAG_INDEX ) ;
       if( nel != NULL ){
         if( doascii ) nel->outmode = NI_TEXT_MODE ;
         nout += NI_write_element( ns_out , nel ,
                                   (doascii) ? NI_TEXT_MODE : NI_BINARY_MODE ) ;
       }
       NI_free_element(nel) ;
     }
   }

   /*-- Ciao baby --*/

   NI_stream_closenow( ns_out ) ;
   fprintf(stderr,"++ Wrote %d bytes\n",nout) ;
   exit(0) ;
}
