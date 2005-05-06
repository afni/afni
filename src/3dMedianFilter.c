#include "mrilib.h"

/*------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float irad=1.5 ;
   int nrep=1 ;
   char *prefix = "MedianFilter" ;
   int iarg , verb=0 , do_mask=0 ;
   THD_3dim_dataset *inset , *outset ;
   MRI_IMAGE *imout ;
   byte *mask=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMedianFilter [options] dataset\n"
             "Computes the median in a spherical nbhd around each point in the\n"
             "input to produce the output.\n"
             "\n"
             "Options:\n"
             "  -irad x    = Radius in voxels of spherical regions\n"
             "  -verb      = Be verbose during run\n"
             "  -prefix pp = Use 'pp' for prefix of output dataset\n"
             "  -automask  = Create a mask (a la 3dAutomask)\n"
             "\n"
             "Output dataset is always stored in float format.  If the input\n"
             "dataset has more than 1 sub-brick, only sub-brick #0 is processed.\n"
             "\n"
             "-- Feb 2005 - RWCox\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dMedianFilter main"); machdep(); AFNI_logger("3dMedianFilter",argc,argv);

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-automask",5) == 0 ){
        do_mask++ ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
        verb++ ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-irad") == 0 ){
         irad = strtod( argv[++iarg] , NULL ) ;
         if( irad < 1.0 ){
            fprintf(stderr,"*** Illegal value after -irad!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal value after -prefix!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[iarg]); exit(1) ;
   }

   if( iarg >= argc ){
      fprintf(stderr,"*** No dataset name on command line?\n"); exit(1);
   }

   /*-- read input --*/

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
     fprintf(stderr,"**  ERROR: Can't open dataset %s\n",argv[iarg]); exit(1);
   }
   DSET_load( inset ) ;
   if( !DSET_LOADED(inset) ){
     fprintf(stderr,"** ERROR: can't load dataset %s\n",argv[iarg]); exit(1);
   }
   if( DSET_NVALS(inset) > 1 ){
     fprintf(stderr,"++ WARNING: only processing sub-brick #0\n") ;
   }

   if( do_mask ){
     THD_automask_verbose( verb ) ;
     THD_automask_extclip( 1 ) ;
     mask = THD_automask( inset ) ;
   }

   imout = mri_medianfilter( DSET_BRICK(inset,0) , irad , mask , verb ) ;
   free((void *)mask) ;

   outset = EDIT_empty_copy( inset )  ;
   EDIT_dset_items( outset ,
                       ADN_prefix , prefix ,
                       ADN_nvals  , 1 ,
                       ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dMedianFilter" , argc,argv , outset ) ;
   fprintf(stderr,"++ output dataset: %s\n",DSET_BRIKNAME(outset)) ;
   DSET_write(outset) ;
   exit(0) ;
}
