#include "mrilib.h"

/*------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float irad=1.5 ;
   int nrep=1 ;
   char *prefix = "MedianFilter" ;
   int iarg , verb=0 , do_mask=0 , niter=1,pit ;
   THD_3dim_dataset *inset , *outset ;
   MRI_IMAGE *imout=NULL, *imin=NULL;
   byte *mask=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMedianFilter [options] dataset\n"
             "Computes the median in a spherical nbhd around each point in the\n"
             "input to produce the output.\n"
             "\n"
             "Options:\n"
             "  -irad x    = Radius in voxels of spherical regions\n"
             "  -iter n    = Iterate 'n' times [default=1]\n"
             "  -verb      = Be verbose during run\n"
             "  -prefix pp = Use 'pp' for prefix of output dataset\n"
             "  -automask  = Create a mask (a la 3dAutomask)\n"
             "\n"
             "Output dataset is always stored in float format.  If the input\n"
             "dataset has more than 1 sub-brick, only sub-brick #0 is processed.\n"
             "\n"
             "-- Feb 2005 - RWCox\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dMedianFilter main"); machdep(); AFNI_logger("3dMedianFilter",argc,argv);
   PRINT_VERSION("3dMedianFilter") ;

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-iter") == 0 ){
        niter = (int)strtol( argv[++iarg], NULL , 10 ) ;
        if( niter < 1 )
          ERROR_exit("Illegal value after -iter!") ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-automask",5) == 0 ){
        do_mask++ ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
        verb++ ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-irad") == 0 ){
        irad = strtod( argv[++iarg] , NULL ) ;
        if( irad < 1.0f )
          ERROR_exit("Illegal value after -irad!\n");
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) )
          ERROR_exit("Illegal value after -prefix!\n");
        iarg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s\n",argv[iarg]);
   }

   if( iarg >= argc )
     ERROR_exit("No dataset name on command line?\n");

   /*-- read input --*/

   inset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   DSET_load( inset ) ; CHECK_LOAD_ERROR(inset) ;
   if( DSET_NVALS(inset) > 1 )
     WARNING_message("Only processing sub-brick #0\n");

   if( do_mask ){
     THD_automask_verbose( verb ) ;
     THD_automask_extclip( 1 ) ;
     mask = THD_automask( inset ) ;
   }

   imin = mri_to_float( DSET_BRICK(inset,0) ) ;
   if( imin == NULL ) ERROR_exit("Can't copy input dataset brick") ;
   for( pit=0 ; pit < niter ; pit++ ){
     imout = mri_medianfilter( imin , irad , mask , verb ) ;
     if( pit < niter-1 && mri_equal(imout,imin) ){ /* 08 Aug 2018 */
       mri_free(imin) ;
       INFO_message("median filter reaches fixed 'root' image at iteration %d",pit+1) ;
       break ;
     }
     mri_free(imin) ; imin = imout ;
   }

   if( mask != NULL ) free((void *)mask) ;

   outset = EDIT_empty_copy( inset )  ;
   EDIT_dset_items( outset ,
                       ADN_prefix , prefix ,
                       ADN_nvals  , 1 ,
                       ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dMedianFilter" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
