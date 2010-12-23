#include "mrilib.h"

/*------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float mrad=0.0f ;
   int nrep=1 ;
   char *prefix = "Polyfit" ;
   int iarg , verb=0 , do_mask=0 , nord=3 , meth=2 ;
   THD_3dim_dataset *inset , *outset ;
   MRI_IMAGE *imout , *imin ;
   byte *mask=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dPolyfit [options] dataset\n"
             "Fits a polynomial in space to the dataset and outputs that.\n"
             "\n"
             "Options:\n"
             "  -nord n    = Maximum polynomial order (0..9) [default=3]\n"
             "  -mrad x    = Radius in voxels of preliminary median filter\n"
             "  -prefix pp = Use 'pp' for prefix of output dataset\n"
             "  -automask  = Create a mask (a la 3dAutomask)\n"
             "  -meth mm   = Set 'mm' to 2 for least squares fit; set it\n"
             "               to 1 for L1 fit [default=2]\n"
             "\n"
             "Output dataset is always stored in float format.  If the input\n"
             "dataset has more than 1 sub-brick, only sub-brick #0 is processed.\n"
             "\n"
             "-- Dec 2010 - RWCox\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dPolyfit main"); machdep(); AFNI_logger("3dPolyfit",argc,argv);
   PRINT_VERSION("3dPolyfit") ;

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-nord") == 0 ){
        nord = (int)strtol( argv[++iarg], NULL , 10 ) ;
        if( nord < 0 || nord > 9 )
          ERROR_exit("Illegal value after -nord!") ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-meth") == 0 ){
        meth = (int)strtol( argv[++iarg], NULL , 10 ) ;
        if( nord < 1 || nord > 2 )
          ERROR_exit("Illegal value after -meth!") ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-automask",5) == 0 ){
        do_mask++ ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
        verb++ ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mrad") == 0 ){
        mrad = strtod( argv[++iarg] , NULL ) ; iarg++ ; continue ;
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
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   if( DSET_NVALS(inset) > 1 )
     WARNING_message("Only processing sub-brick #0 !!");

   if( do_mask ){
     THD_automask_verbose( verb ) ;
     THD_automask_extclip( 1 ) ;
     mask = THD_automask( inset ) ;
   }

   imin = mri_to_float( DSET_BRICK(inset,0) ) ;
   if( imin == NULL ) ERROR_exit("Can't copy input dataset brick?! :-(") ;
   DSET_unload(inset) ;

   imout = mri_polyfit( imin , nord , mask , mrad , meth ) ;

   if( mask != NULL ) free((void *)mask) ;
   free(imin) ;

   outset = EDIT_empty_copy( inset )  ;
   EDIT_dset_items( outset ,
                      ADN_prefix , prefix ,
                      ADN_nvals  , 1 ,
                      ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dPolyfit" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
