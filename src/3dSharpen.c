#include "mrilib.h"


int main( int argc, char *argv[] )
{
   THD_3dim_dataset *din=NULL, *dout;
   MRI_IMAGE *im ;
   char *prefix = "Sharpen3D.nii" ;
   float phi=0.4f ;
   int iarg , nopt ;

   /*-- help? --*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
      "Usage: 3dSharpen [options] dataset\n"
      "\n"
      "Applies a simple 3D sharpening filter to the POSITIVE values\n"
      "in the #0 volume of the input dataset, and writes out a new\n"
      "dataset.\n"
      "\n"
      "Only operates on positive valued voxels in the dataset.\n"
      "Non-positive values will not be altered.\n"
      "\n"
      "Options:\n"
      "--------\n"
      "\n"
      " -phi fff       = Sharpening factor, between 0.1 and 0.9 (inclusive).\n"
      "                  Larger means more sharpening. Default is 0.4.\n"
      "\n"
      " -input dataset = An option to input the dataset anywhere,\n"
      "                  not just at the end of the command line.\n"
      "\n"
      " -prefix pref   = Select the name of the output dataset\n"
      "                  (it will be in floating point format).\n"
      "\n"
      "* A quick hack for experimental purposes.\n"
      "* e.g., Cleaning up the results of brain template construction.\n"
      "* RWCox - Feb 2017.\n"
      "\n"
     ) ;
     exit(0) ;
   }

   /*-- process options --*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("Need argument after -prefix") ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad prefix '%s'",prefix) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-phi") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("Need argument after -phi") ;
       phi = (float)strtod(argv[nopt],NULL) ;
       if( phi < 0.1f || phi > 0.9f ){
         WARNING_message("Re-setting -phi from input %g to 0.5",phi) ;
         phi = 0.5f ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ||
         strcasecmp(argv[nopt],"-inset") == 0   ){
       if( din != NULL )    ERROR_exit("Can't give input dataset twice :(") ;
       if( ++nopt >= argc ) ERROR_exit("Need argument after %s",argv[nopt-1]) ;
       din = THD_open_one_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(din,argv[nopt]) ;
       DSET_load(din) ; CHECK_LOAD_ERROR(din) ;
       nopt++ ; continue ;
     }

     ERROR_exit("3dSharpen: unknown option '%s'",argv[nopt]) ;
   }

   /*-- read input if not already there --*/

   if( nopt < argc-1 ){
     if( din == NULL ){
       din = THD_open_one_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(din,argv[nopt]) ;
       DSET_load(din) ; CHECK_LOAD_ERROR(din) ;
     } else {
       WARNING_message("arguments after last option are ignored") ;
     }
   }

   if( din == NULL )
     ERROR_exit("3dSharpen: No input dataset?") ;

   /*-- create output dataset shell --*/

   dout = EDIT_empty_copy(din) ;
   EDIT_dset_items( dout ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , 1      ,
                      ADN_brick_fac , NULL   ,
                    ADN_none ) ;

   /*-- get copy of dataset brick to process in place --*/

   im = THD_extract_float_brick(0,din) ;
   if( im == NULL )
     ERROR_exit("Can't extract sub-brick #0 from input dataset :(") ;
   DSET_unload(din) ;

   /*-- do the work --*/

   mri_sharpen3D_pos( im , phi ) ;

   /*-- write it out --*/

   EDIT_substitute_brick( dout , 0 , MRI_float , MRI_FLOAT_PTR(im) ) ;
   DSET_write(dout) ; WROTE_DSET(dout) ;

   exit(0) ;
}
