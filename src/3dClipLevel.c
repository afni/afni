#include "mrilib.h"

int main( int argc , char * argv[] )
{
   float mfrac=0.50f , val ;
   MRI_IMAGE *medim ;
   THD_3dim_dataset *dset ;
   char *gprefix=NULL ;
   int iarg=1 , doall=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dClipLevel [options] dataset\n"
       "Estimates the value at which to clip the anatomical dataset so\n"
       "  that background regions are set to zero.\n"
       "\n"
       "The program's output is a single number sent to stdout.  This\n"
       "  value can be 'captured' to a shell variable using the backward\n"
       "  single quote operator; a trivial csh/tcsh example is\n"
       "\n"
       "    set ccc = `3dClipLevel -mfrac 0.333 Elvis+orig`\n"
       "    3dcalc -a Elvis+orig -expr \"step(a-$ccc)\" -prefix Presley\n"
       "\n"
       "Algorithm:\n"
       "  (a) Set some initial clip value using wizardry (AKA 'variance').\n"
       "  (b) Find the median of all positive values >= clip value.\n"
       "  (c) Set the clip value to 0.50 of this median.\n"
       "  (d) Loop back to (b) until the clip value doesn't change.\n"
       "This method was made up out of nothing, based on histogram gazing.\n"
       "\n"
       "Options:\n"
       "--------\n"
       "  -mfrac ff = Use the number ff instead of 0.50 in the algorithm.\n"
       "  -doall    = Apply the algorithm to each sub-brick separately.\n"
       "              [Cannot be combined with '-grad'!]\n"
       "\n"
       "  -grad ppp = In addition to using the 'one size fits all routine',\n"
       "              also compute a 'gradual' clip level as a function\n"
       "              of voxel position, and output that to a dataset with\n"
       "              prefix 'ppp'.\n"
       "             [This is the same 'gradual' clip level that is now the\n"
       "              default in 3dAutomask - as of 24 Oct 2006.\n"
       "              You can use this option to see how 3dAutomask clips\n"
       "              the dataset as its first step.  The algorithm above is\n"
       "              is used in each octant of the dataset, and then these\n"
       "              8 values are interpolated to cover the whole volume.]\n"
       "Notes:\n"
       "------\n"
       "* Use at your own risk!  You might want to use the AFNI Histogram\n"
       "    plugin to see if the results are reasonable.  This program is\n"
       "    likely to produce bad results on images gathered with local\n"
       "    RF coils, or with pulse sequences with unusual contrasts.\n"
       "\n"
       "* For brain images, most brain voxels seem to be in the range from\n"
       "    the clip level (mfrac=0.5) to about 3-3.5 times the clip level.\n"
       "    - In T1-weighted images, voxels above that level are usually\n"
       "      blood vessels (e.g., inflow artifact brightens them).\n"
       "\n"
       "* If the input dataset has more than 1 sub-brick, the data is\n"
       "    analyzed on the median volume -- at each voxel, the median\n"
       "    of all sub-bricks at that voxel is computed, and then this\n"
       "    median volume is used in the histogram algorithm.\n"
       "\n"
       "* If the input dataset is short- or byte-valued, the output will\n"
       "    be an integer; otherwise, the output is a float value.\n"
       "\n"
       "* Example -- Scaling a sequence of sub-bricks from a collection of\n"
       "             anatomicals from different sites to have about the\n"
       "             same numerical range (from 0 to 255):\n"
       "       3dTcat -prefix input anat_*+tlrc.HEAD\n"
       "       3dClipLevel -doall input+tlrc > clip.1D\n"
       "       3dcalc -datum byte -nscale -a input+tlrc -b clip.1D \\\n"
       "              -expr '255*max(0,min(1,a/(3.2*b)))' -verb -prefix scaled\n"
       "----------------------------------------------------------------------\n"
       "* Author: Emperor Zhark -- Sadistic Galactic Domination since 1994!\n\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dClipLevel main"); machdep(); AFNI_logger("3dCliplevel",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-doall") == 0 ){  /* 01 Nov 2010 */
       doall++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mfrac") == 0 || strcmp(argv[iarg],"-clfrac") == 0 ){
       mfrac = strtod( argv[++iarg] , NULL ) ;
       if( mfrac <= 0.0f ) ERROR_exit("Illegal -mfrac '%s'",argv[iarg]) ;
       while( mfrac >= 1.0f ) mfrac *= 0.01f ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-grad") == 0 ){
       gprefix = argv[++iarg] ;
       if( !THD_filename_ok(gprefix) ) ERROR_exit("Illegal -grad '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s\n",argv[iarg]) ;
   }

   /*-- read input dataset --*/

   dset = THD_open_dataset(argv[iarg]) ;
   CHECK_OPEN_ERROR(dset,argv[iarg]) ;
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   if( !doall ){

     /*-- get median at each voxel --*/

     medim = THD_median_brick( dset ) ;
     if( medim == NULL ) ERROR_exit("Can't load dataset '%s'",argv[iarg]);

     DSET_unload(dset) ;  /* no longer needed */

     val = THD_cliplevel( medim , mfrac ) ;  /* floating point clip level */

     if( !THD_need_brick_factor(dset) &&     /* convert to integer? */
         DSET_datum_constant(dset)    &&
        (DSET_BRICK_TYPE(dset,0)==MRI_short || DSET_BRICK_TYPE(dset,0)==MRI_byte) )
     val = (float)rint((double)val) ;

     printf("%g\n",val) ;        /***** write the output value! *****/

   /*--- 25 Oct 2006: create the gradual clip dataset? ---*/

     if( gprefix != NULL && val > 0.0f ){
       THD_3dim_dataset *gset ;
       MRI_IMAGE *gim ;
       gim = THD_cliplevel_gradual( medim , mfrac ) ;
       if( gim == NULL ) ERROR_exit("Can't compute gradual clip?!") ;
       gset = EDIT_empty_copy(dset) ;
       EDIT_dset_items( gset ,
                          ADN_nvals , 1 ,
                          ADN_ntt   , 0 ,
                          ADN_prefix, gprefix ,
                        ADN_none ) ;
       EDIT_substitute_brick( gset , 0 , MRI_float , MRI_FLOAT_PTR(gim) ) ;
       tross_Copy_History( dset , gset ) ;
       tross_Make_History( "3dClipLevel" , argc,argv , gset ) ;
       DSET_write(gset) ; DSET_delete(gset) ;
     }

   } else {  /* -doall [01 Nov 2010] */
     int nvals=DSET_NVALS(dset) , ii ; MRI_IMAGE *bim ;

     printf("# %s %d values\n",DSET_BRIKNAME(dset),nvals) ;
     for( ii=0 ; ii < nvals ; ii++ ){
       bim = THD_extract_float_brick(ii,dset) ; DSET_unload_one(dset,ii) ;
       val = THD_cliplevel( bim , mfrac ) ;  /* floating point clip level */
       printf("%g\n",val) ; mri_free(bim) ;
     }
   }

   exit(0) ;
}
