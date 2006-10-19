#include "mrilib.h"

int main( int argc , char * argv[] )
{
   float mfrac=0.50f , val ;
   MRI_IMAGE *medim ;
   THD_3dim_dataset *dset ;
   int iarg=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dClipLevel [options] dataset\n"
             "Estimates the value at which to clip the anatomical dataset so\n"
             "  that background regions are set to zero.\n"
             "\n"
             "The program's output is a single number sent to stdout.  This\n"
             "  value can be 'captured' to a shell variable using the backward\n"
             "  single quote operator; a trivial csh/tcsh example is\n"
             "\n"
             "    set ccc = `3dClipLevel -mfrac 0.333 Elvis+orig`\n"
             "    3dcalc -a Elvis+orig -expr \"a-$ccc\" -prefix Presley\n"
             "\n"
             "Algorithm:\n"
             "  Find the median of all positive values >= clip value.\n"
             "  Set the clip value to 0.50 of this median.\n"
             "  Repeat until the clip value doesn't change.\n"
             "\n"
             "Options:\n"
             "  -mfrac ff = Use the number ff instead of 0.50 in the algorithm.\n"
             "\n"
             "N.B.: Use at your own risk!  You might want to use the AFNI Histogram\n"
             "        plugin to see if the results are reasonable.  This program is\n"
             "        likely to produce bad results on images gathered with local\n"
             "        RF coils, or with pulse sequences with unusual contrasts.\n"
             "\n"
             "N.B.: For brain images, most brain voxels seem to be in the range\n"
             "        from the clip level to about 3 times the clip level.\n"
             "\n"
             "N.B.: If the input dataset has more than 1 sub-brick, the data is\n"
             "        analyzed on the median volume -- at each voxel, the median\n"
             "        of all sub-bricks at that voxel is computed, and then this\n"
             "        median volume is used in the histogram algorithm.\n"
             "\n"
             "A shell command line for the truly adventurous:\n"
             "  afni -dset \"v1:time+orig<`3dClipLevel 'Grot+orig[4]'` .. 10000>\"\n"
             "Can you figure out what this does?\n"
             "(Hint: each type of quote \"'` means something different to the shell.)\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dClipLevel main"); machdep(); AFNI_logger("3dCliplevel",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-mfrac") == 0 || strcmp(argv[iarg],"-clfrac") == 0 ){
         mfrac = strtod( argv[++iarg] , NULL ) ;
         if( mfrac <= 0.0f ) ERROR_exit("Illegal -mfrac '%s'",argv[iarg]) ;
         while( mfrac >= 1.0f ) mfrac *= 0.01f ;
         iarg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s\n",argv[iarg]) ;
   }

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ) ERROR_exit("Can't open dataset %s",argv[iarg]) ; 

   medim = THD_median_brick( dset ) ;
   if( medim == NULL )       ERROR_exit("Can't load dataset %s",argv[iarg]) ;

   DSET_unload(dset) ;

   val = THD_cliplevel( medim , mfrac ) ;

   mri_free(medim) ;

   if( !THD_need_brick_factor(dset) &&
       DSET_datum_constant(dset)    &&
      (DSET_BRICK_TYPE(dset,0)==MRI_short || DSET_BRICK_TYPE(dset,0)==MRI_byte) )
     val = (float)rint((double)val) ;

   printf("%g\n",val) ;
   exit(0) ;
}
