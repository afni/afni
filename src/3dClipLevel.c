#include "mrilib.h"

int main( int argc , char * argv[] )
{
   float mfrac=0.50 , fac ;
   double dsum ;
   int nvox , iarg=1 , *hist , ii,npos=0 , ncut,nmed,kk,ib , qq,nold ;
   THD_3dim_dataset * dset ;
   short * sar ;
   byte * bar ;
   int nhist , nneg=0 , verb=0 , nhalf , iv,nvals ;

   mainENTRY("3dClipLevel main") ; machdep() ;  /* 10 Aug 2001 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dClipLevel [options] dataset\n"
             "Estimates the value at which to clip the anatomical dataset so\n"
             "that background regions are set to zero.\n"
             "Method:\n"
             "  Find the median of all positive values >= clip value.\n"
             "  Set the clip value to 0.50 of this median.\n"
             "  Repeat until the clip value doesn't change.\n"
             "Options:\n"
             "  -mfrac ff = Use the number ff instead of 0.50 in the algorithm.\n"
             "  -verb     = The clip value is always printed to stdout.  If\n"
             "                this option is used to select verbose output,\n"
             "                progress reports are printed to stderr as well.\n"
             "\n"
             "N.B.: This program only works with byte- and short-valued\n"
             "        datasets, and prints a warning message if any input\n"
             "        voxels are negative.  If the dataset has more than one\n"
             "        sub-brick, all sub-bricks are used to build the histogram.\n"
             "N.B.: Use at your own risk!  You might want to use the AFNI Histogram\n"
             "        plugin to see if the results are reasonable.  This program is\n"
             "        likely to produce bad results on images gathered with local\n"
             "        RF coils, or with pulse sequences with unusual contrasts.\n"
             "\n"
             "A csh command line for the truly adventurous:\n"
             "  afni -dset \"v1:time+orig<`3dClipLevel 'v1:time+orig[4]'` .. 10000>\"\n"
             "(the dataset is from the 'sample96.tgz' data samples).  Can you\n"
             "figure out what this does?\n"
             "(Hint: each type of quote \"'` means something different to csh.)\n"
            ) ;
      exit(0) ;
   }

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-mfrac") == 0 ){
         mfrac = strtod( argv[++iarg] , NULL ) ;
         if( mfrac <= 0.0 ){ fprintf(stderr,"** ILLEGAL -mfrac\n");exit(1);}
         if( mfrac >= 1.0 ) mfrac *= 0.01 ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
         verb++ ; iarg++ ; continue ;
      }

      fprintf(stderr,"** ILLEGAL option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"** CAN'T open dataset\n");exit(1); }
   if( DSET_BRICK_TYPE(dset,0) != MRI_short && DSET_BRICK_TYPE(dset,0) != MRI_byte ){
      fprintf(stderr,"** ILLEGAL dataset type\n");exit(1);
   }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   nvals = DSET_NVALS(dset) ;

   /*-- allocate histogram --*/

   switch( DSET_BRICK_TYPE(dset,0) ){
      case MRI_short: nhist = 32767 ; break ;
      case MRI_byte : nhist =   255 ; break ;
   }

   hist = (int *) calloc(sizeof(int),nhist) ;
   nvox = DSET_NVOX(dset) ;

   /*-- make histogram --*/

   dsum = 0.0 ;
   for( iv=0 ; iv < nvals ; iv++ ){
      switch( DSET_BRICK_TYPE(dset,0) ){
         case MRI_short:
            sar =  DSET_ARRAY(dset,iv) ;
            for( ii=0 ; ii < nvox ; ii++ ){
               if( sar[ii] > 0 ){
                  hist[sar[ii]]++ ; dsum += (double)(sar[ii])*(double)(sar[ii]) ; npos++ ;
               } else if( sar[ii] < 0 )
                 nneg++ ;
            }
         break ;
   
         case MRI_byte:                       /* there are no negative bytes */
            bar =  DSET_ARRAY(dset,iv) ;
            for( ii=0 ; ii < nvox ; ii++ ){
               if( bar[ii] > 0 ){
                  hist[bar[ii]]++ ; dsum += (double)(bar[ii])*(double)(bar[ii]) ; npos++ ;
               }
            }
         break ;
      }
   }

   DSET_unload(dset) ;

   if( verb ) fprintf(stderr,"++ %d positive voxels\n",npos) ;

   if( npos <= 999 ){
      fprintf(stderr,"** TOO few positive voxel (%d)!\n",npos) ; exit(1) ;
   }
   if( nneg > 0 ){
      fprintf(stderr,"++ WARNING: ignored %d negative voxels!\n",nneg) ;
   }

   /*-- initialize cut position to include upper 65% of positive voxels --*/

   qq = 0.65 * npos ; ib = rint(0.5*sqrt(dsum/npos)) ;
   for( kk=0,ii=nhist-1 ; ii >= ib && kk < qq ; ii-- ) kk += hist[ii] ;

   /*-- algorithm --*/

   ncut = ii ; qq = 0 ;
   do{
      for( npos=0,ii=ncut ; ii < nhist ; ii++ ) npos += hist[ii] ; /* number >= cut */
      nhalf = npos/2 ;
      for( kk=0,ii=ncut ; ii < nhist && kk < nhalf ; ii++ )        /* find median */
         kk += hist[ii] ;
      if( verb )
         fprintf(stderr,"++ Clip=%d  Median=%d  Number>=Clip=%d\n",ncut,ii,npos) ;
      nold = ncut ;
      ncut = mfrac * ii ;                                          /* new cut */
      qq++ ;
   } while( qq < 20 && ncut != nold ) ;

   fac = DSET_BRICK_FACTOR(dset,0) ;
   if( fac > 0.0 )
      printf("%g\n",ncut*fac) ;
   else
      printf("%d\n",ncut) ;

   exit(0) ;
}
