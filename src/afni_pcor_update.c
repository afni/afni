/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*********************************************************************
   These are the template routines to form functional images
   recursively from various data types.

   To create actual routines for this purpose, you must compile
   the file with the preprocessor symbol DTYPE set to one of
   the following types:

      byte short int float

      cc -c -DDTYPE=short afni_pcor.c
      mv -f afni_pcor.o afni_pcor_short.o

   In the example above, the resulting routine will be named
   PCOR_update_short and will take as input a "short *"
   (plus the other stuff, which isn't DTYPE dependent).
**********************************************************************/

#ifndef DTYPE
#error "Cannot compile, since DTYPE is undefined."
#endif

#undef MAIN
#include "afni_pcor.h"

/** macros for function names defined in this file **/

#define PCOR_UPDATE TWO_TWO(PCOR_update_,DTYPE)

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** update all voxels with a new array of data
      inputs:  vdata = array[nvox] of new data for each voxel
               ref   = pointer to references structure to use
               vc    = pointer to correlation data structure
      output:  updated vc
***/

void PCOR_UPDATE( DTYPE * vdata , PCOR_references * ref , PCOR_voxel_corr * vc )
{
   int vox , jj ,       /* loop indices */
       nv = vc->nvox ,  /* number of voxels */
       nr = vc->nref ;  /* number of references */

   float *aaa = ref->alp ,
         *fff = ref->ff  ,
         *ggg = ref->gg  ;

   float zz , bq = ref->betasq ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "PCOR_UPDATE: reference size mismatch!\n" ) ;
      exit(1) ;
   }

/**----------------------------------------------------------------------
    innermost loop expansion is for speedup if nref is small, if enabled
      UPZZ updates zz for each row element  > This pair is performed for
      UPCH updates the Cholesky row element > each non-diagonal element
      UPLL updates the Cholesky diagonal element
-------------------------------------------------------------------------**/

#ifdef EXPAND_UPDATE
#  define UPZZ(j)  zz -= aaa[j] * VCH(vc,vox,j)
#  define UPCH(j)  VCH(vc,vox,j) = fff[j] * VCH(vc,vox,j) + ggg[j] * zz
#  define UPLL(j)  VCH(vc,vox,j) += bq * zz * zz

   switch( nr ){
   default:       /*** generic case: nested loops ***/
#endif

   /*** for each voxel ***/

   for( vox=0 ; vox < nv ; vox++ ){

      /*** update last row of each Cholesky factor ***/

      zz = (float) vdata[vox] ;
      for( jj=0 ; jj < nr ; jj++ ){
         zz            -= aaa[jj] * VCH(vc,vox,jj) ;
         VCH(vc,vox,jj) = fff[jj] * VCH(vc,vox,jj) + ggg[jj] * zz ;
      }
      VCH(vc,vox,nr) += bq * zz * zz ; /* square of true Cholesky diagonal */
   }

#ifdef EXPAND_UPDATE
   break ;

   /***------------------------------------------------------------
     Below here are the special cases for 1-9 reference waveforms:
        The above loop over jj is just unrolled manually
        (using the UP?? macros) in order to gain speed.
   ----------------------------------------------------------------***/

   case 1:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ; UPLL(1) ;
      }
   break ;

   case 2:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ; UPLL(2) ;
      }
   break ;

   case 3:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ; UPLL(3) ;
   }
   break ;

   case 4:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ; UPLL(4) ;
   }
   break ;

   case 5:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ; UPLL(5) ;
   }
   break ;

   case 6:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ; UPLL(6) ;
   }
   break ;

   case 7:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ;
         UPZZ(6) ; UPCH(6) ; UPLL(7) ;
   }
   break ;

   case 8:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ;
         UPZZ(6) ; UPCH(6) ;
         UPZZ(7) ; UPCH(7) ; UPLL(8) ;
   }
   break ;

   case 9:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ;
         UPZZ(6) ; UPCH(6) ;
         UPZZ(7) ; UPCH(7) ;
         UPZZ(8) ; UPCH(8) ; UPLL(9) ;
   }
   break ;

   }
#endif /* EXPAND_UPDATE */

   (vc->nupdate)++ ;  /* June 1995: another update completed! */
   return ;
}
