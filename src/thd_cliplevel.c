#include "mrilib.h"

/*--------------------------------------------------------------------------
   12 Aug 2001: compare with 3dClipLevel.c
   - compute a clipping level for an image, to eliminate non-brain voxels
   05 Nov 2001: increased size of hist array to nhist+1 (from nhist), to
                store properly elements [0..nhist] (d'oh).
----------------------------------------------------------------------------*/

float THD_cliplevel( MRI_IMAGE *im , float mfrac )
{
   MRI_IMAGE *lim ;
   float fac , sfac=1.0 ;
   double dsum ;
   int nvox , *hist , ii,npos=0 , ncut,kk,ib , qq,nold ;
   short *sar ;
   byte  *bar ;
   int nhist , nneg=0 , nhalf ;

ENTRY("THD_cliplevel") ;
   if( im == NULL ) RETURN(0.0) ;

   if( mfrac <= 0.0 || mfrac >= 0.99 ) mfrac = 0.50 ;

   /*-- allocate histogram --*/

   switch( im->kind ){
      case MRI_short: nhist = 32767 ; lim = im ; break ;
      case MRI_byte : nhist =   255 ; lim = im ; break ;

      default:
        fac = mri_maxabs(im) ; if( fac == 0.0 ) RETURN(0.0) ;
        sfac = 32767.0/fac ; nhist = 32767 ;
        lim = mri_to_short( sfac , im ) ;
      break ;
   }

   hist = (int *) calloc(sizeof(int),nhist+1) ;  /* 05 Nov 2001: +1 */
   nvox = im->nvox ;

   /*-- make histogram --*/

   dsum = 0.0 ;
   switch( lim->kind ){
      default: break ;

      case MRI_short:
         sar =  MRI_SHORT_PTR(lim) ;
         for( ii=0 ; ii < nvox ; ii++ ){
            if( sar[ii] > 0 && sar[ii] <= nhist ){
               hist[sar[ii]]++ ;
               dsum += (double)(sar[ii])*(double)(sar[ii]); npos++;
            } else if( sar[ii] < 0 )
              nneg++ ;
         }
      break ;

      case MRI_byte:                       /* there are no negative bytes */
         bar =  MRI_BYTE_PTR(lim) ;
         for( ii=0 ; ii < nvox ; ii++ ){
            if( bar[ii] > 0 ){
               hist[bar[ii]]++ ;
               dsum += (double)(bar[ii])*(double)(bar[ii]); npos++;
            }
         }
      break ;
   }

   if( npos <= 999 ){ free(hist); if(lim!=im)mri_free(lim); RETURN(0.0); }

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
      nold = ncut ;
      ncut = mfrac * ii ;                                          /* new cut */
      qq++ ;
   } while( qq < 20 && ncut != nold ) ;

   free(hist) ; if( lim != im ) mri_free(lim) ;

   RETURN( (ncut/sfac) ) ;
}
