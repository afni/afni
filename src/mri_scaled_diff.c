#include "mrilib.h"

/*----------------------------------------------------------------------*/
/*! Return a measure of the difference between 2 images bim and nim,
    possibly with a mask to indicate which voxels to include.

    The difference is defined as dd below:

    ss = min sum  [ bim[i] - a * nim[i] ]**2
          a   i

    dd = sqrt( ss / #voxels )  i.e., the RMS difference.

    That is, we least squares fit image nim to bim by a scale factor,
    and the residual determins the difference.  The minimizing value of
    'a' is given by

    a = BN / NN  where BN = sum [ bim[i]*nim[i] ]
                       NN = sum [ nim[i]**2     ]

    so the difference is given by

    diff = BB - (BN)**2/NN  where BB = sum[ bim[i]**2 ]

    - A negative return value indicates an error.
    - If bim=0, then the return value will be 0.
    - If bim!=0 and nim=0, then the return value is computed with a=0
      (that is, it will be the RMS value of bim)
------------------------------------------------------------------------*/

float mri_scaled_diff( MRI_IMAGE *bim , MRI_IMAGE *nim , MRI_IMAGE *msk )
{
   int nvox , ii , nmask=0 ;
   MRI_IMAGE *fim , *gim ;
   float     *far , *gar , sdif , ff,gg,fg ;
   byte      *mar=NULL ;

ENTRY("mri_scaled_diff") ;

   if( bim == NULL || nim == NULL ) RETURN(-1.0f) ;

   nvox = bim->nvox ; if( nim->nvox != nvox ) RETURN(-1.0f) ;

   if( msk != NULL && msk->kind == MRI_byte && msk->nvox == nvox ){
     mar   = MRI_BYTE_PTR(msk) ;
     nmask = THD_countmask( nvox , mar ) ;
     if( nmask < 3 ){ nmask = 0 ; mar = NULL ; }
   }

   fim = (bim->kind != MRI_float) ? mri_to_float(bim) : bim ;
   gim = (nim->kind != MRI_float) ? mri_to_float(nim) : nim ;

   far = MRI_FLOAT_PTR(fim) ; gar = MRI_FLOAT_PTR(gim) ;

   ff = gg = fg = 0.0f ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( nmask == 0 || mar[ii] != 0 ){
       ff += far[ii] * far[ii] ;
       gg += gar[ii] * gar[ii] ;
       fg += far[ii] * gar[ii] ;
     }
   }
   if( gg > 0.0f ){          /* normal case  */
     sdif = ff - fg*fg/gg ;
     if( sdif > 0.0f )
       sdif = sqrt( sdif / ((nmask > 0) ? nmask : nvox) ) ;

   } else if( ff <= 0.0f )   /* far=gar=0 */
     sdif =  0.0f ;
   else                      /* gar=0 far!=0 */
     sdif =  sqrt( ff / ((nmask > 0) ? nmask : nvox) ) ;

   /** done **/

   if( fim != bim ) mri_free(fim) ;
   if( gim != nim ) mri_free(gim) ;
   RETURN(sdif) ;
}
