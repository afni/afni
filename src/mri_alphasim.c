#include "mrilib.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>


MRI_IMAGE * mri_alphasim( int   nx, int   ny, int   nz ,
                          float dx, float dy, float dz ,
                          int niter , int max_clustsize , float rmm ,
                          int num_pval , float *pval ,
                          int num_fwhm , float *fwhm , byte *mask , long seed )
{
   MRI_IMAGE *bim , *tim , *aim , *cim , *dim ;
   float     *bar , *tar , *aar , *car , *dar ;
   int ite , jsm , kth , nxyz ;
   float *thr ; double p,q,z,mean,sd,bound ; int which,status ;
   static long sseed=0 ;

ENTRY("mri_alphasim") ;

   if( nx < 8 || ny < 8 || nz < 1 ) RETURN(NULL) ;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   if( niter < 1 ) niter = 1000 ;
   if( max_clustsize < 16 ) max_clustsize = 10000 ;

   if( num_pval < 1 || pval == NULL ){
     static float pp = 0.001f ;
     num_pval = 1 ; pval = &pp ;
   }
   thr = (float *)malloc(sizeof(float)*num_pval) ;

   if( num_fwhm < 1 || fwhm == NULL ){
     static float ff = 0.0f ;
     num_fwhm = 1 ; fwhm = &ff ;
   }

   bim = mri_new_vol( nx,ny,nz , MRI_FLOAT ) ; bar = MRI_FLOAT_PTR(bim) ;
   tim = mri_new_vol( nx,ny,nz , MRI_FLOAT ) ; tar = MRI_FLOAT_PTR(tim) ;

   aim = mri_new_vol( max_clustize , num_fwhm , num_pval , MRI_FLOAT ) ;
   aar = MRI_FLOAT_PTR(aim) ;
#undef  ALP
#define ALP(c,s,t) aar[c+s*num_fwhm+t*(num_fwhm*num_pval)]

   nxyz = nx*ny*nz ;

   if( seed != 0 ){
     srand48(seed) ;
   } else if( sseed == 0 ){
     sseed = (long)(time(NULL)) + (long)((long)getpid()) ;
     srand48(sseed) ;
   }

   /*-- iteration loop --*/

   for( ite=0 ; ite < niter ; ite++ ){

     /*-- create uncorrelated random field --*/

#undef  TPI
#define TPI 6.283185f

     for( ii=0 ; ii < nxyz ; ii+=2 ){
       do{ u1 = (float)drand48(); } while( u1==0.0f ) ;
       u1 = sqrtf(-2.0f*logf(u1)) ;
       u2 = TPI * (float)drand48() ;
       bar[ii] = u1 * cosf(u2) ; bar[ii+1] = u1 * sinf(u2) ;
     }
     if( ii == nxyz-1 ){
       do{ u1 = (float)drand48(); } while( u1==0.0f ) ;
       u1 = sqrtf(-2.0f*logf(u1)) ;
       u2 = TPI * (float)drand48() ;
       bar[ii] = u1 * cosf(u2) ;
     }

     /*-- loop over smoothings --*/

     for( jsm=0 ; jsm < num_fwhm ; jsm++ ){

       /* blur dataset */

       cim = mri_copy(bim) ; car = MRI_FLOAT_PTR(cim) ;
       if( fwhm[jsm] > 0.0f )
         EDIT_blur_volume( nx,ny,nz , dx,dy,dz , MRI_FLOAT,car , fwhm[jsm] ) ;

       /* find sigma of blurred dataset (we know the mean is zero) */

       sd = 0.0 ;
       for( ii=0 ; ii < nxyz ; ii++ ) sd += car[ii]*car[ii] ;
       sd = sqrt(sd/nxyz) ;

       /* find thresholds for p-values in blurred dataset */

       which = 2 ;
       mean  = 0.0 ;
       for( kth=0 ; kth < num_pval ; kth++ ){
         p = 1.0 - pval[kth] ;
         q =       pval[kth] ;
         cdfnor( &which , &p , &q , &z , &mean , &sd , &status , &bound ) ;
         thr[kth] = (float)z ;
       }

       /* mask blurred dataset */

       if( mask != NULL )
         for( ii=0 ; ii < nxyz ; ii++ ) if( mask[ii] == 0 ) car[ii] = 0.0f ;

       /*-- loop over per-voxel thresholds --*/

       for( kth=0 ; kth < num_pval ; kth++ ){

          dim = mri_copy(cim) ; dar = MRI_FLOAT_PTR(dim) ;

          /* threshold at thr[kth] */

          for( ii=0 ; ii < nxyz ; ii++ ) if( dar[ii] < tt ) dar[ii] = 0.0f ;

          /* clusterize and count into aar[ csize + jsm*num_fwhm + kth*num_fwhm*num_pval ] */

       } /* end of loop over thresholds */

     } /* end of loop over smoothings */

   } /* end of iterations */

   /* normalize aar[] to be alpha instead of counts */

   free(tim) ; free(bim) ;
   RETURN(aim) ;
}
