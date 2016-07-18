#include "mrilib.h"

/*--------------------------------------------------------------------------*/

float mri_topclip( MRI_IMAGE *im )  /* 28 Sep 2006 */
{
   float cv , dv ;
ENTRY("mri_topclip") ;
   cv = 3.11f * THD_cliplevel( im , 0.511f ) ;
   dv = (float)mri_max( im ) ;
   cv = MIN(cv,dv) ; RETURN(cv) ;
}

/*--------------------------------------------------------------------------
   12 Aug 2001: compare with 3dClipLevel.c
   - compute a clipping level for an image, to eliminate non-brain voxels
   - negative voxels are ignored
   - 05 Nov 2001: increased size of hist array to nhist+1 (from nhist), to
                  store properly elements [0..nhist] (d'oh).
----------------------------------------------------------------------------*/

float THD_cliplevel( MRI_IMAGE *im , float mfrac )
{
   MRI_IMAGE *lim ;
   double fac , sfac=1.0 , dsum ;
   int nvox , *hist , ii,npos=0 , ncut,kk,ib , qq,nold ;
   short *sar ;
   byte  *bar ;
   int nhist , nneg=0 , nhalf ;

ENTRY("THD_cliplevel") ;
   if( im == NULL ) RETURN(0.0f) ;

   if( mfrac <= 0.0f || mfrac >= 0.99f ) mfrac = 0.50f ;

   /*-- allocate histogram --*/

   switch( im->kind ){
      case MRI_short: nhist = 32767 ; lim = im ; break ;
      case MRI_byte : nhist =   255 ; lim = im ; break ;
      case MRI_float: nhist = 10000 ; lim = im ; break ; /* 20 Dec 2006 */

      default:
        if( im->kind == MRI_rgb ){
          nhist = 255 ;
        } else {
          fac = mri_maxabs(im) ; if( fac < 1.0e-100 ) RETURN(0.0f) ;
          sfac = 32767.0/fac ; nhist = 32767 ;
        }
        lim = mri_to_short( sfac , im ) ;
      break ;
   }

   hist = (int *) calloc(sizeof(int),nhist+1) ;  /* 05 Nov 2001: +1 */
   nvox = lim->nvox ;

   /*-- make histogram --*/

   dsum = 0.0 ;
   switch( lim->kind ){
      default: break ;

      case MRI_float:{   /* 20 Dec 2006: do the float->int conversion inline */
        float *far = MRI_FLOAT_PTR(lim) ;
        fac = mri_max(im) ; if( fac < 1.e-100 ){ free(hist); RETURN(0.0f); }
        sfac = nhist / fac ;
        for( ii=0 ; ii < nvox ; ii++ ){
          if( far[ii] > 0.0f ){
            kk = (int)(sfac*far[ii]+0.499) ;
            if( kk <= nhist ){
              hist[kk]++ ;
              dsum += ((double)kk) * ((double)(kk)) ; npos++ ;
            }
          }
        }
      }
      break ;

      case MRI_short:
         sar = MRI_SHORT_PTR(lim) ;
         for( ii=0 ; ii < nvox ; ii++ ){
            if( sar[ii] > 0 && sar[ii] <= nhist ){
               hist[sar[ii]]++ ;
               dsum += (double)(sar[ii])*(double)(sar[ii]); npos++;
            } else if( sar[ii] < 0 )
              nneg++ ;
         }
      break ;

      case MRI_byte:                       /* there are no negative bytes */
         bar = MRI_BYTE_PTR(lim) ;
         for( ii=0 ; ii < nvox ; ii++ ){
            if( bar[ii] > 0 ){
               hist[bar[ii]]++ ;
               dsum += (double)(bar[ii])*(double)(bar[ii]); npos++;
            }
         }
      break ;
   }

   if( lim != im ) mri_free(lim) ;

   if( npos <= 222 ){ free(hist); RETURN(0.0f); }

   /*-- initialize cut position to include upper 65% of positive voxels --*/

   qq = 0.65f * npos ; ib = rint(0.5*sqrt(dsum/npos)) ;
   for( kk=0,ii=nhist-1 ; ii >= ib && kk < qq ; ii-- ) kk += hist[ii] ;

   /*-- median adjustment algorithm:
        we find a cut level so that it equals mfrac times
        the median of all the values above the cut level. ---*/

   ncut = ii ; qq = 0 ;  /* qq is iteration count */
   do{
      for( npos=0,ii=ncut; ii < nhist; ii++ ) npos += hist[ii]; /* num >= cut */
      nhalf = npos/2 ;                              /* half the number >= cut */
      for( kk=0,ii=ncut ; ii < nhist && kk < nhalf ; ii++ )    /* find median */
         kk += hist[ii] ;                       /* loop ends at ii=median bin */
      nold = ncut ;
      ncut = mfrac * ii ;                                          /* new cut */
      qq++ ;
   } while( qq < 66 && ncut != nold ) ; /* usually only 2-3 iterations needed */

   free(hist) ;

   fac = ncut / sfac ;
   if( fac > 1.e+38 ) fac = 1.e+38 ;   
   RETURN( (float)fac ) ;
}

/*-------------------------------------------------------------------------*/
/* (1) apply THD_cliplevel() algorithm above to absolute values.
   (2) also, if mfrac < 0, then find 90% point on CDF of absolute values
       and return smaller of this or the THD_cliplevel() result.
   Purpose is to find some reasonable place to threshold an image for
   visual effect only.
---------------------------------------------------------------------------*/

float THD_cliplevel_abs( MRI_IMAGE *im , float mfrac )
{
   MRI_IMAGE *fim ;
   register float *far ;
   register int ii ;
   float val,tv ; int dotwo=0 ;

ENTRY("THD_cliplevel_abs") ;
   if( im == NULL ) RETURN(0.0f) ;
   fim = mri_to_float(im) ; if( fim == NULL ) RETURN(0.0f) ;
   far = MRI_FLOAT_PTR(fim) ;
   for( ii=0 ; ii < fim->nvox ; ii++ ) far[ii] = fabsf(far[ii]) ;
   if( mfrac < 0.0f ){ dotwo = 1; mfrac = -mfrac; }
   val = THD_cliplevel( fim , mfrac ) ;

   if( dotwo ){
     qsort_float( fim->nvox , far ) ;
     ii = (int)(0.9*fim->nvox) ; tv = far[ii] ;
     if( tv == 0.0f ){
       for( ; ii < fim->nvox && far[ii] == 0.0f ; ii++ ) ; /* nada */
       if( ii < fim->nvox ) tv = far[ii] ;
     }
     if( val > tv && tv > 0.0f ) val = tv ;
   }

   mri_free(fim) ; RETURN(val) ;
}

/*-------------------------------------------------------------------------*/
/*! Cliplevel for part of an image.  Quick and easy to write!
    Not very efficient, but this isn't a big CPU sink.  [24 Oct 2006] */

float THD_cliplevel_partial( MRI_IMAGE *im , float mfrac ,
                             int xa,int xb, int ya,int yb, int za,int zb )
{
   MRI_IMAGE *qim ; float val ;

ENTRY("THD_cliplevel_partial") ;
   qim = mri_cut_3D( im , xa,xb , ya,yb , za,zb ) ;
   val = THD_cliplevel( qim , mfrac ) ;
   mri_free(qim) ; RETURN(val) ;
}

/*-------------------------------------------------------------------------*/

typedef struct {
   float clip_000, clip_100, clip_010, clip_110,
         clip_001, clip_101, clip_011, clip_111 ;
   float x0,x1,dxi , y0,y1,dyi , z0,z1,dzi ;
   float clip_min , clip_max ;
} clipvec ;

/*-------------------------------------------------------------------------*/
/*! Get cliplevel for each octant about the center-of-mass. [24 Oct 2006]  */

static clipvec get_octant_clips( MRI_IMAGE *im , float mfrac )
{
   float xcm,ycm,zcm , sum,val , clip_min , clip_max ;
   int ii,jj,kk , nx,ny,nz , ic,jc,kc , it,jt,kt , ijk ;
   int icp,icm , jcp,jcm , kcp,kcm ;
   clipvec cv ;

ENTRY("get_octant_clips") ;

   cv.clip_000 = -1 ;  /* flags error return */

   if( im == NULL ) RETURN(cv) ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ;
   it = nx-1   ; jt = ny-1   ; kt = nz-1   ;

   /* compute CM of image */

   mri_get_cmass_3D( im, &xcm, &ycm, &zcm ) ;

   ic = (int)rint(xcm); jc = (int)rint(ycm); kc = (int)rint(zcm);

   /* compute cliplevel in each octant about the CM */

   val = 0.333f * THD_cliplevel( im,mfrac ) ;

   ii = (int)rint(0.01*nx); if( ii < 1 ) ii = 1 ;  /* 1% quadrant overlap */
   jj = (int)rint(0.01*ny); if( jj < 1 ) jj = 1 ;
   kk = (int)rint(0.01*nz); if( kk < 1 ) kk = 1 ;
   icm = ic-ii ; icp = ic+ii ; if( icm < 0 ) icm = 0; if( icp > it ) icp = it ;
   jcm = jc-jj ; jcp = jc+jj ; if( jcm < 0 ) jcm = 0; if( jcp > jt ) jcp = jt ;
   kcm = kc-kk ; kcp = kc+kk ; if( kcm < 0 ) kcm = 0; if( kcp > kt ) kcp = kt ;

   cv.clip_000 = THD_cliplevel_partial( im,mfrac,  0 ,icp,  0 ,jcp,  0 ,kcp );
   cv.clip_100 = THD_cliplevel_partial( im,mfrac, icm,it ,  0 ,jcp,  0 ,kcp );
   cv.clip_010 = THD_cliplevel_partial( im,mfrac,  0 ,icp, jcm,jt ,  0 ,kcp );
   cv.clip_110 = THD_cliplevel_partial( im,mfrac, icm,it , jcm,jt ,  0 ,kcp );
   cv.clip_001 = THD_cliplevel_partial( im,mfrac,  0 ,icp,  0 ,jcp, kcm,kt  );
   cv.clip_101 = THD_cliplevel_partial( im,mfrac, icm,it ,  0 ,jcp, kcm,kt  );
   cv.clip_011 = THD_cliplevel_partial( im,mfrac,  0 ,icp, jcm,jt , kcm,kt  );
   cv.clip_111 = THD_cliplevel_partial( im,mfrac, icm,it , jcm,jt , kcm,kt  );

   if( cv.clip_000 < val ) cv.clip_000 = val ;  /* don't let   */
   if( cv.clip_100 < val ) cv.clip_100 = val ;  /* clip levels */
   if( cv.clip_010 < val ) cv.clip_010 = val ;  /* get too     */
   if( cv.clip_110 < val ) cv.clip_110 = val ;  /* small!      */
   if( cv.clip_001 < val ) cv.clip_001 = val ;
   if( cv.clip_101 < val ) cv.clip_101 = val ;
   if( cv.clip_011 < val ) cv.clip_011 = val ;
   if( cv.clip_111 < val ) cv.clip_111 = val ;

   clip_min =              cv.clip_000  ;
   clip_min = MIN(clip_min,cv.clip_100) ;
   clip_min = MIN(clip_min,cv.clip_010) ;
   clip_min = MIN(clip_min,cv.clip_110) ;
   clip_min = MIN(clip_min,cv.clip_001) ;
   clip_min = MIN(clip_min,cv.clip_101) ;
   clip_min = MIN(clip_min,cv.clip_011) ;
   clip_min = MIN(clip_min,cv.clip_111) ;  cv.clip_min = clip_min ;

   clip_max =              cv.clip_000  ;
   clip_max = MAX(clip_max,cv.clip_100) ;
   clip_max = MAX(clip_max,cv.clip_010) ;
   clip_max = MAX(clip_max,cv.clip_110) ;
   clip_max = MAX(clip_max,cv.clip_001) ;
   clip_max = MAX(clip_max,cv.clip_101) ;
   clip_max = MAX(clip_max,cv.clip_011) ;
   clip_max = MAX(clip_max,cv.clip_111) ;  cv.clip_max = clip_max ;

   /* (x0,y0,z0) = center of lowest octant
      (x1,y1,z1) = center of highest octant */

   cv.x0  = 0.5*ic ; cv.x1 = 0.5*(ic+it) ;
   cv.y0  = 0.5*jc ; cv.y1 = 0.5*(jc+jt) ;
   cv.z0  = 0.5*kc ; cv.z1 = 0.5*(kc+kt) ;
   cv.dxi = (cv.x1 > cv.x0) ? 1.0/(cv.x1-cv.x0) : 0.0 ;
   cv.dyi = (cv.y1 > cv.y0) ? 1.0/(cv.y1-cv.y0) : 0.0 ;
   cv.dzi = (cv.z1 > cv.z0) ? 1.0/(cv.z1-cv.z0) : 0.0 ;

   RETURN(cv) ;
}

/*--------------------------------------------------------------------------*/
/*! Return the cliplevel at any point,
    tri-linearly interpolated between octant centers. [24 Oct 2006] */

static INLINE float pointclip( int ii, int jj, int kk , clipvec *cv )
{
   float x1,y1,z1 , x0,y0,z0 , val ;

   /* get relative position in box defined by octant centers */

   x1 = (ii-cv->x0)*cv->dxi; if(x1 < 0.0f) x1=0.0f; else if(x1 > 1.0f) x1=1.0f;
   y1 = (jj-cv->y0)*cv->dyi; if(y1 < 0.0f) y1=0.0f; else if(y1 > 1.0f) y1=1.0f;
   z1 = (kk-cv->z0)*cv->dzi; if(z1 < 0.0f) z1=0.0f; else if(z1 > 1.0f) z1=1.0f;

   x0 = 1.0f-x1 ; y0 = 1.0f-y1 ; z0 = 1.0f-z1 ;

   val =  cv->clip_000 * x0*y0*z0 + cv->clip_100 * x1*y0*z0
        + cv->clip_010 * x0*y1*z0 + cv->clip_110 * x1*y1*z0
        + cv->clip_001 * x0*y0*z1 + cv->clip_101 * x1*y0*z1
        + cv->clip_011 * x0*y1*z1 + cv->clip_111 * x1*y1*z1 ;
   return val ;
}

/*--------------------------------------------------------------------------*/
/*! Return a cliplevel that varies gradually across the image.
    At this time [24 Oct 2006], the clipping varies trilinearly
    between octants.  A fancier method would use higher order
    polynomials, but I can't really be bothered with that now.
----------------------------------------------------------------------------*/

MRI_IMAGE * THD_cliplevel_gradual( MRI_IMAGE *im , float mfrac )
{
   int ii,jj,kk,ijk , nx,ny,nz ;
   MRI_IMAGE *cim ; float *car ;
   clipvec bvec ;

ENTRY("THD_cliplevel_gradual") ;
   if( im == NULL ) RETURN(NULL) ;

   bvec = get_octant_clips( im , mfrac ) ;
   if( bvec.clip_000 < 0.0 ) RETURN(NULL) ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ;

   cim = mri_new_conforming( im , MRI_float ) ;
   car = MRI_FLOAT_PTR(cim) ;

   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,ijk++ ){
       car[ijk] = pointclip( ii,jj,kk , &bvec ) ; /* cliplevel here */
   }}}

   RETURN(cim) ;
}

/*--------------------------------------------------------------------------*/

float THD_cliplevel_search( MRI_IMAGE *im )  /* experimental */
{
#define NC 10
#define DC 0.05f
#define CB 0.10f
   int qc , nmask[NC] ; float cc ; byte *mask ;

   THD_automask_verbose(0) ;
INFO_message("\nTHD_cliplevel_search:") ;
   for( qc=0 ; qc < NC ; qc++ ){
     cc = DC * qc + CB ;
     THD_automask_set_clipfrac(cc) ;
     THD_automask_set_cheapo(1) ;
     mask = mri_automask_image(im) ;
     nmask[qc] = THD_countmask( im->nvox , mask ) ;
     free(mask) ;
ININFO_message("  clfrac=%.2f nmask=%d (%.1f%%)",cc,nmask[qc],(100.0f*nmask[qc])/(float)(im->nvox)) ;
   }
   THD_automask_set_cheapo(0) ;
   return 0.0f ;
}
