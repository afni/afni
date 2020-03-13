#include "mrilib.h"

/*********************************************************************
  * Wrapper functions to interpolate images in various ways.
  * Built on top of functions in mri_genalign_util (thus the GA_).
  * Could be optimized for various sub-cases, but is that
    really worth the effort?
*********************************************************************/

/*---------------------------------------------------------------------------*/

void mri_interp_scalar_to_floats_pointset( MRI_IMAGE *fim,
                                           int npp, float *ip, float *jp, float *kp,
                                           int code, float *vv )
{
   MRI_IMAGE *inim=fim ;

ENTRY("mri_interp_scalar_to_floats_pointset") ;

   if( inim == NULL || npp <= 0    ||
       ip   == NULL || jp  == NULL || kp == NULL || vv == NULL ){
     ERROR_message("NULL inputs to mri_interp_scalar_to_floats_pointset()") ;
     EXRETURN ;
   }

   /* convert to float? */

   if( inim->kind != MRI_float ) inim = mri_to_float(fim) ;

   switch( code ){

     case MRI_NN:
       GA_interp_NN     ( inim , npp,ip,jp,kp , vv ) ;
     break ;

     case MRI_LINEAR:
       GA_interp_linear ( inim , npp,ip,jp,kp , vv ) ;
     break ;

     case MRI_CUBIC:
       GA_interp_cubic  ( inim , npp,ip,jp,kp , vv ) ;
     break ;

     case MRI_QUINTIC:
       GA_interp_quintic( inim , npp,ip,jp,kp , vv ) ;
     break ;

     case MRI_WSINC5:
       GA_interp_wsinc5 ( inim , npp,ip,jp,kp , vv ) ;
     break ;

     default:
       ERROR_message("unknown interp code = %d in mri_interp_to_same_pointset()",code) ;

   }

   if( inim != fim ) mri_free(inim) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void mri_interp_to_same_pointset( MRI_IMAGE *fim,
                                  int npp, float *ip, float *jp, float *kp,
                                  int code, void *vv )
{
   register int ii ;

ENTRY("mri_interp_to_same_pointset") ;

   if( fim == NULL || npp <= 0    ||
       ip  == NULL || jp  == NULL || kp == NULL || vv == NULL ){
     ERROR_message("NULL inputs to mri_interp_to_same_pointset()") ;
     EXRETURN ;
   }

   switch( fim->kind ){

     case MRI_float:
       mri_interp_scalar_to_floats_pointset( fim,
                                             npp, ip,jp,kp, code,(float *)vv ) ;
     break ;

     case MRI_int:{
       int *pp = (int *)vv ;
       float *ff = (float *)malloc(sizeof(float)*npp) ;
       mri_interp_scalar_to_floats_pointset( fim ,
                                             npp , ip,jp,kp , code,ff ) ;
       for( ii=0 ; ii < npp ; ii++ ) pp[ii] = (int)rintf(ff[ii]) ;
       free(ff) ;
     }
     break ;

     case MRI_short:{
       short *pp = (short *)vv ;
       float *ff = (float *)malloc(sizeof(float)*npp) ;
       mri_interp_scalar_to_floats_pointset( fim ,
                                             npp , ip,jp,kp , code,ff ) ;
       for( ii=0 ; ii < npp ; ii++ ) pp[ii] = SHORTIZE(ff[ii]) ;
       free(ff) ;
     }
     break ;

     case MRI_byte:{
       byte *pp = (byte *)vv ;
       float *ff = (float *)malloc(sizeof(float)*npp) ;
       mri_interp_scalar_to_floats_pointset( fim ,
                                             npp , ip,jp,kp , code,ff ) ;
       for( ii=0 ; ii < npp ; ii++ ) pp[ii] = BYTEIZE(ff[ii]) ;
       free(ff) ;
     }
     break ;

     case MRI_rgb:{
       byte *pval = (byte *)vv ;
       MRI_IMARR *ppar ;
       float *rr = (float *)malloc(sizeof(float)*npp) ;
       float *gg = (float *)malloc(sizeof(float)*npp) ;
       float *bb = (float *)malloc(sizeof(float)*npp) ;
       ppar = mri_rgb_to_3float(fim) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,0) ,
                                             npp , ip,jp,kp , code, rr ) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,1) ,
                                             npp , ip,jp,kp , code, gg ) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,2) ,
                                             npp , ip,jp,kp , code, bb ) ;
       for( ii=0 ; ii < npp ; ii++ ){
         pval[3*ii+0] = BYTEIZE(rr[ii]) ;
         pval[3*ii+1] = BYTEIZE(gg[ii]) ;
         pval[3*ii+2] = BYTEIZE(bb[ii]) ;
       }
       free(bb) ; free(gg) ; free(rr) ;
     }
     break ;

     case MRI_rgba:{
       rgba *pval = (rgba *)vv ;
       MRI_IMARR *ppar ;
       float *rr = (float *)malloc(sizeof(float)*npp) ;
       float *gg = (float *)malloc(sizeof(float)*npp) ;
       float *bb = (float *)malloc(sizeof(float)*npp) ;
       float *aa = (float *)malloc(sizeof(float)*npp) ;
       ppar = mri_rgba_to_4float(fim) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,0) ,
                                             npp , ip,jp,kp , code, rr ) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,1) ,
                                             npp , ip,jp,kp , code, gg ) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,2) ,
                                             npp , ip,jp,kp , code, bb ) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,3) ,
                                             npp , ip,jp,kp , code, aa ) ;
       for( ii=0 ; ii < npp ; ii++ ){
         pval[ii].r = BYTEIZE(rr[ii]) ;
         pval[ii].g = BYTEIZE(gg[ii]) ;
         pval[ii].b = BYTEIZE(bb[ii]) ;
         pval[ii].a = BYTEIZE(rr[ii]) ;
       }
       free(aa) ; free(bb) ; free(gg) ; free(rr) ;
     }
     break ;

     case MRI_complex:{
       complex *pval = (complex *)vv ;
       MRI_IMARR *ppar ;
       float *xx = (float *)malloc(sizeof(float)*npp) ;
       float *yy = (float *)malloc(sizeof(float)*npp) ;
       ppar =  mri_complex_to_pair(fim) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,0) ,
                                             npp , ip,jp,kp , code, xx ) ;
       mri_interp_scalar_to_floats_pointset( IMARR_SUBIM(ppar,0) ,
                                             npp , ip,jp,kp , code, yy ) ;
       for( ii=0 ; ii < npp ; ii++ ){
         pval[ii].r = xx[ii] ;
         pval[ii].i = yy[ii] ;
       }
       free(yy) ; free(xx) ;
     }
     break ;

     default:
       ERROR_message("unknown image kind = %d in  mri_interp_to_same_pointset()",fim->kind) ;
     break ;

   }

   EXRETURN ;
}
