#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


/*** NOT 7D SAFE ***/

#define REF_FLOAT_SINGLE
#undef  VOX_SHORT
#include "pcor.h"

#define DFILT_SIGMA  (4.0*0.42466090)  /* 4.0 = FWHM */
#define MAX_ITER     5
#define DXY_THRESH   0.15         /* pixels */
#define PHI_THRESH   0.45         /* degrees */
#define DFAC         (PI/180.0)

/** FINE_FIT:
    experimental code for a less smoothed fit at the end
    RW Cox, 17 July 1995
 **/

#define FINE_FIT

#ifdef FINE_FIT
#  define FINE_SIGMA   (1.0*0.42466090)
#  define FINE_DXY_THRESH  0.07
#  define FINE_PHI_THRESH  0.21
#endif

#define USE_DELAYED_FIT

/** option to use bilinear interpolation during the coarse phase **/

#define MRI_ROTA(a,b,c,d) \
   ( (bilinear) ? mri_rota_bilinear((a),(b),(c),(d)) : mri_rota((a),(b),(c),(d)) )

/*********************************************************************
  05 Nov 1997: make the parameters that control the iterations
               be alterable.
**********************************************************************/

static float dfilt_sigma     = DFILT_SIGMA ,
             dxy_thresh      = DXY_THRESH ,
             phi_thresh      = PHI_THRESH ,
             fine_sigma      = FINE_SIGMA ,
             fine_dxy_thresh = FINE_DXY_THRESH ,
             fine_phi_thresh = FINE_PHI_THRESH  ;

static int max_iter = MAX_ITER ;

void mri_align_params( int maxite,
                       float sig , float dxy , float dph ,
                       float fsig, float fdxy, float fdph )
{
   if( maxite > 0   ) max_iter    = maxite ; else max_iter    = MAX_ITER    ;
   if( sig    > 0.0 ) dfilt_sigma = sig    ; else dfilt_sigma = DFILT_SIGMA ;
   if( dxy    > 0.0 ) dxy_thresh  = dxy    ; else dxy_thresh  = DXY_THRESH  ;
   if( dph    > 0.0 ) phi_thresh  = dph    ; else phi_thresh  = PHI_THRESH  ;

   fine_sigma = fsig ;
   if( fdxy > 0.0 ) fine_dxy_thresh = fdxy ; else fine_dxy_thresh = FINE_DXY_THRESH ;
   if( fdph > 0.0 ) fine_phi_thresh = fdph ; else fine_phi_thresh = FINE_PHI_THRESH ;

   return ;
}
/*********************************************************************/

/*---------------------------------------------------------------------
   Inputs:  imbase = base image that others will align to
            imwt   = image of weight factors to align to
                       (if NULL, will generate one internally)
            ims    = array of images to align to imbase
            code   = inclusive OR of ALIGN_* codes, which are:
                       ALIGN_NOITER_CODE   --> no iteration
                       ALIGN_VERBOSE_CODE  --> print progress reports
                       ALIGN_REGISTER_CODE --> return images registered
                       ALIGN_DEBUG_CODE    --> print debugging info

   Outputs: dx[i] , dy[i] , phi[i] , i=0..(ims->num - 1) =
            inputs to mri_rota that will bring ims->imarr[i] into
            alignment with imbase (dx,dy in pixels, phi in radians).

            if( (code & ALIGN_REGISTER_CODE) != 0 ) THEN
            the return value from this function will be an array of
            registered images;  otherwise, the return value is NULL.

   Method:  Iterated differential alignment.  Only works for small
            displacements (up to 4 pixels, say).
-----------------------------------------------------------------------*/

MRI_IMARR * mri_align_dfspace( MRI_IMAGE * imbase , MRI_IMAGE * imwt ,
                               MRI_IMARR * ims ,
                               int code , float dx[] , float dy[] , float phi[] )
{
   MRI_IMAGE * im1 , *bim,*xim,*yim,*tim , *bim2 , * im2 , *imww ;
   MRI_IMARR * fitim ;
   float * fit , *tar,*xar,*yar , *dfit ;
   int nx,ny , ii,jj , joff , iter , good , kim , debug , verbose , bilinear ;
   float hnx,hny ;
   double * chol_fitim=NULL ;

#ifdef FINE_FIT
   MRI_IMARR * fine_fitim  =NULL ;
   MRI_IMAGE * fine_imww   =NULL ;
   double * chol_fine_fitim=NULL ;
   int use_fine_fit = (fine_sigma > 0.0) ;
#endif

   if( ! MRI_IS_2D(imbase) ){
      fprintf(stderr,"\n*** mri_align_dfspace: cannot use nD images!\a\n") ;
      exit(1) ;
   }

   debug    = (code & ALIGN_DEBUG_CODE)    != 0 ;
   verbose  = (code & ALIGN_VERBOSE_CODE)  != 0 && !debug ;
   bilinear = (code & ALIGN_BILINEAR_CODE) != 0 ;

   if( verbose ){printf("-- mri_align_dfspace");fflush(stdout);}

   /** create the fitting images **/

   if( debug ){
      printf("-- mri_align_dfspace: code=%d\n",code);
      if( imbase->name != NULL )
        printf("-- imbase name = %s\n",imbase->name);
   }

   im1 = mri_to_float( imbase ) ;
   nx  = im1->nx ;  hnx = 0.5 * nx ;
   ny  = im1->ny ;  hny = 0.5 * ny ;

   bim = mri_filt_fft( im1 , dfilt_sigma , 0 , 0 , FILT_FFT_WRAPAROUND ) ;  /* smooth */
   xim = mri_filt_fft( im1 , dfilt_sigma , 1 , 0 , FILT_FFT_WRAPAROUND ) ;  /* d/dx */
   yim = mri_filt_fft( im1 , dfilt_sigma , 0 , 1 , FILT_FFT_WRAPAROUND ) ;  /* d/dy */

   tim = mri_new( nx , ny , MRI_float ) ;    /* x * d/dy - y * d/dx */
   tar = mri_data_pointer( tim ) ;           /* which is d/d(theta) */
   xar = mri_data_pointer( xim ) ;
   yar = mri_data_pointer( yim ) ;
   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         tar[ii+joff] = DFAC * (  (ii-hnx) * yar[ii+joff]
                                - (jj-hny) * xar[ii+joff] ) ;
      }
   }
   INIT_IMARR ( fitim ) ;
   ADDTO_IMARR( fitim , bim ) ;
   ADDTO_IMARR( fitim , xim ) ;
   ADDTO_IMARR( fitim , yim ) ;
   ADDTO_IMARR( fitim , tim ) ;

   if( imwt == NULL ) imww = mri_to_float( bim ) ;  /* 28 Oct 1996 */
   else               imww = mri_to_float( imwt ) ;

#ifdef USE_DELAYED_FIT
   chol_fitim = mri_startup_lsqfit( fitim , imww ) ;
#endif

   /*** create the FINE_FIT analog of the above, if required ***/
#ifdef FINE_FIT
   if( use_fine_fit ){
     bim = mri_filt_fft( im1 , fine_sigma , 0 , 0 , FILT_FFT_WRAPAROUND ) ;  /* smooth */
     xim = mri_filt_fft( im1 , fine_sigma , 1 , 0 , FILT_FFT_WRAPAROUND ) ;  /* d/dx */
     yim = mri_filt_fft( im1 , fine_sigma , 0 , 1 , FILT_FFT_WRAPAROUND ) ;  /* d/dy */

     tim = mri_new( nx , ny , MRI_float ) ;    /* x * d/dy - y * d/dx */
     tar = mri_data_pointer( tim ) ;           /* which is d/d(theta) */
     xar = mri_data_pointer( xim ) ;
     yar = mri_data_pointer( yim ) ;
     for( jj=0 ; jj < ny ; jj++ ){
        joff = jj * nx ;
        for( ii=0 ; ii < nx ; ii++ ){
           tar[ii+joff] = DFAC * (  (ii-hnx) * yar[ii+joff]
                                  - (jj-hny) * xar[ii+joff] ) ;
        }
     }
     INIT_IMARR ( fine_fitim ) ;
     ADDTO_IMARR( fine_fitim , bim ) ;
     ADDTO_IMARR( fine_fitim , xim ) ;
     ADDTO_IMARR( fine_fitim , yim ) ;
     ADDTO_IMARR( fine_fitim , tim ) ;

     if( imwt == NULL ) fine_imww = mri_to_float( bim ) ;  /* 03 Oct 1997 */
     else               fine_imww = mri_to_float( imwt ) ;

#ifdef USE_DELAYED_FIT
     chol_fine_fitim = mri_startup_lsqfit( fine_fitim , fine_imww ) ;
#endif
   }
#endif  /* FINE_FIT */

   mri_free( im1 ) ;

   /** fit each image to the fitting images **/

   for( kim=0 ; kim < ims->num ; kim++ ){

      if( verbose && kim%5 == 0 ){printf(".");fflush(stdout);}

      if( debug ) printf("-- Start image %d: %s\n",kim,
                         (ims->imarr[kim]->name==NULL)?" ":ims->imarr[kim]->name);

      im2  = mri_to_float( ims->imarr[kim] ) ;
      bim2 = mri_filt_fft( im2 , dfilt_sigma , 0 , 0 , FILT_FFT_WRAPAROUND ) ;
#ifdef USE_DELAYED_FIT
      fit  = mri_delayed_lsqfit( bim2 , fitim , chol_fitim ) ;
#else
      fit  = mri_lsqfit( bim2 , fitim , imww ) ;
#endif
      mri_free( bim2 ) ;

      if( debug ) printf("   fit = %13.6g %13.6g %13.6g\n",
                         fit[1],fit[2],fit[3] ) ;

      iter = 0 ;
      good = ( fabs(fit[1]) > dxy_thresh ||
               fabs(fit[2]) > dxy_thresh || fabs(fit[3]) > phi_thresh ) &&
             ( (code & ALIGN_NOITER_CODE) == 0 ) ;

      while( good ){
         tim  = MRI_ROTA( im2 , fit[1] , fit[2] , fit[3]*DFAC ) ;
         bim2 = mri_filt_fft( tim , dfilt_sigma , 0 , 0 , FILT_FFT_WRAPAROUND ) ;
#ifdef USE_DELAYED_FIT
         dfit = mri_delayed_lsqfit( bim2 , fitim , chol_fitim ) ;
#else
         dfit = mri_lsqfit( bim2 , fitim , imww ) ;
#endif
         mri_free( bim2 ) ; mri_free( tim ) ;

         if( debug )
            printf(" Cdfit = %13.6g %13.6g %13.6g\n",
                   dfit[1],dfit[2],dfit[3] ) ;

         fit[1] += dfit[1] ;
         fit[2] += dfit[2] ;
         fit[3] += dfit[3] ;

         good = (++iter < max_iter) &&
                  ( fabs(dfit[1]) > dxy_thresh ||
                    fabs(dfit[2]) > dxy_thresh || fabs(dfit[3]) > phi_thresh ) ;

         free(dfit) ; dfit = NULL ;
      } /* end while */

      /*** perform fine adjustments (always use bicubic interpolation) ***/
#ifdef FINE_FIT
      if( use_fine_fit && iter < max_iter && (code & ALIGN_NOITER_CODE) == 0 ){
         good = 1 ;
         while( good ){
            tim  = mri_rota( im2 , fit[1] , fit[2] , fit[3]*DFAC ) ;
            bim2 = mri_filt_fft( tim , fine_sigma , 0 , 0 , FILT_FFT_WRAPAROUND ) ;
#ifdef USE_DELAYED_FIT
            dfit = mri_delayed_lsqfit( bim2 , fine_fitim , chol_fine_fitim ) ;
#else
            dfit = mri_lsqfit( bim2 , fine_fitim , fine_imww ) ;
#endif
            mri_free( bim2 ) ; mri_free( tim ) ;

            if( debug )
               printf(" Fdfit = %13.6g %13.6g %13.6g\n",
                      dfit[1],dfit[2],dfit[3] ) ;

            fit[1] += dfit[1] ;
            fit[2] += dfit[2] ;
            fit[3] += dfit[3] ;

            good = (++iter < max_iter) &&
                     ( fabs(dfit[1]) > fine_dxy_thresh ||
                       fabs(dfit[2]) > fine_dxy_thresh || fabs(dfit[3]) > fine_phi_thresh ) ;

            free(dfit) ; dfit = NULL ;
         } /* end while */
      }
#endif

      dx[kim]  = fit[1] ;
      dy[kim]  = fit[2] ;
      phi[kim] = fit[3]*DFAC ;

      if( debug )
         printf("   FIT = %13.6g %13.6g %13.6g\n",
                fit[1],fit[2],fit[3] ) ;

      free(fit) ; fit = NULL ; mri_free( im2 ) ;
   }

   if( verbose ){printf("\n");fflush(stdout);}

   DESTROY_IMARR( fitim ) ;
#ifdef USE_DELAYED_FIT
   free(chol_fitim) ; chol_fitim = NULL ;
#endif

#ifdef FINE_FIT
   if( use_fine_fit ){
     DESTROY_IMARR( fine_fitim ) ;
     mri_free( fine_imww ) ;
#ifdef USE_DELAYED_FIT
     free(chol_fine_fitim) ; chol_fine_fitim = NULL ;
#endif
   }
#endif

   mri_free( imww ) ;

   if( (code & ALIGN_REGISTER_CODE) == 0 ) return NULL ;

   /** do the actual registration, if ordered (is always bicubic) **/

   INIT_IMARR( fitim ) ;

   if( verbose ){printf("-- registering");fflush(stdout);}

   for( kim=0 ; kim < ims->num ; kim++ ){
      tim = mri_rota( ims->imarr[kim] , dx[kim],dy[kim],phi[kim] ) ;
      ADDTO_IMARR( fitim , tim ) ;
      if( verbose && kim%5 == 0 ){printf(".");fflush(stdout);}
   }

   if( verbose ){printf("\n");fflush(stdout);}

   return fitim ;
}

#ifdef ALLOW_DFTIME
/*----------------------------------------------------------------------------
  Similar routine to above, but uses temporal filtering to determine how
  much to alter each pixel.  Note that if ALIGN_REGISTER_CODE is not turned
  on, this routine will produce identical outputs to the dfspace routine.
------------------------------------------------------------------------------*/

#define NREF 5  /* 1st 3 refs = dx,dy,phi; others are polynomials */

MRI_IMARR * mri_align_dftime( MRI_IMAGE * imbase , MRI_IMAGE * imwt ,
                              MRI_IMARR * ims ,
                              int code , float dx[] , float dy[] , float phi[] )
{
   int kim , ii , nim,npix , my_code , nx,ny , jj , detrend , doboth , verbose,debug ;
   references * xypref ;
   voxel_corr * xypcor ;
   float ref[NREF] , val , sum ;
   MRI_IMAGE * tim ;
   MRI_IMARR * fitim , * outim , * lims ;
   float * fitar[NREF] , * tar ;

   /** run the dfspace code to get the displacements **/

   debug   = (code & ALIGN_DEBUG_CODE) != 0 ;
   verbose = (code & ALIGN_VERBOSE_CODE) != 0 && !debug ;

   doboth  = (code & ALIGN_DOBOTH_CODE) != 0  && (code & ALIGN_REGISTER_CODE) != 0 ;

   if( !doboth ){
      my_code = code & ~ALIGN_REGISTER_CODE ;
   } else {
      my_code = code ;
   }
   lims = mri_align_dfspace( imbase , imwt , ims , my_code , dx,dy,phi ) ;
   if( (code & ALIGN_REGISTER_CODE) == 0 ) return NULL ;

   /** do some real work now **/

   detrend = (code & ALIGN_DETREND_CODE) != 0 ;
   if( !doboth ) lims = ims ;

   /** run the time series code to get the pixel-by-pixel
       correlation between the displacements and the pixel intensities **/

   nim  = lims->num ;
   nx   = imbase->nx ; ny = imbase->ny ; npix = nx * ny ;

   xypref = new_references( NREF ) ;
   xypcor = new_voxel_corr( npix , NREF ) ;

   if( verbose ){printf("-- temporal filtering");fflush(stdout);}

   for( kim=0 ; kim < nim ; kim++ ){
      ref[0] = dx[kim] ; ref[1] = dy[kim] ; ref[2] = phi[kim] / DFAC ;

      val = 1.0 ;
      for( jj=3 ; jj < NREF ; jj++ ){
         ref[jj] = val ;
         val    *= (kim-0.5*nim) ;
      }

      tim = lims->imarr[kim] ;
      if( tim->kind != MRI_float ) tim = mri_to_float( lims->imarr[kim] ) ;

      update_references( ref , xypref ) ;
      update_voxel_corr( MRI_FLOAT_PTR(tim) , xypref , xypcor ) ;

      if( tim != lims->imarr[kim] ) mri_free( tim ) ;

      if( verbose && kim%10==0 ){printf(".");fflush(stdout);}
   }

   INIT_IMARR( fitim ) ;
   for( jj=0 ; jj < 3 ; jj++ ){
      tim = mri_new( nx , ny , MRI_float ) ;
      ADDTO_IMARR( fitim , tim ) ;
      fitar[jj] = MRI_FLOAT_PTR(tim) ;
   }

   if( detrend ){
      for( jj=3 ; jj < NREF ; jj++ ){
         tim = mri_new( nx , ny , MRI_float ) ;
         ADDTO_IMARR( fitim , tim ) ;
         fitar[jj] = MRI_FLOAT_PTR(tim) ;
      }
   } else {
      for( jj=3 ; jj < NREF ; jj++ ) fitar[jj] = NULL ;
   }

   if( verbose ){printf("!");fflush(stdout);}

   get_lsqfit( xypref , xypcor , fitar ) ;

   free_references( xypref ) ;
   free_voxel_corr( xypcor ) ;

   /** apply the fits to each image **/

   INIT_IMARR( outim ) ;

   for( kim=0 ; kim < nim ; kim++ ){

      tim = mri_to_float( lims->imarr[kim] ) ;
      tar = MRI_FLOAT_PTR(tim) ;

      if( detrend ){
         for( ii=0 ; ii < npix ; ii++ ){
            sum = tar[ii] - (  fitar[0][ii] * dx[kim]
                             + fitar[1][ii] * dy[kim] + fitar[2][ii] * phi[kim] / DFAC ) ;
            for( jj=3 ; jj < NREF ; jj++ ) sum -= fitar[jj][ii] * ref[jj] ;
            tar[ii] = sum ;
         }
      } else {
         for( ii=0 ; ii < npix ; ii++ )
            tar[ii] -= (  fitar[0][ii] * dx[kim]
                        + fitar[1][ii] * dy[kim] + fitar[2][ii] * phi[kim] / DFAC ) ;
      }

      ADDTO_IMARR( outim , tim ) ;

      if( verbose && kim%10==0 ){printf(".");fflush(stdout);}

      if( lims != ims ) mri_free( lims->imarr[kim] ) ;
   }

   if( verbose ){printf("\n");fflush(stdout);}

   DESTROY_IMARR( fitim ) ;
   if( lims != ims ) FREE_IMARR( lims ) ;

   return outim ;
}
#endif /* ALLOW_DFTIME */
