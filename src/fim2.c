/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mrilib.h"
#include "pcor.h"
#include "ts.h"

#ifndef MAX
#  define MAX(x,y)  (((x)>(y)) ? (x) : (y))
#endif

#ifndef MIN
#  define MIN(x,y)  (((x)<(y)) ? (x) : (y))
#endif

#ifndef MALLOC_ERR
#  define MALLOC_ERR(str) {fprintf(stderr,"malloc error for %s\a\n",str);exit(1);}
#endif

#define SCALE 10000
#define BLAST 33333.0

#define DFILT_NONE   0           /* no derivative filtering */

#define DFILT_SPACE  1           /* spatial mode */
#define DFILT_SPACE0 11

#define DFILT_TIME   2           /* temporal mode */
#define DFILT_TIME0  21

#define DFILT_BOTH   3           /* both, separately */
#define DFILT_BOTH0  31

#define DFILT_SPACETIME  4       /* both, together */
#define DFILT_SPACETIME0 41

#define CLIP_FRAC 0.10           /* for clipping nonbrain (low intensity) pixels */

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/** structure type to hold results from scanning the command line options **/

typedef struct {
    char *fname_corr ,          /* filename for correlation image */
         *fname_fim ,           /* filename for activation image */
         *fname_cnr ,           /* filename for contrast-to-noise image */
         *fname_sig ,           /* filename for standard deviation image */
         *fname_fit  ;          /* filename root for fit coefficients */
    MRI_IMARR * ims ;           /* array of images themselves */
    MRI_IMARR * rims ;          /* array of registered images */
    float scale_fim ,           /* scale factor for alpha image */
          thresh_pcorr ,        /* correlation coeff. threshold */
          thresh_report ;       /* reporting threshold for output */
    int nxim , nyim ,           /* image dimensions */
        ntime ;                 /* number of time points */
    time_series *weight ;       /* pointer to weighting vector */
    time_series_array * refts,  /* array of reference (detrending) vectors */
                      * idts  ; /* array of ideal vectors */
    int flim ,                  /* if TRUE, write floating point images */
        norm_fim ,              /* if TRUE, normalize alpha image */
        scale_output ;          /* if TRUE, force scale data into outputs' corners */
    int dfilt_code ;            /* one of the DFILT_ consts above */
    int reg_bilinear ;          /* 1 for bilinear registration, 0 for bicubic */
    MRI_IMAGE * first_flim ;    /* first image to use, in flim format */
    int do_clip , debug , quiet ;
    char * fname_subort ;
} line_opt ;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** internal prototypes ***/

void get_line_opt( int argc , char *argv[] , line_opt *opt ) ;

void Syntax( char * ) ;

void write_images( line_opt * , char * , MRI_IMAGE * ,
                   float      , char * , MRI_IMAGE *  ) ;

time_series * edit_weight( time_series * , time_series * ) ;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int main( int argc , char *argv[] )
{
   line_opt   opt ;        /* holds data constructed from command line */
   references *ref ;       /* holds reference covariance computations */
   voxel_corr *voxcor ;    /* holds voxel-specific covariances */
   float      *pcorr ;     /* holds partial correlation coefficients */
   float      *alpha ;     /* holds activation levels */
   float      *refvec ,    /* values of reference vectors at given time */
              *voxvec ;    /* values of voxel data at given time */
   MRI_IMAGE  *voxim ;     /* image data (contains voxvec) */
   MRI_IMAGE  *imcor ,     /* correlation image (pcorr is part of this) */
              *imalp  ;    /* activation level image (alpha is in this ) */
   int nref , nideal ;

   float      wt ;         /* weight factor at given time */

   /* duplicates for rims */

   references *ref_reg ;
   voxel_corr *voxcor_reg ;
   float      *pcorr_reg , *alpha_reg ;
   MRI_IMAGE  *imcor_reg , *imalp_reg  ;
   int nref_reg ;

   int   itime , numvox , good , vox , ii , ii_idts ;
   int   do_cnr , do_sig , do_fit , do_subort ;

   time_series * wtnew ;

   MRI_IMARR * cor_ims , * alp_ims , * cnr_ims , * sig_ims ;
   MRI_IMARR ** fit_ims ;  /* a whole bunch of arrays of images! */
   MRI_IMAGE * best_im ;
   int       * best ;
   float     * cornew ;
   float       cnew , cold ;

   int   npass_pos , npass_neg ;   /* number of voxels that pass the test */
   float cormin , cormax ,         /* min and max correlations seen */
         alpmin , alpmax ;         /* ditto for activations */

/*** read command line, and set up as it bids ***/

   get_line_opt( argc , argv , &opt ) ;

   numvox = opt.nxim * opt.nyim ;
   nref   = opt.refts->num ;
   nideal = opt.idts->num ;

   do_cnr    = opt.fname_cnr    != NULL ;
   do_sig    = opt.fname_sig    != NULL ;
   do_fit    = opt.fname_fit    != NULL ;
   do_subort = opt.fname_subort != NULL ;

   /** create place to put output images for each ideal **/

   INIT_IMARR(cor_ims) ;
   INIT_IMARR(alp_ims) ;
   if( do_cnr ) INIT_IMARR(cnr_ims) ;
   if( do_sig ) INIT_IMARR(sig_ims) ;
   if( do_fit || do_subort )
      fit_ims = (MRI_IMARR **) malloc( sizeof(MRI_IMARR *) * nideal ) ;

   /*** June 1995: set up to clip on intensities of first_flim ***/

   if( CLIP_FRAC > 0.0 && opt.do_clip ){
      float ftop , fbot , clipper , val ;
      float * far ;
      int nclipper ;

      ftop = mri_max( opt.first_flim ) ;
      fbot = mri_min( opt.first_flim ) ;
      ftop = (fabs(ftop) > fabs(fbot)) ? fabs(ftop) : fabs(fbot) ;
      ftop = CLIP_FRAC * ftop ;
      far  = MRI_FLOAT_PTR( opt.first_flim ) ;

      clipper  = 0.0 ;
      nclipper = 0 ;
      for( vox=0 ; vox < numvox ; vox++ ){
         val = fabs(far[vox]) ;
         if( val >= ftop ){ clipper += val ; nclipper++ ; }
      }
      clipper  = CLIP_FRAC * clipper / nclipper ;
      nclipper = 0 ;
      for( vox=0 ; vox < numvox ; vox++ ){
         val = fabs(far[vox]) ;
         if( val < clipper ){ far[vox] = 0.0 ; nclipper++ ; }
      }
      if( nclipper > 0 && !opt.quiet )
         printf("CLIPPING %d voxels to zero for low intensity in base image!\n",nclipper) ;
   }

   refvec = (float *) malloc( sizeof(float) * nref ) ;
   if( refvec == NULL ) MALLOC_ERR("refvec") ;

   /** July 1995: master loop over multiple ideals **/

   for( ii_idts=0 ; ii_idts < nideal ; ii_idts++ ){

      opt.refts->tsarr[nref-1] = opt.idts->tsarr[ii_idts] ;          /* last ref = new ideal */
      wtnew = edit_weight( opt.weight , opt.idts->tsarr[ii_idts] ) ; /* make weight vector */

      if( wtnew == NULL ){
         fprintf(stderr,"** bad weight at ideal # %d -- end of run!\a\n",ii_idts+1) ;
         exit(1) ;
      }

      ref    = new_references( nref ) ;
      voxcor = new_voxel_corr( numvox , nref ) ;

      if( opt.rims != NULL ){
         nref_reg   = nref - 3 ;                    /* don't use the 1st 3 refs */
         ref_reg    = new_references( nref_reg ) ;
         voxcor_reg = new_voxel_corr( numvox , nref_reg ) ;
      }

      /*** loop through time,
           getting new reference vector data, and
           new images to correlate with the references vectors ***/

      itime = 0 ;
      do{
         int wt_not_one ;

         /*** find weighting factor for this time ***/

         wt = wtnew->ts[itime] ;

         /*** process new image for this time (if any weight is given to it) ***/

         if( wt != 0.0 ){
            wt_not_one = (wt != 1.0) ;
            voxim      = (wt_not_one) ? mri_to_float( opt.ims->imarr[itime] )  /* copy */
                                      : opt.ims->imarr[itime] ;                /* pointer */
            voxvec = MRI_FLOAT_PTR( voxim ) ;
            for( ii=0 ; ii < nref ; ii++ )
               refvec[ii] = opt.refts->tsarr[ii]->ts[itime] ;
            if( wt_not_one ){
               for( vox=0 ; vox < nref   ; vox++ ) refvec[vox] *= wt ;
               for( vox=0 ; vox < numvox ; vox++ ) voxvec[vox] *= wt ;
            }
            update_references( refvec , ref ) ;
            update_voxel_corr( voxvec , ref , voxcor ) ;
            if( wt_not_one ) mri_free( voxim ) ;

            /*** same for registered images, but don't use refs 0, 1, or 2 ***/

            if( opt.rims != NULL ){
               voxim      = (wt_not_one) ? mri_to_float( opt.rims->imarr[itime] )  /* copy */
                                         : opt.rims->imarr[itime] ;                /* pointer */
               voxvec = MRI_FLOAT_PTR( voxim ) ;
               for( ii=0 ; ii < nref_reg ; ii++ )
                  refvec[ii] = opt.refts->tsarr[ii+3]->ts[itime] ;
               if( wt_not_one ){
                  for( vox=0 ; vox < nref_reg ; vox++ ) refvec[vox] *= wt ;
                  for( vox=0 ; vox < numvox   ; vox++ ) voxvec[vox] *= wt ;
               }
               update_references( refvec , ref_reg ) ;
               update_voxel_corr( voxvec , ref_reg , voxcor_reg ) ;
               if( wt_not_one ) mri_free( voxim ) ;
            }
         }

      } while( ++itime < opt.ntime ) ;

      /*** compute outputs ***/

      imcor = mri_new( opt.nxim , opt.nyim , MRI_float ) ;   /* output images */
      imalp = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
      pcorr = MRI_FLOAT_PTR( imcor ) ;                       /* output arrays */
      alpha = MRI_FLOAT_PTR( imalp ) ;

      get_pcor( ref , voxcor , pcorr ) ;   /* compute partial correlation */
      get_coef( ref , voxcor , alpha ) ;  /* compute activation levels */

      /*** if have images AND registered images (DFBOTH), merge them ***/

      if( opt.rims != NULL ){
         float pc , pcreg ;

         imcor_reg  = mri_new( opt.nxim , opt.nyim , MRI_float ) ;   /* output images */
         imalp_reg  = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
         pcorr_reg  = MRI_FLOAT_PTR( imcor ) ;                       /* output arrays */
         alpha_reg  = MRI_FLOAT_PTR( imalp ) ;

         get_pcor( ref_reg , voxcor_reg , pcorr_reg ) ;
         get_coef( ref_reg , voxcor_reg , alpha_reg ) ;

         for( ii=0 ; ii < numvox ; ii++ ){
            pc = pcorr[ii] ; pcreg = pcorr_reg[ii] ;
            if( fabs(pc) > fabs(pcreg) ){
               pcorr[ii] = pcreg ; alpha[ii] = alpha_reg[ii] ;
            }
         }

         mri_free( imalp_reg ) ; mri_free( imcor_reg ) ;
         free_references( ref_reg ) ; free_voxel_corr( voxcor_reg ) ;
      }

      /*** June 1995: clip on intensities of first_flim ***/

      if( CLIP_FRAC > 0.0 && opt.do_clip ){
         float * far = MRI_FLOAT_PTR( opt.first_flim ) ;
         for( vox=0 ; vox < numvox ; vox++ )
            if( far[vox] == 0.0 ){ pcorr[vox] = alpha[vox] = 0.0 ; }
      }

      /*** store images for later processing ***/

      ADDTO_IMARR( cor_ims , imcor ) ;
      ADDTO_IMARR( alp_ims , imalp ) ;

      /*** compute CNR and SIG, if desired ***/

      if( do_cnr || do_sig ){
         MRI_IMAGE * imcnr , * imsig ;
         float     * cnr   , * sig ;
         float rbot,rtop , scale , sbot ;
         int ii , first , its ;

         first = 1 ;
         rbot  = rtop = 0 ;
         its   = nref - 1 ;         /* index of ideal */

         for( ii=0 ; ii < opt.ntime ; ii++ ){
            if( wtnew->ts[ii] > 0.0 ){
               if( first ){
                  rtop  = rbot = opt.refts->tsarr[its]->ts[ii] ;
                  first = 0 ;
               } else {
                  rbot = MIN( opt.refts->tsarr[its]->ts[ii] , rbot ) ;
                  rtop = MAX( opt.refts->tsarr[its]->ts[ii] , rtop ) ;
               }
            }
         }
         scale = rtop-rbot ;

         imsig = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
         sig   = MRI_FLOAT_PTR(imsig) ;
         get_variance( voxcor , sig ) ;
         sbot = 0.0 ;
         for( vox=0 ; vox < numvox ; vox++ )
            if( sig[vox] > 0.0 ){ sig[vox] = sqrt(sig[vox]) ; sbot += sig[vox] ; }
            else                  sig[vox] = 0.0 ;
         sbot = 0.001 * sbot / numvox ;   /* for clipping cnr */

         if( do_cnr ){
            imcnr = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
            cnr   = MRI_FLOAT_PTR(imcnr) ;
            for( vox=0 ; vox < numvox ; vox++ )
               if( sig[vox] > sbot ) cnr[vox] = scale * alpha[vox] / sig[vox] ;
               else                  cnr[vox] = 0.0 ;
            ADDTO_IMARR( cnr_ims , imcnr ) ;

#ifdef DEBUG
   { char buf[64] ;
     printf("ideal %d: sbot = %g\n",ii_idts,sbot) ;
     sprintf(buf,"cnr.%02d",ii_idts) ; mri_write(buf,imcnr) ;
     sprintf(buf,"sig.%02d",ii_idts) ; mri_write(buf,imsig) ;
     sprintf(buf,"alp.%02d",ii_idts) ; mri_write(buf,imalp) ;
   }
#endif
         }

         if( do_sig ) ADDTO_IMARR( sig_ims , imsig ) ;
         else         mri_free(imsig) ;
      }

      /** save fit coefficients for the -fitfile option **/

      if( do_fit || do_subort ){
         MRI_IMARR * fitim ;
         MRI_IMAGE * tim ;
         float ** fitar ;
         int ir ;

         INIT_IMARR(fitim) ;  /* create array of fit coefficients */
                              /* (one for each ref vector) */

         fitar = (float **) malloc( sizeof(float *) * nref ) ;
         for( ir=0 ; ir < nref ; ir++ ){
            tim = mri_new( opt.nxim , opt.nyim , MRI_float ) ; /* ir-th fit image */
            fitar[ir] = MRI_FLOAT_PTR(tim) ;                   /* ir-th array */
            ADDTO_IMARR(fitim,tim) ;
         }

         get_lsqfit( ref , voxcor , fitar ) ; /* compute all fit arrays at once */
         fit_ims[ii_idts] = fitim ;           /* add a whole new image array */

         free( fitar ) ;  /* don't need these pointers, are stored in fit_ims */
      }

      /*** cleanup for next ideal ***/

      free_references( ref ) ; free_voxel_corr( voxcor ) ;
      RWC_free_time_series( wtnew ) ;

   }  /*** end of loop over ideals (ii_idts) ***/

   /** release images and other data (unless they are needed below) **/

   if( !do_subort )       DESTROY_IMARR(opt.ims) ;
   if( opt.rims != NULL ) DESTROY_IMARR(opt.rims) ;

   opt.refts->tsarr[nref-1] = NULL ;           /* this one is in opt.idts */
   if( !do_subort ) DESTROY_TSARR(opt.refts) ;
   DESTROY_TSARR(opt.idts) ;

   mri_free( opt.first_flim ) ;
   free( refvec ) ;
   RWC_free_time_series( opt.weight ) ;

   /*************** scan through all ideals and pick the "best" ***************/

   if( nideal == 1 ){
      imcor = cor_ims->imarr[0] ;
      imalp = alp_ims->imarr[0] ;
   } else {
      imcor   = mri_to_float( cor_ims->imarr[0] ) ;          /* to be best correlation */
      pcorr   = MRI_FLOAT_PTR( imcor ) ;

      best_im = mri_new( opt.nxim , opt.nxim , MRI_int ) ;   /* to be index of best */
      best    = MRI_INT_PTR(best_im) ;
      for( vox=0 ; vox < numvox ; vox++ ) best[vox] = 0 ;    /* start at first ideal */

      /** find best correlation image **/

      for( ii_idts=1 ; ii_idts < nideal ; ii_idts++ ){
         cornew = MRI_FLOAT_PTR( cor_ims->imarr[ii_idts] ) ;  /* ii_idts'th correlation */
         for( vox=0 ; vox < numvox ; vox++ ){
            cnew = cornew[vox] ; cold = pcorr[vox] ;
            if( fabs(cnew) > fabs(cold) ){
               best[vox]  = ii_idts ; pcorr[vox] = cnew ;
            }
         }
      }

      /** load best alpha image **/

      imalp = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
      alpha = MRI_FLOAT_PTR( imalp ) ;
      for( vox=0 ; vox < numvox ; vox++ )
         alpha[vox] = MRI_FLOAT_PTR( alp_ims->imarr[best[vox]] )[vox] ;
   }

   /*** write correlation and alpha out ***/

   write_images( &opt ,            opt.fname_corr , imcor ,
                 opt.thresh_pcorr, opt.fname_fim  , imalp  ) ;

   /*** write a report to the screen ***/

   npass_pos = npass_neg = 0 ;

   for( vox=0 ; vox < numvox ; vox++ )
      if( fabs(pcorr[vox]) >= opt.thresh_pcorr ){
         if( pcorr[vox] > 0 ) npass_pos++ ;
         if( pcorr[vox] < 0 ) npass_neg++ ;
      }

   cormin = mri_min( imcor ) ; cormax = mri_max( imcor ) ;
   alpmin = mri_min( imalp ) ; alpmax = mri_max( imalp ) ;

   if( !opt.quiet ){
      printf( "minimum activation  = %11.4g  maximum = %11.4g\n", alpmin,alpmax);
      printf( "minimum correlation = %11.4g  maximum = %11.4g\n", cormin,cormax);
      printf( "number of voxels with correlation >=  %5.3f is %d\n",
              opt.thresh_pcorr , npass_pos ) ;
      printf( "number of voxels with correlation <= -%5.3f is %d\n",
              opt.thresh_pcorr , npass_neg ) ;
   }

   DESTROY_IMARR( cor_ims ) ;
   DESTROY_IMARR( alp_ims ) ;
   if( nideal > 1 ){ mri_free(imcor) ; mri_free(imalp) ; }

   /*** write CNR out ***/

   if( do_cnr ){
      MRI_IMAGE * imcnr ;
      float * cnr ;

      if( nideal == 1 ){
         imcnr = cnr_ims->imarr[0] ;
      } else {
         imcnr = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
         cnr   = MRI_FLOAT_PTR( imcnr ) ;
         for( vox=0 ; vox < numvox ; vox++ )
            cnr[vox] = MRI_FLOAT_PTR( cnr_ims->imarr[best[vox]] )[vox] ;
      }

      if( opt.flim ){
         mri_write( opt.fname_cnr , imcnr ) ;
      } else {
         MRI_IMAGE * shim ;
         shim = mri_to_short( 100.0 , imcnr ) ;
         mri_write( opt.fname_cnr , shim ) ;
         mri_free( shim ) ;
      }

      DESTROY_IMARR( cnr_ims ) ;
      if( nideal > 1 ) mri_free(imcnr) ;
   }

   /*** write SIG out ***/

   if( do_sig ){
      MRI_IMAGE * imsig ;
      float * sig ;

      if( nideal == 1 ){
         imsig = sig_ims->imarr[0] ;
      } else {
         imsig = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
         sig   = MRI_FLOAT_PTR( imsig ) ;
         for( vox=0 ; vox < numvox ; vox++ )
            sig[vox] = MRI_FLOAT_PTR( sig_ims->imarr[best[vox]] )[vox] ;
      }

      mri_write( opt.fname_sig , imsig ) ;  /* always flim */
      DESTROY_IMARR( sig_ims ) ;
      if( nideal > 1 ) mri_free(imsig) ;
   }

   /*** write FIT out ***/

   if( do_fit || do_subort ){
      char root[128] , fname[128] ;
      int ir , ib ;
      MRI_IMAGE * tim ;
      float * tar , * qar ;
      float ortval ;

      if( do_fit ){
         strcpy(root,opt.fname_fit) ; ir = strlen(root) ;
         if( root[ir-1] != '.' ){ root[ir]   = '.' ; root[ir+1] = '\0' ; }
      }

      /* if have more than 1 ideal, must pick best fit and put into new image */

      if( nideal > 1 ){
         tim = mri_new( opt.nxim , opt.nyim , MRI_float ) ;
         tar = MRI_FLOAT_PTR( tim ) ;
      }

      for( ir=0 ; ir < nref ; ir++ ){
         if( nideal == 1 ){
            tim = fit_ims[0]->imarr[ir] ;  /* 1 ideal --> output the 1 fit */
            tar = MRI_FLOAT_PTR( tim ) ;   /* ptr to coefficients */
         } else {
            for( vox=0 ; vox < numvox ; vox++ )
               tar[vox] = MRI_FLOAT_PTR( fit_ims[best[vox]]->imarr[ir] )[vox] ;
         }

         if( do_fit ){
            sprintf(fname,"%s%02d",root,ir+1) ;
            mri_write( fname , tim ) ;          /* always flim */
         }

         if( do_subort && ir < nref-1 ){  /* subtract ort # ir from images */

            for( itime=0 ; itime < opt.ntime ; itime++ ){   /* loop over time */
               ortval = opt.refts->tsarr[ir]->ts[itime] ;
               if( fabs(ortval) >= BLAST ) continue ;       /* skip this one */
               qar = MRI_FLOAT_PTR(opt.ims->imarr[itime]) ; /* current image */
               for( vox=0 ; vox < numvox ; vox++ )          /* loop over space */
                  qar[vox] -= ortval * tar[vox] ;           /* subtract ort */

            } /* end of loop over time */
         }
      } /* end of loop over ref vectors */

      for( ii_idts=0 ; ii_idts < nideal ; ii_idts++ )  /* don't need fits no more */
         DESTROY_IMARR(fit_ims[ii_idts]) ;
      free(fit_ims) ;

      if( nideal > 1 ) mri_free(tim) ;  /* don't need best fit image no more */

      if( do_subort ){
         strcpy(root,opt.fname_subort) ; ir = strlen(root) ;
         if( root[ir-1] != '.' ){ root[ir]   = '.' ; root[ir+1] = '\0' ; }

         for( itime=0 ; itime < opt.ntime ; itime++ ){
            sprintf(fname,"%s%04d",root,itime+1) ;
            mri_write( fname , opt.ims->imarr[itime] ) ; /* always flim */
         }

         DESTROY_IMARR(opt.ims) ;    /* no longer used */
         DESTROY_TSARR(opt.refts) ;
      }
   }

   /******** all done ********/

   if( nideal > 1 ) mri_free( best_im ) ;
   exit(0) ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** write images out
     (inputs must be floating mrilib format) ***/

void write_images( line_opt *opt ,    char *fname_imcor , MRI_IMAGE *imcor ,
                   float thresh_cor , char *fname_imalp , MRI_IMAGE *imalp  )
{
   int vox ,
       numvox = imcor->nx * imcor->ny ;

   float *pcorr = imcor->im.float_data ,
         *alpha = imalp->im.float_data  ;

   MRI_IMAGE *shim ;  /* image of shorts to actually write */
   short     *shar ;  /* array inside shim */

   float sfac , alpmin , alpmax ;

/*** threshold alpha on pcorr ***/

   if( thresh_cor > 0.0 ){
      for( vox=0 ; vox < numvox ; vox++ )
         if( fabs(pcorr[vox]) < thresh_cor ) alpha[vox] = 0.0 ;
   }

/*** write output images ***/

   if( opt->flim ){  /* write floating point images [an advanced user :-)] */

      if( fname_imcor != NULL ){            /* write correlation image */
         mri_write( fname_imcor , imcor ) ;
      }

      if( fname_imalp != NULL ){            /* write activation image */
         mri_write( fname_imalp , imalp ) ;
      }

   } else {  /* write short images [a primitive user :-(] */

   /*** scale and write correlation image ***/

      if( fname_imcor != NULL ){

         sfac = SCALE ;
         shim = mri_to_short( sfac , imcor ) ;   /* scale to shorts */

         if( opt->scale_output ){
            shar    = MRI_SHORT_PTR( shim ) ;    /* access array in shim */
            shar[0] = 0 ;                           /* for display purposes */
            shar[1] = -SCALE ;
            shar[2] =  SCALE ;
         }

         mri_write( fname_imcor , shim ) ;   /* write short image */
         mri_free( shim ) ;                      /* toss it away */
      }

   /*** scale and write activation image ***/

      if( fname_imalp != NULL ){

         alpmin = mri_min( imalp ) ; alpmin = fabs(alpmin) ;  /* find */
         alpmax = mri_max( imalp ) ; alpmax = fabs(alpmax) ;  /* biggest */
         alpmax = (alpmin < alpmax) ? (alpmax) : (alpmin) ;   /* alpha */

         /*** default scaling (normalization) ***/

         if( opt->norm_fim ){  /* normalize alpha to +/-SCALE range */
            if( alpmax==0.0 ){   /* no data range! */
               sfac = SCALE ;
            } else {
               sfac = SCALE / alpmax ;
            }
         } else {

         /*** user input scale factor ***/

            sfac = opt->scale_fim ;
         }

         shim = mri_to_short( sfac , imalp ) ;    /* do the scaling */
         shar = MRI_SHORT_PTR( shim ) ;        /* get array of shorts */

         for( vox=0 ; vox < numvox ; vox++ )
                 if( shar[vox] >  SCALE ) shar[vox] =  SCALE ;
            else if( shar[vox] < -SCALE ) shar[vox] = -SCALE ;

         if( opt->scale_output ){
            shar    = MRI_SHORT_PTR( shim ) ;    /* access array in shim */
            shar[0] = 0 ;                           /* for display purposes */
            shar[1] = -SCALE ;
            shar[2] =  SCALE ;
         }

         mri_write( fname_imalp , shim ) ;      /* write short image */
         mri_free( shim ) ;                     /* and toss it */
      }

   }  /* endif (opt->flim) */

   return ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** read and interpret command line arguments ***/

#ifdef DEBUG
#  define DBOPT(str) fprintf(stderr,"at option %s: %s\n",argv[nopt],str)
#else
#  define DBOPT(str) /* nothing */
#endif

void get_line_opt( int argc , char *argv[] , line_opt *opt )
{
   int nopt , ii , pp , rr , bad ;
   int first_im , do_corr , num_im , num_time , polref ;
   float ftemp , fscal ;
   MRI_IMAGE *im ;
   time_series *ideal , *vec ;
   char err_note[128] , * regbase ;

/*** give help if needed ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax(NULL) ;

/*** setup opt with defaults ***/

   opt->fname_corr    = NULL ;   /* no output names yet */
   opt->fname_fim     = NULL ;
   opt->fname_cnr     = NULL ;
   opt->fname_fit     = NULL ;
   opt->fname_subort  = NULL ;
   opt->fname_sig     = NULL ;
   opt->thresh_pcorr  = 0.70 ;   /* default 70% threshold */
   opt->thresh_report = 10.0 ;   /* not currently used */
   opt->nxim          = 0 ;
   opt->nyim          = 0 ;
   opt->ntime         = 0 ;
   opt->weight        = NULL ;   /* no weight vector yet */
   opt->ims           = NULL ;
   opt->rims          = NULL ;
   opt->flim          = FALSE ;  /* default: don't write float images */
   opt->scale_fim     = 1.0 ;    /* scale factor for writing short images */
                                 /* (used only if norm_fim is FALSE) */
   opt->norm_fim      = TRUE ;   /* default: normalize alpha image */
   opt->scale_output  = TRUE ;   /* default: force scales into output corners */

   opt->dfilt_code    = DFILT_NONE ;    /* default: do neither of these things */
   opt->reg_bilinear  = 0 ;             /* default: bicubic */
   opt->do_clip       = 0 ;
   opt->debug         = 0 ;
   opt->quiet         = 0 ;

   /*** initialize array of time series data ***/

   INIT_TSARR(opt->refts) ;
   INIT_TSARR(opt->idts) ;

/*** set defaults in local variables ***/

   polref   = 0 ;         /* maximum order of polynomial reference vectors */
   first_im = 1 ;         /* first image to actually use */
   num_im   = 999999 ;    /* maximum number of images to use */
   do_corr  = FALSE ;     /* write correlation image out? */
   ideal    = NULL ;      /* time_series of ideal response vector */
   regbase  = NULL ;      /* pointer to name of registration image */

/*** read command line arguments and process them:
      coding technique is to examine each argv, and if it matches
      something we expect (e.g., -flim), process it, then skip to
      the next loop through ('continue' statements in each strncmp if).
      Note that the 'while' will increment the argument index (nopt) ***/

   nopt = 1 ;
   do{  /* a while loop, continuing until we reach the MRI filenames */

      /** clip option **/

      if( strncmp(argv[nopt],"-clip",5) == 0 ){
         DBOPT("-clip") ;
         opt->do_clip = 1 ;
         continue ;
      }

      /** quiet option **/

      if( strncmp(argv[nopt],"-q",2) == 0 ){
         DBOPT("-q") ;
         opt->quiet = 1 ;
         continue ;
      }

      /** debug option **/

      if( strncmp(argv[nopt],"-debug",5) == 0 ){
         DBOPT("-debug") ;
         opt->debug = 1 ;
         continue ;
      }

      /** anti-motion filtering options **/

      if( strncmp(argv[nopt],"-bilinear",6) == 0 ){
         DBOPT("-bilinear") ;
         opt->reg_bilinear = 1 ;
         continue ;
      }

      if( strncmp(argv[nopt],"-regbase",6) == 0 ){
         DBOPT("-regbase") ;
         if( ++nopt >= argc ) Syntax("-regbase needs a filename!") ;
         regbase = argv[nopt] ;
         continue ;
      }

#ifdef ALLOW_DFTIME
      if( strstr(argv[nopt],"-dftime") != NULL ){
         DBOPT("-dftime") ;
         opt->dfilt_code = DFILT_TIME ;
         if( strstr(argv[nopt],":0") != NULL ) opt->dfilt_code = DFILT_TIME0 ;
         continue ;
      }
#endif

      if( strstr(argv[nopt],"-dfspace") != NULL && strstr(argv[nopt],"-dfspacetime") == NULL ){
         DBOPT("-dfspace") ;
         opt->dfilt_code = DFILT_SPACE ;
         if( strstr(argv[nopt],":0") != NULL ) opt->dfilt_code = DFILT_SPACE0 ;
         continue ;
      }

#ifdef ALLOW_DFTIME
      if( strstr(argv[nopt],"-dfboth") != NULL ){
         DBOPT("-dfboth") ;
         opt->dfilt_code = DFILT_BOTH ;
         if( strstr(argv[nopt],":0") != NULL ) opt->dfilt_code = DFILT_BOTH0 ;
         continue ;
      }
#endif

#ifdef ALLOW_DFTIME
      if( strstr(argv[nopt],"-dfspacetime") != NULL ){
         DBOPT("-dfspacetime") ;
         opt->dfilt_code = DFILT_SPACETIME ;
         if( strstr(argv[nopt],":0") != NULL ) opt->dfilt_code = DFILT_SPACETIME0 ;
         continue ;
      }
#endif

      /** -clean **/

      if( strncmp(argv[nopt],"-clean",5) == 0 ){
         DBOPT("-clean") ;
         opt->scale_output = FALSE;
         continue ;
      }

      /*** -flim  ==> output floating point images (default is shorts) ***/

      if( strncmp(argv[nopt],"-flim",5) == 0 ){
         DBOPT("-flim") ;
         opt->flim = TRUE ;
         continue ;
      }

      /*** -non  ==> don't normalize alpha image ***/

      if( strncmp(argv[nopt],"-non",4) == 0 ){
         DBOPT("-non") ;
         opt->norm_fim = FALSE ;
         continue ;
      }

      /*** -coef #  ==>  coefficient for normalizing alpha image ***/

      if( strncmp(argv[nopt],"-coe",4) == 0 ){
         DBOPT("-coef") ;
         if( ++nopt >= argc ) Syntax("-coef needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp <= 0 ){
            sprintf( err_note , "illegal -coef value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         opt->scale_fim = ftemp ;
         continue ;
      }

      /*** -list #  ==>  threshold for report listing (not used now) ***/

      if( strncmp(argv[nopt],"-list",4) == 0 ){
         DBOPT("-list") ;
         if( ++nopt >= argc ) Syntax("-list needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
#if 0
         if( ftemp <= 0 ){
            sprintf( err_note , "illegal -list value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
#else
         fprintf(stderr,"WARNING: -list option is ignored in fim2!\n") ;
#endif
         opt->thresh_report = ftemp ;
         continue ;
      }

      /*** -polref #  ==>  order of polynomial reference functions ***/

      if( strncmp(argv[nopt],"-polref",5) == 0 || strncmp(argv[nopt],"-polort",5) == 0 ){
         DBOPT("-polref") ;
         if( ++nopt >= argc ) Syntax("-polref needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp > 3 ){
            fprintf( stderr , "WARNING: large -polref value %f\n" , ftemp ) ;
         }
         polref = (int) ftemp ;
         continue ;
      }

      /*** -pcnt #  ==>  percent of deviation from perfect correlation ***/

      if( strncmp(argv[nopt],"-pcnt",4) == 0 ){
         DBOPT("-pcnt") ;
         if( ++nopt >= argc ) Syntax("-pcnt needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp < 0.0 || ftemp > 100.0 ){
            sprintf( err_note , "illegal -pcnt value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         opt->thresh_pcorr = 1.0 - 0.01 * ftemp ;
         continue ;
      }

      /*** -pcthresh #  ==>  actual threshold on \rho ***/

      if( strncmp(argv[nopt],"-pcthresh",5) == 0 ){
         DBOPT("-pcthresh") ;
         if( ++nopt >= argc ) Syntax("-pcthresh needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp < 0.0 || ftemp > 1.0 ){
            sprintf( err_note , "illegal -pcthresh value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         opt->thresh_pcorr = ftemp ;
         continue ;
      }

      /*** -im1 #  ==>  index (1...) of first image to actually use ***/

      if( strncmp(argv[nopt],"-im1",4) == 0 ){
         DBOPT("-im1") ;
         if( ++nopt >= argc ) Syntax("-im1 needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp < 1.0 ){
            sprintf( err_note , "illegal -im1 value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         first_im = (int)(ftemp+0.499) ;
         continue ;
      }

      /*** -num #  ==>  maximum number of images to use from command line ***/

      if( strncmp(argv[nopt],"-num",4) == 0 ){
         DBOPT("-num") ;
         if( ++nopt >= argc ) Syntax("-num needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp < 2 ){
            sprintf( err_note , "illegal -num value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         num_im = (int)(ftemp+0.499) ;
         continue ;
      }

#define OPENERS "[{/%"
#define CLOSERS "]}/%"

#define IS_OPENER(sss) (strlen((sss))==1 && strstr(OPENERS,(sss))!=NULL)
#define IS_CLOSER(sss) (strlen((sss))==1 && strstr(CLOSERS,(sss))!=NULL)

      /*** -ort file  ==>  reference time series ***/

      if( strncmp(argv[nopt],"-ort",4) == 0 ){
         DBOPT("-ort") ;
         if( ++nopt >= argc ) Syntax("-ort needs a filename") ;

         /** July 1995: read a bunch of orts using [ a b c ... ] format **/

         if( ! IS_OPENER(argv[nopt]) ){                                 /* one file */
            vec = RWC_read_time_series( argv[nopt] ) ;
            if( vec == NULL ){ fprintf( stderr , "cannot continue\a\n" ) ; exit(1) ; }
            ADDTO_TSARR( opt->refts , vec ) ;
         } else {                                                           /* many */
            for( nopt++ ; !IS_CLOSER(argv[nopt]) && nopt<argc ; nopt++ ){
               vec = RWC_read_time_series( argv[nopt] ) ;
               if( vec == NULL ){ fprintf( stderr , "cannot continue\a\n" ); exit(1) ; }
               ADDTO_TSARR( opt->refts , vec ) ;
            }
            if( nopt == argc ) Syntax("-ort never finishes!") ;
         }
         continue ;
      }

      /*** -ideal file  ==>  ideal response vector time series ***/

      if( strncmp(argv[nopt],"-ideal",5) == 0 ){
         DBOPT("-ideal") ;
         if( ++nopt >= argc ) Syntax("-ideal needs a filename") ;

         /** July 1995: read a bunch of ideals using [ a b c ... ] format **/

         if( ! IS_OPENER(argv[nopt]) ){                                 /* one file */
            ideal = RWC_read_time_series( argv[nopt] ) ;
            if( ideal == NULL ){ fprintf( stderr , "cannot continue\a\n" ); exit(1) ; }
            ADDTO_TSARR( opt->idts , ideal ) ;
         } else {                                                           /* many */
            for( nopt++ ; !IS_CLOSER(argv[nopt]) && nopt<argc ; nopt++ ){
               ideal = RWC_read_time_series( argv[nopt] ) ;
               if( ideal == NULL ){ fprintf( stderr , "cannot continue\a\n" ); exit(1) ; }
               ADDTO_TSARR( opt->idts , ideal ) ;
            }
            if( nopt == argc ) Syntax("-ideal never finishes!") ;
         }
         continue ;
      }

      /*** -weight file  ==>  weight vector time series ***/

      if( strncmp(argv[nopt],"-weight",5) == 0 ){
         DBOPT("-weight") ;
         if( ++nopt >= argc ) Syntax("-weight needs a filename") ;
         if( opt->weight != NULL ){
            fprintf( stderr , "cannot have two weight vectors!\a\n" ) ;
            exit(1) ;
         }
         opt->weight = RWC_read_time_series( argv[nopt] ) ;
         if( opt->weight == NULL ){ fprintf( stderr , "cannot continue\a\n" ); exit(1) ; }
         DBOPT("read in OK") ;
         continue ;
      }

      /*** -fimfile file  ==>  name of output file ***/

      if( strncmp(argv[nopt],"-fimfile",5) == 0 ){
         DBOPT("-fimfile") ;
         if( ++nopt >= argc ) Syntax("-fimfile needs a filename") ;
         if( opt->fname_fim != NULL ){
            fprintf( stderr , "cannot have two fim output files!\a\n" ) ;
            exit(1) ;
         }
         opt->fname_fim = argv[nopt] ;
         DBOPT("stored as fimfile") ;
         continue ;
      }

      /*** -corr  ==>  write correlation file as fimfile.CORR ***/

      if( strncmp(argv[nopt],"-corr",5) == 0 ){
         DBOPT("-corr") ;
         do_corr = TRUE ;
         continue ;
      }

      /*** -corfile file  ==>  write correlation file as this name ***/

      if( strncmp(argv[nopt],"-corfile",5)  == 0 ||
          strncmp(argv[nopt],"-corrfile",6) == 0   ){

         DBOPT("-corfile") ;
         if( ++nopt >= argc ) Syntax("-corfile needs a filename") ;
         if( opt->fname_corr != NULL ){
            fprintf( stderr , "cannot have two corr output files!\a\n" ) ;
            exit(1) ;
         }
         opt->fname_corr = argv[nopt] ;
         do_corr         = TRUE ;
         DBOPT("stored as corfile") ;
         continue ;
      }

      /*** -cnrfile file  ==>  write cnr file as this name ***/

      if( strncmp(argv[nopt],"-cnrfile",5)  == 0 ){

         DBOPT("-cnrfile") ;
         if( ++nopt >= argc ) Syntax("-cnrfile needs a filename") ;
         if( opt->fname_cnr != NULL ){
            fprintf( stderr , "cannot have two cnr output files!\a\n" ) ;
            exit(1) ;
         }
         opt->fname_cnr = argv[nopt] ;
         DBOPT("stored as cnrfile") ;
         continue ;
      }

      /*** -sigfile file  ==>  write sig file as this name ***/

      if( strncmp(argv[nopt],"-sigfile",5)  == 0 ){

         DBOPT("-sigfile") ;
         if( ++nopt >= argc ) Syntax("-sigfile needs a filename") ;
         if( opt->fname_sig != NULL ){
            fprintf( stderr , "cannot have two sig output files!\a\n" ) ;
            exit(1) ;
         }
         opt->fname_sig = argv[nopt] ;
         DBOPT("stored as sigfile") ;
         continue ;
      }

      /*** -fitfile file  ==>  write fit files as this name ***/

      if( strncmp(argv[nopt],"-fitfile",5)  == 0 ){

         DBOPT("-fitfile") ;
         if( ++nopt >= argc ) Syntax("-fitfile needs a filename") ;
         if( opt->fname_fit != NULL ){
            fprintf( stderr , "cannot have two fit output filenames!\a\n" ) ;
            exit(1) ;
         }
         opt->fname_fit = argv[nopt] ;
         DBOPT("stored as fitfile") ;
         continue ;
      }

      /*** -subort file ==> compute time series with orts removed ***/

      if( strncmp(argv[nopt],"-subort",5) == 0 ){

         DBOPT("-subort") ;
         if( ++nopt >= argc ) Syntax("-subort needs a filename") ;
         if( opt->fname_subort != NULL ){
            fprintf( stderr , "cannot have two subort output filenames!\a\n" ) ;
            exit(1) ;
         }
         opt->fname_subort = argv[nopt] ;
         DBOPT("stored as subortfile") ;
         continue ;
      }

      /*** get to here, and start with a '-'  ==>  bad news city, Arizona ***/

      if( strncmp(argv[nopt],"-",1) == 0 ){
         sprintf( err_note , "unknown option %s" , argv[nopt] ) ;
         Syntax(err_note) ;
      }

   /*** finished with switches.  Anything that makes it here is a filename ***/

      if( opt->idts->num == 0 ){   /* if ideal timeseries not given yet, is first */
         DBOPT("will be ideal") ;
         ideal = RWC_read_time_series( argv[nopt] ) ;
         if( ideal == NULL ){ fprintf( stderr , "cannot continue\a\n" ); exit(1) ; }
         ADDTO_TSARR( opt->idts , ideal ) ;
         continue ;
      }

      /*** this point is the start of image filenames;
           break out of the loop and process them       ***/

      DBOPT("1st image file") ;

      break ;   /* time to leave */

   } while( ++nopt < argc ) ;  /* loop until all args are read, or we break */

/***** when this loop ends, nopt = index of first image filename in argv[] *****/

   /*** check the situation for reasonabilityositiness ***/

   if( opt->idts->num == 0 ) Syntax("no ideal response vector is defined!") ;

   /*** compute the number of image files ***/

   num_time = argc - nopt ;                   /* # of arguments left */
   if( opt->fname_fim == NULL ){
      num_time -- ;                           /* one less, if need be */
      opt->fname_fim = argv[argc-1] ;         /* last name = output file */
   }

   /*** July 1995: read all images in now! ***/

   if( num_time < 1 ) Syntax("No input files given on command line?!") ;

   opt->ims = mri_read_many_files( num_time , argv+nopt ) ;
   if( opt->ims == NULL ) Syntax("Cannot read all images!") ;
   num_time = MIN( opt->ims->num , num_im ) ;
   if( num_time < 2 ) Syntax("must have at least 2 images to correlate!") ;
   opt->ntime = num_time ;
   opt->nxim  = opt->ims->imarr[0]->nx ;
   opt->nyim  = opt->ims->imarr[0]->ny ;

#ifdef DEBUG
   fprintf(stderr,"num_time = %d  nx = %d  ny = %d\n",num_time,opt->nxim,opt->nyim) ;
#endif

   /** convert images to float format **/

   for( ii=0 ; ii < num_time ; ii++ ){
      if( opt->ims->imarr[ii]->kind != MRI_float ){
         im = mri_to_float( opt->ims->imarr[ii] ) ;
         mri_free( opt->ims->imarr[ii] ) ;
         opt->ims->imarr[ii] = im ;
      }
      if( opt->ims->imarr[ii]->nx != opt->nxim ||
          opt->ims->imarr[ii]->ny != opt->nyim   ){

         fprintf(stderr,"** Image size mismatch at image # %d -- end of run!\a\n",ii+1) ;
         exit(1) ;
      }
   }

   if( first_im < 1 || first_im >= num_time ){
      sprintf( err_note ,
               "-im1 was %d, but only have %d images" , first_im , num_time ) ;
      Syntax(err_note) ;
   }

   /*** replace earliest images with copies of "first_im", if needed ***/

   for( ii=0 ; ii < first_im-1 ; ii++ ){
      im = mri_to_float( opt->ims->imarr[first_im-1] ) ;  /* copy data, not just pointer */
      mri_free( opt->ims->imarr[ii] ) ;
      opt->ims->imarr[ii] = im ;
   }

   if( do_corr && opt->fname_corr == NULL ){
#ifdef DEBUG
      fprintf(stderr,"creating .CORR filename\n");
#endif
      ii = strlen( opt->fname_fim ) + 7 ;
      opt->fname_corr = (char *) malloc( sizeof(char) * ii ) ;
      if( opt->fname_corr == NULL ) MALLOC_ERR(".CORR filename") ;
      strcpy( opt->fname_corr , opt->fname_fim ) ;
      strcat( opt->fname_corr , ".CORR" ) ;
   }

   /*** put the polynomial responses into opt->refts ***/

   if( polref >= 0 ){

#ifdef DEBUG
      fprintf(stderr,"creating polynomial references; polref=%d\n",polref);
#endif

      for( pp=0 ; pp <= polref ; pp++ ){
         vec = RWC_blank_time_series( num_time ) ;
#if 1
         if( pp == 0 ){
            for( ii=0 ; ii < num_time ; ii++ ) vec->ts[ii] = 1.0 ;
         } else {
            fscal = 1.0 / num_time ;
            for( ii=0 ; ii < num_time ; ii++ ) vec->ts[ii] = pow(fscal*ii,pp) ;
         }
#else
         ftemp = 0.5 * num_time - 0.4321 ;  /* 0.4321 ensures (ii-ftemp)!=0 */
         fscal = 2.0 / num_time ;           /* range of arg to pow is -1..1 */
         for( ii=0 ; ii < num_time ; ii++ )
            vec->ts[ii] = pow( fscal*(ii-ftemp) , pp ) ;
#endif

         ADDTO_TSARR( opt->refts , vec ) ;
      }
   }

/*** if no weight vector input (via -weight filename), make one up ***/

   if( opt->weight == NULL ){

#ifdef DEBUG
      fprintf(stderr,"creating default weight\n");
#endif

      vec = RWC_blank_time_series( num_time ) ;

      for( ii=0 ; ii < num_time   ; ii++ ) vec->ts[ii] = 1.0 ;  /* weight = all ones */

      opt->weight = vec ;
   }
   for( ii=0 ; ii < first_im-1 ; ii++ ) opt->weight->ts[ii] = 0.0 ;  /* except skipped images */

   /*** make space for the ideal vector in refts (but don't but it there yet) ***/

   ADDTO_TSARR( opt->refts , NULL ) ;

/*** check all time series for being long enough ***/

   bad = FALSE ;

   for( ii=0 ; ii < opt->refts->num-1 ; ii++ ){  /* note not checking last one */
#ifdef DEBUG
      fprintf(stderr,"checking ref %d for size\n",ii) ;
#endif
      if( opt->refts->tsarr[ii]->len < num_time ){
         fprintf( stderr ,
                  "reference vector %s has %d elements: too short!\a\n" ,
                  opt->refts->tsarr[ii]->fname , opt->refts->tsarr[ii]->len ) ;
         bad = TRUE ;
      }
   }

   for( ii=0 ; ii < opt->idts->num-1 ; ii++ ){
#ifdef DEBUG
      fprintf(stderr,"checking ideal %d for size\n",ii) ;
#endif
      if( opt->idts->tsarr[ii]->len < num_time ){
         fprintf( stderr ,
                  "ideal vector %s has %d elements: too short!\a\n" ,
                  opt->idts->tsarr[ii]->fname , opt->idts->tsarr[ii]->len ) ;
         bad = TRUE ;
      }
   }

   if( opt->weight->len < num_time ){
      fprintf( stderr ,
               "weight vector %s has %d elements: too short!\a\n" ,
               opt->weight->fname , opt->weight->len ) ;
      bad = TRUE ;
   }

   if( bad ) exit(1) ;

/*** zero out weight at any time where a refts time_series is blasted ***/

#ifdef DEBUG
   fprintf(stderr,"blasting away ... ") ;
#endif

   for( ii=0 ; ii < num_time ; ii++ ){

      bad = (opt->weight->ts[ii] >= BLAST) || (opt->weight->ts[ii] < 0.0) ;

      for( rr=0 ; !bad && rr < opt->refts->num-1 ; rr++ )
         bad = (opt->refts->tsarr[rr]->ts[ii] >= BLAST) ;

      if( bad ){
         opt->weight->ts[ii] = 0 ;
         for( rr=0 ; rr < opt->refts->num-1 ; rr++ )  /* note not checking last one */
            opt->refts->tsarr[rr]->ts[ii] = 0 ;
      }
   }

#ifdef DEBUG
   fprintf(stderr,"\n") ;
#endif

/*** check to see if the edited vectors are still nonzero enough ***/

   bad = FALSE ;

#ifdef DEBUG
   fprintf(stderr,"checking weight for nonnegativity\n") ;
#endif

   ftemp = RWC_norm_ts( num_time , opt->weight ) ;
   if( ftemp < 1.e-9 ){
      fprintf( stderr , "there is no time with nonzero weighting!\n" ) ;
      bad = TRUE ;
   }

   for( rr=0 ; rr < opt->refts->num-1 ; rr++ ){  /* note not checking last one */
#ifdef DEBUG
      fprintf(stderr,"checking ref %d for nonzeroness\n",rr) ;
#endif
      ftemp = RWC_norm_ts( num_time , opt->refts->tsarr[rr] ) ;
      if( ftemp < 1.e-9 ){
         fprintf( stderr , "reference vector %d is all zeroes\n" , rr+1 ) ;
         bad = TRUE ;
      }
   }
   if( bad ) exit(1) ;

   /*** June 1995: get first image with nonzero weighting ***/

   if( regbase == NULL ){
      for( ii=0 ; ii < num_time ; ii++ )
         if( opt->weight->ts[ii] != 0.0 && ideal->ts[ii] < BLAST ) break ;

      if( ii == num_time ){ fprintf(stderr,"FIRST_IM: scan error!\a\n");exit(1); }

      opt->first_flim = mri_to_float( opt->ims->imarr[ii] ) ;  /* copy it */
   } else {
      MRI_IMAGE * qim ;
      qim = mri_read_just_one( regbase ) ;
      if( qim == NULL ) Syntax("Couldn't read -regbase image!") ;
      if( qim->kind == MRI_float ) opt->first_flim = qim ;
      else {
         opt->first_flim = mri_to_float( qim ) ;
         mri_free( qim ) ;
      }
      if( opt->first_flim->nx != opt->nxim || opt->first_flim->ny != opt->nyim ){
         fprintf(stderr,"-regbase: image size mismatch!\a\n") ; exit(1) ;
      }
   }

   /*** Setup for differential filtering (registration) ***/

   if( opt->dfilt_code != DFILT_NONE ){
      int alcode ;
      MRI_IMARR * tims ;
      time_series * dxts , * dyts , * phits ;

      switch( opt->dfilt_code ){
         case DFILT_TIME:   alcode = 0                                       ; break ;

         case DFILT_TIME0:  alcode = ALIGN_NOITER_CODE                       ; break ;

         case DFILT_SPACETIME:
         case DFILT_BOTH:
         case DFILT_SPACE:  alcode = ALIGN_REGISTER_CODE                     ; break ;

         case DFILT_SPACETIME0:
         case DFILT_BOTH0:
         case DFILT_SPACE0: alcode = ALIGN_REGISTER_CODE | ALIGN_NOITER_CODE ; break ;

         default:
            Syntax("Internal error: opt->dfilt_code illegal!") ;
      }

      dxts  = RWC_blank_time_series( num_time ) ;  /* space for motion params */
      dyts  = RWC_blank_time_series( num_time ) ;
      phits = RWC_blank_time_series( num_time ) ;

      if( opt->reg_bilinear ) alcode |= ALIGN_BILINEAR_CODE ;
      if( opt->debug        ) alcode |= ALIGN_DEBUG_CODE ;

      tims = mri_align_dfspace( opt->first_flim , NULL , opt->ims ,
                                alcode , dxts->ts , dyts->ts , phits->ts ) ;

      switch( opt->dfilt_code ){

         case DFILT_TIME:
         case DFILT_TIME0:
            ADDTO_TSARR( opt->refts , NULL ) ;  /* make 3 blank spots at end */
            ADDTO_TSARR( opt->refts , NULL ) ;
            ADDTO_TSARR( opt->refts , NULL ) ;

            /* move previously existing time series up by 3 */

            for( ii=opt->refts->num-4 ; ii >=0 ; ii-- )
               opt->refts->tsarr[ii+3] = opt->refts->tsarr[ii] ;

            /* put dx,dy,phi at beginning of list */

            opt->refts->tsarr[0] = dxts ;
            opt->refts->tsarr[1] = dyts ;
            opt->refts->tsarr[2] = phits ;
         break ;

         case DFILT_SPACE:
         case DFILT_SPACE0:
            DESTROY_IMARR( opt->ims ) ;   /* put registered images in */
            opt->ims = tims ;             /* place of the input images */

#if 0
if( opt->debug ){
   char fff[64] ;
   for( ii=0 ; ii < IMARR_COUNT(opt->ims) ; ii++ ){
      sprintf(fff,"rrr.%04d",ii+1) ;
      mri_write( fff , IMARR_SUBIMAGE(opt->ims,ii) ) ;
   }
}
#endif

            RWC_free_time_series( dxts ) ;
            RWC_free_time_series( dyts ) ;
            RWC_free_time_series( phits ) ;
         break ;

         case DFILT_BOTH:
         case DFILT_BOTH0:
            ADDTO_TSARR( opt->refts , NULL ) ;
            ADDTO_TSARR( opt->refts , NULL ) ;
            ADDTO_TSARR( opt->refts , NULL ) ;

            for( ii=opt->refts->num-4 ; ii >=0 ; ii-- )
               opt->refts->tsarr[ii+3] = opt->refts->tsarr[ii] ;

            opt->refts->tsarr[0] = dxts ;
            opt->refts->tsarr[1] = dyts ;
            opt->refts->tsarr[2] = phits ;

            opt->rims = tims ;   /* keep registered images as a separate data set */
         break ;

         case DFILT_SPACETIME:
         case DFILT_SPACETIME0:
            ADDTO_TSARR( opt->refts , NULL ) ;
            ADDTO_TSARR( opt->refts , NULL ) ;
            ADDTO_TSARR( opt->refts , NULL ) ;

            for( ii=opt->refts->num-4 ; ii >=0 ; ii-- )
               opt->refts->tsarr[ii+3] = opt->refts->tsarr[ii] ;

            opt->refts->tsarr[0] = dxts ;
            opt->refts->tsarr[1] = dyts ;
            opt->refts->tsarr[2] = phits ;

            DESTROY_IMARR( opt->ims ) ;   /* put registered images in */
            opt->ims = tims ;             /* place of the input images */
         break ;
      }
   }

   /*** Done! ***/

   return ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** give some help and exit ***/

void Syntax( char *note )
{
   static char *mesg[] = {
   "Usage: fim2 [options] image_files ..." ,
   "where 'image_files ...' is a sequence of MRI filenames," ,
   " " ,
   "options are:",
   "-pcnt #         correlation coeff. threshold will be 1 - 0.01 * #",
   "-pcthresh #     correlation coeff. threshold will be #",
   "-im1 #          index of image file to use as first in time series;",
   "                  default is 1; previous images are filled with this",
   "                  image to synchronize with the reference time series",
   "-num #          number of images to actually use, if more than this",
   "                  many are specified on the command line;  default is",
   "                  to use all images",
   "-non            this option turns off the default normalization of",
   "                  the output activation image;  the user should provide",
   "                  a scaling factor via '-coef #', or '1' will be used",
   "-coef #         the scaling factor used to convert the activation output",
   "                  from floats to short ints (if -non is also present)",
   " ",
   "-ort fname      fname = filename of a time series to which the image data",
   "                  will be orthogonalized before correlations are computed;",
   "                  any number of -ort options (from 0 on up) may be used",
   "-ideal fname    fname = filename of a time series to which the image data",
   "                  is to be correlated; exactly one such time series is",
   "                  required;  if the -ideal option is not used, then the",
   "                  first filename after all the options will be used",
   "      N.B.: This version of fim2 allows the specification of more than",
   "            one ideal time series file.  Each one is separately correlated",
   "            with the image time series and the one most highly correlated",
   "            is selected for each pixel.  Multiple ideals are specified",
   "            using more than one '-ideal fname' option, or by using the",
   "            form '-ideal [ fname1 fname2 ... ]' -- this latter method",
   "            allows the use of wildcarded ideal filenames.",
   "            The '[' character that indicates the start of a group of",
   "            ideals can actually be any ONE of these: " OPENERS ,
   "            and the ']' that ends the group can be:  " CLOSERS ,
   " ",
   "      [Format of ort and ideal time series files:",
   "       ASCII; one number per line;",
   "       Same number of lines as images in the time series;",
   "       Value over 33333 --> don't use this image in the analysis]",
   " ",
   "-polref #       use polynomials of order 0..# as extra 'orts';",
   "[or -polort #]    default is 0 (yielding a constant vector).",
   "                  Use # = -1 to suppress this feature.",
#if 0
   "-weight fname   fname = filename of a times series to be used as weights",
   "                  in the correlation calculation;  all time series",
   "                  (orts, ideal, and image) will be weighted at time i",
   "                  by the weight at that time;  if not given, defaults to",
   "                  1 at all times (but any ort or ideal >= 33333 gives 0)",
#endif
   " ",
   "-fimfile fname  fname = filename to save activation magnitudes in;",
   "                  if not given, the last name on the command line will",
   "                  be used",
   "-corr           if present, indicates to write correlation output to",
   "                  image file 'fimfile.CORR' (next option is better)",
   "-corfile fname  fname = filename to save correlation image in;",
   "                  if not present, and -corr is not present, correlation",
   "                  image is not saved.",
   "-cnrfile fname  fname = filename to save contrast-to-noise image in;" ,
   "                  if not present, will not be computed or saved;" ,
   "                  CNR is scaled by 100 if images are output as shorts" ,
   "                  and is written 'as-is' if output as floats (see -flim)." ,
   "                  [CNR is defined here to be alpha/sigma, where" ,
   "                   alpha = amplitude of normalized ideal in a pixel" ,
   "                   sigma = standard deviation of pixel after removal" ,
   "                           of orts and ideal" ,
   "                   normalized ideal = ideal scaled so that trough-to-peak" ,
   "                     height is one.]" ,
   "-sigfile fname  fname = filename to save standard deviation image in;" ,
   "                  the standard deviation is of what is left after the" ,
   "                  least squares removal of the -orts, -polrefs, and -ideal." ,
   "                 N.B.: This is always output in the -flim format!" ,
   "-fitfile fname  Image files of the least squares fit coefficients of" ,
   "                  all the -ort and -polref time series that" ,
   "                  are projected out of the data time series before" ,
   "                  the -ideal is fit.  The actual filenames will" ,
   "                  be fname.01 fname.02 ...." ,
   "                  Their order is -orts, then -polrefs, and last -ideal." ,
   "                 N.B.: These are always output in the -flim format!" ,
   "-subort fname   A new timeseries of images is written to disk, with",
   "                  names of the form 'fname.0001', etc.  These images",
   "                  have the orts and polrefs (but not ideals) subtracted out.",
   "                 N.B.: These are always output in the -flim format!" ,
   "-flim           If present, write outputs in mrilib 'float' format,",
   "                  rather than scale+convert to integers",
   "                  [The 'ftosh' program can later convert to short integers]",
   "-clean          if present, then output images won't have the +/- 10000",
   "                  values forced into their corners for scaling purposes.",
   "-clip           if present, output correlations, etc., will be set to",
   "                  zero in regions of low intensity.",
   "-q              if present, indicates 'quiet' operation.",
   "-dfspace[:0]    Indicates to use the 'dfspace' filter (a la imreg) to",
   "                  register the images spatially before filtering." ,
#ifdef ALLOW_DFTIME
   "-dftime[:0]     Indicates to use the 'dftime' filter (a la imreg) to",
   "                  produce 3 additional orts, in an attempt to reduce",
   "                  motion artifacts.",
   "-dfboth[:0]     Indicates to use both -dftime and -dfspace (separately)," ,
   "                  then take as the resulting correlation the smaller of the",
   "                  two results in each pixel (the conservative approach: " ,
   "                  to be above -pcthresh, both calculations must 'hit')." ,
   "                  The resulting fim is the one corresponding to the chosen" ,
   "                  correlation in each pixel." ,
   "-dfspacetime:[0] Indicates to use -dfspace and then -dftime together",
   "                  (not separately) on the time series of images." ,
#endif
   "-regbase fname   Indicates to read image in file 'fname' as the base",
   "                  image for registration.  If not given, the first image",
   "                  in the time series that is used in the correlation",
   "                  computations will be used.  This is also the image",
   "                  that is used to define 'low intensity' for the -clip option.",
   " "
   } ;

   int nsize , ii ;

   if( note != NULL && (nsize=strlen(note)) > 0 ){
      fprintf(stderr,"\n") ;
      for(ii=0;ii<nsize;ii++) fprintf(stderr,"*") ;
      fprintf(stderr,"\a\n%s\n", note ) ;
      for(ii=0;ii<nsize;ii++) fprintf(stderr,"*") ;
      fprintf(stderr,"\ntry fim2 -help for instructions\a\n") ;
      exit(-1) ;
   }

   for( ii=0 ; ii < sizeof(mesg)/sizeof(char *) ; ii++ ){
      printf( " %s\n" , mesg[ii] );
   }
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

time_series * edit_weight( time_series * wt , time_series * ideal )
{
   time_series * wtnew ;
   int ii , ntime ;
   float ftemp ;

   ntime = MIN(wt->len,ideal->len) ;
   wtnew = RWC_blank_time_series( ntime ) ;

   for( ii=0 ; ii < ntime ; ii++ )
      wtnew->ts[ii] = (ideal->ts[ii] >= BLAST) ? (0.0) : (wt->ts[ii]) ;

   ftemp = RWC_norm_ts( ntime , wtnew ) ;
   if( ftemp < 1.e-9 ){
      fprintf(stderr,"** bad ideal: no times with nonzero weight!\n") ;
      RWC_free_time_series( wtnew ) ;
      return NULL ;
   }

   return wtnew ;
}
