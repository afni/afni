/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*--------------------------------------------------------------------
  routine to edit an input dataset in place according to inputs
  in "edopt" (see editvol.h).

  Feb 1996: This routine is much more complex now due to the need to deal
            with byte, short, float, or complex data in sub-bricks.

  30 Nov 1997: added ability to edit a given sub-brick, using the
               edopt->iv_fim entry

  17 June 1998:  Modifications for erosion and dilation of clusters.
----------------------------------------------------------------------*/

void EDIT_one_dataset( THD_3dim_dataset *dset , EDIT_options *edopt )
{
   int   edit_thtoin   = edopt->thtoin ;       /* copy into local variables */
   int   edit_noneg    = edopt->noneg ;        /* for historical reasons    */
   int   edit_abs      = edopt->abss ;
   float edit_clip_bot = edopt->clip_bot ;     /* Nov 1995: changed to floats */
   float edit_clip_top = edopt->clip_top ;
   float edit_thresh   = edopt->thresh ;
   float edit_thbot    = edopt->thbot ;          /* 26 Dec 2007 */
   int   edit_clust    = edopt->edit_clust ;     /* 10 Sept 1996 */
   float clust_rmm     = edopt->clust_rmm ;
   float clust_vmul    = edopt->clust_vmul ;
   float erode_pv      = edopt->erode_pv;        /* 17 June 1998 */
   int   dilate        = edopt->dilate;          /* 17 June 1998 */
   int   filter_opt    = edopt->filter_opt;      /* 11 Sept 1996 */
   float filter_rmm    = edopt->filter_rmm;      /* 11 Sept 1996 */
   int   thrfilter_opt = edopt->thrfilter_opt;   /* 1 Oct 1996 */
   float thrfilter_rmm = edopt->thrfilter_rmm;   /* 1 Oct 1996 */
   float edit_blur     = edopt->blur ;
   float edit_thrblur  = edopt->thrblur;         /* 4 Oct 1996 */
   int   edit_scale    = edopt->scale ;
   float edit_mult     = edopt->mult ;
   int   edit_zvol     = edopt->do_zvol ;
   int   edit_ivfim    = edopt->iv_fim ;         /* 30 Nov 1997 */
   int   edit_ivthr    = edopt->iv_thr ;         /* 30 Nov 1997 */
   int   verbose       = edopt->verbose ;        /* 01 Nov 1999 */
   int   fake_dxyz     = edopt->fake_dxyz ;      /* 11 Sep 2000 */
   int   rank          = edopt->rank;            /* 13 Nov 2007 */
   int   edit_clip_unscaled = edopt->clip_unscaled ;  /* 09 Aug 1996 */

   THD_dataxes   *daxes ;
   short   *sfim = NULL , *sthr = NULL ;
   float   *ffim = NULL , *fthr = NULL ;
   complex *cfim = NULL ;
   byte    *bfim = NULL , *bthr = NULL ;
   void    *vfim = NULL , *vthr = NULL ;
   int nx,ny,nz,nxy,nxyz , jj,kk , ptmin , iclu,nclu , fim_max ;
   int iv_fim , iv_thr , fim_type , thr_type ;
   register int ii ;
   float dx,dy,dz , dxyz , rmm,vmul , val , vvv ;
   MCW_cluster_array *clar ;
   MCW_cluster       *blur=NULL ;
   int fimtype , thrtype ;
   float fimfac , thrfac ;

   /** get the data from this dataset **/

ENTRY("EDIT_one_dataset") ;

   THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;
   THD_load_datablock( dset->dblk ) ;

   if( !DSET_LOADED(dset) ){
      fprintf(stderr,
              "\n*** Cannot read data brick for dataset %s\a\n",
              DSET_BRIKNAME(dset) ) ;
      EXRETURN ;
   }

   /** load the data sub-brick indexes (iv_*) and check types for legality **/

   if( ISANAT(dset) ){
      if( edit_ivfim >= 0 && edit_ivfim < DSET_NVALS(dset) )  /* 30 Nov 1997 */
         iv_fim = edit_ivfim ;
      else
         iv_fim = ANAT_ival_zero[dset->func_type] ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: fim index = %d\n",iv_fim) ;

      fim_type = DSET_BRICK_TYPE(dset,iv_fim) ;
      fimfac   = DSET_BRICK_FACTOR(dset,iv_fim) ;
      if( edit_ivthr >= 0 && edit_ivthr < DSET_NVALS(dset) ){  /* 26 Dec 2007 */
         iv_thr   = edit_ivthr ;
         thr_type = DSET_BRICK_TYPE(dset,iv_thr) ;
         thrfac   = DSET_BRICK_FACTOR(dset,iv_thr) ;
         if( thrfac == 0.0f ) thrfac = 1.0f ;
      } else {
        iv_thr   = -1 ;            /* old code didn't allow thresholding */
        thr_type = ILLEGAL_TYPE ;  /* of an anatomical type dataset!    */
      }

      if( !AFNI_GOOD_DTYPE(fim_type) || fim_type == MRI_rgb ){
         fprintf(stderr,"\n*** Illegal anatomy data type %s in dataset %s\a\n" ,
                    MRI_type_name[fim_type] ,
                    dset->dblk->diskptr->brick_name ) ;
         EXRETURN ;
      }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"Anat dset: iv=%d type=%s fac=%g",iv_fim,MRI_TYPE_name[fim_type],fimfac) ;
  STATUS(str) ; }
#endif

   }

   if( ISFUNC(dset) ){
      if( edit_ivfim >= 0 && edit_ivfim < DSET_NVALS(dset) )  /* 30 Nov 1997 */
         iv_fim = edit_ivfim ;
      else
         iv_fim = FUNC_ival_fim[dset->func_type] ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: fim index = %d\n",iv_fim) ;

      fim_type = DSET_BRICK_TYPE(dset,iv_fim) ;
      fimfac   = DSET_BRICK_FACTOR(dset,iv_fim) ;

      if( edit_ivthr >= 0 && edit_ivthr < DSET_NVALS(dset) )  /* 30 Nov 1997 */
         iv_thr = edit_ivthr ;
      else
         iv_thr = FUNC_ival_thr[dset->func_type] ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: thr index = %d\n",iv_thr) ;

      if( iv_thr < 0 ){
         thr_type = ILLEGAL_TYPE ;
         thrfac   = 0.0 ;
      } else {
         thr_type = DSET_BRICK_TYPE(dset,iv_thr) ;
         thrfac   = DSET_BRICK_FACTOR(dset,iv_thr) ;
         if( thrfac == 0.0 ){
            switch( thr_type ){
               case MRI_short: thrfac = 1.0/FUNC_scale_short[dset->func_type]; break;
               case MRI_byte : thrfac = 1.0/FUNC_scale_byte [dset->func_type]; break;
            }
         }
      }

      if( !AFNI_GOOD_FUNC_DTYPE(fim_type) || fim_type == MRI_rgb ){
         fprintf(stderr,"\n*** Illegal functional data type %s in dataset %s\a\n" ,
                   MRI_type_name[fim_type], dset->dblk->diskptr->brick_name ) ;
         EXRETURN ;
      }

      if( thr_type >= 0 && (!AFNI_GOOD_FUNC_DTYPE(thr_type) || fim_type == MRI_rgb) ){
         fprintf(stderr,"\n*** Illegal threshold data type %s in dataset %s\a\n" ,
                    MRI_type_name[fim_type] , dset->dblk->diskptr->brick_name ) ;
         EXRETURN ;
      }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"Func dset: iv_fim=%d type=%s fac=%g",iv_fim,MRI_TYPE_name[fim_type],fimfac) ;
  STATUS(str) ;
  if( iv_thr >= 0 ){
  sprintf(str,"Func dset: iv_thr=%d type=%s fac=%g",iv_thr,MRI_TYPE_name[thr_type],thrfac) ;
  STATUS(str) ; } }
#endif

   }

   /** load the pointers to the sub-bricks **/

   vfim = DSET_ARRAY(dset,iv_fim) ;
   switch( fim_type ){
      default:
         fprintf(stderr,"\n*** Illegal data type in dataset %s\a\n",
                 dset->dblk->diskptr->brick_name ) ;
      EXRETURN ;

      case MRI_short:   sfim = (short *)   vfim ; break ;
      case MRI_float:   ffim = (float *)   vfim ; break ;
      case MRI_byte:    bfim = (byte *)    vfim ; break ;
      case MRI_complex: cfim = (complex *) vfim ; break ;
   }

   if( iv_thr >= 0 ){
      vthr = DSET_ARRAY(dset,iv_thr) ;
      switch( thr_type ){
         default:
            fprintf(stderr,"\n*** Illegal thresh data type in dataset %s\a\n",
                    dset->dblk->diskptr->brick_name ) ;
         EXRETURN ;

         case MRI_short:  sthr = (short *)vthr ; break ;
         case MRI_float:  fthr = (float *)vthr ; break ;
         case MRI_byte:   bthr = (byte *) vthr ; break ;
      }
   }

   /** load the grid parameters **/

   daxes = dset->daxes ;
   nx    = daxes->nxx ; dx = fabs(daxes->xxdel) ;
   ny    = daxes->nyy ; dy = fabs(daxes->yydel) ;
   nz    = daxes->nzz ; dz = fabs(daxes->zzdel) ;

   if( fake_dxyz ) dx = dy = dz = 1.0 ;  /* 11 Sep 2000 */

   nxy = nx * ny ; nxyz = nxy * nz ; dxyz = dx*dy*dz ;

   /*----- copy threshold over intensity? -----*/

STATUS("dataset loaded") ;

   if( edit_thtoin && iv_thr >= 0 ){
      float new_fimfac , scaling ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: copy thr over fim\n") ;

      /****
            Find scaling factors for various conversions (0 --> no scaling)
            scaling    = factor to actually scale data by when copying to new brick
            new_fimfac = factor to later scale data by when converting to floats
      ****/

      if( edit_thtoin == 2 ){
         new_fimfac = scaling = 0.0 ;  /** -2thtoin --> no scaling **/
      } else {
         switch( thr_type ){

         /** threshold datum is shorts **/

           case MRI_short:{
              switch( fim_type ){
                 case MRI_short:   /* fim datum is shorts --> no new scaling needed */
                    new_fimfac = thrfac ;
                    scaling    = 0.0 ;
                 break ;

                 case MRI_float:   /* fim datum is floats --> will be scaled properly */
                    new_fimfac = 0.0 ;
                    scaling    = thrfac ;
                 break ;

                 case MRI_byte:    /* fim datum is bytes */
                    new_fimfac = 1.0 / FUNC_scale_byte[dset->func_type] ;
                    scaling    = thrfac * FUNC_scale_byte[dset->func_type] ;
                 break ;
              }
           }
           break ;

           /** threshold datum is bytes **/

           case MRI_byte:{
              switch( fim_type ){
                 case MRI_short:   /* fim datum is shorts */
                    new_fimfac = 1.0 / FUNC_scale_short[dset->func_type] ;
                    scaling    = thrfac * FUNC_scale_short[dset->func_type] ;
                 break ;

                 case MRI_float:   /* fim datum is floats */
                    new_fimfac = 0.0 ;
                    scaling    = thrfac ;
                 break ;

                 case MRI_byte:    /* fim datum is bytes */
                    new_fimfac = thrfac ;
                    scaling    = 0.0 ;
                 break ;
              }
           }
           break ;

           /** threshold datum is floats **/

           case MRI_float:{
              switch( fim_type ){
                 case MRI_short:  /* fim datum is shorts */
                    new_fimfac = 1.0 / FUNC_scale_short[dset->func_type] ;
                    scaling    = FUNC_scale_short[dset->func_type] ;
                 break ;

                 case MRI_float:  /* fim datum is floats --> no scaling needed */
                    new_fimfac = 0.0 ;
                    scaling    = 0.0 ;
                 break ;

                 case MRI_byte:   /* fim datum is bytes */
                    new_fimfac = 1.0 / FUNC_scale_byte[dset->func_type] ;
                    scaling    = FUNC_scale_byte[dset->func_type] ;
                 break ;
              }
           }
           break ;
        }
      }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"thtoin: scaling=%f new_fimfac=%f input=%s output=%s",
          scaling,new_fimfac,MRI_TYPE_name[thr_type],MRI_TYPE_name[fim_type]) ;
  STATUS(str) ; }
#endif

      /** have scaling factors, so use them **/

      EDIT_coerce_scale_type( nxyz , scaling ,
                              thr_type , vthr , fim_type , vfim ) ;

      DSET_BRICK_FACTOR(dset,iv_fim) = fimfac = new_fimfac ;
   } /* end -1thtoin */

   /*----- non-negative? -----*/

   if( edit_noneg ){   /* meaningless for byte and complex */
STATUS("noneg") ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: remove negative values\n") ;

      switch( fim_type ){
         case MRI_short:
            for( ii=0 ; ii < nxyz ; ii++ ) if( sfim[ii] < 0 ) sfim[ii] = 0 ;
         break ;

         case MRI_float:
            for( ii=0 ; ii < nxyz ; ii++ ) if( ffim[ii] < 0 ) ffim[ii] = 0 ;
         break ;

         default:
STATUS("noneg applied to meaningless type: will be ignored") ;
      }
   }

   /*----- absolute? -----*/

   if( edit_abs ){   /* meaningless for byte */
STATUS("abs") ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: take absolute value\n") ;

      switch( fim_type ){
         case MRI_short:
            for( ii=0 ; ii < nxyz ; ii++ ) if( sfim[ii] < 0 ) sfim[ii] = -sfim[ii] ;
         break ;

         case MRI_float:
            for( ii=0 ; ii < nxyz ; ii++ ) if( ffim[ii] < 0 ) ffim[ii] = -ffim[ii] ;
         break ;

         case MRI_complex:
            for( ii=0 ; ii < nxyz ; ii++ ){
               cfim[ii].r = CABS(cfim[ii]) ; cfim[ii].i = 0.0 ;
            }
         break ;

         default:
STATUS("abs applied to meaningless type: will be ignored") ;
      }
   }

   /*----- clip? -----*/

   if( edit_clip_bot < edit_clip_top ){

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: clip fim values\n") ;

      switch( fim_type ){
         case MRI_short:{
            int top , bot ;
            float ftop,fbot ;
            if( fimfac > 0.0 && ! edit_clip_unscaled ){
               ftop = edit_clip_top / fimfac ;
               fbot = edit_clip_bot / fimfac ;
            } else {
               ftop = edit_clip_top ;
               fbot = edit_clip_bot ;
            }

            top = rint(ftop) ;  /* this code was modifed 28 Sep 1998 */
            if( top >=  MRI_maxshort ) top =   MRI_maxshort + 1  ;
            if( top <= -MRI_maxshort ) top = -(MRI_maxshort + 1) ;

            bot = rint(fbot) ;
            if( bot >=  MRI_maxshort ) bot =   MRI_maxshort + 1  ;
            if( bot <= -MRI_maxshort ) bot = -(MRI_maxshort + 1) ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"clipping short from %d to %d",bot,top) ;
  STATUS(str) ; }
#endif
            for( ii=0 ; ii < nxyz ; ii++ )
               if( sfim[ii] > bot && sfim[ii] < top ) sfim[ii] = 0 ;
         }
         break ;

         case MRI_byte:{
            int top , bot ;
            float ftop,fbot ;
            if( fimfac > 0.0 && ! edit_clip_unscaled ){
               ftop = edit_clip_top / fimfac ;
               fbot = edit_clip_bot / fimfac ;
            } else {
               ftop = edit_clip_top ;
               fbot = edit_clip_bot ;
            }

            top = rint(ftop) ;
            if( top >=  MRI_maxbyte ) top =   MRI_maxbyte + 1  ;
            if( top <= -MRI_maxbyte ) top = -(MRI_maxbyte + 1) ;

            bot = rint(fbot) ;
            if( bot >=  MRI_maxbyte ) bot =   MRI_maxbyte + 1  ;
            if( bot <= -MRI_maxbyte ) bot = -(MRI_maxbyte + 1) ;

            if( bot < 0 )   bot = 0 ;
            if( top < bot ) top = bot ;
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"clipping byte from %d to %d",bot,top) ;
  STATUS(str) ; }
#endif
            for( ii=0 ; ii < nxyz ; ii++ )
               if( bfim[ii] > bot && bfim[ii] < top ) bfim[ii] = 0 ;
         }
         break ;

         case MRI_float:{
            float top , bot ;
            if( fimfac > 0.0 && ! edit_clip_unscaled ){
               top = edit_clip_top / fimfac ;
               bot = edit_clip_bot / fimfac ;
            } else {
               top = edit_clip_top ;
               bot = edit_clip_bot ;
            }
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"clipping float from %g to %g",bot,top) ;
  STATUS(str) ; }
#endif
            for( ii=0 ; ii < nxyz ; ii++ )
               if( ffim[ii] > bot && ffim[ii] < top ) ffim[ii] = 0.0 ;
         }
         break ;

         case MRI_complex:{
            float val ;
            float top , bot ;
            if( fimfac > 0.0 && ! edit_clip_unscaled ){
               top = edit_clip_top / fimfac ;
               bot = edit_clip_bot / fimfac ;
            } else {
               top = edit_clip_top ;
               bot = edit_clip_bot ;
            }
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"clipping complex from %g to %g",bot,top) ;
  STATUS(str) ; }
#endif
            for( ii=0 ; ii < nxyz ; ii++ ){
               val = CABS(cfim[ii]) ;
               if( val > bot && val < top ) cfim[ii].r = cfim[ii].i = 0.0 ;
            }
         }
         break ;
      }
   }

   /*----- apply threshold? -----*/

   if( iv_thr >= 0 && (edit_thresh > 0.0 || edit_thresh > edit_thbot) ){

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: apply threshold\n") ;

      switch( thr_type ){

         /** threshold datum is shorts **/

         case MRI_short:{
            short thrplu , thrmin ;
            float fplu = edit_thresh / thrfac ;
            float fmin = edit_thbot  / thrfac ;
            thrplu = SHORTIZE(fplu) ; thrmin = SHORTIZE(fmin) ;
            switch( fim_type ){
               case MRI_short:   /* fim datum is shorts */
                 for( ii=0 ; ii < nxyz ; ii++ )
                   if( sthr[ii] < thrplu && sthr[ii] > thrmin ){ sfim[ii] = 0; }
               break ;

               case MRI_byte:    /* fim datum is bytes */
                 for( ii=0 ; ii < nxyz ; ii++ )
                   if( sthr[ii] < thrplu && sthr[ii] > thrmin ){ bfim[ii] = 0; }
               break ;

               case MRI_float:   /* fim datum is floats */
                 for( ii=0 ; ii < nxyz ; ii++ )
                   if( sthr[ii] < thrplu && sthr[ii] > thrmin ){ ffim[ii] = 0.0; }
               break ;

               case MRI_complex: /* fim datum is complex */
                 for( ii=0 ; ii < nxyz ; ii++ )
                   if( sthr[ii] < thrplu && sthr[ii] > thrmin ){
                     cfim[ii].r = cfim[ii].i = 0.0 ;
                   }
               break ;
            }
         }
         break ;

         /** threshold datum is bytes **/

         case MRI_byte:{
            byte thrplu , thrmin ;
            float fplu = edit_thresh / thrfac ;
            float fmin = edit_thbot  / thrfac ;
            thrplu = BYTEIZE(fplu) ; thrmin = BYTEIZE(fmin) ;
            switch( fim_type ){
              case MRI_short:   /* fim datum is shorts */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( bthr[ii] < thrplu && (thrmin == 0 || bthr[ii] > thrmin) ){
                    sfim[ii] = 0;
                  }
              break ;

              case MRI_byte:    /* fim datum is bytes */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( bthr[ii] < thrplu && (thrmin == 0 || bthr[ii] > thrmin) ){
                     bfim[ii] = 0;
                  }
              break ;

              case MRI_float:   /* fim datum is floats */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( bthr[ii] < thrplu && (thrmin == 0 || bthr[ii] > thrmin) ){
                     ffim[ii] = 0.0;
                  }
              break ;

              case MRI_complex: /* fim datum is complex */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( bthr[ii] < thrplu && (thrmin == 0 || bthr[ii] > thrmin) ){
                    cfim[ii].r = cfim[ii].i = 0.0 ;
                  }
              break ;
            }
         }
         break ;

         /** threshold datum is floats **/

         case MRI_float:{
            float thrplu , thrmin ;
            thrplu = edit_thresh ; if( thrfac > 0.0 ) thrplu /= thrfac ;
            thrmin = edit_thbot  ; if( thrfac > 0.0 ) thrmin /= thrfac ;
            switch( fim_type ){
              case MRI_short:   /* fim datum is shorts */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( fthr[ii] < thrplu && fthr[ii] > thrmin ){ sfim[ii] = 0 ;  }
              break ;

              case MRI_byte:    /* fim datum is bytes */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( fthr[ii] < thrplu && fthr[ii] > thrmin ){ bfim[ii] = 0 ;  }
              break ;

              case MRI_float:   /* fim datum is floats */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( fthr[ii] < thrplu && fthr[ii] > thrmin ){ ffim[ii] = 0.0 ;  }
              break ;

              case MRI_complex: /* fim datum is complex */
                for( ii=0 ; ii < nxyz ; ii++ )
                  if( fthr[ii] < thrplu && fthr[ii] > thrmin ){
                    cfim[ii].r = cfim[ii].i = 0.0 ;
                  }
              break ;
            }
         }
         break ;
      }
   }

   /*----- blur? -----*/

   if( AFNI_yesenv("AFNI_BLUR_FFT") ) EDIT_blur_allow_fir(0) ;  /* 04 Oct 2005 */

   if( edit_blur > 0.0 ){

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: blurring fim\n") ;

      EDIT_blur_volume( nx,ny,nz, dx,dy,dz , fim_type,vfim , edit_blur ) ;
   }

   /*----- threshold blur? -----*/   /* 4 Oct 1996 */
   if(( edit_thrblur > 0.0) && (vthr != NULL) ){

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: blurring threshold\n") ;

      EDIT_blur_volume( nx,ny,nz, dx,dy,dz , thr_type,vthr , edit_thrblur ) ;
   }


   /*----- zvol? -----*/

   if( edit_zvol ){
      THD_ivec3 iv1 , iv2 ;
      int ix1,ix2 , jy1,jy2 , kz1,kz2 , jj,kk ;

      iv1 = THD_3dmm_to_3dind(dset,TEMP_FVEC3(edopt->zv_x1,edopt->zv_y1,edopt->zv_z1));
      iv2 = THD_3dmm_to_3dind(dset,TEMP_FVEC3(edopt->zv_x2,edopt->zv_y2,edopt->zv_z2));

      ix1 = iv1.ijk[0] ; ix2 = iv2.ijk[0] ;
      jy1 = iv1.ijk[1] ; jy2 = iv2.ijk[1] ;
      kz1 = iv1.ijk[2] ; kz2 = iv2.ijk[2] ;

      if( ix1 > ix2 ){ ii=ix1 ; ix1=ix2 ; ix2=ii ; }
      if( jy1 > jy2 ){ ii=jy1 ; jy1=jy2 ; jy2=ii ; }
      if( kz1 > kz2 ){ ii=kz1 ; kz1=kz2 ; kz2=ii ; }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"edit_zvol: x1=%g x2=%g y1=%g y2=%g z1=%g z2=%g",
          edopt->zv_x1,edopt->zv_x2,edopt->zv_y1,edopt->zv_y2,edopt->zv_z1,edopt->zv_z2) ;
  STATUS(str) ;
  sprintf(str,"         : ix1=%d ix2=%d jy1=%d jy2=%d kz1=%d kz2=%d",
          ix1,ix2,jy1,jy2,kz1,kz2) ;
  STATUS(str) ; }
#endif

      if( verbose )
         fprintf(stderr,"--- EDIT_one_dataset: zeroing indexes [%d,%d]x[%d,%d]x[%d,%d]\n",
                 ix1,ix2,jy1,jy2,kz1,kz2 ) ;

      for( kk=kz1 ; kk <= kz2 ; kk++ ){
         for( jj=jy1 ; jj <= jy2 ; jj++ ){
            switch( fim_type ){
               case MRI_short:
                  for( ii=ix1 ; ii <= ix2 ; ii++ ) sfim[ii+jj*nx+kk*nxy] = 0 ;
               break ;

               case MRI_byte:
                  for( ii=ix1 ; ii <= ix2 ; ii++ ) bfim[ii+jj*nx+kk*nxy] = 0 ;
               break ;

               case MRI_float:
                  for( ii=ix1 ; ii <= ix2 ; ii++ ) ffim[ii+jj*nx+kk*nxy] = 0 ;
               break ;

               case MRI_complex:
                  for( ii=ix1 ; ii <= ix2 ; ii++ )
                     cfim[ii+jj*nx+kk*nxy].r = cfim[ii+jj*nx+kk*nxy].i = 0 ;
               break ;
            }
         }
      }
   }

   /*----- form clusters? -----*/

   rmm  = clust_rmm ;
   vmul = clust_vmul ;

   if( rmm >= 0.0 ){       /* do clustering? */

      MCW_cluster_array *clbig ;
      MCW_cluster *cl ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: clustering with rmm=%g vmul=%g\n",
                            rmm,vmul ) ;

     /*----- Erosion and dilation of clusters -----*/   /* 17 June 1998 */
     if (erode_pv > 0.0)
       MCW_erode_clusters (nx, ny, nz, dx, dy, dz, fim_type, vfim, rmm,
                           erode_pv, dilate);


STATUS("clustering") ;

      if( vmul >= 0.0 )
        ptmin = (int)( vmul / dxyz + 0.99 ) ;
      else
        ptmin = (int) fabs(vmul) ;  /* 30 Apr 2002 */

      vmul = MAX(1,ptmin) * dxyz ;  /* for use below */

      clar  = MCW_find_clusters( nx,ny,nz , dx,dy,dz , fim_type,vfim , rmm ) ;
      nclu  = 0 ;

      if( clar != NULL ){
         INIT_CLARR(clbig) ;
         for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
            cl = clar->clar[iclu] ;
            if( cl->num_pt >= ptmin ){ /* big enough */
               ADDTO_CLARR(clbig,cl) ;    /* copy pointer */
               clar->clar[iclu] = NULL ;  /* null out original */
               nclu++ ;
            }
         }
         DESTROY_CLARR(clar) ;
         clar = clbig ;
         if( nclu == 0 || clar == NULL || clar->num_clu == 0 ){
            printf("*** NO CLUSTERS FOUND ***\n") ;
            if( clar != NULL ) DESTROY_CLARR(clar) ;
            EXRETURN ;
         }
         SORT_CLARR(clar) ;
      }

      if( nclu == 0 ){  /* no data left */
STATUS("no data left after cluster edit!") ;
         DESTROY_CLARR(clar) ;
         EXRETURN ;
      }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"number clusters = %d",nclu) ; STATUS(str) ; }
#endif

      /*----- edit clusters? -----*/   /* 10 Sept 1996 */
      if (edit_clust > ECFLAG_SAME)
         EDIT_cluster_array (clar, edit_clust, dxyz, vmul);
      if (edit_clust == ECFLAG_SIZE || edit_clust == ECFLAG_ORDER)
         DSET_BRICK_FACTOR(dset,iv_fim) = 0.0;

      for( iclu=0 ; iclu < clar->num_clu ; iclu++ )
         if( clar->clar[iclu] != NULL && clar->clar[iclu]->num_pt > 0 ){
            MCW_cluster_to_vol( nx,ny,nz , fim_type,vfim , clar->clar[iclu] ) ;
         } else {
         }

      DESTROY_CLARR(clar) ;
   }


   /*----- filter? -----*/   /* 11 Sept 1996 */
   if (filter_opt > FCFLAG_NONE){

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: filtering fim\n") ;

      EDIT_filter_volume (nx, ny, nz, dx, dy, dz, fim_type, vfim,
                          filter_opt, filter_rmm ,
                          edopt->fmask,edopt->fmclip , edopt->fexpr );
   }


   /*----- threshold filter? -----*/   /* 1 Oct 1996 */
   if ((thrfilter_opt > FCFLAG_NONE) && (vthr != NULL)){

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: filtering thr\n") ;

      EDIT_filter_volume (nx, ny, nz, dx, dy, dz, thr_type, vthr,
                          thrfilter_opt, thrfilter_rmm ,
                          edopt->fmask,edopt->fmclip , edopt->fexpr );
   }


   /*----- scale? -----*/

#ifdef ALLOW_SCALE_TO_MAX
   if( edit_scale ){
STATUS("scale") ;

      if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: scaling fim to max\n") ;

      MCW_scale_to_max( nx,ny,nz , fim_type , vfim ) ;
   }
#endif

   /*----- mult? -----*/
   /*--- correction for scaling of short and byte bricks (13 Sept. 1996) ---*/

   if( edit_mult != 0.0 ){
STATUS("mult") ;

    if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: multiplying fim\n") ;

     switch( fim_type ){
        case MRI_short:
           if (fimfac > 0)
              DSET_BRICK_FACTOR(dset,iv_fim) =
                 DSET_BRICK_FACTOR(dset,iv_fim) * edit_mult ;
           else
              for( ii=0 ; ii < nxyz ; ii++ ) sfim[ii] *= edit_mult ;
        break ;

        case MRI_byte :
           if (fimfac > 0)
              DSET_BRICK_FACTOR(dset,iv_fim) =
                 DSET_BRICK_FACTOR(dset,iv_fim) * edit_mult ;
           else
              for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] *= edit_mult ;
        break ;

        case MRI_float: for( ii=0 ; ii < nxyz ; ii++ ) ffim[ii] *= edit_mult ;
        break ;

        case MRI_complex: for( ii=0 ; ii < nxyz ; ii++ )
                             cfim[ii].r *= edit_mult , cfim[ii].i *= edit_mult ;
        break ;
      }
   }

   /*----- 17 Sep 1998: conversion to z-score? -----*/

   if( edopt->zscore ){                          /* 07 Jun 1999! How did this get lost? */
     int kv = DSET_BRICK_STATCODE(dset,iv_fim) ;
     float par[2] ;

     if( FUNC_IS_STAT(kv) && kv != FUNC_ZT_TYPE ){

#if 0
fprintf(stderr," -1zscore: converting\n") ;
#endif

       if( verbose ) fprintf(stderr,"--- EDIT_one_dataset: converting to zscore\n") ;

        EDIT_zscore_vol( nxyz , fim_type , fimfac , vfim ,
                         kv , DSET_BRICK_STATAUX(dset,iv_fim) ) ;

        if( ISBUCKET(dset) ){

#if 0
fprintf(stderr," -1zscore: bucketing\n") ;
#endif

           par[0] = FUNC_ZT_TYPE ;
           par[1] = 0 ;
           EDIT_dset_items( dset , ADN_brick_stataux_one+iv_fim,par , ADN_none ) ;

        } else if( ISFUNC(dset)                  &&
                   FUNC_IS_STAT(dset->func_type) &&
                   iv_fim == FUNC_ival_thr[dset->func_type]  ){

#if 0
fprintf(stderr," -1zscore: retyping\n") ;
#endif

           dset->func_type   = FUNC_ZT_TYPE ;
           dset->stat_aux[0] = 0.0 ;

        } else {
           fprintf(stderr,"*** -1zscore error: non-bucket & non-func!\n") ;
        }

        if( fim_type == MRI_short )
           DSET_BRICK_FACTOR(dset,iv_fim) = 1.0 / FUNC_ZT_SCALE_SHORT ;
      }
   }

   /* turn dset to rank */
   if ( edopt->rank) {
      if( verbose ) 
         fprintf(stderr,"--- EDIT_one_dataset: Converting to rank value.\n");
      if (!(THD_unique_rank_edit(dset ,
                              iv_fim,
                              edopt->fmask,
                              edopt->rankmapname))) {
         fprintf(stderr,"*** Ranking error.\n");
      }
   }
   /*------ DONE! -----*/

   EXRETURN ;
}
