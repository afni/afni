#undef MAIN

/****** This file contains routines related to warping datasets ******/
/****** and getting pieces of them into MRI_IMAGE structures.   ******/

#include "afni_warp.h"

#ifdef AFNI_DEBUG
#  define USE_TRACING
#  define PRINT_TRACING
#endif
#include "dbtrace.h"

/*------------------------------------------------------------------------
   Return a slice from a dataset, possibly to be warped on-the-fly
   from its parent:
     dset        = input dataset
     fixed_axis  = 1, 2, or 3 (for x,y,z)
     fixed_index = value of frozen index
     ival        = index of value (or sub-brick) to extract
     resam_mode  = used if "warp-on-the-fly" is used

   Output is in the MRI_IMAGE format, and will be
      nyy * nzz for fixed_axis = 1
      nzz * nxx for fixed_axis = 2
      nxx * nyy for fixed_axis = 3
--------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_dataset_slice( THD_3dim_dataset * dset ,
                                int fixed_axis , int fixed_index ,
                                int ival , int resam_mode )
{
   MRI_IMAGE * newim ;
   void * sar , * bar ;
   int nxx,nyy,nzz ;
   MRI_TYPE typ ;
   THD_dataxes * daxes ;
   THD_3dim_dataset * parent_dset ;
   THD_warp parent_to_child_warp ;

ENTRY("AFNI_dataset_slice") ;

   /*--------- sanity checks ---------*/

   if( ! ISVALID_3DIM_DATASET(dset) || fixed_index < 0 ||
       ival >= dset->dblk->nvals                         ) RETURN(NULL) ;

   /** must allow for ival < 0 --> return an empty (not NULL) image **/

   typ = (ival < 0 ) ? MRI_short : DSET_BRICK_TYPE(dset,ival) ;
   if( ! AFNI_GOOD_DTYPE(typ) ) RETURN(NULL) ;

   /*--------- brick dimensions ---------*/

   daxes = CURRENT_DAXES(dset) ;
   nxx   = daxes->nxx ;
   nyy   = daxes->nyy ;
   nzz   = daxes->nzz ;

   /*** June 1996: if resampling a threshold, always use NN ***/

   if( ival == DSET_THRESH_VALUE(dset) ) resam_mode = RESAM_NN_TYPE ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"nxx=%d nyy=%d nzz=%d",nxx,nyy,nzz) ;  STATUS(str) ;
  sprintf(str,"fixed_axis=%d fixed_index=%d ival=%d resam=%d",
          fixed_axis,fixed_index,ival,resam_mode ) ; STATUS(str) ; }
#endif

   /*--------- setup output image ---------*/

   switch( fixed_axis ){
      default: RETURN(NULL) ;  /* should not happen */

      /* fixed x --> image is y-z */

      case 1:
         if( fixed_index >= nxx ) RETURN(NULL) ;
         newim     = mri_new( nyy , nzz , typ ) ;
         newim->dx = fabs(daxes->yydel) ;
         newim->dy = fabs(daxes->zzdel) ;
         newim->dz = fabs(daxes->xxdel) ;
         sar       = mri_data_pointer( newim ) ;
      break ;

      /* fixed y --> image is z-x */

      case 2:
         if( fixed_index >= nyy ) RETURN(NULL) ;
         newim     = mri_new( nzz , nxx , typ ) ;
         newim->dx = fabs(daxes->zzdel) ;
         newim->dy = fabs(daxes->xxdel) ;
         newim->dz = fabs(daxes->yydel) ;
         sar       = mri_data_pointer( newim ) ;
      break ;

      /* fixed z --> image is x-y */

      case 3:
         if( fixed_index >= nzz ) RETURN(NULL) ;
         newim     = mri_new( nxx , nyy , typ ) ;
         newim->dx = fabs(daxes->xxdel) ;
         newim->dy = fabs(daxes->yydel) ;
         newim->dz = fabs(daxes->zzdel) ;
         sar       = mri_data_pointer( newim ) ;
      break ;
   }

   /** return empty image? **/

   if( ival < 0 ) RETURN(newim) ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"newim nx=%d ny=%d dx=%f dy=%f",
          newim->nx , newim->ny , newim->dx , newim->dy ) ;
  STATUS(str) ; }
#endif

   /*----- if datablock exists and not forcing warp-on-demand, use it -----*/

   if( !dset->wod_flag && DSET_INMEMORY(dset) ){

      bar = DSET_ARRAY(dset,ival) ;  /* pointer to data brick array */

      if( bar == NULL ){  /* if data needs to be loaded from disk */
         (void) THD_load_datablock( dset->dblk , THD_FREEUP ) ;
         bar = DSET_ARRAY(dset,ival) ;
         if( bar == NULL ){
            mri_free(newim) ;
            RETURN(NULL) ;  /* couldn't load data --> return nothing */
         }
      }

STATUS("reading from memory") ;

      switch( typ ){  /* copy data from brick (bar) to newim (sar) */

         default:              /* should not happen! */
            mri_free(newim) ;
            RETURN(NULL) ;

         case MRI_short:{
            AFNI_br2sl_short( nxx,nyy,nzz , fixed_axis,fixed_index ,
                              (short *) bar , (short *) sar         ) ;

            if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
               MRI_IMAGE * qim ;
               qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
               mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
            }
            RETURN(newim) ;
         }

         case MRI_float:{
            AFNI_br2sl_float( nxx,nyy,nzz , fixed_axis,fixed_index ,
                              (float *) bar , (float *) sar         ) ;

            if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
               MRI_IMAGE * qim ;
               qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
               mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
            }
            RETURN(newim) ;
         }

         case MRI_byte:{
            AFNI_br2sl_byte( nxx,nyy,nzz , fixed_axis,fixed_index ,
                             (byte *) bar , (byte *) sar           ) ;

            if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
               MRI_IMAGE * qim ;
               qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
               mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
            }
            RETURN(newim) ;
         }

         case MRI_complex:{
            AFNI_br2sl_complex( nxx,nyy,nzz , fixed_axis,fixed_index ,
                                (complex *) bar , (complex *) sar         ) ;
            RETURN(newim) ;
         }
      }
   } /* end of if on dataset brick existing */

   /*------------------ must warp from parent dataset -------------------*/

   if( dset->warp != NULL ){
STATUS("setting parent_to_child_warp to stored warp") ;
      parent_to_child_warp = *(dset->warp) ;
   } else {
STATUS("setting parent_to_child_warp to identity") ;
      parent_to_child_warp = IDENTITY_WARP ;  /* no warp ==> use identity */
   }

   if( dset->warp_parent != NULL ){
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"setting parent_dset to stored warp_parent=%p  this dset=%p",
          (void *) dset->warp_parent , (void *) dset ) ; STATUS(str) ; }
#endif
      parent_dset = dset->warp_parent ;
   } else {
STATUS("setting parent_dset to self, and parent_to_child_warp to identity") ;
      parent_dset = dset ;                    /* self-parenting */
      parent_to_child_warp = IDENTITY_WARP ;  /* use identity warp */
   }

   /*----- make the voxel-to-voxel warp, if needed -----*/

   if( ! ISVALID_WARP(dset->vox_warp) ){
      THD_warp * qwarp ;
      qwarp = AFNI_make_voxwarp( &parent_to_child_warp, parent_dset, dset ) ;

      if( dset->vox_warp == NULL ){    /* totally new */
         dset->vox_warp = qwarp ;
         ADDTO_KILL(dset->kl,dset->vox_warp) ;
      } else {
         *(dset->vox_warp) = *qwarp ;  /* just copy insides */
         myXtFree( qwarp ) ;
      }
   }

   if( DSET_ARRAY(parent_dset,ival) == NULL ){  /* reload from disk */
      Boolean good ;
      good = THD_load_datablock( parent_dset->dblk , THD_FREEUP ) ;
      if( ! good ){
         mri_free(newim) ;
         RETURN(NULL) ;  /* couldn't load data --> return nothing */
      }
   }

   bar = DSET_ARRAY(parent_dset,ival) ;

STATUS("warp-on-demand") ;

   /******************************************************************/
   /*** Select warp routine based on data type and slice direction ***/
   /**********   [See template routines in afni_slice.c]    **********/
   /******************************************************************/

   switch( typ ){

      default:               /** Illegal type: should not happen! **/
         mri_free(newim) ;
         RETURN(NULL) ;

      /**************************** short ****************************/
#undef  DTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  CUBIC_CLIP
#define DTYPE      short
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,DTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,DTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,DTYPE)
#define CUBIC_CLIP                                                         \
   if( resam_mode == RESAM_CUBIC_TYPE && ISVALID_STATISTIC(dset->stats) ){ \
     int ii , npix = newim->nx * newim->ny ;                               \
     DTYPE * ar = mri_data_pointer(newim) ;                                \
     DTYPE bot = dset->stats->bstat[ival].min ,                            \
           top = dset->stats->bstat[ival].max ;                            \
     if( bot < top ) for( ii=0 ; ii < npix ; ii++ )                        \
                        if( ar[ii] < bot ) ar[ii] = bot ;                  \
                   else if( ar[ii] > top ) ar[ii] = top ; }

      case TWO_TWO(MRI_,DTYPE):
      switch( fixed_axis ){

         case 1:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_XNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_XNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;

         case 2:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_YNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_YNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;

         case 3:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_ZNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_ZNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;
      }

      /**************************** float ****************************/
#undef  DTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  CUBIC_CLIP
#define DTYPE      float
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,DTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,DTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,DTYPE)
#define CUBIC_CLIP                                                         \
   if( resam_mode == RESAM_CUBIC_TYPE && ISVALID_STATISTIC(dset->stats) ){ \
     int ii , npix = newim->nx * newim->ny ;                               \
     DTYPE * ar = mri_data_pointer(newim) ;                                \
     DTYPE bot = dset->stats->bstat[ival].min ,                            \
           top = dset->stats->bstat[ival].max ;                            \
     if( bot < top ) for( ii=0 ; ii < npix ; ii++ )                        \
                        if( ar[ii] < bot ) ar[ii] = bot ;                  \
                   else if( ar[ii] > top ) ar[ii] = top ; }

      case TWO_TWO(MRI_,DTYPE):
      switch( fixed_axis ){

         case 1:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_XNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_XNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;

         case 2:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_YNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_YNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;

         case 3:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_ZNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_ZNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;
      }

      /**************************** byte ****************************/
#undef  DTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  CUBIC_CLIP
#define DTYPE      byte
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,DTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,DTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,DTYPE)
#define CUBIC_CLIP                                                         \
   if( resam_mode == RESAM_CUBIC_TYPE && ISVALID_STATISTIC(dset->stats) ){ \
     int ii , npix = newim->nx * newim->ny ;                               \
     DTYPE * ar = mri_data_pointer(newim) ;                                \
     DTYPE bot = dset->stats->bstat[ival].min ,                            \
           top = dset->stats->bstat[ival].max ;                            \
     if( bot < top ) for( ii=0 ; ii < npix ; ii++ )                        \
                        if( ar[ii] < bot ) ar[ii] = bot ;                  \
                   else if( ar[ii] > top ) ar[ii] = top ; }

      case TWO_TWO(MRI_,DTYPE):
      switch( fixed_axis ){

         case 1:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_XNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_XNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;

         case 2:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_YNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_YNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;

         case 3:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_ZNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_ZNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               if( DSET_BRICK_FACTOR(dset,ival) != 0.0 ){
                  MRI_IMAGE * qim ;
                  qim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,ival) , newim ) ;
                  mri_free(newim) ; newim = qim ;
STATUS("scaling slice to floats") ;
               }
               RETURN(newim) ;
         }
         break ;
      }

      /**************************** complex ****************************/
#undef  DTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  CUBIC_CLIP
#define DTYPE      complex
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,DTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,DTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,DTYPE)

      case TWO_TWO(MRI_,DTYPE):
      switch( fixed_axis ){

         case 1:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_XNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_XNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               RETURN(newim) ;
         }
         break ;

         case 2:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_YNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_YNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               RETURN(newim) ;
         }
         break ;

         case 3:{

            switch( dset->vox_warp->type ){

               case WARP_AFFINE_TYPE:{
                  LMAP_ZNAME( &(dset->vox_warp->rig_bod.warp) ,
                              resam_mode ,
                              parent_dset->daxes ,
                              (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
               }
               RETURN(newim) ;

               case WARP_TALAIRACH_12_TYPE:{
                  int iw ;
                  for( iw=0 ; iw < 12 ; iw++ )
                     LMAP_ZNAME( &(dset->vox_warp->tal_12.warp[iw]) ,
                                 resam_mode ,
                                 parent_dset->daxes ,
                                 (DTYPE *)bar , daxes , fixed_index , (DTYPE *)sar ) ;
                  }
               }
               RETURN(newim) ;
         }
         break ;
      }
      /**************************** DONE ****************************/
#undef  DTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  CUBIC_CLIP
   }  /** end of switch over legal types for warping **/

   /*--- should never reach this code! ---*/

   mri_free(newim) ; RETURN(NULL) ;
}

/*-------------------------------------------------------------
   a version of FD_brick_to_mri that allows on-the-fly warping
---------------------------------------------------------------*/

MRI_IMAGE * FD_warp_to_mri( int kslice , int ival , FD_brick * br )
{
   int ax_1 , ax_2 , ax_3 ;
   int fixed_axis , fixed_index , dsl_1 , dsl_2 , rot,mir;
   MRI_IMAGE * dsim , * flim ;
   THD_dataxes * daxes ;

ENTRY("FD_warp_to_mri") ;

   /*--- get the image from the dataset ---*/

   if( br == NULL || kslice < 0 ) RETURN(NULL) ;  /* should not happen! */

   ax_1 = br->a123.ijk[0] ;
   ax_2 = br->a123.ijk[1] ;
   ax_3 = br->a123.ijk[2] ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"kslice=%d ax_1=%d ax_2=%d ax_3=%d",
          kslice,ax_1,ax_2,ax_3) ;
  STATUS(str) ; }
#endif

   daxes = CURRENT_DAXES(br->dset) ;

   switch( ax_3 ){
      default: RETURN(NULL) ;

      case  1:  fixed_axis  = 1 ; dsl_1 = 2 ; dsl_2 = 3 ;
                fixed_index = kslice ; break ;

      case -1:  fixed_axis  = 1 ; dsl_1 = 2 ; dsl_2 = 3 ;
                fixed_index = daxes->nxx - kslice - 1 ; break ;

      case  2:  fixed_axis  = 2 ; dsl_1 = 3 ; dsl_2 = 1 ;
                fixed_index = kslice ; break ;

      case -2:  fixed_axis  = 2 ; dsl_1 = 3 ; dsl_2 = 1 ;
                fixed_index = daxes->nyy - kslice - 1 ; break ;

      case  3:  fixed_axis  = 3 ; dsl_1 = 1 ; dsl_2 = 2 ;
                fixed_index = kslice ; break ;

      case -3:  fixed_axis  = 3 ; dsl_1 = 1 ; dsl_2 = 2 ;
                fixed_index = daxes->nzz - kslice - 1 ; break ;
   }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"fixed_axis=%d fixed_index=%d dsl_1=%d dsl_2=%d",
          fixed_axis,fixed_index,dsl_1,dsl_2) ;
  STATUS(str) ; }
#endif

   dsim = AFNI_dataset_slice( br->dset , fixed_axis , fixed_index ,
                                         ival , br->resam_code ) ;

   if( dsim == NULL ) RETURN(NULL) ;

   /*--- the image may need to be flippo-ed to be
         in the orientation that the FD_brick wants ---*/

          if( dsl_1 ==  ax_1 && dsl_2 ==  ax_2 ){
      RETURN(dsim) ;

   } else if( dsl_1 ==  ax_1 && dsl_2 == -ax_2 ){
      rot = MRI_ROT_180 ;
      mir = True ;

   } else if( dsl_1 == -ax_1 && dsl_2 ==  ax_2 ){
      rot = MRI_ROT_0 ;
      mir = True ;

   } else if( dsl_1 == -ax_1 && dsl_2 == -ax_2 ){
      rot = MRI_ROT_180 ;
      mir = False ;

   } else if( dsl_1 ==  ax_2 && dsl_2 == -ax_1 ){
      rot = MRI_ROT_270 ;
      mir = False ;

   } else if( dsl_1 ==  ax_2 && dsl_2 ==  ax_1 ){
      rot = MRI_ROT_270 ;
      mir = True ;

   } else if( dsl_1 == -ax_2 && dsl_2 ==  ax_1 ){
      rot = MRI_ROT_90 ;
      mir = False ;

   } else if( dsl_1 == -ax_2 && dsl_2 == -ax_1 ){
      rot = MRI_ROT_90 ;
      mir = True ;

   } else {  /* should never happen! */

      fprintf(stderr,"\a\n*** dsl_1=%d dsl_2=%d ax_1=%d ax_2=%d\n",
              dsl_1,dsl_2,ax_1,ax_2 ) ;
      mri_free( dsim ) ;
      RETURN(NULL) ;
   }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"flipping output image: rot=%d mir=%d",rot,mir) ; STATUS(str) ;
  sprintf(str,"input to mri_flippo: nx=%d ny=%d type=%s",
          dsim->nx,dsim->ny,MRI_TYPE_name[dsim->kind]) ; STATUS(str) ; }
#endif

   flim = mri_flippo( rot , mir , dsim ) ;  /* will set dx,dy OK */

   if( dsim != flim ){
STATUS("freeing sliced image in favor of flipped image") ;
      mri_free( dsim ) ;
   }
   RETURN(flim) ;
}
