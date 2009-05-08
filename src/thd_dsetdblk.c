/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------*/
static int allow_nodata = 0 ;  /* 23 Mar 2001 */

void THD_allow_empty_dataset( int n ){ allow_nodata = n ; }
/*---------------------------------------------------------------*/

/*! With this macro defined, many of the attributes will actually
    be set in function THD_datablock_apply_atr() instead of herein.
    This change is to allow attributes read after the dataset struct
    is already created to be useful, for the NIfTI-ization project. */

#define USE_APPLICATOR  /* 10 May 2005 */

extern int Matvec_2_WarpData(ATR_float  *atr_flo, THD_affine_warp *ww, float *wdv);
extern int THD_WarpData_From_3dWarpDrive(THD_3dim_dataset *dset, ATR_float *atr_flt);

/*-------------------------------------------------------------------*/
/*!  Given a datablock, make it into a 3D dataset if possible.
---------------------------------------------------------------------*/

THD_3dim_dataset * THD_3dim_from_block( THD_datablock *blk )
{
   THD_3dim_dataset *dset ;
   THD_diskptr      *dkptr ;
   THD_dataxes      *daxes ;

   Boolean dset_ok = True ;
   int iq ;

   ATR_int    *atr_int ;
   ATR_string *atr_str ;
   ATR_float  *atr_flo ;

#if 0
   int new_idcode = 0 ;   /* no longer needed */
#endif

ENTRY("THD_3dim_from_block") ; /* 29 Aug 2001 */

   /* sanity check */

   if( ! ISVALID_DATABLOCK(blk) || ! ISVALID_DISKPTR(blk->diskptr) )
      RETURN( NULL );

   /*-- initialize a new 3D dataset --*/

   dset       = myXtNew( THD_3dim_dataset ) ;  /* uses XtCalloc() */
   dset->dblk = blk  ;
   dkptr      = blk->diskptr ;

   if( PRINT_TRACING ){
     char str[256] ;
     sprintf(str,"rank=%d nvals=%d dim[0]=%d dim[1]=%d dim[2]=%d",
             dkptr->rank , dkptr->nvals ,
             dkptr->dimsizes[0] , dkptr->dimsizes[1] , dkptr->dimsizes[2] ) ;
     STATUS(str) ;
   }

   INIT_KILL(dset->kl) ;
   ADDTO_KILL(dset->kl,blk) ;

   blk->parent  = (XtPointer) dset ;
   dset->parent = NULL ;

   daxes = dset->daxes = myXtNew(THD_dataxes) ;
   daxes->parent = (XtPointer) dset ;

   dset->wod_daxes = NULL ;  /* 02 Nov 1996 */

   dset->wod_flag    = False ;  /* set special flags */
   dset->death_mark  = 0 ;
   dset->tcat_list   = NULL ;
   dset->tcat_num    = 0 ;
   dset->tcat_len    = NULL ;

   ADDTO_KILL(dset->kl,daxes) ;

   dset->stats  = NULL ;
#ifdef ALLOW_DATASET_VLIST
   dset->pts    = NULL ;
#endif
   dset->taxis  = NULL ;
   dset->tagset = NULL ;   /* 23 Oct 1998 */

   /*------------------*/
   /*-- check for 3D --*/
   /*------------------*/

   if( dkptr->rank != 3 ) DSET_ERRN("illegal # of dimensions",dkptr->rank) ;

   /*--------------------------------------------------*/
   /*-- find type of image from TYPESTRING attribute --*/
   /*--------------------------------------------------*/

   atr_str = THD_find_string_atr( blk , ATRNAME_TYPESTRING ) ;
   if( atr_str == NULL ){
      DSET_ERR("no TYPESTRING") ;
      dset->type = -1 ;
   } else {
      int type ;
      for( type=FIRST_3DIM_TYPE ; type <= LAST_3DIM_TYPE ; type++ )
         if( strcmp( atr_str->ch , DATASET_typestr[type] ) == 0 ) break ;

      if( type > LAST_3DIM_TYPE ) DSET_ERR("illegal TYPESTRING") ;
      dset->type = type ;
   }

   /*-------------------------------------------------------*/
   /*-- find view_type and func_type from SCENE_TYPE data --*/
   /*-------------------------------------------------------*/

   atr_int = THD_find_int_atr( blk , ATRNAME_SCENE_TYPE ) ;
   if( atr_int == NULL ){
      DSET_ERR("missing or illegal SCENE_TYPE") ;
   } else {
      dset->view_type = atr_int->in[0] ;
      dset->func_type = atr_int->in[1] ;

      if( dset->type != atr_int->in[2] ){
         DSET_ERR("non-matching SCENE_TYPE[2]") ;
      }
   }

   /*-------------------------------------------------------*/
   /*--               find identifier codes               --*/
   /*-------------------------------------------------------*/

#ifndef USE_APPLICATOR
   atr_str = THD_find_string_atr( blk , ATRNAME_IDSTRING ) ;
   if( atr_str != NULL ){
     MCW_strncpy( dset->idcode.str , atr_str->ch , MCW_IDSIZE ) ;
     atr_str = THD_find_string_atr( blk , ATRNAME_IDDATE ) ;
     if( atr_str == NULL )
       MCW_strncpy( dset->idcode.date , "None" , MCW_IDDATE ) ;
     else
       MCW_strncpy( dset->idcode.date , atr_str->ch , MCW_IDDATE ) ;
   }
#endif

   ZERO_IDCODE(dset->anat_parent_idcode) ;
   ZERO_IDCODE(dset->warp_parent_idcode) ;

#ifndef USE_APPLICATOR
   atr_str = THD_find_string_atr( blk , ATRNAME_IDANATPAR ) ;
   if( atr_str != NULL )
      MCW_strncpy( dset->anat_parent_idcode.str , atr_str->ch , MCW_IDSIZE ) ;

   atr_str = THD_find_string_atr( blk , ATRNAME_IDWARPPAR ) ;
   if( atr_str != NULL )
      MCW_strncpy( dset->warp_parent_idcode.str , atr_str->ch , MCW_IDSIZE ) ;
#endif

   /*--------------------------------*/
   /*-- get data labels (optional) --*/
   /*--------------------------------*/

   atr_str = THD_find_string_atr( blk , ATRNAME_LABEL1 ) ;
   if( atr_str == NULL )
     atr_str = THD_find_string_atr( blk , ATRNAME_DATANAME ) ;

   if( atr_str != NULL ){
     MCW_strncpy( dset->label1 , atr_str->ch , THD_MAX_LABEL ) ;
   } else {
     MCW_strncpy( dset->label1 , THD_DEFAULT_LABEL , THD_MAX_LABEL ) ;
   }

   atr_str = THD_find_string_atr( blk , ATRNAME_LABEL2 ) ;
   if( atr_str != NULL ){
     MCW_strncpy( dset->label2 , atr_str->ch , THD_MAX_LABEL ) ;
   } else {
     MCW_strncpy( dset->label2 , THD_DEFAULT_LABEL , THD_MAX_LABEL ) ;
   }

   dset->keywords = NULL ;
#ifndef USE_APPLICATOR
   atr_str = THD_find_string_atr( blk , ATRNAME_KEYWORDS ) ;
   if( atr_str != NULL )
      dset->keywords = XtNewString( atr_str->ch ) ;
#endif

   /*---------------------------------*/
   /*-- get parent names (optional) --*/
   /*---------------------------------*/

   dset->anat_parent_name[0] = '\0' ;
   dset->warp_parent_name[0] = '\0' ;
   MCW_strncpy( dset->self_name , THD_DEFAULT_LABEL , THD_MAX_NAME ) ;
   dset->anat_parent = dset->warp_parent = NULL ;  /* must be set later */

#ifndef USE_APPLICATOR
   atr_str = THD_find_string_atr( blk , ATRNAME_ANATOMY_PARENT ) ;
   if( atr_str != NULL && ISZERO_IDCODE(dset->anat_parent_idcode) ){
     MCW_strncpy( dset->anat_parent_name , atr_str->ch , THD_MAX_NAME ) ;
   }

   atr_str = THD_find_string_atr( blk , ATRNAME_WARP_PARENT ) ;
   if( atr_str != NULL && ISZERO_IDCODE(dset->warp_parent_idcode) ){
     MCW_strncpy( dset->warp_parent_name , atr_str->ch , THD_MAX_NAME ) ;
   }

   atr_str = THD_find_string_atr( blk , ATRNAME_DATANAME ) ;
   if( atr_str != NULL ){
     MCW_strncpy( dset->self_name , atr_str->ch , THD_MAX_NAME ) ;
   }
#endif

   /*---------------------------*/
   /*-- find axes orientation --*/
   /*---------------------------*/

   daxes->type = DATAXES_TYPE ;
   daxes->nxx  = dkptr->dimsizes[0] ;
   daxes->nyy  = dkptr->dimsizes[1] ;
   daxes->nzz  = dkptr->dimsizes[2] ;

   atr_int = THD_find_int_atr( blk , ATRNAME_ORIENT_SPECIFIC ) ;
   if( atr_int == NULL ){
     DSET_ERR("illegal or missing ORIENT_SPECIFIC") ;
   } else {
     daxes->xxorient = atr_int->in[0] ;
     daxes->yyorient = atr_int->in[1] ;
     daxes->zzorient = atr_int->in[2] ;
   }

   /*----------------------*/
   /*-- find axes origin --*/
   /*----------------------*/

   atr_flo = THD_find_float_atr( blk , ATRNAME_ORIGIN ) ;
   if( atr_flo == NULL ){
     DSET_ERR("illegal or missing ORIGIN") ;
   } else {
     daxes->xxorg = atr_flo->fl[0] ;
     daxes->yyorg = atr_flo->fl[1] ;
     daxes->zzorg = atr_flo->fl[2] ;
   }

   /*------------------------*/
   /*-- find axes spacings --*/
   /*------------------------*/

   atr_flo = THD_find_float_atr( blk , ATRNAME_DELTA ) ;
   if( atr_flo == NULL ){
     DSET_ERR("illegal or missing DELTA") ;
   } else {
     daxes->xxdel = atr_flo->fl[0] ;
     daxes->yydel = atr_flo->fl[1] ;
     daxes->zzdel = atr_flo->fl[2] ;
   }

   /*---------------------------------------*/
   /*-- set bounding box for this dataset --*/
   /*---------------------------------------*/

   THD_set_daxes_bbox( daxes ) ; /* 20 Dec 2005 */

   /*----------------------------------------------------------------*/
   /*--  matrix that transforms to Dicom (left-posterior-superior) --*/
   /*----------------------------------------------------------------*/

   THD_set_daxes_to_dicomm( daxes ) ; /* 20 Dec 2005 */

   /* 09 Dec 2005: set ijk_to_dicom matrix and its inverse */

   if( !ISVALID_MAT44(daxes->ijk_to_dicom) )
     THD_daxes_to_mat44( daxes ) ;

   atr_flo = THD_find_float_atr( blk, "IJK_TO_DICOM_REAL" );
   /* load oblique transformation matrix */
   if(atr_flo) {
     LOAD_MAT44(daxes->ijk_to_dicom_real, \
         atr_flo->fl[0], atr_flo->fl[1], atr_flo->fl[2], atr_flo->fl[3], \
         atr_flo->fl[4], atr_flo->fl[5], atr_flo->fl[6], atr_flo->fl[7], \
         atr_flo->fl[8], atr_flo->fl[9], atr_flo->fl[10], atr_flo->fl[11]);
   }
   else {
     ZERO_MAT44(daxes->ijk_to_dicom_real);    /* clear values */
     daxes->ijk_to_dicom_real.m[3][3] = 0.0;  /* and make invalid */
   }

   /*------------------------------------*/
   /*-- read set of markers (optional) --*/
   /*------------------------------------*/

   dset->markers = NULL ;

#ifndef USE_APPLICATOR
   atr_flo = THD_find_float_atr( blk , ATRNAME_MARKSXYZ ) ;

   if( atr_flo != NULL ){
      dset->markers = myXtNew( THD_marker_set ) ;  /* new set */
      ADDTO_KILL(dset->kl , dset->markers) ;

      /*-- copy floating coordinates into marker struct --*/

      COPY_INTO_STRUCT( *(dset->markers) ,  /* actual struct */
                        MARKS_FSTART ,      /* byte offset */
                        float ,             /* type being copied */
                        atr_flo->fl ,       /* start of source */
                        MARKS_FSIZE  ) ;    /* number of floats */

      /*----- must have labels along with coordinates -----*/

      atr_str = THD_find_string_atr( blk , ATRNAME_MARKSLAB ) ;
      if( atr_str == NULL ){
         DSET_ERR("MARKS_XYZ present but not MARKS_LAB!") ;
      } else {
         int im , llen ;
         THD_ivec3 iv ;
         float xxdown,xxup , yydown,yyup , zzdown,zzup ;

         /*-- copy labels into marker struct --*/

         COPY_INTO_STRUCT( *(dset->markers) ,
                           MARKS_LSTART ,
                           char ,
                           atr_str->ch ,
                           MARKS_LSIZE  ) ;

         /*-- check each marker for validity:
                non-blank label string,
                all coordinates inside bounding box --*/

         /** July 1995: extend bounding box a little, maybe **/

#ifndef EXTEND_BBOX
         xxdown = daxes->xxmin - 0.501 * fabs(daxes->xxdel) ;
         xxup   = daxes->xxmax + 0.501 * fabs(daxes->xxdel) ;
         yydown = daxes->yymin - 0.501 * fabs(daxes->yydel) ;
         yyup   = daxes->yymax + 0.501 * fabs(daxes->yydel) ;
         zzdown = daxes->zzmin - 0.501 * fabs(daxes->zzdel) ;
         zzup   = daxes->zzmax + 0.501 * fabs(daxes->zzdel) ;
#else
         xxdown = daxes->xxmin ;
         xxup   = daxes->xxmax ;
         yydown = daxes->yymin ;
         yyup   = daxes->yymax ;
         zzdown = daxes->zzmin ;
         zzup   = daxes->zzmax ;
#endif

         dset->markers->numdef = dset->markers->numset = 0 ;

         for( im=0 ; im < MARKS_MAXNUM ; im++ ){
            llen = strlen( &(dset->markers->label[im][0]) ) ;
            dset->markers->valid[im]   =
               (llen > 0) &&
               ( dset->markers->xyz[im][0] >= xxdown ) &&
               ( dset->markers->xyz[im][0] <= xxup   ) &&
               ( dset->markers->xyz[im][1] >= yydown ) &&
               ( dset->markers->xyz[im][1] <= yyup   ) &&
               ( dset->markers->xyz[im][2] >= zzdown ) &&
               ( dset->markers->xyz[im][2] <= zzup   )    ;

            if( dset->markers->valid[im] ) (dset->markers->numset)++ ;

            if( llen > 0 ) (dset->markers->numdef)++ ;

            dset->markers->ovcolor[im] = -1 ;  /* default color */

         } /* end of loop over markers */
      } /* end of if marker labels exist */

      /*----- should also have help for each marker -----*/

      atr_str = THD_find_string_atr( blk , ATRNAME_MARKSHELP ) ;
      if( atr_str == NULL ){
         int im ;
         for( im=0 ; im < MARKS_MAXNUM ; im++ )
            dset->markers->help[im][0] = '\0' ;  /* empty string */
      } else {
         COPY_INTO_STRUCT( *(dset->markers) ,
                           MARKS_HSTART ,
                           char ,
                           atr_str->ch ,
                           MARKS_HSIZE  ) ;
      } /* end of if marker help exists */

      /*----- should also have action flags for the marker set -----*/

      atr_int = THD_find_int_atr( blk , ATRNAME_MARKSFLAG ) ;
      if( atr_int == NULL ){
         int im ;
         for( im=0 ; im < MARKS_MAXFLAG ; im++ )
            dset->markers->aflags[im] = -1 ;
      } else {
         COPY_INTO_STRUCT( *(dset->markers) ,
                           MARKS_ASTART ,
                           int ,
                           atr_int->in ,
                           MARKS_ASIZE  ) ;
         dset->markers->type = dset->markers->aflags[0] ;
      } /* end of if marker flags exist */

   } /* end of if markers exist */
#endif

   /*--------------------------*/
   /*-- read warp (optional) --*/
   /*--------------------------*/

   dset->vox_warp  = NULL ;  /* 02 Nov 1996 */
   dset->self_warp = NULL ;  /* 26 Aug 2002 */
   dset->warp      = NULL ;

#ifndef USE_APPLICATOR
   atr_int = THD_find_int_atr( blk , ATRNAME_WARP_TYPE ) ;
   if( atr_int != NULL ){  /* no warp */
      int wtype = atr_int->in[0] , rtype = atr_int->in[1]  ;

      dset->warp = myXtNew( THD_warp ) ;
      ADDTO_KILL( dset->kl , dset->warp ) ;

      atr_flo = THD_find_float_atr( blk , ATRNAME_WARP_DATA ) ;
      if( atr_flo == NULL ){
         DSET_ERR("illegal or missing WARP_DATA") ;
      } else {
         switch( wtype ){

            default: DSET_ERR("illegal WARP_TYPE warp code") ; break;

            case WARP_AFFINE_TYPE:{
               THD_affine_warp *ww = (THD_affine_warp *) dset->warp ;
               ww->type       = wtype ;
               ww->resam_type = rtype ;
               ww->warp.type  = MAPPING_LINEAR_TYPE ;

               COPY_INTO_STRUCT( ww->warp ,
                                 MAPPING_LINEAR_FSTART ,
                                 float ,
                                 atr_flo->fl ,
                                 MAPPING_LINEAR_FSIZE ) ;
            }
            break ;  /* end affine warp */

            case WARP_TALAIRACH_12_TYPE:{
               THD_talairach_12_warp *ww =
                  (THD_talairach_12_warp *) dset->warp ;
               int iw , ioff ;
               ww->type       = wtype ;
               ww->resam_type = rtype ;
               for( iw=0 ; iw < 12 ; iw++ ){
                  ww->warp[iw].type = MAPPING_LINEAR_TYPE ;

                  ioff = iw * MAPPING_LINEAR_FSIZE ;

                  COPY_INTO_STRUCT( ww->warp[iw] ,
                                    MAPPING_LINEAR_FSTART ,
                                    float ,
                                    &(atr_flo->fl[ioff]) ,
                                    MAPPING_LINEAR_FSIZE ) ;

               }  /* end loop over 12 warps */
            }
            break ;  /* end talairach_12 warp */

         } /* end of switch on warp type */
      } /* end of if on legal warp data */
   } /* end of if on warp existing */   else { /* But perhaps there is a little something from auto talairaching ZSS, June 06 */
      if (dset->view_type == VIEW_TALAIRACH_TYPE) { /* something to do */
         atr_flo = THD_find_float_atr( blk , ATRNAME_WARP_DATA_3DWD_AF ) ; 
         if ( atr_flo == NULL ){
            /* A tlrc set with no transform. No problem */
         } else {
            if (!THD_WarpData_From_3dWarpDrive(dset, atr_flo)) {
               fprintf(stderr,"Error: Failed to create WarpData!\n");
            }
         }
      } else {
         /* fprintf(stderr,"Not in TLRC space, bother not.\n"); */
      }
   } /* the very end of if on warp existing */
#endif

   /*----- read statistics, if available -----*/

#ifndef USE_APPLICATOR
   atr_flo = THD_find_float_atr( blk , ATRNAME_BRICK_STATS ) ;  /* new style */

   if( atr_flo != NULL ){  /*** have new style statistics ***/
      int qq ;
      dset->stats         = myXtNew( THD_statistics ) ;
      dset->stats->type   = STATISTICS_TYPE ;
      dset->stats->parent = (XtPointer) dset ;
      dset->stats->nbstat = blk->nvals ;
      dset->stats->bstat  = (THD_brick_stats *)
                               XtMalloc( sizeof(THD_brick_stats) * blk->nvals ) ;
      for( qq=0 ; qq < blk->nvals ; qq++ ){
         if( 2*qq+1 < atr_flo->nfl ){
            dset->stats->bstat[qq].min = atr_flo->fl[2*qq] ;
            dset->stats->bstat[qq].max = atr_flo->fl[2*qq+1] ;
         } else {
            INVALIDATE_BSTAT( dset->stats->bstat[qq] ) ;
         }
      }
      ADDTO_KILL( dset->kl , dset->stats->bstat ) ;
      ADDTO_KILL( dset->kl , dset->stats ) ;

   } else {  /**** check for old style (version 1.03-4) statistics ****/

      atr_int = THD_find_int_atr( blk , ATRNAME_MINMAX ) ;

      if( atr_int == NULL ){  /*** no statistics at all ***/
         dset->stats = NULL ;

      } else {                /*** have old style (integer) statistics ***/
         int qq ;
         dset->stats         = myXtNew( THD_statistics ) ;
         dset->stats->type   = STATISTICS_TYPE ;
         dset->stats->parent = (XtPointer) dset ;
         dset->stats->nbstat = blk->nvals ;
         dset->stats->bstat  = (THD_brick_stats *)
                                  XtMalloc( sizeof(THD_brick_stats) * blk->nvals ) ;
         for( qq=0 ; qq < blk->nvals ; qq++ ){
            if( 2*qq+1 < atr_int->nin ){
               dset->stats->bstat[qq].min = (float) atr_int->in[2*qq] ;
               dset->stats->bstat[qq].max = (float) atr_int->in[2*qq+1] ;
            } else {
               INVALIDATE_BSTAT( dset->stats->bstat[qq] ) ;
            }
         }
         ADDTO_KILL( dset->kl , dset->stats->bstat ) ;
         ADDTO_KILL( dset->kl , dset->stats ) ;
      }
   }
#endif

   /*--- read auxiliary statistics info, if any ---*/

   atr_flo = THD_find_float_atr( blk , ATRNAME_STAT_AUX ) ;

   if( atr_flo != NULL ){
     INIT_STAT_AUX( dset , atr_flo->nfl , atr_flo->fl ) ;
     iq = atr_flo->nfl ;
   } else {
     ZERO_STAT_AUX( dset ) ;
     iq = 0 ;
   }

   if( ISFUNC(dset) && FUNC_need_stat_aux[dset->func_type] > iq ){
     DSET_ERR("function type missing auxiliary statistical data") ;
   }

   /*--- read time-dependent information, if any ---*/

   atr_int = THD_find_int_atr  ( blk , ATRNAME_TAXIS_NUMS ) ;
   atr_flo = THD_find_float_atr( blk , ATRNAME_TAXIS_FLOATS ) ;

   if( atr_int != NULL && atr_flo != NULL ){
     int isfunc , nvals ;

     dset->taxis = myXtNew( THD_timeaxis ) ;

     dset->taxis->type    = TIMEAXIS_TYPE ;
     dset->taxis->ntt     = atr_int->in[0] ;
     dset->taxis->nsl     = atr_int->in[1] ;
     dset->taxis->ttorg   = atr_flo->fl[0] ;
     dset->taxis->ttdel   = atr_flo->fl[1] ;
     dset->taxis->ttdur   = atr_flo->fl[2] ;
     dset->taxis->zorg_sl = atr_flo->fl[3] ;
     dset->taxis->dz_sl   = atr_flo->fl[4] ;

     dset->taxis->units_type = atr_int->in[2] ;    /* 21 Oct 1996 */
     if( dset->taxis->units_type < 0 )             /* assign units */
       dset->taxis->units_type = UNITS_SEC_TYPE ;  /* to the time axis */

     if( dset->taxis->nsl > 0 ){
       atr_flo = THD_find_float_atr( blk , ATRNAME_TAXIS_OFFSETS ) ;
       if( atr_flo == NULL || atr_flo->nfl < dset->taxis->nsl ){
         dset->taxis->nsl     = 0 ;
         dset->taxis->toff_sl = NULL ;
         dset->taxis->zorg_sl = 0.0 ;
         dset->taxis->dz_sl   = 0.0 ;
       } else {
         int ii ;
         dset->taxis->toff_sl = (float *) XtMalloc(sizeof(float)*dset->taxis->nsl) ;
         for( ii=0 ; ii < dset->taxis->nsl ; ii++ )
           dset->taxis->toff_sl[ii] = atr_flo->fl[ii] ;
       }
     } else {
       dset->taxis->nsl     = 0 ;
       dset->taxis->toff_sl = NULL ;
       dset->taxis->zorg_sl = 0.0 ;
       dset->taxis->dz_sl   = 0.0 ;
     }

     isfunc = ISFUNCTYPE(dset->type) ;
     nvals  = (isfunc) ? FUNC_nvals[dset->func_type]
                       : ANAT_nvals[dset->func_type]  ;

     if( nvals != 1 ){
       WARNING_message("Illegal 3D+time dataset & func_type combination: '%s'" ,
                       DSET_HEADNAME(dset) ) ;
       if( dset->taxis->toff_sl != NULL ) myXtFree(dset->taxis->toff_sl) ;
       myXtFree(dset->taxis) ;
     }

     /** 15 Aug 2005: don't allow milliseconds on input any more **/

     if( !AFNI_yesenv("AFNI_ALLOW_MILLISECONDS") ){ DSET_UNMSEC(dset); }
   }

   /*--- 23 Oct 1998: read the tagset information ---*/

#ifndef USE_APPLICATOR
   atr_int = THD_find_int_atr   ( blk , ATRNAME_TAGSET_NUM    ) ;
   atr_flo = THD_find_float_atr ( blk , ATRNAME_TAGSET_FLOATS ) ;
   atr_str = THD_find_string_atr( blk , ATRNAME_TAGSET_LABELS ) ;

   if( atr_int != NULL && atr_flo != NULL && atr_str != NULL ){
      int nin=atr_int->nin , nfl=atr_flo->nfl , nch=atr_str->nch ;
      int ii , ntag , nfper , jj , kk ;

      ntag  = atr_int->in[0] ;  /* number of tags */
      nfper = atr_int->in[1] ;  /* number of floats per tag */

      if( ntag > MAX_TAG_NUM ) ntag = MAX_TAG_NUM ;

      dset->tagset = myXtNew( THD_usertaglist ) ;  /* create tagset */
      ADDTO_KILL( dset->kl , dset->tagset ) ;

      dset->tagset->num = ntag ;
      strcpy( dset->tagset->label , "Bebe Rebozo" ) ;  /* not used */

      /* read out tag values; allow for chance there isn't enough data */

#undef  TF
#define TF(i,j) ( ((j)<nfper && (i)*nfper+(j)<nfl) ? atr_flo->fl[(i)*nfper+(j)] : -666.0 )
      for( ii=0 ; ii < ntag ; ii++ ){
         dset->tagset->tag[ii].x   = TF(ii,0) ;  /* coords */
         dset->tagset->tag[ii].y   = TF(ii,1) ;
         dset->tagset->tag[ii].z   = TF(ii,2) ;
         dset->tagset->tag[ii].val = TF(ii,3) ;  /* value */
         dset->tagset->tag[ii].ti  = TF(ii,4) ;  /* time index; if < 0 ==> not set */
         if( dset->tagset->tag[ii].ti >= 0 ){
             dset->tagset->tag[ii].set = 1 ;
         } else {
             dset->tagset->tag[ii].set = 0 ; dset->tagset->tag[ii].ti = 0 ;
         }
      }
#undef TF

      /* read out tag labels; allow for empty labels */

      jj = 0 ;
      for( ii=0 ; ii < ntag ; ii++ ){
         if( jj < nch ){
            kk = strlen( atr_str->ch + jj ) ;
            if( kk > 0 ) TAG_SETLABEL( dset->tagset->tag[ii] , atr_str->ch + jj ) ;
            else         sprintf( dset->tagset->tag[ii].label , "Tag %d" , ii+1 ) ;
            jj += kk+1 ;
         } else {
            sprintf( dset->tagset->tag[ii].label , "Tag %d" , ii+1 ) ;
         }
      }
   }
#endif

   /* 10 May 2005: new function to apply some attributes
                   to dataset, rather than doing it here */

#ifdef USE_APPLICATOR
   THD_datablock_apply_atr( dset ) ;
#endif

   /*--- check for the following conditions:
           if warp exists,    warp_parent_name or _idcode must exist;
           if warp nonexists, warp_parent_name and _idcode must nonexist,
                              AND data must exist on disk                 ---*/

   if( dset->warp != NULL ){
     if( strlen(dset->warp_parent_name) <= 0 &&
         ISZERO_IDCODE(dset->warp_parent_idcode) )
       DSET_ERR("have warp but have no warp parent") ;

     dset->wod_flag = !allow_nodata && !DSET_ONDISK(dset) ;
   } else {
     if( strlen(dset->warp_parent_name) > 0 ||
         !ISZERO_IDCODE(dset->warp_parent_idcode) )
       DSET_ERR("have no warp but have warp parent") ;

     if( !allow_nodata && !DSET_ONDISK(dset) )
       DSET_ERR("have no warp but have no data on disk as well") ;
   }

   /* backup assignment of ID code if not assigned earlier */

   if( dset->idcode.str[0] == '\0' ) dset->idcode = MCW_new_idcode() ;

   /*--- that's all the work for now;
         if any error was flagged, kill this dataset and return nothing ---*/

   if( dset_ok == False ){
     fprintf(stderr,"PURGING dataset %s from memory\n",DSET_HEADNAME(dset)) ;
     THD_delete_3dim_dataset( dset , False ) ;
     RETURN(NULL) ;
   }

   /*--- If we assigned a new dataset idcode, write it back to disk ---*/
      /* (This code was for the old days, when there were datasets)
         (hanging around that hadn't yet been assigned ID codes.  ) */

#if 0
   if( dset != NULL && new_idcode ){
     fprintf(stderr,"** Writing new ID code to dataset header %s\n",
             dset->dblk->diskptr->header_name ) ;
     THD_write_3dim_dataset( NULL , NULL , dset , False ) ;
   }
#endif

   RETURN( dset );
}
