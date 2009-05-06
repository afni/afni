#include "afni.h"

#ifndef ALLOW_PLUGINS
IPLUGIN_interface *  ICOR_init(char *lab)
{
  MCW_popup_message( THE_TOPSHELL ,
                     " \n"
                     " InstaCorr not available\n"
                     " since this copy of AFNI\n"
                     "  was compiled without\n"
                     "  support for plugins!\n " , MCW_USER_KILL ) ;
  return NULL ;
}
#else

/***********************************************************************
  Pseudo-plugin to setup InstaCorr operations
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: control AFNI InstaCorr operations\n"
  "\n"
  "Author -- RW Cox -- Apr 2009"
;

/*----------------- prototypes for internal routines -----------------*/

static char * ICOR_main( PLUGIN_interface * ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * ICOR_init( char *lab )
{
   PLUGIN_interface *plint ;     /* will be the output of this routine */
   static char *yn[2] = { "No" , "Yes" } ;
   char sk[32] , sc[32] ;

   if( lab == NULL ) lab = "\0" ;

   /*---------------- set titles and call point ----------------*/

   sprintf(sk,"%sControl InstaCorr",lab) ;
   plint = PLUTO_new_interface( "InstaCorr" ,
                                sk ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())ICOR_main  ) ;

   sprintf(sk,"%sSetup+Keep",lab) ; sprintf(sc,"%sSetup+Close",lab) ;
   PLUTO_set_runlabels( plint , sk , sc ) ;

   /*--------- make interface lines -----------*/

   PLUTO_add_option ( plint , "Time Series" , "TimeSeries" , TRUE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number ( plint , "Ignore" , 0,50,0,0,FALSE ) ;
   PLUTO_add_number ( plint , "Blur"   , 0,99,1,0,TRUE  ) ;

   PLUTO_add_option ( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number ( plint , "Index" , 0,99999,0,0,TRUE ) ;
   PLUTO_add_string ( plint , "Auto"  , 2 , yn , 1 ) ;

   PLUTO_add_option    ( plint , "Global Orts" , "GlobalOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;

#if 0
   PLUTO_add_option    ( plint , "Slice Orts" , "SliceOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;
#endif

   PLUTO_add_option( plint , "Bandpass(Hz)" , "Bandpass" , FALSE ) ;
   PLUTO_add_number( plint , "Lower" , 0,1000,3, 10 , TRUE ) ;
   PLUTO_add_number( plint , "Upper" , 0,1000,3,100 , TRUE ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * ICOR_main( PLUGIN_interface *plint )
{
   char *tag ;
   float fbot=-1.0f , ftop=999999.9f ;
   MRI_IMAGE *gortim=NULL ;
   THD_3dim_dataset *dset=NULL , *mset=NULL ;
   int ignore=0 , mindex=0 , automask=0 , qq ; float blur=0.0f ;
   ICOR_setup *iset ; char *cpt ;
   Three_D_View *im3d = plint->im3d ;

   if( !IM3D_OPEN(im3d) || im3d->vwid->func->options_vedit_av->ival != 1 ){
     XtUnmapWidget(plint->wid->shell); return NULL;
   }

   /*--- loop over input option lines ---*/

   while(1){
     tag = PLUTO_get_optiontag(plint) ;  /* which line? */
     if( tag == NULL ) break ;           /* none ==> done */

     /** TimeSeries **/

     if( strcmp(tag,"TimeSeries") == 0 ){
       MCW_idcode *idc ;
       idc  = PLUTO_get_idcode(plint) ;
       dset = PLUTO_find_dset(idc) ;
       if( dset == NULL ) ERROR_message("Can't find Time Series dataset") ;
       ignore = PLUTO_get_number(plint) ;
       blur   = PLUTO_get_number(plint) ;
       continue ;
     }

     /** Mask **/

     if( strcmp(tag,"Mask") == 0 ){
       MCW_idcode *idc ; char *am ;
       idc      = PLUTO_get_idcode(plint) ;
       mset     = PLUTO_find_dset(idc) ;
       mindex   = PLUTO_get_number(plint) ;
       am       = PLUTO_get_string(plint) ;
INFO_message("Automask string = '%s'",am) ;
       automask = (am[0] == 'Y') ;
       if( !automask && mset == NULL )
         WARNING_message("No Masking selected?!") ;
       else if( mset != NULL && automask )
         WARNING_message("Mask dataset disabled when Automask is Yes") ;
       continue ;
     }

     /** GlobalOrts **/

     if( strcmp(tag,"GlobalOrts") == 0 ){
       gortim = PLUTO_get_timeseries(plint) ;
       if( gortim == NULL ) ERROR_message("Ignoring NULL 'Global Orts' time series") ;
       continue ;
     }

     /** Bandpass **/

     if( strcmp(tag,"Bandpass") == 0 ){
       fbot = PLUTO_get_number(plint) ;
       ftop = PLUTO_get_number(plint) ;
       if( fbot >= ftop ) ERROR_message("Ignoring disordered Bandpass frequencies") ;
INFO_message("fbot=%.3f ftop=%.3f",fbot,ftop) ;
       continue ;
     }

     /** should never transpire **/

     return "** ICOR_main: table corruption! **" ;
   }

   /*** check inputs for stoopiditeeze ***/

   if( dset == NULL )
     return "** No Time Series dataset? **" ;
   if( DSET_NVALS(dset)-ignore < 9 )
     return "** Time Series dataset is too short for InstaCorr **" ;
   if( !automask && mset != NULL && DSET_NVOX(mset) != DSET_NVOX(dset) )
     return "** Mask dataset doesn't match up with Time Series dataset **" ;
   if( !automask && mset != NULL && mindex >= DSET_NVALS(mset) )
     return "** Mask dataset index is out of range **" ;
   if( gortim != NULL && gortim->nx < DSET_NVALS(dset)-ignore )
     return "** Global Orts file is too short for Time Series dataset **" ;

   if( fbot >= ftop ){ fbot = 0.0f ; ftop = 999999.9f ; }
   if( fbot <  0.0f )  fbot = 0.0f ;

   /** (re)create InstaCorr setup **/

   DESTROY_ICOR_setup(im3d->iset) ;
   INIT_ICOR_setup(iset) ;

   iset->dset     = dset ;
   iset->mset     = (automask) ? NULL : mset ;
   iset->gortim   = gortim ;
   iset->ignore   = ignore ;
   iset->automask = automask ;
   iset->mindex   = mindex ;
   iset->fbot     = fbot ;
   iset->ftop     = ftop ;
   iset->blur     = blur ;
   iset->prefix   = (char *)malloc(sizeof(char)*16) ;
   cpt = AFNI_controller_label(im3d); sprintf(iset->prefix,"%c_ICOR",cpt[1]);

   INSTACORR_LABEL_OFF(im3d) ;
   SHOW_AFNI_PAUSE ;
   /**************/   qq = THD_instacorr_prepare( iset ) ;  /**************/
   SHOW_AFNI_READY ;
   if( qq == 0 ){
     DESTROY_ICOR_setup(iset) ; return "** Error in InstaCorr setup!? **" ;
   }
   INSTACORR_LABEL_ON(im3d) ;
   INFO_message("InstaCorr setup: %d voxels ready for work",qq) ;

   im3d->iset = iset ;

   ENABLE_INSTACORR(im3d) ;  /* manage the widgets */
   return NULL ;
}
#endif  /* ALLOW_PLUGINS */

/*------------------------------------------------------------------*/

int AFNI_icor_setref( Three_D_View *im3d )
{
   MRI_IMAGE *iim; float *iar; THD_fvec3 iv,jv; THD_ivec3 kv; int ijk;
   THD_3dim_dataset *icoset ; THD_slist_find slf ; int nds=0 ;

ENTRY("AFNI_icor_setref") ;

   if( !IM3D_OPEN(im3d) || !ISVALID_ICOR_setup(im3d->iset) ) RETURN(0) ;

   /* find where we are */

   LOAD_FVEC3( iv , im3d->vinfo->xi,im3d->vinfo->yj,im3d->vinfo->zk ) ;
   jv  = THD_dicomm_to_3dmm       ( im3d->iset->dset, iv ) ;

   if( jv.xyz[0] < im3d->iset->dset->daxes->xxmin ||
       jv.xyz[0] > im3d->iset->dset->daxes->xxmax ||
       jv.xyz[1] < im3d->iset->dset->daxes->yymin ||
       jv.xyz[1] > im3d->iset->dset->daxes->yymax ||
       jv.xyz[2] < im3d->iset->dset->daxes->zzmin ||
       jv.xyz[2] > im3d->iset->dset->daxes->zzmax   ){

     WARNING_message("InstaCorr set point outside dataset box") ;
     RETURN(0) ;
   }

   kv  = THD_3dmm_to_3dind_no_wod( im3d->iset->dset, jv ) ;
   ijk = DSET_ixyz_to_index      ( im3d->iset->dset, kv.ijk[0],kv.ijk[1],kv.ijk[2] );

   /* do the real work */

   iim = THD_instacorr( im3d->iset , ijk , 0 ) ;

   if( iim == NULL ) RETURN(0) ;  /* did it fail? */

   /* find the output dataset */

   slf = THD_dset_in_session( FIND_PREFIX , im3d->iset->prefix , im3d->ss_now ) ;

   /* if it doesn't exist, or is not the right grid, create it now */

   if( !ISVALID_DSET (slf.dset) ||
       !EQUIV_DATAXES(slf.dset->daxes,im3d->iset->dset->daxes) ){

     icoset = EDIT_empty_copy( im3d->iset->dset ) ;  /* make new dataset */
     EDIT_dset_items( icoset ,
                        ADN_prefix    , im3d->iset->prefix ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_func_type , FUNC_FIM_TYPE ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     DSET_superlock(icoset) ;

     if( slf.dset != NULL ){       /* exists, but isn't right for us */

       MCW_idcode old_idc = slf.dset->idcode ;
       THD_delete_3dim_dataset(slf.dset,True) ;  /* destroy the guts */
       *slf.dset = *icoset ;      /* copy the guts, keep the pointer */
       slf.dset->idcode = old_idc ;           /* and keep the idcode */
       nds = slf.dset_index ;
INFO_message("trashed and re-used old dataset %s",im3d->iset->prefix) ;

     } else {                                  /* add to the session */
       int vv = icoset->view_type ;
       nds = im3d->ss_now->num_dsset ;
       im3d->ss_now->dsset[nds][vv] = icoset ;
       im3d->ss_now->num_dsset++ ;
INFO_message("created new dataset %s",im3d->iset->prefix) ;
     }

   /* just need to use existing dataset that matches */

   } else {

     icoset = slf.dset ; nds = slf.dset_index ;

   }

   /* save the result into the output dataset */

   iar = MRI_FLOAT_PTR(iim) ;
   EDIT_substitute_brick( icoset , 0 , MRI_float , iar ) ;
   mri_clear_data_pointer(iim) ; mri_free(iim) ;
   DSET_KILL_STATS(icoset) ; THD_load_statistics(icoset) ;

   /* redisplay overlay */

   if( im3d->fim_now != icoset ){  /* switch to this dataset */
     MCW_choose_cbs cbs ;
     cbs.ival = nds ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                               (XtPointer)im3d ,  &cbs           ) ;
     AFNI_set_fim_index(im3d,0) ;
     AFNI_set_thr_index(im3d,0) ;
   }
   AFNI_reset_func_range(im3d) ;

   if( MCW_val_bbox(im3d->vwid->view->see_func_bbox) == 0 ){ /* overlay is off */
     char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     sprintf(cmd,"SEE_OVERLAY %c.+",cpt[1]) ;
     AFNI_driver(cmd) ;
   } else {                                                  /* overlay is on */
     AFNI_redisplay_func(im3d) ;
   }

   RETURN(1) ;
}
