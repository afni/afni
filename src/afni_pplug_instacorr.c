#include "afni.h"
#ifndef ALLOW_PLUGINS
PLUGIN_interface * ICOR_init(char *lab)
{
  MCW_popup_message( THE_TOPSHELL ,
                     " \n"
                     " InstaCorr not available\n"
                     " since this copy of AFNI\n"
                     "  was compiled without\n"
                     "  support for plugins!\n " , MCW_USER_KILL ) ;
  return NULL ;
}

PLUGIN_interface * GICOR_init(char *lab)
{
  MCW_popup_message( THE_TOPSHELL ,
                     " \n"
                     " GrpInCorr not available\n"
                     " since this copy of AFNI\n"
                     "  was compiled without\n"
                     "  support for plugins!\n " , MCW_USER_KILL ) ;
  return NULL ;
}
#else

/***********************************************************************
  Pseudo-plugin to setup InstaCorr operations
************************************************************************/

static int ncall=0 ;

/*--------------------- string to 'help' the user --------------------*/

static char i_helpstring[] =
  "Purpose: control AFNI InstaCorr operations\n"
  "\n"
  "===================  The Two Steps to Using InstaCorr  ===================\n"
  "\n"
  "(1) Use the Setup controller to prepare for the correlation computations.\n"
  "   * A detailed description of the controls is given farther below.\n"
  "\n"
  "(2) In an image viewer window:\n"
  "   * Right-click (or Ctrl-Left-click) to get a popup menu.\n"
  "\n"
  "   * The top item is 'InstaCorr Set'.\n"
  "   ++ This item will only be enabled when Setup processing is complete.\n"
  "\n"
  "   * Choosing this item will cause the current crosshair voxel to\n"
  "     be the seed time series that is correlated with all the others.\n"
  "\n"
  "   * A new functional overlay dataset will be created to show the result,\n"
  "     which comprises the collection of correlation coefficients of each\n"
  "     processed voxel time series with the (processed) seed voxel time series.\n"
  "   ++ In AFNI controller 'A', this dataset will be named 'A_ICOR', etc.\n"
  "   ++ This correlation dataset will NOT be saved to disk automatically.\n"
  "   ++ You can save this dataset to disk with the 'Datamode -> Write OLay'\n"
  "      button in the main AFNI controller window.\n"
  "   ++ Pressing this button more than once will result in this dataset\n"
  "      being over-written by the latest version!\n"
  "\n"
  "   * Each time you do 'InstaCorr Set', the functional overlay\n"
  "     will be updated to reflect the new correlation map.\n"
  "\n"
  "  ** Alternative to using 'InstaCorr Set' on the popup menu:\n"
  "   ++ Hold down BOTH the Shift and Control keys on the keyboard\n"
  "      while clicking down the Left mouse button\n"
  "      (i.e., Shift-Ctrl-Left-click).\n"
  "   ++ This combination will jump the crosshairs to the selected point\n"
  "      AND then run 'InstaCorr Set' using this new voxel as the seed.\n"
  "\n"
  "CONTROLS\n"
  "========\n"
  "* Time Series:\n"
  "    Dataset  = time series dataset to auto-correlate\n"
  "                [this dataset does NOT have to be the Underlay]\n"
  "    Ignore   = number of initial time points to ignore\n"
  "    Blur     = FWHM in mm of Gaussian blurring to perform\n"
  "                [if a Mask is used, blurring is only inside the mask]\n"
  "\n"
  "* Mask:\n"
  "    Automask = Yes to compute an automask from the time series dataset\n"
  "               No to skip this automask step\n"
  "    Dataset  = Dataset from which to draw a mask\n"
  "                [this dataset will be ignored if Automask is Yes]\n"
  "    Index    = Sub-brick index to use for dataset-derived mask\n"
  "\n"
  "* Bandpass:\n"
  "    Lower    = Smallest frequency to allow (in Hz)  [can be 0]\n"
  "    Upper    = Largest frequency to allow (in Hz)   [must be > Lower]\n"
  "                [Even if Bandpass is turned off, each voxel time series]\n"
  "                [is detrended against a quadratic polynomial and then  ]\n"
  "                [has the 0 and Nyquist frequencies removed.            ]\n"
  "\n"
  "* Global Orts:\n"
  "    1D file  = Extra time series to remove from each voxel before\n"
  "               computing the correlations\n"
  "                [These are also bandpassed to avoid re-introducing any]\n"
  "                [of the frequency components rejected by Bandpass.    ]\n"
  "\n"
  "* Misc Opts:\n"
  "  IF environment variable AFNI_INSTACORR_SEEDBLUR is YES\n"
  "    SeedBlur = Extra radius about which to Gaussian blur when extracting\n"
  "               the seed voxel time series.\n"
  "  IF environment variable AFNI_INSTACORR_SEEDBLUR is NO or isn't set\n"
  "    SeedRad  = Radius of sphere over which to average when extracting\n"
  "               the seed voxel time series.\n"
  "  These extra averages/blurs are done only inside the mask (if any), and\n"
  "  are applied after the dataset is blurred (if Blur > 0).\n"
  "\n"
  "OPERATION\n"
  "=========\n"
  "* Once you have set the controls the way you want, press one of the ''Setup'n"
  "  buttons, and the program will process (filter & blur) the data time series.\n"
  "\n"
  "* When this processing is finished, you will be ready to use 'InstaCorr Set'\n"
  "  (or Shift-Ctrl-Left-click) and have some InstaCorr fun!\n"
  "\n"
  "* The 'InstaCorr SeedJump' popup menu item will jump the crosshairs back\n"
  "  to the voxel that is the currently used InstaCorr seed.\n"
  "\n"
  "* If you switch session directories, or switch views (e.g., +orig to +tlrc),\n"
  "  InstaCorr will be disabled and you'll have to use the 'Setup ICorr'\n"
  "  button again to re-initialize the computations.\n"
  "\n"
  "Author -- RW Cox -- May 2009\n"
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
   int gblur = AFNI_yesenv("AFNI_INSTACORR_SEEDBLUR") ;

   if( lab == NULL ) lab = "\0" ;

   /*---------------- set titles and call point ----------------*/

   sprintf(sk,"%sSetup InstaCorr",lab) ;
   plint = PLUTO_new_interface( "InstaCorr" ,
                                sk ,
                                i_helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())ICOR_main  ) ;

   sprintf(sk,"%sSetup+Keep",lab) ; sprintf(sc,"%sSetup+Close",lab) ;
   PLUTO_set_runlabels( plint , sk , sc ) ;

   /*--------- make interface lines -----------*/

   PLUTO_add_option ( plint , "Time Series" , "TimeSeries" , TRUE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number ( plint , "Ignore" , 0,50,0,0,FALSE ) ;
   PLUTO_add_number ( plint , "Blur"   , 0,10,0,0,TRUE  ) ;

   PLUTO_add_option ( plint , "Mask" , "Mask" , TRUE ) ;
   PLUTO_add_string ( plint , "Automask"  , 2 , yn , 1 ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number ( plint , "Index" , 0,99999,0,0,TRUE ) ;

   PLUTO_add_option( plint , "Bandpass(Hz)" , "Bandpass" , FALSE ) ;
   PLUTO_add_number( plint , "Lower" , 0,1000,3, 10 , TRUE ) ;
   PLUTO_add_number( plint , "Upper" , 0,1000,3,100 , TRUE ) ;

   PLUTO_add_option    ( plint , "Global Orts" , "GlobalOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;

#if 0
   PLUTO_add_option    ( plint , "Slice Orts" , "SliceOrts" , FALSE ) ;
   PLUTO_add_timeseries( plint , "1D file" ) ;
#endif

   PLUTO_add_option( plint , "Misc Opts" , "MiscOpts" , FALSE ) ;
   PLUTO_add_number( plint , (gblur) ? "SeedBlur" : "SeedRad" , 0,10,0,0,TRUE ) ;

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
   int ignore=0 , mindex=0 , automask=0 , qq ; float blur=0.0f , sblur=0.0f ;
   ICOR_setup *iset ; char *cpt ;
   Three_D_View *im3d = plint->im3d ;
   double etim ;

   ncall = 0 ;

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
       am       = PLUTO_get_string(plint) ; automask = (am[0] == 'Y') ;
       idc      = PLUTO_get_idcode(plint) ; mset     = PLUTO_find_dset(idc) ;
       mindex   = PLUTO_get_number(plint) ;
       if( !automask && mset == NULL )
         WARNING_message("No Masking selected?!") ;
       else if( mset != NULL && automask )
         WARNING_message("Mask dataset disabled when Automask is Yes") ;
       continue ;
     }

     /** GlobalOrts **/

     if( strcmp(tag,"GlobalOrts") == 0 ){
       MRI_IMAGE *qim = PLUTO_get_timeseries(plint) ;
       if( qim == NULL ) ERROR_message("Ignoring NULL 'Global Orts' time series") ;
       else              gortim = mri_copy(qim) ;
       continue ;
     }

     /** Bandpass **/

     if( strcmp(tag,"Bandpass") == 0 ){
       fbot = PLUTO_get_number(plint) ;
       ftop = PLUTO_get_number(plint) ;
       if( fbot >= ftop ) ERROR_message("Ignoring disordered Bandpass frequencies") ;
       continue ;
     }

     /** MiscOpts **/

     if( strcmp(tag,"MiscOpts") == 0 ){
       sblur = PLUTO_get_number(plint) ;
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

   /** check if only thing changed is sblur -- don't need to re-prepare in that case **/

   if( im3d->iset           != NULL     &&
       im3d->iset->mv       != NULL     &&
       im3d->iset->dset     == dset     &&
       im3d->iset->mset     == mset     &&
       im3d->iset->gortim   == gortim   &&
       im3d->iset->ignore   == ignore   &&
       im3d->iset->automask == automask &&
       im3d->iset->mindex   == mindex   &&
       im3d->iset->fbot     == fbot     &&
       im3d->iset->ftop     == ftop     &&
       im3d->iset->blur     == blur        ){

     INFO_message("InstaCorr setup: minor changes accepted") ;
     im3d->iset->sblur = sblur ; return NULL ;
   }

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
   iset->sblur    = sblur ;
   iset->prefix   = (char *)malloc(sizeof(char)*16) ;
   cpt = AFNI_controller_label(im3d); sprintf(iset->prefix,"%c_ICOR",cpt[1]);

   etim = PLUTO_elapsed_time() ;

   INSTACORR_LABEL_OFF(im3d) ;
   SHOW_AFNI_PAUSE ;
   /**************/   qq = THD_instacorr_prepare( iset ) ;  /**************/
   SHOW_AFNI_READY ;
   if( qq == 0 ){
     DESTROY_ICOR_setup(iset) ; return "** Error in InstaCorr setup!? **" ;
   }
   INSTACORR_LABEL_ON(im3d) ;

   etim = PLUTO_elapsed_time() - etim ;
   INFO_message("InstaCorr setup: %d voxels ready for work: %.2f sec",qq,etim) ;

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
   double etim ;

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
   ijk = DSET_ixyz_to_index( im3d->iset->dset, kv.ijk[0],kv.ijk[1],kv.ijk[2] ) ;

   /* do the real work: ijk = voxel index */

   etim = PLUTO_elapsed_time() ;

   iim = THD_instacorr( im3d->iset , ijk , 0 ) ;

   if( ncall <= 1 )
     ININFO_message(" InstaCorr elapsed time = %.2f sec: correlations",
                    PLUTO_elapsed_time()-etim) ;

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
                        ADN_func_type , FUNC_BUCK_TYPE ,
                        ADN_type      , HEAD_FUNC_TYPE ,
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
       AFNI_force_adoption( im3d->ss_now , False ) ;
       AFNI_make_descendants( GLOBAL_library.sslist ) ;
       INFO_message("created new dataset %s",im3d->iset->prefix) ;
     }

   /* just need to use existing dataset that matches */

   } else {

     icoset = slf.dset ; nds = slf.dset_index ;

   }
   icoset->dblk->diskptr->allow_directwrite = 1 ;

   /* save the result into the output dataset */

   iar = MRI_FLOAT_PTR(iim) ;
   EDIT_substitute_brick( icoset , 0 , MRI_float , iar ) ;
   mri_clear_data_pointer(iim) ; mri_free(iim) ;
   DSET_KILL_STATS(icoset) ; THD_load_statistics(icoset) ;

   if( ncall <= 1 )
     ININFO_message(" InstaCorr elapsed time = %.2f sec: dataset ops",PLUTO_elapsed_time()-etim) ;

   EDIT_BRICK_LABEL  (icoset,0,"Correlation") ;
   EDIT_BRICK_TO_FICO(icoset,0,im3d->iset->mv->nvals,1,im3d->iset->ndet) ;

   DSET_BRICK_FDRCURVE_ALLKILL(icoset) ;
   DSET_BRICK_MDFCURVE_ALLKILL(icoset) ;

   if( AFNI_yesenv("AFNI_INSTACORR_FDR") ){
     THD_create_all_fdrcurves(icoset) ;
     if( ncall <= 1 )
       ININFO_message(" InstaCorr elapsed time = %.2f sec: FDR curve",PLUTO_elapsed_time()-etim) ;
   }

   /* 10 May 2009: save seed timeseries into timeseries library */

   if( im3d->iset->tseed != NULL ){
     MRI_IMAGE *tsim ; float *tsar ;
     tsim = mri_new( im3d->iset->mv->nvals + im3d->iset->ignore,1,MRI_float ) ;
     tsar = MRI_FLOAT_PTR(tsim) ;
     memcpy( tsar + im3d->iset->ignore , im3d->iset->tseed ,
             sizeof(float)*im3d->iset->mv->nvals ) ;
     tsim->name = (char *)malloc(sizeof(char)*16) ;
     strcpy(tsim->name,im3d->iset->prefix) ; strcat(tsim->name,"_seed") ;
     AFNI_replace_timeseries(tsim) ;
   }

   /* redisplay overlay */

   if( im3d->fim_now != icoset ){  /* switch to this dataset */
     MCW_choose_cbs cbs ; char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     cbs.ival = nds ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                               (XtPointer)im3d ,  &cbs           ) ;
     AFNI_set_fim_index(im3d,0) ;
     AFNI_set_thr_index(im3d,0) ;
     sprintf(cmd,"SET_FUNC_RANGE %c.0.7",cpt[1]) ;
     AFNI_driver(cmd) ;
   }
   AFNI_reset_func_range(im3d) ;

   if( MCW_val_bbox(im3d->vwid->view->see_func_bbox) == 0 ){ /* overlay is off */
     char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     sprintf(cmd,"SEE_OVERLAY %c.+",cpt[1]) ;
     AFNI_driver(cmd) ;
   } else {                                                  /* overlay is on */
     AFNI_redisplay_func(im3d) ;
   }
   AFNI_set_thr_pval(im3d) ; AFNI_process_drawnotice(im3d) ;

   if( ncall <= 1 )
     ININFO_message(" InstaCorr elapsed time = %.2f sec: redisplay",PLUTO_elapsed_time()-etim) ;

   ncall++ ; RETURN(1) ;
}

/*------------------------------------------------------------------*/

void AFNI_icor_setref_locked( Three_D_View *im3d )
{
   Three_D_View *qq3d ; int ii,cc,qq , glock ; static int busy=0 ;

ENTRY("AFNI_icor_setref_locked") ;

   glock = GLOBAL_library.controller_lock ;

   if( busy )                       EXRETURN ;  /* routine already busy */
   if( glock == 0 )                 EXRETURN ;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )           EXRETURN ;  /* bad input */
   if( GLOBAL_library.ignore_lock ) EXRETURN ;  /* ordered not to do anything */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */
   if( ii < 0 )                     EXRETURN ;  /* bad input: shouldn't happen */
   if( ((1<<ii) & glock) == 0 )     EXRETURN ;  /* input not locked */

   busy = 1 ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
     qq3d = GLOBAL_library.controllers[cc] ; /* controller */
     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){
       qq = AFNI_icor_setref(qq3d) ;
       if( qq ){
         qq3d->vinfo->i1_icor = qq3d->vinfo->i1 ;
         qq3d->vinfo->j2_icor = qq3d->vinfo->j2 ;
         qq3d->vinfo->k3_icor = qq3d->vinfo->k3 ;
       }
     }
   }

   busy = 0 ; EXRETURN ;
}

/****************************************************************************/
/******* Group InstaCorr stuff below here! **********************************/
/****************************************************************************/

#define GIQUIT \
 do { free(im3d->giset); im3d->giset = NULL; return; } while(0)

void GICOR_setup_func( NI_stream nsg , NI_element *nel )
{
   GICOR_setup *giset ;
   char *atr ;
   Three_D_View *im3d = A_CONTROLLER ;

   if( im3d->giset != NULL && im3d->giset->ready ) return ;

   if( im3d->giset == NULL ){
     im3d->giset = (GICOR_setup *)calloc(1,sizeof(GICOR_setup)) ;
   }
   giset = im3d->giset ;

   giset->ns    = nsg ;
   giset->ready = 0 ;

   atr = NI_get_attribute( nel , "ndset_A" ) ;
   if( atr == NULL ) GIQUIT ;
   giset->ndset_A = (int)strtod(atr,NULL) ;
   if( giset->ndset_A < 2 ) GIQUIT ;

   atr = NI_get_attribute( nel , "ndset_B" ) ;
   if( atr == NULL ) GIQUIT ;
   giset->ndset_B = (int)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "nvec" ) ;
   if( atr == NULL ) GIQUIT ;
   giset->nvec = (int)strtod(atr,NULL) ;
   if( giset->nvec < 2 ) GIQUIT ;

   atr = NI_get_attribute( nel , "seedrad" ) ;
   if( atr != NULL ){
     giset->seedrad = (float)strtod(atr,NULL) ;
   }

   giset->ready = 1 ;
   GRPINCORR_LABEL_ON(im3d) ;
   return ;
}

/***********************************************************************
   Set up the interface to the user
************************************************************************/

static char g_helpstring[] =
  "Purpose: control AFNI Group InstaCorr operations\n"
  "\n"
  "Author -- RW Cox -- Dec 2009\n"
;

static char *topts[3] = { "pooled" , "unpooled" , "paired" } ;

static char * GICOR_main( PLUGIN_interface * ) ;

PLUGIN_interface * GICOR_init( char *lab )
{
   int ntops ;
   PLUGIN_interface *plint ;     /* will be the output of this routine */
   char sk[32] ;

   if( lab == NULL ) lab = "\0" ;

   if( !IM3D_OPEN(A_CONTROLLER) || A_CONTROLLER->giset == NULL || !A_CONTROLLER->giset->ready )
     return NULL ;

   /*---------------- set titles and call point ----------------*/

   sprintf(sk,"%sSetup Group InstaCorr",lab) ;
   plint = PLUTO_new_interface( "GrpInCorr" ,
                                sk ,
                                g_helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())GICOR_main  ) ;

   PLUTO_set_runlabels( plint , "Setup+Keep" , "Setup+Close" ) ;

   /*--------- make interface lines -----------*/

   PLUTO_add_option ( plint , "Params" , "Params" , TRUE ) ;
   PLUTO_add_number ( plint , "SeedRad" , 0,10,0,0, TRUE ) ;

   ntops = (A_CONTROLLER->giset->ndset_A == A_CONTROLLER->giset->ndset_B) ? 3 : 2 ;
   PLUTO_add_string ( plint , "t-test"  , ntops , topts , 0  ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * GICOR_main( PLUGIN_interface *plint )
{
   Three_D_View *im3d = plint->im3d ;
   float srad=0.0f ; int toption=0 ; char *tch ;
   GICOR_setup *giset ;

   if( !IM3D_OPEN(im3d)    ||
       im3d->giset == NULL ||
       !im3d->giset->ready   ){   /* should not happen */

     GRPINCORR_LABEL_OFF(im3d) ;
     if( im3d->giset != NULL ) im3d->giset->ready = 0 ;
     XtUnmapWidget(plint->wid->shell) ;
     return " ***** AFNI: ***** \n 3dGroupInCorr is no longer enabled!? \n " ;
   }

   giset = im3d->giset ;

   /* if socket has gone bad, we're done */

   if( NI_stream_goodcheck(giset->ns,1) < 1 ){
     GRPINCORR_LABEL_OFF(im3d) ;
     if( im3d->giset != NULL ) im3d->giset->ready = 0 ;
     XtUnmapWidget(plint->wid->shell) ;
     return " ***** AFNI: ***** \n 3dGroupInCorr is no longer connected! \n " ;
   }

   PLUTO_next_option(plint) ;
   srad    = PLUTO_get_number(plint) ;
   tch     = PLUTO_get_string(plint) ;
   toption = PLUTO_string_index( tch , 3 , topts ) ;

   /* do something with these changes */

   return NULL ;
}
