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
  "                [has the 0 and Nyquist frequencies removed; cf. Polort ]\n"
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
  "  are applied AFTER the dataset is blurred (if Blur > 0).\n"
  "\n"
  "    Polort   = polynomial detrending level prior to Bandpass (if that's on);\n"
  "                -1 == no detrending                               [for Ziad]\n"
  "                 0 == mean removal  [if you don't like mean things, I guess]\n"
  "                 1 == linear trend removal         [for nonlinear thinkers?]\n"
  "                 2 == quadratic trend removal                  [the default]\n"
  "               You should only change this option from '2' if you understand\n"
  "               what you are doing.\n"
  "            ** If Polort <= 0 AND if Bandpass is turned off, then the FFT\n"
  "               filtering of the time series will not be done at all.\n"
  "            ** There is essentially no reason I can think of to set Polort = -1\n"
  "               AND to turn Bandpass on -- and the results won't be pleasant.\n"
  "            ** If Polort > 0, then FFT filtering will be done even if Bandpass\n"
  "               is off -- but only the 0 and Nyquist frequencies will be removed.\n"
  "            ** To get NO preprocessing of the time series before correlation,\n"
  "               set Polort = -1, turn Bandpass off, and don't input Global Orts.\n"
  "\n"
  "    Method   = Pearson (moment), Spearman (rank), or Quadrant Correlation.\n"
  "            ** For short time series, all methods will be 'Insta'.  For longer\n"
  "               datasets, Spearman and Quadrant correlation will be perceptibly\n"
  "               slower than Pearson.\n"
  "\n"
  "OPERATION\n"
  "=========\n"
  "* Once you have set the controls the way you want, press one of the ''Setup'\n"
  "  buttons, and the program will process (filter & blur) the data time series.\n"
  "\n"
  "* When this processing is finished, you will be ready to use 'InstaCorr Set'\n"
  "  (or Shift-Ctrl-Left-click) and have some InstaCorr fun!\n"
  "\n"
  "* The 'InstaCorr SeedJump' popup menu item will jump the crosshairs back\n"
  "  to the voxel that is the currently used InstaCorr seed.\n"
  "\n"
  "* The current seed time series is saved in the 1D 'library', and can be\n"
  "  plotted in a graph viewer using the 'FIM->Pick Ideal' menu item.\n"
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
   static char *meth_string[4] = { "Pearson" , "Spearman" ,
                                   "Quadrant", "Ken Tau_b"  } ;
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
   PLUTO_add_number ( plint , "Index" , 0,ICOR_MAX_FTOP,0,0,TRUE ) ;

   PLUTO_add_option( plint , "Bandpass(Hz)" , "Bandpass" , MAYBE ) ;
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
   PLUTO_add_number( plint , "Polort" , -1,2,0,2 , FALSE ) ;
   PLUTO_add_string( plint , "Method" , 4 , meth_string , 0 ) ;

   return plint ;
}

#define ICOR_BIG (9.999f*ICOR_MAX_FTOP)

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * ICOR_main( PLUGIN_interface *plint )
{
   char *tag ;
   float fbot=-1.0f , ftop=ICOR_BIG ;
   MRI_IMAGE *gortim=NULL ;
   THD_3dim_dataset *dset=NULL , *mset=NULL ;
   int ignore=0 , mindex=0 , automask=0 , qq ; float blur=0.0f , sblur=0.0f ;
   ICOR_setup *iset ; char *cpt ;
   Three_D_View *im3d = plint->im3d ;
   double etim ;
   int polort = 2 ; /* 26 Feb 2010 */
   int cmeth  = NBISTAT_PEARSON_CORR ;

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
       char *cm ;
       sblur  = PLUTO_get_number(plint) ;
       polort = PLUTO_get_number(plint) ;  /* 26 Feb 2010 */
       cm     = PLUTO_get_string(plint) ;
       switch( cm[0] ){
         default:  cmeth = NBISTAT_PEARSON_CORR  ; break ;
         case 'S': cmeth = NBISTAT_SPEARMAN_CORR ; break ;
         case 'Q': cmeth = NBISTAT_QUADRANT_CORR ; break ;
         case 'K': cmeth = NBISTAT_KENDALL_TAUB  ; break ;
       }
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

   if( fbot >= ftop ){ fbot = 0.0f ; ftop = ICOR_BIG ; }
   if( fbot <  0.0f )  fbot = 0.0f ;

   if( polort == -1 && (fbot > 0.0f || ftop < ICOR_MAX_FTOP) ) /* 26 Feb 2010 */
     WARNING_message("Combining Polort=-1 and Bandpass may give peculiar results!") ;

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
       im3d->iset->blur     == blur     &&
       im3d->iset->polort   == polort      ){

     INFO_message("InstaCorr setup: minor changes accepted") ;
     im3d->iset->sblur = sblur ; im3d->iset->cmeth = cmeth ; return NULL ;
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
   iset->polort   = polort ;  /* 26 Feb 2010 */
   iset->cmeth    = cmeth ;   /* 01 Mar 2010 */
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

/*-------------------------------------------------------------------------*/
/* Seed location at crosshairs */

int AFNI_icor_setref( Three_D_View *im3d )
{
   int ijk ;

ENTRY("AFNI_icor_setref") ;

   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   ijk = AFNI_icor_setref_xyz( im3d , im3d->vinfo->xi ,
                                      im3d->vinfo->yj , im3d->vinfo->zk ) ;
   RETURN(ijk) ;
}

/*-------------------------------------------------------------------------*/
/* Seed location at anat dataset i,j,k voxel indexes */

int AFNI_icor_setref_anatijk( Three_D_View *im3d , int ii,int jj,int kk )
{
   int ijk ;
   THD_ivec3 iv ; THD_fvec3 fv ;

ENTRY("AFNI_icor_setref_anatijk") ;

   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /* convert (i,j,k) to DICOM */

   LOAD_IVEC3(iv,ii,jj,kk) ;
   fv = THD_3dind_to_3dmm ( im3d->anat_now , iv ) ;
   fv = THD_3dmm_to_dicomm( im3d->anat_now , fv ) ;

   ijk = AFNI_icor_setref_xyz( im3d , fv.xyz[0] , fv.xyz[1] , fv.xyz[2] ) ;
   RETURN(ijk) ;
}

/*-----------------------------------------------------------------------*/
/* Seed location at DICOM x,y,z location */

int AFNI_icor_setref_xyz( Three_D_View *im3d , float xx,float yy,float zz )
{
   MRI_IMAGE *iim; float *iar; THD_fvec3 iv,jv; THD_ivec3 kv; int ijk;
   THD_3dim_dataset *icoset ; THD_slist_find slf ; int nds=0 ;
   double etim ;

ENTRY("AFNI_icor_setref_xyz") ;

   if( !IM3D_OPEN(im3d) ) RETURN(-1) ;

   /**** divert to Group InstaCorr? ****/

   if( im3d->giset != NULL && im3d->giset->ready ){
     ijk = AFNI_gicor_setref_xyz(im3d,xx,yy,zz) ; RETURN(ijk) ;
   }

   if( !ISVALID_ICOR_setup(im3d->iset) ) RETURN(-1) ;

   /* find where we are working from, in dataset coordinates */

   LOAD_FVEC3( iv , xx,yy,zz ) ;
   jv = THD_dicomm_to_3dmm( im3d->iset->dset, iv ) ;

   /* test if this point is possibly OK */

   if( jv.xyz[0] < im3d->iset->dset->daxes->xxmin ||
       jv.xyz[0] > im3d->iset->dset->daxes->xxmax ||
       jv.xyz[1] < im3d->iset->dset->daxes->yymin ||
       jv.xyz[1] > im3d->iset->dset->daxes->yymax ||
       jv.xyz[2] < im3d->iset->dset->daxes->zzmin ||
       jv.xyz[2] > im3d->iset->dset->daxes->zzmax   ){

     WARNING_message("InstaCorr set point outside dataset box") ;
     RETURN(-1) ;
   }

   /* convert to index in the InstaCorr dataset */

   kv  = THD_3dmm_to_3dind_no_wod( im3d->iset->dset, jv ) ;
   ijk = DSET_ixyz_to_index( im3d->iset->dset, kv.ijk[0],kv.ijk[1],kv.ijk[2] ) ;

   /* do the real work: ijk = voxel index */

   etim = PLUTO_elapsed_time() ;

   iim = THD_instacorr( im3d->iset , ijk , 0 ) ;  /* 0 == no arctanh */

   if( ncall <= 1 )
     ININFO_message(" InstaCorr elapsed time = %.2f sec: correlations",
                    PLUTO_elapsed_time()-etim) ;

   if( iim == NULL ) RETURN(-1) ;  /* did it fail? */

   /* 17 Mar 2010: save seed location we just did in the im3d struct */

   im3d->vinfo->xi_icor = xx ;
   im3d->vinfo->yj_icor = yy ;
   im3d->vinfo->zk_icor = zz ;

   kv = THD_3dmm_to_3dind ( im3d->anat_now , jv ) ;
   UNLOAD_IVEC3( kv , im3d->vinfo->i1_icor ,
                      im3d->vinfo->j2_icor , im3d->vinfo->k3_icor ) ;

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
     ININFO_message(" InstaCorr elapsed time = %.2f sec: dataset ops" ,
                    PLUTO_elapsed_time()-etim ) ;

   EDIT_BRICK_LABEL  (icoset,0,"Correlation") ;
   EDIT_BRICK_TO_FICO(icoset,0,im3d->iset->mv->nvals,1,im3d->iset->ndet) ;

   DSET_BRICK_FDRCURVE_ALLKILL(icoset) ;
   DSET_BRICK_MDFCURVE_ALLKILL(icoset) ;

   if( AFNI_yesenv("AFNI_INSTACORR_FDR") ){
     THD_create_all_fdrcurves(icoset) ;
     if( ncall <= 1 )
       ININFO_message(" InstaCorr elapsed time = %.2f sec: FDR curve" ,
                      PLUTO_elapsed_time()-etim ) ;
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
     ININFO_message(" InstaCorr elapsed time = %.2f sec: redisplay" ,
                    PLUTO_elapsed_time()-etim ) ;

   ncall++ ; RETURN(1) ;
}

/*---------------------------------------------------------------------------*/
/* Set seeds at same x,y,z location as the 'master' viewer.
   This locking is only for individual dataset InstaCorr,
   since only controller A can have Group InstaCorr running.
*//*-------------------------------------------------------------------------*/

void AFNI_icor_setref_locked( Three_D_View *im3d )
{
   Three_D_View *qq3d ; int ii,cc , glock ; static int busy=0 ;

ENTRY("AFNI_icor_setref_locked") ;

   if( !IM3D_OPEN(im3d)                          ) EXRETURN ;
   if( im3d->giset != NULL && im3d->giset->ready ) EXRETURN ;

   glock = GLOBAL_library.controller_lock ;

   if( busy )                       EXRETURN ;  /* routine already busy */
   if( glock == 0 )                 EXRETURN ;  /* nothing to do */
   if( GLOBAL_library.ignore_lock ) EXRETURN ;  /* ordered not to do anything */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */
   if( ii < 0 )                     EXRETURN ;  /* bad input: shouldn't happen */
   if( ((1<<ii) & glock) == 0 )     EXRETURN ;  /* input not locked */

   busy = 1 ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
     qq3d = GLOBAL_library.controllers[cc] ; /* controller */
     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 )
       (void)AFNI_icor_setref_xyz( qq3d ,
                                   im3d->vinfo->xi_icor,
                                   im3d->vinfo->yj_icor, im3d->vinfo->zk_icor);
   }

   busy = 0 ; EXRETURN ;
}

/******************************************************************************/
/*****--- Group InstaCorr stuff below here! ------------------------------*****/
/******************************************************************************/

#define GIQUIT \
 do { free(im3d->giset); im3d->giset = NULL; EXRETURN; } while(0)

/*-- Called from afni_niml.c when 3dGroupInCorr sends a setup NIML element --*/

void GICOR_setup_func( NI_stream nsg , NI_element *nel )
{
   GICOR_setup *giset ;
   char *atr , *pre ;
   Three_D_View *im3d = A_CONTROLLER ;  /* global variable */
   THD_slist_find sf ;
   THD_session *ss = im3d->ss_now ; int qs = ss->num_dsset , vv,qq ;
   THD_3dim_dataset *dset ; int nvals=2 ;
   static char *blab[6] = { "GIC_Delta" , "GIC_Zscore" ,
                            "AAA_Delta" , "AAA_Zscore" ,
                            "BBB_Delta" , "BBB_Zscore"  } ;
   NI_str_array *labar=NULL ;

ENTRY("GICOR_setup_func") ;

   /* if we are ready, nothing more to do (don't setup twice) */

   if( im3d->giset != NULL && im3d->giset->ready ) EXRETURN ;

   /* if can't shoehorn in another dataset, give up in disgust */

   if( qs >= THD_MAX_SESSION_SIZE ){
     (void) MCW_popup_message( im3d->vwid->picture ,
                                 " \n"
                                 " ******* AFNI: ******* \n"
                                 "  Can't use GrpInCorr  \n"
                                 " because dataset table \n"
                                 "  is completely full!  \n " ,
                               MCW_USER_KILL | MCW_TIMER_KILL ) ;
     NI_stream_closenow(nsg) ;
     DISABLE_GRPINCORR(im3d) ;
     SENSITIZE_INSTACORR(im3d,False) ;
     DESTROY_GICOR_setup(im3d->giset) ;
   }

   /* create or clear out Group InstaCorr setup struct */

   if( im3d->giset == NULL ){
     im3d->giset = (GICOR_setup *)calloc(1,sizeof(GICOR_setup)) ;
   } else {
     memset(im3d->giset,sizeof(GICOR_setup),0) ;
   }
   giset = im3d->giset ;

   giset->ns    = nsg ;  /* save socket for I/O back to 3dGroupInCorr */
   giset->ready = 0 ;    /* not ready yet */
   giset->busy  = 0 ;    /* not busy yet, either [18 Mar 2010] */

   /* set various parameters from the NIML header */

   atr = NI_get_attribute( nel , "ndset_A" ) ; if( atr == NULL )        GIQUIT;
   giset->ndset_A = (int)strtod(atr,NULL) ;    if( giset->ndset_A < 2 ) GIQUIT;

   atr = NI_get_attribute( nel , "ndset_B" ) ; if( atr == NULL )        GIQUIT;
   giset->ndset_B = (int)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "nvec" ) ;    if( atr == NULL )        GIQUIT;
   giset->nvec = (int)strtod(atr,NULL) ;       if( giset->nvec < 2 )    GIQUIT;

   atr = NI_get_attribute( nel , "seedrad" ) ;
   if( atr != NULL ) giset->seedrad = (float)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "ttest_opcode" ) ;
   if( atr != NULL ) giset->ttest_opcode = (int)strtod(atr,NULL) ;

   /* create output dataset, to be filled in from 3dGroupInCorr data later */

   atr = NI_get_attribute( nel , "geometry_string" ); if( atr == NULL ) GIQUIT;
   pre = NI_get_attribute( nel , "target_name" ) ;
   if( pre == NULL || *pre == '\0' ) pre = "A_GRP_ICORR" ;
   dset = giset->dset = EDIT_geometry_constructor( atr , pre ) ;
                                                     if( dset == NULL ) GIQUIT;

   atr = NI_get_attribute( nel , "target_nvals" ) ;
   if( atr != NULL ){ nvals = (int)strtod(atr,NULL); nvals = MAX(1,nvals); }
   vv = AFNI_yesenv("AFNI_GROUPINCORR_ORIG") ;
   EDIT_dset_items( dset , ADN_nvals     , nvals ,
                           ADN_view_type , (vv) ? VIEW_ORIGINAL_TYPE
                                                : VIEW_TALAIRACH_TYPE ,
                           ADN_brick_fac , NULL ,
                    ADN_none ) ;

   atr = NI_get_attribute( nel , "target_labels" ) ;
   if( atr != NULL )
     labar = NI_decode_string_list( atr , ";" ) ;

   for( vv=0 ; vv < nvals ; vv++ ){
     if( labar != NULL && vv < labar->num )
       EDIT_BRICK_LABEL( dset , vv , labar->str[vv] ) ;
     else if( vv < 6 )
       EDIT_BRICK_LABEL( dset , vv , blab[vv] ) ;
     EDIT_substitute_brick( dset, vv, MRI_float, NULL ) ; /* calloc sub-brick */
     if( vv%2 == 1 ) EDIT_BRICK_TO_FIZT(dset,vv) ;   /* alternate ones are Z */
   }
   DSET_superlock( dset ) ;
   giset->nvox = DSET_NVOX(dset) ;

   /* add dataset to current session (change name if necessary) */

   sf = THD_dset_in_session( FIND_PREFIX , pre , ss ) ;
   if( sf.dset != NULL ){
     int jj,nn ; char *npre ;
     nn = strlen(pre); npre = malloc(sizeof(char)*(nn+8)); strcpy(npre,pre);
     for( jj=1 ; ; jj++ ){  /* loop until we find something that works */
       sprintf(npre+nn,"_%d",jj) ;
       sf = THD_dset_in_session( FIND_PREFIX , npre , ss ) ;
       if( sf.dset == NULL ) break ;
     }
     EDIT_dset_items( dset , ADN_prefix , npre , ADN_none ) ;
     free(npre) ;
   }

   POPDOWN_strlist_chooser ;

   vv = dset->view_type ;
   if( vv < FIRST_VIEW_TYPE || vv > LAST_VIEW_TYPE ) vv = FIRST_VIEW_TYPE;
   for( qq=FIRST_VIEW_TYPE ; qq <= LAST_VIEW_TYPE ; qq++ )
     ss->dsset[qs][qq] = NULL ;
   ss->dsset[qs][vv] = dset ; ss->num_dsset++ ; giset->nds = qs ;
   giset->session = ss ;

   UNDUMMYIZE ;

   /* list of voxels to expect from each 3dGroupInCorr data */

   if( nel->vec_len == 0 || nel->vec_num == 0 || nel->vec == NULL ){  /* all */
     giset->ivec = NULL ; giset->nivec = 0 ;
/* INFO_message("DEBUG: GICOR_setup_func has ivec=NULL") ; */
   } else {                                     /* make index list of voxels */
     int ii , nn , *iv=(int *)nel->vec[0] ;
     giset->ivec = (int *)calloc(sizeof(int),giset->nvec) ;
     nn = MIN(giset->nvec,nel->vec_len) ; giset->nivec = nn ;
     for( ii=0 ; ii < nn ; ii++ ) giset->ivec[ii] = iv[ii] ;
/* INFO_message("DEBUG: GICOR_setup_func has ivec=int[%d]",nn) ; */
   }

   giset->ready = 1 ;          /* that is, ready to ROCK AND ROLL */
   GRPINCORR_LABEL_ON(im3d) ;
   SENSITIZE_INSTACORR(im3d,True) ;

#undef  PUTENV
#define PUTENV(nm,val) do{ if( getenv((nm)) == NULL ){           \
                             char *str = (char *)malloc(256) ;   \
                             strcpy(str,(nm)); strcat(str,"=");  \
                             strcat(str,val);  putenv(str);      \
                           }} while(0)

   PUTENV("AFNI_THRESH_LOCK","VALUE") ;
   PUTENV("AFNI_RANGE_LOCK" ,"YES"  ) ;

   /* some messages to the screen */

   INFO_message("Added 3dGroupInCorr dataset '%s' to controller %s",
                DSET_FILECODE(dset), AFNI_controller_label(im3d) ) ;
   ININFO_message("%d datasets in set A",giset->ndset_A) ;
   if( giset->ndset_B > 0 )
     ININFO_message("%d datasets in set B",giset->ndset_B) ;
   ININFO_message("----- AFNI is connnected to 3dGroupInCorr -----") ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Called from afni_niml.c to process the received dataset from 3dGroupInCorr */

void GICOR_process_dataset( NI_element *nel , int ct_start )
{
   Three_D_View *im3d = A_CONTROLLER , *qq3d ;
   GICOR_setup *giset = im3d->giset ;
   float *nelar , *dsdar ;
   int nvec,nn,vv , vmul ; float thr ;

ENTRY("GICOR_process_dataset") ;

   if( nel == NULL || nel->vec_num < 2 ){  /* should never happen */
     ERROR_message("badly formatted dataset from 3dGroupInCorr!") ;
     EXRETURN ;
   }

   nvec = nel->vec_len ;  /* how many values in each column transmitted */

   if( !IM3D_OPEN(im3d) ||
       giset == NULL    ||
       !giset->ready      ){   /* should never happen */

     GRPINCORR_LABEL_OFF(im3d) ; SENSITIZE_INSTACORR(im3d,False) ;
     if( giset != NULL ) giset->ready = giset->busy = 0 ;
     AFNI_misc_CB(im3d->vwid->func->gicor_pb,(XtPointer)im3d,NULL) ;
     (void) MCW_popup_message( im3d->vwid->picture ,
                                 " \n"
                                 " ******* AFNI: *********\n"
                                 "  3dGrpInCorr sent data \n"
                                 "  but setup isn't ready!\n " ,
                               MCW_USER_KILL | MCW_TIMER_KILL ) ;
     EXRETURN ;
   }

   /* copy NIML data into dataset */

   for( vv=0 ; vv < DSET_NVALS(giset->dset) ; vv++ ){
     nelar = (float *)nel->vec[vv] ;                /* NIML array */
     dsdar = (float *)DSET_ARRAY(giset->dset,vv) ;  /* dataset array */

     if( giset->ivec == NULL ){               /* all voxels */
       nn = MIN( giset->nvox , nvec ) ;
       memcpy(dsdar,nelar,sizeof(float)*nn) ;
     } else {                                 /* some voxels */
       int *ivec=giset->ivec , kk ;
       nn = MIN( giset->nivec , nvec ) ;
       for( kk=0 ; kk < nn ; kk++ ) dsdar[ivec[kk]] = nelar[kk] ;
     }
   }

   /* allow dset to be written out [ZSS Jan 2010] */

   giset->dset->dblk->diskptr->allow_directwrite = 1 ;

   /* switch to this dataset as overlay */

   if( im3d->fim_now != giset->dset ){
     MCW_choose_cbs cbs ; char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     cbs.ival = giset->nds ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                               (XtPointer)im3d ,  &cbs           ) ;
     AFNI_set_fim_index(im3d,0) ;
     AFNI_set_thr_index(im3d,1) ;
#if 1
     sprintf(cmd,"SET_FUNC_RANGE %c 0.2"   ,cpt[1]) ; AFNI_driver(cmd) ;
     sprintf(cmd,"SET_THRESHNEW %c 0.01 *p",cpt[1]) ; AFNI_driver(cmd) ;
#endif
   }

   /* self-threshold and clusterize? */

#undef  THBOT
#undef  THTOP
#undef  THBIG
#define THBIG    1.e+9f
#define THBOT(t) ((thrsign==0 || thrsign==2) ? (-(t)) : (-THBIG))
#define THTOP(t) ((thrsign==0 || thrsign==1) ? (t)    :  (THBIG))

   vmul = giset->vmul ;
   thr  = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;
   if( vmul > 0 && thr > 0.0f ){
     MRI_IMAGE *dsim , *tsim , *clim ;
     int thrsign=im3d->vinfo->thr_sign , pfun=im3d->vinfo->use_posfunc ;
     float thb,tht ;

     thb = THBOT(thr) ; tht = THTOP(thr) ;

     dsim = DSET_BRICK(giset->dset,im3d->vinfo->fim_index) ;
     tsim = DSET_BRICK(giset->dset,im3d->vinfo->thr_index) ;
     clim = mri_clusterize( 0.0f , vmul , dsim , thb,tht,tsim , pfun ) ;
     if( clim != NULL ){
       float *csar = MRI_FLOAT_PTR(clim) ;
       memcpy( MRI_FLOAT_PTR(dsim) , csar , sizeof(float)*clim->nvox ) ;
       mri_free(clim) ;
     }
   }
   DSET_KILL_STATS(giset->dset) ; THD_load_statistics(giset->dset) ;
   AFNI_reset_func_range(im3d) ;

   /* redisplay overlay */

   if( MCW_val_bbox(im3d->vwid->view->see_func_bbox) == 0 ){ /* overlay = off */
     char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     sprintf(cmd,"SEE_OVERLAY %c.+",cpt[1]) ;
     AFNI_driver(cmd) ;
   } else {                                                  /* overlay = on */
     AFNI_redisplay_func(im3d) ;
   }
   AFNI_set_thr_pval(im3d) ; AFNI_process_drawnotice(im3d) ;

   for( vv=1 ; vv < MAX_CONTROLLERS ; vv++ ){  /* other controllers need redisplay? */
     qq3d = GLOBAL_library.controllers[vv] ;
     if( !IM3D_OPEN(qq3d) ) continue ;
     if( qq3d->fim_now == giset->dset && MCW_val_bbox(qq3d->vwid->view->see_func_bbox) ){
       AFNI_reset_func_range(qq3d) ; AFNI_redisplay_func(qq3d) ;
     }
   }

   giset->busy = 0 ; /* Not busy waiting anymore [18 Mar 2010] */
   GRPINCORR_LABEL_ON(im3d) ;                  /* 07 Apr 2010 */
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/* Called to set the reference voxel,
   and to start the calculations over in 3dGroupInCorr.
*//*------------------------------------------------------------------------*/

int AFNI_gicor_setref_xyz( Three_D_View *im3d , float xx,float yy,float zz )
{
   NI_element *nel ;
   char buf[256] ;
   GICOR_setup *giset = im3d->giset ;
   THD_fvec3 iv,jv; THD_ivec3 kv; int ijk,ii ;

ENTRY("AFNI_gicor_setref_xyz") ;

   if( !IM3D_OPEN(im3d) ||
       giset == NULL    ||
       !giset->ready      ){   /* should not happen */

STATUS("something wrong in the setup") ;

     GRPINCORR_LABEL_OFF(im3d) ; SENSITIZE_INSTACORR(im3d,False) ;
     if( giset != NULL ) giset->ready = giset->busy = 0 ;
     AFNI_misc_CB(im3d->vwid->func->gicor_pb,(XtPointer)im3d,NULL) ;
     RETURN(-1) ;
   }

   /* if socket has gone bad, we're done */

STATUS("check socket to 3dGroupInCorr") ;

   if( NI_stream_goodcheck(giset->ns,1) < 1 ){
     GRPINCORR_LABEL_OFF(im3d) ; SENSITIZE_INSTACORR(im3d,False) ;
     if( giset != NULL ) giset->ready = giset->busy = 0 ;
     AFNI_misc_CB(im3d->vwid->func->gicor_pb,(XtPointer)im3d,NULL) ;
     RETURN(-1) ;
   }

STATUS("check if already busy") ;

   if( giset->busy ){                     /* Already waiting? [18 Mar 2010] */
#if 0
     MCW_flash_widget( 2 , im3d->vwid->func->gicor_label ) ; /* 07 Apr 2010 */
#endif
     RETURN(0) ;
   }

   /* find where we are working from, in dataset coordinates */

STATUS("check coordinates") ;

   LOAD_FVEC3( iv , xx,yy,zz ) ;
   jv = THD_dicomm_to_3dmm( giset->dset, iv ) ;

   if( jv.xyz[0] < giset->dset->daxes->xxmin ||
       jv.xyz[0] > giset->dset->daxes->xxmax ||
       jv.xyz[1] < giset->dset->daxes->yymin ||
       jv.xyz[1] > giset->dset->daxes->yymax ||
       jv.xyz[2] < giset->dset->daxes->zzmin ||
       jv.xyz[2] > giset->dset->daxes->zzmax   ){

     WARNING_message("GrpInCorr set point outside dataset box") ;
     RETURN(-1) ;
   }

STATUS("transform coords to index" ) ;

   kv  = THD_3dmm_to_3dind_no_wod( giset->dset, jv ) ;
   ijk = DSET_ixyz_to_index( giset->dset, kv.ijk[0],kv.ijk[1],kv.ijk[2] ) ;

   /** INFO_message("DEBUG: AFNI_gicor_setref called: ijk=%d",ijk) ; **/

   if( giset->ivec != NULL ){
STATUS("search for index in mask") ;
     ii = bsearch_int( ijk , giset->nvec , giset->ivec ) ;
     if( ii < 0 ){
       WARNING_message("GrpInCorr set point not in mask from 3dGroupInCorr") ;
       RETURN(-1) ;
     }
   }

   /* send ijk node index to 3dGroupInCorr */

STATUS("create NIML element to send info") ;

   nel = NI_new_data_element( "SETREF_ijk" , 0 ) ;

   sprintf( buf , "%d" , ijk ) ;
   NI_set_attribute( nel , "index" , buf ) ;

   sprintf( buf , "%g" , giset->seedrad ) ;
   NI_set_attribute( nel , "seedrad" , buf ) ;

   sprintf( buf , "%d" , giset->ttest_opcode ) ;
   NI_set_attribute( nel , "ttest_opcode" , buf ) ;

STATUS("send NIML element") ;

   ii = NI_write_element( giset->ns , nel , NI_TEXT_MODE ) ;
   NI_free_element( nel ) ;
   if( ii <= 0 ){
     ERROR_message("3dGroupInCorr connection has failed?!") ;
     RETURN(-1) ;
   }

STATUS("This is my only chance at building a disreputable past.") ;
   /* 12 Apr 2010: save this seed location ZSS */
   LOAD_FVEC3( iv , xx,yy,zz ) ; /* reload to be safe, use of anat_now */
   im3d->vinfo->xi_icor = xx ;   /* should do the trick. */
   im3d->vinfo->yj_icor = yy ;
   im3d->vinfo->zk_icor = zz ;
   jv = THD_dicomm_to_3dmm( im3d->anat_now , iv ) ;
   kv = THD_3dmm_to_3dind ( im3d->anat_now , jv ) ;
   UNLOAD_IVEC3( kv , im3d->vinfo->i1_icor ,
                      im3d->vinfo->j2_icor , im3d->vinfo->k3_icor ) ;
   INFO_message("Called GICOR from %.2f %.2f %.2f (RAI), "
                "seed radius %g",
                xx, yy, zz, giset->seedrad) ;

STATUS("mark that we're busy for now") ;

   giset->busy = 1 ; /* Mark that we're busy right now [18 Mar 2010] */
   GRPINCORR_LABEL_WORKING(im3d) ;                   /* 07 Apr 2010 */
   RETURN(0) ;
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
   GICOR_setup *giset = A_CONTROLLER->giset ;

   if( lab == NULL ) lab = "\0" ;

   if( !IM3D_OPEN(A_CONTROLLER) || giset == NULL || !giset->ready )
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
   PLUTO_add_number ( plint , "SeedRad" , 0,16,0,(int)rint(giset->seedrad), TRUE ) ;

   ntops = (giset->ndset_A == giset->ndset_B) ? 3 : 2 ;
   PLUTO_add_string ( plint , "t-test"  , ntops , topts , giset->ttest_opcode ) ;

   PLUTO_add_option ( plint , "Cluster" , "Cluster" , TRUE ) ;
   PLUTO_add_number ( plint , "Voxels"  , 0,9999,0 , 0,TRUE ) ;

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
   float srad=0.0f ; int topcod=0 , vmul=0 ; char *tch ;
   GICOR_setup *giset ;

   if( !IM3D_OPEN(im3d)    ||
       im3d->giset == NULL ||
       !im3d->giset->ready   ){   /* should not happen */

     GRPINCORR_LABEL_OFF(im3d) ; SENSITIZE_INSTACORR(im3d,False) ;
     if( im3d->giset != NULL ) im3d->giset->ready = im3d->giset->busy = 0 ;
     XtUnmapWidget(plint->wid->shell) ;
     return " ************ AFNI: ************ \n 3dGroupInCorr is no longer enabled!? \n " ;
   }

   giset = im3d->giset ;

   /* if socket has gone bad, we're done */

   if( NI_stream_goodcheck(giset->ns,1) < 1 ){
     GRPINCORR_LABEL_OFF(im3d) ; SENSITIZE_INSTACORR(im3d,False) ;
     if( giset != NULL ) giset->ready = giset->busy = 0 ;
     XtUnmapWidget(plint->wid->shell) ;
     return " ************ AFNI: ************ \n 3dGroupInCorr is no longer connected! \n " ;
   }

   PLUTO_next_option(plint) ;
   srad   = PLUTO_get_number(plint) ;
   tch    = PLUTO_get_string(plint) ;
   topcod = PLUTO_string_index( tch , 3 , topts ) ;

   PLUTO_next_option(plint) ;
   vmul   = (int)PLUTO_get_number(plint) ;

   /* do something with these changes */

   giset->seedrad      = srad ;
   giset->ttest_opcode = topcod ;
   giset->vmul         = vmul ;

   return NULL ;
}
