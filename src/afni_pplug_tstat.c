#include "afni.h"
#ifndef ALLOW_PLUGINS
PLUGIN_interface * TSTAT_init(char *lab)
{
  MCW_popup_message( THE_TOPSHELL ,
                     " \n"
                     " Tstat not available,\n"
                     " since this copy of AFNI\n"
                     " was compiled without\n"
                     " support for plugins :(\n " , MCW_USER_KILL ) ;
  return NULL ;
}
#else

/***********************************************************************
  Pseudo-plugin to setup Tstat operations
************************************************************************/

static int ncall=0 ;

static unsigned int called_before[26] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;

/*--------------------- string to 'help' the user --------------------*/

static char t_helpstring[] =
  "Purpose: control AFNI Tstat operations\n"
  "\n"
  "================  Voxel-wise Statistics of 3D+time Datasets  ===============\n"
  "\n"
  "CONTROLS\n"
  "========\n"
  "* TimeSeries:\n"
  "    Dataset   = time series dataset to auto-correlate\n"
  "                 [this dataset does NOT have to be the Underlay]\n"
  "    Start,End = indexes of start and stop times\n"
  "                 Examples (assume 100 sub-bricks total):\n"
  "                   0,50  = correlate with first 51 time points\n"
  "                   10    = correlate with time points 10..99\n"
  "                   10+30 = correlate with 30 time points, 10..39\n"
  "                 [if End is missing or 0, then it is the last sub-brick]\n"
  "                 [N.B.: 'Start,End' replaces 'Ignore', as of Nov 2012]\n"
  " * Operation:\n"
  "    Statistic = What value to compute at each voxel :)\n"
#if 0
  "    Despike  = If this is YES, then the time series has large spikes\n"
  "                filtered out BEFORE the detrending operation (if any).\n"
  "                This option is here to let you process datasets that have\n"
  "                a few large spikes, which could otherwise totally dominate\n"
  "                the statistic calculation.\n"
  "    Polort   = polynomial detrending level\n"
  "                -1 == no detrending                            [the default]\n"
  "                 0 == mean removal  [if you don't like mean things, I guess]\n"
  "                 1 == linear trend removal         [for nonlinear thinkers?]\n"
  "                 2 == quadratic trend removal\n"
  "               You should only change this option from '-1' if you understand\n"
  "               what you are doing.\n"
#endif
  "\n"
  "COMPUTING\n"
  "=========\n"
  "* Once you have set the controls the way you want, press one of the 'Run'\n"
  "  buttons, and the program will process the data time series.\n"
  "* 'Run+Keep' will compute the result and keep the interface open.\n"
  "* 'Run+Quit' will compute the result and close the interface.\n"
  "\n"
  "Author -- RW Cox -- Mar 2018\n"
;

/*----------------- prototypes for internal routines -----------------*/

static char * TSTAT_main( PLUGIN_interface * ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

#include "Tstat.h"

static char **meth_used=NULL ;
static int   *meth_nums=NULL , num_meths=0 ;

PLUGIN_interface * TSTAT_init( char *lab )
{
   PLUGIN_interface *plint ;     /* will be the output of this routine */
   static char *yn[2] = { "No" , "Yes" } ;

   char sk[32] , sc[32] ;

ENTRY("TSTAT_init") ;

   if( lab == NULL ) lab = "\0" ;

   /*---------------- set titles and call point ----------------*/

   sprintf(sk,"%sSetup Tstat",lab) ;
   plint = PLUTO_new_interface( "Tstat" ,
                                sk ,
                                t_helpstring ,
                                PLUGIN_CALL_VIA_MENU ,
                                (char *(*)())TSTAT_main  ) ;

   sprintf(sk,"%sRun+Keep",lab) ; sprintf(sc,"%sRun+Quit",lab) ;
   PLUTO_set_runlabels( plint , sk , sc ) ;

   /*--------- make interface lines -----------*/

   PLUTO_add_option ( plint , "TimeSeries" , "TimeSeries" , TRUE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                      ANAT_ALL_MASK , FUNC_ALL_MASK , DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_string ( plint , "Start,End" , 0,NULL,10 ) ;

   if( meth_used == NULL ){                        /* initialize method table */
     int ii , nmax=sizeof(meth_pluginned)/sizeof(int) ;
     meth_used = (char **)malloc(sizeof(char *)*nmax) ;
     meth_nums = (int   *)malloc(sizeof(int   )*nmax) ;
     for( num_meths=ii=0 ; ii < nmax ; ii++ ){
       if( meth_pluginned[ii] ){
         meth_used[num_meths] = strdup(meth_names[ii]) ;
         meth_nums[num_meths] = ii ; num_meths++ ;
       }
     }
   }

   PLUTO_add_option( plint , "Operation" , "Operation" , TRUE ) ;
   PLUTO_add_string( plint , "Statistic" , num_meths , meth_used , 0 ) ;
#if 0
   PLUTO_add_string( plint , "Despike" , 2 , yn , 0 ) ;
   PLUTO_add_number( plint , "Polort" , -1,2,0,-1 , FALSE ) ;
#endif

   RETURN(plint) ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * TSTAT_main( PLUGIN_interface *plint )
{
   char *tag , *cpt , *opname=NULL ;
   int meth_indx=-1 , meth_code=-1 , nim,qim,nds,ic , do_relabel=1 ;

   THD_3dim_dataset *dset=NULL , *tset ;
   int start=0,end=0 ;
   Three_D_View *im3d = plint->im3d ;
   int polort = -1 , despike = 0 ;
   char cmd[4096] , tname[64] ;
   THD_3dim_dataset *iiset ; THD_slist_find slf ; char iiprefix[64] ;
   MRI_IMAGE *iim=NULL ; float *iar=NULL ;

   /*** ncall = 0 ; ***/

   /* check rationality */

   if( !IM3D_OPEN(im3d) || im3d->vwid->func->options_vedit_av->ival != VEDIT_TSTAT ){
     XtUnmapWidget(plint->wid->shell); return NULL;
   }

   ic = AFNI_controller_index(im3d) ; if( ic < 0 || ic > 25 ) return "** Bad ic?! **" ;

   /*--- loop over input option lines ---*/

   while(1){
     tag = PLUTO_get_optiontag(plint) ;  /* which line? */
     if( tag == NULL ) break ;           /* none ==> done */

     /** TimeSeries **/

     if( strcmp(tag,"TimeSeries") == 0 ){
       MCW_idcode *idc ; char *stend ;
       idc  = PLUTO_get_idcode(plint) ;
       dset = PLUTO_find_dset(idc) ;
       if( dset == NULL ) ERROR_message("Can't find TimeSeries dataset") ;
       stend = PLUTO_get_string(plint) ;

       start = end = 0 ;
       if( stend != NULL && *stend != '\0' ){
         start = (int)strtod(stend,&cpt) ;
         if( start < 0 ) start = 0 ;
         while( isspace(*cpt) ) cpt++ ;
         if( *cpt == ',' || *cpt == '+' ){
           char qc = *cpt ;
           if( !isdigit(*cpt) ) cpt++ ;
           end = (int)strtod(cpt,NULL) ;
           if( qc == '+' && end > 4 ) end = start + end-1 ;
         } else if( *cpt != '\0' ){
           WARNING_message("Don't understand 'Start,End' string '%s'",stend) ;
         }
       }
       continue ;
     }

     if( strcmp(tag,"Operation") == 0 ){
       opname = PLUTO_get_string(plint) ;
       meth_indx = PLUTO_string_index( opname , num_meths , meth_used ) ;
       if( meth_indx < 0 )
         ERROR_message("Can't find method index") ;
       meth_code = meth_nums[meth_indx] ;
       continue ;
     }

     /** should never transpire **/

     return "** TSTAT_main: table corruption! **" ;
   }

   /*** check inputs for stoopiditeeze ***/

   if( dset == NULL )
     return "** No TimeSeries dataset? **" ;

   if( ! THD_is_file(DSET_HEADNAME(dset)) ){
     ERROR_message("Dataset not on disk: %s",DSET_HEADNAME(dset)) ;
     return "** Dataset not on disk? **" ;
   }

   if( start >= DSET_NVALS(dset)-2 )
     return "** 'Start' value is too large :( **" ;

   if( meth_code < 0 || opname == NULL ) /* should not be possible */
     return "** No Statistic chosen? **" ;

   if( end <= 0 || end <= start || end >= DSET_NVALS(dset) ) end = DSET_NVALS(dset)-1 ;

   if( end-start+1 < 9 ){
     WARNING_message("**************************\n"
                     "   Too few samples in time series!\n"
                     "   I hope you know what you are doing.\n");
   }

   cpt = AFNI_controller_label(im3d) ;
   sprintf( tname , "%c_TSTAT_JUNK.nii" , cpt[1] ) ;
   sprintf( cmd , "3dTstat -overwrite -prefix %s -methnum %d %s &> /dev/null" ,
                  tname , meth_code , DSET_HEADNAME(dset) ) ;
   system(cmd) ;

   tset = THD_open_dataset(tname) ;
   if( tset == NULL ){
     ERROR_message("Can't open dataset %s",tname) ;
     return "** 3dTstat failed :( **" ;
   }
   DSET_load(tset) ;
   if( ! DSET_LOADED(tset) ){
     ERROR_message("Can't read dataset %s",tname) ;
     return "** 3dTstat failed :(( **" ;
   }

   /* find the output dataset */

   sprintf( iiprefix , "%c_TSTAT" , cpt[1] ) ;
   slf = THD_dset_in_session( FIND_PREFIX , iiprefix , im3d->ss_now ) ;
   nim = DSET_NVALS(tset) ;

   /* if it doesn't exist, or is not the right grid, create it now */

   if( !ISVALID_DSET (slf.dset)                    ||
       !EQUIV_DATAXES(slf.dset->daxes,tset->daxes) ||
       DSET_NVALS(slf.dset) != nim                    ){

     iiset = EDIT_empty_copy( tset ) ;  /* make new dataset */
     EDIT_dset_items( iiset ,
                        ADN_prefix    , iiprefix ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_none ) ;
     DSET_superlock(iiset) ;
     do_relabel = 0 ;

     if( slf.dset != NULL ){       /* exists, but isn't right for us */

       MCW_idcode old_idc = slf.dset->idcode ;
       THD_delete_3dim_dataset(slf.dset,True) ;  /* destroy the guts */
       *slf.dset = *iiset ;       /* copy the guts, keep the pointer */
       slf.dset->idcode = old_idc ;           /* and keep the idcode */
       nds = slf.dset_index ;
       INFO_message("trashed and re-used old dataset %s",iiprefix) ;

     } else {                              /* add new to the session */

       int vv = iiset->view_type ;
       nds = im3d->ss_now->num_dsset ;
       SET_SESSION_DSET(iiset, im3d->ss_now, nds, vv);
       im3d->ss_now->num_dsset++ ;
       AFNI_force_adoption( im3d->ss_now , False ) ;
       AFNI_make_descendants( GLOBAL_library.sslist ) ;
       INFO_message("created new dataset %s",iiprefix) ;

     }

   /* just need to use existing dataset that matches */

   } else {
     iiset = slf.dset ; nds = slf.dset_index ; DSET_mallocize(iiset) ;
   }
   iiset->dblk->diskptr->allow_directwrite = 1 ;

   /* save the resulting data into the output dataset */

   for( qim=0 ; qim < nim ; qim++ ){
     iim = DSET_BRICK(tset,qim) ;
     if( iim != NULL ){
       iar = MRI_FLOAT_PTR(iim) ;
       EDIT_substitute_brick( iiset , qim , MRI_float , iar ) ;
       mri_clear_data_pointer(iim) ;
     } else {
       EDIT_substitute_brick( iiset , qim , MRI_float , NULL ) ;
     }
     if( do_relabel ){
       cpt = DSET_BRICK_LABEL(tset,qim) ;
       EDIT_BRICK_LABEL( iiset , qim , cpt ) ;
     }
   }
   DSET_delete(tset) ; unlink(tname) ;
   THD_delete_3dim_dataset( tset , False ) ;

   DSET_KILL_STATS(iiset) ; THD_load_statistics(iiset) ;
   THD_set_string_atr( iiset->dblk , "TSTAT_PARENT" , DSET_HEADNAME(dset) ) ;

   /* redisplay overlay */

   if( called_before[ic] ) AFNI_ignore_pbar_top(1) ;
   if( im3d->fim_now != iiset ){  /* switch to this dataset */
     MCW_choose_cbs cbs ; char *cpt=AFNI_controller_label(im3d) ;
     cbs.ival = nds ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                               (XtPointer)im3d ,  &cbs           ) ;
     AFNI_set_fim_index(im3d,0) ;
     AFNI_set_thr_index(im3d,0) ;
   } else {
     AFNI_set_fim_index(im3d,0) ;
     AFNI_set_thr_index(im3d,0) ;
     AFNI_force_bucket_label_resize(1) ;
     refit_MCW_optmenu( im3d->vwid->func->fim_buck_av ,
                        0 ,                            /* new minval */
                        DSET_NVALS(im3d->fim_now)-1 ,  /* new maxval */
                        im3d->vinfo->fim_index ,       /* new inival */
                        0 ,                            /* new decim? */
                        AFNI_bucket_label_CB ,         /* text routine */
                        im3d->fim_now                  /* text data */
                       ) ;
     refit_MCW_optmenu( im3d->vwid->func->thr_buck_av ,
                        0 ,                            /* new minval */
                        DSET_NVALS(im3d->fim_now)-1 ,  /* new maxval */
                        im3d->vinfo->thr_index ,       /* new inival */
                        0 ,                            /* new decim? */
                        AFNI_bucket_label_CB ,         /* text routine */
                        im3d->fim_now                  /* text data */
                       ) ;
     AFNI_force_bucket_label_resize(0) ;
     AFNI_reset_func_range(im3d) ;
   }
#if 0
   if( MCW_val_bbox(im3d->vwid->func->range_bbox) ){
     char *cpt=AFNI_controller_label(im3d) ;
     AFNI_ignore_pbar_top(0) ;
     sprintf(cmd,"SET_FUNC_RANGE %c.%f",cpt[1],im3d->vinfo->fim_autorange) ;
     AFNI_driver(cmd) ;
     AFNI_ignore_pbar_top(1) ;
   }
#endif
   called_before[ic]++ ; AFNI_ignore_pbar_top(0) ;

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ;
   if( VEDIT_good(im3d->vedset) ) im3d->vedset.flags = 1 ;
   if( MCW_val_bbox(im3d->vwid->view->see_func_bbox) == 0 ){ /* overlay is off */
     char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     sprintf(cmd,"SEE_OVERLAY %c.+",cpt[1]) ;
     AFNI_driver(cmd) ;
   } else {                                                  /* overlay is on */
     AFNI_redisplay_func(im3d) ;
   }
   AFNI_process_drawnotice(im3d) ;

   return NULL ;
}
#endif  /* ALLOW_PLUGINS */
