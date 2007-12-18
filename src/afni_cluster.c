#undef MAIN
#include "afni.h"

/*****************************************************************************/
/*************  Functions for all actions in the cluster group ***************/

/*---------------------------------------------------------------------------*/

static MCW_textwin *clu_twin[MAX_CONTROLLERS] ;

void AFNI_cluster_textkill( Three_D_View *im3d )
{
   int ic ;

ENTRY("AFNI_cluster_textkill") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   ic = AFNI_controller_index(im3d) ;
   if( ic < 0 || ic >= MAX_CONTROLLERS ) EXRETURN ;
   if( clu_twin[ic] != NULL ){
     XtDestroyWidget(clu_twin[ic]->wshell); myXtFree(clu_twin[ic]);
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_cluster_textdeath( XtPointer cb )
{
   int ic = (int)cb ;
   if( ic >= 0 || ic < MAX_CONTROLLERS ) clu_twin[ic] = NULL ;
/** fprintf(stderr,"AFNI_cluster_textdeath(%d)\n",ic) ; **/
   return ;
}

/*---------------------------------------------------------------------------*/

void AFNI_cluster_textize( Three_D_View *im3d , int force )
{
   int nclu , ic , ii ;
   mri_cluster_detail *cld=NULL ;
   char *msg , *rpt , line[80] ;
   float px,py,pz ;

ENTRY("AFNI_cluster_textize") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   ic = AFNI_controller_index(im3d) ;
   if( ic < 0 || ic >= MAX_CONTROLLERS ) EXRETURN ;
   if( !force && clu_twin[ic] == NULL ) EXRETURN ;

   nclu = mri_clusterize_details( &cld ) ;
   if( nclu == 0 || cld == NULL ){
     if( clu_twin[ic] != NULL ){
       XtDestroyWidget(clu_twin[ic]->wshell); myXtFree(clu_twin[ic]);
     }
     EXRETURN ;
   }

   if( nclu > 22 ) nclu = 20 ;
   msg = (char *)malloc(sizeof(char)*(nclu*99+999)) ;
   rpt = mri_clusterize_report() ;
   if( rpt != NULL ) strcpy(msg,rpt) ;
   else              strcpy(msg,"Cluster Report\n") ;
   strcat(msg ,
           "----------------------------------\n"
           "##  --Size-- ---Peak DICOM xyz---\n"
         ) ;

   for( ii=0 ; ii < nclu ; ii++ ){
     MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom ,
                cld[ii].xpk,cld[ii].ypk,cld[ii].zpk , px,py,pz ) ;
     if( cld[ii].nvox <= 99999 )
       sprintf(line,"%2d:%5d vox %+6.1f %+6.1f %+6.1f\n",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else
       sprintf(line,"%2d:%9d %+6.1f %+6.1f %+6.1f\n",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     strcat(msg,line) ;
   }

   if( clu_twin[ic] == NULL ){
     clu_twin[ic] = new_MCW_textwin_2001( im3d->vwid->func->options_label ,
                                          msg , TEXT_READONLY ,
                                          AFNI_cluster_textdeath,(XtPointer)ic ) ;
   } else {
     MCW_textwin_alter( clu_twin[ic] , msg ) ;
   }
   free((void *)msg ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------*/
/* Put a '*' next to the active item in the vedit list on the menu.    */

static char *clubutlab[] = { " Clear Edit" ,
                             " Clusterize"  } ;
#define NTHRBUT (sizeof(clubutlab)/sizeof(char *))

void set_vedit_label( Three_D_View *im3d , int ll )
{
   char lab[64] ;
   if( !IM3D_OPEN(im3d) ) return ;

   strcpy(lab,clubutlab[0]); if( ll==0 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_clear_pb , lab ) ;

   strcpy(lab,clubutlab[1]); if( ll==1 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_cluster_pb , lab ) ;

   return ;
}

/*---------------------------------------------------------------*/
/* Callback from the clusterize parameter chooser.               */

static void AFNI_cluster_choose_CB( Widget wc, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   float *vec = (float *)(cbs->cval) , rmm,vmul ;

ENTRY("AFNI_cluster_choose_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   rmm = vec[0] ; vmul = vec[1] ;
   if( vmul <= 0.0 ){
     im3d->vedset.code = 0 ;
     AFNI_vedit_clear( im3d->fim_now ) ;
     set_vedit_label(im3d,0) ; VEDIT_unhelpize(im3d) ;
   } else {
     im3d->vedset.code     = VEDIT_CLUST ;
     im3d->vedset.param[2] = rmm ;
     im3d->vedset.param[3] = vmul ;
     set_vedit_label(im3d,1) ;
   }
   if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------*/
/* Callback for items on the clu_label menu itself.              */

void AFNI_clu_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_clu_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /*--- Clear editing ---*/

   if( w == im3d->vwid->func->clu_clear_pb ){
     im3d->vedset.code = 0 ;
     AFNI_vedit_clear( im3d->fim_now ) ;
     set_vedit_label(im3d,0) ; VEDIT_unhelpize(im3d) ;
     AFNI_cluster_textkill(im3d) ;
     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
     EXRETURN ;
   }

   /*--- Get clusterizing parameters ---*/

   if( w == im3d->vwid->func->clu_cluster_pb ){
     char *lvec[2] = { "rmm " , "vmul" } ;
     float fvec[2] ;
     if( im3d->vedset.code == VEDIT_CLUST ){
       fvec[0] = im3d->vedset.param[2]; if( fvec[0] <= 0.0f ) fvec[0] =  0.0f;
       fvec[1] = im3d->vedset.param[3]; if( fvec[1] <= 0.0f ) fvec[1] = 20.0f;
     } else {
       fvec[0] = 0.0f ; fvec[1] = 20.0f ;
     }
     MCW_choose_vector( im3d->vwid->func->thr_label ,
                       "------ Set Clusterize Parameters ------\n"
                       "* rmm=0 is nearest neighbor clustering\n"
                       "  with vmul is cluster volume threshold\n"
                       "  measured in voxel count\n"
                       "* rmm>0 is clustering radius in mm, and\n"
                       "  vmul is cluster volume threshold in\n"
                       "  microliters\n"
                       "* Use 'BHelp' on 'Cluster Edit' label\n"
                       "  to get summary of clustering results.\n"
                       "---------------------------------------"
                       , 2 , lvec,fvec ,
                        AFNI_cluster_choose_CB , (XtPointer)im3d ) ;
     EXRETURN ;
   }

   EXRETURN ;  /* should be unreachable */
}

/*---------------------------------------------------------------------------*/

void AFNI_cluster_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
ENTRY("AFNI_cluster_EV") ;
   AFNI_cluster_textize((Three_D_View *)cd , 1 ) ;
   EXRETURN ;
}

/*****************************************************************************/
/************  Functions for all actions in the thr_label popup **************/

/*-----------------------------------------------------------------
  Event handler to find #3 button press for thr_label popup;
  just pops up the menu for the user's gratification.
-------------------------------------------------------------------*/

void AFNI_thr_EV( Widget w , XtPointer cd ,
                  XEvent *ev , Boolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_thr_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   switch( ev->type ){
      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *) ev ;
         im3d->vwid->butx = event->x_root ;  /* 17 May 2005 */
         im3d->vwid->buty = event->y_root ;
         event->button    = Button3 ;                           /* fakeout */
         XmMenuPosition( im3d->vwid->func->thr_menu , event ) ; /* where */
         XtManageChild ( im3d->vwid->func->thr_menu ) ;         /* popup */
      }
      break ;
   }

   EXRETURN ;
}

void AFNI_thronoff_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int qq , pp ;

ENTRY("AFNI_thronoff_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   qq = im3d->vinfo->thr_onoff ;
   pp = MCW_val_bbox( im3d->vwid->func->thr_onoff_bbox ) ;

   if( pp != qq ){
     im3d->vinfo->thr_onoff = pp ;
     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
   }
   EXRETURN ;
}
