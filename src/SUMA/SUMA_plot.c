#include "SUMA_suma.h"
#include "coxplot.h"
#include "SUMA_plot.h"

extern SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
extern int SUMAg_N_SVv; /*!< Number of SVs realized by X  */
/* extern SUMA_SurfaceViewer *SUMAg_cSV; */ /* This variable is no longer used in this file Tue Aug 13 15:27:53 EDT 2002*/ 
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 

static char *redcolor = NULL ;
static char print_command[256] = "\0" ;

SUMA_Boolean SUMA_write_plotmem_ts(MEM_topshell_data * mpcb)
{
   static char FuncName[]={"SUMA_write_plotmem_ts"};
   char stmp[100];
   SUMA_MEMPLOT_USERDATA *mpud=NULL;
   
   SUMA_ENTRY;
   
   if (!mpcb || !mpcb->userdata) SUMA_RETURN(NOPE);
   mpud = (SUMA_MEMPLOT_USERDATA *)mpcb->userdata;
   if (mpud->tsa) {
      if (mpud->tsa_dims[0] != 1) {
         SUMA_S_Err("Not ready for dealing with multiple rows.\n"
                    "Need to modify writing macro for that.\n" );
         SUMA_RETURN(NOPE);
      }
      SUMA_WRITE_ARRAY_1D( mpud->tsa[0], 
                           mpud->tsa_dims[1], 
                           1, mpud->write_name);
      SUMA_S_Notev("Wrote timeseries %s\n",mpud->write_name);
   }
   
   SUMA_RETURN(YUP);
}

/* plotting functions based on those in plot_motif.c */
void SUMA_pm_input_CB( Widget w , XtPointer cd , XtPointer cb )
{
   static char FuncName[]={"SUMA_pm_input_CB"};
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) cb ;
   KeySym keysym;
   XKeyEvent Kev;
   XButtonEvent Bev;
   XMotionEvent Mev;
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   static Time B1time = 0;
   static int pButton, mButton, rButton;
   static SUMA_Boolean DoubleClick = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if( mpcb == NULL || ! MTD_VALID(mpcb)         ) return ;  /* bad */
   if( cbs  == NULL || cbs->reason != XmCR_INPUT ) return ;  /* real bad */
   
   Kev = *(XKeyEvent *) &cbs->event->xkey;
   Bev = *(XButtonEvent *) &cbs->event->xbutton;
   Mev = *(XMotionEvent *) &cbs->event->xmotion;
   
   switch( Kev.type ){

      default: break ;

      /*----- take key press -----*/

      case KeyPress:{
         XKeyEvent * event = (XKeyEvent *) cbs->event ;
         char           buf[32] ;
         KeySym         keysym ;

         buf[0] = '\0' ;
         XLookupString( event , buf , 32 , &keysym , NULL ) ;

         switch( keysym ){
            default: break;
            case XK_h:
               if (Kev.state & ControlMask){
                 if (!list) list = SUMA_CreateList();
                 SUMA_REGISTER_HEAD_COMMAND_NO_DATA(  list, SE_Help_Plot, 
                                                      SES_Suma, NULL); 
                 if (!SUMA_Engine (&list)) {
                     fprintf(stderr, 
                              "Error %s: SUMA_Engine call failed.\n", FuncName);
                 }
               }
               break;
            case XK_w:
               SUMA_write_plotmem_ts(mpcb);
               break;
            case XK_q:
            case XK_Q:
               pm_donebut_CB( NULL , (XtPointer) mpcb , NULL ) ;
               break ;
         }
         break ;
      }
      break ;

      case ButtonPress:
         if (LocalHead) fprintf(stdout,"In ButtonPress\n");      
         pButton = Bev.button;
         if (  SUMAg_CF->SwapButtons_1_3 || 
               (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
            if (pButton == Button1) pButton = Button3;
            else if (pButton == Button3) pButton = Button1;
         }

        /* trap for double click */
         if (Bev.time - B1time < SUMA_DOUBLE_CLICK_MAX_DELAY) {
            if (LocalHead) fprintf( SUMA_STDERR, 
                                    "%s: Double click.\n", FuncName);
            DoubleClick = YUP;
         } else {
            DoubleClick = NOPE;
         }
         B1time = Bev.time; 

         switch (pButton) { /* switch type of button Press */
            case Button1:
               break;
            default:
               break;
         } /* switch type of button Press */
         break;

      case ButtonRelease:
         if (LocalHead) fprintf( SUMA_STDERR,
                                 "%s: In ButtonRelease\n", FuncName); 
         rButton = Bev.button;
         if (  SUMAg_CF->SwapButtons_1_3 || 
               (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
            if (rButton == Button1) rButton = Button3;
            else if (rButton == Button3) rButton = Button1;
         }
         switch (rButton) { /* switch type of button Press */
            case Button3:
               break;
            default:
               break;
         } /* switch type of button Press */
         break;

      case MotionNotify:
         if (LocalHead) fprintf(stdout,"In MotionNotify\n"); 
         if (  SUMAg_CF->SwapButtons_1_3 || 
               (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
           if (   (  (Mev.state & Button3MotionMask) && 
                     (Mev.state & Button2MotionMask)   ) 
               || (  (Mev.state & Button2MotionMask) && 
                     (Mev.state & ShiftMask)    )  ) {
               mButton = SUMA_Button_12_Motion;
            } else if(Mev.state & Button3MotionMask) {
               mButton = SUMA_Button_1_Motion;
            }else if(Mev.state & Button2MotionMask) { 
               mButton = SUMA_Button_2_Motion;
            }else if(Mev.state & Button1MotionMask) { 
               mButton = SUMA_Button_3_Motion;
            }else {
               break;
            } 
         } else {
            if (  (  (Mev.state & Button1MotionMask) && 
                     (Mev.state & Button2MotionMask)  ) 
               || (  (Mev.state & Button2MotionMask) && 
                     (Mev.state & ShiftMask)    )  ) {
               mButton = SUMA_Button_12_Motion;
            } else if(Mev.state & Button1MotionMask) {
               mButton = SUMA_Button_1_Motion;
            }else if(Mev.state & Button2MotionMask) { 
               mButton = SUMA_Button_2_Motion;
            } else if(Mev.state & Button3MotionMask) { 
               mButton = SUMA_Button_3_Motion;
            }else {
               break;
            }
         }

         switch (mButton) {
            case SUMA_Button_12_Motion:
            case SUMA_Button_2_Shift_Motion:
               break;
            default:
               break;
         }


         break;
  }/* switch event type */

  SUMA_RETURNe;
}


/* save the plot into a standalone verion */
static void clonebut_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;

   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;
   /* release yourself from your creator */
   mpcb->clonebut_user_cb((void *)mpcb);
   
   
   /* preserve mpcb->userdata, it is cleared when close button is used */  
   mpcb->clonebut_user_cb=NULL; /* should also remove that button */
   
   return ;
}

/*------------------------------------------------------------------
   Make a toplevel widget and put an existing plot into it for use
   with SUMA.

   If kfun is not NULL, when the user closes the window, it
   will be called as in
        kfun(mpcb) ;
   where mpcb is the pointer returned by this function.
   After this has been done, the memory used will be destroyed,
   including all the contents of mp and mpcb.

   The user may attach extra data to the void * pointer
   mpcb->userdata after this function returns mpcb.  If this
   data involves the use of malloc, it is the user's responsibility
   to free it in the call to kfun.
--------------------------------------------------------------------*/
extern void pm_decode_geom( char * geom , int *ww, int *hh , int *xx, int *yy );
MEM_topshell_data * SUMA_memplot_to_topshell( Display *dpy,
                                         MEM_plotdata *mp, void_func *kfun )
{
   Widget topshell , drawing , donebut , form , psfilebut , 
         psprintbut ;
   MEM_topshell_data *mpcb ;
   int hmin=400 , wmin , ibut=0 , hh,ww,xx,yy ;
   char *prc , *ept ;

   /* sanity check */

   if( dpy == NULL || mp == NULL ) return NULL ;

   mpcb = (MEM_topshell_data *) malloc( sizeof(MEM_topshell_data) ) ;
   memset((void*)mpcb, 0, sizeof(MEM_topshell_data));
   mpcb->valid = 0 ;

#ifdef HAVE_XDBE
   init_XDBE(dpy) ; mpcb->have_xdbe = 0 ;
#endif

   wmin = MEMPLOT_ASPECT(mp) * hmin ;

   /* 12 Oct 2000: a crude way to set the geometry of the popup */

   pm_decode_geom( getenv("AFNI_tsplotgeom") , &ww,&hh,&xx,&yy ) ;
   if( ww < wmin ) ww = wmin ;
   if( hh < hmin ) hh = hmin ;

   /* shell to hold it all */

   topshell = XtVaAppCreateShell(
                 "AFNI" , "AFNI" , topLevelShellWidgetClass , dpy ,
                   XmNborderWidth ,   0  ,
                   XmNminHeight   , hmin , XmNheight , hh ,
                   XmNminWidth    , wmin , XmNwidth  , ww ,
                   XmNallowShellResize , True ,
                   XmNinitialResourcesPersistent , False ,
                   XmNdeleteResponse   , 
                     XmDO_NOTHING ,   /* deletion handled below */
                 NULL ) ;

   XmAddWMProtocolCallback(
        topshell , XmInternAtom(dpy,"WM_DELETE_WINDOW",False) ,
        pm_donebut_CB , (XtPointer) mpcb ) ;

   mpcb->top = topshell ;
   mpcb->mp  = mp ;
   mpcb->dial= NULL ;
   mpcb->wtf = NULL ;

   mpcb->killfunc = kfun ;

   /* form to manage it all */

#undef TIG
#undef NBUT
#define TIG  20
#define NBUT 3

   mpcb->form = form =
        XtVaCreateWidget( "dialog" , xmFormWidgetClass , topshell ,
                             XmNborderWidth , 0 ,
                             XmNfractionBase , TIG*NBUT - 1 ,
                             XmNinitialResourcesPersistent , False ,
                          NULL ) ;

   /* buttons across the top */

   if( redcolor == NULL ){ HOTCOLOR(form,redcolor) ; }

   ibut = 0 ;
   psfilebut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("save image to file") ,
                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( psfilebut , XmNactivateCallback , 
                  pm_psfile_CB , (XtPointer) mpcb ) ;

   ibut++ ;
   psprintbut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("to printer") ,
                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   prc = getenv( "AFNI_PSPRINT" ) ;
   if( prc != NULL ){
      sprintf( print_command , "|%.250s" , prc ) ;
      XtAddCallback( psprintbut , 
                     XmNactivateCallback , pm_psprint_CB , 
                     (XtPointer) mpcb ) ;
   } else {
#if 0
      XtAddCallback( psprintbut , XmNactivateCallback , beep_CB ,
         (XtPointer)"*** AFNI_PSPRINT not defined - see README.environment" );
#elif 0
      XtSetSensitive( psprintbut , False ) ;  /* 05 Nov 2001 */
#else
      XtUnmanageChild( psprintbut ) ;
#endif
   }

#if 1 /* Fur SUMA */
   ibut--; /* UGLY! cancel index of unused button above !*/
   ibut++ ;
   mpcb->clonebut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("Freeze") ,
#if 1
                    BGCOLOR_ARG(redcolor) ,
#endif

                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , True ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( mpcb->clonebut , XmNactivateCallback , 
                  clonebut_CB , (XtPointer) mpcb ) ;

#endif

   ibut++ ;
   donebut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("Done") ,
#if 1
                    BGCOLOR_ARG(redcolor) ,
#endif
 
                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( donebut , XmNactivateCallback , 
                  pm_donebut_CB , (XtPointer) mpcb ) ;

   /* drawing area to receive the picture */

   drawing = XtVaCreateManagedWidget( "dialog" , 
                                       xmDrawingAreaWidgetClass , form ,
                                       XmNtopAttachment    , XmATTACH_WIDGET ,
                                       XmNtopWidget        , donebut ,
                                       XmNleftAttachment   , XmATTACH_FORM ,
                                       XmNrightAttachment  , XmATTACH_FORM ,
                                       XmNbottomAttachment , XmATTACH_FORM ,
                                       XmNinitialResourcesPersistent , False ,
                                       NULL ) ;

   XtAddCallback( drawing , XmNexposeCallback , 
                  pm_expose_CB , (XtPointer) mpcb ) ;
   XtAddCallback( drawing , XmNresizeCallback , 
                  pm_resize_CB , (XtPointer) mpcb ) ;
   XtAddCallback( drawing , XmNinputCallback  , 
                  SUMA_pm_input_CB  , (XtPointer) mpcb ) ;

   /* finish the job */

   XtVaSetValues( form , BGCOLOR_ARG("white") , NULL ) ;

   if( xx >= 0 && yy >= 0 )
      XtVaSetValues( topshell , XmNx,xx , XmNy,yy , NULL ) ;

   XtManageChild(form) ;
   XtRealizeWidget(topshell);

   mpcb->valid = 1 ; mpcb->userdata = NULL ; mpcb->drawing = drawing ;
   return mpcb ;
}

/* Detaches a plot window from its bond to SUMA 
   That's a way to preserve a plot.
*/
void SUMA_memplot_clone(void *mpv)
{
   static char FuncName[]={"SUMA_memplot_clone"};
   MEM_topshell_data *mp=(MEM_topshell_data *)mpv;
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_MEMPLOT_USERDATA *mpud=NULL;

   int iso=0;
   
   SUMA_ENTRY;
   
   if (mp && mp->userdata) {
      mpud = (SUMA_MEMPLOT_USERDATA *)mp->userdata;
      Sover = mpud->Sover;
      /* desentize le bouton */
      XtUnmanageChild(Sover->rowgraph_mtd->clonebut); 
      /* clear content of graph structure in Sover */
      Sover->rowgraph_mtd = NULL;
      
      /* an attempt to recreate a replacement plot.
         No sure fire way to know which SO to use,
         but it seems like a safe bet to just go
         for any deserving surface. */
      for (iso=0; iso<SUMAg_N_DOv; ++iso) {
         if (SUMA_isSO(SUMAg_DOv[iso])) {
            SO = (SUMA_SurfaceObject *)SUMAg_DOv[iso].OP;
            if (SUMA_isOverlayOfSO(SO, Sover)) {
               SUMA_OverlayGraphAtNode(Sover, SO, SO->SelectedNode);  
            } 
         }
      }
      /* Now if there was a new window, position it below the old one */
      SUMA_PositionWindowRelative(  Sover->rowgraph_mtd->top,
                                    mp->top,
                                    SWP_STEP_DOWN_RIGHT);
      if (!mp->userdata) {
         SUMA_S_Err("What the hell");
      }
   } else {
      SUMA_S_Err("NULL input at clone!");
   }
   
   SUMA_RETURNe;
}

void SUMA_Show_Rowgraph_MTD(MEM_topshell_data *rowgraph_mtd)
{
   static char FuncName[]={"SUMA_Show_Rowgraph_MTD"};
   int i=0;
   float **yar=NULL;
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_MEMPLOT_USERDATA *MPUD=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!rowgraph_mtd) 
      SS = SUMA_StringAppend(SS,"NULL rowgraph_mtd");
   else {
      SS = SUMA_StringAppend_va(SS,
            "rowgraph_mtd->userdata: %p\n", rowgraph_mtd->userdata);
      MPUD = (SUMA_MEMPLOT_USERDATA *)rowgraph_mtd->userdata;
      if (MPUD) {
         Sover = (SUMA_OVERLAYS *)MPUD->Sover;
         SS = SUMA_StringAppend_va(SS,
            "   tsa (%dx%d) at %p\n",
            MPUD->tsa_dims[0], MPUD->tsa_dims[1], MPUD->tsa);
         SS = SUMA_StringAppend_va(SS,
            "      row %d: [%f .. %f]\n",
               MPUD->tsa ? 0:-1,
               MPUD->tsa ? MPUD->tsa[0][0]:0.0, 
               MPUD->tsa ? MPUD->tsa[0][MPUD->tsa_dims[1]-1]:0.0);
         if (MPUD->tsa_dims[0] > 1) {
         SS = SUMA_StringAppend_va(SS,
            "      row %d: [%f .. %f]\n",
               MPUD->tsa ? MPUD->tsa_dims[0]-1:-1,
               MPUD->tsa ? MPUD->tsa[MPUD->tsa_dims[0]-1][0]:0.0, 
               MPUD->tsa ? 
                  MPUD->tsa[MPUD->tsa_dims[0]-1][MPUD->tsa_dims[1]-1]:0.0);
         
         }
         SS = SUMA_StringAppend_va(SS,
            "   tsnode %d\n"
            "   Sover (%s) %p\n",
                              MPUD->tsnode,
               Sover ? Sover->Label:"NULL", Sover);
      }
   }
   
   
   
   SUMA_SS2S(SS, s);
   fprintf(stdout, "%s", s); SUMA_free(s); s = NULL;
   SUMA_RETURNe;
}

#define REFILL_MPUD(mpud){\
   int m_i=0;  \
   char *m_pref=NULL;   \
   mpud = SUMA_clear_mpud_contents(mpud); \
   MPUD->tsa = yar; \
   yar = NULL;  /* protect yar from freedom */\
   res = NULL; /* No need to free res. \
               res is in tsa and tsa will be freed \
               when plot is closed */  \
   MPUD->tsa_dims[0] = nrow;  \
   MPUD->tsa_dims[1] = N_res; \
   MPUD->Sover = Sover; \
   MPUD->tsnode = inode; \
   m_pref = SUMA_RemoveDsetExtension_s(Sover->Label, SUMA_NO_DSET_FORMAT); \
   snprintf(MPUD->write_name, \
            90*sizeof(char),  \
            "%s.%05d.1D", \
            m_pref ? m_pref:"NoName", mpud->tsnode); \
   if (m_pref) SUMA_free(m_pref); m_pref = NULL;   \
}

SUMA_Boolean SUMA_OverlayGraphAtNode(SUMA_OVERLAYS *Sover,
               SUMA_SurfaceObject *SO,
               int inode) {
   static char FuncName[]={"SUMA_OverlayGraphAtNode"};
   MEM_plotdata *mp =NULL;
   float *res = NULL;
   int jj, nrow = 1, ymask=TSP_SEPARATE_YBOX;
   int N_res = -1;
   float **yar=NULL ;
   char title_str[101]={""}, xlabel_str[101]={""}, *sl1=NULL, *sl2=NULL;
   double TR=0.0;
   SUMA_DSET *Dset = NULL;
   SUMA_MEMPLOT_USERDATA *MPUD=NULL;
   SUMA_SurfaceViewer *sv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (  !Sover || 
         !SO || !Sover || 
         !Sover->dset_link) {
      SUMA_SL_Err("Nothing to graph");
      SUMA_RETURN(0);
   }
   Dset = Sover->dset_link;
   /* Excerpts right out of ISQ_rowgraph_draw*/

   if (!(res = (float*)SUMA_GetDsetAllNodeValsInCols2(Dset, 
                              NULL, 0, 
                              inode, SO->N_Node, /* test this
                                                    SO->N_Node when
                                                    using patches. 
                                                    Else -1 */
                              &N_res,
                              SUMA_float))) { 
      /* instead of returning with
         SUMA_RETURN(0); Prioir to March 12 08 
         , produce a no data graph */
       
      N_res = SDSET_VECNUM(Dset);
      res = (float *) SUMA_calloc(N_res , sizeof(float));
      snprintf(title_str, 100*sizeof(char), 
               "No Data: %s, node %d on %s", 
               SUMA_CHECK_NULL_STR(Sover->Label),
               inode,
               SUMA_CHECK_NULL_STR(SO->Label));
      sl1 = SUMA_EscapeChars(title_str, "_","\\");
   } else {
      snprintf(title_str, 100*sizeof(char), 
               "%s, node %d on %s", 
               SUMA_CHECK_NULL_STR(Sover->Label),
               inode,
               SUMA_CHECK_NULL_STR(SO->Label));
      sl1 = SUMA_EscapeChars(title_str, "_","\\");
   }
   
   if (!SUMA_is_TimeSeries_dset(Dset, &TR)) { 
      snprintf(xlabel_str, 100*sizeof(char), 
               "column index");
   } else {
      snprintf(xlabel_str, 100*sizeof(char), 
               "TR (%.2fs) step", TR);
   }
   
   if (!res) SUMA_RETURN(0);
   
   yar = (float **)SUMA_calloc(nrow, sizeof(float*));        
   for (jj=0; jj<nrow; jj++) yar[jj] = res;

   ymask = TSP_SEPARATE_YBOX ;
   plot_ts_xypush(0,0);
   mp = plot_ts_mem( N_res , NULL , 
                     nrow, ymask, yar , 
                     xlabel_str,
                     NULL,sl1,NULL ) ;
   if (sl1) SUMA_free(sl1); sl1=NULL;
   if( mp == NULL ){
      SUMA_S_Err("can't make plot_ts_mem") ;
      SUMA_RETURN(0);
   }
   
   /* if there is a plot window open, 
      plot into it, otherwise open a new window */

   if( Sover->rowgraph_mtd != NULL ){
      /* cleanup old user data and replace with new */
      MPUD = NULL;
      REFILL_MPUD(MPUD);
      if (!MPUD) {
         SUMA_S_Err("MPUD NULL!");
         SUMA_RETURN(0);
      }
      MTD_replace_plotdata( Sover->rowgraph_mtd , mp ) ;
      redraw_topshell( Sover->rowgraph_mtd ) ;
      /* and replace userdata */
      Sover->rowgraph_mtd->userdata = (void *)MPUD;
      
   } else {  /* make a new plot window */

      Sover->rowgraph_mtd = SUMA_memplot_to_topshell( 
                                       SUMAg_CF->X->DPY_controller1, 
                                       mp, 
                                       SUMA_rowgraph_mtdkill ) ;

      if( Sover->rowgraph_mtd == NULL ){ 
         delete_memplot( mp );  
         SUMA_RETURN(1);
      }
      /* position plot */
      sv = SUMA_BestViewerForSO(SO);
      if (sv) {
         SUMA_PositionWindowRelative(  Sover->rowgraph_mtd->top , 
                                       sv->X->TOPLEVEL, 
                                       SWP_TOP_RIGHT);
      }                              
      Sover->rowgraph_mtd->clonebut_user_cb = SUMA_memplot_clone;
      MPUD = (SUMA_MEMPLOT_USERDATA*)SUMA_calloc(1,
                                                 sizeof(SUMA_MEMPLOT_USERDATA));
      REFILL_MPUD(MPUD);
      Sover->rowgraph_mtd->userdata = (void *) MPUD ;
   }
   if (LocalHead) SUMA_Show_Rowgraph_MTD(Sover->rowgraph_mtd);

   /* res should not be freed here anymore... */
   if (res) { 
      SUMA_S_Note("Should not BE!");
      SUMA_free(res); 
      res = NULL;
   }
   SUMA_RETURN(1);
}   

/* if the input structure is NULL is passed, a new structure is created and
returned. Else the contents of mpud are cleared */
SUMA_MEMPLOT_USERDATA * SUMA_clear_mpud_contents(SUMA_MEMPLOT_USERDATA *mpud) 
{ 
   static char FuncName[]={"SUMA_clear_mpud_contents"};
   int m_i=0;  
   
   SUMA_ENTRY;
   
   if (!mpud) {
      mpud = (SUMA_MEMPLOT_USERDATA *)
                                 SUMA_calloc(1, sizeof(SUMA_MEMPLOT_USERDATA));
   } 
   if (mpud->tsa) {  
      for (m_i=0; m_i<mpud->tsa_dims[0]; ++m_i) {  
         if (mpud->tsa[m_i]) SUMA_free(mpud->tsa[m_i]);  
      }  
      SUMA_free(mpud->tsa); mpud->tsa=NULL;  
   }  
   mpud->tsa_dims[0]=0; mpud->tsa_dims[1]=0; 
   mpud->tsnode = -1;   
   
   SUMA_RETURN(mpud);
}

      
void SUMA_rowgraph_mtdkill( MEM_topshell_data * mp )
{
   static char FuncName[]={"SUMA_rowgraph_mtdkill"};
   SUMA_OVERLAYS * Sover=NULL ;
   int i=0;
   SUMA_MEMPLOT_USERDATA *mpud=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY ;

   if( mp == NULL ) SUMA_RETURNe; 
   mpud = (SUMA_MEMPLOT_USERDATA *)mp->userdata ; 
   if (!mpud) {
      SUMA_S_Err("I don't like it!");
      SUMA_RETURNe ;
   }
   SUMA_LHv("Freeing tsa's %d arrays and other contents\n", mpud->tsa_dims[0]);
   mpud = SUMA_clear_mpud_contents(mpud);
   
   Sover = mpud->Sover ; 
   if( ! Sover ) SUMA_RETURNe ;
   Sover->rowgraph_mtd = NULL ;
   /* here you might do something with some widgets on 
   say surface controller */
      
   Sover->rowgraph_num = 0 ; /* not sure I need that one yet...*/
   
   /* now free userdata structure, this plot is dying*/
   SUMA_free(mpud); 
   mp->userdata = NULL; /* to be sure */
   
   SUMA_RETURNe ;
}
