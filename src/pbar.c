/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"
#if 0
#include "pbar.h"
#include "xim.h"    /* for display of the colorscale in "big" mode */
#include <ctype.h>
#include "cs.h"
#endif

static void PBAR_button_EV( Widget w, XtPointer cd, XEvent *ev, Boolean *ctd ) ;
static void PBAR_bigmap_finalize( Widget w, XtPointer cd, MCW_choose_cbs *cbs );
static void PBAR_big_menu_CB( Widget , XtPointer , XtPointer ) ;

static void PBAR_define_bigmap_float( int ncol, float *rgb, char *nam ) ;

static int      bigmap_num=0 ;    /* 31 Jan 2003 */
static char   **bigmap_name ;
static rgbyte **bigmap ;
static int      debugprint=0;     /* debug print */

static float PBAR_get_bigmax( MCW_pbar * ) ;
static void PBAR_show_bigthree_panes( MCW_pbar * ) ;
static void PBAR_show_bigmode( MCW_pbar *pbar ) ;

static MCW_DC *myfirst_dc = NULL ;  /* 04 Feb 2003 */

extern int npane_big ;  /* 06 May 2016 */

#undef  PBAR_callback
#define PBAR_callback(pb,vv)                                            \
 do{ void (*pf)(MCW_pbar *,XtPointer,int) =                             \
      (void (*)(MCW_pbar *,XtPointer,int))(pb->pb_CB) ;                 \
     if( pf != NULL ) pf( pb, (XtPointer)(pb->pb_data), (int)(vv) );    \
 } while(0)

#include "pbardefs.h"

/*---------------------------------------------------------------------------*/
/* Make a new paned-window color+threshold selection bar:

     parent  = parent Widget
     dc      = pointer to MCW_DC for display info
     npane   = initial number of panes
     pheight = initial height (in pixels) of each pane
     pmin    = min value (bottom of lowest pane)
     pmax    = max value (top of highest pane)
     cbfunc  = function to call when a change is made

                 void cbfunc( MCW_pbar *pbar, XtPointer cbdata, int reason )

     cbdata  = data for this call
     reason  = pbCR_COLOR --> color changed
               pbCR_VALUE --> value changed

     flags   = stuff

  WARNING: this code is a mess!  Especially the parts dealing
           with resizing, where the geometry management of the
           Motif widgets must be allowed for.
*//*-------------------------------------------------------------------------*/

MCW_pbar * new_MCW_pbar( Widget parent , MCW_DC *dc ,
                         int npane , int pheight , float pmin , float pmax ,
                         gen_func *cbfunc , XtPointer cbdata , int flags )

{
   MCW_pbar *pbar ;
   int i , np , jm , lcol , ic , ph , bigthree ;
   Widget frm ;

ENTRY("new_MCW_pbar") ;

   /* sanity check */

   if( npane   < NPANE_MIN        || npane > NPANE_MAX ||
       pheight < PANE_MIN_HEIGHT  || pmin == pmax         ) RETURN( NULL );

   /* new pbar */

   lcol = dc->ovc->ncol_ov - 1 ;  /* last color available */

   pbar = myXtNew( MCW_pbar ) ;

   pbar->top = XtVaCreateWidget( "pbar" , xmBulletinBoardWidgetClass , parent ,
                                     XmNmarginHeight , 0 ,
                                     XmNmarginWidth , 0 ,
                                     XmNheight , npane*pheight+(npane-1)*PANE_SPACING ,
                                     XmNresizePolicy , XmRESIZE_ANY ,
                                     XmNtraversalOn , True  ,
                                     XmNinitialResourcesPersistent , False ,
                                  NULL ) ;

   frm = XtVaCreateManagedWidget( "pbar" , xmFrameWidgetClass , pbar->top ,
                                     XmNshadowType , XmSHADOW_ETCHED_IN ,
                                  NULL ) ;

   pbar->panew = XtVaCreateWidget( "pbar" , xmPanedWindowWidgetClass , frm ,
                                      XmNsashWidth , PANE_WIDTH-2*PANE_SPACING,
                                      XmNsashIndent , PANE_SPACING ,
                                      XmNsashHeight , (npane<NPANE_NOSASH) ? SASH_HYES
                                                                           : SASH_HNO ,
                                      XmNmarginHeight , 0 ,
                                      XmNmarginWidth , 0 ,
                                      XmNspacing , PANE_SPACING ,
                                      XmNx , 0 , XmNy , 0 ,
                                      XmNtraversalOn, True  ,
                                      XmNinitialResourcesPersistent , False ,
                                   NULL ) ;

   if( check_pixmap == XmUNSPECIFIED_PIXMAP )
      check_pixmap = XCreatePixmapFromBitmapData(
                        XtDisplay(parent) , RootWindowOfScreen(XtScreen(parent)) ,
                        check_bits , check_width , check_height ,
                        dc->ovc->pixov_brightest , dc->ovc->pixov_darkest ,
                        DefaultDepthOfScreen(XtScreen(parent)) ) ;

   /** make the panes **/

   pbar->pane_hsum[0] = 0 ;  /* Dec 1997 */

   for( i=0 ; i < NPANE_MAX ; i++ ){
      ph = (i<npane) ? pheight : PANE_MIN_HEIGHT ;  /* Dec 1997 */
      pbar->pane_hsum[i+1] = pbar->pane_hsum[i] + ph ;

      pbar->panes[i] = XtVaCreateWidget(
                          "pbar" , xmDrawnButtonWidgetClass , pbar->panew ,
                              XmNpaneMinimum , PANE_MIN_HEIGHT ,
                              XmNallowResize , True ,
                              XmNheight , ph ,
                              XmNwidth , PANE_WIDTH,
                              XmNborderWidth , 0 ,
                              XmNmarginWidth , 0 ,
                              XmNmarginHeight , 0 ,
                              XmNhighlightThickness , 0 ,
                              XmNpushButtonEnabled , True ,
                              XmNshadowThickness , 1 ,
                              XmNuserData , (XtPointer)pbar ,
                              XmNtraversalOn , True ,
                              XmNinitialResourcesPersistent , False ,
                            NULL ) ;

      if( i < npane ) XtManageChild( pbar->panes[i] ) ;

      XtAddCallback( pbar->panes[i], XmNactivateCallback, PBAR_click_CB , dc   ) ;
      XtAddCallback( pbar->panes[i], XmNresizeCallback  , PBAR_resize_CB, pbar ) ;

      pbar->ov_index[i] = ic = MIN( lcol , i+1 ) ;
      MCW_set_widget_bg( pbar->panes[i] , NULL , dc->ovc->pix_ov[ic] ) ;
   }
   XtManageChild( pbar->panew ) ;

   pbar->panes_sum    = pheight * npane ;
   pbar->num_panes    = npane ;
   pbar->panew_height = pbar->panes_sum + (npane-1)*PANE_SPACING ;

   pbar->pb_CB     = cbfunc ;
   pbar->pb_data   = cbdata ;
   pbar->dc        = dc ;
   pbar->renew_all = 0 ;

   /** make the labels **/

   for( i=0 ; i <= NPANE_MAX ; i++ ){
      int yy ;
      char buf[16] ;

      pbar->pval[i] = pmax - i * (pmax-pmin)/npane ;
      PBAR_labelize( pbar->pval[i] , buf ) ;

      if( i < npane ){
         yy = i * (pheight+PANE_SPACING) ;
         if( i > 0 ) yy -= PANE_LOFF ;
      } else {
         yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
         yy = pbar->panew_height - 2 * PANE_LOFF + PANE_SPACING ;
      }

      pbar->labels[i] =  XtVaCreateWidget(
                            " XXXXX" , xmLabelWidgetClass , pbar->top ,
                               XmNrecomputeSize , False ,
                               XmNx , PANE_WIDTH+PANE_SPACING+4 ,
                               XmNy , yy ,
                               XmNborderWidth , 0 ,
                               XmNmarginWidth , 0 ,
                               XmNmarginHeight , 0 ,
                               XmNalignment , XmALIGNMENT_BEGINNING ,
                               XmNhighlightThickness , 0 ,
                               XmNshadowThickness , 0 ,
                             NULL ) ;
      LABELIZE(pbar->labels[i]) ;

      if( KEEP_LABEL(i,npane) ){
         XtManageChild( pbar->labels[i] ) ;
         MCW_set_widget_label( pbar->labels[i] , buf ) ;
      }
   }

   /*-- add _save & mode stuff --*/

STATUS("init pval_save") ;
   for( np=NPANE_MIN ; np <= NPANE_MAX ; np++ ){
     for( i=0 ; i <= np ; i++ )
       for( jm=0 ; jm < PANE_MAXMODE ; jm++ )
         pbar->pval_save[np][i][jm] = pmax - i * (pmax-pmin)/np ;

     for( i=0 ; i < np ; i++ )
       for( jm=0 ; jm < PANE_MAXMODE ; jm++ )
         pbar->ovin_save[np][i][jm] = MIN(lcol,i+1) ;
   }
   pbar->update_me    = 0 ;
   pbar->mode         = 0 ;
   pbar->hide_changes = 0 ;
   pbar->keep_pval    = 0 ;  /* Dec 1997 */

   for( jm=0 ; jm < PANE_MAXMODE ; jm++ )
     pbar->npan_save[jm] = pbar->num_panes ;

   /*-- 31 Jan 2003: create palettes to choose between for "big" mode --*/

   if( myfirst_dc == NULL ) myfirst_dc = dc ;  /* 04 Feb 2003 */

   PBAR_add_bigmap(NULL,NULL) ;

   /* bigthree == bigmode shows 1 or 3 panes */

   bigthree = (flags & PBAR_BIGTHREE_MASK) != 0 ;

   /*-- 30 Jan 2003: setup the "big" mode for 256 (NPANE_BIG) colors --*/

   pbar->bigmode      = 0 ;
   pbar->bigflip      = 0 ;
   pbar->bigrota      = 0 ;
   pbar->bigset       = 0 ;
   pbar->bigmap_index = 0 ;
   for( i=0 ; i < NPANE_BIG ; i++ ) pbar->bigcolor[i] = bigmap[0][i] ;
   pbar->bigname = bigmap_name[0] ;
   pbar->bigxim  = NULL ;
   pbar->bigbot  = -1.0f ; pbar->bigtop = 1.0f ; pbar->dont_alter_bigmax = 0 ;

   pbar->bigmax  = PBAR_get_bigmax(pbar) ;                /* Feb 2012 */
   pbar->big30   = pbar->big32 = pbar->big31 = bigthree ;
   pbar->bigh0   = pbar->bigh1 = pbar->bigh2 = -1       ;
   pbar->ignore_resize = 0 ;

   XtAddCallback( pbar->panes[bigthree], XmNexposeCallback, PBAR_bigexpose_CB, pbar ) ;

   XtInsertEventHandler( pbar->panes[bigthree] ,
                         ButtonPressMask ,      /* get button presses */
                         FALSE ,                /* nonmaskable events? */
                         PBAR_button_EV ,       /* event handler */
                         (XtPointer) pbar ,     /* client data */
                         XtListTail ) ;         /* last in queue */

   /* 11 Feb 2003: create a popup menu for doing stuff */

   pbar->bigfac  = 0.0f ;
#ifdef BAD_BUTTON3_POPUPS   /* 21 Jul 2003 */
   pbar->big_menu = XmCreatePopupMenu( pbar->top      , "menu" , NULL , 0 ) ;
#else
   pbar->big_menu = XmCreatePopupMenu( pbar->panes[0] , "menu" , NULL , 0 ) ;
#endif

   SAVEUNDERIZE(XtParent(pbar->big_menu)) ;
   VISIBILIZE_WHEN_MAPPED(pbar->big_menu) ;
#if 0
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(pbar->big_menu) ;
#endif

   pbar->big_label = XtVaCreateManagedWidget(
                     "menu" , xmLabelWidgetClass , pbar->big_menu ,
                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;
   LABELIZE(pbar->big_label) ;

   (void) XtVaCreateManagedWidget( "menu",
                                    xmSeparatorWidgetClass, pbar->big_menu ,
                                       XmNseparatorType , XmSINGLE_LINE ,
                                    NULL ) ;

   pbar->big_choose_pb = XtVaCreateManagedWidget(
                           "menu" , xmPushButtonWidgetClass , pbar->big_menu ,
                             LABEL_ARG("Choose Colorscale") ,
                             XmNtraversalOn , True  ,
                             XmNinitialResourcesPersistent , False ,
                          NULL ) ;
   XtAddCallback( pbar->big_choose_pb, XmNactivateCallback, PBAR_big_menu_CB , pbar ) ;
   MCW_register_hint( pbar->big_choose_pb , "Change the continuous colorscale" ) ;

   if( bigthree ){
      pbar->big_scaleup_pb = XtVaCreateManagedWidget(
                                "menu" , xmPushButtonWidgetClass , pbar->big_menu ,
                                  LABEL_ARG("Scale X 2") ,
                                  XmNtraversalOn , True  ,
                                  XmNinitialResourcesPersistent , False ,
                               NULL ) ;
      XtAddCallback( pbar->big_scaleup_pb, XmNactivateCallback, PBAR_big_menu_CB , pbar ) ;
      MCW_register_hint( pbar->big_scaleup_pb , "Double the maximum possible value" ) ;

      pbar->big_scaledn_pb = XtVaCreateManagedWidget(
                                "menu" , xmPushButtonWidgetClass , pbar->big_menu ,
                                  LABEL_ARG("Scale / 2") ,
                                  XmNtraversalOn , True  ,
                                  XmNinitialResourcesPersistent , False ,
                               NULL ) ;
      XtAddCallback( pbar->big_scaledn_pb, XmNactivateCallback, PBAR_big_menu_CB , pbar ) ;
      MCW_register_hint( pbar->big_scaledn_pb , "Halve the maximum possible value" ) ;

      pbar->big_picktopbot_pb = XtVaCreateManagedWidget(
                                "menu" , xmPushButtonWidgetClass , pbar->big_menu ,
                                  LABEL_ARG("Pick Bot/Top") ,
                                  XmNtraversalOn , True  ,
                                  XmNinitialResourcesPersistent , False ,
                               NULL ) ;
      XtAddCallback( pbar->big_picktopbot_pb, XmNactivateCallback, PBAR_big_menu_CB , pbar ) ;
      MCW_register_hint( pbar->big_picktopbot_pb , "Choose the display range" ) ;
   } else {
     pbar->big_scaleup_pb = pbar->big_scaledn_pb = NULL ;
     pbar->big_picktopbot_pb = NULL ;
   }

   /*-- go home --*/

   XtManageChild( pbar->top ) ;

   /* ZSS: Jan 13 Now add some funky ones */

   PBAR_define_bigmap( R_AND_B_INV_256_CMD ); // [PT: Aug 17, 2017]
   PBAR_define_bigmap( CB_CS_35 );
   PBAR_define_bigmap( CB_CS );
   PBAR_define_bigmap( CYTOARCH_ROI_256_CMD );
   PBAR_define_bigmap( CYTOARCH_ROI_256_GAP_CMD );
   PBAR_define_bigmap( FREESURFER_SEG_255_CMD );
   PBAR_define_bigmap( GRAY_CS );
   PBAR_define_bigmap( GRAY_CIRCLE_CS );
   PBAR_define_bigmap( GRAY_INV_CIRCLE_CS );
   PBAR_define_bigmap( AMBER_CS );
   PBAR_define_bigmap( AMBER_CIRCLE_CS );
   PBAR_define_bigmap( AMBER_INV_CIRCLE_CS );
   PBAR_define_bigmap( GREEN_CS );
   PBAR_define_bigmap( RED_CS );
   PBAR_define_bigmap( BLUE_CS );
   PBAR_define_bigmap( ROI_32_256_CMD );
   PBAR_define_bigmap( ROI_64_256_CMD );
   PBAR_define_bigmap( ROI_128_256_CMD );
   PBAR_define_bigmap( ROI_256_CMD );
   PBAR_define_bigmap( AMBER_REDTOP_BLUEBOT_CS );
   PBAR_define_bigmap( ADD_EDGE );
   PBAR_define_bigmap( RedBlueGreen_CMD);

   /*- colorscales defined by arrays of floats from pbardefs.h [17 Nov 2016] -*/

#ifdef viridis_num
   PBAR_define_bigmap_float( viridis_num, viridis_data, viridis_name ) ;
#endif
#ifdef magma_num
   PBAR_define_bigmap_float( magma_num  , magma_data  , magma_name   ) ;
#endif
#ifdef inferno_num
   PBAR_define_bigmap_float( inferno_num, inferno_data, inferno_name ) ;
#endif
#ifdef plasma_num
   PBAR_define_bigmap_float( plasma_num , plasma_data , plasma_name  ) ;
#endif

   RETURN( pbar );
}

/*-------------------------------------------------------------------*/
/*! Read pbar bigmaps ordered by environment */

static void PBAR_enviro_bigmaps( MCW_DC *dc )
{
   static int first=1 ;
   char nnn[32] , *eh , *en , *fn , bn[2000] ;
   int ii ;

ENTRY("PBAR_enviro_bigmaps") ;

   if( !first || dc == NULL ) EXRETURN ;
   first = 0 ;

   eh = getenv("HOME") ;
   for( ii=1 ; ii <= 99 ; ii++ ){
     sprintf(nnn,"AFNI_COLORSCALE_%02d",ii) ;
     en = getenv(nnn) ;              /** 21 Apr 2005: check alternatives **/
     if( en == NULL            ){sprintf(nnn,"AFNI_COLOR_SCALE_%02d",ii); en=getenv(nnn);}
     if( en == NULL && ii <= 9 ){sprintf(nnn,"AFNI_COLORSCALE_O%1d" ,ii); en=getenv(nnn);}
     if( en == NULL && ii <= 9 ){sprintf(nnn,"AFNI_COLORSCALE_%1d"  ,ii); en=getenv(nnn);}
     if( en != NULL ){
       if( THD_is_file(en) ){
         fn = en ;
       } else if( eh != NULL ){
         sprintf(bn,"%.999s/%.999s",eh,en) ; fn = bn ;
       } else {
         continue ;  /* skip this name */
       }
       PBAR_read_bigmap( fn , dc ) ;
     }
   }
   EXRETURN ;
}


/*-----------------------------------------------------------------------*/
/*! Add a color map for "big" mode.
-------------------------------------------------------------------------*/

void PBAR_add_bigmap( char *name , rgbyte *cmap )
{
   int ii , nn , kk ;

ENTRY("PBAR_add_bigmap") ;

   /* if needed, setup initial colorscale tables */

   if( bigmap_num == 0 ){
     bigmap_num     = NBIGMAP_INIT ;
     if( NJ_bigmaps_init( bigmap_num, &bigmap_name, &bigmap) )
       ERROR_exit("Calamitous");
     PBAR_enviro_bigmaps( myfirst_dc ) ;
   }
   if( name == NULL || *name == '\0' || cmap == NULL ) EXRETURN ;

   /* 07 Feb 2003: see if name is a duplicate;
                   if so, replace the old colorscale */

   for( nn=0 ; nn < bigmap_num ; nn++ )
     if( strcmp(name,bigmap_name[nn]) == 0 ) break ;

   if( nn == bigmap_num ){   /* is NOT a replacement */
     kk          = nn+1 ;    /* so make room for it */
     bigmap_num  = kk ;
     bigmap_name = (char **) realloc(bigmap_name,sizeof(char *)*kk);
     bigmap      = (rgbyte **) realloc(bigmap,sizeof(rgbyte *)*kk);
     bigmap[nn]  = (rgbyte *) malloc(sizeof(rgbyte)*(NPANE_BIG+1)) ;
   } else {                  /* is a replacement */
     free(bigmap_name[nn]) ; /* so just free old name string */
   }

   bigmap_name[nn] = strdup(name) ;

   for( ii=0 ; ii < NPANE_BIG ; ii++ ) bigmap[nn][ii] = cmap[ii] ;
   if(debugprint)
      fprintf(stderr,"%s: %d\n", name, nn);
   POPDOWN_strlist_chooser ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void PBAR_make_bigmap( char *name,
                       int neq, float *val, rgbyte *col, MCW_DC *dc )
{
   int ii,jj ;
   float fr,fg,top,bot,del,vv ;
   rgbyte map[NPANE_BIGGEST] ;

ENTRY("PBAR_make_bigmap") ;

   if( neq < 2 || val == NULL || col == NULL || dc == NULL ){
     STATUS("bad inputs") ; EXRETURN ;
   }

   /* bubble sort val,col pairs */

   do{
    for( jj=ii=0 ; ii < neq-1 ; ii++ ){
     if( val[ii+1] > val[ii] ){
       fr     = val[ii] ; val[ii] = val[ii+1] ; val[ii+1] = fr     ;
       map[0] = col[ii] ; col[ii] = col[ii+1] ; col[ii+1] = map[0] ;
       jj = 1 ;
     }
    }
   } while(jj) ;

   top = val[0] ; bot = val[neq-1] ; if( bot >= top ) EXRETURN ;
   del = (top-bot)/(NPANE_BIG-1) ;

   for( jj=ii=0 ; ii < NPANE_BIG ; ii++ ){
     vv = top - ii*del ;
     for( ; jj < neq-1 ; jj++ )
       if( vv <= val[jj] && vv >= val[jj+1] ) break ; /* no break at end!*/
     if(jj>=(neq-1)) jj = neq - 2;  /* check for end index - 24 Sep 2007 */
     if( vv >= val[jj] ){
       map[ii] = col[jj] ;
     } else if( vv <= val[jj+1] ){
       map[ii] = col[jj+1] ;
    } else {
       fr = (vv-val[jj+1])/(val[jj]-val[jj+1]) ;
       fg = 1.0f-fr ;
       map[ii].r = (byte)(fr*col[jj].r + fg*col[jj+1].r + 0.5f) ;
       map[ii].g = (byte)(fr*col[jj].g + fg*col[jj+1].g + 0.5f) ;
       map[ii].b = (byte)(fr*col[jj].b + fg*col[jj+1].b + 0.5f) ;
     }
   }

   PBAR_add_bigmap( name, map ) ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/

int PBAR_define_bigmap( char *cmd )
{
  int ii , neq=0 , nonum=0 ;
  char name[NSBUF], eqn[NSBUF] , rhs[NSBUF] ;
  float  val[NPANE_BIGGEST+2] , fr,fg,fb ;
  rgbyte col[NPANE_BIGGEST+2] ;

ENTRY("PBAR_define_bigmap") ;

  if( myfirst_dc == NULL ) RETURN(-1) ;

  name[0] = '\0' ; ii = 0 ;
  sscanf(cmd,"%127s%n",name,&ii) ;
  if(debugprint)
       fprintf(stderr,"%s %d\n",name,ii);
  if( *name == '\0' || ii == 0 ) RETURN(-1);
  cmd += ii ;
  /* get lines of form "value=colordef" */

  while( neq < NPANE_BIG ){
    eqn[0] = '\0' ; ii = 0 ;
    sscanf(cmd,"%127s%n",eqn,&ii) ;
    if(debugprint)
       fprintf(stderr,"%s %d\n",eqn,ii);
    if( *eqn == '\0' || ii == 0 ) break ;   /* exit loop */
    cmd += ii ;
    if( neq == 0 && (isalpha(eqn[0]) || eqn[0]=='#') ) nonum = 1 ;
    rhs[0] = '\0' ; ii = 0 ;
    if( !nonum ) sscanf(eqn,"%f=%s%n",val+neq,rhs,&ii) ;
    else         sscanf(eqn,"%s%n"           ,rhs,&ii) ;
    if( *rhs == '\0' || ii == 0 ) RETURN(-1);
      ii = DC_parse_color( myfirst_dc , rhs, &fr,&fg,&fb ) ;
      if(debugprint)
         fprintf(stderr,"%s %f %f %f\n",rhs,fr,fg,fb);
      if( ii ) RETURN(-1); /* bad */
      col[neq].r = (byte)(255.0f*fr+0.5f) ;
      col[neq].g = (byte)(255.0f*fg+0.5f) ;
      col[neq].b = (byte)(255.0f*fb+0.5f) ; neq++ ;
  }

  if( nonum ) {                   /* supply numbers, if missing */
    if(debugprint) fprintf(stderr,"Supplying indices to colorscale\n");
    for( ii=0 ; ii < neq ; ii++ ) val[ii] = neq-ii ;
  }

  if(debugprint) {
     for(ii=0;ii<neq;ii++)
       fprintf(stderr,"%f %x %x %x\n", val[ii], col[ii].r, col[ii].g, col[ii].b);
  }
  PBAR_make_bigmap( name , neq, val, col, myfirst_dc );
  RETURN(0) ;
}

/*-----------------------------------------------------------------------*/

static void PBAR_define_bigmap_float( int ncol, float *rgb, char *nam )
{
  int ii ;
  char name[NSBUF] ;
  float  val[NPANE_BIGGEST+2] , fr,fg,fb ;
  rgbyte col[NPANE_BIGGEST+2] ;

ENTRY("PBAR_define_bigmap_float") ;

  if( myfirst_dc == NULL ) EXRETURN ;
  if( ncol < 4 || rgb == NULL || nam == NULL ) EXRETURN ;
  if( ncol > NPANE_BIG ) ncol = NPANE_BIG ;

  MCW_strncpy( name, nam, NSBUF ) ;

  for( ii=0 ; ii < ncol ; ii++ ){
    fr = rgb[3*ii+0] ;
    fg = rgb[3*ii+1] ;
    fb = rgb[3*ii+2] ;
    col[ii].r = (byte)(255.0f*fr+0.5f) ;
    col[ii].g = (byte)(255.0f*fg+0.5f) ;
    col[ii].b = (byte)(255.0f*fb+0.5f) ;
    val[ii]   = ii ;
  }

  PBAR_make_bigmap( name , ncol, val, col, myfirst_dc );
  EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void PBAR_read_bigmap( char *fname , MCW_DC *dc )
{
  int ii , neq=0 , nonum=0 , yeseq=0 ;
  char name[NSBUF], lhs[NSBUF],rhs[NSBUF],mid[NSBUF],line[2*NSBUF] , *cpt ;
  float  val[NPANE_BIGGEST+2] , fr,fg,fb , top,bot,del,vv ;
  rgbyte col[NPANE_BIGGEST+2] ;
  FILE *fp ;

ENTRY("PBAR_read_bigmap") ;

  if( fname == NULL || *fname == '\0' || dc == NULL ) EXRETURN ;

  STATUS(fname) ;
  fp = fopen(fname,"r"); if( fp == NULL ){
    STATUS("can't open file") ; EXRETURN;
  }

  /* get name */

  do{
    cpt = afni_fgets( line , 2*NSBUF , fp ) ;
    if( cpt == NULL ){ STATUS("can't read title line"); fclose(fp); EXRETURN; }
    name[0] = '\0' ;
    sscanf(line,"%127s",name) ;
  } while( name[0]=='\0' || name[0]=='!' || (name[0]=='/' && name[1]=='/') ) ;

  /* get lines of form "value = colordef" */

  while( neq < NPANE_BIG ){
    cpt = afni_fgets( line , 2*NSBUF , fp ) ;
    if( cpt == NULL ){ STATUS("!!end of file"); break; } /* exit while loop */
    lhs[0] = mid[0] = rhs[0] = '\0' ;
    sscanf(line,"%127s %127s %127s",lhs,mid,rhs) ;
    if( lhs[0]=='\0' || lhs[0]=='!' || (lhs[0]=='/' && lhs[1]=='/') ) continue;
    STATUS(line) ;

         if( neq == 0 && (isalpha(lhs[0]) || lhs[0]=='#') ) nonum = 1 ;
    else if( neq == 0 && strchr(lhs,'=') != NULL          ) yeseq = 1 ;

    if( yeseq ){
      val[neq] = strtod(lhs,&cpt) ;
      if( *cpt != '\0' ) cpt++ ;     /* skip ending character */
    } else if( !nonum ){
      val[neq] = strtod(lhs,&cpt) ;
      if( val[neq] == 0.0f && *cpt != '\0' ){
        STATUS("!!bad number") ;
        fprintf(stderr,"** %s: %s is a bad number\n",fname,lhs); continue;
      }
      cpt = (mid[0] == '=') ? rhs : mid ;  /* color is string #2 or #3 */
    } else {
      cpt = lhs ;                          /* no number => lhs is the color */
    }
    if( *cpt == '\0' ){ STATUS("no color string?"); continue; } /* not good */

    ii = DC_parse_color( dc , cpt , &fr,&fg,&fb ) ;
    if( ii ){
      STATUS("!!bad color") ;
      fprintf(stderr,"** %s: %s is bad colorname\n",fname,rhs); continue;
    }
    col[neq].r = (byte)(255.0f*fr+0.5f) ;
    col[neq].g = (byte)(255.0f*fg+0.5f) ;
    col[neq].b = (byte)(255.0f*fb+0.5f) ; neq++ ;
  } /* end of loop over color lines */
  fclose(fp) ;

  if( nonum ){                    /* supply numbers, if missing */
    for( ii=0 ; ii < neq ; ii++ )
      val[ii] = neq-ii ;
  }

  PBAR_make_bigmap( name , neq, val, col, dc ) ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Button 3 event handler for pane #bigthree of a pbar, used only when
    in "big" mode, to select a color map.
-------------------------------------------------------------------------*/

static void PBAR_button_EV( Widget w, XtPointer cd, XEvent *ev, Boolean *ctd )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;
   XButtonEvent *bev = (XButtonEvent *) ev ;
   int hh , ii , rr,gg,bb , bigthree ;
   float yy ;

ENTRY("PBAR_button_EV") ;

#if 0
   if( bev->button == Button2 )
     XUngrabPointer( bev->display , CurrentTime ) ;
#endif

   if( pbar == NULL || !pbar->bigmode || bev->button == Button1 ) EXRETURN ;

   bigthree = pbar->big31 ;

   /* get current position, value, and color */

   MCW_widget_geom( pbar->panes[bigthree] , NULL,&hh , NULL,NULL ) ;
   ii = (int)( ((NPANE_BIG-1.0f)*bev->y)/(hh-1) + 0.5f ) ;      /* color index */
   rr = (int)pbar->bigcolor[ii].r ;                             /* color */
   gg = (int)pbar->bigcolor[ii].g ;
   bb = (int)pbar->bigcolor[ii].b ;

   yy = ii/(NPANE_BIG-1.0f) ;
   yy = (yy * pbar->bigbot + (1.0f-yy) * pbar->bigtop) ;
   if( pbar->bigfac != 0.0f ) yy *= pbar->bigfac ;             /* value */

   switch( bev->button ){

     case Button3:{                  /* 11 Feb 2003: popup a menu */
       char str[256] ;               /* but first, put informative label on it */
       sprintf(str,
               "value = %s\nRGB=(%03d,%03d,%03d)" ,
               AV_uformat_fval(yy) , rr,gg,bb      ) ;
       MCW_set_widget_label( pbar->big_label , str ) ;

       XmMenuPosition( pbar->big_menu , bev ) ; /* where */
       XtManageChild ( pbar->big_menu ) ;       /* popup */
     }
     break ;

#if 0
     case Button2:{
       ii = (int)( ((NPANE_BIG-1.0f)*bev->y)/(hh-1) + 0.5f ) ;
       fprintf(stderr,"Color[%03d]: R=%03d G=%03d B=%03d #%02x%02x%02x\n",
               ii , (int)pbar->bigcolor[ii].r          ,
                    (int)pbar->bigcolor[ii].g          ,
                    (int)pbar->bigcolor[ii].b          ,
                    (unsigned int)pbar->bigcolor[ii].r ,
                    (unsigned int)pbar->bigcolor[ii].g ,
                    (unsigned int)pbar->bigcolor[ii].b  ) ;
     }
     break ;
#endif

   }
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void PBAR_topbot_finalize( Widget w , XtPointer cd , int nval , void **val )
{
   MCW_pbar *pbar = (MCW_pbar *)cd ;
   float bot , top ;
   char *spt ;
   Three_D_View *im3d ;

ENTRY("PBAR_topbot_finalize") ;

   if( pbar==NULL ) EXRETURN ;

   im3d = (Three_D_View *)pbar->parent ;
   if( !ISVALID_IM3D(im3d) || !pbar->bigmode || w==NULL || nval!=2 || val==NULL ){
     XBell(pbar->dc->display,100) ; EXRETURN ;
   }

   spt = (char *)val[0] ;
   if( spt != NULL && *spt != '\0' ){
     bot = (float)strtod(spt,NULL) ;
   } else {
     XBell(pbar->dc->display,100) ; EXRETURN ;
   }

   spt = (char *)val[1] ;
   if( spt != NULL && *spt != '\0' ){
     top = (float)strtod(spt,NULL) ;
   } else {
     XBell(pbar->dc->display,100) ; EXRETURN ;
   }
   if( top <= bot ){
     XBell(pbar->dc->display,100) ; EXRETURN ;
   }

   /* INFO_message("bot=%g top=%g",bot,top) ; */
   PBAR_set_bigmode( pbar , 1 , bot,top ) ;
   AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

static void PBAR_big_menu_CB( Widget w , XtPointer cd , XtPointer qd )
{
   MCW_pbar *pbar = (MCW_pbar *)cd ;

ENTRY("PBAR_big_menu_CB") ;

   if( pbar == NULL || !pbar->bigmode || w == NULL ) EXRETURN ;

   if( w == pbar->big_choose_pb ){

     MCW_choose_strlist( w , "Choose Colorscale" ,
                         bigmap_num ,
                         pbar->bigmap_index ,
                         bigmap_name ,
                         PBAR_bigmap_finalize , cd ) ;

   } else if ( w == pbar->big_scaleup_pb ){  /* Feb 2012 */

     pbar->bigmax *= 2.0f ;
     pbar->dont_alter_bigmax = 1 ;
     PBAR_show_bigmode(pbar) ;
     pbar->dont_alter_bigmax = 0 ;
     PBAR_callback(pbar,pbCR_COLOR) ;

   } else if ( w == pbar->big_scaledn_pb ){  /* Feb 2012 */

     float at,ab,am , bm ;
     at = fabsf(pbar->bigtop) ; ab = fabsf(pbar->bigbot) ;
     am = MAX(at,ab)          ; bm = pbar->bigmax * 0.5f ;

     if( 1.05f*am < bm ){
       pbar->bigmax = bm ;
       pbar->dont_alter_bigmax = 1 ;
       PBAR_show_bigmode(pbar) ;
       pbar->dont_alter_bigmax = 0 ;
       PBAR_callback(pbar,pbCR_COLOR) ;
     } else {
       XBell(pbar->dc->display,100) ;
     }

   } else if( w == pbar->big_picktopbot_pb ){
     Three_D_View *im3d=(Three_D_View *)pbar->parent ;
     Widget wtop ;
     if( ISVALID_IM3D(im3d) ){
       wtop = im3d->vwid->func->inten_label ;
     } else {
       XBell(pbar->dc->display,100) ;
     }
     MCW_choose_stuff( wtop , "Color pbar range" ,
                       PBAR_topbot_finalize, (XtPointer)pbar ,
                         MSTUF_STRING , "Bot" ,
                         MSTUF_STRING , "Top" ,
                       MSTUF_END ) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

static void PBAR_bigmap_finalize( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;
   int ii , ind=cbs->ival , bigthree ;

ENTRY("PBAR_bigmap_finalize") ;

   if( ind < 0 || ind >= bigmap_num || !pbar->bigmode ){
     XBell(pbar->dc->display,100); POPDOWN_strlist_chooser; EXRETURN;
   }

   bigthree = pbar->big31 ;

   pbar->bigflip      = 0 ;                 /* 07 Feb 2004 */
   pbar->bigrota      = 0 ;
   pbar->bigname      = bigmap_name[ind] ;  /* 22 Oct 2003 */
   for( ii=0 ; ii < NPANE_BIG ; ii++ )
     pbar->bigcolor[ii] = bigmap[ind][ii] ;

   /* If colormap is meant for ROI data, set range
     parameters automatically          ZSS Feb 15 2010 */

   if( pbar->parent != NULL ){
     if (strstr(pbar->bigname,"i32")) {
        AFNI_set_func_range_nval(pbar->parent, 32.0f);
     } else if (strstr(pbar->bigname,"i64")) {
        AFNI_set_func_range_nval(pbar->parent, 64.0f);
     } else if (strstr(pbar->bigname,"i128")) {
        AFNI_set_func_range_nval(pbar->parent, 128.0f);
     } else if (strstr(pbar->bigname,"i255")) {
        AFNI_set_func_range_nval(pbar->parent, 255.0f);
     } else if (strstr(pbar->bigname,"i256")) {
        AFNI_set_func_range_nval(pbar->parent, 256.0f);
     }
   }
   pbar->bigmap_index = ind ;

   MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
   PBAR_bigexpose_CB(NULL,pbar,NULL) ;
   if( XtIsManaged(pbar->panes[bigthree]) )
     PBAR_callback(pbar,pbCR_COLOR) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

int PBAR_get_bigmap_index( char *bnam ) /* 26 Feb. 2010 ZSS */
{
   int ii;

   if (!bnam) return(-1);

   for( ii=0 ; ii < bigmap_num ; ii++ )
     if( strcmp(bnam,bigmap_name[ii]) == 0 ) return(ii);

   return(-1);
}

/*--------------------------------------------------------------------*/

void PBAR_set_bigmap( MCW_pbar *pbar , char *bnam )  /* 03 Feb 2003 */
{
   int ii ;

ENTRY("PBAR_set_bigmap") ;

   if( pbar == NULL || bnam == NULL || *bnam == '\0' ) EXRETURN ;
   for( ii=0 ; ii < bigmap_num ; ii++ )
     if( strcmp(bnam,bigmap_name[ii]) == 0 ) break ;
   if( ii < bigmap_num ){
     MCW_choose_cbs cbs ;
     cbs.ival = ii ;
     PBAR_bigmap_finalize( NULL , pbar , &cbs ) ;
   }
   EXRETURN ;
}


/*--------------------------------------------------------------------*/
/* set the color bar by index rather than name */

void PBAR_set_bigmap_index( MCW_pbar *pbar , int pbar_index )  /* 08 Mar 2011 */
{
   int ii ;
   MCW_choose_cbs cbs ;

ENTRY("PBAR_set_bigmap_index") ;

   if( pbar == NULL  || pbar_index > bigmap_num ) EXRETURN ;
   cbs.ival = pbar_index ;
   PBAR_bigmap_finalize( NULL , pbar , &cbs ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

char * PBAR_get_bigmap( MCW_pbar *pbar )    /* 03 Feb 2003 */
{
   return bigmap_name[pbar->bigmap_index] ;
}

/*--------------------------------------------------------------------*/
/* if in bigthree mode,
   set the fixed colors for the upper and lower panes [Feb 2012] */

static void PBAR_show_bigthree_panes( MCW_pbar *pbar )
{
   char cname[16] ; int rr,gg,bb ;

   if( pbar == NULL || !pbar->big31 || !pbar->bigmode ) return ;

   if( pbar->big30 ){
     rr = (int)pbar->bigcolor[0].r ;
     gg = (int)pbar->bigcolor[0].g ;
     bb = (int)pbar->bigcolor[0].b ;
     sprintf(cname,"#%02x%02x%02x",rr,gg,bb) ;
     XtVaSetValues( pbar->panes[0] , XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP , NULL ) ;
     MCW_set_widget_bg( pbar->panes[0] , cname , 0 ) ;
   } else {
     XtVaSetValues( pbar->panes[0] , XmNbackgroundPixmap , check_pixmap , NULL ) ;
   }

   if( pbar->big32 ){
     rr = (int)pbar->bigcolor[NPANE_BIG-1].r ;
     gg = (int)pbar->bigcolor[NPANE_BIG-1].g ;
     bb = (int)pbar->bigcolor[NPANE_BIG-1].b ;
     sprintf(cname,"#%02x%02x%02x",rr,gg,bb) ;
     XtVaSetValues( pbar->panes[2] , XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP , NULL ) ;
     MCW_set_widget_bg( pbar->panes[2] , cname , 0 ) ;
   } else {
     XtVaSetValues( pbar->panes[2] , XmNbackgroundPixmap , check_pixmap , NULL ) ;
   }

   return ;
}

/*--------------------------------------------------------------------*/
/*! Actually redisplay pane #bigthree in "big" mode.
----------------------------------------------------------------------*/

static XImage *temp_bigxim=NULL ;       /* 15 Dec 2014 */
static int     temp_nxy=0 ;

void PBAR_bigexpose_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MCW_pbar *pbar = (MCW_pbar *)cd ; int bigthree ;
   XImage **ximout=NULL ; int nx=0,ny=0 ;

ENTRY("PBAR_bigexpose_CB") ;

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   bigthree = pbar->big31 ;

   if( temp_nxy > 0 ){       /* 15 Dec 2014 */
     nx = temp_nxy % 1024 ;
     ny = temp_nxy / 1024 ;
     if( nx >= 4 && ny >= 32 ){
       MCW_kill_XImage(temp_bigxim) ; temp_bigxim = NULL ; ximout = &temp_bigxim ;
     } else {
       nx = ny = 0 ; ximout = &(pbar->bigxim) ;
     }
   } else {
       nx = ny = 0 ; ximout = &(pbar->bigxim) ;
   }

   /* make an image of what we want to see */

   if( *ximout == NULL ){
     int ww,hh , ii , jj , kk ;
     MRI_IMAGE *cim ;
     XImage    *xim ;
     byte      *car , r,g,b ;

     if( nx < 4 || ny < 32 ){
       MCW_widget_geom( pbar->panes[bigthree] , &ww,&hh , NULL,NULL ) ;
     } else {
       ww = nx ; hh = ny ;
     }
     cim = mri_new( ww,NPANE_BIG , MRI_rgb ) ;
     car = MRI_RGB_PTR(cim) ;
     for( kk=ii=0 ; ii < NPANE_BIG ; ii++ ){
       r=pbar->bigcolor[ii].r; g= pbar->bigcolor[ii].g; b=pbar->bigcolor[ii].b;
       if( r > 0 || g > 0 || b > 0 ){
         for( jj=0 ; jj < ww ; jj++ ){
           car[kk++] = r; car[kk++] = g; car[kk++] = b;
         }
       } else {                                            /* 06 Feb 2003 */
         for( jj=0 ; jj < ww ; jj++ ){
           car[kk++]=128; car[kk++]=128; car[kk++]=128;   /* grayish gray */
         }
       }
     }
     { char *eee ; Three_D_View *im3d=(Three_D_View *)pbar->parent ;
       MRI_IMAGE *dim = mri_resize( cim , ww,hh ) ;
       if( IM3D_VALID(im3d) ) AFNI_alpha_fade_mri(im3d,dim) ;
       *ximout = mri_to_XImage( pbar->dc , dim ) ;
       mri_free(dim) ;
       eee = getenv("AFNI_PBAR_TICK") ;          /* tick marks [25 Jun 2013] */
       if( eee == NULL || toupper(*eee) != 'N' ){
         int jj,kk , ntic=9 , wtic=3 ; float ftic ;
         if( eee != NULL && isdigit(*eee) ){
           ntic = (int)strtod(eee,NULL) ; if( ntic > 63 ) ntic = 63 ;
         }
         ftic = 1.0f/(ntic+1) ; if( ww > 42 ) wtic = (int)rintf(0.07f*ww) ;
         for( kk=1 ; kk <= ntic ; kk++ ){
           jj = (int)rintf( ftic*kk*hh ) ;
           rectzero_XImage( pbar->dc , *ximout , 0      ,jj , wtic-1 , jj ) ;
           rectzero_XImage( pbar->dc , *ximout , ww-wtic,jj , ww-1   , jj ) ;
         }
       }
     }
     mri_free(cim) ;
   }

   /* actually show the image to the window pane */

   if( temp_nxy == 0 ){
     if( XtIsManaged(pbar->panes[bigthree]) )
       XPutImage( pbar->dc->display , XtWindow(pbar->panes[bigthree]) ,
                  pbar->dc->origGC , pbar->bigxim , 0,0,0,0 ,
                  pbar->bigxim->width , pbar->bigxim->height ) ;

     PBAR_show_bigthree_panes(pbar) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

static float fround3( float aa )
{
   double am=fabs(aa) , bb ; float rr ;

   if( am == 0.0 ) return 0.0f ;

   bb = pow( 10.0 , (double)( ((int)log10(am))-2 ) ) ;
   am = bb * rint(am/bb) ;

   rr = (float)am ; if( aa < 0.0f ) rr = -rr ;
   return rr ;
}

/*--------------------------------------------------------------------*/

static float PBAR_get_bigmax( MCW_pbar *pbar )
{
   double abot,atop ; int bigthree ;

   if( pbar == NULL ) return 1.0f ;  /* should not happen */

   bigthree = pbar->big31 ;

   abot = fabs(pbar->bigbot) ; atop = fabs(pbar->bigtop) ;
   if( atop <  abot ) atop = abot ;
   if( atop == 0.0  ) atop = 1.0  ;  /* should not transpire */

   if( bigthree ){         /* move it upwards, and round it to 2 figures */
     int dd ; double bb ;
     atop *= 1.2 ;
     dd    = (int)log10((double)atop) ;
     bb    = pow(10.0,(double)(dd-1)) ;
     dd    = 1+(int)(atop/bb-0.0001) ;
     atop  = dd * bb ;
   }

   return (float)atop ;
}

/*--------------------------------------------------------------------*/
/*! Set "big" mode in the pbar -- 30 Jan 2003 - RWCox.
----------------------------------------------------------------------*/

void PBAR_set_bigmode( MCW_pbar *pbar, int bmode, float bot,float top )
{
ENTRY("PBAR_set_bigmode") ;
   if( bmode && bot < top ){
     pbar->bigbot = bot; pbar->bigtop = top;
#if 0
if(pbar->big31) INFO_message("set_bigmode: bot=%g top=%g",bot,top) ;
#endif
   }
   pbar->bigmode   = bmode ;
   pbar->update_me = 1 ;
   update_MCW_pbar( pbar ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

static void PBAR_show_bigmode( MCW_pbar *pbar )  /* 30 Jan 2003 */
{
   int ii , yy , bigthree ;
   char buf[16] ;

ENTRY("PBAR_show_bigmode") ;

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   bigthree = pbar->big31 ;

   if( pbar->hide_changes ) XtUnmapWidget( pbar->top ) ;
   pbar->ignore_resize = 1 ;

   if( !bigthree ){ /* turn off all but 1 pane and all but 2 labels */

     /* manage and unmanage panes and labels */

     XtManageChild( pbar->labels[0] ) ;
     XtManageChild( pbar->labels[1] ) ;
     for( ii=2 ; ii <= NPANE_MAX ; ii++ ) XtUnmanageChild( pbar->labels[ii] ) ;
     XtManageChild( pbar->panes[0] ) ;
     for( ii=1 ; ii <  NPANE_MAX ; ii++ ) XtUnmanageChild( pbar->panes[ii] ) ;

     /* set the only pane left standing to fill the entire window */

     XtVaSetValues( pbar->panes[0] , XmNheight,pbar->panew_height , NULL ) ;
     XtVaSetValues( pbar->panew    , XmNheight,pbar->panew_height , NULL ) ;
     XtVaSetValues( pbar->top      , XmNheight,pbar->panew_height , NULL ) ;

     if( pbar->hide_changes ) XtMapWidget( pbar->top ) ;

     /* position and set top label */

     MCW_widget_geom( pbar->panes[0] , NULL,NULL,NULL , &yy ) ;
     XtVaSetValues( pbar->labels[0] , XmNy , yy , NULL ) ;
     PBAR_labelize( pbar->bigtop , buf ) ;
     MCW_set_widget_label( pbar->labels[0] , buf ) ;

     /* position and set bottom label */

     yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
     XtVaSetValues( pbar->labels[1] , XmNy , yy , NULL ) ;
     PBAR_labelize( pbar->bigbot , buf ) ;
     MCW_set_widget_label( pbar->labels[1] , buf ) ;

   } else {  /* Feb 2012: keep 3 panes and 4 labels */

     float ab,at,am,bm , hfac ; int h0,h1,h2 ;

     /* manage and unmanage panes and labels */

     XtManageChild( pbar->labels[0] ) ;
     XtManageChild( pbar->labels[1] ) ;
     XtManageChild( pbar->labels[2] ) ;
     XtManageChild( pbar->labels[3] ) ;
     for( ii=4 ; ii <= NPANE_MAX ; ii++ ) XtUnmanageChild( pbar->labels[ii] ) ;
     XtManageChild( pbar->panes[0] ) ;
     XtManageChild( pbar->panes[1] ) ;
     XtManageChild( pbar->panes[2] ) ;
     for( ii=3 ; ii <  NPANE_MAX ; ii++ ) XtUnmanageChild( pbar->panes[ii] ) ;

     /* set the height of the 3 panes left upright */
#if 0
INFO_message("show_bigmode: bigbot=%g  bigtop=%g  bigmax=%g",pbar->bigbot,pbar->bigtop,pbar->bigmax) ;
#endif

     ab = fabsf(pbar->bigbot) ; at = fabsf(pbar->bigtop) ; am = MAX(ab,at) ;
     if( !pbar->dont_alter_bigmax && (1.05f*am > pbar->bigmax || 5.01f*am < pbar->bigmax) )
       pbar->bigmax = PBAR_get_bigmax(pbar) ;
     bm = pbar->bigmax ; ab = pbar->bigbot ; at = pbar->bigtop ;

     hfac = (pbar->panew_height - 2*PANE_SPACING) / (2.0f*bm) ;

     h0 = (int)( (bm-at)*hfac + 0.499f ) ; if( h0 < PANE_MIN_HEIGHT ) h0 = PANE_MIN_HEIGHT ;
     h1 = (int)( (at-ab)*hfac + 0.499f ) ; if( h1 < PANE_MIN_HEIGHT ) h1 = PANE_MIN_HEIGHT ;
     h2 = pbar->panew_height - 2*PANE_SPACING - h0 - h1 ;
     if( h2 < PANE_MIN_HEIGHT ){
       int deficit=PANE_MIN_HEIGHT-h2 , dh=deficit/2 ;  /* I cut the deficit in half! */
       h2  = PANE_MIN_HEIGHT ;
       h0 -= (deficit - dh) ;
       h1 -= dh ;
     }
#if 0
ININFO_message("set h0=%d h1=%d h2=%d",h0,h1,h2) ;
ININFO_message("  bm=%g  ab=%g  at=%g",bm,ab,at) ;
#endif

     XtVaSetValues( pbar->panes[0] , XmNheight,h0 , NULL ) ;
     XtVaSetValues( pbar->panes[1] , XmNheight,h1 , NULL ) ;
     XtVaSetValues( pbar->panes[2] , XmNheight,h2 , NULL ) ;
     XtVaSetValues( pbar->panew    , XmNheight,pbar->panew_height , NULL ) ;
     XtVaSetValues( pbar->top      , XmNheight,pbar->panew_height , NULL ) ;

     pbar->bigh0 = h0 ; pbar->bigh1 = h1 ; pbar->bigh2 = h2 ;

     if( pbar->hide_changes ) XtMapWidget( pbar->top ) ;

     /* position and set top label */

     MCW_widget_geom( pbar->panes[0] , NULL,NULL,NULL , &yy ) ;
     XtVaSetValues( pbar->labels[0] , XmNy , yy , NULL ) ;
     PBAR_labelize( bm , buf ) ;
     MCW_set_widget_label( pbar->labels[0] , buf ) ;

     /* second label */

     MCW_widget_geom( pbar->panes[1] , NULL,NULL,NULL , &yy ) ;
     yy -= PANE_LOFF ;
     XtVaSetValues( pbar->labels[1] , XmNy , yy , NULL ) ;
     PBAR_labelize( at , buf ) ;
     MCW_set_widget_label( pbar->labels[1] , buf ) ;

     /* third label */

     MCW_widget_geom( pbar->panes[2] , NULL,NULL,NULL , &yy ) ;
     yy -= PANE_LOFF ;
     XtVaSetValues( pbar->labels[2] , XmNy , yy , NULL ) ;
     PBAR_labelize( ab , buf ) ;
     MCW_set_widget_label( pbar->labels[2] , buf ) ;

     /* fourth label */

     yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
     XtVaSetValues( pbar->labels[3] , XmNy , yy , NULL ) ;
     PBAR_labelize( -bm , buf ) ;
     MCW_set_widget_label( pbar->labels[3] , buf ) ;

   }

   pbar->bigset = 1 ; pbar->ignore_resize = 0 ;

   /* show the thing */

   MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
   PBAR_bigexpose_CB( NULL , pbar , NULL ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
   make a label for the edge out of the floating value
----------------------------------------------------------------------*/

void PBAR_labelize( float val , char *buf )
{
   float aval = fabsf(val) ;
   char prefix[4] ;

   if( aval == 0.0f  ){ strcpy(buf," 0") ; return ; }

   if( val > 0.0f ) strcpy(prefix," ") ;
   else             strcpy(prefix,"-") ;

   if( aval <   0.0010f ){
     int dec = (int)(0.999 - log10(aval)) ;
     float zval = aval * pow( 10.0 , (double)dec ) ;  /* between 1 and 10 */
     if( dec < 10 ) sprintf(buf,"%s%3.1f-%1d",prefix,          zval ,dec) ;
     else           sprintf(buf,"%s%1d.-%2d" ,prefix,(int)rint(zval),dec) ;
   } else if( aval <=  0.9994f ){
     sprintf(buf,"%6.4f",aval) ; buf[0] = prefix[0] ;
   }
   else if( aval <=  9.994f ) sprintf(buf,"%s%5.3f",prefix,aval) ;
   else if( aval <= 99.94f  ) sprintf(buf,"%s%5.2f",prefix,aval) ;
   else if( aval <= 999.4f  ) sprintf(buf,"%s%5.1f",prefix,aval) ;
   else if( aval <= 99999   ) sprintf(buf,"%s%5f"  ,prefix,aval) ;
   else {
     int dec = (int)( log10(aval) + 0.004365 ) ;
     float zval = aval * pow( 10.0 , -(double)dec ) ;
     if( dec < 10 ) sprintf(buf,"%s%3.1f+%1d",prefix,          zval ,dec) ;
     else           sprintf(buf,"%s%1d.+%2d" ,prefix,(int)rint(zval),dec) ;
   }
   return ;
}

/*--------------------------------------------------------------------*/

void PBAR_flip( MCW_pbar *pbar )  /* 07 Feb 2004 */
{
   rgbyte tc ; int ip ;

ENTRY("PBAR_flip") ;

   if( pbar == NULL ) EXRETURN ;

   if( pbar->bigmode ){  /* flip a 'continuous' colorscale */

     for( ip=0 ; ip < NPANE_BIG/2 ; ip++ ){
       tc = pbar->bigcolor[ip] ;
       pbar->bigcolor[ip] = pbar->bigcolor[NPANE_BIG-1-ip] ;
       pbar->bigcolor[NPANE_BIG-1-ip] = tc ;
     }
     MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
     PBAR_bigexpose_CB( NULL , pbar , NULL ) ;
     pbar->bigflip = ! pbar->bigflip ;

   } else {  /* flip a discrete set of panes [08 Feb 2012] */

     int iov[NPANE_MAX], np, kov, jm ; Widget w ; MCW_DC *dc ;

     dc = pbar->dc ;
     np = pbar->num_panes ;
     jm = pbar->mode ;
     for( ip=0 ; ip < np ; ip++ ) iov[ip] = pbar->ov_index[ip] ;

     for( ip=0 ; ip < np ; ip++ ){
       kov = iov[ np-1-ip ] ;  /* new overlay index for ip-th pane */
       w   = pbar->panes[ip] ;
       if( kov > 0 && kov < dc->ovc->ncol_ov ){
         XtVaSetValues( w , XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP , NULL ) ;
         MCW_set_widget_bg( w , NULL , dc->ovc->pix_ov[kov] ) ;
       } else {
         XtVaSetValues( w , XmNbackgroundPixmap , check_pixmap , NULL ) ;
       }
       pbar->ovin_save[pbar->num_panes][ip][jm] =
                             pbar->ov_index[ip] = kov ;
     }

   }

   EXRETURN ;
}

/*--------------------------------------------------------------------
  pbar pane was clicked --> set its color
----------------------------------------------------------------------*/

void PBAR_click_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MCW_DC * dc = (MCW_DC *) cd ;
   MCW_pbar * pbar = NULL ;
   int ip , bigthree ;

ENTRY("PBAR_click_CB") ;

   XtVaGetValues( w , XmNuserData , &pbar , NULL ) ;
   if( pbar == NULL ) EXRETURN ;

   bigthree = pbar->big31 ;

   /*-- continuous colors --*/

   if( pbar->bigmode ){   /* 30 Jan 2003: reverse color spectrum */
     int con=1 ;          /*    Feb 2012: or choose on/off for above/below */
     if( w == pbar->panes[bigthree] ){  /* flipology */
       PBAR_flip( pbar ) ;
       PBAR_callback(pbar,pbCR_COLOR) ;
       EXRETURN ;
     }
          if( w == pbar->panes[0] ){ ip = 0 ; con = pbar->big30 ; }
     else if( w == pbar->panes[2] ){ ip = 2 ; con = pbar->big32 ; }
     else                           EXRETURN ; /* should not happen */
     MCW_choose_binary( w ,
                        ((ip==0) ? "Colorize Above?" : "Colorize Below?") ,
                        con , "Off" , "On" , PBAR_setonoff_CB , pbar ) ;
     EXRETURN ;
   }

   /*-- discrete colors --*/

   for( ip=0 ; ip < pbar->num_panes ; ip++ ) if( pbar->panes[ip] == w ) break ;
   if( ip == pbar->num_panes ) EXRETURN ;

   MCW_choose_ovcolor( w , dc , pbar->ov_index[ip] , PBAR_setcolor_CB , dc ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void PBAR_set_panecolor( MCW_pbar *pbar , int ip , int ovc ) /* 17 Jan 2003 */
{
ENTRY("PBAR_set_panecolor") ;
   if( pbar == NULL || pbar->bigmode ) EXRETURN ;  /* 30 Jan 2003 */
   if( ovc > 0 ){
      XtVaSetValues( pbar->panes[ip] ,
                        XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP ,
                     NULL ) ;
      MCW_set_widget_bg( pbar->panes[ip] , NULL , pbar->dc->ovc->pix_ov[ovc] ) ;
   } else {
      XtVaSetValues( pbar->panes[ip] ,
                        XmNbackgroundPixmap , check_pixmap ,
                     NULL ) ;
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void PBAR_setonoff_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   MCW_pbar *pbar = (MCW_pbar *)cd ;
   int *bp=NULL ;

ENTRY("PBAR_setonoff_CB") ;

   if( pbar == NULL || !pbar->big31 || !pbar->bigmode ) EXRETURN ;  /* error */

        if( w == pbar->panes[0] ) bp = &(pbar->big30) ;
   else if( w == pbar->panes[2] ) bp = &(pbar->big32) ;
   else                          EXRETURN ;      /* error */

   *bp = cbs->ival ;  /* set big30 or big32 */

   PBAR_bigexpose_CB( NULL , pbar , NULL ) ;
   PBAR_callback(pbar,pbCR_COLOR) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
  actual place where color of pane is changed, and user is callbacked
----------------------------------------------------------------------*/

void PBAR_setcolor_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   MCW_DC *dc = (MCW_DC *)cd ;
   MCW_pbar *pbar = NULL ;
   int ip , jm ;

ENTRY("PBAR_setcolor_CB") ;

   if( cbs->ival > 0 && cbs->ival < dc->ovc->ncol_ov ){
      XtVaSetValues( w , XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP , NULL ) ;
      MCW_set_widget_bg( w , NULL , dc->ovc->pix_ov[cbs->ival] ) ;
   } else {
      XtVaSetValues( w , XmNbackgroundPixmap , check_pixmap , NULL ) ;
   }

   XtVaGetValues( w , XmNuserData , &pbar , NULL ) ;
   if( pbar == NULL ) EXRETURN ;
   if( pbar->bigmode ) EXRETURN ;  /* 30 Jan 2003 */

   for( ip=0 ; ip < pbar->num_panes ; ip++ ) if( pbar->panes[ip] == w ) break ;
   if( ip == pbar->num_panes ) EXRETURN ;

   jm = pbar->mode ;
   pbar->ovin_save[pbar->num_panes][ip][jm] =
                         pbar->ov_index[ip] = cbs->ival ;

   PBAR_callback(pbar,pbCR_COLOR) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------
   Rotate the colors in a pbar by n locations (+ or -) -- 30 Mar 2001
----------------------------------------------------------------------------*/

void rotate_MCW_pbar( MCW_pbar *pbar , int n )
{
   int ip , iov[NPANE_MAX] , np , kov , jm ;
   Widget w ;
   MCW_DC *dc ;

ENTRY("rotate_MCW_pbar") ;

   if( pbar == NULL || n == 0 ) EXRETURN ;

   if( pbar->bigmode ){             /* 30 Jan 2003: rotate the spectrum */
     rgbyte *oldcolor ;

     oldcolor = (rgbyte *)malloc(sizeof(rgbyte)*(NPANE_BIG+1)) ;
     memcpy(oldcolor,pbar->bigcolor,sizeof(rgbyte)*NPANE_BIG) ;

     while( n < 0 ) n += NPANE_BIG ;  /* make n positive */
     for( ip=0 ; ip < NPANE_BIG ; ip++ )
       pbar->bigcolor[ip] = oldcolor[(ip+n)%NPANE_BIG] ;

     MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
     PBAR_bigexpose_CB( NULL , pbar , NULL ) ;

     pbar->bigrota += (pbar->bigflip) ? -n : n ;  /* 07 Feb 2004 */
     free(oldcolor) ;

   } else {                         /* the older way */
     dc = pbar->dc ;
     np = pbar->num_panes ;
     jm = pbar->mode ;
     while( n < 0 ) n += np ;  /* make n positive */
     for( ip=0 ; ip < np ; ip++ ) iov[ip] = pbar->ov_index[ip] ;

     for( ip=0 ; ip < np ; ip++ ){
        kov = iov[ (ip+n)%np ] ;  /* new overlay index for ip-th pane */
        w   = pbar->panes[ip] ;
        if( kov > 0 && kov < dc->ovc->ncol_ov ){
           XtVaSetValues( w , XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP , NULL ) ;
           MCW_set_widget_bg( w , NULL , dc->ovc->pix_ov[kov] ) ;
        } else {
           XtVaSetValues( w , XmNbackgroundPixmap , check_pixmap , NULL ) ;
        }
        pbar->ovin_save[pbar->num_panes][ip][jm] =
                              pbar->ov_index[ip] = kov ;
     }
   }

   PBAR_callback(pbar,pbCR_COLOR) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------
  callback when a pane is resized:
    - if the panes don't all add up to the right height, then
      this isn't the last callback in the sequence, and we should
      wait for that one to occur
-----------------------------------------------------------------------*/

void PBAR_resize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MCW_pbar *pbar = (MCW_pbar *)cd ;
   int i , sum , hh[NPANE_MAX] , yy , ip=-1 , jm ;
   char buf[16] ;
   float pmin , pmax , val ;
   int alter_all = pbar->renew_all ;
   static int recur=0 ;  /* Feb 2012 */

ENTRY("PBAR_resize_CB") ;

   if( pbar == NULL || pbar->renew_all < 0 ) EXRETURN ; /* skip it */

   if( recur || pbar->ignore_resize ) EXRETURN ;
   recur++ ;

   /*-- continuous colors --*/

   if( pbar->bigmode ){
     int h0,h1,h2,yy ; float ab,at,bm,hfac ; char buf[16] ;

     if( !pbar->big31 || w != pbar->panes[1] || !MCW_widget_visible(w) ){ recur--; EXRETURN; }

     MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;  /* resize the */
     PBAR_bigexpose_CB( NULL , pbar , NULL ) ;              /* colorscale */

     /* find position and size of the middle pane */

     MCW_widget_geom( pbar->panes[1] , NULL,&h1 , NULL,&yy ) ;
     h0 = yy - PANE_SPACING ;  /* height of pane#0 */
     h2 = pbar->panew_height - 2*PANE_SPACING - h0 - h1 ;

     /* use these to compute the adjust bigtop and bigbot */

     bm   = pbar->bigmax ;
     hfac = (pbar->panew_height - 2*PANE_SPACING) / (2.0f*bm) ;
#if 0
     at   = bm - h0/hfac ; ab = at - h1/hfac ;
     at   = fround3(at)  ; ab = fround3(ab)  ;
#else
     at   = fround3(bm - h0/hfac) ;
     ab   = fround3(at - h1/hfac) ;
#endif
     if( h2 == pbar->bigh2 ) ab = pbar->bigbot ;

#if 0
INFO_message("Resize: h1=%d yy=%d bigbot=%g->%g bigtop=%g->%g",
h1,yy,pbar->bigbot,ab,pbar->bigtop,at) ;
#endif

     pbar->bigtop = at ; pbar->bigbot = ab ;

#if 1
     PBAR_show_bigmode(pbar) ;
#else
     /* change the labels */

     XtVaSetValues( pbar->labels[1] , XmNy , yy-PANE_LOFF , NULL ) ;
     PBAR_labelize( at , buf ) ;
     MCW_set_widget_label( pbar->labels[1] , buf ) ;

     XtVaSetValues( pbar->labels[2] , XmNy , yy+h1 , NULL ) ;
     PBAR_labelize( ab , buf ) ;
     MCW_set_widget_label( pbar->labels[2] , buf ) ;
#endif

     PBAR_callback(pbar,pbCR_VALUE) ;

     recur-- ; EXRETURN ;
   }

   /*-- discrete panes --*/

   for( sum=i=0 ; i < pbar->num_panes ; i++ ){
     MCW_widget_geom( pbar->panes[i] , NULL , &(hh[i]) , NULL,NULL ) ;
     sum += hh[i] ; if( w == pbar->panes[i] ) ip = i ;
   }
   if( ip < 0 ){ recur--; EXRETURN; } /* should not happen */
   jm = pbar->mode ;

   if( sum != pbar->panes_sum ){
      if( ip != pbar->num_panes - 1 ){ recur--; EXRETURN; }
/* INFO_message("reset panes_sum from %d to %d; old panew_height=%d",
                pbar->panes_sum,sum,pbar->panew_height) ; */
      pbar->panes_sum = sum ;
      MCW_widget_geom( pbar->panew , NULL,&(pbar->panew_height),NULL,NULL) ;
/* ININFO_message("new panew_height = %d",pbar->panew_height) ; */
      alter_all = 1 ;
   }

   sum  = 0 ;
   pmax = pbar->pval[0] ;
   pmin = pbar->pval[pbar->num_panes] ;

   for( i=0 ; i <= pbar->num_panes ; i++ ){

      if( alter_all || (i>0 && pbar->pane_hsum[i] != sum) ){

        if( ! pbar->keep_pval ){  /* Dec 1997 */
STATUS("reset pval_save") ;
          val = pmax - sum * (pmax-pmin) / pbar->panes_sum ;
          pbar->pval_save[pbar->num_panes][i][jm] =         /* reset this */
                                    pbar->pval[i] = val ;   /* threshold  */
                                                            /* to match pane size */
         }

        if( KEEP_LABEL(i,pbar->num_panes) ){
          if( i < pbar->num_panes ){
             MCW_widget_geom( pbar->panes[i] , NULL,NULL,NULL , &yy ) ;
             if( i > 0 ) yy -= PANE_LOFF ;
          } else {
             yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
          }

          XtVaSetValues( pbar->labels[i] , XmNy , yy , NULL ) ;
          PBAR_labelize( pbar->pval[i] , buf ) ;
          MCW_set_widget_label( pbar->labels[i] , buf ) ;
        }

      }
      if( i < pbar->num_panes ) sum += hh[i] ;
   }

   pbar->pane_hsum[0] = 0 ;
   for( i=0 ; i < pbar->num_panes ; i++ )
      pbar->pane_hsum[i+1] = pbar->pane_hsum[i] + hh[i] ;

   PBAR_callback(pbar,pbCR_VALUE) ;

   pbar->renew_all = 0 ;
   recur-- ; EXRETURN ;
}

/*-------------------------------------------------------------------------
  user want to programatically alter the pbar:
    number of panes, and/or new array of values
---------------------------------------------------------------------------*/

void update_MCW_pbar( MCW_pbar *pbar )
{
ENTRY("update_MCW_pbar") ;
   if( pbar == NULL || !XtIsManaged(pbar->panew) ) EXRETURN ;
   if( pbar->update_me ){
     if( pbar->bigmode ) PBAR_show_bigmode( pbar ) ;         /* 30 Jan 2003 */
     else                alter_MCW_pbar( pbar , 0 , NULL ) ;
     pbar->update_me = 0 ;
   }
   EXRETURN ;
}

/*-------------------------------------------------------------------------*/

void alter_MCW_pbar( MCW_pbar *pbar , int new_npane , float *new_pval )
{
   int i , npane , npane_old , sum , hh , ovc , jm ;
   float pmin , pmax , pval[NPANE_MAX+1] , fhh , rhh ;
   int was_bigset ;

   /* sanity check */

ENTRY("alter_MCW_pbar") ;

   if( pbar == NULL || new_npane > NPANE_MAX ||
       ( new_npane < NPANE_MIN && new_npane != 0 ) ) EXRETURN ;

   if( pbar->bigmode ) EXRETURN ;   /* 30 Jan 2003 */
   was_bigset   = pbar->bigset ;
   pbar->bigset = 0 ;

   /* count of panes, old and new */

   jm              = pbar->mode ;
   npane           = (new_npane > 0) ? new_npane : pbar->num_panes ;
   npane_old       = pbar->num_panes ;
   pbar->num_panes = pbar->npan_save[jm] = npane ;

STATUS("setup done") ;

   if( was_bigset ) npane_old = 1 ;

   /*-- get new value array --*/

   if( new_pval == NULL ){
STATUS("re-use pval_save") ;
     for( i=0 ; i <= npane ; i++ ) pval[i] = pbar->pval_save[npane][i][jm] ;
   } else {
STATUS("use new_pval") ;
     for( i=0 ; i <= npane ; i++ ) pval[i] = new_pval[i] ;
   }
   pmax = pval[0] ;
   pmin = pval[npane] ;

   /*--- make new panes or destroy old ones ---*/

   if( pbar->hide_changes ) XtUnmapWidget( pbar->top ) ;

   /* set new pane colors */

STATUS("set new colors") ;

   for( i=0 ; i < npane ; i++ ){
      ovc = pbar->ov_index[i] = pbar->ovin_save[npane][i][jm] ;

      if( ovc > 0 ){
         XtVaSetValues( pbar->panes[i] ,
                           XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP ,
                        NULL ) ;
         MCW_set_widget_bg( pbar->panes[i] , NULL , pbar->dc->ovc->pix_ov[ovc] ) ;
      } else {
         XtVaSetValues( pbar->panes[i] ,
                           XmNbackgroundPixmap , check_pixmap ,
                        NULL ) ;
      }
   }

#ifdef PBAR_DEBUG
printf("\n"); fflush(stdout) ;
#endif

   pbar->renew_all = -1 ;  /* skip updates for the moment */
   for( i=0 ; i < NPANE_MAX ; i++ )
     XtVaSetValues( pbar->panes[i] , XmNheight , PANE_MIN_HEIGHT , NULL ) ;

   for( i=0 ; i <= NPANE_MAX ; i++ )
     if( KEEP_LABEL(i,npane) ) XtManageChild  ( pbar->labels[i] ) ;
     else                      XtUnmanageChild( pbar->labels[i] ) ;

   if( npane > npane_old ){
      for( i=npane_old ; i < npane ; i++ ){
#ifdef PBAR_DEBUG
printf("manage pane %d\n",i) ; fflush(stdout) ;
#endif

         XtManageChild( pbar->panes[i] ) ;

      }
   } else if( npane < npane_old ){
      for( i=npane_old-1 ; i >= npane ; i-- ){
#ifdef PBAR_DEBUG
printf("unmanage pane %d\n",i) ; fflush(stdout) ;
#endif
         XtUnmanageChild( pbar->panes[i] ) ;
      }
   }

   /* set new pane heights */

STATUS("set pane heights") ;

   pbar->panes_sum = pbar->panew_height - (npane-1)*PANE_SPACING ;
   for( i=0 ; i <= npane ; i++ ) pbar->pval[i] = pval[i] ;

   sum = pbar->panes_sum ;
   rhh = 0.0f ;
   for( i=0 ; i < npane-1 ; i++ ){
      fhh  = pbar->panes_sum * (pval[i]-pval[i+1]) / (pmax-pmin) ;
      hh   = (int) (rhh+fhh+0.45f) ;
      rhh  = fhh - hh ;
      sum -= hh ;
#ifdef PBAR_DEBUG
printf("set pane %d to height %d (top=%g bot=%g float=%g rem=%g sum=%d)\n",
       i,hh,pval[i],pval[i+1],fhh,rhh,sum) ; fflush(stdout) ;
#endif
      XtVaSetValues( pbar->panes[i] , XmNheight , hh , NULL ) ;
   }
#ifdef PBAR_DEBUG
printf("set pane %d to height %d\n",npane-1,sum) ; fflush(stdout) ;
#endif
   XtVaSetValues( pbar->panes[npane-1] , XmNheight , sum , NULL ) ;

   XtVaSetValues( pbar->panew ,
                     XmNheight , pbar->panew_height ,
                     XmNsashHeight , (npane<NPANE_NOSASH) ? SASH_HYES
                                                          : SASH_HNO ,
                  NULL ) ;

   XtVaSetValues( pbar->top , XmNheight , pbar->panew_height , NULL ) ;

   if( pbar->hide_changes ) XtMapWidget( pbar->top ) ;

   pbar->renew_all = 1 ;
   pbar->keep_pval = 1 ;  /* Dec 1997 */
   PBAR_resize_CB( pbar->panes[pbar->num_panes-1] , (XtPointer) pbar , NULL ) ;

   if( pbar->keep_pval ){                  /* Dec 1997 */
STATUS("save pval_save") ;
     for( i=0 ; i <= npane ; i++ )
       pbar->pval_save[pbar->num_panes][i][jm] =
                                 pbar->pval[i] = pval[i] ;
   }
   pbar->keep_pval = 0 ;

#ifdef PBAR_DEBUG
 { int hh,ww,xx,yy , i ;

   XmUpdateDisplay(pbar->top) ;

   MCW_widget_geom(pbar->top , &ww,&hh,&xx,&yy ) ;
   printf("pbar->top  :  w=%d h=%d x=%d y=%d\n",ww,hh,xx,yy) ; fflush(stdout) ;

   MCW_widget_geom(pbar->panew , &ww,&hh,&xx,&yy ) ;
   printf("pbar->panew: w=%d h=%d x=%d y=%d\n",ww,hh,xx,yy) ; fflush(stdout) ;

   for( i=0 ; i < pbar->num_panes ; i++ ){
      MCW_widget_geom(pbar->panes[i] , &ww,&hh,&xx,&yy ) ;
      printf("pane # %d: w=%d h=%d x=%d y=%d\n",i,ww,hh,xx,yy) ; fflush(stdout) ;
   }
 }
#endif

   EXRETURN ;
}

/*-------------------------------------------------------------------------
   Make an image of the pbar (sans handles)
   -- RWCox - 15 Jun 2000
---------------------------------------------------------------------------*/

MRI_IMAGE * MCW_pbar_to_mri( MCW_pbar *pbar , int nx , int ny )
{
   MRI_IMAGE *im ;
   int   ii,npix,kk,ll,jj , sum,hh=0 ;
   float pmin,pmax , rhh,fhh , hfrac ;
   byte rr,gg,bb , *bar ;

ENTRY("MCW_pbar_to_mri") ;

   /* check for decent inputs */

   if( pbar == NULL ) RETURN(NULL) ;
   if( nx < 1 ) nx = 1 ;

   if( pbar->bigmode ){    /* 30 Jan 2003: save spectrum */
     XImage *xim=NULL ;
     if( ny < NPANE_BIG ) ny = NPANE_BIG ;
     temp_nxy = nx + 1024*ny ;
     PBAR_bigexpose_CB(NULL,pbar,NULL) ;
     temp_nxy = 0 ;
     if( temp_bigxim == NULL ) RETURN(NULL) ;
     if( temp_bigxim->width != nx || temp_bigxim->height != ny ){
       xim = resize_XImage( pbar->dc , temp_bigxim , nx,ny ) ;
     } else {
       xim = temp_bigxim ;
     }
     im  = XImage_to_mri( pbar->dc , xim , X2M_USE_CMAP|X2M_FORCE_RGB ) ;
     if( xim != temp_bigxim ) MCW_kill_XImage( xim ) ;
     MCW_kill_XImage(temp_bigxim) ; temp_bigxim = NULL ;
     RETURN(im) ;
   }

   /** the old way: make the image by brute force **/

   if( ny < 4*pbar->num_panes ) ny = 4*pbar->num_panes ;

   im  = mri_new( nx , ny , MRI_rgb ) ;
   bar = MRI_RGB_PTR(im) ;

   pmax = pbar->pval[0] ;
   pmin = pbar->pval[pbar->num_panes] ;

   hfrac = ny / (pmax-pmin) ;
   rhh  = 0.0f ;
   sum  = ny ;

   /* do each pane */

   for( kk=0 ; kk < pbar->num_panes-1 ; kk++ ){
      fhh  = hfrac * (pbar->pval[kk]-pbar->pval[kk+1]) ; /* wannabe height */
      hh   = (int) (rhh+fhh+0.45) ;                      /* actual height */
      rhh  = fhh - hh ;                                  /* remainder */
      sum -= hh ;                                        /* # pixels left */

      if( pbar->ov_index[kk] > 0 ){                      /* solid color */
         rr = DCOV_REDBYTE  (pbar->dc,pbar->ov_index[kk]) ;
         gg = DCOV_GREENBYTE(pbar->dc,pbar->ov_index[kk]) ;
         bb = DCOV_BLUEBYTE (pbar->dc,pbar->ov_index[kk]) ;

         npix = hh*nx ;
         for( ii=0 ; ii < npix ; ii++ ){
           *bar++ = rr ; *bar++ = gg ; *bar++ = bb ;
         }
      } else {                                           /* check pattern */
         byte bwj , bwi ;
         bwj = 255 ;
         for( jj=0 ; jj < hh ; jj++ ){
            bwi = bwj ;
            for( ii=0 ; ii < nx ; ii++ ){
              *bar++ = bwi ; *bar++ = bwi ; *bar++ = bwi ; bwi = ~bwi ;
            }
            bwj = ~bwj ;
         }
      }
   }

   /* last pane */

   kk = pbar->num_panes-1 ;

   if( pbar->ov_index[kk] > 0 ){                      /* solid color */
      rr = DCOV_REDBYTE  (pbar->dc,pbar->ov_index[kk]) ;
      gg = DCOV_GREENBYTE(pbar->dc,pbar->ov_index[kk]) ;
      bb = DCOV_BLUEBYTE (pbar->dc,pbar->ov_index[kk]) ;

      npix = sum*nx ;
      for( ii=0 ; ii < npix ; ii++ ){
        *bar++ = rr ; *bar++ = gg ; *bar++ = bb ;
      }
   } else {                                           /* check pattern */
      byte bwj , bwi ;
      bwj = 255 ;
      for( jj=0 ; jj < hh ; jj++ ){
         bwi = bwj ;
         for( ii=0 ; ii < nx ; ii++ ){
           *bar++ = bwi ; *bar++ = bwi ; *bar++ = bwi ; bwi = ~bwi ;
         }
         bwj = ~bwj ;
      }
   }

   RETURN(im) ;
}

/*---------------------------------------------------------------*/
/* Return 1 if the 2 pbars are set to be equivalent at this
   moment, and return 0 if anything differs [07 Jul 2014].
*//*-------------------------------------------------------------*/

int MCW_pbars_equivalent( MCW_pbar *lbar , MCW_pbar *rbar )
{
   if( lbar == rbar ) return 1 ;

   if( lbar->bigmode   != rbar->bigmode   ) return 0 ;
   if( lbar->mode      != rbar->mode      ) return 0 ;
   if( ! lbar->bigmode ){
     int ii ;
     if( lbar->num_panes != rbar->num_panes ) return 0 ;
     for( ii=0 ; ii < lbar->num_panes ; ii++ ){
       if( lbar->pval[ii]     != rbar->pval[ii]     ) return 0 ;
       if( lbar->ov_index[ii] != rbar->ov_index[ii] ) return 0 ;
     }
   } else {
     char *lmap = PBAR_get_bigmap(lbar) , *rmap = PBAR_get_bigmap(rbar) ;
     if( strcmp(lmap,rmap) != 0         ) return 0 ;
     if( lbar->big30   != rbar->big30   ) return 0 ;
     if( lbar->big31   != rbar->big31   ) return 0 ;
     if( lbar->big32   != rbar->big32   ) return 0 ;
     if( lbar->bigflip != rbar->bigflip ) return 0 ;
     if( lbar->bigrota != rbar->bigrota ) return 0 ;
     if( lbar->bigtop  != rbar->bigtop  ) return 0 ;
     if( lbar->bigbot  != rbar->bigbot  ) return 0 ;
   }
   return 1;
}
