/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "pbar.h"
#include "xim.h"    /* for display of the colorscale in "big" mode */
#include <ctype.h>

static void PBAR_button_EV( Widget w, XtPointer cd, XEvent *ev, Boolean *ctd ) ;
static void PBAR_bigmap_finalize( Widget w, XtPointer cd, MCW_choose_cbs *cbs );
static void PBAR_big_menu_CB( Widget , XtPointer , XtPointer ) ;

static int      bigmap_num=0 ;    /* 31 Jan 2003 */
static char   **bigmap_name ;
static rgbyte **bigmap ;
static int      debugprint=0;   /* print for debug purposes*/

static MCW_DC *myfirst_dc = NULL ;  /* 04 Feb 2003 */

#undef  PBAR_callback
#define PBAR_callback(pb,vv)                                            \
 do{ void (*pf)(MCW_pbar *,XtPointer,int) =                             \
      (void (*)(MCW_pbar *,XtPointer,int))(pb->pb_CB) ;                 \
     if( pf != NULL ) pf( pb, (XtPointer)(pb->pb_data), (int)(vv) );    \
 } while(0)

#include "pbardefs.h"

/*----------------------------------------------------------------------
   Make a new paned-window color+threshold selection bar:

     parent  = parent Widget
     dc      = pointer to MCW_DC for display info
     npane   = initial number of panes
     pheight = initial height (in pixels) of each pane
     pmin    = min value (bottom of lowest pane)
     pmax    = max value (top of highest pane)
     cbfunc  = function to call when a change is made

                 void cbfunc( MCW_pbar * pbar , XtPointer cbdata , int reason )

     cbdata  = data for this call
     reason  = pbCR_COLOR --> color changed
               pbCR_VALUE --> value changed

  WARNING: this code is a mess!  Especially the parts dealing
           with resizing, where the geometry management of the
           Motif widgets must be allowed for.
------------------------------------------------------------------------*/

MCW_pbar * new_MCW_pbar( Widget parent , MCW_DC * dc ,
                         int npane , int pheight , float pmin , float pmax ,
                         gen_func * cbfunc , XtPointer cbdata )

{
   MCW_pbar * pbar ;
   int i , np , jm , lcol , ic , ph ;
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
#if 0
                        1,0,
#else
                        dc->ovc->pixov_brightest , dc->ovc->pixov_darkest ,
#endif
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
                              XmNuserData , (XtPointer) pbar ,
                              XmNtraversalOn , True ,
                              XmNinitialResourcesPersistent , False ,
                            NULL ) ;

      if( i < npane ) XtManageChild( pbar->panes[i] ) ;

      XtAddCallback( pbar->panes[i] , XmNactivateCallback , PBAR_click_CB , dc ) ;
      XtAddCallback( pbar->panes[i] , XmNresizeCallback , PBAR_resize_CB , pbar ) ;

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
#if 1
         yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
#else
         yy = pbar->panew_height - 2 * PANE_LOFF + PANE_SPACING ;
#endif
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

   /*-- 30 Jan 2003: setup the "big" mode for 128 colors --*/

   pbar->bigmode      = 0 ;
   pbar->bigflip      = 0 ;
   pbar->bigrota      = 0 ;
   pbar->bigset       = 0 ;
   pbar->bigmap_index = 0 ;
   pbar->bigbot  = -1.0 ; pbar->bigtop = 1.0 ;
   pbar->bigxim  = NULL ;
   for( i=0 ; i < NPANE_BIG ; i++ )
     pbar->bigcolor[i] = bigmap[0][i] ;
   pbar->bigname = bigmap_name[0] ;

   XtAddCallback( pbar->panes[0], XmNexposeCallback, PBAR_bigexpose_CB, pbar ) ;

   XtInsertEventHandler( pbar->panes[0] ,
                         ButtonPressMask ,      /* get button presses */
                         FALSE ,                /* nonmaskable events? */
                         PBAR_button_EV ,       /* event handler */
                         (XtPointer) pbar ,     /* client data */
                         XtListTail ) ;         /* last in queue */

   /* 11 Feb 2003: create a popup menu for doing stuff */

   pbar->bigfac  = 0.0 ;
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

   /*-- go home --*/

   XtManageChild( pbar->top ) ;

   /* ZSS: Jan 13 Now add some funky ones */
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
     if (NJ_bigmaps_init( bigmap_num, &bigmap_name, &bigmap)) {
      ERROR_exit("Calamitous");
     }
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
     bigmap[nn]  = (rgbyte *) malloc(sizeof(rgbyte)*NPANE_BIG) ;
   } else {                  /* is a replacement */
     free(bigmap_name[nn]) ; /* so just free old name string */
   }

   bigmap_name[nn] = strdup(name) ;

   for( ii=0 ; ii < NPANE_BIG ; ii++ ) bigmap[nn][ii] = cmap[ii] ;
   if(debugprint)
      printf("%s: %d\n", name, nn);
   POPDOWN_strlist_chooser ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void PBAR_make_bigmap( char *name,
                       int neq, float *val, rgbyte *col, MCW_DC *dc )
{
   int ii,jj ;
   float fr,fg,top,bot,del,vv ;
   rgbyte map[NPANE_BIG] ;

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
       fg = 1.0-fr ;
       map[ii].r = (byte)(fr*col[jj].r + fg*col[jj+1].r + 0.5) ;
       map[ii].g = (byte)(fr*col[jj].g + fg*col[jj+1].g + 0.5) ;
       map[ii].b = (byte)(fr*col[jj].b + fg*col[jj+1].b + 0.5) ;
     }
   }

   PBAR_add_bigmap( name, map ) ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/

int PBAR_define_bigmap( char *cmd )
{
  int ii , neq=0 , nonum=0 ;
  char name[NSBUF], eqn[NSBUF] , rhs[NSBUF] ;
  float  val[NPANE_BIG+1] , fr,fg,fb ;
  rgbyte col[NPANE_BIG+1] ;

ENTRY("PBAR_define_bigmap") ;

  if( myfirst_dc == NULL ) RETURN(-1) ;

  name[0] = '\0' ; ii = 0 ;
  sscanf(cmd,"%127s%n",name,&ii) ;
  if(debugprint)
       printf("%s %d\n",name,ii);
  if( *name == '\0' || ii == 0 ) RETURN(-1) ;
  cmd += ii ;
  /* get lines of form "value=colordef" */

  while( neq < NPANE_BIG ){
    eqn[0] = '\0' ; ii = 0 ;
    sscanf(cmd,"%127s%n",eqn,&ii) ;
    if(debugprint)
       printf("%s %d\n",eqn,ii);
    if( *eqn == '\0' || ii == 0 ) break ;   /* exit loop */
    cmd += ii ;
    if( neq == 0 && (isalpha(eqn[0]) || eqn[0]=='#') ) nonum = 1 ;
    rhs[0] = '\0' ; ii = 0 ;
    if( !nonum ) sscanf(eqn,"%f=%s%n",val+neq,rhs,&ii) ;
    else         sscanf(eqn,"%s%n"           ,rhs,&ii) ;
    if( *rhs == '\0' || ii == 0 ) RETURN(-1);               /* bad */
      ii = DC_parse_color( myfirst_dc , rhs, &fr,&fg,&fb ) ;
      if(debugprint)
         printf("%s %f %f %f\n",rhs,fr,fg,fb);
      if( ii ) RETURN(-1);                                    /* bad */
      col[neq].r = (byte)(255.0*fr+0.5) ;
      col[neq].g = (byte)(255.0*fg+0.5) ;
      col[neq].b = (byte)(255.0*fb+0.5) ; neq++ ;
  }

  if( nonum ) {                   /* supply numbers, if missing */
    if(debugprint) printf("Supplying indices to colorscale\n");
    for( ii=0 ; ii < neq ; ii++ ) val[ii] = neq-ii ;
  }

  if(debugprint) {
     for(ii=0;ii<neq;ii++)
       printf("%f %x %x %x\n", val[ii], col[ii].r, col[ii].g, col[ii].b);
  }
  PBAR_make_bigmap( name , neq, val, col, myfirst_dc ); RETURN(0);
}

/*-----------------------------------------------------------------------*/

void PBAR_read_bigmap( char *fname , MCW_DC *dc )
{
  int ii , neq=0 , nonum=0 , yeseq=0 ;
  char name[NSBUF], lhs[NSBUF],rhs[NSBUF],mid[NSBUF],line[2*NSBUF] , *cpt ;
  float  val[NPANE_BIG] , fr,fg,fb , top,bot,del,vv ;
  rgbyte col[NPANE_BIG] ;
  FILE *fp ;

ENTRY("PBAR_read_bigmap") ;

  if( fname == NULL || *fname == '\0' || dc == NULL ) EXRETURN ;

  STATUS(fname) ;
  fp = fopen(fname,"r"); if( fp == NULL ){
    STATUS("can't open file") ; EXRETURN;
  }

  /* get name */

  do{
    cpt = fgets( line , 2*NSBUF , fp ) ;
    if( cpt == NULL ){ STATUS("can't read title line"); fclose(fp); EXRETURN; }
    name[0] = '\0' ;
    sscanf(line,"%127s",name) ;
  } while( name[0]=='\0' || name[0]=='!' || (name[0]=='/' && name[1]=='/') ) ;

  /* get lines of form "value = colordef" */

  while( neq < NPANE_BIG ){
    cpt = fgets( line , 2*NSBUF , fp ) ;
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
      if( val[neq] == 0.0 && *cpt != '\0' ){
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
    col[neq].r = (byte)(255.0*fr+0.5) ;
    col[neq].g = (byte)(255.0*fg+0.5) ;
    col[neq].b = (byte)(255.0*fb+0.5) ; neq++ ;
  } /* end of loop over color lines */
  fclose(fp) ;

  if( nonum ){                    /* supply numbers, if missing */
    for( ii=0 ; ii < neq ; ii++ )
      val[ii] = neq-ii ;
  }

  PBAR_make_bigmap( name , neq, val, col, dc ) ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Button 3 event handler for pane #0 of a pbar, used only when
    in "big" mode, to select a color map.
-------------------------------------------------------------------------*/

static void PBAR_button_EV( Widget w, XtPointer cd, XEvent *ev, Boolean *ctd )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;
   XButtonEvent *bev = (XButtonEvent *) ev ;
   int hh , ii , rr,gg,bb ;
   float yy ;

ENTRY("PBAR_button_EV") ;

#if 0
   if( bev->button == Button2 )
     XUngrabPointer( bev->display , CurrentTime ) ;
#endif

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   /* get current position, value, and color */

   MCW_widget_geom( pbar->panes[0] , NULL,&hh , NULL,NULL ) ;
   ii = (int)( ((NPANE_BIG-1.0)*bev->y)/(hh-1) + 0.5 ) ;      /* color index */
   rr = (int)pbar->bigcolor[ii].r ;                           /* color */
   gg = (int)pbar->bigcolor[ii].g ;
   bb = (int)pbar->bigcolor[ii].b ;

   yy = ii/(NPANE_BIG-1.0) ;
   yy = (yy * pbar->bigbot + (1.0-yy) * pbar->bigtop) ;
   if( pbar->bigfac != 0.0 ) yy *= pbar->bigfac ;             /* value */

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
       ii = (int)( ((NPANE_BIG-1.0)*bev->y)/(hh-1) + 0.5 ) ;
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

static void PBAR_big_menu_CB( Widget w , XtPointer cd , XtPointer qd )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;

ENTRY("PBAR_big_menu_CB") ;

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   if( w == pbar->big_choose_pb ){
     MCW_choose_strlist( w , "Choose Colorscale" ,
                         bigmap_num ,
                         pbar->bigmap_index ,
                         bigmap_name ,
                         PBAR_bigmap_finalize , cd ) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

static void PBAR_bigmap_finalize( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;
   int ii , ind=cbs->ival ;

ENTRY("PBAR_bigmap_finalize") ;

   if( ind < 0 || ind >= bigmap_num || !pbar->bigmode ){
     XBell( pbar->dc->display,100); POPDOWN_strlist_chooser; EXRETURN;
   }

   pbar->bigflip      = 0 ;                 /* 07 Feb 2004 */
   pbar->bigrota      = 0 ;
   pbar->bigname      = bigmap_name[ind] ;  /* 22 Oct 2003 */
   pbar->bigmap_index = ind ;
   for( ii=0 ; ii < NPANE_BIG ; ii++ )
     pbar->bigcolor[ii] = bigmap[ind][ii] ;

   /* If colormap is meant for ROI data, set range
     parameters automatically          ZSS Feb 15 2010 */

   if( pbar->parent != NULL ){
     if (strstr(pbar->bigname,"i32")) {
        AFNI_set_func_range_nval(pbar->parent, 32.0);
     } else if (strstr(pbar->bigname,"i64")) {
        AFNI_set_func_range_nval(pbar->parent, 64.0);
     } else if (strstr(pbar->bigname,"i128")) {
        AFNI_set_func_range_nval(pbar->parent, 128.0);
     } else if (strstr(pbar->bigname,"i255")) {
        AFNI_set_func_range_nval(pbar->parent, 255.0);
     } else if (strstr(pbar->bigname,"i256")) {
        AFNI_set_func_range_nval(pbar->parent, 256.0);
     }
   }

   MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
   PBAR_bigexpose_CB(NULL,pbar,NULL) ;
   if( XtIsRealized(pbar->panes[0]) )
     PBAR_callback(pbar,pbCR_COLOR) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

int PBAR_get_bigmap_index ( char *bnam ) /* 26 Feb. 2010 ZSS */
{
   int ii;

   if (!bnam) return(-1);

   for( ii=0 ; ii < bigmap_num ; ii++ )
     if( strcmp(bnam,bigmap_name[ii]) == 0 ) return(ii);

   return(-1);
}

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

char * PBAR_get_bigmap( MCW_pbar *pbar )    /* 03 Feb 2003 */
{
   return bigmap_name[pbar->bigmap_index] ;
}

/*--------------------------------------------------------------------*/
/*! Actually redisplay pane #0 in "big" mode.
----------------------------------------------------------------------*/

void PBAR_bigexpose_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;

ENTRY("PBAR_bigexpose_CB") ;

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   /* make an image of what we want to see */

   if( pbar->bigxim == NULL ){
     int ww,hh , ii , jj , kk ;
     MRI_IMAGE *cim ;
     XImage    *xim ;
     byte      *car , r,g,b ;

     MCW_widget_geom( pbar->panes[0] , &ww,&hh , NULL,NULL ) ;
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
           car[kk++]=128; car[kk++]=128; car[kk++]=128;
         }
       }
     }
     xim = mri_to_XImage( pbar->dc , cim ) ;
     pbar->bigxim = resize_XImage( pbar->dc , xim , ww,hh ) ;
     MCW_kill_XImage(xim) ; mri_free(cim) ;
   }

   /* actually show the image to the window pane */

   if( XtIsRealized(pbar->panes[0]) )
     XPutImage( pbar->dc->display , XtWindow(pbar->panes[0]) ,
                pbar->dc->origGC , pbar->bigxim , 0,0,0,0 ,
                pbar->bigxim->width , pbar->bigxim->height ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Set "big" mode in the pbar -- 30 Jan 2003 - RWCox.
----------------------------------------------------------------------*/

void PBAR_set_bigmode( MCW_pbar *pbar, int bmode, float bot,float top )
{
ENTRY("PBAR_set_bigmode") ;
   if( bmode && bot < top ){ pbar->bigbot = bot; pbar->bigtop = top; }
   pbar->bigmode   = bmode ;
   pbar->update_me = 1 ;
   update_MCW_pbar( pbar ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

static void PBAR_show_bigmode( MCW_pbar *pbar )  /* 30 Jan 2003 */
{
   int ii , yy ;
   char buf[16] ;

ENTRY("PBAR_show_bigmode") ;

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   if( !pbar->bigset ){   /* set up big mode */

     if( pbar->hide_changes ) XtUnmapWidget( pbar->top ) ;

     /* turn off all but 1 pane and all but 2 labels */

     XtManageChild( pbar->labels[0] ) ;
     XtManageChild( pbar->labels[1] ) ;
     for( ii=2 ; ii <= NPANE_MAX ; ii++ )
       XtUnmanageChild( pbar->labels[ii] ) ;
     XtManageChild( pbar->panes[0] ) ;
     for( ii=1 ; ii < NPANE_MAX ; ii++ )
       XtUnmanageChild( pbar->panes[ii] ) ;
     XtVaSetValues( pbar->panes[0] , XmNheight,pbar->panew_height , NULL ) ;
     XtVaSetValues( pbar->panew    , XmNheight,pbar->panew_height , NULL ) ;
     XtVaSetValues( pbar->top      , XmNheight,pbar->panew_height , NULL ) ;

     if( pbar->hide_changes ) XtMapWidget( pbar->top ) ;

     MCW_widget_geom( pbar->panes[0] , NULL,NULL,NULL , &yy ) ;
     XtVaSetValues( pbar->labels[0] , XmNy , yy , NULL ) ;
     PBAR_labelize( pbar->bigtop , buf ) ;
     MCW_set_widget_label( pbar->labels[0] , buf ) ;

     yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
     XtVaSetValues( pbar->labels[1] , XmNy , yy , NULL ) ;
     PBAR_labelize( pbar->bigbot , buf ) ;
     MCW_set_widget_label( pbar->labels[1] , buf ) ;

     pbar->bigset = 1 ;
   }

   /* show the thing */

   PBAR_bigexpose_CB( NULL , pbar , NULL ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
   make a label for the edge out of the floating value
----------------------------------------------------------------------*/

void PBAR_labelize( float val , char * buf )
{
   float aval = fabs(val) ;
   char prefix[4] ;

   if( val == 0.0  ){ strcpy(buf," 0") ; return ; }

   if( val > 0.0 ) strcpy(prefix," ") ;
   else            strcpy(prefix,"-") ;

        if( aval <= 9.994 ) sprintf(buf,"%s%4.2f",prefix,aval) ;
   else if( aval <= 99.94 ) sprintf(buf,"%s%4.1f",prefix,aval) ;
   else                     sprintf(buf,"%s%4f"  ,prefix,aval) ;
   return ;
}

/*--------------------------------------------------------------------*/

void PBAR_flip( MCW_pbar *pbar )  /* 07 Feb 2004 */
{
   rgbyte tc ; int ip ;

ENTRY("PBAR_flip") ;

   if( pbar == NULL || !pbar->bigmode ) EXRETURN ;

   for( ip=0 ; ip < NPANE_BIG/2 ; ip++ ){
     tc = pbar->bigcolor[ip] ;
     pbar->bigcolor[ip] = pbar->bigcolor[NPANE_BIG-1-ip] ;
     pbar->bigcolor[NPANE_BIG-1-ip] = tc ;
   }
   MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
   PBAR_bigexpose_CB( NULL , pbar , NULL ) ;
   pbar->bigflip = ! pbar->bigflip ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
  pbar pane was clicked --> set its color
----------------------------------------------------------------------*/

void PBAR_click_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MCW_DC * dc = (MCW_DC *) cd ;
   MCW_pbar * pbar = NULL ;
   int ip ;

ENTRY("PBAR_click_CB") ;

   XtVaGetValues( w , XmNuserData , &pbar , NULL ) ;
   if( pbar == NULL ) EXRETURN ;

   if( pbar->bigmode ){   /* 30 Jan 2003: reverse color spectrum */
     PBAR_flip( pbar ) ;
     PBAR_callback(pbar,pbCR_COLOR) ;
     EXRETURN ;
   }

   for( ip=0 ; ip < pbar->num_panes ; ip++ ) if( pbar->panes[ip] == w ) break ;
   if( ip == pbar->num_panes ) EXRETURN ;

   MCW_choose_ovcolor( w , dc , pbar->ov_index[ip] , PBAR_set_CB , dc ) ;
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

/*--------------------------------------------------------------------
  actual place where color of pane is changed, and user is callbacked
----------------------------------------------------------------------*/

void PBAR_set_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_DC * dc = (MCW_DC *) cd ;
   MCW_pbar * pbar = NULL ;
   int ip , jm ;

ENTRY("PBAR_set_CB") ;

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

void rotate_MCW_pbar( MCW_pbar * pbar , int n )
{
   int ip , iov[NPANE_MAX] , np , kov , jm ;
   Widget w ;
   MCW_DC * dc ;

ENTRY("rotate_MCW_pbar") ;

   if( pbar == NULL || n == 0 ) EXRETURN ;

   if( pbar->bigmode ){             /* 30 Jan 2003: rotate the spectrum */
     rgbyte oldcolor[NPANE_BIG] ;

     MCW_kill_XImage(pbar->bigxim) ; pbar->bigxim = NULL ;
     memcpy(oldcolor,pbar->bigcolor,sizeof(rgbyte)*NPANE_BIG) ;

     while( n < 0 ) n += NPANE_BIG ;  /* make n positive */
     for( ip=0 ; ip < NPANE_BIG ; ip++ )
       pbar->bigcolor[ip] = oldcolor[(ip+n)%NPANE_BIG] ;

     PBAR_bigexpose_CB( NULL , pbar , NULL ) ;

     pbar->bigrota += (pbar->bigflip) ? -n : n ;  /* 07 Feb 2004 */

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
   MCW_pbar * pbar = (MCW_pbar *) cd ;
   int i , sum , hh[NPANE_MAX] , yy , ip=-1 , jm ;
   char buf[16] ;
   float pmin , pmax , val ;
   int alter_all = pbar->renew_all ;

ENTRY("PBAR_resize_CB") ;

   if( pbar == NULL || pbar->renew_all < 0 ) EXRETURN ;  /* skip it */
   if( pbar->bigmode ) EXRETURN ;  /* 30 Jan 2003 */

   jm  = pbar->mode ;
   sum = 0 ;
   for( i=0 ; i < pbar->num_panes ; i++ ){
     MCW_widget_geom( pbar->panes[i] , NULL , &(hh[i]) , NULL,NULL ) ;
#ifdef PBAR_DEBUG
printf("resize: read pane # %d height=%d\n",i,hh[i]) ; fflush(stdout) ;
#endif
     sum += hh[i] ;
     if( w == pbar->panes[i] ) ip = i ;
   }

   if( sum != pbar->panes_sum ){
      if( ip != pbar->num_panes - 1 ) EXRETURN ;
      pbar->panes_sum = sum ;
      MCW_widget_geom( pbar->panew , NULL,&(pbar->panew_height),NULL,NULL) ;
#if 0
      XtVaSetValues( pbar->top , XmNheight , pbar->panew_height , NULL ) ;
#endif
      alter_all = 1 ;
   }

   sum  = 0 ;
   pmax = pbar->pval[0] ;
   pmin = pbar->pval[pbar->num_panes] ;

   for( i=0 ; i <= pbar->num_panes ; i++ ){

#if 0  /* the pre Dec 1997 way */
      val = pmax - sum * (pmax-pmin) / pbar->panes_sum ;
      if( alter_all || val != pbar->pval[i] ){
#else
      if( alter_all || (i>0 && pbar->pane_hsum[i] != sum) ){
#endif

         if( ! pbar->keep_pval ){  /* Dec 1997 */
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
#if 1
               yy = pbar->panew_height - PANE_LOFF + PANE_SPACING ;
#else
               yy = pbar->panew_height - 2 * PANE_LOFF + PANE_SPACING ;
#endif
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
   EXRETURN ;
}

/*-------------------------------------------------------------------------
  user want to programatically alter the pbar:
    number of panes, and/or new array of values
---------------------------------------------------------------------------*/

void update_MCW_pbar( MCW_pbar * pbar )
{
ENTRY("update_MCW_pbar") ;
   if( pbar == NULL ) EXRETURN ;
   if( pbar->update_me ){
     if( pbar->bigmode ) PBAR_show_bigmode( pbar ) ;         /* 30 Jan 2003 */
     else                alter_MCW_pbar( pbar , 0 , NULL ) ;
   }
   pbar->update_me = 0 ;
   EXRETURN ;
}

void alter_MCW_pbar( MCW_pbar * pbar , int new_npane , float * new_pval )
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

   if( was_bigset ) npane_old = 1 ;

   /*-- get new value array --*/

   if( new_pval == NULL ){
     for( i=0 ; i <= npane ; i++ ) pval[i] = pbar->pval_save[npane][i][jm] ;
   } else {
     for( i=0 ; i <= npane ; i++ ) pval[i] = new_pval[i] ;
   }
   pmax = pval[0] ;
   pmin = pval[npane] ;

   /*--- make new panes or destroy old ones ---*/

   if( pbar->hide_changes ) XtUnmapWidget( pbar->top ) ;

   /* set new pane colors */

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

   pbar->panes_sum = pbar->panew_height - (npane-1)*PANE_SPACING ;
   for( i=0 ; i <= npane ; i++ ) pbar->pval[i] = pval[i] ;

   sum = pbar->panes_sum ;
   rhh = 0.0 ;
   for( i=0 ; i < npane-1 ; i++ ){
      fhh  = pbar->panes_sum * (pval[i]-pval[i+1]) / (pmax-pmin) ;
      hh   = (int) (rhh+fhh+0.45) ;
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

MRI_IMAGE * MCW_pbar_to_mri( MCW_pbar * pbar , int nx , int ny )
{
   MRI_IMAGE * im ;
   int   ii,npix,kk,ll,jj , sum,hh=0 ;
   float pmin,pmax , rhh,fhh , hfrac ;
   byte rr,gg,bb , *bar ;

ENTRY("MCW_pbar_to_mri") ;

   /* check for decent inputs */

   if( pbar == NULL ) RETURN(NULL) ;
   if( nx < 1 ) nx = 1 ;

   if( pbar->bigmode ){    /* 30 Jan 2003: save spectrum */
     XImage *xim ;
     if( pbar->bigxim == NULL ){
       PBAR_bigexpose_CB(NULL,pbar,NULL) ;
       if( pbar->bigxim == NULL ) RETURN(NULL) ;
     }
     if( ny < NPANE_BIG ) ny = NPANE_BIG ;
     xim = resize_XImage( pbar->dc , pbar->bigxim , nx,ny ) ;
     im  = XImage_to_mri( pbar->dc , xim , X2M_USE_CMAP|X2M_FORCE_RGB ) ;
     MCW_kill_XImage( xim ) ;
     RETURN(im) ;
   }

   /** the old way: make the image by brute force **/

   if( ny < 4*pbar->num_panes ) ny = 4*pbar->num_panes ;

   im  = mri_new( nx , ny , MRI_rgb ) ;
   bar = MRI_RGB_PTR(im) ;

   pmax = pbar->pval[0] ;
   pmin = pbar->pval[pbar->num_panes] ;

   hfrac = ny / (pmax-pmin) ;
   rhh  = 0.0 ;
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
