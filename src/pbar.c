/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "pbar.h"

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

   /* sanity check */

   if( npane < NPANE_MIN          || npane > NPANE_MAX ||
       pheight < PANE_MIN_HEIGHT  || pmin == pmax         ) return NULL ;

   /* new pbar */

   lcol = dc->ovc->ncol_ov - 1 ;  /* last color available */

   pbar = myXtNew( MCW_pbar ) ;

   pbar->top = XtVaCreateWidget( "pbar" , xmBulletinBoardWidgetClass , parent ,
                                     XmNmarginHeight , 0 ,
                                     XmNmarginWidth , 0 ,
                                     XmNheight , npane*pheight+(npane-1)*PANE_SPACING ,
                                     XmNresizePolicy , XmRESIZE_ANY ,
                                     XmNtraversalOn , False ,
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
                                      XmNtraversalOn, False ,
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
                              XmNtraversalOn , False,
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

   /*-- go home --*/

   XtManageChild( pbar->top ) ;
   return pbar ;
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

/*--------------------------------------------------------------------
  pbar pane was clicked --> set its color
----------------------------------------------------------------------*/

void PBAR_click_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MCW_DC * dc = (MCW_DC *) cd ;
   MCW_pbar * pbar = NULL ;
   int ip ;

   XtVaGetValues( w , XmNuserData , &pbar , NULL ) ;
   if( pbar == NULL ) return ;
   for( ip=0 ; ip < pbar->num_panes ; ip++ ) if( pbar->panes[ip] == w ) break ;
   if( ip == pbar->num_panes ) return ;

   MCW_choose_ovcolor( w , dc , pbar->ov_index[ip] , PBAR_set_CB , dc ) ;
}

/*--------------------------------------------------------------------
  actual place where color of pane is changed, and user is callbacked
----------------------------------------------------------------------*/

void PBAR_set_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_DC * dc = (MCW_DC *) cd ;
   MCW_pbar * pbar = NULL ;
   int ip , jm ;

   if( cbs->ival > 0 && cbs->ival < dc->ovc->ncol_ov ){
      XtVaSetValues( w , XmNbackgroundPixmap , XmUNSPECIFIED_PIXMAP , NULL ) ;
      MCW_set_widget_bg( w , NULL , dc->ovc->pix_ov[cbs->ival] ) ;
   } else {
      XtVaSetValues( w , XmNbackgroundPixmap , check_pixmap , NULL ) ;
   }

   XtVaGetValues( w , XmNuserData , &pbar , NULL ) ;
   if( pbar == NULL ) return ;

   for( ip=0 ; ip < pbar->num_panes ; ip++ ) if( pbar->panes[ip] == w ) break ;
   if( ip == pbar->num_panes ) return ;

   jm = pbar->mode ;
   pbar->ovin_save[pbar->num_panes][ip][jm] =
                         pbar->ov_index[ip] = cbs->ival ;

   if( pbar->pb_CB != NULL ) pbar->pb_CB( pbar , pbar->pb_data , pbCR_COLOR ) ;
   return ;
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

   if( pbar->renew_all < 0 ) return ;  /* skip it */

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
      if( ip != pbar->num_panes - 1 ) return ;
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

   if( pbar->pb_CB != NULL )
      pbar->pb_CB( pbar , pbar->pb_data , pbCR_VALUE ) ;

   pbar->renew_all = 0 ;
}

/*-------------------------------------------------------------------------
  user want to programatically alter the pbar:
    number of panes, and/or new array of values
---------------------------------------------------------------------------*/

void update_MCW_pbar( MCW_pbar * pbar )
{
   if( pbar->update_me ) alter_MCW_pbar( pbar , 0 , NULL ) ;
   pbar->update_me = 0 ;
}

void alter_MCW_pbar( MCW_pbar * pbar , int new_npane , float * new_pval )
{
   int i , npane , npane_old , sum , hh , ovc , jm ;
   float pmin , pmax , pval[NPANE_MAX+1] , fhh , rhh ;

   /* sanity check */

   if( pbar == NULL || new_npane > NPANE_MAX ||
       ( new_npane < NPANE_MIN && new_npane != 0 ) ) return ;

   /* count of panes, old and new */

   jm              = pbar->mode ;
   npane           = (new_npane > 0) ? new_npane : pbar->num_panes ;
   npane_old       = pbar->num_panes ;
   pbar->num_panes = pbar->npan_save[jm] = npane ;

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

}

/*-------------------------------------------------------------------------
   Make an image of the pbar (sans handles)
   -- RWCox - 15 Jun 2000
---------------------------------------------------------------------------*/

MRI_IMAGE * MCW_pbar_to_mri( MCW_pbar * pbar , int nx , int ny )
{
   MRI_IMAGE * im ;
   int   ii,npix,kk,ll,jj , sum,hh ;
   float pmin,pmax , rhh,fhh , hfrac ;
   byte rr,gg,bb , *bar ;

   /* check for decent inputs */

   if( pbar == NULL ) return NULL ;
   if( nx < 1                 ) nx = 1 ;
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

   return im ;
}
