#include "bbox.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*-------------------------------------------------------------------------
   create a new MCW_bbox:
       parent    = parent Widget
       num_but   = number of buttons (from 1 to MCW_MAX_BB)
       label_but = array of strings of button labels
       bb_type   = MCW_BB_check      -> check box (any may be on)
                   MCW_BB_radio_one  -> radio box, exactly one will be on
                   MCW_BB_radio_zero -> radio box, zero or one will be on
       bb_frame  = MCW_BB_noframe    -> no frame around box
                 = MCW_BB_frame      -> put frame around box
       cb        = Callback procedure for Disarm (NULL for none)
       cb_data   = data to pass to Callback procedure (NULL for none)
---------------------------------------------------------------------------*/

MCW_bbox * new_MCW_bbox( Widget parent ,
                         int num_but , char * label_but[] ,
                         int bb_type , int bb_frame ,
                         XtCallbackProc cb , XtPointer cb_data )
{
   MCW_bbox * bb ;
   int ib , initial_value ;
   Widget rc_parent ;
   Arg wa[30] ;  int na ;
   Pixel  fg_pix ;

   if( num_but <= 0 || num_but >= 32 ){
      fprintf(stderr,"\n*** illegal new_MCW_bbox has %d buttons\n",num_but) ;
      exit(-1) ;
   }

   bb = (MCW_bbox *) XtMalloc( sizeof(MCW_bbox) ) ;

   bb->nbut      = num_but ;
   initial_value = 0 ;

   /***--- create Frame, if desired ---***/

   switch( bb_frame ){

      case MCW_BB_frame:
         rc_parent = bb->wtop = bb->wframe =
            XtVaCreateManagedWidget(
               "frame" , xmFrameWidgetClass , parent ,
                   XmNshadowType , XmSHADOW_ETCHED_IN ,
                   XmNinitialResourcesPersistent , False ,
               NULL ) ;
         break ;

      case MCW_BB_noframe:
      default:
        rc_parent  = parent ;
        bb->wframe = NULL ;
        break ;
   }

   /***--- create RowColumn to hold the buttons ---***/

#define MAX_PER_COL 8

   na = 0 ;

#ifdef BBOX_COL
   XtSetArg( wa[na] , XmNpacking    , XmPACK_COLUMN )               ; na++ ;
   XtSetArg( wa[na] , XmNnumColumns , 1 + (num_but-1)/MAX_PER_COL ) ; na++ ;
#else
   XtSetArg( wa[na] , XmNpacking    , XmPACK_TIGHT )                ; na++ ;
#endif

   XtSetArg( wa[na] , XmNmarginHeight , 0 ) ; na++ ;  /* squash things in */
   XtSetArg( wa[na] , XmNmarginWidth  , 0 ) ; na++ ;
   XtSetArg( wa[na] , XmNspacing      , 1 ) ; na++ ;

   XtSetArg( wa[na] , XmNinitialResourcesPersistent , False ) ; na++ ;

   if( bb_type == MCW_BB_radio_zero || bb_type == MCW_BB_radio_one ){

      XtSetArg( wa[na] , XmNradioBehavior , True ) ; na++ ;

      if( bb_type == MCW_BB_radio_one ){
         initial_value = 1 ;
         XtSetArg( wa[na] , XmNradioAlwaysOne , True ) ; na++ ;
      } else {
         XtSetArg( wa[na] , XmNradioAlwaysOne , False ) ; na++ ;
      }
   }

   bb->wrowcol = XtCreateWidget(
                   "dialog" , xmRowColumnWidgetClass , rc_parent ,
                   wa , na ) ;

   if( bb->wframe == NULL ) bb->wtop = bb->wrowcol ;  /* topmost widget */

   XtVaGetValues( bb->wtop , XmNforeground , &fg_pix , NULL ) ;

   /***--- create the buttons ---***/

   for( ib=0 ; ib < num_but ; ib++ ){
      bb->wbut[ib] = XtVaCreateManagedWidget(
                        "dialog" , xmToggleButtonWidgetClass , bb->wrowcol ,
                           LABEL_ARG(label_but[ib]) ,
                           XmNmarginHeight  , 0 ,
                           XmNmarginWidth   , 0 ,
                           XmNselectColor   , fg_pix ,  /* 04 Nov 1996 */
                           XmNrecomputeSize , False ,
                           XmNtraversalOn   , False ,
                           XmNinitialResourcesPersistent , False ,
                        NULL ) ;

      if( cb != NULL )
         XtAddCallback( bb->wbut[ib] , XmNdisarmCallback , cb , cb_data ) ;

   }
   for( ib=num_but ; ib < MCW_MAX_BB ; ib++ ) bb->wbut[ib] = NULL ;

   MCW_set_bbox( bb , initial_value ) ;
   XtManageChild( bb->wrowcol ) ;

   bb->parent = bb->aux = NULL ;
   return bb ;
}

/*------------------------------------------------------------------------*/

void MCW_bbox_hints( MCW_bbox * bb , int nh , char ** hh )
{
   int ib ;

   if( bb == NULL || nh == 0 || hh == NULL ) return ;
   if( nh > bb->nbut ) nh = bb->nbut ;
   for( ib=0 ; ib < nh ; ib++ )
      MCW_register_hint( bb->wbut[ib] , hh[ib] ) ;
   return ;
}

/*------------------------------------------------------------------------*/

void MCW_set_bbox( MCW_bbox * bb , int val )
{
   int     ib ;
   Boolean set ;

   bb->value = val ;

   for( ib=0 ; ib < bb->nbut ; ib++ ){
      set = ( val & (1<<ib) ) ? (True) : (False) ;
      XmToggleButtonSetState( bb->wbut[ib] , set , False ) ;
   }
   return ;
}

/*------------------------------------------------------------------------*/

int MCW_val_bbox( MCW_bbox * bb )
{
   int ib , val ;
   Boolean set ;

   val = 0 ;

   for( ib=0 ; ib < bb->nbut ; ib++ ){
      set = XmToggleButtonGetState( bb->wbut[ib] ) ;
      if( set ) val |= (1<<ib) ;
   }

   bb->value = val ;
   return val ;
}

/*----------------------------------------------------------------------
  Create a new MCW_arrowval:   [label] [v][^] [value]
    parent  = parent Widget
    label   = string to put to left of arrows (NULL means none)
    direc   = MCW_AV_downup    for down and up arrows
              MCW_AV_leftright for left and right arrows
              MCW_AV_optmenu   for option menu (completely different style!)
    minval  = smallest value allowed } value is like in Scales:
    maxval  = largest  value allowed }   an integer
    inival  = initial  value         }
    textype = MCW_AV_notext   to turn display of value off
              MCW_AV_editext  to allow user to edit the text
              MCW_AV_noactext like above, but no "activation" when the
                              cursor leaves the window
              MCW_AV_readtext to make the text display readonly
    decim   = # of decimals to shift to left for display of value
              (like Scales)

    delta_value = pointer to a function that will be called when the value
                  changes (due to arrows or text edit);  not used if NULL
    delta_data  = pointer to data to be passed to delta_value;
                     delta_value( av , delta_data ) ;
                  where av is a pointer to the MCW_arrowval that changed.
                  (N.B.: the old value is available as av->old_ival
                                                    or av->old_fval)

    text_proc   = pointer to a function that returns the text to display
                    in the value window;  if non-NULL, then textype is
                    forced to be MCW_AV_readtext;  the routine is called by

                      string = text_proc( av , text_data )

    text_data   = pointer to data to be passed to text_proc
---------------------------------------------------------------------------*/

MCW_arrowval * new_MCW_arrowval( Widget parent ,
                                 char * label ,
                                 int    direc ,
                                 int    minval , int maxval , int inival ,
                                 int    textype ,  int decim ,
                                 gen_func * delta_value, XtPointer delta_data,
                                 str_func * text_proc  , XtPointer text_data
                               )
{
   MCW_arrowval * av = NULL ;
   int asizx = 20 , asizy = 15 ;  /* arrow sizes */

   /** July 1996: optmenu capability as a dropin for arrowval **/

   if( direc == MCW_AV_optmenu ){
      av = new_MCW_optmenu( parent , label , minval,maxval,inival , decim ,
                            delta_value , delta_data , text_proc , text_data ) ;
      return av ;
   }

   av = myXtNew( MCW_arrowval ) ;
   av->wrowcol = XtVaCreateWidget(
                    "dialog" , xmRowColumnWidgetClass , parent ,

                       XmNpacking      , XmPACK_TIGHT ,
                       XmNorientation  , XmHORIZONTAL ,
                       XmNmarginHeight , 0 ,
                       XmNmarginWidth  , 0 ,
                       XmNspacing      , 0 ,
#if 0
                       XmNresizeHeight , False ,
                       XmNresizeWidth  , False ,
#endif
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , False ,
                    NULL ) ;

   if( label != NULL && strlen(label) > 0 ){
      XmString   xstr = XmStringCreateLtoR( label , XmFONTLIST_DEFAULT_TAG );
      XmFontList xflist ;

      av->wlabel = XtVaCreateManagedWidget(
                    "dialog" , xmLabelWidgetClass , av->wrowcol ,

                       XmNlabelString   , xstr  ,
                       XmNrecomputeSize , False ,
                       XmNmarginWidth   , 0     ,

                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

      XtVaGetValues( av->wlabel , XmNfontList , &xflist , NULL ) ;

      asizy = XmStringHeight( xflist , xstr ) ;
      XmStringFree( xstr ) ;

   } else {
      av->wlabel = NULL ;
   }

   if( asizx < asizy ) asizx = asizy ;
   else                asizy = asizx ;

   av->wdown = XtVaCreateManagedWidget(
                  "arrow" , xmArrowButtonWidgetClass , av->wrowcol ,

                     XmNarrowDirection , (direc==MCW_AV_leftright)
                                         ? XmARROW_LEFT : XmARROW_DOWN ,

                     XmNheight , asizy , XmNwidth , asizx ,
                     XmNborderWidth , 0 ,

                     XmNinitialResourcesPersistent , False ,
                     XmNtraversalOn , False ,
                  NULL ) ;

   av->wup    = XtVaCreateManagedWidget(
                  "arrow" , xmArrowButtonWidgetClass , av->wrowcol ,

                     XmNarrowDirection , (direc==MCW_AV_leftright)
                                         ? XmARROW_RIGHT : XmARROW_UP ,

                     XmNheight , asizy , XmNwidth , asizx ,
                     XmNborderWidth , 0 ,

                     XmNinitialResourcesPersistent , False ,
                     XmNtraversalOn , False ,
                  NULL ) ;

   XtAddCallback( av->wdown , XmNarmCallback    , AV_press_CB , av ) ;
   XtAddCallback( av->wdown , XmNdisarmCallback , AV_press_CB , av ) ;
   XtAddCallback( av->wup   , XmNarmCallback    , AV_press_CB , av ) ;
   XtAddCallback( av->wup   , XmNdisarmCallback , AV_press_CB , av ) ;

   if( text_proc != NULL && textype != MCW_AV_notext )
      textype = MCW_AV_readtext ;

   switch( textype ){

      default:
      case MCW_AV_notext:
         av->wtext     = NULL ;
         av->text_CB   = NULL ;
         av->text_data = NULL ;
      break ;

      /* Note hardwire of 9 columns of text, here and in AV_fval_to_char;
         this CANNOT be changed just by changing AV_NCOL below, you must
         also edit the sprintf formats in AV_fval_to_char.

         If text_proc is not NULL, then av->wtext could have its dimensions
         changed later to handle the user supplied string.  My point above
         is that the default text_proc is hardwired to 9 characters wide.
      */

#ifndef AV_NCOL
#define AV_NCOL 9
#endif

      case MCW_AV_readtext:
         av->wtext = XtVaCreateManagedWidget(
                       "dialog" , TEXT_CLASS , av->wrowcol ,

                          XmNcolumns         , AV_NCOL ,
                          XmNeditable        , False ,
                          XmNmaxLength       , AV_NCOL ,
                          XmNresizeWidth     , False ,
                          XmNshadowThickness , 0 ,
#if 0
                          XmNsensitive       , False ,   /* looks bad */
#endif
                          XmNmarginHeight    , 1 ,
                          XmNmarginWidth     , 1 ,

                          XmNcursorPositionVisible , False ,

                          XmNinitialResourcesPersistent , False ,
                          XmNtraversalOn , False ,
                       NULL ) ;

         av->text_CB   = (text_proc != NULL ) ? (text_proc)
                                              : (AV_default_text_CB) ;
         av->text_data = text_data ;
      break ;

      case MCW_AV_noactext:                   /* noactext added 08 Feb 1999 */
      case MCW_AV_editext:{
         Widget wf ; int maxlen ;

         if( textype == MCW_AV_noactext ){
            wf = XtVaCreateWidget( "dialog" , xmFrameWidgetClass , av->wrowcol ,
                                      XmNshadowType , XmSHADOW_OUT ,
                                      XmNshadowThickness , 1 ,
                                      XmNtraversalOn , False ,
                                      XmNinitialResourcesPersistent , False ,
                                   NULL ) ;
            maxlen = AV_MAXLEN ;
         } else {
            wf     = av->wrowcol ;
            maxlen = AV_NCOL ;
         }

         av->wtext = XtVaCreateManagedWidget(
                       "dialog" , TEXT_CLASS , wf ,

                          XmNcolumns         , AV_NCOL ,
                          XmNeditable        , True ,
                          XmNmaxLength       , maxlen ,
                          XmNresizeWidth     , False ,

                          XmNmarginHeight    , 1 ,
                          XmNmarginWidth     , 1 ,

                          XmNcursorPositionVisible , True ,
                          XmNblinkRate , 0 ,
                          XmNautoShowCursorPosition , True ,

                          XmNinitialResourcesPersistent , False ,
                          XmNtraversalOn , False ,
                       NULL ) ;

         if( textype == MCW_AV_noactext ) XtManageChild(wf) ;

         if( textype == MCW_AV_editext ){
            XtAddCallback( av->wtext , XmNactivateCallback    ,
                                       AV_textact_CB , av ) ; /* return key */

            XtAddCallback( av->wtext , XmNlosingFocusCallback ,
                                       AV_textact_CB , av ) ; /* tab key */

            XtInsertEventHandler( av->wtext ,        /* notify when */
                                  LeaveWindowMask ,  /* pointer leaves */
                                  FALSE ,            /* this window */
                                  AV_leave_EV ,
                                  (XtPointer) av ,
                                  XtListTail ) ;     /* last in queue */
         }

         av->text_CB   = AV_default_text_CB ;
         av->text_data = NULL ;
      }
      break ;

   }

   XtManageChild( av->wrowcol ) ;

   if( minval < maxval ){
      av->fmin = av->imin = minval ; AV_SHIFT_VAL(decim,av->fmin) ;
      av->fmax = av->imax = maxval ; AV_SHIFT_VAL(decim,av->fmax) ;
   } else {
      av->fmin = av->imin = -9999999 ; AV_SHIFT_VAL(decim,av->fmin) ;
      av->fmax = av->imax =  9999999 ; AV_SHIFT_VAL(decim,av->fmax) ;
   }
   av->decimals  = decim ;
   av->timer_id  = 0 ;
   av->fastdelay = MCW_AV_shortdelay ;  /* default delay on 2nd call */

   av->fval = av->ival = inival ; AV_SHIFT_VAL(decim,av->fval) ;

   av->sval = av->old_sval = NULL ;  /* string values */

   av->block_assign_actions = 0 ;    /* don't block these actions */
   av->wmenu  = NULL ;               /* signal that this is NOT an optmenu */

   AV_assign_ival( av , inival ) ;

   av->dval_CB   = delta_value ;
   av->dval_data = delta_data ;

   av->allow_wrap = 0 ;

   av->parent = av->aux = NULL ;
   av->fstep = 0.0 ;  /* 16 Feb 1999 */
   return av ;
}

/*-----------------------------------------------------------------------
  This can be used as a "drop in" replacement for a arrowval with a
  small fixed number of elements.  The textype argument is missing because
  the only one that makes sense is MCW_AV_readtext (readonly text as button
  labels).  The direc argument is missing because that choice has already
  been made.
-------------------------------------------------------------------------*/

#define COLSIZE 20

MCW_arrowval * new_MCW_optmenu( Widget parent ,
                                char * label ,
                                int    minval , int maxval , int inival , int decim ,
                                gen_func * delta_value, XtPointer delta_data,
                                str_func * text_proc  , XtPointer text_data
                              )
{
   MCW_arrowval * av = myXtNew( MCW_arrowval ) ;
   Widget wmenu , wbut ;
   Arg args[5] ;
   int nargs , ival ;
   XmString xstr ;
   char * butlabel , * blab ;

   /** create the menu window **/

   av->wmenu = wmenu = XmCreatePulldownMenu( parent , "menu" , NULL , 0 ) ;

   /** create the button that pops down the menu **/

   nargs = 0 ;
   XtSetArg( args[0] , XmNsubMenuId , wmenu ) ; nargs++ ;

   xstr = XmStringCreateLtoR( label , XmFONTLIST_DEFAULT_TAG ) ;
   XtSetArg( args[1] , XmNlabelString , xstr ) ; nargs++ ;

   av->wrowcol = XmCreateOptionMenu( parent , "dialog" , args , nargs ) ;
   XmStringFree(xstr) ;
   XtVaSetValues( av->wrowcol ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 2 ,
                  NULL ) ;

   av->wlabel = XmOptionLabelGadget (av->wrowcol) ;
   av->wdown  = XmOptionButtonGadget(av->wrowcol) ;
   av->wup    = NULL ;
   av->wtext  = NULL ;  /* signal that this is NOT really an arrowval */

   XtVaSetValues( av->wlabel ,              /* label next to menu button */
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginBottom , 0 ,
                     XmNmarginTop    , 0 ,
                     XmNmarginRight  , 0 ,
                     XmNmarginLeft   , 0 ,
                  NULL ) ;

   if( label == NULL || strlen(label) == 0 ){
      XtVaSetValues( av->wlabel  , XmNwidth   , 0 , NULL ) ;
      XtVaSetValues( av->wrowcol , XmNspacing , 2 , NULL ) ;
   }

   XtVaSetValues( av->wdown ,               /* menu button */
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginBottom , 0 ,
                     XmNmarginTop    , 0 ,
                     XmNmarginRight  , 0 ,
                     XmNmarginLeft   , 0 ,
                     XmNhighlightThickness , 0 ,
                  NULL ) ;

   av->text_CB   = (text_proc != NULL ) ? (text_proc)
                                        : (AV_default_text_CB) ;
   av->text_data = text_data ;
   av->decimals  = decim ;
   av->fmin      = av->imin = minval ; AV_SHIFT_VAL(decim,av->fmin) ;
   av->fmax      = av->imax = maxval ; AV_SHIFT_VAL(decim,av->fmax) ;
   av->sval      = av->old_sval = NULL ;

   av->block_assign_actions = 1 ;    /* temporarily block these actions */

   /** create the buttons on the menu window **/

   for( ival=minval ; ival <= maxval ; ival++ ){

      AV_assign_ival( av , ival ) ;  /* just to create label */

      blab = butlabel = XtNewString( av->sval ) ;
      if( av->text_CB==AV_default_text_CB && butlabel[0]==' ' && minval >= 0 ){
         blab += 1 ;  /* deal with leading blanks in default routine */
      }

      xstr = XmStringCreateLtoR( blab , XmFONTLIST_DEFAULT_TAG ) ;

      wbut = XtVaCreateManagedWidget(
                "dialog" , xmPushButtonWidgetClass , wmenu ,
                  XmNlabelString  , xstr ,
                  XmNmarginWidth  , 0 ,
                  XmNmarginHeight , 0 ,
                  XmNmarginBottom , 0 ,
                  XmNmarginTop    , 0 ,
                  XmNmarginRight  , 0 ,
                  XmNmarginLeft   , 0 ,
                  XmNuserData     , (XtPointer) ival ,    /* Who am I? */
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
                NULL ) ;

      XmStringFree(xstr) ; myXtFree(butlabel) ;

      XtAddCallback( wbut , XmNactivateCallback , AVOPT_press_CB , av ) ;

      if( ival == inival )
         XtVaSetValues( av->wrowcol ,  XmNmenuHistory , wbut , NULL ) ;
   }

   XtManageChild( av->wrowcol ) ;

   av->timer_id  = 0 ;  /* won't be used for this type of arrowval! */
   av->fastdelay = 0 ;

   av->block_assign_actions = 0 ;   /* unblock these actions */

   AV_assign_ival( av , inival ) ;  /* actual initial assignment */

   av->dval_CB   = delta_value ;
   av->dval_data = delta_data ;

   av->allow_wrap = 0 ;

   av->parent = av->aux = NULL ;
   av->fstep = 0.0 ;  /* 16 Feb 1999 */

   return av ;
}

/*----------------------------------------------------------------------------
   Relabels all the buttons on an optmenu, adding or unmanaging as needed.
   The label and action callback remain the same.
------------------------------------------------------------------------------*/

void refit_MCW_optmenu( MCW_arrowval * av ,
                        int  minval , int maxval , int inival , int decim ,
                        str_func * text_proc  , XtPointer text_data )
{
   Widget * children , wbut , wmenu ;
   int  num_children , ic , ival ;
   char * butlabel , * blab ;
   XmString xstr ;

#ifdef AFNI_DEBUG
printf("Enter refit_MCW_optmenu: %d..%d init %d\n",minval,maxval,inival) ;
#endif

   /** sanity check **/

   if( av == NULL || av->wmenu == NULL ) return ;
   wmenu = av->wmenu ;

   /** get all the existing children **/

#if 0
   XtUnmanageChild( av->wrowcol ) ; XtUnmanageChild( wmenu ) ;
#endif

   XtVaGetValues( wmenu ,
                     XmNchildren    , &children ,
                     XmNnumChildren , &num_children ,
                  NULL ) ;

#ifdef BBOX_DEBUG
printf(" -- found %d old children\n",num_children) ;
#endif

   /** reset some internal parameters **/

   av->text_CB   = (text_proc != NULL ) ? (text_proc)
                                        : (AV_default_text_CB) ;
   av->text_data = text_data ;
   av->decimals  = decim ;
   av->fmin      = av->imin = minval ; AV_SHIFT_VAL(decim,av->fmin) ;
   av->fmax      = av->imax = maxval ; AV_SHIFT_VAL(decim,av->fmax) ;

   myXtFree(av->sval) ; myXtFree(av->old_sval) ;  /* 09 Mar 1999 */

   av->block_assign_actions = 1 ;    /* temporarily block these actions */

   /** create buttons anew **/

   for( ival=minval ; ival <= maxval ; ival++ ){

      ic = ival - minval ;           /* index into widget list */

      AV_assign_ival( av , ival ) ;  /* just to create label */

      blab = butlabel = XtNewString( av->sval ) ;
      if( av->text_CB==AV_default_text_CB && butlabel[0]==' ' && minval >= 0 ){
         blab += 1 ;  /* deal with leading blanks in default routine */
      }

#ifdef BBOX_DEBUG
printf(" -- button %d: index=%d label=%s",ival,ic,blab) ; fflush(stdout) ;
#endif

      xstr = XmStringCreateLtoR( blab , XmFONTLIST_DEFAULT_TAG ) ;

      /** re-use old button if possible, otherwise add a new one **/

      if( ic < num_children ){
         XtPointer user_old ;
         int       ival_old ;
         XmString  xstr_old ;

         wbut = children[ic] ;
         XtVaGetValues( wbut ,
                           XmNlabelString , &xstr_old ,
                           XmNuserData    , &user_old ,
                        NULL ) ;
         ival_old = (int) user_old ;

         if( ival_old != ival || XmStringCompare(xstr_old,xstr) != True ){

#ifdef BBOX_DEBUG
printf(": relabeling old button\n") ;
#endif
            XtVaSetValues( wbut ,
                              XmNlabelString , xstr ,             /* change label */
                              XmNuserData    , (XtPointer) ival , /* Who am I? */
                           NULL ) ;
         }
#ifdef BBOX_DEBUG
         else { printf(": reusing old button\n") ; }
#endif
         XmStringFree( xstr_old ) ;
         XtManageChild( wbut ) ;    /* if not now managed */
      } else {
#ifdef BBOX_DEBUG
printf(": creating new button\n") ;
#endif
         wbut = XtVaCreateManagedWidget(
                   "dialog" , xmPushButtonWidgetClass , wmenu ,
                     XmNlabelString  , xstr ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginBottom , 0 ,
                     XmNmarginTop    , 0 ,
                     XmNmarginRight  , 0 ,
                     XmNmarginLeft   , 0 ,
                     XmNuserData     , (XtPointer) ival ,    /* Who am I? */
                     XmNtraversalOn , False ,
                     XmNinitialResourcesPersistent , False ,
                   NULL ) ;
         XtAddCallback( wbut , XmNactivateCallback , AVOPT_press_CB , av ) ;
      }

      XmStringFree(xstr) ; myXtFree(butlabel) ;

      if( ival == inival )
         XtVaSetValues( av->wrowcol ,  XmNmenuHistory , wbut , NULL ) ;
   }

   /** Unmanage extra children from an old incarnation **/

#ifdef BBOX_DEBUG
if( maxval-minval+1 < num_children )
   printf(" -- Unmanaging unneeded old children\n") ;
#endif

   ic = maxval-minval+1 ;  /* first child after those used above */

   if( ic < num_children )
      XtUnmanageChildren( children + ic , num_children - ic ) ;

   /** set number of columns to see **/

#ifdef BBOX_DEBUG
printf(" -- setting columnization\n") ;
#endif

   AVOPT_columnize( av , 1+(maxval-minval)/COLSIZE ) ;

#ifdef BBOX_DEBUG
printf(" -- managing whole shebang\n") ;
#endif

#if 0
   XtManageChild( wmenu ) ;
   XtManageChild( av->wrowcol ) ;
#endif

#if 0
   XtPopdown( XtParent(wmenu) ) ;  /* 28 Apr 1997 */
#endif

#ifdef BBOX_DEBUG
printf(" -- performing initial assignment\n") ;
#endif

   av->block_assign_actions = 0 ;   /* unblock these actions */
   AV_assign_ival( av , inival ) ;  /* actual initial assignment */

#ifdef AFNI_DEBUG
printf("Exit refit_MCW_optmenu\n") ;
#endif
   return ;
}

/*--------------------------------------------------------------------------
   Create a colormenu -- an optmenu with buttons colorized
----------------------------------------------------------------------------*/

MCW_arrowval * new_MCW_colormenu( Widget parent , char * label , MCW_DC * dc ,
                                  int min_col , int max_col , int ini_col ,
                                  gen_func * delta_value, XtPointer delta_data
                                )
{
   MCW_arrowval * av ;
   Widget * children ;
   int  num_children , ic , icol ;

   av = new_MCW_optmenu( parent , label ,
                         min_col , max_col , ini_col , 0 ,
                         delta_value , delta_data ,
                         MCW_DC_ovcolor_text , (XtPointer) dc ) ;

   XtVaGetValues( av->wmenu , XmNchildren    , &children ,
                              XmNnumChildren , &num_children , NULL ) ;

   for( ic=0 ; ic < num_children ; ic++ ){
      icol = min_col + ic ;
      if( icol > 0 ) MCW_set_widget_bg( children[ic] , 0 , dc->ovc->pix_ov[icol] ) ;
      else           MCW_set_widget_bg( children[ic] , "gray40" , 0 ) ;
   }

   if( max_col > COLSIZE ) AVOPT_columnize( av , 1+(max_col-1)/COLSIZE ) ;

   return av ;
}

char * MCW_av_substring_CB( MCW_arrowval * av , XtPointer cd )
{
   char ** str = (char **) cd ;
   return str[av->ival] ;
}

/*-----------------------------------------------------------------------*/

void AVOPT_press_CB( Widget wbut, XtPointer client_data, XtPointer call_data )
{
   MCW_arrowval * av = (MCW_arrowval *) client_data ;
   int newval ;
   XtPointer xval ;

   XtVaGetValues( wbut , XmNuserData , &xval , NULL ) ;
   newval = (int) xval ;

   AV_assign_ival( av , newval ) ;  /* assign */

   /* call user callback, if present */

   if( av->dval_CB != NULL && av->fval != av->old_fval )
      av->dval_CB( av , av->dval_data ) ;

   return ;
}

/*-----------------------------------------------------------------------*/

void AV_press_CB( Widget warrow, XtPointer client_data, XtPointer call_data )
{
   MCW_arrowval * av                 = (MCW_arrowval *) client_data ;
   XmArrowButtonCallbackStruct * cbs =
                        (XmArrowButtonCallbackStruct *) call_data ;

   XtIntervalId fake_id = 0 ;

   /* release of button */

   switch( cbs->reason ){

      default:
      case XmCR_DISARM:
         if( av->timer_id != 0 ) XtRemoveTimeOut( av->timer_id ) ; /* stop */
         av->timer_id = 0 ;
      break ;

      case XmCR_ARM:
              if( warrow == av->wup   ) av->incr =  1 ;  /* go up */
         else if( warrow == av->wdown ) av->incr = -1 ;  /* down  */
         else                           return ;

         if( cbs->event->type == ButtonPress ) av->delay = MCW_AV_longdelay ;
         else                                  av->delay = 0 ;

         av->xev = *(cbs->event) ;  /* copy event for user's info */

         AV_timer_CB( av , &fake_id ) ; /* do the work */
   }
   return;
}

/*------------------------------------------------------------------------*/

void AV_timer_CB( XtPointer client_data , XtIntervalId * id )
{
   MCW_arrowval * av = (MCW_arrowval *) client_data ;
   int newval ;
   double sval ;

   if( av->fstep == 0.0 ){   /* 16 Feb 1999: this is the old way */

      sval = av->fval ;  AV_SHIFT_VAL( -av->decimals , sval ) ;

      if( av->incr < 0 ){
         newval = (int) floor( 0.99 + sval + av->incr ) ;
      } else {
         newval = (int)  ceil(-0.99 + sval + av->incr ) ;
      }

      if( newval > av->imax && av->allow_wrap ){            /* out of range? wrap. */
         newval = av->imin ;
      } else if( newval < av->imin && av->allow_wrap ){
         newval = av->imax ;

      } else if( newval > av->imax || newval < av->imin ){  /* out of range? stop. */
         av->timer_id = 0 ;
         return ;
      }

      AV_assign_ival( av , newval ) ;  /* assign */

   } else {  /* 16 Feb 1999: this is the new way, if user sets fstep */

      if( av->incr > 0 )
         sval = av->fval + av->fstep ;
      else
         sval = av->fval - av->fstep ;

      if( sval > av->fmax || sval < av->fmin ){  /* out of range? stop. */
         av->timer_id = 0 ;
         return ;
      }

      AV_assign_fval( av , sval ) ;
   }

   /* call user callback, if present */

   if( av->dval_CB != NULL && av->fval != av->old_fval )
      av->dval_CB( av , av->dval_data ) ;

   /* delay and then call again, if desired */

   if( av->delay <= 0 ) return ;

   av->timer_id = XtAppAddTimeOut(
                     XtWidgetToApplicationContext( av->wrowcol ) ,
                     av->delay , AV_timer_CB , av ) ;

   if( av->delay == MCW_AV_longdelay )
      if( av->fastdelay > 0 ) av->delay = av->fastdelay ;
      else                    av->delay = MCW_AV_shortdelay ;

   return ;
}

/*------------------------------------------------------------------------*/

void AV_assign_ival( MCW_arrowval * av , int nval )
{
   int newival = nval ;
   char * cval ;

   if( newival > av->imax ) newival = av->imax ;
   if( newival < av->imin ) newival = av->imin ;

   /* assign */

   av->old_ival = av->ival ;
   av->old_fval = av->fval ;

   av->fval = av->ival = newival ;

   /* adjust decimal point */

   AV_SHIFT_VAL( av->decimals , av->fval ) ;

   /* change text display, if present */

   if( av->text_CB != NULL ){
      cval = av->text_CB( av , av->text_data ) ;            /* save   */
      myXtFree( av->old_sval ) ; av->old_sval = av->sval ;  /* string */
      av->sval = XtNewString( cval ) ;                      /* values */

      if( av->wtext != NULL && ! av->block_assign_actions )
         TEXT_SET( av->wtext , cval ) ;
   }

   /* if an option menu, change display */

   if( av->wmenu != NULL && ! av->block_assign_actions ){

      Widget * children , wbut ;
      int  num_children , ic ;

      XtVaGetValues( av->wmenu ,
                        XmNchildren    , &children ,
                        XmNnumChildren , &num_children ,
                     NULL ) ;

      XtVaGetValues( av->wrowcol , XmNmenuHistory , &wbut , NULL ) ;

      ic = newival - av->imin ;

      if( ic >= 0 && ic < num_children && wbut != children[ic] )
         XtVaSetValues( av->wrowcol ,  XmNmenuHistory , children[ic] , NULL ) ;
   }

   return ;
}

/*-------------------------------------------------------------------------
  format a floating value for output
---------------------------------------------------------------------------*/

char * AV_default_text_CB( MCW_arrowval * av , XtPointer junk )
{
   static char buf[32] ;

   AV_fval_to_char( av->fval , buf ) ;
   return &(buf[0]) ;
}

void AV_fval_to_char( float qval , char * buf )
{
   float aval = fabs(qval) ;
   int lv ;
   char lbuf[16] ;
   int il ;

   /* special case if the value is an integer */

   lv = (fabs(qval) < 9999999.0) ? (int)qval : 10000001 ;

   if( qval == lv && abs(lv) < 10000000 ){
      if( lv >= 0 ) sprintf( buf , " %d" , lv ) ;
      else          sprintf( buf , "%d"  , lv ) ;
      return ;
   }

/* macro to strip trailing zeros from output */

#define BSTRIP \
   for( il=AV_NCOL-1 ; il>1 && lbuf[il]=='0' ; il-- ) lbuf[il] = '\0'

   /* noninteger: choose floating format based on magnitude */

   lv = (int) (10.0001 + log10(aval)) ;

   switch( lv ){

      default:
         if( qval > 0.0 ) sprintf( lbuf , "%9.3e" , qval ) ;
         else             sprintf( lbuf , "%9.2e" , qval ) ;
      break ;

      case  6:  /* 0.0001-0.001 */
      case  7:  /* 0.001 -0.01  */
      case  8:  /* 0.01  -0.1   */
      case  9:  /* 0.1   -1     */
      case 10:  /* 1     -9.99  */
         sprintf( lbuf , "%9.6f" , qval ) ; BSTRIP ; break ;

      case 11:  /* 10-99.9 */
         sprintf( lbuf , "%9.5f" , qval ) ; BSTRIP ; break ;

      case 12:  /* 100-999.9 */
         sprintf( lbuf , "%9.4f" , qval ) ; BSTRIP ; break ;

      case 13:  /* 1000-9999.9 */
         sprintf( lbuf , "%9.3f" , qval ) ; BSTRIP ; break ;

      case 14:  /* 10000-99999.9 */
         sprintf( lbuf , "%9.2f" , qval ) ; BSTRIP ; break ;

      case 15:  /* 100000-999999.9 */
         sprintf( lbuf , "%9.1f" , qval ) ; BSTRIP ; break ;

      case 16:  /* 1000000-9999999.9 */
         sprintf( lbuf , "%9.0f" , qval ) ; break ;
   }

   lv = strlen(lbuf) ;                /* length of result at this stage */

   if( lv <= AV_NCOL ){               /* length OK */
      strcpy(buf,lbuf) ;
   } else {                           /* too long (should not occur!) */
      sprintf( lbuf , "%%%d.%dg" , AV_NCOL , AV_NCOL-7 ) ;
      sprintf( buf , lbuf , qval ) ;
   }
   return ;
}

char * AV_format_fval( float fval )
{
   static char buf[32] ;
   AV_fval_to_char( fval , buf ) ;
   return buf ;
}

/*------------------------------------------------------------------------*/

void AV_assign_fval( MCW_arrowval * av , float qval )
{
   double newfval = qval ;
   char * cval ;

   if( newfval > av->fmax ) newfval = av->fmax ;
   if( newfval < av->fmin ) newfval = av->fmin ;

   /* assign */

   av->old_ival = av->ival ;
   av->old_fval = av->fval ;

   av->fval = newfval ;

   /* adjust decimal point */

   AV_SHIFT_VAL( -av->decimals , newfval ) ;

   av->ival = (int) floor(newfval) ;

   /* change text display, if present */

   if( av->text_CB != NULL ){
      cval = av->text_CB( av , av->text_data ) ;            /* save   */
      myXtFree( av->old_sval ) ; av->old_sval = av->sval ;  /* string */
      av->sval = XtNewString( cval ) ;                      /* values */

      if( av->wtext != NULL && ! av->block_assign_actions )
         TEXT_SET( av->wtext , cval ) ;
   }

   return ;
}

/*----------------------------------------------------------------------*/

void AV_leave_EV( Widget w , XtPointer client_data ,
                  XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_arrowval * av       = (MCW_arrowval *) client_data ;
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;

   if( lev->type != LeaveNotify || av == NULL ) return ;

   cbs.reason = XmCR_ACTIVATE ;  /* simulate a return press */
   AV_textact_CB( av->wtext , (XtPointer) av , &cbs ) ;
}

/*----------------------------------------------------------------------*/

void AV_textact_CB( Widget wtex, XtPointer client_data, XtPointer call_data )
{
   MCW_arrowval * av         = (MCW_arrowval *) client_data ;
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) call_data ;

   float sval ;
   int   ii ;
   char * str ;

   if( (cbs->reason != XmCR_ACTIVATE && cbs->reason != XmCR_LOSING_FOCUS )
       || wtex != av->wtext ){
      fprintf(stderr,"\n*** Illegal call to AV_textact_CB ***\n") ;
      return ;
   }

   str = TEXT_GET( wtex ) ;  /* get the new text */

   /* check if new text is any different from last value */

   if( av->sval != NULL && strcmp( av->sval , str ) == 0 ) return ;

   MCW_invert_widget( wtex ) ;  /* start flash */

   ii = sscanf( str , "%f" , &sval ) ;  /* convert to float in sval */

   if( ii == 0 ) sval = av->fval ;  /* bad float conversion */

   AV_assign_fval( av , sval ) ;  /* will alter ival,fval,sval in av */

   if( av->dval_CB != NULL && av->fval != av->old_fval )  /* value changed */
      av->dval_CB( av , av->dval_data ) ;

   myXtFree(str) ;  /* give it back */

   MCW_invert_widget( wtex ) ;  /* end flash */
   return ;
}

/*----------------------------------------------------------------------
   NULL out the pointer to a popup widget when the widget is destroyed
------------------------------------------------------------------------*/

void MCW_destroy_chooser_CB( Widget wpop ,
                             XtPointer client_data, XtPointer call_data )
{
   Widget * wpointer = (Widget *) client_data ;
   *wpointer = NULL ;
}

void MCW_kill_chooser_CB( Widget w ,
                          XtPointer client_data, XtPointer call_data )
{
   Widget wpop = (Widget) client_data ;
   XtDestroyWidget(wpop) ;
}

/*-----------------------------------------------------------------------*/

#undef RECOLOR_OPTMENU

char * MCW_DC_ovcolor_text( MCW_arrowval * av , MCW_DC * dc )
{
   int ii = av->ival ;
   Widget wfix ;

        if( ii < 0                    ) ii = 0 ;
   else if( ii > dc->ovc->ncol_ov - 1 ) ii = dc->ovc->ncol_ov - 1 ;

   wfix = av->wtext ;

   if( wfix != NULL ){
      if( ii > 0 ) MCW_set_widget_bg( wfix , 0 , dc->ovc->pix_ov[ii] ) ;
      else         MCW_set_widget_bg( wfix , "gray40" , 0 ) ;
   }

#ifdef RECOLOR_OPTMENU
   /** make the option menu cascade button gadget be outlined in color **/

   else if( av->wmenu != NULL && XtIsRealized(av->wrowcol) ){
      Pixel ptop , pbot ;
      wfix = av->wrowcol ;
      if( ii > 0 ) pbot = ptop = dc->ovc->pix_ov[ii] ;
      else         XtVaGetValues( XtParent(wfix) ,
                                     XmNtopShadowColor    , &ptop ,
                                     XmNbottomShadowColor , &pbot ,
                                  NULL ) ;
      XtVaSetValues( wfix ,
                        XmNtopShadowColor    , ptop ,
                        XmNbottomShadowColor , pbot ,
                     NULL ) ;
   }
#endif

   return dc->ovc->label_ov[ii] ;
}

/*------------------------------------------------------------------------
   Get a table index for a DC overlay color:
     pops up a shell to let the user make the selection

     wpar     = parent widget (where to popup)
     dc       = display context to choose colors from
     ovc_init = initial overlay color index

     func = routine to call when a selection is made:
             void func( Widget wpar,XtPointer func_data,MCW_choose_cbs * cbs )
     func_data = data to pass to func

     The "ival" stored in the MCW_choose_cbs will be the index into the
     DC overlay color table.  The pixel value is thus dc->ovc->pix_ov[cbs->ival].
     The color name is dc->ovc->name_ov[cbs->ival].  Etc.

     Note that cbs->ival = 0 means that the user choose the "None" color.

   This routine is coded in such a way that only one color chooser will be
   active at a time (per application).  This is a deliberate choice.
--------------------------------------------------------------------------*/

#define OVC_quit_label   "Quit"
#define OVC_apply_label  "Apply"
#define OVC_done_label   "Set"
#define OVC_clear_label  "Clear"

#define OVC_quit_help   "Press to close\nthis `chooser'"
#define OVC_apply_help  "Press to apply\nthis choice and\nkeep this `chooser'"
#define OVC_done_help   "Press to apply\nthis choice and\nclose this `chooser'"
#define OVC_clear_help  "Press to clear\nthe entry"

#define OVC_av_help   "Use arrows to\nsee what choices\nare available"
#define OVC_opt_help  "Click this button\nto pop up a menu\nof overlay colors"

#define OVC_list_help_1 "Click Button 1 on the\n"   \
                        "item of your choice,\n"    \
                        "then press one of the\n"   \
                        "control buttons below.\n"  \
                        "        ** OR **\n"        \
                        "Double-click Button 1\n"   \
                        "on an item to choose it."

#define OVC_list_help_2 "Multiple Mode:\n"               \
                        "  Click Button 1 on items to\n" \
                        "  select or de-select them.\n"  \
                        "\n"                             \
                        "Extended Mode:\n"               \
                        "+ Click and drag Button 1 to\n" \
                        "   select ranges of items.\n"   \
                        "+ Press Ctrl (on keyboard)\n"   \
                        "   at same time to avoid\n"     \
                        "   de-selecting items that\n"   \
                        "   were selected previously."

#define NUM_OVC_ACT 3
#define NUM_CLR_ACT 4

static MCW_action_item OVC_act[] = {
 { OVC_quit_label , MCW_choose_CB, NULL, OVC_quit_help ,"Close window"                 ,0 },
 { OVC_apply_label, MCW_choose_CB, NULL, OVC_apply_help,"Apply choice and keep window" ,0 },
 { OVC_done_label , MCW_choose_CB, NULL, OVC_done_help ,"Apply choice and close window",1 },
 { OVC_clear_label, MCW_choose_CB, NULL, OVC_clear_help,"Clear entry"                  ,0 }
} ;

#define NUM_LIST_MODES 2
static char * list_modes[NUM_LIST_MODES] = { "Multiple" , "Extended" } ;

void MCW_choose_ovcolor( Widget wpar , MCW_DC * dc , int ovc_init ,
                         gen_func * func , XtPointer func_data )
{
   static Widget wpop = NULL , wrc ;
   static MCW_arrowval *  av = NULL ;
   static MCW_choose_data cd ;
   Position xx,yy ;
   int ib ;

   /** destructor callback **/

   if( wpar == NULL ){
      if( wpop != NULL ){
         XtUnmapWidget( wpop ) ;
         XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
         XtDestroyWidget( wpop ) ;
      }
      wpop = NULL ; return ;
   }

   if( ! XtIsRealized(wpar) || dc->ovc->ncol_ov < 2 ){  /* illegal call */
      fprintf(stderr,"\n*** illegal call to MCW_choose_ovcolor: %s %d\n",
             XtName(wpar) , dc->ovc->ncol_ov ) ;
      return ;
   }

   if( ovc_init < 0 || ovc_init >= dc->ovc->ncol_ov ) ovc_init = 1 ;

   /*--- if popup widget already exists, destroy it ---*/

   if( wpop != NULL ){
      XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
      XtDestroyWidget( wpop ) ;
   }

   if( av != NULL ){
       myXtFree( av ) ; av = NULL ;
   }

   /*--- create popup widget ---*/

   wpop = XtVaCreatePopupShell(                        /* Popup Shell */
             "AFNI" , xmDialogShellWidgetClass , wpar ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm(wpar) ){
      XtVaSetValues( wpop ,
                        XmNmwmDecorations , MWM_DECOR_BORDER ,
                        XmNmwmFunctions   ,  MWM_FUNC_MOVE ,
                     NULL ) ;
   }

   XtAddCallback( wpop , XmNdestroyCallback , MCW_destroy_chooser_CB , &wpop ) ;

   XmAddWMProtocolCallback(
        wpop ,
        XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
        MCW_kill_chooser_CB , wpop ) ;

   wrc  = XtVaCreateWidget(                    /* RowColumn to hold all */
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking      , XmPACK_TIGHT ,
                XmNorientation  , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   av = new_MCW_colormenu( wrc , "Color " , dc ,
                           0 , dc->ovc->ncol_ov - 1 , ovc_init , NULL,NULL ) ;
   MCW_reghelp_children( av->wrowcol , OVC_opt_help ) ;
   MCW_reghint_children( av->wrowcol , "Overlay colors" ) ;

   cd.dc      = dc ;    /* data to be passed to pushbutton callback */
   cd.wpop    = wpop ;
   cd.wcaller = wpar ;
   cd.av      = av ;
   cd.sel_CB  = func ;
   cd.sel_cd  = func_data ;
   cd.ctype   = mcwCT_ovcolor ;

   for( ib=0 ; ib < NUM_OVC_ACT ; ib++ ) OVC_act[ib].data = &cd ;

   (void) MCW_action_area( wrc , OVC_act , NUM_OVC_ACT ) ;

   XtTranslateCoords( wpar , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;

   if( av->wtext != NULL )
      MCW_set_widget_bg( av->wtext , NULL , dc->ovc->pix_ov[ovc_init] ) ; /* after popup */

   return ;
}

/*-------------------------------------------------------------------------
   Get an integer:
     pops up a shell to let the user make the selection

     wpar         = parent widget (where to popup)
     label        = label for chooser
     bot,top,init = integers defining range and initial value
     func         = routine to call when a selection is made:
            void func( Widget wpar,XtPointer func_data,MCW_choose_cbs * cbs )
     func_data    = data to pass to func

     The "ival" stored in the MCW_choose_cbs will be the desired result.

   This routine is coded in such a way that only one chooser will be
   active at a time (per application).  This is a deliberate choice.
---------------------------------------------------------------------------*/

void MCW_choose_integer( Widget wpar , char * label ,
                         int bot , int top , int init ,
                         gen_func * func , XtPointer func_data )
{
   static Widget wpop = NULL , wrc ;
   static MCW_arrowval *  av = NULL ;
   static MCW_choose_data cd ;
   Position xx,yy ;
   int ib ;

   /** destructor callback **/

   if( wpar == NULL ){
      if( wpop != NULL ){
         XtUnmapWidget( wpop ) ;
         XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
         XtDestroyWidget( wpop ) ;
      }
      wpop = NULL ; return ;
   }

   if( ! XtIsRealized(wpar) ){  /* illegal call */
      fprintf(stderr,"\n*** illegal call to MCW_choose_integer: %s\n",
              XtName(wpar) ) ;
      return ;
   }

   /*--- if popup widget already exists, destroy it ---*/

   if( wpop != NULL ){
      XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
      XtDestroyWidget( wpop ) ;
   }

   if( av != NULL ){
      myXtFree( av ) ; av = NULL ;
   }

   /*--- create popup widget ---*/

   wpop = XtVaCreatePopupShell(                           /* Popup Shell */
             "AFNI" , xmDialogShellWidgetClass , wpar ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm(wpar) ){
      XtVaSetValues( wpop ,
                        XmNmwmDecorations , MWM_DECOR_BORDER ,
                        XmNmwmFunctions   ,  MWM_FUNC_MOVE ,
                     NULL ) ;
   }

   XtAddCallback( wpop , XmNdestroyCallback , MCW_destroy_chooser_CB , &wpop ) ;

   XmAddWMProtocolCallback(
        wpop ,
        XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
        MCW_kill_chooser_CB , wpop ) ;

   wrc  = XtVaCreateWidget(                 /* RowColumn to hold all */
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking      , XmPACK_TIGHT ,
                XmNorientation  , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   av = new_MCW_arrowval( wrc ,
                          label , MCW_AV_downup ,  /* selection */
                          bot,top,init ,
                          MCW_AV_edittext , 0 ,
                          NULL , NULL , NULL , NULL ) ;

   av->allow_wrap = 1 ;

   MCW_reghelp_children( av->wrowcol , OVC_av_help ) ;
   MCW_reghint_children( av->wrowcol , "Pick value" ) ;

   cd.wpop    = wpop ;  /* data to be passed to pushbutton callback */
   cd.wcaller = wpar ;
   cd.av      = av ;
   cd.sel_CB  = func ;
   cd.sel_cd  = func_data ;
   cd.ctype   = mcwCT_integer ;

   for( ib=0 ; ib < NUM_OVC_ACT ; ib++ ) OVC_act[ib].data = &cd ;

   (void) MCW_action_area( wrc , OVC_act , NUM_OVC_ACT ) ;

   XtTranslateCoords( wpar , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;

   return ;
}

/*-------------------------------------------------------------------------
                                ^
  Create a new MCW_arrowpad:  < O >
                                v
    parent  = parent Widget

    press_func = pointer to a function that will be called when an arrow or
                   the button at the center is pressed
    press_data =  pointer to data to be passed to press_func;
                     press_func( apad , press_data ) ;
                  where apad is a pointer to the MCW_arrowpad that was hit.

    If press_func is NULL, no callback occurs. You may create the arrowpad and
    later set the callback by apad->action_CB = press_func.
---------------------------------------------------------------------------*/

MCW_arrowpad * new_MCW_arrowpad( Widget parent ,
                                 gen_func * press_func, XtPointer press_data
                               )
{
   MCW_arrowpad * apad ;
   int asizx = 20 , asizy = 20 ;  /* arrow sizes */
   int iar ;

   apad = myXtNew( MCW_arrowpad ) ;

   /*--- form to hold the stuff: everything is tied to rubber positions ---*/

   apad->wform = XtVaCreateWidget(
                    "dialog" , xmFormWidgetClass , parent ,
                       XmNfractionBase , AP_FBASE ,
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , False ,
                    NULL ) ;

   /*--- create the arrowbuttons ---*/

   for( iar = 0 ; iar < 4 ; iar++ ){
      apad->wbut[iar] =
         XtVaCreateManagedWidget(
                  "arrow" , xmArrowButtonWidgetClass , apad->wform ,

                     XmNtopAttachment    , XmATTACH_POSITION ,
                     XmNbottomAttachment , XmATTACH_POSITION ,
                     XmNleftAttachment   , XmATTACH_POSITION ,
                     XmNrightAttachment  , XmATTACH_POSITION ,

                     XmNarrowDirection , AP_but_def[iar].atype ,
                     XmNtopPosition    , AP_but_def[iar].atop ,
                     XmNbottomPosition , AP_but_def[iar].abottom ,
                     XmNleftPosition   , AP_but_def[iar].aleft ,
                     XmNrightPosition  , AP_but_def[iar].aright ,

                     XmNheight , asizy ,
                     XmNwidth  , asizx ,
                     XmNborderWidth , 0 ,

                     XmNinitialResourcesPersistent , False ,
                     XmNtraversalOn , False ,
                  NULL ) ;


      XtAddCallback( apad->wbut[iar], XmNarmCallback   , AP_press_CB, apad ) ;
      XtAddCallback( apad->wbut[iar], XmNdisarmCallback, AP_press_CB, apad ) ;
   }

   /*--- create the pushbutton in the middle ---*/

   apad->wbut[4] = XtVaCreateManagedWidget(
                   "arrow" , xmPushButtonWidgetClass , apad->wform ,

                     XmNtopAttachment    , XmATTACH_POSITION ,
                     XmNbottomAttachment , XmATTACH_POSITION ,
                     XmNleftAttachment   , XmATTACH_POSITION ,
                     XmNrightAttachment  , XmATTACH_POSITION ,

                     XmNtopPosition    , AP_but_def[4].atop ,
                     XmNbottomPosition , AP_but_def[4].abottom ,
                     XmNleftPosition   , AP_but_def[4].aleft ,
                     XmNrightPosition  , AP_but_def[4].aright ,

                     XtVaTypedArg , XmNlabelString , XmRString , " " , 2 ,

                     XmNheight , asizy ,
                     XmNwidth  , asizx ,
                     XmNborderWidth , 0 ,
                     XmNrecomputeSize , False ,

                     XmNinitialResourcesPersistent , False ,
                     XmNtraversalOn , False ,
                  NULL ) ;

   XtAddCallback( apad->wbut[4] , XmNactivateCallback , AP_press_CB , apad ) ;

   XtManageChild( apad->wform ) ;

   apad->action_CB   = press_func ;
   apad->action_data = press_data ;
   apad->fastdelay   = MCW_AV_shortdelay ;  /* default delay on 2nd call */
   apad->count       = 0 ;

   apad->parent = apad->aux = NULL ;
   return apad ;
}

/*-------------------------------------------------------------------------*/

void AP_press_CB( Widget wbut , XtPointer client_data , XtPointer call_data )
{
   MCW_arrowpad * apad               = (MCW_arrowpad *) client_data ;
   XmArrowButtonCallbackStruct * cbs =
             (XmArrowButtonCallbackStruct *) call_data ;

   XtIntervalId fake_id = 0 ;

   switch( cbs->reason ){

      /*--- release of button (will only happen for arrows) ---*/

      default:
      case XmCR_DISARM:  /* time to stop */
         if( apad->timer_id != 0 ) XtRemoveTimeOut( apad->timer_id ) ;
         apad->timer_id = 0 ;
      break ;

      /*--- press of button ---*/

      case XmCR_ARM:        /* arrow press */
      case XmCR_ACTIVATE:{  /* button press */
         int iar ;

         for( iar=0 ; iar < 5 ; iar++ )
            if( wbut == apad->wbut[iar] ) break ;

         if( iar > 4 ) return ;  /* something wrong, exit */

         apad->which_pressed = iar ;
         apad->count         = 0 ;

         if( cbs->reason      == XmCR_ARM &&
             cbs->event->type == ButtonPress ) apad->delay = MCW_AV_longdelay;
         else                                  apad->delay = 0 ;

         apad->xev = *(cbs->event) ;  /* copy event for user's info */

         AP_timer_CB( apad , &fake_id ) ; /* do the work */
      }
   }
   return;
}

/*-------------------------------------------------------------------------*/

void AP_timer_CB( XtPointer client_data , XtIntervalId * id )
{
   MCW_arrowpad * apad = (MCW_arrowpad *) client_data ;

   /* call user callback */

   if( apad->action_CB != NULL )
      apad->action_CB( apad , apad->action_data ) ;

   /* delay and then call again, if desired */

   if( apad->delay <= 0 ) return ;

   (apad->count)++ ;
   if( apad->count > AP_MAXCOUNT ){
      apad->count = 0 ;
      return ;
   }

   apad->timer_id = XtAppAddTimeOut(
                     XtWidgetToApplicationContext( apad->wform ) ,
                     apad->delay , AP_timer_CB , apad ) ;

   if( apad->delay == MCW_AV_longdelay )
      if( apad->fastdelay > 0 ) apad->delay = apad->fastdelay ;
      else                      apad->delay = MCW_AV_shortdelay ;

   return ;
}

/*-------------------------------------------------------------------------
   Get a string:
     pops up a shell to let the user make the selection

     wpar           = parent widget (where to popup)
     label          = label for chooser
     default_string = initial value
     func           = routine to call when a selection is made:
            void func( Widget wpar,XtPointer func_data,MCW_choose_cbs * cbs )
     func_data      = data to pass to func

     The "cval" stored in the MCW_choose_cbs will be the desired result.

   This routine is coded in such a way that only one chooser will be
   active at a time (per application).  This is a deliberate choice.
---------------------------------------------------------------------------*/

void MCW_choose_string( Widget wpar , char * label ,
                        char * default_string ,
                        gen_func * func , XtPointer func_data )
{
   static Widget wpop = NULL , wrc , wtf , wlab ;
   static MCW_choose_data cd ;
   Position xx,yy ;
   int ib , ncol=0 ;

   /** destructor callback **/

   if( wpar == NULL ){
      if( wpop != NULL ){
         XtUnmapWidget( wpop ) ;
         XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
         XtDestroyWidget( wpop ) ;
      }
      wpop = NULL ; return ;
   }

   if( ! XtIsWidget(wpar) ) return ;

   if( ! XtIsRealized(wpar) ){  /* illegal call */
      fprintf(stderr,"\n*** illegal call to MCW_choose_string: %s\n",
              XtName(wpar) ) ;
      return ;
   }

   /*--- if popup widget already exists, destroy it ---*/

   if( wpop != NULL ){
      XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
      XtDestroyWidget( wpop ) ;
   }

   /*--- create popup widget ---*/

   wpop = XtVaCreatePopupShell(                           /* Popup Shell */
             "AFNI" , xmDialogShellWidgetClass , wpar ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm(wpar) ){
      XtVaSetValues( wpop ,
                        XmNmwmDecorations , MWM_DECOR_BORDER ,
                        XmNmwmFunctions   ,  MWM_FUNC_MOVE ,
                     NULL ) ;
   }

   XtAddCallback( wpop , XmNdestroyCallback , MCW_destroy_chooser_CB , &wpop ) ;

   XmAddWMProtocolCallback(
        wpop ,
        XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
        MCW_kill_chooser_CB , wpop ) ;

   wrc  = XtVaCreateWidget(                 /* RowColumn to hold all */
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking      , XmPACK_TIGHT ,
                XmNorientation  , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( label != NULL && (ncol=strlen(label)) > 0 )
      wlab = XtVaCreateManagedWidget(
                "menu" , xmLabelWidgetClass , wrc ,
                   LABEL_ARG(label) ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;

   if( default_string != NULL && default_string[0] != '\0' ){
      int qq = strlen(default_string) ;
      if( qq > ncol ) ncol = qq ;
   }
   if( ncol < AV_NCOL ) ncol = AV_NCOL ;

   wtf = XtVaCreateManagedWidget(
             "menu" , TEXT_CLASS , wrc ,

                 XmNcolumns         , ncol ,
                 XmNeditable        , True ,
                 XmNmaxLength       , ncol+4 ,
                 XmNresizeWidth     , False ,

                 XmNmarginHeight    , 1 ,
                 XmNmarginWidth     , 1 ,

                 XmNcursorPositionVisible , True ,
                 XmNblinkRate , 0 ,
                 XmNautoShowCursorPosition , True ,

                 XmNinitialResourcesPersistent , False ,
                 XmNtraversalOn , False ,
              NULL ) ;

   if( default_string != NULL && default_string[0] != '\0' ){
      int qq  = strlen(default_string) , ii ;
      for( ii=0 ; ii < qq ; ii++ ) if( default_string[ii] != ' ' ) break ;
      if( ii < qq ) TEXT_SET( wtf , default_string ) ;
   }

   cd.wpop    = wpop ;  /* data to be passed to pushbutton callback */
   cd.wcaller = wpar ;
   cd.wchoice = wtf ;
   cd.sel_CB  = func ;
   cd.sel_cd  = func_data ;
   cd.ctype   = mcwCT_string ;

   XtAddCallback( wtf, XmNactivateCallback, MCW_choose_CB, &cd ) ; /* return key */

   for( ib=0 ; ib < NUM_CLR_ACT ; ib++ ) OVC_act[ib].data = &cd ;

   (void) MCW_action_area( wrc , OVC_act , NUM_CLR_ACT ) ;

   XtTranslateCoords( wpar , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;

   return ;
}

/*-------------------------------------------------------------------------*/

static int list_max = -1 , list_maxmax ;

static void MCW_set_listmax( Widget wpar )
{
   if( list_max < 0 ){
#if 0
      char * xdef = XGetDefault( XtDisplay(wpar) , "AFNI" , "chooser_listmax" ) ;
#else
      char * xdef = RWC_getname( XtDisplay(wpar) , "chooser_listmax" ) ;
#endif
      if( xdef != NULL ) list_max = strtol( xdef , NULL , 10 ) ;
      if( list_max <= 1 ) list_max = LIST_MAX ;
      list_maxmax = list_max + 5 ;
   }
   return ;
}

/*-------------------------------------------------------------------------
   Get an integer, as an index to an array of strings:
     pops up a shell to let the user make the selection, cycling through
     the string array

     wpar         = parent widget (where to popup)
     label        = label for chooser
     num_str      = number of strings
     init         = index of initial string
     strlist      = array of char *, pointing to strings
     func         = routine to call when a selection is made:
            void func( Widget wpar,XtPointer func_data,MCW_choose_cbs * cbs )
     func_data    = data to pass to func

     The "ival" stored in the MCW_choose_cbs will be the desired result
       (from 0 to num_str-1).

   This routine is coded in such a way that only one chooser will be
   active at a time (per application).  This is a deliberate choice.
---------------------------------------------------------------------------*/

void MCW_choose_strlist( Widget wpar , char * label ,
                         int num_str , int init , char * strlist[] ,
                         gen_func * func , XtPointer func_data )
{
   int initar[2] ;
   initar[0] = init ;
   initar[1] = -666 ;

   MCW_choose_multi_strlist( wpar , label , mcwCT_single_mode ,
                             num_str , initar , strlist , func , func_data ) ;
   return ;
}

void MCW_choose_multi_strlist( Widget wpar , char * label , int mode ,
                               int num_str , int * init , char * strlist[] ,
                               gen_func * func , XtPointer func_data )
{
   static Widget wpop = NULL , wrc ;
   static MCW_choose_data cd ;
   Position xx,yy ;
   int ib , ll , ltop ;
   Widget wlist = NULL , wlab ;
   XmStringTable xmstr ;
   XmString xms ;
   char * lbuf ;
   int nvisible ;

   /** destructor callback **/

   if( wpar == NULL ){
      if( wpop != NULL ){
         XtUnmapWidget( wpop ) ;
         XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
         XtDestroyWidget( wpop ) ;
      }
      wpop = NULL ; return ;
   }

   if( ! XtIsRealized(wpar) ){  /* illegal call */
      fprintf(stderr,"\n*** illegal call to MCW_choose_strlist %s\n",
              XtName(wpar) ) ;
      return ;
   }

   MCW_set_listmax( wpar ) ;

   /*--- if popup widget already exists, destroy it ---*/

   if( wpop != NULL ){
      XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
      XtDestroyWidget( wpop ) ;
   }

   wlist = NULL ;

   /*--- create popup widget ---*/

   wpop = XtVaCreatePopupShell(                           /* Popup Shell */
             "AFNI" , xmDialogShellWidgetClass , wpar ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm(wpar) ){
      XtVaSetValues( wpop ,
                        XmNmwmDecorations ,  MWM_DECOR_BORDER ,
                        XmNmwmFunctions   ,  MWM_FUNC_MOVE
                                           | MWM_FUNC_CLOSE ,
                     NULL ) ;
   }

   XtAddCallback( wpop , XmNdestroyCallback , MCW_destroy_chooser_CB , &wpop ) ;

   XmAddWMProtocolCallback(
        wpop ,
        XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
        MCW_kill_chooser_CB , wpop ) ;

   wrc  = XtVaCreateWidget(                 /* RowColumn to hold all */
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( label != NULL ){
      lbuf = XtMalloc( strlen(label) + 32 ) ;
      sprintf( lbuf , "----Choose %s----\n%s" ,
               (mode == mcwCT_single_mode) ? "One" : "One or More" , label ) ;
   } else {
      lbuf = XtMalloc( 32 ) ;
      sprintf( lbuf , "----Choose %s----",
               (mode == mcwCT_single_mode) ? "One" : "One or More" ) ;
   }
   xms = XmStringCreateLtoR( lbuf , XmFONTLIST_DEFAULT_TAG ) ;
   wlab = XtVaCreateManagedWidget(
                "menu" , xmLabelWidgetClass , wrc ,
                   XmNlabelString   , xms  ,
                   XmNalignment     , XmALIGNMENT_CENTER ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;
   myXtFree(lbuf) ; XmStringFree(xms) ;

   (void) XtVaCreateManagedWidget(
            "menu" , xmSeparatorWidgetClass , wrc ,
                XmNseparatorType , XmSHADOW_ETCHED_IN ,
                XmNinitialResourcesPersistent , False ,
            NULL ) ;

   xmstr = (XmStringTable) XtMalloc( num_str * sizeof(XmString *) ) ;
   for( ib=0 ; ib < num_str ; ib++ )
      xmstr[ib] = XmStringCreateSimple(strlist[ib]) ;

   wlist = XmCreateScrolledList( wrc , "menu" , NULL , 0 ) ;

   nvisible = (num_str < list_maxmax ) ? num_str : list_max ;
   XtVaSetValues( wlist ,
                    XmNitems            , xmstr ,
                    XmNitemCount        , num_str ,
                    XmNvisibleItemCount , nvisible ,
                    XmNtraversalOn      , True ,
                    XmNselectionPolicy  , (mode == mcwCT_single_mode)
                                          ? XmSINGLE_SELECT : XmMULTIPLE_SELECT ,
                  NULL ) ;

   if( init != NULL ){
      for( ib=0 ; init[ib] >= 0 && init[ib] < num_str ; ib++ ){
         XmListSelectPos( wlist , init[ib]+1 , False ) ;
      }
      if( ib > 0 && init[ib-1] > nvisible )
         XmListSetBottomPos( wlist , init[ib-1]+1 ) ;
   }

   XtManageChild(wlist) ;

   if( mode == mcwCT_multi_mode ){
      MCW_register_help( wlist , OVC_list_help_2 ) ;
      MCW_register_help( wlab  , OVC_list_help_2 ) ;
   } else {
      MCW_register_help( wlist , OVC_list_help_1 ) ;
      MCW_register_help( wlab  , OVC_list_help_1 ) ;
      XtAddCallback( wlist , XmNdefaultActionCallback , MCW_choose_CB , &cd ) ;
   }

   cd.wchoice = wlist ;
   cd.av      = NULL ;   /* this is NULL --> will use the list widget */

   for( ib=0 ; ib < num_str ; ib++ ) XmStringFree(xmstr[ib]) ;
   myXtFree(xmstr) ;

   cd.wpop    = wpop ;  /* data to be passed to pushbutton callback */
   cd.wcaller = wpar ;
   cd.sel_CB  = func ;
   cd.sel_cd  = func_data ;
   cd.ctype   = mcwCT_integer ;

   for( ib=0 ; ib < NUM_OVC_ACT ; ib++ ) OVC_act[ib].data = &cd ;

   (void) MCW_action_area( wrc , OVC_act , NUM_OVC_ACT ) ;

   if( mode == mcwCT_multi_mode ){
      MCW_arrowval * av ;

      (void) XtVaCreateManagedWidget(
               "menu" , xmSeparatorWidgetClass , wrc ,
                   XmNseparatorType , XmSHADOW_ETCHED_IN ,
                   XmNinitialResourcesPersistent , False ,
               NULL ) ;

      av = new_MCW_optmenu( wrc , "Selection Mode" , 0,NUM_LIST_MODES-1,0,0 ,
                            MCW_list_mode_CB , wlist ,
                            MCW_av_substring_CB , list_modes ) ;

      MCW_reghelp_children( av->wrowcol , OVC_list_help_2 ) ;
      MCW_reghint_children( av->wrowcol , "How list selections work" ) ;
   }

   XtTranslateCoords( wpar , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;

   return ;
}

void MCW_list_mode_CB( MCW_arrowval * av , XtPointer cd )
{
   Widget wlist = (Widget) cd ;

   if( av == NULL || wlist == NULL ) return ;

   XtVaSetValues( wlist ,
                     XmNselectionPolicy ,
                     (av->ival == 0) ? XmMULTIPLE_SELECT
                                     : XmEXTENDED_SELECT ,
                  NULL ) ;
}

/*==================================================================================*/

#define TSC_quit_label   "Quit"
#define TSC_plot_label   "Plot"
#define TSC_apply_label  "Apply"
#define TSC_done_label   "Set"

#define TSC_quit_help   "Press to close\nthis `chooser'"
#define TSC_plot_help   "Press to popup\na graph of the\nselected time series"
#define TSC_apply_help  "Press to apply\nthis choice and\nkeep this `chooser'"
#define TSC_done_help   "Press to apply\nthis choice and\nclose this `chooser'"

#define TSC_list_help_1  OVC_list_help_1

#define NUM_TSC_ACT 4

#undef DONT_USE_COXPLOT  /* for the long-delayed Plot option */

#undef PCODE
#ifdef DONT_USE_COXPLOT
# define PCODE -1
#else
# define PCODE  0
# include "coxplot.h"
#endif

static MCW_action_item TSC_act[] = {
 { TSC_quit_label , MCW_choose_CB, NULL, TSC_quit_help ,"Close window"              , 0 },
 { TSC_plot_label , MCW_choose_CB, NULL, TSC_plot_help ,"Plot data"                 , PCODE },
 { TSC_apply_label, MCW_choose_CB, NULL, TSC_apply_help,"Apply choice, keep window" , 0 },
 { TSC_done_label , MCW_choose_CB, NULL, TSC_done_help ,"Apply choice, close window", 1 }
} ;

#undef PCODE

/*-------------------------------------------------------------------------
   Get a time series (1D MRI_IMAGE *) from an array of such things:
     pops up a shell to let the user make the selection, with a list
     of time series name

     wpar         = parent widget (where to popup)
     label        = label for chooser
     tsarr        = array of time series (1D images)
     init         = index of initial time series to select
     func         = routine to call when choice is made
            void func( Widget wpar,XtPointer func_data,MCW_choose_cbs * cbs )
     func_data    = data to pass to func

     The "ival" stored in the MCW_choose_cbs will the the index of the
       chosen timeseries in tsarr.  The "imval" be the pointer to the
       chosen timeseries itself.  Do NOT mri_free this, since it will just be a
       pointer to the correct entry in tsarr (which should not be modified
       by any other code during the lifetime of this popup!).

   This routine is coded in such a way that only one chooser will be
   active at a time (per application).  This is a deliberate choice.
---------------------------------------------------------------------------*/

void MCW_choose_timeseries( Widget wpar , char * label ,
                            MRI_IMARR * tsarr , int init ,
                            gen_func * func , XtPointer func_data )
{
   static Widget wpop = NULL , wrc ;
   static MCW_choose_data cd ;
   Position xx,yy ;
   int ib , ll , ltop , num_ts , nvisible , xd,yd ;
   Widget wlist = NULL , wlab ;
   XmStringTable xmstr ;
   XmString xms ;
   char * lbuf ;
   char pbuf[256] , qbuf[256] ;
   MRI_IMAGE * tsim ;

   /** destructor callback **/

   if( wpar == NULL ){
      if( wpop != NULL ){
         XtUnmapWidget( wpop ) ;
         XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
         XtDestroyWidget( wpop ) ;
      }
      wpop = NULL ; return ;
   }

   if( ! XtIsRealized(wpar) ){  /* illegal call */
      fprintf(stderr,"\n*** illegal call to MCW_choose_timeseries %s\n",
              XtName(wpar) ) ;
      return ;
   }

   MCW_set_listmax( wpar ) ;

   /*--- if popup widget already exists, destroy it ---*/

   if( wpop != NULL ){
      XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
      XtDestroyWidget( wpop ) ;
   }

   wlist = NULL ;

   /*--- sanity checks ---*/

   if( tsarr == NULL || tsarr->num == 0 ) return ;
   num_ts = tsarr->num ;

#ifdef AFNI_DEBUG
printf("MCW_choose_timeseries: creation with %d choices\n",num_ts) ;
#endif

   /*--- create popup widget ---*/

   wpop = XtVaCreatePopupShell(                           /* Popup Shell */
             "AFNI" , xmDialogShellWidgetClass , wpar ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm(wpar) ){
      XtVaSetValues( wpop ,
                        XmNmwmDecorations ,  MWM_DECOR_BORDER ,
                        XmNmwmFunctions   ,  MWM_FUNC_MOVE
                                           | MWM_FUNC_CLOSE ,
                     NULL ) ;
   }

   XtAddCallback( wpop , XmNdestroyCallback , MCW_destroy_chooser_CB , &wpop ) ;

   XmAddWMProtocolCallback(
        wpop ,
        XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
        MCW_kill_chooser_CB , wpop ) ;

   wrc  = XtVaCreateWidget(                 /* RowColumn to hold all */
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( label != NULL ){
      lbuf = XtMalloc( strlen(label) + 24 ) ;
      sprintf( lbuf , "----Choose One----\n%s" , label ) ;
   } else {
      lbuf = XtMalloc( 24 ) ;
      sprintf( lbuf , "----Choose One----" ) ;
   }
   xms = XmStringCreateLtoR( lbuf , XmFONTLIST_DEFAULT_TAG ) ;
   wlab = XtVaCreateManagedWidget(
                "menu" , xmLabelWidgetClass , wrc ,
                   XmNlabelString   , xms  ,
                   XmNalignment     , XmALIGNMENT_CENTER ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;
   myXtFree(lbuf) ; XmStringFree(xms) ;

   (void) XtVaCreateManagedWidget(
            "menu" , xmSeparatorWidgetClass , wrc ,
                XmNseparatorType , XmSHADOW_ETCHED_IN ,
                XmNinitialResourcesPersistent , False ,
            NULL ) ;

   xmstr = (XmStringTable) XtMalloc( num_ts * sizeof(XmString *) ) ;

   xd = yd = ltop = 1 ;
   for( ib=0 ; ib < num_ts ; ib++ ){
      tsim = IMARR_SUBIMAGE(tsarr,ib) ;
      if( tsim == NULL ){
         strcpy(pbuf,"** NULL series **") ;
      } else {
         if( tsim->name != NULL )
            strcpy(pbuf,IMARR_SUBIMAGE(tsarr,ib)->name) ;
         else
            strcpy(pbuf,"** NO NAME **") ;

         sprintf(qbuf,"%d",tsim->nx) ; ll = strlen(qbuf) ; xd = MAX(xd,ll) ;
         sprintf(qbuf,"%d",tsim->ny) ; ll = strlen(qbuf) ; yd = MAX(yd,ll) ;
      }
      ll = strlen(pbuf) ; ltop = MAX(ltop,ll) ;
   }

   for( ib=0 ; ib < num_ts ; ib++ ){
      tsim = IMARR_SUBIMAGE(tsarr,ib) ;
      if( tsim == NULL ){
         strcpy(qbuf,"** NULL series **") ;
      } else {
         if( tsim->name != NULL )
            strcpy(pbuf,IMARR_SUBIMAGE(tsarr,ib)->name) ;
         else
            strcpy(pbuf,"** NO NAME **") ;

         sprintf(qbuf,"%-*s [%*d x %*d]", ltop,pbuf , xd,tsim->nx , yd,tsim->ny ) ;
      }
      xmstr[ib] = XmStringCreateSimple( qbuf ) ;
   }

   wlist = XmCreateScrolledList( wrc , "menu" , NULL , 0 ) ;

   nvisible = (num_ts < list_maxmax ) ? num_ts : list_max ;
   XtVaSetValues( wlist ,
                    XmNitems            , xmstr ,
                    XmNitemCount        , num_ts ,
                    XmNvisibleItemCount , nvisible ,
                    XmNtraversalOn      , True ,
                    XmNselectionPolicy  , XmSINGLE_SELECT ,
                  NULL ) ;
   if( init >= 0 && init < num_ts ){
      XmListSelectPos( wlist , init+1 , False ) ;
      if( init+1 > nvisible ) XmListSetBottomPos( wlist , init+1 ) ;
   }
   XtManageChild(wlist) ;

   XtAddCallback( wlist , XmNdefaultActionCallback , MCW_choose_CB , &cd ) ;

   MCW_register_help( wlist , TSC_list_help_1 ) ;
   MCW_register_help( wlab  , TSC_list_help_1 ) ;

   cd.tsarr   = tsarr ;
   cd.wchoice = wlist ;
   cd.av      = NULL ;

   for( ib=0 ; ib < num_ts ; ib++ ) XmStringFree(xmstr[ib]) ;
   myXtFree(xmstr) ;

   cd.wpop    = wpop ;  /* data to be passed to pushbutton callback */
   cd.wcaller = wpar ;
   cd.sel_CB  = func ;
   cd.sel_cd  = func_data ;
   cd.ctype   = mcwCT_timeseries ;

   for( ib=0 ; ib < NUM_TSC_ACT ; ib++ ) TSC_act[ib].data = &cd ;

   (void) MCW_action_area( wrc , TSC_act , NUM_TSC_ACT ) ;

   XtTranslateCoords( wpar , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;

   return ;
}

/*-------------------------------------------------------------------------
   Get an integer, as an index to an array of strings:
   * pops up a shell to let the user make the selection, cycling through
     the string array
   * allows the user to add to the string array

     wpar         = parent widget (where to popup)
     label        = label for chooser
     sar          = array of initial strings (see 3ddata.h)
                    [may be changed by the user during operations]
     init         = index of initial string
     func         = routine to call when a selection is made:
            void func( Widget wpar,XtPointer func_data,MCW_choose_cbs * cbs )
     func_data    = data to pass to func

     The "ival" stored in the MCW_choose_cbs will be the desired result.

   This routine is coded in such a way that only one chooser will be
   active at a time (per application).  This is a deliberate choice.
---------------------------------------------------------------------------*/

void MCW_choose_editable_strlist( Widget wpar , char * label ,
                                  THD_string_array * sar ,
                                  int init , gen_func * func , XtPointer func_data )
{
   int initar[2] ;
   initar[0] = init ;
   initar[1] = -666 ;

   MCW_choose_multi_editable_strlist( wpar , label , mcwCT_single_mode ,
                                      sar , initar , func , func_data ) ;
   return ;
}

void MCW_choose_multi_editable_strlist( Widget wpar , char * label , int mode ,
                                        THD_string_array * sar ,
                                        int * init ,
                                        gen_func * func , XtPointer func_data )
{
   static Widget wpop = NULL , wrc , wrc2 ;
   static MCW_choose_data cd ;
   Position xx,yy ;
   int ib , ll , ltop , num_str ;
   Widget wlist = NULL , wlab , wtf , wadd ;
   XmStringTable xmstr ;
   XmString xms ;
   char * lbuf ;
   int nvisible ;

   /** destructor callback **/

   if( wpar == NULL ){
      if( wpop != NULL ){
         XtUnmapWidget( wpop ) ;
         XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
         XtDestroyWidget( wpop ) ;
      }
      wpop = NULL ; return ;
   }

   if( ! XtIsRealized(wpar) ){  /* illegal call */
      fprintf(stderr,"\n*** illegal call to MCW_choose_strlist %s\n",
              XtName(wpar) ) ;
      return ;
   }

   MCW_set_listmax( wpar ) ;

   /*--- if popup widget already exists, destroy it ---*/

   if( wpop != NULL ){
      XtRemoveCallback( wpop, XmNdestroyCallback, MCW_destroy_chooser_CB, &wpop ) ;
      XtDestroyWidget( wpop ) ;
   }

   wlist = NULL ;

   /*--- create popup widget ---*/

   wpop = XtVaCreatePopupShell(                           /* Popup Shell */
             "AFNI" , xmDialogShellWidgetClass , wpar ,
                XmNallowShellResize , True ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm(wpar) ){
      XtVaSetValues( wpop ,
                        XmNmwmDecorations ,  MWM_DECOR_BORDER ,
                        XmNmwmFunctions   ,  MWM_FUNC_MOVE
                                           | MWM_FUNC_CLOSE ,
                     NULL ) ;
   }

   XtAddCallback( wpop , XmNdestroyCallback , MCW_destroy_chooser_CB , &wpop ) ;

   XmAddWMProtocolCallback(
        wpop ,
        XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
        MCW_kill_chooser_CB , wpop ) ;

   /* RowColumn to hold all */

   wrc  = XtVaCreateWidget(
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /* Label at the top */

   if( label != NULL ){
      lbuf = XtMalloc( strlen(label) + 32 ) ;
      sprintf( lbuf , "----Choose %s----\n%s" ,
               (mode == mcwCT_single_mode) ? "One" : "One or More" , label ) ;
   } else {
      lbuf = XtMalloc( 32 ) ;
      sprintf( lbuf , "----Choose %s----",
               (mode == mcwCT_single_mode) ? "One" : "One or More" ) ;
   }
   xms = XmStringCreateLtoR( lbuf , XmFONTLIST_DEFAULT_TAG ) ;
   wlab = XtVaCreateManagedWidget(
                "menu" , xmLabelWidgetClass , wrc ,
                   XmNlabelString   , xms  ,
                   XmNalignment     , XmALIGNMENT_CENTER ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;
   myXtFree(lbuf) ; XmStringFree(xms) ;

   /* Separator line */

   (void) XtVaCreateManagedWidget(
            "menu" , xmSeparatorWidgetClass , wrc ,
                XmNseparatorType , XmSHADOW_ETCHED_IN ,
                XmNinitialResourcesPersistent , False ,
            NULL ) ;

   /* List to choose from */

   wlist = XmCreateScrolledList( wrc , "menu" , NULL , 0 ) ;
   XtVaSetValues( wlist ,
                    XmNtraversalOn      , True ,
                    XmNselectionPolicy  , (mode == mcwCT_single_mode)
                                          ? XmSINGLE_SELECT : XmMULTIPLE_SELECT ,
                  NULL ) ;

   num_str = SARR_NUM(sar) ;

   if( num_str > 0 ){
      xmstr = (XmStringTable) XtMalloc( num_str * sizeof(XmString *) ) ;
      for( ib=0 ; ib < num_str ; ib++ )
         xmstr[ib] = XmStringCreateSimple( SARR_STRING(sar,ib) ) ;

      nvisible = (num_str < list_maxmax ) ? num_str : list_max ;
      XtVaSetValues( wlist ,
                       XmNitems            , xmstr ,
                       XmNitemCount        , num_str ,
                       XmNvisibleItemCount , nvisible ,
                     NULL ) ;

      if( init != NULL ){
         for( ib=0 ; init[ib] >= 0 && init[ib] < num_str ; ib++ ){
            XmListSelectPos( wlist , init[ib]+1 , False ) ;
         }
         if( ib > 0 && init[ib-1] > nvisible )
            XmListSetBottomPos( wlist , init[ib-1]+1 ) ;
      }

      for( ib=0 ; ib < num_str ; ib++ ) XmStringFree(xmstr[ib]) ;
      myXtFree(xmstr) ;
   }

   XtManageChild(wlist) ;

   /* Some help? */

   if( mode == mcwCT_multi_mode ){
      MCW_register_help( wlist , OVC_list_help_2 ) ;
      MCW_register_help( wlab  , OVC_list_help_2 ) ;
   } else {
      MCW_register_help( wlist , OVC_list_help_1 ) ;
      MCW_register_help( wlab  , OVC_list_help_1 ) ;
      XtAddCallback( wlist , XmNdefaultActionCallback , MCW_choose_CB , &cd ) ;
   }

   cd.wchoice = wlist ;
   cd.av      = NULL ;   /* this is NULL --> will use the list widget */

   cd.wpop    = wpop ;  /* data to be passed to pushbutton callback */
   cd.wcaller = wpar ;
   cd.sel_CB  = func ;
   cd.sel_cd  = func_data ;
   cd.ctype   = mcwCT_integer ;
   cd.sar     = sar ;

   /* action buttons */

   for( ib=0 ; ib < NUM_OVC_ACT ; ib++ ) OVC_act[ib].data = &cd ;

   (void) MCW_action_area( wrc , OVC_act , NUM_OVC_ACT ) ;

   /* choosing mode, for multiple selections */

   if( mode == mcwCT_multi_mode ){
      MCW_arrowval * av ;

      (void) XtVaCreateManagedWidget(
               "menu" , xmSeparatorWidgetClass , wrc ,
                   XmNseparatorType , XmSHADOW_ETCHED_IN ,
                   XmNinitialResourcesPersistent , False ,
               NULL ) ;

      av = new_MCW_optmenu( wrc , "Selection Mode" , 0,NUM_LIST_MODES-1,0,0 ,
                            MCW_list_mode_CB , wlist ,
                            MCW_av_substring_CB , list_modes ) ;

      MCW_reghelp_children( av->wrowcol , OVC_list_help_2 ) ;
      MCW_reghint_children( av->wrowcol , "How list selections work" ) ;
   }

   /* Separator line */

   (void) XtVaCreateManagedWidget(
            "menu" , xmSeparatorWidgetClass , wrc ,
                XmNseparatorType , XmSINGLE_LINE ,
                XmNinitialResourcesPersistent , False ,
            NULL ) ;

   /*-- Stuff to string array --*/

   wrc2 = XtVaCreateWidget(                            /* Rowcol for stuff */
             "menu" , xmRowColumnWidgetClass , wrc ,
                XmNpacking      , XmPACK_TIGHT ,
                XmNorientation  , XmHORIZONTAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   wtf = XtVaCreateManagedWidget(                      /* String to add */
             "menu" , TEXT_CLASS , wrc2 ,

                 XmNcolumns         , 24 ,
                 XmNeditable        , True ,
                 XmNmaxLength       , 128 ,
                 XmNresizeWidth     , False ,

                 XmNmarginHeight    , 1 ,
                 XmNmarginWidth     , 1 ,

                 XmNcursorPositionVisible , True ,
                 XmNblinkRate , 0 ,
                 XmNautoShowCursorPosition , True ,

                 XmNinitialResourcesPersistent , False ,
                 XmNtraversalOn , False ,
              NULL ) ;

   xms = XmStringCreateLtoR( "Add" , XmFONTLIST_DEFAULT_TAG ) ;

   wadd = XtVaCreateManagedWidget(                     /* Button to add it */
             "menu" , xmPushButtonWidgetClass , wrc2 ,
                XmNlabelString  , xms ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   XmStringFree(xms) ;

   { static char * redcolor = NULL ;
     if( redcolor == NULL ){ HOTCOLOR(wpar,redcolor) ; }
     MCW_set_widget_bg( wadd , redcolor , 0 ) ;
   }

   XtAddCallback( wadd, XmNactivateCallback, MCW_stradd_CB, &cd ) ;
   XtAddCallback( wtf , XmNactivateCallback, MCW_stradd_CB, &cd ) ;
   cd.wtf = wtf ;

   MCW_reghelp_children( wrc2 , "Type an entry and press Add\n"
                                "or hit Enter to make a new\n"
                                "entry in the chooser list."   ) ;

   MCW_reghint_children( wrc2 , "Enter new item into chooser list" ) ;

   XtManageChild( wrc2 ) ;

   /* make it appear, like magic! */

   XtTranslateCoords( wpar , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;

   return ;
}

/*--------------------------------------------------------------------------*/

void MCW_stradd_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_choose_data * cd = (MCW_choose_data *) client_data ;
   char * nstr = TEXT_GET( cd->wtf ) ;
   int id , nvisible , num_str ;
   XmString xms ;

   if( nstr == NULL || strlen(nstr) == 0 ){ XBell(XtDisplay(w),100); return; }

   /* see if new string is already in the list */

   for( id=0 ; id < SARR_NUM(cd->sar) ; id++ )
      if( strcmp(nstr,SARR_STRING(cd->sar,id)) == 0 ) break ;

   if( id < SARR_NUM(cd->sar) ){ /* found it, so just jump to it in the list */

      XmListSetBottomPos( cd->wchoice , id+1 ) ;      /* put on bottom */
      XmListSelectPos( cd->wchoice , id+1 , False ) ; /* select it */

   } else {                      /* is a new string, so add it to the list */

      ADDTO_SARR( cd->sar , nstr ) ;           /* add to internal list */

      xms = XmStringCreateSimple( nstr ) ;
      XmListAddItem( cd->wchoice , xms , 0 ) ; /* add to List widget */
      XmStringFree(xms) ;

      num_str = SARR_NUM(cd->sar) ;
      nvisible = (num_str < list_maxmax) ? num_str : list_max ;
      XtVaSetValues( cd->wchoice ,
                       XmNvisibleItemCount , nvisible ,
                     NULL ) ;

      XmListSetBottomPos( cd->wchoice , 0 ) ;      /* make sure it is visible */
      XmListSelectPos( cd->wchoice , 0 , False ) ; /* select it */
   }

   myXtFree(nstr) ; return ;
}

/*--------------------------------------------------------------------------*/

#define LIST_DBCLICK_UNKNOWN   -1
#define LIST_DBCLICK_APPLY      1
#define LIST_DBCLICK_DONE       2

void MCW_choose_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_choose_data * cd       = (MCW_choose_data *) client_data ;
   char * wname               = XtName(w) ;
   XmAnyCallbackStruct * icbs = (XmAnyCallbackStruct *) call_data ;

   static MCW_choose_cbs cbs ;  /* to be passed back to user */
   static int list_dbclick_use = LIST_DBCLICK_UNKNOWN ;
   Boolean clear ;

   /*--- set up what to do for list double clicks ---*/

   if( list_dbclick_use == LIST_DBCLICK_UNKNOWN ){
#if 0
      char * xdef = XGetDefault( XtDisplay(w) , "AFNI" , "chooser_doubleclick" ) ;
#else
      char * xdef = RWC_getname( XtDisplay(w) , "chooser_doubleclick" ) ;
#endif
      if( xdef != NULL && strcmp(xdef,OVC_apply_label) == 0 )
         list_dbclick_use = LIST_DBCLICK_APPLY ;
      else
         list_dbclick_use = LIST_DBCLICK_DONE ;
   }

   /*--- branch on type of chooser that called this ---*/

   clear = (strcmp(wname,OVC_clear_label) == 0) ;

   if( clear && cd->ctype != mcwCT_string ){
      XBell( XtDisplay(cd->wpop) , 100 ) ;
      XtPopdown( cd->wpop ) ;
      return ;
   }

   switch( cd->ctype ){

      default:                                   /* error! */
         XBell( XtDisplay(w) , 100 ) ;
         fprintf(stderr,
                 "\n*** unknown choose type=%d from %s\n" , cd->ctype , wname ) ;
         return ;

      case mcwCT_ovcolor:{                       /* color chooser */
         Boolean done , call ;

         done = strcmp(wname,OVC_apply_label) != 0 ;
         call = strcmp(wname,OVC_quit_label)  != 0 ;

         if( done ) XtPopdown( cd->wpop ) ;

         if( call ){
            cbs.reason = mcwCR_ovcolor ;  /* set structure for call to user */
            cbs.event  = icbs->event ;
            cbs.ival   = cd->av->ival ;

            if( !done ) MCW_invert_widget(w) ;              /* flash */
            cd->sel_CB( cd->wcaller , cd->sel_cd , &cbs ) ; /* call user */
            if( !done ) MCW_invert_widget(w) ;              /* flash */
         }
         return ;
      }

      case mcwCT_integer:{                       /* integer chooser */
         Boolean done , call , flash ;

         done  = strcmp(wname,OVC_apply_label) != 0 ;  /* done unless just "Apply" */
         flash = ! done ;                              /* flash if not done */
         call  = strcmp(wname,OVC_quit_label)  != 0 ;  /* call unless just "Quit" */
         if( w == cd->wchoice ){    /* Double click in List */
            done  = (list_dbclick_use == LIST_DBCLICK_DONE) ;
            flash = False ;
            call  = True ;
         }

         if( done ) XtPopdown( cd->wpop ) ;

         if( call ){
            int pos_count=0 , * pos_list=NULL , ib ;
            Boolean any ;

            cbs.reason = mcwCR_integer ;    /* set structure for call to user */
            cbs.event  = icbs->event ;

            if( cd->av != NULL ){           /* chooser was an arrowval */
               cbs.ival   = cd->av->ival ;
               cbs.fval   = cd->av->fval ;  /* 21 Jan 1997 */
               cbs.nilist = 1 ;
               cbs.ilist  = &(cbs.ival) ;

            } else {                        /* chooser was a List widget */
               any = XmListGetSelectedPos( cd->wchoice, &pos_list, &pos_count ) ;
               if( any ){
                  for( ib=0 ; ib < pos_count ; ib++ )  /* List indexes */
                     pos_list[ib]-- ;                  /* start at 1.  */

                  cbs.ival   = pos_list[0] ;           /* holds the first choice */
                  cbs.fval   = cbs.ival ;              /* 21 Jan 1997 */
                  cbs.nilist = pos_count ;             /* number of choices */
                  cbs.ilist  = pos_list ;              /* holds all choices */
               } else {
                  return ;  /* no choice made */
               }
            }

            if( flash ) MCW_invert_widget(w) ;              /* flash */
            cd->sel_CB( cd->wcaller , cd->sel_cd , &cbs ) ; /* call user */
            if( flash ) MCW_invert_widget(w) ;              /* flash */

            myXtFree(pos_list) ;
         }
         return ;
      }

      case mcwCT_string:{                 /* string chooser */
         Boolean done , call , istextf ;

         /* special action: "Clear" button */

         if( clear ){ TEXT_SET( cd->wchoice , "" ) ; return ; }

         /* find out if called by the text field itself */

         istextf = XtIsSubclass( w , TEXT_CLASS ) ;

         if( istextf == False ){                         /* check button names */
            done = strcmp(wname,OVC_apply_label) != 0 ;  /* to decide upon */
            call = strcmp(wname,OVC_quit_label)  != 0 ;  /* correct actions */
         } else {
            done = False ;   /* input from textfield == press "Apply" */
            call = True ;
         }

         if( done ) XtPopdown( cd->wpop ) ;

         if( call ){
            cbs.reason = mcwCR_string ;  /* set structure for call to user */
            cbs.event  = icbs->event ;
            cbs.cval   = TEXT_GET( cd->wchoice ) ;

            if( !done ) MCW_invert_widget(w) ;              /* flash */
            cd->sel_CB( cd->wcaller , cd->sel_cd , &cbs ) ; /* call user */
            if( !done ) MCW_invert_widget(w) ;              /* flash */

            myXtFree( cbs.cval ) ; cbs.cval = NULL ;
         }
         return ;
      }

      case mcwCT_timeseries:{                       /* timeseries chooser */
         Boolean done , call , flash , any , plot ;
         int pos_count , * pos_list ;

#ifdef AFNI_DEBUG
printf("MCW_choose_CB: timeseries choice made\n") ;
#endif

         if( w == cd->wchoice ){  /* choice is from double click in List widget */
            done  = (list_dbclick_use == LIST_DBCLICK_DONE) ;
            flash = False ;
            plot  = False ;
            call  = True ;
         } else {                 /* choice is from control buttons */

            done  = (strcmp(wname,TSC_quit_label) == 0) ||   /* are we done with   */
                    (strcmp(wname,TSC_done_label) == 0)    ; /* this popup widget? */

            flash = ! done ;                                 /* flash if not done */

            call  = (strcmp(wname,TSC_apply_label) == 0) ||  /* do we call the  */
                    (strcmp(wname,TSC_done_label)  == 0)   ; /* user's routine? */

            plot  = (strcmp(wname,TSC_plot_label)  == 0) ;   /* do we plot a graph? */
         }

#ifdef BBOX_DEBUG
printf("MCW_choose_CB: done=%d  call=%d  plot=%d  flash=%d\n",
       (int)done , (int)call , (int)plot , (int)flash ) ;
#endif

         if( done ) XtPopdown( cd->wpop ) ;

         if( call || plot ){  /* must find out what is selected */
            int pos_count , * pos_list , first ;
            MRI_IMAGE * fim ;

#ifdef BBOX_DEBUG
printf("MCW_choose_CB: querying list for choice\n") ;
#endif

            any = XmListGetSelectedPos( cd->wchoice , &pos_list , &pos_count ) ;

#ifdef BBOX_DEBUG
printf("MCW_choose_CB: queryed list for choice\n") ;
#endif

            if( any ){
                first = pos_list[0] - 1 ;                 /* XmList index starts at 1 */
                fim   = IMARR_SUBIMAGE(cd->tsarr,first) ;
                myXtFree(pos_list) ;
            } else {  /* no choice made --> nothing to do! */
                if( plot ) XBell( XtDisplay(w) , 100 ) ;
                return ;
            }

#ifdef BBOX_DEBUG
printf("MCW_choose_CB: choice index = %d\n",first) ;
#endif

            if( call ){
               cbs.reason = mcwCR_timeseries ;  /* set structure for call to user */
               cbs.event  = icbs->event ;
               cbs.ival   = first ;
               cbs.imval  = fim ;

#ifdef BBOX_DEBUG
printf("MCW_choose_CB: calling user supplied routine\n") ;
#endif

               if( flash ) MCW_invert_widget(w) ;              /* flash */
               cd->sel_CB( cd->wcaller , cd->sel_cd , &cbs ) ; /* call user */
               if( flash ) MCW_invert_widget(w) ;              /* flash */
               return ;
            }

            if( plot ){

#ifdef BBOX_DEBUG
printf("MCW_choose_CB: plotting selected timeseries\n") ;
#endif

            /*-- 17 Aug 1998: plotting code (at last!) --*/

#ifdef DONT_USE_COXPLOT
               (void) MCW_popup_message( w , "Plot not yet\nimplemented." ,
                                         MCW_USER_KILL | MCW_TIMER_KILL ) ;
               return ;
#else
               if( fim->kind != MRI_float ){
                  (void) MCW_popup_message( w , "Can't plot\nnon-float data!" ,
                                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
                  return ;
               } else {
                  float ** yar , * far = MRI_FLOAT_PTR(fim) ;
                  char ** nar=NULL ;
                  int jj ;

#undef USE_NAR  /* use labels for each column?  (just to test the code) */

                  yar = (float **) malloc( sizeof(float *) * fim->ny ) ;
                  for( jj=0 ; jj < fim->ny ; jj++ )
                     yar[jj] = far + jj * fim->nx ;

#ifdef USE_NAR
                  nar = (char **)  malloc( sizeof(char * ) * fim->ny ) ;
                  for( jj=0 ; jj < fim->ny ; jj++ ){
                     nar[jj] = (char *) malloc( sizeof(char) * 32 ) ;
                     sprintf(nar[jj],"column %d",jj+1) ;
                  }
#endif
                  plot_ts_lab( XtDisplay(w) ,
                               fim->nx , NULL , fim->ny , yar ,
                               "index" , NULL , fim->name , nar , NULL ) ;

                  if( nar != NULL ){
                     for( jj=0 ; jj < fim->ny ; jj++ ) free(nar[jj]) ;
                     free(nar) ;
                  }
                  free(yar) ;
                  return ;
               }
#endif /* DONT_USE_COXPLOT */
            }

         }
         return ;
      }

   }  /* end of switch on ctype */
}
