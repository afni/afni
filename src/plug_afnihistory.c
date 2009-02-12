#include "afni.h"
#include <time.h>

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to add to an afni_history_*.c file.
  Makes a custom interface.
************************************************************************/

/*---------- prototypes for internal routines ----------*/

char * AHIST_main( PLUGIN_interface * ) ;

static void AHIST_make_widgets(void) ;

static void AHIST_quit_CB   ( Widget , XtPointer , XtPointer ) ;
static void AHIST_save_CB   ( Widget , XtPointer , XtPointer ) ;
static void AHIST_clear_CB  ( Widget , XtPointer , XtPointer ) ;
static void AHIST_show_CB   ( Widget , XtPointer , XtPointer ) ;
static void AHIST_compile_CB( Widget , XtPointer , XtPointer ) ;
static void AHIST_stdout_CB ( Widget , XtPointer , XtPointer ) ;

static PLUGIN_interface *plint = NULL ;

#define TWIDTH  72
#define THEIGHT 19

static char *histfilename = NULL ;  /* from environment variables */
static char *histusername = NULL ;

#define BMARKER "/*=====BELOW THIS LINE=====*/"
#define AMARKER "/*=====ABOVE THIS LINE=====*/"

#define NLEVEL 5
static char *level_labs[NLEVEL] = { "MICRO" , "MINOR" ,
                                    "MAJOR" , "SUPER" , "SUPERDUPER" } ;

#define NTYPE 6
static char *type_labs[NTYPE] = { "TYPE_GENERAL" , "TYPE_NEW_PROG" ,
                                  "TYPE_NEW_OPT" , "TYPE_NEW_ENV"  ,
                                  "TYPE_BUG_FIX" , "TYPE_MODIFY"    } ;

/* don't need to symbolically define the number
   of months, since that's unlikely to change soon */

static char *month_name[12] = { "JAN" , "FEB" , "MAR" , "APR" ,
                                "MAY" , "JUN" , "JUL" , "AUG" ,
                                "SEP" , "OCT" , "NOV" , "DEC"  } ;

/***********************************************************************
   Set up the interface to the user.  Note that we bypass the
   normal interface creation, and simply have the menu selection
   directly call the main function, which will create a custom
   set of interface widgets the first time in.
************************************************************************/

DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   if( ncall > 0 ) return( NULL );  /* at most one interface */

   /*-- check if we can access the defined history file --*/

   histusername = getenv("AFNI_HISTORY_USERNAME") ;
   if( !THD_filename_pure(histusername) ){
     /* INFO_message("Invalid AFNI_HISTORY_USERNAME") ; */
     return( NULL );
   }

   if( !AFNI_yesenv("AFNI_HISTORY_DONTSAVE") ){
     char *cpt=NULL ;
     histfilename = getenv("AFNI_HISTORY_PERSONAL_FILE") ;
     cpt = AFNI_suck_file(histfilename) ;
     if( cpt == NULL ||
         (strstr(cpt,BMARKER) == NULL && strstr(cpt,AMARKER) == NULL) )
       histfilename = NULL ;

     if( cpt != NULL ) free(cpt) ;
   }

   /*-- at this point, histusername and histfilename are setup, so proceed --*/
   plint = PLUTO_new_interface( "++AFNI_History++" , NULL , NULL ,
                                PLUGIN_CALL_IMMEDIATELY , AHIST_main ) ;

   PLUTO_add_hint( plint , "Add to your personal afni_history file" ) ;

   PLUTO_set_butcolor( plint , "lightblue" ) ;

   return( plint );
}

/***************************************************************************
  Will be called from AFNI when user selects from Plugins menu.
****************************************************************************/

/* Interface widgets */

static Widget shell=NULL , topman ;
static Widget quit_pb, save_pb, clear_pb, show_pb, comp_pb, stdout_pb ;
static Widget program_tf , oneline_tf , verbtext_t ;
static MCW_arrowval *level_av , *type_av ;

static int text_width = 0 ;
static int text_height = 0 ;

static MCW_DC * dc ;                 /* display context */
static Three_D_View * im3d ;         /* AFNI controller */
static THD_3dim_dataset * dset ;     /* The dataset!    */
static MCW_idcode         dset_idc ; /* 31 Mar 1999     */

static int editor_open  = 0 ;

char * AHIST_main( PLUGIN_interface * plint )
{
   XmString xstr ;
   int ii ;
   char label[1234] ;

   /*-- sanity checks --*/

   if( ! IM3D_OPEN(plint->im3d) ) return "AFNI Controller\nnot opened?!" ;

   /*-- is already open, so just raise it so the user can see it --*/

   if( editor_open ){
     XtMapWidget(shell) ;
     XRaiseWindow( XtDisplay(shell) , XtWindow(shell) ) ;
     return NULL ;
   }

   im3d = plint->im3d ;  /* save for local re-use */

   /*-- create widgets, first time through --*/

   if( shell == NULL ){
      dc = im3d->dc ;        /* save this too */
      AHIST_make_widgets() ;
      PLUTO_set_topshell( plint , shell ) ;  /* 22 Sep 2000 */
      RWC_visibilize_widget( shell ) ;       /* 27 Sep 2000 */
   }

   sprintf(label,"%s :: %s",
           histusername , (histfilename != NULL) ? histfilename
                                                 : "AFNI History Editor" ) ;
   XtVaSetValues( shell , XmNtitle , label , NULL ) ;

   /*-- pop the widget up --*/

   XtMapWidget(shell) ;
   PLUTO_cursorize(shell) ;

   editor_open = 1 ;      /* editor is now open for business */

   return NULL ;
}

/*------------------------------------------------------------------------
  Make the control popup window for this beast
--------------------------------------------------------------------------*/

/*-- structures defining action buttons (at bottom of popup) --*/

#define NACT 6  /* number of action buttons */

static MCW_action_item AHIST_actor[NACT] = {
 {"Quit",AHIST_quit_CB,NULL,
  "Clear and close this Editor" , "Clear and close" , 1 } ,

 {"Clear form", AHIST_clear_CB,NULL,
  "Clear the text fields" , "Clear text fields" , 0 } ,

 {"Print",AHIST_stdout_CB,NULL,
  "Write history struct\nelement to screen","Print to screen",0} ,

 {"Save history",AHIST_save_CB,NULL,
  "Save edits to disk and Clear" , "Save to disk & Clear",0} ,

 {"afni_history",AHIST_show_CB,NULL,
  "Run afni_history and show output\nDoes not recompile!" , "Run afni_history",0} ,

 {"Compile",AHIST_compile_CB,NULL,
  "Recompile afni_history program" , "Recompile afni_history",0}
} ;

static void AHIST_make_widgets(void)
{
   XmString xstr ;
   Widget twid  ;
   int nact ;

   /*** top level shell for window manager ***/

   shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNiconName          , "History"    , /* label on icon */
           XmNdeleteResponse    , XmDO_NOTHING , /* deletion handled below */
           XmNallowShellResize  , True ,         /* let code resize shell? */
           XmNmappedWhenManaged , False ,        /* must map it manually */
      NULL ) ;

   DC_yokify( shell , dc ) ;

   if( afni48_good )             /* set icon pixmap */
      XtVaSetValues( shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;

   XmAddWMProtocolCallback(      /* make "Close" window menu work */
           shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           AHIST_quit_CB , (XtPointer) plint ) ;

   /*** Vertical rowcol widget to hold all user interface stuff ***/

   topman = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , shell ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , True  ,
             NULL ) ;

   /*** horizontal rowcol to hold top row of controls ***/

   twid = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , topman ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmHORIZONTAL ,
                XmNtraversalOn , True  ,
             NULL ) ;

   /* Program label and textfield */

   xstr = XmStringCreateLtoR( "Program" , XmFONTLIST_DEFAULT_TAG ) ;
   (void)XtVaCreateManagedWidget( "AFNI" , xmLabelWidgetClass , twid ,
                                    XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;
   program_tf = XtVaCreateManagedWidget(
                 "AFNI" , xmTextFieldWidgetClass , twid ,
                     XmNautoShowCursorPosition , True ,
                     XmNeditable               , True ,
                     XmNcursorPositionVisible  , True ,
                     XmNcolumns                , 23   ,
                     XmNmaxLength              , 23   ,
                 NULL ) ;

   /* Level chooser */

   level_av = new_MCW_optmenu( twid , "Level" ,
                               0,NLEVEL-1 , 0,0 , NULL,NULL ,
                               MCW_av_substring_CB , level_labs ) ;

   /* Type chooser */

   type_av = new_MCW_optmenu( twid , "Type" ,
                              0,NTYPE-1 , 0,0 , NULL,NULL ,
                              MCW_av_substring_CB , type_labs ) ;

   XtManageChild(twid) ;  /*** end of top row ***/

   /*** separator for visual neatness ***/

   (void)XtVaCreateManagedWidget(
              "AFNI" , xmSeparatorWidgetClass , topman ,
              XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   /*** One liner textfield (with a label, in a horizontal rowcol) ***/

   twid = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , topman ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmHORIZONTAL ,
                XmNtraversalOn , True  ,
             NULL ) ;

   xstr = XmStringCreateLtoR( "Note" , XmFONTLIST_DEFAULT_TAG ) ;
   (void)XtVaCreateManagedWidget( "AFNI" , xmLabelWidgetClass , twid ,
                                    XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;
   oneline_tf = XtVaCreateManagedWidget(
                 "AFNI" , xmTextFieldWidgetClass , twid ,
                     XmNautoShowCursorPosition , True ,
                     XmNeditable               , True ,
                     XmNcursorPositionVisible  , True ,
                     XmNcolumns                , TWIDTH-5 ,
                     XmNmaxLength              , TWIDTH ,
                 NULL ) ;
   XtManageChild(twid) ;

   /*** separator for visual neatness ***/

   (void)XtVaCreateManagedWidget(
              "AFNI" , xmSeparatorWidgetClass , topman ,
              XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   /*** longer text window for the long note (but not the long line!) ***/

   xstr = XmStringCreateLtoR( "Verbose Text" , XmFONTLIST_DEFAULT_TAG ) ;
   (void)XtVaCreateManagedWidget( "AFNI" , xmLabelWidgetClass , topman ,
                                    XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   verbtext_t = XtVaCreateManagedWidget(
                    "AFNI" , xmTextWidgetClass , topman ,
                     XmNeditMode               , XmMULTI_LINE_EDIT ,
                     XmNautoShowCursorPosition , True ,
                     XmNeditable               , True ,
                     XmNcursorPositionVisible  , True ,
                     XmNcolumns                , TWIDTH  ,
                     XmNrows                   , THEIGHT ,
                     XmNmaxLength              , TWIDTH*THEIGHT*2 ,
                     XmNwordWrap               , True ,
                  NULL ) ;

   /* this old code is left from the Notes plugin,
      in case it ever becomes useful again to know the pixel dimens */
#if 0
   { char cbuf[TWIDTH+8] ; int ii ;
     XmFontList xflist ;

     for( ii=0; ii < TWIDTH+3; ii++ ) cbuf[ii] = 'x' ; cbuf[ii] = '\0' ;
     xstr = XmStringCreateLtoR( cbuf , XmFONTLIST_DEFAULT_TAG ) ;
     XtVaGetValues( verbtext_t , XmNfontList , &xflist , NULL ) ;
     text_width  = XmStringWidth ( xflist , xstr ) + 14 ;
     text_height = XmStringHeight( xflist , xstr ) ;
     XmStringFree( xstr ) ;
     ii = WidthOfScreen(XtScreen(shell)) - 128 ;
     if( text_width > ii ) text_width = ii ;
   }
#endif

   /*** separator for visual neatness ***/

   (void)XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , topman ,
              XmNseparatorType   , XmSINGLE_LINE , NULL ) ;

   /*** a set of action buttons below the line ***/

#if 0
   nact = (THD_is_file("./Makefile") && THD_is_file("./afni_history.c"))
          ? NACT : NACT-1 ;
#else
   nact = NACT -1 ;
#endif

   (void)MCW_action_area( topman , AHIST_actor , nact ) ;

   quit_pb  = (Widget) AHIST_actor[0].data ;
   clear_pb = (Widget) AHIST_actor[1].data ;
   stdout_pb= (Widget) AHIST_actor[2].data ;
   save_pb  = (Widget) AHIST_actor[3].data ;
   show_pb  = (Widget) AHIST_actor[4].data ;
   comp_pb  = (Widget) AHIST_actor[5].data ;  /* not yet implemented */

   if( histfilename == NULL ) XtSetSensitive(save_pb,False) ;

   /*** done ***/


   XtManageChild(topman) ; XtRealizeWidget(shell) ;

   MCW_set_widget_bg(program_tf,"black",0) ;  /* for fun */
   MCW_set_widget_bg(oneline_tf,"black",0) ;
   MCW_set_widget_bg(verbtext_t,"black",0) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for quit button
---------------------------------------------------------------------*/

static void AHIST_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   AHIST_clear_CB(NULL,NULL,NULL) ;
   XtUnmapWidget( shell ) ; editor_open = 0 ;
   return ;
}

/*-------------------------------------------------------------------*/
/* Clip trailing whitespace off the end of the string (in situ).
---------------------------------------------------------------------*/

static void truncate_string( char *str )
{
   int slen , ss ;

   if( str == NULL || *str == '\0' ) return ;
   slen = strlen(str) ;
   for( ss=slen-1 ; ss > 0 && isspace(str[ss]) ; ss-- ) str[ss] = '\0';
   return ;
}

/*-------------------------------------------------------------------*/
/* Break a string into lines of lengths between lbot and ltop
   (in situ).  You probably want to use truncate_string() first.
---------------------------------------------------------------------*/

static void my_breakup_string( char *str , int lbot , int ltop )
{
   int slen , ii , ibot,itop , ldif ;

   if( str == NULL || lbot >= ltop || lbot < 4 ) return ; /* bad inputs */

   slen = strlen(str) ; if( slen <= ltop ) return ; /* already short enough */

   ibot = 0 ; ldif = ltop-lbot+1 ;
   while(1){
     itop = ibot + ltop ;    /* want to treat str[ibot..itop] as a line */

     /* if itop is past end of str, then we am done */

     if( itop >= slen ) return ;

     /* scan forwards to find a newline character before itop; */
     /* if one is present, output the string up to there,     */
     /* and continue again starting after the newline        */

     for( ii=ibot ; ii <= itop ; ii++ ) if( str[ii] == '\n' ) break ;

     if( ii <= itop ){  /* found it, so loop back */
       ibot = ii+1 ; continue ;
     }

     /* scan backwards to find a whitespace character close before itop */

     for( ii=itop ; ii > itop-ldif ; ii-- ) if( isspace(str[ii]) ) break ;

     /* found one before the minimum location:   */
     /* replace it with a newline, and loop back */

      if( ii > itop-ldif ){
        str[ii] = '\n' ; ibot = ii+1 ; continue ;
      }

      /* scan ahead to next whitespace after itop instead */

      for( ii=itop ; ii < slen ; ii++ ) if( isspace(str[ii]) ) break ;

      /* found one, so put a newline there and loop back */

      if( ii < slen ){
        str[ii] = '\n' ; ibot = ii+1 ; continue ;
      }

      /* no more whitespace in entire string ==> give up in disgust */

      return ;
   } /* there is now way out of this loop except a return above ! */
}

/*-------------------------------------------------------------------*/
/* Put a string in quotes, dealing with special characters.
   You should probably call truncate_string() before this.
---------------------------------------------------------------------*/

static char * quotize_string( char *str )
{
   int slen=strlen(str) , qlen , nlin , nquo;
   char *qtr , *spt , *qpt ;

   /* count number of newlines and other special things */

   for( nquo=nlin=0,spt=str ; *spt != '\0' ; spt++ )
          if( *spt == '\n' )                                nlin++ ;
     else if( *spt == '"' || *spt == '\\' || *spt == '\t' ) nquo++ ;

   qlen = 16*(nlin+16) + 4*nquo + slen ;  /* overlong, but who cares? */
   qtr  = (char *)calloc(sizeof(char),qlen) ;   /* will be the output */
   qpt  = qtr ;               /* pointer to where we are adding chars */
   *qpt++ = '"' ;                   /* throw out the first quote mark */

   /* examine each character in input string and output it or something */

   for( spt=str ; *spt != '\0' ; spt++ ){
     if( *spt == '"'  ){
       strcpy(qpt,"\\\"");         qpt = qtr+strlen(qtr);           /* quote */
     } else if( *spt == '\\' ){
       strcpy(qpt,"\\\\");         qpt = qtr+strlen(qtr);       /* backslash */
     } else if( *spt == '\t' ){
       strcpy(qpt,"   ");          qpt = qtr+strlen(qtr);      /* expand tab */
     } else if( *spt == '\n' ){
       strcpy(qpt,"\\n\"\n   \""); qpt = qtr+strlen(qtr);  /* expand newline */
     } else if( !isprint(*spt) ){
       *qpt++ = '?' ;                             /* non-printable character */
     } else {
       *qpt++ = *spt ;                                   /* normal character */
     }
   }

   *qpt = '"' ;  /* close the initial quote */

   return qtr ;
}

/*-------------------------------------------------------------------*/
/* Callback for stdout button
---------------------------------------------------------------------*/

static void AHIST_stdout_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   AHIST_save_CB( w , (XtPointer)1 , call_data ) ;
}

/*-------------------------------------------------------------------
  Callback for save button -- the point of the whole exercise
---------------------------------------------------------------------*/

#define MINLEN 2

static void AHIST_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   char *program_text , *oneline_text , *verbose_text ;
   char err[2048] = " \n ----- Can't save because of errors: -----\n\n" ;
   int nerr=0 , hlen,vlen , iv ;
   char *hdata , *imark , *ndata ;
   int ilev=level_av->ival , ityp=type_av->ival ;
   struct tm *tod ; time_t ttt ; int slen ; char *sstr , *qstr,*zstr ;
   FILE *fp ;
   int dontsave = ( histfilename       == NULL          ||
                    ((int)client_data) == 1             ||
                    AFNI_yesenv("AFNI_HISTORY_DONTSAVE")  ) ;

   /*-- get the strings from the user and process them a little --*/

   program_text = XmTextFieldGetString( program_tf ) ;
   oneline_text = XmTextFieldGetString( oneline_tf ) ;
   verbose_text = XmTextGetString     ( verbtext_t ) ;
   truncate_string(program_text) ;  /* all this stuff */
   truncate_string(oneline_text) ;  /* is done in place */
   truncate_string(verbose_text) ;
   my_breakup_string(verbose_text,TWIDTH/4,TWIDTH) ;

   /*-- check if required strings are present --*/

   if( program_text == NULL || strlen(program_text) < MINLEN ){
     strcat(err,"* Program text is empty or too short\n") ; nerr++ ;
   }
   if( oneline_text == NULL || strlen(oneline_text) < MINLEN ){
     strcat(err,"* Note text is empty or too short\n") ; nerr++ ;
   }
   if( nerr > 0 ){
     (void)MCW_popup_message( w , err , MCW_USER_KILL | MCW_TIMER_KILL ) ;
     XtFree(program_text); XtFree(oneline_text); XtFree(verbose_text);
     return ;
   }

   /*-- get the contents of the user's history file --*/

   if( !dontsave ){
     hdata = AFNI_suck_file(histfilename) ;
     if( hdata == NULL || (hlen=strlen(hdata)) < 16 ){
       strcat(err,"* Can't get data from file\n  ") ;
       strcat(err,histfilename) ; if( hdata != NULL ) free(hdata) ;
       (void)MCW_popup_message( w , err , MCW_USER_KILL | MCW_TIMER_KILL ) ;
       XtFree(program_text); XtFree(oneline_text); XtFree(verbose_text);
       return ;
     }

     /*-- find the marker; we will write just after imark --*/

     imark = strstr(hdata,BMARKER) ;
     if( imark != NULL ){
       imark += strlen(BMARKER) ;
       if( *imark == '\0' ) imark = NULL ;
       else                 imark+= 2 ;
     }
     if( imark == NULL ){
       imark = strstr(hdata,AMARKER) ;
     }
     if( imark == NULL ){  /* should not happen */
       strcat(err,"* Can't find BELOW or ABOVE markers in\n  ") ;
       strcat(err,histfilename) ; free(hdata) ;
       (void)MCW_popup_message( w , err , MCW_USER_KILL | MCW_TIMER_KILL ) ;
       XtFree(program_text); XtFree(oneline_text); XtFree(verbose_text);
       return ;
     }
   }

   /*-- create the output struct entry string --*/

   ttt = time(NULL) ;
   tod = localtime( &ttt ) ;

   vlen = strlen(verbose_text) ;
   slen = strlen(program_text) + strlen(oneline_text) + vlen + 2048 ;
   sstr = malloc(sizeof(char)*slen) ;    /* the output string */
   qstr = quotize_string(oneline_text) ; /* in case the user put */
   zstr = quotize_string(program_text) ; /* something sneaky in these */

   /* everything but the verbose text */

   sprintf(sstr, " { %d , %s , %d , %s , %s , %s , %s ,\n"
                 "   %s ,\n   " ,
           tod->tm_mday , month_name[tod->tm_mon] , 1900+tod->tm_year ,
           histusername , zstr , level_labs[ilev] , type_labs[ityp] , qstr ) ;
   free(zstr) ; free(qstr) ;

   /* the verbose text */

   if( vlen < MINLEN || strcmp(verbose_text,"NULL") == 0 ){
     strcat(sstr,"NULL") ;
   } else {
     qstr = quotize_string(verbose_text) ; strcat(sstr,qstr) ; free(qstr) ;
   }

   strcat(sstr," } ,\n\n") ;  /* the bracing close */

   XtFree(program_text); XtFree(oneline_text); XtFree(verbose_text);

   /*-- print to screen for the user's edificationizing --*/

   fputs("\n",stdout) ;
   fputs(sstr,stdout) ; fflush(stdout) ;

   if( dontsave ){
     if( histfilename != NULL )
       INFO_message("Did NOT write into file %s",histfilename) ;
     free(sstr) ; return ;
   }

   /*-- create output string to replace user's file --*/

   vlen  = strlen(hdata) + strlen(sstr) + 32 ;
   ndata = (char *)calloc(sizeof(char),vlen) ;
   memcpy( ndata , hdata , imark-hdata ) ;
   strcat( ndata , sstr ) ;                 free(sstr) ;
   strcat( ndata , imark ) ;                free(hdata) ;

   sprintf(err,"%s.BAK",histfilename) ;  /* make a backup copy */
   iv = rename( histfilename , err ) ;   /* of user's history */
   if( iv ){
     (void)MCW_popup_message( w , " \n Can't rename history file!\n" ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
     WARNING_message("Did NOT write into file %s",histfilename) ;
   } else {
     fp = fopen(histfilename,"w") ;
     if( fp == NULL ){
       (void)MCW_popup_message( w , " \n Can't open output file!\n" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
       WARNING_message("Did NOT write into file %s",histfilename) ;
     } else {
       fputs(ndata,fp) ; fclose(fp) ;
       INFO_message("Wrote into file %s",histfilename) ;
     }
   }

   AHIST_clear_CB(NULL,NULL,NULL) ; free(ndata) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for clear button
---------------------------------------------------------------------*/

static void AHIST_clear_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   XmTextFieldSetString( oneline_tf , "\0" ) ;
   XmTextFieldSetString( program_tf , "\0" ) ;
   XmTextSetString     ( verbtext_t , "\0" ) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for compile button
---------------------------------------------------------------------*/

static void AHIST_compile_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
  /* doesn't do much */
}

/*-------------------------------------------------------------------
  Callback for show button
---------------------------------------------------------------------*/

#define NBUF 1024  /* buffer size for reading from program output */

static void AHIST_show_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   static char *cmd=NULL ;
   FILE *fp ;
   char buf[NBUF+1] , *all=NULL ;
   int nbuf         , nall=0 ;

   /*-- get the path to the command to run --*/

   if( cmd == NULL ){
     char *pg = THD_find_executable("afni_history") ;
     if( pg == NULL || *pg == '\0' ){
       (void)MCW_popup_message( w ,
                                  " \n"
                                  " Can't find afni_history \n"
                                  " program in your PATH!!! \n" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
       XtSetSensitive(w,False) ; return ;
     }
     cmd = (char *)calloc( sizeof(char) , (64+strlen(pg)) ) ;
     sprintf(cmd,"%s -reverse -past_months 3",pg) ;
   }

   /*-- open a pipe to read from the command --*/

   fp = popen( cmd , "r" );
   if( fp == NULL ){
     (void)MCW_popup_message( w ,
                              " \n"
                              " Can't run afni_history\n"
                              " program for some reason!\n" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
     return ;
   }

   /*-- read the first bunch of data fromt the pipe --*/

   nbuf = fread( buf , 1 , NBUF , fp ) ;
   if( nbuf < 16 || *buf == '\0' ){
     (void)MCW_popup_message( w ,
                              " \n"
                              " afni_history program\n"
                              " fails to give output!\n" ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
     return ;
   }

   /*-- store this initial string in 'all' --*/

   buf[nbuf] = '\0' ; all = strdup(buf) ; nall = strlen(all) ;

   /*-- loop: read buffer, copy into 'all', until nothing left to read --*/

   do{
     nbuf = fread( buf , 1 , NBUF , fp ) ;
     if( nbuf <= 0 ){ pclose(fp); break; }  /* read failed ==> done */
     buf[nbuf] = '\0' ;
     all = realloc( all , nall+nbuf+2 ) ;
     strcat(all,buf) ; nall = strlen(all) ;
   } while(1) ;

   /*-- display results in a window, and exeunt omnes --*/

   (void)new_MCW_textwin( program_tf , all , 1 ) ;

   free(all) ; return ;
}
