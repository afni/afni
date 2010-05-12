/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN
#include "afni.h"

#include "mri_render.h"
#include "mcw_graf.h"
#include "parser.h"

/*========================================================================*/
/*==== Compile this only if plugins are properly enabled in machdep.h ====*/

#ifdef ALLOW_PLUGINS

#define EMPTY_STRING "\0"

#define COLSIZE AV_colsize()

static Widget wtemp ;

/***************************************************************************
   Routines to open and initialize plugins.  These should only
   be called at the very end of AFNI initialization, since they
   will call each plugin's internal initializer function, which
   may make requests to get AFNI data structures.
****************************************************************************/

/*================ dynamic loading of plugins is allowed ==================*/
#ifndef NO_DYNAMIC_LOADING

/*-------------------------------------------------------------------------
   Routine to read in all plugins found in a given directory
---------------------------------------------------------------------------*/

AFNI_plugin_array * PLUG_get_all_plugins( char * dname )
{
   THD_string_array * flist , * rlist ;
   int ir , ii ;
   char * fname , * suff ;
   AFNI_plugin_array * outar ;
   AFNI_plugin       * plin ;

   /*----- sanity check and initialize -----*/

ENTRY("PLUG_get_all_plugins") ;

   if( dname == NULL || strlen(dname) == 0 ) RETURN(NULL) ;
   if( ! THD_is_directory(dname) )           RETURN(NULL) ;

   INIT_PLUGIN_ARRAY( outar ) ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"scanning directory %s",dname) ; STATUS(str) ; }

   /*----- find all filenames -----*/

   flist = THD_get_all_filenames( dname ) ;
   if( flist == NULL || flist->num <= 0 ){
      DESTROY_SARR(flist) ;
      DESTROY_PLUGIN_ARRAY(outar) ;
      RETURN(NULL) ;
   }

   rlist = THD_extract_regular_files( flist ) ;
   DESTROY_SARR(flist) ;
   if( rlist == NULL || rlist->num <= 0 ){
      DESTROY_SARR(rlist) ;
      DESTROY_PLUGIN_ARRAY(outar) ;
      RETURN(NULL) ;
   }

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"%d files to scan",rlist->num) ; STATUS(str) ; }

   /*----- scan thru and find all filenames ending in DYNAMIC_suffix -----*/

   for( ir=0 ; ir < rlist->num ; ir++ ){
      fname = rlist->ar[ir] ; if( fname == NULL ) continue ;
      if( strstr(fname,"plug") == NULL ) continue ;

      suff = strstr(fname,DYNAMIC_suffix) ;
      if( suff != NULL && strlen(suff) == strlen(DYNAMIC_suffix) ){
         plin  = PLUG_read_plugin( fname ) ;
         if( plin != NULL ) ADDTO_PLUGIN_ARRAY( outar , plin ) ;
      }
   }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"directory %s has %d plugins",dname,outar->num) ; STATUS(str) ; }

   DESTROY_SARR(rlist) ;
   if( outar->num == 0 ) DESTROY_PLUGIN_ARRAY(outar) ;

   /* 06 Aug 1999: sort array by seqcodes */

   if( outar != NULL && outar->num > 1 ){
      int iid , qq ; AFNI_plugin * plin ;
      do{ qq = 0 ;
          for( iid=1 ; iid < outar->num ; iid++ )
             if( strcmp(outar->plar[iid-1]->seqcode,
                        outar->plar[iid  ]->seqcode ) > 0 ){

                plin               = outar->plar[iid-1] ;
                outar->plar[iid-1] = outar->plar[iid] ;
                outar->plar[iid]   = plin ;
                qq++ ;
             }
      } while( qq > 0 ) ;
   }

   RETURN(outar) ;
}

/*----------------------------------------------------------------------
   Routine to open and initialize a single plugin
------------------------------------------------------------------------*/

AFNI_plugin * PLUG_read_plugin( char *fname )
{
   AFNI_plugin *plin ;
   PLUGIN_interface *plint=NULL ;
   int nin ;
   static int firsterr=1 ;

   /*----- sanity checks -----*/

ENTRY("PLUG_read_plugin") ;

   if( fname == NULL || strlen(fname) == 0 ) RETURN(NULL) ;
   if( ! THD_is_file(fname) )                RETURN(NULL) ;

   /*----- make space for new plugin -----*/

   plin = (AFNI_plugin *) XtMalloc( sizeof(AFNI_plugin) ) ;
   memset(plin, 0, sizeof(AFNI_plugin)) ; /* 11 Feb 2009 [lesstif patrol] */
   plin->type = AFNI_PLUGIN_TYPE ;

   /*----- copy name into plin structure -----*/

   MCW_strncpy( plin->libname , fname , MAX_PLUGIN_NAME ) ;

   /*----- open the library (we hope) -----*/

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"opening plugin %s" , fname ) ; STATUS(str) ; }

   DYNAMIC_OPEN( fname , plin->libhandle ) ;

STATUS("returned from DYNAMIC_OPEN()") ;

   if( ! ISVALID_DYNAMIC_handle( plin->libhandle ) ){  /* open failed */

      /* 24 May 2001: always print if there is an error */

      char *er ;
      if( firsterr ){fprintf(stderr,"\n"); firsterr=0; }
      fprintf(stderr,"Failed to open plugin %s",fname) ;
      er = (char *)DYNAMIC_ERROR_STRING ;
      if( er != NULL ) fprintf(stderr," -- %s\n",er) ;
      else             fprintf(stderr,"\n") ;

      myXtFree(plin) ; RETURN(NULL) ;
   }

   /* open was good */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"opened library %s with handle %p" , fname,plin->libhandle ) ;
  STATUS(str) ; }

   /*----- find the required symbol -----*/
   /*..... 13 Sep 2001: add _ for stupid Darwin .....*/
   /*..... 30 Oct 2003: remove for OS X 10.3    .....*/

#ifndef NEED_UNDERSCORE
   DYNAMIC_SYMBOL(plin->libhandle, "PLUGIN_init" , plin->libinit_func );
#else
   DYNAMIC_SYMBOL(plin->libhandle,"_PLUGIN_init" , plin->libinit_func );
#endif

   /*----- if symbol not found, complain and kill this plugin -----*/

   if( plin->libinit_func == (vptr_func *) NULL ){
      char *er = (char *)DYNAMIC_ERROR_STRING ;
      if( firsterr ){fprintf(stderr,"\n"); firsterr=0; }
      fprintf(stderr,"plugin %s lacks PLUGIN_init() function\n",fname) ;
      if( er != NULL ) fprintf(stderr," -- %s\n",er) ;
      DYNAMIC_CLOSE( plin->libhandle ) ;
      myXtFree(plin) ;
      RETURN(NULL) ;
   }

   /*----- create interface(s) by calling initialization function -----*/

   plin->interface_count = nin = 0 ;
   plin->interface       = NULL ;

#ifdef AFNI_DEBUG
   MCHECK ;
#else
   MPROBE ;
#endif

   do {
#if 0
      plint = (PLUGIN_interface *) plin->libinit_func( nin ) ;
#else
      AFNI_CALL_VALU_1ARG(plin->libinit_func ,
                          PLUGIN_interface *,plint , int,nin ) ;
#endif
      if( plint == NULL ) break ;

      plin->interface = (PLUGIN_interface **)
                          XtRealloc( (char *) plin->interface ,
                                     sizeof(PLUGIN_interface *) * (nin+1) ) ;

      plin->interface[nin] = plint ;
      if( nin == 0 ) strcpy( plin->seqcode , plint->seqcode ) ;  /* 06 Aug 1999 */
      nin++ ;
   } while( plint != NULL ) ;

   plin->interface_count = nin ;

#if 1
   if( nin > 0 ){                                    /* 01 Nov 1999 */
      char * bcol , benv[256] ;
      int ii = strlen(DYNAMIC_suffix) , jj ;

      strcpy(benv,"AFNI_") ; strcat(benv,THD_trailname(fname,0)) ; /* make */
      jj = strlen(benv) ; benv[jj-ii] = '\0' ;                     /* name */
      strcat(benv,"_butcolor") ;
      bcol = my_getenv(benv) ;                       /* find name */
      if( bcol != NULL ){                            /* if have name: */
         for( ii=0 ; ii < nin ; ii++ ){
            plint = plin->interface[ii] ;
            if( plint->butcolor[0] == '\0' )         /* set color if */
               PLUTO_set_butcolor( plint , bcol ) ;  /* not defined */
         }
      }
   }
#endif

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"library %s created %d interfaces",fname,nin) ; STATUS(str) ; }

   /*----- done -----*/

   RETURN(plin) ;
}

/*--------------------------------------------------------------------
   Routine to read in all plugins in the desired list of directories
   29 Mar 2001: pname = argv[0] = potential program name
----------------------------------------------------------------------*/

#ifdef DARWIN
#include <mach-o/dyld.h>
/* extern unsigned long _dyld_present(void); */
#endif

AFNI_plugin_array * PLUG_get_many_plugins(char *pname)
{
   char * epath , * elocal , * eee ;
   char ename[THD_MAX_NAME] ;
   AFNI_plugin_array * outar , * tmpar ;
   int epos , ll , ii , id ;
   THD_string_array *qlist ; /* 02 Feb 2002 */

   /*----- sanity checks -----*/

ENTRY("PLUG_get_many_plugins") ;

/**
#if defined(DARWIN) && !defined(c_plusplus) && !defined(__cplusplus)
**/
#ifdef DARWIN
   if( _dyld_present() == 0 ) RETURN(NULL) ;  /* 05 Sep 2001: Mac OSX */
#endif

   epath = getenv("AFNI_PLUGINPATH") ;     /* get the path list to read from */

   if( epath == NULL )
     epath = getenv("AFNI_PLUGIN_PATH") ; /* try another name? */

   if( epath == NULL ){
     epath = getenv("PATH") ;              /* try yet another name? */
#if 0
     if( epath != NULL )
       fprintf(stderr,
               "\n++ WARNING: AFNI_PLUGINPATH not set; searching PATH\n") ;
#endif
   }

   if( epath == NULL && pname != NULL && strchr(pname,'/') != NULL ){ /* 29 Mar 2001 */
     char *ep = strdup(pname) ;                                       /* get path    */
     char *tp = THD_trailname(ep,0) ;                                 /* to program  */
     *tp = '\0' ;
     if( strlen(ep) > 0 ) epath = ep ;    /* got some path */
     else                 free(ep) ;      /* got zipperoni */
   }

   if( epath == NULL )                                /* put in a fake path instead? */
     epath = "./ /usr/local/bin /sw/bin /opt/local/bin /Applications/AFNI" ;

   INIT_SARR(qlist) ; /* 02 Feb 2002: list of checked directories */

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = (char *) XtMalloc( sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
     if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

if(PRINT_TRACING)
{ STATUS("paths to be searched for plugins follows:") ;
  printf("%s\n",elocal) ; fflush(stdout) ; }

   /*----- extract blank delimited strings;
           use as directory names to get libraries -----*/

   INIT_PLUGIN_ARRAY( outar ) ;
   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , ename , &id ) ; /* next substring */
      if( ii < 1 || id < 1 ) break ;                      /* none --> end of work */
      epos += id ;                                        /* char after last scanned */

      if( !THD_is_directory(ename) ) continue ;           /* 21 May 2002 -rcr */

      /* 02 Feb 2002: did we check this one already? */

      for( ii=0 ; ii < qlist->num ; ii++ )
         if( THD_equiv_files(qlist->ar[ii],ename) ) break ;
      if( ii < qlist->num ) continue ;
      ADDTO_SARR(qlist,ename) ;

      ii = strlen(ename) ;                                /* make sure name has */
      if( ename[ii-1] != '/' ){                           /* a trailing '/' on it */
          ename[ii]  = '/' ; ename[ii+1] = '\0' ;
      }

      tmpar = PLUG_get_all_plugins( ename ) ;             /* read this directory */
      if( tmpar != NULL ){
         for( ii=0 ; ii < tmpar->num ; ii++ )             /* move results to output */
           ADDTO_PLUGIN_ARRAY( outar , tmpar->plar[ii] ) ;

         FREE_PLUGIN_ARRAY(tmpar) ;                       /* toss temp array */
      }
   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   myXtFree(elocal) ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"found %d plugins",outar->num) ; STATUS(str) ; }

   if( outar->num == 0 ) DESTROY_PLUGIN_ARRAY(outar) ;
   DESTROY_SARR(qlist) ; /* 02 Feb 2002 */
   RETURN(outar) ;
}

/*===================  Plugins are statically linked into AFNI ===================*/

#else  /* NO_DYNAMIC_LOADING is defined (e.g., CYGWIN) */

/* get the list of fixed (compiled-in) plugins */

#include "fixed_plugins.h"

/*-------------------------------------------------------------------------------*/
/*! Function to load one fixed plugin (for CYGWIN) */

AFNI_plugin * PLUG_load_fixed_plugin( FIXED_plugin pin )
{
   AFNI_plugin * plin ;
   PLUGIN_interface * plint ;
   int nin ;

   /*----- sanity checks -----*/

ENTRY("PLUG_load_plugin") ;

   if( pin.pfunc == NULL ) RETURN(NULL) ;

   /*----- make space for new plugin -----*/

   plin = (AFNI_plugin *) XtMalloc( sizeof(AFNI_plugin) ) ;
   memset(plin, 0, sizeof(AFNI_plugin)) ; /* 11 Feb 2009 [lesstif patrol] */
   plin->type = AFNI_PLUGIN_TYPE ;

   /*----- copy name into plin structure -----*/

   MCW_strncpy( plin->libname , pin.pname , MAX_PLUGIN_NAME ) ;
   plin->libinit_func = pin.pfunc ;

   /*----- create interface(s) by calling initialization function -----*/

   plin->interface_count = nin = 0 ;
   plin->interface       = NULL ;

   do {
      plint = (PLUGIN_interface *) plin->libinit_func( nin ) ;
      if( plint == NULL ) break ;

      plin->interface = (PLUGIN_interface **)
                          XtRealloc( (char *) plin->interface ,
                                     sizeof(PLUGIN_interface *) * (nin+1) ) ;

      plin->interface[nin] = plint ;
      if( nin == 0 ) strcpy( plin->seqcode , plint->seqcode ) ;  /* 06 Aug 1999 */
      nin++ ;
   } while( plint != NULL ) ;

   plin->interface_count = nin ;

   /*----- done -----*/

   RETURN(plin) ;
}

/*------------------------------------------------------------------------*/
/*! Function to load all fixed plugins (for CYGWIN).  pname is ignored. */

AFNI_plugin_array * PLUG_get_many_plugins(char *pname)
{
   int ir ;
   AFNI_plugin_array * outar ;
   AFNI_plugin       * plin ;

   /*----- sanity check and initialize -----*/

ENTRY("PLUG_get_many_plugins") ;

   INIT_PLUGIN_ARRAY( outar ) ;

   /*----- scan thru and create plugins from the fixed list -----*/

   for( ir=0 ; ir < NUM_FIXED_plugin_funcs ; ir++ ){
      plin = PLUG_load_fixed_plugin( FIXED_plugin_funcs[ir] ) ;
      if( plin != NULL ) ADDTO_PLUGIN_ARRAY( outar , plin ) ;
   }

   RETURN(outar) ;
}

#endif /* NO_DYNAMIC_LOADING */

/*==============================================================================*/

/****************************************************************************
  Routines to create interface descriptions for new plugins. Usage:
    1) Use "new_PLUGIN_interface" to create the initial data structure.
    2) Use "add_option_to_PLUGIN_interface" to create an option line in the
         AFNI interface menu.  There is no built-in limit to the number
         of option lines that may be added to an AFNI interface menu.
    2(abcdef)
       Use "add_number_to_PLUGIN_interface"  , and
           "add_string_to_PLUGIN_interface"  , and
           "add_dataset_to_PLUGIN_interface" , et cetera,
         to add control parameter choosers to the most recently created
         option line.  Up to 6 choosers may be added to an option line.
    3) When done, return the new "PLUGIN_interface *" to AFNI.
*****************************************************************************/

/*--------------------------------------------------------------------------
   Create a new plugin interface.

   label       = C string to go on the button that activates this interface
                   (will be truncated to 15 characters)

   description = C string to go on the interface control panel popped-up
                   when the button above is pressed -- this has no
                   meaning for call_type = PLUGIN_CALL_IMMEDIATELY

   help        = C string to be popped up if the user presses "Help" on
                   the interface control panel -- this has no
                   meaning for call_type = PLUGIN_CALL_IMMEDIATELY.
                   If this is NULL, then there will be no help available.

   call_type   = int that describes how the plugin is to be called from AFNI:
                   PLUGIN_CALL_IMMEDIATELY means to call APL_main as soon
                     as the activating button is pressed;
                   PLUGIN_CALL_VIA_MENU means to have AFNI popup a menu
                     to control the input parameters passed to APL_main.

   call_func   = routine that AFNI should call when the user activates
                   this plugin.  The routine will be passed the the
                   pointer "PLUGIN_interface *" created herein, which
                   can be interrogated with the get_*_from_PLUGIN_interface
                   routines.  The call_func should return a "char *", which
                   is NULL if everything is OK, and points to an error
                   message that AFNI will display if something bad happened.

   The value returned is the pointer to the new interface struct.

   Note that the three input strings are copied by AFNI, and so could
   be freed after this routine returns.
----------------------------------------------------------------------------*/

PLUGIN_interface * new_PLUGIN_interface( char * label , char * description ,
                                         char * help ,
                                         int call_type , cptr_func * call_func )
{
   PLUGIN_interface * plint ;

ENTRY("new_PLUGIN_interface") ;

   plint = new_PLUGIN_interface_1999( label , description , help ,
                                      call_type , call_func , NULL ) ;
   RETURN(plint) ;
}

/**** 15 Jun 1999: modified to crosscheck compilation dates ****/

#include <time.h>

PLUGIN_interface * new_PLUGIN_interface_1999( char * label , char * description ,
                                              char * help ,
                                              int call_type , cptr_func * call_func ,
                                              char * compile_date )
{
   PLUGIN_interface * plint ;
   static int num_date_err = 0 ;

   /*-- sanity check --*/

ENTRY("new_PLUGIN_interface_1999") ;

   if( label == NULL || strlen(label) == 0 ) RETURN(NULL) ;

   if( !( (call_type == PLUGIN_CALL_IMMEDIATELY) ||
          (call_type == PLUGIN_CALL_VIA_MENU   )
        ) ) RETURN(NULL) ;

   if( call_func == (cptr_func *) NULL ) RETURN(NULL) ;

   /*-- create new interface --*/

   plint = (PLUGIN_interface *) XtMalloc(sizeof(PLUGIN_interface)) ;
   if( plint == NULL ) RETURN(NULL) ;
   memset(plint, 0, sizeof(PLUGIN_interface)) ; /* 11 Feb 2009 [LPatrol] */

   plint->flags = 0 ;  /* 29 Mar 2002 */

   MCW_strncpy( plint->label , label , PLUGIN_LABEL_SIZE ) ;

   if( description != NULL )
      MCW_strncpy( plint->description , description , PLUGIN_STRING_SIZE ) ;
   else
      MCW_strncpy( plint->description , label , PLUGIN_STRING_SIZE ) ;

   plint->call_method  = call_type ;
   plint->call_func    = call_func ;
   plint->option_count = 0 ;
   plint->option       = NULL ;
   plint->wid          = NULL ;
   plint->im3d         = NULL ;
   plint->hint         = NULL ;

   if( help == NULL || strlen(help) == 0 )
      plint->helpstring = NULL ;
   else
      plint->helpstring = XtNewString( help ) ;

   strcpy( plint->seqcode , "zzzzzzz" ) ; /* 06 Aug 1999 */
   strcpy( plint->butcolor, "\0" ) ;      /* 01 Nov 1999 */

   /** 15 Jun 1999 stuff for date checking **/

#ifndef DONT_USE_STRPTIME
   if( compile_date == NULL ){

      if( num_date_err == 0 ) fprintf(stderr,"\n") ;
      fprintf(stderr,
              "*** Warning: Plugin %-15s was compiled with an earlier version of AFNI\n",
              label ) ;
      num_date_err++ ;

#if 0
#  define AFNI_DATE "Jun 17 1999"  /* for testing purposes */
#else
#  define AFNI_DATE __DATE__
#endif

   } else {
      struct tm compile_tm  ={0} , date_tm={0} ;
      time_t    compile_time     , date_time   ;
      double    date_minus_compile ;

      strptime( compile_date , "%b %d %Y" , &compile_tm ) ; compile_time = mktime( &compile_tm ) ;
      strptime( AFNI_DATE    , "%b %d %Y" , &date_tm    ) ; date_time    = mktime( &date_tm    ) ;
      date_minus_compile = difftime( date_time , compile_time ) ;

      if( date_minus_compile > 3600.0 ){
         if( num_date_err == 0 ) fprintf(stderr,"\n") ;
         fprintf(stderr,
                 "\n*** Warning: Plugin %-15s compile date=%s predates AFNI=%s",
                 label , compile_date , AFNI_DATE ) ;
         num_date_err++ ;
      } else if( PRINT_TRACING ){
         char str[256] ;
         sprintf(str,"Plugin %-15s compile date=%s  AFNI date=%s  difftime=%g\n",
                 label , compile_date , AFNI_DATE , date_minus_compile ) ;
         STATUS(str) ;
      }
   }
#endif

   plint->run_label[0]  = '\0' ;  /* 04 Nov 2003 */
   plint->doit_label[0] = '\0' ;

   RETURN(plint) ;
}

/*------------------------------------------------------------------------*/
/*! Set the "Run+Keep" and "Run+Close" labels for a plugin. [04 Nov 2003] */
/*------------------------------------------------------------------------*/

void PLUTO_set_runlabels( PLUGIN_interface *plint , char *rlab , char *dlab )
{
   if( plint == NULL ) return ;
   if( rlab != NULL  ) MCW_strncpy( plint->run_label , rlab, PLUGIN_LABEL_SIZE );
   if( dlab != NULL  ) MCW_strncpy( plint->doit_label, dlab, PLUGIN_LABEL_SIZE );
   return ;
}

/*----------------------------------------------------------------------
  Set the seqcode in a plugin, for sorting in the interface.
  [06 Aug 1999]
------------------------------------------------------------------------*/

void PLUTO_set_sequence( PLUGIN_interface * plint , char * sq )
{
ENTRY("PLUTO_set_sequence") ;
   if( plint == NULL || sq == NULL || sq[0] == '\0' ) EXRETURN ;
   MCW_strncpy( plint->seqcode , sq , PLUGIN_STRING_SIZE ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
  Set the button color in a plugin [01 Nov 1999]
------------------------------------------------------------------------*/

void PLUTO_set_butcolor( PLUGIN_interface * plint , char * sq )
{
ENTRY("PLUTO_set_butcolor") ;
   if( plint == NULL || sq == NULL || sq[0] == '\0' ) EXRETURN ;
   if( strncmp(sq,"hot",3) == 0 ) sq = MCW_hotcolor(NULL) ;
   MCW_strncpy( plint->butcolor , sq , PLUGIN_STRING_SIZE ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Routine to add a new option line to a plugin interface menu.

   plint      = PLUGIN_interface * which will have the option added
   label     = C string to be displayed in the menu describing this option
   tag       = C string to be passed to the plugin when this option is used
   mandatory = TRUE  (1)  if this option is always passed to the plugin
               FALSE (0)  if the user may or may not select this option
               MAYBE (-1) if it is to be turned on, but it can be turned off
-----------------------------------------------------------------------------*/

void add_option_to_PLUGIN_interface( PLUGIN_interface * plint ,
                                     char * label , char * tag , int mandatory )
{
   int nopt , isv ;
   PLUGIN_option * opt ;

ENTRY("add_option_to_PLUGIN_interface") ;

   /*-- sanity check --*/

   if( plint == NULL ) EXRETURN ;
   if( plint->call_method == PLUGIN_CALL_IMMEDIATELY ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;
   if( tag   == NULL ) tag   = EMPTY_STRING ;

   /*-- create space for new option --*/

   nopt = plint->option_count ;
   plint->option = (PLUGIN_option **)
                     XtRealloc( (char *) plint->option ,
                                sizeof(PLUGIN_option *) * (nopt+1) ) ;

   plint->option[nopt] = opt = (PLUGIN_option *)XtMalloc(sizeof(PLUGIN_option));
   memset(opt, 0, sizeof(PLUGIN_option)) ; /* 11 Feb 2009 [lesstif patrol] */

   /*-- put values in new option --*/

   PLUGIN_LABEL_strcpy( opt->label , label ) ;
   MCW_strncpy( opt->tag , tag , PLUGIN_STRING_SIZE ) ;

   opt->mandatory      = mandatory ;
   opt->subvalue_count = 0 ;

   opt->hint = NULL ;
   for( isv=0 ; isv < PLUGIN_MAX_SUBVALUES ; isv++ )
      opt->subvalue[isv].hint = NULL ;

   (plint->option_count)++ ;  /* one more option */

#if 0
{int qq; fprintf(stderr,"Option tags thus far:\n");
 for(qq=0;qq<plint->option_count;qq++)
   fprintf(stderr," %s",plint->option[qq]->tag) ;
 fprintf(stderr,"\n"); }
#endif

   EXRETURN ;
}

/*-------------------------------------------------------------------------
  Add a hint to the most recently created interface item
---------------------------------------------------------------------------*/

void PLUTO_add_hint( PLUGIN_interface * plint , char * hh )
{
   int nopt , nsv ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

ENTRY("PLUTO_add_hint") ;

   if( plint == NULL || hh == NULL ) EXRETURN ;

   nopt = plint->option_count - 1 ;
   if( nopt < 0 ){                  /* no options yet, so hint is global */
      myXtFree(plint->hint) ;
      plint->hint = XtNewString(hh) ;
      EXRETURN ;
   }

   opt  = plint->option[nopt] ;     /* latest option line */
   nsv  = opt->subvalue_count ;     /* number of subvalues on it */

   if( nsv == 0 ){                  /* no subvalues yet */
      myXtFree(opt->hint) ;         /* so put hint on the option line */
      opt->hint = XtNewString(hh) ;
   } else {                         /* add hint to last subvalue */
      sv = &(opt->subvalue[nsv-1]) ;
      myXtFree(sv->hint) ;
      sv->hint = XtNewString(hh) ;

#if 0
if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"%s: %s hint=%s",
                          plint->label,sv->label,sv->hint) ; STATUS(str) ; }
#endif

   }

   EXRETURN ;
}

/*-------------------------------------------------------------------------
   Routine to add a number-type "chooser" to the most recently created
   option within a plugin interface.  "Numbers" are always passed to
   the plugin in float format, but are specified here using an
   integer range with a decimal shift, sort of like the Motif Scale Widget.

   label     = C string to go in the menu, next to the "chooser" for
               the integer.

   bot, top, = Smallest and largest integer values allowed in the chooser.
   decim     = Number of decimals to shift to left for display of value.
               For example, bot=1 top=100 with decim=2 will actually
               specify a range of 0.01 to 1.00.  One function of decim
               is simply to set the increments between values in the
               chooser.  With decim=2, the increment is 0.01.

   defval    = Integer value for the default.  For example, if decim=2,
               and defval=43, then the floating point default value
               (as it appears to the user) is actually 0.43.

   editable  = TRUE (1) if the user will be allowed to type in any
                 value in the chooser.  For example, this would allow
                 the input of 0.43721.
               FALSE (0) if the user is restricted to the range of
                 values given by bot/10**decim to top/10**decim,
                 in steps of 1/10**decim.
---------------------------------------------------------------------------*/

void add_number_to_PLUGIN_interface( PLUGIN_interface * plint ,
                                     char * label ,
                                     int bot , int top , int decim ,
                                     int defval , int editable     )
{
   int nopt , nsv ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

   /*-- sanity check --*/

ENTRY("add_number_to_PLUGIN_interface") ;

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;

   nopt = plint->option_count - 1 ;
   opt  = plint->option[nopt] ;

   nsv = opt->subvalue_count ;
   if( nsv == PLUGIN_MAX_SUBVALUES ){
      fprintf(stderr,"*** Warning: maximum plugin subvalue count exceeded!\n");
      EXRETURN ;
   }

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_NUMBER_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   sv->int_range_bot   = bot ;
   sv->int_range_top   = top ;
   sv->int_range_decim = decim ;
   sv->value_default   = defval ;
   sv->editable        = editable ;

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

/*-------------------------------------------------------------------
   Routine to add a string-type "chooser" to the most recently created
   option within a plugin interface.

   label     = C string to go in the menu, next to the "chooser" for
               the string.

   num_str   = Count of the number of strings provided in strlist.
   strlist   = strlist[i] is a pointer to the i'th string value
               that the user is to choose from, for i=0...num_str-1.

   defval    = If num_str > 0:
                 Integer from 0...num_str-1 indicating which string
                 in strlist is the default value.
               If num_str == 0:
                 Gives width of field supplied for string input.

   Note that if num_str is <= 0, then instead of being presented with
   a menu of fixed strings, the user will have to type in a string.
---------------------------------------------------------------------*/

void add_string_to_PLUGIN_interface( PLUGIN_interface * plint ,
                                     char * label ,
                                     int num_str , char ** strlist ,
                                     int defval )
{
   int nopt , nsv , ii ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

ENTRY("add_string_to_PLUGIN_interface") ;

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;

   nopt = plint->option_count - 1 ;
   opt  = plint->option[nopt] ;

   nsv = opt->subvalue_count ;
   if( nsv == PLUGIN_MAX_SUBVALUES ){
      fprintf(stderr,"*** Warning: maximum plugin subvalue count exceeded!\n");
      EXRETURN ;
   }

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_STRING_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   if( num_str > 0 ){
      sv->string_range_count = num_str ;
      for( ii=0 ; ii < num_str ; ii++ ){
         sv->string_range[ii] = (char*)XtMalloc( PLUGIN_STRING_SIZE ) ;
         MCW_strncpy( sv->string_range[ii] , strlist[ii] , PLUGIN_STRING_SIZE ) ;
      }
      sv->value_default   = defval ;
      sv->editable        = FALSE ;
   } else {
      sv->string_range_count = 0 ;
      sv->editable           = TRUE ;
      sv->value_default      = defval ;

      if( strlist != NULL && strlist[0] != NULL ){     /* 19 Jun 2000 */
         sv->string_range_count = -1 ;
         sv->string_range[0] = (char*)XtMalloc( PLUGIN_STRING_SIZE ) ;
         MCW_strncpy( sv->string_range[0], strlist[0], PLUGIN_STRING_SIZE ) ;
      }
   }

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Routine to add a dataset "chooser" to the most recently created
   option within a plugin interface.

   label = C string to go in the menu, next to the "chooser" for
           the dataset.

   The _mask inputs are bitwise ORs (|) of dataset type masks.
   These are used to specify the types of datasets that can be
   passed to the plugin.  The first two _mask inputs below cannot both
   be zero (for then no dataset would be allowed into the plugin!).

   anat_mask = Chosen from the list in 3ddata.h, which is currently
                   ANAT_SPGR_MASK   ANAT_FSE_MASK   ANAT_EPI_MASK
                   ANAT_MRAN_MASK   ANAT_CT_MASK    ANAT_SPECT_MASK
                   ANAT_PET_MASK    ANAT_MRA_MASK   ANAT_BMAP_MASK
                   ANAT_DIFF_MASK   ANAT_OMRI_MASK
               and ANAT_ALL_MASK, which will allow any anatomical
               dataset.  Entering 0 for this mask will mean that
               no anatomical datasets will be choosable.

   func_mask = Similar mask for functional dataset types, chosen from
                  FUNC_FIM_MASK   FUNC_THR_MASK
                  FUNC_COR_MASK   FUNC_TT_MASK
               and FUNC_ALL_MASK, which will allow any functional
               dataset.  Entering 0 for this mask will mean that
               no functional datasets will be choosable.

   ctrl_mask = An additional mask to specify further exactly which
                 datasets should be choosable.  Mask options are:

               SESSION_ALL_MASK    = If this is set, then the choice of
                                     datasets will be drawn from all
                                     sessions now loaded into AFNI.
                                   * By default, only the "current"
                                     session will be included.

               DIMEN_3D_MASK       = Masks that define whether 3D and/or
               DIMEN_4D_MASK         3D+time (4D) datasets are allowable.
               DIMEN_ALL_MASK

               WARP_ON_DEMAND_MASK = If this is set, then datasets that may
                                     not have a BRIK file will be included
                                     in the list of datasets to choose from.
                                     In this case, the plugin must be ready
                                     to deal with the warp-on-demand routines
                                     that return one slice at a time.
                                   * By default, only datasets with actual
                                     BRIKs will be included.

               BRICK_BYTE_MASK     = Masks that define what type of data
               BRICK_SHORT_MASK      should be stored in the sub-bricks
               BRICK_FLOAT_MASK      of the allowable datasets.
               BRICK_COMPLEX_MASK
               BRICK_RGB_MASK
               BRICK_ALLTYPE_MASK
               BRICK_ALLREAL_MASK

               *  Note that entering 0 for ctrl_mask means that no   *
               *  datasets will be choosable.  At the least, one of  *
               *  the DIMEN_ masks must be chosen, and one of the    *
               *  BRICK_ masks must be chosen.                       *
-------------------------------------------------------------------------*/

void add_dataset_to_PLUGIN_interface( PLUGIN_interface * plint ,
                                      char * label ,
                                      int anat_mask , int func_mask , int ctrl_mask )
{
   int nopt , nsv , ii ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

ENTRY("add_dataset_to_PLUGIN_interface") ;

   /*-- sanity checks --*/

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;
   if( anat_mask == 0 && func_mask == 0 ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;

   if( (ctrl_mask & BRICK_ALLTYPE_MASK)==0 ||
       (ctrl_mask & DIMEN_ALL_MASK)    ==0   ) EXRETURN ;

   nopt = plint->option_count - 1 ;
   opt  = plint->option[nopt] ;

   nsv = opt->subvalue_count ;
   if( nsv == PLUGIN_MAX_SUBVALUES ){
      fprintf(stderr,"*** Warning: maximum plugin subvalue count exceeded!\n");
      EXRETURN ;
   }

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_DATASET_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   sv->dset_anat_mask = anat_mask ;
   sv->dset_func_mask = func_mask ;
   sv->dset_ctrl_mask = ctrl_mask ;

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

void add_dataset_list_to_PLUGIN_interface( PLUGIN_interface * plint ,
                                           char * label ,
                                           int anat_mask , int func_mask , int ctrl_mask )
{
   int nopt , nsv , ii ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

ENTRY("add_dataset_list_to_PLUGIN_interface") ;

   /*-- sanity checks --*/

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;
   if( anat_mask == 0 && func_mask == 0 ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;

   if( (ctrl_mask & BRICK_ALLTYPE_MASK)==0 ||
       (ctrl_mask & DIMEN_ALL_MASK)    ==0   ) EXRETURN ;

   nopt = plint->option_count - 1 ;
   opt  = plint->option[nopt] ;

   nsv = opt->subvalue_count ;
   if( nsv == PLUGIN_MAX_SUBVALUES ){
      fprintf(stderr,"*** Warning: maximum plugin subvalue count exceeded!\n");
      EXRETURN ;
   }

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_DATASET_LIST_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   sv->dset_anat_mask = anat_mask ;
   sv->dset_func_mask = func_mask ;
   sv->dset_ctrl_mask = ctrl_mask ;

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Routine to add a timeseries "chooser" to the most recently created
   option within a plugin interface.

   label = C string to go in the menu, next to the "chooser" for
           the dataset.
-------------------------------------------------------------------------*/

void add_timeseries_to_PLUGIN_interface( PLUGIN_interface * plint, char * label )
{
   int nopt , nsv , ii ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

ENTRY("add_timeseries_to_PLUGIN_interface") ;

   /*-- sanity checks --*/

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;

   nopt = plint->option_count - 1 ;
   opt  = plint->option[nopt] ;

   nsv = opt->subvalue_count ;
   if( nsv == PLUGIN_MAX_SUBVALUES ){
      fprintf(stderr,"*** Warning: maximum plugin subvalue count exceeded!\n");
      EXRETURN ;
   }

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_TIMESERIES_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

static int initcolorindex=1 ;
void PLUTO_set_initcolorindex( int i ){ initcolorindex = i; } /* 10 Oct 2007 */

/*-----------------------------------------------------------------------
   Routine to add a color overlay  "chooser" to the most recently
   created option within a plugin interface -- 11 Jul 2001 - RWCox.

   label = C string to go in the menu, next to the "chooser" for
           the color.
-------------------------------------------------------------------------*/

void add_overlaycolor_to_PLUGIN_interface( PLUGIN_interface *plint, char *label )
{
   int nopt , nsv , ii ;
   PLUGIN_option *opt ;
   PLUGIN_subvalue *sv ;

ENTRY("add_overlaycolor_to_PLUGIN_interface") ;

   /*-- sanity checks --*/

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;

   if( label == NULL ) label = EMPTY_STRING ;

   nopt = plint->option_count - 1 ;
   opt  = plint->option[nopt] ;

   nsv = opt->subvalue_count ;
   if( nsv == PLUGIN_MAX_SUBVALUES ){
     WARNING_message("maximum plugin subvalue count exceeded - overlaycolor");
     EXRETURN ;
   }

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_OVERLAY_COLOR_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;
   sv->value_default = initcolorindex ;

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

int PLUG_nonblank_len( char *str )
{
   int ii , ll ;

   if( str == NULL || *str == '\0' ) return 0 ;
   ll = strlen(str) ;

   for( ii=ll-1 ; ii >= 0 ; ii-- ) if( !isspace(str[ii]) ) break ;

   return (ii+1) ;
}

/*--------------------------------------------------------------------------
  Decide if a dataset prefix is OK.  The string itself is checked for
  legality, then the list of all datasets is checked for duplicates.
----------------------------------------------------------------------------*/

int PLUTO_prefix_ok( char *str )
{
   int ll , ii ;
   THD_slist_find find ;

ENTRY("PLUTO_prefix_ok") ;

   /*--- check the string itself for OK-osity ---*/

   if( str == NULL ) RETURN(0) ;
   ll = strlen( str ) ; if( ll == 0 ) RETURN(0) ;

   for( ii=0 ; ii < ll ; ii++ )
      if( iscntrl(str[ii]) || isspace(str[ii]) ||
          str[ii] == '/'   || str[ii] == ';'   ||
          str[ii] == '*'   || str[ii] == '?'   ||
          str[ii] == '&'   || str[ii] == '|'   ||
          str[ii] == '"'   || str[ii] == '>'   ||
          str[ii] == '<'   || str[ii] == '\''  ||
          str[ii] == '['   || str[ii] == ']'     ) RETURN(0) ;

   /*--- now see if the prefix already exists in AFNI ---*/

   find = THD_dset_in_sessionlist( FIND_PREFIX , str ,
                                   GLOBAL_library.sslist , -1 ) ;

   RETURN(find.dset == NULL) ;
}

/*------------------------------------------------------------------------------
  Routine to create (but not map) the widgets for a plugin interface.
--------------------------------------------------------------------------------*/

/***** definitions for the action area controls *****/

#define PLUG_quit_label   "Quit"
#define PLUG_run_label    "Run+Keep"
#define PLUG_doit_label   "Run+Close"
#define PLUG_help_label   "Help"

#define PLUG_quit_help   "Press to close\nthis panel without\nrunning program."
#define PLUG_run_help    "Press to run\nthe program\nand keep panel open."
#define PLUG_doit_help   "Press to run\nthe program\nand close this panel."
#define PLUG_help_help   "Press to get\nthe help for\nthis program."

#define NUM_PLUG_ACT 4

static MCW_action_item PLUG_act[] = {
 { PLUG_quit_label, PLUG_action_CB, NULL, PLUG_quit_help,"Close window"               , 0 },
 { PLUG_run_label , PLUG_action_CB, NULL, PLUG_run_help ,"Run plugin and keep window" , 0 },
 { PLUG_doit_label, PLUG_action_CB, NULL, PLUG_doit_help,"Run plugin and close window", 1 },
 { PLUG_help_label, PLUG_action_CB, NULL, PLUG_help_help,"Get help for plugin"        , 0 }
} ;

void PLUG_setup_widgets( PLUGIN_interface * plint , MCW_DC * dc )
{
   int iopt , ib , max_nsv , ww,hh , shh , toff , zlen ;
   XmString xstr ;
   char str[256] ;
   PLUGIN_widgets * wid ;
   PLUGIN_option_widgets ** opwid ;
   PLUGIN_option_widgets * ow ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;
   Widget actar , wframe , separator = NULL ;
   int opt_lwid , sv_lwid[PLUGIN_MAX_SUBVALUES] ;
   Widget widest_chooser[PLUGIN_MAX_SUBVALUES] ;
   int    widest_width[PLUGIN_MAX_SUBVALUES] ;
   Pixel  fg_pix=0 ;

ENTRY("PLUG_setup_widgets") ;

   /**** sanity checks ****/

   if( plint == NULL || plint->wid != NULL ||
       plint->call_method == PLUGIN_CALL_IMMEDIATELY ) EXRETURN ;

   /**** create widgets structure ****/

   plint->wid = wid = myXtNew(PLUGIN_widgets) ;
   
   /**** create Shell that can be opened up later ****/
   /* 'LessTif Widormous' 
      With 64bit LessTif, some widgets get created with 
      enormous sizes. To complicate matters, the problem
      occurred randomly so tracking it is a frustrating
      exercise. It looks like adding a few, harmless,
      XmNheight and XmNwidth parameters at widget creation
      fixed the problem. Those parameters have no effect
      on the final plugin's look so we won't bother to find
      out which of them was necessary for fixing the problem.
                  12 Feb 2009 9Lesstif patrol] 
   */
   wid->shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , plint->label , /* top of window */
           XmNiconName          , plint->label , /* label on icon */
           XmNmappedWhenManaged , False ,        /* must map it manually */
           XmNdeleteResponse    , XmDO_NOTHING , /* deletion handled below */
           XmNallowShellResize  , False ,        /* let code resize shell? */
           XmNinitialResourcesPersistent , False ,
               XmNheight, 207, /* See 'LessTif Widormous' comment above */
               XmNwidth, 206,  /*         12 Feb 2009 9Lesstif patrol] */
           XmNkeyboardFocusPolicy , XmEXPLICIT ,
      NULL ) ;

   DC_yokify( wid->shell , dc ) ; /* 14 Sep 1998 */

   if( afni48_good )
      XtVaSetValues( wid->shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;

   if( MCW_isitmwm(wid->shell) )
      XtVaSetValues( wid->shell ,
                        XmNmwmDecorations , MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE ,
                     NULL ) ;

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           wid->shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           PLUG_delete_window_CB , (XtPointer) plint ) ;

   /**** create RowColumn to hold all widgets ****/

   wid->form = XtVaCreateWidget(
                 "AFNI" , xmFormWidgetClass , wid->shell ,
                     XmNborderWidth , 0 ,
                     XmNborderColor , 0 ,
                        XmNwidth, 70,/* See 'LessTif Widormous' comment above */
                        XmNheight, 100,/*         12 Feb 2009 9Lesstif patrol] */
                     XmNtraversalOn , True  ,
                     XmNinitialResourcesPersistent , False ,
                 NULL ) ;

   /**** create Label at top to hold description of this program ****/

   sprintf( str , "AFNI Plugin: %s" , plint->description ) ;
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   wid->label =
      XtVaCreateManagedWidget(
        "AFNI" , xmLabelWidgetClass ,  wid->form ,
           XmNlabelString , xstr ,
           XmNalignment  , XmALIGNMENT_CENTER ,

           XmNleftAttachment , XmATTACH_FORM ,
           XmNrightAttachment, XmATTACH_FORM ,
           XmNtopAttachment  , XmATTACH_FORM ,
           XmNtopOffset      , 5 ,
               XmNheight, 20, /* See 'LessTif Widormous' comment above */
               XmNwidth, 20, /*         12 Feb 2009 9Lesstif patrol] */
           XmNinitialResourcesPersistent , False ,
        NULL ) ;
    XmStringFree( xstr ) ; LABELIZE(wid->label) ;

    /* now that we have the label,
       find its sizes and make sure the shell doesn't get too small */

    MCW_widget_geom( wid->label , &ww , &hh , NULL , NULL ) ;
    XtVaSetValues( wid->shell ,
                      XmNminWidth  , ww+3*hh ,
                      XmNminHeight , (plint->option_count == 0) ? 5*hh : 7*hh ,
                   NULL ) ;

   /**** create an action area beneath to hold user control buttons ****/

   for( ib=0 ; ib < NUM_PLUG_ACT ; ib++ )
     PLUG_act[ib].data = (XtPointer) plint ;

   /* 04 Nov 2003: allow for change of Run+Close and Run+Keep labels */

   if( plint->run_label[0]  == '\0' ) strcpy(plint->run_label ,PLUG_run_label );
   if( plint->doit_label[0] == '\0' ) strcpy(plint->doit_label,PLUG_doit_label);
   PLUG_act[1].label = plint->run_label ;
   PLUG_act[2].label = plint->doit_label;

   actar = MCW_action_area( wid->form , PLUG_act ,
                            (plint->helpstring!=NULL) ? NUM_PLUG_ACT
                                                      : NUM_PLUG_ACT-1 ) ;

   XtVaSetValues( actar ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , wid->label ,
                     XmNtopOffset      , 7 ,
                  NULL ) ;

   /**** create a Scrolled Window and Form to hold
         the user input option widgets, if they will be needed ****/

   if( plint->option_count > 0 ){
      wid->scrollw =
         XtVaCreateWidget(
           "AFNI" , xmScrolledWindowWidgetClass ,  wid->form ,
              XmNscrollingPolicy , XmAUTOMATIC ,
              XmNwidth  , ww+2*hh ,
              XmNheight ,    3*hh ,
              XmNleftAttachment  , XmATTACH_FORM ,
              XmNrightAttachment , XmATTACH_FORM ,
              XmNtopAttachment   , XmATTACH_WIDGET ,
              XmNbottomAttachment, XmATTACH_FORM ,
              XmNtopWidget       , actar ,
              XmNtopOffset       , 7 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

      wframe =
         XtVaCreateWidget(
           "AFNI" , xmFrameWidgetClass , wid->scrollw ,
               XmNshadowType , XmSHADOW_ETCHED_IN ,
               XmNshadowThickness , 5 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      wid->workwin =
         XtVaCreateWidget(
           "AFNI" , xmFormWidgetClass , wframe ,
              XmNborderWidth , 0 ,
              XmNborderColor , 0 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

      wid->opwid = opwid =
             (PLUGIN_option_widgets **)
                XtMalloc(sizeof(PLUGIN_option_widgets *) * plint->option_count) ;

      /** setup widest widgets for each column of the Form **/

      for( ib=0 ; ib < PLUGIN_MAX_SUBVALUES ; ib++ ){
         widest_chooser[ib] = NULL ;
         widest_width[ib]   = 0 ;
      }

      XtVaGetValues( wframe , XmNforeground , &fg_pix , NULL ) ;

   } else {
      wframe = wid->scrollw = wid->workwin = NULL ;
      opwid = NULL ;
   }

   /**** for each column within each option, find the max string width ****/

   opt_lwid = 1 ;
   for( ib=0 ; ib < PLUGIN_MAX_SUBVALUES ; ib++ ) sv_lwid[ib] = 1 ;

   for( iopt=0 ; iopt < plint->option_count ; iopt++ ){
      opt = plint->option[iopt] ;
      if( opt == NULL ) continue ; /* bad? */

      opt_lwid = MAX( opt_lwid , PLUG_nonblank_len(opt->label) ) ;
      for( ib=0 ; ib < opt->subvalue_count ; ib++ ){
         sv = &(opt->subvalue[ib]) ;
         sv_lwid[ib] = MAX( sv_lwid[ib] , PLUG_nonblank_len(sv->label) ) ;
      }
   }

   /**** now clip each label string to its column's width ****/

   for( iopt=0 ; iopt < plint->option_count ; iopt++ ){
      opt = plint->option[iopt] ;
      if( opt == NULL ) continue ; /* bad? */

#define LPAD 0   /* 29 Mar 2002: used to be 1 */

      opt->label[ opt_lwid + LPAD ] = '\0' ;
      for( ib=0 ; ib < opt->subvalue_count ; ib++ ){
         sv = &(opt->subvalue[ib]) ;
         sv->label[ sv_lwid[ib] + LPAD ] = '\0' ;
      }
   }

   /**** create a row of Widgets for each option ****/

   for( iopt=0 ; iopt < plint->option_count ; iopt++ ){
      opt = plint->option[iopt] ;
      if( opt == NULL ) continue ; /* bad? */

      ow = opwid[iopt] = myXtNew(PLUGIN_option_widgets) ;

      for( ib=0 ; ib < PLUGIN_MAX_SUBVALUES ; ib++ ){  /* initialize */
         ow->chooser[ib]      = NULL ;                 /* all subvalue */
         ow->chtop[ib]        = NULL ;                 /* stuff */
         ow->chooser_type[ib] = OP_CHOOSER_NONE ;
         opt->callvalue[ib]   = NULL ;
      }

      /** create ToggleButton to indicate whether this is used **/

      ow->toggle =
         XtVaCreateManagedWidget(
           "AFNI" , xmToggleButtonWidgetClass , wid->workwin ,
              XmNleftAttachment   , XmATTACH_FORM ,
              XmNtopAttachment    , (iopt==0) ? XmATTACH_FORM     /* first row */
                                              : XmATTACH_WIDGET , /* 2nd+ row */
              XmNtopOffset        , 1 ,
              XmNleftOffset       , 1 ,
              XmNtopWidget        , separator ,  /* 2nd+ row */

              XmNlabelType        , XmPIXMAP ,             /* No label attached */
              XmNlabelPixmap      , XmUNSPECIFIED_PIXMAP , /* Just the toggle!  */

              XmNset              , (opt->mandatory != FALSE) ? True  : False ,
              XmNsensitive        , (opt->mandatory == TRUE ) ? False : True  ,

              XmNindicatorSize    , hh-1 ,
              XmNmarginHeight     , 0  ,
              XmNmarginWidth      , 0  ,
              XmNselectColor      , fg_pix ,  /* fill with foreground when set */
               XmNheight, 20,
               XmNwidth, 20,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

      MCW_register_help( ow->toggle ,
                         "When pressed in and filled,\n"
                         "this button denotes that this\n"
                         "line of input will be passed\n"
                         "to the plugin.  If the plugin\n"
                         "has specified that this line\n"
                         "is required, then you cannot\n"
                         "toggle this button off."
                       ) ;

      if( opt->mandatory == TRUE )
         MCW_register_hint( ow->toggle ,
                            "This input line is mandatory" ) ;
      else
         MCW_register_hint( ow->toggle ,
                            "IN: this input line sent to plugin" ) ;

      /* this callback will change the appearance of the
         option's row of widgets when the toggle button is pressed */

      if( opt->mandatory != TRUE )
         XtAddCallback( ow->toggle , XmNvalueChangedCallback ,
                        PLUG_optional_toggle_CB , (XtPointer) ow ) ;

      /** create Label to describe this option **/

#if 0
fprintf(stderr,"Option setup %s\n",opt->label) ;
#endif

      zlen = (PLUG_nonblank_len(opt->label) == 0) ;
      xstr = XmStringCreateLtoR( opt->label , XmFONTLIST_DEFAULT_TAG ) ;
      ow->label =
         XtVaCreateManagedWidget(
           "AFNI" , xmLabelWidgetClass , wid->workwin ,
              XmNleftAttachment   , XmATTACH_WIDGET ,
              XmNtopAttachment    , (iopt==0) ? XmATTACH_FORM     /* 1st row */
                                              : XmATTACH_WIDGET , /* 2nd+ row */
              XmNtopOffset        , 6 ,
              XmNleftOffset       , 0 ,
              XmNleftWidget       , ow->toggle ,
              XmNtopWidget        , separator ,  /* 2nd+ row */

              XmNmarginHeight     , 0  ,
              XmNmarginWidth      , 0  ,

              XmNlabelString , xstr ,
              XmNuserData    , (XtPointer)ITOP(zlen) ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
      XmStringFree( xstr ) ;
      if( opt->mandatory == TRUE && !zlen ){
         MCW_invert_widget( ow->label ) ;
         MCW_register_help( ow->label ,
                            "This label is inverted as a\n"
                            "signal that this line of inputs\n"
                            "is required by the plugin, and\n"
                            "can't be toggled off."
                          ) ;
      }
      if( opt->hint != NULL )
         MCW_register_hint( ow->label , opt->hint ) ;

      /** for each subvalue, create a chooser to select it **/

      for( ib=0 ; ib < opt->subvalue_count ; ib++ ){
         sv   = &(opt->subvalue[ib]) ;
         toff = 4 ;                      /* default top offset */

         switch( sv->data_type ){

            /** type I can't handle yet, so just put some label there **/
            fprintf(stderr,"Doing type %d\n", sv->data_type);
            default:
               xstr = XmStringCreateLtoR( "** N/A **" , XmFONTLIST_DEFAULT_TAG ) ;
               ow->chtop[ib] =
                  XtVaCreateManagedWidget(
                     "AFNI" , xmLabelWidgetClass , wid->workwin ,
                     XmNlabelString , xstr ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;
               XmStringFree( xstr ) ;
               ow->chooser_type[ib] = OP_CHOOSER_NONE ;
               ow->chooser[ib]      = NULL ;
            break ;

            /** overlay color type -- 11 Jul 2001 **/

            case PLUGIN_OVERLAY_COLOR_TYPE:{
               MCW_arrowval *av ; int iv=sv->value_default ;
#if 0
fprintf(stderr,"colormenu setup %s; opt->tag=%s.\n",sv->label,opt->tag) ;
#endif
               if( iv < 0 || iv > dc->ovc->ncol_ov-1 ) iv = 1 ;

               av = new_MCW_colormenu(
                       wid->workwin ,                    /* parent */
                       sv->label ,                       /* label  */
                       dc ,                              /* display context */
                       1 ,                               /* first color */
                       dc->ovc->ncol_ov - 1 ,            /* last color */
                       iv ,                              /* initial color */
                       NULL,NULL                         /* callback func,data */
                    ) ;

               ow->chooser[ib] = (void *) av ;
               ow->chtop[ib]   = av->wrowcol ;  /* get the top widget */
               #ifdef USING_LESSTIF_NOT_DOING_THIS
                  if (CPU_IS_64_BIT() ){
                     ow->chtop[ib]   = XtParent(XtParent(av->wrowcol)); 
                              /* get the very top rowcol holding the 
                                 optmenu rowcol, see function
                                 new_MCW_optmenu_64fix   [LPatrol Feb 19 2009] */
                  }
               #endif

               ow->chooser_type[ib] = OP_CHOOSER_COLORMENU ;
            }
            break ;

            /** number type: make an arrowval or an option menu **/

            case PLUGIN_NUMBER_TYPE:{
               int num_choice , use_optmenu ;
               MCW_arrowval * av ;

               num_choice  = abs(sv->int_range_top - sv->int_range_bot) + 1 ;
               use_optmenu = (num_choice < OP_OPTMENU_LIMIT) && !sv->editable ;

               av = new_MCW_arrowval(
                       wid->workwin ,                    /* parent */
                       sv->label ,                       /* label  */
                       (use_optmenu) ? MCW_AV_optmenu    /* type   */
                                     : MCW_AV_downup ,
                       sv->int_range_bot ,               /* min */
                       sv->int_range_top ,               /* max */
                       sv->value_default ,               /* initial */
                       (sv->editable) ? MCW_AV_editext   /* type */
                                      : MCW_AV_readtext ,
                       sv->int_range_decim ,             /* decimals? */
                       NULL , NULL , NULL , NULL
                     ) ;

               if( use_optmenu && num_choice > COLSIZE )
                  AVOPT_columnize(av, 1+(num_choice-1)/COLSIZE) ;

               ow->chooser[ib] = (void *) av ;
               ow->chtop[ib]   = av->wrowcol ;  /* get the top widget */
               #ifdef USING_LESSTIF_NOT_DOING_THIS
                  if (CPU_IS_64_BIT() && use_optmenu){
                     ow->chtop[ib]   = XtParent(XtParent(av->wrowcol)); 
                              /* get the very top rowcol holding the 
                                 optmenu rowcol, see function
                                 new_MCW_optmenu_64fix [LPatrol Feb 19 2009]*/
                  }
               #endif
               ow->chooser_type[ib] = (use_optmenu) ? OP_CHOOSER_OPTMENU
                                                    : OP_CHOOSER_NUMBER ;

               if( !use_optmenu ) av->allow_wrap = 1 ;
               if(  use_optmenu ) toff-- ;

               if( !use_optmenu && (plint->flags & SHORT_NUMBER_FLAG) )
                  XtVaSetValues( av->wtext , XmNcolumns , 6 , NULL ) ;
            }
            break ;

            /** string type:
                  make an arrowval if a finite number of choices,
                  or a label+textfield if the user can type in anything **/

            case PLUGIN_STRING_TYPE:{
               if( sv->string_range_count > 0 ){    /* finite number of choices */
                  int num_choice , use_optmenu ;
                  MCW_arrowval * av ;

                  num_choice  = sv->string_range_count ;
                  use_optmenu = (num_choice < OP_OPTMENU_LIMIT) ;

                  av = new_MCW_arrowval(
                          wid->workwin ,                    /* parent */
                          sv->label ,                       /* label  */
                          (use_optmenu) ? MCW_AV_optmenu    /* type   */
                                        : MCW_AV_downup ,
                          0 ,                               /* min */
                          num_choice-1 ,                    /* max */
                          sv->value_default ,               /* initial */
                          MCW_AV_readtext ,                 /* type */
                          0 ,                               /* decimals? */
                          NULL , NULL ,                     /* callbacks */
                          MCW_av_substring_CB ,             /* text routine */
                          sv->string_range                  /* text data */
                        ) ;

                  if( !use_optmenu ){                        /* 11 Jul 2001 */
                    int ss , ll , mm=9 ;                     /* increase   */
                    for( ss=0 ; ss < num_choice ; ss++ ){    /* width of  */
                       if( sv->string_range[ss] != NULL ){   /* widget,   */
                          ll = strlen(sv->string_range[ss]); /* if needed */
                          if( ll > mm ) mm = ll ;
                       }
                    }
                    if( mm > 9 )
                      XtVaSetValues( av->wtext ,
                                       XmNmaxLength,mm , XmNcolumns,mm ,
                                     NULL ) ;
                  }

                  if( use_optmenu && num_choice > COLSIZE )
                     AVOPT_columnize(av, 1+(num_choice-1)/COLSIZE) ;

                  ow->chooser[ib] = (void *) av ;
                  ow->chtop[ib]   = av->wrowcol ;  /* get the top widget */
                  #ifdef USING_LESSTIF_NOT_DOING_THIS
                  if (CPU_IS_64_BIT() && use_optmenu ){
                     ow->chtop[ib]   = XtParent(XtParent(av->wrowcol)); 
                              /* get the very top rowcol holding the 
                                 optmenu rowcol, see function
                                 new_MCW_optmenu_64fix [LPatrol Feb 19 2009]*/
                  }
                  #endif
                  ow->chooser_type[ib] = (use_optmenu) ? OP_CHOOSER_OPTMENU
                                                       : OP_CHOOSER_STRING ;

                  if( !use_optmenu ) av->allow_wrap = 1 ;
                  if(  use_optmenu ) toff-- ;

               } else {  /* arbitrary string input allowed */

                  PLUGIN_strval * av = myXtNew(PLUGIN_strval) ;

                  av->rowcol =
                     XtVaCreateWidget(
                       "AFNI" , xmRowColumnWidgetClass , wid->workwin ,
                          XmNpacking     , XmPACK_TIGHT ,
                          XmNorientation , XmHORIZONTAL ,
                          XmNmarginHeight, 0 ,
                          XmNmarginWidth , 0 ,
                          XmNspacing     , 0 ,
                          XmNtraversalOn , True  ,
                          XmNinitialResourcesPersistent , False ,
                       NULL ) ;

                  xstr = XmStringCreateLtoR( sv->label , XmFONTLIST_DEFAULT_TAG ) ;
                  av->label =
                     XtVaCreateManagedWidget(
                       "AFNI" , xmLabelWidgetClass , av->rowcol ,
                          XmNlabelString , xstr ,
                          XmNmarginWidth   , 0  ,
                          XmNinitialResourcesPersistent , False ,
                       NULL ) ;
                  XmStringFree( xstr ) ;

                  av->textf =
                     XtVaCreateManagedWidget(
                       "AFNI" , xmTextFieldWidgetClass , av->rowcol ,
                           XmNcolumns      , (sv->value_default > 1)
                                            ? sv->value_default : 9 ,
                           XmNeditable     , True ,
                           XmNmaxLength    , PLUGIN_STRING_SIZE ,
                           XmNresizeWidth  , False ,
                           XmNmarginHeight , 1 ,
                           XmNmarginWidth  , 1 ,
                           XmNcursorPositionVisible , True ,
                           XmNblinkRate , 0 ,
                           XmNautoShowCursorPosition , True ,
                           XmNtraversalOn , True  ,
                           XmNinitialResourcesPersistent , False ,
                        NULL ) ;

                  if( sv->string_range_count == -1 )
                    XmTextFieldSetString( av->textf , sv->string_range[0] ) ;

                  XtManageChild( av->rowcol ) ;

                  ow->chooser[ib] = (void *) av ;
                  ow->chtop[ib]   = av->rowcol ;  /* get the top widget */

                  ow->chooser_type[ib] = OP_CHOOSER_STRING ;
               }
            }
            break ;

            /** single dataset type: make a pushbutton to popup a chooser **/
            /** 24 Nov 1996: adapt to include dataset list type as well   **/

            case PLUGIN_DATASET_LIST_TYPE:
            case PLUGIN_DATASET_TYPE:{
               PLUGIN_dsetval * av = myXtNew(PLUGIN_dsetval) ;

               av->sv = sv ;  /* what this is linked to */

               av->dset_count  = 0 ;     /* will be array of datasets */
               av->dset_link   = NULL ;  /* we can choose amongst */

               av->dset_choice = -1 ;    /* will be index of our choice */

               /* 24 Nov 1996 */

               av->multi   = (sv->data_type == PLUGIN_DATASET_LIST_TYPE) ;
               av->nchosen = 0 ;
               av->chosen  = NULL ;
               av->current = 0 ;
               av->idclist = NULL ;

               av->rowcol =
                  XtVaCreateWidget(
                    "AFNI" , xmRowColumnWidgetClass , wid->workwin ,
                       XmNpacking     , XmPACK_TIGHT ,
                       XmNorientation , XmHORIZONTAL ,
                       XmNmarginHeight, 0 ,
                       XmNmarginWidth , 0 ,
                       XmNspacing     , 0 ,
                       XmNtraversalOn , True  ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

               xstr = XmStringCreateLtoR( sv->label , XmFONTLIST_DEFAULT_TAG ) ;
               av->label =
                  XtVaCreateManagedWidget(
                    "AFNI" , xmLabelWidgetClass , av->rowcol ,
                       XmNlabelString , xstr ,
                       XmNmarginWidth   , 0  ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;
               XmStringFree( xstr ) ;

               if( plint->flags & SHORT_CHOOSE_FLAG )
                 xstr = XmStringCreateLtoR(
                          (av->multi) ? "* Datasets *"
                                      : "- Dataset -" ,
                          XmFONTLIST_DEFAULT_TAG ) ;
               else
                 xstr = XmStringCreateLtoR(
                          (av->multi) ? "** Choose Datasets *"
                                      : "-- Choose Dataset --" ,
                          XmFONTLIST_DEFAULT_TAG ) ;

               av->pb = XtVaCreateManagedWidget(
                           "AFNI" , xmPushButtonWidgetClass , av->rowcol ,
                              XmNlabelString   , xstr ,
                              XmNmarginHeight  , 0 ,
                              XmNmarginWidth   , 0 ,
                              XmNrecomputeSize , False ,
                              XmNtraversalOn   , True  ,
                              XmNuserData      , (XtPointer) av ,
                              XmNinitialResourcesPersistent , False ,
                           NULL ) ;

               XtAddCallback( av->pb , XmNactivateCallback ,
                              PLUG_choose_dataset_CB , (XtPointer) plint ) ;

               XtManageChild( av->rowcol ) ;

               ow->chooser[ib] = (void *) av ;
               ow->chtop[ib]   = av->rowcol ;  /* get the top widget */

               ow->chooser_type[ib] = OP_CHOOSER_DSET ;
               toff-- ;
            }
            break ;

            /** single timeseries type (similiar to dataset above) **/

            case PLUGIN_TIMESERIES_TYPE:{
               PLUGIN_tsval * av = myXtNew(PLUGIN_tsval) ;

               av->sv        = sv ;                        /* a friend in need  */
               av->tsimar    = GLOBAL_library.timeseries ; /* to choose amongst */
               av->ts_choice = -1 ;                        /* no initial choice */
               av->tsim      = NULL ;

               av->rowcol =
                  XtVaCreateWidget(
                    "AFNI" , xmRowColumnWidgetClass , wid->workwin ,
                       XmNpacking     , XmPACK_TIGHT ,
                       XmNorientation , XmHORIZONTAL ,
                       XmNmarginHeight, 0 ,
                       XmNmarginWidth , 0 ,
                       XmNspacing     , 0 ,
                       XmNtraversalOn , True  ,
                              XmNheight, 20,
                              XmNwidth, 30,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

               xstr = XmStringCreateLtoR( sv->label , XmFONTLIST_DEFAULT_TAG ) ;
               av->label =
                  XtVaCreateManagedWidget(
                    "AFNI" , xmLabelWidgetClass , av->rowcol ,
                       XmNlabelString , xstr ,
                       XmNmarginWidth   , 0  ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;
               XmStringFree( xstr ) ;

               if( plint->flags & SHORT_CHOOSE_FLAG )
                 xstr = XmStringCreateLtoR( "-Timeseries-",XmFONTLIST_DEFAULT_TAG ) ;
               else
                 xstr = XmStringCreateLtoR( "-Choose Timeseries- ",XmFONTLIST_DEFAULT_TAG ) ;

               av->pb = XtVaCreateManagedWidget(
                           "AFNI" , xmPushButtonWidgetClass , av->rowcol ,
                              XmNlabelString   , xstr ,
                              XmNmarginHeight  , 0 ,
                              XmNmarginWidth   , 0 ,
                              XmNrecomputeSize , False ,
                              XmNtraversalOn   , True  ,
                              XmNuserData      , (XtPointer) av ,
                              XmNinitialResourcesPersistent , False ,
                           NULL ) ;

               XtAddCallback( av->pb , XmNactivateCallback ,
                              PLUG_choose_timeseries_CB , (XtPointer) plint ) ;

               XtManageChild( av->rowcol ) ;

               ow->chooser[ib] = (void *) av ;
               ow->chtop[ib]   = av->rowcol ;  /* get the top widget */

               ow->chooser_type[ib] = OP_CHOOSER_TIMESERIES ;
               toff-- ;
            }
            break ;

         }  /* end of switch on subvalue type */

         /** set initial attachments for the topmost chooser widget **/

         XtVaSetValues(
            ow->chtop[ib] ,
               XmNleftAttachment   , (ib==0) ? XmATTACH_WIDGET    /* First column  */
                                             : XmATTACH_NONE ,    /* of widgets    */
               XmNleftOffset       , (ib==0) ? 6                  /* is attached   */
                                             : 0 ,                /* to the label. */
               XmNleftWidget       , (ib==0) ? ow->label          /* Later columns */
                                             : NULL ,             /* fixed below.  */

               XmNtopAttachment    , (iopt==0) ? XmATTACH_FORM     /* 1st row */
                                               : XmATTACH_WIDGET , /* 2nd+ row */
               XmNtopOffset        , toff ,
               XmNtopWidget        , separator ,
            NULL ) ;

         if( opt->mandatory == FALSE ) XtSetSensitive( ow->chtop[ib] , False ) ;

         if( sv->hint != NULL )
             MCW_reghint_children( ow->chtop[ib] , sv->hint ) ;

         /** find out if this is the widest one in its column so far **/

         MCW_widget_geom( ow->chtop[ib] , &ww , NULL,NULL,NULL ) ;

         if( ww > widest_width[ib] ){
            widest_width[ib]   = ww ;
            widest_chooser[ib] = ow->chtop[ib] ;
         }

      }  /* end of loop over subvalues */

      /** separator between option rows **/

      separator = XtVaCreateManagedWidget(
                    "AFNI" , xmSeparatorWidgetClass , wid->workwin ,
                       XmNseparatorType  , XmSHADOW_ETCHED_OUT ,
                       XmNshadowThickness, 5 ,
                       XmNleftAttachment , XmATTACH_FORM ,
                       XmNrightAttachment, XmATTACH_FORM ,
                       XmNtopAttachment  , XmATTACH_WIDGET ,
                       XmNtopWidget      , ow->toggle ,
                       XmNtopOffset      , 7 ,
                    NULL ) ;

   } /* end of loop over options */

#if 0
fprintf(stderr,"Widget attachments\n") ;
#endif

   /**** Now that we've created all the rows,
         and have found all the widest widgets in each column,
         go back and attach each column to the widest one to its left. ****/

   for( iopt=0 ; iopt < plint->option_count ; iopt++ ){
      opt = plint->option[iopt] ;
      if( opt == NULL ) continue ; /* bad? */

      ow = opwid[iopt] ;
      for( ib=1 ; ib < opt->subvalue_count ; ib++ ){
         XtVaSetValues( ow->chtop[ib] ,
                           XmNleftAttachment , XmATTACH_WIDGET ,
                           XmNleftWidget     , widest_chooser[ib-1] ,
                           XmNleftOffset     , 6 ,
                        NULL ) ;
      }
   }

#if 0
fprintf(stderr,"Widget separators\n") ;
#endif

   /**** Create a vertical separator to the left of each column ****/

   if( plint->option_count > 0 ){
      for( ib=0 ; ib < PLUGIN_MAX_SUBVALUES && widest_width[ib] > 0 ; ib++ ){
         separator = XtVaCreateManagedWidget(
                       "AFNI" , xmSeparatorWidgetClass , wid->workwin ,
                          XmNseparatorType   , XmSHADOW_ETCHED_OUT ,
                          XmNorientation     , XmVERTICAL ,
                          XmNtopAttachment   , XmATTACH_FORM ,
                          XmNbottomAttachment, XmATTACH_FORM ,
                          XmNrightAttachment , XmATTACH_WIDGET ,
                          XmNrightWidget     , widest_chooser[ib] ,
                          XmNrightOffset     , 2 ,
                       NULL ) ;

      }
   }

#if 0
fprintf(stderr,"Widget management\n") ;
#endif

   /**** Manage the managers, and go home ****/

   if( plint->option_count > 0 ){
      XtManageChild( wid->workwin ) ;
      XtManageChild( wframe ) ;
      XtManageChild( wid->scrollw ) ;
   }
   XtManageChild( wid->form ) ;

#if 0
fprintf(stderr,"Widget realization\n") ;
#endif

   XtRealizeWidget( wid->shell ) ;  /* will not be mapped */
   NI_sleep(1) ;

   /** set its width after it is mapped **/

#define OPC_MAX     8
#define OPC_MAXMAX 10
#define LUCK        5

   if( wframe != NULL ){
      Widget bar=NULL ;
      int fww , fhh , fyy , bww ;

#if 0
fprintf(stderr,"Widget geometrization\n") ;
#endif

      MCW_widget_geom( wid->label   , &ww , &hh , NULL, NULL ) ;  /* get dimensions */
      MCW_widget_geom( wframe       , &fww, &fhh, NULL, NULL ) ;  /* of various */
      MCW_widget_geom( wid->scrollw , NULL, NULL, NULL, &fyy ) ;  /* pieces-parts */

      fww = MAX( fww+LUCK , ww ) ;  /* extra pixels for luck */

      ib = plint->option_count ;     /* too many options --> will use vertical scrollbar */
      if( ib > OPC_MAXMAX ){
         fhh = (OPC_MAX * fhh) / ib ;  /* set height to allow for OPC_MAX options visible */

         if( fww > ww ){               /* set width to allow for vertical scrollbar */
            XtVaGetValues( wid->scrollw , XmNverticalScrollBar , &bar , NULL ) ;
            MCW_widget_geom( bar , &bww , NULL,NULL,NULL ) ;
            fww += bww+LUCK+LUCK ;     /* need more luck here, I guess */
         }
      }
      fhh += fyy+LUCK ;  /* set height to allow for stuff above options */

      XtVaSetValues( wid->shell , XmNwidth , fww , XmNheight , fhh , NULL ) ;
   }

   /** set the popup meter to be nothing at all right now **/

   wid->meter = NULL ;

#if 0
fprintf(stderr,"Widgets done!\n") ;
#endif

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  21 Feb 2001: routine to turn off optional inputs
-------------------------------------------------------------------------*/

void PLUTO_turnoff_options( PLUGIN_interface * plint )
{
   int kk ;

ENTRY("PLUTO_turnoff_options") ;

   /**** sanity checks ****/

   if( plint == NULL || plint->wid == NULL ||
       plint->call_method == PLUGIN_CALL_IMMEDIATELY ) EXRETURN ;

   /**** loop over options */

   for( kk=0 ; kk < plint->option_count ; kk++ ){
      if( plint->option[kk]->mandatory != TRUE )
         XmToggleButtonSetState( plint->wid->opwid[kk]->toggle, False,True ) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   What happens when a plugin interface action button is pressed
-------------------------------------------------------------------------*/

void PLUG_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   PLUGIN_interface *plint = (PLUGIN_interface *) cd ;
   char *wname = XtName(w) ;
   char *mesg=NULL ;
   int close , run , badrun=0 , help ;

ENTRY("PLUG_action_CB") ;

   run   = (strcmp(wname,plint->doit_label)==0) || (strcmp(wname,plint->run_label)==0);
   close = (strcmp(wname,plint->doit_label)==0) || (strcmp(wname,PLUG_quit_label) ==0);
   help  = (strcmp(wname,PLUG_help_label)==0) ;

   if( run ){
      PLUG_fillin_values( plint ) ;       /* load callvalues  */
      plint->opnum = plint->svnum = -1 ;  /* initialize get_  */

      /***** CALL THE PLUGIN !!!! *****/

      MPROBE ;

      SHOW_AFNI_PAUSE ;
#if 0
      mesg = plint->call_func( plint ) ;
#else
      AFNI_CALL_VALU_1ARG( plint->call_func ,
                           char *,mesg , PLUGIN_interface *,plint ) ;
#endif
      SHOW_AFNI_READY ;

      PLUTO_popdown_meter( plint ) ;  /* if the user forgets */

      MPROBE ;

      /********************************/

      badrun = (mesg != NULL) ;
      if( badrun ){
         if( w != NULL ){
            (void) MCW_popup_message( w , mesg , MCW_USER_KILL ) ;
            XBell( XtDisplay(w) , 100 ) ;
         } else {
            fprintf(stderr,"\n%s\a\n",mesg) ;
         }
      }
   }

   if( close && !badrun ) PLUG_delete_window_CB( w , cd , cbs ) ;

   /* 28 Dec 1997: use a scrolling text window if help too big */

   if( help ){
      int nl = THD_linecount( plint->helpstring ) ;
      if( nl < 10 ) MCW_popup_message( plint->wid->label ,
                                       plint->helpstring , MCW_USER_KILL ) ;
      else          new_MCW_textwin  ( plint->wid->label ,
                                       plint->helpstring , TEXT_READONLY ) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------
   What happens when the user selects "Close" from the window
   menu in a plugin interface menu window.
--------------------------------------------------------------------------*/

void PLUG_delete_window_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) cd ;

ENTRY("PLUG_delete_window_CB") ;

   if( plint != NULL && plint->wid != NULL ){
      XtUnmapWidget(plint->wid->shell) ;
      XmUpdateDisplay(plint->wid->shell) ;
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------
   What happens when the user toggles an option's toggle button.
--------------------------------------------------------------------------*/

void PLUG_optional_toggle_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   PLUGIN_option_widgets * ow = (PLUGIN_option_widgets *) cd ;
   int ib , zlen ;
   XtPointer xptr=NULL ;

ENTRY("PLUG_optional_toggle_CB") ;

   /** invert label widget, and switch sensitivity of subvalue widgets **/

   if( ow != NULL ){
#if 0
      if( ow->label != NULL ){
         XtVaGetValues( ow->label , XmNuserData , &xptr , NULL ) ;
         zlen = (int) xptr ;
         if( !zlen ) MCW_invert_widget( ow->label ) ;
      }
#endif

      for( ib=0 ; ib < PLUGIN_MAX_SUBVALUES ; ib++ )
         if( ow->chtop[ib] != NULL )
            XtSetSensitive( ow->chtop[ib] , !XtIsSensitive(ow->chtop[ib]) ) ;
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------------
  Routine to take the interface widgets and put their values into
  place, so that the plugin can get them out again.
----------------------------------------------------------------------------*/

void PLUG_fillin_values( PLUGIN_interface * plint )
{
   int iopt , ib ;
   PLUGIN_option_widgets ** opwid , * ow ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;

ENTRY("PLUG_fillin_values") ;

   /*--- check if there is anything to do ---*/

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;

   /*--- scan thru options ---*/

   opwid = plint->wid->opwid ;

   for( iopt=0 ; iopt < plint->option_count ; iopt++){

      opt = plint->option[iopt] ;   /* option to deal with */
      if( opt == NULL ) continue ;  /* bad? */

      ow = opwid[iopt] ;  /* row of widgets to deal with */

      /*-- find if this option is chosen by the user --*/

      opt->chosen = (int) XmToggleButtonGetState( ow->toggle ) ;

      /*-- scan thru subvalues, and load callvalue.
           note that we do this even for unchosen options. --*/

      for( ib=0 ; ib < opt->subvalue_count ; ib++ ){

        myXtFree( opt->callvalue[ib]  ) ;  /* free any old value here */
        sv = &(opt->subvalue[ib]) ;        /* subvalue to deal with */

        /*-- deal with each type of subvalue --*/

        switch( sv->data_type ){

           /** some type I don't know, so just put NULL in **/

           default:
              opt->callvalue[ib] = NULL ;
           break ;

           /** 11 Jul 2001: overlay color type; send in the color index **/

           case PLUGIN_OVERLAY_COLOR_TYPE:{
              MCW_arrowval *av = (MCW_arrowval *)ow->chooser[ib] ;
              int *iptr ;

              iptr  = (int *)XtMalloc( sizeof(int) ) ;
              *iptr = av->ival ;
              opt->callvalue[ib] = (void *) iptr ;
           }
           break ;

           /** number type: uses arrowval interface;
                            send in the float value **/

           case PLUGIN_NUMBER_TYPE:{
              MCW_arrowval *av = (MCW_arrowval *) ow->chooser[ib] ;
              float *fptr ;

              fptr  = (float *) XtMalloc( sizeof(float) ) ;
              *fptr = av->fval ;
              opt->callvalue[ib] = (void *) fptr ;
           }
           break ;

           /** string type: may be an arrowval or a strval **/

           case PLUGIN_STRING_TYPE:{
              if( sv->string_range_count > 0 ){
                 MCW_arrowval * av = (MCW_arrowval *) ow->chooser[ib] ;
                 char * cptr ;

                 cptr = XtNewString( av->sval ) ;
                 opt->callvalue[ib] = (void *) cptr ;
              } else {
                 PLUGIN_strval * av = (PLUGIN_strval *) ow->chooser[ib] ;
                 char * cptr ;

                 cptr = XmTextFieldGetString( av->textf ) ;
                 opt->callvalue[ib] = (void *) cptr ;
              }
           }
           break ;

           /** dataset type **/

           case PLUGIN_DATASET_TYPE:{
              PLUGIN_dsetval * av = (PLUGIN_dsetval *) ow->chooser[ib] ;
              MCW_idcode * idc ;

              idc = myXtNew( MCW_idcode ) ;
              if( av->dset_choice < 0 )
                 ZERO_IDCODE(*idc) ;
              else
                 *idc = av->dset_link[av->dset_choice].idcode ;

              opt->callvalue[ib] = (void *) idc ;
           }
           break ;

           /** 25 Nov 1996: list of datasets **/

           case PLUGIN_DATASET_LIST_TYPE:{
              PLUGIN_dsetval * av = (PLUGIN_dsetval *) ow->chooser[ib] ;
              MCW_idclist ** llist ;
              int id ;

              llist = myXtNew(MCW_idclist *) ; *llist = av ;

              av->current = 0 ;
              if( av->nchosen <= 0 ){
                 myXtFree(av->idclist) ;
              } else {
                 av->idclist = (MCW_idcode *)
                               XtRealloc( (char *) av->idclist ,
                                          sizeof(MCW_idcode) * av->nchosen ) ;
                 for( id=0 ; id < av->nchosen ; id++ )
                    av->idclist[id] = av->dset_link[av->chosen[id]].idcode ;
              }

              opt->callvalue[ib] = (void *) llist ;
           }
           break ;
           /** timeseries type **/

           case PLUGIN_TIMESERIES_TYPE:{
              PLUGIN_tsval * av = (PLUGIN_tsval *) ow->chooser[ib] ;
              MRI_IMAGE ** imp ;

              imp  = myXtNew(MRI_IMAGE *) ;
              *imp = av->tsim ;

              opt->callvalue[ib] = (void *) imp ;
           }
           break ;

        } /* end of switch over subvalue type */
      }  /* end of scan thru subvalues */
   } /* end of scan thru options */

   EXRETURN ;
}

/*--------------------------------------------------------------------------
  Routine to take the interface widgets and clear their callvalues to NULL.
----------------------------------------------------------------------------*/

void PLUG_freeup_values( PLUGIN_interface * plint )
{
   int iopt , ib ;
   PLUGIN_option * opt ;

ENTRY("PLUG_freeup_values") ;

   /*--- check if there is anything to do ---*/

   if( plint == NULL || plint->option_count == 0 ) EXRETURN ;

   /*--- scan thru options ---*/

   for( iopt=0 ; iopt < plint->option_count ; iopt++){

      opt = plint->option[iopt] ;   /* option to deal with */
      if( opt == NULL ) continue ;  /* bad? */

      /*-- scan thru subvalues, and free all callvalues --*/

      for( ib=0 ; ib < opt->subvalue_count ; ib++ ){
        XtFree( opt->callvalue[ib]  ) ;  /* free any old value here */
        opt->callvalue[ib] = NULL ;
      }

   } /* end of scan thru options */

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Routines to get values from a plugin interface, when passed
   to a plugin.
-------------------------------------------------------------------------*/

char * get_label_from_PLUGIN_interface( PLUGIN_interface * plint )
{
ENTRY("get_label_from_PLUGIN_interface") ;
   if( plint == NULL ) RETURN(NULL) ;
   else                RETURN(plint->label) ;
}

char * get_description_from_PLUGIN_interface( PLUGIN_interface * plint )
{
ENTRY("get_description_from_PLUGIN_interface") ;
   if( plint == NULL ) RETURN(NULL) ;
   else                RETURN(plint->description) ;
}

/*-----------------------------------------------------------------------
   Get the next chosen option, and return its string "tag".
   If there is no "next chosen option", return NULL.
-------------------------------------------------------------------------*/

char * get_optiontag_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   int iopt ;
   PLUGIN_option * opt ;

ENTRY("get_optiontag_from_PLUGIN_interface") ;

   if( plint == NULL ) RETURN(NULL) ;

   iopt = plint->opnum + 1 ;
   while( iopt < plint->option_count ){

      opt = plint->option[iopt++] ;
      if( opt == NULL )   continue ; /* bad? */
      if( ! opt->chosen ) continue ; /* not used this time */

      plint->opnum = iopt-1 ;        /* keep track of which option */
      plint->svnum = 0 ;             /* start at 1st subvalue */
      RETURN(opt->tag) ;
   }

   plint->opnum = plint->option_count ;
   RETURN(NULL) ;
}

/*-----------------------------------------------------------------------
   Return a string encaspulating information about how the plugin
   was called.  This string is created with malloc() and should be
   free()-ed when it is used up.  If NULL is returned, an error
   occurred (and you should be ashamed of yourself).
   -- 31 Aug 1999 -- RWCox
-------------------------------------------------------------------------*/

#include <stdarg.h>

static void blanktrim( char * ch )
{
   int ii , ll ;
   if( ch == NULL ) return ;
   ll = strlen(ch) ; if( ll < 2 ) return ;
   for( ii=ll-1 ; ii > 0 ; ii-- )
      if( isspace(ch[ii]) ) ch[ii] = '\0' ; else break ;
   return ;
}

#undef BUFIT
#define BUFIT(s) do{ strcpy(buf,s); blanktrim(buf); } while(0)

char * PLUTO_commandstring( PLUGIN_interface * plint )
{
   char * outbuf = NULL ;
   PLUGIN_option * opt ;
   PLUGIN_subvalue * sv ;
   int iopt , jsv ;
   char buf[256] ;

ENTRY("PLUTO_commandstring") ;

   if( plint == NULL ) RETURN(outbuf) ;

   BUFIT(plint->label) ;
   outbuf = THD_zzprintf( outbuf , "%s " , buf ) ;  /* start with name */

   if( plint->call_method != PLUGIN_CALL_VIA_MENU ||
       plint->option_count == 0                   ||
       plint->option == NULL                        ) RETURN(outbuf) ;

   /* loop over each option for the plugin */

   for( iopt=0 ; iopt < plint->option_count ; iopt++ ){
      opt = plint->option[iopt] ;
      if( opt == NULL ) continue ;   /* bad? */
      if( ! opt->chosen ) continue ; /* not used this time */

      BUFIT(opt->label) ;
      outbuf = THD_zzprintf( outbuf , "{%s: " , buf ) ;

      /* if this option is used, put a list of its subvalues in the string */

      for( jsv=0 ; jsv < opt->subvalue_count ; jsv++ ){
         sv = &(opt->subvalue[jsv]) ;
         BUFIT(sv->label) ;
         outbuf = THD_zzprintf( outbuf , "%s=" , buf ) ;
         switch( sv->data_type ){

            default:
               outbuf = THD_zzprintf( outbuf,"?" ) ; break ;

            case PLUGIN_OVERLAY_COLOR_TYPE:{
               int *val = (int *) opt->callvalue[jsv] ;
               MCW_DC *dc = plint->im3d->dc ;
               if( val != NULL && *val >= 0 )
                 outbuf = THD_zzprintf( outbuf,"%s",dc->ovc->label_ov[*val] );
               else
                 outbuf = THD_zzprintf( outbuf,"?" ) ;
            }
            break ;

            case PLUGIN_NUMBER_TYPE:{
               float * val = (float *) opt->callvalue[jsv] ;
               if( val != NULL ) outbuf = THD_zzprintf( outbuf,"%g",*val) ;
               else              outbuf = THD_zzprintf( outbuf,"?" ) ;
            }
            break ;

            case PLUGIN_STRING_TYPE:{
                char * val = (char *) opt->callvalue[jsv] ;
                if( val != NULL ){ BUFIT(val); outbuf = THD_zzprintf( outbuf,"%s",buf); }
                else                           outbuf = THD_zzprintf( outbuf,"?" ) ;
            }
            break ;

            case PLUGIN_DATASET_LIST_TYPE:{
               MCW_idclist ** llist = (MCW_idclist **) opt->callvalue[jsv] ;
               int nd = PLUTO_idclist_count(*llist) ;
               outbuf = THD_zzprintf( outbuf , "[%d dsets]" , nd ) ;
            }
            break ;

            case PLUGIN_DATASET_TYPE:{
               MCW_idcode * idc = (MCW_idcode *) opt->callvalue[jsv] ;
               THD_3dim_dataset * dset ;

               dset = PLUTO_find_dset( idc ) ;
               if( dset != NULL ){
                  char * qb = THD_trailname(DSET_HEADNAME(dset),SESSTRAIL+1) ;
                  outbuf = THD_zzprintf( outbuf,"%s",qb) ;
               } else
                  outbuf = THD_zzprintf( outbuf,"?" ) ;
            }
            break ;

            case PLUGIN_TIMESERIES_TYPE:{
               MRI_IMAGE ** imp = (MRI_IMAGE **) opt->callvalue[jsv] ;

               if( imp != NULL && *imp != NULL && (*imp)->name != NULL )
                  outbuf = THD_zzprintf( outbuf,"%s",(*imp)->name ) ;
               else
                  outbuf = THD_zzprintf( outbuf,"?" ) ;
            }
            break ;

         } /* end of switch on subvalue type */

         if( jsv < opt->subvalue_count - 1 )
            outbuf = THD_zzprintf( outbuf,"; ") ;

      } /* end of loop on subvalues */

      outbuf = THD_zzprintf( outbuf , "} " ) ;  /* end of this option */

   } /* end of loop on options */

   RETURN(outbuf) ;
}

/*-------------------------------------------------------------------------
   Find out what the next chosen option is, without actually retrieving it.
---------------------------------------------------------------------------*/

char * peek_optiontag_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   int iopt ;
   PLUGIN_option * opt ;

ENTRY("peek_optiontag_from_PLUGIN_interface") ;

   if( plint == NULL ) RETURN(NULL) ;

   iopt = plint->opnum + 1 ;
   while( iopt < plint->option_count ){
      opt = plint->option[iopt++] ;
      if( opt == NULL ) continue ; /* bad? */
      if( opt->chosen ) RETURN(opt->tag) ;
   }
   RETURN(NULL) ;
}

/*-------------------------------------------------------------------------
   Find out what the next subvalue type is, without actually retrieving it.
   This will be one of the PLUGIN_*_TYPE codes.  If something is wrong,
   the return value will be ILLEGAL_TYPE.  This can be used to detect that
   no more subvalues are present in the option currently being processed.
---------------------------------------------------------------------------*/

int peek_callvalue_type_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   int isv ;
   PLUGIN_option * opt ;

ENTRY("peek_callvalue_type_from_PLUGIN_interface") ;

   if( plint == NULL ) RETURN(ILLEGAL_TYPE) ;
   if( plint->opnum >= plint->option_count ) RETURN(ILLEGAL_TYPE) ;

   opt = plint->option[ plint->opnum ] ;
   if( opt == NULL ) RETURN(ILLEGAL_TYPE) ;

   isv = plint->svnum ;
   if( isv >= opt->subvalue_count ) RETURN(ILLEGAL_TYPE) ;

   RETURN(opt->subvalue[isv].data_type) ;
}

/*-------------------------------------------------------------------------
   Get the next subvalue, which should be of the type given by
   "type".  If it is not, return NULL, otherwise return a "void *",
   which must be properly de-referenced to get the true value:

     type                      output is really
     ------------------        ----------------
     PLUGIN_NUMBER_TYPE        float *         points to number
     PLUGIN_STRING_TYPE        char *          duh
     PLUGIN_DATASET_TYPE       MCW_idcode *    duh
     PLUGIN_TIMESERIES_TYPE    MRI_IMAGE **    duh
     PLUGIN_OVERLAY_COLOR_TYPE int *           points to overlay index

   Following this are convenience routines to do similar work on specific
   data types.
---------------------------------------------------------------------------*/

void * get_callvalue_from_PLUGIN_interface( PLUGIN_interface * plint , int type )
{
   int isv ;
   PLUGIN_option * opt ;

ENTRY("get_callvalue_from_PLUGIN_interface") ;

   if( plint == NULL ) RETURN( NULL );

   opt = plint->option[ plint->opnum ] ;
   if( opt == NULL ) RETURN( NULL );

   isv = plint->svnum ;
   if( isv >= opt->subvalue_count ) RETURN( NULL );

   if( opt->subvalue[isv].data_type != type ) RETURN( NULL );

   plint->svnum ++ ;
   RETURN( opt->callvalue[isv] );
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * get_timeseries_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   MRI_IMAGE ** imp ;

ENTRY("get_timeseries_from_PLUGIN_interface") ;

   imp = (MRI_IMAGE **)
         get_callvalue_from_PLUGIN_interface(plint,PLUGIN_TIMESERIES_TYPE) ;

   if( imp == NULL ) RETURN(NULL) ;
   RETURN(*imp) ;
}

/*----------------------------------------------------------------------------*/

float get_number_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   float * fp ;
ENTRY("get_number_from_PLUGIN_interface") ;
   fp = (float *)get_callvalue_from_PLUGIN_interface(plint,PLUGIN_NUMBER_TYPE) ;
   if( fp == NULL ) RETURN(BAD_NUMBER) ;
   RETURN(*fp) ;
}

/*----------------------------------------------------------------------------*/

int get_overlaycolor_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   int * ip ;
ENTRY("get_overlaycolor_from_PLUGIN_interface") ;
   ip = (int *)get_callvalue_from_PLUGIN_interface(plint,PLUGIN_OVERLAY_COLOR_TYPE) ;
   if( ip == NULL ) RETURN(-1) ;
   RETURN(*ip) ;
}

/*----------------------------------------------------------------------------*/

char * get_string_from_PLUGIN_interface( PLUGIN_interface * plint )
{
ENTRY("get_string_from_PLUGIN_interface") ;
   RETURN(
     (char *) get_callvalue_from_PLUGIN_interface(plint,PLUGIN_STRING_TYPE) );
}

/*----------------------------------------------------------------------------*/

MCW_idcode * get_idcode_from_PLUGIN_interface( PLUGIN_interface * plint )
{
ENTRY("get_idcode_from_PLUGIN_interface") ;
   RETURN(
     (MCW_idcode *)get_callvalue_from_PLUGIN_interface(plint,PLUGIN_DATASET_TYPE) );
}

/*----------------------------------------------------------------------------*/

MCW_idclist * get_idclist_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   MCW_idclist ** llist ;

ENTRY("get_idclist_from_PLUGIN_interface") ;

   llist = (MCW_idclist **)
           get_callvalue_from_PLUGIN_interface(plint,PLUGIN_DATASET_LIST_TYPE) ;

   if( llist != NULL ) RETURN(*llist) ;
   RETURN(NULL) ;
}

/*------------------------------------------------------------------------
   What happens when a dataset chooser button is pressed:
     1) Retrieve the pointers to the data structures associated
          with the button.
     2) Select the datasets that are allowable and make a list of
          their names and idcodes.
     3) Popup a strlist chooser to let the user make the choice.

   24 Nov 1996: add the option for multiple choices, flagged by av->multi.
--------------------------------------------------------------------------*/

void PLUG_choose_dataset_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) cd ;
   PLUGIN_dsetval   * av = NULL ;
   PLUGIN_subvalue  * sv = NULL ;
   Three_D_View     * im3d ;

   THD_session * ss ;
   THD_3dim_dataset * dset ;
   int iss_bot , iss_top , iss , vv ;
   int id , num_dset , qd ;
   MCW_idcode old_idcode ;
   static char ** strlist = NULL ;
   char label[64] ;
   int llen , ltop ;
   char qnam[THD_MAX_NAME] ;

   int          num_old = 0 , qold ;  /* multi-choice stuff */
   MCW_idcode * old_chosen = NULL ;
   int        * indold = NULL ;

ENTRY("PLUG_choose_dataset_CB") ;

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;

   if( plint == NULL || av == NULL ) EXRETURN ;
   sv = av->sv ;
   if( sv == NULL ) EXRETURN ;
   im3d = plint->im3d ;

   /** Select the datasets **/

   if( ! IM3D_VALID(im3d) || (sv->dset_ctrl_mask & SESSION_ALL_MASK) != 0 ){
      iss_bot = 0 ;
      iss_top = GLOBAL_library.sslist->num_sess - 1 ;
   } else {
      iss_bot = iss_top = im3d->vinfo->sess_num ;
   }

   if( im3d != NULL ) vv = im3d->vinfo->view_type ;  /* select view type */
   else               vv = VIEW_ORIGINAL_TYPE ;

   /** Save the current selection, if any **/

   if( ! av->multi ){
      if( av->dset_choice >= 0 && av->dset_choice < av->dset_count )
         old_idcode = av->dset_link[av->dset_choice].idcode ;
      else
         ZERO_IDCODE(old_idcode) ;
   } else {
      for( id=0 ; id < av->nchosen ; id++ ){
         if( av->chosen[id] >= 0 && av->chosen[id] < av->dset_count ){
            num_old++ ;
            old_chosen = (MCW_idcode *) XtRealloc((char *)old_chosen,
                                                  sizeof(MCW_idcode)*num_old) ;
            old_chosen[num_old-1] = av->dset_link[av->chosen[id]].idcode ;
         }
      }
   }

   /** Scan sessions **/

   num_dset = 0 ;
   for( iss=iss_bot ; iss <= iss_top ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;

      /* check datasets in this session */

      for( id=0 ; id < ss->num_dsset ; id++ ){
        dset = ss->dsset[id][vv] ;            if( dset == NULL ) continue ;

        if( sv->dset_anat_mask != 0 && ISANAT(dset) )
          if( ! PLUGIN_dset_check( sv->dset_anat_mask ,
                                   sv->dset_ctrl_mask , dset ) ) continue ;

        if( sv->dset_func_mask != 0 && ISFUNC(dset) )
          if( ! PLUGIN_dset_check( sv->dset_func_mask ,
                                   sv->dset_ctrl_mask , dset ) ) continue ;

        /* if we get here, then this dataset is OK to choose! */

        num_dset++ ;
        av->dset_link = (PLUGIN_dataset_link *)
                         XtRealloc( (char *) av->dset_link ,
                                    sizeof(PLUGIN_dataset_link)*num_dset ) ;

        make_PLUGIN_dataset_link( dset , av->dset_link + (num_dset-1) ) ;
      }

   } /* end of loop over sessions */

   /*--- if nothing was found that fits, then nothing further can happen ---*/

   if( num_dset == 0 ){
      av->dset_count  = 0 ;
      av->dset_choice = -1 ;
      myXtFree(old_chosen) ;
      XBell( XtDisplay(w) , 100 ) ;
      EXRETURN ;
   }

   /*--- 23 Nov 1996: loop over dataset links and patch their titles
                      to include an indicator of the dataset type    ---*/

   ltop = 4 ;
   for( id=0 ; id < num_dset ; id++ ){
      llen = strlen(av->dset_link[id].title) ;
      ltop = MAX(ltop,llen) ;
   }

   for( id=0 ; id < num_dset ; id++ ){
      dset = PLUTO_find_dset( &(av->dset_link[id].idcode) ) ;
      if( ! ISVALID_3DIM_DATASET(dset) ) continue ;
      if( ISANAT(dset) ){
         if( ISANATBUCKET(dset) )         /* 30 Nov 1997 */
            sprintf(qnam,"%-*s [%s:%d]" ,
                    ltop,av->dset_link[id].title ,
                    ANAT_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

         else if( DSET_NUM_TIMES(dset) == 1 )
            sprintf(qnam,"%-*s [%s]" ,
                    ltop,av->dset_link[id].title ,
                    ANAT_prefixstr[dset->func_type] ) ;

         else
            sprintf(qnam,"%-*s [%s:3D+t:%d]" ,
                    ltop,av->dset_link[id].title ,
                    ANAT_prefixstr[dset->func_type] , DSET_NUM_TIMES(dset) ) ;

      } else {
         if( ISFUNCBUCKET(dset) )         /* 30 Nov 1997 */
            sprintf(qnam,"%-*s [%s:%d]" ,
                    ltop,av->dset_link[id].title ,
                    FUNC_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

         else if( DSET_NUM_TIMES(dset) == 1 )
            sprintf(qnam,"%-*s [%s]" ,
                    ltop,av->dset_link[id].title ,
                    FUNC_prefixstr[dset->func_type] ) ;

         else
            sprintf(qnam,"%-*s [%s:3D+t:%d]" ,
                    ltop,av->dset_link[id].title ,
                    FUNC_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;
      }

      if( DSET_COMPRESSED(dset) ) strcat(qnam,"z") ;

      strcpy( av->dset_link[id].title , qnam ) ;
   }

   /*--- find the old choice in the current list, if any ---*/

   av->dset_count = num_dset ;

   if( ! av->multi ){
      if( !ISZERO_IDCODE(old_idcode) ){
         for( id=0 ; id < num_dset ; id++ )
            if( EQUIV_IDCODES(old_idcode,av->dset_link[id].idcode) ) break ;

         if( id < num_dset ) av->dset_choice = id ;
         else                av->dset_choice = -1 ;
      }
   } else {
      qold = 0 ;
      for( qd=0 ; qd < num_old ; qd++ ){
         if( !ISZERO_IDCODE(old_chosen[qd]) ){
            for( id=0 ; id < num_dset ; id++ )
               if( EQUIV_IDCODES(old_chosen[qd],av->dset_link[id].idcode) ) break ;

            if( id < num_dset ){
              qold++ ;
              indold = (int *) XtRealloc((char *)indold , sizeof(int)*qold) ;
              indold[qold-1] = id ;
              av->chosen[qold-1] = id ;
            }
          }
      }
      av->nchosen = qold ;
      if( qold > 0 ){
        qold++ ;
        indold = (int *) XtRealloc((char *)indold , sizeof(int)*qold) ;
        indold[qold-1] = -666 ;
      }
   }

   /*--- make a popup chooser for the user to browse ---*/

   POPDOWN_strlist_chooser ;

   strlist = (char **) XtRealloc( (char *)strlist , sizeof(char *)*num_dset ) ;
   for( id=0 ; id < num_dset ; id++ ) strlist[id] = av->dset_link[id].title ;

   sprintf( label , "AFNI Dataset from\nthe %s" , VIEW_typestr[vv] ) ;

   if( av->multi ){
      MCW_choose_multi_strlist( w , label , mcwCT_multi_mode ,
                                num_dset , indold , strlist ,
                                PLUG_finalize_dataset_CB , (XtPointer) plint ) ;
   } else {
      MCW_choose_strlist( w , label ,
                          num_dset , av->dset_choice , strlist ,
                          PLUG_finalize_dataset_CB , (XtPointer) plint ) ;
   }

   myXtFree(indold) ; myXtFree(old_chosen) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Called when the user actually selects a dataset from the chooser.
  This routine just changes the original pushbutton label, and
  notes the index of the choice in the right place, for later retrieval.
-------------------------------------------------------------------------*/

void PLUG_finalize_dataset_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) fd ;
   PLUGIN_dsetval   * av = NULL ;
   XmString           xstr ;
   int id ;
   char str[THD_MAX_NAME] ;

ENTRY("PLUG_finalize_dataset_CB") ;

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;
   if( plint == NULL || av == NULL ) EXRETURN ;

   if( ! av->multi ){
      xstr = XmStringCreateLtoR( av->dset_link[cbs->ival].title ,
                                 XmFONTLIST_DEFAULT_TAG ) ;
   } else {
      sprintf( str , "[%d]%s" , cbs->nilist , av->dset_link[cbs->ival].title ) ;
      xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   }
   XtVaSetValues( w , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;

   if( ! av->multi ){
      av->dset_choice = cbs->ival ;
   } else {
      av->nchosen = cbs->nilist ;
      av->chosen  = (int *) XtRealloc( (char *) av->chosen ,
                                       sizeof(int) * av->nchosen ) ;
      for( id=0 ; id < av->nchosen ; id++ ) av->chosen[id] = cbs->ilist[id] ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Fill in a PLUGIN_dataset_link
-------------------------------------------------------------------------*/

void make_PLUGIN_dataset_link( THD_3dim_dataset * dset ,
                               PLUGIN_dataset_link * dsl )
{
   char nam[THD_MAX_NAME] ;
   char * tnam ;

ENTRY("make_PLUGIN_dataset_link") ;

   /*-- sanity checks --*/

   if( dsl == NULL ) EXRETURN ;

   if( ! ISVALID_3DIM_DATASET(dset) ){
      strcpy( dsl->title , "* garbage *" ) ;
      ZERO_IDCODE( dsl->idcode ) ;
      EXRETURN ;
   }

   /*-- make title (cf. AFNI_set_window_titles) --*/

   strcpy( nam , dset->dblk->diskptr->directory_name ) ;
   strcat( nam , dset->dblk->diskptr->filecode ) ;
   tnam = THD_trailname(nam,SESSTRAIL+1) ;
   MCW_strncpy( dsl->title , tnam , PLUGIN_STRING_SIZE ) ;

   /*-- copy idcode --*/

   dsl->idcode = dset->idcode ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Determine if a dataset passes the type_mask and ctrl_mask
  criteria for acceptability.
-------------------------------------------------------------------------*/

int PLUTO_dset_check( int anat_mask, int func_mask,
                      int ctrl_mask, THD_3dim_dataset * dset )
{
   int iv=0 ;

   if( ISANAT(dset) )
      iv = PLUGIN_dset_check( anat_mask , ctrl_mask , dset ) ;
   else if( ISFUNC(dset) )
      iv = PLUGIN_dset_check( func_mask , ctrl_mask , dset ) ;

   return iv ;
}

int PLUGIN_dset_check( int type_mask , int ctrl_mask , THD_3dim_dataset * dset )
{
   int itmp ;

ENTRY("PLUGIN_dset_check") ;

   if( ! ISVALID_3DIM_DATASET(dset) ) RETURN(0) ;

   if( ((1 << dset->func_type) & type_mask) == 0 ) RETURN(0) ;

   itmp = (DSET_NVALS(dset) > 1) ? DIMEN_4D_MASK : DIMEN_3D_MASK ;
   if( (itmp & ctrl_mask) == 0 ) RETURN(0) ;

   if( !DSET_INMEMORY(dset) && (ctrl_mask & WARP_ON_DEMAND_MASK) == 0 ) RETURN(0) ;

   itmp = DSET_PRINCIPAL_VALUE(dset) ;  /* get the type of */
   itmp = DSET_BRICK_TYPE(dset,itmp) ;  /* the "principal" brick */

   if( itmp == MRI_byte    && (ctrl_mask & BRICK_BYTE_MASK)    == 0 ) RETURN(0) ;
   if( itmp == MRI_short   && (ctrl_mask & BRICK_SHORT_MASK)   == 0 ) RETURN(0) ;
   if( itmp == MRI_float   && (ctrl_mask & BRICK_FLOAT_MASK)   == 0 ) RETURN(0) ;
   if( itmp == MRI_complex && (ctrl_mask & BRICK_COMPLEX_MASK) == 0 ) RETURN(0) ;
   if( itmp == MRI_rgb     && (ctrl_mask & BRICK_RGB_MASK)     == 0 ) RETURN(0) ;

   RETURN(1) ;
}

/*-------------------------------------------------------------------------
   Loop over a bunch of dataset links and patch their
   titles to include an indicator of the dataset type, etc.
   23 October 1998 -- RWCox
---------------------------------------------------------------------------*/

void patch_PLUGIN_dataset_links( int ndsl , PLUGIN_dataset_link * dsl )
{
   int id , ltop , llen ;
   char qnam[THD_MAX_NAME] ;
   THD_3dim_dataset * dset ;

ENTRY("patch_PLUGIN_dataset_links") ;

   if( ndsl < 1 || dsl == NULL ) EXRETURN ;

   ltop = 4 ;
   for( id=0 ; id < ndsl ; id++ ){    /* find longest string */
      llen = strlen(dsl[id].title) ;
      ltop = MAX(ltop,llen) ;
   }

   /* patch each title string */

   for( id=0 ; id < ndsl ; id++ ){
      dset = PLUTO_find_dset( &(dsl[id].idcode) ) ;  /* get the dataset */
      if( ! ISVALID_3DIM_DATASET(dset) ) continue ;  /* bad news for Bozo */

      if( ISANAT(dset) ){
         if( ISANATBUCKET(dset) )         /* 30 Nov 1997 */
            sprintf(qnam,"%-*s [%s:%d]" ,
                    ltop,dsl[id].title ,
                    ANAT_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

         else if( DSET_NUM_TIMES(dset) == 1 )
            sprintf(qnam,"%-*s [%s]" ,
                    ltop,dsl[id].title ,
                    ANAT_prefixstr[dset->func_type] ) ;

         else
            sprintf(qnam,"%-*s [%s:3D+t:%d]" ,
                    ltop,dsl[id].title ,
                    ANAT_prefixstr[dset->func_type] , DSET_NUM_TIMES(dset) ) ;

      } else {
         if( ISFUNCBUCKET(dset) )         /* 30 Nov 1997 */
            sprintf(qnam,"%-*s [%s:%d]" ,
                    ltop,dsl[id].title ,
                    FUNC_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

         else if( DSET_NUM_TIMES(dset) == 1 )
            sprintf(qnam,"%-*s [%s]" ,
                    ltop,dsl[id].title ,
                    FUNC_prefixstr[dset->func_type] ) ;

         else
            sprintf(qnam,"%-*s [%s:3D+t:%d]" ,
                    ltop,dsl[id].title ,
                    FUNC_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;
      }

      if( DSET_COMPRESSED(dset) ) strcat(qnam,"z") ;

      strcpy( dsl[id].title , qnam ) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------
   Popup a chooser list of datasets that meet some criteria.
   Note that only one such chooser will be popped up at any given time.
   This routine is for use in user-written plugins with custom interfaces.
   23 October 1998 -- RWCox

   w = widget to popup near

   vv = view type of datasets (cf. VIEW_*_TYPE from 3ddata.h)
           [one way to select this is plint->im3d->vinfo->view_type]

   multi = 1 to allow selection of multiple datasets
         = 0 to allow selection of only 1 dataset at a time

   chk_func = int function( THD_3dim_dataset * dset, void * cd ) ;
                If this function pointer is not NULL, it will be called to
                check if each dataset should be allowed in.  A zero return
                value means don't allow; any other return value means dset
                will be in the displayed list.  If chk_func is NULL, then
                all datasets known to AFNI will be allowed.  [The function
                PLUTO_dset_check() may be useful inside chk_func.]

   cb_func  = void function( int num, THD_3dim_dataset ** dslist, void * cd ) ;
                This function pointer must not be NULL.  It will be called
                when the user makes a choice on the popup chooser.
                The value num will be the number of datasets chosen.
                dslist[i] will be a pointer to the i-th dataset, for
                i=0..num-1.  If multi was 0, then num will be 1 and the
                single selected dataset will be pointed to by dlist[0].

   cd = A pointer to anything the user likes.  It will be passed to
          chk_func and cb_func, as described above.  (Can be NULL.)
--------------------------------------------------------------------------*/

static int                   num_user_dset     = 0 ;
static PLUGIN_dataset_link * user_dset_link    = NULL ;
static char **               user_dset_strlist = NULL ;
static int                   user_dset_numds   = 0 ;
static THD_3dim_dataset **   user_dset_dslist  = NULL ;
static void_func *           user_dset_cb_func = NULL ;
static void *                user_dset_cb_data = NULL ;

void PLUTO_popup_dset_chooser( Widget w , int vv , int multi ,
                               int_func * chk_func ,
                               void_func * cb_func , void * cd )
{
   THD_session * ss ;
   THD_3dim_dataset * dset ;
   int iss_bot , iss_top , iss ;
   int id ;
   char label[64] ;

ENTRY("PLUTO_popup_dset_chooser") ;

   if( w == NULL            || cb_func == NULL     ||
       vv < FIRST_VIEW_TYPE || vv > LAST_VIEW_TYPE   ) EXRETURN ;

   /** Scan sessions **/

   iss_bot  = 0 ;
   iss_top  = GLOBAL_library.sslist->num_sess - 1 ;
   num_user_dset = 0 ;

   for( iss=iss_bot ; iss <= iss_top ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;

      /* check datasets from this session */

      for( id=0 ; id < ss->num_dsset ; id++ ){
         dset = ss->dsset[id][vv] ;    if( dset == NULL ) continue ;
#if 0
         if( chk_func != NULL && chk_func(dset,cd) == 0 ) continue ; /* skip */
#else
         { int cval=1 ;
           AFNI_CALL_VALU_2ARG( chk_func ,
                                int,cval , THD_3dim_dataset *,dset, void *,cd ) ;
           if( cval == 0 ) continue ;
         }
#endif

         num_user_dset++ ;
         user_dset_link = (PLUGIN_dataset_link *)
                          XtRealloc( (char *) user_dset_link ,
                                     sizeof(PLUGIN_dataset_link)*num_user_dset ) ;

         make_PLUGIN_dataset_link( dset , user_dset_link + (num_user_dset-1) ) ;

      } /* end of loop over datasets */

   } /* end of loop over sessions */

   /*--- if nothing was found that fits, then nothing further can happen ---*/

   if( num_user_dset == 0 ){
      myXtFree(user_dset_link) ; BEEPIT ;
      MCW_popup_message( w ,
                        "No datasets that meet this\ncriterion are available!" ,
                        MCW_USER_KILL|MCW_TIMER_KILL ) ;
      EXRETURN ;
   }

   /*--- make a popup chooser for the user to browse ---*/

   POPDOWN_strlist_chooser ;  /* death to the old regime */

   /* fix the dataset titles to be more fun */

   patch_PLUGIN_dataset_links( num_user_dset , user_dset_link ) ;

   /* make an array of pointers to all the titles */

   user_dset_strlist = (char **) XtRealloc( (char *) user_dset_strlist ,
                                            sizeof(char *) * num_user_dset ) ;
   for( id=0 ; id < num_user_dset ; id++ )
      user_dset_strlist[id] = user_dset_link[id].title ;

   /* label for the top of the chooser */

   sprintf( label , "AFNI Dataset from\nthe %s" , VIEW_typestr[vv] ) ;

   /* and take it away, Goldie */

   user_dset_cb_func = cb_func ;
   user_dset_cb_data = cd ;

   if( multi ){
      MCW_choose_multi_strlist( w , label , mcwCT_multi_mode ,
                                num_user_dset , NULL , user_dset_strlist ,
                                PLUG_finalize_user_dset_CB , NULL ) ;
   } else {
      MCW_choose_strlist( w , label ,
                          num_user_dset , -1 , user_dset_strlist ,
                          PLUG_finalize_user_dset_CB , NULL ) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Called when the user actually selects a dataset from the chooser.
   Will call the user's pitiful and loathsome routine.
   23 October 1998 -- RWCox
-------------------------------------------------------------------------*/

void PLUG_finalize_user_dset_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   int id , jd , num ;

ENTRY("PLUG_finalize_user_dset_CB") ;

   if( cbs == NULL || cbs->nilist < 1 ) EXRETURN ;

   user_dset_numds = num = cbs->nilist ;

   user_dset_dslist = (THD_3dim_dataset **)
                         XtRealloc( (char *) user_dset_dslist ,
                                    sizeof(THD_3dim_dataset *) * num ) ;

   for( id=0 ; id < num ; id++ ){
      jd = cbs->ilist[id] ;
      user_dset_dslist[id] = PLUTO_find_dset( &(user_dset_link[jd].idcode) ) ;
   }

#if 0
   user_dset_cb_func( num , user_dset_dslist , user_dset_cb_data ) ;
#else
   AFNI_CALL_VOID_3ARG( user_dset_cb_func ,
                        int,num , THD_3dim_dataset **,user_dset_dslist ,
                        void *,user_dset_cb_data ) ;
#endif

   EXRETURN ;
}

/*----------------------------------------------------------------------
   What happens when a timeseries chooser button is pressed:
     Popup a timeseries chooser window.
------------------------------------------------------------------------*/

void PLUG_choose_timeseries_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) cd ;
   PLUGIN_tsval     * av = NULL ;
   PLUGIN_subvalue  * sv = NULL ;
   Three_D_View     * im3d ;
   int init_ts ;

ENTRY("PLUG_choose_timeseries_CB") ;

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;

   if( plint == NULL || av == NULL ) EXRETURN ;
   sv = av->sv ;
   if( sv == NULL ) EXRETURN ;
   im3d = plint->im3d ;

   av->tsimar = GLOBAL_library.timeseries ; /* to choose amongst */
   if( av->tsimar==NULL || IMARR_COUNT(av->tsimar)==0 ){
      av->ts_choice = -1 ;
      av->tsim      = NULL ;
      XBell( XtDisplay(w) , 100 ) ;
      EXRETURN ;
   }

   init_ts = AFNI_ts_in_library( av->tsim ) ;

   MCW_choose_timeseries( w , "Choose Timeseries" ,
                          av->tsimar , init_ts ,
                          PLUG_finalize_timeseries_CB , (XtPointer) plint ) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Called when the user actually selects a timeseries from the chooser.
  This routine just changes the original pushbutton label, and
  notes the index of the choice in the right place, for later retrieval.
-------------------------------------------------------------------------*/

void PLUG_finalize_timeseries_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) fd ;
   PLUGIN_tsval     * av = NULL ;
   XmString           xstr ;
   int                its ;

ENTRY("PLUG_finalize_timeseries_CB") ;

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;
   if( plint == NULL || av == NULL || av->tsimar == NULL ) EXRETURN ;
   if( cbs->reason != mcwCR_timeseries ) EXRETURN ;  /* error */

   /** store the choice, and change the widget label **/

   its = cbs->ival ;
   if( its >= 0 && its < IMARR_COUNT(av->tsimar) ){
      av->tsim      = IMARR_SUBIMAGE(av->tsimar,its) ;
      av->ts_choice = its ;

      xstr = XmStringCreateLtoR( av->tsim->name , XmFONTLIST_DEFAULT_TAG ) ;
      XtVaSetValues( w , XmNlabelString , xstr , NULL ) ;
      XmStringFree( xstr ) ;
   }

   EXRETURN ;
}

/********************************************************************************
   Routine to interface to AFNI, after plugins are read in.
*********************************************************************************/

void AFNI_plugin_button( Three_D_View * im3d )
{
   AFNI_plugin_array * exten = GLOBAL_library.plugins ;
   AFNI_plugin * plug ;
   int pp , ipl , nbut , npbut , kpl ;
   Widget rc , mbar , menu , cbut , pbut , wpar , sep ;
   XmString xstr ;
   PLUGIN_interface ** plintar ;  /* 28 Nov 2000 */

ENTRY("AFNI_plugin_button") ;

   /*-- check inputs for legality --*/

   if( exten == NULL      ||
       ! IM3D_VALID(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;

   /*-- 23 Sep 2000: count number of buttons that will be made --*/

   npbut = 0 ;
   for( pp=0 ; pp < exten->num ; pp++ ){
      plug = exten->plar[pp] ;
      for( ipl=0 ; ipl < plug->interface_count ; ipl++ ){
         npbut++ ;
      }
   }

   /*-- make arrays to hold the plugin buttons (etc.),
        so that plugins can be started from layouts (in afni_splash.c) --*/

   /* from malloc    12 Feb 2009 [lesstif patrol] */
   im3d->vwid->nplugbut = npbut ;
   im3d->vwid->plugbut  = (Widget *)            calloc(1, sizeof(Widget)            *npbut) ;
   im3d->vwid->pluglab  = (char **)             calloc(1, sizeof(char *)            *npbut) ;
   im3d->vwid->plugint  = (PLUGIN_interface **) calloc(1, sizeof(PLUGIN_interface *)*npbut) ;

   /*-- create menu bar --*/

   wpar = im3d->vwid->dmode->mbar_rowcol ;

   rc =  XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , wpar ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   mbar = XmCreateMenuBar( rc , "dialog" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , True  ,
                     XmNbackground   , im3d->dc->ovc->pixov_brightest ,
                  NULL ) ;
   XtManageChild( mbar ) ;

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;

   VISIBILIZE_WHEN_MAPPED(menu) ;
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(menu) ;

   xstr = XmStringCreateLtoR( "Plugins" , XmFONTLIST_DEFAULT_TAG ) ;
   cbut = XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , mbar ,
               XmNlabelString , xstr ,
               XmNsubMenuId , menu ,
               XmNmarginWidth  , 0 ,
               XmNmarginHeight , 0 ,
               XmNmarginBottom , 0 ,
               XmNmarginTop    , 0 ,
               XmNmarginRight  , 0 ,
               XmNmarginLeft   , 0 ,
               XmNtraversalOn  , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree( xstr ) ;

   MCW_register_help( cbut , "Pressing this drops down\n"
                             "the menu of the plugin\n"
                             "programs loaded when\n"
                             "AFNI started."
                    ) ;
   MCW_register_hint( cbut , "Plugins menu" ) ;

   /* macro to create a new menu button */

#define MENU_BUT(pl)                                                      \
   do{                                                                    \
      xstr = XmStringCreateLtoR( (pl)->label , XmFONTLIST_DEFAULT_TAG ) ; \
      pbut = XtVaCreateManagedWidget(                                     \
               "dialog" , xmPushButtonWidgetClass , menu ,                \
                  XmNlabelString , xstr ,                                 \
                  XmNmarginHeight , 0 ,                                   \
                  XmNuserData , (XtPointer) im3d ,                        \
                  XmNtraversalOn , True  ,                                \
                  XmNinitialResourcesPersistent , False ,                 \
               NULL ) ;                                                   \
      im3d->vwid->plugbut[npbut] = pbut ;            /* 23 Sep 2000 */    \
      im3d->vwid->plugint[npbut] = (pl) ;                                 \
      im3d->vwid->pluglab[npbut] = XtNewString((pl)->label) ;             \
      XtAddCallback( pbut , XmNactivateCallback ,                         \
                     PLUG_startup_plugin_CB , (XtPointer)(pl) ) ;         \
      XmStringFree(xstr) ;                                                \
      if( (pl)->hint != NULL ) MCW_register_hint( pbut , (pl)->hint ) ;   \
      if( (pl)->butcolor[0] != '\0' )                                     \
         MCW_set_widget_bg( pbut , (pl)->butcolor , 0 ) ;                 \
   } while(0)

   /*** top of menu = a label to click on that does nothing at all ***/

   xstr = XmStringCreateLtoR( "-- Cancel --" , XmFONTLIST_DEFAULT_TAG ) ;
   wtemp = XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree(xstr) ; LABELIZE(wtemp) ;

   sep = XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   /*-- 28 Nov 2000: allow user to specify that
                     button should be alphabetized --*/

   /* make single array of all plugin interfaces */

   plintar = (PLUGIN_interface **) malloc(sizeof(PLUGIN_interface *)*npbut) ;
   for( kpl=pp=0 ; pp < exten->num ; pp++ ){
      plug = exten->plar[pp] ;
      for( ipl=0 ; ipl < plug->interface_count ; ipl++ ){
         plintar[kpl++] = plug->interface[ipl] ;
      }
   }

   /* sort this array, if desired */

   if( !AFNI_noenv("AFNI_PLUGINS_ALPHABETIZE") ){
      int qq , ss ;
      PLUGIN_interface * tpl ;

      do{                                       /* bubble sort */
        for( ss=qq=0 ; qq < npbut-1 ; qq++ ){
           if( strcasecmp(plintar[qq]->label,plintar[qq+1]->label) > 0 ){
             tpl           = plintar[qq+1] ;
             plintar[qq+1] = plintar[qq] ;
             plintar[qq]   = tpl ;
             ss++ ;
           }
        }
      } while( ss ) ;
   }

   /*-- prepare to make the actual buttons --*/

   nbut  = 2 ;  /* allow for the Cancel label and separator */
   npbut = 0 ;  /* actual number of buttons */

   /*** make buttons for each interface ***/

   for( kpl=pp=0 ; pp < exten->num ; pp++ ){
      plug = exten->plar[pp] ;
      for( ipl=0 ; ipl < plug->interface_count ; ipl++ ){
         MENU_BUT( plintar[kpl] ) ;  /* uses npbut */
         nbut++ ; npbut++ ; kpl++ ;
      }
   }

   free(plintar) ;  /* don't need no more */

   if( nbut > COLSIZE ){
      int ncol = (nbut-2)/COLSIZE + 1 ;
      XtDestroyWidget(sep) ;
      XtVaSetValues( menu ,
                        XmNpacking , XmPACK_COLUMN ,
                        XmNnumColumns , ncol ,
                     NULL ) ;
   }

   XtManageChild( rc ) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------
   Routine that actually starts up a plugin when the user makes
   his or her choice from the menu created above.
--------------------------------------------------------------------------*/

void PLUG_startup_plugin_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   PLUGIN_interface *plint = (PLUGIN_interface *) cd ;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) cbd ;
   char *mesg=NULL ;
   Widget wpop ;
   Three_D_View *im3d = NULL ;

ENTRY("PLUG_startup_plugin_CB") ;

   if( plint == NULL ) EXRETURN ;  /* error? */

   if( w != NULL ){
     XtVaGetValues( w , XmNuserData , &im3d , NULL ) ;  /* set controller from */
     plint->im3d = im3d ;                               /* data on menu button */
   } else if( cbs != NULL ){                            /* 21 Jul 2003 */
     plint->im3d = (Three_D_View *) cbs ;
     cbs = NULL ;
   }
   if( plint->im3d == NULL ) plint->im3d = AFNI_find_open_controller() ;

   /*-- if no interface is needed, just call it --*/

   if( plint->call_method == PLUGIN_CALL_IMMEDIATELY ){

STATUS("calling plugin") ;

      MPROBE ;

      SHOW_AFNI_PAUSE ;
#if 0
      mesg = plint->call_func( plint ) ;
#else
      AFNI_CALL_VALU_1ARG( plint->call_func ,
                           char *,mesg , PLUGIN_interface *,plint ) ;
#endif
      SHOW_AFNI_READY ;

      MPROBE ;

      if( mesg != NULL ){
         if( w != NULL ){
            (void) MCW_popup_message( w , mesg , MCW_USER_KILL ) ;
            XBell( XtDisplay(w) , 100 ) ;
         } else {
            fprintf(stderr,"\n%s\a\n",mesg) ;
         }
      }
      EXRETURN ;
   }

   /*-- if widgets not created yet, create them now --*/

   if( plint->wid == NULL )
      PLUG_setup_widgets( plint , GLOBAL_library.dc ) ;

   /*-- set labels to go on shell widget and icon;
        include the [] controller window index, if possible --*/

   { char ttl[PLUGIN_STRING_SIZE] ;

     sprintf(ttl , "%s%s" , AFNI_controller_label(plint->im3d) , plint->label ) ;

     XtVaSetValues( plint->wid->shell ,
                       XmNtitle     , ttl , /* top of window */
                       XmNiconName  , ttl , /* label on icon */
                    NULL ) ;
   }

   /*-- if possible, find where this popup should go --*/

   wpop = plint->wid->shell ;

   if( cbs != NULL && cbs->event != NULL
                   && cbs->event->type == ButtonRelease ){

      XButtonEvent * xev = (XButtonEvent *) cbs->event ;
      int xx = (int)xev->x_root , yy = (int)xev->y_root ;
      int ww,hh , sw,sh ;

STATUS("trying to position popup") ;

      MCW_widget_geom( wpop , &ww,&hh , NULL,NULL ) ; /* widget width and height */
      sw = WidthOfScreen (XtScreen(wpop)) ;           /* screen width and height */
      sh = HeightOfScreen(XtScreen(wpop)) ;

      if( xx+ww+3 >= sw && ww <= sw ) xx = sw-ww ;    /* make sure is on screen */
      if( yy+hh+3 >= sh && hh <= sh ) yy = sh-hh ;

      XtVaSetValues( wpop , XmNx , xx , XmNy , yy , NULL ) ;
   }

   /*-- popup the widgets that control this plugin --*/

STATUS("popping up interface") ;

   XtMapWidget( wpop ) ;
   RWC_visibilize_widget( wpop ) ; /* 27 Sep 2000 */
   PLUTO_cursorize( plint->wid->shell ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Routine to add a dataset to the AFNI global collection.
   Return value is 0 if all is OK, 1 if bad things happened.
   "action_flag" is the OR (|) of various possibilities:
      DSET_ACTION_NONE         == do nothing
      DSET_ACTION_MAKE_CURRENT == make this the currently viewed dataset
-----------------------------------------------------------------------------*/

int PLUTO_add_dset( PLUGIN_interface *plint ,
                    THD_3dim_dataset *dset , int action_flag )
{
   Three_D_View *im3d ;
   THD_session *sess ;
   int iss , vv , id ;
   int make_current = (action_flag & DSET_ACTION_MAKE_CURRENT) ;

ENTRY("PLUTO_add_dset") ;

   /** sanity check **/

   if( plint == NULL || ! ISVALID_3DIM_DATASET(dset) ) RETURN(1) ;

   /** find some indices **/

   im3d = plint->im3d ;
   iss  = IM3D_VALID(im3d) ? im3d->vinfo->sess_num : 0 ;
   sess = GLOBAL_library.sslist->ssar[iss] ;
   vv   = dset->view_type ;

   /** add the dataset to the session **/

   id = sess->num_dsset ;
   if( id >= THD_MAX_SESSION_SIZE ){
     fprintf(stderr,"*** Overflow session dataset limit ***\n") ;
     RETURN(1) ;
   }
   sess->dsset[id][vv] = dset ;
   sess->num_dsset ++ ;

   /** make sure the dataset is properly fit into the situation **/

   POPDOWN_strlist_chooser ;  /* added dataset --> old choosers are invalid */

#if 0
   THD_load_statistics( dset ) ;
   THD_write_3dim_dataset( NULL,NULL , dset , True ) ;
#else
   DSET_overwrite(dset) ;
#endif

   if( dset->anat_parent == NULL )                          /* if() added 14 Dec 1999 */
     AFNI_force_adoption( sess , GLOBAL_argopt.warp_4D ) ;

   AFNI_make_descendants( GLOBAL_library.sslist ) ;

   /** if desired, jump to this puppy in the viewer **/

   if( make_current && IM3D_VALID(im3d) ){
     if( ISANAT(dset) )
       im3d->vinfo->anat_num = sess->num_dsset - 1 ;
     else
       im3d->vinfo->func_num = sess->num_dsset - 1 ;

     AFNI_initialize_view( im3d->anat_now , im3d ) ;
   }

   THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_ANY ) ;
   RETURN(0) ;
}

/*---------------------------------------------------------------------
   Routine to make a copy of a dataset, with data attached.
   [Moved into edt_fullcopy.c -- RWCox, 07 Oct 1998]
-----------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_copy_dset( THD_3dim_dataset * dset , char * new_prefix )
{
   THD_3dim_dataset * new_dset ;
   int ival , ityp , nbytes , nvals ;
   void * new_brick , * old_brick ;

ENTRY("PLUTO_copy_dset") ;

   new_dset = EDIT_full_copy( dset , new_prefix ) ;
   RETURN(new_dset) ;
}

/*----------------------------------------------------------------------
   Routine to force AFNI to redisplay controllers that are attached
   to a given dataset.  (Feb 1998)
------------------------------------------------------------------------*/

void PLUTO_dset_redisplay( THD_3dim_dataset *dset )
{
   PLUTO_dset_redisplay_mode( dset , REDISPLAY_OPTIONAL ) ;
}

/*---- 23 Oct 1998: superseded above routine with this one; RWCox -----*/

void PLUTO_dset_redisplay_mode( THD_3dim_dataset *dset , int mode )
{
   Three_D_View * im3d ;
   int ii , amode , fmode ;

ENTRY("PLUTO_dset_redisplay_mode") ;

   if( mode == REDISPLAY_OPTIONAL ){
      amode = REDISPLAY_ALL ;
      fmode = REDISPLAY_OVERLAY ;
   } else {
      amode = fmode = mode ;
   }

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      im3d = GLOBAL_library.controllers[ii] ;
      if( ! IM3D_OPEN(im3d) ) continue ;

      if( ! ISVALID_DSET(dset) ){
         im3d->anat_voxwarp->type = ILLEGAL_TYPE ;
         im3d->fim_voxwarp->type  = ILLEGAL_TYPE ;
         AFNI_reset_func_range( im3d ) ;
         IM3D_VEDIT_FORCE(im3d) ; /* 01 Feb 2008 */
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
      } else if( im3d->anat_now == dset ){
         im3d->anat_voxwarp->type = ILLEGAL_TYPE ;
         AFNI_reset_func_range( im3d ) ;
         AFNI_imseq_clearstat( im3d ) ;
         IM3D_VEDIT_FORCE(im3d) ; /* 01 Feb 2008 */
         AFNI_set_viewpoint( im3d , -1,-1,-1 , amode ) ;
      } else if( im3d->fim_now == dset ){
         im3d->fim_voxwarp->type = ILLEGAL_TYPE ;
         AFNI_reset_func_range( im3d ) ;
         AFNI_imseq_clearstat( im3d ) ;
         IM3D_VEDIT_FORCE(im3d) ; /* 01 Feb 2008 */
         AFNI_redisplay_func( im3d ) ;
      }
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------
   Routine to inform AFNI that some dataset names are now different,
   so stuff should be done about it.
------------------------------------------------------------------------*/

void PLUTO_fixup_names(void)
{
   Three_D_View * im3d ;
   int ii ;

ENTRY("PLUTO_fixup_names") ;

   POPDOWN_strlist_chooser ;  /* get rid of any dataset chooser that is open */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      im3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_OPEN(im3d) )
         AFNI_set_window_titles( im3d ) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Let the user popup a message
-------------------------------------------------------------------------*/

void PLUTO_popup_worker( PLUGIN_interface * plint , char * mesg , int flag )
{
   Widget w = NULL ;
   Three_D_View * im3d ;
   int ii ;

ENTRY("PLUTO_popup_worker") ;

   if( mesg == NULL || strlen(mesg) == 0 ) EXRETURN ;

   /* find a widget to popup next to */

   if( plint->wid != NULL && plint->wid->label != NULL ){
      w = plint->wid->label ;
   } else {
      im3d = plint->im3d ;
      if( !IM3D_OPEN(im3d) ) im3d = AFNI_find_open_controller() ;
      w = im3d->vwid->top_shell ;
   }

   if( w != NULL ){
      if( flag >= 0 )
         (void) MCW_popup_message( w , mesg , flag ) ;
      else
         (void) new_MCW_textwin( w , mesg , TEXT_READONLY ) ;
   } else {
      fprintf(stderr,"\n%s\a\n",mesg) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------*/

void PLUTO_beep(void)
{
   XBell( GLOBAL_library.dc->display , 100 ) ;
   return ;
}

/*------------------------------------------------------------------*/

#include <ctype.h>

static int PLUTO_strncmp( char * aa , char * bb , int nn ) /* 09 Oct 2000 */
{
   int ii ;

   if( aa == bb )                 return 0 ;  /* special cases */
   if( aa == NULL || bb == NULL ) return 1 ;

   for( ii=0 ; ii < nn ; ii++,aa++,bb++ ){
      if( *aa == '\0'  && *bb == '\0' )  return 0 ; /* got to end all same! */
      if( *aa == '\0'  || *bb == '\0' )  return 1 ; /* premature end of one */
      if( isspace(*aa) || isspace(*bb) ) continue ; /* don't compare blanks */
      if( toupper(*aa) != toupper(*bb) ) return 1 ; /* case insensitive     */
   }

   return 0 ;                                       /* finished max # chars */
}

/*------------------------------------------------------------------*/

int PLUTO_string_index( char * target , int num , char * source[] )
{
   int ii ;

   if( num <= 0 || source == NULL || target == NULL ) return -1 ;

   for( ii=0 ; ii < num ; ii++ )
      if( PLUTO_strncmp(target,source[ii],PLUGIN_STRING_SIZE) == 0 ) return ii ;

   return -1 ;
}

/*-----------------------------------------------------------------------
  Function to let an IMMEDIATE mode plugin let AFNI know where its
  toplevel shell is -- 22 Sep 2000 -- RWCox.
-------------------------------------------------------------------------*/

void PLUTO_set_topshell( PLUGIN_interface * plint , Widget ts )
{
ENTRY("PLUTO_set_topshell") ;

   if( plint == NULL                                ||
       plint->wid != NULL                           ||
       ts == (Widget) 0                             ||
       plint->call_method != PLUGIN_CALL_IMMEDIATELY  ) EXRETURN ;

   plint->wid        = myXtNew(PLUGIN_widgets) ;
   plint->wid->shell = ts ;
   plint->wid->meter = NULL ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Routines to put a progress meter on the top of the interface shell
-------------------------------------------------------------------------*/

void PLUTO_popup_meter( PLUGIN_interface * plint )
{
ENTRY("PLUTO_popup_meter") ;

   if( plint == NULL             || plint->wid == NULL       ||
       plint->wid->shell == NULL || plint->wid->meter != NULL  ) EXRETURN ;

   plint->wid->meter = MCW_popup_meter( plint->wid->shell , METER_TOP_WIDE ) ;
   EXRETURN ;
}

void PLUTO_popdown_meter( PLUGIN_interface * plint )
{
ENTRY("PLUTO_popdown_meter") ;

   if( plint == NULL             || plint->wid == NULL       ||
       plint->wid->shell == NULL || plint->wid->meter == NULL  ) EXRETURN ;

   MCW_popdown_meter( plint->wid->meter ) ;
   plint->wid->meter = NULL ;
   EXRETURN ;
}

void PLUTO_set_meter( PLUGIN_interface * plint , int percent )
{
ENTRY("PLUTO_set_meter") ;

   if( plint == NULL             || plint->wid == NULL       ||
       plint->wid->shell == NULL || plint->wid->meter == NULL  ) EXRETURN ;

   MCW_set_meter( plint->wid->meter , percent ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Routine to popup an image.
   Inputs:
     handle = (a) value returned from previous call to this routine
              (b) or NULL to start a new image display window
     im     = pointer to MRI_IMAGE structure
              (a) NULL to popdown the image display window
              (b) non-NULL to display this image in the window

   Output is a "handle" that can be used to control the next usage
   of this routine (whether to send an image to a new window or
   replace it in an old one).
-------------------------------------------------------------------------*/

void * PLUTO_popup_image( void * handle , MRI_IMAGE * im )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

ENTRY("PLUTO_popup_image") ;

   /*-- input image is NULL ==> popdown, if applicable --*/

   if( im == NULL ){
      if( imp != NULL )
         drive_MCW_imseq( imp->seq , isqDR_destroy , NULL ) ;

      RETURN((void *) imp) ;
   }

   /*-- input = no popper handle ==> create one --*/

   if( imp == NULL ){
      imp      = myXtNew(PLUGIN_impopper) ;
      imp->seq = NULL ; imp->im  = NULL ;
   }

   /*-- input = non-null image ==> replace image --*/

   mri_free( imp->im ) ;      /* toss old copy */
   imp->im = mri_copy( im ) ; /* make new copy */

   /*-- input = inactive popper handle ==> activate it --*/

   if( imp->seq == NULL ){
      imp->seq = open_MCW_imseq( GLOBAL_library.dc ,
                                 PLUGIN_imseq_getim , (XtPointer) imp ) ;

      drive_MCW_imseq( imp->seq , isqDR_realize, NULL ) ;
      drive_MCW_imseq( imp->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;

      drive_MCW_imseq( imp->seq , isqDR_opacitybut , (XtPointer) 0 ) ; /* 07 Mar 2001 */
      drive_MCW_imseq( imp->seq , isqDR_zoombut    , (XtPointer) 0 ) ; /* 12 Mar 2002 */
      drive_MCW_imseq( imp->seq , isqDR_penbbox    , (XtPointer) 0 ) ; /* 18 Jul 2003 */
   }

   /*-- display image at last --*/

   if( im->name != NULL && strlen(im->name) > 0 )
      drive_MCW_imseq( imp->seq , isqDR_title, im->name ) ;

   drive_MCW_imseq( imp->seq , isqDR_clearstat , NULL ) ;
   drive_MCW_imseq( imp->seq , isqDR_reimage , (XtPointer) 0 ) ;

   RETURN((void *) imp) ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq for PLUGIN_popup_image.
   Just returns the control information, or the given image.
--------------------------------------------------------------------*/

XtPointer PLUGIN_imseq_getim( int n , int type , XtPointer handle )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

ENTRY("PLUGIN_imseq_getim") ;

   if( imp == NULL ) RETURN(NULL) ;

   /*--- control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status * stat = myXtNew( MCW_imseq_status ) ;
      stat->num_total  = 1 ;
      stat->num_series = 1 ;
      stat->send_CB    = PLUGIN_seq_send_CB ;
      stat->parent     = (XtPointer) imp  ;
      stat->aux        = NULL ;

      stat->transforms0D = & (GLOBAL_library.registered_0D) ;
      stat->transforms2D = & (GLOBAL_library.registered_2D) ;
      stat->slice_proj   = NULL ;

      RETURN((XtPointer) stat) ;
   }

   /*--- no overlay ---*/

   if( type == isqCR_getoverlay ) RETURN(NULL) ;

   /*--- return a copy of the image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE * im = NULL ;
      if( imp->im != NULL ) im = mri_copy( imp->im ) ;
      RETURN((XtPointer) im) ;
   }

   RETURN(NULL) ;  /* should not occur, but who knows? */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

void PLUGIN_seq_send_CB( MCW_imseq * seq , XtPointer handle , ISQ_cbs * cbs )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

ENTRY("PLUGIN_seq_send_CB") ;

   if( imp == NULL ) EXRETURN ;

   switch( cbs->reason ){

      case isqCR_destroy:{
         XtFree((char*)imp->seq->status) ;
         XtFree((char*)imp->seq)         ; imp->seq = NULL ;
         mri_free( imp->im )             ; imp->im  = NULL ;
      }
      break ;

#ifndef NO_FRIVOLITIES
      case isqCR_buttonpress:{
         XButtonEvent *xev = (XButtonEvent *) cbs->event ;
#define NBIRN 10
         static int nold=0 ;
         static char *birn[NBIRN] = {
           " \n** Don't DO That! **\n "                        ,
           " \n** Stop it, Rasmus! **\n "                      ,
           " \n** Do NOT read this message! **\n "             ,
           " \n** Having fun yet? **\n "                       ,
           " \n** What do you want NOW? **\n "                 ,
           " \n** Too much time on your hands? **\n "          ,
           " \n** Why are you bothering me? **\n "             ,
           " \n** Danger! Danger, Will Robinson! **\n "        ,
           " \n** WARNING: Planetary meltdown imminent! **\n " ,

           " \n"
           " God of our fathers, known of old,\n"
           " Lord of our far-flung battle-line,\n"
           " Beneath whose awful hand we hold\n"
           " Dominion over palm and pine -\n"
           " Lord God of Hosts, be with us yet,\n"
           " Lest we forget - lest we forget!\n"
           " \n"
           " The tumult and the shouting dies;\n"
           " The captains and the kings depart:\n"
           " Still stands Thine ancient sacrifice,\n"
           " An humble and a contrite heart.\n"
           " Lord God of Hosts, be with us yet,\n"
           " Lest we forget - lest we forget!\n"
           " \n"
           " Far-called, our navies melt away;\n"
           " On dune and headland sinks the fire:\n"
           " Lo, all our pomp of yesterday\n"
           " Is one with Nineveh and Tyre!\n"
           " Judge of the Nations, spare us yet.\n"
           " Lest we forget - lest we forget!\n"
           " \n"
           " If, drunk with sight of power, we loose\n"
           " Wild tongues that have not Thee in awe,\n"
           " Such boastings as the Gentiles use,\n"
           " Or lesser breeds without the Law -\n"
           " Lord God of Hosts, be with us yet,\n"
           " Lest we forget - lest we forget!\n"
           " \n"
           " For heathen heart that puts her trust\n"
           " In reeking tube and iron shard,\n"
           " All valiant dust that builds on dust,\n"
           " And, guarding, calls not Thee to guard,\n"
           " For frantic boast and foolish word -\n"
           " The Mercy on Thy People, Lord!\n"
         } ;

#define NKLING 5
         static int nkl=0 ;
         static char *kling[NKLING] = {
            " \n What is this talk of 'release'?\n"
            " Klingons do not make software 'releases'.\n"
            " Our software 'escapes', leaving a bloody trail of\n"
            " designers and 'Quality Assurance' people in its wake.\n"        ,

            " \n Debugging? Klingons do not debug.\n"
            " Our software does not coddle the weak.\n"                       ,

            " \n Klingon software does NOT have BUGS.\n"
            " It has FEATURES, and those features are too\n"
            " sophisticated for a Romulan pig like you to understand.\n"      ,

            " \n Our users will know fear and cower before our software!\n"
            " Ship it! Ship it and let them flee like the dogs they are!\n"  ,

            " \n You question the worthiness of my code?\n"
            " I should kill you where you stand!\n"
         } ;

         if( xev == NULL || xev->button == Button1 ){
           if( !NO_frivolities && nold < NBIRN ){
             if( strstr(birn[nold],"Rasmus") != NULL )
               AFNI_speak("Stop it, Rasmus", 0 ) ;
             MCW_popup_message( seq->wimage , birn[nold++] , MCW_USER_KILL ) ;
           } else {
             PLUTO_beep() ;
             if( nold == NBIRN ){ AFNI_speak("Stop it",0); nold++; }
           }
         } else if( xev->button == Button3 ){
           if( !NO_frivolities && nkl < NKLING ){
             MCW_popup_message( seq->wimage , kling[nkl++] , MCW_USER_KILL ) ;
           } else {
             PLUTO_beep() ;
             if( nkl == NKLING ){ AFNI_speak("Deesist at once",0); nkl++; }
           }
         }
      }
      break ;

      /*--------------------------------------*/

      case isqCR_keypress:{    /* 12 Sep 2002 */
        static char *nash[] = {
                   " \n"
                   "The ant has made himself illustrious\n"
                   "Through constant industry industrious.\n"
                   "So what?\n"
                   "Would you be calm and placid\n"
                   "If you were full of formic acid?\n"
                ,
                   " \n"
                   "Celery, raw\n"
                   "Develops the jaw,\n"
                   "But celery, stewed,\n"
                   "Is more quietly chewed.\n"
                ,
                   " \n"
                   "I objurgate the centipede,\n"
                   "A bug we do not really need.\n"
                   "At sleepy-time he beats a path\n"
                   "Straight to the bedroom or the bath.\n"
                   "You always wallop where he's not,\n"
                   "Or, if he is, he makes a spot. \n"
                ,
                   " \n"
                   "The cow is of the bovine ilk;\n"
                   "One end is moo, the other, milk.\n"
                ,
                   " \n"
                   "This is my dream,\n"
                   "It is my own dream,\n"
                   "I dreamt it.\n"
                   "I dreamt that my hair was kempt.\n"
                   "Then I dreamt that my true love unkempt it.\n"
                ,
                   " \n"
                   "I find it very difficult to enthuse\n"
                   "Over the current news.\n"
                   "Just when you think that at least the outlook\n"
                   "   is so black that it can grow no blacker, it worsens,\n"
                   "And that is why I do not like the news, because there\n"
                   "   has never been an era when so many things were going\n"
                   "   so right for so many of the wrong persons. \n"
                ,
                   " \n"
                   "I test my bath before I sit,\n"
                   "And I'm always moved to wonderment\n"
                   "That what chills the finger not a bit\n"
                   "Is so frigid upon the fundament.\n"
                ,
                   " \n"
                   "The turtle lives 'twixt plated decks\n"
                   "Which practically conceal its sex.\n"
                   "I think it clever of the turtle\n"
                   "In such a fix to be so fertile.\n"
                ,
                   " \n"
                   "Candy\n"
                   "Is Dandy\n"
                   "But liquor\n"
                   "Is quicker.\n"
                ,
                   " \n"
                   "I've never seen an abominable snowman,\n"
                   "I'm hoping not to see one,\n"
                   "I'm also hoping, if I do,\n"
                   "That it will be a wee one. \n"
                ,
                   " \n"
                   "Children aren't happy without\n"
                   "something to ignore, and that's\n"
                   "what parents were created for. \n"
                ,
                   " \n"
                   "Middle age is when you've met so\n"
                   "many people that every new person\n"
                   "you meet reminds you of someone else.\n"
                } ;
#define NUM_NASH (sizeof(nash)/sizeof(char *)) ;

        static int iold=-1 ; int ii ;
        do{ ii=lrand48()%NUM_NASH; } while( ii==iold ) ; iold = ii ;
        MCW_popup_message( seq->wimage , nash[ii] , MCW_USER_KILL ) ;
      }
      break ;
#endif  /* NO_FRIVOLITIES */

   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   28 April 2000: Open an image sequence display window.
   The input image(s) are copied, so they can be destroyed after
   these calls.
      PLUTO_imseq_popim(im,kfunc,kdata)
         starts the image window off with a single image;
         return value is a "handle" that is used in other calls;
         if kfunc is not NULL, it is a function that will be called
         with argument kdata when the imseq window is closed
         (e.g., this can be used to set the saved handle to NULL)
      PLUTO_imseq_popup(imar,kfunc,kdata)
         starts the window off with an array of images
      PLUTO_imseq_retitle(handle,string)
         changes the window manager title
      PLUTO_imseq_rekill(handle,kfunc,kdata)
         changes the kill function/data to kfunc/kdata;
      PLUTO_imseq_addto(handle,im)
         adds the single image "im" to the display
      PLUTO_imseq_setim(handle,n)
         sets the image index to 'n'
      PLUTO_imseq_destroy(handle)
         pops down the image viewer;
         destroys the internal copies of the images;
         calls kfunc(kdata) if kfunc is not NULL
         (so do PLUTO_imseq_rekill(handle,NULL,NULL); before
                PLUTO_imseq_destroy(handle);
          if you don't want the kfunc to be called)
-------------------------------------------------------------------------*/

void * PLUTO_imseq_popim( MRI_IMAGE * im, generic_func * kfunc, void * kdata )
{
   MRI_IMARR * imar ;
   void * handle ;

   if( im == NULL ) return NULL ;
   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,im) ;
   handle = PLUTO_imseq_popup( imar,kfunc,kdata ) ;
   FREE_IMARR(imar) ;  /* not DESTROY_IMARR: we don't 'own' im */
   return handle ;
}

void * PLUTO_imseq_popup( MRI_IMARR * imar, generic_func * kfunc, void * kdata )
{
   int ntot , ii ;
   MRI_IMAGE * im=NULL , * cim=NULL ;
   PLUGIN_imseq * psq=NULL ;

   if( imar == NULL || IMARR_COUNT(imar) == 0 ) return NULL ;

   ntot = IMARR_COUNT(imar) ;

   psq = (PLUGIN_imseq *) calloc(1,sizeof(PLUGIN_imseq)) ;
   if( psq == NULL ) return NULL ;

   INIT_IMARR(psq->imar) ;
   psq->kill_func = kfunc ;
   psq->kill_data = kdata ;
   psq->rgb_count = 0 ;

   for( ii=0 ; ii < ntot ; ii++ ){
      im = IMARR_SUBIMAGE(imar,ii) ;
      if( im != NULL ){
         cim = mri_copy( im ) ;
         ADDTO_IMARR(psq->imar,cim) ;
         if( cim->kind == MRI_rgb ) psq->rgb_count++ ;
      }
   }
   ntot = IMARR_COUNT(psq->imar) ;
   if( ntot == 0 ){
      DESTROY_IMARR(psq->imar) ; free(psq) ; return NULL ;
   }

   psq->seq = open_MCW_imseq( GLOBAL_library.dc , PLUTO_imseq_getim , psq ) ;

   drive_MCW_imseq( psq->seq , isqDR_clearstat , NULL ) ;

   { ISQ_options opt ;       /* change some options from the defaults */

     memset(&opt, 0, sizeof(opt));
     ISQ_DEFAULT_OPT(opt) ;
     opt.save_one = False ;  /* change to Save:bkg */
     opt.save_pnm = False ;
     drive_MCW_imseq( psq->seq , isqDR_options      , (XtPointer) &opt ) ;
     drive_MCW_imseq( psq->seq , isqDR_periodicmont , (XtPointer) 0    ) ;
   }

   /* make it popup */

   drive_MCW_imseq( psq->seq , isqDR_realize, NULL ) ;
   drive_MCW_imseq( psq->seq , isqDR_title, "Images" ) ;

   if( ntot == 1 )
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
   else {
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid ) ;
      drive_MCW_imseq( psq->seq , isqDR_opacitybut , (XtPointer) 0 ) ; /* 07 Mar 2001 */
      drive_MCW_imseq( psq->seq , isqDR_zoombut    , (XtPointer) 0 ) ; /* 12 Mar 2002 */
      drive_MCW_imseq( psq->seq , isqDR_penbbox    , (XtPointer) 0 ) ; /* 18 Jul 2003 */
   }

   return (void *) psq ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_retitle( void * handle , char * title )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   if( psq == NULL || psq->seq == NULL || title == NULL ) return ;
   drive_MCW_imseq( psq->seq , isqDR_title, title ) ;
   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_rekill( void * handle, generic_func * kfunc, void * kdata )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   if( psq == NULL ) return ;
   psq->kill_func = kfunc ;
   psq->kill_data = kdata ;
   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_addto( void * handle , MRI_IMAGE * im )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;
   int ntot , ii ;
   MRI_IMAGE * cim ;

   if( psq == NULL || psq->seq == NULL || im == NULL ) return ;

   ntot = IMARR_COUNT(psq->imar) ;
   cim  = mri_copy(im) ;
   if( cim->kind == MRI_rgb ) psq->rgb_count++ ;
   ADDTO_IMARR(psq->imar,cim) ;

   drive_MCW_imseq( psq->seq , isqDR_newseq , psq ) ;

   if( ntot == 1 )
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
   else {
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid ) ;
      drive_MCW_imseq( psq->seq , isqDR_opacitybut , (XtPointer) 0 ) ; /* 07 Mar 2001 */
      drive_MCW_imseq( psq->seq , isqDR_zoombut    , (XtPointer) 0 ) ; /* 12 Mar 2002 */
   }

   drive_MCW_imseq( psq->seq , isqDR_reimage , (XtPointer)ITOP(ntot) ) ;

   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_setim( void *handle , int n )    /* 17 Dec 2004 */
{
   PLUGIN_imseq *psq = (PLUGIN_imseq *)handle ;

   if( psq == NULL || psq->seq == NULL ||
       n   <  0    || n        >= IMARR_COUNT(psq->imar) ) return ;

   drive_MCW_imseq( psq->seq , isqDR_reimage , (XtPointer)ITOP(n) ) ;
   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_destroy( void * handle )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   if( psq == NULL ) return ;
   drive_MCW_imseq( psq->seq , isqDR_destroy , NULL ) ;
   return ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq.
   Just returns the control information, or the selected image.
--------------------------------------------------------------------*/

XtPointer PLUTO_imseq_getim( int n , int type , XtPointer handle )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   int ntot = 0 ;

   if( psq->imar != NULL ) ntot = IMARR_COUNT(psq->imar) ;
   if( ntot < 1 ) ntot = 1 ;

   /*--- send control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status * stat = myXtNew( MCW_imseq_status ) ;  /* will be free-d */
                                                               /* when imseq is */
                                                               /* destroyed    */
      stat->num_total  = ntot ;
      stat->num_series = ntot ;
      stat->send_CB    = PLUTO_imseq_send_CB ;
      stat->parent     = NULL ;
      stat->aux        = NULL ;

      stat->transforms0D = &(GLOBAL_library.registered_0D) ;
      stat->transforms2D = &(GLOBAL_library.registered_2D) ;
      stat->slice_proj   = NULL ;

      return (XtPointer) stat ;
   }

   /*--- no overlay, never ---*/

   if( type == isqCR_getoverlay ) return NULL ;

   /*--- return a copy of an image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE * im = NULL , * rim ;

      if( psq->imar != NULL ){
         if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
         rim = IMARR_SUBIMAGE(psq->imar,n) ;
         if( psq->rgb_count > 0 )
            im = mri_to_rgb( rim ) ;
         else
            im = mri_copy( rim ) ;
      }
      return (XtPointer) im ;
   }

   return NULL ; /* should not occur, but who knows? */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

void PLUTO_imseq_send_CB( MCW_imseq * seq , XtPointer handle , ISQ_cbs * cbs )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   switch( cbs->reason ){
      case isqCR_destroy:{
         myXtFree(psq->seq) ;
         DESTROY_IMARR( psq->imar ) ;

         if( psq->kill_func != NULL )
#if 0
            psq->kill_func( psq->kill_data ) ;
#else
            AFNI_CALL_VOID_1ARG( psq->kill_func , void *,psq->kill_data ) ;
#endif

         free(psq) ;
      }
      break ;
   }
   return ;
}

/*-------------------------------------------------------------------------*/

/*-- 13 Dec 1997: moved guts into thd_make*.c --*/

THD_3dim_dataset * PLUTO_4D_to_typed_fim( THD_3dim_dataset * old_dset ,
                                          char * new_prefix , int new_datum ,
                                          int ignore , int detrend ,
                                          generic_func * user_func ,
                                          void * user_data )
{
   THD_3dim_dataset * new_dset ;  /* output dataset */

ENTRY("PLUTO_4D_to_typed_fim") ;

   if( ! PLUTO_prefix_ok(new_prefix) ) RETURN(NULL) ;

   new_dset = MAKER_4D_to_typed_fim( old_dset , new_prefix , new_datum ,
                                     ignore , detrend , user_func , user_data ) ;

   RETURN(new_dset) ;
}

/*-------------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_4D_to_typed_fith( THD_3dim_dataset * old_dset ,
                                           char * new_prefix , int new_datum ,
                                           int ignore , int detrend ,
                                           generic_func * user_func ,
                                           void * user_data )
{
   THD_3dim_dataset * new_dset ;  /* output dataset */

ENTRY("PLUTO_4D_to_typed_fith") ;

   if( ! PLUTO_prefix_ok(new_prefix) ) RETURN(NULL) ;

   new_dset = MAKER_4D_to_typed_fith( old_dset , new_prefix , new_datum ,
                                      ignore , detrend , user_func , user_data ) ;

   RETURN(new_dset) ;
}

/*-------------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_4D_to_typed_fbuc( THD_3dim_dataset * old_dset ,
                                           char * new_prefix , int new_datum ,
                                           int ignore , int detrend ,
                                           int nbrik ,
                                           generic_func * user_func ,
                                           void * user_data )
{
   THD_3dim_dataset * new_dset ;  /* output dataset */

ENTRY("PLUTO_4D_to_typed_fbuc") ;

   if( ! PLUTO_prefix_ok(new_prefix) ) RETURN(NULL) ;

   new_dset = MAKER_4D_to_typed_fbuc( old_dset , new_prefix , new_datum ,
                                      ignore , detrend , nbrik , user_func , 
                                      user_data, NULL ) ;

   RETURN(new_dset) ;
}

/*----------------------------------------------------------------------------*/

void PLUTO_report( PLUGIN_interface *plint , char *str )
{
   if( plint == NULL || str == NULL || !AFNI_VERBOSE ) return ;
   printf("\n%15.15s= %s" , plint->label , str ) ; fflush(stdout) ;
   return ;
}

/*----------------------------------------------------------------------------
  Routines to add a text entry box.
------------------------------------------------------------------------------*/

PLUGIN_strval * new_PLUGIN_strval( Widget wpar , char * str )
{
   PLUGIN_strval * av ;
   XmString xstr ;

ENTRY("new_PLUGIN_strval") ;

   if( wpar == (Widget) NULL ) RETURN(NULL) ;

   av = myXtNew(PLUGIN_strval) ;

   av->rowcol = XtVaCreateWidget(
                  "AFNI" , xmRowColumnWidgetClass , wpar ,
                     XmNpacking     , XmPACK_TIGHT ,
                     XmNorientation , XmHORIZONTAL ,
                     XmNmarginHeight, 0 ,
                     XmNmarginWidth , 0 ,
                     XmNspacing     , 0 ,
                     XmNtraversalOn , True  ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;

   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   av->label = XtVaCreateManagedWidget(
                  "AFNI" , xmLabelWidgetClass , av->rowcol ,
                     XmNlabelString , xstr ,
                     XmNmarginWidth   , 0  ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;
   XmStringFree( xstr ) ;

   av->textf = XtVaCreateManagedWidget(
                  "AFNI" , xmTextFieldWidgetClass , av->rowcol ,
                      XmNcolumns      , 9 ,
                      XmNeditable     , True ,
                      XmNmaxLength    , PLUGIN_STRING_SIZE ,
                      XmNresizeWidth  , False ,
                      XmNmarginHeight , 1 ,
                      XmNmarginWidth  , 1 ,
                      XmNcursorPositionVisible , True ,
                      XmNblinkRate , 0 ,
                      XmNautoShowCursorPosition , True ,
                      XmNtraversalOn , True  ,
                      XmNinitialResourcesPersistent , False ,
                   NULL ) ;

   XtManageChild( av->rowcol ) ;
   RETURN(av) ;
}

void destroy_PLUGIN_strval( PLUGIN_strval * av )
{
   if( av != NULL ){
      XtDestroyWidget( av->rowcol ) ;
      myXtFree(av) ;
   }
   return ;
}

void alter_PLUGIN_strval_width( PLUGIN_strval * av , int nchar )
{
   if( av != NULL && nchar > 0 )
      XtVaSetValues( av->textf , XmNcolumns , nchar , NULL ) ;
   return ;
}

void set_PLUGIN_strval( PLUGIN_strval * av , char * str )
{
   if( av != NULL && str != NULL )
      XmTextFieldSetString( av->textf , str ) ;
   return ;
}

char * get_PLUGIN_strval( PLUGIN_strval * av )   /* must be XtFree-d */
{
   if( av == NULL ) return NULL ;
                    return XmTextFieldGetString( av->textf ) ;
}

/* Set the addresses of the main vol2surf globals.  Note that the
 * plugin options pointer is stored as (void *) so that vol2surf.h
 * will not need to percolate up to afni.h.	09 Sep 2004 [rickr]
 */
#include "vol2surf.h"
int PLUTO_set_v2s_addrs(void ** vopt, char *** maps, char ** hist)
{
    if ( !vopt || !maps || !hist ) return -1;

    *vopt = (void *)&gv2s_plug_opts;
    *maps = gv2s_map_names;
    *hist = gv2s_history;

    return 0;
}

/**************************************************************************/
/*========================================================================*/
/*============ These must remain the last lines of this file! ============*/

/** put library routines here that must be loaded **/

#include "cox_render.h"                 /* 14 Feb 2002 */

extern void matrix_initialize(void *);  /* 30 Jul 2007 */

static vptr_func * forced_loads[] = {
#ifndef NO_DYNAMIC_LOADING
   (vptr_func *) startup_lsqfit ,
   (vptr_func *) delayed_lsqfit ,
   (vptr_func *) mri_align_dfspace ,
   (vptr_func *) EDIT_one_dataset ,
   (vptr_func *) EDIT_add_brick ,
   (vptr_func *) mri_2dalign_setup ,
   (vptr_func *) mri_3dalign_setup ,
   (vptr_func *) qsort_floatint ,
   (vptr_func *) qsort_floatfloat ,
   (vptr_func *) symeig_double ,
#ifndef DONT_USE_VOLPACK
   (vptr_func *) MREN_render ,
#endif
   (vptr_func *) new_MCW_graf ,
   (vptr_func *) THD_makemask ,
   (vptr_func *) mri_copy ,
   (vptr_func *) beta_t2p ,
   (vptr_func *) get_laguerre_table ,
   (vptr_func *) mri_fix_data_pointer ,
   (vptr_func *) THD_zeropad ,
   (vptr_func *) THD_axcode ,
   (vptr_func *) THD_dataset_rowfillin ,
   (vptr_func *) mri_histobyte ,          /* 25 Jul 2001 */
   (vptr_func *) sphere_voronoi_vectors , /* 18 Oct 2001 */
   (vptr_func *) new_CREN_renderer ,      /* 14 Feb 2002 */
   (vptr_func *) THD_average_timeseries , /* 03 Apr 2002 */
   (vptr_func *) cl1_solve ,              /* 07 Aug 2002 */
   (vptr_func *) new_Dtable ,             /* 20 Oct 2003 */
   (vptr_func *) powell_newuoa ,          /* 24 Jul 2006 */
   (vptr_func *) matrix_initialize ,      /* 30 Jul 2007 */
   (vptr_func *) r_new_resam_dset ,       /* 31 Jul 2007 */
   (vptr_func *) r_hex_str_to_long ,      /* 31 Jul 2007 */
   (vptr_func *) r_idisp_vec3f ,          /* 31 Jul 2007 */
   (vptr_func *) THD_dataset_mismatch ,   /* 04 Sep 2009 */
#endif
NULL } ;

vptr_func * MCW_onen_i_estel_edain(void *n){
  THD_3dim_dataset *ds = (THD_3dim_dataset *)n ;
  double             x = (double)(ds->dblk->total_bytes) ;
  return forced_loads[(int)x] ;
}

#else  /* not ALLOW_PLUGINS */

void * MCW_onen_i_estel_edain(void *n){} ;  /* dummy routine */

#endif /* ALLOW_PLUGINS */

/***********************************************************************
   Routines that are always compiled, since they are used in
   a few places in AFNI that are not plugin-specific.
************************************************************************/

void PLUTO_register_timeseries( char * cname , MRI_IMAGE * tsim )
{
   MRI_IMAGE * qim ;

ENTRY("PLUTO_register_timeseries") ;

   if( tsim != NULL ){
      qim = mri_to_float( tsim ) ;  /* a copy */
      mri_add_name( cname , qim ) ; /* the name */
      AFNI_add_timeseries( qim ) ;  /* give it to AFNI */
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------
  Find a dataset, given its idcode string. [02 Mar 2002]
------------------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_find_dset_idc( char * idc )
{
   MCW_idcode idcode ;
   if( idc == NULL ) return NULL ;
   MCW_strncpy( idcode.str , idc , MCW_IDSIZE ) ;
   return PLUTO_find_dset( &idcode ) ;
}

/*----------------------------------------------------------------------------
  Routine to find a dataset in the global sessionlist, given its idcode.
  If this returns NULL, then you are SOL.
------------------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_find_dset( MCW_idcode *idcode )
{
   THD_slist_find find ;

ENTRY("PLUTO_find_dset") ;

   if( idcode == NULL || ISZERO_IDCODE(*idcode) ) RETURN(NULL) ;

   find = THD_dset_in_sessionlist( FIND_IDCODE , idcode ,
                                   GLOBAL_library.sslist , -1 ) ;

   RETURN(find.dset) ;
}

/*-----------------------------------------------------------------*/

THD_slist_find PLUTO_dset_finder( char *idc )
{
   MCW_idcode idcode ;
   THD_slist_find find ;

   BADFIND(find) ; if( idc == NULL || *idc == '\0' ) return find ;

   MCW_strncpy( idcode.str , idc , MCW_IDSIZE ) ;
   find = THD_dset_in_sessionlist( FIND_IDCODE , &idcode ,
                                   GLOBAL_library.sslist , -1 ) ;
   if( find.dset != NULL ) return find ;

   find = THD_dset_in_sessionlist( FIND_PREFIX , idc ,
                                   GLOBAL_library.sslist , -1 ) ;
   if( find.dset != NULL ) return find ;

   find = THD_dset_in_sessionlist( FIND_NAME , idc ,
                                   GLOBAL_library.sslist , -1 ) ;
   return find ;
}

/*-----------------------------------------------------------------
   Plot a histogram; input might be from mri_histogram():
     nbin = # of bins in hist[]
     bot  = bottom of hist[0] bin   } bin size is
     top  = top of hist[nbin-1] bin } (top-bot)/nbin
     hist = array of counts in each bin
     xlab } labels for x-axis,
     ylab }            y-axis
     tlab }        and top of graph (NULL => skip this label)

     njist = number of extra histograms [can be 0]
     jist  = "extra" histograms to plot atop hist
               (if jist == NULL, this plot is skipped)
   Graph is popped up and then "forgotten" -- RWCox - 30 Sep 1999.
-------------------------------------------------------------------*/

void PLUTO_histoplot_f( int nbin, float bot, float top, float *hist ,
                        char *xlab , char *ylab , char *tlab ,
                        int njist , float **jist )
{
   int ii , nx , ny,jj ;
   float *xar , *yar , *zar=NULL , **yzar ;
   float dx ;

ENTRY("PLUTO_histoplot") ;

   if( nbin < 2 || hist == NULL ) EXRETURN ;
   if( bot >= top ){ bot = 0.0f ; top = nbin ; }

   nx  = 2*(nbin+1) ;
   dx  = (top-bot)/nbin ;
   xar = (float *) malloc(sizeof(float)*nx) ;
   yar = (float *) malloc(sizeof(float)*nx) ;

   if( jist == NULL || njist < 0 ) njist = 0 ;
   ny = njist + 1 ;

   yzar = (float **) malloc(sizeof(float *)*ny) ;
   yzar[0] = yar ;
   for( jj=0 ; jj < njist ; jj++ )
     yzar[jj+1] = (float *) malloc(sizeof(float)*nx) ;

   xar[0] = bot ; yar[0] = 0.0f ;
   for( ii=0 ; ii < nbin ; ii++ ){
     xar[2*ii+1] = bot+ii*dx     ; yar[2*ii+1] = hist[ii] ;
     xar[2*ii+2] = bot+(ii+1)*dx ; yar[2*ii+2] = hist[ii] ;

     for( jj=0 ; jj < njist ; jj++ )
       yzar[jj+1][2*ii+1] = yzar[jj+1][2*ii+2] = jist[jj][ii] ;
   }
   xar[2*nbin+1] = top ; yar[2*nbin+1] = 0.0f ;
   for( jj=0 ; jj < njist ; jj++ )
     yzar[jj+1][0] = yzar[jj+1][2*nbin+1] = 0.0f ;

   plot_ts_lab( GLOBAL_library.dc->display ,
                nx , xar , ny , yzar ,
                xlab,ylab,tlab , NULL , NULL ) ;

   for( jj=0 ; jj < njist ; jj++ ) free(yzar[jj+1]) ;
   free(yzar) ; free(xar) ; free(yar) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void PLUTO_histoplot( int nbin, float bot, float top, int *hist ,
                      char *xlab , char *ylab , char *tlab ,
                      int njist , int **jist )
{
   float *hist_f , **jist_f ;
   int ii,jj ;

ENTRY("PLUTO_histoplot") ;

   if( nbin < 2 || hist == NULL ) EXRETURN ;

   hist_f = (float *)malloc(sizeof(float)*nbin) ;
   for( ii=0 ; ii < nbin ; ii++ ) hist_f[ii] = (float)hist[ii] ;

   if( njist <= 0 || jist == NULL ){
     PLUTO_histoplot_f( nbin, bot,top , hist_f , xlab,ylab,tlab , 0,NULL ) ;
     free((void *)hist_f) ;
     EXRETURN ;
   }

   jist_f = (float **)malloc(sizeof(float *)*njist) ;
   for( jj=0 ; jj < njist ; jj++ ){
     jist_f[jj] = (float *)malloc(sizeof(float)*nbin) ;
     for( ii=0 ; ii < nbin ; ii++ ) jist_f[jj][ii] = (float)jist[jj][ii] ;
   }
   PLUTO_histoplot_f( nbin,bot,top , hist_f , xlab,ylab,tlab , njist,jist_f );
   for( jj=0 ; jj < njist ; jj++ ) free((void *)jist_f[jj]) ;
   free((void *)jist_f) ; free((void *)hist_f) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
  Return p10 as a power of 10 such that
    p10 <= fabs(x) < 10*p10
  unless x == 0, in which case return 0.
------------------------------------------------------------------------*/

static float p10( float x )
{
   double y ;
   if( x == 0.0 ) return 0.0 ;
   if( x <  0.0 ) x = -x ;
   y = floor(log10(x)+0.000001) ; y = pow( 10.0 , y ) ;
   return (float) y ;
}

#define STGOOD(s) ( (s) != NULL && (s)[0] != '\0' )

/*-----------------------------------------------------------------
   Plot a scatterplot.
     npt  = # of points in x[] and y[]
     x    = x-axis values array
     y    = y-axis values array
     xlab } labels for x-axis,
     ylab }            y-axis
     tlab }        and top of graph (NULL => skip this label)
     a,b  = if nonzero, plots line y=ax+b on top
   Graph is popped up and then "forgotten" -- RWCox - 13 Jan 2000
-------------------------------------------------------------------*/

void PLUTO_scatterplot( int npt , float *x , float *y ,
                        char *xlab , char *ylab , char *tlab ,
                        float a , float b )
{
   int ii , np , nnax,mmax , nnay,mmay ;
   float xbot,xtop , ybot,ytop , pbot,ptop ,
         xobot,xotop,yobot,yotop , xa,xb,ya,yb , dx,dy ;
   float *xar , *yar , *zar=NULL , **yzar ;
   float dsq , rx,ry ;
   char str[32] ;
   MEM_plotdata * mp ;

ENTRY("PLUTO_scatterplot") ;

   if( npt < 2 || x == NULL || y == NULL ) EXRETURN ;

   /* find range of data */

   xbot = xtop = x[0] ; ybot = ytop = y[0] ;
   for( ii=1 ; ii < npt ; ii++ ){
           if( x[ii] < xbot ) xbot = x[ii] ;
      else if( x[ii] > xtop ) xtop = x[ii] ;

           if( y[ii] < ybot ) ybot = y[ii] ;
      else if( y[ii] > ytop ) ytop = y[ii] ;
   }
   if( xbot >= xtop || ybot >= ytop ){
      fprintf(stderr,"*** Data has no range in PLUTO_scatterplot!\n\a");
      EXRETURN ;
   }

   /*-- push range of x outwards --*/

   pbot = p10(xbot) ; ptop = p10(xtop) ; if( ptop < pbot ) ptop = pbot ;
   if( ptop != 0.0 ){
      np = (xtop-xbot) / ptop + 0.5 ;
      switch( np ){
         case 1:  ptop *= 0.1  ; break ;
         case 2:  ptop *= 0.2  ; break ;
         case 3:  ptop *= 0.25 ; break ;
         case 4:
         case 5:  ptop *= 0.5  ; break ;
      }
      xbot = floor( xbot/ptop ) * ptop ;
      xtop =  ceil( xtop/ptop ) * ptop ;
      nnax = floor( (xtop-xbot) / ptop + 0.5 ) ;
      mmax = (nnax < 3) ? 10
                        : (nnax < 6) ? 5 : 2 ;
   } else {
      nnax = 1 ; mmax = 10 ;
   }

   /*-- push range of y outwards --*/

   pbot = p10(ybot) ; ptop = p10(ytop) ; if( ptop < pbot ) ptop = pbot ;
   if( ptop != 0.0 ){
      np = (ytop-ybot) / ptop + 0.5 ;
      switch( np ){
         case 1:  ptop *= 0.1  ; break ;
         case 2:  ptop *= 0.2  ; break ;
         case 3:  ptop *= 0.25 ; break ;
         case 4:
         case 5:  ptop *= 0.5  ; break ;
      }
      ybot = floor( ybot/ptop ) * ptop ;
      ytop =  ceil( ytop/ptop ) * ptop ;
      nnay = floor( (ytop-ybot) / ptop + 0.5 ) ;
      mmay = (nnay < 3) ? 10
                        : (nnay < 6) ? 5 : 2 ;
   } else {
      nnay = 1 ; mmay = 10 ;
   }

   /*-- setup to plot --*/

   create_memplot_surely( "ScatPlot" , 1.3 ) ;
   set_thick_memplot( 0.0 ) ;

   /*-- plot labels, if any --*/

   xobot = 0.15 ; xotop = 1.27 ;  /* set objective size of plot */
   yobot = 0.1  ; yotop = 0.95 ;

   if( STGOOD(tlab) ){ yotop -= 0.02 ; yobot -= 0.01 ; }

   /* x-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(xlab) )
      plotpak_pwritf( 0.5*(xobot+xotop) , yobot-0.06 , xlab , 16 , 0 , 0 ) ;

   /* y-axis label? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(ylab) )
      plotpak_pwritf( xobot-0.12 , 0.5*(yobot+yotop) , ylab , 16 , 90 , 0 ) ;

   /* label at top? */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   if( STGOOD(tlab) )
      plotpak_pwritf( xobot+0.01 , yotop+0.01 , tlab , 18 , 0 , -2 ) ;

   /* plot axes */

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   plotpak_set( xobot,xotop , yobot,yotop , xbot,xtop , ybot,ytop , 1 ) ;
   plotpak_periml( nnax,mmax , nnay,mmay ) ;

   /* plot data */

#define DSQ 0.001

   dsq = AFNI_numenv( "AFNI_SCATPLOT_FRAC" ) ;   /* 15 Feb 2005 */
   if( dsq <= 0.0 || dsq >= 0.01 ) dsq = DSQ ;

   dx = dsq*(xtop-xbot) ;
   dy = dsq*(ytop-ybot) * (xotop-xobot)/(yotop-yobot) ;
   for( ii=0 ; ii < npt ; ii++ ){

#if 0
      rx = (drand48()-0.5)*dx ;
      ry = (drand48()-0.5)*dy ;
#else
      rx = ry = 0.0 ;
#endif
      xa = x[ii]+rx - dx ; xb = x[ii]+rx + dx ;
      ya = y[ii]+ry - dy ; yb = y[ii]+ry + dy ;

      plotpak_line( xa,ya , xa,yb ) ;
      plotpak_line( xa,yb , xb,yb ) ;
      plotpak_line( xb,yb , xb,ya ) ;
      plotpak_line( xb,ya , xa,ya ) ;
   }

   if( a != 0.0f || b != 0.0f ){              /* 02 May 2005 */
     set_color_memplot( 0.8 , 0.0 , 0.0 ) ;
     plotpak_line( xbot,a*xbot+b , xtop,a*xtop+b ) ;
   }

   mp = get_active_memplot() ;

   (void) memplot_to_topshell( GLOBAL_library.dc->display , mp , NULL ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------
   Routine to force AFNI to redisplay images in all open controllers.
------------------------------------------------------------------------*/

void PLUTO_force_redisplay(void)
{
   Three_D_View * im3d ;
   int ii ;

ENTRY("PLUTO_force_redisplay") ;

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      im3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_OPEN(im3d) ){
         im3d->anat_voxwarp->type =                       /* 11 Jul 1997 */
            im3d->fim_voxwarp->type = ILLEGAL_TYPE ;
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
      }
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------
   Force the redisplay of the color bars in all image windows.
   23 Aug 1998 -- RWCox.
------------------------------------------------------------------------*/

void PLUTO_force_rebar(void)
{
   Three_D_View * im3d ;
   int ii ;

ENTRY("PLUTO_force_rebar") ;

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      im3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_OPEN(im3d) ){
         drive_MCW_imseq( im3d->s123 , isqDR_rebar , NULL ) ;
         drive_MCW_imseq( im3d->s231 , isqDR_rebar , NULL ) ;
         drive_MCW_imseq( im3d->s312 , isqDR_rebar , NULL ) ;
      }
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
static int num_workp      = 0 ;
static XtWorkProc * workp = NULL ;
static XtPointer *  datap = NULL ;
static XtWorkProcId wpid ;

#undef WPDEBUG

void PLUTO_register_workproc( XtWorkProc func , XtPointer data )
{
ENTRY("PLUTO_register_workproc") ;

   if( func == NULL ){
      fprintf(stderr,"PLUTO_register_workproc: func=NULL on entry!\n") ;
      EXRETURN ;
   }

   if( num_workp == 0 ){
      workp = (XtWorkProc *) malloc( sizeof(XtWorkProc) ) ;
      datap = (XtPointer *)  malloc( sizeof(XtPointer) ) ;
      wpid  = XtAppAddWorkProc( PLUTO_Xt_appcontext, PLUG_workprocess, NULL ) ;
#ifdef WPDEBUG
      fprintf(stderr,"PLUTO_register_workproc: wpid = %x\n",(int)wpid) ;
#endif
   } else {
      workp = (XtWorkProc *) realloc( workp, sizeof(XtWorkProc)*(num_workp+1) ) ;
      datap = (XtPointer*)   realloc( datap, sizeof(XtPointer) *(num_workp+1) ) ;
   }

   workp[num_workp] = func ;
   datap[num_workp] = data ;
   num_workp++ ;

#ifdef WPDEBUG
fprintf(stderr,"PLUTO_register_workproc: have %d workprocs\n",num_workp) ;
#endif

   EXRETURN ;
}

void PLUTO_remove_workproc( XtWorkProc func )
{
   int ii , ngood ;

ENTRY("PLUTO_remove_workproc") ;

   if( func == NULL || num_workp == 0 ){
      fprintf(stderr,"*** PLUTO_remove_workproc: illegal parameters!\n") ;
      EXRETURN ;
   }

   for( ii=0 ; ii < num_workp ; ii++ ){
      if( func == workp[ii] ) workp[ii] = NULL ;
   }

   for( ii=0,ngood=0 ; ii < num_workp ; ii++ )
      if( workp[ii] != NULL ) ngood++ ;

   if( ngood == 0 ){
#ifdef WPDEBUG
      fprintf(stderr,"PLUTO_remove_workproc: No workprocs left\n") ;
#endif
      XtRemoveWorkProc( wpid ) ;
      free(workp) ; workp = NULL ; free(datap) ; datap = NULL ;
      num_workp = 0 ;
   } else {
#ifdef WPDEBUG
      fprintf(stderr,"PLUTO_remove_workproc: %d workprocs left\n",ngood) ;
#endif
   }

   EXRETURN ;
}

Boolean PLUG_workprocess( XtPointer fred )
{
   int ii , ngood ;
   Boolean done ;

#ifdef WPDEBUG
   { static int ncall=0 ;
     if( (ncall++) % 1000 == 0 )
       fprintf(stderr,"PLUG_workprocess: entry %d\n",ncall) ; }
#endif

   if( num_workp == 0 ) return True ;

   for( ii=0,ngood=0 ; ii < num_workp ; ii++ ){
      if( workp[ii] != NULL ){
         done = workp[ii]( datap[ii] ) ;
         if( done == True ) workp[ii] = NULL ;
         else               ngood++ ;
      }
   }

   if( ngood == 0 ){
#ifdef WPDEBUG
      fprintf(stderr,"Found no workprocs left\n") ;
#endif
      free(workp) ; workp = NULL ; free(datap) ; datap = NULL ;
      num_workp = 0 ;
      return True ;
   }
   return False ;
}

/*---------------------------------------------------------------*/

typedef struct {
  generic_func * func ;
  XtPointer      cd ;
} mytimeout ;

static void PLUG_dotimeout_CB( XtPointer cd , XtIntervalId * id )
{
   mytimeout * myt = (mytimeout *) cd ;

ENTRY("PLUTO_dotimeout_CB") ;

   if( myt == NULL ) EXRETURN ;  /* bad news */

STATUS("calling user timeout function") ;

#if 0
   myt->func( myt->cd ) ;
#else
   AFNI_CALL_VOID_1ARG( myt->func , XtPointer,myt->cd ) ;
#endif

   myXtFree(myt) ; EXRETURN ;
}

void PLUTO_register_timeout( int msec, generic_func * func, XtPointer cd )
{
   mytimeout * myt ;

ENTRY("PLUTO_register_timeout") ;

   if( func == NULL ){
      fprintf(stderr,"PLUTO_register_timeout: func=NULL on entry!\n") ;
      EXRETURN ;
   }

   if( msec < 0 ) msec = 0 ;

   myt       = myXtNew(mytimeout) ;
   myt->func = func ;
   myt->cd   = cd ;

   (void) XtAppAddTimeOut( PLUTO_Xt_appcontext , msec ,
                           PLUG_dotimeout_CB , (XtPointer) myt ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------*/

double PLUTO_elapsed_time(void) /* in seconds */
{
   struct timeval  new_tval ;
   struct timezone tzone ;
   static struct timeval old_tval ;
   static int first = 1 ;

   gettimeofday( &new_tval , &tzone ) ;

   if( first ){
      old_tval = new_tval ;
      first    = 0 ;
      return 0.0 ;
   }

   if( old_tval.tv_usec > new_tval.tv_usec ){
      new_tval.tv_usec += 1000000 ;
      new_tval.tv_sec -- ;
   }

   return (double)( (new_tval.tv_sec  - old_tval.tv_sec )
                   +(new_tval.tv_usec - old_tval.tv_usec)*1.0e-6 ) ;
}

double PLUTO_cpu_time(void)  /* in seconds */
{
#ifdef CLK_TCK
   struct tms ttt ;

   (void) times( &ttt ) ;
   return (  (double) (ttt.tms_utime
                                     /* + ttt.tms_stime */
                      )
           / (double) CLK_TCK ) ;
#else
   return 0.0l ;
#endif
}
