#undef MAIN
#include "afni.h"

#ifdef AFNI_DEBUG
#  define USE_TRACING
#  define PRINT_TRACING
#endif
#include "dbtrace.h"

/*========================================================================*/
/*==== Compile this only if plugins are properly enabled in machdep.h ====*/

#ifdef ALLOW_PLUGINS

#define EMPTY_STRING "\0"

/***************************************************************************
   Routines to open and initialize plugins.  These should only
   be called at the very end of AFNI initialization, since they
   will call each plugin's internal initializer function, which
   may make requests to get AFNI data structures.
****************************************************************************/

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

#ifdef AFNI_DEBUG
{ char str[256] ; sprintf(str,"scanning directory %s",dname) ; STATUS(str) ; }
#endif

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

#ifdef AFNI_DEBUG
{ char str[256] ; sprintf(str,"%d files to scan",rlist->num) ; STATUS(str) ; }
#endif

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

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"directory %s has %d plugins",dname,outar->num) ; STATUS(str) ; }
#endif

   DESTROY_SARR(rlist) ;
   if( outar->num == 0 ) DESTROY_PLUGIN_ARRAY(outar) ;
   RETURN(outar) ;
}

/*----------------------------------------------------------------------
   Routine to open and initialize a single plugin
------------------------------------------------------------------------*/

AFNI_plugin * PLUG_read_plugin( char * fname )
{
   AFNI_plugin * plin ;
   PLUGIN_interface * plint ;
   int nin ;

   /*----- sanity checks -----*/

ENTRY("PLUG_read_plugin") ;

   if( fname == NULL || strlen(fname) == 0 ) RETURN(NULL) ;
   if( ! THD_is_file(fname) )                RETURN(NULL) ;

   /*----- make space for new plugin -----*/

   plin = (AFNI_plugin *) XtMalloc( sizeof(AFNI_plugin) ) ;
   plin->type = AFNI_PLUGIN_TYPE ;

   /*----- copy name into plin structure -----*/

   MCW_strncpy( plin->libname , fname , MAX_PLUGIN_NAME ) ;

   /*----- open the library (we hope) -----*/

   DYNAMIC_OPEN( fname , plin->libhandle ) ;
   if( ! ISVALID_DYNAMIC_handle( plin->libhandle ) ){

#ifdef AFNI_DEBUG
{ char str[256]; sprintf(str,"failed to open library %s",fname); STATUS(str); }
#endif

      myXtFree(plin) ;
      RETURN(NULL) ;
   }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"opened library %s with handle %p" , fname,plin->libhandle ) ;
  STATUS(str) ; }
#endif

   /*----- find the required symbols -----*/

   DYNAMIC_SYMBOL(plin->libhandle, "PLUGIN_init" , plin->libinit_func );

   /*----- if symbols not found, complain and kill this plugin -----*/

   if( plin->libinit_func == (vptr_func *) NULL ){

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"library %s lacks required global symbol",fname) ; STATUS(str) ; }
#endif

      DYNAMIC_CLOSE( plin->libhandle ) ;
      myXtFree(plin) ;
      RETURN(NULL) ;
   }

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
      nin++ ;
   } while( plint != NULL ) ;

   plin->interface_count = nin ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"library %s created %d interfaces",fname,nin) ; STATUS(str) ;
#ifdef MALLOC_TRACE
   check_malloc() ;
#endif
}
#endif

   /*----- done -----*/

   RETURN(plin) ;
}

/*--------------------------------------------------------------------
   Routine to read in all plugins in the desired list of directories
----------------------------------------------------------------------*/

AFNI_plugin_array * PLUG_get_many_plugins(void)
{
   char * epath , * elocal , * eee ;
   char ename[THD_MAX_NAME] , efake[]="/usr/local/lib/afni:./" ;
   AFNI_plugin_array * outar , * tmpar ;
   int epos , ll , ii , id ;

   /*----- sanity checks -----*/

ENTRY("PLUG_get_many_plugins") ;

   epath = getenv("AFNI_PLUGINPATH") ;     /* get the path list to read from */

   if( epath == NULL )
      epath = getenv("AFNI_PLUGIN_PATH") ; /* try another name? */

   if( epath == NULL )
      epath = getenv("PATH") ;             /* try another name? */

   if( epath == NULL ) epath = efake ;     /* put in a fake path instead? */

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = (char *) XtMalloc( sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
      if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

#ifdef AFNI_DEBUG
{ STATUS("paths to be searched for plugins follows:") ;
  printf("%s\n",elocal) ; fflush(stdout) ; }
#endif

   /*----- extract blank delimited strings;
           use as directory names to get libraries -----*/

   INIT_PLUGIN_ARRAY( outar ) ;
   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , ename , &id ) ; /* next substring */
      if( ii < 1 ) break ;                                /* none --> end of work */

      /** check if ename occurs earlier in elocal **/

      eee = strstr( elocal , ename ) ;
      if( eee != NULL && (eee-elocal) < epos ){ epos += id ; continue ; }

      epos += id ;                                        /* char after last scanned */

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

#ifdef AFNI_DEBUG
{ char str[256] ; sprintf(str,"found %d plugins",outar->num) ; STATUS(str) ; }
#endif

   if( outar->num == 0 ) DESTROY_PLUGIN_ARRAY(outar) ;
   RETURN(outar) ;
}

/****************************************************************************
  Routines to create interface descriptions for new plugins. Usage:
    1) Use "new_PLUGIN_interface" to create the initial data structure.
    2) Use "add_option_to_PLUGIN_interface" to create an option line in the
         AFNI interface menu.  There is no built-in limit to the number
         of option lines that may be added to an AFNI interface menu.
    2(abcdef)
       Use "add_number_to_PLUGIN_interface"  , and
           "add_string_to_PLUGIN_interface"  , and
           "add_dataset_to_PLUGIN_interface"
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

   /*-- sanity check --*/

ENTRY("new_PLUGIN_interface") ;

   if( label == NULL || strlen(label) == 0 ) RETURN(NULL) ;

   if( !( (call_type == PLUGIN_CALL_IMMEDIATELY) ||
          (call_type == PLUGIN_CALL_VIA_MENU   )
        ) ) RETURN(NULL) ;

   if( call_func == (cptr_func *) NULL ) RETURN(NULL) ;

   /*-- create new interface --*/

   plint = (PLUGIN_interface *) XtMalloc(sizeof(PLUGIN_interface)) ;
   if( plint == NULL ) RETURN(NULL) ;

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

   RETURN(plint) ;
}

/*----------------------------------------------------------------------
   Routine to add a new option line to a plugin interface menu.

   plint      = PLUGIN_interface * which will have the option added
   label     = C string to be displayed in the menu describing this option
   tag       = C string to be passed to the plugin when this option is used
   mandatory = TRUE  (1) if this option is always passed to the plugin
               FALSE (0) if the user may or may not select this option
------------------------------------------------------------------------*/

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

   plint->option[nopt] = opt = (PLUGIN_option *) XtMalloc( sizeof(PLUGIN_option) ) ;

   /*-- put values in new option --*/

   PLUGIN_LABEL_strcpy( opt->label , label ) ;
   MCW_strncpy( opt->tag , tag , PLUGIN_STRING_SIZE ) ;

   opt->mandatory      = mandatory ;
   opt->subvalue_count = 0 ;

   opt->hint = NULL ;
   for( isv=0 ; isv < PLUGIN_MAX_SUBVALUES ; isv++ )
      opt->subvalue[isv].hint = NULL ;

   (plint->option_count)++ ;  /* one more option */
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
   if( nsv == PLUGIN_MAX_SUBVALUES ) EXRETURN ;

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
   if( nsv == PLUGIN_MAX_SUBVALUES ) EXRETURN ;

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_STRING_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   if( num_str > 0 ){
      sv->string_range_count = num_str ;
      for( ii=0 ; ii < num_str ; ii++ ){
         sv->string_range[ii] = XtMalloc( PLUGIN_STRING_SIZE ) ;
         MCW_strncpy( sv->string_range[ii] , strlist[ii] , PLUGIN_STRING_SIZE ) ;
      }
      sv->value_default   = defval ;
      sv->editable        = FALSE ;
   } else {
      sv->string_range_count = 0 ;
      sv->editable           = TRUE ;
      sv->value_default      = defval ;
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
   if( nsv == PLUGIN_MAX_SUBVALUES ) EXRETURN ;

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
   if( nsv == PLUGIN_MAX_SUBVALUES ) EXRETURN ;

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
   if( nsv == PLUGIN_MAX_SUBVALUES ) EXRETURN ;

   /*-- load values into next subvalue --*/

   sv = &(opt->subvalue[nsv]) ;

   sv->data_type = PLUGIN_TIMESERIES_TYPE ;
   PLUGIN_LABEL_strcpy( sv->label , label ) ;

   (opt->subvalue_count)++ ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

int PLUG_nonblank_len( char * str )
{
   int ii , ll ;

   if( str == NULL ) return 0 ;
   ll = strlen(str) ; if( ll == 0 ) return 0 ;

   for( ii=ll-1 ; ii >= 0 ; ii-- ) if( str[ii] != ' ' ) break ;

   return (ii+1) ;
}

/*--------------------------------------------------------------------------
  Decide if a dataset prefix is OK.  The string itself is checked for
  legality, then the list of all datasets is checked for duplicates.
----------------------------------------------------------------------------*/

int PLUTO_prefix_ok( char * str )
{
   int ll , ii ;
   THD_slist_find find ;

   /*--- check the string itself for OK-osity ---*/

   if( str == NULL ) return 0 ;
   ll = strlen( str ) ; if( ll == 0 ) return 0 ;

   for( ii=0 ; ii < ll ; ii++ )
      if( iscntrl(str[ii]) || isspace(str[ii]) ||
          str[ii] == '/'   || str[ii] == ';'   ||
          str[ii] == '*'   || str[ii] == '?'   ||
          str[ii] == '&'   || str[ii] == '|'   ||
          str[ii] == '"'   || str[ii] == '>'   ||
          str[ii] == '<'   || str[ii] == '\''  ||
          str[ii] == '['   || str[ii] == ']'     ) return 0 ;

   /*--- now see if the prefix already exists in AFNI ---*/

   find = THD_dset_in_sessionlist( FIND_PREFIX , str ,
                                   GLOBAL_library.sslist , -1 ) ;

   return (find.dset == NULL) ;
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
 { PLUG_quit_label , PLUG_action_CB , NULL , PLUG_quit_help ,"Close window"               , 0 } ,
 { PLUG_run_label  , PLUG_action_CB , NULL , PLUG_run_help  ,"Run plugin and keep window" , 0 } ,
 { PLUG_doit_label , PLUG_action_CB , NULL , PLUG_doit_help ,"Run plugin and close window", 1 } ,
 { PLUG_help_label , PLUG_action_CB , NULL , PLUG_help_help ,"Get help for plugin"        , 0 }
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
   Pixel  fg_pix ;

ENTRY("PLUG_setup_widgets") ;

   /**** sanity checks ****/

   if( plint == NULL || plint->wid != NULL ||
       plint->call_method == PLUGIN_CALL_IMMEDIATELY ) EXRETURN ;

   /**** create widgets structure ****/

   plint->wid = wid = myXtNew(PLUGIN_widgets) ;

   /**** create Shell that can be opened up later ****/

   wid->shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , plint->label , /* top of window */
           XmNiconName          , plint->label , /* label on icon */
           XmNmappedWhenManaged , False ,        /* must map it manually */
           XmNdeleteResponse    , XmDO_NOTHING , /* deletion handled below */
           XmNallowShellResize  , False ,        /* let code resize shell? */
           XmNinitialResourcesPersistent , False ,
      NULL ) ;

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
                     XmNtraversalOn , False ,
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
           XmNinitialResourcesPersistent , False ,
        NULL ) ;
    XmStringFree( xstr ) ;

    /* now that we have the label,
       find its sizes and make sure the shell doesn't get too small */

    MCW_widget_geom( wid->label , &ww , &hh , NULL , NULL ) ;
    XtVaSetValues( wid->shell ,
                      XmNminWidth  , ww+3*hh ,
                      XmNminHeight , (plint->option_count == 0) ? 5*hh : 7*hh ,
                   NULL ) ;

   /**** create an action area beneath to hold user control buttons ****/

   for( ib=0 ; ib < NUM_PLUG_ACT ; ib++ ) PLUG_act[ib].data = (XtPointer) plint ;

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
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

      wframe =
         XtVaCreateWidget(
           "AFNI" , xmFrameWidgetClass , wid->scrollw ,
               XmNshadowType , XmSHADOW_ETCHED_IN ,
               XmNshadowThickness , 5 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      wid->workwin =
         XtVaCreateWidget(
           "AFNI" , xmFormWidgetClass , wframe ,
              XmNborderWidth , 0 ,
              XmNborderColor , 0 ,
              XmNtraversalOn , False ,
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

      opt->label[ opt_lwid+1 ] = '\0' ;
      for( ib=0 ; ib < opt->subvalue_count ; ib++ ){
         sv = &(opt->subvalue[ib]) ;
         sv->label[ sv_lwid[ib] + 1 ] = '\0' ;
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

              XmNset              , (opt->mandatory) ? True  : False ,
              XmNsensitive        , (opt->mandatory) ? False : True  ,

              XmNindicatorSize    , hh-1 ,
              XmNmarginHeight     , 0  ,
              XmNmarginWidth      , 0  ,
              XmNselectColor      , fg_pix ,  /* fill with foreground when set */

              XmNtraversalOn , False ,
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

      if( opt->mandatory )
         MCW_register_hint( ow->toggle ,
                            "This input line is mandatory" ) ;
      else
         MCW_register_hint( ow->toggle ,
                            "IN: this input line sent to plugin" ) ;

      /* this callback will change the appearance of the
         option's row of widgets when the toggle button is pressed */

      if( ! opt->mandatory )
         XtAddCallback( ow->toggle , XmNvalueChangedCallback ,
                        PLUG_optional_toggle_CB , (XtPointer) ow ) ;

      /** create Label to describe this option **/

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
              XmNuserData    , (XtPointer) zlen ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
      XmStringFree( xstr ) ;
      if( opt->mandatory && !zlen ){
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

               if( use_optmenu && num_choice > OP_OPTMENU_COLSIZE )
                  AVOPT_columnize(av, 1+(num_choice-1)/OP_OPTMENU_COLSIZE) ;

               ow->chooser[ib] = (void *) av ;
               ow->chtop[ib]   = av->wrowcol ;  /* get the top widget */

               ow->chooser_type[ib] = (use_optmenu) ? OP_CHOOSER_OPTMENU
                                                    : OP_CHOOSER_NUMBER ;

               if( !use_optmenu ) av->allow_wrap = 1 ;
               if(  use_optmenu ) toff-- ;
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

                  if( use_optmenu && num_choice > OP_OPTMENU_COLSIZE )
                     AVOPT_columnize(av, 1+(num_choice-1)/OP_OPTMENU_COLSIZE) ;

                  ow->chooser[ib] = (void *) av ;
                  ow->chtop[ib]   = av->wrowcol ;  /* get the top widget */

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
                          XmNtraversalOn , False ,
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
                           XmNtraversalOn , False ,
                           XmNinitialResourcesPersistent , False ,
                        NULL ) ;

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
                       XmNtraversalOn , False ,
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
                              XmNtraversalOn   , False ,
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
                       XmNtraversalOn , False ,
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

               xstr = XmStringCreateLtoR( "-Choose Timeseries- ",XmFONTLIST_DEFAULT_TAG ) ;

               av->pb = XtVaCreateManagedWidget(
                           "AFNI" , xmPushButtonWidgetClass , av->rowcol ,
                              XmNlabelString   , xstr ,
                              XmNmarginHeight  , 0 ,
                              XmNmarginWidth   , 0 ,
                              XmNrecomputeSize , False ,
                              XmNtraversalOn   , False ,
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

         if( !opt->mandatory ) XtSetSensitive( ow->chtop[ib] , False ) ;

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

   /**** Manage the managers, and go home ****/

   if( plint->option_count > 0 ){
      XtManageChild( wid->workwin ) ;
      XtManageChild( wframe ) ;
      XtManageChild( wid->scrollw ) ;
   }
   XtManageChild( wid->form ) ;

   XtRealizeWidget( wid->shell ) ;  /* will not be mapped */

   /** set its width after it is mapped **/

#define OPC_MAX     8
#define OPC_MAXMAX 10
#define LUCK        5

   if( wframe != NULL ){
      Widget bar ;
      int fww , fhh , fyy , bww ;

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

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   What happens when a plugin interface action button is pressed
-------------------------------------------------------------------------*/

void PLUG_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) cd ;
   char * wname = XtName(w) ;
   char * mesg ;
   int close , run , badrun=0 , help ;

ENTRY("PLUG_action_CB") ;

   run   = (strcmp(wname,PLUG_doit_label)==0) || (strcmp(wname,PLUG_run_label) ==0);
   close = (strcmp(wname,PLUG_doit_label)==0) || (strcmp(wname,PLUG_quit_label)==0);
   help  = (strcmp(wname,PLUG_help_label)==0) ;

   if( run ){
      PLUG_fillin_values( plint ) ;       /* load callvalues  */
      plint->opnum = plint->svnum = -1 ;  /* initialize get_  */

      /***** CALL THE PLUGIN !!!! *****/

      SHOW_AFNI_PAUSE ;
      mesg = plint->call_func( plint ) ;
      SHOW_AFNI_READY ;

      PLUTO_popdown_meter( plint ) ;  /* if the user forgets */

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

   if( help )
      (void) MCW_popup_message( plint->wid->label ,
                                plint->helpstring , MCW_USER_KILL ) ;

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
   XtPointer xptr ;

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
   return ;
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

           /** number type: uses arrowval interface;
                            send in the float value **/

           case PLUGIN_NUMBER_TYPE:{
              MCW_arrowval * av = (MCW_arrowval *) ow->chooser[ib] ;
              float * fptr ;

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

   if( plint == NULL ) return NULL ;
   else                return plint->label ;
}

char * get_description_from_PLUGIN_interface( PLUGIN_interface * plint )
{

   if( plint == NULL ) return NULL ;
   else                return plint->description ;
}

/*-----------------------------------------------------------------------
   Get the next chosen option, and return its string "tag".
   If there is no "next chosen option", return NULL.
-------------------------------------------------------------------------*/

char * get_optiontag_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   int iopt ;
   PLUGIN_option * opt ;

   if( plint == NULL ) return NULL ;

   iopt = plint->opnum + 1 ;
   while( iopt < plint->option_count ){

      opt = plint->option[iopt++] ;
      if( opt == NULL ) continue ; /* bad? */
      if( ! opt->chosen ) continue ; /* not used this time */

      plint->opnum = iopt-1 ;      /* keep track of which option */
      plint->svnum = 0 ;           /* start at 1st subvalue */
      return opt->tag ;
   }

   plint->opnum = plint->option_count ;
   return NULL ;
}

/*-------------------------------------------------------------------------
   Find out what the next chosen option is, without actually retrieving it.
---------------------------------------------------------------------------*/

char * peek_optiontag_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   int iopt ;
   PLUGIN_option * opt ;

   if( plint == NULL ) return NULL ;

   iopt = plint->opnum + 1 ;
   while( iopt < plint->option_count ){
      opt = plint->option[iopt++] ;
      if( opt == NULL ) continue ; /* bad? */
      if( opt->chosen ) return opt->tag ;
   }
   return NULL ;
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

   if( plint == NULL ) return ILLEGAL_TYPE ;
   if( plint->opnum >= plint->option_count ) return ILLEGAL_TYPE ;

   opt = plint->option[ plint->opnum ] ;
   if( opt == NULL ) return ILLEGAL_TYPE ;

   isv = plint->svnum ;
   if( isv >= opt->subvalue_count ) return ILLEGAL_TYPE ;

   return opt->subvalue[isv].data_type ;
}

/*-------------------------------------------------------------------------
   Get the next subvalue, which should be of the type given by
   "type".  If it is not, return NULL, otherwise return a "void *",
   which must be properly de-referenced to get the true value:

     type                    output is really
     ------------------      ----------------
     PLUGIN_NUMBER_TYPE       float *
     PLUGIN_STRING_TYPE       char *
     PLUGIN_DATASET_TYPE      MCW_idcode *
     PLUGIN_TIMESERIES_TYPE   MRI_IMAGE **

   Following this are convenience routines to do similar work on specific
   data types.
---------------------------------------------------------------------------*/

void * get_callvalue_from_PLUGIN_interface( PLUGIN_interface * plint , int type )
{
   int isv ;
   PLUGIN_option * opt ;

   if( plint == NULL ) return( NULL );

   opt = plint->option[ plint->opnum ] ;
   if( opt == NULL ) return( NULL );

   isv = plint->svnum ;
   if( isv >= opt->subvalue_count ) return( NULL );

   if( opt->subvalue[isv].data_type != type ) return( NULL );

   plint->svnum ++ ;
   return( opt->callvalue[isv] );
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * get_timeseries_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   MRI_IMAGE ** imp ;

   imp = (MRI_IMAGE **)
         get_callvalue_from_PLUGIN_interface(plint,PLUGIN_TIMESERIES_TYPE) ;

   if( imp == NULL ) return NULL ;
   return *imp ;
}

/*----------------------------------------------------------------------------*/

float get_number_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   float * fp ;
   fp = (float *)get_callvalue_from_PLUGIN_interface(plint,PLUGIN_NUMBER_TYPE) ;
   if( fp == NULL ) return BAD_NUMBER ;
   return *fp ;
}

/*----------------------------------------------------------------------------*/

char * get_string_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   return
     (char *) get_callvalue_from_PLUGIN_interface(plint,PLUGIN_STRING_TYPE) ;
}

/*----------------------------------------------------------------------------*/

MCW_idcode * get_idcode_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   return
     (MCW_idcode *)get_callvalue_from_PLUGIN_interface(plint,PLUGIN_DATASET_TYPE) ;
}

/*----------------------------------------------------------------------------*/

MCW_idclist * get_idclist_from_PLUGIN_interface( PLUGIN_interface * plint )
{
   MCW_idclist ** llist ;

   llist = (MCW_idclist **)
           get_callvalue_from_PLUGIN_interface(plint,PLUGIN_DATASET_LIST_TYPE) ;

   if( llist != NULL ) return *llist ;
   return NULL ;
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

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;

   if( plint == NULL || av == NULL ) return ;
   sv = av->sv ;
   if( sv == NULL ) return ;
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

      /* check anat datasets, if reasonable */

      if( sv->dset_anat_mask != 0 ){
         for( id=0 ; id < ss->num_anat ; id++ ){
            dset = ss->anat[id][vv] ;

            if( ! PLUGIN_dset_check( sv->dset_anat_mask ,
                                     sv->dset_ctrl_mask , dset ) ) continue ;

            /* if we get here, then this dataset is OK to choose! */

            num_dset++ ;
            av->dset_link = (PLUGIN_dataset_link *)
                             XtRealloc( (char *) av->dset_link ,
                                        sizeof(PLUGIN_dataset_link)*num_dset ) ;

            make_PLUGIN_dataset_link( dset , av->dset_link + (num_dset-1) ) ;

         }
      } /* end of loop over anat datasets */

      /* do the same for func datasets */

      if( sv->dset_func_mask != 0 ){
         for( id=0 ; id < ss->num_func ; id++ ){
            dset = ss->func[id][vv] ;

            if( ! PLUGIN_dset_check( sv->dset_func_mask ,
                                     sv->dset_ctrl_mask , dset ) ) continue ;

            /* if we get here, then this dataset is OK to choose! */

            num_dset++ ;
            av->dset_link = (PLUGIN_dataset_link *)
                             XtRealloc( (char *) av->dset_link ,
                                        sizeof(PLUGIN_dataset_link)*num_dset ) ;

            make_PLUGIN_dataset_link( dset , av->dset_link + (num_dset-1) ) ;

         }
      } /* end of loop over func datasets */

   } /* end of loop over sessions */

   /*--- if nothing was found that fits, then nothing further can happen ---*/

   if( num_dset == 0 ){
      av->dset_count  = 0 ;
      av->dset_choice = -1 ;
      myXtFree(old_chosen) ;
      XBell( XtDisplay(w) , 100 ) ;
      return ;
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
            sprintf(qnam,"%-*s [%s:3D+t]" ,
                    ltop,av->dset_link[id].title ,
                    ANAT_prefixstr[dset->func_type] ) ;

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
            sprintf(qnam,"%-*s [%s:3D+t]" ,
                    ltop,av->dset_link[id].title ,
                    FUNC_prefixstr[dset->func_type] ) ;
      }
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
   return ;
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

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;
   if( plint == NULL || av == NULL ) return ;

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

   return ;
}

/*-----------------------------------------------------------------------
  Fill in a PLUGIN_dataset_link
-------------------------------------------------------------------------*/

void make_PLUGIN_dataset_link( THD_3dim_dataset * dset ,
                               PLUGIN_dataset_link * dsl )
{
   char nam[THD_MAX_NAME] ;
   char * tnam ;

   /*-- sanity checks --*/

   if( dsl == NULL ) return ;

   if( ! ISVALID_3DIM_DATASET(dset) ){
      strcpy( dsl->title , "* garbage *" ) ;
      ZERO_IDCODE( dsl->idcode ) ;
      return ;
   }

   /*-- make title (cf. AFNI_set_window_titles) --*/

   strcpy( nam , dset->dblk->diskptr->directory_name ) ;
   strcat( nam , dset->dblk->diskptr->filecode ) ;
   tnam = THD_trailname(nam,1) ;
   MCW_strncpy( dsl->title , tnam , PLUGIN_STRING_SIZE ) ;

   /*-- copy idcode --*/

   dsl->idcode = dset->idcode ;

   return ;
}

/*-----------------------------------------------------------------------
  Determine if a dataset passes the type_mask and ctrl_mask
  criteria for acceptability.
-------------------------------------------------------------------------*/

int PLUGIN_dset_check( int type_mask , int ctrl_mask , THD_3dim_dataset * dset )
{
   int itmp ;

   if( ! ISVALID_3DIM_DATASET(dset) ) return 0 ;

   if( ((1 << dset->func_type) & type_mask) == 0 ) return 0 ;

   itmp = (DSET_NUM_TIMES(dset) > 1) ? DIMEN_4D_MASK : DIMEN_3D_MASK ;
   if( (itmp & ctrl_mask) == 0 ) return 0 ;

   if( !DSET_INMEMORY(dset) && (ctrl_mask & WARP_ON_DEMAND_MASK) == 0 ) return 0 ;

   itmp = DSET_PRINCIPAL_VALUE(dset) ;  /* get the type of */
   itmp = DSET_BRICK_TYPE(dset,itmp) ;  /* the "principal" brick */

   if( itmp == MRI_byte    && (ctrl_mask & BRICK_BYTE_MASK)    == 0 ) return 0 ;
   if( itmp == MRI_short   && (ctrl_mask & BRICK_SHORT_MASK)   == 0 ) return 0 ;
   if( itmp == MRI_float   && (ctrl_mask & BRICK_FLOAT_MASK)   == 0 ) return 0 ;
   if( itmp == MRI_complex && (ctrl_mask & BRICK_COMPLEX_MASK) == 0 ) return 0 ;

   return 1 ;
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

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;

   if( plint == NULL || av == NULL ) return ;
   sv = av->sv ;
   if( sv == NULL ) return ;
   im3d = plint->im3d ;

   av->tsimar = GLOBAL_library.timeseries ; /* to choose amongst */
   if( av->tsimar==NULL || IMARR_COUNT(av->tsimar)==0 ){
      av->ts_choice = -1 ;
      av->tsim      = NULL ;
      XBell( XtDisplay(w) , 100 ) ;
      return ;
   }

   init_ts = AFNI_ts_in_library( av->tsim ) ;

   MCW_choose_timeseries( w , "Choose Timeseries" ,
                          av->tsimar , init_ts ,
                          PLUG_finalize_timeseries_CB , (XtPointer) plint ) ;

   return ;
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

   /** find the stuff that is associated with this button **/

   XtVaGetValues( w , XmNuserData , &av , NULL ) ;
   if( plint == NULL || av == NULL || av->tsimar == NULL ) return ;
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

   return ;
}

/********************************************************************************
   Routine to interface to AFNI, after plugins are read in.
*********************************************************************************/

void AFNI_plugin_button( Three_D_View * im3d )
{
   AFNI_plugin_array * exten = GLOBAL_library.plugins ;
   AFNI_plugin * plug ;
   int pp , ipl , nbut ;
   Widget rc , mbar , menu , cbut , pbut , wpar , sep ;
   XmString xstr ;

   /*-- check inputs for legality --*/

   if( exten == NULL      ||
       ! IM3D_VALID(im3d) || im3d->type != AFNI_3DDATA_VIEW ) return ;

   /*-- create menu bar --*/

   wpar = im3d->vwid->dmode->mbar_rowcol ;

   rc =  XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , wpar ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   mbar = XmCreateMenuBar( rc , "dialog" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , False ,
                     XmNbackground   , im3d->dc->pixov_brightest ,
                  NULL ) ;
   XtManageChild( mbar ) ;

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;

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
               XmNtraversalOn  , False ,
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
                  XmNtraversalOn , False ,                                \
                  XmNinitialResourcesPersistent , False ,                 \
               NULL ) ;                                                   \
      XtAddCallback( pbut , XmNactivateCallback ,                         \
                     PLUG_startup_plugin_CB , (XtPointer)(pl) ) ;         \
      XmStringFree(xstr) ;                                                \
      if( (pl)->hint != NULL ) MCW_register_hint( pbut , (pl)->hint ) ;   \
   } while(0)

   /*** top of menu = a label to click on that does nothing at all ***/

   xstr = XmStringCreateLtoR( "-- Cancel --" , XmFONTLIST_DEFAULT_TAG ) ;
   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree(xstr) ;

   sep = XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   nbut = 2 ;

   /*** make buttons for each interface ***/

   for( pp=0 ; pp < exten->num ; pp++ ){
      plug = exten->plar[pp] ;
      for( ipl=0 ; ipl < plug->interface_count ; ipl++ ){
         MENU_BUT( plug->interface[ipl] ) ;
         nbut++ ;
      }
   }

#define COLSIZE 20
   if( nbut > COLSIZE ){
      int ncol = (nbut-2)/COLSIZE + 1 ;
      XtDestroyWidget(sep) ;
      XtVaSetValues( menu ,
                        XmNpacking , XmPACK_COLUMN ,
                        XmNnumColumns , ncol ,
                     NULL ) ;
   }

   XtManageChild( rc ) ;
   return ;
}

/*------------------------------------------------------------------------
   Routine that actually starts up a plugin when the user makes
   his or her choice from the menu created above.
--------------------------------------------------------------------------*/

void PLUG_startup_plugin_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   PLUGIN_interface * plint = (PLUGIN_interface *) cd ;
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) cbd ;
   char * mesg ;
   Widget wpop ;
   Three_D_View * im3d = NULL ;

ENTRY("PLUG_startup_plugin_CB") ;

   if( plint == NULL ) EXRETURN ;  /* error? */

   XtVaGetValues( w , XmNuserData , &im3d , NULL ) ;  /* set controller from */
   plint->im3d = im3d ;                               /* data on menu button */

   /*-- if no interface is needed, just call it --*/

   if( plint->call_method == PLUGIN_CALL_IMMEDIATELY ){

STATUS("calling plugin") ;

      SHOW_AFNI_PAUSE ;
      mesg = plint->call_func( plint ) ;
      SHOW_AFNI_READY ;

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

   { static char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ; /* see afni_func.c */
     char ttl[PLUGIN_STRING_SIZE] ;
     int ic ;

     ic = AFNI_controller_index( im3d ) ;  /* find out which controller */

     if( ic >=0 && ic < 26 )
        sprintf( ttl , "[%c] %s" , clabel[ic] , plint->label ) ;
     else
        strcpy( ttl , plint->label ) ;

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
      int xx = (int) xev->x_root , yy = (int) xev->y_root ;
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
   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Routine to add a dataset to the AFNI global collection.
   Return value is 0 if all is OK, 1 if bad things happened.
   "action_flag" is the OR (|) of various possibilities:
      DSET_ACTION_NONE         == do nothing
      DSET_ACTION_MAKE_CURRENT == make this the currently viewed dataset
-----------------------------------------------------------------------------*/

int PLUTO_add_dset( PLUGIN_interface * plint ,
                     THD_3dim_dataset * dset , int action_flag )
{
   Three_D_View * im3d ;
   THD_session * sess ;
   int iss , vv , id ;
   int make_current = (action_flag & DSET_ACTION_MAKE_CURRENT) ;

   /** sanity check **/

   if( plint == NULL || ! ISVALID_3DIM_DATASET(dset) ) return 1 ;

   /** find some indices **/

   im3d = plint->im3d ;
   iss  = IM3D_VALID(im3d) ? im3d->vinfo->sess_num : 0 ;
   sess = GLOBAL_library.sslist->ssar[iss] ;
   vv   = dset->view_type ;

   /** add the dataset to the session **/

   if( ISANAT(dset) ){
      id = sess->num_anat ;
      if( id >= THD_MAX_SESSION_ANAT ){
         fprintf(stderr,"*** Overflow anat dataset limit ***\n") ;
         return 1 ;
      }
      sess->anat[id][vv] = dset ;
      (sess->num_anat)++ ;
   } else if( ISFUNC(dset) ){
      id = sess->num_func ;
      if( id >= THD_MAX_SESSION_FUNC ){
         fprintf(stderr,"*** Overflow func dataset limit ***\n") ;
         return 1 ;
      }
      sess->func[id][vv] = dset ;
      (sess->num_func)++ ;
   } else {
      fprintf(stderr,"*** Bizarre type error in PLUTO_add_dset!\n") ;
      return 1 ;
   }

   /** make sure the dataset is properly fit into the situation **/

   POPDOWN_strlist_chooser ;  /* added dataset --> old choosers are invalid */

   THD_load_statistics( dset ) ;
   THD_write_3dim_dataset( NULL,NULL , dset , True ) ;

   AFNI_force_adoption( sess , GLOBAL_argopt.warp_4D ) ;
   AFNI_make_descendants( GLOBAL_library.sslist ) ;

   /** if desired, jump to this puppy in the viewer **/

   if( make_current && IM3D_VALID(im3d) ){
      if( ISANAT(dset) )
         im3d->vinfo->anat_num = sess->num_anat - 1 ;
      else
         im3d->vinfo->func_num = sess->num_func - 1 ;

      AFNI_initialize_view( im3d->anat_now , im3d ) ;
   }

   THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_ANY ) ;
   return 0 ;
}

/*---------------------------------------------------------------------
   Routine to make a copy of a dataset, with data attached.
-----------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_copy_dset( THD_3dim_dataset * dset , char * new_prefix )
{
   THD_3dim_dataset * new_dset ;
   int ival , ityp , nbytes , nvals ;
   void * new_brick , * old_brick ;

   /*-- sanity check --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   /*-- make the empty copy --*/

   new_dset = EDIT_empty_copy( dset ) ;

   /*-- change its name? --*/

   if( new_prefix != NULL )
      EDIT_dset_items( new_dset ,
                          ADN_prefix , new_prefix ,
                          ADN_label1 , new_prefix ,
                       ADN_none ) ;

   /*-- make brick(s) for this dataset --*/

   THD_load_datablock( dset->dblk , NULL ) ;  /* make sure old one is in memory */

   nvals = DSET_NVALS(dset) ;

   for( ival=0 ; ival < nvals ; ival++ ){
      ityp      = DSET_BRICK_TYPE(new_dset,ival) ;   /* type of data */
      nbytes    = DSET_BRICK_BYTES(new_dset,ival) ;  /* how much data */
      new_brick = malloc( nbytes ) ;                 /* make room */

      if( new_brick == NULL ){
        THD_delete_3dim_dataset( new_dset , False ) ;
        return NULL ;
      }

      EDIT_substitute_brick( new_dset , ival , ityp , new_brick ) ;

      /*-- copy data from old brick to new brick --*/

      old_brick = DSET_BRICK_ARRAY(dset,ival) ;

      if( old_brick == NULL ){
         THD_delete_3dim_dataset( new_dset , False ) ;
         return NULL ;
      }

      memcpy( new_brick , old_brick , nbytes ) ;
   }

   return new_dset ;
}

/*----------------------------------------------------------------------
   Routine to force AFNI to redisplay images in all open controllers.
------------------------------------------------------------------------*/

void PLUTO_force_redisplay(void)
{
   Three_D_View * im3d ;
   int ii ;

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      im3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_OPEN(im3d) ){
         im3d->anat_voxwarp->type =                       /* 11 Jul 1997 */
            im3d->fim_voxwarp->type = ILLEGAL_TYPE ;
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
      }
   }
   return ;
}

/*----------------------------------------------------------------------
   Routine to inform AFNI that some dataset names are now different,
   so stuff should be done about it.
------------------------------------------------------------------------*/

void PLUTO_fixup_names(void)
{
   Three_D_View * im3d ;
   int ii ;

   POPDOWN_strlist_chooser ;  /* get rid of any dataset chooser that is open */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      im3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_OPEN(im3d) )
         AFNI_set_window_titles( im3d ) ;
   }
   return ;
}

/*-----------------------------------------------------------------------
   Let the user popup a message
-------------------------------------------------------------------------*/

void PLUTO_popup_worker( PLUGIN_interface * plint , char * mesg , int flag )
{
   Widget w = NULL ;
   Three_D_View * im3d ;
   int ii ;

   if( mesg == NULL || strlen(mesg) == 0 ) return ;

   /* find a widget to popup next to */

   if( plint->wid != NULL && plint->wid->label != NULL ){
      w = plint->wid->label ;
   } else {
      im3d = plint->im3d ;
      if( ! IM3D_VALID(im3d) ){
         for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
            im3d = GLOBAL_library.controllers[ii] ;
            if( IM3D_OPEN(im3d) ) break ;
         }
      }
      w = im3d->vwid->top_shell ;
   }

   if( w != NULL )
      (void) MCW_popup_message( w , mesg , flag ) ;
   else
      fprintf(stderr,"\n%s\a\n",mesg) ;

   return ;
}

void PLUTO_beep(void)
{
   XBell( GLOBAL_library.dc->display , 100 ) ;
   return ;
}

int PLUTO_string_index( char * target , int num , char * source[] )
{
   int ii ;

   if( num <= 0 || source == NULL || target == NULL ) return -1 ;

   for( ii=0 ; ii < num ; ii++ )
      if( strncmp(target,source[ii],PLUGIN_STRING_SIZE) == 0 ) return ii ;

   return -1 ;
}

/*-----------------------------------------------------------------------
  Routines to put a progress meter on the top of the interface shell
-------------------------------------------------------------------------*/

void PLUTO_popup_meter( PLUGIN_interface * plint )
{
   if( plint == NULL             || plint->wid == NULL       ||
       plint->wid->shell == NULL || plint->wid->meter != NULL  ) return ;

   plint->wid->meter = MCW_popup_meter( plint->wid->shell , METER_TOP_WIDE ) ;
   return ;
}

void PLUTO_popdown_meter( PLUGIN_interface * plint )
{
   if( plint == NULL             || plint->wid == NULL       ||
       plint->wid->shell == NULL || plint->wid->meter == NULL  ) return ;

   MCW_popdown_meter( plint->wid->meter ) ;
   plint->wid->meter = NULL ;
   return ;
}

void PLUTO_set_meter( PLUGIN_interface * plint , int percent )
{
   if( plint == NULL             || plint->wid == NULL       ||
       plint->wid->shell == NULL || plint->wid->meter == NULL  ) return ;

   MCW_set_meter( plint->wid->meter , percent ) ;
   return ;
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

   /*-- input image is NULL ==> popdown, if applicable --*/

   if( im == NULL ){
      if( imp != NULL )
         drive_MCW_imseq( imp->seq , isqDR_destroy , NULL ) ;

      return (void *) imp ;
   }

   /*-- input = no popper handle ==> create one --*/

   if( imp == NULL ){
      imp      = myXtNew(PLUGIN_impopper) ;
      imp->seq = NULL ; imp->im  = NULL ;
   }

   /*-- input = non-null image ==> replace image --*/

   mri_free( imp->im ) ;                   /* toss old copy */
   imp->im = mri_to_mri( im->kind , im ) ; /* make new copy */

   /*-- input = inactive popper handle ==> activate it --*/

   if( imp->seq == NULL ){
      imp->seq = open_MCW_imseq( GLOBAL_library.dc ,
                                 PLUGIN_imseq_getim , (XtPointer) imp ) ;

      drive_MCW_imseq( imp->seq , isqDR_realize, NULL ) ;
      drive_MCW_imseq( imp->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
   }

   /*-- display image at last --*/

   if( im->name != NULL && strlen(im->name) > 0 )
      drive_MCW_imseq( imp->seq , isqDR_title, im->name ) ;

   drive_MCW_imseq( imp->seq , isqDR_clearstat , NULL ) ;
   drive_MCW_imseq( imp->seq , isqDR_reimage , (XtPointer) 0 ) ;

   return (void *) imp ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq for PLUGIN_popup_image.
   Just returns the control information, or the given image.
--------------------------------------------------------------------*/

XtPointer PLUGIN_imseq_getim( int n , int type , XtPointer handle )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

   if( imp == NULL ) return NULL ;

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

      return (XtPointer) stat ;
   }

   /*--- no overlay ---*/

   if( type == isqCR_getoverlay ) return NULL ;

   /*--- return a copy of the image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE * im = NULL ;
      if( imp->im != NULL ) im = mri_to_mri( imp->im->kind , imp->im ) ;
      return (XtPointer) im ;
   }

   return NULL ;  /* should not occur, but who knows? */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

void PLUGIN_seq_send_CB( MCW_imseq * seq , XtPointer handle , ISQ_cbs * cbs )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

   if( imp == NULL ) return ;

   switch( cbs->reason ){

      case isqCR_destroy:{
         XtFree((char*)imp->seq) ; imp->seq = NULL ;
         mri_free( imp->im )     ; imp->im  = NULL ;
      }
      break ;
   }
   return ;
}

/*-------------------------------------------------------------------------*/

/*-- 13 Dec 1997: moved guts into 3dmaker.c --*/

THD_3dim_dataset * PLUTO_4D_to_typed_fim( THD_3dim_dataset * old_dset ,
                                          char * new_prefix , int new_datum ,
                                          int ignore , int detrend ,
                                          generic_func * user_func ,
                                          void * user_data )
{
   THD_3dim_dataset * new_dset ;  /* output dataset */

   if( ! PLUTO_prefix_ok(new_prefix) ) return NULL ;

   new_dset = MAKER_4D_to_typed_fim( old_dset , new_prefix , new_datum ,
                                     ignore , detrend , user_func , user_data ) ;

   return new_dset ;
}

/*-------------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_4D_to_typed_fith( THD_3dim_dataset * old_dset ,
                                           char * new_prefix , int new_datum ,
                                           int ignore , int detrend ,
                                           generic_func * user_func ,
                                           void * user_data )
{
   THD_3dim_dataset * new_dset ;  /* output dataset */

   if( ! PLUTO_prefix_ok(new_prefix) ) return NULL ;

   new_dset = MAKER_4D_to_typed_fith( old_dset , new_prefix , new_datum ,
                                      ignore , detrend , user_func , user_data ) ;

   return new_dset ;
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

   if( ! PLUTO_prefix_ok(new_prefix) ) return NULL ;

   new_dset = MAKER_4D_to_typed_fbuc( old_dset, new_prefix, new_datum ,
                                      ignore, detrend, nbrik, user_func, user_data ) ;

   return new_dset ;
}


/*------------------------------------------------------------------------*/
static int num_workp      = 0 ;
static XtWorkProc * workp = NULL ;
static XtPointer *  datap = NULL ;
static XtWorkProcId wpid ;

void PLUTO_register_workproc( XtWorkProc func , XtPointer data )
{
   if( func == NULL ) return ;

   if( num_workp == 0 ){
      workp = (XtWorkProc *) malloc( sizeof(XtWorkProc) ) ;
      datap = (XtPointer *)  malloc( sizeof(XtPointer) ) ;
      wpid  = XtAppAddWorkProc( PLUTO_Xt_appcontext, PLUG_workprocess, NULL ) ;
   } else {
      workp = (XtWorkProc *) realloc( workp, sizeof(XtWorkProc)*(num_workp+1) ) ;
      datap = (XtPointer*)   realloc( datap, sizeof(XtPointer) *(num_workp+1) ) ;
   }

   workp[num_workp] = func ;
   datap[num_workp] = data ;
   num_workp++ ;

/**
fprintf(stderr,"Now have %d workprocs\n",num_workp) ;
**/

   return ;
}

void PLUTO_remove_workproc( XtWorkProc func )
{
   int ii , ngood ;

   if( func == NULL || num_workp == 0 ) return ;

   for( ii=0 ; ii < num_workp ; ii++ ){
      if( func == workp[ii] ) workp[ii] = NULL ;
   }

   for( ii=0,ngood=0 ; ii < num_workp ; ii++ )
      if( workp[ii] != NULL ) ngood++ ;

   if( ngood == 0 ){
fprintf(stderr,"No workprocs left\n") ;
      XtRemoveWorkProc( wpid ) ;
      free(workp) ; workp = NULL ; free(datap) ; datap = NULL ;
      num_workp = 0 ;
   } else {
fprintf(stderr,"%d workprocs left\n",ngood) ;
   }

   return ;
}

Boolean PLUG_workprocess( XtPointer fred )
{
   int ii , ngood ;
   Boolean done ;

   if( num_workp == 0 ) return True ;

   for( ii=0,ngood=0 ; ii < num_workp ; ii++ ){
      if( workp[ii] != NULL ){
         done = workp[ii]( datap[ii] ) ;
         if( done == True ) workp[ii] = NULL ;
         else               ngood++ ;
      }
   }

   if( ngood == 0 ){
fprintf(stderr,"Found no workprocs left\n") ;
      free(workp) ; workp = NULL ; free(datap) ; datap = NULL ;
      num_workp = 0 ;
      return True ;
   }
   return False ;
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
   struct tms ttt ;

   (void) times( &ttt ) ;
   return (  (double) (ttt.tms_utime
                                     /* + ttt.tms_stime */
                      )
           / (double) CLK_TCK ) ;
}

void PLUTO_report( PLUGIN_interface * plint , char * str )
{
   if( plint == NULL || str == NULL ) return ;
   printf("\n%15.15s= %s" , plint->label , str ) ;
   return ;
}

/**************************************************************************/
/*========================================================================*/
/*============ These must remain the last lines of this file! ============*/

/** put library routines here that must be loaded **/

static vptr_func * forced_loads[] = {
   (vptr_func *) startup_lsqfit ,
   (vptr_func *) delayed_lsqfit ,
   (vptr_func *) mri_align_dfspace ,
NULL } ;

vptr_func * MCW_onen_i_estel_edain(int n){
  return forced_loads[n] ;
}

#else  /* not ALLOW_PLUGINS */

void * MCW_onen_i_estel_edain(int n){} ;  /* dummy routine */

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
  Routine to find a dataset in the global sessionlist, given its idcode.
  If this returns NULL, then you are SOL.
------------------------------------------------------------------------------*/

THD_3dim_dataset * PLUTO_find_dset( MCW_idcode * idcode )
{
   THD_slist_find find ;

   if( idcode == NULL || ISZERO_IDCODE(*idcode) ) return NULL ;

   find = THD_dset_in_sessionlist( FIND_IDCODE , idcode ,
                                   GLOBAL_library.sslist , -1 ) ;

   return find.dset ;
}
