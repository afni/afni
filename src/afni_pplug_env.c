/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"

#ifndef ALLOW_PLUGINS
void ENV_init(void){}
#else

/***********************************************************************
  Pseudo-plugin to set/show environment variables
************************************************************************/

#define USE_SESSTRAIL

/*--------------------- string to 'help' the user --------------------*/

static char help_start[] =
  "Purpose: control the AFNI environment variables.\n"
  "\n"
  "The environment variables controlled from this plugin affect the\n"
  "operation of various AFNI components.  Full documentation is in\n"
  "the file README.environment.  These environment variables may also\n"
  "also set from the command line, your login script, or from your\n"
  ".afnirc file.\n"
  "\n"
  "Some environment variables CANNOT be set in this plugin, since the\n"
  "choices they imply are irrevocable, they only affect the startup\n"
  "operation of AFNI, or the parameters they control can be set by\n"
  "other AFNI controls.  Some of these 'fixed' variables are:\n"
;

static char help_mid[] =
  "\n"
  "The variables that CAN be set by this plugin are:\n"
;

static char help_end[] =
  "\n"
  "The variables shown above may be set using the plugin interface.\n"
  "As usual, one of the 'Run' buttons must be used for any changes\n"
  "you make to be communicated to AFNI.  Depending on the variables\n"
  "that are altered, there may be no immediate effects visible in the\n"
  "AFNI interface, until you take some action that is dependent on\n"
  "the variable settings.\n"
  "\n"
  "Author -- RW Cox -- June 2000"
;

/*----------- list of variables that CANNOT be edited ---------------------*/

static char * env_fixed[] = {
    "AFNI_PLUGINPATH"    , "AFNI_NOPLUGINS"  , "AFNI_YESPLUGOUTS"   ,
    "AFNI_TSPATH"        , "AFNI_MODELPATH"  , "AFNI_HINTS"         ,
    "AFNI_NO_MCW_MALLOC" , "AFNI_NOREALPATH" , "AFNI_NOSPLASH"      ,
    "AFNI_NOTES_DLINES"  , "AFNI_OPTIONS"    , "AFNI_SYSTEM_AFNIRC" ,
    "AFNI_ALWAYS_LOCK"   , "AFNI_FIM_BKTHR"  , "AFNI_NO_XDBE"       ,
    "AFNI_GRAYSCALE_BOT" , "AFNI_NOMMAP"     ,
#ifndef USE_SESSTRAIL
    "AFNI_SESSTRAIL" ,
#endif
    "AFNI_RENDER_PRECALC_MODE"     ,
    "AFNI_NO_ADOPTION_WARNING"     ,
    "AFNI_BYTEORDER"               ,
    "AFNI_BYTEORDER_INPUT"         ,
    "AFNI_NO_BYTEORDER_WARNING"
} ;

#define NUM_env_fixed (sizeof(env_fixed)/sizeof(char *))

/*--------------------- strings for Cooordinate format --------------------*/

#define NUM_cord_strings 50

static char * cord_strings[NUM_cord_strings] = {
 "Dicom" , "Flipped" ,
  "RAI" , "RAS" , "RPI" , "RPS" , "RIA" , "RIP" , "RSA" , "RSP" ,
  "LAI" , "LAS" , "LPI" , "LPS" , "LIA" , "LIP" , "LSA" , "LSP" ,
  "AIR" , "ASR" , "PIR" , "PSR" , "IAR" , "IPR" , "SAR" , "SPR" ,
  "AIL" , "ASL" , "PIL" , "PSL" , "IAL" , "IPL" , "SAL" , "SPL" ,
  "IRA" , "SRA" , "IRP" , "SRP" , "ARI" , "PRI" , "ARS" , "PRS" ,
  "ILA" , "SLA" , "ILP" , "SLP" , "ALI" , "PLI" , "ALS" , "PLS"
} ;

static void ENV_coorder( char * ) ;
static void ENV_compressor( char * ) ;
static void ENV_leftisleft( char * ) ;
static void ENV_marksquality( char * ) ;
static void ENV_trusthost( char * ) ;     /* 21 Feb 2001 */
static void ENV_cwd( char * ) ;           /* 22 Feb 2001 */

#ifdef USE_SESSTRAIL
static void ENV_sesstrail( char * ) ;
#endif

/*-------------------------------------------------------------------------*/

#if 0
#define NUM_byteorder_list 3
static char *byteorder_list[] = { "This CPU" , "LSB_FIRST" , "MSB_FIRST" } ;

static void ENV_byteorder( char * ) ;
#endif

#define NUM_yesno_list 2
static char *yesno_list[] = { "YES" , "NO" } ;

/*--------- variables that can be edited ----------------------------------*/

#define NAME_NMAX 32
#define HINT_NMAX 64
#define VAL_NMAX  (PLUGIN_STRING_SIZE+NAME_NMAX+8)

#define ENV_NUMBER_EDITABLE 1
#define ENV_NUMBER_FIXED    2
#define ENV_STRING          9

typedef struct {
  char vname[NAME_NMAX] ;
  char vhint[HINT_NMAX] ;
  int  vtype ;                 /* ENV_NUMBER_* or ENV_STRING */
  int  vbot,vtop,vdecim,vdef ; /* for NUMBER */
  int  vcount ;                /* for STRING */
  char ** vlist ;              /* for STRING */
  char vvalue[VAL_NMAX] ;
  generic_func * vfunc ;
} ENV_var ;

static int   NUM_env_var = 0 ;
static ENV_var * env_var = NULL ;

/*----------------- prototypes for internal routines -----------------*/

static char * ENV_main( PLUGIN_interface * ) ;  /* the entry point */

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * ENV_init(void)
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   char *helpstring=NULL , *ept , *eval ;
   int ii ;

   /*------- some environment variables for AFNI ------*/

   { static char buf[VAL_NMAX] = "AFNI_CWD=" ;      /* 22 Feb 2001 */
     ept = getcwd( buf+9 , VAL_NMAX-9 ) ;
     if( ept != NULL ){
        putenv(buf) ;
        ENV_add_string( "AFNI_CWD" ,
                        "Current working directory (gets output files)" ,
                        0,NULL , ENV_cwd ) ;
     }
   }

   ENV_add_string( "AFNI_ENFORCE_ASPECT" ,
                   "To make AFNI enforce image window aspect ratio?" ,
                   NUM_yesno_list , yesno_list , NULL  ) ;

   ENV_add_numeric( "AFNI_FIM_PERCENT_LIMIT" ,
                    "Upper limit on % Change in FIM" ,
                    10,1000,0,100 , NULL              ) ;

   ENV_add_numeric( "AFNI_IMAGE_MINFRAC" ,
                    "Minimum size of image window" ,
                    0,10,2,2 , NULL                 ) ;

   ENV_add_string( "AFNI_LEFT_IS_LEFT" ,
                   "To show subject's left on image left?" ,
                   NUM_yesno_list , yesno_list , ENV_leftisleft  ) ;

   ENV_add_string( "AFNI_NO_SIDES_LABELS" ,
                   "Skip showing image window left-side label?" ,
                   NUM_yesno_list , yesno_list , NULL  ) ;

   ENV_add_string( "AFNI_VIEW_ANAT_BRICK" ,
                   "Show Anat brick whenever possible" ,
                   NUM_yesno_list , yesno_list , NULL  ) ;

   ENV_add_string( "AFNI_VIEW_FUNC_BRICK" ,
                   "Show Func brick whenever possible" ,
                   NUM_yesno_list , yesno_list , NULL  ) ;

   ENV_add_string( "AFNI_ORIENT" ,
                   "Coordinate display orientation" ,
                   NUM_cord_strings,cord_strings , ENV_coorder ) ;

   ENV_add_string( "AFNI_MARKERS_NOQUAL" ,
                   "Skip markers quality checking?" ,
                   NUM_yesno_list , yesno_list , ENV_marksquality  ) ;

   ENV_add_string( "AFNI_COMPRESSOR" ,
                   "Output BRIK compression method" ,
                   NUM_COMPRESS_elist,COMPRESS_elist , ENV_compressor ) ;

   ENV_add_string( "AFNI_AUTOGZIP" ,
                   "Use gzip on output if BRIK seems highly compressible" ,
                   NUM_yesno_list , yesno_list , NULL  ) ;

#if 0
   ENV_add_string( "AFNI_BYTEORDER" ,
                   "Byte ordering for output BRIKs" ,
                   NUM_byteorder_list , byteorder_list , ENV_byteorder ) ;
#endif

#if 0
   ENV_add_string( "AFNI_NOMMAP" ,
                   "Whether to read BRIKs using mmap()" ,
                   NUM_yesno_list , yesno_list , NULL    ) ;
#endif

   ENV_add_string( "AFNI_FLOATSCAN" ,
                   "Scan floating BRIKs for errors on input?" ,
                   NUM_yesno_list , yesno_list , NULL          ) ;

#ifdef USE_SESSTRAIL
   ENV_add_numeric( "AFNI_SESSTRAIL" ,
                    "# directory levels seen in Switch Session, etc." ,
                    0,9,0,SESSTRAIL , ENV_sesstrail ) ;
#endif

   ENV_add_string( "AFNI_PBAR_IMXY" ,
                   "Sizes of 'Save to PPM' for color pbars" ,
                   0,NULL , NULL ) ;

   ENV_add_string( "AFNI_PSPRINT" ,
                   "Command to send stdin to PostScript printer" ,
                   0,NULL , NULL ) ;

   ENV_add_string( "AFNI_TRUSTHOST" ,
                   "Name of host to trust for plugouts and realtime data" ,
                   0,NULL , ENV_trusthost ) ;

   ENV_add_string( "AFNI_IMAGE_LABEL_COLOR" ,
                   "Name of color for image overlay labels" , /* 21 Sep 2001 */
                   0,NULL , NULL ) ;

   ENV_add_numeric( "AFNI_IMAGE_LABEL_SETBACK" ,
                    "Size of setback for image overlay labels" ,
                    0 , 40 , 3 , 3 , NULL ) ;

   if( SUMA_ENABLED ){
     ENV_add_string( "AFNI_SUMA_BOXCOLOR" ,
                     "Name of color for surface node overlays" , /* 21 Sep 2001 */
                     0,NULL , NULL ) ;

     ENV_add_numeric( "AFNI_SUMA_BOXSIZE" ,
                      "Box size for surface node overlays" ,     /* 10 Mar 2002 */
                      1 , 10 , 1 , 2 , NULL ) ;

     ENV_add_string( "AFNI_SUMA_LINECOLOR" ,
                     "Name of color for surface line overlays" , /* 10 Mar 2002 */
                     0,NULL , NULL ) ;
   }

#ifndef NO_FRIVOLITIES
   ENV_add_string( "AFNI_IMAGE_PGMFILE" ,
                   "Image file for AFNI splash window" ,
                   0,NULL , NULL ) ;
#endif

   /* 08 Aug 2001 */

   ENV_add_yesno( "AFNI_DONT_MOVE_MENUS" ,
                  "Move popup menus to enhance visibility?" ) ;

   /* 07 Mar 2002 */

   ENV_add_numeric( "AFNI_GRAPH_TEXTLIMIT" ,
                    "Max rows in Graph Button-3 popup" ,
                    1 , 99 , 0 , 40 , NULL ) ;

   /*---------------- compute helpstring -----------------------*/

   helpstring = THD_zzprintf( helpstring , "%s\n" , help_start ) ;

   for( ii=0 ; ii < NUM_env_fixed ; ii++ ){
      ept = getenv(env_fixed[ii]) ;
      if( ept == NULL )
         eval = "(Not set)" ;
      else if( ept[0] == '\0' )
         eval = "(Set to empty string)" ;
      else
         eval = ept ;
      helpstring = THD_zzprintf( helpstring, " %-25.25s= %s\n",
                                             env_fixed[ii],eval  ) ;
   }

   helpstring = THD_zzprintf( helpstring , "%s\n" , help_mid ) ;

   for( ii=0 ; ii < NUM_env_var ; ii++ ){
      helpstring = THD_zzprintf( helpstring, " %-25.25s: %s\n" ,
                                 env_var[ii].vname , env_var[ii].vhint ) ;
   }

   helpstring = THD_zzprintf( helpstring , "%s\n" , help_end ) ;

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Environment" ,
                                "Environment variables control" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , ENV_main  ) ;
   free(helpstring) ;

   PLUTO_add_hint( plint , "Environment variables control" ) ;

   /*--------- make interface lines: 1 for each variable -----------*/

   for( ii=0 ; ii < NUM_env_var ; ii++ ){

      PLUTO_add_option( plint ,
                        "" ,                 /* label at left of input line */
                        env_var[ii].vname ,  /* tag to return to plugin */
                        FALSE                /* is this mandatory? */
                      ) ;

      ept = getenv( env_var[ii].vname ) ;

      switch( env_var[ii].vtype ){

         default: break ;  /* should never happen */

         case ENV_NUMBER_FIXED:
         case ENV_NUMBER_EDITABLE:{
            double dval=env_var[ii].vdef ; int ival ;

            if( ept != NULL ){
               dval = strtod(ept,NULL) ;
               AV_SHIFT_VAL(-env_var[ii].vdecim,dval) ;
            }
            ival = rint(dval) ;
                 if( ival < env_var[ii].vbot ) ival = env_var[ii].vbot ;
            else if( ival > env_var[ii].vtop ) ival = env_var[ii].vtop ;

            PLUTO_add_number( plint ,
                              env_var[ii].vname ,
                              env_var[ii].vbot  ,
                              env_var[ii].vtop  ,
                              env_var[ii].vdecim,
                              ival,(env_var[ii].vtype==ENV_NUMBER_EDITABLE) ) ;
         }
         break ;

         case ENV_STRING:{
            if( env_var[ii].vcount <= 0 ){
               PLUTO_add_string( plint ,
                                 env_var[ii].vname ,
                                 0 , &ept , NAME_NMAX ) ;
            } else {
               int dval , jj ;
               char ** sval ;

               dval = 1 + PLUTO_string_index( ept , env_var[ii].vcount ,
                                                    env_var[ii].vlist   ) ;

               sval = (char **) malloc( sizeof(char *)*(1+env_var[ii].vcount) ) ;
               sval[0] = "(Not set)" ;
               for( jj=0 ; jj < env_var[ii].vcount ; jj++ )
                  sval[jj+1] = env_var[ii].vlist[jj] ;

               PLUTO_add_string( plint ,
                                 env_var[ii].vname  ,
                                 1+env_var[ii].vcount , sval , dval ) ;
               free(sval) ;
            }
         }
         break ;
      } /* end of switch over variable type */

   } /* end of loop over variables */

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Add a new variable to the list
****************************************************************************/

void ENV_add_numeric( char * vname , char * vhint ,
                      int vbot , int vtop , int vdecim , int vdef ,
                      generic_func * cbfunc )
{
   int ii ;

   if( vname == NULL || vname[0] == '\0' ) return ;
   if( vhint == NULL || vhint[0] == '\0' ) return ;
   if( vbot >= vtop )                      return ;

   if( NUM_env_var == 0 )
      env_var = (ENV_var *) malloc(sizeof(ENV_var)) ;
   else
      env_var = (ENV_var *) realloc(env_var,(NUM_env_var+1)*sizeof(ENV_var)) ;

   ii = NUM_env_var ; NUM_env_var++ ;

   MCW_strncpy( env_var[ii].vname , vname , NAME_NMAX ) ;
   MCW_strncpy( env_var[ii].vhint , vhint , HINT_NMAX ) ;

   env_var[ii].vtype = (vtop-vbot < 100) ? ENV_NUMBER_FIXED
                                         : ENV_NUMBER_EDITABLE ;

   env_var[ii].vbot   = vbot ;
   env_var[ii].vtop   = vtop ;
   env_var[ii].vdecim = vdecim ;
   env_var[ii].vdef   = vdef ;
   env_var[ii].vfunc  = cbfunc ;

   return ;
}

void ENV_add_yesno( char * vname , char * vhint ) /* 08 Aug 2001 */
{
   ENV_add_string( vname , vhint ,
                   NUM_yesno_list , yesno_list , NULL  ) ;
}

void ENV_add_string( char * vname , char * vhint ,
                     int vcount , char ** vlist , generic_func * cbfunc )
{
   int ii ;

   if( vname == NULL || vname[0] == '\0' ) return ;
   if( vhint == NULL || vhint[0] == '\0' ) return ;
   if( vcount < 0 )                        return ;
   if( vcount > 0    && vlist    == NULL ) return ;

   if( NUM_env_var == 0 )
      env_var = (ENV_var *) malloc(sizeof(ENV_var)) ;
   else
      env_var = (ENV_var *) realloc(env_var,(NUM_env_var+1)*sizeof(ENV_var)) ;

   ii = NUM_env_var ; NUM_env_var++ ;

   MCW_strncpy( env_var[ii].vname , vname , NAME_NMAX ) ;
   MCW_strncpy( env_var[ii].vhint , vhint , HINT_NMAX ) ;

   env_var[ii].vtype  = ENV_STRING ;
   env_var[ii].vcount = vcount ;
   env_var[ii].vlist  = vlist ;
   env_var[ii].vfunc  = cbfunc ;

   return ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * ENV_main( PLUGIN_interface * plint )
{
   char *tag ;
   int ii,kk , ndone=0 ;

   /*--------- loop over input lines ---------*/

   while(1){
      tag = PLUTO_get_optiontag(plint) ;
      if( tag == NULL ) break ;

      /* find which variable */

      for( ii=0 ; ii < NUM_env_var ; ii++ )
         if( strcmp(tag,env_var[ii].vname) == 0 ) break ;

      if( ii == NUM_env_var )
        return "** ENV_main: table corruption! **" ;  /* should not happen */

      switch( env_var[ii].vtype ){  /* set vvalue for each type */

         default:
           return "** ENV_main: table corruption! **" ;  /* should not happen */

         /* write a numeric value into the environment */

         case ENV_NUMBER_FIXED:
         case ENV_NUMBER_EDITABLE:{
            float val = PLUTO_get_number(plint) ;
            sprintf(env_var[ii].vvalue,"%s=%s" ,
                    env_var[ii].vname , AV_format_fval(val) ) ;
         }
         break ;

         /* write a string value into the environment */

         case ENV_STRING:{
            char * str = PLUTO_get_string(plint) ; int jj ;

            if( env_var[ii].vcount > 0 ){
               jj = PLUTO_string_index( str , env_var[ii].vcount ,
                                              env_var[ii].vlist   ) ;
               if( jj >= 0 )
                  sprintf(env_var[ii].vvalue,"%s=%s" ,
                          env_var[ii].vname , str     ) ;
               else
                  sprintf(env_var[ii].vvalue,"%s=" , env_var[ii].vname ) ;
            } else {
               sprintf(env_var[ii].vvalue,"%s=%s" ,
                       env_var[ii].vname , str     ) ;
            }
         }
         break ;

      } /* end of switch over environment variable types */

      /* actually set environment variable */

      putenv(env_var[ii].vvalue) ;

      /* call callback, if there is one */

      if( env_var[ii].vfunc != NULL ) env_var[ii].vfunc( env_var[ii].vname ) ;

      /* turn this option off (for the user's convenience) */

      for( kk=0 ; kk < plint->option_count ; kk++ )            /* find widget */
         if( strcmp(tag,plint->option[kk]->tag) == 0 ) break ;

      if( kk < plint->option_count )                           /* turn off */
         XmToggleButtonSetState( plint->wid->opwid[kk]->toggle, False,True ) ;

      ndone++ ;  /* count of how many we've done */

   } /* end of while(1) loop over option lines */

   /*--------- finished -------*/

   if( ndone == 0 ) return " \n*** Don't you want to do anything? ***\n " ;

   return NULL ;
}

/*-----------------------------------------------------------------------*/

static void ENV_coorder( char * vname )
{
   char * str = getenv(vname) ;
   if( str == NULL ) str = "RAI" ;
   MCW_strncpy(GLOBAL_argopt.orient_code,str,4) ;
   THD_coorder_fill( GLOBAL_argopt.orient_code , &GLOBAL_library.cord ) ;
   PLUTO_force_redisplay() ;
}

/*-----------------------------------------------------------------------*/

static void ENV_compressor( char * vname )
{
   char * str = getenv(vname) ;
   int meth ;

   if( str == NULL ) str = "None" ;
   meth = PLUTO_string_index( str , NUM_COMPRESS_elist , COMPRESS_elist ) ;
   if( meth < 0 ) meth = COMPRESS_NONE ;
   THD_set_write_compression(meth) ;
}

/*-----------------------------------------------------------------------*/

#ifdef USE_SESSTRAIL
static void ENV_sesstrail( char * vname )
{
   int ii , tt ;
   THD_session * sess ;
   char * str = getenv(vname) ;

   if( str == NULL ) str = "1" ;
   ii = SESSTRAIL ; SESSTRAIL = (int) strtod(str,NULL) ;
   if( ii == SESSTRAIL ) return ;

   /* relabel controller windows */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
      if( IM3D_OPEN(GLOBAL_library.controllers[ii]) )
         AFNI_set_window_titles( GLOBAL_library.controllers[ii] ) ;

   /* relabel sessions (cf. thd_initsess.c) */

   for( ii=0 ; ii < GLOBAL_library.sslist->num_sess ; ii++ ){
     sess = GLOBAL_library.sslist->ssar[ii] ;
     str  = THD_trailname(sess->sessname,SESSTRAIL) ;
     tt   = 1+strlen(str) - THD_MAX_LABEL ; if( tt < 0 ) tt = 0 ;
     strcpy( sess->lastname , str+tt ) ;
   }
}
#endif

/*-----------------------------------------------------------------------*/

#if 0
static void ENV_byteorder( char * vname )
{
   char * str = getenv(vname) ;
   int meth ;

   meth = PLUTO_string_index( str , NUM_byteorder_list , byteorder_list ) ;
   switch( meth ){
      default: meth = -1                ; break ;
      case 0:  meth = mri_short_order() ; break ;
      case 1:  meth = LSB_FIRST         ; break ;
      case 2:  meth = MSB_FIRST         ; break ;
   }
   THD_set_write_order( meth ) ;
}
#endif

/*-----------------------------------------------------------------------*/

static void ENV_trusthost( char * vname )  /* 21 Feb 2001 */
{
   char * str = getenv(vname) ;
   TRUST_addhost(str) ;
}

/*-----------------------------------------------------------------------*/

static void ENV_cwd( char * vname )  /* 22 Feb 2001 */
{
  char * str = getenv(vname) , buf[256] , *bpt ;

  if( str != NULL && str[0] != '\0' ){
     int ii = chdir(str) ;
     if( ii ){
        perror("** Setting CWD fails") ;
        bpt = getcwd( buf , 256 ) ;
        if( bpt != NULL ) fprintf(stderr,"** CWD still = %s\n",buf) ;
        BEEPIT ;
     } else {
        bpt = getcwd( buf , 256 ) ;
        if( bpt != NULL ) fprintf(stderr,"++ CWD now = %s\n",buf) ;
     }
  } else {
     fprintf(stderr,"** CWD not changed!\n") ;
  }
}

/*-----------------------------------------------------------------------*/

static void ENV_leftisleft( char * vname )
{
   char * str = getenv(vname) ;
   GLOBAL_argopt.left_is_left = YESSISH(str) ;
}

/*-----------------------------------------------------------------------*/

static void ENV_marksquality( char * vname )
{
   char * str = getenv(vname) ;
   GLOBAL_argopt.elide_quality = YESSISH(str) ;
}
#endif
