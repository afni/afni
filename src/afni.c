/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/**********************************************************************/
/* GPL AFNI:                                                          */
/*    Analysis of Functional NeuroImages                              */
/*                                                                    */
/* Author: Robert W. Cox, PhD                                         */
/*         Biophysics Research Institute                              */
/*         Medical College of Wisconsin                               */
/*         8701 Watertown Plank Road                                  */
/*         Milwaukee, WI 53226                                        */
/*                                                                    */
/* Acknowledgments:                                                   */
/*   + This program would have been much more difficult had           */
/*     not Andrzej Jesmanowicz forged the way with FD.                */
/*   + Many neuroscientists have made helpful suggestions along       */
/*     the way, including Jeff Binder, Ted DeYoe, Jim Hyde, Steve     */
/*     Rao, and Elliot Stein.                                         */
/*   + Thanks are also due to Mike Beauchamp, who is perhaps the most */
/*     sophisticated user of AFNI that I've met, and who has found    */
/*     many bugs or gotchas in this code.                             */
/*   + Doug Ward of MCW has contributed much to the overall package.  */
/*   + Ziad Saad of NIH has also contributed many useful suggestions. */
/*   + Peter Bandettini of NIH has asked many many "quick" questions. */
/**********************************************************************/

#define MAIN

#include "afni.h"

#define ANNOUNCEMENT                                                           \
 "GPL AFNI: Analysis of Functional NeuroImages, by RW Cox (" COXEMAIL ")\n"    \
 "This is Version " VERSION " of " RELEASE"\n\n"                               \
 " ** This software was designed to be used only for research purposes. **\n"  \
 " ** Clinical uses are not recommended, and have never been evaluated. **\n"  \
 " ** This software comes with no warranties of any kind whatsoever,    **\n"  \
 " ** and may not be useful for anything.  Use it at your own risk!     **\n"  \
 " ** If these terms are not acceptable, you aren't allowed to use AFNI.**\n"  \
 " ** See 'Define Datamode->Misc->License Info' for more details.       **\n\n"

#define USE_FRIENDS

#ifdef AFNI_DEBUG
#  define REPORT_PROGRESS(str)  /* nada */
#else
#  define REPORT_PROGRESS(str)  \
    do{ if(AFNI_VERBOSE){printf(str);fflush(stdout);} } while(0)
#endif

#define EMPTY_STRING(str) ((str)[0] = '\0')

#ifdef AFNI_DEBUG
#  define USE_TRACING
#endif

/*----------------------------------------------------------------
   Global variables that used to be local variables in main()
------------------------------------------------------------------*/

static XtAppContext   MAIN_app ;
static XtErrorHandler MAIN_old_handler ;
static Three_D_View * MAIN_im3d ;
static MCW_DC *       MAIN_dc ;
static Widget         MAIN_shell=NULL ;
static int            MAIN_argc ;
static char **        MAIN_argv ;
static Boolean        MAIN_workprocess( XtPointer ) ;

#define USE_SIDES  /* 01 Dec 1999: replace "left is xxx" */
                   /* labels with "sides" labels         */

/********************************************************************
   Print out some help information and then quit quit quit
*********************************************************************/

void AFNI_syntax(void)
{
   printf(
     ANNOUNCEMENT
     "\n"
     "Usage 1: read in sessions of 3D datasets (created by to3d)\n"
     "\n"
     "   afni [options] [session_directory ...]\n"
     "\n"
#if MMAP_THRESHOLD > 0
     "   -purge       Conserve memory by purging data to disk.\n"
     "                  [Use this if you run out of memory when running AFNI.]\n"
     "                  [This will slow the code down, so use only if needed.]\n"
#endif
     "   -posfunc     Set up the color 'pbar' to use only positive function values.\n"
     "   -R           Recursively search each session_directory for more session\n"
     "                  subdirectories.\n"
     "       WARNING: This will descend the entire filesystem hierarchy from\n"
     "                  each session_directory given on the command line.  On a\n"
     "                  large disk, this may take a long time.  To limit the\n"
     "                  recursion to 5 levels (for example), use -R5.\n"
     "   -ignore N    Tells the program to 'ignore' the first N points in\n"
     "                  time series for graphs and FIM calculations.\n"
     "   -im1 N       Tells the program to use image N as the first one for\n"
     "                  graphs and FIM calculations (same as '-ignore N-1')\n"
     "   -tlrc_small  These options set whether to use the 'small' or 'big'\n"
     "   -tlrc_big      Talairach brick size.  The compiled in default for\n"
     "                  the program is now 'big', unlike AFNI 1.0x.\n"
#ifndef WARP_4D
     "   -warp_4D     Allows the program to Talairach transform and write\n"
     "                  to disk 3D+time datasets.  Note that the resulting\n"
     "                  disk files will be gigantic (100s of Megabytes).\n"
#endif
     "   -no1D        Tells AFNI not to read *.1D timeseries files from\n"
     "                  the dataset directories.  The *.1D files in the\n"
     "                  directories listed in the AFNI_TSPATH environment\n"
     "                  variable will still be read (if this variable is\n"
     "                  not set, then './' will be scanned for *.1D files.)\n"
     "\n"
     "   -noqual      Tells AFNI not to enforce the 'quality' checks when\n"
     "                  making the transformations to +acpc and +tlrc.\n"
     "   -unique      Tells the program to create a unique set of colors\n"
     "                  for each AFNI controller window.  This allows\n"
     "                  different datasets to be viewed with different\n"
     "                  grayscales or colorscales.  Note that -unique\n"
     "                  will only work on displays that support 12 bit\n"
     "                  PseudoColor (e.g., SGI workstations) or TrueColor.\n"
     "   -orient code Tells afni the orientation in which to display\n"
     "                  x-y-z coordinates (upper left of control window).\n"
     "                  The code must be 3 letters, one each from the\n"
     "                  pairs {R,L} {A,P} {I,S}.  The first letter gives\n"
     "                  the orientation of the x-axis, the second the\n"
     "                  orientation of the y-axis, the third the z-axis:\n"
     "                   R = right-to-left         L = left-to-right\n"
     "                   A = anterior-to-posterior P = posterior-to-anterior\n"
     "                   I = inferior-to-superior  S = superior-to-inferior\n"
     "                  The default code is RAI ==> DICOM order.  This can\n"
     "                  be set with the environment variable AFNI_ORIENT.\n"
     "                  As a special case, using the code 'flipped' is\n"
     "                  equivalent to 'LPI' (this is for Steve Rao).\n"
#ifdef ALLOW_PLUGINS
     "   -noplugins   Tells the program not to load plugins.\n"
     "                  (Plugins can also be disabled by setting the\n"
     "                   environment variable AFNI_NOPLUGINS.)\n"
     "   -yesplugouts Tells the program to listen for plugouts.\n"
     "                  (Plugouts can also be enabled by setting the\n"
     "                   environment variable AFNI_YESPLUGOUTS.)\n"
     "   -YESplugouts Makes the plugout code print out lots of messages\n"
     "                  (useful for debugging a new plugout).\n"
     "   -noplugouts  Tells the program NOT to listen for plugouts.\n"
     "                  (This option is available to override\n"
     "                   the AFNI_YESPLUGOUTS environment variable.)\n"
#endif
     "   -skip_afnirc Tells the program NOT to read the file .afnirc\n"
     "                  in the home directory.  See README.setup for\n"
     "                  details on the use of .afnirc for initialization.\n"
     "   -layout fn   Tells AFNI to read the initial windows layout from\n"
     "                  file 'fn'.  If this option is not given, then\n"
     "                  environment variable AFNI_LAYOUT_FILE is used.\n"
     "                  If neither is present, then AFNI will do whatever\n"
     "                  it feels like.\n"
     "\n"
     " If no session_directories are given, then the program will use\n"
     "   the current working directory (i.e., './').\n"
     " The maximum number of sessions is now set to   %d.\n"
     " The maximum number of anatomies per session is %d.\n"
     " The maximum number of functions per session is %d.\n"
     " A session must have at least one anatomical dataset in it.\n"

     , THD_MAX_NUM_SESSION , THD_MAX_SESSION_ANAT , THD_MAX_SESSION_FUNC
   ) ;

   printf(
     "\n"
     "Usage 2: read in images for 'quick and dirty' viewing\n"
     "(Many advanced features of AFNI will be disabled.)\n"
     "\n"
     "   afni -im [options] im1 im2 im3 ...\n"
     "\n"
     "   -im          Flag to read in images instead of 3D datasets\n"
     "                  (Talaraich and functional stuff won't work)\n"
     "   -dy yratio   Tells afni the downscreen pixel size is 'yratio' times\n"
     "                  the across-screen (x) pixel dimension (default=1.0)\n"
     "   -dz zratio   Tells afni the slice thickness is 'zratio' times\n"
     "                  the x pixel dimension (default=1.0)\n"
     "   -orient code Tells afni the orientation of the input images.\n"
     "                  The code must be 3 letters, one each from the\n"
     "                  pairs {R,L} {A,P} {I,S}.  The first letter gives\n"
     "                  the orientation of the x-axis, the second the\n"
     "                  orientation of the y-axis, the third the z-axis:\n"
     "                   R = right-to-left         L = left-to-right\n"
     "                   A = anterior-to-posterior P = posterior-to-anterior\n"
     "                   I = inferior-to-superior  S = superior-to-inferior\n"
     "                  (the default code is ASL ==> sagittal images).\n"
     "                  Note that this use of '-orient' is different from\n"
     "                  the use when viewing datasets.\n"
     "   -resize      Tells afni that all images should be resized to fit\n"
     "                  the size of the first one, if they don't already fit\n"
     "                  (by default, images must all 'fit' or afni will stop)\n"
     "   -datum type  Tells afni to convert input images into the type given:\n"
     "                  byte, short, float, complex are the legal types.\n"
     " The image files (im1 ...) are the same formats as accepted by to3d.\n"
     "\n"
     " New image display options (alternatives to -im) [19 Oct 1999]:\n"
     "   -tim         These options tell AFNI to arrange the input images\n"
     "   -tim:<nt>    into a internal time-dependent dataset.  Suppose that\n"
     "   -zim:<nz>    there are N input 2D slices on the command line.\n"
     "              * -tim alone means these are N points in time (1 slice).\n"
     "              * -tim:<nt> means there are nt points in time (nt is\n"
     "                  an integer > 1), so there are N/nt slices in space,\n"
     "                  and the images on the command line are input in\n"
     "                  time order first (like -time:tz in to3d).\n"
     "              * -zim:<nz> means there are nz slices in space (nz is\n"
     "                  an integer > 1), so there are N/nz points in time,\n"
     "                  and the images on the command line are input in\n"
     "                  slice order first (like -time:zt in to3d).\n"
     "\n"
     " N.B.: You may wish to use the -ignore option to set the number of\n"
     "       initial points to ignore in the time series graph if you use\n"
     "       -tim or -zim, since there is no way to change this from\n"
     "       within an AFNI run (the FIM menus are disabled).\n"
   ) ;

   printf(
     "\n"
     "Usage 3: read in datasets specified on the command line\n"
     "\n"
     "  afni -dset [options] dname1 dname2 ...\n"
     "\n"
     "where 'dname1' is the name of a dataset, etc.  With this option, only\n"
     "the chosen datasets are read in, and they are all put in the same\n"
     "'session'.  Follower datasets are not created.\n"
     "\n"
    MASTER_HELP_STRING
     "\n"
    CALC_HELP_STRING
   ) ;

   printf(
     "\n"
     "General options (for any Usage):\n"
     "\n"
     "   -q           Tells afni to be 'quiet' on startup\n"
     "   -gamma gg    Tells afni that the gamma correction factor for the\n"
     "                  monitor is 'gg' (default gg is 1.0; greater than\n"
     "                  1.0 makes the image contrast larger -- this may\n"
     "                  also be adjusted interactively)\n"
     "   -install     Tells afni to install a new X11 Colormap.  This only\n"
     "                  means something for PseudoColor displays.  Also, it\n"
     "                  usually cause the notorious 'technicolor' effect.\n"
     "   -ncolors nn  Tells afni to use 'nn' gray levels for the image\n"
     "                  displays (default is %d)\n"
     "   -xtwarns     Tells afni to show any Xt warning messages that may\n"
     "                  occur; the default is to suppress these messages.\n"
     "   -tbar name   Uses 'name' instead of 'AFNI' in window titlebars.\n"
     "   -flipim and  The '-flipim' option tells afni to display images in the\n"
     "   -noflipim      'flipped' radiology convention (left on the right).\n"
     "                  The '-noflipim' option tells afni to display left on\n"
     "                  the left, as neuroscientists generally prefer.  This\n"
     "                  latter mode can also be set by the Unix environment\n"
     "                  variable 'AFNI_LEFT_IS_LEFT'.  The '-flipim' mode is\n"
     "                  the default.\n"
#ifdef USE_TRACING
     "   -trace       Turns routine call tracing on, for debugging purposes.\n"
     "   -TRACE       Turns even more verbose tracing on, for more debugging.\n"
#endif
#ifdef USING_MCW_MALLOC
     "   -nomall      Disables use of the mcw_malloc() library routines.\n"
#endif
     "\n"
     "N.B.: Many of these options, as well as the initial color set up,\n"
     "      can be controlled by appropriate X11 resources.  See the\n"
     "      file AFNI.Xdefaults for instructions and examples.\n"

     , DEFAULT_NGRAY
   ) ;

   exit(0) ;
}

/*----------------------------------------------------------------------
   parse command line switches and store results in a data structure
------------------------------------------------------------------------*/

void AFNI_parse_args( int in_argc , char * in_argv[] )
{
   int narg = 1 ;
   char * env_orient , * env ;
   int     argc=in_argc ,    new_argc      ; /* 18 Nov 1999 */
   char ** argv=in_argv , ** new_argv=NULL ;

ENTRY("AFNI_parse_args") ;

   if( argc > 1 && strncmp(argv[1],"-help",2) == 0 ) AFNI_syntax() ;

   GLOBAL_argopt.dz       = 1.0 ;          /* set up defaults */
   GLOBAL_argopt.dy       = 1.0 ;
   GLOBAL_argopt.ignore   = INIT_ignore ;
   GLOBAL_argopt.elide_quality  = 0 ;      /* Dec 1997 */
   GLOBAL_argopt.skip_afnirc    = 0 ;      /* 14 Jul 1998 */
   GLOBAL_argopt.no_frivolities = 0 ;      /* 01 Aug 1998 */
   GLOBAL_argopt.install_cmap   = 0 ;      /* 14 Sep 1998 */
   GLOBAL_argopt.read_1D        = 1 ;      /* 27 Jan 2000 */

   GLOBAL_argopt.enable_suma    = 1 ;      /* 29 Aug 2001 */

   GLOBAL_argopt.yes_niml       = AFNI_yesenv("AFNI_NIML_START") ;

#if 0
   GLOBAL_argopt.allow_rt = 0 ;            /* April 1997 */
#else                                      /* 09 Oct 2000 */
   GLOBAL_argopt.allow_rt = AFNI_yesenv("AFNI_REALTIME_Activate") ;
   GLOBAL_argopt.no_frivolities = (GLOBAL_argopt.allow_rt != 0) ;
#endif

   SESSTRAIL = 1 ;
   env = getenv( "AFNI_SESSTRAIL" ) ;
   if( env != NULL ){
      SESSTRAIL = strtol(env,NULL,10) ;
      if( SESSTRAIL < 0 ) SESSTRAIL = 0 ;  /* 24 Aug 2000 */
   }

   GLOBAL_argopt.elide_quality = AFNI_yesenv("AFNI_MARKERS_NOQUAL") ;

   /* 24 Sep 2000: get the default layout name (add $HOME) */

   { char * lf = getenv("AFNI_LAYOUT_FILE") ;
     if( lf != NULL ){
        char * eh = getenv("HOME") , * ff ;
        int ll = strlen(lf) + 8 ;
        if( eh != NULL ) ll += strlen(eh) ;
        ff = malloc(ll) ;
        if( eh != NULL && lf[0] != '/' ){ strcpy(ff,eh) ; strcat(ff,"/") ; }
        else                            { ff[0] = '\0' ; }
        strcat(ff,lf) ;
        GLOBAL_argopt.layout_fname = ff ;
     }
   }

   /*-- 18 Nov 1999: Allow setting of options from environment --*/

   env = getenv( "AFNI_OPTIONS" ) ;
   if( env != NULL )
     prepend_string_to_args( env, in_argc, in_argv, &new_argc, &new_argv ) ;
   if( new_argv != NULL ){
      MAIN_argc = argc = new_argc ;
      MAIN_argv = argv = new_argv ;
   }

#ifdef ALLOW_PLUGINS
   GLOBAL_argopt.noplugins  =  AFNI_yesenv( "AFNI_NOPLUGINS" ) ;
   GLOBAL_argopt.noplugouts = !AFNI_yesenv( "AFNI_YESPLUGOUTS" ) ;
#endif

   env_orient = getenv( "AFNI_ORIENT" ) ;

   GLOBAL_argopt.read_sessions = True ;        /* exactly one of these should be True */
   GLOBAL_argopt.read_images   = False ;
   GLOBAL_argopt.read_dsets    = False ;       /* 17 Mar 2000 */

   GLOBAL_argopt.datum         = ILLEGAL_TYPE ;

   GLOBAL_argopt.gamma         = INIT_gamma ;
   GLOBAL_argopt.gsfac         = 0.0 ;
   GLOBAL_argopt.ncolor        = INIT_ngray ;
#if MMAP_THRESHOLD > 0
   GLOBAL_argopt.auto_purge    = INIT_purge ;
#else
   GLOBAL_argopt.auto_purge    = True ;
#endif
   GLOBAL_argopt.resize_images = False ;       /* False means all images must match */
   GLOBAL_argopt.keep_logo     = False ;       /* For making pretty pictures? */
   GLOBAL_argopt.pos_func      = INIT_posfunc ;/* Positive valued functions? */
   GLOBAL_argopt.recurse       = 0 ;           /* Recurse on session directories? */
   GLOBAL_argopt.xtwarns       = False ;       /* True means keep Xt warnings turned on */
   GLOBAL_argopt.destruct      = False ;       /* True means allow overwrite of datasets */
                                               /* (Not yet properly implemented!) */

   GLOBAL_argopt.tlrc_big      = INIT_tlrc_big ; /* use the big Talairach box? */
#ifndef WARP_4D
   GLOBAL_argopt.warp_4D       = False ;
#else
   GLOBAL_argopt.warp_4D       = True ;
#endif

   GLOBAL_argopt.unique_dcs    = False ;  /* 06 Nov 1996 */

   strcpy(GLOBAL_argopt.orient_code,"---") ;

   strcpy(GLOBAL_argopt.title_name,"AFNI " VERSION) ;   /* default title bar name */
   { int ll = strlen(GLOBAL_argopt.title_name) ;
     if( GLOBAL_argopt.title_name[ll-1] == ' ' )
        GLOBAL_argopt.title_name[ll-1] = '\0' ;
   }

   GLOBAL_argopt.left_is_left = AFNI_yesenv( "AFNI_LEFT_IS_LEFT" ) ;

   GLOBAL_argopt.read_tim = 0 ;   /* 19 Oct 1999 */

   while( narg < argc ){

      if( argv[narg][0] != '-' ) break ;   /* no - ==> quit */

#ifdef USE_TRACING
      if( strncmp(argv[narg],"-trace",5) == 0 && !ALLOW_real_time ){
         DBG_trace = 1 ;
         narg++ ; continue ;
      }
      if( strncmp(argv[narg],"-TRACE",5) == 0 && !ALLOW_real_time ){  /* 23 Aug 1998 */
         DBG_trace = 2 ;
         if( MAIN_shell != NULL )
            XSynchronize(XtDisplay(MAIN_shell),TRUE) ; /* 01 Dec 1999 */
         narg++ ; continue ;
      }
#endif

      /*----- -layout (23 Sep 2000) -----*/

      if( strcmp(argv[narg],"-layout") == 0 ){
         if( narg+1 >= argc ) FatalError("need an argument after -layout!") ;
         GLOBAL_argopt.layout_fname = argv[++narg] ;  /* just a pointer */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -no1D option (27 Jan 2000) ----- */

      if( strncmp(argv[narg],"-no1D",5) == 0 ){
         GLOBAL_argopt.read_1D = 0 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -skip_afnirc option (14 Jul 1998) -----*/

      if( strncmp(argv[narg],"-skip_afnirc",12) == 0 ){
         GLOBAL_argopt.skip_afnirc  = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -rt option -----*/

      if( strncmp(argv[narg],"-rt",3) == 0 ){
         GLOBAL_argopt.allow_rt       = -1 ;
         GLOBAL_argopt.no_frivolities =  1 ;
#ifdef USE_TRACING
         DBG_trace                    =  0 ;  /* 26 Jan 2001 */
#endif
         narg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[narg],"-nort",5) == 0 ){  /* 09 Oct 2000 */
         GLOBAL_argopt.allow_rt       = 0 ;
         GLOBAL_argopt.no_frivolities = 0 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -noqual -----*/

      if( strncmp(argv[narg],"-noqual",6) == 0 ){
         GLOBAL_argopt.elide_quality = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*---- -agni [29 Aug 2001] or -suma -----*/

      if( strcmp(argv[narg],"-agni")==0 || strcmp(argv[narg],"-suma")==0 ){
         fprintf(stderr,"\n-agni/-suma are now turned on by default\n") ;
         GLOBAL_argopt.enable_suma = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*---- -niml [28 Feb 2002] -----*/

      if( strcmp(argv[narg],"-niml") == 0 ){
         if( GLOBAL_argopt.yes_niml )
           fprintf(stderr,"\n-niml is already turned on\n") ;
         GLOBAL_argopt.yes_niml++ ;
         narg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[narg],"-noniml") == 0 ){
         GLOBAL_argopt.yes_niml-- ;
         if( GLOBAL_argopt.yes_niml < 0 ) GLOBAL_argopt.yes_niml = 0 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -tbar 'name' option -----*/

      if( strncmp(argv[narg],"-tbar",5) == 0 ){
         if( narg+1 >= argc ) FatalError("need an argument after -tbar!");
         MCW_strncpy(GLOBAL_argopt.title_name,argv[++narg],32) ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -xtwarns option -----*/

      if( strncmp(argv[narg],"-xtwarns",6) == 0 ){
         GLOBAL_argopt.xtwarns = True ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -destruct option -----*/

      if( strncmp(argv[narg],"-destruct",6) == 0 ){   /** has no effect at present **/
         fprintf(stderr,"\n*** -destruct option not implemented at present! ***\n") ;
         GLOBAL_argopt.destruct = False ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -posfunc option -----*/

      if( strncmp(argv[narg],"-posfunc",6) == 0 ){
         GLOBAL_argopt.pos_func = True ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -R option -----*/

      if( strncmp(argv[narg],"-R",2) == 0 ){
         int ll = strlen(argv[narg]) ;
         if( ll == 2 ) GLOBAL_argopt.recurse = 999 ;
         else {
            ll = strtol( argv[narg]+2 , NULL , 10 ) ;
            if( ll > 0 ) GLOBAL_argopt.recurse = ll ;
            else FatalError("illegal -R option!") ;
         }
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -tlrc_big option -----*/

      if( strncmp(argv[narg],"-tlrc_big",7) == 0 ){
         GLOBAL_argopt.tlrc_big = True ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -unique option (06 Nov 1996) -----*/

      if( strncmp(argv[narg],"-unique",5) == 0 ){
         GLOBAL_argopt.unique_dcs = True ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -install option (14 Sep 1998) -----*/

      if( strncmp(argv[narg],"-install",5) == 0 ){
         GLOBAL_argopt.install_cmap = True ;
         narg++ ; continue ;  /* go to next arg */
      }

#ifndef WARP_4D
      /*----- -warp_4D option -----*/

      if( strncmp(argv[narg],"-warp_4D",7) == 0 ){
         GLOBAL_argopt.warp_4D = True ;
         narg++ ; continue ;  /* go to next arg */
      }
#endif

      /*----- -tlrc_small option -----*/

      if( strncmp(argv[narg],"-tlrc_small",7) == 0 ){
         GLOBAL_argopt.tlrc_big = False ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -logo option -----*/

      if( strncmp(argv[narg],"-logo",4) == 0 ){
         GLOBAL_argopt.keep_logo = True ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -resize option -----*/

      if( strncmp(argv[narg],"-resize",4) == 0 ){
         GLOBAL_argopt.resize_images = True ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -purge option -----*/

      if( strncmp(argv[narg],"-purge",4) == 0 ){
         GLOBAL_argopt.auto_purge = True ;
         narg++ ; continue ;  /* go to next arg */
      }

#ifdef ALLOW_PLUGINS
      /*----- -noplugins option -----*/

      if( strncmp(argv[narg],"-noplugins",10) == 0 ){
         GLOBAL_argopt.noplugins = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -noplugouts option -----*/

      if( strncmp(argv[narg],"-noplugouts",10) == 0 ){
         GLOBAL_argopt.noplugouts = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -yesplugouts option -----*/

      if( strncmp(argv[narg],"-yesplugouts",10) == 0 ){
         GLOBAL_argopt.noplugouts   = 0 ;
         GLOBAL_argopt.plugout_code = 0 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -yesplugouts option -----*/

      if( strncmp(argv[narg],"-YESplugouts",10) == 0 ){
         GLOBAL_argopt.noplugouts   = 0 ;
         GLOBAL_argopt.plugout_code = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }
#endif

      /*----- -flipim option -----*/

      if( strncmp(argv[narg],"-flipim",5) == 0 ){
         GLOBAL_argopt.left_is_left = 0 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -noflipim option -----*/

      if( strncmp(argv[narg],"-noflipim",5) == 0 ){
         GLOBAL_argopt.left_is_left = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -orient code option -----*/

      if( strncmp(argv[narg],"-orient",4) == 0 ){
         if( narg+1 >= argc ) FatalError("need an argument after -orient!");

         MCW_strncpy(GLOBAL_argopt.orient_code,argv[++narg],4) ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -ignore # option -----*/

      if( strncmp(argv[narg],"-ignore",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -ignore!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val >= 0 ) GLOBAL_argopt.ignore = (int) val ;
         else fprintf(stderr,
                "\n*** warning: -ignore value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -im1 # option [must come before '-im' option!] -----*/

      if( strncmp(argv[narg],"-im1",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -im1!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val >= 1 ) GLOBAL_argopt.ignore = (int) (val-1.0) ;
         else fprintf(stderr,
                "\n*** warning: -ignore value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }


      /*----- -dy # option -----*/

      if( strncmp(argv[narg],"-dy",3) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -dy!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 0 ) GLOBAL_argopt.dy = val ;
         else fprintf(stderr,
                "\n*** warning: -dy value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -dz # option -----*/

      if( strncmp(argv[narg],"-dz",3) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -dz!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 0 ) GLOBAL_argopt.dz = val ;
         else fprintf(stderr,
                "\n*** warning: -dz value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -gamma # option -----*/

      if( strncmp(argv[narg],"-gamma",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -gamma!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 0 ) GLOBAL_argopt.gamma = val ;
         else fprintf(stderr,
                "\n*** warning: -gamma value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

#ifdef USE_GSFAC
      /*----- -gsfac # option -----*/

      if( strncmp(argv[narg],"-gsfac",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -gsfac!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val != 0 ) GLOBAL_argopt.gsfac = val ;
         else fprintf(stderr,
                "\n*** warning: -gsfac value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }
#endif

      /*----- -datum type option -----*/

      if( strncmp(argv[narg],"-datum",6) == 0 ){
         if( ++narg >= argc ) FatalError("need an argument after -datum!") ;

         if( strcmp(argv[narg],"short") == 0 ){
            GLOBAL_argopt.datum= MRI_short ;
         } else if( strcmp(argv[narg],"float") == 0 ){
            GLOBAL_argopt.datum= MRI_float ;
         } else if( strcmp(argv[narg],"complex") == 0 ){
            GLOBAL_argopt.datum= MRI_complex ;
         } else if( strcmp(argv[narg],"byte") == 0 ){
            GLOBAL_argopt.datum= MRI_byte ;
         } else {
            char buf[256] ;
            sprintf(buf,"-datum of type '%s' is not supported in AFNI!",
                   argv[narg] ) ;
            FatalError(buf) ;
         }
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -ncolor # option -----*/

      if( strncmp(argv[narg],"-ncolor",3) == 0 ){
         float val ;
         if( narg+1 >= argc ) FatalError("need an argument after -ncolor!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 2 ) GLOBAL_argopt.ncolor = val ;
         else fprintf(stderr,
                "\n*** warning: -ncolor value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -dset option [17 Mar 2000] -----*/

      if( strncmp(argv[narg],"-dset",5) == 0 ){
         GLOBAL_argopt.read_images   = False ;
         GLOBAL_argopt.read_sessions = False ;
         GLOBAL_argopt.read_dsets    = True  ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -im option -----*/

      if( strncmp(argv[narg],"-im",3) == 0 ){
         GLOBAL_argopt.read_images   = True ;
         GLOBAL_argopt.read_sessions = False ;
         GLOBAL_argopt.read_dsets    = False ;       /* 17 Mar 2000 */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -tim option [19 Oct 1999] -----*/

      if( strncmp(argv[narg],"-tim",4)==0 || strncmp(argv[narg],"-zim",4)==0 ){
         int ll=strlen(argv[narg]) , nn ;

         GLOBAL_argopt.read_images   = True ;
         GLOBAL_argopt.read_sessions = False ;
         GLOBAL_argopt.read_dsets    = False ;  /* 17 Mar 2000 */
         GLOBAL_argopt.read_tim      = 1 ;

         if( ll > 5 && argv[narg][4] == ':' ){         /* 20 Oct 1999 */
            nn = strtol( argv[narg]+5 , NULL , 10 ) ;
            if( nn > 1 ){
               GLOBAL_argopt.read_tim = nn ; /* will be nz or nt */
            } else {
               fprintf(stderr,"** Error: illegal value in %s\n",argv[narg]);
               exit(1) ;
            }
         }

         /* negate flag for time-order first (-tim) vs z-order first (-zim) */

         if( strncmp(argv[narg],"-tim",4)==0 && GLOBAL_argopt.read_tim > 1 )
            GLOBAL_argopt.read_tim = - GLOBAL_argopt.read_tim ;

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -nomall option -----*/

      if( strncmp(argv[narg],"-nomall",5) == 0 ){    /* was handled in main() */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -q option -----*/

      if( strcmp(argv[narg],"-q") == 0 ){            /* was handled in main() */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -- option -----*/

      if( strcmp(argv[narg],"--") == 0 ){
         narg++ ; break ;  /* end of args */
      }

      /*----- if we get here, bad news for America! -----*/

      fprintf(stderr,"\n*** Unknown option %s ***",argv[narg]) ;
      fprintf(stderr,"\n*** Try 'afni -help' for a list of command line options.\n") ;
      exit(1) ;

   } /* end of loop over argv's starting with '-' */

#ifdef USE_TRACING
   if( ALLOW_real_time ) DBG_trace = 0 ; /* 26 Jan 2001 */
#endif

   /** 16 July 1997: orientation code change **/

   if( GLOBAL_argopt.orient_code[0] == '-' ){
      if( GLOBAL_argopt.read_images )
         strcpy(GLOBAL_argopt.orient_code,"ASL") ;
      else if( env_orient != NULL )
         MCW_strncpy(GLOBAL_argopt.orient_code,env_orient,4) ;
      else
         strcpy(GLOBAL_argopt.orient_code,"RAI") ;
   }

   THD_coorder_fill( GLOBAL_argopt.orient_code , &GLOBAL_library.cord ) ;

#if 0
fprintf(stderr,"\ncoorder: signs = %d %d %d  order = %d %d %d\n" ,
        GLOBAL_library.cord.xxsign ,
        GLOBAL_library.cord.yysign ,
        GLOBAL_library.cord.zzsign ,
        GLOBAL_library.cord.first ,
        GLOBAL_library.cord.second ,
        GLOBAL_library.cord.third   ) ;
#endif

   GLOBAL_argopt.first_file_arg = narg ;  /* rest of args must be files (I hope) */

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   This routine is used if hiding Xt warnings is enabled.
   It simply does nothing -- it replaces the default Xt warning handler.
-------------------------------------------------------------------------*/

void AFNI_handler(char * msg){ return ; }

/*-----------------------------------------------------------------------
   Fallback resources for AFNI.  May be overridden by the user's
   .Xdefaults file, or other resource sources.  AFNI does not come
   with an "app-defaults" file, since that would be too much like work.
-------------------------------------------------------------------------*/

static char * FALLback[] =
  {   "AFNI*fontList:             9x15bold=charset1"    ,
      "AFNI*pbar*fontList:        6x10=charset1"        ,
      "AFNI*background:           gray40"               ,
      "AFNI*menu*background:      gray40"               ,
      "AFNI*borderColor:          gray40"               ,
      "AFNI*foreground:           yellow"               ,
      "AFNI*borderWidth:          0"                    ,
      "AFNI*troughColor:          green"                ,
      "AFNI*XmLabel.translations: #override<Btn2Down>:" , /* Motif 2.0 bug */
      "AFNI*help*background:      black"                ,
      "AFNI*help*foreground:      yellow"               ,
      "AFNI*cluefont:             9x15bold"             ,
      "AFNI*help*waitPeriod:      1066"                 ,
      "AFNI*help*cancelWaitPeriod: 50"                  ,
   NULL } ;

/*-----------------------------------------------------------------------*/

#define CATCH_SIGNALS
#ifdef CATCH_SIGNALS
#include <signal.h>
void AFNI_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char * sname ;
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGINT:  sname = "SIGINT"  ; break ;
      case SIGPIPE: sname = "SIGPIPE" ; break ;
      case SIGSEGV: sname = "SIGSEGV" ; break ;
      case SIGBUS:  sname = "SIGBUS"  ; break ;
      case SIGTERM: sname = "SIGTERM" ; break ;
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname) ;
   fprintf(stderr,"*** Program Abort ***\n") ; fflush(stderr) ;
   exit(1) ;
}
#endif

/*=========================================================================
  The new AFNI main program.
    02 Aug 1999: Have moved much of the startup into a work process.
===========================================================================*/

int main( int argc , char * argv[] )
{
   int ii ;

   machdep() ; /* RWCox: 20 Apr 2001 */

#ifdef CATCH_SIGNALS
   signal(SIGINT ,AFNI_sigfunc) ;   /* may be superseded by mainENTRY below */
   signal(SIGBUS ,AFNI_sigfunc) ;
   signal(SIGSEGV,AFNI_sigfunc) ;
   signal(SIGTERM,AFNI_sigfunc) ;
#endif

   /** debug stuff **/

#ifdef USING_MCW_MALLOC
   for( ii=1 ; ii < argc ; ii++ )
      if( strncmp(argv[ii],"-nomall",5) == 0 || strncmp(argv[ii],"-rt",3) == 0 ) break ;

   if( ii == argc ) enable_mcw_malloc() ; /* 06 Mar 1999 */
#endif

#ifdef USE_TRACING
   if( argc > 1 && strncmp(argv[1],"-trace",5) == 0 ) DBG_trace = 1 ;
   if( argc > 1 && strncmp(argv[1],"-TRACE",5) == 0 ) DBG_trace = 2 ; /* 23 Aug 1998 */
#endif

#ifdef USE_TRACING
   if( ALLOW_real_time ) DBG_trace = 0 ; /* 26 Jan 2001 */
#endif

   /** 25 Oct 2001: check for -q (quiet) option right away **/

   GLOBAL_argopt.quiet = AFNI_yesenv("AFNI_QUIET") ;
   if( AFNI_VERBOSE ){
     for( ii=1 ; ii < argc ; ii++ )
       if( strcmp(argv[ii],"-q") == 0 ){ GLOBAL_argopt.quiet = 1 ; break ; }
   }

   /*--- help? ---*/

   if( argc > 1 && strncmp(argv[1],"-help",2) == 0 ) AFNI_syntax() ;

   mainENTRY("AFNI:main") ; /* 26 Jan 2001: replace ENTRY w/ mainENTRY */

   THD_set_freeup( AFNI_purge_unused_dsets ) ;  /* 18 Oct 2001 */

#if 0
   if( argc > 1 ) AFNI_logger("afni",argc,argv) ; /* 14 Aug 2001 */
#endif

   /*--- with a little help from my FRIENDS --*/

   srand48((long)time(NULL)) ;  /* initialize random number generator */

   REPORT_PROGRESS( "\n" ) ;         /* 02 Dec 2000 */
   REPORT_PROGRESS( ANNOUNCEMENT ) ;

   /*------------ 29 Nov 1999: print out precompiled version -----------*/
#ifdef SHOWOFF
#undef SHSH
#undef SHSHSH
#define SHSH(x)   #x
#define SHSHSH(x) SHSH(x)
   if( DBG_trace ){                             /* 01 Dec 1999 */
      if( strcmp("NO",SHSHSH(SHOWOFF)) != 0 ){  /* 29 Nov 1999 */
         REPORT_PROGRESS( "[[Precompiled binary "
                          SHSHSH(SHOWOFF)
                          ": "
                          __DATE__
                          "]]\n" ) ;
      } else {
         REPORT_PROGRESS( "[[Compilation date: "
                          __DATE__
                          "]]\n" ) ;
      }
   }
#undef SHSH
#undef SHSHSH
#endif

#ifdef USE_FRIENDS
   { char * sf = AFNI_get_friend() ;
     REPORT_PROGRESS( sf ) ; REPORT_PROGRESS( "\n\n" ) ;
     if( argc > 1 && strcmp(argv[1],"-friend") == 0 ) exit(0) ;  /* 19 Sep 1998 */
   }
#endif

   /*-------------------------------------------------------------*/
   /*------------ initialize the controllers list ----------------*/

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
      GLOBAL_library.controllers[ii] = NULL ;

   GLOBAL_library.controller_lock = 0 ; ENABLE_LOCK ;
   GLOBAL_library.time_lock = 0 ;                      /* 03 Nov 1998 */
   GLOBAL_library.ijk_lock  = 0 ;                      /* 11 Sep 2000 */
   SET_FIM_bkthr(10.0) ;                               /* 02 Jun 1999 */

   GLOBAL_library.hints_on = 0 ;                       /* 07 Aug 1999 */

#ifdef ALLOW_PLUGINS
   GLOBAL_library.plugins  = NULL ;
#endif

   GLOBAL_library.session  = NULL ;                    /* 20 Dec 2001 */

   /*--------------------------------------------------------------------*/
   /*--- initialize X, toplevel window, defaults, and display context ---*/

   REPORT_PROGRESS("Initializing: X11");

   MAIN_shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                                   &argc , argv , FALLback , NULL ) ;

   if( MAIN_shell == NULL ){
      fprintf(stderr,"\n*** Cannot initialize X11 ***\n") ; exit(1) ;
   }
   if( DBG_trace == 2 ){                           /* 01 Dec 1999 */
      XSynchronize(XtDisplay(MAIN_shell),TRUE) ;
      STATUS("XSynchronize is enabled") ;
   }

   MAIN_argc = argc ; MAIN_argv = argv ;  /* what's left after XtVaAppInit */

   REPORT_PROGRESS(".") ;

   /*-- 04 Jun 1999: modify order of loading arguments and defaults --*/

   for( ii=1 ; ii < argc ; ii++ )
      if( strncmp(argv[ii],"-skip_afnirc",12) == 0 ){ GLOBAL_argopt.skip_afnirc = 1; break; }

   if( ! GLOBAL_argopt.skip_afnirc ){
       char * sysenv = getenv("AFNI_SYSTEM_AFNIRC") ;       /* 12 Apr 2000 */
       if( sysenv != NULL ) AFNI_process_environ(sysenv) ;  /* 12 Apr 2000 */

       AFNI_process_environ(NULL) ;                         /* 07 Jun 1999 */
   } else {
       AFNI_mark_environ_done() ;                           /* 16 Apr 2000 */
   }

   AFNI_load_defaults( MAIN_shell ) ;

   if( ! GLOBAL_argopt.skip_afnirc ){          /* this line added 14 Jul 1998 */
      char * home = getenv("HOME") ; char fname[256] ;
      char * sysenv = getenv("AFNI_SYSTEM_AFNIRC") ;       /* 12 Apr 2000 */

      GPT = NULL ;  /* 19 Dec 1997 */

      if( sysenv != NULL )                                 /* 12 Apr 2000 */
         AFNI_process_setup( sysenv , SETUP_INIT_MODE , NULL ) ;

      if( home != NULL ){ strcpy(fname,home) ; strcat(fname,"/.afnirc") ; }
      else              { strcpy(fname,".afnirc") ; }
      AFNI_process_setup( fname , SETUP_INIT_MODE , NULL ) ;

#ifdef AFNI_DEBUG
      home = dump_PBAR_palette_table(0) ;
      if( home != NULL ){ puts(home) ; free(home) ; }
#endif

   } else {                                    /* these lines also 14 Jul 1998 */
      REPORT_PROGRESS( "[skip .afnirc]" ) ;
   }

   AFNI_parse_args( argc , argv ) ;  /* after Xt init above, only my args left */

   if( GLOBAL_argopt.xtwarns == False ){
      MAIN_old_handler = XtAppSetWarningHandler(MAIN_app,AFNI_handler) ;  /* turn off */
   }

   { char * lenv = getenv("AFNI_FIM_BKTHR") ;          /* 04 Jun 1999 */
     if( lenv != NULL ){
        float bk = strtod(lenv,NULL) ;
        if( bk >= 0.0 && bk < 100.0 ) SET_FIM_bkthr(bk) ;
     }
   }

   if( AFNI_yesenv("AFNI_ALWAYS_LOCK") ){
      for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
         GLOBAL_library.controller_lock |= (1<<ii) ;
   }

   /*-- now create first display context: MAIN_dc --*/

   GLOBAL_library.dc = MAIN_dc =
        MCW_new_DC( MAIN_shell , GLOBAL_argopt.ncolor ,
                    INIT_ncolovr , INIT_colovr , INIT_labovr ,
                    GLOBAL_argopt.gamma , GLOBAL_argopt.install_cmap ) ;

   if( MAIN_dc->depth < 9 && MAIN_dc->visual_class != TrueColor && GLOBAL_argopt.unique_dcs ){
      GLOBAL_argopt.unique_dcs = False ;
      REPORT_PROGRESS("[-unique off]") ;
   }

   /*------------------------------------*/
   /*------- take it away, Goldie -------*/
   /*------------------------------------*/

#if 0
   (void) XtAppAddWorkProc( MAIN_app, MAIN_workprocess, NULL ) ;
#else
   PLUTO_register_workproc( MAIN_workprocess , NULL ) ;
#endif

   MCW_disable_help() ;

   XtAppMainLoop(MAIN_app) ;
   exit(0) ;
}

#undef HUBERIZE
#ifdef HUBERIZE
#include "huber.c"
#endif

/*---------------------------------------------------------------------------------
   Xt work process to do most of the initialization stuff.
-----------------------------------------------------------------------------------*/

#define REFRESH XmUpdateDisplay(MAIN_im3d->vwid->top_shell)

static Boolean MAIN_workprocess( XtPointer fred )
{
   static int MAIN_calls = 0 ;  /* controls what happens */
   static int nosplash = 0 , nodown = 0 ;
   static double eltime=0.0 , max_splash=1.0 ;
   int ii ;

ENTRY("MAIN_workprocess") ;  /* 23 Jan 2001: added ENTRY/RETURN to this routine */

if(PRINT_TRACING){ char str[256]; sprintf(str,"MAIN_calls=%d",MAIN_calls); STATUS(str); }

   switch( MAIN_calls ){

      /*============================================================================
         This code is executed at the end (when MAIN_calls gets too big).
        ============================================================================*/

      default:{
         if( nosplash || nodown ) RETURN(True) ;
         if( !nodown &&
             COX_clock_time()-eltime >= max_splash ){ AFNI_splashdown(); RETURN(True); }
      }
      break ;

      /*============================================================================
         Stuff to popup the AFNI splash screen (see afni_splash.[ch]).
        ============================================================================*/

      case 0:{

#ifdef NO_FRIVOLITIES
        nosplash = 1 ;
#else
        nosplash = AFNI_yesenv("AFNI_NOSPLASH") ;
#endif
        if( !nosplash ){
           char * hh ;
           AFNI_splashup() ; eltime = COX_clock_time() ;
           hh = getenv("AFNI_SPLASHTIME") ;
           if( hh != NULL ) max_splash = strtod(hh,NULL) ;
        }
      }
      break ;

      case 1:
      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
      case 10: if( !nosplash) iochan_sleep(1) ; /* waste time to let splash popup */
      break ;

      /*============================================================================
         Next, create the first AFNI controller window.
        ============================================================================*/

      case 11:{

        int do_images ;                           /* 19 Oct 1999 */

        REPORT_PROGRESS(". Widgets") ;

        MCW_enable_help() ;

        do_images = GLOBAL_argopt.read_images ;

        MAIN_im3d = new_AFNI_controller( MAIN_shell , MAIN_dc ,
                                         do_images ? AFNI_IMAGES_VIEW
                                                   : AFNI_3DDATA_VIEW ) ;

        GLOBAL_library.controllers[0] = MAIN_im3d ;

        REPORT_PROGRESS(".") ;

        /* Always turn off Drag-n-Drop (courtesy the Motif FAQ) */

        XtVaSetValues( XmGetXmDisplay(XtDisplay(MAIN_im3d->vwid->top_shell)) ,
                          XmNdragInitiatorProtocolStyle , XmDRAG_NONE ,
                          XmNdragReceiverProtocolStyle  , XmDRAG_NONE ,
                       NULL ) ;
      }
      break ;

      /*============================================================================
         Next, read the input files (may take a while).
        ============================================================================*/

      case 12:{

        REPORT_PROGRESS(". Input files:") ;

        AFNI_read_inputs( MAIN_argc , MAIN_argv ) ;

        if( GLOBAL_library.have_dummy_dataset && MAIN_im3d->type == AFNI_3DDATA_VIEW )
           XtSetSensitive( MAIN_im3d->vwid->prog->clone_pb , False ) ;
      }
      break ;

      /*============================================================================
         Next, setup the plugins, etc.
        ============================================================================*/

      case 13:{

        GLOBAL_library.registered_0D.num = 0 ;               /* initialize registry */
        GLOBAL_library.registered_1D.num = 0 ;               /* initialize registry */
        GLOBAL_library.registered_2D.num = 0 ;               /* initialize registry */

        GLOBAL_library.registered_fim.num = 0 ;              /* 30 Jan 2000 */

        GLOBAL_library.registered_slice_proj.num = 0 ;       /* 31 Jan 2002 */

        /* these functions are now in afni_transforms.c [01 Feb 2002] */

        AFNI_register_0D_function( "Log10" , log10_func ) ;
        AFNI_register_0D_function( "SSqrt" , ssqrt_func ) ;

        AFNI_register_1D_function( "Median3" , median3_func) ;
        AFNI_register_1D_function( "OSfilt3" , osfilt3_func) ;
        AFNI_register_1D_function( "|FFT()|" , absfft_func ) ;

        AFNI_register_2D_function( "Median9" , median9_box_func ) ;
        AFNI_register_2D_function( "Winsor9" , winsor9_box_func ) ;
        AFNI_register_2D_function( "OSfilt9" , osfilt9_box_func ) ;

        AFNI_register_2D_function( "Median21", median21_box_func );
        AFNI_register_2D_function( "Winsor21", winsor21_box_func );

        AFNI_register_2D_function( "|FFT2D|" , fft2D_func ) ;

        /* 01 Feb 2000: see afni_fimfunc.c */

        AFNI_register_fimfunc("Spearman CC",1,spearman_fimfunc,NULL);
        AFNI_register_fimfunc("Quadrant CC",1,quadrant_fimfunc,NULL);

        /* 31 Jan 2002 */

        AFNI_register_slice_proj( "Minimum" , min_proj   ) ;
        AFNI_register_slice_proj( "Maximum" , max_proj   ) ;
        AFNI_register_slice_proj( "Mean"    , mean_proj  ) ;

        AFNI_register_slice_proj( "Median"  , qmed_float ) ; /* in cs_qmed.c */

        AFNI_register_slice_proj( "Extreme" , extreme_proj ) ; /* 02 Feb 2002 */

#ifdef HUBERIZE
        AFNI_register_1D_funcstr( "Huber Fit" , huber_func ) ;
#endif

#ifdef ALLOW_PLUGINS
        if( MAIN_im3d->type == AFNI_3DDATA_VIEW ){
           int nplug = 0 ;
           char str[128] ;

           if( ! GLOBAL_argopt.noplugins ){
              GLOBAL_library.plugins = PLUG_get_many_plugins( MAIN_argv[0] ) ;
              AFNI_plugin_button( MAIN_im3d ) ;
           }

           if( GLOBAL_library.plugins != NULL ) nplug = GLOBAL_library.plugins->num ;
           sprintf(str,"\n Plugins       = %d libraries read",nplug) ;
           REPORT_PROGRESS(str) ;

           if( !GLOBAL_argopt.noplugouts && !ALLOW_real_time ){  /* June 1997 */
               AFNI_init_plugouts() ;
               XtSetSensitive(MAIN_im3d->vwid->dmode->misc_plugout_pb,False) ; /* 07 Nov 2001 */
               REPORT_PROGRESS("\n Plugouts      = listening for connections") ;
           }
        }
#endif

      }
      break ;

      /*============================================================================
         Next, do the initial setup on entering the initial view.
        ============================================================================*/

      case 14:{

        OPEN_CONTROLLER( MAIN_im3d ) ;

        AFNI_initialize_controller( MAIN_im3d ) ;  /* decide what to see */
        AFNI_initialize_view( NULL , MAIN_im3d ) ; /* set up to see it */

#if 0
        if( GLOBAL_argopt.xtwarns == False )
           (void) XtAppSetWarningHandler(MAIN_app,MAIN_old_handler) ;  /* turn back on */
#endif

        /*--- Other small and quick startup stuff before AFNI can go ---*/

        MCW_help_CB( MAIN_im3d->vwid->top_shell,NULL,NULL ); /* initialize help */

        { char str[64] ;
          sprintf(str,"\n -orient       = %s", GLOBAL_library.cord.orcode ) ;
          REPORT_PROGRESS(str) ;
        }

        /* initialize hints */

        GLOBAL_library.hints_on = !AFNI_noenv("AFNI_HINTS") ;
        if( !GLOBAL_library.hints_on ) MCW_hint_toggle() ;

        if( MAIN_im3d->vwid->dmode->misc_hints_pb != NULL )
           MCW_set_bbox( MAIN_im3d->vwid->dmode->misc_hints_bbox ,
                         GLOBAL_library.hints_on ) ;

        /* Feb 1998: setup write compression from environment */
        /*           (read de-compression always works)       */

        ii = THD_enviro_write_compression() ;
        if( ii >= 0 && ii <= COMPRESS_LASTCODE ){
           char str[64] ;
           sprintf(str,"\n write compress= %s", COMPRESS_enviro[ii]) ;
           REPORT_PROGRESS(str) ;
        }

        if( ALLOW_real_time > 0 )
           REPORT_PROGRESS("\nRT: realtime plugin is active") ;

        /* 23 Sep 2000: this function will be called 0.123 seconds
                        from now to initialize the window layouts  */

        if( GLOBAL_argopt.layout_fname != NULL &&
            MAIN_im3d->type == AFNI_3DDATA_VIEW   ){

          (void) XtAppAddTimeOut( MAIN_app , 123 ,
                                  AFNI_startup_layout_CB , GLOBAL_argopt.layout_fname ) ;

          nodown = 1 ;  /* splashdown will be done in AFNI_startup_layout_CB */
        }

        /* this function will be called 1.234 seconds from now to finalize
           anything else that needs fixing up once AFNI is fully started   */

        PICTURE_ON(MAIN_im3d) ;
        (void) XtAppAddTimeOut( MAIN_app, 1234, AFNI_startup_timeout_CB, MAIN_im3d ) ;

        (void) TRUST_host(NULL) ; /* 21 Feb 2001: initialize trust mechanism */

        /* see if there is an initial FIM ideal timeseries */

        { char *eee = getenv( "AFNI_FIM_IDEAL" ) ;
          static MRI_IMAGE *tsim ;
          tsim = mri_read_1D( eee ) ;
          if( tsim != NULL ){
            float *far = MRI_FLOAT_PTR(tsim) ; int ii ; char *tname ;
            for( ii=0 ; ii < tsim->nvox ; ii++ )
               if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;
            tname = THD_trailname(eee,1) ;
            mri_add_name( tname , tsim ) ;
            AFNI_fimmer_setref( MAIN_im3d , tsim ) ;
          }
        }

        REPORT_PROGRESS("\n") ;
      }
      break ;

      /*============================================================================*/
#if 0
      case 15:{  /* not used at present, but ready to be added when needed */
      }
      break ;
#endif
   }

   MAIN_calls++ ; RETURN(False) ;
}

/*-------------------------------------------------------------------------*/

void FatalError(char * str)
{
   fprintf(stderr,"\n**** Fatal Error ****\n %s\n",str) ;
   sleep(1) ;
   exit(1) ;
}

/*-------------------------------------------------------------------------
   Callback for the quit button.  If called with the widget == NULL,
   resets the button to the lowercase state.
---------------------------------------------------------------------------*/

void AFNI_quit_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   XmPushButtonCallbackStruct * pbcbs = (XmPushButtonCallbackStruct *) cbs ;

ENTRY("AFNI_quit_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /* NULL widget --> reset button to lowercase */

   if( wcall == NULL ){
      if( im3d->vwid->prog->quit_first == False ){
         MCW_set_widget_label( im3d->vwid->prog->quit_pb , "done " ) ;
         im3d->vwid->prog->quit_first = True ;
         if( im3d->vwid->picture != NULL && !GLOBAL_argopt.keep_logo )
            PICTURE_OFF( im3d ) ;
      }
      EXRETURN ;
   }

   /* Press of button with Shift or Control key pressed --> Death Now */

   if( pbcbs != NULL                       &&
       pbcbs->event != NULL                &&
       pbcbs->event->type == ButtonRelease &&
       ((XButtonEvent *)(pbcbs->event))->state &  /* note single & here! */
       (ShiftMask|ControlMask|Button2Mask|Button3Mask) ){

      XtCloseDisplay( XtDisplay(im3d->vwid->top_shell) ) ;
      /* MCHECK ; */
      exit(0) ;
   }

   /* First press --> just change button label */

   if( wcall == im3d->vwid->prog->quit_pb && im3d->vwid->prog->quit_first ){
      MCW_set_widget_label( im3d->vwid->prog->quit_pb , "DONE " ) ;
      im3d->vwid->prog->quit_first = False ;
      if( im3d->vwid->picture != NULL ) PICTURE_ON( im3d ) ;

      /* if not re-pressed in 5 seconds, will reset to lowercase */

      (void) XtAppAddTimeOut(
               XtWidgetToApplicationContext(im3d->vwid->prog->quit_pb) ,
               5000 , AFNI_quit_timeout_CB , im3d ) ;

      EXRETURN ;
   }

   /* close window callback OR button already uppercase --> close window */

   /* if no controller windows will be left, exit the program */

   if( AFNI_count_controllers() <= 1 ){
      XtCloseDisplay( XtDisplay(im3d->vwid->top_shell) ) ;
      /* MCHECK ; */
      exit(0) ;

   } else {  /* otherwise, patch up the other windows and continue */

      CLOSE_CONTROLLER(im3d) ;     /* close window */
      AFNI_controller_clonify() ;  /* let other controllers know */
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------
  Timeout routine to change 'DONE' button label back to 'done'
  after 5 seconds have passed.
------------------------------------------------------------------------*/

void AFNI_quit_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
ENTRY("AFNI_quit_timeout_CB") ;
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
  This function is called about 1 s after AFNI startup is completed.
  It's original purpose was to make sure that the help window was
  popped down - the help initializing routine does this, too, but
  it didn't work properly on the old Tektronix X-terminal at MCW.
  Thus, the timeout - waiting a little made things work OK.
------------------------------------------------------------------------*/

void AFNI_startup_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;

ENTRY("AFNI_startup_timeout_CB") ;

   /* make sure help window is popped down */

   MCW_help_CB(NULL,NULL,NULL) ;

   /* NIML listening on [moved here 17 Mar 2002] */

   if( MAIN_im3d->type == AFNI_3DDATA_VIEW && GLOBAL_argopt.yes_niml ){
     AFNI_init_niml() ;
     XtSetSensitive(MAIN_im3d->vwid->dmode->misc_niml_pb,False) ;
   }

   /* finish up getting AFNI ready to be presented to the world */

   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;
   PICTURE_OFF(im3d) ;
   MPROBE ;                       /* check mcw_malloc() for integrity */
   EXRETURN ;
}

/*----------------------------------------------------------------------
   routine to extract a plane of data from a 3D brick
   (used as a "get_image" routine for an MCW_imseq)
------------------------------------------------------------------------*/

XtPointer AFNI_brick_to_mri( int n , int type , FD_brick * br )
{
   MRI_IMAGE * im ;
   MCW_imseq_status * stat ;
   int i1,i2,jb,bb , dd1,dd2,tt1,tt2 ;

ENTRY("AFNI_brick_to_mri") ;

if(PRINT_TRACING){ char str[256] ; sprintf(str,"n=%d type=%d",n,type) ; STATUS(str) ; }

   /*-------------------------------------------------*/
   /*-------- May 1996: graph callbacks first --------*/

   if( type == graCR_getstatus ){
      MCW_grapher_status * grstat = myXtNew( MCW_grapher_status ) ;

      grstat->num_total  = grstat->num_series = br->dset->dblk->nvals ;
      grstat->nx         = br->n1 ;
      grstat->ny         = br->n2 ;
      grstat->nz         = br->n3 ;

      grstat->send_CB    = AFNI_gra_send_CB ;
      grstat->parent     = (XtPointer) br ;
      grstat->aux        = NULL ;

      grstat->transforms0D = & (GLOBAL_library.registered_0D) ;
      grstat->transforms1D = & (GLOBAL_library.registered_1D) ;

      strcpy( grstat->namecode , br->namecode ) ;

      RETURN( (XtPointer) grstat ) ;
   }

   if( type == graCR_getseries ){
      im = FD_brick_to_series( n , br ) ;
      RETURN( (XtPointer) im ) ;
   }

   /*----------------------------------------*/
   /*-------- Now do imseq callbacks --------*/

   if( n < 0 || n >= br->n3 ) RETURN(NULL) ;

   /*--- overlay # n ---*/

   if( type == isqCR_getoverlay  ){
      Three_D_View * im3d = (Three_D_View *) br->parent ;

STATUS("get overlay") ;

      im = AFNI_overlay( n , br ) ;
      if( AFNI_yesenv("AFNI_VALUE_LABEL") ) AFNI_do_bkgd_lab( im3d ) ;
      RETURN( (XtPointer) im ) ;
   }

   /*--- status ---*/

   if( type == isqCR_getstatus ){

STATUS("get status") ;

      stat = myXtNew( MCW_imseq_status ) ;

      stat->num_total  = br->n3 ;
      stat->num_series = br->n3 ;
      stat->send_CB    = AFNI_seq_send_CB ;
      stat->parent     = (XtPointer) br ;
      stat->aux        = NULL ;

      stat->transforms0D = & (GLOBAL_library.registered_0D) ;
      stat->transforms2D = & (GLOBAL_library.registered_2D) ;
      stat->slice_proj   = & (GLOBAL_library.registered_slice_proj) ;

      RETURN( (XtPointer) stat ) ;
   }

   /*--- 26 Feb 2001: return a memplot drawing struct ---*/
   /*--- 22 Mar 2002: add crosshairs to surface stuff ---*/

#define RX 0.25
   if( type == isqCR_getmemplot ){
     Three_D_View * im3d = (Three_D_View *) br->parent ;
     int do_surf=(SUMA_ENABLED                   && (br->dset->su_surf != NULL)) ;
     int do_xhar=(im3d->vinfo->crosshair_visible && AFNI_yesenv("AFNI_CROSSHAIR_LINES")) ;
     MEM_plotdata * mp ;

     if( !do_surf && !do_xhar ) RETURN(NULL) ;

     /* get ready to plot */

     create_memplot_surely( "AGNI_plot" , 1.0 ) ;
     mp = get_active_memplot() ;

     /* plot surface stuff, if any */

     if( do_surf ){
      SUMA_surface *ag = br->dset->su_surf ;
      int nn=ag->num_ixyz , ii,jj ;
      SUMA_ixyz *nod = ag->ixyz ;
      THD_ivec3 iv,ivp,ivm ;
      THD_fvec3 fv,fvp,fvm ;
      float s1=1.0/br->n1 , s2=1.0/br->n2 , dxyz , rr=1.0,gg=0.8,bb=0.0 ;
      char str[32] , *eee ;
      float rx=RX ;         /* default rectangle halfsize */
      int   kkk=0 ;
      float xyz=0 , rxm,rxp ;
      int skip_boxes=0 , skip_lines=0 ;

      if( nn < 1 || nod == NULL ) RETURN(NULL) ;  /* nothing to do */

      /* define overlay color for node boxes */

      eee = getenv("AFNI_SUMA_BOXCOLOR") ;
      if( eee == NULL )
        eee = getenv("AGNI_OVERLAY_COLOR") ;  /* the old way */
      if( eee != NULL ){
         if( strcmp(eee,"none") == 0 || strcmp(eee,"skip") == 0 )
            skip_boxes = 1 ;                  /* don't do boxes */
         else
            DC_parse_color( im3d->dc , eee , &rr,&gg,&bb ) ;
      }

      eee = getenv("AFNI_SUMA_LINECOLOR") ;   /* don't do lines? */
      if( eee != NULL &&
         (strcmp(eee,"none")==0 || strcmp(eee,"skip")==0) ) skip_lines = 1 ;

      if( skip_boxes && skip_lines ) RETURN(NULL) ; /* nothing to do? */

      eee = getenv("AFNI_SUMA_BOXSIZE") ;  /* maybe set boxsize? */
      if( eee != NULL ){
         float val=strtod(eee,NULL) ;
         if( val > 0.0 ) rx = val ;
      }

      /** 21 Mar 2002:
          We calculate plotting coordinates in "fdfind" coordinates,
          which are floating point indexes into the FD_brick.  However,
          these run from 0..n1-1 (in x), which are the centers of the
          voxels.  In turn these must be mapped to screen locations.
          For example, with n1=5, we have these voxels

              0   1   2   3   4    = index of voxel
            ---------------------
            |   |   |   |   |   |
            ---------------------
           0.0                 1.0 = screen coordinate (for memplot)

          Thus voxel index i maps to screen location (i+0.5)/n1.
          Previously, I forgot the +0.5, which didn't matter much,
          until the introduction of the image zoom feature last week. **/

      rxm = rx-0.5 ; rxp = rx+0.5 ;  /* The 0.5 voxel shift */

      /* find DICOM coordinates of next slice and previous slice */

      LOAD_IVEC3(iv,0,0,n+1) ;                     /* next */
      ivp = THD_fdind_to_3dind( br , iv ) ;
      fvp = THD_3dind_to_3dmm ( br->dset , ivp ) ;
      fvp = THD_3dmm_to_dicomm( br->dset , fvp ) ;
      LOAD_IVEC3(iv,0,0,n-1) ;                     /* previous */
      ivm = THD_fdind_to_3dind( br , iv ) ;
      fvm = THD_3dind_to_3dmm ( br->dset , ivm ) ;
      fvm = THD_3dmm_to_dicomm( br->dset , fvm ) ;

      /* threshold for determining which axis this slice is along */

      dxyz = MIN(br->del1,br->del2) ;
      dxyz = MIN(dxyz    ,br->del3) ; dxyz *= 0.1 ;

      set_color_memplot(rr,gg,bb) ;

      /* find nodes inside this slice */

      if( fabs(fvm.xyz[0]-fvp.xyz[0]) > dxyz ){               /* search x */
         float xb=fvm.xyz[0] , xt=fvp.xyz[0] , xm,xw ;        /* range of  */
         if( xb > xt ){ float t=xb ; xb=xt ; xt=t ; }         /* x in slice */
         xm = 0.5*(xb+xt); xw = 0.25*(xt-xb); xb = xm-xw; xt = xm+xw;
         if( !skip_boxes ){
          for( ii=0 ; ii < nn ; ii++ ){
            if( nod[ii].x > xb && nod[ii].x < xt ){           /* inside?  */
               LOAD_FVEC3(fv,nod[ii].x,nod[ii].y,nod[ii].z) ; /* convert  */
               fv = THD_dicomm_to_3dmm( br->dset , fv ) ;     /* coords   */
               fv = THD_3dmm_to_3dfind( br->dset , fv ) ;     /* to slice */
               fv = THD_3dfind_to_fdfind( br , fv ) ;         /* indexes  */
               plotrect_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                 s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  ) ;
            }
          }
         }
         kkk = 0 ; xyz = xm ;  /* for the triangles/lines below */
      }
      else if( fabs(fvm.xyz[1]-fvp.xyz[1]) > dxyz ){          /* search y */
         float yb=fvm.xyz[1] , yt=fvp.xyz[1] , ym,yw ;
         if( yb > yt ){ float t=yb ; yb=yt ; yt=t ; }
         ym = 0.5*(yb+yt); yw = 0.25*(yt-yb); yb = ym-yw; yt = ym+yw;
         if( !skip_boxes ){
          for( ii=0 ; ii < nn ; ii++ ){
            if( nod[ii].y > yb && nod[ii].y < yt ){
               LOAD_FVEC3(fv,nod[ii].x,nod[ii].y,nod[ii].z) ;
               fv = THD_dicomm_to_3dmm( br->dset , fv ) ;
               fv = THD_3dmm_to_3dfind( br->dset , fv ) ;
               fv = THD_3dfind_to_fdfind( br , fv ) ;
               plotrect_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                 s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  ) ;
            }
          }
         }
         kkk = 1 ; xyz = ym ;  /* for the triangles/lines below */
      }
      else if( fabs(fvm.xyz[2]-fvp.xyz[2]) > dxyz ){          /* search z */
         float zb=fvm.xyz[2] , zt=fvp.xyz[2] , zm,zw ;
         if( zb > zt ){ float t=zb ; zb=zt ; zt=t ; }
         zm = 0.5*(zb+zt); zw = 0.25*(zt-zb); zb = zm-zw; zt = zm+zw;
         if( !skip_boxes ){
          for( ii=0 ; ii < nn ; ii++ ){
            if( nod[ii].z > zb && nod[ii].z < zt ){
               LOAD_FVEC3(fv,nod[ii].x,nod[ii].y,nod[ii].z) ;
               fv = THD_dicomm_to_3dmm( br->dset , fv ) ;
               fv = THD_3dmm_to_3dfind( br->dset , fv ) ;
               fv = THD_3dfind_to_fdfind( br , fv ) ;
               plotrect_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                 s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  ) ;
            }
          }
         }
         kkk = 2 ; xyz = zm ;  /* for the triangles/lines below */
      }

      /* 10 Mar 2002:
         For each triangle that crosses the plane of the slice,
         plot a line segment at the intersection of the plane and triangle.
         The plane is along DICOM axis #kkk at coordinate xyz;
         these variables were set just above in the node display code. */

      if( !skip_lines && ag->num_ijk > 0 && ag->ijk != NULL ){
        SUMA_ijk *tr = ag->ijk ;        /* triangle list  */
        int      ntr = ag->num_ijk ;    /* number of triangles */
        int id,jd,kd ;
        THD_fvec3 fvijk[3] ;
        float ci,cj,ck ;

        eee = getenv("AFNI_SUMA_LINECOLOR") ;    /* define overlay color */
        if( eee != NULL )
           DC_parse_color( im3d->dc , eee , &rr,&gg,&bb ) ;
        else
           rr = gg = bb = 1.0 ;                  /* default is white */
        set_color_memplot(rr,gg,bb) ;

        /* loop over triangles */

        for( ii=0 ; ii < ntr ; ii++ ){

          /* get indexes of triangle's nodes (from their id's) */

          id = SUMA_find_node_id(ag,tr[ii].id); if( id <0 ) continue;
          jd = SUMA_find_node_id(ag,tr[ii].jd); if( jd <0 ) continue;
          kd = SUMA_find_node_id(ag,tr[ii].kd); if( kd <0 ) continue;

          /* load DICOM coords of triangle's nodes */

          LOAD_FVEC3(fvijk[0], nod[id].x, nod[id].y, nod[id].z) ;
          LOAD_FVEC3(fvijk[1], nod[jd].x, nod[jd].y, nod[jd].z) ;
          LOAD_FVEC3(fvijk[2], nod[kd].x, nod[kd].y, nod[kd].z) ;

          /* want 1 node on one size of plane, and 2 on the other */

          ci = fvijk[0].xyz[kkk] - xyz ;      /* differences from center */
          cj = fvijk[1].xyz[kkk] - xyz ;      /* of current slice plane */
          ck = fvijk[2].xyz[kkk] - xyz ;
          jj = 4*(ci > 0.0) + 2*(cj > 0.0) + (ck > 0.0) ;
          if( jj == 0 || jj == 7 ) continue ; /* all have same sign */

          /* setup id,jd,kd so fvijk[id] is on one side of plane,
             and so that fvijk[jd] and fvijk[kd] are on other side */

          switch( jj ){
             case 6:
             case 1: id = 2 ; jd = 0 ; kd = 1 ; break ;  /* kd is the 1 */
             case 5:
             case 2: id = 1 ; jd = 0 ; kd = 2 ; break ;  /* jd is the 1 */
             case 4:
             case 3: id = 0 ; jd = 1 ; kd = 2 ; break ;  /* id is the 1 */
          }

          /* linearly interpolate between fvijk[id] and fvijk[jd]
             to find the point where this line hits the slice plane */

          ci = fvijk[id].xyz[kkk] - xyz ;
          cj = fvijk[id].xyz[kkk] - fvijk[jd].xyz[kkk] ;
          if( cj == 0.0 ) continue ;            /* should not happen */
          ck = ci / cj ;
          if( ck < 0.0 || ck > 1.0 ) continue ; /* should not happen */
          cj = 1.0 - ck ;
          fvp = SCLADD_FVEC3(cj,fvijk[id],ck,fvijk[jd]) ;

          /* linearly interpolate between fvijk[id] and fvijk[kd] */

          cj = fvijk[id].xyz[kkk] - fvijk[kd].xyz[kkk] ;
          if( cj == 0.0 ) continue ;
          ck = ci / cj ;
          if( ck < 0.0 || ck > 1.0 ) continue ;
          cj = 1.0 - ck ;
          fvm = SCLADD_FVEC3(cj,fvijk[id],ck,fvijk[kd]) ;

          /* transform interpolated points to FD_brick coords */

          fvp = THD_dicomm_to_3dmm( br->dset , fvp ) ;
          fvp = THD_3dmm_to_3dfind( br->dset , fvp ) ;
          fvp = THD_3dfind_to_fdfind( br , fvp ) ;
          fvm = THD_dicomm_to_3dmm( br->dset , fvm ) ;
          fvm = THD_3dmm_to_3dfind( br->dset , fvm ) ;
          fvm = THD_3dfind_to_fdfind( br , fvm ) ;

          /* plot a line segment between them, in the plane of the slice */
          /* [21 Mar 2002: include the 0.5 shift mentioned way up above] */

          plotline_memplot( s1*(fvp.xyz[0]+0.5) , 1.0-s2*(fvp.xyz[1]+0.5) ,
                            s1*(fvm.xyz[0]+0.5) , 1.0-s2*(fvm.xyz[1]+0.5)  ) ;

        } /* end of loop over triangles */
      } /* end of if over doing lines */
     } /* end of plotting surface stuff */

     /*----- put crosshairs on with lines, if desired -----*/
     /****** 22 Mar 2002: adapted from pixel overlay  ******/

     if( do_xhar ){
      MCW_grapher * grapher = UNDERLAY_TO_GRAPHER(im3d,br) ;

      THD_ivec3 ib = THD_3dind_to_fdind( br ,
                                         TEMP_IVEC3( im3d->vinfo->i1 ,
                                                     im3d->vinfo->j2 ,
                                                     im3d->vinfo->k3  ) ) ;

      if( n == ib.ijk[2] || im3d->vinfo->xhairs_all ){
         int jp,ip , jcen,icen , gappp , jj,ii ;
         int idown,iup,iskip , jdown,jup,jskip , imon,jmon ;
         int a1 = br->a123.ijk[0] ,   /* x axis of the brick?    */
             ax = abs(a1) - 1       ; /* 0,1,2 for dataset x,y,z */
         int a2 = br->a123.ijk[1] ,   /* y axis of the brick?    */
             ay = abs(a2) - 1       ; /* 0,1,2 for dataset x,y,z */
         int a3 = br->a123.ijk[2] ,   /* z axis of the brick?    */
             az = abs(a3) - 1       ; /* 0,1,2 for dataset x,y,z */

         int gap,icr,jcr , nx=br->n1 , ny=br->n2 ;

         float rr,gg,bb ;             /* colors */
         float s1=1.0/br->n1 , s2=1.0/br->n2 ;  /* scale pixels to plot coords */
#define PSX(i) (s1*((i)+0.5))
#define PSY(j) (1.0-s2*((j)+0.5))

         /* spatial orientations of image axes */

         int ox = (ax==0) ? br->dset->daxes->xxorient :
                  (ax==1) ? br->dset->daxes->yyorient : br->dset->daxes->zzorient ;

         int oy = (ay==0) ? br->dset->daxes->xxorient :
                  (ay==1) ? br->dset->daxes->yyorient : br->dset->daxes->zzorient ;

         jp = im3d->vinfo->crosshair_ovcolor ;
         rr = DCOV_REDBYTE  (im3d->dc,jp) / 255.0 ;
         gg = DCOV_GREENBYTE(im3d->dc,jp) / 255.0 ;
         bb = DCOV_BLUEBYTE (im3d->dc,jp) / 255.0 ;
         set_color_memplot(rr,gg,bb) ;

         gap  = (grapher==NULL) ? im3d->vinfo->crosshair_gap : (grapher->mat+1)/2 ;

         icen = ib.ijk[0] ;  /* x-index of image pixel at focus */
         jcen = ib.ijk[1] ;  /* y-index */

         /** initialize montage steps **/

         if( im3d->vinfo->xhairs_show_montage ){           /* in "Multi" mode */
            iskip = im3d->vinfo->xhairs_nskip.ijk[ax] + 1 ;
            jskip = im3d->vinfo->xhairs_nskip.ijk[ay] + 1 ;
            if( a1 > 0 ){
               idown = im3d->vinfo->xhairs_ndown.ijk[ax] ;
               iup   = im3d->vinfo->xhairs_nup.ijk[ax] ;
            } else {
               iup   = im3d->vinfo->xhairs_ndown.ijk[ax] ;
               idown = im3d->vinfo->xhairs_nup.ijk[ax] ;
            }
            if( a2 > 0 ){
               jdown = im3d->vinfo->xhairs_ndown.ijk[ay] ;
               jup   = im3d->vinfo->xhairs_nup.ijk[ay] ;
            } else {
               jup   = im3d->vinfo->xhairs_ndown.ijk[ay] ;
               jdown = im3d->vinfo->xhairs_nup.ijk[ay] ;
            }

         } else {                                          /* in "Single" Mode */
           idown = iup = jdown = jup = iskip = jskip = 0 ;
           if( grapher != NULL ){ idown=-(iup+1); jdown=-(jup+1); } /* skip lines? */
         }

         /* draw vertical lines first */

         if( (im3d->vinfo->xhairs_orimask & (1<<oy)) != 0 ){
           for( imon=-idown ; imon <= iup ; imon++ ){
             icr = icen + imon * iskip ;

             if( im3d->vinfo->xhairs_periodic ){
                while( icr < 0 )   icr += nx ;
                while( icr >= nx ) icr -= nx ;
             } else {
                if( icr < 0 || icr >= nx ) continue ;
             }

             gappp = (abs(icr-icen) <= gap) ? gap : -1 ; /* no gap if far from center */

             if( gappp < 0 ){  /* no gap => 1 vertical line */

                plotline_memplot( PSX(icr) , 0.0 , PSX(icr) , 1.0 ) ;

             } else {          /* gap => 2 vertical lines */

                jj = jcen-gappp-1 ;
                if( jj >= 0 )
                  plotline_memplot( PSX(icr) , 1.0 , PSX(icr) , PSY(jj+0.5) ) ;

                jj = jcen+gappp+1 ;
                if( jj < ny )
                  plotline_memplot( PSX(icr) , PSY(jj-0.5) , PSX(icr) , 0.0 ) ;
             }

           }
         }

         /* draw horizontal lines */

         if( (im3d->vinfo->xhairs_orimask & (1<<ox)) != 0 ){  /* 31 Dec 1998 */
           for( jmon=-jdown ; jmon <= jup ; jmon++ ){
             jcr = jcen + jmon * jskip ;
             if( im3d->vinfo->xhairs_periodic ){
                while( jcr < 0 )   jcr += ny ;
                while( jcr >= ny ) jcr -= ny ;
             } else {
                if( jcr < 0 || jcr >= ny ) continue ;
             }

             gappp = (abs(jcr-jcen) <= gap) ? gap : -1 ; /* no gap if far from center */

             if( gappp < 0 ){  /* no gap => 1 horizontal line */

                plotline_memplot( 0.0 , PSY(jcr) , 1.0 , PSY(jcr) ) ;

             } else {          /* gap => 2 horizontal lines */

                ii = icen-gappp-1 ;
                if( ii >= 0 )
                  plotline_memplot( 0.0 , PSY(jcr) , PSX(ii+0.5) , PSY(jcr) ) ;

                ii = icen+gappp+1 ;
                if( ii < nx )
                  plotline_memplot( PSX(ii-0.5) , PSY(jcr) , 1.0 , PSY(jcr) ) ;
             }
           }
         }

         /* draw grapher frame, if needed */

         if( grapher != NULL ){
            int gs = gap , gb = (grapher->mat +2)/2 ;

            jcr = jcen ; icr = icen ;

            ip = icr - gb ; if( ip < 0   ) ip = 0 ;
            ii = icr + gs ; if( ii >= nx ) ii = nx-1 ;

            jp = jcr - gb ; if( jp <  0  ) jp = 0 ;
            jj = jcr + gs ; if( jj >= ny ) jj = ny-1 ;

            plotline_memplot( PSX(ip+0.5),PSY(jp+0.5) , PSX(ii-0.5),PSY(jp+0.5) ) ;
            plotline_memplot( PSX(ii-0.5),PSY(jp+0.5) , PSX(ii-0.5),PSY(jj-0.5) ) ;
            plotline_memplot( PSX(ii-0.5),PSY(jj-0.5) , PSX(ip+0.5),PSY(jj-0.5) ) ;
            plotline_memplot( PSX(ip+0.5),PSY(jj-0.5) , PSX(ip+0.5),PSY(jp+0.5) ) ;

         } /* end if "if grapher exists" */

      } /* end of "if correct slice" (or do all slices) */
     } /* end of crosshairs */

     /*----- return the completed plot -----*/

     if( MEMPLOT_NLINE(mp) < 1 ) DESTROY_MEMPLOT(mp) ;

     RETURN(mp) ; /* will be destroyed in imseq */
   }

   /*--- 20 Sep 2001: image label ---*/

   if( type == isqCR_getlabel ){
      Three_D_View * im3d = (Three_D_View *) br->parent ;
      char *lab , str[32] , *dd ;
      THD_ivec3 iv,ivp,ivm ;
      THD_fvec3 fv,fvp,fvm ;
      float dxyz , cc ;
      int ii ;

      if( im3d->type != AFNI_3DDATA_VIEW ) RETURN(NULL) ;

      LOAD_IVEC3(iv,0,0,n) ;
      ivp = THD_fdind_to_3dind( br , iv ) ;
      fvp = THD_3dind_to_3dmm ( br->dset , ivp ) ;
      fvp = THD_3dmm_to_dicomm( br->dset , fvp ) ;

      if( n == 0 ) LOAD_IVEC3(iv,0,0,1) ;
      else         LOAD_IVEC3(iv,0,0,n-1) ;
      ivm = THD_fdind_to_3dind( br , iv ) ;
      fvm = THD_3dind_to_3dmm ( br->dset , ivm ) ;
      fvm = THD_3dmm_to_dicomm( br->dset , fvm ) ;

      dxyz = MIN(br->del1,br->del2) ;
      dxyz = MIN(dxyz    ,br->del3) ; dxyz *= 0.1 ;

      if( fabs(fvm.xyz[0]-fvp.xyz[0]) > dxyz ){ /* +=R -=L */
         cc = fvp.xyz[0] ;
         dd = ( cc >= 0.0 ) ? "L" : "R" ;
      } else if( fabs(fvm.xyz[1]-fvp.xyz[1]) > dxyz ){ /* +=P -=A */
         cc = fvp.xyz[1] ;
         dd = ( cc >= 0.0 ) ? "P" : "A" ;
      } else if( fabs(fvm.xyz[2]-fvp.xyz[2]) > dxyz ){ /* +=S -=I */
         cc = fvp.xyz[2] ;
         dd = ( cc >= 0.0 ) ? "S" : "I" ;
      } else {
        RETURN(NULL) ;   /* should never happen */
      }

      sprintf(str,"%6.2f",fabs(cc)) ;
      for( ii=strlen(str)-1 ; ii > 0 && str[ii] == '0' ; ii-- ) str[ii] = '\0' ;
      if( str[ii] == '.' ) str[ii] = '\0' ;
      strcat(str,dd) ; lab = strdup(str) ; RETURN(lab) ;
   }

   /*--- underlay image # n ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      Three_D_View * im3d = (Three_D_View *) br->parent ;
      int ival = 0 ;

      /*** decide which 3D brick to extract data from (ival) ***/

      if( DSET_NUM_TIMES(br->dset) > 1 ){     /* a time-dependent brick */
         ival = im3d->vinfo->time_index ;
         if( ival >= br->dset->dblk->nvals ) ival = br->dset->dblk->nvals -1 ;

      } else if( ISANAT(br->dset) ){          /* an anatomy brick */

         if( ISANATBUCKET(br->dset) )                     /* 30 Nov 1997 */
            ival = im3d->vinfo->anat_index ;
         else
            ival = ANAT_ival_zero[br->dset->func_type] ;  /* get default data */

      } else if( ISFUNC(br->dset) ){   /* a functional brick */

        if( ISFUNCBUCKET(br->dset) ){                     /* 30 Nov 1997 */
           ival = im3d->vinfo->fim_index ;
        }
        else {
           if( im3d->vinfo->showfunc_type == SHOWFUNC_THR  &&  /* showing thr */
               ISFUNC_UNDERLAY(im3d->vinfo->underlay_type) &&  /* as underlay? */
               FUNC_HAVE_THR(br->dset->func_type) ){

              ival = FUNC_ival_thr[br->dset->func_type] ; /* get thresh data */
           } else {
              ival = FUNC_ival_fim[br->dset->func_type] ; /* get fim data */
           }
        }
     }

      if( type == isqCR_getqimage ) ival = -1 ; /* get empty image */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"getting image n1=%d n2=%d ival=%d",br->n1,br->n2,ival) ;
  STATUS(str) ; }

      LOAD_DSET_VIEWS(im3d) ;  /* 02 Nov 1996 */

      if( ival > 0 && GLOBAL_library.have_dummy_dataset && lrand48()%2 == 1 ){
         RETURN(NULL) ;  /* 23 Apr 2001: test of imseq.c NULL image handler */
      }

      im = FD_warp_to_mri( n , ival , br ) ; /* actually get image from dataset */

      if( ival < 0 ) RETURN( (XtPointer) im ) ;  /* return fake image */

      /* allow for thresholding of underlay image */

      if( im != NULL && im3d->vinfo->underlay_type == UNDERLAY_THRFUNC &&
          ISFUNC(br->dset) && FUNC_HAVE_THR(br->dset->func_type) &&
          im3d->vinfo->func_threshold > 0.0 ){

         MRI_IMAGE * im_thr , * qim ;
         double thresh ;
         int jval ;

         if( ISFUNCBUCKET(br->dset) )          /* 30 Nov 1997 */
            jval = im3d->vinfo->thr_index ;
         else
            jval = FUNC_ival_thr[br->dset->func_type] ;

         if( jval != ival ){
            im_thr = FD_warp_to_mri( n , jval ,  br ) ;
            if( im_thr == NULL ) im_thr = im ;
         } else {
            im_thr = im ;  /* if already have threshold data */
         }

         if( ! AFNI_GOOD_FUNC_DTYPE(im_thr->kind) ){ /* should not occur */
            qim = mri_to_float( im_thr ) ;           /* but if it does,  */
            if( im_thr != im ) mri_free( im_thr ) ;  /* make an OK type  */
            im_thr = qim ;
         }

         thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;

         if( im_thr->kind == MRI_short )
            thresh *= FUNC_scale_short[br->dset->func_type];
         else if( im_thr->kind == MRI_byte )
            thresh *= FUNC_scale_byte[br->dset->func_type];

         mri_threshold( -thresh , thresh , im_thr , im ) ;
         if( im_thr != im ) mri_free( im_thr ) ;
      }

      /* Load value of current pixel into display label */
      /* April 1996: only if image is at current slice  */

      { char buf[32] = "\0" ;
        AFNI_set_valabel( br , n , im , buf ) ;
        if( buf[0] != '\0' ){
           if( im3d->vinfo->underlay_type == UNDERLAY_ANAT )
              strcpy( im3d->vinfo->anat_val , buf ) ;
           else
              im3d->vinfo->anat_val[0] = '\0' ;

           if( AFNI_yesenv("AFNI_VALUE_LABEL") ) AFNI_do_bkgd_lab( im3d ) ;

           if( im->kind != MRI_complex ){
              char qbuf[32] = "bg =" ;
              strcat(qbuf,buf) ; strcpy(buf,qbuf) ;
           }
           MCW_set_widget_label( im3d->vwid->imag->pop_bkgd_lab , buf ) ;
           XtManageChild( im3d->vwid->imag->pop_bkgd_lab ) ;
        }
      }

      RETURN( (XtPointer) im ) ;
   }

STATUS("get something else, but I don't care!") ;

   RETURN( NULL ) ;
}

/*-----------------------------------------------------------------------------*/
/*! Set a value label when the nsl-th image is in "im".
-------------------------------------------------------------------------------*/

void AFNI_set_valabel( FD_brick * br , int nsl , MRI_IMAGE * im , char * blab )
{
   Three_D_View * im3d = (Three_D_View *) br->parent ;
   THD_ivec3 ib ;

ENTRY("AFNI_set_valabel") ;

   if( ! IM3D_VALID(im3d) || ! im3d->vwid->imag->do_bkgd_lab ||
       im == NULL         || blab == NULL                      ) EXRETURN ;

   /* convert current voxel index location to FD_brick indexes */

   ib = THD_3dind_to_fdind( br , TEMP_IVEC3( im3d->vinfo->i1 ,
                                             im3d->vinfo->j2 ,
                                             im3d->vinfo->k3  ) ) ;

   /* if the input image slice index (nsl) doesn't match the current
      location of the crosshairs, then we don't care about this image */

   if( nsl != ib.ijk[2] ) EXRETURN ;

   /* otherwise, extract a value from the image and put into blab */

   switch( im->kind ){

      case MRI_byte:{
         int val = MRI_BYTE_2D(im , ib.ijk[0],ib.ijk[1]) ;
         sprintf( blab , "%6d" , val ) ;
      }
      break ;

      case MRI_short:{
         int val = MRI_SHORT_2D(im , ib.ijk[0],ib.ijk[1]) ;
         sprintf( blab , "%6d" , val ) ;
      }
      break ;

      case MRI_int:{
         int val = MRI_INT_2D(im , ib.ijk[0],ib.ijk[1]) ;
         sprintf( blab , "%6d" , val ) ;
      }
      break ;

      case MRI_float:{
         float val = MRI_FLOAT_2D(im , ib.ijk[0],ib.ijk[1]) ;
         AV_fval_to_char(val,blab) ;
      }
      break ;

      case MRI_complex:{
         int iblab ;
         complex val ;
         val = MRI_COMPLEX_2D(im , ib.ijk[0],ib.ijk[1]) ;
         AV_fval_to_char(val.r,blab) ; iblab = strlen(blab) ;
         if( val.i >= 0.0 ) blab[iblab++] = '+' ;
         AV_fval_to_char(val.i,blab+iblab) ; iblab = strlen(blab) ;
         blab[iblab++] = 'I' ; blab[iblab++] = '\0' ;
      }
      break ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------
   read image files directly into a 3D dataset.
   this will be incomplete, but is enough for display purposes.
------------------------------------------------------------------------*/

THD_3dim_dataset * AFNI_read_images( int nf , char * fname[] )
{
   MRI_IMAGE * im , * shim ;
   char * bar ;
   register int     npix , ii ;
   int nx , ny , nz , lf , kz , kim ;
   MRI_IMARR * arr ;
   char str[256] ;
   THD_3dim_dataset * dset ;
   int datum = GLOBAL_argopt.datum , dsize ;

   int nvals , nzz , nzin ;  /* 19 Oct 1999 */

ENTRY("AFNI_read_images") ;

   /*----- see if there are any images to read! -----*/

   if( nf < 1 ) FatalError("*** No images on command line!? ***") ;

   /* count total number of images */

   nz = 0 ;
   for( lf=0 ; lf < nf ; lf++ ){
      ii = mri_imcount( fname[lf] ) ;
      if( ii == 0 ){
         sprintf(str,"*** Illegal image file specifier: %s",fname[lf]) ;
         FatalError(str) ;
      }
      nz += ii ;
   }
   if( nz == 1 ) nz = 2 ;  /* special case for just one image */

   /*--- read 1st file to get sizes ---*/

   arr = mri_read_file( fname[0] ) ;
   if( arr == NULL || arr->num == 0 ){
      sprintf(str,"*** cannot read first image file: %s",fname[0]) ;
      FatalError(str) ;
   }

   im = arr->imarr[0] ;
   nx = im->nx ;
   ny = im->ny ; npix = nx * ny ;

   if( datum < 0 ) datum = im->kind ;
   if( ! AFNI_GOOD_DTYPE(datum) )
      FatalError("*** Illegal datum type found ***") ;

   dsize = mri_datum_size( (MRI_TYPE) datum ) ;
   bar   = (char *) malloc( dsize * nx*ny*nz ) ;
   if( bar == NULL ){
      fprintf(stderr,"\n** Can't malloc memory for image input!\a\n") ;
      exit(1) ;
   }

   /*--- read all files, convert if needed, put in the cube ---*/

   kz = 0 ;
   for( lf=0 ; lf < nf ; lf++ ){

      /** read the file (except the first, which we already have **/

      if( lf != 0 ){
         arr = mri_read_file( fname[lf] ) ;
         if( arr == NULL || arr->num == 0 ){
           sprintf(str,"*** cannot read image file: %s",fname[lf]) ;
           FatalError(str) ;
         }
      }

      /** for each image in file ... **/

      for( kim=0 ; kim < arr->num ; kim++ ){
         im = arr->imarr[kim] ;

         /** check if image matches dimensions of first slice **/

         if( im->nx != nx || im->ny != ny ){
            if( ! GLOBAL_argopt.resize_images ){
               sprintf(str, "*** image size mismatch:\n"
                           " *** expected nx=%d ny=%d but got nx=%d ny=%d in file %s" ,
                           nx,ny,im->nx,im->ny , fname[lf] ) ;
               FatalError(str) ;
            } else {
               MRI_IMAGE * rim ;
               rim = mri_resize( im , nx , ny ) ;
               mri_free( im ) ;
               im = rim ;
            }
         }

         /** check if image data type matches the kind we want **/

         if( im->kind == datum ){
            shim = im ;
         } else {
            shim = mri_to_mri( datum , im ) ;
            if( shim == NULL ) FatalError("*** Illegal convert! ***") ;
            mri_free( im ) ;
         }

         /** copy bytes from slice into the "bar" brick **/

         memcpy( bar + dsize*npix*kz , mri_data_pointer(shim) , dsize*npix ) ;
         kz++ ;

         KILL_1MRI(shim) ;
         if( kz%10 == 5 ) REPORT_PROGRESS(".") ;
      }
      FREE_IMARR(arr) ;  /* not DESTROY_IMARR, since images are already gone */
   }

   /*** special case of one input image ***/

   if( kz == 1 && nz == 2 ){
      memcpy( bar + dsize*npix , bar , dsize*npix ) ;
   }

   /*** tell the user what all we've read ***/

   sprintf(str,": nx=%d ny=%d nslice=%d (%s)",nx,ny,nz,MRI_TYPE_name[datum]) ;
   REPORT_PROGRESS(str) ;

   /*- 19 Oct 1999: if we are doing a -tim read,
                    then have to setup the time and z dimensions -*/

   if( GLOBAL_argopt.read_tim != 0 ){

      if( GLOBAL_argopt.read_tim > 0 ){          /* 20 Oct 1999 */
         nzin  = nzz = GLOBAL_argopt.read_tim ;  /* -zim:nzz */
         nvals = nz / nzz ;

         if( nvals*nzz != nz )
            fprintf(stderr,
                    "\n** Warning: -zim:%d does not evenly divide"
                    "number of 2D slices read=%d\n",
                    nzz , nz ) ;

      } else {
         nvals = - GLOBAL_argopt.read_tim ;      /* -tim:nvals */
         nzin  = nzz = nz / nvals ;

         if( nvals*nzz != nz )
            fprintf(stderr,
                    "\n** Warning: -tim:%d does not evenly divide"
                    "number of 2D slices read=%d\n",
                    nvals , nz ) ;
      }

      if( nvals == 1 ){
         fprintf(stderr,
                 "\n** Error: -tim or -zim has only 1 point in time!\n") ;
         exit(1) ;
      }

      if( nzz == 1 ) nzz = 2 ;  /* can't have just 1 slice */

   } else {   /* the old code */
      nvals = 1 ;
      nzz   = nz ;
   }

   /*--- now create the rest of the data structure, as far as we can ---*/

   dset                = myXtNew( THD_3dim_dataset ) ;
   dset->dblk          = myXtNew( THD_datablock ) ;
   dset->daxes         = myXtNew( THD_dataxes ) ;
   dset->dblk->diskptr = myXtNew( THD_diskptr ) ;
   dset->markers       = NULL ;
   dset->warp          = NULL ;
   dset->vox_warp      = NULL ;
   dset->warp_parent   = NULL ;
   dset->anat_parent   = NULL ;
   dset->stats         = NULL ;
   dset->pts           = NULL ;
   dset->merger_list   = NULL ;
   dset->death_mark    = 0 ;
   dset->taxis         = NULL ;
   dset->tagset        = NULL ;  /* Oct 1998 */
   ZERO_STAT_AUX( dset ) ;

   INIT_KILL(dset->kl) ;
   INIT_KILL(dset->dblk->kl) ;

   dset->dblk->diskptr->type         = DISKPTR_TYPE ;
   dset->dblk->diskptr->rank         = 3 ;
   dset->dblk->diskptr->nvals        = nvals ;  /* modified 19 Oct 1999 */
   dset->dblk->diskptr->dimsizes[0]  = nx ;
   dset->dblk->diskptr->dimsizes[1]  = ny ;
   dset->dblk->diskptr->dimsizes[2]  = nzz ;    /* modified 19 Oct 1999 */
   dset->dblk->diskptr->storage_mode = STORAGE_UNDEFINED ;
   dset->dblk->diskptr->byte_order   = THD_get_write_order() ;  /* 25 April 1998 */

   EMPTY_STRING(dset->dblk->diskptr->prefix) ;
   EMPTY_STRING(dset->dblk->diskptr->viewcode) ;
   EMPTY_STRING(dset->dblk->diskptr->filecode) ;
   EMPTY_STRING(dset->dblk->diskptr->directory_name) ;
   EMPTY_STRING(dset->dblk->diskptr->header_name) ;
   EMPTY_STRING(dset->dblk->diskptr->brick_name) ;

   dset->dblk->type        = DATABLOCK_TYPE ;
   dset->dblk->nvals       = nvals ;            /* modified 19 Oct 1999 */

   /** here is where we attach "bar" to the dataset **/

   dset->dblk->malloc_type  = DATABLOCK_MEM_MALLOC ;
   dset->dblk->brick_fac    = NULL ; /* let THD_init_datablock_brick do these */
   dset->dblk->brick_bytes  = NULL ;
   dset->dblk->brick        = NULL ;

   DSET_lock(dset) ;  /* Feb 1998: lock into memory */

   dset->dblk->brick_lab      = NULL ; /* 30 Nov 1997 */
   dset->dblk->brick_keywords = NULL ;
   dset->dblk->brick_statcode = NULL ;
   dset->dblk->brick_stataux  = NULL ;
   dset->keywords             = NULL ;

   THD_init_datablock_brick( dset->dblk , datum , NULL ) ;

   if( nvals == 1 ){

      mri_fix_data_pointer( bar , DSET_BRICK(dset,0) ) ;  /* the attachment! */

   } else {   /* 19 Oct 1999: make up a lot of bricks and attach them all */
              /* 20 Oct 1999: allow for the 3rd dimension as well         */

      int iv , jj , kk ;
      char * qbar ;

      for( iv=0 ; iv < nvals ; iv++ ){
         qbar = (char *) malloc( dsize*npix*nzz ) ;  /* space for nzz slices */

         if( GLOBAL_argopt.read_tim > 0 ){
            for( jj=0 ; jj < nzz ; jj++ ){              /* copy slices */
               kk = MIN(jj,nzin-1) ;
               memcpy( qbar + jj*dsize*npix ,
                       bar + (iv*nzin+kk)*dsize*npix , dsize*npix ) ;
            }
         } else {
            for( jj=0 ; jj < nzz ; jj++ ){              /* copy slices */
               kk = MIN(jj,nzin-1) ;
               memcpy( qbar + jj*dsize*npix ,
                       bar + (kk*nvals+iv)*dsize*npix , dsize*npix ) ;
            }
         }

         mri_fix_data_pointer( qbar , DSET_BRICK(dset,iv) ) ;
      }

      free(bar) ;  /* not needed no more no how */

      EDIT_dset_items( dset , ADN_ntt,nvals , ADN_ttdel,1.0 , ADN_none ) ;
   }

   dset->dblk->natr   = dset->dblk->natr_alloc = 0 ;
   dset->dblk->atr    = NULL ;
   dset->dblk->parent = (XtPointer) dset ;

   dset->daxes->type  = DATAXES_TYPE ;
   dset->daxes->nxx   = nx ;
   dset->daxes->nyy   = ny ;
   dset->daxes->nzz   = nzz ;        /* modified 19 Oct 1999 */
   dset->daxes->xxdel = 1.0 ;        /* arbitary units */
   dset->daxes->yydel = GLOBAL_argopt.dy ;  /* these allow user to alter */
   dset->daxes->zzdel = GLOBAL_argopt.dz ;  /* the images' aspect ratio */
   dset->daxes->xxorg = dset->daxes->yyorg = dset->daxes->zzorg = 0.0 ;
   dset->daxes->parent= (XtPointer) dset ;

#ifndef OMIT_DATASET_IDCODES
   dset->idcode = MCW_new_idcode() ;
   ZERO_IDCODE(dset->anat_parent_idcode) ;
   ZERO_IDCODE(dset->warp_parent_idcode) ;
#endif

   /* set the daxes orientation codes from the command line argument */

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )

   { char acod ;
     int xx,yy,zz ;

     acod = toupper(GLOBAL_argopt.orient_code[0]) ; xx = ORCODE(acod) ;
     acod = toupper(GLOBAL_argopt.orient_code[1]) ; yy = ORCODE(acod) ;
     acod = toupper(GLOBAL_argopt.orient_code[2]) ; zz = ORCODE(acod) ;

     if( xx<0 || yy<0 || zz<0 || ! OR3OK(xx,yy,zz) )
        FatalError("Unusable -orient code!") ;

     dset->daxes->xxorient = xx ;
     dset->daxes->yyorient = yy ;
     dset->daxes->zzorient = zz ;
   }

   dset->wod_flag  = False ;  /* no warp-on-demand */
   dset->wod_daxes = NULL ;   /* 02 Nov 1996 */

   dset->type      = GEN_ANAT_TYPE ;
   dset->view_type = dset->func_type = 0 ;

   MCW_strncpy(  dset->self_name , fname[0]             , THD_MAX_NAME  ) ;
   MCW_strncpy(  dset->label1    , "Image Display Mode" , THD_MAX_LABEL ) ;
   EMPTY_STRING( dset->label2 ) ;
   EMPTY_STRING( dset->warp_parent_name ) ;
   EMPTY_STRING( dset->anat_parent_name ) ;

   RETURN( dset ) ;
}

/*----------------------------------------------------------------------
   respond to events that one of the MCW_imseq's sends to us
------------------------------------------------------------------------*/

void AFNI_seq_send_CB( MCW_imseq * seq , FD_brick * br , ISQ_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) seq->parent ;

ENTRY("AFNI_seq_send_CB") ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"reason=%d",cbs->reason) ; STATUS(str) ; }

   if( ! IM3D_VALID(im3d) ||
       (   im3d->ignore_seq_callbacks == AFNI_IGNORE_EVERYTHING
        && cbs->reason                != isqCR_getxynim        ) ) EXRETURN ;

   switch( cbs->reason ){

      default: break ;

      case isqCR_destroy:{
         MCW_imseq * sxyz = im3d->s123 ,
                   * syzx = im3d->s231 ,
                   * szxy = im3d->s312  ;
         Widget w ;
         int a3 = br->a123.ijk[2] ,   /* z axis of the brick?    */
             az = abs(a3) - 1       ; /* 0,1,2 for dataset x,y,z */

              if( seq == sxyz ){
                 w = im3d->vwid->imag->image_xyz_pb ; im3d->s123 = NULL ; }
         else if( seq == syzx ){
                 w = im3d->vwid->imag->image_yzx_pb ; im3d->s231 = NULL ; }
         else if( seq == szxy ){
                 w = im3d->vwid->imag->image_zxy_pb ; im3d->s312 = NULL ; }
         else
                 EXRETURN ;  /* something goofy happened? */

#if 1
         myXtFree( seq->status ) ; /* 28 Sep 1998: via Purify */
#endif
         myXtFree( seq ) ;
         MCW_invert_widget(w) ;  /* back to normal */
         INIT_BKGD_LAB(im3d) ;

         /* July 1996: redraw if we just lost a crosshair montage
            (it would have been in the z direction of the brick) */

         if( im3d->vinfo->xhairs_ndown.ijk[az] > 0 ||
             im3d->vinfo->xhairs_nup.ijk[az]   > 0   ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"imseq close on axis %d --> lost xhairs in that direction",az) ;
  STATUS(str) ; }

            CLEAR_MONTAGE(im3d,br) ;

            if( im3d->vinfo->xhairs_show_montage &&
                im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING ){

               AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
            }
         }
      }
      MPROBE ;
      break ;  /* end of destroy */

      case isqCR_buttonpress:{
         XButtonEvent * xev = (XButtonEvent *) cbs->event ;

if(PRINT_TRACING){
 char str[256] ;
 sprintf(str,"isqCR_buttonpress: button=%d state=%x",xev->button,xev->state) ;
 STATUS(str) ; }

         switch( xev->button ){

            default: EXRETURN ;  /* unused button */

            case Button3:{  /* popup */
               XtVaSetValues( im3d->vwid->imag->popmenu ,
                                 XmNuserData , (XtPointer) seq ,   /* who */
                              NULL ) ;
               XmMenuPosition( im3d->vwid->imag->popmenu , xev ) ; /* where */
               XtManageChild ( im3d->vwid->imag->popmenu ) ;       /* pop */
            }
            break ;

            case Button1:{
               THD_ivec3 id ;

               /* April 1996:  only use this button press if
                               it is inside the confines of the brick */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"Button1 at %d %d %d",
          cbs->xim,cbs->yim,cbs->nim) ; STATUS(str) ; }

               if( cbs->xim >= 0 && cbs->xim < br->n1 &&
                   cbs->yim >= 0 && cbs->yim < br->n2 &&
                   cbs->nim >= 0 && cbs->nim < br->n3   ){

                  id = THD_fdind_to_3dind(
                          br , TEMP_IVEC3(cbs->xim,cbs->yim,cbs->nim) );

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str," 3D dataset coordinates %d %d %d",
          id.ijk[0],id.ijk[1],id.ijk[2] ) ; STATUS(str) ; }

                  SAVE_VPT(im3d) ;  /* save current location as jumpback */

                  if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING )
                     AFNI_set_viewpoint(
                        im3d , id.ijk[0] , id.ijk[1] , id.ijk[2] ,
                        (im3d->vinfo->crosshair_visible==True) ?
                        REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
               }
            } /* end of button 1 */
            break ;
         } /* end of switch on which button */
      }
      break ;  /* end of button press */

      case isqCR_newimage:{
         THD_ivec3 id ;

         id = THD_fdind_to_3dind( br, TEMP_IVEC3(-99999,-99999,cbs->nim) );

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"newimage input %d -> %d %d %d",
          cbs->nim , id.ijk[0],id.ijk[1],id.ijk[2] ) ;
  STATUS(str) ; }

         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING )
            AFNI_set_viewpoint(
               im3d , id.ijk[0] , id.ijk[1] , id.ijk[2] ,
               (im3d->vinfo->crosshair_visible==True) ?
               REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
      }
      break ;  /* end of new image */

      /** July 1996: an image viewer changed montage layout **/

      case isqCR_newmontage:{
         THD_ivec3 * minf = (THD_ivec3 *) cbs->userdata ;
         int ndown = minf->ijk[0], nup = minf->ijk[1], nskip = minf->ijk[2] ;
         int a3 = br->a123.ijk[2] ,   /* z axis of the brick?    */
             az = abs(a3) - 1       ; /* 0,1,2 for dataset x,y,z */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"newmontage: ndown=%d nup=%d nskip=%d a3=%d (on axis az=%d)",
          ndown,nup,nskip,a3,az) ; STATUS(str) ; }

         im3d->vinfo->xhairs_nskip.ijk[az] = nskip ;

         if( a3 > 0 ){
            im3d->vinfo->xhairs_ndown.ijk[az] = ndown ;
            im3d->vinfo->xhairs_nup.ijk[az]   = nup ;
         } else {
            im3d->vinfo->xhairs_ndown.ijk[az] = nup ;
            im3d->vinfo->xhairs_nup.ijk[az]   = ndown ;
         }

         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING )
            AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
      }
      break ;

      /* 30 Dec 1998: return the current focus position */

      case isqCR_getxynim:{
         THD_ivec3 ib ;

         ib = THD_3dind_to_fdind( br , TEMP_IVEC3( im3d->vinfo->i1 ,
                                                   im3d->vinfo->j2 ,
                                                   im3d->vinfo->k3  ) ) ;

         cbs->xim = ib.ijk[0] ; cbs->yim = ib.ijk[1] ; cbs->nim = ib.ijk[2] ;
      }
      break ;  /* end of getxynim */

      /* Arrowpad stuff */

      case isqCR_appress:{
         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING )
            AFNI_crosshair_gap_CB( NULL , (XtPointer) im3d ) ;
      }
      break ;  /* end of arrowpad center key press */

      case isqCR_dxplus:
      case isqCR_dxminus:
      case isqCR_dyplus:
      case isqCR_dyminus:{
         THD_ivec3 ib , id ;
         XButtonEvent * xev = (XButtonEvent *) cbs->event ;
         int step = 1 ;

         if( ( xev->type == ButtonPress ||
               xev->type == ButtonRelease ) &&
             (xev->state & (ShiftMask | ControlMask)) ) step = INIT_bigscroll ;

         ib = THD_3dind_to_fdind( br , TEMP_IVEC3( im3d->vinfo->i1 ,
                                                   im3d->vinfo->j2 ,
                                                   im3d->vinfo->k3  ) ) ;

         switch( cbs->reason ){
            case isqCR_dxplus:   ib.ijk[0] += step ; break ;
            case isqCR_dxminus:  ib.ijk[0] -= step ; break ;
            case isqCR_dyplus:   ib.ijk[1] += step ; break ;
            case isqCR_dyminus:  ib.ijk[1] -= step ; break ;
         }

         id = THD_fdind_to_3dind( br , ib ) ;

         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING )
            AFNI_set_viewpoint(
               im3d , id.ijk[0] , id.ijk[1] , id.ijk[2] ,
               (im3d->vinfo->crosshair_visible==True) ?
               REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
      }
      break ;  /* end of arrowpad arrow press */

      case isqCR_keypress:{
#if 0
         MCW_grapher * grapher = VIEWER_TO_GRAPHER(im3d,seq) ;
         if( grapher != NULL ){
            char buf[2] ;
            buf[0] = cbs->key ; buf[1] = '\0' ;
            GRA_handle_keypress( grapher , buf , cbs->event ) ;
         }
#endif
      }
      break ; /* end of keyboard press */

      /*--- Feb 1998: list of coordinates from button2 drawing ---*/

      case isqCR_button2_points:{
         int npts = cbs->key , zim = cbs->nim ;
         int * xyout = (int *) cbs->userdata ;
         THD_ivec3 id ;
         int nvec , ii , xim,yim , fixed_plane ;
         int * xdset , * ydset , * zdset ;

         if( zim >= 0 && zim < br->n3 && npts > 0 ){  /* if input is good */

            /* make space for translated coordinates */

            xdset = (int *) malloc( npts * sizeof(int) ) ;
            ydset = (int *) malloc( npts * sizeof(int) ) ;
            zdset = (int *) malloc( npts * sizeof(int) ) ;

            /* translate coordinates to dataset xyz indices,
               casting out any that are outside the dataset brick */

            nvec = 0 ;
            for( ii=0 ; ii < npts ; ii++ ){
               xim = xyout[2*ii] ; yim = xyout[2*ii+1] ;

               /* skip points not in the volume */

               if( xim >= 0 && xim < br->n1 && yim >= 0 && yim < br->n2 ){

                  id = THD_fdind_to_3dind( br , TEMP_IVEC3(xim,yim,zim) );
                  xdset[nvec] = id.ijk[0] ;
                  ydset[nvec] = id.ijk[1] ;
                  zdset[nvec] = id.ijk[2] ;

                  /* skip sequentially duplicate points */

                  if( nvec == 0                    ||
                      xdset[nvec] != xdset[nvec-1] ||
                      ydset[nvec] != ydset[nvec-1] ||
                      zdset[nvec] != zdset[nvec-1]   ) nvec++ ;
               }
            }

            /* send coordinates to processing routine */

            fixed_plane = abs(br->a123.ijk[2]) ;

            if( nvec > 0 ) AFNI_process_drawing( im3d ,
                                                 PLANAR_MODE+fixed_plane ,
                                                 nvec,xdset,ydset,zdset ) ;

            /* free coordinate memory */

            free(xdset) ; free(ydset) ; free(zdset) ;
         }
      }
      break ; /* end of button2 coordinates */

      /*--- 22 Aug 1998: redraw everything ---*/

      case isqCR_force_redisplay:{
         PLUTO_force_redisplay() ;  /* see afni_plugin.c */
         PLUTO_force_rebar() ;      /* ditto [23 Aug 1998] */
      }
      break ; /* end of forced redisplay */

   }  /* end of switch on reason for call */

   EXRETURN ;
}

/*----------------------------------------------------------------------
   respond to events that one of the MCW_grapher's sends to us
------------------------------------------------------------------------*/

void AFNI_gra_send_CB( MCW_grapher * grapher , FD_brick * br , GRA_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) grapher->parent ;

ENTRY("AFNI_gra_send_CB") ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"reason=%d",cbs->reason) ; STATUS(str) ; }

   if( ! IM3D_VALID(im3d) ||
       (im3d->ignore_seq_callbacks==AFNI_IGNORE_EVERYTHING) ) EXRETURN ;

   switch( cbs->reason ){

      default: break ;  /* unimplemented reasons */

      /*** Death ***/

      case graCR_destroy:{
         MCW_grapher * gxyz = im3d->g123 ,
                     * gyzx = im3d->g231 ,
                     * gzxy = im3d->g312  ;
         MCW_imseq * seq = GRAPHER_TO_VIEWER(im3d,grapher) ;
         Widget w ;

              if( grapher == gxyz ){
                 w = im3d->vwid->imag->graph_xyz_pb ; im3d->g123 = NULL ;
                 STATUS("destruction of g123") ;
         }
         else if( grapher == gyzx ){
                 w = im3d->vwid->imag->graph_yzx_pb ; im3d->g231 = NULL ;
                 STATUS("destruction of g231") ;
         }
         else if( grapher == gzxy ){
                 w = im3d->vwid->imag->graph_zxy_pb ; im3d->g312 = NULL ;
                 STATUS("destruction of g312") ;
         }
         else
                 EXRETURN ;  /* something goofy happened? */

         myXtFree( grapher->status ) ;  /* 08 Mar 1999: via mcw_malloc.c */
         myXtFree( grapher ) ;          /* free the data space */
         MCW_invert_widget(w) ;         /* back to normal */

         /* redisplay the crosshairs, if needed */

         if( seq != NULL && im3d->vinfo->crosshair_visible==True &&
             im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING     )

            drive_MCW_imseq( seq , isqDR_overlay , (XtPointer) -1 ) ;
      }
      MPROBE ;
      break ;  /* end of destroy */

      /*** User sets new location ***/

      case graCR_newxyzm:{
         THD_ivec3 id ;

         if( cbs->xcen >= 0 && cbs->xcen < br->n1 &&
             cbs->ycen >= 0 && cbs->ycen < br->n2 &&
             cbs->zcen >= 0 && cbs->zcen < br->n3   ){

            id = THD_fdind_to_3dind(
                    br , TEMP_IVEC3(cbs->xcen,cbs->ycen,cbs->zcen) );

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str," 3D dataset coordinates %d %d %d",
          id.ijk[0],id.ijk[1],id.ijk[2] ) ; STATUS(str) ; }

            if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING )
               AFNI_set_viewpoint(
                  im3d ,
                  id.ijk[0] , id.ijk[1] , id.ijk[2] ,
                  (im3d->vinfo->crosshair_visible==True) ?
                  REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
         }
      }
      break ; /* end of newxyzm */

      /*** User asks for a reference function ***/

      case graCR_pickref:{

STATUS("graCR_pickref") ;

         if( IMARR_COUNT(GLOBAL_library.timeseries) > 0 ){
            int init_ts = AFNI_ts_in_library( im3d->fimdata->fimref ) ;

            MCW_choose_timeseries( grapher->fdw_graph , "FIM Reference Vector" ,
                                   GLOBAL_library.timeseries , init_ts ,
                                   AFNI_fimmer_pickref_CB , (XtPointer) im3d ) ;
         } else {
            (void) MCW_popup_message(
                      grapher->option_rowcol ,
                      "No timeseries library\nexists to pick from!" ,
                      MCW_USER_KILL | MCW_TIMER_KILL ) ;
         }
      }
      break ; /* end of pickref */

      /*** User asks for an ort function ***/

      case graCR_pickort:{

STATUS("graCR_pickort") ;

         if( IMARR_COUNT(GLOBAL_library.timeseries) > 0 ){
            int init_ts = AFNI_ts_in_library( im3d->fimdata->fimort ) ;

            MCW_choose_timeseries( grapher->fdw_graph , "FIM Ort Vector" ,
                                   GLOBAL_library.timeseries , init_ts ,
                                   AFNI_fimmer_pickort_CB , (XtPointer) im3d ) ;
         } else {
            (void) MCW_popup_message(
                      grapher->option_rowcol ,
                      "No timeseries library\nexists to pick from!" ,
                      MCW_USER_KILL | MCW_TIMER_KILL ) ;
         }
      }
      break ; /* end of pickort */


      /*** User asks to clear FIM ***/

      case graCR_clearfim:{
         AFNI_fimmer_setref( im3d , NULL ) ;
         im3d->fimdata->refadd_count = 0 ;
      }
      break ; /* end of clearfim */

      /*** User asks to clear Ort ***/

      case graCR_clearort:{
         AFNI_fimmer_setort( im3d , NULL ) ;
      }
      break ; /* end of clearfim */

      /*** 12 Nov 1996:
           User supplies a timeseries to add to the global library ***/

      case graCR_timeseries_library:{
         MRI_IMAGE * tsim = (MRI_IMAGE *) cbs->userdata ;

         AFNI_add_timeseries( tsim ) ;
      }
      break ; /* end of timeseries_library */

      /*** User supplies a timeseries for FIM (equals or add) ***/

      case graCR_refadd:
      case graCR_refequals:{
         MRI_IMAGE * tsim = (MRI_IMAGE *) cbs->userdata ;
         MRI_IMAGE * qim , * sim ;
         float * sar , * qar ;

         if( tsim != NULL ){
            qim = mri_to_float( tsim ) ;        /* make a copy of input */
            if( im3d->fimdata->fimref == NULL   ||
                cbs->reason == graCR_refequals  ||
                im3d->fimdata->refadd_count < 1   ){

               /** equals **/

               AFNI_fimmer_setref( im3d , qim ) ;
               im3d->fimdata->refadd_count = 1 ;

            } else {
               int jj,ii , nxs , nyy , nxq , nxx , npix ;
               float fs , fq ;

               /** average **/

               sim  = mri_to_float( im3d->fimdata->fimref ) ; /* add into this copy */
               sar  = MRI_FLOAT_PTR(sim) ;
               qar  = MRI_FLOAT_PTR(qim) ;
               nxs  = sim->nx ; nxq = qim->nx ; nxx = MIN(nxs,nxq) ;
               nyy  = MIN( sim->ny , qim->ny ) ;
               npix = MIN( sim->nvox , qim->nvox ) ;

               fq = 1.0/( im3d->fimdata->refadd_count + 1.0 ) ;
               fs = 1.0 - fq ;

               for( jj=0 ; jj < nyy ; jj++ ){
                  for( ii=0 ; ii < nxx ; ii++ ){
                     if( sar[ii+jj*nxs] >= WAY_BIG || qar[ii+jj*nxq] >= WAY_BIG )
                        sar[ii+jj*nxs] = WAY_BIG ;
                     else
                        sar[ii+jj*nxs] = fs * sar[ii+jj*nxs] + fq * qar[ii+jj*nxq] ;
                  }
               }
               mri_free( qim ) ;

               AFNI_fimmer_setref( im3d , sim ) ;  /* since caller may free it later */
               im3d->fimdata->refadd_count++ ;
            }
         }
      }
      break ;

      /*** User asks to smooth reference ***/

      case graCR_refsmooth:{
         if( im3d->fimdata->fimref != NULL ){
            MRI_IMAGE * sim = mri_to_float(im3d->fimdata->fimref) ; /* copy */
            float * sar = MRI_FLOAT_PTR(sim) ;
            float aa,bb,cc ;
            int ii,jj , nx=sim->nx , ny=sim->ny ;

            for( jj=0 ; jj < ny ; jj++ ){
               bb = sar[jj*nx] ; cc = sar[1+jj*nx] ;
               for( ii=1 ; ii < nx-1 ; ii++ ){
                  aa = bb ; bb = cc ; cc = sar[ii+1+jj*nx] ;
                  if( aa < WAY_BIG && bb < WAY_BIG &&
                      cc < WAY_BIG && ii > im3d->fimdata->init_ignore )
                     sar[ii+jj*nx] = OSFILT(aa,bb,cc) ;
               }
            }
            AFNI_fimmer_setref( im3d , sim ) ;
         }
      }
      break ;

      /*** User asks to do fim! ***/

      case graCR_dofim:{
         AFNI_fimmer_execute( im3d , cbs->key , cbs->mat ) ;
      }
      break ; /* end of dofim */

      /*** User sets initial ignore count ***/

      case graCR_setignore:{
         AFNI_fimmer_setignore( im3d , cbs->key ) ;
      }
      break ;

      /*** User sets the polort order [27 May 1999] ***/

      case graCR_polort:{
         AFNI_fimmer_setpolort( im3d , cbs->key ) ;
      }
      break ;

      /*** User sets time_index ***/
      /*** 24 Jan 2001: or bucket index ***/

      case graCR_setindex:{
         MCW_arrowval * tav = im3d->vwid->imag->time_index_av ;
         MCW_arrowval * aav = im3d->vwid->func->anat_buck_av ;
         int new_index = cbs->key ;

         if( DSET_NUM_TIMES(im3d->anat_now) > 1 ){       /* 24 Jan 2001: check type */
            if( new_index != im3d->vinfo->time_index ){
               AV_assign_ival( tav , new_index ) ;
               AFNI_time_index_CB( tav , (XtPointer) im3d ) ;
            }
         } else if( ISANATBUCKET(im3d->anat_now) ){      /* 24 Jan 2001: new case */
            if( new_index != im3d->vinfo->anat_index ){
               AV_assign_ival( aav , new_index ) ;
               AFNI_bucket_CB( aav , im3d ) ;
            }
         }
      }
      break ;

      /*** Feb 1998: user clicked button2 ***/

      case graCR_button2_points:{
         THD_ivec3 id ;
         int fixed_plane ;

         if( cbs->xcen >= 0 && cbs->xcen < br->n1 &&
             cbs->ycen >= 0 && cbs->ycen < br->n2 &&
             cbs->zcen >= 0 && cbs->zcen < br->n3   ){

            /* translate image to dataset coordinates */

            id = THD_fdind_to_3dind(
                    br , TEMP_IVEC3(cbs->xcen,cbs->ycen,cbs->zcen) );

            /* send a single point */

            fixed_plane = abs(br->a123.ijk[2]) ;

            AFNI_process_drawing( im3d , SINGLE_MODE + fixed_plane ,
                                  1, &id.ijk[0], &id.ijk[1], &id.ijk[2] ) ;
         }
      }
      break ;

   } /* end of switch on callback reasons */

  EXRETURN ;
}

/*----------------------------------------------------------------------
   read the files specified on the command line
   and create the data structures
------------------------------------------------------------------------*/

void AFNI_read_inputs( int argc , char * argv[] )
{
   int id , last_color ;
   Boolean isfunc ;

ENTRY("AFNI_read_inputs") ;

   /* create empty library of dataset sessions */

   GLOBAL_library.sslist = myXtNew( THD_sessionlist ) ;
   GLOBAL_library.sslist->type = SESSIONLIST_TYPE ;
   BLANK_SESSIONLIST(GLOBAL_library.sslist) ;
   GLOBAL_library.sslist->parent = NULL ;

   /*----- read files -----*/

   if( GLOBAL_argopt.first_file_arg >= argc && GLOBAL_argopt.read_images ){
      FatalError("No image files on command line!!") ;
   }

   /*--- read directly from images (the old-fashioned way) ---*/

   if( GLOBAL_argopt.read_images ){
      THD_3dim_dataset * dset ;
      THD_session * new_ss ;
      int vv ;
      int gnim ;  /* 16 Mar 1998: names from globbing */
      char ** gname ;

      MCW_warn_expand(1) ;  /* 13 Jul 2001 */

      MCW_file_expand( argc - GLOBAL_argopt.first_file_arg ,
                       &(argv[GLOBAL_argopt.first_file_arg]) ,
                       &gnim , &gname ) ;

      MCW_warn_expand(0) ;  /* 13 Jul 2001 */

      if( gnim < 1 )
         FatalError("No valid filenames on command line?!" ) ;

      dset = AFNI_read_images( gnim , gname ) ;

      if( dset == NULL )
         FatalError("Could not form 3D dataset from images!" ) ;

      MCW_free_expand( gnim , gname ) ;

      /* set up minuscule session and session list */

      new_ss             = myXtNew( THD_session ) ;
      new_ss->type       = SESSION_TYPE ;
      BLANK_SESSION(new_ss) ;
      new_ss->num_anat   = 1 ;
      new_ss->anat[0][0] = dset ;
      new_ss->parent     = NULL ;

      MCW_strncpy( new_ss->sessname ,
                   argv[GLOBAL_argopt.first_file_arg] , THD_MAX_NAME ) ;
      MCW_strncpy( new_ss->lastname ,
                   argv[GLOBAL_argopt.first_file_arg] , THD_MAX_LABEL ) ;

      GLOBAL_library.sslist->num_sess   = 1 ;
      GLOBAL_library.sslist->ssar[0]    = new_ss ;
      GLOBAL_library.have_dummy_dataset = 1 ;

   } /** end of images input **/

   else if( GLOBAL_argopt.read_sessions ){

   /*--- sessions of 3D datasets (from to3d or from afni itself) ---*/

      char str[256] ;
      Boolean good ;
      int num_ss , qd , qs , vv , no_args , jj , nskip_noanat=0 ;
      THD_string_array * flist , * dlist=NULL ;
      char * dname , *eee ;
      THD_session * new_ss ;
      int num_dsets=0 ;      /* 04 Jan 2000 */

      num_ss  = argc - GLOBAL_argopt.first_file_arg ;
      no_args = (num_ss < 1) ;

      INIT_SARR(dlist) ;
      if( no_args ){
         if( GLOBAL_argopt.recurse > 0 ){
STATUS("no args: recursion on ./") ;
            flist = THD_get_all_subdirs( GLOBAL_argopt.recurse , "./" ) ;
            if( flist != NULL ){
               for( jj=0 ; jj < flist->num ; jj++ ){
                  ADDTO_SARR(dlist,flist->ar[jj]) ;
               }
               DESTROY_SARR(flist) ;
            }
         } else {
STATUS("no args: using ./") ;
            ADDTO_SARR(dlist,"./") ;
         }
      } else {
         for( id=0 ; id < num_ss ; id++ ){
            if( GLOBAL_argopt.recurse > 0 ){
               flist = THD_get_all_subdirs( GLOBAL_argopt.recurse ,
                                            argv[GLOBAL_argopt.first_file_arg+id] ) ;
               if( flist != NULL ){
                  for( jj=0 ; jj < flist->num ; jj++ ){
                     ADDTO_SARR(dlist,flist->ar[jj]) ;
                  }
                  DESTROY_SARR(flist) ;
               }
            } else {
               ADDTO_SARR(dlist,argv[GLOBAL_argopt.first_file_arg+id]) ;
            }
         }
      }

      /** 09 Sep 1998: eliminate duplicates from the directory list **/

      { THD_string_array * qlist ;
STATUS("normalizing directory list") ;
        qlist = THD_normalize_flist( dlist ) ;
        if( qlist != NULL ){ DESTROY_SARR(dlist) ; dlist = qlist ; }
      }

      REFRESH ;

      /* read each session, set parents, put into session list */

      num_ss = dlist->num ;
      for( id=0 ; id < num_ss ; id++ ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"try to read directory %s",dlist->ar[id]) ; STATUS(str) ; }

         dname  = dlist->ar[id] ;                /* try to read datasets from */
         new_ss = THD_init_session( dname ) ;    /* this directory name       */

         REFRESH ;

         if( new_ss != NULL && new_ss->num_anat > 0 ){  /* got something? */

            /* for anats, just set parent pointers */

            new_ss->parent = NULL ;
            for( qd=0 ; qd < new_ss->num_anat ; qd++ )
               for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
                  PARENTIZE( new_ss->anat[qd][vv] , NULL ) ;

            /* for funcs, just set parent pointers */

            for( qd=0 ; qd < new_ss->num_func ; qd++ )
               for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
                  PARENTIZE( new_ss->func[qd][vv] , NULL ) ;

            /* put the new session into place in the list of sessions */

            GLOBAL_library.sslist->ssar[(GLOBAL_library.sslist->num_sess)++] = new_ss ;

            sprintf(str,"\n session #%3d  = %s %d anatomical datasets,"
                        " %d functional datasets",
                GLOBAL_library.sslist->num_sess ,
                new_ss->sessname ,
                new_ss->num_anat , new_ss->num_func ) ;

            num_dsets += (new_ss->num_anat + new_ss->num_func) ;  /* 04 Jan 2000 */

            REPORT_PROGRESS(str) ;

            if( GLOBAL_library.sslist->num_sess == THD_MAX_NUM_SESSION &&
                id < num_ss-1 ){
               sprintf(str,"\n *** reached max no. sessions (%d) ***",
                       THD_MAX_NUM_SESSION) ;
               REPORT_PROGRESS(str) ;
               break ;                            /* exit the loop over id */
            }
         } else {

            if( new_ss != NULL && new_ss->num_anat <= 0 ){  /* not enough */
               sprintf(str,"\n*** session      %s has no anatomies!  Skipping.",dname) ;
               REPORT_PROGRESS(str) ;
               nskip_noanat ++ ;
            } else {
               STATUS("no datasets found!") ;
            }
#if 0
            REMOVEFROM_SARR( dlist , id ) ;  /* no datasets --> don't keep in list */
#endif
         }
      }  /* end of id loop (over input directory names) */

      /** did we get anything?? **/

      if( nskip_noanat > 0 ){
         REPORT_PROGRESS(
               "\n\n*** Hint: a session directory without anatomical datasets can"
                 "\n***   have an anatomical warp-on-demand copy of a functional"
                 "\n***   dataset made using a command of the form"
                 "\n***   '3ddup -spgr func+orig.HEAD'.\n" ) ;

         if( GLOBAL_library.sslist->num_sess <= 0 ) exit(1) ;
      }

      /*-- 20 Dec 2001: Try to read a "global" session --*/

      eee = getenv( "AFNI_GLOBAL_SESSION" ) ;   /* where it's supposed to be */
      if( eee != NULL ){
         new_ss =
          GLOBAL_library.session = THD_init_session( eee ) ; /* try to read datasets */

         if( new_ss != NULL ){                               /* got at least one */

            new_ss->parent = NULL ;
            for( qd=0 ; qd < new_ss->num_anat ; qd++ )
               for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
                  PARENTIZE( new_ss->anat[qd][vv] , NULL ) ;
                  DSET_MARK_FOR_IMMORTALITY( new_ss->anat[qd][vv] ) ;
               }


            for( qd=0 ; qd < new_ss->num_func ; qd++ )
               for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
                  PARENTIZE( new_ss->func[qd][vv] , NULL ) ;{
                  DSET_MARK_FOR_IMMORTALITY( new_ss->func[qd][vv] ) ;
               }

            /*-- No other sessions?  Put this session in place for viewing. --*/

            if( GLOBAL_library.sslist->num_sess == 0 ){

               GLOBAL_library.sslist->ssar[(GLOBAL_library.sslist->num_sess)++] = new_ss ;

               sprintf(str,"\n session #%3d  = %s %d anatomical datasets,"
                           " %d functional datasets",
                   GLOBAL_library.sslist->num_sess ,
                   new_ss->sessname , new_ss->num_anat , new_ss->num_func ) ;

               num_dsets += (new_ss->num_anat + new_ss->num_func) ;

               REPORT_PROGRESS(str) ;

            /*-- Have other sessions?  Append these datasets to them. --*/

            } else {
               for( id=0 ; id < GLOBAL_library.sslist->num_sess ; id++ )
                  AFNI_append_sessions( GLOBAL_library.sslist->ssar[id] , new_ss ) ;
            }
         }
      } /* end of dealing with AFNI_GLOBAL_SESSION */

      /** if nothing read at all, make up a dummy **/

      GLOBAL_library.have_dummy_dataset = 0 ;

#define QQ_NXYZ 16
#define QQ_NT   12
#define QQ_FOV  240.0

      if( GLOBAL_library.sslist->num_sess <= 0 ){
         byte * bar ;  /* as opposed to a bite bar */
         int ii , nbar , jj ;
         THD_ivec3 nxyz ;
         THD_fvec3 fxyz , oxyz ;
         char *snam = dlist->ar[0] ; /* 10 Mar 2002 */

         if( !THD_is_directory(snam) ) snam = "./" ;

         REPORT_PROGRESS("\n*** No datasets or sessions input -- Dummy dataset created.") ;

         /** manufacture a minimal session **/

         new_ss         = myXtNew( THD_session ) ;
         new_ss->type   = SESSION_TYPE ;
         new_ss->parent = NULL ;
         BLANK_SESSION(new_ss) ;
         strcpy( new_ss->sessname , snam ) ; /* pretend the dummy session */
         strcpy( new_ss->lastname , snam ) ; /* is the first argv directory */
         GLOBAL_library.sslist->num_sess   = 1 ;
         GLOBAL_library.sslist->ssar[0]    = new_ss ;
         GLOBAL_library.have_dummy_dataset = 1 ;

         /** manufacture a minimal dataset **/

         new_ss->num_anat   = 1 ;
         new_ss->anat[0][0] = EDIT_empty_copy(NULL) ;
         nxyz.ijk[0] = nxyz.ijk[1] = nxyz.ijk[2] = QQ_NXYZ ;
         fxyz.xyz[0] = fxyz.xyz[1] = fxyz.xyz[2] = QQ_FOV / QQ_NXYZ ;
         oxyz.xyz[0] = oxyz.xyz[1] = oxyz.xyz[2] = -0.5 * QQ_FOV ;
         ii = EDIT_dset_items( new_ss->anat[0][0]  ,
                                 ADN_datum_all     , MRI_byte            ,
                                 ADN_nxyz          , nxyz                ,
                                 ADN_xyzdel        , fxyz                ,
                                 ADN_xyzorg        , oxyz                ,
                                 ADN_directory_name, snam                ,
                                 ADN_prefix        , "Dummy"             ,
                                 ADN_nvals         , QQ_NT               ,
                                 ADN_malloc_type   , DATABLOCK_MEM_MALLOC,
                                 ADN_type          , HEAD_ANAT_TYPE      ,
                                 ADN_view_type     , VIEW_ORIGINAL_TYPE  ,
                                 ADN_func_type     , ANAT_EPI_TYPE       ,
#if QQ_NT > 1
                                 ADN_ntt            , QQ_NT                ,
                                 ADN_ttdel          , 1.0                  ,
                                 ADN_ttorg          , 0.0                  ,
                                 ADN_ttdur          , 0.0                  ,
                                 ADN_tunits         , UNITS_SEC_TYPE       ,
#endif
                              ADN_none ) ;
         if( ii > 0 ){
            fprintf(stderr,"\n%d errors creating dummy dataset!\a\n",ii) ;
            exit(1) ;
         }
         DSET_lock(new_ss->anat[0][0]) ; /* lock into memory */

         nbar = DSET_BRICK_BYTES(new_ss->anat[0][0],0) ;

#ifdef NO_FRIVOLITIES
         for( jj=0 ; jj < QQ_NT ; jj++ ){
            bar    = (byte *) malloc( nbar ) ;
            bar[0] = (byte) (lrand48()%127) ;
            for( ii=1 ; ii < nbar ; ii++ )
               bar[ii] = bar[ii-1] + lrand48()%(jj+2) ;
            EDIT_substitute_brick( new_ss->anat[0][0] , jj , MRI_byte , bar ) ;
         }
#else
        { /* 11 Jun 1999: start of loading RWCOX images into dummy dataset */
          static byte rrr[QQ_NXYZ*QQ_NXYZ] = {
            0,0,0,0,10,94,135,135,135,135,135,135,135,135,135,135,
            0,0,0,32,216,255,255,255,255,255,255,255,255,255,255,255,
            0,0,4,171,255,255,255,255,255,255,255,255,255,255,255,255,
            0,0,22,255,255,255,255,241,162,75,75,140,255,255,255,255,
            0,0,22,255,255,255,255,100,0,0,0,92,255,255,255,255,
            0,0,22,255,255,255,255,71,0,0,0,92,255,255,255,255,
            0,0,13,213,255,255,255,234,193,105,105,160,255,255,255,255,
            0,0,0,95,255,255,255,255,255,255,255,255,255,255,255,255,
            0,0,0,0,75,209,255,255,255,250,239,245,255,255,255,255,
            0,0,0,0,22,220,255,255,255,105,0,92,255,255,255,255,
            0,0,0,0,118,255,255,255,243,45,0,92,255,255,255,255,
            0,0,0,21,228,255,255,255,157,0,0,92,255,255,255,255,
            0,0,0,124,255,255,255,255,63,0,0,92,255,255,255,255,
            0,0,18,237,255,255,255,205,11,0,0,92,255,255,255,255,
            0,0,73,255,255,255,255,85,0,0,0,92,255,255,255,255,
            0,6,128,134,134,134,134,37,0,0,0,48,134,134,134,134 } ;

          static byte www[QQ_NXYZ*QQ_NXYZ] = {
            0,45,135,135,0,0,0,135,135,95,0,0,5,135,135,135,
            0,74,255,255,11,0,10,255,255,255,0,0,85,255,255,205,
            0,0,254,255,86,0,84,255,255,255,15,0,100,255,255,155,
            0,0,234,255,106,0,105,255,255,255,85,0,170,255,255,85,
            0,0,169,255,171,0,169,255,255,255,110,0,195,255,255,60,
            0,0,99,255,201,0,200,255,255,255,170,0,255,255,255,0,
            0,0,84,255,255,1,254,255,255,255,205,35,255,255,180,0,
            0,0,5,254,255,81,255,255,135,255,255,85,255,255,170,0,
            0,0,0,249,255,170,255,255,85,249,255,135,255,255,85,0,
            0,0,0,169,255,220,255,255,35,170,255,255,255,255,75,0,
            0,0,0,114,255,255,255,240,0,154,255,255,255,255,0,0,
            0,0,0,84,255,255,255,171,0,85,255,255,255,195,0,0,
            0,0,0,20,254,255,255,145,0,59,255,255,255,170,0,0,
            0,0,0,0,254,255,255,86,0,0,255,255,255,100,0,0,
            0,0,0,0,179,255,255,50,0,0,179,255,255,50,0,0,
            0,0,0,0,89,134,134,0,0,0,89,134,134,0,0,0 } ;

          static byte ccc[QQ_NXYZ*QQ_NXYZ] = {
            0,0,0,0,2,94,160,255,255,219,135,92,9,0,0,0,
            0,0,0,17,165,255,255,255,255,255,255,255,214,41,2,0,
            0,0,4,128,255,255,255,255,255,255,255,255,255,255,38,0,
            0,0,22,255,255,255,242,108,75,111,244,255,255,255,167,2,
            0,0,116,255,255,255,202,0,0,0,113,255,255,255,255,44,
            0,0,94,165,165,165,72,0,0,0,15,223,255,255,255,131,
            0,0,0,0,0,0,0,0,0,0,0,216,255,255,255,183,
            0,0,0,0,0,0,0,0,0,0,0,216,255,255,255,255,
            0,0,0,0,0,0,0,0,0,0,0,216,255,255,255,247,
            0,0,0,0,0,0,0,0,0,0,0,216,255,255,255,131,
            0,0,94,166,166,136,0,0,0,0,55,241,255,255,255,131,
            0,0,116,255,255,242,85,0,0,0,114,255,255,255,255,44,
            0,0,15,225,255,255,243,109,76,112,244,255,255,255,166,2,
            0,0,0,109,255,255,255,255,255,255,255,255,255,217,31,0,
            0,0,0,3,105,219,255,255,255,255,255,255,162,28,0,0,
            0,0,0,0,0,9,97,134,225,160,134,91,2,0,0,0 } ;

          static byte ooo[QQ_NXYZ*QQ_NXYZ] = {
            0,0,0,0,0,12,121,135,255,255,234,107,11,0,0,0,
            0,0,0,0,58,236,255,255,255,255,255,255,224,108,4,0,
            0,0,0,60,234,255,255,255,255,255,255,255,255,255,51,0,
            0,0,10,197,255,255,255,171,75,75,163,255,255,255,224,11,
            0,0,80,255,255,255,224,39,0,0,31,233,255,255,255,107,
            0,0,164,255,255,255,151,0,0,0,0,180,255,255,255,135,
            0,12,202,255,255,255,151,0,0,0,0,180,255,255,255,185,
            0,29,255,255,255,255,151,0,0,0,0,180,255,255,255,255,
            0,27,249,255,255,255,151,0,0,0,0,180,255,255,255,248,
            0,0,164,255,255,255,151,0,0,0,0,180,255,255,255,135,
            0,0,164,255,255,255,169,3,0,0,0,180,255,255,255,135,
            0,0,79,255,255,255,255,44,0,0,60,233,255,255,255,50,
            0,0,10,197,255,255,255,171,76,90,234,255,255,255,174,3,
            0,0,0,59,233,255,255,255,255,255,255,255,255,223,40,0,
            0,0,0,0,57,186,255,255,255,255,255,255,139,19,0,0,
            0,0,0,0,0,5,119,134,191,134,134,49,3,0,0,0 } ;

          static byte xxx[QQ_NXYZ*QQ_NXYZ] = {
            0,0,21,131,135,135,135,8,0,0,3,100,135,135,135,128,
            0,0,0,108,255,255,255,86,0,0,115,255,255,255,255,121,
            0,0,0,21,216,255,255,213,0,19,223,255,255,255,187,5,
            0,0,0,0,92,244,255,255,114,114,255,255,255,234,58,0,
            0,0,0,0,0,174,255,255,252,230,255,255,255,130,0,0,
            0,0,0,0,0,58,244,255,255,255,255,255,228,29,0,0,
            0,0,0,0,0,0,118,255,255,255,255,255,74,0,0,0,
            0,0,0,0,0,0,55,248,255,255,255,199,3,0,0,0,
            0,0,0,0,0,5,170,255,255,255,255,227,32,0,0,0,
            0,0,0,0,0,104,255,255,255,255,255,255,140,5,0,0,
            0,0,0,0,13,217,255,255,252,215,255,255,255,67,0,0,
            0,0,0,0,159,255,255,255,212,23,233,255,255,187,7,0,
            0,0,0,81,241,255,255,255,85,0,72,255,255,255,66,0,
            0,0,16,206,255,255,255,212,0,0,8,193,255,255,237,12,
            0,0,94,255,255,255,255,86,0,0,0,73,255,255,255,121,
            0,14,129,134,134,134,85,1,0,0,0,3,106,134,134,127 } ;

          static byte bob[QQ_NXYZ*QQ_NXYZ] = {
               0,0,0,60,101,133,155,165,173,161,112,54,0,0,0,0,
               0,48,104,139,141,144,154,164,162,183,195,162,76,0,0,0,
               0,111,126,119,120,132,146,174,172,194,222,226,195,88,0,0,
               70,112,100,90,108,123,175,222,229,242,247,249,246,195,50,0,
               54,53,75,87,110,129,161,219,247,249,250,250,250,241,76,0,
               53,55,93,112,116,124,151,212,243,249,250,250,249,228,103,0,
               52,62,97,134,131,125,126,154,213,242,250,250,248,200,121,0,
               50,66,89,140,130,120,125,130,151,172,187,209,242,221,174,99,
               46,71,106,150,132,79,77,111,145,133,108,159,231,247,203,174,
               110,124,134,140,114,95,78,104,211,232,205,231,250,250,221,167,
               103,115,150,146,126,104,102,120,170,215,209,202,245,250,245,103,
               62,115,140,151,136,116,102,108,110,172,225,138,184,243,123,0,
               0,56,94,122,143,128,106,106,91,122,166,113,146,197,50,0,
               0,0,0,60,140,139,119,120,117,124,164,160,152,71,0,0,
               0,0,0,0,69,124,138,131,120,168,227,194,81,0,0,0,
               0,0,0,0,0,49,69,103,131,153,141,54,0,0,0,0 } ;

          static byte * rwcox[6] = { rrr,www,ccc,ooo,xxx,bob } ;
          int kk ;

            for( jj=0 ; jj < QQ_NT ; jj++ ){
               bar = (byte *) malloc( nbar ) ;
               for( kk=0 ; kk < QQ_NXYZ ; kk++ )
                  memcpy( bar + kk*QQ_NXYZ*QQ_NXYZ , rwcox[jj%6] , QQ_NXYZ*QQ_NXYZ ) ;
               EDIT_substitute_brick( new_ss->anat[0][0] , jj , MRI_byte , bar ) ;
            }
          } /* end of loading RWCOX */
#endif

         PARENTIZE( new_ss->anat[0][0] , NULL ) ;

      } else {  /* 04 Jan 2000: show total number of datasets */

         sprintf(str,"\n dataset count = %d" , num_dsets ) ;
         REPORT_PROGRESS(str) ;
      }

      /*** read all timeseries files from all directories ***/

STATUS("reading timeseries files") ;

      /* 27 Jan 2000: allow skipping *.1D files from dataset directories */

      GLOBAL_library.timeseries =
           THD_get_many_timeseries( (GLOBAL_argopt.read_1D) ? dlist : NULL ) ;

      REFRESH ;

      if( GLOBAL_library.timeseries == NULL )
         INIT_IMARR(GLOBAL_library.timeseries) ;

      sprintf( str , "\n Time series   = %d files read" ,
               IMARR_COUNT(GLOBAL_library.timeseries) ) ;
      REPORT_PROGRESS(str) ;

      /*** throw away the list of directories that were scanned ***/

      DESTROY_SARR(dlist) ;

      /* assign the warp and anatomy parent pointers;
         then, make any datasets that don't exist but logically
         descend from the warp and anatomy parents just assigned */

STATUS("checking idcodes for duplicates") ;

      THD_check_idcodes( GLOBAL_library.sslist ) ;     /* 08 Jun 1999 */

STATUS("reconciling parent pointers") ;

      THD_reconcile_parents( GLOBAL_library.sslist ) ; /* parents from .HEAD files */

STATUS("forcible adoption of unparented datasets") ;

      for( id=0 ; id < GLOBAL_library.sslist->num_sess ; id++ ){  /* functions w/o parents, */
         new_ss = GLOBAL_library.sslist->ssar[id] ;               /* forcibly get one */
         AFNI_force_adoption( new_ss , GLOBAL_argopt.warp_4D ) ;
      }

      if( GLOBAL_library.session != NULL )
         AFNI_force_adoption( GLOBAL_library.session , GLOBAL_argopt.warp_4D ) ;

STATUS("making descendant datasets") ;

      AFNI_make_descendants( GLOBAL_library.sslist ) ;

   } /** end of sessions input **/

   else if( GLOBAL_argopt.read_dsets ){  /* 17 Mar 2000 */

      int nds = argc - GLOBAL_argopt.first_file_arg ;
      char str[256] ;
      THD_3dim_dataset * dset ;
      XtPointer_array * dsar ;
      MRI_IMARR * webtsar ;        /* 26 Mar 2001 */
      THD_session * new_ss ;
      int ii,nerr=0,vv,nn , dd ;

      if( nds <= 0 ){
         fprintf(stderr,"\a\n*** No datasets on command line?!\n"); exit(1);
      }
      nds = 0 ;

      /* set up minuscule session and session list */

      new_ss             = myXtNew( THD_session ) ;
      new_ss->type       = SESSION_TYPE ;
      BLANK_SESSION(new_ss) ;
      new_ss->parent     = NULL ;

      strcpy( new_ss->sessname , "." ) ;
      strcpy( new_ss->lastname , "." ) ;

      GLOBAL_library.sslist->num_sess   = 1 ;
      GLOBAL_library.sslist->ssar[0]    = new_ss ;
      GLOBAL_library.have_dummy_dataset = 0 ;

      /* read datasets from command line */

STATUS("reading commandline dsets") ;

      INIT_IMARR(webtsar) ; /* 26 Mar 2001 */

      for( ii=GLOBAL_argopt.first_file_arg ; ii < argc ; ii++ ){

         /** 23 Mar 2001: modified code to deal with an array of
                          datasets, rather than just one at a time **/

         if( strstr(argv[ii],"://")      != NULL &&
             strstr(argv[ii],"AFNILIST") != NULL   ){ /** 23 Mar 2001: read from Web list **/

            dsar = THD_fetch_many_datasets( argv[ii] ) ;
            if( dsar == NULL || dsar->num == 0 ){
               fprintf(stderr,"\a\n*** Can't read datasets from %s\n",argv[ii]) ;
               nerr++ ; continue ; /* next ii */
            }

         } else { /** read from one file (local or Web), make a small array **/

            dset = THD_open_dataset( argv[ii] ) ;
            if( dset == NULL ){
               fprintf(stderr,"\a\n*** Can't read dataset %s\n",argv[ii]) ;
               nerr++ ; continue ; /* next ii */
            }
            INIT_XTARR(dsar) ; ADDTO_XTARR(dsar,dset) ; XTARR_IC(dsar,0) = IC_DSET ;
         }

         for( dd=0 ; dd < dsar->num ; dd++ ){  /* loop over all entries in array */

            /* 26 Mar 2001: might get some 1D files, too */

            if( XTARR_IC(dsar,dd) == IC_FLIM ){  /* save 1D file for later */
               MRI_IMAGE * im = (MRI_IMAGE *) XTARR_XT(dsar,dd) ;
               ADDTO_IMARR(webtsar,im) ;
               continue ;              /* next one */
            }
            if( XTARR_IC(dsar,dd) != IC_DSET ){
               fprintf(stderr,"\n** Unknown filetype returned from %s\n",argv[ii]) ;
               nerr++ ; continue ;   /* bad */
            }

            /* get to here ==> have a dataset */

            dset = (THD_3dim_dataset *) XTARR_XT(dsar,dd) ;
            if( !ISVALID_DSET(dset) ) continue ;            /* bad */
            nds++ ;   /* increment count of dataset */
            REFRESH ;
            vv = dset->view_type ;
            if( ISANAT(dset) ){
               nn = new_ss->num_anat ;
               if( nn >= THD_MAX_SESSION_ANAT ){
                  fprintf(stderr,"\a\n*** too many anatomical datasets!\n") ;
                  nerr++ ;
               } else {
                  new_ss->anat[nn][vv] = dset ;
                  new_ss->num_anat++ ;
               }
            } else if( ISFUNC(dset) ){
               nn = new_ss->num_func ;
               if( nn >= THD_MAX_SESSION_FUNC ){
                  fprintf(stderr,"\a\n*** too many functional datasets!\n") ;
                  nerr++ ;
               } else {
                  new_ss->func[nn][vv] = dset ;
                  new_ss->num_func++ ;
               }
            } else {
               fprintf(stderr,"\a\n*** Unrecognized dataset type: %s\n",argv[ii]);
               nerr++ ;
            }
         } /* end of loop over dd=datasets in dsar */

         FREE_XTARR(dsar) ;  /* don't need array no more */

      } /* end of loop over ii=command line arguments past options */

      if( nerr > 0 ){
         fprintf(stderr,"** FATAL ERRORS on input\n") ; exit(1) ;  /* bad */
      }

      sprintf(str,"\n dataset count = %d" , nds ) ;
      if( nds == 0 ) exit(1) ;
      if( new_ss->num_anat == 0 ){
         fprintf(stderr,"\n*** No anatomical datasets in the list!\n") ;
         exit(1) ;
      }
      REPORT_PROGRESS(str) ;

STATUS("reading timeseries files") ;

      GLOBAL_library.timeseries = THD_get_many_timeseries( NULL ) ;

      REFRESH ;

      if( GLOBAL_library.timeseries == NULL )
         INIT_IMARR(GLOBAL_library.timeseries) ;

      /* 26 Mar 2001: store timeseries fetched from the Web */

      for( dd=0 ; dd < IMARR_COUNT(webtsar) ; dd++ )
         AFNI_add_timeseries( IMARR_SUBIMAGE(webtsar,dd) ) ;

      FREE_IMARR(webtsar) ;

      sprintf( str , "\n Time series   = %d files read" ,
               IMARR_COUNT(GLOBAL_library.timeseries) ) ;
      REPORT_PROGRESS(str) ;

      /* assign the warp and anatomy parent pointers;
         then, make any datasets that don't exist but logically
         descend from the warp and anatomy parents just assigned */

STATUS("checking idcodes for duplicates") ;

      THD_check_idcodes( GLOBAL_library.sslist ) ;

#if 0
STATUS("reconciling parent pointers") ;

      THD_reconcile_parents( GLOBAL_library.sslist ) ; /* parents from .HEAD files */

STATUS("forcible adoption of unparented datasets") ;

      for( id=0 ; id < GLOBAL_library.sslist->num_sess ; id++ ){  /* functions w/o parents, */
         new_ss = GLOBAL_library.sslist->ssar[id] ;               /* forcibly get one */
         AFNI_force_adoption( new_ss , GLOBAL_argopt.warp_4D ) ;
      }
#endif

   }  /** end of read datasets from command line **/

   else {  /* should never occur! */

     fprintf(stderr,"\a\n*** Illegal Usage configuration detected!\n"); exit(1);
   }

   MPROBE ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------
  Final adjustments before a controller is opened for use - 15 Jun 2000
----------------------------------------------------------------------------*/

void AFNI_startup_3dview( Three_D_View * im3d )
{
   static int old_0D_num=0 , old_2D_num=0 ;

ENTRY("AFNI_startup_3dview") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* the pbar Tran 0D menu */

   if( GLOBAL_library.registered_0D.num != old_0D_num ){
      old_0D_num = GLOBAL_library.registered_0D.num ;
      refit_MCW_optmenu( im3d->vwid->func->pbar_transform0D_av ,
                           0 ,                                 /* new minval */
                           GLOBAL_library.registered_0D.num ,  /* new maxval */
                           0 ,                                 /* new inival */
                           0 ,                                 /* new decim? */
                           ISQ_transform_label ,               /* text func  */
                           &(GLOBAL_library.registered_0D)     /* text data  */
                        ) ;
      XtManageChild( im3d->vwid->func->pbar_transform0D_av->wrowcol ) ;
   } else {
      if( old_0D_num == 0 )
        XtUnmanageChild( im3d->vwid->func->pbar_transform0D_av->wrowcol ) ;
   }

   im3d->vwid->func->pbar_transform0D_index = 0 ;
   im3d->vwid->func->pbar_transform0D_func  = NULL ;

   /* the pbar Tran 2D menu */

   if( GLOBAL_library.registered_2D.num != old_2D_num ){
      old_2D_num = GLOBAL_library.registered_2D.num ;
      refit_MCW_optmenu( im3d->vwid->func->pbar_transform2D_av ,
                           0 ,                                 /* new minval */
                           GLOBAL_library.registered_2D.num ,  /* new maxval */
                           0 ,                                 /* new inival */
                           0 ,                                 /* new decim? */
                           ISQ_transform_label ,               /* text func  */
                           &(GLOBAL_library.registered_2D)     /* text data  */
                        ) ;
      XtManageChild( im3d->vwid->func->pbar_transform2D_av->wrowcol ) ;
   } else {
      if( old_2D_num == 0 )
        XtUnmanageChild( im3d->vwid->func->pbar_transform2D_av->wrowcol ) ;
   }

   im3d->vwid->func->pbar_transform2D_index = 0 ;
   im3d->vwid->func->pbar_transform2D_func  = NULL ;

   /* Hey Rocky!  Watch me pull a rabbit out of my hat! */

   EXRETURN ;
}

/*--------------------------------------------------------------------------
   delete the viewers associated with this controller panel
---------------------------------------------------------------------------*/

void AFNI_closedown_3dview( Three_D_View * im3d )
{
ENTRY("AFNI_closedown_3dview") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* Mar 1999: shutoff receivers, if any */

   AFNI_receive_destroy( im3d ) ;

   /* destroy any viewers attached */

   drive_MCW_imseq( im3d->s123 , isqDR_destroy , NULL ) ;
   drive_MCW_imseq( im3d->s231 , isqDR_destroy , NULL ) ;
   drive_MCW_imseq( im3d->s312 , isqDR_destroy , NULL ) ;

   drive_MCW_grapher( im3d->g123 , graDR_destroy , NULL ) ;
   drive_MCW_grapher( im3d->g231 , graDR_destroy , NULL ) ;
   drive_MCW_grapher( im3d->g312 , graDR_destroy , NULL ) ;

   /* erase FD bricks */

   myXtFree(im3d->b123_anat) ;
   myXtFree(im3d->b231_anat) ;
   myXtFree(im3d->b312_anat) ;

   myXtFree(im3d->b123_fim)  ;
   myXtFree(im3d->b231_fim)  ;
   myXtFree(im3d->b312_fim)  ;

   im3d->b123_ulay = im3d->b231_ulay = im3d->b312_ulay = NULL ;

   if( XtIsManaged(im3d->vwid->view->frame) == True )
      AFNI_controller_panel_CB( NULL , im3d , NULL ) ;

   /* null out montage info */

   LOAD_IVEC3(im3d->vinfo->xhairs_ndown,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nup  ,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nskip,0,0,0) ;

   /* de-fim */

   AFNI_fimmer_setref(im3d,NULL) ; CLEAR_FIMDATA(im3d) ;

   RESET_AFNI_QUIT(im3d) ;

   im3d->anat_now = im3d->fim_now = NULL ;

   AFNI_purge_unused_dsets() ;

   MPROBE ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------
  Open or close the viewing controls panel
---------------------------------------------------------------------------*/
void AFNI_controller_panel_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_controller_panel_CB") ;

   if( ! IM3D_OPEN(im3d) || im3d->vwid->prog->panel_pb == NULL ) EXRETURN ;

   /** if view frame is open, close it and all its children **/

   if( XtIsManaged(im3d->vwid->view->frame) == True ){

      if( XtIsManaged(im3d->vwid->marks->frame) == True ){
         AFNI_marks_action_CB( NULL , (XtPointer) im3d , NULL ) ;
      }

      if( XtIsManaged(im3d->vwid->func->frame) ){
         CLOSE_PANEL(im3d,func) ;
      }

      if( XtIsManaged(im3d->vwid->dmode->frame) ){
         CLOSE_PANEL(im3d,dmode) ;
      }

      XtUnmanageChild(im3d->vwid->view->frame) ;
      if( im3d->vwid->prog->panel_pb_inverted ){
         MCW_invert_widget(im3d->vwid->prog->panel_pb) ;
         im3d->vwid->prog->panel_pb_inverted = False ;
      }

   } else {  /** open the view frame (but not its children) **/

      XtManageChild(im3d->vwid->view->frame) ;
      if( ! im3d->vwid->prog->panel_pb_inverted ){
         MCW_invert_widget(im3d->vwid->prog->panel_pb) ;
         im3d->vwid->prog->panel_pb_inverted = True ;
      }
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------
  Called when the user selects a new option for crosshair visibility
---------------------------------------------------------------------------*/

void AFNI_crosshair_visible_CB( MCW_arrowval * av , XtPointer client_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int val , omold ;

ENTRY("AFNI_crosshair_visible_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( av->ival == av->old_ival ) EXRETURN ;

   switch( av->ival ){
      case AFNI_XHAIRS_OFF:
         im3d->vinfo->crosshair_visible   = False ;
         im3d->vinfo->xhairs_show_montage = False ;
      break ;

      case AFNI_XHAIRS_SINGLE:
         im3d->vinfo->crosshair_visible   = True ;
         im3d->vinfo->xhairs_show_montage = False ;
      break ;

      default:                                     /* 31 Dec 1998:  */
      case AFNI_XHAIRS_MULTI:                      /*   new options */
         im3d->vinfo->crosshair_visible   = True ; /*   like Multi  */
         im3d->vinfo->xhairs_show_montage = True ;
      break ;
   }

   /* 31 Dec 1998: only allow crosshairs of some orientations */

   omold = im3d->vinfo->xhairs_orimask ;  /* 02 Jun 1999 */

   switch( av->ival ){
      default:                im3d->vinfo->xhairs_orimask = ORIMASK_ALL  ; break;
      case AFNI_XHAIRS_LR_AP: im3d->vinfo->xhairs_orimask = ORIMASK_LR_AP; break;
      case AFNI_XHAIRS_LR_IS: im3d->vinfo->xhairs_orimask = ORIMASK_LR_IS; break;
      case AFNI_XHAIRS_AP_IS: im3d->vinfo->xhairs_orimask = ORIMASK_AP_IS; break;
      case AFNI_XHAIRS_LR:    im3d->vinfo->xhairs_orimask = ORIMASK_LR   ; break;
      case AFNI_XHAIRS_AP:    im3d->vinfo->xhairs_orimask = ORIMASK_AP   ; break;
      case AFNI_XHAIRS_IS:    im3d->vinfo->xhairs_orimask = ORIMASK_IS   ; break;
   }

   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;

   /* 02 Jun 1999: if xhairs layout has changed, send a notice */

   if( omold != im3d->vinfo->xhairs_orimask ) AFNI_process_viewpoint( im3d ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_wrap_bbox_CB( Widget w ,
                        XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int bval ;

ENTRY("AFNI_wrap_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bval = MCW_val_bbox( im3d->vwid->imag->wrap_bbox ) ;

   if( (Boolean) bval == im3d->vinfo->xhairs_periodic ) EXRETURN ;

   im3d->vinfo->xhairs_periodic = (Boolean) bval ;

   if( w != NULL ){
      drive_MCW_imseq( im3d->s123, isqDR_periodicmont, (XtPointer) bval );
      drive_MCW_imseq( im3d->s231, isqDR_periodicmont, (XtPointer) bval );
      drive_MCW_imseq( im3d->s312, isqDR_periodicmont, (XtPointer) bval );
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_xhall_bbox_CB( Widget w ,
                         XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int bval ;

ENTRY("AFNI_xhall_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bval = MCW_val_bbox( im3d->vwid->imag->xhall_bbox ) ;

   if( (Boolean) bval == im3d->vinfo->xhairs_all ) EXRETURN ;

   im3d->vinfo->xhairs_all = (Boolean) bval ;

   if( im3d->vinfo->crosshair_visible ){
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_crosshair_color_CB( MCW_arrowval * av , XtPointer client_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int ipx = av->ival ;

ENTRY("AFNI_crosshair_color_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   im3d->vinfo->crosshair_ovcolor = ipx ;
   if( im3d->vinfo->crosshair_visible ){
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_crosshair_gap_CB( MCW_arrowval * av ,  XtPointer client_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int ipx ;

ENTRY("AFNI_crosshair_gap_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( av != NULL ){
      ipx = av->ival ;
   } else {
      if( im3d->vinfo->crosshair_gap_old > 0 ){
         ipx = im3d->vinfo->crosshair_gap_old ;
         im3d->vinfo->crosshair_gap_old = 0 ;
      } else {
         im3d->vinfo->crosshair_gap_old = im3d->vinfo->crosshair_gap ;
         ipx = 0 ;
      }
   }

   im3d->vinfo->crosshair_gap = ipx ;
   if( im3d->vinfo->crosshair_visible ){
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
    03 Nov 1998: allow locking of time index
-------------------------------------------------------------------------*/

void AFNI_time_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Three_D_View * qq3d ;
   int            bval , ii , bold ;

ENTRY("AFNI_time_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.time_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->time_lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.time_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

      MCW_set_bbox( qq3d->vwid->dmode->time_lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_time_lock_carryout( Three_D_View * im3d )
{
   Three_D_View * qq3d ;
   MCW_arrowval * tav ;
   int new_index , qq_index , qq_top , cc , glock , ii ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */

ENTRY("AFNI_time_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                       EXRETURN ;  /* routine already busy */
   if( glock == 0 )                 EXRETURN ;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )           EXRETURN ;  /* bad input */
   if( GLOBAL_library.ignore_lock ) EXRETURN ;  /* ordered not to do anything */
   if( ! GLOBAL_library.time_lock ) EXRETURN ;  /* don't lock time */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* load time index of this controller => all others get this value, too*/

   new_index = im3d->vinfo->time_index ;

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current
        one, and ARE locked, jump to the new time index */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      qq3d = GLOBAL_library.controllers[cc] ; /* controller */

      if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

         qq_index = qq3d->vinfo->time_index ;           /* old index */
         qq_top   = DSET_NUM_TIMES(qq3d->anat_now) ;    /* range allowed */

         if( qq_top > 1 && qq_index != new_index ){
            tav = qq3d->vwid->imag->time_index_av ;
            AV_assign_ival( tav , new_index ) ;         /* will check range */
            if( tav->ival != qq_index )
               AFNI_time_index_CB( tav , (XtPointer) qq3d ) ;
         }

      }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_time_index_CB( MCW_arrowval * av ,  XtPointer client_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int ipx ;

ENTRY("AFNI_time_index_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   ipx = av->ival ;
   if( ipx >= im3d->vinfo->top_index )    /* don't let index be too big */
      ipx = im3d->vinfo->top_index - 1 ;

   im3d->vinfo->time_index = ipx ;        /* change time index */

   im3d->vinfo->tempflag = 1 ;
   AFNI_modify_viewing( im3d , False ) ;  /* setup new bricks to view */

   if( ISVALID_DSET(im3d->fim_now)       &&   /* if time index on */
       DSET_NUM_TIMES(im3d->fim_now) > 1   )  /* function changed */
     AFNI_process_funcdisplay( im3d ) ;       /* notify receivers */

   AFNI_time_lock_carryout( im3d ) ;  /* 03 Nov 1998 */
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------
   Start a view (12-3, 23-1, or 31-2)
---------------------------------------------------------------------------*/

static char * AFNI_image_help =
 "Button 1: Set crosshair location\n"
 "Button 3: Pop up image menu\n\n"
 "Shift/Ctrl/Alt + Button 3\n"
 "will open up the Disp/Mont/Save\n"
 "control panels, respectively.\n\n"
 "q = close window (quit)\n"
 "z = zoom out  Z = zoom in\n"
 "p = toggle panning" ;

static char * AFNI_arrowpad_help =
   "Click arrows to scroll crosshair position\n"
   "Click button to open/close crosshair gap " ;

static char * AFNI_arrowpad_hint[] = {
  "Scroll crosshairs down" ,
  "Scroll crosshairs up" ,
  "Scroll crosshairs left" ,
  "Scroll crosshairs right" ,
  "Close/open crosshairs gap"
} ;

/*.........................................................................*/

void AFNI_view_xyz_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   MCW_imseq   * sxyz , * syzx , * szxy , ** snew = NULL ;
   MCW_grapher * gxyz , * gyzx , * gzxy , ** gnew = NULL ;
   Widget        pboff , pb_xyz , pb_yzx , pb_zxy ;
   Widget        groff , gr_xyz , gr_yzx , gr_zxy ;
   FD_brick    * brnew ;
   int mirror=0 ;

ENTRY("AFNI_view_xyz_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

    sxyz = im3d->s123 ; gxyz = im3d->g123 ;
    syzx = im3d->s231 ; gyzx = im3d->g231 ;
    szxy = im3d->s312 ; gzxy = im3d->g312 ;

    pb_xyz = im3d->vwid->imag->image_xyz_pb ;
    pb_yzx = im3d->vwid->imag->image_yzx_pb ;
    pb_zxy = im3d->vwid->imag->image_zxy_pb ;

    gr_xyz = im3d->vwid->imag->graph_xyz_pb ;
    gr_yzx = im3d->vwid->imag->graph_yzx_pb ;
    gr_zxy = im3d->vwid->imag->graph_zxy_pb ;

    /* handle case of button press of already
       open window by bringing that window to the top */

    if( w == pb_xyz && sxyz != NULL ){
       if( ISQ_REALZ(sxyz) )
          XMapRaised( XtDisplay(sxyz->wtop) , XtWindow(sxyz->wtop) ) ;
       EXRETURN ;
    } else if( w == pb_yzx && syzx != NULL ){
       if( ISQ_REALZ(syzx) )
          XMapRaised( XtDisplay(syzx->wtop) , XtWindow(syzx->wtop) ) ;
       EXRETURN ;
    } else if( w == pb_zxy && szxy != NULL ){
       if( ISQ_REALZ(szxy) )
          XMapRaised( XtDisplay(szxy->wtop) , XtWindow(szxy->wtop) ) ;
       EXRETURN ;
    } else if( w == gr_xyz && gxyz != NULL ){
       if( GRA_REALZ(gxyz) )
          XMapRaised( XtDisplay(gxyz->fdw_graph) , XtWindow(gxyz->fdw_graph) ) ;
       EXRETURN ;
    } else if( w == gr_yzx && gyzx != NULL ){
       if( GRA_REALZ(gyzx) )
          XMapRaised( XtDisplay(gyzx->fdw_graph) , XtWindow(gyzx->fdw_graph) ) ;
       EXRETURN ;
    } else if( w == gr_zxy && gzxy != NULL ){
       if( GRA_REALZ(gzxy) )
          XMapRaised( XtDisplay(gzxy->fdw_graph) , XtWindow(gzxy->fdw_graph) ) ;
       EXRETURN ;
    }

    /* button pressed and window not open, so prepare to open it */

    if( w == pb_xyz && sxyz == NULL ){
       snew  = &(im3d->s123) ;
       brnew = im3d->b123_ulay ;
       pboff = pb_xyz ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else if( w == pb_yzx && syzx == NULL ){
       snew  = &(im3d->s231) ;
       brnew = im3d->b231_ulay ;
       pboff = pb_yzx ;

    } else if( w == pb_zxy && szxy == NULL ){
       snew  = &(im3d->s312) ;
       brnew = im3d->b312_ulay ;
       pboff = pb_zxy ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else if( w == gr_xyz && gxyz == NULL ){
       gnew  = &(im3d->g123) ;
       brnew = im3d->b123_ulay ;
       pboff = gr_xyz ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else if( w == gr_yzx && gyzx == NULL ){
       gnew  = &(im3d->g231) ;
       brnew = im3d->b231_ulay ;
       pboff = gr_yzx ;

    } else if( w == gr_zxy && gzxy == NULL ){
       gnew  = &(im3d->g312) ;
       brnew = im3d->b312_ulay ;
       pboff = gr_zxy ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else
       EXRETURN ;  /* something funny */

    /** Mar 1997: don't open if x or y dimension is 1 **/

    if( brnew->n1 < 2 || brnew->n2 < 2 ) EXRETURN  ;

    SHOW_AFNI_PAUSE ;

    if( snew != NULL ){
STATUS("opening an image window") ;
      MCW_invert_widget(pboff) ;
      *snew = open_MCW_imseq( im3d->dc, AFNI_brick_to_mri, (XtPointer) brnew ) ;

      (*snew)->parent = (XtPointer) im3d ;

      INIT_BKGD_LAB(im3d) ;

      drive_MCW_imseq( *snew, isqDR_imhelptext, (XtPointer) AFNI_image_help ) ;
      drive_MCW_imseq( *snew, isqDR_arrowpadon, (XtPointer) AFNI_arrowpad_help ) ;
      drive_MCW_imseq( *snew, isqDR_arrowpadhint , (XtPointer) AFNI_arrowpad_hint ) ;
      drive_MCW_imseq( *snew, isqDR_realize, NULL ) ;
      drive_MCW_imseq( *snew, isqDR_title, (XtPointer) im3d->window_title ) ;
      drive_MCW_imseq( *snew, isqDR_periodicmont,
                      (XtPointer)(int) im3d->vinfo->xhairs_periodic );

STATUS("realizing new image viewer") ;

      drive_MCW_imseq( *snew, isqDR_realize, NULL ) ;

      /* 09 Oct 1998: force L-R mirroring on axial and coronal images? */

      if( mirror ){
         ISQ_options opt ;

STATUS("setting image view to be L-R mirrored") ;

         ISQ_DEFAULT_OPT(opt) ;
         opt.mirror = TRUE ;
         drive_MCW_imseq( *snew,isqDR_options  ,(XtPointer) &opt ) ;
      }

#ifdef USE_SIDES
#define LL 0
#define RR 1
#define AA 2
#define PP 3
#define SS 4
#define II 5
      if( !AFNI_yesenv("AFNI_NO_SIDES_LABELS") ){
         static char * ssix[6] = { "Left"     , "Right"     ,
                                   "Anterior" , "Posterior" ,
                                   "Superior" , "Inferior"   } ;
         char * ws[4] ;

         if( *snew == im3d->s123 ){
           ws[0] = ssix[RR]; ws[1] = ssix[AA]; ws[2] = ssix[LL]; ws[3] = ssix[PP];
         } else if( *snew == im3d->s231 ){
           ws[0] = ssix[AA]; ws[1] = ssix[SS]; ws[2] = ssix[PP]; ws[3] = ssix[II];
         } else if( *snew == im3d->s312 ){
           ws[0] = ssix[RR]; ws[1] = ssix[SS]; ws[2] = ssix[LL]; ws[3] = ssix[II];
         } else {
           ws[0] = ws[1] = ws[2] = ws[3] = NULL ;
         }

STATUS("setting image viewer 'sides'") ;

         drive_MCW_imseq( *snew,isqDR_winfosides,(XtPointer)ws ) ;

      }
#undef LL
#undef RR
#undef AA
#undef PP
#undef SS
#undef II
#endif

      AFNI_toggle_drawing( im3d ) ;

#ifndef DONT_INSTALL_ICONS
      if( afni48_good ){
         Pixmap pm = XmUNSPECIFIED_PIXMAP ;

              if( w == pb_xyz ) pm = afni48axi_pixmap ;
         else if( w == pb_yzx ) pm = afni48sag_pixmap ;
         else if( w == pb_zxy ) pm = afni48cor_pixmap ;

         drive_MCW_imseq( *snew,isqDR_icon , (XtPointer) pm ) ;
      }
#endif
    }

    /** Don't forget to send information like the reference timeseries ... **/

    if( gnew != NULL && DSET_GRAPHABLE(brnew->dset) ){
       MCW_grapher * gr ;

STATUS("opening a graph window") ;

       MCW_invert_widget(pboff) ;
       gr = new_MCW_grapher( im3d->dc , AFNI_brick_to_mri , (XtPointer) brnew ) ;
       drive_MCW_grapher( gr, graDR_title, (XtPointer) im3d->window_title );
       drive_MCW_grapher( gr, graDR_addref_ts, (XtPointer) im3d->fimdata->fimref );
       drive_MCW_grapher( gr, graDR_setignore, (XtPointer) im3d->fimdata->init_ignore );
       drive_MCW_grapher( gr, graDR_polort, (XtPointer) im3d->fimdata->polort );
       drive_MCW_grapher( gr, graDR_setindex , (XtPointer) im3d->vinfo->time_index );

       if( im3d->type == AFNI_IMAGES_VIEW )
          drive_MCW_grapher( gr , graDR_fim_disable , NULL ) ; /* 19 Oct 1999 */

       if( mirror )                                            /* 12 Jul 2000 */
          drive_MCW_grapher( gr , graDR_mirror , (XtPointer) 1 ) ;

STATUS("realizing new grapher") ;

       /* 07 Aug 2001: set global baseline level, if possible */

       if( ISVALID_STATISTIC(brnew->dset->stats) ){
         char *eee = getenv( "AFNI_GRAPH_GLOBALBASE" ) ;  /* 08 Mar 2002 */
         if( eee == NULL ){                               /* skip this? */
           float vbot=WAY_BIG ; int ii ;
           for( ii=0 ; ii < brnew->dset->stats->nbstat ; ii++ )
             if( ISVALID_BSTAT(brnew->dset->stats->bstat[ii]) )
               vbot = MIN( vbot , brnew->dset->stats->bstat[ii].min ) ;

           if( vbot < WAY_BIG )
             drive_MCW_grapher( gr, graDR_setglobalbaseline, (XtPointer)&vbot );
         }
       }

       drive_MCW_grapher( gr , graDR_realize , NULL ) ;

       *gnew = gr ;
       (*gnew)->parent = (XtPointer) im3d ;

#ifndef DONT_INSTALL_ICONS
      if( afni48_good ){
         Pixmap pm = XmUNSPECIFIED_PIXMAP ;

              if( w == gr_xyz ) pm = afni48graaxi_pixmap ;
         else if( w == gr_yzx ) pm = afni48grasag_pixmap ;
         else if( w == gr_zxy ) pm = afni48gracor_pixmap ;

         drive_MCW_grapher( gr , graDR_icon , (XtPointer) pm ) ;
      }
#endif
    }

   /*-- force a jump to the viewpoint of the current location --*/

   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;

   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;

   MPROBE ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   The following routines for "locks" were added 04 Nov 1996
-------------------------------------------------------------------------*/

void AFNI_lock_enforce_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_lock_enforce_CB") ;
   AFNI_lock_carryout( im3d ) ;
   AFNI_time_lock_carryout( im3d ) ;  /* 03 Nov 1998 */
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

void AFNI_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Three_D_View * qq3d ;
   int            bval , ii , bold ;

ENTRY("AFNI_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.controller_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.controller_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

      MCW_set_bbox( qq3d->vwid->dmode->lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

void AFNI_lock_clear_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * qq3d ;
   int ii ;

ENTRY("AFNI_lock_clear_CB") ;

   GLOBAL_library.controller_lock = 0 ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_VALID(qq3d) )
         MCW_set_bbox( qq3d->vwid->dmode->lock_bbox , 0 ) ;
   }
   EXRETURN ;
}

void AFNI_lock_setall_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * qq3d ;
   int ii ;

ENTRY("AFNI_lock_setall_CB") ;

   GLOBAL_library.controller_lock = 0 ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
      GLOBAL_library.controller_lock |= (1<<ii) ;

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_VALID(qq3d) )
         MCW_set_bbox( qq3d->vwid->dmode->lock_bbox ,
                       GLOBAL_library.controller_lock ) ;
   }
   EXRETURN ;
}

void AFNI_lock_carryout( Three_D_View * im3d )
{
   Three_D_View * qq3d ;
   int ii,jj,kk , cc , glock ;
   THD_fvec3 old_fv , fv ;
   THD_ivec3 iv ;
   THD_dataxes * qaxes , * daxes ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */

ENTRY("AFNI_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;

   if( busy )                       EXRETURN ;  /* routine already busy */
   if( glock == 0 )                 EXRETURN ;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )           EXRETURN ;  /* bad input */
   if( GLOBAL_library.ignore_lock ) EXRETURN ;  /* ordered not to do anything */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* load Dicom location of current point of view in this controller */

   LOAD_FVEC3( old_fv , im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk ) ;

   LOAD_ANAT_VIEW(im3d) ;  /* prepare coordinates */
   daxes = CURRENT_DAXES(im3d->anat_now) ;

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current one,
        and ARE locked, transform the above vector to the
        controller's dataset, and then jump to that point */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      qq3d = GLOBAL_library.controllers[cc] ; /* controller */

      if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

         LOAD_ANAT_VIEW(qq3d) ;  /* prepare coordinates */
         qaxes = CURRENT_DAXES(qq3d->anat_now) ;

         if( !GLOBAL_library.ijk_lock ){  /* xyz coord lock */

            fv = AFNI_transform_vector( im3d->anat_now, old_fv, qq3d->anat_now ) ;
            fv = THD_dicomm_to_3dmm( qq3d->anat_now , fv ) ;
            iv = THD_3dmm_to_3dind ( qq3d->anat_now , fv ) ;
            ii = iv.ijk[0] ; jj = iv.ijk[1] ; kk = iv.ijk[2] ;

         } else {   /* 11 Sep 2000: ijk index lock */

            ii = im3d->vinfo->i1 * qaxes->nxx / daxes->nxx ;
            jj = im3d->vinfo->j2 * qaxes->nyy / daxes->nyy ;
            kk = im3d->vinfo->k3 * qaxes->nzz / daxes->nzz ;
         }

         /* if have good new ijk coords, jump to them */

         if( ii >= 0 && ii < qaxes->nxx &&
             jj >= 0 && jj < qaxes->nyy && kk >= 0 && kk < qaxes->nzz   ){

            SAVE_VPT(qq3d) ;
            AFNI_set_viewpoint( qq3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
         }
      }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*-----------------------------------------------------------------------
    11 Sep 2000: allow locking using ijk instead of xyz
-------------------------------------------------------------------------*/

void AFNI_ijk_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Three_D_View * qq3d ;
   int            bval , ii , bold ;

ENTRY("AFNI_ijk_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.ijk_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->ijk_lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.ijk_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

      MCW_set_bbox( qq3d->vwid->dmode->ijk_lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_redisplay_func( Three_D_View * im3d )  /* 05 Mar 2002 */
{
   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   AFNI_process_funcdisplay( im3d ) ;
}

/*------------------------------------------------------------------------*/

void AFNI_do_bkgd_lab( Three_D_View * im3d )
{
   char str[256] ;

   if( !IM3D_OPEN(im3d) || !im3d->vwid->imag->do_bkgd_lab ) return ;

#define VSTR(x) ( ((x)[0] == '\0') ? ("?") : (x) )

   sprintf(str,"Anat = %s\n"
               "Func = %s\n"
               "Thr  = %s" ,
           VSTR(im3d->vinfo->anat_val),
           VSTR(im3d->vinfo->func_val),
           VSTR(im3d->vinfo->thr_val ) ) ;

#undef VSTR

   MCW_set_widget_label( im3d->vwid->func->bkgd_lab , str ) ;
   XtManageChild( im3d->vwid->func->bkgd_lab ) ;
   FIX_SCALE_SIZE(im3d) ;
}

/*------------------------------------------------------------------------*/

void AFNI_set_viewpoint( Three_D_View * im3d ,
                         int xx,int yy,int zz , int redisplay_option )
{
   int old_i1 , old_j2 , old_k3 , i1,j2,k3 ;
   int dim1,dim2,dim3 , isq_driver , do_lock , new_xyz ;
   int newti ; /* 24 Jan 2001 */

   THD_dataxes * daxes ;
   THD_fvec3 fv ;
   THD_ivec3 old_ib , new_ib , old_id , new_id ;

ENTRY("AFNI_set_viewpoint") ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"input xx=%d yy=%d zz=%d",xx,yy,zz) ;
  STATUS(str) ; }

   if( ! IM3D_OPEN(im3d) || ! ISVALID_3DIM_DATASET(im3d->anat_now) ) EXRETURN ;

   /** 02 Nov 1996:
         Attach view-specific dataxes and warps to the datasets **/

   LOAD_DSET_VIEWS(im3d) ;

   /** find if input points are inside axes of current display **/

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   dim1  = daxes->nxx ; dim2 = daxes->nyy ; dim3 = daxes->nzz ;

   old_i1 = im3d->vinfo->i1 ;
   old_j2 = im3d->vinfo->j2 ;
   old_k3 = im3d->vinfo->k3 ;

   i1 = im3d->vinfo->i1 = (xx < 0 || xx >= dim1) ? (old_i1) : xx ;
   j2 = im3d->vinfo->j2 = (yy < 0 || yy >= dim2) ? (old_j2) : yy ;
   k3 = im3d->vinfo->k3 = (zz < 0 || zz >= dim3) ? (old_k3) : zz ;

   /** determine redisplay mode for image viewers **/

   new_xyz =
    do_lock = !( i1 == old_i1 && j2 == old_j2 && k3 == old_k3 ) ;  /* 11 Nov 1996 */

   if( !redisplay_option && !new_xyz ) EXRETURN ;

   isq_driver = (redisplay_option == REDISPLAY_ALL) ? isqDR_display
                                                    : isqDR_overlay ;

   if( AFNI_yesenv("AFNI_VALUE_LABEL") && new_xyz &&
       (im3d->s123 == NULL || im3d->s231 == NULL || im3d->s312 == NULL) )
     isq_driver = isqDR_display ;         /* 08 Mar 2002 */

   LOAD_IVEC3(old_id,old_i1,old_j2,old_k3) ;
   LOAD_IVEC3(new_id,    i1,    j2,    k3) ;

#ifdef AFNI_DEBUG
STATUS(" ") ;
DUMP_IVEC3("  old_id",old_id) ;
DUMP_IVEC3("  new_id",new_id) ;
#endif

   if( im3d->type == AFNI_3DDATA_VIEW ){
      fv = THD_3dind_to_3dmm( im3d->anat_now , new_id ) ;
      fv = THD_3dmm_to_dicomm( im3d->anat_now , fv ) ;
      im3d->vinfo->xi = fv.xyz[0] ;  /* set display coords */
      im3d->vinfo->yj = fv.xyz[1] ;  /* to Dicom standard  */
      im3d->vinfo->zk = fv.xyz[2] ;
   }

   /* clear labels */

   im3d->vinfo->func_val[0] = im3d->vinfo->thr_val[0] = '\0' ;
   if( do_lock || isq_driver==isqDR_display )
      im3d->vinfo->anat_val[0] = '\0';
   if( AFNI_yesenv( "AFNI_VALUE_LABEL") ) AFNI_do_bkgd_lab( im3d ) ;

   /*--- redraw images now ---*/

   im3d->ignore_seq_callbacks = AFNI_IGNORE_EVERYTHING ;

   if( im3d->s123 != NULL || im3d->g123 != NULL ){
      int xyzm[4] ;

      old_ib = THD_3dind_to_fdind( im3d->b123_ulay , old_id ) ;
      new_ib = THD_3dind_to_fdind( im3d->b123_ulay , new_id ) ;

#ifdef AFNI_DEBUG
STATUS(" ") ;
DUMP_IVEC3(" redraw s123 old_ib",old_ib) ;
DUMP_IVEC3("             new_ib",new_ib) ;
#endif

      if( redisplay_option || old_ib.ijk[2] != new_ib.ijk[2] )
         drive_MCW_imseq( im3d->s123 ,
                          isq_driver , (XtPointer) new_ib.ijk[2] ) ;


      xyzm[0] = new_ib.ijk[0] ; xyzm[1] = new_ib.ijk[1] ;
      xyzm[2] = new_ib.ijk[2] ; xyzm[3] = 0 ;

      if( im3d->g123 != NULL && ( im3d->g123->never_drawn ||
                                  redisplay_option == REDISPLAY_ALL || new_xyz ) )
         drive_MCW_grapher( im3d->g123 , graDR_redraw , (XtPointer) xyzm ) ;
   }

   if( im3d->s231 != NULL || im3d->g231 != NULL ){
      int xyzm[4] ;

      old_ib = THD_3dind_to_fdind( im3d->b231_ulay , old_id ) ;
      new_ib = THD_3dind_to_fdind( im3d->b231_ulay , new_id ) ;

#ifdef AFNI_DEBUG
STATUS(" ") ;
DUMP_IVEC3(" redraw s231 old_ib",old_ib) ;
DUMP_IVEC3("             new_ib",new_ib) ;
#endif

      if( redisplay_option || old_ib.ijk[2] != new_ib.ijk[2] )
         drive_MCW_imseq( im3d->s231 ,
                          isq_driver , (XtPointer) new_ib.ijk[2] ) ;

      xyzm[0] = new_ib.ijk[0] ; xyzm[1] = new_ib.ijk[1] ;
      xyzm[2] = new_ib.ijk[2] ; xyzm[3] = 0 ;

      if( im3d->g231 != NULL && ( im3d->g231->never_drawn ||
                                  redisplay_option == REDISPLAY_ALL || new_xyz ) )
         drive_MCW_grapher( im3d->g231 , graDR_redraw , (XtPointer) xyzm ) ;
   }

   if( im3d->s312 != NULL || im3d->g312 != NULL ){
      int xyzm[4] ;

      old_ib = THD_3dind_to_fdind( im3d->b312_ulay , old_id ) ;
      new_ib = THD_3dind_to_fdind( im3d->b312_ulay , new_id ) ;

#ifdef AFNI_DEBUG
STATUS(" ") ;
DUMP_IVEC3(" redraw s312 old_ib",old_ib) ;
DUMP_IVEC3("             new_ib",new_ib) ;
#endif

      if( redisplay_option || old_ib.ijk[2] != new_ib.ijk[2] )
         drive_MCW_imseq( im3d->s312 ,
                          isq_driver , (XtPointer) new_ib.ijk[2] ) ;

      xyzm[0] = new_ib.ijk[0] ; xyzm[1] = new_ib.ijk[1] ;
      xyzm[2] = new_ib.ijk[2] ; xyzm[3] = 0 ;

      if( im3d->g312 != NULL && ( im3d->g312->never_drawn ||
                                  redisplay_option == REDISPLAY_ALL || new_xyz ) )
         drive_MCW_grapher( im3d->g312 , graDR_redraw , (XtPointer) xyzm ) ;
   }

   im3d->ignore_seq_callbacks = AFNI_IGNORE_NOTHING ;

   /*--- redraw coordinate display now ---*/

   if( redisplay_option || new_xyz ){
      XmString xstr ;
      Boolean same ;

      xstr = AFNI_crosshair_label( im3d ) ;
      same = XmStringCompare( xstr , im3d->vinfo->old_crosshair_label ) ;

      if( same == False ){
         XtVaSetValues( im3d->vwid->imag->crosshair_label ,       /* redisplay */
                           XmNlabelString , xstr ,                /* if changed */
                        NULL ) ;
         MCW_expose_widget( im3d->vwid->imag->crosshair_label ) ; /* redraw now! */
         XmStringFree(im3d->vinfo->old_crosshair_label) ;         /* toss old */
         im3d->vinfo->old_crosshair_label = xstr ;                /* new old */
      } else {
         XmStringFree( xstr ) ;  /* was same --> don't need this copy */
      }

      AFNI_do_bkgd_lab( im3d ) ;  /* 08 Mar 2002: moved labelizing to function */
   }

   /* 24 Jan 2001: set grapher index based on type of dataset */

   if( DSET_NUM_TIMES(im3d->anat_now) > 1 ){
      newti = im3d->vinfo->time_index ;
   } else if( ISANATBUCKET(im3d->anat_now) ){
      newti = im3d->vinfo->anat_index ;
   } else {
      newti = -1 ;
   }

   if( newti >= 0 ){
      drive_MCW_grapher( im3d->g123, graDR_setindex, (XtPointer)newti );
      drive_MCW_grapher( im3d->g231, graDR_setindex, (XtPointer)newti );
      drive_MCW_grapher( im3d->g312, graDR_setindex, (XtPointer)newti );
   }

   if( do_lock )                    /* 11 Nov 1996 */
      AFNI_lock_carryout( im3d ) ;  /* 04 Nov 1996 */

   /** Feb 1998: if desired, send coordinates to receiver **/
   /** Mar 1999: do it in an external routine, not here.  **/

   if( new_xyz ) AFNI_process_viewpoint( im3d ) ;
   else          AFNI_process_redisplay( im3d ) ;

   if( new_xyz && im3d->vwid->imag->pop_whereami_twin != NULL ){

      char * tlab = AFNI_ttatlas_query( im3d ) ;

      if( tlab == NULL ){
         MCW_textwin_alter( im3d->vwid->imag->pop_whereami_twin ,
                           "\n*** Can't compute Talairach coordinates now ***\n");
      } else {
         MCW_textwin_alter( im3d->vwid->imag->pop_whereami_twin , tlab ) ;
         free(tlab) ;
      }
   }

   if( new_xyz                         &&
       SUMA_ENABLED                    &&
       im3d->anat_now->su_surf != NULL &&
       im3d->anat_now->su_vmap != NULL &&
      !im3d->anat_wod_flag               ){

      int pp = im3d->anat_now->su_vmap[ i1 + j2*dim1 + k3*dim1*dim2 ] ;

      if( pp >= 0 ){
        int ll = SUMA_VMAP_LEVEL(pp) ;
        pp = SUMA_VMAP_UNMASK(pp) ;
        pp = im3d->anat_now->su_surf->ixyz[pp].id ;

        fprintf(stderr,"surface node ID = %d (level %d)\n" , pp,ll ) ;
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------------
   get the n-th overlay as an MRI_IMAGE *
   (return NULL if none;  note that the result must be mri_freed by the user)
---------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_overlay( int n , FD_brick * br )
{
   Three_D_View * im3d = (Three_D_View *) br->parent ;
   MRI_IMAGE * im = NULL , * fov = NULL ;
   register short * oar ;
   int ii,jj , npix , xx,yy,zz , nx,ny , gap,ovc , icr,jcr,kcr ;
   Boolean ovgood ;
   THD_ivec3 ib ;
   THD_3dim_dataset * dset ;
   FD_brick * br_fim ;
   int do_xhar ;         /* 22 Mar 2002 */

ENTRY("AFNI_overlay") ;

   if( ! IM3D_VALID(im3d) ) RETURN(NULL) ;

   /*--- check if crosshairs, markers, or functions are visible ---*/

   do_xhar = (im3d->vinfo->crosshair_visible && !AFNI_yesenv("AFNI_CROSSHAIR_LINES")) ;

   dset = im3d->anat_now ;

   ovgood =  do_xhar                                                  ||

            (  dset->markers != NULL       &&
              (dset->markers->numset > 0)  &&
              (im3d->vwid->marks->ov_visible == True) )               ||

            (  dset->tagset != NULL  &&
               dset->tagset->num > 0 &&
               (im3d->vwid->marks->tag_visible == True) )             ||

            ( dset->pts != NULL && im3d->vinfo->pts_visible == True ) ||

            ( im3d->vinfo->func_visible == True )                     ||

            ( im3d->vinfo->see_ttatlas &&
              im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE ) ;

   if( ! ovgood ) RETURN(NULL) ;

   /*-- at least one source of an overlay is present --*/

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"n1=%d n2=%d",br->n1,br->n2) ; STATUS(str) ; }

   LOAD_DSET_VIEWS(im3d) ;  /* 02 Nov 1996 */

   /*----- get functional overlay, if desired -----*/

   if( im3d->vinfo->func_visible ){
      br_fim = UNDERLAY_TO_OVERLAY(im3d,br) ;
      fov    = AFNI_func_overlay( n , br_fim ) ;
   }

   /*----- 25 Jul 2001: get TT atlas overlay, if desired and possible -----*/

   if( im3d->vinfo->see_ttatlas &&
       im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE ){

      MRI_IMAGE * tov ;

      int ax_1 = br->a123.ijk[0] ;
      int ax_2 = br->a123.ijk[1] ;
      int ax_3 = br->a123.ijk[2] ;

      tov = AFNI_ttatlas_overlay( im3d , n , ax_1 , ax_2 , ax_3 , fov ) ;
      if( tov != NULL && tov != fov ){
         if( fov != NULL ) mri_free(fov) ;  /* should not happen */
         fov = tov ;
      }
   }

   /*----- now set up overlay image as the functional overlay
           (if present), or as a new blank image (otherwise). -----*/

   if( fov != NULL ){

if(PRINT_TRACING)
{ char str[256] ;
sprintf(str,"new overlay from AFNI_func_overlay: nx=%d ny=%d\n",fov->nx,fov->ny) ;
STATUS(str) ; }

      im  = fov ; ovgood = True ;
      oar = MRI_SHORT_PTR(im) ;
   } else {
STATUS("new overlay is created de novo") ;
      im  = mri_new( br->n1 , br->n2 , MRI_short ) ; ovgood = False ;
      oar = MRI_SHORT_PTR(im) ;
#ifdef DONT_USE_MEMCPY
      for( ii=0 ; ii < im->nvox ; ii++ ) oar[ii] = 0 ;  /* blank overlay */
#else
      (void) memset( oar , 0 , sizeof(short)*im->nvox ) ;
#endif
   }

   nx     = im->nx ;
   ny     = im->ny ;
   npix   = nx * ny ;
   im->dx = br->del1 ;  /* load dimensions (not that anyone cares) */
   im->dy = br->del2 ;
   im->dz = br->del3 ;

   /*----- put crosshairs on, if desired -----*/

   if( do_xhar ){
      MCW_grapher * grapher = UNDERLAY_TO_GRAPHER(im3d,br) ;

      ib = THD_3dind_to_fdind( br ,
                              TEMP_IVEC3( im3d->vinfo->i1 ,
                                          im3d->vinfo->j2 ,
                                          im3d->vinfo->k3  ) ) ;

      /** April 1996: Only put crosshairs on if image number
                      matches current slice number of viewpoint.
                      (This allows for the montage multislice view) **/

      /** July 1996: Allow for multiple crosshairs to indicate
                     the location of montage multislice views. **/

      /** Aug 1996: Allow for periodic (wrap) or non-periodic montages.
                    Also, if in "Single" mode and also are graphing,
                    then only draw the grapher frame, not the crosshairs. **/

      /** Dec 1998: Allow for user to turn off some directions of crosshairs **/

      if( n == ib.ijk[2] || im3d->vinfo->xhairs_all ){
         int jp,ip , jcen,icen , gappp ;
         int idown,iup,iskip , jdown,jup,jskip , imon,jmon ;
         int a1 = br->a123.ijk[0] ,   /* x axis of the brick?    */
             ax = abs(a1) - 1       ; /* 0,1,2 for dataset x,y,z */
         int a2 = br->a123.ijk[1] ,   /* y axis of the brick?    */
             ay = abs(a2) - 1       ; /* 0,1,2 for dataset x,y,z */
         int a3 = br->a123.ijk[2] ,   /* z axis of the brick?    */
             az = abs(a3) - 1       ; /* 0,1,2 for dataset x,y,z */

         /* 31 Dec 1998: spatial orientations of image axes */

         int ox = (ax==0) ? br->dset->daxes->xxorient :
                  (ax==1) ? br->dset->daxes->yyorient : br->dset->daxes->zzorient ;

         int oy = (ay==0) ? br->dset->daxes->xxorient :
                  (ay==1) ? br->dset->daxes->yyorient : br->dset->daxes->zzorient ;

         ovc  = im3d->vinfo->crosshair_ovcolor ;
         gap  = (grapher==NULL) ? im3d->vinfo->crosshair_gap : (grapher->mat+1)/2 ;
         icen = ib.ijk[0] ;
         jcen = ib.ijk[1] ;

         /** initialize montage steps **/

         if( im3d->vinfo->xhairs_show_montage ){           /* in "Multi" mode */
            iskip = im3d->vinfo->xhairs_nskip.ijk[ax] + 1 ;
            jskip = im3d->vinfo->xhairs_nskip.ijk[ay] + 1 ;
            if( a1 > 0 ){
               idown = im3d->vinfo->xhairs_ndown.ijk[ax] ;
               iup   = im3d->vinfo->xhairs_nup.ijk[ax] ;
            } else {
               iup   = im3d->vinfo->xhairs_ndown.ijk[ax] ;
               idown = im3d->vinfo->xhairs_nup.ijk[ax] ;
            }
            if( a2 > 0 ){
               jdown = im3d->vinfo->xhairs_ndown.ijk[ay] ;
               jup   = im3d->vinfo->xhairs_nup.ijk[ay] ;
            } else {
               jup   = im3d->vinfo->xhairs_ndown.ijk[ay] ;
               jdown = im3d->vinfo->xhairs_nup.ijk[ay] ;
            }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"montage xhairs: ax   =%d ay   =%d az =%d",ax,ay,az)       ; STATUS(str);
  sprintf(str,"                iskip=%d idown=%d iup=%d",iskip,idown,iup); STATUS(str);
  sprintf(str,"                jskip=%d jdown=%d jup=%d",jskip,jdown,jup); STATUS(str);
  sprintf(str,"orimask=%d ox=%d oy=%d",im3d->vinfo->xhairs_orimask,ox,oy); STATUS(str);
}

         } else {                                          /* in "Single" Mode */
           idown = iup = jdown = jup = iskip = jskip = 0 ;
           if( grapher != NULL ){ idown=-(iup+1); jdown=-(jup+1); } /* skip lines? */
         }

         /* draw vertical lines first */

         if( (im3d->vinfo->xhairs_orimask & (1<<oy)) != 0 ){  /* 31 Dec 1998 */
            for( imon=-idown ; imon <= iup ; imon++ ){
               icr = icen + imon * iskip ;

               if( im3d->vinfo->xhairs_periodic ){
                  while( icr < 0 )   icr += nx ;
                  while( icr >= nx ) icr -= nx ;
               } else {
                  if( icr < 0 || icr >= nx ) continue ;
               }

               gappp = (abs(icr-icen) <= gap) ? gap : -1 ; /* no gap if far from center */

               /* if lines are closely packed, only do alternate pixels */

               if( idown+iup > 0 && iskip == 1 && icr != icen ){
                  for( jj=(imon+idown)%2 ; jj < ny ; jj+=2 )
                     if( abs(jj-jcen) > gappp ) oar[icr+nx*jj] = ovc ;
               } else {
                  for( jj=0 ; jj < ny ; jj++ )
                     if( abs(jj-jcen) > gappp ) oar[icr+nx*jj] = ovc ;
               }
            }
         }

         /* draw horizontal lines */

         if( (im3d->vinfo->xhairs_orimask & (1<<ox)) != 0 ){  /* 31 Dec 1998 */
            for( jmon=-jdown ; jmon <= jup ; jmon++ ){
               jcr = jcen + jmon * jskip ;
               if( im3d->vinfo->xhairs_periodic ){
                  while( jcr < 0 )   jcr += ny ;
                  while( jcr >= ny ) jcr -= ny ;
               } else {
                  if( jcr < 0 || jcr >= ny ) continue ;
               }

               gappp = (abs(jcr-jcen) <= gap) ? gap : -1 ;  /* no gap if far from center */

               /* if lines are closely packed, only do alternate pixels */

               if( jdown+jup > 0 && jskip == 1 && jcr != jcen ){
                  for( ii=(jmon+jdown)%2 ; ii < nx ; ii+=2 )
                     if( abs(ii-icen) > gappp ) oar[ii+nx*jcr] = ovc ;
               } else {
                  for( ii=0 ; ii < nx ; ii++ )
                     if( abs(ii-icen) > gappp ) oar[ii+nx*jcr] = ovc ;
               }
            }
         }

         /* draw grapher frame, if needed */

         if( grapher != NULL ){
            int gs = gap , gb = (grapher->mat +2)/2 ;

            jcr = jcen ; icr = icen ;

            ip = icr - gb ; if( ip <  0  ) ip += nx ;
            ii = icr + gs ; if( ii >= nx ) ii -= nx ;
            for( jj=jcr-gb ; jj <= jcr+gs ; jj++ ){
               jp = jj ; if( jp <  0  ) jp += ny ;
                    else if( jp >= ny ) jp -= ny ;
               oar[ip+nx*jp] = ovc ;
               oar[ii+nx*jp] = ovc ;
            }

            jp = jcr - gb ; if( jp <  0  ) jp += ny ;
            jj = jcr + gs ; if( jj >= ny ) jj -= ny ;
            for( ii=icr-gb ; ii <= icr+gs ; ii++ ){
              ip = ii ; if( ip <  0  ) ip += nx ;
                   else if( ip >= nx ) ip -= nx ;
              oar[ip+nx*jp] = ovc ;
              oar[ip+nx*jj] = ovc ;
            }
         } /* end if "if grapher exists" */

         ovgood = True ;
      } /* end of "if correct slice" */

   } /* end of crosshairs */

   /*----- put markers on, if desired -----*/

   if( im3d->anat_now->markers != NULL &&
       im3d->anat_now->markers->numset > 0 &&
       (im3d->vwid->marks->ov_visible == True) ){

      THD_marker_set     * markers = im3d->anat_now->markers ;
      AFNI_marks_widgets * marks   = im3d->vwid->marks ;
      AFNI_ovtemplate    * tem     = &(marks->ov_mask) ;
      int xbase , ybase , zbase , color ;
      THD_ivec3 ib ;

      /* do secondary points first */

      color = marks->ov_scolor ;

      for( jj=0 ; jj < MARKS_MAXNUM ; jj++ ){
         if( markers->valid[jj] &&     /* is point set? */
             color > 0          &&     /* will show up? */
             !marks->isprimary[jj] ){  /* is secondary? */

            ib = THD_3dmm_to_3dind( br->dset ,
                                    TEMP_FVEC3( markers->xyz[jj][0] ,
                                                markers->xyz[jj][1] ,
                                                markers->xyz[jj][2]  ) ) ;
            ib = THD_3dind_to_fdind( br , ib ) ;

            xbase = ib.ijk[0] ;  /* coordinates */
            ybase = ib.ijk[1] ;  /* in and out */
            zbase = ib.ijk[2] ;  /* of plane  */

            if( zbase == n ){  /* in this display plane */
               ovgood = True ;
               for( ii=0 ; ii < tem->numpix ; ii++ ){
                  xx = xbase + tem->dx[ii] ;
                  yy = ybase + tem->dy[ii] ;
                  if( xx >= 0 && xx < nx && yy >=0 && yy < ny )
                                              oar[xx+nx*yy] = color ;
               }
            }
         } /* end if point set, and secondary */
      } /* end for loop over all secondary points */

      /* duplicate above for primary points */

      color = marks->ov_pcolor ;

      for( jj=0 ; jj < MARKS_MAXNUM ; jj++ ){
         if( markers->valid[jj] &&     /* is point set? */
             color > 0          &&     /* will show up? */
             marks->isprimary[jj]  ){  /* is primary? */

            ib = THD_3dmm_to_3dind( br->dset ,
                                    TEMP_FVEC3( markers->xyz[jj][0] ,
                                                markers->xyz[jj][1] ,
                                                markers->xyz[jj][2]  ) ) ;
            ib = THD_3dind_to_fdind( br , ib ) ;

            xbase = ib.ijk[0] ;  /* coordinates */
            ybase = ib.ijk[1] ;  /* in and out */
            zbase = ib.ijk[2] ;  /* of plane  */

            if( zbase == n ){  /* in this display plane */
               ovgood = True ;
               for( ii=0 ; ii < tem->numpix ; ii++ ){
                  xx = xbase + tem->dx[ii] ;
                  yy = ybase + tem->dy[ii] ;
                  if( xx >= 0 && xx < nx && yy >=0 && yy < ny )
                                              oar[xx+nx*yy] = color ;
               }
            }
         } /* end if point set, and primary */
      } /* end for loop over all secondary points */

   } /* end if markers to be shown */

   /*----- put tags on, if desired -----*/

   if( im3d->anat_now->tagset != NULL  &&
       im3d->anat_now->tagset->num > 0 &&
       (im3d->vwid->marks->tag_visible == True) ){

      static AFNI_ovtemplate * tem = NULL ;
      static int             npold = -1 ;

      THD_usertaglist * tl = im3d->anat_now->tagset ;
      int xbase , ybase , zbase , color , np ;
      THD_ivec3 ib ;
      THD_fvec3 fb ;

      if( tem == NULL ) tem = myXtNew(AFNI_ovtemplate) ; /* once only */
      np = MAX(nx,ny)/64 ; np = MAX(np,2) ;
      if( np != npold ){ npold = np ; AFNI_make_tagmask(np,0,tem) ; }

      color = im3d->vwid->marks->ov_pcolor ;  /* doesn't have its own color */

      for( jj=0 ; jj < tl->num ; jj++ ){
         if( tl->tag[jj].set && color > 0 ){

            fb = THD_dicomm_to_3dmm( br->dset, TEMP_FVEC3( tl->tag[jj].x ,
                                                           tl->tag[jj].y ,
                                                           tl->tag[jj].z  ) );
            ib = THD_3dmm_to_3dind( br->dset , fb ) ;
            ib = THD_3dind_to_fdind( br , ib ) ;

            xbase = ib.ijk[0] ;  /* coordinates */
            ybase = ib.ijk[1] ;  /* in and out */
            zbase = ib.ijk[2] ;  /* of plane  */

            if( zbase == n ){  /* in this display plane */
               ovgood = True ;
               for( ii=0 ; ii < tem->numpix ; ii++ ){
                  xx = xbase + tem->dx[ii] ;
                  yy = ybase + tem->dy[ii] ;
                  if( xx >= 0 && xx < nx && yy >=0 && yy < ny )
                                              oar[xx+nx*yy] = color ;
               }
            }
         }
      }
   } /* end if tags to be shown */

   /*----- May 1995: additional points (single pixels) -----*/

   if( im3d->vinfo->pts_visible   &&
       dset->pts != NULL          &&
       im3d->vinfo->pts_color > 0   ){

      int color , jj ;
      THD_ivec3 ib ;

      color = im3d->vinfo->pts_color ;

      for( jj=0 ; jj < dset->pts->num ; jj++ ){
         ib = THD_3dind_to_fdind( br , dset->pts->ijk[jj] ) ;
         if( ib.ijk[2] == n ){
            oar[ ib.ijk[0] + nx * ib.ijk[1] ] = color ;
            ovgood = True ;
         }
      }
   }

   /*----- return overlay (kill it if nothing happened) -----*/

   if( !ovgood ) KILL_1MRI(im) ;

   RETURN( im ) ;
}

/*------------------------------------------------------------------------*/

XmString AFNI_crosshair_label( Three_D_View * im3d )
{
   char buf[64] ;
   XmString xstr ;
   static char * RR="[R]" , * LL="[L]" ,
               * PP="[P]" , * AA="[A]" ,
               * SS="[S]" , * II="[I]" , * ZZ="   " ;
   char * xx , * yy , * zz ;
   float xval,yval,zval ;

ENTRY("AFNI_crosshair_label") ;

   if( ! IM3D_VALID(im3d) ) RETURN( NULL );

   if( ! IM3D_OPEN(im3d) ){
      sprintf(buf,"1234567890123456789\n1234567890123456789\n1234567890123456789") ;

   } else if( im3d->type == AFNI_IMAGES_VIEW || im3d->vinfo->show_voxind ){

STATUS("voxel indexes") ;

      if( ISVALID_3DIM_DATASET(im3d->fim_now) &&
          im3d->vinfo->func_visible && DSET_INMEMORY(im3d->fim_now) ){
         THD_fvec3 fv ;
         THD_ivec3 iv ;
         int flag ;

         flag = im3d->fim_now->wod_flag ;
         im3d->fim_now->wod_flag = False ;

         fv = THD_dicomm_to_3dmm( im3d->fim_now ,
                                  TEMP_FVEC3(im3d->vinfo->xi,im3d->vinfo->yj,im3d->vinfo->zk) ) ;
         iv = THD_3dmm_to_3dind( im3d->fim_now , fv ) ;

         im3d->fim_now->wod_flag = flag ;

         sprintf( buf , "x: an=%4d fun=%4d\ny: an=%4d fun=%4d\nz: an=%4d fun=%4d" ,
                  im3d->vinfo->i1,iv.ijk[0] ,
                  im3d->vinfo->j2,iv.ijk[1] ,
                  im3d->vinfo->k3,iv.ijk[2]  ) ;
      } else {
         sprintf( buf , "voxel x = %4d\nvoxel y = %4d\nvoxel z = %4d" ,
                  im3d->vinfo->i1 , im3d->vinfo->j2 , im3d->vinfo->k3  ) ;
      }
   } else {
      char bxyz[3][32] ;

STATUS("voxel coordinates") ;

      xval = im3d->vinfo->xi ;
      yval = im3d->vinfo->yj ;
      zval = im3d->vinfo->zk ;

      xx = (xval==0.0) ? (ZZ) : ( (xval<0.0) ? (RR) : (LL) ) ;
      yy = (yval==0.0) ? (ZZ) : ( (yval<0.0) ? (AA) : (PP) ) ;
      zz = (zval==0.0) ? (ZZ) : ( (zval<0.0) ? (II) : (SS) ) ;

      /** 16 July 1997 **/
#if 1
      sprintf( bxyz[0] , "=%9.3f mm %s" ,
               GLOBAL_library.cord.xxsign * xval , xx ) ;

      sprintf( bxyz[1] , "=%9.3f mm %s" ,
               GLOBAL_library.cord.yysign * yval , yy ) ;

      sprintf( bxyz[2] , "=%9.3f mm %s" ,
               GLOBAL_library.cord.zzsign * zval , zz ) ;

      sprintf( buf , "x %17s\ny %17s\nz %17s"   ,
               bxyz[GLOBAL_library.cord.first]  ,
               bxyz[GLOBAL_library.cord.second] ,
               bxyz[GLOBAL_library.cord.third]   ) ;
#else
      sprintf( buf , "x =%9.3f mm %s\ny =%9.3f mm %s\nz =%9.3f mm %s" ,
               xval,xx , yval,yy , zval,zz ) ;
#endif
   }

   xstr = XmStringCreateLtoR( buf , XmFONTLIST_DEFAULT_TAG ) ;

   RETURN( xstr ) ;
}

/*-------------------------------------------------------------------------
   handle the selection of a marker name by the user
   (using the toggle buttons from the control panel or the popup menu)
---------------------------------------------------------------------------*/

void AFNI_marktog_CB( Widget w ,
                      XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   XmToggleButtonCallbackStruct * cbs =
         (XmToggleButtonCallbackStruct *) call_data ;

   int bval , ip , xx=-1 , yy=-1 , zz=-1 ;
   Widget * other_tog ;

ENTRY("AFNI_marktog_CB") ;

   if( ! IM3D_VALID(im3d) || im3d->anat_now->markers == NULL ) EXRETURN ;

   switch( cbs->reason ){

      default:  XBell(XtDisplay(w),100) ; EXRETURN ;  /* error */

      case XmCR_DISARM:   /* button on the control panel */
         bval      = AFNI_first_tog( MARKS_MAXNUM ,
                                     im3d->vwid->marks->tog ) ;
         other_tog = im3d->vwid->marks->poptog ;
      break ;

      case XmCR_VALUE_CHANGED:  /* button on the menu panel */
         bval = AFNI_first_tog( MARKS_MAXNUM ,
                                im3d->vwid->marks->poptog ) ;
         other_tog = im3d->vwid->marks->tog ;
      break ;
   }

   /* bval      = index of toggle that is set (-1 if none)
      other_tog = pointer to other set of toggles;
                  set those buttons to match now */

   AFNI_set_tog( bval , MARKS_MAXNUM , other_tog ) ;

   /* set point overlay colors based on bval */

   for( ip=0 ; ip < MARKS_MAXNUM ; ip++ )
      im3d->vwid->marks->isprimary[ip] = False ;

   if( bval >= 0 ){
      im3d->vwid->marks->isprimary[bval] = True ;

      if( im3d->anat_now->markers->valid[bval] ){  /* jump to this point */
         THD_ivec3 ib ;

         LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */
         ib = THD_3dmm_to_3dind(
                 im3d->anat_now ,
                 TEMP_FVEC3( im3d->anat_now->markers->xyz[bval][0] ,
                             im3d->anat_now->markers->xyz[bval][1] ,
                             im3d->anat_now->markers->xyz[bval][2]  )) ;

         xx = ib.ijk[0] ; yy = ib.ijk[1] ; zz = ib.ijk[2] ;  /* jump is below */
         SAVE_VPT(im3d) ;  /* save current location as jumpback point */
      }
   }

   if( im3d->anat_now->markers->numset > 0 )
      AFNI_set_viewpoint( im3d , xx,yy,zz , REDISPLAY_OVERLAY ) ;  /* redraw */

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

void AFNI_set_tog( int nset , int ntog , Widget * tog )
{
   int ib ;

ENTRY("AFNI_set_tog") ;

   for( ib=0 ; ib < ntog ; ib++ )
      XmToggleButtonSetState( tog[ib] , ib==nset , False ) ;

   EXRETURN ;
}

int AFNI_first_tog( int ntog , Widget * tog )
{
   int ib ;

ENTRY("AFNI_first_tog") ;

   for( ib=0 ; ib < ntog ; ib++ )
      if( XmToggleButtonGetState(tog[ib]) ) break ;

   if( ib >= ntog ) ib = -1 ;
   RETURN(ib) ;
}

#if 0
int AFNI_all_tog( int ntog , Widget * tog )
{
   int ib , val = 0 ;

   for( ib=0 ; ib < ntog ; ib++ )
      if( XmToggleButtonGetState(tog[ib]) ) val |= (1<<ib) ;
   return val ;
}
#endif

/*-------------------------------------------------------------------------
   handle pushbuttons for marks actions:  set and clear markers, etc.
---------------------------------------------------------------------------*/

void AFNI_marks_action_CB( Widget w ,
                           XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int itog , ipt , setmask , vwarp ;
   Boolean sens , transformable ;
   THD_marker_set * markers ;
   AFNI_marks_widgets * marks ;
   THD_fvec3 fv ;

ENTRY("AFNI_marks_action_CB") ;

   /* sanity check */

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   marks = im3d->vwid->marks ;

   /*------ done button (it used to exist) -----*/

   if( w == NULL ){  /* close down */

      Boolean redisplay ;

      MCW_set_bbox( marks->edits_bbox , 0 ) ;
      AFNI_marks_edits_CB( NULL , (XtPointer) im3d , NULL ) ;

      MCW_set_bbox( im3d->vwid->view->see_marks_bbox ,
                    marks->old_visible ? 1 : 0 ) ;
      AFNI_see_marks_CB( NULL , (XtPointer) im3d , NULL ) ;

      redisplay = ! marks->old_visible ;

      for( ipt=0 ; ipt < MARKS_MAXNUM ; ipt++ ){  /* all display as */
         redisplay = ( redisplay ||  marks->isprimary[ipt] == True ) ;
         marks->isprimary[ipt] = False ;          /* secondary now */
      }

      CLOSE_PANEL(im3d,marks) ;  /* close this panel */

      if( redisplay )
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */

      /* save markers as they exist now, if any changes made */

      if( im3d->anat_now->markers != NULL && marks->changed ){
#if 0
         (void) MCW_popup_message(
                   im3d->vwid->view->define_marks_pb ,
                   "Saved changed markers\nto dataset disk file." ,
                   MCW_USER_KILL | MCW_TIMER_KILL ) ;
#endif

         tross_Append_History( im3d->anat_now , "AFNI: markers were edited" ) ;
         (void) THD_write_3dim_dataset( NULL,NULL , im3d->anat_now , False ) ;
      }

      EXRETURN ;
   }

   /*----- quality button (only on when all markers are defined) -----*/

   if( w == marks->action_quality_pb ){
      transformable = AFNI_marks_quality_check(True,im3d) ;
      SENSITIZE( marks->transform_pb , transformable ) ;
      EXRETURN ;
   }

   /*----- if here, either a Set or a Clear -----*/

   markers = im3d->anat_now->markers ;
   if( markers == NULL ) EXRETURN ;  /* should not happen */

   /* find which point is active (i.e., which toggle is set, if any) */

   itog = AFNI_first_tog( MARKS_MAXNUM , marks->tog ) ;

   if( itog < 0 || ! marks->editable ){
      XBell(XtDisplay(w),100) ;  /* none active --> beep and return */
      EXRETURN ;
   }

   ipt = itog ;  /* index of point to deal with */

   /*----- set button pressed -----*/

   if( w == marks->action_set_pb || w == marks->pop_set_pb ){

      if( ! markers->valid[ipt] ) (markers->numset) ++ ;  /* newly set */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"set #%d numset=%d",ipt,markers->numset) ;
  STATUS(str) ; }

      markers->valid[ipt] = True ;

      LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

      fv = THD_3dind_to_3dmm( im3d->anat_now ,    /* convert to mm */
                              TEMP_IVEC3( im3d->vinfo->i1 ,
                                          im3d->vinfo->j2 ,
                                          im3d->vinfo->k3  ) ) ;

      markers->xyz[ipt][0] = fv.xyz[0] ;  /* mm in local x,y,z */
      markers->xyz[ipt][1] = fv.xyz[1] ;
      markers->xyz[ipt][2] = fv.xyz[2] ;

      /* invert colors to mark that the point is set */

      if( ! marks->inverted[itog] ){
         MCW_invert_widget( marks->tog[itog] ) ;
         MCW_invert_widget( marks->poptog[itog] ) ;
         marks->inverted[itog] = True ;
      }

      marks->changed = True ;  /* set or reset a marker --> a change */
   }

   /*----- clear button pressed -----*/

   else if( w == marks->action_clear_pb || w == marks->pop_clear_pb ){

      if( ! markers->valid[ipt] ){
         XBell(XtDisplay(w),100) ;  /* already clear */
         EXRETURN ;
      } else {
         (markers->numset) -- ;   /* newly unset --> sub one from count */
         marks->changed = True ;  /* cleared a set marker --> a change */
      }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"clr #%d numset=%d",ipt,markers->numset) ;
  STATUS(str) ; }

      markers->valid[ipt] = False ;

      /* restore colors to mark that the point is unset */

      if( marks->inverted[itog] ){
         MCW_invert_widget( marks->tog[itog] ) ;
         MCW_invert_widget( marks->poptog[itog] ) ;
         marks->inverted[itog] = False ;
      }
   }

   /*--- allow transformation if all marks are set, etc. ---*/

   vwarp         = WARPED_VIEW(im3d->vinfo->view_type) ;
   transformable = marks->editable                         &&
                   (markers->aflags[1] != MARKACTION_NONE) &&
                   (markers->numdef == markers->numset)    &&
                   ISVALID_VIEW(vwarp)                       ;

   SENSITIZE( marks->action_quality_pb , transformable ) ;
   SENSITIZE( marks->transform_pb      , False ) ;  /* require QC first */

   /*--- force a redraw ---*/

   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  change the resampling size
-------------------------------------------------------------------------*/

void AFNI_resam_vox_av_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_resam_vox_av_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( av == im3d->vwid->dmode->resam_vox_av ){
      im3d->vinfo->resam_vox = av->fval ;
      SHOW_AFNI_PAUSE ;
      im3d->vinfo->tempflag = 1 ;
      AFNI_modify_viewing( im3d , True ) ;  /* redisplay */
      SHOW_AFNI_READY ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------
   handle the changing arrowvals for marker display controls
     (set colors and sizes of markers)
--------------------------------------------------------------------------*/

void AFNI_marks_disp_av_CB( MCW_arrowval * av , XtPointer client_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int ipx = av->ival ;

ENTRY("AFNI_marks_disp_av_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

          if( av == im3d->vwid->marks->disp_pcolor_av ){

            im3d->vwid->marks->ov_pcolor = ipx ;

   } else if( av == im3d->vwid->marks->disp_scolor_av ){

            im3d->vwid->marks->ov_scolor = ipx ;

   } else if( av == im3d->vwid->marks->disp_size_av ){

            im3d->vwid->marks->ov_size = ipx ;

            AFNI_make_ptmask( im3d->vwid->marks->ov_size ,
                              im3d->vwid->marks->ov_gap ,
                               &(im3d->vwid->marks->ov_mask) ) ;

   } else if( av == im3d->vwid->marks->disp_gap_av ){

            im3d->vwid->marks->ov_gap = ipx ;

            AFNI_make_ptmask( im3d->vwid->marks->ov_size ,
                              im3d->vwid->marks->ov_gap ,
                               &(im3d->vwid->marks->ov_mask) ) ;

   } else
      EXRETURN ;  /* some error */

   /* force a redraw if any points are set */

   if( im3d->anat_now->tagset != NULL ||
      (im3d->anat_now->markers != NULL && im3d->anat_now->markers->numset > 0) ){

      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

#define PUTPIX(x,y) (tem->dx[npix] = (x) , tem->dy[npix++] = (y))
#define CHKPIX      if( npix >= MAXOVPIX ) break

void AFNI_make_ptmask( int size , int gap , AFNI_ovtemplate * tem )
{
   register int ix , npix=0 , ax ;

ENTRY("AFNI_make_ptmask") ;

   for( ix=-size ; ix <= size ; ix++ ){
      PUTPIX(ix,-size) ; CHKPIX ;
      PUTPIX(ix, size) ; CHKPIX ;
      ax = abs(ix) ;
      if( ax != size ){ PUTPIX( size,ix); CHKPIX; PUTPIX(-size,ix); CHKPIX; }
      if( ax >  gap  ){ PUTPIX(ix,0)    ; CHKPIX; PUTPIX(0,ix)    ; CHKPIX; }
   }

   tem->numpix = npix ;
   EXRETURN ;
}

/*-------------  October 1998 --------------------------------------------*/

void AFNI_make_tagmask( int size , int gap , AFNI_ovtemplate * tem )
{
   register int ix , npix=0 , ax ;

ENTRY("AFNI_make_tagmask") ;

   PUTPIX(-size,0) ; PUTPIX(size,0) ;
   for( ix=-size+1 ; ix < size ; ix++ ){
      ax = abs(ix) ;
      PUTPIX(ix,ax-size) ; CHKPIX ;
      PUTPIX(ix,size-ax) ; CHKPIX ;

      if( ax > gap ){ PUTPIX(ix,0); CHKPIX; PUTPIX(0,ix); CHKPIX; }
   }

   tem->numpix = npix ;
   EXRETURN ;
}


/*========================================================================
   routines to switch "views" on a dataset
==========================================================================*/

void AFNI_switchview_CB( Widget w ,
                         XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int bval ;

ENTRY("AFNI_switchview_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bval = AFNI_first_tog( LAST_VIEW_TYPE+1 ,
                          im3d->vwid->view->view_bbox->wbut ) ;

   if( bval < 0 || bval == im3d->vinfo->view_type ) EXRETURN ;
   if( im3d->anat_dset[bval] == NULL ) EXRETURN ;

   SHOW_AFNI_PAUSE ;

   POPDOWN_strlist_chooser ;                        /* might be choosing datasets */

   im3d->vinfo->view_type = bval ;                  /* set the new view type */
   AFNI_initialize_view( im3d->anat_now , im3d ) ;  /* and initialize it */

   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------
  Routines to clear out datasets
----------------------------------------------------------*/

void AFNI_purge_unused_dsets(void)
{
   AFNI_purge_dsets( 0 ) ;
}

void AFNI_purge_dsets( int doall )
{
   int icc , iss , idd , ivv ;
   Three_D_View * im3d ;
   THD_session  * sess ;
   THD_sessionlist * ssl = GLOBAL_library.sslist ;
   THD_3dim_dataset * dset ;

ENTRY("AFNI_purge_dsets") ;

   /*-- sanity check --*/

   if( ! ISVALID_SESSIONLIST(ssl) || ssl->num_sess <= 0 ) EXRETURN ;

   /*-- for each session in the list --*/

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      sess = ssl->ssar[iss] ;

      /*-- for each anat dataset in the session --*/

      for( idd=0 ; idd < sess->num_anat ; idd++ ){
         for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){

            dset = sess->anat[idd][ivv] ;
            if( dset == NULL ) continue ;
            if( doall ){ PURGE_DSET(dset) ; continue ; }

            /*-- for each controller now running --*/

            for( icc=0 ; icc < MAX_CONTROLLERS ; icc++ ){
               im3d = GLOBAL_library.controllers[icc] ;
               if( IM3D_VALID(im3d) &&
                   ((dset==im3d->anat_now) || (dset==im3d->fimdata->fimdset)) ) break ;
            }

            /*-- if didn't find it, purge it --*/
            if( icc == MAX_CONTROLLERS ){ PURGE_DSET(dset) ; }
         }
      }

      /*-- for each func dataset in the session --*/

      for( idd=0 ; idd < sess->num_func ; idd++ ){
         for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){

            dset = sess->func[idd][ivv] ;
            if( dset == NULL ) continue ;
            if( doall ){ PURGE_DSET(dset) ; continue ; }

            /*-- for each controller now running --*/

            for( icc=0 ; icc < MAX_CONTROLLERS ; icc++ ){
               im3d = GLOBAL_library.controllers[icc] ;
               if( IM3D_VALID(im3d) &&
                   ((dset==im3d->fim_now) || (dset==im3d->fimdata->fimdset)) ) break ;
            }

            /*-- if didn't find it, purge it --*/
            if( icc == MAX_CONTROLLERS ){ PURGE_DSET(dset) ; }
         }
      }

   } /* end of loop over sessions */
   EXRETURN ;
}

/*---------------------------------------------------------------------
   setup for viewing, given the choices in im3d->vinfo
   (the indexes of the desired session and datasets, that is)
-----------------------------------------------------------------------*/

void AFNI_initialize_view( THD_3dim_dataset * old_anat, Three_D_View * im3d )
{
   int vvv , itog , lll , sss , aaa , fff , id ;
   THD_3dim_dataset     * dset , * new_anat , * new_func ;
   THD_marker_set       * markers ;
   AFNI_viewing_widgets * view ;
   AFNI_marks_widgets   * marks ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_initialize_view") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   vvv = im3d->vinfo->view_type ;  /* locations of new data to view */
   sss = im3d->vinfo->sess_num ;
   aaa = im3d->vinfo->anat_num ;
   fff = im3d->vinfo->func_num ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"view=%d session=%d anat=%d func=%d",vvv,sss,aaa,fff);
  STATUS(str) ; }

   new_anat = GLOBAL_library.sslist->ssar[sss]->anat[aaa][vvv] ;
   new_func = GLOBAL_library.sslist->ssar[sss]->func[fff][vvv] ;

   /*----------------------------------------------*/
   /*--- if the old dataset has markers and the
         marker panel is open, shut it down now ---*/

   if( old_anat != NULL     && /** old_anat->markers != NULL && **/
       old_anat != new_anat && XtIsManaged(im3d->vwid->marks->frame) ){

      AFNI_marks_action_CB( NULL, (XtPointer) im3d, NULL) ; /* "done" */
   }

   if( GLOBAL_argopt.auto_purge == True ){ /* purge old datasets? */

STATUS("purging old datasets from memory (maybe)") ;

      im3d->anat_now = new_anat ;
      im3d->fim_now  = new_func ;
      AFNI_purge_unused_dsets() ;
   }
   if( SUMA_ENABLED ){                                    /* 29 Aug 2001 */
      if( old_anat != new_anat ) SUMA_unload( old_anat ) ;
      SUMA_load( new_anat ) ;
   }

   /*---------------------------------------------------------*/
   /* set the new datasets that we will deal with from now on */

   for( id=0 ; id <= LAST_VIEW_TYPE ; id++ ){
      im3d->anat_dset[id] = GLOBAL_library.sslist->ssar[sss]->anat[aaa][id] ;
      im3d->fim_dset[id]  = GLOBAL_library.sslist->ssar[sss]->func[fff][id] ;

      if( ISVALID_3DIM_DATASET(im3d->anat_dset[id]) )
         SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], True ) ;
      else
         SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], False) ;
   }

   im3d->anat_now = im3d->anat_dset[vvv] ;
   im3d->fim_now  = im3d->fim_dset[vvv] ;
   im3d->ss_now   = GLOBAL_library.sslist->ssar[sss] ;

   /*------------------------------------------------*/
   /*--- if markers are defined, then set them up ---*/

   dset    = im3d->anat_now ;
   markers = dset->markers ;
   view    = im3d->vwid->view ;
   marks   = im3d->vwid->marks ;

   if( markers == NULL ){   /*--------- markers NOT defined ---------*/

STATUS("turning markers off") ;

      /* turn controls off */

#if 0
      SENSITIZE(  view->define_marks_pb , False ) ;
      SENSITIZE(  view->see_marks_bbox->wrowcol , False ) ;
#endif

      marks->editable = False ;

      vvv = MCW_val_bbox( view->see_marks_bbox ) ;
      marks->tag_visible = marks->ov_visible = (vvv) ? True : False ;

      XtUnmanageChildren( marks->always_popup    , marks->num_always_popup    ) ;
      XtUnmanageChildren( marks->sometimes_popup , marks->num_sometimes_popup ) ;

   } else {   /*------------- markers ARE defined ----------------*/

STATUS("turning markers on") ;

      /* turn controls on */

      SENSITIZE( view->define_marks_pb , True ) ;
      SENSITIZE( view->see_marks_bbox->wrowcol , True ) ;

      vvv = MCW_val_bbox( view->see_marks_bbox ) ;
      marks->tag_visible = marks->ov_visible = (vvv) ? True : False ;

      marks->editable = False ;
      MCW_set_bbox( marks->edits_bbox , 0 ) ;

      SENSITIZE( marks->pop_set_pb   , marks->editable ) ;
      SENSITIZE( marks->pop_clear_pb , marks->editable ) ;

      /* copy help into location where MCW_help will find it */

      for( itog=0 ; itog < MARKS_MAXNUM ; itog++ ){
         MCW_strncpy( &(marks->tog_help[itog][0]) ,
                      &(markers->help[itog][0]) , MARKS_MAXHELP ) ;
      }

      /* copy the non-empty labels into the toggle labels,
         and make the toggle buttons active (panel AND popup) */

      XtManageChildren( marks->always_popup ,
                        marks->num_always_popup ) ;

      for( itog=0 ; itog < MARKS_MAXNUM ; itog++ ){
         lll = strlen( &(markers->label[itog][0]) ) ;

         if( lll == 0 ){
            XtUnmanageChild( marks->tog[itog] ) ;   /* empty label! */
            XtUnmanageChild( marks->poptog[itog] ) ;
         } else {
            MCW_set_widget_label( marks->tog[itog] ,
                                  &(markers->label[itog][0]) ) ;
            SENSITIZE( marks->tog[itog] , True ) ;
            XtManageChild( marks->tog[itog] ) ;

            MCW_set_widget_label( marks->poptog[itog] ,
                                  &(markers->label[itog][0]) ) ;
            SENSITIZE( marks->poptog[itog] , True ) ;
            XtManageChild( marks->poptog[itog] ) ;

            if( markers->valid[itog] && ! marks->inverted[itog] ){
               MCW_invert_widget( marks->tog[itog] ) ;
               MCW_invert_widget( marks->poptog[itog] ) ;
               marks->inverted[itog] = True ;
            }

            if( ! markers->valid[itog] && marks->inverted[itog] ){
               MCW_invert_widget( marks->tog[itog] ) ;
               MCW_invert_widget( marks->poptog[itog] ) ;
               marks->inverted[itog] = False ;
            }
         }
      } /* end of loop over markers */

   } /* end of dealing with markers */

   /*------------------------------*/
   /*----- set up for viewing -----*/

   AFNI_setup_viewing( im3d , True ) ;

   /*-----------------------------------------------------*/
   /*----- reset viewpoint to same Dicom coordinates -----*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      fv = AFNI_transform_vector(
              old_anat ,
              TEMP_FVEC3( im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk ),
              dset ) ;

      LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

      fv = THD_dicomm_to_3dmm( dset , fv ) ;
      iv = THD_3dmm_to_3dind( dset , fv ) ;
   } else {
      LOAD_IVEC3( iv,  im3d->vinfo->i1, im3d->vinfo->j2, im3d->vinfo->k3 ) ;
   }

   DISABLE_LOCK ;  /* 11 Nov 1996 */

   AFNI_set_viewpoint( im3d, iv.ijk[0],iv.ijk[1],iv.ijk[2] , REDISPLAY_ALL ) ;

   ENABLE_LOCK ;   /* 11 Nov 1996 */

   SAVE_VPT(im3d) ;  /* save current location as jumpback */

   EXRETURN ;
}

/*----------------------------------------------------------------------
   set the stage for viewing:
     -- prepare for warp-on-demand image production
     -- setup the viewing FD_bricks
     -- attach them to the viewing windows
     -- turn widget controls on and off, based on data status

   02 Nov 1996: set up view specific viewing stuff in im3d,
                rather than in the datasets.  This is to allow
                for the possibility that more than one im3d
                may be looking at the same dataset at once.

   30 Nov 1997: add bucket stuff
------------------------------------------------------------------------*/

void AFNI_setup_viewing( Three_D_View * im3d , Boolean rescaled )
{
   FD_brick ** fbr ;
   XmString xstr ;
   Boolean  same , dont_fix_pts , writer ,
            anat_brick_possible , func_brick_possible ;
   int      val , top ;

   static THD_3dim_dataset *old_fim  = NULL ; /* 12 Dec 2001 */
   static Three_D_View     *old_im3d = NULL ; /* 29 Jan 2002 */
   static THD_3dim_dataset *old_anat = NULL ; /* 12 Dec 2001 */

ENTRY("AFNI_setup_viewing") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /*-----------------------------------------------------*/
   /*--- set up the anat w-o-d axes and viewing bricks ---*/

   anat_brick_possible = DSET_INMEMORY(im3d->anat_now) ;

   /*- The Ides of March, 2000: allow switching back to "view brick" -*/

   if( anat_brick_possible         &&
       im3d->vinfo->force_anat_wod &&
       im3d->vinfo->tempflag == 0  &&
       AFNI_yesenv("AFNI_VIEW_ANAT_BRICK") ){

      im3d->vinfo->force_anat_wod = 0 ;
      MCW_set_bbox( im3d->vwid->dmode->anatmode_bbox , DMODE_BRICK_BVAL ) ;
   }

   im3d->anat_wod_flag = ( im3d->vinfo->force_anat_wod ||       /* 02 Nov 1996 */
                           !anat_brick_possible          ) ;

   if( im3d->anat_wod_flag )                                    /* 02 Nov 1996 */
      THD_edit_dataxes( im3d->vinfo->resam_vox ,
                        im3d->anat_now->daxes , im3d->wod_daxes ) ;
   else
      *(im3d->wod_daxes) = *(im3d->anat_now->daxes) ;

   im3d->anat_voxwarp->type =
      im3d->fim_voxwarp->type = ILLEGAL_TYPE ;                  /* 02 Nov 1996 */

   LOAD_ANAT_VIEW(im3d) ;                                       /* 02 Nov 1996 */

   fbr = THD_setup_bricks( im3d->anat_now ) ;
   if( fbr == NULL ){
      fprintf(stderr,"THD_setup_bricks of anat_now fails!\n") ; EXRETURN ;
   }
   myXtFree(im3d->b123_anat) ; im3d->b123_anat = fbr[0] ;
   myXtFree(im3d->b231_anat) ; im3d->b231_anat = fbr[1] ;
   myXtFree(im3d->b312_anat) ; im3d->b312_anat = fbr[2] ;
   myXtFree(fbr) ;

   im3d->b123_anat->parent =
     im3d->b231_anat->parent =
       im3d->b312_anat->parent = (XtPointer) im3d ;

   im3d->b123_anat->resam_code =
     im3d->b231_anat->resam_code =
       im3d->b312_anat->resam_code = im3d->vinfo->anat_resam_mode ;

   /* 30 Nov 1997: don't go past end of bucket */

   if( ISANATBUCKET(im3d->anat_now) &&
       im3d->vinfo->anat_index >= DSET_NVALS(im3d->anat_now) )
      im3d->vinfo->anat_index = DSET_NVALS(im3d->anat_now) - 1 ;

   /*-----------------------------------------------------*/
   /*--- set up the func w-o-d axes and viewing bricks ---*/

   if( ISVALID_3DIM_DATASET( im3d->fim_now ) ){

STATUS("function") ;

      /*-- access data in dataset im3d->fim_now IF AND ONLY IF
             1) its actual data axes are the same as the wod_daxes
             2) it has actual data
             3) the user hasn't officially declared for warp-on-demand --*/

STATUS("deciding whether to use function WOD") ;

      func_brick_possible =
         EQUIV_DATAXES( im3d->fim_now->daxes , im3d->wod_daxes ) &&   /* 02 Nov 1996 */
         DSET_INMEMORY( im3d->fim_now ) ;

      /*- The Ides of March, 2000: allow switching back to "view brick" -*/

      if( func_brick_possible          &&
          im3d->vinfo->force_func_wod  &&
          im3d->vinfo->tempflag == 0   &&
          AFNI_yesenv("AFNI_VIEW_FUNC_BRICK") ){

         im3d->vinfo->force_func_wod = 0 ;
         MCW_set_bbox( im3d->vwid->dmode->funcmode_bbox , DMODE_BRICK_BVAL ) ;
      }

      if( func_brick_possible && ! im3d->vinfo->force_func_wod ){
STATUS("not forcing function WOD") ;
         im3d->fim_wod_flag = False ;   /* 02 Nov 1996 */
      } else {
STATUS("forcing function WOD") ;
         im3d->fim_wod_flag = True ;   /* 02 Nov 1996 */
      }

      LOAD_FUNC_VIEW(im3d) ;  /* 02 Nov 1996 */

      fbr = THD_setup_bricks( im3d->fim_now ) ;
      if( fbr == NULL ){
         fprintf(stderr,"THD_setup_bricks of fim_now fails!\n") ; EXRETURN ;
      }
      myXtFree(im3d->b123_fim) ; im3d->b123_fim = fbr[0] ;
      myXtFree(im3d->b231_fim) ; im3d->b231_fim = fbr[1] ;
      myXtFree(im3d->b312_fim) ; im3d->b312_fim = fbr[2] ;
      myXtFree(fbr) ;

      im3d->b123_fim->parent =
        im3d->b231_fim->parent =
          im3d->b312_fim->parent = (XtPointer) im3d ;

      im3d->b123_fim->resam_code =
        im3d->b231_fim->resam_code =
          im3d->b312_fim->resam_code = im3d->vinfo->func_resam_mode ;

      im3d->b123_fim->thr_resam_code =     /* 09 Dec 1997 */
        im3d->b231_fim->thr_resam_code =
          im3d->b312_fim->thr_resam_code = im3d->vinfo->thr_resam_mode ;

      /* 30 Nov 1997: don't go past end of bucket */

      if( ISFUNCBUCKET(im3d->fim_now) ){

          if( im3d->vinfo->fim_index >= DSET_NVALS(im3d->fim_now) )
             im3d->vinfo->fim_index = DSET_NVALS(im3d->fim_now) - 1 ;

          if( im3d->vinfo->thr_index >= DSET_NVALS(im3d->fim_now) )
             im3d->vinfo->thr_index = DSET_NVALS(im3d->fim_now) - 1 ;
      }

   } else {

STATUS("no function") ;

      myXtFree(im3d->b123_fim) ; im3d->b123_fim = NULL ;
      myXtFree(im3d->b231_fim) ; im3d->b231_fim = NULL ;
      myXtFree(im3d->b312_fim) ; im3d->b312_fim = NULL ;

      func_brick_possible = False ;
   }

   /*------------------------------------------------------------------*/
   /*--- set widget sensitivity based on kind of data now available ---*/

STATUS("turning widgets on and/or off:") ;

   /*--- datamode controls ---*/

STATUS(" -- datamode widgets") ;

   if( anat_brick_possible ){
      SENSITIZE( im3d->vwid->dmode->anatmode_bbox->wbut[DMODE_BRICK] , True ) ;
   } else {
      SENSITIZE( im3d->vwid->dmode->anatmode_bbox->wbut[DMODE_BRICK] , False ) ;
      MCW_set_bbox( im3d->vwid->dmode->anatmode_bbox , DMODE_WOD_BVAL ) ;
      im3d->vinfo->force_anat_wod = True ;
   }

   if( func_brick_possible ){
      SENSITIZE( im3d->vwid->dmode->funcmode_bbox->wbut[DMODE_BRICK] , True ) ;
   } else {
      SENSITIZE( im3d->vwid->dmode->funcmode_bbox->wbut[DMODE_BRICK] , False ) ;
      MCW_set_bbox( im3d->vwid->dmode->funcmode_bbox , DMODE_WOD_BVAL ) ;
      im3d->vinfo->force_func_wod = True ;
   }

   AV_SENSITIZE( im3d->vwid->dmode->anat_resam_av , im3d->anat_wod_flag ) ;

   AV_SENSITIZE( im3d->vwid->dmode->resam_vox_av , im3d->anat_wod_flag ) ;

   /* Jan 31, 1995: don't allow writes of datasets without warp parents */
   /* Jun 22, 1995: allow it if destruct mode is actuated!              */

   if( GLOBAL_argopt.destruct ){  /* not currently implemented */
      writer = True ;
   } else {
      writer = (Boolean) DSET_WRITEABLE(im3d->anat_now) ;  /* mod 26 Mar 2001 */
   }

   SENSITIZE( im3d->vwid->dmode->write_anat_pb , writer ) ;

   if( GLOBAL_argopt.destruct ){  /* not currently implemented */
      writer = (Boolean) ISVALID_3DIM_DATASET(im3d->fim_now) ;
   } else {
      writer = (Boolean) DSET_WRITEABLE(im3d->fim_now) ;  /* mod 26 Mar 2001 */
   }

   SENSITIZE( im3d->vwid->dmode->write_func_pb , writer ) ;

   /*--- dataset chooser controls (01 Nov 1996: always allow) ---*/

#if 0
STATUS(" -- dataset chooser widgets") ;

   SENSITIZE( im3d->vwid->view->choose_sess_pb ,
                     (Boolean) (GLOBAL_library.sslist->num_sess > 1) ) ;

   SENSITIZE( im3d->vwid->view->choose_anat_pb ,
                     (Boolean) (im3d->ss_now->num_anat > 1) ) ;

   SENSITIZE( im3d->vwid->view->choose_func_pb ,
                     (Boolean) (im3d->ss_now->num_func > 1) ) ;

#ifdef POPUP_CHOOSERS
   XtSetSensitive( im3d->vwid->view->popchoose_sess_pb ,
                     (Boolean) (GLOBAL_library.sslist->num_sess > 1) ) ;

   XtSetSensitive( im3d->vwid->view->popchoose_anat_pb ,
                     (Boolean) (im3d->ss_now->num_anat > 1) ) ;

   XtSetSensitive( im3d->vwid->view->popchoose_func_pb ,
                     (Boolean) (im3d->ss_now->num_func > 1) ) ;
#endif
#endif

   /*--- function controls ---*/

#undef CLOSE_FUNC_PANEL
#ifdef CLOSE_FUNC_PANEL
   if( ! ISVALID_3DIM_DATASET(im3d->fim_now) ){

STATUS(" -- function widgets OFF") ;

      CLOSE_PANEL(im3d,func) ;  /* close the panel */

      SENSITIZE( im3d->vwid->view->define_func_pb      , False ) ;
      SENSITIZE( im3d->vwid->view->see_func_bbox->wtop , False ) ;
      im3d->vinfo->underlay_type = UNDERLAY_ANAT ;
   } else
#endif
   {
      Boolean have_fim = ISVALID_3DIM_DATASET(im3d->fim_now) ;
      Boolean have_thr = have_fim && FUNC_HAVE_THR(im3d->fim_now->func_type) ;
      int do_buck ;

STATUS(" -- function widgets ON") ;

      SENSITIZE( im3d->vwid->view->define_func_pb      , True ) ;
      SENSITIZE( im3d->vwid->view->see_func_bbox->wtop , True ) ;

      /* make some widgets sensitive if we have the threshold available */

      if( ! have_thr ){
STATUS(" ---- threshold scale OFF") ;
         XtUnmanageChild( im3d->vwid->func->thr_rowcol ) ;
      } else {
STATUS(" ---- threshold scale ON") ;
         XtManageChild  ( im3d->vwid->func->thr_rowcol ) ;
         FIX_SCALE_SIZE(im3d) ; FIX_SCALE_VALUE(im3d) ;
      }

      SENSITIZE( im3d->vwid->func->underlay_bbox->wbut[UNDERLAY_ALLFUNC],
                      have_fim ) ;
      SENSITIZE( im3d->vwid->func->underlay_bbox->wbut[UNDERLAY_THRFUNC],
                      have_thr ) ;

      SENSITIZE( im3d->vwid->func->functype_bbox->wbut[SHOWFUNC_FIM],
                      have_thr ) ;
      SENSITIZE( im3d->vwid->func->functype_bbox->wbut[SHOWFUNC_THR],
                      have_thr ) ;

           if( ! have_fim )
              im3d->vinfo->underlay_type = UNDERLAY_ANAT ;
      else if( ! have_thr && im3d->vinfo->underlay_type == UNDERLAY_THRFUNC )
              im3d->vinfo->underlay_type = UNDERLAY_ALLFUNC ;

      if( ! have_thr ) im3d->vinfo->showfunc_type = SHOWFUNC_FIM ;

      if( have_fim && ISFUNCBUCKET(im3d->fim_now) )    /* 30 Nov 1997 */
         im3d->vinfo->showfunc_type = SHOWFUNC_FIM ;

#if 0
      /* 12 Aug 1996: fix range control sensitivity and settings */

      SENSITIZE( im3d->vwid->func->range_bbox->wrowcol ,
                 (im3d->vinfo->showfunc_type == SHOWFUNC_FIM) ) ;
#endif

      /* allow resample control only if we are using w-o-d */

      AV_SENSITIZE( im3d->vwid->dmode->func_resam_av,
                    have_fim && im3d->fim_wod_flag ) ;

      AV_SENSITIZE( im3d->vwid->dmode->thr_resam_av,    /* 09 Dec 1997 */
                    have_fim && im3d->fim_wod_flag ) ;

      /** Mar 1996: modify the threshold scale stuff **/
      /** Oct 1996: increase decim by 1 to allow for
                    new precision 0..999 of scale (used to be 0..99) **/
      /** Nov 1997: the scale precision is now set by macro THR_TOP_EXPON,
                    and its settings are done in routine AFNI_set_thresh_top **/

      if( have_thr ){

         if( ! ISFUNCBUCKET(im3d->fim_now) ){  /* 30 Nov 1997 */


#if 1
            /* set number of decimal places to shift for thr_scale */

STATUS(" ---- set threshold decim OLD") ;
            AFNI_set_thresh_top( im3d , FUNC_topval[im3d->fim_now->func_type] ) ;
#endif

            /* set the label at the top of the scale */

            MCW_set_widget_label( im3d->vwid->func->thr_label ,
                                  FUNC_label[im3d->fim_now->func_type] ) ;
         } else {
            int iv = im3d->vinfo->thr_index , jj ;

#if 0
STATUS(" ---- set threshold decim NEW") ;
            if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
               float bb = fabs(im3d->fim_now->stats->bstat[iv].min) ;
               float tt = fabs(im3d->fim_now->stats->bstat[iv].max) ;
               float xx = (bb<tt) ? tt : bb ;

               if( xx > 0.0 ){
                  jj = (int)( 0.999 + log10(xx) ) ;
                       if( jj < 0             ) jj = 0 ;
                  else if( jj > THR_TOP_EXPON ) jj = THR_TOP_EXPON ;
                  xx = pow(10.0,jj) ;
                  AFNI_set_thresh_top( im3d, xx ) ;
               }
            }
#endif

            jj = DSET_BRICK_STATCODE(im3d->fim_now,iv) ;
            if( jj > 0 )
               MCW_set_widget_label( im3d->vwid->func->thr_label ,
                                     FUNC_label[jj] ) ;
            else
               MCW_set_widget_label( im3d->vwid->func->thr_label ,
                                     DSET_BRICK_LABEL(im3d->fim_now,iv) ) ;
         }

         /* set the pval label at the bottom of the scale */

         AFNI_set_thr_pval( im3d ) ;
      }

      /*** 30 Nov 1997:
           Open/close widgets depending on if we have function buckets ***/

      do_buck = 0 ;

      /* 12 Dec 2001: only refit menus if dataset has changed */

      if( ISFUNCBUCKET(im3d->fim_now) ){
         if( im3d->fim_now != old_fim || im3d != old_im3d ){
STATUS(" ---- func bucket widgets ON") ;
          XtUnmanageChild( im3d->vwid->func->functype_bbox->wtop )  ;
          refit_MCW_optmenu( im3d->vwid->func->fim_buck_av ,
                             0 ,                            /* new minval */
                             DSET_NVALS(im3d->fim_now)-1 ,  /* new maxval */
                             im3d->vinfo->fim_index ,       /* new inival */
                             0 ,                            /* new decim? */
                             AFNI_bucket_label_CB ,         /* text routine */
                             im3d->fim_now                  /* text data */
                           ) ;
          refit_MCW_optmenu( im3d->vwid->func->thr_buck_av ,
                             0 ,                            /* new minval */
                             DSET_NVALS(im3d->fim_now)-1 ,  /* new maxval */
                             im3d->vinfo->thr_index ,       /* new inival */
                             0 ,                            /* new decim? */
                             AFNI_bucket_label_CB ,         /* text routine */
                             im3d->fim_now                  /* text data */
                           ) ;
          XtManageChild  ( im3d->vwid->func->fim_buck_av->wrowcol ) ;
          XtManageChild  ( im3d->vwid->func->thr_buck_av->wrowcol ) ;
         }
         do_buck = 1 ;
      } else {
STATUS(" ---- func bucket widgets OFF") ;
         XtManageChild  ( im3d->vwid->func->functype_bbox->wtop )  ;
         XtUnmanageChild( im3d->vwid->func->fim_buck_av->wrowcol ) ;
         XtUnmanageChild( im3d->vwid->func->thr_buck_av->wrowcol ) ;
      }

      if( ISANATBUCKET(im3d->anat_now) ){
         if( im3d->anat_now != old_anat ){
STATUS(" ---- anat bucket widgets ON") ;
          refit_MCW_optmenu( im3d->vwid->func->anat_buck_av ,
                             0 ,                             /* new minval */
                             DSET_NVALS(im3d->anat_now)-1 ,  /* new maxval */
                             im3d->vinfo->anat_index ,       /* new inival */
                             0 ,                             /* new decim? */
                             AFNI_bucket_label_CB ,          /* text routine */
                             im3d->anat_now                  /* text data */
                           ) ;
          XtManageChild( im3d->vwid->func->anat_buck_av->wrowcol ) ;
         }
         do_buck = 1 ;
      } else {
STATUS(" ---- anat bucket widgets OFF") ;
         XtUnmanageChild( im3d->vwid->func->anat_buck_av->wrowcol ) ;
      }

      if( do_buck ){
         XtManageChild( im3d->vwid->func->buck_rowcol ) ;
         XtManageChild( im3d->vwid->func->buck_frame ) ;
      } else {
         XtUnmanageChild( im3d->vwid->func->buck_frame ) ;
      }
   }

   /*--- set the function type bboxes based on the current
         viewing set up (which may have changed due to lack of function) ---*/

STATUS(" -- function underlay widgets") ;

   MCW_set_bbox( im3d->vwid->func->underlay_bbox ,
                 1 << im3d->vinfo->underlay_type ) ;

   MCW_set_bbox( im3d->vwid->func->functype_bbox ,
                 1 << im3d->vinfo->showfunc_type ) ;

   /*--------------------------------------------------------*/
   /*--- 3/24/95: deal with the new range widgets in func ---*/

   AFNI_reset_func_range( im3d ) ;

   /*---------------------------------------------------------*/
   /*--- May 1995: if points exist in some other dataset   ---*/
   /*---           associated with this one, but not here, ---*/
   /*---           transform the points to this dataset.   ---*/

   dont_fix_pts = ! rescaled ;  /* if didn't rescale, don't need to fix */

   if( im3d->anat_now->pts == NULL ){
      int ii ;
      THD_3dim_dataset * dset_orig = NULL ;
      THD_fvec3 fv ;

STATUS(" -- scanning for points in other datasets") ;
      for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ ){
         if( ISVALID_3DIM_DATASET(im3d->anat_dset[ii]) &&
             im3d->anat_dset[ii]->pts != NULL &&
             im3d->anat_dset[ii]->pts_original == True ){

            dset_orig = im3d->anat_dset[ii] ;
            break ;
         }
      }

      if( dset_orig != NULL ){
STATUS(" -- processing points in other dataset") ;

         dont_fix_pts                 = True ;   /* fixing here, so not later */
         im3d->anat_now->pts_original = False ;
         INIT_VLIST( im3d->anat_now->pts , im3d->anat_now ) ;

         for( ii=0 ; ii < dset_orig->pts->num ; ii++ ){
            fv = THD_3dmm_to_dicomm( dset_orig , dset_orig->pts->xyz[ii] ) ;
            fv = AFNI_transform_vector( dset_orig , fv  , im3d->anat_now ) ;
            fv = THD_dicomm_to_3dmm( im3d->anat_now , fv ) ;
            ADD_FVEC_TO_VLIST( im3d->anat_now->pts , fv ) ;
         }
      }
   }

   /*--------------------------------------------------*/
   /*--- May 1995: if points exist in this dataset, ---*/
   /*---           load their 3dind coordinates.    ---*/

   if( im3d->anat_now->pts != NULL && ! dont_fix_pts ){
      int ii ;
STATUS(" -- processing points in this dataset") ;

      for( ii=0 ; ii < im3d->anat_now->pts->num ; ii++ )
         im3d->anat_now->pts->ijk[ii] =
            THD_3dmm_to_3dind( im3d->anat_now , im3d->anat_now->pts->xyz[ii] ) ;
   }

   /*------ 06 Mar 2002: turn "SUMA to" on image popup on or off ------*/

   if( im3d->vwid->imag->pop_sumato_pb != NULL ){
     if( im3d->anat_now->su_surf == NULL )
        XtSetSensitive( im3d->vwid->imag->pop_sumato_pb , False ) ;
     else
        XtSetSensitive( im3d->vwid->imag->pop_sumato_pb , True  ) ;
   }

   /*-------------------------------------------------------------------*/
   /*--- Sep 1995: turn Talairach to button on image popup on or off ---*/

STATUS(" -- managing talairach_to button") ;

   if( im3d->vwid->imag->pop_talto_pb != NULL ){
      if( CAN_TALTO(im3d) ){
         XtSetSensitive( im3d->vwid->imag->pop_talto_pb , True ) ;
         if( im3d->vwid->imag->pop_whereami_pb != NULL )
          XtSetSensitive( im3d->vwid->imag->pop_whereami_pb , True ); /* 10 Jul 2001 */
         if( im3d->vwid->imag->pop_ttren_pb != NULL )
          XtSetSensitive( im3d->vwid->imag->pop_ttren_pb ,              /* 12 Jul 2001 */
                          im3d->vinfo->view_type==VIEW_TALAIRACH_TYPE); /* 01 Aug 2001 */
      } else {
         XtSetSensitive( im3d->vwid->imag->pop_talto_pb, False ) ;
         if( im3d->vwid->imag->pop_whereami_pb != NULL )
          XtSetSensitive( im3d->vwid->imag->pop_whereami_pb, False ); /* 10 Jul 2001 */
         if( im3d->vwid->imag->pop_ttren_pb != NULL )
          XtSetSensitive( im3d->vwid->imag->pop_ttren_pb , False ); /* 12 Jul 2001 */
      }
   }

   /*--- 25 Jul 2001: sensitize 'See TT Atlas Regions' button ---*/

#if 1
   XtSetSensitive( im3d->vwid->func->see_ttatlas_bbox->wrowcol ,
                   (Boolean)( im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE &&
                              TT_retrieve_atlas()       != NULL                  ) ) ;
#else
   XtSetSensitive( im3d->vwid->func->see_ttatlas_bbox->wrowcol , False ) ;
#endif

   /*------------------------------------*/
   /*--- May 1996: Time index control ---*/
   /*--- Mar 1997: Allow FIM also     ---*/

   top = DSET_NUM_TIMES(im3d->anat_now) ;
   if( ISVALID_3DIM_DATASET(im3d->fim_now) )
      top = MAX( top , DSET_NUM_TIMES(im3d->fim_now) ) ;

   if( top > 1 ){
      MCW_arrowval * tav = im3d->vwid->imag->time_index_av ;
STATUS(" -- turning time index control on") ;

      AV_SENSITIZE( tav , True ) ;
      tav->fmax = tav->imax = top - 1 ; im3d->vinfo->top_index = top ;
      if( im3d->vinfo->time_index > tav->imax ){
         im3d->vinfo->time_index = tav->imax ;
         AV_assign_ival( tav , tav->imax ) ;
      }
   } else {
STATUS(" -- turning time index control off") ;
      AV_SENSITIZE( im3d->vwid->imag->time_index_av , False ) ;
   }

   /*--------------------------------------------------------------*/
   /*--- 19 Nov 1996: Set FIM-able dataset to this, if possible ---*/

   if( DSET_GRAPHABLE(im3d->anat_now) )
      im3d->fimdata->fimdset = im3d->anat_now ;

   ALLOW_COMPUTE_FIM(im3d) ;

   /*------------------------------------------*/
   /*--- attach to viewing windows (if any) ---*/

   AFNI_underlay_CB( NULL , (XtPointer) im3d , NULL ) ;

   im3d->vinfo->tempflag = 0 ;

   old_im3d = im3d ;
   old_fim  = im3d->fim_now ;   /* remembrance */
   old_anat = im3d->anat_now ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Tell if AFNI_transform_vector can take a vector from old_dset
  to new_dset coordinates -- 09 Jul 2001 -- RWCox.
-------------------------------------------------------------------------*/

int AFNI_can_transform_vector( THD_3dim_dataset * old_dset ,
                               THD_3dim_dataset * new_dset  )
{
   if( old_dset==NULL || new_dset==NULL  ) return 0 ;

   if( old_dset == new_dset->warp_parent ) return 1 ;

   if( old_dset->warp_parent == new_dset ) return 1 ;

   if( old_dset->warp_parent == new_dset->warp_parent &&
       old_dset->warp_parent != NULL                   ) return 1 ;

   if( new_dset->view_type   == VIEW_ORIGINAL_TYPE &&
       old_dset->view_type   != VIEW_ORIGINAL_TYPE &&
       old_dset->anat_parent != NULL               &&
       old_dset->anat_parent->warp_parent != NULL      ) return 1 ;

   if( old_dset->view_type   == VIEW_ORIGINAL_TYPE &&
       new_dset->view_type   != VIEW_ORIGINAL_TYPE &&
       new_dset->anat_parent != NULL               &&
       new_dset->anat_parent->warp_parent != NULL      ) return 1 ;

   return 0 ;
}

/*-----------------------------------------------------------------------
  Eventually, will warp an input Dicom vector from one dataset to another.
-------------------------------------------------------------------------*/

THD_fvec3 AFNI_transform_vector( THD_3dim_dataset * old_dset ,
                                 THD_fvec3 old_fv ,
                                 THD_3dim_dataset * new_dset  )
{
   if( old_dset==NULL || new_dset==NULL || old_dset==new_dset ) return old_fv ;

   if( old_dset == new_dset->warp_parent ){

      return AFNI_forward_warp_vector( new_dset->warp , old_fv ) ;

   } else if( old_dset->warp_parent == new_dset ){

      return AFNI_backward_warp_vector( old_dset->warp , old_fv ) ;

   } else if( old_dset->warp_parent == new_dset->warp_parent &&
              old_dset->warp_parent != NULL ){

      THD_fvec3 par_fv ;
      par_fv = AFNI_backward_warp_vector( old_dset->warp , old_fv ) ;
      return   AFNI_forward_warp_vector ( new_dset->warp , par_fv ) ;
   }

   /*-- 09 Jul 2001:
        If old_dset is in +tlrc/+acpc and new_dset is in +orig,
        see if can find anat_parent to do the job for us --*/

   if( new_dset->view_type   == VIEW_ORIGINAL_TYPE &&
       old_dset->view_type   != VIEW_ORIGINAL_TYPE &&
       old_dset->anat_parent != NULL               &&
       old_dset->anat_parent->warp_parent != NULL      ){

      return AFNI_backward_warp_vector( old_dset->anat_parent->warp , old_fv ) ;
   }

   /*-- If old_dset is +orig and new_dset is +tlrc/+acpc, try anat_parent --*/

   if( old_dset->view_type   == VIEW_ORIGINAL_TYPE &&
       new_dset->view_type   != VIEW_ORIGINAL_TYPE &&
       new_dset->anat_parent != NULL               &&
       new_dset->anat_parent->warp_parent != NULL      ){

      return AFNI_forward_warp_vector( new_dset->anat_parent->warp , old_fv ) ;
   }

   /*-- default is no change --*/

   return old_fv ;
}

/*------------------------------------------------------------------------
   Forward transform a vector following a warp
--------------------------------------------------------------------------*/

THD_fvec3 AFNI_forward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* forward transform each possible case,
            and test if result is in bot..top of defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map    = warp->tal_12.warp[iw] ;
            new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;

            if( new_fv.xyz[0] >= map.bot.xyz[0] &&
                new_fv.xyz[1] >= map.bot.xyz[1] &&
                new_fv.xyz[2] >= map.bot.xyz[2] &&
                new_fv.xyz[0] <= map.top.xyz[0] &&
                new_fv.xyz[1] <= map.top.xyz[1] &&
                new_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;
      }
      break ;

   }
   return new_fv ;
}

/*------------------------------------------------------------------------
   Backward transform a vector following a warp
--------------------------------------------------------------------------*/

THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* test if input is in bot..top of each defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map = warp->tal_12.warp[iw] ;

            if( old_fv.xyz[0] >= map.bot.xyz[0] &&
                old_fv.xyz[1] >= map.bot.xyz[1] &&
                old_fv.xyz[2] >= map.bot.xyz[2] &&
                old_fv.xyz[0] <= map.top.xyz[0] &&
                old_fv.xyz[1] <= map.top.xyz[1] &&
                old_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

   }
   return new_fv ;
}

/*------------------------------------------------------------------------
  09 May 2001: fix a Solaris stupidity, where the scale is resized
               improperly when the Define Function panel is opened!
--------------------------------------------------------------------------*/

#ifdef FIX_SCALE_SIZE_LATER
static void fixscale( XtPointer client_data , XtIntervalId * id )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   FIX_SCALE_SIZE(im3d) ;

#if 0
   XtVaSetValues( im3d->vwid->func->thr_scale , XmNscaleWidth,24 , NULL ) ;
#endif
}
#endif

/*------------------------------------------------------------------------*/

void AFNI_define_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   int vwarp ;

ENTRY("AFNI_define_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /*-----  define marks panel -----*/

   if( w == im3d->vwid->view->define_marks_pb ){

      AFNI_viewing_widgets  * view  = im3d->vwid->view  ;
      AFNI_marks_widgets    * marks = im3d->vwid->marks ;

      if( XtIsManaged(marks->frame) == True ){  /* close it down */

STATUS("closing marks") ;

         AFNI_marks_action_CB( NULL , (XtPointer) im3d , NULL ) ;

      } else {                                  /* open it up */

STATUS("opening marks") ;

         marks->old_visible = marks->ov_visible ;
         marks->ov_visible  = True ;
         marks->changed     = False ;  /* not changed yet! */

         MCW_set_bbox( marks->edits_bbox , 0 ) ;
         AFNI_marks_edits_CB( NULL , (XtPointer) im3d , NULL ) ;

         MCW_set_bbox( view->see_marks_bbox , 1 ) ;
         if( marks->old_visible != marks->ov_visible )
            AFNI_see_marks_CB( NULL , (XtPointer) im3d , NULL ) ;

         /* Oct 1998: turn off some controls if no markers present */

         if( im3d->anat_now->markers == NULL ){
            SENSITIZE( marks->edits_bbox->wrowcol , False ) ;
            SENSITIZE( marks->tlrc_big_bbox->wrowcol , False ) ;
            AV_SENSITIZE( marks->disp_scolor_av , False ) ;
            AV_SENSITIZE( marks->disp_size_av   , False ) ;
            AV_SENSITIZE( marks->disp_gap_av    , False ) ;
            SENSITIZE( marks->action_rowcol , False ) ;
            SENSITIZE( marks->transform_pb , False ) ;
         } else {
            vwarp = WARPED_VIEW(im3d->vinfo->view_type) ;
            SENSITIZE( marks->edits_bbox->wrowcol ,
                            (Boolean) ISVALID_VIEW(vwarp) ) ;
            SENSITIZE( marks->tlrc_big_bbox->wrowcol ,
                       (Boolean) (vwarp==VIEW_TALAIRACH_TYPE) ) ;
            AV_SENSITIZE( marks->disp_scolor_av , True ) ;
            AV_SENSITIZE( marks->disp_size_av   , True ) ;
            AV_SENSITIZE( marks->disp_gap_av    , True ) ;
            SENSITIZE( marks->action_rowcol , True ) ;
         }

   /*** I don't know why this is needed, but it prevents the
        marks panels geometry from getting screwed up, so it's here ***/

#ifndef USING_LESSTIF
#define REMANAGE_MARKS 1
#else
#define REMANAGE_MARKS 0
#endif

#if 1
       { static int first=1 ;
         if( REMANAGE_MARKS || first ){               /* CYGWIN: must do 1st time in */
           XtUnmanageChild( marks->rowcol ) ;         /* but not on later times --   */
           XtUnmanageChild( marks->tog_rowcol ) ;     /* probably a LessTif bug      */
           XtUnmanageChild( marks->control_rowcol ) ;
           XtUnmanageChild( marks->control_frame ) ;
           XtUnmanageChild( marks->tog_frame ) ;
           first = 0 ;
         }
       }
#endif

         OPEN_PANEL(im3d,marks) ;

#if 1
#if 0
         XFlush( XtDisplay(marks->rowcol) ) ; XSync( XtDisplay(marks->rowcol),False ) ;
#endif
         if( im3d->anat_now->markers != NULL ){  /* Oct 1998 */
            XtManageChild( marks->tog_rowcol ) ;
            XtManageChild( marks->tog_frame ) ;
         }
         XtManageChild( marks->control_rowcol ) ;
         XtManageChild( marks->control_frame ) ;
         XtManageChild( marks->rowcol ) ;
#endif

      /* redraw markers if not visible already (if there are any to redraw) */

         if( marks->old_visible != True &&
             im3d->anat_now->markers != NULL &&
             im3d->anat_now->markers->numset > 0 )

            AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
      }

      EXRETURN ;
   }

   /*----- define function panel -----*/

   if( w == im3d->vwid->view->define_func_pb ){
      AFNI_viewing_widgets  * view  = im3d->vwid->view  ;
      AFNI_function_widgets * func  = im3d->vwid->func ;

      if( XtIsManaged(func->frame) ){

STATUS("closing function") ;

         CLOSE_PANEL(im3d,func) ;
      } else {
         Boolean have_fim = ISVALID_3DIM_DATASET(im3d->fim_now) ;
         Boolean have_thr = have_fim && FUNC_HAVE_THR(im3d->fim_now->func_type) ;

STATUS("opening function" ) ;

#ifndef USING_LESSTIF
#define REMANAGE_FUNC
#endif

#ifdef REMANAGE_FUNC
         XtUnmanageChild( im3d->vwid->func->rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->thr_rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->inten_rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->options_rowcol ) ;
#endif

         OPEN_PANEL(im3d,func) ;

#ifdef REMANAGE_FUNC
         if( have_thr ) XtManageChild( im3d->vwid->func->thr_rowcol ) ;
         XtManageChild( im3d->vwid->func->inten_rowcol ) ;
         XtManageChild( im3d->vwid->func->options_rowcol ) ;
         XtManageChild( im3d->vwid->func->rowcol ) ;
#endif
         HIDE_SCALE(im3d) ;
         update_MCW_pbar( im3d->vwid->func->inten_pbar ) ;
         FIX_SCALE_SIZE(im3d) ; FIX_SCALE_VALUE(im3d) ;

#ifdef FIX_SCALE_SIZE_LATER
        (void) XtAppAddTimeOut( MAIN_app,50,fixscale,im3d ) ; /* 09 May 2001 */
#endif

/***     XtManageChild( im3d->vwid->func->inten_bbox->wrowcol ) ; ***/
      }

      EXRETURN ;
   }

   /*-- define datamode panel --*/

   if( w == im3d->vwid->view->define_dmode_pb ){
      AFNI_viewing_widgets  * view  = im3d->vwid->view  ;
      AFNI_datamode_widgets * dmode = im3d->vwid->dmode ;

      if( XtIsManaged(dmode->frame) ){

STATUS("closing dmode") ;

         CLOSE_PANEL(im3d,dmode) ;
      } else {

STATUS("opening dmode" ) ;

         OPEN_PANEL(im3d,dmode) ;
      }

      EXRETURN ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_marks_edits_CB( Widget w ,
                          XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   AFNI_marks_widgets * marks ;
   int bval , vwarp ;
   Boolean transformable ;

ENTRY("AFNI_marks_edits_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   marks = im3d->vwid->marks ;
   bval  = MCW_val_bbox( marks->edits_bbox ) ;

   marks->editable = (bval == 0) ? (False) : (True) ;

   if( im3d->anat_now->markers == NULL ) EXRETURN ;

   /*----- allow transformation if
             edits are allowed, AND
             the markers are flagged for it, AND
             all the markers are defined  ----------*/

   vwarp = WARPED_VIEW(im3d->vinfo->view_type) ;

   transformable =
      marks->editable                                         &&
      (im3d->anat_now->markers->aflags[1] != MARKACTION_NONE) &&
      (im3d->anat_now->markers->numdef == im3d->anat_now->markers->numset) &&
      ISVALID_VIEW(vwarp) ;

   /* turn some buttons on or off, depending on editability */

   SENSITIZE( marks->tog_frame         , True ) ;
   SENSITIZE( marks->action_set_pb     , marks->editable ) ;
   SENSITIZE( marks->action_clear_pb   , marks->editable ) ;
   SENSITIZE( marks->pop_set_pb        , marks->editable ) ;
   SENSITIZE( marks->pop_clear_pb      , marks->editable ) ;
   SENSITIZE( marks->action_quality_pb , transformable ) ;
   SENSITIZE( marks->transform_pb      , False ) ;  /* require QC first */

   if( ! marks->editable ){
      AFNI_set_tog( -1 , MARKS_MAXNUM , marks->tog ) ;    /* none will */
      AFNI_set_tog( -1 , MARKS_MAXNUM , marks->poptog ) ; /* be "on" */
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_see_marks_CB( Widget w ,
                        XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   AFNI_marks_widgets * marks ;
   AFNI_viewing_widgets * view ;
   int bval ;

ENTRY("AFNI_see_marks_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   view  = im3d->vwid->view ;
   marks = im3d->vwid->marks ;
   bval  = MCW_val_bbox( view->see_marks_bbox ) ;

   marks->tag_visible = marks->ov_visible = (bval == 0) ? (False) : (True) ;

   if( w != NULL ){
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------
  callback for non-marker buttons on the popup
--------------------------------------------------------------------*/

void AFNI_imag_pop_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;

ENTRY("AFNI_imag_pop_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /*-- jump back to old location --*/

   if( w == im3d->vwid->imag->pop_jumpback_pb ){
     int ij,jj,kk ;

     ij = im3d->vinfo->i1_old ;  /* extract old place */
     jj = im3d->vinfo->j2_old ;
     kk = im3d->vinfo->k3_old ;

     SAVE_VPT(im3d) ;  /* save current place as old one */
     AFNI_set_viewpoint( im3d , ij,jj,kk , REDISPLAY_OVERLAY ) ; /* jump */
   }

   /*-- switch window display mode --*/

   if( w == im3d->vwid->imag->pop_imageonly_pb ){
      MCW_imseq * seq ;
      XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;
      if( ISQ_REALZ(seq) )
         drive_MCW_imseq( seq , isqDR_onoffwid , (XtPointer) isqDR_togwid ) ;
   }

   /*-- jump to a point --*/

   if( w == im3d->vwid->imag->pop_jumpto_pb &&
       im3d->type == AFNI_3DDATA_VIEW         ){

      MCW_imseq * seq ;
      char tbuf[128] ;

      XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;
      if( ISQ_REALZ(seq) ){
         sprintf(tbuf , "Enter new x y z (%s mm):" , GLOBAL_library.cord.orcode ) ;
         MCW_choose_string( seq->wbar , tbuf , NULL ,
                            AFNI_jumpto_CB , (XtPointer) im3d ) ;
      }
   }

   if( w == im3d->vwid->imag->pop_jumpto_ijk_pb &&
       im3d->type == AFNI_3DDATA_VIEW             ){

      MCW_imseq * seq ;

      XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;
      if( ISQ_REALZ(seq) ){
         MCW_choose_string( seq->wbar , "Enter new i j k:" , NULL ,
                            AFNI_jumpto_ijk_CB , (XtPointer) im3d ) ;
      }
   }

   /*-- 06 Mar 2002: jump to a node in a surface --*/

   if( w == im3d->vwid->imag->pop_sumato_pb &&
       im3d->anat_now->su_surf != NULL      &&
       im3d->type == AFNI_3DDATA_VIEW         ){

      MCW_imseq * seq ;

      XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;
      if( ISQ_REALZ(seq) ){
         MCW_choose_string( seq->wbar , "Enter SUMA node ID:" , NULL ,
                            AFNI_sumato_CB , (XtPointer) im3d ) ;
      }
   }

   /*-- jump to a predetermined Talairach anatomical reference point --*/

   if( w == im3d->vwid->imag->pop_talto_pb &&
       im3d->type == AFNI_3DDATA_VIEW      &&
       CAN_TALTO(im3d)                       ){

      MCW_imseq * seq ;
      XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;

      if( ! TTO_labeled ){  /* initialize labels */
         int ii ;
         for( ii=0 ; ii < TTO_COUNT ; ii++ ){
            TTO_labels[ii] = (char *) malloc( sizeof(char) * TTO_LMAX ) ;
            sprintf( TTO_labels[ii] , TTO_FORMAT , TTO_list[ii].name ,
                     TTO_list[ii].xx , TTO_list[ii].yy , TTO_list[ii].zz ) ;
         }
         TTO_labeled = 1 ;
      }
      if( ISQ_REALZ(seq) )
         MCW_choose_strlist( seq->wbar ,
                             "Brain Structure (from San Antonio Talairach Daemon)" ,
                             TTO_COUNT , TTO_current , TTO_labels ,
                             AFNI_talto_CB , (XtPointer) im3d ) ;
   }

   /*---- 10 Jul 2001: Talairach "Where Am I?" ----*/

   else if( w == im3d->vwid->imag->pop_whereami_pb &&
            w != NULL                              &&
            im3d->type == AFNI_3DDATA_VIEW         &&
            CAN_TALTO(im3d)                          ){

      MCW_imseq *seq ; char *tlab ;

      /*- if one is already open, kill it -*/

      if( im3d->vwid->imag->pop_whereami_twin != NULL ){
         MCW_textwinkill_CB(NULL,
                            (XtPointer)im3d->vwid->imag->pop_whereami_twin,NULL);
         im3d->vwid->imag->pop_whereami_twin == NULL ;
      }

      /*- get TT atlas location, if any -*/

      tlab = AFNI_ttatlas_query( im3d ) ;

      /*- open a window to show it -*/

      if( tlab != NULL ){
         XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;

         im3d->vwid->imag->pop_whereami_twin =
           new_MCW_textwin_2001( seq->wbar , tlab , TEXT_READONLY ,
                                 AFNI_pop_whereami_kill , im3d     ) ;

#if 0
         /* 31 Jul 2001: NULL out the pointer when the window is destroyed */

         NULLIFY_ON_DESTROY( im3d->vwid->imag->pop_whereami_twin ,
                             im3d->vwid->imag->pop_whereami_twin->wshell ) ;
#endif

         XtVaSetValues( im3d->vwid->imag->pop_whereami_twin->wtext ,
                          XmNresizeHeight , True ,
                          XmNresizeWidth  , True ,
                        NULL ) ;

         MCW_register_hint( im3d->vwid->imag->pop_whereami_twin->wtext ,
                            "Use BHelp for documentation" ) ;

         MCW_register_help( im3d->vwid->imag->pop_whereami_twin->wtext ,
          "Lists the brain structures near the crosshair focus point\n"
          "according to the Talairach Daemon database (kindly provided\n"
          "by Jack Lancaster and Peter Fox of RIC UTHSCSA).\n"
          "\n"
          "The search is conducted outwards from the focus point, until\n"
          "9 different structures are found, or a 7 mm radius is reached,\n"
          "whichever occurs first. (Distances are rounded to nearest 1 mm,\n"
          "the grid spacing on which the database is constructed.) Labels\n"
          "reported on different output lines came from different voxels.\n"
          "\n"
          "In the database, some voxels have 2 labels - a larger scale\n"
          "'gyral' name and a finer scale 'area' name.  Locations that\n"
          "are doubly labeled will appear with a listing like\n"
          "    Within 2 mm: Right Precuneus -AND- Right Brodmann area 31\n"
          "In the database there are\n"
          "    1,205,737 voxels with at least one label\n"
          "      709,953 voxels with only a 'gyral' label\n"
          "       15,898 voxels with only a 'area' label\n"
          "      479,886 voxels with both types of labels\n"
          "A list of all the labels (of either type) is presented by the\n"
          "'Talairach to' control.  In the database, there are\n"
          "           50 'gyral' labels (times 2 for Left and Right)\n"
          "           68 'area' labels\n"
          "          355 distinct combinations of labels\n"
          "Note Very Well:\n"
          "* This feature of AFNI is experimental, and is subject to change.\n"
          "* The Atlas is only useful as a ROUGH guide to determining where\n"
          "    you are in any individual brain.  Do not rely exclusively on\n"
          "    the Atlas for brain region identification: you must use your\n"
          "    knowledge, skills, and abilities as well.\n"
          "* Do NOT use this feature for surgical or therapeutic planning!!!"
         ) ;

         free(tlab) ;
      }
   }

   /*---- 12 Jul 2001 ----*/

   else if( w == im3d->vwid->imag->pop_ttren_pb           &&
            w != NULL                                     &&
            im3d->type == AFNI_3DDATA_VIEW                &&
            im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE && /* 01 Aug 2001 */
            CAN_TALTO(im3d)                                 ){

      TTRR_popup( im3d ) ;
   }

   /*--- unmap of the popup itself ---*/

   /*--- exit ---*/

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   called when the talto chooser is set
-----------------------------------------------------------------------*/

void AFNI_talto_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   THD_dataxes  * daxes ;
   float xx,yy,zz ;
   int nn , ii,jj,kk ;
   THD_fvec3 fv,tv ; THD_ivec3 iv ;

ENTRY("AFNI_talto_CB") ;

   /* check for errors */

   if( ! IM3D_VALID(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;

   if( !CAN_TALTO(im3d)             ||
       cbs->reason != mcwCR_integer   ){

      POPDOWN_strlist_chooser ;
      XBell( im3d->dc->display , 100 ) ;
      EXRETURN ;
   }

   nn = cbs->ival ;
   if( nn < 0 || nn >= TTO_COUNT ) EXRETURN ;
   TTO_current = nn ;

   /* transform point from Dicom to local coords and go there */

   xx = TTO_list[nn].xx ; yy = TTO_list[nn].yy ; zz = TTO_list[nn].zz ;

   LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

   LOAD_FVEC3(tv,xx,yy,zz) ; /* Talairach coords */

   /* 09 Jul 2001: if not now viewing in Talairach coordinates,
                   then transform vector to current coordinates */

   if( im3d->anat_now->view_type != VIEW_TALAIRACH_TYPE )
      tv = AFNI_transform_vector( im3d->anat_dset[VIEW_TALAIRACH_TYPE] ,
                                  tv , im3d->anat_now ) ;

   fv = THD_dicomm_to_3dmm( im3d->anat_now , tv ) ;
   iv = THD_3dmm_to_3dind ( im3d->anat_now , fv ) ;
   ii = iv.ijk[0] ; jj = iv.ijk[1] ; kk = iv.ijk[2] ;

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz   ){

      SAVE_VPT(im3d) ;
      AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
   } else {
      XBell( im3d->dc->display , 100 ) ;
   }
   EXRETURN ;
}

/*-------------------------------------------------------------------------
   10 Jul 2001
---------------------------------------------------------------------------*/

void AFNI_pop_whereami_kill( Three_D_View * im3d )
{
   if( im3d == NULL ) return ;

   MCW_unregister_hint( im3d->vwid->imag->pop_whereami_twin->wtext ) ;
   MCW_unregister_help( im3d->vwid->imag->pop_whereami_twin->wtext ) ;

   im3d->vwid->imag->pop_whereami_twin = NULL ;
   return ;
}

/*-------------------------------------------------------------------------*/

char * AFNI_ttatlas_query( Three_D_View * im3d )
{
   static int have_TT = -1 ;

   if( !IM3D_OPEN(im3d) || !CAN_TALTO(im3d) ) return NULL ;

   /*-- make sure we have the TT atlas --*/

   if( have_TT == -1 ){
      have_TT = TT_load_atlas() ;
      if( !have_TT ) return NULL ;
   }

   if( have_TT ){
     THD_fvec3 tv ; char *tlab ;

     /*-- current position --*/

     LOAD_FVEC3(tv,im3d->vinfo->xi,im3d->vinfo->yj,im3d->vinfo->zk) ;

     /*-- transform to Talairach, if needed --*/

     if( im3d->anat_now->view_type != VIEW_TALAIRACH_TYPE )
        tv = AFNI_transform_vector( im3d->anat_now , tv ,
                                    im3d->anat_dset[VIEW_TALAIRACH_TYPE] ) ;

     /*-- get result string --*/

     tlab = TT_whereami( tv.xyz[0] , tv.xyz[1] , tv.xyz[2] ) ;
     return tlab ;
   }

   return NULL ;
}

/*-------------------------------------------------------------------------
  See the TT atlas in the overlay? -- 25 Jul 2001
---------------------------------------------------------------------------*/

void AFNI_see_ttatlas_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int newsee = MCW_val_bbox(im3d->vwid->func->see_ttatlas_bbox) ;

   if( newsee == im3d->vinfo->see_ttatlas ) return ;

   im3d->vinfo->see_ttatlas = newsee ;

   if( im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE )
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */

   return ;
}

/*---------------------------------------------------------------------
   called when the jumpto chooser is set
-----------------------------------------------------------------------*/

void AFNI_jumpto_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   float xx,yy,zz ;
   char dum1[32],dum2[32];
   int nn ;

ENTRY("AFNI_jumpto_CB") ;

   if( ! IM3D_VALID(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string ) EXRETURN ;  /* error */

   nn = sscanf( cbs->cval , "%f%[ ,]%f%[ ,]%f" , &xx,dum1,&yy,dum2,&zz ) ;
   if( nn != 5 ){ XBell( im3d->dc->display , 100 ) ; EXRETURN ; }

   THD_coorder_to_dicom( &GLOBAL_library.cord , &xx,&yy,&zz ) ;

   nn = AFNI_jumpto_dicom( im3d , xx,yy,zz ) ;
   if( nn < 0 ) XBell( im3d->dc->display , 100 ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

int AFNI_jumpto_dicom( Three_D_View * im3d , float xx, float yy, float zz )
{
   THD_dataxes  * daxes ;
   THD_fvec3 fv ; THD_ivec3 iv ;
   int ii,jj,kk ;

ENTRY("AFNI_jumpto_dicom") ;

   LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

   fv = THD_dicomm_to_3dmm( im3d->anat_now , TEMP_FVEC3(xx,yy,zz) ) ;
   iv = THD_3dmm_to_3dind ( im3d->anat_now , fv ) ;
   ii = iv.ijk[0] ; jj = iv.ijk[1] ; kk = iv.ijk[2] ;

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz ){

      SAVE_VPT(im3d) ;
      AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
      RETURN(1) ;
   } else {
      XBell( im3d->dc->display , 100 ) ;
      RETURN(-1) ;
   }
}

/*-- the two functions below date to 19 Aug 1999 --*/

int AFNI_jumpto_ijk( Three_D_View * im3d , int ii, int jj, int kk )
{
   THD_dataxes * daxes ;

ENTRY("AFNI_jumpto_ijk") ;

   LOAD_ANAT_VIEW(im3d) ;

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz ){

      SAVE_VPT(im3d) ;
      AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
      RETURN(1) ;
   } else {
      XBell( im3d->dc->display , 100 ) ;
      RETURN(-1) ;
   }
}

void AFNI_jumpto_ijk_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int ii,jj,kk ;
   int nn ;
   char dum1[32],dum2[32];

ENTRY("AFNI_jumpto_CB") ;

   if( ! IM3D_VALID(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string ) EXRETURN ;  /* error */

   nn = sscanf( cbs->cval , "%d%[ ,]%d%[ ,]%d" , &ii,dum1,&jj,dum2,&kk ) ;
   if( nn != 5 ){ XBell( im3d->dc->display , 100 ) ; EXRETURN ; }

   nn = AFNI_jumpto_ijk( im3d , ii,jj,kk ) ;
   if( nn < 0 ) XBell( im3d->dc->display , 100 ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   called when the sumato chooser is set
-----------------------------------------------------------------------*/

void AFNI_sumato_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int nn , ii ;

ENTRY("AFNI_sumato_CB") ;

   if( !IM3D_VALID(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string ) EXRETURN ;  /* error */
   if( im3d->anat_now->su_surf == NULL ) EXRETURN ;

   nn = -1 ;
   sscanf( cbs->cval , "%d" , &nn ) ;
   ii = SUMA_find_node_id( im3d->anat_now->su_surf , nn ) ;
   if( ii < 0 ){ XBell(im3d->dc->display,100); EXRETURN; }

   (void) AFNI_jumpto_dicom( im3d ,
                             im3d->anat_now->su_surf->ixyz[ii].x ,
                             im3d->anat_now->su_surf->ixyz[ii].y ,
                             im3d->anat_now->su_surf->ixyz[ii].z  ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   Transform current dataset based on the existing set of markers
-----------------------------------------------------------------------*/

#define BEEP_AND_RETURN { XBell(XtDisplay(w),100); EXRETURN ; }

void AFNI_marks_transform_CB( Widget w ,
                              XtPointer client_data , XtPointer call_data )
{
   Three_D_View     * im3d = (Three_D_View *) client_data ;
   THD_marker_set   * markers ;
   THD_warp         * warp ;
   THD_3dim_dataset * new_dset ;
   THD_session      * ss ;
   int                vnew , vvv , sss , aaa , fff , id ;
   float              resam_size ;
   Widget             wmsg ;

ENTRY("AFNI_marks_transform_CB") ;

   /*--- sanity checks ---*/

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   markers = im3d->anat_now->markers ;
   if(markers == NULL || markers->aflags[1] != MARKACTION_WARP) BEEP_AND_RETURN ;

   vnew = WARPED_VIEW(im3d->vinfo->view_type) ; /* view index of new dataset */
   if( !ISVALID_VIEW(vnew) ) BEEP_AND_RETURN ;

   /*--- make warp ---*/

   warp = AFNI_make_warp( im3d ) ;
   if( warp == NULL ) BEEP_AND_RETURN ;

   /*--- create new dataset (empty at this point) ---*/

   resam_size = im3d->vinfo->resam_vox ;
   new_dset   = AFNI_init_warp( im3d , im3d->anat_now , warp , resam_size ) ;
   if( new_dset == NULL ) BEEP_AND_RETURN ;

   { char his[128] ;
     tross_Copy_History( im3d->anat_now , new_dset ) ;
     sprintf(his,"afni: transformed to %s",VIEW_typestr[vnew]) ;
     tross_Append_History( new_dset , his ) ;
   }

   /*----- This new dataset may replace a current dataset,
           and if so, THAT dataset may have a warp child,
           and so on.  Mark those datasets for destruction,
           mark their anatomy children for destruction, and destroy them -----*/

   vvv = vnew ;
   while( ISVALID_VIEW(vvv) && ISVALID_3DIM_DATASET(im3d->anat_dset[vvv]) ){
      DSET_MARK_FOR_DEATH( im3d->anat_dset[vvv] ) ;
      vvv = WARPED_VIEW(vvv) ;
   }

   AFNI_mark_for_death(GLOBAL_library.sslist ) ;        /* find descendants */
   AFNI_andersonville (GLOBAL_library.sslist , True ) ; /* kill (including files) */

   /*----- Can now place the new dataset into its rightful place -----*/

   sss = im3d->vinfo->sess_num ;
   aaa = im3d->vinfo->anat_num ;
   fff = im3d->vinfo->func_num ;
   GLOBAL_library.sslist->ssar[sss]->anat[aaa][vnew] = new_dset ;

   /* reload active datasets, to allow for destruction that may
      have occured (this code is copied from AFNI_initialize_view) */

   for( id=0 ; id <= LAST_VIEW_TYPE ; id++ ){
      im3d->anat_dset[id] = GLOBAL_library.sslist->ssar[sss]->anat[aaa][id] ;
      im3d->fim_dset[id]  = GLOBAL_library.sslist->ssar[sss]->func[fff][id] ;

      if( ISVALID_3DIM_DATASET(im3d->anat_dset[id]) )
         SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], True ) ;
      else
         SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], False) ;
   }

STATUS("writing new dataset") ;

   (void) THD_write_3dim_dataset( NULL,NULL , new_dset , False ) ; /* header */

   /*--- have transformed this anatomy dataset
         ==> if the input was the original view, then
             destroy any other marker sets in the original view
             and make their datasets anatomical children of this one ---*/

   if( im3d->vinfo->view_type == VIEW_ORIGINAL_TYPE ){
      int id ;
      THD_3dim_dataset * dss ;

      /* perform surgery on the anat datasets in this session */

STATUS("re-anat_parenting anatomical datasets in this session") ;

      for( id=0 ; id < im3d->ss_now->num_anat ; id++ ){
         dss = im3d->ss_now->anat[id][0] ;

         if( ! ISVALID_3DIM_DATASET(dss) || dss == im3d->anat_now ) continue ;

         if( dss->markers != NULL ) SINGLE_KILL(dss->kl,dss->markers) ;
         dss->markers = NULL ;

         if( dss->anat_parent == NULL ){
            dss->anat_parent = im3d->anat_now ;
            MCW_strncpy( dss->anat_parent_name ,
                         im3d->anat_now->self_name , THD_MAX_NAME ) ;
#ifndef OMIT_DATASET_IDCODES
            dss->anat_parent_idcode = im3d->anat_now->idcode ;
#endif
         }
      }

      /* and on the functional datasets */

STATUS("re-anat_parenting functional datasets in this session") ;

      for( id=0 ; id < im3d->ss_now->num_func ; id++ ){
         dss = im3d->ss_now->func[id][0] ;

         if( ! ISVALID_3DIM_DATASET(dss) ) continue ;
         if( dss->anat_parent == NULL ){
            dss->anat_parent = im3d->anat_now ;
            MCW_strncpy( dss->anat_parent_name ,
                         im3d->anat_now->self_name , THD_MAX_NAME ) ;
#ifndef OMIT_DATASET_IDCODES
            dss->anat_parent_idcode = im3d->anat_now->idcode ;
#endif
         }
      }
   }

   /*--- now that we have a new potential parent (the warped dataset),
         and maybe some links to its warp_parent (done just above),
         try to make some descendants (followup warps) from it     ---*/

   AFNI_make_descendants( GLOBAL_library.sslist ) ;

   /*--- clean up some stuff ---*/

   if( GLOBAL_argopt.auto_purge == True ) AFNI_purge_unused_dsets() ;

   /* Jan 31, 1995: force adoption of any dataset
                    that was orphaned at Andersonville */

   for( sss=0 ; sss < GLOBAL_library.sslist->num_sess ; sss++ ){
      ss = GLOBAL_library.sslist->ssar[sss] ;
      if( ISVALID_SESSION(ss) ) AFNI_force_adoption( ss , GLOBAL_argopt.warp_4D ) ;
   }

   /*---  close marks panel and exit ---*/

#if 0
   XtSetSensitive( im3d->vwid->top_shell , True ) ;
   SHOW_AFNI_READY ;
#endif

   AFNI_marks_action_CB( NULL , (XtPointer) im3d , NULL ) ;

   MPROBE ;
   EXRETURN ;
}

/*-------------------------------------------------------------------
   Compute the warp.

   Notice the convention: the warp is always stored as Dicom-to-Dicom
   coordinates.  For actual use, it must be converted into the
   correct form for 3dmm-to-3dmm, or 3dfind-to-3dfind, etc.

   The reason for this convention is that anatomical warps will be
   applied to functional datasets, which will in general be oriented
   differently from the anatomical dataset that the warp was computed
   upon.  By keeping the warp in the generic Dicom system, this
   application of a warp to another dataset will be possible.
---------------------------------------------------------------------*/

/* macro to extract the im-th marker vector into a THD_fvec3 structure */

#define MVEC(im) \
 TEMP_FVEC3(markers->xyz[im][0],markers->xyz[im][1],markers->xyz[im][2])

THD_warp * AFNI_make_warp( Three_D_View * im3d )
{
   THD_3dim_dataset * anat    = im3d->anat_now ;
   THD_marker_set   * markers = im3d->anat_now->markers ;
   THD_warp         * warp ;
   Boolean good ;

ENTRY("AFNI_make_warp") ;

   /*--- check the markers for OK-osity ---*/

   good = AFNI_marks_quality_check( False , im3d ) ;
   if( !good ) RETURN(NULL) ;

   /*--- make a new warp, and then construct it,
         based on the type of marker set we have here ---*/

   warp = myXtNew( THD_warp ) ;

   switch( markers->type ){  /* type of marker set */

      default: RETURN(NULL) ;      /* something bad happened */

      /*--- bounding box markers set ---*/

      case MARKSET_BOUNDING:{
         THD_talairach_12_warp * twarp = (THD_talairach_12_warp *) warp ;
         THD_fvec3 mant,mpos,msup,minf,mrig,mlef , pcie ;
         float dist_sup , dist_inf , dist_ant , dist_med , dist_pos ,
               dist_lef , dist_rig ;
         float scale_S , scale_I , scale_A , scale_M , scale_P ,
               scale_L , scale_R , shift_P ;
         float bot_S   , bot_I   , bot_A   , bot_M   , bot_P ,
               bot_L   , bot_R ;
         float top_S   , top_I   , top_A   , top_M   , top_P ,
               top_L   , top_R ;
         THD_fvec3 bv_A , bv_M , bv_P , sv_A , sv_M , sv_P ;

         /* let the world know what kind of warp is being built */

         twarp->type = WARP_TALAIRACH_12_TYPE ;

         /* extract the marker vectors, put in Dicom coords */

         mant = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MANT) ) ;
         mpos = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MPOS) ) ;
         msup = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MSUP) ) ;
         minf = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MINF) ) ;
         mrig = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MRIG) ) ;
         mlef = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MLEF) ) ;

         /* convert Posterior Commissure from parent to current coords */

         LOAD_FVEC3( pcie ,
                     anat->warp_parent->markers->xyz[IMARK_PCIE][0] ,
                     anat->warp_parent->markers->xyz[IMARK_PCIE][1] ,
                     anat->warp_parent->markers->xyz[IMARK_PCIE][2]  ) ;

         pcie = THD_3dmm_to_dicomm( anat->warp_parent , pcie ) ;
         pcie = AFNI_transform_vector( anat->warp_parent , pcie , anat ) ;

         /* compute distances between points in various directions */

         dist_ant = -mant.xyz[1] ;
         dist_med =  pcie.xyz[1] ;
         dist_pos =  mpos.xyz[1] - pcie.xyz[1] ;

         dist_sup =  msup.xyz[2] ;
         dist_inf = -minf.xyz[2] ;
         dist_lef =  mlef.xyz[0] ;
         dist_rig = -mrig.xyz[0] ;

         /* from these, compute the scalings needed in each
            direction and the shift needed posterior to the PC */

         scale_A = ATLAS_FRONT_TO_AC / dist_ant ;
         scale_M = ATLAS_AC_TO_PC    / dist_med ;
         scale_P = ATLAS_PC_TO_BACK  / dist_pos ;
         scale_S = ATLAS_AC_TO_TOP   / dist_sup ;
         scale_I = ATLAS_BOT_TO_AC   / dist_inf ;
         scale_L = ATLAS_AC_TO_LAT   / dist_lef ;
         scale_R = ATLAS_AC_TO_LAT   / dist_rig ;

         shift_P = scale_P * dist_med - ATLAS_AC_TO_PC ;

         /* shift vectors in each direction, for each y cell (A,M,P) */

         LOAD_FVEC3( bv_A , 0,0,0 ) ; bv_M = sv_A = sv_M = bv_A ;

         LOAD_FVEC3( bv_P , 0 , shift_P , 0 ) ;
         LOAD_FVEC3( sv_P , 0 , -shift_P / scale_P , 0 ) ;

         /* bounds information for each direction, for each cell */

         bot_A = -9999.0        ; top_A = 0.0 ;
         bot_M =     0.0        ; top_M = ATLAS_AC_TO_PC ;
         bot_P = ATLAS_AC_TO_PC ; top_P = 9999.0 ;

         bot_R = -9999.0        ; top_R =    0.0 ;
         bot_L =     0.0        ; top_L = 9999.9 ;

         bot_I = -9999.0        ; top_I =    0.0 ;
         bot_S =     0.0        ; top_S = 9999.9 ;

         /* Compute the 12 linear maps:
               They are all linear scalings (diagonal matrices);
               posterior to the PC, they also contain shifts
               to align stuff to the nominal PC location.
            N.B.: these are maps from AC-PC aligned coordinates
                  to the Talairach system.  Maps from the
                  original data to the Talairach system
                  will be computed later (in AFNI_concatenate_warps). */

   /* ------- a macro to automate map making:
              xx = R or L , yy = A, M, or P , zz = I or S --------*/

#define MAKE_MAP(xx,yy,zz) \
(\
   LOAD_DIAG_MAT( twarp->warp[W_ ## xx ## yy ## zz].mfor ,      \
                  scale_ ## xx , scale_ ## yy , scale_ ## zz ) ,\
\
   LOAD_DIAG_MAT( twarp->warp[W_ ## xx ## yy ## zz].mbac ,                 \
           1.0 / scale_ ## xx , 1.0 / scale_ ## yy , 1.0 / scale_ ## zz ) ,\
\
   twarp->warp[W_ ## xx ## yy ## zz].bvec = bv_ ## yy , \
\
   twarp->warp[W_ ## xx ## yy ## zz].svec = sv_ ## yy , \
\
   LOAD_FVEC3( twarp->warp[W_ ## xx ## yy ## zz].bot ,   \
               bot_ ## xx , bot_ ## yy , bot_ ## zz   ) ,\
\
   LOAD_FVEC3( twarp->warp[W_ ## xx ## yy ## zz].top ,   \
               top_ ## xx , top_ ## yy , top_ ## zz   )  \
)

   /*------- end of MAKE_MAP macro --------*/

         MAKE_MAP(R,A,S) ;   /* right-anterior -superior */
         MAKE_MAP(L,A,S) ;   /* left -anterior -superior */
         MAKE_MAP(R,M,S) ;   /* right-medial   -superior */
         MAKE_MAP(L,M,S) ;   /* left -medial   -superior */
         MAKE_MAP(R,P,S) ;   /* right-posterior-superior */
         MAKE_MAP(L,P,S) ;   /* left -posterior-superior */
         MAKE_MAP(R,A,I) ;   /* right-anterior -inferior */
         MAKE_MAP(L,A,I) ;   /* left -anterior -inferior */
         MAKE_MAP(R,M,I) ;   /* right-medial   -inferior */
         MAKE_MAP(L,M,I) ;   /* left -medial   -inferior */
         MAKE_MAP(R,P,I) ;   /* right-posterior-inferior */
         MAKE_MAP(L,P,I) ;   /* left -posterior-inferior */

#undef MAKE_MAP

      }
      break ; /* end of Bounding markers set */

      /*--- AC-PC alignment markers set ---*/

      case MARKSET_ALIGN:{
         THD_affine_warp * awarp = (THD_affine_warp *) warp ;

         THD_fvec3 acsup , acpos , pcinf , msag1 , msag2 ,
                   alpha1,alpha2,alpha,beta,gamma,rr1,rr2,rr , dif ;
         THD_mat33 to_al ;
         float size ;

         /* let the world know what kind of warp is being built */

         awarp->type = WARP_AFFINE_TYPE ;

         /* extract the marker vectors, put in Dicom coords */

         acsup = THD_3dmm_to_dicomm( anat , MVEC(IMARK_ACSE) ) ;
         acpos = THD_3dmm_to_dicomm( anat , MVEC(IMARK_ACPM) ) ;
         pcinf = THD_3dmm_to_dicomm( anat , MVEC(IMARK_PCIE) ) ;
         msag1 = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MSA1) ) ;
         msag2 = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MSA2) ) ;

         /*--- new y direction (beta) ---*/

         beta = SUB_FVEC3(pcinf,acsup) ;  beta = NORMALIZE_FVEC3(beta) ;

         /*--- new x direction (alpha) ---*/

         rr     = SUB_FVEC3(msag1,acsup) ;
         alpha1 = CROSS_FVEC3(beta,rr) ; alpha1 = NORMALIZE_FVEC3(alpha1) ;

         rr     = SUB_FVEC3(msag2,acsup) ;
         alpha2 = CROSS_FVEC3(beta,rr) ; alpha2 = NORMALIZE_FVEC3(alpha2) ;

         alpha  = SCLADD_FVEC3(0.5,alpha1,0.5,alpha2) ;
         alpha  = NORMALIZE_FVEC3(alpha) ;

         /*--- new z direction (gamma) ---*/

         gamma = CROSS_FVEC3(alpha,beta) ; gamma = NORMALIZE_FVEC3(gamma) ;

         /*--- origin of Talairach coordinates (rr) --*/

         dif  = SUB_FVEC3(acsup,acpos) ;
         size = DOT_FVEC3(dif,gamma) ;
         rr1  = SCLADD_FVEC3(1.0,acpos,size,gamma) ;

         size = DOT_FVEC3(dif,beta) ;
         rr2  = SCLADD_FVEC3(1.0,acsup,-size,beta) ;

         rr   = SCLADD_FVEC3(0.5,rr1,0.5,rr2) ;

         /*--- at this point, have:
                  new origin in rr ;
                  new axes directions in alpha,beta,gamma.
               Now construct the transformation between
               the Dicom coordinate systems ---------------------*/

         to_al.mat[0][0] = alpha.xyz[0] ;  /* first row is alpha */
         to_al.mat[0][1] = alpha.xyz[1] ;
         to_al.mat[0][2] = alpha.xyz[2] ;

         to_al.mat[1][0] = beta.xyz[0] ;   /* second row is beta */
         to_al.mat[1][1] = beta.xyz[1] ;
         to_al.mat[1][2] = beta.xyz[2] ;

         to_al.mat[2][0] = gamma.xyz[0] ;  /* third row is gamma */
         to_al.mat[2][1] = gamma.xyz[1] ;
         to_al.mat[2][2] = gamma.xyz[2] ;

         /*--- put into warp structure ---*/

         awarp->warp.type = MAPPING_LINEAR_TYPE ;
         awarp->warp.mfor = to_al ;
         awarp->warp.mbac = TRANSPOSE_MAT(to_al) ;  /* orthog^(-1) */
         awarp->warp.bvec = MATVEC(to_al,rr) ;
         awarp->warp.svec = rr ;  NEGATE_FVEC3(awarp->warp.svec) ;

         /* load bot & top with largest possible excursions from
            origin (the ALIGNBOX dimensions were added 3/25/95)  */
#if 1
         LOAD_FVEC3(awarp->warp.bot,
                    -ATLAS_ALIGNBOX_LAT,-ATLAS_ALIGNBOX_ANT,-ATLAS_ALIGNBOX_INF);
         LOAD_FVEC3(awarp->warp.top,
                     ATLAS_ALIGNBOX_LAT, ATLAS_ALIGNBOX_POS, ATLAS_ALIGNBOX_SUP);
#else
         LOAD_FVEC3(awarp->warp.bot,
                     -ATLAS_BBOX_LAT,-ATLAS_BBOX_ANT,-ATLAS_BBOX_INF);
         LOAD_FVEC3(awarp->warp.top,
                      ATLAS_BBOX_LAT, ATLAS_BBOX_POS, ATLAS_BBOX_SUP);
#endif

#ifdef AFNI_DEBUG
STATUS("Original -> Aligned Map::") ;
DUMP_LMAP(awarp->warp) ;
#endif

      }  /* end of AC-PC alignment case */
      break ;

   } /* end of switch on marker set type */

   RETURN(warp) ;
}

/*---------------------------------------------------------------------*/

#define ADD_ERROR(str)                                \
   { int ll = strlen(str) + strlen(error_list) + 16 ; \
     STATUS(str) ;                                    \
     error_list = XtRealloc( error_list , ll ) ;      \
     strcat( error_list , "*** ERROR:  ") ;           \
     strcat( error_list , str ) ; num_error++ ; }

#define ADD_REPORT(str)                               \
   { int ll = strlen(str) + strlen(error_list) + 16 ; \
     STATUS(str) ;                                    \
     error_list = XtRealloc( error_list , ll ) ;      \
     strcat( error_list , str ) ; num_report++ ; }

Boolean AFNI_marks_quality_check( Boolean make_report, Three_D_View * im3d )
{
   THD_3dim_dataset * anat    = im3d->anat_now ;
   THD_marker_set   * markers = im3d->anat_now->markers ;

   char *  error_list ;
   int     num_error , num_report ;
   char    msg[128] ;
   Boolean good ;

ENTRY("AFNI_marks_quality_check") ;

   /*--- for compiling a list of errors and/or reports ---*/

   if( markers == NULL ){ BEEPIT ; RETURN(False) ; }  /* should not happen */

   error_list = XtNewString(
                "             *** MARKERS QUALITY REPORT ***           \n\n") ;
   num_error  = 0 ;
   num_report = 0 ;

   /*--- what we do depends on the kinds of markers we have ---*/

   switch( markers->type ){

      default: RETURN(False) ;      /* something bad happened */

      /*--- bounding box markers set ---*/

      case MARKSET_BOUNDING:{
         THD_fvec3 mant,mpos,msup,minf,mrig,mlef , pcie ;
         float dist_sup , dist_inf , dist_ant , dist_med , dist_pos ,
               dist_lef , dist_rig ;

         /* extract the marker vectors, put in Dicom coords */

         mant = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MANT) ) ;
         mpos = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MPOS) ) ;
         msup = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MSUP) ) ;
         minf = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MINF) ) ;
         mrig = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MRIG) ) ;
         mlef = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MLEF) ) ;

         /* convert Posterior Commissure from parent to current coords */

         LOAD_FVEC3( pcie ,
                     anat->warp_parent->markers->xyz[IMARK_PCIE][0] ,
                     anat->warp_parent->markers->xyz[IMARK_PCIE][1] ,
                     anat->warp_parent->markers->xyz[IMARK_PCIE][2]  ) ;

         pcie = THD_3dmm_to_dicomm( anat->warp_parent , pcie ) ;
         pcie = AFNI_transform_vector( anat->warp_parent , pcie , anat ) ;

         /* compute distances between points in various directions */

         dist_ant = -mant.xyz[1] ;
         dist_med =  pcie.xyz[1] ;
         dist_pos =  mpos.xyz[1] - pcie.xyz[1] ;

         dist_sup =  msup.xyz[2] ;
         dist_inf = -minf.xyz[2] ;

         dist_lef =  mlef.xyz[0] ;
         dist_rig = -mrig.xyz[0] ;

         /* check anterior distance and report it */

         if( dist_ant/ATLAS_FRONT_TO_AC < MIN_ALLOWED_DEVIATION ||
             dist_ant/ATLAS_FRONT_TO_AC > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");

         sprintf(msg,"Front to Anterior commissure: %5.1f mm (Atlas:%5.1f)\n",
                 dist_ant,ATLAS_FRONT_TO_AC) ;
         ADD_REPORT(msg) ;

         /* medial */

#if 0
         if( dist_med/ATLAS_AC_TO_PC < MIN_ALLOWED_DEVIATION ||
             dist_med/ATLAS_AC_TO_PC > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");
#endif
         sprintf(msg,"Intercommissural distance:    %5.1f mm (Atlas:%5.1f)\n",
                 dist_med,ATLAS_AC_TO_PC) ;
         ADD_REPORT(msg) ;


         /* posterior */

         if( dist_pos/ATLAS_PC_TO_BACK < MIN_ALLOWED_DEVIATION ||
             dist_pos/ATLAS_PC_TO_BACK > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");

         sprintf(msg,"Posterior commissure to back: %5.1f mm (Atlas:%5.1f)\n",
                 dist_pos,ATLAS_PC_TO_BACK) ;
         ADD_REPORT(msg) ;

         /* inferior */

         if( dist_inf/ATLAS_BOT_TO_AC < MIN_ALLOWED_DEVIATION ||
             dist_inf/ATLAS_BOT_TO_AC > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");

         sprintf(msg,"Bottom to Anterior commissure:%5.1f mm (Atlas:%5.1f)\n",
                 dist_inf,ATLAS_BOT_TO_AC) ;
         ADD_REPORT(msg) ;

         /* superior */

         if( dist_sup/ATLAS_AC_TO_TOP < MIN_ALLOWED_DEVIATION ||
             dist_sup/ATLAS_AC_TO_TOP > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");

         sprintf(msg,"Anterior commissure to top:   %5.1f mm (Atlas:%5.1f)\n",
                 dist_sup,ATLAS_AC_TO_TOP) ;
         ADD_REPORT(msg) ;

         /* left */

         if( dist_lef/ATLAS_AC_TO_LAT < MIN_ALLOWED_DEVIATION ||
             dist_lef/ATLAS_AC_TO_LAT > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");

         sprintf(msg,"Anterior commissure to left:  %5.1f mm (Atlas:%5.1f)\n",
                 dist_lef,ATLAS_AC_TO_LAT) ;
         ADD_REPORT(msg) ;

         /* right */

         if( dist_rig/ATLAS_AC_TO_LAT < MIN_ALLOWED_DEVIATION ||
             dist_rig/ATLAS_AC_TO_LAT > MAX_ALLOWED_DEVIATION   )
         ADD_ERROR("The following measurement is outside the allowed range!\n");

         sprintf(msg,"Anterior commissure to right: %5.1f mm (Atlas:%5.1f)\n",
                 dist_rig,ATLAS_AC_TO_LAT) ;
         ADD_REPORT(msg) ;
      }
      break ;  /* end of Boundings marker case */

      /*--- AC-PC alignment markers set ---*/

      case MARKSET_ALIGN:{
         THD_fvec3 acsup , acpos , pcinf , msag1 , msag2 ,
                   alpha1,alpha2,alpha,beta,gamma,rr1,rr2,rr , dif ;
         float size , slim ;

         /* extract the marker vectors, put in Dicom coords */

         acsup = THD_3dmm_to_dicomm( anat , MVEC(IMARK_ACSE) ) ;
         acpos = THD_3dmm_to_dicomm( anat , MVEC(IMARK_ACPM) ) ;
         pcinf = THD_3dmm_to_dicomm( anat , MVEC(IMARK_PCIE) ) ;
         msag1 = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MSA1) ) ;
         msag2 = THD_3dmm_to_dicomm( anat , MVEC(IMARK_MSA2) ) ;

         /*-- check the points for proper distances between each other --*/

         rr = SUB_FVEC3(acsup,acpos) ; size = SIZE_FVEC3(rr) ;
         if( size > 4.0 )
         ADD_ERROR("The two AC points are more than 4 mm apart.\n") ;

         slim = MIN_ALLOWED_DEVIATION * ATLAS_AC_TO_PC ;
         rr = SUB_FVEC3(acsup,pcinf) ; size = SIZE_FVEC3(rr) ;
         if( size <= slim ){
            sprintf(msg, "The AC & PC points are separated by %5.2f mm\n"
                         "which is closer than the minimum %5.2f mm!\n" ,
                    size,slim ) ;
            ADD_ERROR(msg) ;
         }

         rr = SUB_FVEC3(acsup,msag1) ; size = SIZE_FVEC3(rr) ;
         if( size < 20.0 )
         ADD_ERROR("The AC and 1st mid-sag points are closer than 20 mm.\n");

         rr = SUB_FVEC3(acsup,msag2) ; size = SIZE_FVEC3(rr) ;
         if( size < 20.0 )
         ADD_ERROR("The AC and 2nd mid-sag points are closer than 20 mm.\n");

         rr = SUB_FVEC3(msag1,msag2) ; size = SIZE_FVEC3(rr) ;
         if( size < 20.0 )
         ADD_ERROR("The two mid-sag points are closer than 20 mm.\n");

         rr = SUB_FVEC3(pcinf,msag1) ; size = SIZE_FVEC3(rr) ;
         if( size < 20.0 )
         ADD_ERROR("The PC and 1st mid-sag points are closer than 20 mm.\n");

         rr = SUB_FVEC3(pcinf,msag2) ; size = SIZE_FVEC3(rr) ;
         if( size < 20.0 )
         ADD_ERROR("The PC and 2nd mid-sag points are closer than 20 mm.\n");

         /*--- compute the new y direction (beta) ---*/

         beta = SUB_FVEC3(pcinf,acsup) ;  beta = NORMALIZE_FVEC3(beta) ;

         /*--- compute the new x direction (alpha) ---*/

         rr     = SUB_FVEC3(msag1,acsup) ;
         alpha1 = CROSS_FVEC3(beta,rr) ; alpha1 = NORMALIZE_FVEC3(alpha1) ;

         rr     = SUB_FVEC3(msag2,acsup) ;
         alpha2 = CROSS_FVEC3(beta,rr) ; alpha2 = NORMALIZE_FVEC3(alpha2) ;

         size = DOT_FVEC3(alpha1,alpha2) ;  /* angle < 2 degrees ? */
         if( size < 0.99939 )               /* (size = cos(angle) */
         ADD_ERROR("The AC + PC + mid-sag pts do not form a good plane.\n");

         size = acos((double)size) * 180/3.14159265 ;  /* report angle */
         sprintf(msg,
         "Angular deviation between AC+PC+mid-sag pts: %6.2f degrees\n",size);
         ADD_REPORT(msg) ;

         alpha = SCLADD_FVEC3(0.5,alpha1,0.5,alpha2) ;
         alpha = NORMALIZE_FVEC3(alpha) ;

         /*--- compute the new z direction (gamma) ---*/

         gamma = CROSS_FVEC3(alpha,beta) ; gamma = NORMALIZE_FVEC3(gamma) ;

         /*--- now, consider the ray from the AC posterior margin (acpos)
               in the gamma direction, and the ray from the AC superior
               edge (acsup) in the beta direction.  Nominally, these rays
               should intersect.  Find their points of closest approach
               (rr1,rr2).  The average of these is the Talairach center
               of coordinates (rr). ------------------------------------*/

         dif  = SUB_FVEC3(acsup,acpos) ;
         size = DOT_FVEC3(dif,gamma) ;
         rr1  = SCLADD_FVEC3(1.0,acpos,size,gamma) ;

         size = DOT_FVEC3(dif,beta) ;
         rr2  = SCLADD_FVEC3(1.0,acsup,-size,beta) ;

         dif = SUB_FVEC3(rr1,rr2) ; size = SIZE_FVEC3(dif) ;
         if( size > 2.0 )
         ADD_ERROR("AC Talairach origin mismatch more than 2 mm!\n") ;

         sprintf(msg,
         "Mismatch between AC-PC line and Talairach origin: %6.2f mm\n",size);
         ADD_REPORT(msg) ;

         rr = SCLADD_FVEC3(0.5,rr1,0.5,rr2) ;

         /*-- Use the trace of the rotation matrix to find
              the total rotation angle [suggested by M. Klosek] --*/

         { float theta, costheta ;

            costheta = 0.5 * sqrt(1.0+alpha.xyz[0]+beta.xyz[1]+gamma.xyz[2]) ;
            theta    = 2.0 * acos(costheta) * 180/3.14159265 ;
            sprintf(msg,
            "Total rotation to align AC-PC and mid-sag:   %6.2f degrees\n",theta) ;
            ADD_REPORT(msg) ;
         }

#ifdef AFNI_DEBUG
STATUS("AC-PC alignment markers computation:") ;
DUMP_FVEC3("   acsup ",acsup ) ;
DUMP_FVEC3("   acpos ",acpos ) ;
DUMP_FVEC3("   pcinf ",pcinf ) ;
DUMP_FVEC3("   msag1 ",msag1 ) ;
DUMP_FVEC3("   msag2 ",msag2 ) ;
DUMP_FVEC3("   beta  ",beta  ) ;
DUMP_FVEC3("   alpha1",alpha1) ;
DUMP_FVEC3("   alpha2",alpha2) ;
DUMP_FVEC3("   alpha ",alpha ) ;
DUMP_FVEC3("   gamma ",gamma ) ;
DUMP_FVEC3("   rr1   ",rr1   ) ;
DUMP_FVEC3("   rr2   ",rr2   ) ;
DUMP_FVEC3("   rr    ",rr    ) ;
printf("\n") ;
#endif

      }  /* end of AC-PC alignment case */
      break ;

   } /* end of switch on marker set type */

   if( num_error > 0 || (make_report && num_report > 0) ){
      (void) MCW_popup_message( im3d->vwid->marks->frame ,
                                error_list ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }

   myXtFree( error_list ) ;

   if( num_error > 0 && ! ELIDE_quality ) RETURN(False) ;
   RETURN(True) ;
}

/*------------------------------------------------------------------
  Create a new dataset that has the geometry specified as the
  warp from the parent.  The actual data will not be filled in yet.
--------------------------------------------------------------------*/

THD_3dim_dataset * AFNI_init_warp( Three_D_View * im3d ,
                                   THD_3dim_dataset * parent_dset ,
                                   THD_warp * warp_init , float resam_vox )
{
   THD_3dim_dataset * adam_dset ;  /* the farthest ancestor */
   THD_warp         * warp_total ; /* the warp from that ancestor */
   THD_fvec3          xnew_bot , xnew_top ;

   THD_3dim_dataset * new_dset ;
   THD_datablock    * new_dblk  , * adam_dblk  , * parent_dblk ;
   THD_dataxes      * new_daxes , * adam_daxes , * parent_daxes ;
   THD_diskptr      * new_dkptr , * adam_dkptr , * parent_dkptr ;
   THD_marker_set   * new_markers ;

   int new_nx , new_ny , new_nz , ii ;
   THD_ivec3 ivbot , ivtop ;

ENTRY("AFNI_init_warp") ;

   /*----- It is possible that this warp is one in a succession
           of warps.  In that case, the actual transformation is
           to be done directly from the "adam" dataset, rather
           than in succession from the parent.  The reason for
           this is to avoid repeated interpolation.  Thus, we
           first scan backward along the line of descent, and
           create the total warp from "adam" to the new dataset -----*/

   adam_dset   = parent_dset ;
   warp_total  = myXtNew( THD_warp ) ;  /* copy initial warp into final warp */
   *warp_total = *warp_init ;

   while( adam_dset->warp != NULL ){
      AFNI_concatenate_warp( warp_total , adam_dset->warp ) ;
      adam_dset = adam_dset->warp_parent ;
   }

   if( warp_total->type < FIRST_WARP_TYPE ||
       warp_total->type > LAST_WARP_TYPE    ) RETURN(NULL) ;  /* error! */

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"parent = %s ; adam = %s",
          parent_dset->self_name , adam_dset->self_name ) ;
  STATUS(str) ;

  STATUS("warp_total dump:") ;
  if( warp_total->type == WARP_AFFINE_TYPE ){
     DUMP_LMAP(warp_total->rig_bod.warp) ;
  } else {
     DUMP_T12_WARP(warp_total->tal_12) ;
  }
}
#endif

   adam_dblk  = adam_dset->dblk ;
   adam_daxes = adam_dset->daxes ;
   adam_dkptr = adam_dblk->diskptr ;

   parent_dblk  = parent_dset->dblk ;
   parent_daxes = parent_dset->daxes ;
   parent_dkptr = parent_dblk->diskptr ;

   /*----- We now determine the bounding box of the new dataset.
           This depends on the warp type:
              affine warps   --> use transformed bounding box of adam;
              Talairach warp --> use Talairach standard geometry;
           The results are in the vectors xnew_bot and xnew_top. -----*/

   switch( warp_total->type ){

      default:  RETURN(NULL) ;  /* something bad happened */

      /*--- 12 case Talairach mapping
            (sizes chosen to include borders of Atlas figures) ---*/

      case WARP_TALAIRACH_12_TYPE:{
         float zbottom ;
         int   use_tlrc_big ;

         use_tlrc_big = MCW_val_bbox( im3d->vwid->marks->tlrc_big_bbox ) ;
         zbottom      = (use_tlrc_big != 0) ? ATLAS_BBOX_INF_NEW : ATLAS_BBOX_INF ;

         LOAD_FVEC3(xnew_bot,-ATLAS_BBOX_LAT,-ATLAS_BBOX_ANT,-zbottom       );
         LOAD_FVEC3(xnew_top, ATLAS_BBOX_LAT, ATLAS_BBOX_POS, ATLAS_BBOX_SUP);
      }
      break ;

      /*--- linear warp ---*/

      case WARP_AFFINE_TYPE:{
         THD_fvec3 corner , base , xnew , aff_bot , aff_top ;
         THD_mat33 to_new ;

         to_new  = warp_total->rig_bod.warp.mfor ;
         base    = warp_total->rig_bod.warp.bvec ;

         /*--- transform each of the 8 corner locations in
               the adam dataset to the aligned system, and
               determine the outer limits of the new datablock ---*/

         LOAD_FVEC3(corner,adam_daxes->xxmin,
                           adam_daxes->yymin,adam_daxes->zzmin) ;  /* 1 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew_bot = xnew_top = MATVEC_SUB(to_new,corner,base) ;

         LOAD_FVEC3(corner,adam_daxes->xxmax,
                           adam_daxes->yymin,adam_daxes->zzmin) ;  /* 2 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         LOAD_FVEC3(corner,adam_daxes->xxmin,
                           adam_daxes->yymax,adam_daxes->zzmin) ;  /* 3 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         LOAD_FVEC3(corner,adam_daxes->xxmax,
                           adam_daxes->yymax,adam_daxes->zzmin) ;  /* 4 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         LOAD_FVEC3(corner,adam_daxes->xxmin,
                           adam_daxes->yymin,adam_daxes->zzmax) ;  /* 5 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         LOAD_FVEC3(corner,adam_daxes->xxmax,
                           adam_daxes->yymin,adam_daxes->zzmax) ;  /* 6 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         LOAD_FVEC3(corner,adam_daxes->xxmin,
                           adam_daxes->yymax,adam_daxes->zzmax) ;  /* 7 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         LOAD_FVEC3(corner,adam_daxes->xxmax,
                           adam_daxes->yymax,adam_daxes->zzmax) ;  /* 8 */
         corner   = THD_3dmm_to_dicomm( adam_dset , corner ) ;
         xnew     = MATVEC_SUB(to_new,corner,base) ;
         xnew_bot = MIN_FVEC3(xnew_bot,xnew) ;
         xnew_top = MAX_FVEC3(xnew_top ,xnew ) ;

         /* If the warp had any data in it about
            the region to map to, apply that data. */

         aff_bot = warp_total->rig_bod.warp.bot  ;
         aff_top = warp_total->rig_bod.warp.top  ;

         if( (aff_bot.xyz[0] < aff_top.xyz[0]) &&
             (aff_bot.xyz[1] < aff_top.xyz[1]) &&
             (aff_bot.xyz[2] < aff_top.xyz[2])   ){

   /* 3/25/95: use the bot & top INSTEAD of the corners
               (old version used in ADDITION to corners) */

#if 0
            xnew_bot = MIN_FVEC3(xnew_bot,aff_bot) ;
            xnew_top = MAX_FVEC3(xnew_top,aff_top) ;
#else
            xnew_bot = aff_bot ;
            xnew_top = aff_top ;
#endif
         }

      }  /* end of affine warp case */
      break ;

   } /* end of xnew_bot & xnew_top computed from warp */

   /* force bounds to be integral multiples of resampling size */

#define FLOOR(qq) ( ((qq) >= 0) ? ((int)(qq)) : (-1+(int)(qq)) )

   ivbot.ijk[0] = FLOOR( 0.01 + xnew_bot.xyz[0] / resam_vox ) ;
   ivbot.ijk[1] = FLOOR( 0.01 + xnew_bot.xyz[1] / resam_vox ) ;
   ivbot.ijk[2] = FLOOR( 0.01 + xnew_bot.xyz[2] / resam_vox ) ;

   ivtop.ijk[0] = FLOOR( 0.99 + xnew_top.xyz[0] / resam_vox ) ;
   ivtop.ijk[1] = FLOOR( 0.99 + xnew_top.xyz[1] / resam_vox ) ;
   ivtop.ijk[2] = FLOOR( 0.99 + xnew_top.xyz[2] / resam_vox ) ;

#undef FLOOR

   xnew_bot.xyz[0] = ivbot.ijk[0] * resam_vox ;
   xnew_bot.xyz[1] = ivbot.ijk[1] * resam_vox ;
   xnew_bot.xyz[2] = ivbot.ijk[2] * resam_vox ;

   xnew_top.xyz[0] = ivtop.ijk[0] * resam_vox ;
   xnew_top.xyz[1] = ivtop.ijk[1] * resam_vox ;
   xnew_top.xyz[2] = ivtop.ijk[2] * resam_vox ;

   /* compute dimensions of the new brick */

   new_nx = (xnew_top.xyz[0] - xnew_bot.xyz[0])/resam_vox + 1.5 ;
   new_ny = (xnew_top.xyz[1] - xnew_bot.xyz[1])/resam_vox + 1.5 ;
   new_nz = (xnew_top.xyz[2] - xnew_bot.xyz[2])/resam_vox + 1.5 ;

   xnew_top.xyz[0] = xnew_bot.xyz[0] + (new_nx-1) * resam_vox ;
   xnew_top.xyz[1] = xnew_bot.xyz[1] + (new_ny-1) * resam_vox ;
   xnew_top.xyz[2] = xnew_bot.xyz[2] + (new_nz-1) * resam_vox ;

#ifdef AFNI_DEBUG
DUMP_FVEC3("  -- xnew_bot",xnew_bot) ;
DUMP_FVEC3("  -- xnew_top",xnew_top) ;
printf("  ==> new nx=%d ny=%d nz=%d\n",new_nx,new_ny,new_nz) ;
#endif

   /*----- make a new 3D dataset !!! -----*/

   new_dset    =                     myXtNew( THD_3dim_dataset ) ;
   new_dblk    = new_dset->dblk    = myXtNew( THD_datablock ) ;
   new_daxes   = new_dset->daxes   = myXtNew( THD_dataxes ) ;
   new_markers = new_dset->markers = NULL ;                 /* later, dude */
   new_dkptr   = new_dblk->diskptr = myXtNew( THD_diskptr ) ;

   INIT_KILL(new_dset->kl) ; INIT_KILL(new_dblk->kl) ;

   ADDTO_KILL(new_dset->kl,new_dblk)  ;
   ADDTO_KILL(new_dset->kl,new_daxes) ;
   ADDTO_KILL(new_dset->kl,new_dkptr) ;

   ADDTO_KILL(new_dset->kl,warp_total) ;

   new_dset->wod_daxes = NULL ;
   new_dset->wod_flag  = True ;

   new_dset->taxis = NULL ;
   new_dset->tagset = NULL ;  /* Oct 1998 */

   INIT_STAT_AUX( new_dset , MAX_STAT_AUX , parent_dset->stat_aux ) ;

#define PARENT_MYSELF  /* 14 Dec 1999 */

   new_dset->idcode             = MCW_new_idcode() ;
   new_dset->warp_parent_idcode = adam_dset->idcode ;
#ifndef PARENT_MYSELF
   ZERO_IDCODE(new_dset->anat_parent_idcode) ;
   new_dset->anat_parent = NULL ;
#else
   new_dset->anat_parent_idcode = new_dset->idcode ; /* 14 Dec 1999 */
   new_dset->anat_parent        = new_dset ;         /* 14 Dec 1999 */
#endif

   EMPTY_STRING(new_dset->anat_parent_name) ;

   /*------------ initialize dataset fields -------------*/
   /**** July 1997: be careful about adam and parent ****/

STATUS("init new_dset") ;

   new_dset->type      = parent_dset->type;                    /* data types */
   new_dset->func_type = parent_dset->func_type;
   new_dset->view_type = WARPED_VIEW(parent_dset->view_type) ; /* view type */

   new_dset->warp      = warp_total ;                          /* warp info */
   new_dset->vox_warp  = NULL ;

   new_dset->warp_parent = adam_dset ;
   MCW_strncpy( new_dset->warp_parent_name ,
                adam_dset->self_name       , THD_MAX_NAME ) ;

   MCW_strncpy( new_dset->label1 , parent_dset->label1 , THD_MAX_LABEL ) ;
   MCW_strncpy( new_dset->label2 , parent_dset->label2 , THD_MAX_LABEL ) ;

   MCW_strncpy( new_dset->self_name  ,
                parent_dset->self_name , THD_MAX_NAME ) ;  /* make up */
   ii = strlen( new_dset->self_name ) ;                    /* a new name */
   new_dset->self_name[ii++] = '+' ;
   MCW_strncpy( &(new_dset->self_name[ii]) ,
                VIEW_typestr[new_dset->view_type] ,
                THD_MAX_NAME-ii ) ;

   new_dset->merger_list = NULL ;
   new_dset->death_mark  = 0 ;

   /*--- initialize disk pointer fields ---*/

STATUS("init new_dkptr") ;

   new_dkptr->type         = DISKPTR_TYPE ;
   new_dkptr->rank         = 3 ;
   new_dkptr->nvals        = adam_dkptr->nvals ;
   new_dkptr->dimsizes[0]  = new_nx ;
   new_dkptr->dimsizes[1]  = new_ny ;
   new_dkptr->dimsizes[2]  = new_nz ;
   new_dkptr->storage_mode = STORAGE_UNDEFINED ;
   new_dkptr->byte_order   = THD_get_write_order() ;  /* 25 April 1998 */

   THD_init_diskptr_names( new_dkptr ,
                           parent_dkptr->directory_name, NULL, parent_dkptr->prefix ,
                           new_dset->view_type , True ) ;

   /*--- initialize datablock fields ---*/

STATUS("init new_dblk") ;

   new_dblk->type        = DATABLOCK_TYPE ;
   new_dblk->nvals       = adam_dblk->nvals ;
   new_dblk->malloc_type = DATABLOCK_MEM_UNDEFINED ;
   new_dblk->natr        = new_dblk->natr_alloc = 0 ;
   new_dblk->atr         = NULL ;
   new_dblk->parent      = (XtPointer) new_dset ;

   new_dblk->brick_fac   = NULL ;  /* THD_init_datablock_brick */
   new_dblk->brick_bytes = NULL ;  /* will initialize these arrays */
   new_dblk->brick       = NULL ;
   THD_init_datablock_brick( new_dblk , -1 , adam_dblk ) ;

   new_dblk->master_nvals = 0 ;     /* 11 Jan 1999 */
   new_dblk->master_ival  = NULL ;
   new_dblk->master_bytes = NULL ;

   DSET_unlock(new_dset) ;

   THD_null_datablock_auxdata( new_dblk ) ;
   THD_copy_datablock_auxdata( adam_dblk , new_dblk ) ; /* 30 Nov 1997 */

   /*--- initialize data axes fields ---*/

STATUS("init new_daxes") ;

   new_daxes->type     = DATAXES_TYPE ;
   new_daxes->nxx      = new_nx ;
   new_daxes->nyy      = new_ny ;
   new_daxes->nzz      = new_nz ;
   new_daxes->xxorg    = xnew_bot.xyz[0] ;
   new_daxes->yyorg    = xnew_bot.xyz[1] ;
   new_daxes->zzorg    = xnew_bot.xyz[2] ;
   new_daxes->xxdel    = resam_vox ;       /* cubical voxels */
   new_daxes->yydel    = resam_vox ;
   new_daxes->zzdel    = resam_vox ;
   new_daxes->xxmin    = xnew_bot.xyz[0] ; /* save new bounding box */
   new_daxes->yymin    = xnew_bot.xyz[1] ;
   new_daxes->zzmin    = xnew_bot.xyz[2] ;
   new_daxes->xxmax    = xnew_top.xyz[0] ;
   new_daxes->yymax    = xnew_top.xyz[1] ;
   new_daxes->zzmax    = xnew_top.xyz[2] ;
   new_daxes->parent   = (XtPointer) new_dset ;

   new_daxes->xxorient = ORI_R2L_TYPE ;    /* Dicom standard axes! */
   new_daxes->yyorient = ORI_A2P_TYPE ;
   new_daxes->zzorient = ORI_I2S_TYPE ;
   LOAD_DIAG_MAT(new_daxes->to_dicomm,1,1,1) ;  /* identity matrix */

   /*--- if view type is appropriate, set new markers ---*/

   switch( new_dset->view_type ){

      default:
STATUS("no new_markers") ;
      break ;   /* no markers */

      /*--- AC-PC aligned ==> can do the BOUNDING set of markers ---*/

      case VIEW_ACPCALIGNED_TYPE:
      if( new_dset->type == HEAD_ANAT_TYPE ){
         int ii , jj ;

STATUS("init new_markers") ;

         new_markers = new_dset->markers = myXtNew( THD_marker_set ) ;
         ADDTO_KILL(new_dset->kl,new_markers) ;

         new_markers->numdef = NMARK_BOUNDING ;
         new_markers->numset = 0 ;                /* null data out */
         for( ii=0 ; ii < MARKS_MAXNUM ; ii++ ){

            new_markers->xyz[ii][0] =
              new_markers->xyz[ii][1] =
                new_markers->xyz[ii][2] = -99999999.99 ;

            for( jj=0 ; jj < MARKS_MAXLAB ; jj++ )
               new_markers->label[ii][jj] = '\0' ;

            for( jj=0 ; jj < MARKS_MAXHELP ; jj++ )
               new_markers->help[ii][jj] = '\0' ;

            new_markers->valid[ii]   = False ;
            new_markers->ovcolor[ii] = -1 ;    /* not used yet */
         }

         for( ii=0 ; ii < NMARK_BOUNDING ; ii++ ){       /* copy strings in */
            MCW_strncpy( &(new_markers->label[ii][0]) ,
                         THD_bounding_label[ii] , MARKS_MAXLAB ) ;
            MCW_strncpy( &(new_markers->help[ii][0]) ,
                         THD_bounding_help[ii] , MARKS_MAXHELP ) ;
         }

         for( ii=0 ; ii < MARKS_MAXFLAG ; ii++ )     /* copy flags in */
            new_markers->aflags[ii] = THD_bounding_aflags[ii] ;
         new_markers->type = new_markers->aflags[0] ;
      }
      break ;  /* end of BOUNDING markers */

   }  /* end of marker creation */

   /*----- copy statistics, if any -----*/

   new_dset->stats = NULL ;
   AFNI_copy_statistics( adam_dset , new_dset ) ;

   new_dset->pts = NULL ;

   /*----- dataset ready for warping -----*/

   PARENTIZE(new_dset,adam_dset->parent) ;

STATUS("initialization complete") ;

   RETURN( new_dset ) ;
}

/*-----------------------------------------------------------------*/

void AFNI_copy_statistics( THD_3dim_dataset * dsold , THD_3dim_dataset * dsnew )
{
   int ibr , nvold , nvnew ;
   THD_statistics * stold , * stnew ;

ENTRY("AFNI_copy_statistics") ;

   if( !ISVALID_3DIM_DATASET(dsold) || !ISVALID_3DIM_DATASET(dsnew) ) EXRETURN ;

   nvold = dsold->dblk->nvals ;
   nvnew = dsnew->dblk->nvals ;
   stold = dsold->stats ;
   stnew = dsnew->stats ;
   if( !ISVALID_STATISTIC(stold) ) EXRETURN ;

   if( stnew == NULL ){
      dsnew->stats  = stnew = myXtNew( THD_statistics ) ;
      stnew->type   = STATISTICS_TYPE ;
      stnew->nbstat = nvnew ;
      stnew->bstat  = (THD_brick_stats *)
                        XtMalloc( sizeof(THD_brick_stats) * nvnew ) ;
      ADDTO_KILL(dsnew->kl,stnew) ;
      stnew->parent = (XtPointer) dsnew ;
   } else {
      stnew->nbstat = nvnew ;
      stnew->bstat  = (THD_brick_stats *)
                        XtRealloc( (char *) stnew->bstat ,
                                   sizeof(THD_brick_stats) * nvnew ) ;
   }

   for( ibr=0 ; ibr < nvnew ; ibr++ ){
      if( ibr < nvold )
         stnew->bstat[ibr] = stold->bstat[ibr] ;
      else
         INVALIDATE_BSTAT(stnew->bstat[ibr]) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_set_cursor( int cursor_code )
{
   Three_D_View * im3d ;
   int id ;

ENTRY("AFNI_set_cursor") ;

   for( id=0 ; id < MAX_CONTROLLERS ; id++ ){
      im3d = GLOBAL_library.controllers[id] ;
      if( IM3D_OPEN(im3d) ){
         switch( cursor_code ){

            default:
            case AFNI_DEFAULT_CURSOR:
               NORMAL_cursorize( im3d->vwid->top_shell ) ;

               if( ISQ_REALZ(im3d->s123) )
                  NORMAL_cursorize( im3d->s123->wtop ) ;

               if( ISQ_REALZ(im3d->s231) )
                  NORMAL_cursorize( im3d->s231->wtop ) ;

               if( ISQ_REALZ(im3d->s312) )
                  NORMAL_cursorize( im3d->s312->wtop ) ;

               if( GRA_REALZ(im3d->g123) )
                  NORMAL_cursorize( im3d->g123->fdw_graph ) ;

               if( GRA_REALZ(im3d->g231) )
                  NORMAL_cursorize( im3d->g231->fdw_graph ) ;

               if( GRA_REALZ(im3d->g312) )
                  NORMAL_cursorize( im3d->g312->fdw_graph ) ;

               if( im3d->vinfo->inverted_pause ){
                  im3d->vinfo->inverted_pause = False ;
                  if( im3d->vwid->picture != NULL ){
                     if( !GLOBAL_argopt.keep_logo ) PICTURE_OFF(im3d) ;
                  } else
                     MCW_invert_widget( im3d->vwid->top_form ) ;
               }

               break ;

            case AFNI_WAITING_CURSOR:
               WATCH_cursorize( im3d->vwid->top_shell ) ;

               if( ISQ_REALZ(im3d->s123) )
                  WATCH_cursorize( im3d->s123->wtop ) ;

               if( ISQ_REALZ(im3d->s231) )
                  WATCH_cursorize( im3d->s231->wtop ) ;

               if( ISQ_REALZ(im3d->s312) )
                  WATCH_cursorize( im3d->s312->wtop ) ;

               if( GRA_REALZ(im3d->g123) )
                  WATCH_cursorize( im3d->g123->fdw_graph ) ;

               if( GRA_REALZ(im3d->g231) )
                  WATCH_cursorize( im3d->g231->fdw_graph ) ;

               if( GRA_REALZ(im3d->g312) )
                  WATCH_cursorize( im3d->g312->fdw_graph ) ;

               if( ! im3d->vinfo->inverted_pause ){
                  im3d->vinfo->inverted_pause = True ;
                  if( im3d->vwid->picture != NULL )
                     PICTURE_ON(im3d) ;
                  else
                     MCW_invert_widget( im3d->vwid->top_form ) ;
               }

               break ;
         }

         XSync( XtDisplay(im3d->vwid->top_shell) , False ) ;
         XmUpdateDisplay( im3d->vwid->top_shell ) ;
      }
   }

   EXRETURN ;
}

/****************************************************************/
/***** June 1995: routine to load constants from X defaults *****/
/***** June 1999: also allow loading from Unix environment  *****/

#if 0
# define NAME2INT(nnn,iii,bot,top)           \
  { xdef = XGetDefault(display,"AFNI",nnn) ; \
    if( xdef != NULL ){                      \
       ival = strtol( xdef , &cpt , 10 ) ;   \
       if( *cpt == '\0' && ival >= (bot) && ival <= (top) ) (iii) = ival ; } }

# define NAME2FLOAT(nnn,fff,bot,top)         \
  { xdef = XGetDefault(display,"AFNI",nnn) ; \
    if( xdef != NULL ){                      \
       fval = strtod( xdef , &cpt ) ;        \
       if( *cpt == '\0' && fval >= (bot) && fval <= (top) ) (fff) = fval ; } }

# define NAME2STRING(nnn,sss)                \
  { xdef = XGetDefault(display,"AFNI",nnn) ; \
    if( xdef != NULL ) sss  = XtNewString(xdef) ; }
#else
# define NAME2INT(nnn,iii,bot,top)           \
  { xdef = RWC_getname(display,nnn) ;        \
    if( xdef != NULL ){                      \
       ival = strtol( xdef , &cpt , 10 ) ;   \
       if( *cpt == '\0' && ival >= (bot) && ival <= (top) ) (iii) = ival ; } }

# define NAME2FLOAT(nnn,fff,bot,top)         \
  { xdef = RWC_getname(display,nnn) ;        \
    if( xdef != NULL ){                      \
       fval = strtod( xdef , &cpt ) ;        \
       if( *cpt == '\0' && fval >= (bot) && fval <= (top) ) (fff) = fval ; } }

# define NAME2STRING(nnn,sss)                \
  { xdef = RWC_getname(display,nnn) ;        \
    if( xdef != NULL ) sss  = XtNewString(xdef) ; }
#endif

#define BAD -999

void AFNI_load_defaults( Widget w )
{
   char    * xdef ;
   Display * display ;
   int       ival , ii,jj ;
   float     fval ;
   char *    cpt ;
   char      buf[64] ;
   float     pthr[NPANE_MAX+1] ;
   int       pov[NPANE_MAX+1] ;

ENTRY("AFNI_load_defaults") ;

   if( w == NULL ){
      fprintf(stderr,"\n*** AFNI_load_defaults: NULL input widget ***\n") ;
      EXRETURN ;
   }

   display = XtDisplay( w ) ;

   /** initialize overlay color arrays from defaults **/

   for( ii=0 ; ii < DEFAULT_NCOLOVR ; ii++ ){
      INIT_colovr[ii] = XtNewString(INIT_def_colovr[ii]) ;
      INIT_labovr[ii] = XtNewString(INIT_def_labovr[ii]) ;
   }
   for( ; ii < MAX_NCOLOVR ; ii++ ){
      INIT_colovr[ii] = INIT_labovr[ii] = NULL ;
   }

   /** initialize display and overlay colors **/

   NAME2INT("ncolors",INIT_ngray,3,MAX_COLORS) ;

   NAME2INT("ncolovr",INIT_ncolovr,2,MAX_NCOLOVR) ;

   NAME2FLOAT("gamma",INIT_gamma,0.1,9.9) ;

   for( ii=0 ; ii < INIT_ncolovr ; ii++ ){
      sprintf( buf , "ovdef%02d" , ii+1 ) ;
      NAME2STRING(buf,INIT_colovr[ii] ) ;

      sprintf( buf , "ovlab%02d" , ii+1 ) ;
      NAME2STRING(buf,INIT_labovr[ii] ) ;
   }

   NAME2INT("ovcrosshair"      , INIT_crosshair_color,0,INIT_ncolovr) ;
   NAME2INT("ovmarksprimary"   , INIT_marks1_color   ,0,INIT_ncolovr) ;
   NAME2INT("ovmarkssecondary" , INIT_marks2_color   ,0,INIT_ncolovr) ;
   NAME2INT("markssize"        , INIT_marks_size     ,2,MAXOVSIZE   ) ;
   NAME2INT("marksgap"         , INIT_marks_gap      ,0,MAXOVSIZE-1 ) ;
   NAME2INT("crosshairgap"     , INIT_crosshair_gap  ,0,MAXOVSIZE   ) ;
   NAME2INT("bigscroll"        , INIT_bigscroll      ,1,MAXOVSIZE   ) ;

   NAME2INT("graph_boxes_color" ,INIT_GR_boxes_color ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_backg_color" ,INIT_GR_backg_color ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_grid_color"  ,INIT_GR_grid_color  ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_text_color"  ,INIT_GR_text_color  ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_data_color"  ,INIT_GR_data_color  ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_ideal_color" ,INIT_GR_ideal_color ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_ort_color"   ,INIT_GR_ort_color   ,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_ignore_color",INIT_GR_ignore_color,BLUEST_COLOR,INIT_ncolovr) ;
   NAME2INT("graph_dplot_color" ,INIT_GR_dplot_color ,BLUEST_COLOR,INIT_ncolovr) ;

   NAME2INT("graph_boxes_thick" ,INIT_GR_boxes_thick ,0,1) ;
   NAME2INT("graph_grid_thick"  ,INIT_GR_grid_thick  ,0,1) ;
   NAME2INT("graph_data_thick"  ,INIT_GR_data_thick  ,0,1) ;
   NAME2INT("graph_ideal_thick" ,INIT_GR_ideal_thick ,0,1) ;
   NAME2INT("graph_ort_thick"   ,INIT_GR_ort_thick   ,0,1) ;
   NAME2INT("graph_dplot_thick" ,INIT_GR_dplot_thick ,0,1) ;

   NAME2INT("graph_ggap"        ,INIT_GR_ggap        ,0,19);         /* 27 May 1999 */
   NAME2INT("fim_polort"        ,INIT_fim_polort     ,0,MAX_POLORT); /* 30 May 1999 */

   /** initialize other junk **/

   cpt = NULL ;
   NAME2STRING( "tlrc_big" , cpt ) ;
   if( cpt != NULL ){
      INIT_tlrc_big = (strcmp(cpt,"True")==0) ? 1 : 0 ;
      XtFree(cpt) ;
   }

   cpt = NULL ;
   NAME2STRING( "montage_periodic" , cpt ) ;
   if( cpt != NULL ){
      INIT_montage_periodic = (strcmp(cpt,"True")==0) ? 1 : 0 ;
      XtFree(cpt) ;
   }

   NAME2INT("fim_ignore",INIT_ignore,0,999) ;

   cpt = NULL ;
   NAME2STRING( "purge" , cpt ) ;
   if( cpt != NULL ){
      INIT_purge = (strcmp(cpt,"True")==0) ? 1 : 0 ;
      myXtFree(cpt) ;
   }

   NAME2FLOAT("resam_vox",INIT_resam_vox,0.1,4.0) ;
   INIT_resam_vox = 0.1 * ( (int)(10*INIT_resam_vox) ) ;

   cpt = NULL ;
   NAME2STRING( "resam_anat" , cpt ) ;
   if( cpt != NULL ){
      for( ii=FIRST_RESAM_TYPE ; ii <= LAST_RESAM_TYPE ; ii++ ){
         if( strcmp(cpt,RESAM_shortstr[ii]) == 0 ) break ;
      }
      if( ii <= LAST_RESAM_TYPE ) INIT_resam_anat = ii ;
      myXtFree(cpt) ;
   }

   cpt = NULL ;
   NAME2STRING( "resam_func" , cpt ) ;
   if( cpt != NULL ){
      for( ii=FIRST_RESAM_TYPE ; ii <= LAST_RESAM_TYPE ; ii++ ){
         if( strcmp(cpt,RESAM_shortstr[ii]) == 0 ) break ;
      }
      if( ii <= LAST_RESAM_TYPE ) INIT_resam_func = ii ;
      myXtFree(cpt) ;
   }

   cpt = NULL ;
   NAME2STRING( "resam_thr" , cpt ) ;
   if( cpt != NULL ){
      for( ii=FIRST_RESAM_TYPE ; ii <= LAST_RESAM_TYPE ; ii++ ){
         if( strcmp(cpt,RESAM_shortstr[ii]) == 0 ) break ;
      }
      if( ii <= LAST_RESAM_TYPE ) INIT_resam_thr = ii ;
      myXtFree(cpt) ;
   }

   /** initialize pbar panes **/

   cpt = NULL ;
   NAME2STRING( "pbar_posfunc" , cpt ) ;
   if( cpt != NULL ){
      INIT_posfunc = (strcmp(cpt,"True")==0) ? 1 : 0 ;
      myXtFree(cpt) ;
   }

   cpt = NULL ;
   NAME2STRING( "pbar_hide" , cpt ) ;
   if( cpt != NULL ){
      INIT_panes_hide = (strcmp(cpt,"True")==0) ? 1 : 0 ;
      myXtFree(cpt) ;
   }

   NAME2INT("pbar_pos_pane_count" , INIT_panes_pos , NPANE_MIN , NPANE_MAX ) ;
   NAME2INT("pbar_sgn_pane_count" , INIT_panes_sgn , NPANE_MIN , NPANE_MAX ) ;

   for( ii=NPANE_INIT+1 ; ii <= NPANE_MAX ; ii++ ){
      fval     = 1.0 / ii ;
      pthr[0]  = 1.0 ;
      pthr[ii] = 0.0 ;
      for( jj=1 ; jj < ii ; jj++ ) pthr[jj] = fval * (ii-jj) ;
      for( jj=0 ; jj < ii ; jj++ ) pov[jj]  = (jj % INIT_ncolovr) + 1 ;

      for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_pos[ii][jj] = pthr[jj] ;
      for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_pos[ii][jj] = pov[jj] ;
   }

   for( ii=NPANE_MIN ; ii <= NPANE_MAX ; ii++ ){

      for( jj=0 ; jj <= ii ; jj++ ){
         sprintf( buf , "pbar_pos_pane%02d_thr%02d" , ii,jj ) ;
         pthr[jj] = BAD ;
         NAME2FLOAT(buf,pthr[jj],0.0,1.0) ;
      }

      for( jj=0 ; jj < ii ; jj++ ){
         sprintf( buf , "pbar_pos_pane%02d_ov%02d" , ii,jj ) ;
         pov[jj] = BAD ;
         NAME2INT(buf,pov[jj],0,INIT_ncolovr) ;
      }

      /* check pthr for OK-ness; if not good, skip to next pane count (ii) */

      if( pthr[0] != 1.0 || pthr[jj] != 0.0 ) continue ;
      for( jj=1 ; jj <= ii ; jj++ ){
         if( pthr[jj] == BAD || pthr[jj] >= pthr[jj-1] ) break ;
      }
      if( jj <= ii ) continue ;

      /* check pov for OK-ness */

      for( jj=0 ; jj < ii ; jj++ ) if( pov[jj] == BAD ) break ;
      if( jj < ii ) continue ;

      /* get to here --> load pthr and pov into arrays */

      for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_pos[ii][jj] = pthr[jj] ;
      for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_pos[ii][jj] = pov[jj] ;

   }

   /** initialize signed pbar panes **/

   for( ii=NPANE_INIT+1 ; ii <= NPANE_MAX ; ii++ ){
      fval     =  1.0 / ii ;
      pthr[0]  =  1.0 ;
      pthr[ii] = -1.0 ;
      for( jj=1 ; jj < ii ; jj++ ) pthr[jj] = fval * (ii-2*jj) ;
      for( jj=0 ; jj < ii ; jj++ ) pov[jj]  = (jj % INIT_ncolovr) + 1 ;

      for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_sgn[ii][jj] = pthr[jj] ;
      for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_sgn[ii][jj] = pov[jj] ;
   }

   for( ii=NPANE_MIN ; ii <= NPANE_MAX ; ii++ ){

      for( jj=0 ; jj <= ii ; jj++ ){
         sprintf( buf , "pbar_sgn_pane%02d_thr%02d" , ii,jj ) ;
         pthr[jj] = BAD ;
         NAME2FLOAT(buf,pthr[jj],-1.0,1.0) ; /* 14 Apr 1999: 0.0 changed to -1.0! */
      }

      for( jj=0 ; jj < ii ; jj++ ){
         sprintf( buf , "pbar_sgn_pane%02d_ov%02d" , ii,jj ) ;
         pov[jj] = BAD ;
         NAME2INT(buf,pov[jj],0,INIT_ncolovr) ;
      }

      /* check pthr for OK-ness; if not good, skip to next pane count (ii) */

      if( pthr[0] != 1.0 || pthr[jj] != -1.0 ) continue ;
      for( jj=1 ; jj <= ii ; jj++ ){
         if( pthr[jj] == BAD || pthr[jj] >= pthr[jj-1] ) break ;
      }
      if( jj <= ii ) continue ;

      /* check pov for OK-ness */

      for( jj=0 ; jj < ii ; jj++ ) if( pov[jj] == BAD ) break ;
      if( jj < ii ) continue ;

      /* get to here --> load pthr and pov into arrays */

      for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_sgn[ii][jj] = pthr[jj] ;
      for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_sgn[ii][jj] = pov[jj] ;

   }

   EXRETURN ;
}

/********************************************************************/
#ifdef USE_SONNETS

void AFNI_popup_sonnet( Widget w , int ii )  /* 12 Dec 2001 */
{
   char buf[2048] ; int jj=MCW_USER_KILL ;

   if( w == NULL ) return ;

   if( ii < 1 || ii > NUM_SONNETS ){
      ii  = (lrand48()&NUM_SONNETS) + 1 ;
      jj |= MCW_TIMER_KILL ;
   }

   sprintf( buf , "                    * %d *\n" , ii ) ;
   strcat( buf , sonnets[ii-1] ) ;
   (void) MCW_popup_message( w , buf , jj ) ;
   return ;
}

/*..................................................................*/

void AFNI_sonnet_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   MCW_choose_cbs * cbs ;

   if( NO_frivolities || !IM3D_VALID(im3d) ) return ;

   if( w == im3d->vwid->prog->hidden_sonnet_pb ){  /* start the process */

      MCW_choose_integer( im3d->vwid->picture ,
                          "Sonnet " ,
                          1 , NUM_SONNETS , sonnet_index+1 ,
                          AFNI_sonnet_CB , (XtPointer) im3d ) ;
      return ;
   }

   /** if get to here, finish the process **/

   cbs = (MCW_choose_cbs *) call_data ;
   if( cbs->reason != mcwCR_integer ){  /* error */
      XBell( XtDisplay(w) , 100 ) ; return ;
   }

   AFNI_popup_sonnet( im3d->vwid->picture , cbs->ival ) ;
   return ;
}
#endif /* USE_SONNETS */
/********************************************************************/

/*----------------------------------------------------------------------
   Put a function on the list of n-dimensional transformations
   (modified 03 Nov 1996 from just 0D transforms)
   (modified 22 Apr 1997 to add int flags to each function)
   (modified 31 Jan 2002 to add slice_proj for nd=-1)
------------------------------------------------------------------------*/

void AFNI_register_nD_function( int nd, char * name,
                                generic_func * func, int flags )
{
   MCW_function_list * rlist ;
   int num ;

   if( name == NULL || strlen(name) == 0 || func == NULL ) return ;

   switch( nd ){
      default: return ;

      case 0: rlist = &(GLOBAL_library.registered_0D) ; break ;
      case 1: rlist = &(GLOBAL_library.registered_1D) ; break ;
      case 2: rlist = &(GLOBAL_library.registered_2D) ; break ;

      case -1: rlist= &(GLOBAL_library.registered_slice_proj) ; break ;
   }

   num = rlist->num ;

   if( num == 0 ){ rlist->flags=NULL; rlist->labels=NULL; rlist->funcs=NULL;
                   rlist->func_data=NULL; rlist->func_code=NULL;             }

   rlist->flags = (int *) XtRealloc( (char *)rlist->flags, sizeof(int)*(num+1) ) ;

   rlist->labels = (char **) XtRealloc( (char *)rlist->labels ,
                                        sizeof(char *)*(num+1) ) ;

   rlist->funcs = (generic_func **) XtRealloc( (char *)rlist->funcs ,
                                               sizeof(generic_func *)*(num+1) ) ;

   rlist->func_data = (void **) XtRealloc( (char *)rlist->func_data ,
                                           sizeof(void *)*(num+1) ) ;

   rlist->func_code = (int *) XtRealloc( (char *)rlist->func_code, sizeof(int)*(num+1) ) ;

   rlist->flags[num]  = flags ;
   rlist->labels[num] = XtNewString(name) ;
   rlist->funcs[num]  = func ;

   rlist->func_data[num] = NULL ;
   rlist->func_code[num] = nd ;

   rlist->num = num+1 ;
   return ;
}

/*---------- 18 May 2000: save/get dataset index for function calls -------*/

static int dset_ijk=-1 , dset_tin=-1 ;

void AFNI_store_dset_index( int ijk , int tin )
{
   dset_ijk = ijk ; dset_tin = tin ; return ;
}

int AFNI_needs_dset_ijk(void){ return dset_ijk ; }
int AFNI_needs_dset_tin(void){ return dset_tin ; }

/*-----------------------------------------------------------------------
   Add a timeseries to the global list
-------------------------------------------------------------------------*/

void AFNI_add_timeseries( MRI_IMAGE * tsim )
{
ENTRY("AFNI_add_timeseries") ;

   if( tsim != NULL ){
      POPDOWN_timeseries_chooser ;
      ADDTO_IMARR(GLOBAL_library.timeseries,tsim) ;
   }
   EXRETURN ;
}
