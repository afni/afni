
/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/
/*   This is the mother goddess of all FMRI programs, so bow down before it!  */
/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/
/*   Look on my works, Ye Mighty, and despair!                                */
/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2 (or later).  See the file README.Copyright for details.
******************************************************************************/

/**********************************************************************/
/* MCW/GPL/NIH AFNI:                                                  */
/*    Analysis of Functional NeuroImages                              */
/*                                                                    */
/* Author: Robert W. Cox, PhD                                         */
/*         Scientific and Statistical Computing Core                  */
/*         National Institute of Mental Health                        */
/*         Bethesda, MD 20892  USA                                    */
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
/*   + Sean and Alex Bellgowan have contributed in their own way.     */
/**********************************************************************/

/* the definition below is used in some header files,
   and it indicates that the main() program is in this file */

#define MAIN

#include "afni.h"
#include <X11/keysym.h>  /* 20 Feb 2003 */
#include "afni_plugout.h"
#include "readme_afnigui.h"
#include "readme_env.h"

/*------------------------------------------------------*/
/* if the compiler wants to 'show off' the name of this
   edition of AFNI, then turn that macro into a string. */

/* SHSTRING comes from mrilib.h via debugtrace.h */

#ifdef SHOWOFF
# undef  SHSH
# undef  SHSHSH
# undef  SHSTRING
# define SHSH(x)   #x
# define SHSHSH(x) SHSH(x)
# define SHSTRING  SHSHSH(SHOWOFF)   /* now in "quotes" */
#else
# undef  SHSTRING
#endif
/*------------------------------------------------------*/

#if 0  /* ANNOUNCEMENT IS NOT USED  28 Dec 2015 [rickr] */
#ifdef SHSTRING

#define ANNOUNCEMENT                                                           \
 "GPL AFNI: Analysis of Functional NeuroImages, by RW Cox (" COXEMAIL ")\n"    \
 "This is Version " VERSION               "\n"                                 \
 "[[Precompiled binary " SHSTRING ": " __DATE__ "]]\n\n"                       \
 " ** This software was designed to be used only for research purposes. **\n"  \
 " ** Clinical uses are not recommended, and have never been evaluated. **\n"  \
 " ** This software comes with no warranties of any kind whatsoever,    **\n"  \
 " ** and may not be useful for anything.  Use it at your own risk!     **\n"  \
 " ** If these terms are not acceptable, you aren't allowed to use AFNI.**\n"  \
 " ** See 'Define Datamode->Misc->License Info' for more details.       **\n\n"\
 " **** If you DO find AFNI useful, please cite this paper:\n"                 \
 "    RW Cox. AFNI: Software for analysis and visualization of\n"              \
 "    functional magnetic resonance neuroimages.\n"                            \
 "    Computers and Biomedical Research, 29:162-173, 1996.\n\n"                \
 " **** If you find SUMA useful, citing this paper also would be nice:\n"      \
 "    ZS Saad, RC Reynolds, B Argall, S Japee, RW Cox.\n"                      \
 "    SUMA: An Interface For Surface-Based Intra- And Inter-Subject Analysis\n"\
 "    With AFNI. 2nd IEEE International Symposium on Biomedical Imaging:\n"    \
 "    Macro to Nano 2, 1510-1513, 2004.\n\n"

#else

#define ANNOUNCEMENT                                                           \
 "GPL AFNI: Analysis of Functional NeuroImages, by RW Cox (" COXEMAIL ")\n"    \
 "This is Version " AVERZHN " -- compiled " __DATE__  "\n\n"                   \
 " ** This software was designed to be used only for research purposes. **\n"  \
 " ** Clinical uses are not recommended, and have never been evaluated. **\n"  \
 " ** This software comes with no warranties of any kind whatsoever,    **\n"  \
 " ** and may not be useful for anything.  Use it at your own risk!     **\n"  \
 " ** If these terms are not acceptable, you aren't allowed to use AFNI.**\n"  \
 " ** See 'Define Datamode->Misc->License Info' for more details.       **\n\n"\
 " **** If you DO find AFNI useful, please cite this paper:\n"                 \
 "    RW Cox. AFNI: Software for analysis and visualization of\n"              \
 "    functional magnetic resonance neuroimages.\n"                            \
 "    Computers and Biomedical Research, 29:162-173, 1996.\n\n"                \
 " **** If you find SUMA useful, citing this paper also would be nice:\n"      \
 "    ZS Saad, RC Reynolds, B Argall, S Japee, RW Cox.\n"                      \
 "    SUMA: An Interface For Surface-Based Intra- And Inter-Subject Analysis\n"\
 "    With AFNI. 2nd IEEE International Symposium on Biomedical Imaging:\n"    \
 "    Macro to Nano 2, 1510-1513, 2004.\n\n"

#endif /* SHSTRING */
#endif /* if 0 - for SHSTRING and ANNOUNCEMENT */

/*------------------------------------------------------*/

#ifdef AFNI_DEBUG
#  define USE_TRACING
#  define REPORT_PROGRESS(str)  /* nada */
#else
#  define REPORT_PROGRESS(str)  \
    do{ if(AFNI_VERBOSE){fputs(str,stderr);fflush(stderr);} } while(0)
#endif

/*------------------------------------------------------*/

#define EMPTY_STRING(str) ((str)[0] = '\0')

#undef IMAGEIZE_CROSSHAIRS  /* disable crosshairs drawn into overlay pixels */

#define USE_SIDES  /* 01 Dec 1999: replace "left is xxx" */
                   /* labels with "sides" labels.        */

void AFNI_print_startup_tip(int) ;

#include "afni_startup_tips.h"  /* where the tips and goodbye messages live */

int num_bysub     = 0 ;     /* 01 Feb 2018 */
char **bysub      = NULL ;
int num_bysub_dir = 0 ;     /* 15 Apr 2019 */
char **bysub_dir  = NULL ;

/*-----------------------------------------------------------------------
   Fallback resources for AFNI.  May be overridden by the user's
   .Xdefaults file, or other resource sources.  AFNI does not come
   with an "app-defaults" file, since that would be too much like work.
   (And would require sysadmin privileges to install.)
-------------------------------------------------------------------------*/

/*----- things to change the default font sizes -----*/

static void AFNI_set_4fonts(char *AA,char *BB, char *CC, char *DD) ;

#define XXX_PLUS_FONTS \
   AFNI_set_4fonts(                                                     \
     "-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-1"   ,   \
     "-adobe-courier-bold-r-normal--20-140-100-100-m-110-iso8859-1" ,   \
     "-adobe-courier-bold-r-normal--17-120-100-100-m-100-iso8859-1" ,   \
     "-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1"   )

#define XXX_MINUS_FONTS \
   AFNI_set_4fonts("8x13bold","7x13","6x10","5x8")

#define XXX_BIG_FONTS                                                   \
   AFNI_set_4fonts(                                                     \
     "-adobe-courier-bold-r-normal--34-240-100-100-m-200-iso8859-1" ,   \
     "-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-1"   ,   \
     "-adobe-courier-bold-r-normal--20-140-100-100-m-110-iso8859-1" ,   \
     "-adobe-courier-bold-r-normal--17-120-100-100-m-100-iso8859-1"  )

#define XXX_NORM_FONTS \
   AFNI_set_4fonts("9x15bold","8x13bold","7x13","6x10")

static char *FALLback[] =
  {   "AFNI*fontList:              9x15bold=charset1"    , /* normal font */
      "AFNI*pbar*fontList:         6x10=charset1"        , /* next to pbar */
      "AFNI*imseq*fontList:        7x13=charset1"        , /* on imseq */
      "AFNI*font8*fontList:        8x13bold=charset1"    , /* smaller fonts */
      "AFNI*font7*fontList:        7x13=charset1"        ,  /* for various */
      "AFNI*font6*fontList:        6x10=charset1"        ,  /* usages */
      "AFNI*background:            gray22"               , /* background clr */
      "AFNI*menu*background:       gray4"                , /* bkgd in menus */
      "AFNI*menu*foreground:       #ffdd22"              , /* menu text color */
      "AFNI*borderColor:           gray19"               , /* same as bkgd! */
      "AFNI*foreground:            yellow"               , /* normal text */
      "AFNI*borderWidth:           0"                    , /* don't change! */
      "AFNI*troughColor:           blue3"                , /* in sliders */
      "AFNI*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
      "AFNI*help*background:       black"                , /* for help */
      "AFNI*help*foreground:       #ffffff"              ,
      "AFNI*help*helpborder:       False"                ,
      "AFNI*help*waitPeriod:       1066"                 ,
      "AFNI*help*fontList:         9x15bold=charset1"    ,
      "AFNI*cluefont:              9x15bold"             , /* for popup */
      "AFNI*bigtext*fontList:      10x20=charset1"       , /* hints */
      "AFNI*help*cancelWaitPeriod: 333"                  ,
#if 0
      "AFNI*clustA*fontList:       9x15bold=charset1"    , /* for Clusterize */
      "AFNI*clustB*fontList:       9x15bold=charset1"    ,
      "AFNI*clustA*background:     gray22"               ,
      "AFNI*clustB*background:     gray1"                ,
      "AFNI*clustA*foreground:     yellow"               ,
      "AFNI*clustB*foreground:     white"                ,
#endif

      "AFNI*XmList.translations: #augment"                /* 24 Feb 2007 */
           "<Btn4Down>: ListPrevItem()\\n"                /* for scrollwheel */
           "<Btn5Down>: ListNextItem()"                  ,

      "AFNI*XmText.translations: #augment"
           "<Btn4Down>: previous-line() scroll-one-line-down()\\n"
           "<Btn5Down>: next-line() scroll-one-line-up()"          ,
#if 0
      "AFNI*XmScrollBar.translations: #augment"
           "<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)\\n"
           "<Btn5Down>: IncrementDownOrRight(1) IncrementDownOrRight(0)" ,
#endif

   NULL } ;

/* The trick to using multiple Xt translations in the fallback resources
   above is to separate them not with '\n' but with '\\n'.  Ugghhhhhhh.  */

/*-----------------------------------------------------------------------*/
static int    new_FALLback_num = 0 ;    /* for -XXX option [24 Mar 2016] */
static char **new_FALLback     = NULL ;
static char  *xrdb_old         = NULL ;
static char  *xrdb_pg          = NULL ;

/*-----------------------------------------------------------------------*/

static int equiv_FALLback( char *n1 , char *n2 ) /* check if 2 strings */
{                                                /* start the same */
   char *c1,*c2 , *d1,*d2 ; int ee ;
   if( n1 == NULL || *n1 == '\0' ) return 1 ;
   if( n2 == NULL || *n2 == '\0' ) return 1 ;
   if( strcmp(n1,n2) == 0        ) return 1 ;
   d1 = strdup(n1) ; c1 = strchr(d1,':') ;
   d2 = strdup(n2) ; c2 = strchr(d2,':') ;
   if( c1 == NULL || c2 == NULL  ){ free(d1); free(d2); return 1; }
   *c1 = '\0' ; *c2 = '\0' ;
   ee = strcmp(d1,d2) ; free(d1) ; free(d2) ;
   return (ee==0) ;
}

/*-----------------------------------------------------------------------*/

/*-----------------------------------------------------------------------*/

#define ADDTO_FALLback_one(nameval)                                            \
 do{ int nf ;                                                                  \
     nf = new_FALLback_num ;                                                   \
     new_FALLback = (char **)realloc( new_FALLback , sizeof(char *)*(nf+2) ) ; \
     new_FALLback[nf]   = strdup(nameval) ;                                    \
     new_FALLback[nf+1] = NULL ; new_FALLback_num = nf+1 ;                     \
 } while(0)

#define ADDTO_FALLback_pair(name,val)                                          \
 do{ char *str=malloc(strlen(name)+strlen(val)+16) ;                           \
     strcpy(str,name) ; strcat(str,":   ") ; strcat(str,val) ;                 \
     ADDTO_FALLback_one(str) ;                                                 \
 } while(0)

/*-----------------------------------------------------------------------*/

static void AFNI_set_4fonts(char *AA,char *BB, char *CC, char *DD)
{
   ADDTO_FALLback_pair("AFNI*fontList"        , AA) ;
   ADDTO_FALLback_pair("AFNI*help*fontList"   , AA) ;
   ADDTO_FALLback_pair("AFNI*bigtext*fontList", AA) ;
   ADDTO_FALLback_pair("AFNI*cluefont"        , BB) ;
   ADDTO_FALLback_pair("AFNI*font8*fontList"  , BB) ;
   ADDTO_FALLback_pair("AFNI*imseq*fontList"  , CC) ;
   ADDTO_FALLback_pair("AFNI*font7*fontList"  , CC) ;
   ADDTO_FALLback_pair("AFNI*font6*fontList"  , DD) ;
   ADDTO_FALLback_pair("AFNI*pbar*fontList"   , DD) ;

   /*** INFO_message("AFNI_set_4fonts: %s %s %s %s",AA,BB,CC,DD) ; ***/
}

/*-----------------------------------------------------------------------*/

static int XXX_set_default = 0 ; /* 08 Apr 2016 */

static void process_XXX_options( int argc , char *argv[] )
{
   int nopt=1 ;

   while( nopt < argc ){

     if( strcasecmp(argv[nopt],"-big") == 0 ){  /* 17 Mar 2020 */
       XXX_BIG_FONTS ; nopt++ ; continue ;      /* simpler font size options */
     }
     if( strcasecmp(argv[nopt],"-plus") == 0 ){
       static int nplus=0 ;
       if( nplus == 0 ){ XXX_PLUS_FONTS ; }
       else            { XXX_BIG_FONTS ;  }
       nopt++ ; nplus++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-minus") == 0 ){
       XXX_MINUS_FONTS ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-norm") == 0 ){  /* 10 Apr 2020 */
       XXX_NORM_FONTS ; nopt++ ; continue ;
     }

     if( strncasecmp(argv[nopt],"-XXX",4) != 0 ){ nopt++; continue; }

     if( strcasecmp(argv[nopt],"-XXX") == 0 ){
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       if( strncasecmp(argv[nopt],"default",7) == 0 ){ /* 08 Apr 2016 */
         if( THD_find_executable("xrdb") != NULL ){
           int qq ;
           XXX_set_default = 1 ;
           for( qq=0 ; FALLback[qq] != NULL ; qq++ )
             ADDTO_FALLback_one(FALLback[qq]) ;
         }
       } else if( strchr(argv[nopt],':') != NULL ){
         ADDTO_FALLback_one(argv[nopt]) ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXnpane") == 0 ){  /* 06 May 2016 */
       int np ;
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       np = (int)strtod(argv[nopt],NULL) ;
       if( np < 256 || np > NPANE_BIGGEST ){
         WARNING_message("value '%s' after '%s' is illegal -- ignoring",argv[nopt],argv[nopt-1]) ;
       } else {
         if( np%2 == 1 ) np++ ;
         if( np > NPANE_BIGGEST ) np = NPANE_BIGGEST ;
         npane_big = np ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXbgcolor") == 0 ){
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       ADDTO_FALLback_pair("AFNI*background"     ,argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*menu*background",argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*help*background",argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXfgcolor") == 0 ){
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       ADDTO_FALLback_pair("AFNI*foreground"     ,argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*menu*foreground",argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*help*foreground",argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXfontsize") == 0 ){
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       if( strcasecmp(argv[nopt],"plus") == 0 || strcmp(argv[nopt],"+") == 0 ){
         static int nplus=0 ;
         if( nplus == 0 ){ XXX_PLUS_FONTS ; }
         else            { XXX_BIG_FONTS ;  }
         nplus++ ;
       } else if( strcasecmp(argv[nopt],"minus") == 0 || strcmp(argv[nopt],"-") == 0 ){
         XXX_MINUS_FONTS ;
       } else if( strcasecmp(argv[nopt],"big") == 0 || strcmp(argv[nopt],"++") == 0 ){
         XXX_PLUS_FONTS ;
       } else {
         WARNING_message("Don't understand '%s' after option '%s'",
                         argv[nopt] , argv[nopt-1] ) ;
       }

       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXfontA") == 0 ){
       char *fn ;
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       fn = (char *)malloc(sizeof(char)*(strlen(argv[nopt])+32)) ;
       sprintf(fn,"%s=charset1",argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*fontList"        ,fn) ;
       ADDTO_FALLback_pair("AFNI*help*fontList"   ,fn) ;
       ADDTO_FALLback_pair("AFNI*bigtext*fontList",fn) ;
       ADDTO_FALLback_pair("AFNI*cluefont"        ,fn) ;
       free(fn) ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXfontB") == 0 ){
       char *fn ;
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       fn = (char *)malloc(sizeof(char)*(strlen(argv[nopt])+32)) ;
       sprintf(fn,"%s=charset1",argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*font8*fontList",fn) ;
       free(fn) ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXfontC") == 0 ){
       char *fn ;
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       fn = (char *)malloc(sizeof(char)*(strlen(argv[nopt])+32)) ;
       sprintf(fn,"%s=charset1",argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*imseq*fontList",fn) ;
       ADDTO_FALLback_pair("AFNI*font7*fontList",fn) ;
       free(fn) ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-XXXfontD") == 0 ){
       char *fn ;
       if( ++nopt >= argc ){
         WARNING_message("no argument after '%s' :-(",argv[nopt-1]) ;
         break ;
       }
       fn = (char *)malloc(sizeof(char)*(strlen(argv[nopt])+32)) ;
       sprintf(fn,"%s=charset1",argv[nopt]) ;
       ADDTO_FALLback_pair("AFNI*font6*fontList",fn) ;
       ADDTO_FALLback_pair("AFNI*pbar*fontList" ,fn) ;
       free(fn) ; nopt++ ; continue ;
     }

   }

   return ;
}

/*----------------------------------------------------------------------------
   Global variables that used to be local variables in main(),
   but since the advent of the splash screen and startup code
   in MAIN_workprocess() are needed in more than one place.
------------------------------------------------------------------------------*/

static XtAppContext   MAIN_app ;
static XtErrorHandler MAIN_old_handler ;   /* no longer used */
static Three_D_View  *MAIN_im3d ;
static MCW_DC *       MAIN_dc ;
static Widget         MAIN_shell=NULL ;
static int            MAIN_argc ;
static char         **MAIN_argv ;
static RwcBoolean        MAIN_workprocess( XtPointer ) ;

/*----- Stuff saved from the '-com' command line arguments [29 Jul 2005] -----*/

static int COM_num = 0 ;
#define MAX_N_COM 1024
static char *COM_com[MAX_N_COM] ;  /* max of 1024 commands allowed!!! */
static char comsep = ';' ;         /* command separator: 22 Feb 2007 */

static int recursed_ondot = 0 ;  /* 18 Feb 2007 */

/* ---------------------------------------------------------------------- */
/* just display the AFNI version                      26 Oct 2015 [rickr] */
/* (since writing to stdout, do not interfere with print-and-exit funcs)  */

#include "RomanImperator.h"  /* RWC Nov 2018 */

void show_AFNI_version(void)
{
  char *rimp = AFNI_VERSION_RomanImperator ;
  char vvvv[1024] ;

  if( rimp != NULL && *rimp != '\0' ){ sprintf(vvvv,"%s '%s'",AVERZHN,rimp); }
  else                               { strcpy(vvvv,AVERZHN) ; }

#ifdef SHSTRING
     printf( "Precompiled binary " SHSTRING ": " __DATE__ " (Version %s)\n",vvvv ) ;
#else
     printf( "Compile date = " __DATE__ " " __TIME__ " (Version %s)\n",vvvv) ;
#endif
}

// [PT: Oct 7, 2019] functions to plot just the basic info, without
// sep parsing needs: version number and package num
void show_AFNI_package(void)
{
#ifdef SHSTRING
   printf(SHSTRING "\n");
#else
   printf( "Compiled: " __DATE__ );
#endif
}

void show_AFNI_vnum(void)
{
   printf(AVERZHN "\n");
}

// [PT: Oct 17, 2019] useful to be able to get the 'readme_afnigui.h'
// info at the command line (ambitions to parse it for the HTML RST
// pages).  This is called via the -tips option.
void show_AFNI_readme_gui(void)
{
   int ii;

   for( ii=0 ; readme_afnigui[ii] != NULL ; ii++ ){
     printf( "%s" , readme_afnigui[ii] ) ;
   }
}

// [PT: Oct 21, 2019] useful to be able to get the 'readme_env.h' info
// at commandline; also for HTML RST parsing.  This is called via the
// -env option.
void show_AFNI_readme_env(void)
{
   int ii;

   for( ii=0 ; readme_env[ii] != NULL ; ii++ ){
     printf( "%s" , readme_env[ii] ) ;
   }
}

/********************************************************************
   Print out some help information and then quit quit quit
*********************************************************************/

void AFNI_syntax(void)
{
/*   printf(ANNOUNCEMENT) ;*/

   if( AFNI_yesenv("AFNI_POMOC") )  /* for the Web -help page */
     printf(
      " **** At the bottom of this Web page are some slide images to\n"
      "    outline the usage of the AFNI Graphical User Interface (GUI).\n"
      "\n"
     ) ;
   else
     printf(
      " **** Help for all AFNI programs can be found at the Web page\n"
      "    https://afni.nimh.nih.gov/pub/dist/doc/program_help/index.html\n"
      "\n"
     ) ;

   printf(
     "----------------------------------------------------------------\n"
     "USAGE 1: read in sessions of 3D datasets (created by to3d, etc.)\n"
     "----------------------------------------------------------------\n"
     "   afni [options] [session_directory ...]\n"
     "\n"
     "   -bysub       This new [01 Feb 2018] option allows you to have 'sessions'\n"
     "    *OR*          made up from files scattered across multiple directories.\n"
     "   -BIDS          The purpose of this option is to gather all the datasets\n"
     "                  corresponding to a single subject identifier, as is done\n"
     "                  in the BIDS file hierarchy -- http://bids.neuroimaging.io/\n"
     "             **** There are two methods for using this option.\n"
     "    method (1) ** In the first method, you put one or more subject identifiers,\n"
     "     [OLDER]      which are of the form 'sub-XXX' where 'XXX' is some\n"
     "                  subject code (it does not have to be exactly 3 characters).\n"
     "                 ++ If an identifier does NOT start with 'sub-', then that\n"
     "                    4 letter string will be added to the front. This allows\n"
     "                    you to specify your subjects by their numbers 'XXX' alone.\n"
     "    method (2) ** In the second method, you put one or more directory names,\n"
     "     [NEWER]      and all immediate sub-directories whose name starts with\n"
     "                  'sub-' will be included. With this method, you can end up\n"
     "                  reading in an entire BIDS hierarchy of datasets, which\n"
     "                  might take a significant amount of time if there are many\n"
     "                  subjects.\n"
     "               **** Note that if an identifier following '-bysub' on the\n"
     "                    command line is a directory name that starts with 'sub-',\n"
     "                    it will be treated using method (1), not using method (2).\n"
     "  both methods ** In either method, the list of names following '-bysub' ends\n"
     "                  with any argument that starts with '-' (or with the end of\n"
     "                  all command line arguments).\n"
     "    method (2) ** Each directory on the command line (after all options, and\n"
     "                  including any directories directly after the '-bysub' option)\n"
     "                  will be scanned recursively (down the file tree) for\n"
     "                  subdirectories whose name matches each 'sub-XXX' identifier\n"
     "                  exactly. All such subdirectories will have all their\n"
     "                  datasets read in (recursively down the file tree) and\n"
     "                  put into a single session for viewing in AFNI.\n"
     "                ++ In addition, all datasets from all subjects will be\n"
     "                   available in the 'All_Datasets' session in the GUI.\n"
     "                   (Unless environment variable AFNI_ALL_DATASETS is set to NO)\n"
     "                ++ If you do NOT put any directories or subject identifiers\n"
     "                   directly after the '-bysub' (or '-BIDS') option, the\n"
     "                   program will act as if you put '.' there, and it will\n"
     "                   search below the current working directory - the directory\n"
     "                   you were 'in' when you started the AFNI GUI.\n"
     "    method (1) ** If a directory on the command line after this option does\n"
     "                  NOT have any subdirectories that match any of the '-bysub'\n"
     "                  identifiers, then that directory will be read in the normal\n"
     "                  way, with all the datasets in that particular directory\n"
     "                  (but not subdirectories) read into the session.\n"
     "  both methods ** Please note that '-bysub' sessions will NOT be rescanned\n"
     "                  for new datasets that might get placed there after the\n"
     "                  AFNI GUI starts, unlike normal (single directory) sessions.\n"
     "    method (1) ** Example (method 1):\n"
     "                    afni -bysub 10506 50073 - ~/data/OpenFMRI/ds000030\n"
     "                  This will open the data for subjects 10506 and 50073 from\n"
     "                  the data at the specified directory -- presumably the\n"
     "                  data downloaded from https://openfmri.org/dataset/ds000030/\n"
     "                ++ If directory sub-10506 is found and has (say) sub-directories\n"
     "                     anat beh dwi func\n"
     "                   all AFNI-readable datasets from these sub-directories will\n"
     "                   be input and collected into one session, to be easily\n"
     "                   viewed together.\n"
     "                ++ Because of the recursive search, if a directory named (e.g.)\n"
     "                     derivatives/sub-10506\n"
     "                   is found underneath ~/data/OpenFMRI/ds000030, all the\n"
     "                   datasets found underneath that will also be put into the\n"
     "                   same session, so they can be viewed with the 'raw' data.\n"
     "                ++ In this context, 'dataset' also means .png and .jpg files\n"
     "                   found in the sub-XXX directories. These images can be\n"
     "                   opened in the AFNI GUI using the Axial image viewer.\n"
     "                   (You might want to turn the AFNI crosshairs off!)\n"
     "              ++++ If you do NOT want .png and .jpg files read into AFNI,\n"
     "                   set Unix environment variable AFNI_IMAGE_DATASETS to 'NO'.\n"
     "                ++ You can put multiple subject IDs after '-bysub', as\n"
     "                   in the example above. You can also use the '-bysub' option\n"
     "                   more than once, if you like. Each distinct subect ID will\n"
     "                   get a distinct AFNI session in the GUI.\n"
     "    method (2) ** Example (method 2):\n"
     "                    afni -bysub ~/data/OpenFMRI/ds000030\n"
     "                  This will read in all datasets from all subjects. In this\n"
     "                  particular example, there are hundreds of subjects, so this\n"
     "                  command may not actually be a good idea - unless you want to\n"
     "                  go get a cup of chai or coffee, and then sip it very slowly.\n"
     "               ** Example (method 2):\n"
     "                    afni -BIDS\n"
     "                  This will read all 'sub-*' directories from the current\n"
     "                  working directory, and is the same as 'afni -BIDS .' \n"
     "                  As noted earlier, this recursive operation may take a long\n"
     "                  time (especially if the datasets are compressed), as AFNI\n"
     "                  reads the headers from ALL datasets as it finds them,\n"
     "                  to build a table for you to use in the 'OverLay' and\n"
     "                  'UnderLay' dataset choosers.\n"
     "\n"
     "   -all_dsets   Read in all datasets from all listed folders together.\n"
     "                  Has the same effect as choosing 'All_Datasets' in the GUI.\n"
     "                  Example: afni -all_dsets dir1 dir2 dir3\n"
     "                  Can be set to default in .afnirc with ALL_DSETS_STARTUP = YES.\n"
     "                  Overridden silently by AFNI_ALL_DATASETS = NO.\n"
     "\n"
#if MMAP_THRESHOLD > 0
     "   -purge       Conserve memory by purging unused datasets from memory.\n"
     "                  [Use this if you run out of memory when running AFNI.]\n"
     "                  [This will slow the code down, so use only if needed.]\n"
#else
     "   -nopurge     Disable the automatic purging of unused datasets from\n"
     "                  memory, which is invoked when you switch datasets.\n"
#endif
     "                  [When a dataset is needed, it will be re-read from disk.]\n"
     "\n"
     "   -posfunc     Start up the color 'pbar' to use only positive function values.\n"
     "\n"
     "   -R           Recursively search each session_directory for more session\n"
     "                  subdirectories.\n"
     "       WARNING: This will descend the entire filesystem hierarchy from\n"
     "                  each session_directory given on the command line.  On a\n"
     "                  large disk, this may take a long time.  To limit the\n"
     "                  recursion to 5 levels (for example), use -R5.\n"
     "             ** Use of '-bysub' disables recursive descent, since '-bysub'\n"
     "                will do that for you.\n"
#if 0
     "\n"
     "   -ignore N    Tells the program to 'ignore' the first N points in\n"
     "                  time series for graphs and FIM calculations.\n"
     "\n"
     "   -im1 N       Tells the program to use image N as the first one for\n"
     "                  graphs and FIM calculations (same as '-ignore N-1')\n"
     "\n"
     "   -tlrc_small  These options set whether to use the 'small' or 'big'\n"
     "   -tlrc_big      Talairach brick size.  The compiled in default for\n"
     "                  the program is now 'big', unlike AFNI 1.0x.\n"
#endif
     "\n"
#ifndef WARP_4D
     "   -warp_4D     Allows the program to Talairach transform and write\n"
     "                  to disk 3D+time datasets.  Note that the resulting\n"
     "                  disk files will be gigantic (100s of Megabytes).\n"
     "\n"
#endif
     "   -no1D        Tells AFNI not to read *.1D timeseries files from\n"
     "                  the dataset directories.  The *.1D files in the\n"
     "                  directories listed in the AFNI_TSPATH environment\n"
     "                  variable will still be read (if this variable is\n"
     "                  not set, then './' will be scanned for *.1D files).\n"
     "\n"
     "   -nocsv       Each of these option flags does the same thing (i.e.,\n"
     "   -notsv         they are synonyms): each tells AFNI not to read\n"
     "   -notcsv        *.csv or *.tsv files from the dataset directories.\n"
     "                  You can also set env AFNI_SKIP_TCSV_SCAN = YES to the\n"
     "                  same effect.\n"
#if 0
     "\n"
     "   -noqual      Tells AFNI not to enforce the 'quality' checks when\n"
     "                  making the transformations to +acpc and +tlrc.\n"
#endif
     "\n"
     "   -unique      Tells the program to create a unique set of colors\n"
     "                  for each AFNI controller window.  This allows\n"
     "                  different datasets to be viewed with different\n"
     "                  grayscales or colorscales.  Note that -unique\n"
     "                  will only work on displays that support 12 bit\n"
     "                  PseudoColor (e.g., SGI workstations) or TrueColor.\n"
     "\n"
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
     "\n"
#ifdef ALLOW_PLUGINS
     "   -noplugins   Tells the program not to load plugins.\n"
     "                  (Plugins can also be disabled by setting the\n"
     "                   environment variable AFNI_NOPLUGINS.)\n"
     "\n"
     "   -seehidden   Tells the program to show you which plugins\n"
     "                  are hidden.\n"
     "\n"
     "   -DAFNI_ALLOW_ALL_PLUGINS=YES\n"
     "                Tells the program NOT to hide plugins from you.\n"
     "                  Note that there are a lot of hidden plugins,\n"
     "                  most of which are not very useful!\n"
     "\n"
     "   -yesplugouts Tells the program to listen for plugouts.\n"
     "                  (Plugouts can also be enabled by setting the\n"
     "                   environment variable AFNI_YESPLUGOUTS.)\n"
     "\n"
     "   -YESplugouts Makes the plugout code print out lots of messages\n"
     "                  (useful for debugging a new plugout).\n"
     "\n"
     "   -noplugouts  Tells the program NOT to listen for plugouts.\n"
     "                  (This option is available to override\n"
     "                   the AFNI_YESPLUGOUTS environment variable.)\n"
     "\n"
#endif
     "   -skip_afnirc Tells the program NOT to read the file .afnirc\n"
     "                  in the home directory.  See README.setup for\n"
     "                  details on the use of .afnirc for initialization.\n"
     "\n"
     "   -layout fn   Tells AFNI to read the initial windows layout from\n"
     "                  file 'fn'.  If this option is not given, then\n"
     "                  environment variable AFNI_LAYOUT_FILE is used.\n"
     "                  If neither is present, then AFNI will do whatever\n"
     "                  it feels like.\n"
     "\n"
     "   -niml        If present, turns on listening for NIML-formatted\n"
     "                  data from SUMA.  Can also be turned on by setting\n"
     "                  environment variable AFNI_NIML_START to YES.\n"
     "%s"
     "   -list_ports  List all port assignments and quit\n"
     "   -port_number PORT_NAME: Give port number for PORT_NAME and quit\n"
     "   -port_number_quiet PORT_NAME: Same as -port_number but writes out \n"
     "                                    number only\n"
     "   -available_npb: Find the first available block of port numbers, \n"
     "                   print it to stdout and quit\n"
     "                   The value can be used to set the -npb option for\n"
     "                   a new set of chatty AFNI/SUMA/etc group.\n"
     "   -available_npb_quiet: Just print the block number to stdout and quit.\n"
     "\n"
     "   -com ccc     This option lets you specify 'command strings' to\n"
     "                  drive AFNI after the program startup is completed.\n"
     "                  Legal command strings are described in the file\n"
     "                  README.driver.  More than one '-com' option can\n"
     "                  be used, and the commands will be executed in\n"
     "                  the order they are given on the command line.\n"
     "            N.B.: Most commands to AFNI contain spaces, so the 'ccc'\n"
     "                  command strings will need to be enclosed in quotes.\n"
     "\n"
     "   -comsep 'c'  Use character 'c' as a separator for commands.\n"
     "                  In this way, you can put multiple commands in\n"
     "                  a single '-com' option.  Default separator is ';'.\n"
     "            N.B.: The command separator CANNOT be alphabetic or\n"
     "                  numeric (a..z, A..Z, 0..9) or whitespace or a quote!\n"
     "            N.B.: -comsep should come BEFORE any -com option that\n"
     "                  uses a non-semicolon separator!\n"
     "\n"
     "   Example: -com 'OPEN_WINDOW axialimage; SAVE_JPEG axialimage zork; QUIT'\n"
     "   N.B.: You can also put startup commands (one per line) in\n"
     "         the file '~/.afni.startup_script'.  For example,\n"
     "            OPEN_WINDOW axialimage\n"
     "         to always open the axial image window on startup.\n"
     "\n"
     " * If no session_directories are given, then the program will use\n"
     "    the current working directory (i.e., './').\n"
     " * The maximum number of sessions is now set to  %d.\n"
     " * The maximum number of datasets per session is %d.\n"
     " * To change these maximums, you must edit file '3ddata.h' and then\n"
     "    recompile this program.\n"
     "\n"
     "Global Options (available to all AFNI/SUMA programs)\n"
     "%s\n%s"
     , get_np_help() ,
     THD_MAX_NUM_SESSION , THD_MAX_SESSION_SIZE ,
     SUMA_Offset_SLines(get_help_help(),3), get_gopt_help()
   ) ;

#if 0
   printf(
     "\n"
     "-----------------------------------------------------\n"
     "USAGE 2: read in images for 'quick and dirty' viewing\n"
     "-----------------------------------------------------\n"
     "** Most advanced features of AFNI will be disabled **\n"
     "** The aiv program can be used now instead of AFNI **\n"
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
     "   -tim:nt      into a internal time-dependent dataset.  Suppose that\n"
     "   -zim:nz      there are N input 2D slices on the command line.\n"
     "              * -tim alone means these are N points in time (1 slice).\n"
     "              * -tim:nt means there are nt points in time (nt is\n"
     "                  an integer > 1), so there are N/nt slices in space,\n"
     "                  and the images on the command line are input in\n"
     "                  time order first (like -time:tz in to3d).\n"
     "              * -zim:nz means there are nz slices in space (nz is\n"
     "                  an integer > 1), so there are N/nz points in time,\n"
     "                  and the images on the command line are input in\n"
     "                  slice order first (like -time:zt in to3d).\n"
     "\n"
     " N.B.: You may wish to use the -ignore option to set the number of\n"
     "        initial points to ignore in the time series graph if you use\n"
     "        -tim or -zim, since there is no way to change this from\n"
     "        within an AFNI run (the FIM menus are disabled).\n"
     " N.B.: The program 'aiv' (AFNI image viewer) can also be used to\n"
     "        get a quick look at images (but not time series graphs).\n"
   ) ;
#endif

   printf(
     "\n"
     "-------------------------------------------------------\n"
     "USAGE 2: read in datasets specified on the command line\n"
     "-------------------------------------------------------\n"
     "\n"
     "  afni -dset [options] dname1 dname2 ...\n"
     "\n"
     "where 'dname1' is the name of a dataset, etc.  With this option, only\n"
     "the chosen datasets are read in, and they are all put in the same\n"
     "'session'.  Follower datasets are not created.\n"
     "\n"
     "* If you wish to be very tricksy, you can read in .1D files as datasets\n"
     "  using the \\' transpose syntax, as in\n"
     "     afni Fred.1D\\'\n"
     "  However, this isn't very useful (IMHO).\n"
     "\n"
     "* AFNI can also read image files (.jpg and .png) from the command line.\n"
     "  For just viewing images, the 'aiv' program (AFNI image viewer) is\n"
     "  simpler; but unlike aiv, you can do basic image processing on an\n"
     "  image 'dataset' using the AFNI GUI's feature. Sample command:\n"
     "     afni *.jpg\n"
     "  Each image file is a single 'dataset'; to switch between images,\n"
     "  use the 'Underlay' button. To view an image, open the 'Axial' viewer.\n"
     "\n");
    printf(MASTER_HELP_STRING);   putchar('\n');
    printf(CATENATE_HELP_STRING); putchar('\n');
    printf(CALC_HELP_STRING);     putchar('\n');

   printf(
     "\n"
     "-------------------------------\n"
     "GENERAL OPTIONS (for any usage)\n"
     "-------------------------------\n"
     "\n"
     "   -papers      Prints out the list of AFNI papers, and exits.\n"
     "\n"
     "   -q           Tells afni to be 'quiet' on startup\n"
     "\n"
     "   -Dname=val   Sets environment variable 'name' to 'val' inside AFNI;\n"
     "                  will supersede any value set in .afnirc.\n"
     "\n"
     "   -gamma gg    Tells afni that the gamma correction factor for the\n"
     "                  monitor is 'gg' (default gg is 1.0; greater than\n"
     "                  1.0 makes the image contrast larger -- this may\n"
     "                  also be adjusted interactively)\n"
     "\n"
     "   -install     Tells afni to install a new X11 Colormap.  This only\n"
     "                  means something for PseudoColor displays.  Also, it\n"
     "                  usually cause the notorious 'technicolor' effect.\n"
     "\n"
     "   -ncolors nn  Tells afni to use 'nn' gray levels for the image\n"
     "                  displays (default is %d)\n"
     "\n"
     "   -xtwarns     Tells afni to show any Xt warning messages that may\n"
     "                  occur; the default is to suppress these messages.\n"
#ifdef USE_TRACING
     "   -XTWARNS     Trigger a debug trace when an Xt warning happens.\n"
     "\n"
#endif
     "   -tbar name   Uses 'name' instead of 'AFNI' in window titlebars.\n"
     "\n"
     "   -flipim and  The '-flipim' option tells afni to display images in the\n"
     "\n"
     "   -noflipim      'flipped' radiology convention (left on the right).\n"
     "                  The '-noflipim' option tells afni to display left on\n"
     "                  the left, as neuroscientists generally prefer.  This\n"
     "                  latter mode can also be set by the Unix environment\n"
     "                  variable 'AFNI_LEFT_IS_LEFT'.  The '-flipim' mode is\n"
     "                  the default.\n"
     "\n"
#ifdef USE_TRACING
     "   -trace       Turns routine call tracing on, for debugging purposes.\n"
     "   -TRACE       Turns even more verbose tracing on, for more debugging.\n"
#endif
#ifdef USING_MCW_MALLOC
     "   -nomall      Disables use of the mcw_malloc() library routines.\n"
     "\n"
#endif
     "   -motif_ver   Show the applied motif version string.\n"
     "\n"
     "   -no_detach   Do not detach from the terminal.\n"
     "\n"
     "   -no_frivolities  Turn of all frivolities/fun stuff.\n"
     "\n"
     "   -get_processed_env   Show applied AFNI/NIFTI environment variables.\n"
     "\n"
     "   -global_opts Show options that are global to all AFNI programs.\n"
     "\n"
     "   -goodbye [n] Print a 'goodbye' message and exit (just for fun).\n"
     "                If an integer is supplied afterwards, will print that\n"
     "                many (random) goodbye messages.\n"
     "\n"
     "   -startup [n] Similar to '-goodbye', but for startup tips.\n"
     "                [If you want REAL fun, use '-startup ALL'.]\n"
     "\n"
     "   -julian      Print out the current Julian date and exit.\n"
     "\n"
     "   -ver         Print the current AFNI version and compile date, then exit.\n"
     "                Useful to check how up-to-date you are (or aren't).\n"
     "\n"
     "   -vnum        Print just the current AFNI version number (i.e.,\n"
     "                AFNI_A.B.C), then exit.\n"
     "\n"
     "   -package     Print just the current AFNI package (i.e., \n"
     "                linux_ubuntu_12_64, macos_10.12_local, etc.),\n"
     "                then exit.\n"
     "\n"
     "   -tips        Print the tips for the GUI, such as key presses\n"
     "                and other useful advice.  This is the same file that\n"
     "                would be displayed with the 'AFNI Tips' button in the\n"
     "                GUI controller.  Exit after display.\n"
     "\n"
     "   -env         Print the environment variables for AFNI, which a user\n"
     "                might set in their ~/.afnirc file (wait, you *do*\n"
     "                have one on your computer, right?).\n"
     "                Exit after display.\n"
     "\n"
     "\n"
     "N.B.: Many of these options, as well as the initial color set up,\n"
     "      can be controlled by appropriate X11 resources.  See the\n"
     "      files AFNI.Xdefaults and README.environment for instructions\n"
     "      and examples.\n"

     , DEFAULT_NGRAY
   ) ;

   printf("\n"
    "-----------------------------------------------------------\n"
    "Options that affect X11 Display properties: '-XXXsomething'\n"
    "-----------------------------------------------------------\n"
    "\n"
    "My intent with these options is that you use them in aliases\n"
    "or shell scripts, to let you setup specific appearances for\n"
    "multiple copies of AFNI.  For example, put the following\n"
    "command in your shell startup file (e.g., ~/.cshrc or ~/.bashrc)\n"
    "   alias ablue afni -XXXfgcolor white -XXXbgcolor navyblue\n"
    "Then the command 'ablue' will start AFNI with a blue background\n"
    "and using white for the default text color.\n"
    "\n"
    "Note that these options set 'properties' on the X11 server,\n"
    "which might survive after AFNI exits (especially if AFNI crashes).\n"
    "If for some reason these settings cause trouble after AFNI\n"
    "exits, use the option '-XXX defaults' to reset the X11\n"
    "properties for AFNI back to their default values.\n"
    "\n"
    "Also note that each option is of the form '-XXXsomething', followed\n"
    "by a single argument.\n"
    "\n"
    " -XXXfgcolor colorname = set the 'foreground' color (text color)\n"
    "                         to 'colorname'\n"
    "                         [default = yellow]\n"
    "                         ++ This should be a bright color, to contrast\n"
    "                            the background color.\n"
    "                         ++ You can find a list of X11 color names at\n"
    "                              https://en.wikipedia.org/wiki/X11_color_names\n"
    "                            However, if you use a name like Dark Cyan\n"
    "                            (with a space inside the name), you must\n"
    "                            put the name in quotes: 'Dark Cyan', or remove\n"
    "                            the space: DarkCyan.\n"
    "                         ++ Another way to specify X11 colors is in hexadecimal,\n"
    "                            as in '#rgb' or '#rrggbb', where the letters shown\n"
    "                            are replaced by hex values from 0 to f.  For example,\n"
    "                            '#ffcc00' is an orange-yellow mixture.\n"
    "\n"
    " -XXXbgcolor colorname = set the 'background' color to 'colorname'\n"
    "                         [default = gray22]\n"
    "                         ++ This should be a somewhat dark color,\n"
    "                            or parts of the interface may be hard\n"
    "                            to read.\n"
    "                         ++ EXAMPLE:\n"
    "                              afni -XXXfgcolor #00ffaa -XXXbgcolor #330000 -plus\n"
    "                            You can create command aliases to open AFNI with\n"
    "                            different color schemes, to make your life simpler.\n"
    "\n"
    " -XXXfontsize plus     = set all the X11 fonts used by AFNI to be one\n"
    "   *OR*                  size larger ('plus') or to be one size smaller\n"
    " -XXXfontsize minus      ('minus').  The 'plus' version I find useful for\n"
    "   *OR*                  a screen resolution of about 100 dots per inch\n"
    " -XXXfontsize big        (40 dots per cm) -- you can find what the system\n"
    "   *OR*                  thinks your screen resolution is by the command\n"
    " -big                      xdpyinfo | grep -i resolution\n"
    "   *OR*                  ++ Applying 'plus' twice is the same as 'big'.\n"
    " -plus                   ++ Using 'big' will use large Adobe Courier fonts.\n"
    "   *OR*                  ++ Alternatively, you can control each of the 4 fonts\n"
    " -minus                     that AFNI uses, via the 4 following options ...\n"
    "   *OR*                  ++ You can also set the fontsize for your copy\n"
    " -norm                      of AFNI in your ~/.afnirc file by setting\n"
    "                            environment variable AFNI_FONTSIZE to one of:\n"
    "                              big *OR* minus *or* plus\n"
    "                         ++ Using 'norm' gives the default AFNI font sizes.\n"
    "\n"
    " -XXXfontA fontname    = set the X11 font name for the main AFNI\n"
    "                         controller\n"
    "                         [default = 9x15bold]\n"
    "                         ++ To see a list of all X11 font names, type the command\n"
    "  xlsfonts | more\n"
    "                            *or* more elaborately (to show only fixed width fonts):\n"
    "  xlsfonts | grep -e '-[cm]-' | grep -e '-iso8859-1$' | grep -e '-medium-' \\\n"
    "           | grep -e '-r-normal-' | grep -v -e '-0-0-' | sort -t '-' -k 8 -n | uniq\n"
    "                         ++ It is best to use a fixed width font\n"
    "                            (e.g., not Helvetica), or the AFNI buttons\n"
    "                            won't line up nicely!\n"
    "                         ++ If you use an illegal font name here, you\n"
    "                            might make it hard to use the AFNI GUI!\n"
    "                         ++ The default fonts are chosen for 'normal' screen\n"
    "                            resolutions (about 72 dots per inch = 28 dots per cm).\n"
    "                            For higher resolutions ('Retina'), you might\n"
    "                            want to use larger fonts.  Adding these\n"
    "                            '-XXXfont?' options is one way to address this\n"
    "                            problem. (Also see '-plus' above.)\n"
    "                         ++ An example of two quite large fonts on my computer\n"
    "                            (which at this time has a 108 dot per inch display):\n"
    "       '-adobe-courier-bold-r-normal--34-240-100-100-m-200-iso8859-1\n"
    "       '-b&h-lucidatypewriter-medium-r-normal-sans-34-240-100-100-m-200-iso8859-1'\n"
    "                            Note that to use the latter font on the command line,\n"
    "                            you have to enclose the name in quotes, as shown above,\n"
    "                            since the 'foundry name' includes the character '&'.\n"
    "                            To use it in an alias, you need to do something like\n"
    "  alias abig -XXXfontA '-b\\&h-lucidatypewriter-medium-r-normal-sans-34-240-100-100-m-200-iso8859-1'\n"
    "                         ++ When setting the fonts, it is often helpful\n"
    "                            to set the colors as well.\n"
    "\n"
    " -XXXfontB fontname    = set the X11 font name for somewhat smaller text\n"
    "                         [default = 8x13bold]\n"
    "\n"
    " -XXXfontC fontname    = set the X11 font name for even smaller text\n"
    "                         [default = 7x13]\n"
    "\n"
    " -XXXfontD fontname    = set the X11 font name for the smallest text\n"
    "                         [default = 6x10]\n"
    "\n"
    " -XXX defaults         = set the X11 properties to the AFNI defaults\n"
    "                         (the purpose of this is to restore things )\n"
    "                         (to normal if the X11 settings get mangled)\n"
    "\n"
    " -XXXnpane P           = set the number of 'panes' in the continuous\n"
    "                         colorscale to the value 'P', where P is an\n"
    "                         even integer between 256 and 2048 (inclusive).\n"
    "                         Probably will work best if P is an integral\n"
    "                         multiple of 256 (e.g., 256, 512, 1024, 2048).\n"
    "                         [This option is for the mysterious Dr ZXu.]\n"
    "\n"
   ) ;

   printf("\n"
    "--------------------------------------\n"
    "Educational and Informational Material\n"
    "--------------------------------------\n"
    "* The presentations used in our AFNI teaching classes at the NIH can\n"
    "   all be found at\n"
    " https://afni.nimh.nih.gov/pub/dist/edu/latest/      (PowerPoint directories)\n"
    " https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/ (PDF directory)\n"
     "\n"
    "* And for the interactive AFNI program in particular, see\n"
    " https://afni.nimh.nih.gov/pub/dist/edu/latest/afni01_intro/afni01_intro.pdf\n"
    " https://afni.nimh.nih.gov/pub/dist/edu/latest/afni03_interactive/afni03_interactive.pdf\n"
     "\n"
    "* For the -help on all AFNI programs, plus the README files, and more, please see\n"
    " https://afni.nimh.nih.gov/pub/dist/doc/program_help/index.html\n"
     "\n"
    "* For indvidualized help with AFNI problems, and to keep up with AFNI news, please\n"
    "   use the AFNI Message Board:\n"
    " https://discuss.afni.nimh.nih.gov\n"
     "\n"
    "* If an AFNI program crashes, please include the EXACT error messages it outputs\n"
    "   in your message board posting, as well as any other information needed to\n"
    "   reproduce the problem.  Just saying 'program X crashed, what's the problem?'\n"
    "   is not helpful at all!  In all message board postings, detail and context\n"
    "   are highly relevant.\n"
     "\n"
    "* Also, be sure your AFNI distribution is up-to-date.  You can check the date\n"
    "   on your copy with the command 'afni -ver'.  If it is more than a few months\n"
    "   old, you should update your AFNI binaries and try the problematic command\n"
    "   again -- it is quite possible the problem you encountered was already fixed!\n"
#if 0
    "\n"
    "* For some fun, see this image:\n"
    " https://afni.nimh.nih.gov/pub/dist/doc/program_help/images/afni_splashes.gif\n"
#endif
   ) ;

   AFNI_list_papers(NULL) ;  /* 02 May 2014 */
   printf(
    "\n"
    "POSTERS on varied subjects from the AFNI development group can be found at\n"
    "  * https://afni.nimh.nih.gov/sscc/posters\n"
   ) ;

   /*........................................................................*/
#undef  NSLIDE
#define NSLIDE 34
   if( AFNI_yesenv("AFNI_POMOC") ){  /* for the Web -help page */
     int ii ;
     printf("\n"
            "------------------------------------------------------------------------------------\n"
            "                  SLIDE IMAGES to help with learning the AFNI GUI\n"
            "           https://afni.nimh.nih.gov/pub/dist/doc/program_help/images/afni03/\n"
            "------------------------------------------------------------------------------------\n"
     ) ;
     for( ii=1 ; ii <= NSLIDE ; ii++ ){
       printf(
        "https://afni.nimh.nih.gov/pub/dist/doc/program_help/images/afni03/Slide%02d.png\n"
        "------------------------------------------------------------------------------------\n"
        , ii ) ;
     }
   } else {
     printf("\n"
            "SLIDE IMAGES to help with learning the AFNI GUI can be found at\n"
            "  * https://afni.nimh.nih.gov/pub/dist/doc/program_help/images/afni03/\n"
     ) ;
   }
   printf("\n") ;
   /*........................................................................*/

   exit(0) ;
}

/*----------------------------------------------------------------------
   parse command line switches and store results in a data structure
------------------------------------------------------------------------*/

void AFNI_parse_args( int in_argc , char *in_argv[] )
{
   int narg = 1 ;
   char *env_orient , *env ;
   int    argc=in_argc ,   new_argc      ; /* 18 Nov 1999 */
   char **argv=in_argv , **new_argv=NULL ;

ENTRY("AFNI_parse_args") ;

   if( argc > 1 && strcmp(argv[1],"-help")    == 0 ) AFNI_syntax() ;

   GLOBAL_argopt.dz       = 1.0 ;          /* set up defaults */
   GLOBAL_argopt.dy       = 1.0 ;
   GLOBAL_argopt.ignore   = INIT_ignore ;
   GLOBAL_argopt.elide_quality  = 0 ;      /* Dec 1997 */
   GLOBAL_argopt.no_frivolities = 0 ;      /* 01 Aug 1998 */
   GLOBAL_argopt.install_cmap   = 0 ;      /* 14 Sep 1998 */
   GLOBAL_argopt.read_1D        = 1 ;      /* 27 Jan 2000 */
   GLOBAL_argopt.read_tcsv      = 1 ;      /* 16 Jun 2020 */

   GLOBAL_argopt.enable_suma    = 1 ;      /* 29 Aug 2001 */
   GLOBAL_argopt.disable_done   = 0 ;      /* 21 Aug 2008 */

   GLOBAL_argopt.yes_niml       = AFNI_yesenv("AFNI_NIML_START") ;
   /* GLOBAL_argopt.port_niml      = 0 ;      10 Dec 2002 - Blocked, ZSS 2011*/

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
     else if( SESSTRAIL > 4 ) SESSTRAIL = 4 ;  /* 30 May 2018 */
   }

   GLOBAL_argopt.elide_quality = AFNI_yesenv("AFNI_MARKERS_NOQUAL") ;

   /* 24 Sep 2000: get the default layout name (add $HOME) */

   { char *lf = getenv("AFNI_LAYOUT_FILE") ;
     if( lf != NULL ){
        char *eh = getenv("HOME") , *ff ;
        int ll = strlen(lf) + 8 ;
        if( eh != NULL ) ll += strlen(eh) ;
        ff = AFMALL(char, ll) ;
        if( eh != NULL && lf[0] != '/' ){ strcpy(ff,eh) ; strcat(ff,"/") ; }
        else                            { ff[0] = '\0' ; }
        strcat(ff,lf) ;
        GLOBAL_argopt.layout_fname = ff ;
     }
   }

   /* 21 Jan 2003: get the startup script name */

   { char *lf = getenv("AFNI_STARTUP_SCRIPT") ;
     if( lf == NULL ) lf = ".afni.startup_script" ;
     if( lf != NULL ){
       char *eh = NULL , *ff ;
       int ll = strlen(lf) + 8 ;
       if( !THD_is_file(lf) && lf[0] == '.' ){
         if( THD_is_file(lf+1) ) lf = lf+1 ;
       }
       if( !THD_is_file(lf) && lf[0] != '/' ) eh = getenv("HOME") ;
       if( eh != NULL ) ll += strlen(eh) ;
       ff = AFMALL(char, ll) ;
       if( eh != NULL ){ strcpy(ff,eh) ; strcat(ff,"/") ; }
       else            { ff[0] = '\0' ; }
       strcat(ff,lf) ;
       GLOBAL_argopt.script_fname = ff ;
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
   GLOBAL_argopt.auto_purge    = (RwcBoolean)!AFNI_noenv("AFNI_AUTOPURGE") ;
#endif
   GLOBAL_argopt.resize_images = False ;       /* False means all images must match */
   GLOBAL_argopt.keep_logo     = False ;       /* For making pretty pictures? */
   GLOBAL_argopt.pos_func      = INIT_posfunc ;/* Positive valued functions? */
   GLOBAL_argopt.recurse       = 0 ;           /* Recurse on session directories? */
   GLOBAL_argopt.xtwarns       = 0     ;       /* > 0 means keep Xt warnings turned on */
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

   strcpy(GLOBAL_argopt.title_name,"AFNI") ;           /* default title bar name */

   GLOBAL_argopt.left_is_left      = AFNI_yesenv( "AFNI_LEFT_IS_LEFT" ) ;
   GLOBAL_argopt.left_is_posterior = AFNI_yesenv( "AFNI_LEFT_IS_POSTERIOR" ) ;

   GLOBAL_argopt.read_tim = 0 ;   /* 19 Oct 1999 */

   GLOBAL_argopt.cat_sess = !AFNI_noenv("AFNI_ALL_DATASETS") ; /* 02 Jun 2016 */

   /* 04/06/2020 discoraj */
   GLOBAL_argopt.all_dsets_startup = AFNI_yesenv("ALL_DSETS_STARTUP") ;

   /* Jan 2022 ZSS */
   GLOBAL_argopt.read_tcsv = !AFNI_yesenv("AFNI_SKIP_TCSV_SCAN") ;

   while( narg < argc ){

      if( argv[narg][0] != '-' ) break ;   /* no - ==> quit */

#ifdef USE_TRACING
      if( strncmp(argv[narg],"-trace",5) == 0 ){
         DBG_trace = 1 ;
         narg++ ; continue ;
      }
      if( strncmp(argv[narg],"-TRACE",5) == 0 ){  /* 23 Aug 1998 */
         DBG_trace = 2 ;
         if( MAIN_shell != NULL )
            XSynchronize(XtDisplay(MAIN_shell),TRUE) ; /* 01 Dec 1999 */
         narg++ ; continue ;
      }
#endif

#if 0
      /*----- -Dname=val -- set environment variable [22 Mar 2005] -----*/

      if( strncmp(argv[narg],"-D",2) == 0 && strchr(argv[narg],'=') != NULL ){
        (void) AFNI_setenv( argv[narg]+2 ) ;
        narg++ ; continue ;                 /* go to next arg */
      }
#endif

      /*-----   -bysub or -BIDS  -----*/

      if( strcasecmp(argv[narg],"-bysub") == 0 ||
          strcasecmp(argv[narg],"-BIDS")  == 0   ){  /* 01 Feb 2018 */
        int bb ; char **alist ; int nba,nta,aa , inc_narg ;

        narg++ ;
        if( narg >= argc || argv[narg][0] == '-' ){     /* trickery to use */
          alist = (char **)malloc(sizeof(char *)*1) ;   /* "." if aren't */
          alist[0] = "." ; nba = 0 ; nta = 1 ;          /* any args here */
          inc_narg = 0 ;
        } else {                                 /* scan the full arg list */
          alist = argv ; nba = narg ; nta = argc ; inc_narg = 1 ;
        }

        for( aa=nba ;                             /* loop over args */
             aa < nta && alist[aa][0] != '-' ;    /* following option */
             aa++ , narg += inc_narg           ){

          if( THD_is_directory(alist[aa])        &&
              strncmp(alist[aa],"sub-",4) != 0   &&    /* method 2 */
              alist[aa][0]                != '-'   ){  /* find all sub- names [15 Apr 2019] */

            char *cmd=NULL , *flist=NULL , *qmd=NULL ; NI_str_array *qsar=NULL ; int qq ;
            cmd = (char *)malloc(sizeof(char)*(128+strlen(alist[aa]))) ;
            sprintf( cmd, "find %s -maxdepth 1 -type d -name 'sub-*'", alist[aa] ) ;
            flist = THD_suck_pipe( cmd ) ; free(cmd) ;
            if( flist == NULL || strlen(flist) < 4 ){
              WARNING_message("-bysub: Didn't find any 'sub-*' under %s",alist[aa]) ;
              continue ;
            }
            qsar = NI_decode_string_list( flist , ";" ) ;
            if( qsar == NULL || qsar->num == 0 ){    /* should never happen */
              WARNING_message("-bysub: Didn't find any 'sub-*' under %s",alist[aa]) ;
              free(flist) ;
              continue ;
            }
            for( qq=0 ; qq < qsar->num ; qq++ ){
              cmd = qsar->str[qq] ; if( cmd == NULL ) continue ;
              bb  = strlen(cmd) ;   if( bb  <  2    ) continue ;
              if( cmd[bb-1] == '/' ) cmd[bb-1] = '\0' ;
              qmd = strrchr(cmd,'/') ;
              if( qmd != NULL ){ qmd++ ; } else { cmd = qmd ; }
              bysub = (char **)realloc(bysub,sizeof(char *)*(num_bysub+1)) ;
              bysub[num_bysub++] = strdup(qmd) ;
              bysub_dir = (char **)realloc(bysub_dir,sizeof(char *)*(num_bysub_dir+1)) ;
              bysub_dir[num_bysub_dir++] = strdup(alist[aa]) ;
            }

          } else {                             /* method 1: a sub-XXX name */

            bysub = (char **)realloc(bysub,sizeof(char *)*(num_bysub+1)) ;
            if( strncmp(alist[aa],"sub-",4) == 0 ){
              bysub[num_bysub] = strdup(alist[aa]) ;
            } else {
              bysub[num_bysub] = (char *)malloc(sizeof(char)*(strlen(alist[aa])+8)) ;
              sprintf( bysub[num_bysub] , "sub-%s" , alist[aa] ) ;
            }
            bb = strlen(bysub[num_bysub]) ;
            if( bb > 1 && bysub[num_bysub][bb-1] == '/' )
              bysub[num_bysub][bb-1] = '\0' ;
            num_bysub++ ;
          }

        } /* end of loop over -bysub args */

        if( alist != argv ) free(alist) ;
        if( narg < argc && strcmp(argv[narg],"-") == 0 ) narg++ ;
        continue ;
      }

      if( strcmp(argv[narg],"-seehidden") == 0 ){
        first_plugin_check = 1 ;
        narg++ ; continue ;
      }

      /*----- -layout (23 Sep 2000) -----*/

      if( strcmp(argv[narg],"-layout") == 0 ){
         if( narg+1 >= argc ) ERROR_exit("need an argument after -layout!") ;
         GLOBAL_argopt.layout_fname = argv[++narg] ;  /* just a pointer */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -no_frivolities option (22 Apr 2022) ----- */

      if( strcmp(argv[narg],"-no_frivolities") == 0 ){  /* 22 Apr 2022 [rcr] */
         GLOBAL_argopt.no_frivolities = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -no1D option (27 Jan 2000) ----- */

      if( strncmp(argv[narg],"-no1D",5) == 0 ){
         GLOBAL_argopt.read_1D = 0 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -notcsv option (16 Jun 2020) ----- */

      if( strncmp(argv[narg],"-notcsv",7) == 0 ||
          strncmp(argv[narg],"-notsv" ,6) == 0 ||
          strncmp(argv[narg],"-nocsv" ,6) == 0   ){
         GLOBAL_argopt.read_tcsv = 0 ;
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
#if 0
#ifdef USE_TRACING
         DBG_trace                    =  0 ;  /* 26 Jan 2001 */
#endif
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

      /*---- -comsep c [22 Feb 2007] ----*/

      if( strcmp(argv[narg],"-comsep") == 0 ){
        char cc ;
        if( ++narg >= argc ) ERROR_exit("need an argument after -comsep") ;
        cc = argv[narg][0] ;
        if( cc=='\0' || isalnum(cc) || isspace(cc) || cc=='\'' || cc=='\"' )
          ERROR_message("Illegal character after -comsep") ;
        else
          comsep = cc ;

        narg++ ; continue ;
      }

      /*---- -com ccc [29 Jul 2005] ----*/

      if( strcmp(argv[narg],"-com") == 0 ){
        int ii , ll ; char *cm , *cs , *cq ;
        if( ++narg >= argc ) ERROR_exit("need an argument after -com!");
        cm = argv[narg] ; ll = strlen(cm) ; cs = strchr(cm,comsep) ;
             if( ll > 1024   ) ERROR_message("argument after -com is too long:\n'%s'" ,argv[narg] );
        else if( ll <   3   ) ERROR_message ("argument after -com is too short:\n'%s'",argv[narg] );
        else if( cs == NULL ) {
           if (COM_num < MAX_N_COM) COM_com[ COM_num++ ] = strdup(argv[narg]) ;
           else ERROR_message("Too many commands (max allowed=%d)",MAX_N_COM);
        } else {  /* 22 Feb 2007: break into sub-commands */
          cq = cm = strdup(argv[narg]) ;
          for( ii=ll-1 ; isspace(cm[ii]) ; ii-- ) cm[ii] = '\0' ; /* trim end */
          cs = strchr(cm,comsep) ;
          while(1){
            *cs = '\0' ;  /* NUL terminate command at separator */
            for( ; *cm != '\0' && isspace(*cm) ; cm++ ) ; /* trim front */
            ll = strlen(cm) ;
            if( ll > 2 && ll <= 1024 ) {
               if (COM_num < MAX_N_COM) COM_com[ COM_num++ ] = strdup(cm) ;
               else ERROR_message("Too many commands in total.");
            } cm = cs+1 ; if( *cm == '\0' ) break ;  /* reached the end */
            cs = strchr(cm,comsep) ;               /* search for next sep */
            if( cs == NULL ) cs = cm + strlen(cm) ;
          }
          free(cq) ;
        }

        narg++ ; continue ;  /* go to next arg */
      }

      /*---- -disable_done [21 Aug 2008, rickr] ----*/
      /*     This flag disables use of the "done" and X buttons.
             Close afni via plugout_drive or kill.                  */

      if( strcmp(argv[narg],"-disable_done") == 0 ){
         GLOBAL_argopt.disable_done = 1 ;
         narg++ ; continue ;
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
         if( narg+1 >= argc ) ERROR_exit("need an argument after -tbar!");
         MCW_strncpy(GLOBAL_argopt.title_name,argv[++narg],32) ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -xtwarns option -----*/

      if( strncmp(argv[narg],"-xtwarns",6) == 0 ){
         GLOBAL_argopt.xtwarns = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[narg],"-XTWARNS",6) == 0 ){
         GLOBAL_argopt.xtwarns = 2 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -XXX [24 Mar 2016] -----*/

      if( strncasecmp(argv[narg],"-XXX",4) == 0 ){  /* all -XXX options are */
        narg += 2 ; continue ;                      /* followed by one arg */
      }
      if( strcasecmp(argv[narg],"-big")   == 0 ){ narg++; continue; }
      if( strcasecmp(argv[narg],"-plus")  == 0 ){ narg++; continue; }
      if( strcasecmp(argv[narg],"-minus") == 0 ){ narg++; continue; }
      if( strcasecmp(argv[narg],"-norm")  == 0 ){ narg++; continue; }

      /*----- -destruct option -----*/

      if( strncmp(argv[narg],"-destruct",6) == 0 ){   /** has no effect at present **/
         fprintf(stderr,"\n** -destruct option not implemented at present! **\n") ;
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
            else ERROR_exit("illegal -R option!") ;
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
      if( strncmp(argv[narg],"-nopurge",6) == 0 ){
         GLOBAL_argopt.auto_purge = False ;
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
         if( narg+1 >= argc ) ERROR_exit("need an argument after -orient!");

         MCW_strncpy(GLOBAL_argopt.orient_code,argv[++narg],4) ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -ignore # option -----*/

      if( strncmp(argv[narg],"-ignore",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -ignore!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val >= 0 ) GLOBAL_argopt.ignore = (int) val ;
         else fprintf(stderr,
                "\n** WARNING: -ignore value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -im1 # option [must come before '-im' option!] -----*/

      if( strncmp(argv[narg],"-im1",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -im1!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val >= 1 ) GLOBAL_argopt.ignore = (int) (val-1.0) ;
         else fprintf(stderr,
                "\n** WARNING: -ignore value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }


      /*----- -dy # option -----*/

      if( strncmp(argv[narg],"-dy",3) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -dy!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 0 ) GLOBAL_argopt.dy = val ;
         else fprintf(stderr,
                "\n** WARNING: -dy value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -dz # option -----*/

      if( strncmp(argv[narg],"-dz",3) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -dz!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 0 ) GLOBAL_argopt.dz = val ;
         else fprintf(stderr,
                "\n** WARNING: -dz value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -gamma # option -----*/

      if( strncmp(argv[narg],"-gamma",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -gamma!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 0 ) GLOBAL_argopt.gamma = val ;
         else fprintf(stderr,
                "\n** WARNING: -gamma value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }

#ifdef USE_GSFAC
      /*----- -gsfac # option -----*/

      if( strncmp(argv[narg],"-gsfac",4) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -gsfac!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val != 0 ) GLOBAL_argopt.gsfac = val ;
         else fprintf(stderr,
                "\n** WARNING: -gsfac value %s illegal\n", argv[narg]);

         narg++ ; continue ;  /* go to next arg */
      }
#endif

      /*----- -datum type option -----*/

      if( strncmp(argv[narg],"-datum",6) == 0 ){
         if( ++narg >= argc ) ERROR_exit("need an argument after -datum!") ;

         if( strcmp(argv[narg],"short") == 0 ){
            GLOBAL_argopt.datum= MRI_short ;
         } else if( strcmp(argv[narg],"float") == 0 ){
            GLOBAL_argopt.datum= MRI_float ;
         } else if( strcmp(argv[narg],"complex") == 0 ){
            GLOBAL_argopt.datum= MRI_complex ;
         } else if( strcmp(argv[narg],"byte") == 0 ){
            GLOBAL_argopt.datum= MRI_byte ;
         } else {
            char buf[1024] ;
            sprintf(buf,"-datum of type '%s' is not supported in AFNI!",
                   argv[narg] ) ;
            ERROR_exit(buf) ;
         }
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -ncolor # option -----*/

      if( strncmp(argv[narg],"-ncolor",3) == 0 ){
         float val ;
         if( narg+1 >= argc ) ERROR_exit("need an argument after -ncolor!");

         val = strtod( argv[++narg] , NULL ) ;
         if( val > 2 ) GLOBAL_argopt.ncolor = val ;
         else fprintf(stderr,
                "\n** WARNING: -ncolor value %s illegal\n", argv[narg]);

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
         fprintf(stderr,
          "\n** WARNING: option '%s' for viewing plain images is old and not very useful\n" ,
          argv[narg] ) ;
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
               ERROR_exit("Illegal value in %s",argv[narg]);
            }
         }

         /* negate flag for time-order first (-tim) vs z-order first (-zim) */

         if( strncmp(argv[narg],"-tim",4)==0 && GLOBAL_argopt.read_tim > 1 )
            GLOBAL_argopt.read_tim = - GLOBAL_argopt.read_tim ;

         narg++ ; continue ;  /* go to next arg */
      }

      /* -port_number and quit */
      if( strncmp(argv[narg],"-port_number", 8) == 0) {
         int pp = 0;
         if( ++narg >= argc )
            ERROR_exit("need an argument after -port_number!");
         pp = get_port_named(argv[narg]);
         if (strcmp(argv[narg-1], "-port_number_quiet")) {
            fprintf(stdout, "\nPort %s: %d\n", argv[narg], pp);
         } else {
            fprintf(stdout, "%d\n", pp);
         }
         if (pp < 1) exit(1);
         else exit(0);
      }

      /*----- -nomall option -----*/

      if( strncmp(argv[narg],"-nomall",5) == 0 ){    /* was handled in main() */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -motif_ver option -----*/

      if( strncmp(argv[narg],"-motif_ver",10) == 0 ){/* was handled in main() */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -no_detach option -----*/

      if( strncmp(argv[narg],"-no_detach",7) == 0 ){/* was handled in main() */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -q option -----*/

      if( strcmp(argv[narg],"-q") == 0 ){            /* was handled in main() */
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- all data sets 04/06/2020 discoraj -----*/
      if( strcmp(argv[narg],"-all_dsets") == 0 ){

          // check for env variable that overrides -all_dsets
          if( AFNI_noenv("AFNI_ALL_DATASETS") ){
              fprintf(stderr,
                  "\n\n** WARNING: option -all_dsets is ignored silently.") ;
              fprintf(stderr,"\n            AFNI_ALL_DATASETS = NO.\n") ;
              GLOBAL_argopt.all_dsets_startup = 0 ;
           }
           else { GLOBAL_argopt.all_dsets_startup = 1 ; }

         GLOBAL_argopt.all_dsets_startup = 1 ;
         narg++ ; continue ;  /* go to next arg */
      }

      /*----- -- option -----*/

      if( strcmp(argv[narg],"--") == 0 ){
         narg++ ; break ;  /* end of args */
      }

      /*----- if we get here, bad news for America! -----*/

      ERROR_message("Unknown option %s ***",argv[narg]) ;
      suggest_best_prog_option(argv[0], argv[narg]);
      ERROR_exit   ("Try 'afni -help' for a list of command line options.") ;

   } /* end of loop over argv's starting with '-' */

   if( num_bysub > 0 && GLOBAL_argopt.recurse ){  /* 01 Feb 2018 */
     WARNING_message("-bysub disables -R option") ;
     GLOBAL_argopt.recurse = 0 ;
   }

#if 0
#ifdef USE_TRACING
   if( ALLOW_realtime ) DBG_trace = 0 ; /* 26 Jan 2001 */
#endif
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
   It does very little -- it replaces the default Xt warning handler.
-------------------------------------------------------------------------*/

void AFNI_handler(char *msg){
   if( GLOBAL_argopt.xtwarns > 0 &&
       msg != NULL               &&
       strstr(msg,"Attempt to add wrong") == NULL ){
     ERROR_message("Xt message: %s", msg ) ;
     TRACEBACK ;
   }
   return ;
}

/*-----------------------------------------------------------------------
   A terminal version.  This cannot even return.      [23 Apr 2025 rickr]

   Some compilers require __attribute__((noreturn)) for a function passed
   to XtAppSetErrorHandler.  But in that case, exit() might be required,
   rather than return.

   Since AFNI_handler is also used for XtAppSetWarningHandler, it should
   not exit().  So add this new function just for XtAppSetErrorHandler.
-------------------------------------------------------------------------*/

static __attribute__((noreturn)) void AFNI_exit_handler(char *msg) {
   ERROR_message("terminal failure, exiting...") ;
   if( GLOBAL_argopt.xtwarns > 0 &&
       msg != NULL               &&
       strstr(msg,"Attempt to add wrong") == NULL )
   {
     ERROR_message("Xt message: %s", msg ) ;
     TRACEBACK ;
   }
   exit(1) ;
}

/*-----------------------------------------------------------------------*/
/*! Avoid fatal X11 errors.  Stupid thing is so touchy sometimes. */

int AFNI_xerrhandler( Display *d , XErrorEvent *x ){
  if( GLOBAL_argopt.xtwarns > 0 ){
    char buf[1024] = "(null)" ;
    if( x != NULL && d != NULL ) XGetErrorText( d,x->error_code , buf,255 ) ;
    ERROR_message( "Intercepted fatal X11 error: %s\n",buf) ;
    TRACEBACK ;
  }
  return 0 ;
}

/*-----------------------------------------------------------------------*/
/* Signal handler for fatal errors; prints out some info before death. */

#include <signal.h>
void AFNI_sigfunc(int sig)
{
   char *sname ;
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
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname); fflush(stderr);
   TRACEBACK ;
   fprintf(stderr,"** AFNI version = " AVERZHN "  Compile date = " __DATE__ "\n" );
#ifdef SHSTRING
   fprintf(stderr,"** [[Precompiled binary " SHSTRING ": " __DATE__ "]]\n") ;
#endif
   fprintf(stderr,"** AFNI Program Is Dead :-( **\n") ;
   fflush(stderr) ;
   if( sig != SIGINT && sig != SIGTERM ){  /* add crashlog [13 Apr 2015] */
     FILE *dfp ; char *home , fname[1024] ;
     fprintf(stderr,"** If you report this crash to the AFNI message\n"
                    "** board, please copy the error messages EXACTLY.\n") ;
     home = getenv("HOME") ;
     if( home != NULL ){
       strcpy(fname,home) ; strcat(fname,"/.afni.crashlog") ;
     } else {
       strcpy(fname,".afni.crashlog") ;
     }
     dfp = fopen( fname , "a" ) ;
     if( dfp != NULL ){
       fprintf(dfp,"\n*********-----------------------------------------------*********") ;
       fprintf(dfp,"\nFatal Signal %d (%s) received\n",sig,sname); fflush(stderr);
#ifdef USE_TRACING
       DBG_tfp = dfp ; DBG_traceback() ; DBG_tfp = stderr ;
#endif
       fprintf(dfp,"** AFNI version = " AVERZHN "  Compile date = " __DATE__ "\n" );
#ifdef SHSTRING
       fprintf(dfp,"** [[Precompiled binary " SHSTRING ": " __DATE__ "]]\n") ;
#endif
       fprintf(dfp,"** AFNI Program Hideous Death **\n") ;
       fclose(dfp) ;
       fprintf(stderr,"** Crash log appended to file %s\n",fname) ;
     }
   }
   exit(1) ;
}

/*----------------------------------------------------------------------------*/
/* The functions below implement the delayed quit feature:
     - when signal SIGQUIT==3 is sent to AFNI, AFNI_sigfunc_quit() gets called
     - which uses alarm() to send signal SIGALRM after 5 sec
     - which invokes AFNI_sigfunc_alrm() == this function
     - which says something cute and dies
   This catenation of events is for Jerzy 'the Mad Pole' Bodurka.
------------------------------------------------------------------------------*/

extern int selenium_close(void) ;

/* the goodbye messages are now stored in afni_startup_tips.h */

void AFNI_sigfunc_alrm(int sig)
{
   int nn ;

#ifdef NGBY
   /* this first part is to print out 1 or more goodbye messages */

   srand48((long)time(NULL)+(long)getpid()) ; /* reset random number generator */
   nn = (lrand48()>>3) % NGBY ;               /* first message index */

   if( !AFNI_yesenv("AFNI_NEVER_SAY_GOODBYE") ){
     if( nn < NGBY ){
#undef  NDUN
#define NDUN (sizeof(dun)/sizeof(char *))
       static char *dun[] = { "is done" , "wraps up"   , "concludes" ,
                              "is over" , "terminates" , "finishes"   } ;
       if( sig >= 0 ){
         printf("\n** AFNI %s: %s!  [%d/%d]\n\n" ,
                dun[lrand48()%NDUN],gby[nn],nn+1,NGBY) ;
       } else {
         int kk ;
         int dn=AFNI_find_relprime_random(NGBY), ktop=(-sig < NGBY) ? -sig : NGBY ;
         for( kk=0 ; kk < ktop ; kk++ ){
           printf( "%s!\n\n" , gby[nn] + ((gby[nn][0]=='\n') ? 1 : 0) ) ;
           nn = (nn+dn)%NGBY ;
         }
       }
     }
     /** MCHECK ; **/
   }
#endif /* NGBY */

   /* meltdown controller on exit? (disabled) */
#if 0
   if( sig <= 0 && !NO_frivolities ){
     Three_D_View *im3d = AFNI_find_open_controller() ;
     char *eee = getenv("AFNI_SPLASH_MELT") ;
     if( eee == NULL ) eee = "?" ; else eee[0] = toupper(eee[0]) ;
     if( im3d   != NULL  && MCW_widget_visible(im3d->vwid->top_shell) &&
         eee[0] != 'N'   && (eee[0] == 'Y' || lrand48()%19==0)           ){
       int jj ;
       XMapRaised( XtDisplay(im3d->vwid->top_shell) ,
                   XtWindow(im3d->vwid->top_shell)   ) ; /* raise controller */
       AFNI_sleep(111);
       MCW_melt_widget( im3d->vwid->top_form ) ;
     }
     sig = 0 ;
   }
#endif

   selenium_close(); /* close any selenium opened browser windows if open */
   exit(sig);
}

/*-------------------------------------------------------------------------*/
/* Called for sig=3 (cf. main() function) */

void AFNI_sigfunc_quit(int sig)
{
  unsigned int nsec = (unsigned int)AFNI_numenv("AFNI_SIGQUIT_DELAY") ;
  if( nsec == 0 || nsec > 30 ) nsec = 5 ;
  fprintf(stderr,
          "\n** AFNI received QUIT signal ==> exit in %u seconds! **\n",nsec) ;
  signal(SIGALRM,AFNI_sigfunc_alrm) ;  /* call the actual death dealer */
  (void)alarm(nsec) ;                  /* after a quick cigarette break */
  return ;
}

/*-------------------------------------------------------------------------*/
/*! Check if a particular option is present; 1=yes, 0=no.  [15 Jan 2004]
---------------------------------------------------------------------------*/

static int check_string( char *targ , int ns , char *ss[] )
{
   int ii , lt ;
   if( targ == NULL || *targ == '\0' || ns <= 0 || ss == NULL ) return 0 ;
   lt = strlen(targ) ;
   for( ii=0 ; ii < ns ; ii++ )
     if( ss[ii] != NULL && strncmp(ss[ii],targ,lt) == 0 ) return 1 ;
   return 0 ;
}

/*----------------------------------------------------------------------------*/
/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/
/*=============================================================================
  The new (and improved) AFNI main program.
    02 Aug 1999: Have moved much of the startup into a work process.
==============================================================================*/
/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ii ; int dienow=0 ;

   /*--- help the pitiful user? ---*/

   if( argc > 1 && strcasecmp(argv[1],"-help")    == 0 ) AFNI_syntax() ;

   if( argc > 1 && strncasecmp(argv[1],"-goodbye",6) == 0 ){
     printf("\n") ;
     if( argc > 2 && strcasecmp(argv[2],"ALL") == 0 ){ /* 30 Jan 2018 */
       AFNI_sigfunc_alrm(-666666) ;
     } else {
       ii = (argc > 2 ) ? abs((int)rintf((strtod(argv[2],NULL)))) : 0 ;
       AFNI_sigfunc_alrm(-ii) ;
     }
   }

   if( argc > 1 && strncasecmp(argv[1],"-startup",6) == 0 ){ /* 05 Jan 2018 */
     int jj ;
     if( argc > 2 && strcasecmp(argv[2],"ALL") == 0 ){
       for( jj=0 ; jj < NTIP ; jj ++ ) AFNI_print_startup_tip(jj) ;
     } else {
       srand48((long)time(NULL)+(long)getpid()) ;
       ii = (argc > 2 ) ? abs((int)rintf((strtod(argv[2],NULL)))) : 1 ;
       for( jj=0 ; jj < ii ; jj++ ) AFNI_print_startup_tip(-1) ;
     }
     exit(0) ;
   }

   if( argc > 1 && strcasecmp(argv[1],"-julian") == 0 ){ /* 30 Oct 2018 */
     printf("%s days since noon 01 Jan 4713 BC\n",julian_date_string()) ;
     exit(0) ;
   }

   /** Check for -version [15 Aug 2003] **/


   if( check_string("-ver"    ,argc,argv) || check_string("--ver"    ,argc,argv) ||
       check_string("-version",argc,argv) || check_string("--version",argc,argv)   ){
      show_AFNI_version() ;
      dienow++ ;
   }

   if( check_string("-vnum" , argc, argv) ) {
      show_AFNI_vnum();
      dienow++ ;
   }

   if( check_string("-package" , argc, argv) ) {
      show_AFNI_package();
      dienow++ ;
   }

   if( check_string("-tips" , argc, argv) ) {
      show_AFNI_readme_gui();
      dienow++ ;
   }

   if( check_string("-env" , argc, argv) ) {
      show_AFNI_readme_env();
      dienow++ ;
   }

   /** MOTD output **/

   if( check_string("--motd",argc,argv) || check_string("-motd",argc,argv) ){   /* 29 Nov 2005 */
     AFNI_display_motd(NULL) ; dienow++ ;
   }

   /** just print the SHOWOFF string [26 Oct 2004] **/

   if( check_string("-show",argc,argv) || check_string("--show",argc,argv) ){
#ifdef SHSTRING
      printf( SHSTRING "\n" ) ;
#else
      printf("Unknown\n") ;
#endif
      dienow++ ;
   }

   /** debug stuff **/

#ifdef USING_MCW_MALLOC
   if( !check_string("-nomall",argc,argv) && !check_string("-rt",argc,argv) )
     enable_mcw_malloc() ;
#endif

#ifdef USE_TRACING
   if( check_string("-trace",argc,argv) ) DBG_trace = 1 ;
   if( check_string("-TRACE",argc,argv) ) DBG_trace = 2 ;
#endif

   if( check_string("-motif_ver",argc,argv) ) {
     show_motif_version_string() ;
     dienow++ ;
   }

   /* check the processed environment, the afni way: machdep/prefilter
    * and the common main() way: prefilter/machdep  19 Sep 2013 [rickr] */

   if( check_string("-get_processed_env_afni",argc,argv) ) {
     AFNI_prefilter_args( &argc , &argv );
     machdep();
     system("env | grep -e '^AFNI' -e '^NIFTI' | sort");
     dienow++ ;
   }
   else if( check_string("-get_processed_env",argc,argv) ) {
     machdep();
     AFNI_prefilter_args( &argc , &argv );
     system("env | grep -e '^AFNI' -e '^NIFTI' | sort");
     dienow++ ;
   }

   if( check_string("-global_opts",argc,argv) ) {      /* list global */
     fputs(SUMA_Offset_SLines(get_help_help(),3),
                                    stdout);   /* opts used by all AFNI progs */
     fputs(get_gopt_help(), stdout);
     dienow++ ;
   }

   if( check_string("-papers",argc,argv) ){  /* list AFNI papers */
     AFNI_list_papers(NULL) ; dienow++ ;
   }

   /* getting text output, should be early      23 Oct 2015 [rickr] */

   if( check_string("-available_npb_quiet", argc, argv) ) {
      fprintf(stdout,"%d\n",get_available_npb());
      dienow++ ;
   } else if( check_string("-available_npb", argc, argv) ) {
      fprintf(stdout,
              "\nFirst available npb: %d\n",get_available_npb());
      dienow++ ;
   }

   /* -list_ports list and quit */
   if( check_string("-list_ports", argc, argv) ) {
      show_ports_list(); dienow++ ;
   }

   /*** if ordered, die right now ***/

   if( dienow ) exit(0) ;  /* farewell, cruel world */

   /***----- otherwise, perhaps become all detached from reality -----***/

   /* no version until after quick exit checks      23 Oct 2015 [rickr] */
   if( ! check_string("-q",argc,argv) ) show_AFNI_version() ;

   /* Since AFNI_DETACH is applied before machdep() or other my_getenv
      calls, -D cannot be used to apply this env var, so add an option.
                                                     7 Mar 2014 [rickr] */

   if( ! check_string("-no_detach",argc,argv) ) {
      PUTENV("AFNI_DETACH","YES") ;            /* Apr 2013 */
      { char *eee = getenv("AFNI_DETACH") ;    /* 31 May 2011 */
        if( YESSISH(eee) ){
          ii = (int)fork();
          if( ii != 0 ){         /* parent process dies now */
            AFNI_sleep(2345) ;   /* msec */
            fprintf(stderr,"++ AFNI is detached from terminal.\n") ;
            _exit(0) ;
          }
        }
      }
   }

   /*------------- Initialize some more stuff -------------*/

   machdep() ;
/*** INFO_message("before prefilter: argc=%d argv=%p",argc,(void *)argv) ; ***/
   AFNI_prefilter_args( &argc , &argv ) ;  /* 11 Dec 2007 */
/*** INFO_message("after prefilter: argc=%d argv=%p",argc,(void *)argv) ; ***/

   THD_load_datablock_verbose(1) ; /* 21 Aug 2002 */

   signal(SIGINT ,AFNI_sigfunc) ;  /* may be superseded by mainENTRY below */
   signal(SIGBUS ,AFNI_sigfunc) ;
   signal(SIGSEGV,AFNI_sigfunc) ;
   signal(SIGTERM,AFNI_sigfunc) ;

   first_plugin_check = -1 ;  /* 16 Nov 2016 */

#if 0
#ifdef USE_TRACING
   if( ALLOW_realtime ) DBG_trace = 0 ; /* 26 Jan 2001 */
#endif
#endif

   /** 25 Oct 2001: check for -q (quiet) option right away **/

   GLOBAL_argopt.quiet = AFNI_yesenv("AFNI_QUIET") ;
   if( AFNI_VERBOSE && check_string("-q",argc,argv) ) GLOBAL_argopt.quiet = 1;

   /** 12 Dec 2002: scan for "-rt" now,
                    to see if we want to start the version check **/

   GLOBAL_argopt.allow_rt = check_string("-rt",argc,argv) ;

   if( !GLOBAL_argopt.quiet && !ALLOW_realtime )
#if 1 /* 30 Dec 2015 */
     AFNI_start_version_check() ;               /* 21 Nov 2002 */
#else
     AFNI_start_compile_date_check() ;          /* 17 Jun 2014 */
#endif

#ifdef DARWIN
   if( 0 && !THD_is_directory("/sw/bin") && !AFNI_noenv("AFNI_IMSAVE_WARNINGS") )
     WARNING_message("On Mac OS X, it helps if you install the fink software:\n"
                     "            cf. http://fink.sourceforge.net/\n"
                     "            and then do\n"
                     "            fink install netpbm\n" ) ;
#endif

   /** Start the debug traceback stuff (also resets signal handler) **/

   mainENTRY("AFNI:main") ; /* 26 Jan 2001: replace ENTRY w/ mainENTRY */

   signal(SIGQUIT,AFNI_sigfunc_quit) ;  /* For Jerzy -- 09 Jan 2008 */

   /** set the function to call if run out of memory when creating datasets **/

   THD_set_freeup( AFNI_purge_unused_dsets ) ;  /* 18 Oct 2001 */

#if 0
   if( argc > 1 ) AFNI_logger("afni",argc,argv) ; /* 14 Aug 2001 */
#endif

#if 0                                          /* now in machdep() */
   srand48((long)time(NULL)+(long)getpid()) ;  /* initialize random number generator */
#endif

   REPORT_PROGRESS( "\n" ) ;

   /*-- Be friendly or trivial --*/

   if( check_string("-trivia",argc,argv) ){   /* 27 Nov 2007 */
     char **triv ; int ntriv,tt ;
     ntriv = AFNI_get_todays_trivia( &triv ) ;
     for( tt=0 ; tt < ntriv ; tt++ ){
       REPORT_PROGRESS(triv[tt]) ; REPORT_PROGRESS("\n") ;
     }
     REPORT_PROGRESS( "\n" ) ; exit(0) ;
   }

   { char *sf = AFNI_get_friend() ;
     REPORT_PROGRESS( sf ) ;
     REPORT_PROGRESS( "\n\n" ) ;
     if( check_string("-friend",argc,argv) ) exit(0) ;
   }

   /*----- tell the user who we are -----*/

/*   REPORT_PROGRESS( ANNOUNCEMENT ) ;*/

   /*-------------------------------------------------------------*/
   /*------------ initialize the controllers list ----------------*/

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
     GLOBAL_library.controllers[ii] = NULL ;
   GLOBAL_library.dc = NULL ;

   GLOBAL_library.controller_lock = 0 ; ENABLE_LOCK ;
   GLOBAL_library.time_lock = 0 ;                      /* 03 Nov 1998 */
   GLOBAL_library.ijk_lock  = 0 ;                      /* 11 Sep 2000 */
   GLOBAL_library.zoompan_lock = 0 ;                   /* 10 Dec 2019 */
   SET_FIM_bkthr(10.0) ;                               /* 02 Jun 1999 */

   GLOBAL_library.hints_on  = 0 ;                      /* 07 Aug 1999 */

#ifdef ALLOW_PLUGINS
   GLOBAL_library.plugins   = NULL ;
#endif

   GLOBAL_library.session   = NULL ;                   /* 20 Dec 2001 */
   GLOBAL_library.warptable = NULL ;                   /* 28 Aug 2002 */

   GLOBAL_library.realtime_status   = NULL ;           /* 01 Jun 2009 */
   GLOBAL_library.realtime_callback = NULL ;

   /*--------------------------------------------------------------------*/
   /*--- initialize X, toplevel window, defaults, and display context ---*/

   REPORT_PROGRESS("Initializing: X11");

   /*--- check for font size environment control [06 Nov 2018] ---*/

   { char *ep = getenv("AFNI_FONTSIZE") ;
     if( ep != NULL ){
            if( strcasecmp(ep,"minus") == 0 ) XXX_MINUS_FONTS ;
       else if( strcasecmp(ep,"plus")  == 0 ) XXX_PLUS_FONTS ;
       else if( strcasecmp(ep,"big")   == 0 ) XXX_BIG_FONTS ;
       else if( strcasecmp(ep,"norm")  == 0 ) XXX_NORM_FONTS ;
     }
   }

   /*--- look for -XXX options before starting X11 [24 Mar 2016] ---*/

   process_XXX_options( argc , argv ) ;  /* will set new_FALLback */

   if( new_FALLback != NULL ){  /* if found any -XXX options, merge them */
     int qq,pp ;
     if( ! AFNI_yesenv("AFNI_DONT_USE_XRDB") )
       xrdb_pg = THD_find_executable("xrdb") ;

     /* can't find xrdb executable ==> merge FALLback strings */

     if( xrdb_pg == NULL ){
       for( qq=0 ; FALLback[qq] != NULL ; qq++ ){
         for( pp=0 ; new_FALLback[pp] != NULL ; pp++ ){
           if( equiv_FALLback( new_FALLback[pp] , FALLback[qq] ) ) break ;
         }
         if( new_FALLback[pp] == NULL )
           ADDTO_FALLback_one(FALLback[qq]) ;
       }
       for( qq=0 ; new_FALLback[qq] != NULL ; qq++ )
         ININFO_message("new_FALLback[%d] = \"%s\"",qq,new_FALLback[qq]) ;

     } else {  /* use xrdb to merge X11 resources */

#define XXXSIZ 4096
       char *xpg , *xout=NULL ; FILE *fp ;
       xpg = malloc(strlen(xrdb_pg)+64) ;

       /* get the current resources settings */

       sprintf(xpg,"%s -query",xrdb_pg) ;
       fp = popen(xpg,"r") ;
       if( fp != NULL ){
         xout = (char *)malloc(sizeof(char)*XXXSIZ) ; xout[0] = '\0' ;
         while( fgets(xout+strlen(xout),XXXSIZ-2,fp) != NULL ){
           xout = (char *)realloc(xout,sizeof(char)*(strlen(xout)+XXXSIZ)) ;
         }
         (void)pclose(fp) ;
         if( *xout != '\0' ) xrdb_old = xout ;
       }

       /* set the new ones */

       sprintf(xpg,"%s -override -",xrdb_pg) ;
       fp = popen( xpg , "w" ) ;
       if( fp != NULL ){
         for( pp=0 ; new_FALLback[pp] != NULL ; pp++ )
           fprintf(fp,"%s\n",new_FALLback[pp]) ;
         (void)pclose(fp) ;
       }

       /* don't need new_FALLback any more */

       for( pp=0 ; new_FALLback[pp] != NULL ; pp++ ) free(new_FALLback[pp]) ;
       free(new_FALLback) ; new_FALLback = NULL ; free(xpg) ;
#undef XXXSIZ
     }
   }

   /*--- now ready to start X11 for true --*/

#ifdef DARWIN
   { char *eee = getenv("DYLD_LIBRARY_PATH") ;
     char *fff = getenv("AFNI_SKIP_DYLD_WARNING") ;
     if( (fff == NULL || toupper(*fff) != 'Y') &&
         (eee == NULL || strstr(eee,"flat_namespace") == NULL) ){
       int vmajor=0, vminor=0 , vmicro=0 ;
#if 0
       eee = get_XQuartz_version() ;  /* Check XQuartz version [27 Jan 2017] */
       if( eee != NULL && isdigit(*eee) ){
         sscanf(eee,"%d.%d.%d",&vmajor,&vminor,&vmicro) ;
         /* INFO_message("XQuartz version: %d %d %d",vmajor,vminor,vmicro) ; */
       }
#endif
       if( vmajor == 0 || vminor == 0 || vmajor > 2                   ||
           (vmajor == 2 && vminor >  7)                               ||
           (vmajor == 2 && vminor == 7 && (vmicro > 9 || vmicro == 0))  ){
         fprintf(stderr,
          "\n"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
          "++ If you are using XQuartz 2.7.10 (or later), and AFNI crashes when\n"
          " + opening windows, or you cannot type text into AFNI popup windows,\n"
          " + you might need to set an environment variable to solve this problem:\n"
          " +   setenv DYLD_LIBRARY_PATH /opt/X11/lib/flat_namespace    # tcsh\n"
          " +   export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace    # bash\n"
          " + This command is best put in your startup ~/.cshrc file, so that\n"
          " + it will be invoked for every (t)csh shell you open (mutatis mutandis\n"
          " + for the bash shell, of course).\n"
          " +\n"
          " + If things are OK and you want to avoid having this warning message\n"
          " + pollute your terminal, use this:\n"
          " +   setenv AFNI_SKIP_DYLD_WARNING YES                       # tcsh\n"
          " +   export AFNI_SKIP_DYLD_WARNING=YES                       # bash\n\n"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
         ) ;
       }
     }
   }
#endif

   memset(&MAIN_app, 0, sizeof(MAIN_app)) ;  /* 11 Feb 2009 [lesstif patrol] */
   MAIN_shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                                   &argc , argv ,
                                   (new_FALLback!=NULL)?new_FALLback:FALLback ,
                                   NULL ) ;

   if( MAIN_shell == NULL ) ERROR_exit("Cannot initialize X11") ;

#if 1
   { Display *dpy = XtDisplay(MAIN_shell) ;
     char msg[1024] , *xsv ; int xvr ;
     xsv = XServerVendor(dpy) ; xvr = XVendorRelease(dpy) ;
     if( xsv != NULL ){ sprintf(msg,"[%s v %d]",xsv,xvr); REPORT_PROGRESS(msg); }
   }
#endif

   { char *eee = getenv("DISPLAY") ;  /* Sound stuff [20 Aug 2018] */
     GLOBAL_library.local_display = (eee == NULL) || (strstr(eee,":0") != NULL ) ;
     GLOBAL_library.have_sox      = ( THD_find_executable("sox") != NULL ) ;
     GLOBAL_library.sound_player  = get_sound_player() ;
     sound_set_note_ADSR(1) ;
     sound_set_note_waveform(SOUND_WAVEFORM_TRIANGLE) ;
   }

   /* if we used xrdb to set X11 resources, re-set them back to their old
      state so that other AFNIs don't use these new settings by default   */

   if( xrdb_old != NULL && !XXX_set_default ){  /* 24 Mar 2016 */
     FILE *fp ; char *xpg ;
     xpg = malloc(strlen(xrdb_pg)+64) ;
     sprintf(xpg,"%s -override -",xrdb_pg) ;
     fp = popen( xpg , "w" ) ;
     if( fp != NULL ){
       fprintf(fp,"%s",xrdb_old) ;
       (void)pclose(fp) ;
     }
     free(xpg) ;
   }

#ifdef USE_TRACING
   if( DBG_trace == 2 ){                           /* 01 Dec 1999 */
     XSynchronize(XtDisplay(MAIN_shell),TRUE) ;
     STATUS("XSynchronize is enabled") ;
   }
#endif
   XtVaSetValues( MAIN_shell, XmNkeyboardFocusPolicy,XmEXPLICIT , NULL ) ;

   MAIN_argc = argc ; MAIN_argv = argv ;  /* what's left after XtVaAppInit */

   REPORT_PROGRESS(".") ;

   /** set default values of some environment variables [22 Jun 2004] **/

   PUTENV("AFNI_LEFT_IS_LEFT","YES") ;     /* 20 Jun 2019 */
   PUTENV("AFNI_CROSSHAIR_LINES","YES") ;
   PUTENV("AFNI_ALWAYS_LOCK","YES") ;
   PUTENV("AFNI_IMAGE_SAVESQUARE","YES") ;
   PUTENV("AFNI_DECONFLICT","OVERWRITE") ; /* 24 Sep 2007 */
   PUTENV("AFNI_X11_REDECORATE","NO") ;
   PUTENV("AFNI_RESCAN_AT_SWITCH","YES") ; /* 16 Nov 2007 */
   PUTENV("AFNI_VIDEO_DELAY","33") ;       /* 20 Aug 2009 */
   PUTENV("AFNI_GRAPH_FADE","YES") ;          /* Apr 2013 */
   PUTENV("AFNI_MPEG_DATASETS","NO") ;        /* Feb 2015 */
   PUTENV("AFNI_FLASH_VIEWSWITCH","NO") ;  /* 14 Apr 2016 */
   PUTENV("AFNI_DATASET_BROWSE","YES") ;   /* 07 Nov 2018 */
   PUTENV("AFNI_VERSION_CHECK","NO") ;     /* 04 Jan 2019 */
   PUTENV("AFNI_PBAR_FULLRANGE","YES") ;   /* 03 Jun 2014 */

   PUTENV("AFNI_COLORSCALE_DEFAULT","Reds_and_Blues_Inv") ; /* 24 May 2019 */
   PUTENV("AFNI_THRESH_TOP_EXPON"  , "5" ) ;
   PUTENV("AFNI_THRESH_INIT_EXPON" , "1" ) ;
   PUTENV("AFNI_AUTORANGE_PERC"    , "0" ) ;

   PUTENV("AFNI_FIX_SCALE_SIZE"  , "YES" ) ;  /* (from Lucca) */
   PUTENV("AFNI_OPACITY_LOCK"    , "YES" ) ;

   PUTENV("AFNI_INSTACORR_JUMP", "YES" ) ;  /* 24 Sep 2021 */

#if 0
   PUTENV("AFNI_IMAGE_LABEL_MODE","1") ;
   PUTENV("AFNI_IMAGE_LABEL_SIZE","2") ;
   PUTENV("AFNI_IMAGE_LABEL_SETBACK","01") ;
   PUTENV("AFNI_IMAGE_LABEL_COLOR","yellow") ;
#endif

   /*-- 04 Jun 1999: modify order of loading arguments and defaults --*/

   if( ! GLOBAL_argopt.skip_afnirc ){
     char *sysenv = getenv("AFNI_SYSTEM_AFNIRC") ;        /* 12 Apr 2000 */
     if( sysenv != NULL ) AFNI_process_environ(sysenv) ;  /* 12 Apr 2000 */

     AFNI_process_environ(NULL) ;                         /* 07 Jun 1999 */
   } else {
     AFNI_mark_environ_done() ;                           /* 16 Apr 2000 */
   }

   /*-- 30 Apr 2015: some messages about obsolete environment variables --*/

   if( getenv("AFNI_SLAVE_THRTIME") != NULL ){
     WARNING_message("environment variable AFNI_SLAVE_THRTIME is no longer used!") ;
     WARNING_message(" -- see AFNI_SLAVE_FUNCTIME and AFNI_SLAVE_THROLAY instead") ;
   }

   if( getenv("AFNI_SLAVE_BUCKETS_TOO") != NULL ){  /* 30 May 2015 */
     WARNING_message("environment variable AFNI_SLAVE_BUCKETS_TOO is no longer used!") ;
     WARNING_message(" -- see AFNI_SLAVE_FUNCTIME and AFNI_SLAVE_THROLAY instead") ;
   }

   /* set top exponent for threshold slider [04 Nov 2010] -- for Allison */

   { static float tval[9] = { 1.0f , 10.0f , 100.0f , 1000.0f , 10000.0f ,
                              100000.0f , 1000000.0f , 10000000.0f , 100000000.0f } ;
     ii = AFNI_numenv("AFNI_THRESH_TOP_EXPON") ;
          if( ii < 4 ) ii = 4 ;
     else if( ii > 6 ) ii = 6 ;
     THR_top_expon = ii ;
     THR_factor    = 1.0f / tval[ii] ;
     THR_top_value = tval[ii] - 1.0f ;
   }

   PBAR_FULLRANGE = AFNI_yesenv("AFNI_PBAR_FULLRANGE") ; /* 03 Jun 2014 */

   AUTORANGE_PERC = (float)AFNI_numenv("AFNI_AUTORANGE_PERC") ; /* 24 May 2019 */

   AFNI_load_defaults( MAIN_shell ) ;

   if( ! GLOBAL_argopt.skip_afnirc ){          /* this line added 14 Jul 1998 */
      char *home = getenv("HOME") ; char fname[1024] ;
      char *sysenv = getenv("AFNI_SYSTEM_AFNIRC") ;       /* 12 Apr 2000 */

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

   /*--- finally, read the command line for other options ---*/

   AFNI_parse_args( argc , argv ) ;  /* after Xt init above, only my args left */

   /*-- disable X11 and Xt error messages and crashes (we hope) --*/

   (void) XSetErrorHandler( AFNI_xerrhandler ) ;      /* 26 Jun 2003 */
   (void) XtAppSetErrorHandler(MAIN_app, AFNI_exit_handler) ;

   if( GLOBAL_argopt.xtwarns != 1 )
     (void) XtAppSetWarningHandler(MAIN_app,AFNI_handler) ;  /* turn off */

   /* FIM background threshold */

   { char *lenv = getenv("AFNI_FIM_BKTHR") ;          /* 04 Jun 1999 */
     if( lenv != NULL ){
       float bk = strtod(lenv,NULL) ;
       if( bk >= 0.0 && bk < 100.0 ) SET_FIM_bkthr(bk) ;
     }
   }

   /* locking? */

   if( AFNI_yesenv("AFNI_ALWAYS_LOCK") ){
     for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
       GLOBAL_library.controller_lock |= (1<<ii) ;
   }

   if( AFNI_yesenv("AFNI_TIME_LOCK") ){
       GLOBAL_library.time_lock = 1 ;
   }

   if( AFNI_yesenv("AFNI_ZOOM_LOCK") ){    /* 10 Dec 2019 */
       GLOBAL_library.zoompan_lock = 1 ;
   }

   /* default opacity [06 Jun 2019] */

   { int opval = (int)AFNI_numenv("AFNI_DEFAULT_OPACITY") ;
     if( opval > 0 && opval <= 9 )
       GLOBAL_library.opacity_setting = opval ;
     else
       GLOBAL_library.opacity_setting = 6 ;
   }

   /*-- now create first display context: MAIN_dc --*/

   GLOBAL_library.dc = MAIN_dc =
        MCW_new_DC( MAIN_shell , GLOBAL_argopt.ncolor ,
                    INIT_ncolovr , INIT_colovr , INIT_labovr ,
                    GLOBAL_argopt.gamma , GLOBAL_argopt.install_cmap ) ;

   memplot_to_X11_set_DC(MAIN_dc) ; /* 30 Apr 2012 */

   /* for the old PseudoColor world (does anyone live there anymore?) */

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
   PLUTO_register_workproc( MAIN_workprocess , NULL ) ; /* rest of startup */
#endif

   MCW_disable_help() ;

STATUS("start XtAppMainLoop") ;

   XtAppMainLoop(MAIN_app) ;  /* never returns */
   AFexit(0) ;                /* should never be reached */
}


#undef HUBERIZE
#ifdef HUBERIZE
#include "huber.c"  /* this is useless drivel */
#endif

/*---------------------------------------------------------------------------------
   Xt work process to do most of the initialization stuff.
   (So we can so a splash screen to pacify and amuse the user.)
-----------------------------------------------------------------------------------*/

#define REFRESH XmUpdateDisplay(MAIN_im3d->vwid->top_shell)

static RwcBoolean MAIN_workprocess( XtPointer fred )
{
   static int MAIN_calls = 0 ;  /* controls what happens */
   static int nosplash = 0 ;
   static double eltime=0.0 , max_splash=3.0 ;
   int ii ;

ENTRY("MAIN_workprocess") ;  /* 23 Jan 2001: added ENTRY/RETURN to this routine */

if(PRINT_TRACING){ char str[256]; sprintf(str,"MAIN_calls=%d",MAIN_calls); STATUS(str); }

   switch( MAIN_calls ){

      /*============================================================================
         This code is executed at the end (when MAIN_calls gets too big).
        ============================================================================*/

      default:{
STATUS("default call") ;

         RETURN(True) ;  /* I hope this keeps you happy, Ziad :-) */
      }
      break ;

      /*============================================================================
         Stuff to popup the AFNI splash screen (see afni_splash.[ch]).
        ============================================================================*/

      case 0:{
STATUS("call 0") ;

#ifdef NO_FRIVOLITIES
        nosplash = 1 ;
#else
        nosplash = AFNI_yesenv("AFNI_NOSPLASH") ;
#endif
        /* allow no_friv control   [22 Apr 2022 rickr] */
        if( GLOBAL_argopt.no_frivolities )
          nosplash = 1 ;

        if( !nosplash ){
          char *hh ;
          AFNI_splashup() ; eltime = COX_clock_time() ;
          hh = getenv("AFNI_SPLASHTIME") ;
          if( hh != NULL ){
            max_splash = strtod(hh,NULL) ;
            if( max_splash > 9.0 ) max_splash = 9.0 ;
          }
        }
      }
      break ;

      /*** For the Mac users! ***/

      case 1:
#if 0
        AFNI_speak("[[volm 0.65; inpt PHON; rate -10; pbas +5]]1AEf=nnnIY",0) ;  /* fall thru */
#endif

#if 1
        if( check_string("-com",MAIN_argc,MAIN_argv) == 0 &&
            GLOBAL_library.local_display                  &&
            GLOBAL_library.have_sox                       &&
            AFNI_yesenv("AFNI_STARTUP_SOUND")               ) AFNI_startup_sound(1) ;
#endif

      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
      case 10:
STATUS("sleep call") ;
        if( !nosplash) AFNI_sleep(1) ; /* burn some time to let splash windo popup */
      break ;

      /*============================================================================
         Next, create the first AFNI controller window.
        ============================================================================*/

      case 11:{

        int do_images ;                           /* 19 Oct 1999 */

STATUS("call 11") ;

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
         Next, read the input files -- may take a while --
         which is one reason why we need a splash screen (the other is it's fun)
        ============================================================================*/

      case 12:{

STATUS("call 12") ;

        REPORT_PROGRESS(". Input files:") ;

        AFNI_read_inputs( MAIN_argc , MAIN_argv ) ;

        if( GLOBAL_library.have_dummy_dataset   &&
            MAIN_im3d->type == AFNI_3DDATA_VIEW    ){

          XtSetSensitive( MAIN_im3d->vwid->prog->clone_pb , False ) ;
          MAIN_im3d->dummied = 1 ;  /* 27 Jan 2004 */
          MCW_set_widget_bg( MAIN_im3d->vwid->view->sess_lab ,
                             MCW_hotcolor(MAIN_im3d->vwid->view->sess_lab) , 0 ) ;
        }
      }
      break ;

      /*============================================================================
         Next, setup the plugins, and things like that ...
        ============================================================================*/

      case 13:{

STATUS("call 13") ;

        /* registered transformation functions, etc. */

        GLOBAL_library.registered_0D.num = 0 ;               /* initialize registry */
        GLOBAL_library.registered_1D.num = 0 ;               /* initialize registry */
        GLOBAL_library.registered_2D.num = 0 ;               /* initialize registry */

        GLOBAL_library.registered_fim.num = 0 ;              /* 30 Jan 2000 */

        GLOBAL_library.registered_slice_proj.num = 0 ;       /* 31 Jan 2002 */

        /* these functions are now in afni_transforms.c [01 Feb 2002] */

        AFNI_register_0D_function( "Log10" , log10_func ) ;
        AFNI_register_0D_function( "SSqrt" , ssqrt_func ) ;
        AFNI_register_0D_function( "Abs"   , absval_func ) ; /* 20 Oct 2020 */

        AFNI_register_1D_function( "Median3"   , median3_func) ;
#if 0
        AFNI_register_1D_function( "OSfilt3"   , osfilt3_func) ;
#endif
        AFNI_register_1D_function( "AdptMean9" , adpt_wt_mn9 ) ;       /* 04 Sep 2009 */
        AFNI_register_1D_function( "AdptMean19", adpt_wt_mn19 );       /* 29 Sep 2016 */

        { int nad = (int)AFNI_numenv("AFNI_AdptMeanWidth1D") ;         /* 30 Sep 2016 */
          char lab[32] ;                                      /* user specified width */
          if( nad > 3 && nad != 9 && nad != 19 && nad < 100 ){
            if( nad%2 == 0 ){ nad++; INFO_message("increased AFNI_AdptMeanWidth1D to %d",nad); }
            sprintf(lab,"AdptMean%d",nad) ;
            AFNI_register_1D_function( lab , adpt_wt_mnXX ) ;
            adpt_wt_mnXX(nad,0.0,0.0,NULL) ;
          }
        }

        AFNI_register_1D_function( "Despike9"  , despike9_func);       /* 08 Oct 2010 */
        AFNI_register_1D_function( "Despike25" , DES_despike25);       /* 18 May 2018 */
        AFNI_register_1D_function( "HRF decon" , hrfdecon_func);       /* 29 Oct 2010 */
        AFNI_register_1D_function( "|FFT()|"   , absfft_func ) ;
#if 0
        AFNI_register_1D_function( "ZeroToOne" , ztone_func  ) ;       /* 02 Sep 2009 */
        AFNI_register_1D_function( "Normlz_L1" , L1normalize_func  ) ; /* 03 Sep 2009 */
        AFNI_register_1D_function( "Normlz_L2" , L2normalize_func  ) ; /* 03 Sep 2009 */
#endif

        F1D_null() ; /* should be last 1D func - 20 Oct 2020 */

        AFNI_register_2D_function( "Median9" , median9_box_func ) ;
        AFNI_register_2D_function( "Winsor9" , winsor9_box_func ) ;
        AFNI_register_2D_function( "OSfilt9" , osfilt9_box_func ) ;

        AFNI_register_2D_function( "Median21"  , median21_box_func );
        AFNI_register_2D_function( "Winsor21"  , winsor21_box_func );
        AFNI_register_2D_function( "AdptMean21", adapt_mean_21_box_func ); /* 04 Sep 2009 */

        AFNI_register_2D_function( "abs[FFT2D]" , fft2D_absfunc   );
        AFNI_register_2D_function( "arg[FFT2D]" , fft2D_phasefunc );
        AFNI_register_2D_function( "Sharpness"  , sharpness2D_func);   /* 28 Oct 2014 */

        F2D_null() ; /* should be last 2D func - 20 Oct 2020 */

        /* 01 Feb 2000: see afni_fimfunc.c */

        AFNI_register_fimfunc("Spearman CC",1,(generic_func *)spearman_fimfunc,NULL);
        AFNI_register_fimfunc("Quadrant CC",1,(generic_func *)quadrant_fimfunc,NULL);

        /* 31 Jan 2002 */

        AFNI_register_slice_proj( "Minimum" , min_proj   ) ;
        AFNI_register_slice_proj( "Maximum" , max_proj   ) ;
        AFNI_register_slice_proj( "Mean"    , mean_proj  ) ;

        AFNI_register_slice_proj( "Median"  , qmed_float   ) ; /* cs_qmed.c */
        AFNI_register_slice_proj( "OSfilt"  , osfilt_proj  ) ; /* 07 Dec 2007 */
        AFNI_register_slice_proj( "Extreme" , extreme_proj ) ; /* 02 Feb 2002 */
        AFNI_register_slice_proj( "MAD"     , mad_proj     ) ; /* 07 Dec 2007 */

        AFNI_register_slice_proj( "AdptMean", adaptive_weighted_mean ) ; /* 04 Sep 2009 */

#ifdef HUBERIZE
        AFNI_register_1D_funcstr( "Huber Fit" , huber_func ) ;
#endif

        /** find and load the plugins at last! **/

#ifdef ALLOW_PLUGINS
        if( MAIN_im3d->type == AFNI_3DDATA_VIEW ){
          int nplug = 0 ;
          char str[1024] ;

          if( ! GLOBAL_argopt.noplugins ){
STATUS("initialize plugins") ;
            GLOBAL_library.plugins = PLUG_get_many_plugins( MAIN_argv[0] ) ;
            AFNI_plugin_button( MAIN_im3d ) ;
          }

          if( GLOBAL_library.plugins != NULL ) nplug = GLOBAL_library.plugins->num ;
          sprintf(str,"\n Plugins       = %d libraries read",nplug) ;
          REPORT_PROGRESS(str) ;
          if( nplug == 0 && ! GLOBAL_argopt.noplugins )  /* 18 May 2005 */
            REPORT_PROGRESS(
                      "\n ** Your Unix path must include the AFNI binary directory"
                      "\n ** OR you must setenv AFNI_PLUGINPATH to that directory!");

          /** and plugouts! **/

          if( !GLOBAL_argopt.noplugouts ){  /* June 1997 */
            AFNI_init_plugouts() ;
            if( MAIN_im3d->vwid->dmode->misc_plugout_pb != NULL ) /* 07 Nov 2001 */
              XtSetSensitive(MAIN_im3d->vwid->dmode->misc_plugout_pb,False) ;
            REPORT_PROGRESS("\n Plugouts      = listening for connections") ;
          }
        }
#endif

        TT_setup_popup_func( AFNI_popup_message ) ;  /* 26 May 2006 */

        /* 04/06/2020 discoraj */
        if( GLOBAL_argopt.all_dsets_startup ){
            ii = AFNI_find_session( "All_Datasets") ;
            if( ii >= 0 ) MAIN_im3d->vinfo->sess_num = ii ;
        }
      }
      break ;

      /*============================================================================
         Next, do the initial setup on entering the initial view.
        ============================================================================*/

      case 14:{

STATUS("call 14") ;

        OPEN_CONTROLLER( MAIN_im3d ) ;

        if( GLOBAL_argopt.only_images ){   /* 24 Feb 2017 */
          AV_assign_ival( MAIN_im3d->vwid->imag->crosshair_av,0) ;
          MAIN_im3d->vinfo->crosshair_visible = False ;
#if 0
          GLOBAL_argopt.left_is_left = 0 ;
          putenv("AFNI_LEFT_IS_LEFT=NO" ) ;
#endif
        }
        if( AFNI_yesenv("AFNI_CROSSHAIRS_OFF") )
          MAIN_im3d->vinfo->crosshair_visible = False ;

        AFNI_initialize_controller( MAIN_im3d ) ;  /* decide what to see */
        AFNI_initialize_view( NULL, MAIN_im3d ) ;  /* set up to see it */

        /*--- Other small and quick startup stuff before AFNI can go ---*/

STATUS("initialize help") ;

        MCW_help_CB( MAIN_im3d->vwid->top_shell,NULL,NULL ); /* initialize help */

#if 0
        { char str[64] ;
          sprintf(str,"\n -orient       = %s", GLOBAL_library.cord.orcode ) ;
          REPORT_PROGRESS(str) ;
        }
#endif

        /* initialize hints */

STATUS("initialize hints") ;

        GLOBAL_library.hints_on = !AFNI_noenv("AFNI_HINTS") ;
        if( !GLOBAL_library.hints_on ) MCW_hint_toggle() ;

        if( MAIN_im3d->vwid->dmode->misc_hints_pb != NULL )
          MCW_set_bbox( MAIN_im3d->vwid->dmode->misc_hints_bbox ,
                        GLOBAL_library.hints_on ) ;

        /* Feb 1998: setup write compression from environment */
        /*           (read de-compression always works)       */

STATUS("initialize compression mode (if any)") ;

        ii = THD_enviro_write_compression() ;
        if( ii >= 0 && ii <= COMPRESS_LASTCODE ){
          char str[256] ;
          sprintf(str,"\n write compress= %s", COMPRESS_enviro[ii]) ;
          REPORT_PROGRESS(str) ;
        }

        if( ALLOW_realtime > 0 )
          REPORT_PROGRESS("\nRT: AFNI realtime plugin is active; cf. README.realtime document") ;

        /* 23 Sep 2000: this function will be called 0.123 seconds
                        from now to initialize the window layouts, if any  */

        if( GLOBAL_argopt.layout_fname != NULL &&
            MAIN_im3d->type == AFNI_3DDATA_VIEW   ){

STATUS("start user's layout timer") ;

          (void) XtAppAddTimeOut( MAIN_app , 123 ,
                                  AFNI_startup_layout_CB , GLOBAL_argopt.layout_fname ) ;

        } else if (MAIN_im3d->type == AFNI_3DDATA_VIEW){ /* ZSS Dec 02 2010. */

STATUS("start default layout timer") ;

          (void) XtAppAddTimeOut( MAIN_app , 123 ,
                                  AFNI_startup_layout_CB ,
                                  "GIMME_SOMETHING" ) ;
        }

        /* 21 Jan 2003: this function will be called 0.246 seconds
                        from now to run the startup script commands, if any */

        if( GLOBAL_argopt.script_fname != NULL &&
            MAIN_im3d->type == AFNI_3DDATA_VIEW   ){

STATUS("start script timeout") ;

          (void) XtAppAddTimeOut( MAIN_app , 246 ,
                                  AFNI_startup_script_CB , GLOBAL_argopt.script_fname ) ;
        }

        if( MAIN_im3d->vwid->func->do_setup ){
          int ii = (int)AFNI_numenv("AFNI_THRESH_INIT_EXPON") ;
          if( ii > 0 && ii < THR_top_expon ) AFNI_set_thresh_itop(MAIN_im3d,ii) ;
          MAIN_im3d->vwid->func->do_setup = 0 ;
        }

        /* this function will be called 1.666 seconds from now to finalize
           anything else that needs fixing up once AFNI is fully started   */

STATUS("start startup timeout") ;

        PICTURE_ON(MAIN_im3d) ;
        (void) XtAppAddTimeOut( MAIN_app, 1666, AFNI_startup_timeout_CB, MAIN_im3d ) ;

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

        /* 05 Mar 2007: auto-threshold? */

        if( AFNI_yesenv("AFNI_THRESH_AUTO") ){
          float new_thresh = AFNI_get_autothresh(MAIN_im3d) ;
          if( new_thresh > 0.0f ) AFNI_set_threshold(MAIN_im3d,new_thresh) ;
        }

        PUTENV("AFNI_DECONFLICT","OVERWRITE") ; /* 24 Sep 2007 */
        putenv("AFNI_IS_RUNNING=YES") ;         /* 08 Jun 2007 */

        memplot_topshell_setsaver( ".jpg" , memplot_to_jpg ) ; /* 05 Dec 2007 */
        memplot_topshell_setsaver( ".png" , memplot_to_png ) ;

        REPORT_PROGRESS("\n") ;

        { long long lfs = AFNI_logfilesize(); /* 17 Oct 2007 */
          if( lfs > 10000000 ){
            char msg[1024] ;
            sprintf(msg,"\n++ WARNING: ~/.afni.log is now %s (%s) bytes long!"
                        "\n +          (Is that you, Kevin?)\n" ,
                    commaized_integer_string(lfs) ,
                    approximate_number_string((double)lfs) ) ;
            REPORT_PROGRESS(msg) ;
          }
        }

        if( AFNI_yesenv("AFNI_ENFORCE_ASPECT") ){ /* 10 May 2018 */
          REPORT_PROGRESS(
            "\n++ NOTICE: AFNI_ENFORCE_ASPECT no longer has any effect!\n") ;
        }

        /* X11_SET_NEW_PLOT ; */

STATUS("exit call 14") ;

      }
      break ;  /* end of 14th entry case */

      /*============================================================================*/
#if 0
      case 15:{  /* not used at present, but ready to be added when needed */
STATUS("call 15") ;
      }
      break ;
#endif
   }

   MAIN_calls++ ; RETURN(False) ;
}

/*-------------------------------------------------------------------------*/

static char * random_goodbye(void)
{
   static char *bye[] = { "[[volm 0.64]] Farewell",
                          "[[volm 0.64]] Goodbye" ,
                          "[[volm 0.64]] Until next time" } ;
   int nn = sizeof(bye)/sizeof(char *) ;
   return bye[ lrand48() % nn ] ;
}

/*-------------------------------------------------------------------------
   Callback for the quit button.  If called with the widget == NULL,
   resets the button to the lowercase state.
---------------------------------------------------------------------------*/

void AFNI_quit_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   XmPushButtonCallbackStruct *pbcbs = (XmPushButtonCallbackStruct *)cbs ;

ENTRY("AFNI_quit_CB") ;

   if( cd == NULL ) AFexit(0) ;  /* 27 Jul 2015 */

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
#if 0
      AFNI_speak(random_goodbye(),0) ;
#endif
      AFexit(0) ;
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
      AFNI_speak(random_goodbye(),0) ;
      STATUS("calling exit(0) -- farewell cruel world!") ;
      AFexit(0) ;

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

void AFNI_quit_timeout_CB( XtPointer client_data , XtIntervalId *id )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
ENTRY("AFNI_quit_timeout_CB") ;
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void AFNI_vcheck_flasher( Three_D_View *im3d )
{
   int pp ;
   if( im3d == NULL || vers_pixmap == XmUNSPECIFIED_PIXMAP ) return ;

   for( pp=0 ; pp < 19 ; pp++ ){
     PICTURE_SET(im3d,vers_pixmap) ;
       XmUpdateDisplay(im3d->vwid->top_shell); AFNI_sleep(166);
     PICTURE_OFF(im3d) ;
       XmUpdateDisplay(im3d->vwid->top_shell); AFNI_sleep(166);
   }
   logo_pixmap = vers_pixmap ;     /* replace logo with version warning */
   return ;
}

/*----------------------------------------------------------------------*/
/* the tips are stored in afni_startup_tips.h */

void AFNI_print_startup_tip(int qq) /* 03 Jan 2018 */
{
   int nn = (lrand48()>>3) % NTIP ;

   if( qq >= 0 && qq < NTIP ) nn = qq ;

   if( tip[nn] != NULL )
     fprintf( stdout , "\n\n"
              "------------------------- AFNI Startup Tip (%d/%d)----------------------------\n"
              "%s\n"
              "-------------------------------------------------------------------------------\n" ,
              nn+1 , NTIP , tip[nn] ) ;
   return ;
}

/*-------------------------------------------------------------------------
  This function is called periodically to do unimportant stuff [Oct 2019]
---------------------------------------------------------------------------*/

void AFNI_periodic_timeout_CB( XtPointer client_data , XtIntervalId *id )
{
   int qq ; Three_D_View *qq3d ;

ENTRY("AFNI_periodic_timeout_CB") ;

   /* turn off the 'working' pictures and reset to normal cursor */

   SHOW_AFNI_READY ;

   /* restart timer */

   (void) XtAppAddTimeOut( MAIN_app, 9999, AFNI_periodic_timeout_CB, MAIN_im3d ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
  This function is called about 1 s after AFNI startup is completed.
  It's original purpose was to make sure that the help window was
  popped down - the help initializing routine does this, too, but
  it didn't work properly on the old Tektronix X-terminal at MCW.
  Thus, the timeout - waiting a little made things work OK.
------------------------------------------------------------------------*/

void AFNI_startup_timeout_CB( XtPointer client_data , XtIntervalId *id )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   int vv ;

ENTRY("AFNI_startup_timeout_CB") ;

   /*--- make sure help window is popped down ---*/

#if 0
   MCW_help_CB( MAIN_im3d->vwid->top_shell,NULL,NULL ); /* initialize help */
#endif

   MCW_help_CB(NULL,NULL,NULL) ;

   /*--- test geometry of main window [08 Aug 2016]
         if it is negative territory, move it back to positive land;
         this is to (hopefully) fix a peculiarity in XQuartz on El Capitan ---*/

   { Position xroot,yroot ;
     WAIT_for_window( MAIN_im3d->vwid->top_shell ) ;
     XtTranslateCoords( MAIN_im3d->vwid->top_shell , 0,0, &xroot , &yroot ) ;
#if 0
INFO_message("AFNI controller xroot=%d yroot=%d",(int)xroot,(int)yroot) ;
#endif
     if( xroot < 0 || yroot < 0 ){
       INFO_message("AFNI was off screen at x=%d y=%d -- repositioning" ,
                   (int)xroot , (int)yroot ) ;
       XtVaSetValues( MAIN_im3d->vwid->top_shell, XmNx,20, XmNy,20, NULL ) ;
       REFRESH ;
     }
   }

   /*--- tell user if any mixed-type datasets transpired [06 Sep 2006] ---*/

   AFNI_inconstancy_check( im3d , NULL ) ;

   /*--- NIML listening on [moved here 17 Mar 2002] ---*/

   if( MAIN_im3d->type == AFNI_3DDATA_VIEW && GLOBAL_argopt.yes_niml ){
     AFNI_init_niml() ;
     if( MAIN_im3d->vwid->dmode->misc_niml_pb != NULL )
       XtSetSensitive(MAIN_im3d->vwid->dmode->misc_niml_pb,False) ;
   } else if( 0 && get_user_np() > 0 ){  /* 10 Dec 2002 -- ZSS June 2011 */
      /* No need to warn anymore, -np can be set by environment
        variables too. */
      fprintf(stderr,
         "** WARNING: -np was given, but NIML is turned off.\n") ;
   }

   if( AFNI_have_niml() && AFNI_have_plugouts() )  /* 02 Feb 2007 */
     XtSetSensitive(MAIN_im3d->vwid->view->nimlpo_pb,False) ;

   if( !AFNI_noenv("AFNI_STARTUP_WARNINGS") ){  /* 22 Jul 2003 */

#ifdef LESSTIF_VERSION /* 13 Jan 2003: If LessTif was used for this AFNI */
    #ifndef USING_LESSTIF   /* Lesstif is good for you!
                               Keep the warning if the proper
                               -DUSING_LESSTIF was not used
                               when compiling  */
    (void) MCW_popup_message( MAIN_im3d->vwid->picture ,
                                 " \n"
                                 "*** WARNING:                ***\n"
                                 "*** This  copy of AFNI  was ***\n"
                                 "*** built using the LessTif ***\n"
                                 "*** library, but without the***\n"
                                 "*** -DUSING_LESSTIF flag at ***\n"
                                 "*** compile time.           ***\n"
                                 "*** You will find problems; ***\n"
                                 "*** AFNI works with either  ***\n"
                                 "*** Motif or OpenMotif, or  ***\n"
                                 "*** Lesstif with the proper ***\n"
                                 "*** compiler flag.          ***\n" ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
    #endif
#endif

#ifdef BAD_BUTTON3_POPUPS /* 21 Jul 2003: If this is a stupid system */

    (void) MCW_popup_message( MAIN_im3d->vwid->imag->topper,
                              " \n"
                              "*** WARNING:                  ***\n"
                              "*** This computer system has  ***\n"
                              "*** a bug with Button-3 popup ***\n"
                              "*** menus -- to use a button  ***\n"
                              "*** on such a menu, you have  ***\n"
                              "*** to keep Button-3 pressed  ***\n"
                              "*** down and then click the   ***\n"
                              "*** menu button with Button-1 ***\n"
                              "*** at the same time.         ***\n" ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
#endif
   }

   if( recursed_ondot ) /* 18 Feb 2007 */
    (void) MCW_popup_message( MAIN_im3d->vwid->picture ,
                              " \n"
                              "++ NOTICE:                              ++\n"
                              "++ No data was found in './' directory, ++\n"
                              "++ so its subdirectories were searched  ++\n"
                              "++ for dataset files.                   ++\n " ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
   else if( !ALLOW_realtime                    &&
            GLOBAL_library.have_dummy_dataset  &&
            MAIN_im3d->type == AFNI_3DDATA_VIEW   ){
    int horz = MAIN_im3d->vwid->view->session_horz ; /* 29 Apr 2010 */
    char hstr[1024] ;
    sprintf( hstr ,
             "***** NOTICE *** UWAGA *** AVVISO *** WARNUNG *** RABHADH *****\n"
             "                                                               \n"
             "++ No valid datasets were found.  A dummy dataset has been   ++\n"
             "++ created for your viewing pleasure :-)  To read in a real  ++\n"
             "%s"
             "\n"
             "** Or, quit AFNI and restart it with the name of a dataset   **\n"
             "** directory on the command line, as in                      **\n"
             "\n"
             "     afni name_of_data_directory                               \n"
             "                                                               \n"
             "++ For general AFNI program help, see the Web page           ++\n"
             "\n"
             "   https://afni.nimh.nih.gov/pub/dist/doc/program_help/index.html   \n"
             "%s"
             "\n"
             "++ [To close this message window, left-click inside of it.]  ++\n"
            ,
      (horz)
           ? "++ dataset directory, use the 'Read' button near 'DataDir'.  ++\n"
           : "++ dataset directory, use the 'Read New Directory' button,   ++\n"
             "++ located below the 'Data Directory' label.                 ++\n" ,
      (GLOBAL_browser == NULL)
           ? " "
           : "\n"
             "++ which you can open by right-clicking on the logo space to ++\n"
             "++ the right of the 'done' button, and from the resulting    ++\n"
             "++ popup menu, choose the 'Web Browser: Help' item.          ++\n"
           ) ;
     (void)MCW_popup_message( MAIN_im3d->vwid->prog->quit_pb,hstr,MCW_USER_KILL ) ;
     MCW_flash_widget_list( 9 , MAIN_im3d->vwid->view->sess_lab ,
                                MAIN_im3d->vwid->view->choose_sess_pb ,
                                MAIN_im3d->vwid->view->read_sess_pb ,
                            NULL ) ;                           /* 12 Feb 2010 */
   }

   /*--- 05 May 2009: make sure the Cluster widgets show up properly ---*/

   AFNI_vedit_CB( im3d->vwid->func->options_vedit_av , im3d ) ;

   /* 21 Nov 2002: check the AFNI version */

/* changed if 0 to if 1 to revert to version_check()   30 Dec 2015 */
#if 1
   /* do nothing if AFNI_start_version_check() says inactive */
   vv = AFNI_version_check() ;

   if( vv && vers_pixmap != XmUNSPECIFIED_PIXMAP )     /* 08 Aug 2005 */
     AFNI_vcheck_flasher(im3d) ;

#else

   vv = AFNI_compile_date_check() ;  /* 17 Jun 2014 */
   if( vv >= 93 ){
     /* fixed %31 to /31   28 Dec 2015 [rickr] */
     WARNING_message(
       "Your copy of AFNI is over %d months old -- please update it (if practicable)." ,
       vv / 31 ) ;
     if( im3d->vwid->tips_pb != NULL ){
       char msg[1024] ;
       sprintf( msg, " \n"
                     " Your copy of AFNI is over %d months old.\n"
                     "   Please update it (if practicable).\n "  , vv / 31 ) ;
       (void) MCW_popup_message( im3d->vwid->tips_pb , msg ,
                                 MCW_USER_KILL | MCW_TIMER_KILL ) ;
     }
   } else if( vv < 0 ){
     INFO_message("You are %d days AHEAD of the official AFNI compile date -- impressive!",-vv) ;
   }
#endif

   /*--- finish up getting AFNI ready to be presented to the world ---*/

   if( im3d->vwid->tips_pb != NULL && lrand48()%2==0 )
     MCW_flash_widget(1,im3d->vwid->tips_pb) ;

   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;
   PICTURE_OFF(im3d) ;

   if( im3d->vwid->tips_pb != NULL && lrand48()%2==0 )
     MCW_flash_widget(1,im3d->vwid->tips_pb) ;

   /*--- 29 Jul 2005: run any driver commands from the command line ---*/

   for( vv=0 ; vv < COM_num ; vv++ ){
     AFNI_driver(COM_com[vv]) ; free(COM_com[vv]) ;
   }

   /*--- 29 Nov 2005: Message Of The Day -- did it change? ---*/

   if( GLOBAL_motd != NULL && !AFNI_noenv("AFNI_MOTD_CHECK") )
     AFNI_display_motd( im3d->vwid->imag->topper ) ;

   /*--- 09 Nov 2005: start checking periodically for updated datasets ---*/

   if( AFNI_yesenv("AFNI_AUTO_RESCAN") )
    (void) XtAppAddTimeOut( MAIN_app,29999, AFNI_rescan_timeout_CB,&MAIN_app );

   AFNI_coord_filer_setup(im3d) ; /* 07 May 2010 */

   /*--- 12 May 2010: As the last printout, say when this version was created ---*/

   fprintf(stderr,"\n++ NOTE: This version of AFNI was built " __DATE__ " ++\n" ) ;

   if( MAIN_im3d->type == AFNI_3DDATA_VIEW &&
      !AFNI_yesenv("AFNI_ENABLE_MARKERS")    )
     fprintf(stderr,"++ NOTE: 'Define Markers' is hidden: right-click 'DataDir' to see it\n") ;

   if( MAIN_im3d->type == AFNI_3DDATA_VIEW && first_plugin_check < 0 && !GLOBAL_argopt.noplugins )
     fprintf(stderr,"++ NOTE: Use '-seehidden' option to see which plugins are hidden\n") ;

#if 0
   /**--- Apr 2013: delete this message in a few months ---**/

   if( __DATE__[10] == '3' && (__DATE__[0] == 'A' || __DATE__[0] == 'M') )
     fprintf(stderr,
       "++ NOTE: Graph viewer threshold 'fading' is now on by default.  To turn it off,\n"
       "         press the 'F' key in a graph viewer window, or set environment variable\n"
       "         AFNI_GRAPH_FADE to NO.  -- [Apr 2013]\n"
       "++ NOTE: The AFNI GUI now detaches itself from the terminal at startup, so that\n"
       "         it is no longer necessary or useful to start the program with the '&'\n"
       "         key at the end of the command line.  To disable this feature, set\n"
       "         environment variable AFNI_DETACH to NO.  To kill all running copies\n"
       "         of AFNI, you could use the Unix command 'killall afni'. -- [Apr 2013]\n"
     ) ;
#endif

   if( AFNI_check_environ_done() == 0 )
     fprintf(stderr,
       "++ NOTE: you may want to consider creating a '.afnirc' file in your home\n"
       "         directory, to control AFNI's setup.  For more details, see\n"
       "   https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html\n") ;


   /*--- splash window down -- moved here 29 May 2013 ---*/

#ifndef NO_FRIVOLITIES
   if( lrand48()%17 == 3 )
     INFO_message("Want your picture in the AFNI splash screen? Email us a square JPEG!") ;
#endif

   AFNI_splashdown(); STATUS("splashed down");

   /* Startup tip message [03 Jan 2018] */

   if( !ALLOW_realtime                        &&
       !AFNI_yesenv("AFNI_NEVER_SAY_GOODBYE") &&
        MAIN_im3d->type == AFNI_3DDATA_VIEW     ) AFNI_print_startup_tip(-1) ;

   /* this is for me, myself, and I only! */

   { char *eee = getenv("USER") ;
     set_program_name("afni") ;
#ifdef USE_TRACING
     if( eee != NULL && strcmp(eee,"rwcox") == 0 ) atexit(clock_time_atexit) ;
#endif
   }

   /* Start timer to repeatedly fix threshold scale sizes [05 Jun 2019] */

   if( ! AFNI_yesenv("AFNI_DONT_FIX_SCALE_SIZE") )
     AFNI_fix_scale_size_timer_CB(NULL,NULL) ;

   /*--- and AWAY WE GO (how sweet it is!) ---*/

   MPROBE ;                       /* check mcw_malloc() for integrity */

   /*--- Start period timeout [17 Oct 2019] ---*/

   (void) XtAppAddTimeOut( MAIN_app, 9999, AFNI_periodic_timeout_CB, MAIN_im3d ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
   Return the underlay or overlay brick and the sub-brick index that
   the user had selected in the 'Underlay' and 'Overlay' menus.

   So whether or not the user has bkgd:ULay or bkgd:OLay is irrelevant
   to what gets returned.

   The returned structure is brr
   brr->dset will contain the dset selected as underlay if
      type  ==  isqCR_getulayim
              and         the dset selected as overlay if
      type  ==  isqCR_getolayim

   brr = br if type is set to anything other than isqCR_get[uo]ayim

   See also Get_UO_Dset
----------------------------------------------------------------------*/

FD_brick *Get_FD_Brick_As_Selected(FD_brick *br, int type, int *rival)
{
   Three_D_View *im3d = (Three_D_View *)br->parent ;
   FD_brick *brr=NULL ;
   int ival , banat , uu=im3d->vinfo->underlay_type ;

   banat = EQUIV_DSETS(br->dset,im3d->anat_now) ;
   switch( type ){
     case isqCR_getulayim: brr = (banat) ? br : br->brother ; break ;
     case isqCR_getolayim: brr = (banat) ? br->brother : br ; break ;
   }
   if( brr == NULL ) brr = br ; /* I wish it was 'brr' right now -- July 2014 */

   /*** decide which 3D brick to extract data from (ival) ***/

   if( EQUIV_DSETS(im3d->anat_now,im3d->fim_now) ){ /* RWCox [03 Jul 2014] */
     if( type == isqCR_getolayim )                /* datasets are the same */
       ival = im3d->vinfo->fim_index ;  /* so need to be careful with ival */
     else if( type == isqCR_getulayim || uu == UNDERLAY_ANAT )
       ival = im3d->vinfo->anat_index ;
     else
       ival = im3d->vinfo->fim_index ;
    } else {                                      /* the old (Ziad's) way */
     if( EQUIV_DSETS(brr->dset,im3d->anat_now) )      /* underlay dataset */
       ival = im3d->vinfo->anat_index ;
     else if( EQUIV_DSETS(brr->dset,im3d->fim_now) )   /* overlay dataset */
       ival = im3d->vinfo->fim_index ;
     else
       ival = 0 ;                                     /* shouldn't happen */
   }

   if( br->deltival != 0 && DSET_NVALS(brr->dset) > 1 ){  /* 23 Feb 2011 */
            /*    This is for allowing montage to cycle through sub-bricks */
      ival += br->deltival ;
      ININFO_message("afni: deltival changes ival to %d",ival) ;
      if( ival < 0 || ival >= DSET_NVALS(brr->dset) ) RETURN( NULL ) ;
   }

        if( type == isqCR_getqimage       ) ival = -1; /* get empty image */
   else if( ival >= DSET_NVALS(brr->dset) ) ival = brr->dset->dblk->nvals-1;

   *rival = ival;
   /*
   fprintf(stderr,"Get_FD_Brick_As_Selected:\n"
                  "     br->dset %s, brr->dset %s\n"
                  "     anat_now %s, fim_now   %s\n"
                  "     banat = %d, ival = %d\n"
                  "     type %d (%s)\n",
                  DSET_PREFIX(br->dset), DSET_PREFIX(brr->dset),
                  DSET_PREFIX(im3d->anat_now), DSET_PREFIX(im3d->fim_now),
                  banat, ival,
                  type,
                  type == isqCR_getulayim ? "getULAY" :
                           type == isqCR_getolayim ? "getOLAY" : "Other");
   */
   return(brr);
}

/*
   Return the overlay or underaly dset currently selected and set
   the sub-brick number.

   Peculiar input:
   UOlay:  'O' --> Return the overlay
           'U'     Return the underlay
   AsSet: 1 --> Return the dataset that is set by the 'Underlay'
                      or 'Overlay' selectors.
                0 --> Return the 'displayed' overlay or underlay dset.
                      This means, take into account the bkgd:ULay or bkgd:OLay
                      setting.
*/
THD_3dim_dataset *Get_UO_Dset(FD_brick *br, char UOlay,
                              byte AsSet, int *rival)
{
   Three_D_View *im3d = (Three_D_View *)br->parent ;
   FD_brick *brt=NULL;
   THD_3dim_dataset *dset=NULL;

   /* set up with basics */
   if (UOlay=='O') {
      dset = im3d->fim_now;
      *rival = im3d->vinfo->fim_index;
   } else {
      dset = im3d->anat_now;
      *rival = im3d->vinfo->anat_index;
   }

   /* now allow for things like deltival, and other stuff */
   if (AsSet) { /* as selected in 'Underlay' and 'Overlay' */
      if (!(brt = Get_FD_Brick_As_Selected(br,
                  UOlay=='O' ? isqCR_getolayim : isqCR_getulayim, rival))) {
         ERROR_message("Case 1: Failed to select brick, returning defaults");
         return(dset);
      }
   } else { /* as displayed, works OK, not well tested for rival below */
      if (EQUIV_DSETS(br->dset,im3d->anat_now)) {
         /* vanilla */
         if (!(brt = Get_FD_Brick_As_Selected(br,
                  UOlay=='O' ? isqCR_getolayim : isqCR_getulayim, rival))) {
            ERROR_message("Case 2: Failed to select brick, returning defaults");
            return(dset);
         }
      } else {
         /* Olay as Ulay: always return the overlay */
         if (!(brt = Get_FD_Brick_As_Selected(br, isqCR_getolayim, rival))) {
            ERROR_message("Case 3: Failed to select brick, returning defaults");
            return(dset);
         }
      }
   }
   if (brt) dset = brt->dset;
   return(dset);
}

/*!
   Return 0 if FD_brick is not in montage mode.
          1 if Axial montage
          2 if Sagittal montage
          4 if Coronal montage
*/
int FD_brick_montized(FD_brick *br )
{
   Three_D_View *im3d = (Three_D_View *)br->parent ;

   if ( (br == im3d->b123_anat || br == im3d->b123_fim) &&
        (im3d->s123->mont_nx > 1|| im3d->s123->mont_ny > 1) ) {
      return(1); /* Axial brick and in montage */
   }
   if ( (br == im3d->b231_anat || br == im3d->b231_fim) &&
        (im3d->s231->mont_nx > 1|| im3d->s231->mont_ny > 1) ) {
      return(2);  /* Sagittal brick and in montage */
   }
   if ( (br == im3d->b312_anat || br == im3d->b312_fim) &&
        (im3d->s312->mont_nx > 1|| im3d->s312->mont_ny > 1) ) {
      return(4);  /* Coronal brick and in montage */
   }
   return(0);
}

/*----------------------------------------------------------------------
   Fill the various possible string values used in an imseq getlabel.
   Return value is 0 if things are good, 1 if things are bad.
------------------------------------------------------------------------*/

#define LPART_SLICE_INDEX 0  /* %i% */
#define LPART_SLICE_COORD 1  /* %s% */
#define LPART_TIME_INDEX  2  /* %l% */
#define LPART_TIME_COORD  3  /* %t% */
#define NUM_LPART         4

int AFNI_brick_to_label_parts( int n , FD_brick *br , char **lpart )
{
   Three_D_View *im3d = (Three_D_View *)br->parent ;
   char *lab , *dd , *eee=NULL , str[256] ;
   THD_ivec3 iv,ivp,ivm ;
   THD_fvec3 fv,fvp,fvm ;
   float dxyz , cc ;
   int ii, ival;
   double dval;
   THD_3dim_dataset *dset=br->dset ;
   int nnn ; char *fmt ;

   if( lpart == NULL || im3d->type != AFNI_3DDATA_VIEW ) return 1 ;

   /* format slice index */

   LOAD_IVEC3(iv,0,0,n) ;
   ivp = THD_fdind_to_3dind( br , iv ) ;

   if( n == 0 ) LOAD_IVEC3(iv,0,0,1) ;
   else         LOAD_IVEC3(iv,0,0,n-1) ;
   ivm = THD_fdind_to_3dind( br , iv ) ;

   nnn = MAX(br->n1,br->n2) ; nnn = MAX(nnn,br->n3) ;
   fmt =  (nnn < 10)    ? "#%1d"
        : (nnn < 100)   ? "#%02d"
        : (nnn < 1000)  ? "#%03d"
        : (nnn < 10000) ? "#%04d"
        : (nnn < 100000)? "#%05d"
        :                 "#%06d" ;
   if( ivm.ijk[0] != ivp.ijk[0] ){
     sprintf(str,fmt,ivp.ijk[0]) ;
   } else if( ivm.ijk[1] != ivp.ijk[1] ){
     sprintf(str,fmt,ivp.ijk[1]) ;
   } else if( ivm.ijk[2] != ivp.ijk[2] ){
     sprintf(str,fmt,ivp.ijk[2]) ;
   } else {
     sprintf(str,fmt,nnn) ; /* should be impossible */
   }
   strcpy( lpart[LPART_SLICE_INDEX] , str ) ;

   /* slice coordinate */

   fvp = THD_3dind_to_3dmm ( dset , ivp ) ;
   fvp = THD_3dmm_to_dicomm( dset , fvp ) ;

   fvm = THD_3dind_to_3dmm ( dset , ivm ) ;
   fvm = THD_3dmm_to_dicomm( dset , fvm ) ;

   dxyz = MIN(br->del1,br->del2) ;
   dxyz = MIN(dxyz    ,br->del3) ; dxyz *= 0.1f ;
   if( dxyz <= 1.e-4f ) dxyz = 1.e-4f ;

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
     cc = 0.0f ; dd = "" ; /* should be impossible */
   }

   sprintf(str,"%6.2f",fabs(cc)) ;
   for( ii=strlen(str)-1 ; ii > 0 && str[ii] == '0' ; ii-- ) str[ii] = '\0' ;
   if( str[ii] == '.' ) str[ii] = '\0' ;
   strcat(str, dd) ;
   strcpy( lpart[LPART_SLICE_COORD] , str ) ;

   /* time index */

   nnn = DSET_NVALS(dset) ;
   fmt =  (nnn < 10)    ? "#%1d"
        : (nnn < 100)   ? "#%02d"
        : (nnn < 1000)  ? "#%03d"
        : (nnn < 10000) ? "#%04d"
        : (nnn < 100000)? "#%05d"
        :                 "#%06d" ;
   sprintf(str,fmt,im3d->vinfo->time_index) ;
   strcpy( lpart[LPART_TIME_INDEX] , str ) ;

   /* time coord */

   cc = DSET_TIMEORIGIN(dset) + im3d->vinfo->time_index * DSET_TR(dset) ;

   sprintf(str,"%-16.5f",cc) ; /* %- means will be left justified, not right */
   for( ii=strlen(str)-1 ;
        ii > 0 && (str[ii] == '0' || str[ii] == ' ') ; ii-- ) str[ii] = '\0' ;
   if( str[ii] == '.' ) str[ii] = '\0' ;
   strcpy( lpart[LPART_TIME_COORD] , str ) ;

   return 0 ;
}

/*----------------------------------------------------------------------
   routine to extract a plane of data from a 3D brick
   (used as a "get_image" routine for an MCW_imseq)
------------------------------------------------------------------------*/

XtPointer AFNI_brick_to_mri( int n , int type , FD_brick *br )
{
   MRI_IMAGE *im ;
   MCW_imseq_status *stat ;
   int i1,i2,jb,bb , dd1,dd2,tt1,tt2 ;

ENTRY("AFNI_brick_to_mri") ;

if(PRINT_TRACING){ char str[1024] ; sprintf(str,"n=%d type=%d",n,type) ; STATUS(str) ; }

   if( br == NULL ){  /* should never happen */
     ERROR_message("AFNI_brick_to_mri: bad FD_brick :-(") ; RETURN(NULL) ;
   }

   /*-------------------------------------------------*/
   /*-------- May 1996: graph callbacks first --------*/

   if( type == graCR_getlabel ){                  /* 18 Apr 2011 */
     char *lab = DSET_BRICK_LABEL(br->dset,n) ;
     RETURN( (XtPointer)lab ) ;
   }

   if( type == graCR_getstatus ){
      MCW_grapher_status *grstat = myXtNew( MCW_grapher_status ) ;

      grstat->num_total  = grstat->num_series = br->dset->dblk->nvals ;
      grstat->nx         = br->n1 ;
      grstat->ny         = br->n2 ;
      grstat->nz         = br->n3 ;

      grstat->send_CB    = (void (*)(void))AFNI_gra_send_CB ;
      grstat->parent     = (XtPointer) br ;
      grstat->aux        = NULL ;

      grstat->transforms0D = & (GLOBAL_library.registered_0D) ;
      grstat->transforms1D = & (GLOBAL_library.registered_1D) ;

      MCW_strncpy( grstat->namecode , br->namecode , 32 ) ;

      RETURN( (XtPointer) grstat ) ;
   }

   if( type == graCR_getseries ){
      Three_D_View *im3d = (Three_D_View *)br->parent ;
      MCW_grapher *grapher = UNDERLAY_TO_GRAPHER(im3d,br) ;

      im = FD_brick_to_series( n , br ) ; if( im == NULL ) RETURN(NULL) ;
      im->flags = 1 ;

      if( grapher->thresh_fade && im3d->vinfo->func_visible ){  /* Mar 2013 */
        int nsl = n / (br->n1 * br->n2) ;
#if 0
INFO_message("thresh_fade: nsl=%d",nsl) ;
#endif
        if( br->ntmask != nsl ){
          FD_brick *br_fim = UNDERLAY_TO_OVERLAY(im3d,br) ;
          MRI_IMAGE *fov = AFNI_func_overlay(nsl,br_fim) ;
          STATUS("clear overlay tmask") ;
          CLEAR_TMASK(br) ;
#if 0
ININFO_message("  get new tmask") ;
#endif
          if( fov != NULL ){
            br->tmask  = ISQ_binarize_overlay(fov) ;
            STATUSp("new tmask",br->tmask) ;
            br->ntmask = nsl ; mri_free(fov) ;
          }
        }
        if( br->tmask != NULL ){
          byte *tar = MRI_BYTE_PTR(br->tmask) ;
          int ij = n % (br->n1 * br->n2) ;
          im->flags = (int)tar[ij] ;
#if 0
ININFO_message("  set tsim flags=%d at ij=%d",im->flags) ;
#endif
        }
      }

      RETURN( (XtPointer)im ) ;
   }

   /*----------------------------------------*/
   /*-------- Now do imseq callbacks --------*/

   /*--- set the sub-brick (ival) index shift ---*/

   if( type == isqCR_deltival ){  /* 23 Feb 2011 */
     Three_D_View *im3d = (Three_D_View *)br->parent ;
     FD_brick *brfim ;
     br->deltival = n ;
     brfim = UNDERLAY_TO_OVERLAY(im3d,br) ;
     if( brfim != NULL ) brfim->deltival = n ;
     RETURN( NULL ) ;
   }

   if( n < 0 || n >= br->n3 ) RETURN(NULL) ;

   /*--- overlay # n ---*/

   if( type == isqCR_getoverlay  ){
      Three_D_View *im3d = (Three_D_View *)br->parent ;

STATUS("get overlay") ;

      im = AFNI_overlay( n , br ) ;
      if( !AFNI_noenv("AFNI_VALUE_LABEL") ) AFNI_do_bkgd_lab( im3d ) ;
      RETURN( (XtPointer) im ) ;
   }

   /*--- status ---*/

   if( type == isqCR_getstatus ){

STATUS("get status") ;

      stat = myXtNew( MCW_imseq_status ) ;

      stat->num_total  = br->n3 ;
      stat->num_series = br->n3 ;
      stat->send_CB    = (void (*)(void))AFNI_seq_send_CB ;
      stat->parent     = (XtPointer) br ;
      stat->aux        = NULL ;

      stat->transforms0D = & (GLOBAL_library.registered_0D) ;
      stat->transforms2D = & (GLOBAL_library.registered_2D) ;
      stat->slice_proj   = & (GLOBAL_library.registered_slice_proj) ;

      RETURN( (XtPointer) stat ) ;
   }

   /*--- 26 Feb 2001: return a memplot drawing struct ---*/
   /*--- 22 Mar 2002: add crosshairs to surface stuff ---*/

#define RX 0.2
   if( type == isqCR_getmemplot ){
     Three_D_View *im3d = (Three_D_View *)br->parent ;
     THD_3dim_dataset *udset = im3d->anat_now ; /* 07 Jan 2008 */
#ifdef IMAGEIZE_CROSSHAIRS  /* disable crosshairs drawn into overlay pixels */
     int do_xhar=(im3d->vinfo->crosshair_visible && AFNI_yesenv("AFNI_CROSSHAIR_LINES"));
#else
     int do_xhar=(im3d->vinfo->crosshair_visible) ;
#endif
     int do_surf;
     MEM_plotdata *mp ;
     AFNI_surface_widgets *swid = im3d->vwid->view->swid ;  /* 19 Aug 2002 */
     THD_session *suss=im3d->ss_now ;                       /* 20 Jan 2004 */
     THD_dataxes *daxes=CURRENT_DAXES(udset) ;

     if( !IM3D_OPEN(im3d) )     RETURN(NULL) ;

     /* 20 Jan 2004: surfaces are now in the session, not on the dataset! */

     do_surf = SUMA_ENABLED && SESSION_HAS_SUMA(suss) ;
     if( !do_surf && !do_xhar ) RETURN(NULL) ;  /* nothing to do */

     /* get ready to plot */

STATUS("creating memplot for image overlay") ;
     create_memplot_surely( "SUMA_plot" , 1.0 ) ;
     mp = get_active_memplot() ;

     /* plot surface stuff, if any (this is long) */

    if( do_surf ){
     int ks , qs , rs ;
     int kbest=-1 , ibest=-1 ;              /* 24 Feb 2003 */
     int nsurf; SUMA_surface **aglist=NULL; /* 07 Apr 2014 */

     AFNI_get_xhair_node( im3d , &kbest , &ibest ) ;   /* 24 Feb 2003 */

     /* Find all the surfaces to draw -- more complex after 07 Apr 2014 */

     nsurf = suss->su_num ;
     for( qs=0 ; qs < suss->su_nummask ; qs++ )
       nsurf += suss->su_mask[qs]->num_surf ;

     aglist = (SUMA_surface **)calloc(nsurf,sizeof(SUMA_surface *)) ;

     for( qs=0 ; qs < suss->su_num ; qs++ ){
       aglist[qs] = suss->su_surf[qs] ;
     }
     for( ks=qs,qs=0 ; qs < suss->su_nummask ; qs++ ){
       for( rs=0 ; rs < suss->su_mask[qs]->num_surf ; rs++ ){
         aglist[ks] = suss->su_mask[qs]->surf[rs] ;
       }
     }

     for( ks=0 ; ks < nsurf ; ks++ ){      /* 14 Aug 2002: loop over surfaces */
      SUMA_surface *ag = aglist[ks] ; SUMA_mask *msk = NULL ;
      int nn , ii,jj , relxyz=0 ;
      SUMA_ixyz *nod ;
      THD_ivec3 iv,ivp,ivm ;
      THD_fvec3 fv,fvp,fvm ;
      float s1=1.0/br->n1 , s2=1.0/br->n2 , dxyz ;
      float rr_box=1.0,gg_box=0.0,bb_box=0.0 ;   /* white */
      float rr_lin=0.4,gg_lin=0.0,bb_lin=0.7 ;   /* dark blue */
      float rr_led=1.0,gg_led=0.0,bb_led=0.0 ;
      char str[128] , *eee ;
      float rx=RX ;         /* default rectangle halfsize */
      int   kkk=0 ;
      float xyz=0.0,xyzp=0.0,xyzm=0.0 , rxm,rxp ;
      int skip_boxes=1 , skip_lines=0 , skip_lcen=0, skip_ledg=1 ;
      float boxsize=RX , linewidth=0.0 ;      /* 23 Feb 2003 */
      int firstb ;                            /* 23 Jan 2004 */
      float xrel=0.0f,yrel=0.0f,zrel=0.0f ;

      if( ag == NULL ) continue ;             /* skip this non-existent one */
      nn = ag->num_ixyz ; nod = ag->ixyz ;
      if( nn < 1 || nod == NULL ) continue ;  /* nothing can be done */

      if( ag->parent_type == SUMA_MASK_TYPE ){        /* relative coords */
        relxyz = 1 ; msk = (SUMA_mask *)ag->parent ;  /* in a mask surf? */
        xrel = msk->show_cen.xyz[0] - msk->init_cen.xyz[0] ;
        yrel = msk->show_cen.xyz[1] - msk->init_cen.xyz[1] ;
        zrel = msk->show_cen.xyz[2] - msk->init_cen.xyz[2] ;
/** INFO_message("SUMA_mask draw: xyz rel = %g %g %g",xrel,yrel,zrel) ; **/
      }

      /* define parameters for node boxes and triangle lines */

STATUS("defining surface drawing parameters") ;
      if( swid != NULL && ks < swid->nrow ){     /* 19 Aug 2002: the new way */
        int cc, dd ;                             /*           to set colors: */
                                                 /* from the surface widgets */

        cc = MCW_val_bbox(swid->surf_bbox[ks]) ; /* 19 Feb 2003: skip it all? */
        if( cc == 0 ){
          skip_boxes = skip_lines = 1 ;
        } else {                                 /* see what is turned on */
          cc = swid->surf_node_av[ks]->ival ;
          skip_boxes = (cc == 0) ;
          if( !skip_boxes ){
            rr_box = DCOV_REDBYTE(im3d->dc,cc)   / 255.0 ;
            gg_box = DCOV_GREENBYTE(im3d->dc,cc) / 255.0 ;
            bb_box = DCOV_BLUEBYTE(im3d->dc,cc)  / 255.0 ;
          }
          cc = swid->surf_line_av[ks]->ival ;
          dd = swid->surf_ledg_av[ks]->ival ;             /* 26 Feb 2003 */
          skip_lcen  = (cc == 0) ;
          skip_ledg  = (dd == 0) ;
          skip_lines = (skip_lcen && skip_ledg) ;
          if( cc > 0 ){
            rr_lin = DCOV_REDBYTE(im3d->dc,cc)   / 255.0 ;
            gg_lin = DCOV_GREENBYTE(im3d->dc,cc) / 255.0 ;
            bb_lin = DCOV_BLUEBYTE(im3d->dc,cc)  / 255.0 ;
          }
          if( dd > 0 ){                                   /* 26 Feb 2003 */
            rr_led = DCOV_REDBYTE(im3d->dc,dd)   / 255.0 ;
            gg_led = DCOV_GREENBYTE(im3d->dc,dd) / 255.0 ;
            bb_led = DCOV_BLUEBYTE(im3d->dc,dd)  / 255.0 ;
          }
          boxsize   = swid->boxsize_av->ival   * 0.1   ;  /* 23 Feb 2003 */
          linewidth = swid->linewidth_av->ival * 0.002 ;
        }

      } else if( msk != NULL ){ /* set from inside the surf struct [Apr 2014] */

        DC_parse_color( im3d->dc , ag->line_color , &rr_lin,&gg_lin,&bb_lin ) ;
        DC_parse_color( im3d->dc , ag->box_color  , &rr_box,&gg_box,&bb_box ) ;
        linewidth = ag->line_width * 0.002f ;
        skip_boxes = 1 ; skip_lines = 0 ; skip_lcen = 0; skip_ledg = 1 ;

      } else {                                   /* the old way    */
                                                 /* to set colors:  */
        rgbyte bcolor , lcolor ;                 /* from environment */
        AFNI_get_suma_color( ks , &bcolor , &lcolor ) ;
        if( bcolor.r == 1 && bcolor.g == 1 && bcolor.b == 1 ){
          skip_boxes = 1 ;                  /* don't do boxes */
        } else {
          rr_box = bcolor.r / 255.0f ;
          gg_box = bcolor.g / 255.0f ;
          bb_box = bcolor.b / 255.0f ;
        }
        if( lcolor.r == 1 && lcolor.g == 1 && lcolor.b == 1 ){
          skip_lines = 1 ;                  /* don't do lines */
        } else {
          rr_lin = lcolor.r / 255.0f ;
          gg_lin = lcolor.g / 255.0f ;
          bb_lin = lcolor.b / 255.0f ;
        }

        eee = getenv("AFNI_SUMA_BOXSIZE") ;  /* maybe set boxsize? */
        if( eee != NULL ){
          float val=strtod(eee,NULL) ;
          if( val > 0.0 ) boxsize = val ;
        } else if( swid != NULL ){
          boxsize = swid->boxsize_av->ival * 0.1 ;
        }

        eee = getenv( "AFNI_SUMA_LINESIZE" ) ; /* maybe set linewidth? */
        if( eee != NULL ){
          float val = strtod(eee,NULL) ;
          if( val < 0.0 || val > 0.1 ) val = 0.0 ;
          linewidth = val ;
        } else if( swid != NULL ){
          linewidth = swid->linewidth_av->ival * 0.002 ;
        }
      }

      if( skip_boxes && skip_lines ) continue ; /* nothing to do? */

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

      rx  = boxsize ;                /* 23 Feb 2003 */
      rxm = rx-0.5 ; rxp = rx+0.5 ;  /* The 0.5 voxel shift */

      /* find DICOM coordinates of next slice and previous slice */

      LOAD_IVEC3(iv,0,0,n+1) ;                     /* next */
      ivp = THD_fdind_to_3dind( br , iv ) ;
      fvp = THD_3dind_to_3dmm ( udset , ivp ) ;
      fvp = THD_3dmm_to_dicomm( udset , fvp ) ;
      LOAD_IVEC3(iv,0,0,n-1) ;                     /* previous */
      ivm = THD_fdind_to_3dind( br , iv ) ;
      fvm = THD_3dind_to_3dmm ( udset , ivm ) ;
      fvm = THD_3dmm_to_dicomm( udset , fvm ) ;

      /* threshold for determining which axis this slice is along */

      dxyz = MIN(br->del1,br->del2) ;
      dxyz = MIN(dxyz    ,br->del3) ; dxyz *= 0.1f ;

      set_color_memplot(rr_box,gg_box,bb_box) ;  /* box drawing colors */
      set_thick_memplot(0.0) ;
      firstb = 1 ;                               /* 23 Jan 2004 */

      /* find nodes inside this slice */

      if( skip_boxes ) STATUS("finding slice planes") ;
      else             STATUS("drawing node boxes") ;

      if( fabs(fvm.xyz[0]-fvp.xyz[0]) > dxyz ){               /* search x */
         float xb=fvm.xyz[0] , xt=fvp.xyz[0] , xm,xw ;        /* range of  */
         float nodx,nody,nodz ;
         if( xb > xt ){ float t=xb ; xb=xt ; xt=t ; }         /* x in slice */
         xm = 0.5*(xb+xt); xw = 0.25*(xt-xb); xb = xm-xw; xt = xm+xw;
STATUS(" - x plane") ;
         if( !skip_boxes ){
          for( ii=0 ; ii < nn ; ii++ ){
            nodx = nod[ii].x ; nody = nod[ii].y ; nodz = nod[ii].z ;
            if( relxyz ){ nodx += xrel ; nody += yrel ; nodz += zrel ; }
            if( nodx >= xb && nodx <= xt ){         /* inside?  */
               LOAD_FVEC3(fv,nodx,nody,nodz) ;                /* convert  */
               fv = THD_dicomm_to_3dmm( udset , fv ) ;        /* coords   */
               fv = THD_3dmm_to_3dfind( udset , fv ) ;        /* to slice */
               fv = THD_3dfind_to_fdfind( br , fv ) ;         /* indexes  */

               if( firstb ){
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  );
                 firstb = 0 ;
               }

               plotrect_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                 s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  ) ;

               if( ks == kbest && nod[ii].id == ibest ){   /* 24 Feb 2003 */
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  );
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]+rxp),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]-rxm)  );
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]+0.5),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+0.5)  );
                 plotline_memplot( s1*(fv.xyz[0]+0.5), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+0.5), 1.0-s2*(fv.xyz[1]+rxp)  );
               }
            }
          }
         }
         kkk = 0; xyz = xm; xyzp = xt; xyzm = xb;  /* for the triangles/lines below */
      }
      else if( fabs(fvm.xyz[1]-fvp.xyz[1]) > dxyz ){          /* search y */
         float yb=fvm.xyz[1] , yt=fvp.xyz[1] , ym,yw ;
         float nodx,nody,nodz ;
         if( yb > yt ){ float t=yb ; yb=yt ; yt=t ; }
         ym = 0.5*(yb+yt); yw = 0.25*(yt-yb); yb = ym-yw; yt = ym+yw;
STATUS(" - y plane") ;
         if( !skip_boxes ){
          for( ii=0 ; ii < nn ; ii++ ){
            nodx = nod[ii].x ; nody = nod[ii].y ; nodz = nod[ii].z ;
            if( relxyz ){ nodx += xrel ; nody += yrel ; nodz += zrel ; }
            if( nody >= yb && nody <= yt ){
               LOAD_FVEC3(fv,nodx,nody,nodz) ;
               fv = THD_dicomm_to_3dmm( udset , fv ) ;
               fv = THD_3dmm_to_3dfind( udset , fv ) ;
               fv = THD_3dfind_to_fdfind( br , fv ) ;

               if( firstb ){
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  );
                 firstb = 0 ;
               }

               plotrect_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                 s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  ) ;

               if( ks == kbest && nod[ii].id == ibest ){   /* 24 Feb 2003 */
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  );
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]+rxp),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]-rxm)  );
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]+0.5),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+0.5)  );
                 plotline_memplot( s1*(fv.xyz[0]+0.5), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+0.5), 1.0-s2*(fv.xyz[1]+rxp)  );
               }
            }
          }
         }
         kkk = 1; xyz = ym; xyzp = yt; xyzm = yb;  /* for the triangles/lines below */
      }
      else if( fabs(fvm.xyz[2]-fvp.xyz[2]) > dxyz ){          /* search z */
         float zb=fvm.xyz[2] , zt=fvp.xyz[2] , zm,zw ;
         float nodx,nody,nodz ;
         if( zb > zt ){ float t=zb ; zb=zt ; zt=t ; }
         zm = 0.5*(zb+zt); zw = 0.25*(zt-zb); zb = zm-zw; zt = zm+zw;
STATUS(" - z plane") ;
         if( !skip_boxes ){
          for( ii=0 ; ii < nn ; ii++ ){
            nodx = nod[ii].x ; nody = nod[ii].y ; nodz = nod[ii].z ;
            if( relxyz ){ nodx += xrel ; nody += yrel ; nodz += zrel ; }
            if( nodz >= zb && nodz <= zt ){
               LOAD_FVEC3(fv,nod[ii].x,nod[ii].y,nod[ii].z) ;
               fv = THD_dicomm_to_3dmm( udset , fv ) ;
               fv = THD_3dmm_to_3dfind( udset , fv ) ;
               fv = THD_3dfind_to_fdfind( br , fv ) ;

               if( firstb ){
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  );
                 firstb = 0 ;
               }

               plotrect_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                 s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  ) ;

               if( ks == kbest && nod[ii].id == ibest ){   /* 24 Feb 2003 */
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+rxp)  );
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]+rxp),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]-rxm)  );
                 plotline_memplot( s1*(fv.xyz[0]-rxm), 1.0-s2*(fv.xyz[1]+0.5),
                                   s1*(fv.xyz[0]+rxp), 1.0-s2*(fv.xyz[1]+0.5)  );
                 plotline_memplot( s1*(fv.xyz[0]+0.5), 1.0-s2*(fv.xyz[1]-rxm),
                                   s1*(fv.xyz[0]+0.5), 1.0-s2*(fv.xyz[1]+rxp)  );
               }
            }
          }
         }
         kkk = 2; xyz = zm; xyzp = zt; xyzm = zb;  /* for the triangles/lines below */
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
        float ci,cj,ck , xlev ;
        int ilev ;

        for( ilev=0 ; ilev <= 2 ; ilev++ ){  /* 26 Feb 2003: loop over levels: */
                                             /* slice center, top & bot edges  */
          if( ilev == 0 ){
            if( skip_lcen ) continue ;  /* don't do center plane */
            xlev = xyz ;
            set_color_memplot(rr_lin,gg_lin,bb_lin) ;  /* line drawing colors */
            set_thick_memplot(linewidth) ;
          } else {
            if( skip_ledg ) continue ;  /* don't do edge planes */
            xlev = (ilev == 1) ? xyzp : xyzm ;
            set_color_memplot(rr_led,gg_led,bb_led) ;
            set_thick_memplot(0.0) ;
          }

STATUS("drawing triangle lines") ;

          /* loop over triangles */

          for( ii=0 ; ii < ntr ; ii++ ){

            /* get indexes of triangle's nodes (from their id's) */

            id = SUMA_find_node_id(ag,tr[ii].id); if( id < 0 ) continue;
            jd = SUMA_find_node_id(ag,tr[ii].jd); if( jd < 0 ) continue;
            kd = SUMA_find_node_id(ag,tr[ii].kd); if( kd < 0 ) continue;

            /* load DICOM coords of triangle's nodes */

            LOAD_FVEC3(fvijk[0], nod[id].x, nod[id].y, nod[id].z) ;
            LOAD_FVEC3(fvijk[1], nod[jd].x, nod[jd].y, nod[jd].z) ;
            LOAD_FVEC3(fvijk[2], nod[kd].x, nod[kd].y, nod[kd].z) ;

            if( relxyz ){  /* 07 Apr 2014 */
              fvijk[0].xyz[0] += xrel; fvijk[1].xyz[0] += xrel; fvijk[2].xyz[0] += xrel;
              fvijk[0].xyz[1] += yrel; fvijk[1].xyz[1] += yrel; fvijk[2].xyz[1] += yrel;
              fvijk[0].xyz[2] += zrel; fvijk[1].xyz[2] += zrel; fvijk[2].xyz[2] += zrel;
            }

            /* want 1 node on one size of plane, and 2 on the other */

            ci = fvijk[0].xyz[kkk] - xlev;      /* differences from center */
            cj = fvijk[1].xyz[kkk] - xlev;      /* of current slice plane */
            ck = fvijk[2].xyz[kkk] - xlev;
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

            ci = fvijk[id].xyz[kkk] - xlev;
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

            fvp = THD_dicomm_to_3dmm( udset , fvp ) ;
            if( fvp.xyz[0] < daxes->xxmin ||
                fvp.xyz[0] > daxes->xxmax ||
                fvp.xyz[1] < daxes->yymin ||
                fvp.xyz[1] > daxes->yymax ||
                fvp.xyz[2] < daxes->zzmin ||
                fvp.xyz[2] > daxes->zzmax   ) continue ;  /* 08 Jan 2004 */
            fvp = THD_3dmm_to_3dfind( udset , fvp ) ;
            fvp = THD_3dfind_to_fdfind( br , fvp ) ;

            fvm = THD_dicomm_to_3dmm( udset , fvm ) ;
            if( fvm.xyz[0] < daxes->xxmin ||
                fvm.xyz[0] > daxes->xxmax ||
                fvm.xyz[1] < daxes->yymin ||
                fvm.xyz[1] > daxes->yymax ||
                fvm.xyz[2] < daxes->zzmin ||
                fvm.xyz[2] > daxes->zzmax   ) continue ;  /* 08 Jan 2004 */
            fvm = THD_3dmm_to_3dfind( udset , fvm ) ;
            fvm = THD_3dfind_to_fdfind( br , fvm ) ;

            /* plot a line segment between them, in the plane of the slice */
            /* [21 Mar 2002: include the 0.5 shift mentioned way up above] */

            plotline_memplot( s1*(fvp.xyz[0]+0.5) , 1.0-s2*(fvp.xyz[1]+0.5) ,
                              s1*(fvm.xyz[0]+0.5) , 1.0-s2*(fvm.xyz[1]+0.5)  ) ;

          } /* end of loop over triangles */
          set_thick_memplot(0.0) ;              /* 15 Jan 2003 */
        } /* end of loop over levels: 26 Feb 2003 */
      } /* end of if over doing lines */
     } /* end of loop over surface index ks */
    } /* end of plotting surface stuff */

     /*----- put crosshairs on with lines, if desired -----*/
     /****** 22 Mar 2002: adapted from pixel overlay  ******/

     if( do_xhar ){
      MCW_grapher *grapher = UNDERLAY_TO_GRAPHER(im3d,br) ;
      float thth = (float)AFNI_numenv("AFNI_CROSSHAIR_THICKNESS") ;

      THD_ivec3 ib = THD_3dind_to_fdind( br ,
                                         TEMP_IVEC3( im3d->vinfo->i1 ,
                                                     im3d->vinfo->j2 ,
                                                     im3d->vinfo->k3  ) ) ;
STATUS("drawing crosshairs") ;

      if( thth < 0.0f || thth > 0.05f ) thth = 0.0f ;
      set_thick_memplot(thth) ;

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
      Three_D_View *im3d = (Three_D_View *) br->parent ;
      char *lab , str[2048] , *dd , *eee=NULL;
      char labstrf[256]={""}, labstra[256]={""};
      THD_ivec3 iv,ivp,ivm ;
      THD_fvec3 fv,fvp,fvm ;
      float dxyz , cc ;
      int ii, ival;
      double dval;
      THD_3dim_dataset *dset=NULL ;

      if( im3d->type != AFNI_3DDATA_VIEW ) RETURN(NULL) ;

      LOAD_IVEC3(iv,0,0,n) ;
      ivp = THD_fdind_to_3dind( br , iv ) ;

      if( n == 0 ) LOAD_IVEC3(iv,0,0,1) ;
      else         LOAD_IVEC3(iv,0,0,n-1) ;
      ivm = THD_fdind_to_3dind( br , iv ) ;

      if( AFNI_yesenv("AFNI_IMAGE_LABEL_IJK") ){ /* 27 Feb 2017 */

        int nnn ; char *fmt ;
        nnn = MAX(br->n1,br->n2) ; nnn = MAX(nnn,br->n3) ;
        fmt =  (nnn < 10)  ? "#%1d"
             : (nnn < 100) ? "#%02d"
             : (nnn < 1000)? "#%03d"
             :               "#%04d" ;

        if( ivm.ijk[0] != ivp.ijk[0] ){
          sprintf(str,fmt,ivp.ijk[0]) ;

        } else if( ivm.ijk[1] != ivp.ijk[1] ){
          sprintf(str,fmt,ivp.ijk[1]) ;

        } else if( ivm.ijk[2] != ivp.ijk[2] ){
          sprintf(str,fmt,ivp.ijk[2]) ;

        } else {  /* should never happen */
          RETURN(NULL) ;
        }

      } else {
        fvp = THD_3dind_to_3dmm ( br->dset , ivp ) ;
        fvp = THD_3dmm_to_dicomm( br->dset , fvp ) ;

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

        sprintf(str,"%3.2f",fabs(cc)) ;
        for( ii=strlen(str)-1 ; ii > 0 && str[ii] == '0' ; ii-- ) str[ii] = '\0' ;
        if( str[ii] == '.' ) str[ii] = '\0' ;
        strcat(str, dd) ;
      }

      if (!FD_brick_montized(br)){ /* Show labels if any.  ZSS Dec. 2011*/
         dset = Get_UO_Dset(br, 'U', 1, &ival);
         if ((dval = (double)THD_get_voxel_dicom(dset,
                              im3d->vinfo->xi,
                              im3d->vinfo->yj,
                              im3d->vinfo->zk, ival))>0.0) {
            AFNI_get_dset_val_label_maybeCR(dset,    /* Dec 7 2011 ZSS/DRG 2021 */
                                    dval, labstra);
         }
         dset = Get_UO_Dset(br, 'O', 1, &ival);
         if ((dval = (double)THD_get_voxel_dicom(dset,
                              im3d->vinfo->xi,
                              im3d->vinfo->yj,
                              im3d->vinfo->zk, ival))>0.0) {
            AFNI_get_dset_val_label_maybeCR(dset,    /* Dec 7 2011 ZSS/DRG 2021 */
                                    dval, labstrf);

         }

         if (labstrf[0] != '\0' || labstra[0] != '\0') {
            strcat(str, " \\noesc ");
            if (!strcmp(labstrf, labstra)) {
               strcat(str, "U+O:");
               strncat(str, labstra, 126*sizeof(char));
            } else {
               if (labstra[0] != '\0') {
                  strcat(str, "U:");
                  strncat(str, labstra, 126*sizeof(char));
               }
               if (labstrf[0] != '\0') {
                  strcat(str, labstra[0] != '\0' ? " O:" : "O:");
                  strncat(str, labstrf, 126*sizeof(char));
               }
            }
         }
      }

      lab = strdup(str) ;

      RETURN(lab) ;
   }

   /*--- underlay image # n ---*/
   if( type == isqCR_getimage  || type == isqCR_getqimage ||
       type == isqCR_getulayim || type == isqCR_getolayim   ){

      Three_D_View *im3d = (Three_D_View *)br->parent ;
      FD_brick *brr=NULL ;
      int ival;

      if (!(brr = Get_FD_Brick_As_Selected(br, type, &ival))) RETURN(NULL);

#if 0
INFO_message("%s: brr=%p  ival=%d",
              (type == isqCR_getimage ) ? "isqCR_getimage"
            : (type == isqCR_getqimage) ? "isqCR_getqimage"
            : (type == isqCR_getulayim) ? "isqCR_getulayim"
            : (type == isqCR_getolayim) ? "isqCR_getolayim" : "unknown" , brr , ival ) ;
#endif

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"getting image n1=%d n2=%d ival=%d",br->n1,br->n2,ival) ;
  STATUS(str) ; }

      LOAD_DSET_VIEWS(im3d) ;  /* 02 Nov 1996 */

      AFNI_set_ignore_vedit(1) ; /* 28 Jan 2008 */
      im = FD_warp_to_mri( n , ival , brr ) ; /* get image from dataset */
      AFNI_set_ignore_vedit(0) ;

      if( ival < 0 ) RETURN( (XtPointer) im ) ;  /* return fake image */

      /* Load value of current pixel into display label */
      /* April 1996: only if image is at current slice  */

      if( brr == br ){
        char buf[64] = "\0" ; int ibest=-1 ;
        AFNI_set_valabel( br , n , im , buf ) ;
        if( buf[0] != '\0' ){
          if( im3d->vinfo->underlay_type == UNDERLAY_ANAT )
            strcpy( im3d->vinfo->anat_val , buf ) ;
          else
            im3d->vinfo->anat_val[0] = '\0' ;

          if( !AFNI_noenv("AFNI_VALUE_LABEL") ) AFNI_do_bkgd_lab( im3d ) ;

          if( im->kind != MRI_complex && im->kind != MRI_rgb ){
            char qbuf[64] = "bg =" ;
            strcat(qbuf,buf) ; strcpy(buf,qbuf) ;
          }
          AFNI_get_xhair_node( im3d , NULL , &ibest ) ;   /* 21 Feb 2003 */
          if( ibest >= 0 ){
            char qbuf[64]; sprintf(qbuf,"\nxh = #%d",ibest); strcat(buf,qbuf);
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

#undef   PFVAL
#if 1
# define PFVAL(vv,bb) strcpy((bb),AV_uformat_fval(vv))
#else
# define PFVAL(vv,bb) AV_fval_to_char((vv),(bb))
#endif

/*-----------------------------------------------------------------------------*/
/*! Set a value label when the nsl-th image is in "im".
*//*---------------------------------------------------------------------------*/

void AFNI_set_valabel( FD_brick *br , int nsl , MRI_IMAGE *im , char *blab )
{
   Three_D_View *im3d = (Three_D_View *) br->parent ;
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
      default: strcpy(blab,":(") ;

      case MRI_byte:{
         int val = MRI_BYTE_2D(im , ib.ijk[0],ib.ijk[1]) ;
         sprintf( blab , "%d" , val ) ;
      }
      break ;

      case MRI_short:{
         int val = MRI_SHORT_2D(im , ib.ijk[0],ib.ijk[1]) ;
         sprintf( blab , "%d" , val ) ;
      }
      break ;

      case MRI_int:{
         int val = MRI_INT_2D(im , ib.ijk[0],ib.ijk[1]) ;
         sprintf( blab , "%d" , val ) ;
      }
      break ;

      case MRI_float:{
         float val = MRI_FLOAT_2D(im , ib.ijk[0],ib.ijk[1]) ;
         PFVAL(val,blab) ;
      }
      break ;

      case MRI_complex:{
         int iblab ; char qbuf[16] ; complex val ;
         val = MRI_COMPLEX_2D(im , ib.ijk[0],ib.ijk[1]) ;
         PFVAL(val.r,blab) ; iblab = strlen(blab) ;
         if( val.i >= 0.0 ) blab[iblab++] = '+' ;
         PFVAL(val.i,blab+iblab) ; iblab = strlen(blab) ;
         blab[iblab++] = 'I' ; blab[iblab++] = '\0' ;
      }
      break ;

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         int ii = ib.ijk[0] + im->nx * ib.ijk[1] ;
         sprintf(blab,"(%d,%d,%d)",(int)rgb[3*ii],(int)rgb[3*ii+1],(int)rgb[3*ii+2]) ;
      }
      break ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------
   read image files directly into a 3D dataset.
   this will be incomplete, but is enough for display purposes.
------------------------------------------------------------------------*/

THD_3dim_dataset * AFNI_read_images( int nf , char *fname[] )
{
   MRI_IMAGE *im , *shim ;
   char *bar ;
   int nx , ny , nz , lf , kz , kim , npix,ii ;
   MRI_IMARR *arr ;
   char str[256] ;
   THD_3dim_dataset *dset ;
   int datum = GLOBAL_argopt.datum , dsize ;

   int nvals , nzz , nzin=0 ;  /* 19 Oct 1999 */
   float dx=0.0, dy=0.0 , dz=0.0 ;  /* 29 Jul 2002 */

ENTRY("AFNI_read_images") ;

   /*----- see if there are any images to read! -----*/

   if( nf < 1 ) ERROR_exit("No images on command line!?") ;

   /* count total number of images */

   nz = 0 ;
   for( lf=0 ; lf < nf ; lf++ ){
     if( THD_is_directory(fname[lf]) ){  /* 21 Feb 2007 */
       fprintf(stderr,
               "\n** Fatal Error: %s is a directory, not an image file!",
               fname[lf]) ;
       exit(1) ;
     }
     ii = mri_imcount( fname[lf] ) ;
     if( ii == 0 )
       ERROR_exit("Illegal image file specifier: %s",fname[lf]) ;
     nz += ii ;
   }
   if( nz == 1 ) nz = 2 ;  /* special case for just one image */

   /*--- read 1st file to get sizes ---*/

STATUS("read first file") ;
   arr = mri_read_file( fname[0] ) ;
   if( arr == NULL || arr->num == 0 )
     ERROR_exit("Cannot read first image file: %s",fname[0]) ;

   im = arr->imarr[0] ;
   nx = im->nx ;
   ny = im->ny ; npix = nx * ny ;

   if( im->dw > 0.0 ){
     dx = fabs(im->dx); dy = fabs(im->dy); dz = fabs(im->dz);  /* 29 Jul 2002 */
   }

   if( datum < 0 ) datum = im->kind ;
   if( ! AFNI_GOOD_DTYPE(datum) )
     ERROR_exit("Illegal datum type found in image") ;

   dsize = mri_datum_size( (MRI_TYPE) datum ) ;
   bar   = (char *) malloc( dsize * nx*ny*nz ) ;
   if( bar == NULL )
     ERROR_exit("Can't malloc memory for image input :-( !!!") ;

   /*--- read all files, convert if needed, put in the cube ---*/

   REPORT_PROGRESS("\nReading unoriented image data") ;
   kz = 0 ;
   for( lf=0 ; lf < nf ; lf++ ){

      /** read the file (except the first, which we already have **/

      if( lf != 0 ){
STATUS("read next file") ;
         arr = mri_read_file( fname[lf] ) ;
         if( arr == NULL || arr->num == 0 )
           ERROR_exit("Cannot read image file: %s",fname[lf]) ;
      }

      /** for each image in file ... **/

      for( kim=0 ; kim < arr->num ; kim++ ){
         im = arr->imarr[kim] ;

         /** check if image matches dimensions of first slice **/

         if( im->nx != nx || im->ny != ny ){
            if( ! GLOBAL_argopt.resize_images ){
               sprintf(str, "Image size mismatch:\n"
                           " ** expected nx=%d ny=%d but got nx=%d ny=%d in file %s" ,
                           nx,ny,im->nx,im->ny , fname[lf] ) ;
               ERROR_exit(str) ;
            } else {
               MRI_IMAGE *rim ;
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
            if( shim == NULL ) ERROR_exit("Illegal datum convert!") ;
            mri_free( im ) ;
         }

         /** copy bytes from slice into the "bar" brick **/

         memcpy( bar + dsize*npix*kz , mri_data_pointer(shim) , dsize*npix ) ;
         kz++ ;

         mri_free(shim) ;
         if( kz%100 == 1 ) REPORT_PROGRESS(".") ;
      }
      FREE_IMARR(arr) ;  /* not DESTROY_IMARR, since images are already gone */
   }

   /*** special case of one input image: duplicate the image data ***/

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
   dset->self_warp     = NULL ;  /* 26 Aug 2002 */
   dset->warp_parent   = NULL ;
   dset->anat_parent   = NULL ;
   dset->stats         = NULL ;
   dset->death_mark    = 0 ;
   dset->tcat_list     = NULL ;  /* 04 Aug 2004 */
   dset->tcat_num      = 0 ;
   dset->tcat_len      = NULL ;
   dset->taxis         = NULL ;
   dset->tagset        = NULL ;  /* Oct 1998 */
   dset->Label_Dtable  = NULL;    /* ZSS Feb 26 2010 */
   ZERO_STAT_AUX( dset ) ;
#ifdef ALLOW_DATASET_VLIST
   dset->pts           = NULL ;
#endif

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

   dset->dblk->vedim = NULL ;  /* 05 Sep 2006 */

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
      char *qbar ;

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
   dset->daxes->xxdel = 1.0 ;        /* arbitrary units */
   dset->daxes->yydel = GLOBAL_argopt.dy ;  /* these allow user to alter */
   dset->daxes->zzdel = GLOBAL_argopt.dz ;  /* the images' aspect ratio */
   dset->daxes->xxorg = dset->daxes->yyorg = dset->daxes->zzorg = 0.0 ;
   dset->daxes->parent= (XtPointer) dset ;

   if( dx > 0.0 ) dset->daxes->xxdel = dx ;  /* 29 Jul 2002 */
   if( dy > 0.0 ) dset->daxes->yydel = dy ;
   if( dz > 0.0 ) dset->daxes->zzdel = dz ;

   dset->idcode = MCW_new_idcode() ;
   ZERO_IDCODE(dset->anat_parent_idcode) ;
   ZERO_IDCODE(dset->warp_parent_idcode) ;

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

     if( xx < 0 || yy < 0 || zz < 0 || ! OR3OK(xx,yy,zz) )
       ERROR_exit("Unusable -orient code!") ;

     dset->daxes->xxorient = xx ;
     dset->daxes->yyorient = yy ;
     dset->daxes->zzorient = zz ;
   }

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
     THD_daxes_to_mat44( dset->daxes ) ;

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

/*----------------------------------------------------------------------*/
/* Jumpto current cluster peak */

void AFNI_jumpto_clus( Three_D_View *im3d )  /* 19 Oct 2012 */
{
  int ic ; float px,py,pz , xx,yy,zz ;
  AFNI_clu_widgets *cwid = im3d->vwid->func->cwid ;
  mri_cluster_detail *cld = im3d->vwid->func->clu_det ;
  if( cwid == NULL || cld == NULL ) return ;
  ic = AFNI_clus_find_xyz_nearest( im3d,
                           im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk ) ;
  if( ic < 0 ) return ;
  AFNI_clus_action_CB( cwid->clu_jump_pb[ic] , (XtPointer)im3d , (XtPointer)666 ) ;
  return ;
}

/*----------------------------------------------------------------------*/
/* Jumpto nearby cluster peak */

void AFNI_jumpto_clus_nearby( Three_D_View *im3d , int dci ) /* 29 Apr 2019 */
{
  int ic,nic,nclu ; float px,py,pz , xx,yy,zz ;
  AFNI_clu_widgets *cwid = im3d->vwid->func->cwid ;
  mri_cluster_detail *cld = im3d->vwid->func->clu_det ;
  MCW_cluster_array *clar ; MCW_cluster *cl ;

  if( cwid == NULL || cld == NULL ) return ;
  clar = im3d->vwid->func->clu_list ; if( clar == NULL ) return ;
  nclu = clar->num_clu ;              if( nclu == 0    ) return ;
  ic = AFNI_clus_find_xyz( im3d,
                           im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk ) ;
  if( ic >= 0 ){
    nic = ic + dci ;
    if( nic >= nclu  ) nic = nic % nclu ;
    else if( nic < 0 ) nic = (nic + 99*nclu) % nclu ;
  } else {
    nic = AFNI_clus_find_xyz_nearest( im3d,
                           im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk ) ;
  }
  if( nic < 0 ) return ;
  AFNI_clus_action_CB( cwid->clu_jump_pb[nic], (XtPointer)im3d, (XtPointer)666 ) ;
  return ;
}

/*----------------------------------------------------------------------
   respond to events that one of the MCW_imseq's sends to us
------------------------------------------------------------------------*/

void AFNI_seq_send_CB( MCW_imseq *seq , FD_brick *br , ISQ_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) seq->parent ;

ENTRY("AFNI_seq_send_CB") ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"reason=%d",cbs->reason) ; STATUS(str) ; }

   if( ! IM3D_VALID(im3d) ||
       (   im3d->ignore_seq_callbacks == AFNI_IGNORE_EVERYTHING
        && cbs->reason                != isqCR_getxynim        ) ){

     STATUS("ignoring call") ; EXRETURN ;
   }

   switch( cbs->reason ){

      default: break ;

      case isqCR_destroy:{
         MCW_imseq *sxyz = im3d->s123 ,
                   *syzx = im3d->s231 ,
                   *szxy = im3d->s312  ;
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

         myXtFree( seq->status ) ; /* 28 Sep 1998: via Purify */
         myXtFree( seq ) ;
         MCW_invert_widget(w) ;  /* back to normal */
         NORMAL_cursorize(w) ;   /* 20 Jul 2005 */
         INIT_BKGD_LAB(im3d) ;
         AFNI_view_setter(im3d,NULL) ;

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

      case isqCR_buttonmove:{  /* 17 Mar 2010: InstaCorr on the go go go! */
        XMotionEvent *xev = (XMotionEvent *)cbs->event ;

        if( im3d->ignore_seq_callbacks != AFNI_IGNORE_NOTHING    ) EXRETURN ;
        if( !(xev->state&ShiftMask) || !(xev->state&ControlMask) ) EXRETURN ;

        if( cbs->xim >= 0 && cbs->xim < br->n1 &&
            cbs->yim >= 0 && cbs->yim < br->n2 &&
            cbs->nim >= 0 && cbs->nim < br->n3   ){  /* inside brick? */

          THD_ivec3 id ; int qq , ii,jj,kk ;

          /* find location in underlay dataset */

          id = THD_fdind_to_3dind(br,TEMP_IVEC3(cbs->xim,cbs->yim,cbs->nim));
          UNLOAD_IVEC3(id,ii,jj,kk) ;

          qq = AFNI_icor_setref_anatijk(im3d,ii,jj,kk) ;
          if( qq > 0 && im3d->giset == NULL ) AFNI_icor_setref_locked(im3d) ;
        }
      }
      MPROBE ;
      break ;  /* end of button move (while clicked down) */

      case isqCR_buttonpress:{
         XButtonEvent *xev = (XButtonEvent *)cbs->event ;
         int doing_icor = ( (xev->state&ShiftMask) && (xev->state&ControlMask) ) ;
         int doing_jump = AFNI_yesenv("AFNI_INSTACORR_JUMP") ;

if(PRINT_TRACING){
 char str[256] ;
 sprintf(str,"isqCR_buttonpress: button=%d state=%x",(int)xev->button,xev->state) ;
 STATUS(str) ; }

         im3d->vwid->butx = xev->x_root ;  /* 17 May 2005 */
         im3d->vwid->buty = xev->y_root ;
         switch( xev->button ){

            default: EXRETURN ;  /* unused button */

            case Button3:{  /* popup menu */
               XtVaSetValues( im3d->vwid->imag->popmenu ,
                                 XmNuserData , (XtPointer) seq ,   /* who */
                              NULL ) ;
               XmMenuPosition( im3d->vwid->imag->popmenu , xev ) ; /* where */
               XtManageChild ( im3d->vwid->imag->popmenu ) ;       /* pop */
            }
            break ;

            case Button1:{   /* set viewpoint? set InstaCorr? */
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

                  /* get index triple in 3D dataset from viewing FD brick */

                  id = THD_fdind_to_3dind(
                          br , TEMP_IVEC3(cbs->xim,cbs->yim,cbs->nim) );

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str," 3D dataset coordinates %d %d %d",
          id.ijk[0],id.ijk[1],id.ijk[2] ) ; STATUS(str) ; }

                  /* jump viewpoint (crosshairs) to the selected point */

                  if( !doing_icor || (doing_icor && doing_jump) ){
                    SAVE_VPT(im3d) ;  /* save current location as jumpback */

                    if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING ){

                      /* 20 Feb 2003: set plane from which viewpoint is controlled */

                      AFNI_view_setter(im3d,seq) ;
                      AFNI_set_viewpoint(
                         im3d , id.ijk[0] , id.ijk[1] , id.ijk[2] ,
                         (im3d->vinfo->crosshair_visible==True) ?
                         REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
                    }
                  }

                  /* 08 May 2009: if Shift+Control both pressed, do InstaCorr */

                  if( doing_icor ){
                    int qq = AFNI_icor_setref_anatijk(im3d,id.ijk[0],id.ijk[1],id.ijk[2]) ;
                    if( qq > 0 && im3d->giset == NULL ) AFNI_icor_setref_locked(im3d) ; /* 15 May 2009 */
                  }
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

         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING ){

            /* 20 Feb 2003: set plane from which viewpoint is controlled */

            AFNI_view_setter(im3d,seq) ;
            AFNI_set_viewpoint(
               im3d , id.ijk[0] , id.ijk[1] , id.ijk[2] ,
               (im3d->vinfo->crosshair_visible==True) ?
               REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
         }
      }
      break ;  /* end of new image */

      /** July 1996: an image viewer changed montage layout **/

      case isqCR_newmontage:{
         THD_ivec3 *minf = (THD_ivec3 *) cbs->userdata ;
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
         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING ){
            AFNI_view_setter(im3d,seq) ;
            AFNI_crosshair_gap_CB( NULL , (XtPointer) im3d ) ;
         }
      }
      break ;  /* end of arrowpad center key press */

      case isqCR_dxplus:
      case isqCR_dxminus:
      case isqCR_dyplus:
      case isqCR_dyminus:{
         THD_ivec3 ib , id ;
         XButtonEvent *xev = (XButtonEvent *) cbs->event ;
         int step = 1 ;
         THD_dataxes *daxes ;

         if( xev != NULL &&
             ( xev->type == ButtonPress ||
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

         /* 13 May 2003: allow for wraparound */

         LOAD_DSET_VIEWS(im3d) ;  /* 20 Nov 2003 */
         daxes = CURRENT_DAXES(im3d->anat_now) ;
              if( id.ijk[0] <  0          ) id.ijk[0] += daxes->nxx ;
         else if( id.ijk[0] >= daxes->nxx ) id.ijk[0] -= daxes->nxx ;
              if( id.ijk[1] <  0          ) id.ijk[1] += daxes->nyy ;
         else if( id.ijk[1] >= daxes->nyy ) id.ijk[1] -= daxes->nyy ;
              if( id.ijk[2] <  0          ) id.ijk[2] += daxes->nzz ;
         else if( id.ijk[2] >= daxes->nzz ) id.ijk[2] -= daxes->nzz ;

         if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING ){

            /* 20 Feb 2003: set plane from which viewpoint is controlled */

            AFNI_view_setter(im3d,seq) ;
            AFNI_set_viewpoint(
               im3d , id.ijk[0] , id.ijk[1] , id.ijk[2] ,
               (im3d->vinfo->crosshair_visible==True) ?
               REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
         }
      }
      break ;  /* end of arrowpad arrow press */

      case isqCR_keypress:{
#if 1
        switch( cbs->key ){  /* 05 Mar 2007: keys that AFNI needs */
                                       /* to process, not imseq.c */
          case 'U':
          case 'u':{
            int uu = im3d->vinfo->underlay_type ; /* toggle Overlay as Underlay */
            uu = (uu+1) % (LAST_UNDERLAY_TYPE+1) ;
#ifdef USE_UNDERLAY_BBOX
            MCW_set_bbox( im3d->vwid->func->underlay_bbox , 1<<uu ) ;
            AFNI_underlay_CB( im3d->vwid->func->underlay_bbox->wbut[0] , im3d , NULL ) ;
#else
            im3d->vinfo->underlay_type = uu ;
            AFNI_underlay_CB( NULL , im3d , (XtPointer)666 ) ;
            if( cbs->key == 'U' ){
              int qq ; Three_D_View *qq3d ;
              for( qq=0 ; qq < MAX_CONTROLLERS ; qq++ ){
                qq3d = GLOBAL_library.controllers[qq] ;
                if( qq3d == im3d || !IM3D_OPEN(qq3d) ) continue ;
                qq3d->vinfo->underlay_type = uu ;
                AFNI_underlay_CB( NULL , qq3d , (XtPointer)666 ) ;
              }
            }
#endif
          }
          break ;

          case 'o':{                              /* turn overlay on or off */
            int ov = MCW_val_bbox( im3d->vwid->view->see_func_bbox ) ;
            MCW_set_bbox( im3d->vwid->view->see_func_bbox , !ov ) ;
            AFNI_see_func_CB( NULL , im3d , NULL ) ;
          }
          break ;

          case '{':    /* Actually: Mod+Button4/5 = Mod+ScrollWheel */
          case '}':{   /* Change the threshold slider up or down */
            int scl ; float fff,dff,nff ;
            XmScaleGetValue( im3d->vwid->func->thr_scale , &scl ) ;
            fff = scl * im3d->vinfo->func_thresh_top * THR_factor ;
            dff = im3d->vinfo->func_thresh_top * 0.01f ;
            if( cbs->key == '{' ) dff = -dff ;
            nff = fff+dff ;
                 if( nff < 0.0f          ) nff = 0.0f ;
            else if( nff > THR_top_value ) nff = THR_top_value ;
            if( nff != fff ) AFNI_set_threshold( im3d , nff ) ;
          }
          break ;

          case 'j':   /* jump to cluster peak -- for Dale [17 Oct 2012] */
            AFNI_jumpto_clus(im3d) ;
          break ;

          case 'N':   /* jump to nearby cluster [29 Apr 2019] */
          case 'n':{                     /* (for John Butman) */
            int dci = (cbs->key == 'n') ? 1 : -1 ;
            AFNI_jumpto_clus_nearby(im3d,dci) ;
          }
          break ;

          case 'f':{  /* flash cluster [17 Oct 2012] */
            int ic ; float px,py,pz , xx,yy,zz ;
            AFNI_clu_widgets *cwid = im3d->vwid->func->cwid ;
            mri_cluster_detail *cld = im3d->vwid->func->clu_det ;
            if( cwid == NULL || cld == NULL ){ BEEPIT ; break ; }
            ic = AFNI_clus_find_xyz( im3d ,
                                     im3d->vinfo->xi , im3d->vinfo->yj , im3d->vinfo->zk ) ;
            if( ic < 0 ){ BEEPIT ; break ; }
            AFNI_clus_action_CB( cwid->clu_flsh_pb[ic] , (XtPointer)im3d , NULL ) ;
          }
          break ;

        }

#else /* OLD OLD OLD */
        MCW_grapher *grapher = VIEWER_TO_GRAPHER(im3d,seq) ;
        if( grapher != NULL ){
          char buf[2] ;
          buf[0] = cbs->key ; buf[1] = '\0' ;
          GRA_handle_keypress( grapher , buf , cbs->event ) ;
        }
#endif
      }
      break ; /* end of keyboard press */

      /*--- 20 Feb 2003: keypress while in button2 mode ---*/

      case isqCR_button2_key:{
        KeySym ks = (KeySym) cbs->key ;

        switch( ks ){
          case XK_Delete:
            AFNI_process_drawing( im3d , UNDO_MODE , 0,NULL,NULL,NULL ) ;
          break ;

          case XK_F4:
          case XK_F3:  /* 13 Sep 2008 */
            AFNI_process_drawing( im3d ,
             (ks==XK_F3) ? DECVAL_MODE : INCVAL_MODE , 0,NULL,NULL,NULL ) ;
          break ;
        }
      }
      break ; /* end of button2 mode keypress */

      /*--- Feb 1998: list of coordinates from button2 drawing ---*/

      case isqCR_button2_points:{
         int npts = cbs->key , zim = cbs->nim ;
         int *xyout = (int *) cbs->userdata ;
         THD_ivec3 id ;
         int nvec , ii , xim,yim , fixed_plane ;
         int *xdset , *ydset , *zdset ;

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
         AFNI_view_setter(im3d,seq) ;
         PLUTO_force_redisplay() ;  /* see afni_plugin.c */
         PLUTO_force_rebar() ;      /* ditto [23 Aug 1998] */
      }
      break ; /* end of forced redisplay */

      /*--- 06 Jun 2019: force opacity change ---*/

      case isqCR_opacitychange:{
        int opval = cbs->nim ;
        if( opval > 0 && opval <= 9 && opval != GLOBAL_library.opacity_setting ){
          GLOBAL_library.opacity_setting = opval ;
          PLUTO_force_opacity_change() ;
        }
      }
      break ; /* end of opacity change */

      /*--- 10 Dec 2019: zoom change ---*/

      case isqCR_zoomchange:{
        AFNI_zoompan_lock_carryout(im3d) ;
      }
      break ; /* end of opacity change */

      /*--- 26 Apr 2007: time indexing ---*/

      case isqCR_setindex:{
         MCW_arrowval *tav = im3d->vwid->imag->time_index_av ;
         MCW_arrowval *aav = im3d->vwid->func->anat_buck_av ;
         int new_index = im3d->vinfo->anat_index + cbs->key ;

         if( new_index != im3d->vinfo->anat_index ){
           if( im3d->vinfo->time_on ){
             AV_assign_ival( tav , new_index ) ;               /* set time_index */
             AFNI_time_index_CB( tav, (XtPointer) im3d ); /* will set anat_index */
           } else {
             AV_assign_ival( aav, new_index ) ;       /* set anat index directly */
             AFNI_bucket_CB( aav, im3d ) ;
           }
         }
      }
      break ;

      /*--- raise the controller [17 Jun 2011] ---*/

      case isqCR_raiseupthedead:{
        if( IM3D_OPEN(im3d) ) XMapRaised( XtDisplay(im3d->vwid->top_shell) ,
                                          XtWindow(im3d->vwid->top_shell)   ) ;
      }
      break ;

      /*--- cycle global range [03 Feb 2013] ---*/
      case isqCR_globalrange:{
          int ig;
          THD_cycle_image_globalrange();
          ig = THD_get_image_globalrange();
          THD_set_image_globalrange_env(ig);
          ENV_globalrange_view( "AFNI_IMAGE_GLOBALRANGE" );
      }
      break ;

      /*--- reset global range to use new environment value set elsewhere [03 Feb 2013] ---*/
      case isqCR_resetglobalrange:{
          int ig;
          ig = THD_get_image_globalrange();
          THD_set_image_globalrange_env(ig);
          ENV_globalrange_view( "AFNI_IMAGE_GLOBALRANGE" );
      }
      break ;

   }  /* end of switch on reason for call */

   EXRETURN ;
}

/*----------------------------------------------------------------------
   respond to events that one of the MCW_grapher's sends to us
------------------------------------------------------------------------*/

void AFNI_gra_send_CB( MCW_grapher *grapher , FD_brick *br , GRA_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)grapher->parent ;

ENTRY("AFNI_gra_send_CB") ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"reason=%d",cbs->reason) ; STATUS(str) ; }

   if( ! IM3D_VALID(im3d) ||
       (im3d->ignore_seq_callbacks==AFNI_IGNORE_EVERYTHING) ) EXRETURN ;

   switch( cbs->reason ){

      default: break ;  /* unimplemented reasons */

      /*** 17 Jun 2011 ***/

      case graCR_raiseupthedead:{
        if( IM3D_OPEN(im3d) ) XMapRaised( XtDisplay(im3d->vwid->top_shell) ,
                                          XtWindow(im3d->vwid->top_shell)   ) ;
      }
      break ;

      /*** Death ***/

      case graCR_destroy:{
         MCW_grapher *gxyz = im3d->g123 ,
                     *gyzx = im3d->g231 ,
                     *gzxy = im3d->g312  ;
         MCW_imseq *seq = GRAPHER_TO_VIEWER(im3d,grapher) ;
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
         NORMAL_cursorize(w) ;          /* 20 Jul 2005 */

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

            if( im3d->ignore_seq_callbacks == AFNI_IGNORE_NOTHING ){
               AFNI_view_setter(im3d,NULL) ;
               AFNI_set_viewpoint(
                  im3d ,
                  id.ijk[0] , id.ijk[1] , id.ijk[2] ,
                  (im3d->vinfo->crosshair_visible==True) ?
                  REDISPLAY_OVERLAY : REDISPLAY_OPTIONAL ) ;
            }
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
                                   (gen_func *)AFNI_fimmer_pickref_CB ,
                                   (XtPointer) im3d ) ;
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
                                   (gen_func *)AFNI_fimmer_pickort_CB ,
                                   (XtPointer) im3d ) ;
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

      /*** 27 Jan 2004:
           User toggled WinAver on in one graph window,
           so we toggle it off in the other windows.    ***/

      case graCR_winaver:{
        if( im3d->g123 != NULL && im3d->g123 != grapher )
          drive_MCW_grapher( im3d->g123 , graDR_winaver , 0 ) ;
        if( im3d->g231 != NULL && im3d->g231 != grapher )
          drive_MCW_grapher( im3d->g231 , graDR_winaver , 0 ) ;
        if( im3d->g312 != NULL && im3d->g312 != grapher )
          drive_MCW_grapher( im3d->g312 , graDR_winaver , 0 ) ;
      }
      break ; /* end of winaver */

      /*** 12 Nov 1996:
           User supplies a timeseries to add to the global library ***/

      case graCR_timeseries_library:{
         MRI_IMAGE *tsim = (MRI_IMAGE *) cbs->userdata ;

         AFNI_add_timeseries( tsim ) ;
      }
      break ; /* end of timeseries_library */

      /*** User supplies a timeseries for FIM (equals or add) ***/

      case graCR_refadd:
      case graCR_refequals:{
         MRI_IMAGE *tsim = (MRI_IMAGE *) cbs->userdata ;
         MRI_IMAGE *qim , *sim ;
         float *sar , *qar ;

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
            MRI_IMAGE *sim = mri_to_float(im3d->fimdata->fimref) ; /* copy */
            float *sar = MRI_FLOAT_PTR(sim) ;
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

      /*** User sets time_index (from graph) ***/
      /*** 24 Jan 2001: or bucket index      ***/
      /*** 29 Jul 2003: time_index and anat_index are almost merged now ***/

      case graCR_setindex:{
         MCW_arrowval *tav = im3d->vwid->imag->time_index_av ;
         MCW_arrowval *aav = im3d->vwid->func->anat_buck_av ;
         int new_index = cbs->key ;

         if( new_index != im3d->vinfo->anat_index ){
           if( im3d->vinfo->time_on ){
             AV_assign_ival( tav , new_index ) ;               /* set time_index */
             AFNI_time_index_CB( tav, (XtPointer) im3d ); /* will set anat_index */
           } else {
             AV_assign_ival( aav, new_index ) ;       /* set anat index directly */
             AFNI_bucket_CB( aav, im3d ) ;
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

/*----------------------------------------------------------------------*/
/*! Report on datasets with mixed type sub-bricks,
    as they tend to cause problems. */

void AFNI_inconstancy_check( Three_D_View *im3d , THD_3dim_dataset *dset )
{
   static int    nbad = 0 ;
   static char **sbad = NULL ;

ENTRY("AFNI_inconstancy_check") ;

   if( dset == NULL ){
     char *msg ; int ii,nn ; Widget wp ;
     if( nbad == 0 || sbad == NULL ) EXRETURN ; /* nothing to report */
     if( !IM3D_OPEN(im3d) ) im3d =AFNI_find_open_controller();
     wp = im3d->vwid->imag->crosshair_label ;
     BEEPIT ;
     STATUS("creating inconstancy message") ;
     for(ii=nn=0;ii<nbad;ii++) nn += strlen(sbad[ii]) ;
     nn += 255+4*nbad ; msg = malloc(nn) ;
     sprintf(msg,
            "\n========== Datasets With Inconstant Data Types =========\n\n");
     for(ii=0;ii<nbad;ii++) sprintf(msg+strlen(msg)," %s\n",sbad[ii]) ;
     sprintf(msg+strlen(msg),
            "\n===== This is known as the Mike Beauchamp SINdrome =====\n"
              "===== Funky things may happen with these datasets! =====\n" ) ;
     MCW_textwin_setbig(1) ;
     (void)new_MCW_textwin( wp , msg , TEXT_READONLY ) ;
     free((void *)msg) ;
     for(ii=0;ii<nbad;ii++)free((void *)sbad[ii]);
     free((void *)sbad) ; nbad=0 ; sbad=NULL ;
     BEEPIT ;
     EXRETURN ;
   } else if( ISVALID_DSET(dset) && !DSET_datum_constant(dset) ){
     char *str = DSET_BRIKNAME(dset) ;
     sbad = (char **)realloc((void *)sbad,sizeof(char *)*(nbad+1)) ;
     sbad[nbad++] = strdup(str) ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------
   read the files specified on the command line and create the data structures
------------------------------------------------------------------------------*/

void AFNI_read_inputs( int argc , char *argv[] )
{
   int id , last_color ;
   RwcBoolean isfunc ;


ENTRY("AFNI_read_inputs") ;

   /* create empty library of dataset sessions */

   GLOBAL_library.sslist = myXtNew( THD_sessionlist ) ;
   GLOBAL_library.sslist->type = SESSIONLIST_TYPE ;
   BLANK_SESSIONLIST(GLOBAL_library.sslist) ;
   GLOBAL_library.sslist->parent = NULL ;
   GLOBAL_argopt.only_images = 0 ;  /* 24 Feb 2017 */

   /*----- read files -----*/

   if( GLOBAL_argopt.first_file_arg >= argc && GLOBAL_argopt.read_images ){
      ERROR_exit("No image files on command line!!") ;
   }

   /*--- read directly from images (the old-fashioned way) ---*/

   if( GLOBAL_argopt.read_images ){  /* pretty much obsolete */
      THD_3dim_dataset *dset ;
      THD_session *new_ss ;
      int vv ;
      int gnim ;  /* 16 Mar 1998: names from globbing */
      char **gname ;

      MCW_warn_expand(1) ;  /* 13 Jul 2001 */

      MCW_file_expand( argc - GLOBAL_argopt.first_file_arg ,
                       &(argv[GLOBAL_argopt.first_file_arg]) ,
                       &gnim , &gname ) ;

      MCW_warn_expand(0) ;  /* 13 Jul 2001 */

      if( gnim < 1 )
        ERROR_exit("No valid filenames on command line?!") ;

      dset = AFNI_read_images( gnim , gname ) ;

      if( dset == NULL )
        ERROR_exit("Could not form 3D dataset from images!" ) ;

      MCW_free_expand( gnim , gname ) ;

      /* set up minuscule session and session list */

      new_ss              = myXtNew( THD_session ) ;
      new_ss->type        = SESSION_TYPE ;
      new_ss->dsrow       = NULL;  /* row of datasets across spaces not defined yet */
      BLANK_SESSION(new_ss) ;
      new_ss->num_dsset   = 1 ;
      SET_SESSION_DSET(dset, new_ss, 0,0);
/*      new_ss->dsset_xform_table[0][0] = dset ;*/
      new_ss->parent      = NULL ;

      MCW_strncpy( new_ss->sessname ,
                   argv[GLOBAL_argopt.first_file_arg] , THD_MAX_NAME ) ;
      MCW_strncpy( new_ss->lastname ,
                   argv[GLOBAL_argopt.first_file_arg] , THD_MAX_NAME ) ;

      GLOBAL_library.sslist->num_sess   = 1 ;
      GLOBAL_library.sslist->ssar[0]    = new_ss ;
      GLOBAL_library.have_dummy_dataset = 1 ;

   } /** end of images input **/

   /*--- sessions of 3D datasets (from to3d or other AFNI programs) ---*/

   else if( GLOBAL_argopt.read_sessions ){   /*--- the usual method ---*/

      char str[256] ;
      RwcBoolean good ;
      int num_ss , qd , qs , vv=0 , no_args , jj , nskip_noanat=0 ;
      THD_string_array *flist , *dlist=NULL , *elist=NULL , *qlist ;
      char *dname , *eee ;
      THD_session *new_ss ;
      THD_session **new_ssar=NULL ; int num_ssar , qss ;
      int num_dsets=0 ;       /* 04 Jan 2000 */
      THD_session *gss=NULL ; /* 11 May 2002: global session */
      THD_session *dss=NULL ; /* 28 Aug 2003: session for command-line datasets */
      THD_3dim_dataset *temp_dset=NULL; /* 16 Jul 2010: place holder dummy datasets */

      THD_session *css=NULL ; /* 02 Jun 2016: catenated sessions */
      int       do_css=GLOBAL_argopt.cat_sess ;
      int      num_css=0 ;

      /*-- 20 Dec 2001: Try to read a "global" session --*/
      /*-- 11 May 2002: Move read global session up here --*/

      eee = getenv( "AFNI_GLOBAL_SESSION" ) ;   /* where it's supposed to be */
      if( eee != NULL ){
         THD_3dim_dataset *dset ;
         STATUS("reading global session") ;
         gss =
          GLOBAL_library.session = THD_init_session( eee ); /* try to read datasets */

         if( gss != NULL ){                               /* got at least one */
            REPORT_PROGRESS("[Global]") ;
            gss->parent = NULL ;                          /* parentize them */
            for( qd=0 ; qd < gss->num_dsset ; qd++ )
              for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
                dset = GET_SESSION_DSET(gss, qd, vv) ;
/*                dset = gss->dsset_xform_table[qd][vv] ;*/
                if( dset != NULL ){
                  PARENTIZE( dset , NULL ) ;
                  DSET_MARK_FOR_IMMORTALITY( dset ) ;
                  AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
                }
              }
         } else {
           sprintf(str,"\n** No datasets in AFNI_GLOBAL_SESSION=%s",eee) ;
           REPORT_PROGRESS(str) ;
         }
      }

      /* 28 Aug 2003:
         set up session for datasets from command line (vs. directories) */

      dss         = myXtNew( THD_session ) ;
      dss->type   = SESSION_TYPE ;
      dss->parent = NULL ;
      dss->ndsets = 0;
      dss->dsrow  = NULL;
      BLANK_SESSION(dss) ;
      MCW_strncpy( dss->sessname , "fromCLI" , THD_MAX_NAME ) ;
      MCW_strncpy( dss->lastname , "fromCLI" , THD_MAX_NAME ) ;

      if( do_css ){  /* catenated sessions [02 Jun 2016] */
        css         = myXtNew( THD_session ) ;
        css->type   = SESSION_TYPE ;
        css->parent = NULL ;
        css->ndsets = 0;
        css->dsrow  = NULL;
        BLANK_SESSION(css) ;
        css->is_collection = 1 ; /* 01 Feb 2018 */
        MCW_strncpy( css->sessname , "All_Datasets" , THD_MAX_NAME ) ;
        MCW_strncpy( css->lastname , "All_Datasets" , THD_MAX_NAME ) ;
      }

      /* now get the list of strings to read as directories */

      num_ss  = argc - GLOBAL_argopt.first_file_arg ;
      no_args = (num_ss < 1) ;

      INIT_SARR(dlist) ;

      /** 15 Apr 2018: put in any bysub dirs **/

      for( id=0; id < num_bysub_dir; id++ ){ ADDTO_SARR(dlist,bysub_dir[id]); }

      if( no_args && num_bysub_dir <= 0 ){
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
           elist = THD_get_all_subdirs( 1 , "./" ) ;  /* 18 Feb 2007 */
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

      if( dlist->num < 1 ) ADDTO_SARR(dlist,"./") ;  /* just in case */

      /** 09 Sep 1998: eliminate duplicates from the directory list **/

STATUS("normalizing directory list") ;
      qlist = THD_normalize_flist( dlist ) ;
      if( qlist != NULL ){ DESTROY_SARR(dlist); dlist = qlist; }

      if( dlist->num == 1 && css != NULL && gss == NULL ){ /* 02 Jun 2016 */
        myXtFree(css) ; do_css = 0 ; num_css = 0 ;
      }

      REFRESH ;

      /*----- read each session, set parents, put into session list -----*/

      GLOBAL_argopt.only_images = 1 ;  /* 24 Feb 2017 */

      qlist = dlist ;
   RESTART_DIRECTORY_SCAN:   /* 18 Feb 2007 */
      num_ss = qlist->num ;

      for( id=0 ; id < num_ss ; id++ ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"try to read directory %s",qlist->ar[id]) ; STATUS(str) ; }

         dname  = qlist->ar[id] ;
         new_ss = NULL ;

         if( new_ssar != NULL ) free(new_ssar) ; /* 01 Feb 2018 */
         new_ssar = NULL ; num_ssar = 0 ;

         if( THD_is_directory(dname) ){   /* directory? read session(s) */
           static int first=1 ;

           for( qss=0 ; qss < num_bysub ; qss++ ){   /* bysub [01 Feb 2018] */
             if( first ){ fprintf(stderr,"[bysub/BIDS]"); first = 0; }
             new_ss = THD_init_session_bysub(dname,bysub[qss]) ;
             if( new_ss != NULL ){
               new_ssar = (THD_session **)realloc( new_ssar ,
                                                   sizeof(THD_session *)*(num_ssar+1) ) ;
               new_ssar[num_ssar++] = new_ss ;
               fprintf(stderr,"+%s",bysub[qss]) ;
             }
           }
           if( num_ssar == 0 ){                  /* bysub did nothing, try again */
             new_ss = THD_init_session(dname) ;
             if( new_ss != NULL ){
               new_ssar = (THD_session **)malloc(sizeof(THD_session *)) ;
               new_ssar[0] = new_ss ; num_ssar = 1 ;
               fprintf(stderr,"+%s",dname) ;
             }
           }
         }

         REFRESH ;

         if( new_ssar == NULL && !THD_is_directory(dname) ){ /* 28 Aug 2003 */
           STATUS("trying to read it as a dataset file") ;
           qd = dss->num_dsset ;
           if( qd < THD_MAX_SESSION_SIZE ){
             THD_3dim_dataset *dset = THD_open_dataset( dname ) ;
             if( ISVALID_DSET(dset) ){
               STATUS("it IS a dataset file!") ;
               SET_SESSION_DSET(dset, dss, qd, dset->view_type);
/*               dss->dsset_xform_table[qd][dset->view_type] = dset ;*/
               dss->num_dsset ++ ;
               if( dset->dblk->diskptr->storage_mode != STORAGE_BY_IMAGE_FILE )
                 GLOBAL_argopt.only_images = 0 ;  /* 24 Feb 2017 */
               AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
             } else if( qlist == dlist ){
               fprintf(stderr,
                       "\n** Couldn't open %s as session OR as dataset!" ,
                       dname ) ;
             }
           }
         }

         if( new_ssar != NULL && num_ssar > 0 ){ /* got something? */
           THD_3dim_dataset *dset ;

           GLOBAL_argopt.only_images = 0 ;  /* 24 Feb 2017 */

           /* set parent pointers */

           for( qss=0 ; qss < num_ssar ; qss++ ){  /* 01 Feb 2018 */
             new_ss = new_ssar[qss] ;
             new_ss->parent = NULL ;
             for( qd=0 ; qd < new_ss->num_dsset ; qd++ ){
               for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
                   dset = GET_SESSION_DSET(new_ss, qd, vv);
/*                 dset = new_ss->dsset_xform_table[qd][vv] ;*/
                 if( dset != NULL ){
                   PARENTIZE( dset , NULL ) ;
                   AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
                 }
             } }

             /* put the new session into place in the list of sessions */

             GLOBAL_library.sslist->ssar[(GLOBAL_library.sslist->num_sess)++] = new_ss ;
             if( qlist == elist ) recursed_ondot++ ;  /* 18 Feb 2007 */

             sprintf(str,"\n session #%3d  = %s ==> %d dataset%s" ,
                     GLOBAL_library.sslist->num_sess ,
                     new_ss->lastname , new_ss->num_dsset ,
                     (new_ss->num_dsset > 1) ? "s" : " " ) ;
             REPORT_PROGRESS(str) ;

             num_dsets += new_ss->num_dsset ;

             /* 28 Aug 2002: add any inter-dataset warps to global warptable */

             if( new_ss->warptable != NULL ){
               if( GLOBAL_library.warptable == NULL ) /* create global warptable */
                 GLOBAL_library.warptable = new_Htable(101) ;
               subsume_Htable( new_ss->warptable , GLOBAL_library.warptable ) ;
               destroy_Htable( new_ss->warptable ) ;
               new_ss->warptable = NULL ;
             }

             /* 02 Jun 2016: catenate this session with the css (all datasets) */

             if( do_css )
               THD_append_sessions( css , new_ss ) ; num_css++ ;

             /* 11 May 2002: put global datasets into session now */

             if( new_ss != NULL && gss != NULL )
               THD_append_sessions( new_ss , gss ) ;

             /* if we've maxed out on sessions */

             if( GLOBAL_library.sslist->num_sess == THD_MAX_NUM_SESSION ){
               sprintf(str,"\n *** reached max no. sessions (%d) ***",
                     THD_MAX_NUM_SESSION) ;
               REPORT_PROGRESS(str) ;
               goto END_OF_ID_LOOP ; /* exit loop over id [bad news, baby] */
             }
           } /* end of loop over array of new sessions */

         } else if( num_bysub == 0 ){   /* 18 Feb 2007: do -R2 on "./" if no data found */
           if( qlist == dlist && elist != NULL ){
             fprintf(stderr,"\n** Searching subdirectories of './' for data") ;
             qlist = elist; goto RESTART_DIRECTORY_SCAN;
           }
         }

      }  /*----- end of id loop (over input directory names) -----*/

END_OF_ID_LOOP:  /* for the bad news above [01 Feb 2018] */

      /* 28 Aug 2003: if have datasets in session dss, use it */

      if( dss->num_dsset > 0 ){
        if( GLOBAL_library.sslist->num_sess < THD_MAX_NUM_SESSION ){
          if( do_css ){ /* 02 Jun 2016 */
            THD_append_sessions(css,dss); num_css++;
          }
          GLOBAL_library.sslist->ssar[(GLOBAL_library.sslist->num_sess)++] = dss ;
          num_dsets += dss->num_dsset ;
          sprintf(str,"\n session #%3d  = %s ==> %d dataset%s" ,
                  GLOBAL_library.sslist->num_sess, dss->sessname, dss->num_dsset,
                  (dss->num_dsset > 1) ? "s" : " " ) ;
          REPORT_PROGRESS(str) ;
          if( gss != NULL ) THD_append_sessions( dss , gss ) ;
        } else {
          fprintf(stderr,"\n** Can't use command line datasets: session overflow!\n") ;
          free(dss) ;
        }
      } else {
        free(dss) ;
      }

      if( gss != NULL && do_css ){
        THD_append_sessions(css,gss); num_css++;
      }

      /* 11 May 2002: if have global session but no others, use it */

      if( gss != NULL && GLOBAL_library.sslist->num_sess == 0 ){

        GLOBAL_library.sslist->ssar[(GLOBAL_library.sslist->num_sess)++] = gss ;

        sprintf(str,"\n AFNI_GLOBAL_SESSION = %s %d datasets" ,
                gss->sessname , gss->num_dsset ) ;

        num_dsets += gss->num_dsset ;

        REPORT_PROGRESS(str) ;
      }

      /* add the catenated session list, if nontrivial */
      /* only if do_css             7 Mar 2018 [rickr] */

      if( css && do_css ) {
         if( num_css > 1 && GLOBAL_library.sslist->num_sess < THD_MAX_NUM_SESSION ){
           GLOBAL_library.sslist->ssar[(GLOBAL_library.sslist->num_sess)++] = css ;
           sprintf(str,"\n Catenated %d sessions = %s has %d datasets" ,
                   num_css , css->sessname , css->num_dsset ) ;
           REPORT_PROGRESS(str) ;
         } else {
           myXtFree(css) ;
         }
      }

      /** if nothing read yet, try some canonical files [22 Oct 2019] **/

      if( GLOBAL_library.sslist->num_sess <= 0 ){
      }

      /**** if nothing read at all, make up something ****/

      GLOBAL_library.have_dummy_dataset = 0 ;

      if( GLOBAL_library.sslist->num_sess <= 0 ){
#define NCANON 3  /* added 22 Oct 2019 */
         static char *cds[NCANON] = { "MNI152_2009_template_SSW.nii.gz" ,
                                      "MNI_N27.nii.gz" ,
                                      "TT_N27_SSW.nii.gz" } ;

         char *aaa = THD_find_executable( "afni" ) ;
         char *snam = dlist->ar[0] ; /* 10 Mar 2002 */
         char *cpt ; int found_canon=0 ;

         temp_dset = NULL ;               /* this will be the made up dataset */

         if( aaa != NULL && *aaa != '\0' ){  /* try to find canonical dataset */
           char *ddd ; int lll , ccc ;      /* from afni executable directory */
           lll = strlen(aaa) ;
           ddd = malloc(sizeof(char)*(lll+64)) ;
           strcpy(ddd,aaa) ;
           for( ccc=0 ; ccc < NCANON ; ccc++ ){
             strcpy( ddd+(lll-4) , cds[ccc] ) ;   /* cut off the last 4 chars */
             temp_dset = THD_open_dataset( ddd ) ;
             if( temp_dset != NULL ) break ;
           }
           found_canon = (temp_dset != NULL) ;   /* found a canonical dataset */
         }

         if( !THD_is_directory(snam) ) snam = "./" ;

         REPORT_PROGRESS("\n** No datasets found -- making up something **") ;

         /** manufacture a minimal session **/

         new_ss         = myXtNew( THD_session ) ;
         new_ss->type   = SESSION_TYPE ;
         new_ss->parent = NULL ;
         BLANK_SESSION(new_ss) ;
         MCW_strncpy( new_ss->sessname, snam, THD_MAX_NAME ); /* pretend dummy session */
         MCW_strncpy( new_ss->lastname, snam, THD_MAX_NAME ); /* is first argv dir */
         GLOBAL_library.sslist->num_sess   = 1 ;
         GLOBAL_library.sslist->ssar[0]    = new_ss ;

         /** if nothing yet, make up  a minimal dataset [cf. thd_dumdset.c] **/

         new_ss->num_dsset = 1 ;
         GLOBAL_argopt.only_images = 0 ;  /* 24 Feb 2017 */

         cpt = getenv("AFNI_DUMMY_DATASET") ; /* specify type of dummy dset */

         if( temp_dset == NULL && cpt != NULL &&
             ( strcasecmp(cpt,"RWCOX")==0 || strcasecmp(cpt,"OLD")==0 ) ){
           temp_dset = THD_dummy_RWCOX();
           SET_SESSION_DSET(temp_dset,new_ss, 0, 0); /* the olden way */
           GLOBAL_library.have_dummy_dataset = 1 ;

         } else if( temp_dset == NULL ){
           temp_dset = THD_dummy_N27();
           SET_SESSION_DSET(temp_dset,new_ss, 0, 0);   /* 12 Feb 2010 */
           SET_SESSION_DSET(temp_dset,new_ss, 0, 2);
           GLOBAL_library.have_dummy_dataset = 1 ;
         } else {              /* use canonical dataset [22 Oct 2019] */
           SET_SESSION_DSET(temp_dset,new_ss, 0, 0);
           SET_SESSION_DSET(temp_dset,new_ss, 0, 2);
         }

         if( !found_canon ) DSET_lock(temp_dset) ; /* lock into memory */
         PARENTIZE( temp_dset , NULL ) ;

      } else {  /* 04 Jan 2000: show total number of datasets */

         if( GLOBAL_argopt.only_images ) /* 24 Feb 2017 */
           sprintf(str,"\n image count   = %d" , num_dsets ) ;
         else
           sprintf(str,"\n dataset count = %d" , num_dsets ) ;
         GLOBAL_num_dsets = num_dsets ;
         REPORT_PROGRESS(str) ;
      }

      /*** read all timeseries files from all directories ***/

STATUS("reading timeseries files") ;

      /* 27 Jan 2000: allow skipping *.1D files from dataset directories */
      /* 10 Feb 2016: broke sometime - allow skipping */
      if(GLOBAL_argopt.read_1D)
         GLOBAL_library.timeseries = THD_get_many_timeseries(qlist);
      else
         GLOBAL_library.timeseries = NULL;
      REFRESH ;
      if( GLOBAL_library.timeseries == NULL )     /* empty but not NULL */
         INIT_IMARR(GLOBAL_library.timeseries) ;
      sprintf( str , "\n Timeseries.1D = %d files read" ,
               IMARR_COUNT(GLOBAL_library.timeseries) ) ;
      REPORT_PROGRESS(str) ;

      /* 16 Jun 2020: same stuff for *.tsv and *.csv files */
      if(GLOBAL_argopt.read_tcsv)
         GLOBAL_library.tcsv_data = THD_get_many_tcsv(qlist);
      else
         GLOBAL_library.tcsv_data = NULL;
      REFRESH ;
      if( GLOBAL_library.tcsv_data == NULL )     /* empty but not NULL */
         INIT_ELARR(GLOBAL_library.tcsv_data) ;
      sprintf( str , "\n .[tc]sv data  = %d files read" ,
               ELARR_COUNT(GLOBAL_library.tcsv_data) ) ;
      REPORT_PROGRESS(str) ;

      /*** throw away the list of directories that were scanned ***/

      DESTROY_SARR(dlist) ;
      if( elist != NULL ){ DESTROY_SARR(elist); }  /* 18 Feb 2007 */

      /* assign the warp and anatomy parent pointers;
         then, make any datasets that don't exist but logically
         descend from the warp and anatomy parents just assigned */

      if( !GLOBAL_library.have_dummy_dataset ){

        STATUS("checking idcodes for duplicates") ;
        THD_check_idcodes( GLOBAL_library.sslist ) ;     /* 08 Jun 1999 */

        STATUS("reconciling parent pointers") ;
        THD_reconcile_parents( GLOBAL_library.sslist ) ;

        STATUS("forcible adoption of unparented datasets") ;
        for( id=0 ; id < GLOBAL_library.sslist->num_sess ; id++ ){ /* functions w/o parents, */
          new_ss = GLOBAL_library.sslist->ssar[id] ;               /* forcibly get one */
          AFNI_force_adoption( new_ss , GLOBAL_argopt.warp_4D ) ;
       }

      } /* end of if don't have dummy dataset */

      if( GLOBAL_library.session != NULL )
         AFNI_force_adoption( GLOBAL_library.session , GLOBAL_argopt.warp_4D ) ;

STATUS("making descendant datasets") ;

      AFNI_make_descendants( GLOBAL_library.sslist ) ;

   } /** end of sessions input **/

   else if( GLOBAL_argopt.read_dsets ){  /* 17 Mar 2000 */

      int nds = argc - GLOBAL_argopt.first_file_arg ;
      char str[256] ;
      THD_3dim_dataset *dset ;
      RwcPointer_array *dsar ;
      MRI_IMARR *webtsar ;        /* 26 Mar 2001 */
      THD_session *new_ss ;
      int ii,nerr=0,vv,nn , dd ;

      if( nds <= 0 ){
         fprintf(stderr,"\a\n** No datasets on command line?!\n"); exit(1);
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
              fprintf(stderr,"\a\n** Can't read datasets from %s\n",argv[ii]) ;
              nerr++ ; continue ; /* next ii */
            }

         } else { /** read from one file (local or Web), make a small array **/

            dset = THD_open_dataset( argv[ii] ) ;
            if( dset == NULL ){
               fprintf(stderr,"\a\n** Can't read dataset %s\n",argv[ii]) ;
               nerr++ ; continue ; /* next ii */
            }
            INIT_XTARR(dsar) ; ADDTO_XTARR(dsar,dset) ;
            XTARR_IC(dsar,0) = IC_DSET ;
         }

         for( dd=0 ; dd < dsar->num ; dd++ ){  /* over all entries in array */

            /* 26 Mar 2001: might get some 1D files, too */

            if( XTARR_IC(dsar,dd) == IC_FLIM ){  /* save 1D file for later */
               MRI_IMAGE *im = (MRI_IMAGE *) XTARR_XT(dsar,dd) ;
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
            AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
            nds++ ;   /* increment count of dataset */
            REFRESH ;
            vv = dset->view_type ;
            nn = new_ss->num_dsset ;
            if( nn >= THD_MAX_SESSION_SIZE ){
              fprintf(stderr,"\a\n** too many datasets!\n") ;
              nerr++ ;
            } else {
              SET_SESSION_DSET(dset, new_ss, nn, vv);
/*              new_ss->dsset_xform_table[nn][vv] = dset ; */
              new_ss->num_dsset ++ ;
            }
         } /* end of loop over dd=datasets in dsar */

         FREE_XTARR(dsar) ;  /* don't need array no more */

      } /* end of loop over ii=command line arguments past options */

      if( nerr > 0 ){
        fprintf(stderr,"** FATAL ERRORS on input\n") ; exit(1) ;  /* bad */
      }

      sprintf(str,"\n dataset count = %d" , nds ) ;
      if( new_ss->num_dsset == 0 ){
        fprintf(stderr,"\n** No datasets read from the list!\n") ;
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

      sprintf( str , "\n Timeseries.1D = %d files read" ,
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

     fprintf(stderr,"\a\n** Illegal Usage configuration detected!\n"); exit(1);
   }

   /** done at last **/

   MPROBE ; EXRETURN ;
}

/*--------------------------------------------------------------------------
  Final adjustments before a controller is opened for use - 15 Jun 2000
----------------------------------------------------------------------------*/

void AFNI_startup_3dview( Three_D_View *im3d )
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
                           (str_func *)ISQ_transform_label ,   /* text func  */
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
                           (str_func *)ISQ_transform_label ,   /* text func  */
                           &(GLOBAL_library.registered_2D)     /* text data  */
                        ) ;
      XtManageChild( im3d->vwid->func->pbar_transform2D_av->wrowcol ) ;
   } else {
      if( old_2D_num == 0 )
        XtUnmanageChild( im3d->vwid->func->pbar_transform2D_av->wrowcol ) ;
   }

   im3d->vwid->func->pbar_transform2D_index = 0 ;
   im3d->vwid->func->pbar_transform2D_func  = NULL ;

   /* 08 Apr 2005: the cursor on the pbar? */

   if( im3d->vwid->func->inten_pbar->bigmode )
     POPUP_cursorize( im3d->vwid->func->inten_pbar->panew ) ;

   POPUP_cursorize( im3d->vwid->view->sess_lab ) ; /* 30 Apr 2010 */

   /* Hey Rocky!  Watch me pull a rabbit out of my hat! */

   EXRETURN ;
}

/*--------------------------------------------------------------------------
   delete the viewers associated with this controller panel
---------------------------------------------------------------------------*/

void AFNI_closedown_3dview( Three_D_View *im3d )
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

   DESTROY_FD_BRICK(im3d->b123_anat) ;
   DESTROY_FD_BRICK(im3d->b231_anat) ;
   DESTROY_FD_BRICK(im3d->b312_anat) ;

   DESTROY_FD_BRICK(im3d->b123_fim)  ;
   DESTROY_FD_BRICK(im3d->b231_fim)  ;
   DESTROY_FD_BRICK(im3d->b312_fim)  ;

   im3d->b123_ulay = im3d->b231_ulay = im3d->b312_ulay = NULL ;

   if( XtIsManaged(im3d->vwid->view->frame) == True )
     AFNI_controller_panel_CB( NULL , im3d , NULL ) ;

   /* null out montage info */

   LOAD_IVEC3(im3d->vinfo->xhairs_ndown,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nup  ,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nskip,0,0,0) ;

   /* de-fim */

   AFNI_fimmer_setref(im3d,NULL) ; CLEAR_FIMDATA(im3d) ;

   /* Jul 2010 */

   CLU_free_table(im3d->vwid->func->clu_tabNN1_1sid); im3d->vwid->func->clu_tabNN1_1sid = NULL;
   CLU_free_table(im3d->vwid->func->clu_tabNN2_1sid); im3d->vwid->func->clu_tabNN2_1sid = NULL;
   CLU_free_table(im3d->vwid->func->clu_tabNN3_1sid); im3d->vwid->func->clu_tabNN3_1sid = NULL;

   CLU_free_table(im3d->vwid->func->clu_tabNN1_2sid); im3d->vwid->func->clu_tabNN1_2sid = NULL;
   CLU_free_table(im3d->vwid->func->clu_tabNN2_2sid); im3d->vwid->func->clu_tabNN2_2sid = NULL;
   CLU_free_table(im3d->vwid->func->clu_tabNN3_2sid); im3d->vwid->func->clu_tabNN3_2sid = NULL;

   CLU_free_table(im3d->vwid->func->clu_tabNN1_bsid); im3d->vwid->func->clu_tabNN1_bsid = NULL;
   CLU_free_table(im3d->vwid->func->clu_tabNN2_bsid); im3d->vwid->func->clu_tabNN2_bsid = NULL;
   CLU_free_table(im3d->vwid->func->clu_tabNN3_bsid); im3d->vwid->func->clu_tabNN3_bsid = NULL;

   if( im3d->vwid->func->clu_mask != NULL ){
     free(im3d->vwid->func->clu_mask) ; im3d->vwid->func->clu_mask = NULL ;
   }

   RESET_AFNI_QUIT(im3d) ;

   im3d->anat_now = im3d->fim_now = NULL ;
   AFNI_SEE_FUNC_OFF(im3d) ;   /* 22 May 2009 */

   AFNI_purge_unused_dsets() ;

   /* 19 Aug 2002: close surface widgets, too */

   if( im3d->vwid->view->swid != NULL )
     XtUnmapWidget( im3d->vwid->view->swid->wtop ) ;

   im3d->vinfo->fix_qval   = 0 ;    /* 27 Feb 2014 */
   im3d->vinfo->fixed_qval = 0.0f ;
   im3d->vinfo->fix_pval   = 0 ;    /* 27 Feb 2014 */
   im3d->vinfo->fixed_pval = 0.0f ;

   MPROBE ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------
  Open or close the viewing controls panel
---------------------------------------------------------------------------*/

void AFNI_controller_panel_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

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
      XtUnmanageChild(im3d->vwid->forum_pb) ;
      XtUnmanageChild(im3d->vwid->phelp_pb) ;
      XtUnmanageChild(im3d->vwid->ytube_pb) ;

      SHIFT_TIPS( im3d , TIPS_TOTAL_SHIFT ) ;
      SHIFT_NEWS( im3d , TIPS_TOTAL_SHIFT ) ;

   } else {  /** open the view frame (but not its children) **/

      XtManageChild(im3d->vwid->view->frame) ;
      if( ! im3d->vwid->prog->panel_pb_inverted ){
         MCW_invert_widget(im3d->vwid->prog->panel_pb) ;
         im3d->vwid->prog->panel_pb_inverted = True ;
      }
      XtManageChild(im3d->vwid->forum_pb) ;
      XtManageChild(im3d->vwid->phelp_pb) ;
      XtManageChild(im3d->vwid->ytube_pb) ;


      SHIFT_TIPS( im3d , (im3d->vwid->view->marks_enabled) ? TIPS_MINUS_SHIFT
                                                           : TIPS_PLUS_SHIFT ) ;
      SHIFT_NEWS( im3d , (im3d->vwid->view->marks_enabled) ? TIPS_MINUS_SHIFT
                                                           : TIPS_PLUS_SHIFT ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------
  Called when the user selects a new option for crosshair visibility
---------------------------------------------------------------------------*/

void AFNI_crosshair_visible_CB( MCW_arrowval *av , XtPointer client_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
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
   Three_D_View *im3d = (Three_D_View *) client_data ;
   int bval ;

ENTRY("AFNI_wrap_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bval = MCW_val_bbox( im3d->vwid->imag->wrap_bbox ) ;

   if( (RwcBoolean) bval == im3d->vinfo->xhairs_periodic ) EXRETURN ;

   im3d->vinfo->xhairs_periodic = (RwcBoolean) bval ;

   if( w != NULL ){
      drive_MCW_imseq( im3d->s123, isqDR_periodicmont, (XtPointer)ITOP(bval) );
      drive_MCW_imseq( im3d->s231, isqDR_periodicmont, (XtPointer)ITOP(bval) );
      drive_MCW_imseq( im3d->s312, isqDR_periodicmont, (XtPointer)ITOP(bval) );
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_xhall_bbox_CB( Widget w ,
                         XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   int bval ;

ENTRY("AFNI_xhall_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bval = MCW_val_bbox( im3d->vwid->imag->xhall_bbox ) ;

   if( (RwcBoolean) bval == im3d->vinfo->xhairs_all ) EXRETURN ;

   im3d->vinfo->xhairs_all = (RwcBoolean) bval ;

   if( im3d->vinfo->crosshair_visible ){
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_crosshair_color_CB( MCW_arrowval *av , XtPointer client_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
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

void AFNI_crosshair_gap_CB( MCW_arrowval *av ,  XtPointer client_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
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

/*------------------------------------------------------------------------*/

void AFNI_time_index_set_fstep( Three_D_View *im3d , int istep )
{
   MCW_arrowval *av ; char lll[16] ;

ENTRY("AFNI_time_index_set_fstep") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( istep < 1 ) istep = 1 ; else if( istep > 9 ) istep = 9 ;

   av = im3d->vwid->imag->time_index_av ;

   if( istep == 1 ) strcpy(lll, "Index ") ;
   else             sprintf(lll,"Idx[%d]",istep) ;

   MCW_set_widget_label( av->wlabel , lll ) ;
   av->fstep = (istep == 1) ? 0.0f : (float)istep ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

static char *yesno[2] = { "No" , "Yes" } ;
static char *throx[3] = { "free" , " == " , " +1 " } ;

void AFNI_time_index_step_CB( Widget w, XtPointer cd, int nval , void **val )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   char *cpt , cmd[128] ; MCW_bbox *bb ; int ib ;

ENTRY("AFNI_time_index_step_CB") ;

   if( ! IM3D_VALID(im3d) || nval != 3 || val == NULL ) EXRETURN ;

   AFNI_time_index_set_fstep( im3d , (int)(intptr_t)val[0] ) ;

   sprintf(cmd,"AFNI_SLAVE_FUNCTIME %s",(char *)val[1]) ;
   (void)AFNI_setenv(cmd) ;

   cpt = (char *)val[2] ;
   bb  = im3d->vwid->func->thr_olayx_bbox ; ib = MCW_val_bbox(bb) ;
   if( strcmp(cpt,throx[1]) == 0 ){        /* Thr = OLay */
     if( ib != 1 ) MCW_set_bbox(bb,1) ;
   } else if( strcmp(cpt,throx[2]) == 0 ){ /* Thr = Olay + 1 */
     if( ib != 2 ) MCW_set_bbox(bb,2) ;
   } else {                                /* Thr = free and wild */
     if( ib != 0 ) MCW_set_bbox(bb,0) ;
   }
   AFNI_throlayx_change_CB(NULL,im3d,NULL) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_time_index_EV( Widget w , XtPointer cd ,
                         XEvent *ev , RwcBoolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_time_index_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /*** handle events ***/

   switch( ev->type ){

     /*----- take button press -----*/

     case ButtonPress:{
       XButtonEvent *event = (XButtonEvent *)ev ;

       if( event->button == Button3 ){
         int istep = (int)im3d->vwid->imag->time_index_av->fstep ;
         int sftin , thrin ;

         if( istep < 1 ) istep = 1 ; else if( istep > 9 ) istep = 9 ;
         sftin = ( !AFNI_noenv("AFNI_SLAVE_FUNCTIME") ) ? 1 : 0 ;
         thrin = im3d->vinfo->thr_olayx ; if( thrin < 0 || thrin > 2 ) thrin = 0 ;

         MCW_choose_stuff( im3d->vwid->imag->time_index_av->wlabel ,
                             "Time Index Stepping" ,
                             (gen_func *)AFNI_time_index_step_CB , im3d ,
                             MSTUF_INT ,     "Index Step     " , 1 , 9     , istep ,
                             MSTUF_STRLIST , "SLAVE_FUNCTIME " , 2 , sftin , yesno ,
                             MSTUF_STRLIST , "Thr = Olay?+1? " , 3 , thrin , throx ,
                           MSTUF_END ) ;
       } else if( event->button == Button4 ){
         int istep = (int)im3d->vwid->imag->time_index_av->fstep ;
         if( istep < 1 ) istep = 1 ; else if( istep > 9 ) istep = 9 ;
         AFNI_time_index_set_fstep( im3d , istep-1 ) ;
       } else if( event->button == Button5 ){
         int istep = (int)im3d->vwid->imag->time_index_av->fstep ;
         if( istep < 1 ) istep = 1 ; else if( istep > 9 ) istep = 9 ;
         AFNI_time_index_set_fstep( im3d , istep+1 ) ;
       } else {
         (void) MCW_popup_message(
                   im3d->vwid->imag->time_index_av->wlabel ,
                   (event->button == Button1) ?  " \n I really wish you "
                                                 "\n wouldn't do that! \n "
                                              :  " \n   Why do you "
                                                 "\n torment me so? \n "
                 , MCW_USER_KILL | MCW_QUICK_KILL ) ;
       }
     }
     break ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_time_index_CB( MCW_arrowval *av ,  XtPointer client_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   int ipx ;

ENTRY("AFNI_time_index_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   ipx = av->ival ;
   if( ipx >= im3d->vinfo->top_index )    /* don't let index be too big */
     ipx = im3d->vinfo->top_index - 1 ;

   im3d->vinfo->time_index = ipx ;        /* change time index */

   /* 29 Jul 2003: slave underlay and overlay to time_index (maybe) */

   im3d->vinfo->anat_index = ipx ;
   if( im3d->vinfo->anat_index >= DSET_NVALS(im3d->anat_now) )
     im3d->vinfo->anat_index = DSET_NVALS(im3d->anat_now) - 1 ;
   AV_assign_ival( im3d->vwid->func->anat_buck_av , im3d->vinfo->anat_index ) ;

   if( ISVALID_DSET(im3d->fim_now)       &&
       DSET_NVALS(im3d->fim_now) > 1     &&
       !AFNI_noenv("AFNI_SLAVE_FUNCTIME")  ){

     im3d->vinfo->fim_index = ipx ;
     if( im3d->vinfo->fim_index >= DSET_NVALS(im3d->fim_now) )
       im3d->vinfo->fim_index = DSET_NVALS(im3d->fim_now) - 1 ;
     AV_assign_ival( im3d->vwid->func->fim_buck_av , im3d->vinfo->fim_index ) ;

#if 0
     if( AFNI_yesenv("AFNI_SLAVE_THRTIME") ){   /* 24 Jan 2005 - RWCox */
       im3d->vinfo->thr_index = ipx ;
       if( im3d->vinfo->thr_index >= DSET_NVALS(im3d->fim_now) )
         im3d->vinfo->thr_index = DSET_NVALS(im3d->fim_now) - 1 ;
       AV_assign_ival( im3d->vwid->func->thr_buck_av , im3d->vinfo->thr_index ) ;
     } else {
       AFNI_enforce_throlayx(im3d) ;  /* 13 Aug 2010 */
     }
#else
       AFNI_enforce_throlayx(im3d) ;
#endif
   }

   im3d->vinfo->tempflag = 1 ;
   AFNI_modify_viewing( im3d , False ) ;  /* setup new bricks to view */

   if( im3d->vinfo->fix_pval && im3d->vinfo->fixed_pval > 0.0f )
     AFNI_set_pval(im3d,im3d->vinfo->fixed_pval) ;
   else if( im3d->vinfo->fix_qval && im3d->vinfo->fixed_qval > 0.0f )
     AFNI_set_qval(im3d,im3d->vinfo->fixed_qval) ;

   if( ISVALID_DSET(im3d->fim_now)       &&   /* if time index on */
       DSET_NUM_TIMES(im3d->fim_now) > 1   )  /* function changed */
     AFNI_process_funcdisplay( im3d ) ;       /* notify receivers */

   AFNI_time_lock_carryout( im3d ) ;  /* 03 Nov 1998 */
   AFNI_process_timeindex ( im3d ) ;  /* 29 Jan 2003 */

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------*/
/* Set rinfo label in an image viewer */

void AFNI_set_rinfo_labels( Three_D_View *im3d )
{
   float angle ;
   char *label ;
   MCW_imseq   *ssss ;

   if( !IM3D_OPEN(im3d) ) return ;

   angle = THD_compute_oblique_angle(im3d->anat_now->daxes->ijk_to_dicom_real,0) ;
   label = (angle < 1.0f) ? "Card" : "Obliq" ;

   ssss = im3d->s123 ;
   if( ssss != NULL && ISQ_REALZ(ssss) )
     drive_MCW_imseq( ssss , isqDR_rinfolabel , label ) ;

   ssss = im3d->s231 ;
   if( ssss != NULL && ISQ_REALZ(ssss) )
     drive_MCW_imseq( ssss , isqDR_rinfolabel , label ) ;

   ssss = im3d->s312 ;
   if( ssss != NULL && ISQ_REALZ(ssss) )
     drive_MCW_imseq( ssss , isqDR_rinfolabel , label ) ;

   return ;
}

/*-------------------------------------------------------------------------
   Start a view (12-3, 23-1, or 31-2)
---------------------------------------------------------------------------*/

static char * AFNI_image_help =
 "Button 1      = Set crosshair location\n"
 "Button 3      = Pop up image menu\n"
 "Shift+Button2 = drag crop region\n"
 "Scrollwheel   = change slice up/down\n"
 "Alt/Opt-Scrollwheel = adjust threshold\n"
 "\n"
 "q   = close window          a = fix aspect ratio\n"
 "p   = toggle panning mode   c = crop image mode\n"
 "s   = sharpen image         m = toggle Min-to-Max\n"
 "D   = open Disp panel       M = open Montage panel\n"
 "S   = Save image            l = left-right mirror\n"
 "[   = time index down 1     ] = time index up 1\n"
 "i/I = image frac down/up  z/Z = zoom out/in\n"
 "> = Page Up   = forward  1 image (e.g., slice)\n"
 "< = Page Down = backward 1 image (e.g., slice)\n"
 "Ctrl+m = cycle through image global range settings\n"
 "o = toggle (color) overlay on/off\n"
 "u = toggle background from underlay/overlay dataset\n"
 "U = toggle background in ALL image viewers\n"
 "e = toggle edge detection in underlay\n"
 "#/3 = toggle underlay/overlay checkerboard display\n"
 "4 or 5 or 6 = slider for merging ULay/OLay images\n"
 "v/V = Video image sequence up/down\n"
 "r/R = Ricochet image sequence up/down\n"
 "Del = drawing undo       F1 = Help!\n"
 "F2  = drawing pencil     F3 = drawing value --\n"
 "F4  = drawing value ++   F5 = Meltdown!!\n"
 "Left/Right/Up/Down arrow keys\n"
 "    = move crosshairs OR pan zoomed image\n"
 "Shift+keyboard arrow keys = pan crop region around\n"
 "Ctrl+keyboard arrow keys  = expand/shrink crop region\n"
 "Shift+Home = center crop region on current crosshairs\n"
 "--- THESE NEXT KEYSTROKES OPERATE WITH CLUSTERIZE ---\n"
 "j = Jump to nearest cluster's peak (or cmass)\n"
 "f = Flash current cluster (if Xhairs inside one)\n"
 "n/N = Jump to next/previous cluster's peak/cmass"
;

static char * AFNI_arrowpad_help =
   "THD_compute_oblique_angle(dset->daxes->ijk_to_dicom_realClick arrows to scroll crosshair position\n"
   "Click button to open/close crosshair gap " ;

static char * AFNI_arrowpad_hint[] = {
  "Scroll crosshairs down" ,
  "Scroll crosshairs up" ,
  "Scroll crosshairs left" ,
  "Scroll crosshairs right" ,
  "Close/open crosshairs gap"
} ;

#undef  WID2EV
#define WID2EV(w,ev) XtVaSetValues( (w), XmNx,(int)((ev)->x_root)-24,       \
                                         XmNy,(int)((ev)->y_root)-4 , NULL )

/*.........................................................................*/

void AFNI_view_xyz_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   MCW_imseq   *sxyz , *syzx , *szxy , **snew = NULL ;
   MCW_grapher *gxyz , *gyzx , *gzxy , **gnew = NULL ;
   Widget        pboff , pb_xyz , pb_yzx , pb_zxy ;
   Widget        groff , gr_xyz , gr_yzx , gr_zxy ;
   FD_brick    *brnew ;
   int mirror=0 ;
   int m2m=0 ;     /* 04 Nov 2003 */
   int c2c=0 ;     /* 17 Sep 2007 */
   XmPushButtonCallbackStruct *cbs = (XmPushButtonCallbackStruct *)call_data ;
   XButtonEvent *event = NULL ;

ENTRY("AFNI_view_xyz_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   if( cbs != NULL ) event = (XButtonEvent *)cbs->event ;

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
       if( ISQ_REALZ(sxyz) ){
          if( AFNI_yesenv("AFNI_IMAGRA_CLOSER") )
            drive_MCW_imseq( sxyz , isqDR_destroy , NULL ) ;
          else
            XMapRaised( XtDisplay(sxyz->wtop) , XtWindow(sxyz->wtop) ) ;
       }
       EXRETURN ;
    } else if( w == pb_yzx && syzx != NULL ){
       if( ISQ_REALZ(syzx) ){
          if( AFNI_yesenv("AFNI_IMAGRA_CLOSER") )
            drive_MCW_imseq( syzx , isqDR_destroy , NULL ) ;
          else
            XMapRaised( XtDisplay(syzx->wtop) , XtWindow(syzx->wtop) ) ;
       }
       EXRETURN ;
    } else if( w == pb_zxy && szxy != NULL ){
       if( ISQ_REALZ(szxy) ){
          if( AFNI_yesenv("AFNI_IMAGRA_CLOSER") )
            drive_MCW_imseq( szxy , isqDR_destroy , NULL ) ;
          else
            XMapRaised( XtDisplay(szxy->wtop) , XtWindow(szxy->wtop) ) ;
       }
       EXRETURN ;
    } else if( w == gr_xyz && gxyz != NULL ){
       if( GRA_REALZ(gxyz) ){
          if( AFNI_yesenv("AFNI_IMAGRA_CLOSER") )
            drive_MCW_grapher( gxyz , graDR_destroy , NULL ) ;
          else
            XMapRaised( XtDisplay(gxyz->fdw_graph) , XtWindow(gxyz->fdw_graph) ) ;
       }
       EXRETURN ;
    } else if( w == gr_yzx && gyzx != NULL ){
       if( GRA_REALZ(gyzx) ){
          if( AFNI_yesenv("AFNI_IMAGRA_CLOSER") )
            drive_MCW_grapher( gyzx , graDR_destroy , NULL ) ;
          else
            XMapRaised( XtDisplay(gyzx->fdw_graph) , XtWindow(gyzx->fdw_graph) ) ;
       }
       EXRETURN ;
    } else if( w == gr_zxy && gzxy != NULL ){
       if( GRA_REALZ(gzxy) ){
          if( AFNI_yesenv("AFNI_IMAGRA_CLOSER") )
            drive_MCW_grapher( gzxy , graDR_destroy , NULL ) ;
          else
            XMapRaised( XtDisplay(gzxy->fdw_graph) , XtWindow(gzxy->fdw_graph) ) ;
       }
       EXRETURN ;
    }

    /* button pressed and window not open, so prepare to open it */

    m2m = AFNI_yesenv("AFNI_IMAGE_MINTOMAX") ;
    c2c = AFNI_yesenv("AFNI_IMAGE_CLIPPED") ;  /* 17 Sep 2007 */

    if( !IM3D_ULAY_COHERENT(im3d) ){           /* 10 Jun 2014 */
      STATUS("incoherent ulay -- patching") ;
      ERROR_message("AFNI_view_xyz_CB: incoherent ulay -- patching") ;
      AFNI_assign_ulay_bricks(im3d) ;
    }

    if( w == pb_xyz && sxyz == NULL ){         /* axial image */
       snew  = &(im3d->s123) ;
       brnew = im3d->b123_ulay ;
       pboff = pb_xyz ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else if( w == pb_yzx && syzx == NULL ){  /* sagittal image */
       snew  = &(im3d->s231) ;
       brnew = im3d->b231_ulay ;
       pboff = pb_yzx ;
       mirror= GLOBAL_argopt.left_is_posterior ;

    } else if( w == pb_zxy && szxy == NULL ){  /* coronal image */
       snew  = &(im3d->s312) ;
       brnew = im3d->b312_ulay ;
       pboff = pb_zxy ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else if( w == gr_xyz && gxyz == NULL ){  /* axial graph */
       gnew  = &(im3d->g123) ;
       brnew = im3d->b123_ulay ;
       pboff = gr_xyz ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else if( w == gr_yzx && gyzx == NULL ){  /* sagittal graph */
       gnew  = &(im3d->g231) ;
       brnew = im3d->b231_ulay ;
       pboff = gr_yzx ;
       mirror= GLOBAL_argopt.left_is_posterior ;

    } else if( w == gr_zxy && gzxy == NULL ){  /* coronal graph */
       gnew  = &(im3d->g312) ;
       brnew = im3d->b312_ulay ;
       pboff = gr_zxy ;
       mirror= GLOBAL_argopt.left_is_left ;

    } else
       EXRETURN ;  /* something funny */

    /** Mar 1997: don't open if x or y dimension is 1 **/

    if( snew != NULL && (brnew->n1 < 2 || brnew->n2 < 2) ) EXRETURN  ;

    SHOW_AFNI_PAUSE ;

    if( snew != NULL ){
STATUS("opening an image window") ;
      MCW_invert_widget(pboff) ;
      POPUP_cursorize(pboff) ;   /* 20 Jul 2005 */
      *snew = open_MCW_imseq( im3d->dc, (get_ptr)AFNI_brick_to_mri,
                              (XtPointer) brnew ) ;

      (*snew)->parent = (XtPointer)im3d ;

      INIT_BKGD_LAB(im3d) ;

      drive_MCW_imseq( *snew, isqDR_imhelptext, (XtPointer) AFNI_image_help ) ;
      drive_MCW_imseq( *snew, isqDR_arrowpadon, (XtPointer) AFNI_arrowpad_help ) ;
      drive_MCW_imseq( *snew, isqDR_arrowpadhint , (XtPointer) AFNI_arrowpad_hint );
STATUS("realizing new image viewer") ;
      drive_MCW_imseq( *snew, isqDR_ignore_redraws, (XtPointer) 1 ) ; /* 16 Aug 2002 */
      drive_MCW_imseq( *snew, isqDR_realize, NULL ) ;
      AFNI_sleep(17) ;                                                /* 17 Oct 2005 */
      drive_MCW_imseq( *snew, isqDR_title, (XtPointer) im3d->window_title ) ;
if( !AFNI_yesenv("TMONT") )
      drive_MCW_imseq( *snew, isqDR_periodicmont,
                      (XtPointer)ITOP(im3d->vinfo->xhairs_periodic) );
      drive_MCW_imseq( *snew , isqDR_allowmerger , NULL ) ;           /* 25 Aug 2014 */
      AFNI_set_rinfo_labels( im3d ) ;                                 /* 11 Mar 2020 */

      /* 09 Oct 1998: force L-R mirroring on axial and coronal images? */
      /* 04 Nov 2003: or min-to-max on grayscaling? */

      if( mirror || m2m || c2c ){
         ISQ_options opt ;

STATUS("setting image view to be L-R mirrored") ;

         ISQ_DEFAULT_OPT(opt) ;
         if( mirror ) opt.mirror = TRUE ;
         if( m2m    ) opt.scale_range = ISQ_RNG_MINTOMAX ;
         if( c2c    ) opt.scale_range = ISQ_RNG_CLIPPED ;
         drive_MCW_imseq( *snew,isqDR_options  ,(XtPointer) &opt ) ;
      }

#if 0
      /* 23 Jan 2003: set default save? */

      drive_MCW_imseq( *snew , isqDR_setimsave ,
                       (XtPointer)getenv("AFNI_DEFAULT_IMSAVE") ) ;
#endif

#ifdef USE_SIDES
#undef  LL
#undef  RR
#undef  AA
#undef  PP
#undef  SS
#undef  II
#define LL 0
#define RR 1
#define AA 2
#define PP 3
#define SS 4
#define II 5
      if( !AFNI_yesenv("AFNI_NO_SIDES_LABELS") ){
         static char *ssix[6] = { "Left"     , "Right"     ,
                                  "Anterior" , "Posterior" ,
                                  "Superior" , "Inferior"   } ;
         char *ws[4] ;

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

         /* 10 Dec 2007: label image orientation */

         if( *snew == im3d->s123 )
           drive_MCW_imseq( *snew,isqDR_winfoprefix,(XtPointer)"Axial" ) ;
         else if( *snew == im3d->s231 )
           drive_MCW_imseq( *snew,isqDR_winfoprefix,(XtPointer)"Sagittal" ) ;
         else if( *snew == im3d->s312 )
           drive_MCW_imseq( *snew,isqDR_winfoprefix,(XtPointer)"Coronal" ) ;

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

         drive_MCW_imseq( *snew, isqDR_icon , (XtPointer) pm ) ;
      }
#endif
      { int ii=AFNI_controller_index(im3d) ;
        if( ii >= 0 )
         drive_MCW_imseq( *snew, isqDR_bgicon, (XtPointer)afni16_pixmap[ii] ) ;
      }

      if( AFNI_yesenv("AFNI_OPACITY_LOCK") )                 /* 06 Jun 2019 */
        drive_MCW_imseq( *snew, isqDR_setopacity,
                         (XtPointer)ITOP(GLOBAL_library.opacity_setting) );

      drive_MCW_imseq( *snew, isqDR_ignore_redraws, (XtPointer) 0 ) ; /* 16 Aug 2002 */

      AFNI_view_setter ( im3d , *snew ) ;
      AFNI_range_setter( im3d , *snew ) ;  /* 04 Nov 2003 */
      AFNI_sleep(17) ;                     /* 17 Oct 2005 */
      if( event != NULL ) WID2EV( (*snew)->wtop , event ) ;  /* 23 Nov 2010 */

    } /* end of creating a new image viewer */

    /** Don't forget to send information like the reference timeseries ... **/

    if( gnew != NULL && DSET_GRAPHABLE(brnew->dset) ){
       MCW_grapher *gr ;

STATUS("opening a graph window") ;

       MCW_invert_widget(pboff) ;
       POPUP_cursorize(pboff) ;   /* 20 Jul 2005 */
       gr = new_MCW_grapher( im3d->dc , (get_ptr)AFNI_brick_to_mri ,
                             (XtPointer) brnew ) ;
       drive_MCW_grapher( gr, graDR_title, (XtPointer) im3d->window_title );
       drive_MCW_grapher( gr, graDR_addref_ts, (XtPointer) im3d->fimdata->fimref );
       drive_MCW_grapher( gr, graDR_setignore, (XtPointer)ITOP(im3d->fimdata->init_ignore) );
       drive_MCW_grapher( gr, graDR_polort, (XtPointer)ITOP(im3d->fimdata->polort) );
       drive_MCW_grapher( gr, graDR_setindex , (XtPointer)ITOP(im3d->vinfo->time_index) );

       if( im3d->type == AFNI_IMAGES_VIEW )
          drive_MCW_grapher( gr , graDR_fim_disable , NULL ) ; /* 19 Oct 1999 */

       if( mirror )                                            /* 12 Jul 2000 */
          drive_MCW_grapher( gr , graDR_mirror , (XtPointer) 1 ) ;

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

STATUS("realizing new grapher") ;

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

      AFNI_sleep(17) ;
      if( event != NULL ) WID2EV( gr->fdw_graph , event ) ;  /* 23 Nov 2010 */

    } /* end of creating a new graph viewer */

   /*-- force a jump to the viewpoint of the current location --*/

   XSync( im3d->dc->display , False ) ;
   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;

   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;

   MPROBE ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/* Button-3 click on an Image or Graph button:
   recall the corresponding window from offscreen purgatory.  [13 Jul 2005]
----------------------------------------------------------------------------*/

#undef  GETWIN
#define GETWIN(w)                                             \
  do{ XtVaSetValues( (w) , XmNx,(int)(event->x_root),         \
                           XmNy,(int)(event->y_root), NULL ); \
      XMapRaised( XtDisplay(w) , XtWindow(w) ) ;              \
  } while(0)

void AFNI_viewbut_EV( Widget w , XtPointer cd ,
                      XEvent *ev , RwcBoolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   XButtonEvent *event=(XButtonEvent *)ev ;

   MCW_imseq   *sxyz , *syzx , *szxy ;
   MCW_grapher *gxyz , *gyzx , *gzxy ;
   Widget      pb_xyz , pb_yzx , pb_zxy ;
   Widget      gr_xyz , gr_yzx , gr_zxy ;

ENTRY("AFNI_viewbut_EV") ;

   if( ev->type != ButtonPress || !IM3D_OPEN(im3d) ) EXRETURN ;
   if( event->button != Button3 ) EXRETURN ;

   sxyz = im3d->s123 ; gxyz = im3d->g123 ;  /* viewer structs */
   syzx = im3d->s231 ; gyzx = im3d->g231 ;
   szxy = im3d->s312 ; gzxy = im3d->g312 ;

   pb_xyz = im3d->vwid->imag->image_xyz_pb ;  /* buttons */
   pb_yzx = im3d->vwid->imag->image_yzx_pb ;
   pb_zxy = im3d->vwid->imag->image_zxy_pb ;

   gr_xyz = im3d->vwid->imag->graph_xyz_pb ;
   gr_yzx = im3d->vwid->imag->graph_yzx_pb ;
   gr_zxy = im3d->vwid->imag->graph_zxy_pb ;

   /* if the input Widget matches a button, and the item is open, get it */

        if( w == pb_xyz && ISQ_REALZ(sxyz) ) GETWIN(sxyz->wtop) ;
   else if( w == pb_yzx && ISQ_REALZ(syzx) ) GETWIN(syzx->wtop) ;
   else if( w == pb_zxy && ISQ_REALZ(szxy) ) GETWIN(szxy->wtop) ;
   else if( w == gr_xyz && GRA_REALZ(gxyz) ) GETWIN(gxyz->fdw_graph) ;
   else if( w == gr_yzx && GRA_REALZ(gyzx) ) GETWIN(gyzx->fdw_graph) ;
   else if( w == gr_zxy && GRA_REALZ(gzxy) ) GETWIN(gzxy->fdw_graph) ;

   EXRETURN ;
}

/*------------------------------------------------------------------------*/

static int ignore_redisplay_func = 0 ;  /* 03 Jun 2014 */
void AFNI_redisplay_func_ignore( int ig ){ ignore_redisplay_func = ig ; }

void AFNI_redisplay_func( Three_D_View *im3d )  /* 05 Mar 2002 */
{
ENTRY("AFNI_redisplay_func") ;
   if( !ignore_redisplay_func && IM3D_OPEN(im3d) && IM3D_IMAGIZED(im3d) ){
     AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
     AFNI_process_funcdisplay( im3d ) ;
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_redisplay_func_all( Three_D_View *im3d )  /* 03 Jul 2014 */
{
   Three_D_View *qq3d ; int ii ;

ENTRY("AFNI_redisplay_func_all") ;

   ignore_redisplay_func = 0 ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( !IM3D_OPEN(qq3d) || qq3d == im3d ) continue ;
      IM3D_CLEAR_TMASK(qq3d) ;
      IM3D_CLEAR_THRSTAT(qq3d) ;
      AFNI_redisplay_func(qq3d) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_do_bkgd_lab( Three_D_View *im3d )
{
   char str[256] ;
   char labstrf[256]={""}, labstra[256]={""};
   char strhint[256]={"Values at crosshairs voxel"};

ENTRY("AFNI_do_bkgd_lab") ;

   if( !IM3D_VALID(im3d) || !im3d->vwid->imag->do_bkgd_lab ) EXRETURN ;

   AFNI_get_dset_val_label(im3d->anat_now,         /* 26 Feb 2010 ZSS */
                           strtod(im3d->vinfo->anat_val, NULL), labstra);
   AFNI_get_dset_val_label(im3d->fim_now,         /* 26 Feb 2010 ZSS */
                           strtod(im3d->vinfo->func_val, NULL), labstrf);

#define VSTR(x) ( ((x)[0] == '\0') ? ("?") : (x) )

   sprintf(str,"ULay = %s %s\n"
               "OLay = %s %s\n"
               "Thr  = %s" ,
           VSTR(im3d->vinfo->anat_val), labstra,
           VSTR(im3d->vinfo->func_val), labstrf,
           VSTR(im3d->vinfo->thr_val ) ) ;

#undef VSTR

   if (labstra[0] != '\0' || labstrf[0] != '\0') { /* 26 Feb 2010 ZSS */
      sprintf(strhint,"Values at crosshairs: "
                      "%s%s%s%s",
                      (labstra[0] == '\0') ? "OLay=":"ULay=",
                      labstra,
                      (labstra[0] != '\0' && labstrf[0] != '\0') ? ", OLay=":"",
                      labstrf);
      MCW_register_hint( im3d->vwid->func->bkgd_lab, strhint);
   }

   MCW_set_widget_label( im3d->vwid->func->bkgd_lab , str ) ;
   XtManageChild( im3d->vwid->func->bkgd_lab ) ;
   FIX_SCALE_SIZE(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/*! Set the grayscale range in the image window, maybe.  [04 Nov 2003]
--------------------------------------------------------------------------*/

void AFNI_range_setter( Three_D_View *im3d , MCW_imseq *seq )
{
   float rng[3] = {0.0f,0.0f,0.0f} ;
   int ival ;
   FD_brick *br ;
   THD_3dim_dataset *ds ;
   static int first=1 ;
   static THD_3dim_dataset *last_ds   = NULL ;
   static int               last_ival = -1 ;
   static float             last_tc   = 0.0f ;
   float min = 0.0, max = 0.0;

ENTRY("AFNI_range_setter") ;

   if( !IM3D_VALID(im3d) || !ISQ_VALID(seq) ) EXRETURN ;

   if( !IM3D_ULAY_COHERENT(im3d) ){           /* 10 Jun 2014 */
     STATUS("AFNI_range_setter: incoherent ulay -- patching") ;
     ERROR_message("incoherent ulay -- patching") ;
     AFNI_assign_ulay_bricks(im3d) ;
   }

   br = (FD_brick *)im3d->b123_ulay ; if( br == NULL ) EXRETURN ;
   ds = br->dset ;                    if( ds == NULL ) EXRETURN ;

   if( EQUIV_DSETS(ds,im3d->anat_now) )      /* underlay dataset */
     ival = im3d->vinfo->anat_index ;
   else if( EQUIV_DSETS(ds,im3d->fim_now) )  /* overlay dataset */
     ival = im3d->vinfo->fim_index ;
   else
     ival = 0 ;                              /* shouldn't happen */

   if( !EQUIV_DSETS(ds,last_ds) || last_ival != ival ){  /* 14 Sep 2007 */
     float tc=0.0f ;
     if( DSET_INMEMORY(ds) ){
       if( !DSET_BRICK_LOADED(ds,ival) ) DSET_load(ds) ;
       if( DSET_BRICK_LOADED(ds,ival) ){
         float ff = DSET_BRICK_FACTOR(ds,ival) ;
         tc = mri_min( DSET_BRICK(ds,ival) ) ;
         if( tc < 0.0f ) tc = 0.0f ;
         else            tc = mri_topclip( DSET_BRICK(ds,ival) ) ;
         if( ff > 0.0f ) tc *= ff ;
       }
     }
     last_ds = ds ; last_ival = ival ; last_tc = tc ;
   }
   drive_MCW_imseq( seq , isqDR_settopclip , (XtPointer)(&last_tc) ) ;

   /* if default slice-based AFNI_IMAGE_GLOBALRANGE, just return */
   if( !THD_get_image_globalrange() ){ first=1; EXRETURN ; }

   if( ISVALID_STATISTIC(ds->stats)){
     if((THD_get_image_globalrange()==1)     /* user wants sub-brick range */
       && ISVALID_BSTAT(ds->stats->bstat[ival])){
        rng[0] = ds->stats->bstat[ival].min ;
        rng[1] = ds->stats->bstat[ival].max ;
     }
     else {         /* user wants range scaling by whole dataset */
       if(THD_dset_minmax(ds, 1, &min, &max)) {
        rng[0] = min ;
        rng[1] = max ;
       }
       else {  /* no good dset range, so revert to sub-brick range */
        rng[0] = ds->stats->bstat[ival].min ;
        rng[1] = ds->stats->bstat[ival].max ;
       }
     }

     /* first time globalrange has been set to something? */
     if( first ){
       INFO_message(
   "AFNI_IMAGE_GLOBALRANGE is no longer slice-based ==> reset image range to %g .. %g",
        rng[0],rng[1] ) ;
       first = 0 ;
     }
   }
   rng[2] = 1.0f ;  /* 21 Dec 2006: do NOT redisplay image */

   drive_MCW_imseq( seq , isqDR_setrange , (XtPointer) rng ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/* reset globalrange - called by environment GUI and plugout driver */
void ENV_globalrange_view( char *vname ) /* no longer static definition */
{
   Three_D_View *im3d ;
   int ii , gbr ;
   char sgr_str[64];

   /* reset image_globalrange */
   THD_set_image_globalrange(-1);
#if 0
   sprintf(sgr_str,"AFNI_IMAGE_GLOBALRANGE=%s",vname);
printf("setting env %s\n",sgr_str);
   AFNI_setenv(sgr_str);
#endif

   gbr = THD_get_image_globalrange(); /* resets from environment variable setting */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     im3d = GLOBAL_library.controllers[ii] ;
     if( ! IM3D_VALID(im3d) ) continue ;
     if( gbr ){
       AFNI_range_setter( im3d , im3d->s123 ) ;
       AFNI_range_setter( im3d , im3d->s231 ) ;
       AFNI_range_setter( im3d , im3d->s312 ) ;
       drive_MCW_imseq( im3d->s123 , isqDR_display , (XtPointer)(-1) ) ;
       drive_MCW_imseq( im3d->s231 , isqDR_display , (XtPointer)(-1) ) ;
       drive_MCW_imseq( im3d->s312 , isqDR_display , (XtPointer)(-1) ) ;
     } else {
       drive_MCW_imseq( im3d->s123 , isqDR_setrange , (XtPointer)NULL ) ;
       drive_MCW_imseq( im3d->s231 , isqDR_setrange , (XtPointer)NULL ) ;
       drive_MCW_imseq( im3d->s312 , isqDR_setrange , (XtPointer)NULL ) ;
     }
   }
   return ;
}

/* set environment variable too */
void THD_set_image_globalrange_env(int ig)
{
   THD_set_image_globalrange(ig);
   switch(ig) {
      default:
      case 0:
         AFNI_setenv("AFNI_IMAGE_GLOBALRANGE=SLICE");
         break;
      case 1:
         AFNI_setenv("AFNI_IMAGE_GLOBALRANGE=VOLUME");
         break;
      case 2:
         AFNI_setenv("AFNI_IMAGE_GLOBALRANGE=DSET");
         break;
   }
  ENV_globalrange_view( "AFNI_IMAGE_GLOBALRANGE" );
}

/*------------------------------------------------------------------------*/
/*! Define the view_setter code, which says which image viewer plane
    was used to set the current viewpoint.  [26 Feb 2003]
--------------------------------------------------------------------------*/

void AFNI_view_setter( Three_D_View *im3d , MCW_imseq *seq )
{
   MCW_imseq *sxyz, *syzx, *szxy ;
   int val=-1 ;

ENTRY("AFNI_view_setter") ;

   if( !IM3D_VALID(im3d) ) EXRETURN ;

   sxyz = im3d->s123 ; syzx = im3d->s231 ; szxy = im3d->s312 ;

   if( !ISQ_REALZ(seq) ){
     int code = 4*(sxyz!=NULL) + 2*(syzx!=NULL) + (szxy!=NULL) ;
     switch(code){
       case 4: val = AXIAL   ; break ;
       case 2: val = SAGITTAL; break ;
       case 1: val = CORONAL ; break ;
     }
   } else {
          if( seq == sxyz ) val = AXIAL   ;
     else if( seq == syzx ) val = SAGITTAL;
     else if( seq == szxy ) val = CORONAL ;
   }

   im3d->vinfo->view_setter = val ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_set_index_viewpoint( Three_D_View *im3d ,
                               int ijk, int redisplay_option )  /* ZSS July 2010 */
{
   int nij, ni, ii, jj, kk;

   if (  ijk<0 || !im3d ||
         !IM3D_VALID(im3d) ||
         !ISVALID_3DIM_DATASET(im3d->anat_now)) return;

   if (ijk < DSET_NVOX(im3d->anat_now)) {

      ni = DSET_NX(im3d->anat_now);
      nij = (ni * DSET_NY(im3d->anat_now));

      kk = (ijk / nij);
      jj = (ijk % nij);
      ii = (jj % ni);
      jj = (jj / ni);

      AFNI_set_viewpoint( im3d, ii, jj, kk, redisplay_option );
   }

   return;
}

/*------------------------------------------------------------------------*/

void AFNI_check_for_multiple_vedits( Three_D_View *im3d )
{
   static int first=1 ;
   Three_D_View *qq3d ; int qq ;

ENTRY("AFNI_check_for_multiple_vedits") ;

   if( !first || !IM3D_VALID(im3d) ) EXRETURN ;

   for( qq=0 ; qq < MAX_CONTROLLERS ; qq++ ){
     qq3d = GLOBAL_library.controllers[qq] ;
     if( !IM3D_OPEN(qq3d) ) continue ;
     if( qq3d == im3d     ) continue ;
     if( qq3d->vedset.code <= 0 || qq3d->vedset.ival < 0 ) continue ;
     if( qq3d->fim_now == im3d->fim_now ){
#undef  MCMESS
#define MCMESS " \n"                      \
               " Multiple Clusterize \n"  \
               " operations are open \n"  \
               " on the same Overlay \n"  \
               " dataset -- they may \n"  \
               " conflict & confuse! \n "
       AFNI_popup_message( MCMESS ) ;
       WARNING_message   ( MCMESS ) ;
       first = 0 ; break ;
#undef MCMESS
     }
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_set_viewpoint( Three_D_View *im3d ,
                         int xx,int yy,int zz , int redisplay_option )
{
   int old_i1 , old_j2 , old_k3 , i1,j2,k3 ;
   int dim1,dim2,dim3 , isq_driver , do_lock , new_xyz ;
   int newti ;            /* 24 Jan 2001 */
   int ihave , doflash ;  /* 02 Mar 2011 */

   THD_dataxes *daxes ;
   THD_fvec3 fv ;
   THD_ivec3 old_ib , new_ib , old_id , new_id ;

#undef  EXRR
#define EXRR do{ recurse-- ; EXRETURN ; } while(0)
   static int recurse=0 ;
   if( recurse > 1 ) return ;  /* changed from 3 to 1 [16 Sep 2009] */
   recurse++ ;

ENTRY("AFNI_set_viewpoint") ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"input xx=%d yy=%d zz=%d",xx,yy,zz) ;
  STATUS(str) ; }

   if( ! IM3D_VALID(im3d) || ! ISVALID_3DIM_DATASET(im3d->anat_now) ) EXRR ;

   /** 02 Nov 1996:
         Attach view-specific dataxes and warps to the datasets **/

   LOAD_DSET_VIEWS(im3d) ;

   /** find if input points are inside axes of current display **/

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   dim1  = daxes->nxx ; dim2 = daxes->nyy ; dim3 = daxes->nzz ;

   /** save old ijk coordinates **/

   old_i1 = im3d->vinfo->i1 ;
   old_j2 = im3d->vinfo->j2 ;
   old_k3 = im3d->vinfo->k3 ;

   /** get and store new ijk coordinates **/

   i1 = im3d->vinfo->i1 = (xx < 0 || xx >= dim1) ? (old_i1) : xx ;
   j2 = im3d->vinfo->j2 = (yy < 0 || yy >= dim2) ? (old_j2) : yy ;
   k3 = im3d->vinfo->k3 = (zz < 0 || zz >= dim3) ? (old_k3) : zz ;

   /** determine redisplay mode for image viewers **/

   new_xyz =
    do_lock = !( i1 == old_i1 && j2 == old_j2 && k3 == old_k3 ) ;  /* 11 Nov 1996 */

   if( !redisplay_option && !new_xyz ) EXRR ;

   ihave      = (im3d->s123 != NULL || im3d->s231 != NULL || im3d->s312 != NULL);
   doflash    = (redisplay_option == REDISPLAY_FLASH) ;
   isq_driver = (redisplay_option == REDISPLAY_ALL) ? isqDR_display
                                                    : isqDR_overlay ;

   if( !AFNI_noenv("AFNI_VALUE_LABEL") && new_xyz && !ihave )
     isq_driver = isqDR_display ;         /* 08 Mar 2002 */

   LOAD_IVEC3(old_id,old_i1,old_j2,old_k3) ;
   LOAD_IVEC3(new_id,    i1,    j2,    k3) ;

#ifdef AFNI_DEBUG
STATUS(" ") ;
DUMP_IVEC3("  old_id",old_id) ;
DUMP_IVEC3("  new_id",new_id) ;
#endif

   /** store new xyz coordinates into im3d struct **/

   if( im3d->type == AFNI_3DDATA_VIEW ){
     fv = THD_3dind_to_3dmm ( im3d->anat_now , new_id ) ;
     fv = THD_3dmm_to_dicomm( im3d->anat_now , fv     ) ;
     im3d->vinfo->xi = fv.xyz[0] ;  /* set display coords */
     im3d->vinfo->yj = fv.xyz[1] ;  /* to Dicom standard  */
     im3d->vinfo->zk = fv.xyz[2] ;
   }

   /* clear labels */

   im3d->vinfo->func_val[0] = im3d->vinfo->thr_val[0] = '\0' ;
   if( do_lock || isq_driver==isqDR_display )
      im3d->vinfo->anat_val[0] = '\0';
   if( !AFNI_noenv( "AFNI_VALUE_LABEL") ) AFNI_do_bkgd_lab( im3d ) ;

   /*--- 05 Sep 2006: volume edit on demand? ---*/

   if( IM3D_IMAGIZED(im3d) && im3d->vinfo->thr_onoff    &&
       ihave               && im3d->vinfo->func_visible && !doflash ){
     int changed=0 ;
     if( VEDIT_good(im3d->vedset) ){
       STATUS("starting vedit") ;
       im3d->vedset.ival = im3d->vinfo->fim_index ;
       switch( VEDIT_CODE(im3d->vedset) ){
         case VEDIT_CLUST:  /* params 2,3,6 set in afni_cluster.c */
           im3d->vedset.param[0] = (float)im3d->vinfo->thr_index ;
           im3d->vedset.param[1] = get_3Dview_func_thresh(im3d,1);
           im3d->vedset.param[4] = im3d->vinfo->thr_sign ;
           im3d->vedset.param[5] = im3d->vinfo->use_posfunc ;
           im3d->vedset.exinfo   = NULL ;
         break ;
       }
       if( !im3d->vedskip ){
         byte *mmm = (im3d->vednomask) ? NULL : im3d->vwid->func->clu_mask ;
         changed = AFNI_vedit( im3d->fim_now , im3d->vedset , mmm ) ;
       }
       if( !DSET_VEDIT_good(im3d->fim_now) ){
         STATUS("vedit not completed") ;
         UNCLUSTERIZE(im3d) ;
       } else if( changed ){
         mri_cluster_detail *cld ; int nc ; char *rrr ;
         STATUS("vedit completed") ;
         VEDIT_cluster_helpize(im3d);
         if( im3d->vwid->func->clu_rep != NULL ){
           free(im3d->vwid->func->clu_rep); im3d->vwid->func->clu_rep = NULL;
         }
         rrr = mri_clusterize_report() ;
         if( rrr != NULL && *rrr != '\0' ) im3d->vwid->func->clu_rep = strdup(rrr) ;
         DESTROY_CLARR(im3d->vwid->func->clu_list) ;
         im3d->vwid->func->clu_list = mri_clusterize_array(1) ;
         AFNI_cluster_dispize(im3d,0);  /* display the results */
         AFNI_check_for_multiple_vedits(im3d) ;  /* 24 Jul 2014 */
         STATUS("vedit processed") ;
       }
       IM3D_CLEAR_THRSTAT(im3d) ;  /* 12 Jun 2014 */
     } else {
       UNCLUSTERIZE(im3d) ;  /* macro-ized 13 Feb 2008 */
     }
   }

   /*--- 12 Jun 2014: thresholded statistics on fim ---*/

   AFNI_setup_thrstat(im3d,0) ;  /* function-ized 27 Jun 2019 */

   /*--- redraw images now ---*/

   im3d->ignore_seq_callbacks = AFNI_IGNORE_EVERYTHING ;

   if( !IM3D_ULAY_COHERENT(im3d) ){           /* 10 Jun 2014 */
     STATUS("AFNI_set_viewpoint: incoherent ulay -- patching") ;
     ERROR_message("incoherent ulay -- patching") ;
     AFNI_assign_ulay_bricks(im3d) ;
   }

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
                          isq_driver , (XtPointer)ITOP(new_ib.ijk[2]) ) ;


      xyzm[0] = new_ib.ijk[0] ; xyzm[1] = new_ib.ijk[1] ;
      xyzm[2] = new_ib.ijk[2] ; xyzm[3] = 0 ;

      if( im3d->g123 != NULL && !doflash &&
          ( im3d->g123->never_drawn || redisplay_option == REDISPLAY_ALL ||
            new_xyz                 || im3d->g123->thresh_fade             ) )
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
                          isq_driver , (XtPointer)ITOP(new_ib.ijk[2]) ) ;

      xyzm[0] = new_ib.ijk[0] ; xyzm[1] = new_ib.ijk[1] ;
      xyzm[2] = new_ib.ijk[2] ; xyzm[3] = 0 ;

      if( im3d->g231 != NULL && !doflash &&
          ( im3d->g231->never_drawn || redisplay_option == REDISPLAY_ALL ||
            new_xyz                 || im3d->g231->thresh_fade             ) )
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
                          isq_driver , (XtPointer)ITOP(new_ib.ijk[2]) ) ;

      xyzm[0] = new_ib.ijk[0] ; xyzm[1] = new_ib.ijk[1] ;
      xyzm[2] = new_ib.ijk[2] ; xyzm[3] = 0 ;

      if( im3d->g312 != NULL && !doflash &&
          ( im3d->g312->never_drawn || redisplay_option == REDISPLAY_ALL ||
            new_xyz                 || im3d->g312->thresh_fade             ) )
         drive_MCW_grapher( im3d->g312 , graDR_redraw , (XtPointer) xyzm ) ;
   }

   im3d->ignore_seq_callbacks = AFNI_IGNORE_NOTHING ;

   /*--- redraw coordinate display now ---*/

   if( !doflash && (redisplay_option || new_xyz) ){
      AFNI_crosshair_relabel( im3d ) ;  /* 12 Mar 2004: moved this to a function, too */
      AFNI_do_bkgd_lab( im3d ) ;        /* 08 Mar 2002: moved labelizing to function */
   }

   /* 24 Jan 2001: set grapher index based on type of dataset */

   newti = im3d->vinfo->anat_index ;

   if( newti >= 0 && !doflash ){  /* ITOP = Integer to Pointer cast */
     drive_MCW_grapher( im3d->g123, graDR_setindex, (XtPointer)ITOP(newti) );
     drive_MCW_grapher( im3d->g231, graDR_setindex, (XtPointer)ITOP(newti) );
     drive_MCW_grapher( im3d->g312, graDR_setindex, (XtPointer)ITOP(newti) );
   }

   if( do_lock && !doflash )        /* 11 Nov 1996 */
     AFNI_space_lock_carryout( im3d ) ;  /* 04 Nov 1996 */

   /** Feb 1998: if desired, send coordinates to receiver **/
   /** Mar 1999: do it in an external routine, not here.  **/

   if( !doflash ){
     if( new_xyz ) AFNI_process_viewpoint( im3d ) ;
     else          AFNI_process_redisplay( im3d ) ;
   }

   if( !doflash && AFNI_HAVE_WAMI_WIDGET(im3d) ){

      char *tlab = AFNI_ttatlas_query( im3d ) ;

      AFNI_alter_wami_text(im3d, tlab);

      if (tlab) free(tlab) ;
   }

   /*** 15 Jan 2014 **/

   AFNI_sleep(1) ;
   if( im3d->s123 != NULL && im3d->s123->crop_autocenter )
     ISQ_adjust_crop( im3d->s123 , 0,0,0,0 , 1 ) ;
   if( im3d->s231 != NULL && im3d->s231->crop_autocenter )
     ISQ_adjust_crop( im3d->s231 , 0,0,0,0 , 1 ) ;
   if( im3d->s312 != NULL && im3d->s312->crop_autocenter )
     ISQ_adjust_crop( im3d->s312 , 0,0,0,0 , 1 ) ;

   EXRR ;
}
#undef EXRR

/*----------------------------------------------------------------------------
   get the n-th overlay as an MRI_IMAGE *
   (return NULL if none;  the result must be mri_free-d by the user)
   NOTE: most of the code in this function is no longer executed
         under 'normal' circumstances
------------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_overlay( int n , FD_brick *br )
{
   Three_D_View *im3d = (Three_D_View *) br->parent ;
   MRI_IMAGE *im = NULL , *fov = NULL ;
   register short *oar ;
   int ii,jj , npix , xx,yy,zz , nx,ny , gap,ovc , icr,jcr,kcr ;
   RwcBoolean ovgood ;
   THD_ivec3 ib ;
   THD_3dim_dataset *dset ;
   FD_brick *br_fim ;
   int do_xhar=0 ;            /* 22 Mar 2002 */
   MRI_IMAGE *rgbov = NULL ;  /* 30 Jan 2003 */
   int jill = im3d->vinfo->see_ttatlas && AFNI_yesenv("AFNI_JILL_TRAVESTY") ; /* Jill is trouble */

ENTRY("AFNI_overlay") ;

   if( ! IM3D_OPEN(im3d) ) RETURN(NULL) ;

   /*--- check if crosshairs, markers, or functions are visible ---*/

#ifdef IMAGEIZE_CROSSHAIRS  /* since this is disabled, do_xhar is always 0 */
   do_xhar = (im3d->vinfo->crosshair_visible && !AFNI_yesenv("AFNI_CROSSHAIR_LINES")) ;
#endif

   dset = im3d->anat_now ;

   ovgood =  do_xhar   /* crosshairs in overlay pixels? */            ||

            (  dset->markers != NULL       &&  /* Talairach */
              (dset->markers->numset > 0)  &&   /* markers? */
              (im3d->vwid->marks->ov_visible == True) )               ||

            (  dset->tagset != NULL  &&        /* user-input tags? */
               dset->tagset->num > 0 &&
               (im3d->vwid->marks->tag_visible == True) )             ||

#ifdef ALLOW_DATASET_VLIST  /* other random points? */
            ( dset->pts != NULL && im3d->vinfo->pts_visible == True ) ||
#endif

            ( im3d->vinfo->func_visible == True ) /* overlay on? */   ||

            ( im3d->vinfo->see_ttatlas &&    /* Atlas colors on? */
              im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE ) ;

   if( ! ovgood ) RETURN(NULL) ;  /* nothing is asked of us! */

   /*-- at least one source of an overlay is present --*/

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"n1=%d n2=%d",br->n1,br->n2) ; STATUS(str) ; }

   LOAD_DSET_VIEWS(im3d) ;  /* 02 Nov 1996 */

   /*----- get functional overlay, if desired -----*/
     /* Due to the ALWAYS_USE_BIGMODE change in afni_func.c, the  */
     /* return from AFNI_func_overlay() is now always RGBA format */

   if( im3d->vinfo->func_visible ){
      br_fim = UNDERLAY_TO_OVERLAY(im3d,br) ;    /* get overlay FD brick struct */
      fov    = AFNI_func_overlay( n , br_fim ) ; /* get overlay image (afni_func.c) */
      /* get rid of it if it contains nothing [12 Feb 2020] */
      if( fov != NULL && mri_allzero(fov) ){
        mri_free(fov) ; fov = NULL ;
      }

        /* in the olden days, fov might be shorts (indexes to colors),
           or might be RGB, but now it is always RGBA. However, the older
           codes below are built around short (indexed) overlays, and so
           we set aside the RGBA output to build the (obsolescent) short
           overlays, and then merge the RGBA and short overlay images later */

        /* so if by some weird chance (or later code change), we still get
           a color-indexed overlay image back here, convert it to RGB now  */

      if( fov != NULL && fov->kind == MRI_short ){        /* should not happen */
        rgbov = ISQ_index_to_rgb( im3d->dc , 1 , fov ) ;  /* conversion to RGB */
        mri_free(fov) ; fov = NULL ;
      } else if( fov != NULL && IS_RGB_TYPE(fov->kind) ){ /* should happen */
        rgbov = fov ; fov = NULL ;
      } else if( fov != NULL ){                           /* should NEVER happen */
        ERROR_message("AFNI_func_overlay returns illegal image type: %s",MRI_TYPE_NAME(fov)) ;
        mri_free(fov) ; fov = rgbov = NULL ;
      }

      if( jill ){
        if( rgbov == NULL ) INFO_message("AFNI_func_overlay is NULL") ;
        else                INFO_message("AFNI_func_overlay is %s",MRI_TYPE_NAME(rgbov)) ;
      }

      /* at this point, the functional overlay is in rgbov (which might be NULL) */
      /* and the color index overlay image (fov) is NULL for sure */
   }

   /*----- 25 Jul 2001: get an atlas overlay, if desired and possible -----*/
   /*-----              which will be merged with functional overlay  -----*/

   if( im3d->vinfo->see_ttatlas &&
       im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE ){

      MRI_IMAGE *tov ;

      int ax_1 = br->a123.ijk[0] ;
      int ax_2 = br->a123.ijk[1] ;
      int ax_3 = br->a123.ijk[2] ;

      /* if rgbov is non-NULL, the return will be rgbov again,
         after suitable editing; otherwise, the return is a new image;
         however, if nothing is overlaid, then tov will be returned NULL */

      tov = AFNI_ttatlas_overlay( im3d , n , ax_1 , ax_2 , ax_3 , rgbov ) ;

      /* if tov came back as shorts, convert it to RGB [old code] */
      /* if it came back as RGB(A), it is the new RGB(A) overlay */
      /* [this latter situation should be what happens nowadays] */

      if( tov != NULL && tov->kind == MRI_short ){ /* should not happen */
        if( jill ) WARNING_message("AFNI_ttatlas_overlay returned a short-valued image!") ;
        mri_free(rgbov) ;
        rgbov = ISQ_index_to_rgb( im3d->dc , 1 , tov ) ;
        mri_free(tov) ;
      } else if( tov != NULL && IS_RGB_TYPE(tov->kind) && tov != rgbov ){
          if( jill && rgbov != NULL ) ININFO_message(" replacing rgbov with AFNI_ttatlas_tov combo") ;
          else if( jill )             ININFO_message(" making rgbov = AFNI_ttatlas_tov output") ;
        if( rgbov != NULL ) mri_free(rgbov) ;  /* might happen??? */
        rgbov = tov ;
      }

      /* at this point, rgbov is the merged functional + atlas overlay */
   }

   /*----- now set up short (indexed) overlay image as the functional
           overlay (if present), or as a new blank image (otherwise) -----*/

   if( fov != NULL ){ /* if short overlay already exists [not any more] */

if(PRINT_TRACING)
{ char str[256] ;
sprintf(str,"new overlay from AFNI_func_overlay: nx=%d ny=%d\n",fov->nx,fov->ny) ;
STATUS(str) ; }

      im  = fov ; ovgood = True ;
      oar = MRI_SHORT_PTR(im) ;

   } else {   /* this should be the case from now on [13 Feb 2020] */

STATUS("new overlay is created de novo") ;
      im  = mri_new( br->n1 , br->n2 , MRI_short ) ; ovgood = False ;
      oar = MRI_SHORT_PTR(im) ;
   }

   nx     = im->nx ;
   ny     = im->ny ;
   npix   = nx * ny ;
   im->dx = br->del1 ;  /* load dimensions (not that anyone cares) */
   im->dy = br->del2 ;
   im->dz = br->del3 ;

   /*----- put crosshairs on image directly, if desired (and allowed) -----*/

#ifdef IMAGEIZE_CROSSHAIRS  /* this is now disabled, far above */
   if( do_xhar ){
      MCW_grapher *grapher = UNDERLAY_TO_GRAPHER(im3d,br) ;

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
#endif

   /*----- put Talairach markers on, if desired [ancient code] -----*/

   if( im3d->anat_now->markers != NULL &&
       im3d->anat_now->markers->numset > 0 &&
       (im3d->vwid->marks->ov_visible == True) ){

      THD_marker_set     *markers = im3d->anat_now->markers ;
      AFNI_marks_widgets *marks   = im3d->vwid->marks ;
      AFNI_ovtemplate    *tem     = &(marks->ov_mask) ;
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

   /*----- put tags on, if desired [also ancient code] -----*/

   if( im3d->anat_now->tagset != NULL  &&
       im3d->anat_now->tagset->num > 0 &&
       (im3d->vwid->marks->tag_visible == True) ){

      static AFNI_ovtemplate *tem = NULL ;
      static int            npold = -1 ;

      THD_usertaglist *tl = im3d->anat_now->tagset ;
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

#ifdef ALLOW_DATASET_VLIST  /* this is currently disabled in 3ddata.h */
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
#endif

   /*----- return overlay (kill it if nothing happened) -----*/
        /* [this should be the 'normal' case these days] */

   if( !ovgood ) KILL_1MRI(im) ;

   /* 30 Jan 2003:
      If the functional overlay is in RGB format,
      then must meld that with the short color index image */

   if( rgbov != NULL ){
     if( im != NULL ){ /* if short overlay is present */
       MRI_IMAGE *qim ;
       qim = ISQ_overlay( im3d->dc , rgbov , im , 1.0f ) ;
       mri_free(rgbov); mri_free(im); rgbov = qim;
     }
     im = rgbov ; /* overlay is now the RGB(A) image */
   }

   RETURN( im ) ;
}

/*------------------------------------------------------------------------*/

XmString AFNI_crosshair_label( Three_D_View *im3d )
{
   char buf[128] ;
   XmString xstr ;
   static char *RR="[R]" , *LL="[L]" ,
               *PP="[P]" , *AA="[A]" ,
               *SS="[S]" , *II="[I]" , *ZZ="   " ;
   char *xx , *yy , *zz ;
   float xval,yval,zval ;

ENTRY("AFNI_crosshair_label") ;

   if( ! IM3D_VALID(im3d) ) RETURN( NULL );

   if( ! IM3D_OPEN(im3d) ){

      buf[0] = '\0' ;
      if( im3d->type != AFNI_IMAGES_VIEW ) strcat(buf," \n") ;
      strcat(buf, "1234567890123456789\n"
                  "1234567890123456789\n"
                  "1234567890123456789"  ) ;

   } else if( im3d->type == AFNI_IMAGES_VIEW || im3d->vinfo->show_voxind ){
      int ixyz = DSET_ixyz_to_index( im3d->anat_now ,
                    im3d->vinfo->i1, im3d->vinfo->j2, im3d->vinfo->k3 ) ;

STATUS("voxel indexes") ;

      if( ISVALID_3DIM_DATASET(im3d->fim_now) &&
          im3d->vinfo->func_visible && DSET_INMEMORY(im3d->fim_now) ){
         THD_fvec3 fv ;
         THD_ivec3 iv ;
         int flag , fxyz ;

         flag = im3d->fim_now->wod_flag ;
         im3d->fim_now->wod_flag = False ;

         fv = THD_dicomm_to_3dmm( im3d->fim_now ,
                                  TEMP_FVEC3(im3d->vinfo->xi,im3d->vinfo->yj,im3d->vinfo->zk) ) ;
         iv = THD_3dmm_to_3dind( im3d->fim_now , fv ) ;
         fxyz = DSET_ixyz_to_index( im3d->fim_now , iv.ijk[0],iv.ijk[1],iv.ijk[2] ) ;

         im3d->fim_now->wod_flag = flag ;

         sprintf( buf , "Ul=%d Ol=%d\nx: Ul=%4d Ol=%4d\ny: Ul=%4d Ol=%4d\nz: Ul=%4d Ol=%4d" ,
                  ixyz , fxyz ,
                  im3d->vinfo->i1,iv.ijk[0] ,
                  im3d->vinfo->j2,iv.ijk[1] ,
                  im3d->vinfo->k3,iv.ijk[2]  ) ;
      } else {
         sprintf( buf , "index %d:\nvoxel x = %4d\nvoxel y = %4d\nvoxel z = %4d" ,
                  ixyz , im3d->vinfo->i1 , im3d->vinfo->j2 , im3d->vinfo->k3  ) ;
      }
   } else {
      char bxyz[3][32] , *cname, obl[8] ;
      float angle=0.0;

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

      if( strcmp(GLOBAL_library.cord.orcode,"RAI") == 0 )
        cname = "=DICOM" ;
      else if( strcmp(GLOBAL_library.cord.orcode,"LPI") == 0 )
        cname = "=SPM  " ;
      else
        cname = "      " ;


      if (dset_obliquity(im3d->anat_now, &angle)==1) sprintf(obl," *");
      else obl[0]='\0';

      sprintf( buf , "[order: %s%s%s]\nx %17s\ny %17s\nz %17s"   ,
               GLOBAL_library.cord.orcode       , cname , obl,
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
   Three_D_View *im3d                = (Three_D_View *)client_data ;
   XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call_data;

   int bval , ip , xx=-1 , yy=-1 , zz=-1 ;
   Widget *other_tog=NULL ;

ENTRY("AFNI_marktog_CB") ;

   if( ! IM3D_OPEN(im3d) || im3d->anat_now->markers == NULL ) EXRETURN ;

   switch( cbs->reason ){

      default:  EXRETURN ;  /* error */

      /** case XmCR_ACTIVATE: **/
      case XmCR_DISARM:   /* button on the control panel */
         bval      = AFNI_first_tog( MARKS_MAXNUM ,
                                     im3d->vwid->marks->tog ) ;
#ifdef POPTOG
         other_tog = im3d->vwid->marks->poptog ;
#endif
      break ;

#ifdef POPTOG
      case XmCR_VALUE_CHANGED:  /* button on the menu panel */
         bval = AFNI_first_tog( MARKS_MAXNUM ,
                                im3d->vwid->marks->poptog ) ;
         other_tog = im3d->vwid->marks->tog ;
      break ;
#endif
   }

   /* bval      = index of toggle that is set (-1 if none)
      other_tog = pointer to other set of toggles;
                  set those buttons to match now */

   if( other_tog != NULL )
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

   if( im3d->anat_now->markers->numset > 0 ){
      AFNI_set_viewpoint( im3d , xx,yy,zz , REDISPLAY_OVERLAY ) ;  /* redraw */
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

void AFNI_set_tog( int nset , int ntog , Widget *tog )
{
   int ib ;

ENTRY("AFNI_set_tog") ;

   for( ib=0 ; ib < ntog ; ib++ )
      XmToggleButtonSetState( tog[ib] , ib==nset , False ) ;

   EXRETURN ;
}

int AFNI_first_tog( int ntog , Widget *tog )
{
   int ib ;

ENTRY("AFNI_first_tog") ;

   for( ib=0 ; ib < ntog ; ib++ )
      if( XmToggleButtonGetState(tog[ib]) ) break ;

   if( ib >= ntog ) ib = -1 ;
   RETURN(ib) ;
}

#if 0
int AFNI_all_tog( int ntog , Widget *tog )
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
   Three_D_View *im3d = (Three_D_View *) client_data ;
   int itog , ipt , setmask , vwarp ;
   RwcBoolean sens , transformable ;
   THD_marker_set *markers ;
   AFNI_marks_widgets *marks ;
   THD_fvec3 fv ;

ENTRY("AFNI_marks_action_CB") ;

   /* sanity check */

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   marks = im3d->vwid->marks ;

   /*------ done button (it used to exist) -----*/

   if( w == NULL ){  /* close down */

      RwcBoolean redisplay ;

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
         DSET_overwrite_header(im3d->anat_now) ;
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
      BEEPIT ; EXRETURN ;
   }

   ipt = itog ;  /* index of point to deal with */

   /*----- set button pressed -----*/

#ifdef POPTOG
   if( w == marks->action_set_pb || w == marks->pop_set_pb ){
#else
   if( w == marks->action_set_pb ){
#endif

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
#ifdef POPTOG
         MCW_invert_widget( marks->poptog[itog] ) ;
#endif
         marks->inverted[itog] = True ;
      }

      marks->changed = True ;  /* set or reset a marker --> a change */
   }

   /*----- clear button pressed -----*/

   else if( w == marks->action_clear_pb || w == marks->pop_clear_pb ){

      if( ! markers->valid[ipt] ){
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
#ifdef POPTOG
         MCW_invert_widget( marks->poptog[itog] ) ;
#endif
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

void AFNI_resam_vox_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_resam_vox_av_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

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

void AFNI_marks_disp_av_CB( MCW_arrowval *av , XtPointer client_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   int ipx = av->ival ;

ENTRY("AFNI_marks_disp_av_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

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

void AFNI_make_ptmask( int size , int gap , AFNI_ovtemplate *tem )
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

void AFNI_make_tagmask( int size , int gap , AFNI_ovtemplate *tem )
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
   Three_D_View *im3d = (Three_D_View *) client_data ;
   int bval ;

ENTRY("AFNI_switchview_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   bval = AFNI_first_tog( LAST_VIEW_TYPE+1 ,
                          im3d->vwid->view->view_bbox->wbut ) ;

   if( bval < 0 || bval == im3d->vinfo->view_type ) EXRETURN ;
   if( im3d->anat_dset[bval] == NULL ) EXRETURN ;

   SHOW_AFNI_PAUSE ;

   DISABLE_INSTACORR(im3d) ; DESTROY_ICOR_setup(im3d->iset) ; /* 08 May 2009 */

   POPDOWN_strlist_chooser ;                        /* might be choosing datasets */
   UNCLUSTERIZE(im3d) ;                             /* 13 Feb 2008 */

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
   Three_D_View *im3d ;
   THD_session  *sess ;
   THD_sessionlist *ssl = GLOBAL_library.sslist ;
   THD_3dim_dataset *dset ;

ENTRY("AFNI_purge_dsets") ;

   /*-- sanity check --*/

   if( ! ISVALID_SESSIONLIST(ssl) || ssl->num_sess <= 0 ) EXRETURN ;

   /*-- for each session in the list --*/

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      sess = ssl->ssar[iss] ;

      /*-- for each anat dataset in the session --*/

      for( idd=0 ; idd < sess->num_dsset ; idd++ ){
         for( ivv=FIRST_VIEW_TYPE ; ivv <= LAST_VIEW_TYPE ; ivv++ ){
            dset = GET_SESSION_DSET(sess,idd,ivv) ;
/*            dset = sess->dsset_xform_table[idd][ivv] ; */
            if( dset == NULL ) continue ;
            if( doall ){ PURGE_DSET(dset) ; continue ; }

            /*-- for each controller now running --*/

            for( icc=0 ; icc < MAX_CONTROLLERS ; icc++ ){
               im3d = GLOBAL_library.controllers[icc] ;
               if( IM3D_VALID(im3d) &&
                   ((dset==im3d->anat_now) ||
                    (dset==im3d->fim_now)  ||
                    (dset==im3d->fimdata->fimdset)) ) break ;
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

void AFNI_initialize_view( THD_3dim_dataset *old_anat, Three_D_View *im3d )
{
   int vvv , itog , lll , sss , aaa , fff , id ;
   THD_3dim_dataset     *dset , *new_anat , *new_func ;
   THD_marker_set       *markers ;
   AFNI_viewing_widgets *view ;
   AFNI_marks_widgets   *marks ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;
   static int first_image=1 ; /* 02 Feb 2018 */

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

   new_anat = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[sss], aaa, vvv) ;
   new_func = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[sss], fff, vvv) ;

/*   new_anat = GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[aaa][vvv] ;
   new_func = GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[fff][vvv] ;
*/
   AFNI_vedit_clear( im3d->fim_now ) ;  /* 05 Sep 2006 */

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

   /*---------------------------------------------------------*/
   /* set the new datasets that we will deal with from now on */

   for( id=0 ; id <= LAST_VIEW_TYPE ; id++ ){
     im3d->anat_dset[id] = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[sss], aaa, id) ;
     im3d->fim_dset[id]  = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[sss], fff, id) ;

/*     im3d->anat_dset[id] = GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[aaa][id] ;
     im3d->fim_dset[id]  = GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[fff][id] ;
*/
     if( ISVALID_3DIM_DATASET(im3d->anat_dset[id]) )
       SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], True ) ;
     else
       SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], False) ;
   }

   im3d->anat_now = im3d->anat_dset[vvv] ;
   im3d->fim_now  = im3d->fim_dset[vvv] ;
   im3d->ss_now   = GLOBAL_library.sslist->ssar[sss] ;

   if( !ISVALID_DSET(im3d->fim_now) ) AFNI_SEE_FUNC_OFF(im3d) ;  /* 22 May 2009 */

   SENSITIZE( im3d->vwid->func->clu_rowcol , DSET_INMEMORY(im3d->fim_now) ) ;

   if( !DSET_INMEMORY(im3d->fim_now) ) UNCLUSTERIZE(im3d) ; /* 14 Feb 2008 */

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
      SENSITIZE( view->define_marks_pb , False ) ;
      SENSITIZE( view->see_marks_bbox->wrowcol , False ) ;
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

#ifdef POPTOG
      SENSITIZE( marks->pop_set_pb   , marks->editable ) ;
      SENSITIZE( marks->pop_clear_pb , marks->editable ) ;
#endif

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
#ifdef POPTOG
            XtUnmanageChild( marks->poptog[itog] ) ;
#endif
         } else {
            MCW_set_widget_label( marks->tog[itog] ,
                                  &(markers->label[itog][0]) ) ;
            SENSITIZE( marks->tog[itog] , True ) ;
            XtManageChild( marks->tog[itog] ) ;

#ifdef POPTOG
            MCW_set_widget_label( marks->poptog[itog] ,
                                  &(markers->label[itog][0]) ) ;
            SENSITIZE( marks->poptog[itog] , True ) ;
            XtManageChild( marks->poptog[itog] ) ;
#endif

            if( markers->valid[itog] && ! marks->inverted[itog] ){
               MCW_invert_widget( marks->tog[itog] ) ;
#ifdef POPTOG
               MCW_invert_widget( marks->poptog[itog] ) ;
#endif
               marks->inverted[itog] = True ;
            }

            if( ! markers->valid[itog] && marks->inverted[itog] ){
               MCW_invert_widget( marks->tog[itog] ) ;
#ifdef POPTOG
               MCW_invert_widget( marks->poptog[itog] ) ;
#endif
               marks->inverted[itog] = False ;
            }
         }
      } /* end of loop over markers */

   } /* end of dealing with markers */

   /*------------------------------*/
   /*----- set up for viewing -----*/

   AFNI_setup_viewing( im3d , True ) ;
   AFNI_set_rinfo_labels( im3d ) ;      /* 11 Mar 2020 */

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
      LOAD_IVEC3( iv, im3d->vinfo->i1, im3d->vinfo->j2, im3d->vinfo->k3 ) ;
   }

   /* 27 Jan 2004: set coordinate to center of dataset
                   if we were formerly looking at the dummy dataset */

   if( (im3d->dummied && !GLOBAL_library.have_dummy_dataset) ||
       AFNI_yesenv("AFNI_RECENTER_VIEWING")                    ){
     im3d->dummied = 0 ;
     LOAD_IVEC3( iv , im3d->anat_now->daxes->nxx/2 ,
                      im3d->anat_now->daxes->nyy/2 ,
                      im3d->anat_now->daxes->nzz/2  ) ;
   }

   DISABLE_LOCK ;  /* 11 Nov 1996 */

   CLU_setup_alpha_tables(im3d) ;  /* Jul 2010 */

   AFNI_view_setter(im3d,NULL) ;
   AFNI_set_viewpoint( im3d, iv.ijk[0],iv.ijk[1],iv.ijk[2] , REDISPLAY_ALL ) ;

   ENABLE_LOCK ;   /* 11 Nov 1996 */

   SAVE_VPT(im3d) ;  /* save current location as jumpback */

#if 0 /* no longer needed */
   if( first_image                &&
       GLOBAL_argopt.left_is_left &&
       im3d->anat_now->dblk->diskptr->storage_mode == STORAGE_BY_IMAGE_FILE ){

     first_image = 0 ;
     (void) MCW_popup_message( im3d->vwid->picture ,
                                 " \n"
                                 "*****--- WARNING: ---*****\n"
                                 "  Image viewing is set to\n"
                                 "  Left-is-Left, so that\n"
                                 "  viewing .jpg or .png\n"
                                 "  'datasets' might show\n"
                                 "  as reflected left-right.\n"
                                 "  To fix this, press the\n"
                                 "  l (lower case L) key\n"
                                 "  when the mouse cursor\n"
                                 "  focus is over the image\n"
                                 "  viewer window.\n"
                               , MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }
#endif

   EXRETURN ;
}

/*----------------------------------------------------------------------*/
/*! Find the warp that takes one dataset to another, if it exists.
    [28 Aug 2002]
------------------------------------------------------------------------*/

THD_warp * AFNI_find_warp( THD_3dim_dataset *dset_to , THD_3dim_dataset *dset_from )
{
   THD_warp *swarp = NULL ;
   char idkey[256] ;

   if( GLOBAL_library.warptable == NULL ||
       dset_to   == NULL                ||
       dset_from == NULL                  ) return NULL ;

   sprintf(idkey,"%s,%s",dset_to->idcode.str,dset_from->idcode.str) ;
   swarp = (THD_warp *) findin_Htable( idkey , GLOBAL_library.warptable ) ;
   return swarp ;
}

/*----------------------------------------------------------------------------*/
/* Set up the thresholded statistic on the fim [moved here 27 Jun 2019] */

#undef  THBOT
#undef  THTOP
#undef  THBIG
#define THBIG    1.e+37f
#define THBOT(t) ((im3d->vinfo->thr_sign==0 || im3d->vinfo->thr_sign==2) ? (-(t)) : (-THBIG))
#define THTOP(t) ((im3d->vinfo->thr_sign==0 || im3d->vinfo->thr_sign==1) ? (t)    :  (THBIG))

void AFNI_setup_thrstat( Three_D_View *im3d , int force )
{
ENTRY("AFNI_setup_thrstat") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   if( im3d->vinfo->func_visible ){
     float thr,thbot,thtop,fac ; MRI_IMAGE *thim , *ovim ; float_pair ovmm ;
     thr   = get_3Dview_func_thresh(im3d,1) ;
     thbot = THBOT(thr) ; thtop = THTOP(thr) ;
     DSET_load(im3d->fim_now) ; /* 24 Jun 2019 */
     if( force || im3d->vinfo->fim_index != im3d->fim_thrindex ||
                  FLDIF(thbot,im3d->fim_thrbot) || FLDIF(thtop,im3d->fim_thrtop) ){
       ovim = AFNI_dataset_displayim(im3d->fim_now,im3d->vinfo->fim_index) ;
       thim = AFNI_dataset_displayim(im3d->fim_now,im3d->vinfo->thr_index) ;
       IM3D_CLEAR_THRSTAT(im3d) ;
       if( ovim != NULL && thim != NULL ){
         im3d->fim_thrbot = thbot ; im3d->fim_thrtop = thtop ;
         fac = DSET_BRICK_FACTOR(im3d->fim_now,im3d->vinfo->thr_index) ;
         if( fac > 0.0f ){ thbot /= fac ; thtop /= fac ; }
         ovmm = mri_threshold_minmax(thbot,thtop,thim,ovim) ;
         im3d->fim_thresh_min = ovmm.a ; im3d->fim_thresh_max = ovmm.b ;
         fac = DSET_BRICK_FACTOR(im3d->fim_now,im3d->vinfo->fim_index) ;
         if( fac > 0.0f ){ im3d->fim_thresh_min *= fac ; im3d->fim_thresh_max *= fac ; }
       }
       if( im3d->fim_thresh_min <= im3d->fim_thresh_max ){
         char str[256] ; int_pair mij = mri_threshold_minmax_indexes() ;
         sprintf(str,"OLay thresholded range: %f : %f",im3d->fim_thresh_min,im3d->fim_thresh_max ) ;
         MCW_register_hint( im3d->vwid->func->range_label , str ) ;
         im3d->fim_thresh_min_ijk = mij.i ;
         im3d->fim_thresh_max_ijk = mij.j ;
         im3d->fim_thrindex = im3d->vinfo->fim_index ;
       } else {
         MCW_register_hint( im3d->vwid->func->range_label , "OLay thresholded range: unknown" ) ;
         im3d->fim_thrindex = -1 ;
       }
       SENSITIZE(im3d->vwid->func->pbar_jumpto_thmax_pb,(im3d->fim_thresh_max_ijk > 0)) ;
       SENSITIZE(im3d->vwid->func->pbar_jumpto_thmin_pb,
                   (im3d->fim_thresh_min_ijk > 0)
                && (im3d->fim_thresh_min_ijk != im3d->fim_thresh_max_ijk) ) ;
     }
   } else {
     IM3D_CLEAR_THRSTAT(im3d) ;  /* 12 Jun 2014 */
     MCW_register_hint( im3d->vwid->func->range_label , "OLay thresholded range: unknowable" ) ;
   }

   EXRETURN ;
}

#undef  THBOT
#undef  THTOP
#undef  THBIG

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

void AFNI_setup_viewing( Three_D_View *im3d , RwcBoolean rescaled )
{
   FD_brick **fbr ;
   XmString xstr ;
   RwcBoolean  same , dont_fix_pts , writer ,
            anat_brick_possible , func_brick_possible ;
   int      val , top ;

   static THD_3dim_dataset *old_fim  = NULL ; /* 12 Dec 2001 */
   static Three_D_View     *old_im3d = NULL ; /* 29 Jan 2002 */
   static THD_3dim_dataset *old_anat = NULL ; /* 12 Dec 2001 */
   static int         old_anat_nvals = -1 ;   /* 21 Jul 2009 */
   static int         old_func_nvals = -1 ;   /* 21 Jul 2009 */

ENTRY("AFNI_setup_viewing") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /*-----------------------------------------------------*/
   /*--- set up the anat w-o-d axes and viewing bricks ---*/

   anat_brick_possible = DSET_INMEMORY(im3d->anat_now) ;

   /*- The Ides of March, 2000: allow switching back to "view brick" -*/

   if( anat_brick_possible         &&
       im3d->vinfo->force_anat_wod &&
       im3d->vinfo->tempflag == 0  &&
       !AFNI_noenv("AFNI_VIEW_ANAT_BRICK") ){

STATUS("setting anatmode_bbox back to 'View ULay Data Brick'") ;
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
     im3d->fim_voxwarp->type = ILLEGAL_TYPE ;                   /* 02 Nov 1996 */

   LOAD_ANAT_VIEW(im3d) ;                                       /* 02 Nov 1996 */

   fbr = THD_setup_bricks( im3d->anat_now ) ;
   if( fbr == NULL ){
     ERROR_message("THD_setup_bricks of anat_now fails!") ; EXRETURN ;
   }
   DESTROY_FD_BRICK(im3d->b123_anat) ; im3d->b123_anat = fbr[0] ;
   DESTROY_FD_BRICK(im3d->b231_anat) ; im3d->b231_anat = fbr[1] ;
   DESTROY_FD_BRICK(im3d->b312_anat) ; im3d->b312_anat = fbr[2] ;
   myXtFree(fbr) ;

   im3d->b123_anat->parent =
     im3d->b231_anat->parent =
       im3d->b312_anat->parent = (XtPointer) im3d ;

   im3d->b123_anat->resam_code =
     im3d->b231_anat->resam_code =
       im3d->b312_anat->resam_code = im3d->vinfo->anat_resam_mode ;

   im3d->b123_anat->thr_resam_code =     /* 30 Jul 2003 */
     im3d->b231_anat->thr_resam_code =
       im3d->b312_anat->thr_resam_code = im3d->vinfo->anat_resam_mode ;

   /* 30 Nov 1997: don't go past end of dataset */

   if( im3d->vinfo->anat_index >= DSET_NVALS(im3d->anat_now) )
     im3d->vinfo->anat_index = DSET_NVALS(im3d->anat_now) - 1 ;

   /*-----------------------------------------------------*/
   /*--- set up the func w-o-d axes and viewing bricks ---*/

   if( ISVALID_3DIM_DATASET(im3d->fim_now) ){  /* that is, if we have func */

STATUS("function brick setup") ;

      /*-- access data in dataset im3d->fim_now IF AND ONLY IF
             1) its actual data axes are the same as the wod_daxes
             2) it has actual data
             3) the user hasn't officially declared for warp-on-demand
             4) the dataset doesn't have a warp to bring it into
                alignment with the current anat dataset (self_warp)    --*/

STATUS("deciding whether to use function WOD") ;

      func_brick_possible =
         EQUIV_DATAXES( im3d->fim_now->daxes , im3d->wod_daxes ) &&   /* 02 Nov 1996 */
         DSET_INMEMORY( im3d->fim_now ) ;

      /*- 27 Aug 2002: see if there is a self_warp from
                       fim_now back to anat_now; if so, install it;
                       this is a coordinate-to-coordinate transformation,
                       and requires warp-on-demand viewing              -*/

      { THD_warp *swarp = AFNI_find_warp( im3d->anat_now , im3d->fim_now ) ;
        im3d->fim_selfwarp = swarp ;  /* transform from fim to anat coords */
        if( swarp != NULL ) func_brick_possible = 0 ;   /* require warp-on-demand */
      }

      /*- The Ides of March, 2000: allow switching back to "view brick" -*/

      if( func_brick_possible                       &&
          ( ( im3d->vinfo->force_func_wod  &&
              im3d->vinfo->tempflag == 0   &&
              !AFNI_noenv("AFNI_VIEW_FUNC_BRICK") ) ||
            ( !im3d->anat_wod_flag         &&              /* 08 Aug 2003 */
              im3d->anat_now == im3d->fim_now     )   ) ){

STATUS("setting funcmode_bbox back to 'View Func Data Brick'") ;
         im3d->vinfo->force_func_wod = 0 ;
         MCW_set_bbox( im3d->vwid->dmode->funcmode_bbox , DMODE_BRICK_BVAL ) ;
      }

      if( func_brick_possible && ! im3d->vinfo->force_func_wod ){
STATUS("not forcing function WOD") ;
        im3d->fim_wod_flag = False ;   /* 02 Nov 1996 */
      } else {
STATUS("forcing function WOD") ;
        im3d->fim_wod_flag = True ;    /* 02 Nov 1996 */
      }

      LOAD_FUNC_VIEW(im3d) ;  /* 02 Nov 1996 */

      fbr = THD_setup_bricks( im3d->fim_now ) ;
      if( fbr == NULL ){
        fprintf(stderr,"THD_setup_bricks of fim_now fails!\n") ; EXRETURN ;
      }
      DESTROY_FD_BRICK(im3d->b123_fim) ; im3d->b123_fim = fbr[0] ;
      DESTROY_FD_BRICK(im3d->b231_fim) ; im3d->b231_fim = fbr[1] ;
      DESTROY_FD_BRICK(im3d->b312_fim) ; im3d->b312_fim = fbr[2] ;
      myXtFree(fbr) ;

      im3d->b123_fim->brother = (XtPointer)im3d->b123_anat ;
      im3d->b231_fim->brother = (XtPointer)im3d->b231_anat ;
      im3d->b312_fim->brother = (XtPointer)im3d->b312_anat ;

      im3d->b123_anat->brother = (XtPointer)im3d->b123_fim ;
      im3d->b231_anat->brother = (XtPointer)im3d->b231_fim ;
      im3d->b312_anat->brother = (XtPointer)im3d->b312_fim ;

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

      if( im3d->vinfo->fim_index >= DSET_NVALS(im3d->fim_now) )
          im3d->vinfo->fim_index = DSET_NVALS(im3d->fim_now) - 1 ;

      if( im3d->vinfo->thr_index >= DSET_NVALS(im3d->fim_now) )
          im3d->vinfo->thr_index = DSET_NVALS(im3d->fim_now) - 1 ;

      /* first time in for this controller (or if so ordered),
         set fim_index and thr_index to reasonable values (IMHO)
         -- modified 12 Jan 2017 to define 'reasonable' more reasonably */

      { static int first=1, ffim[MAX_CONTROLLERS] ; int qq ;
        if( first ){
          first=0; for( qq=0; qq < MAX_CONTROLLERS; qq++ ) ffim[qq]=1;
        }
        qq = AFNI_controller_index(im3d) ;
        if( ffim[qq] || im3d->vinfo->func_init_subbricks ){
          int_pair otp = find_reasonable_overlay_indexes(im3d->fim_now) ;
          if( otp.i >= 0 ) im3d->vinfo->fim_index = otp.i ;
          if( otp.j >= 0 ) im3d->vinfo->thr_index = otp.j ;
          ffim[qq] = 0 ;
        }
      }

      im3d->vinfo->func_init_subbricks = 0 ;  /* 12 Jan 2017 */

      /* 29 Jan 2008: enable/disable the FDR-izing button */

      { int scod=DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
            doit=FUNC_IS_STAT(scod) && DSET_INMEMORY(im3d->fim_now) ;
        XtSetSensitive( im3d->vwid->func->thr_fdr_pb , (RwcBoolean)doit ) ;
      }

   } else {   /* 29 Jul 2003: no longer possible */

STATUS("no function dataset") ;

      DESTROY_FD_BRICK(im3d->b123_fim) ; im3d->b123_fim = NULL ;
      DESTROY_FD_BRICK(im3d->b231_fim) ; im3d->b231_fim = NULL ;
      DESTROY_FD_BRICK(im3d->b312_fim) ; im3d->b312_fim = NULL ;

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
     im3d->vinfo->force_anat_wod = 1 ;
   }

   if( func_brick_possible ){
     SENSITIZE( im3d->vwid->dmode->funcmode_bbox->wbut[DMODE_BRICK] , True ) ;
   } else {
     SENSITIZE( im3d->vwid->dmode->funcmode_bbox->wbut[DMODE_BRICK] , False ) ;
     MCW_set_bbox( im3d->vwid->dmode->funcmode_bbox , DMODE_WOD_BVAL ) ;
     im3d->vinfo->force_func_wod = 1 ;
   }

   AV_SENSITIZE( im3d->vwid->dmode->anat_resam_av , im3d->anat_wod_flag ) ;

   AV_SENSITIZE( im3d->vwid->dmode->resam_vox_av , im3d->anat_wod_flag ) ;

   /* Jan 31, 1995: don't allow writes of datasets without warp parents */
   /* Jun 22, 1995: allow it if destruct mode is actuated!              */

   if( GLOBAL_argopt.destruct ){  /* not currently implemented */
     writer = True ;
   } else {
     writer = (RwcBoolean) DSET_WRITEABLE(im3d->anat_now) ;  /* mod 26 Mar 2001 */
   }

   SENSITIZE( im3d->vwid->dmode->write_anat_pb , writer ) ;

   if( GLOBAL_argopt.destruct ){  /* not currently implemented */
     writer = (RwcBoolean) ISVALID_3DIM_DATASET(im3d->fim_now) ;
   } else {
     writer = (RwcBoolean) DSET_WRITEABLE(im3d->fim_now) ;  /* mod 26 Mar 2001 */
   }

   SENSITIZE( im3d->vwid->dmode->write_func_pb , writer ) ;

   /* 18 Oct 2010: SaveAs buttons on or off? */

   writer = DSET_INMEMORY(im3d->anat_now) ;
   SENSITIZE( im3d->vwid->dmode->saveas_anat_pb , writer ) ;

   writer = DSET_INMEMORY(im3d->fim_now) ;
   SENSITIZE( im3d->vwid->dmode->saveas_func_pb , writer ) ;

   /*--- function controls (always see them) ---*/

   {  RwcBoolean have_fim = ISVALID_3DIM_DATASET(im3d->fim_now) ;
      RwcBoolean have_thr = have_fim ;

      static int first=1, zfim[MAX_CONTROLLERS] ; int qq ;
      if( first ){
        first=0; for( qq=0; qq < MAX_CONTROLLERS; qq++ ) zfim[qq]=1;
      }

STATUS(" -- function widgets ON") ;

      SENSITIZE( im3d->vwid->view->define_func_pb      , True ) ;  /* always on */
      SENSITIZE( im3d->vwid->view->see_func_bbox->wtop , True ) ;

      /* setup threshold slider to be always on */

      XtManageChild( im3d->vwid->func->thr_rowcol ) ;
      qq = AFNI_controller_index(im3d) ;
      if( zfim[qq] && ISVALID_DSET(im3d->fim_now) ){
STATUS(" -- set threshold to zero (startup)") ;
        XmScaleSetValue( im3d->vwid->func->thr_scale , 1 ) ;
        im3d->vinfo->func_threshold = THR_factor ; zfim[qq] = 0 ;
      }
      FIX_SCALE_SIZE(im3d) ; FIX_SCALE_VALUE(im3d) ;

      /* turn on various ways of making function into underlay */

#ifdef USE_UNDERLAY_BBOX
      SENSITIZE( im3d->vwid->func->underlay_bbox->wbut[UNDERLAY_ALLFUNC], have_fim ) ;
#endif

      /* set underlay type back to anat if no function */

      if( ! have_fim ) im3d->vinfo->underlay_type = UNDERLAY_ANAT ;

      /* allow resample control only if we are using w-o-d */

      AV_SENSITIZE( im3d->vwid->dmode->func_resam_av,
                    have_fim && im3d->fim_wod_flag ) ;

      AV_SENSITIZE( im3d->vwid->dmode->thr_resam_av,    /* 09 Dec 1997 */
                    have_fim && im3d->fim_wod_flag ) ;

      /** Mar 1996: modify the threshold scale stuff **/
      /** Oct 1996: increase decim by 1 to allow for
                    new precision 0..999 of scale (used to be 0..99) **/
      /** Nov 1997: the scale precision is now set by macro THR_top_expon,
                    and its settings are done in routine AFNI_set_thresh_top **/

#if 0
      if( have_thr ){
        int iv = im3d->vinfo->thr_index , jj ;

        jj = DSET_BRICK_STATCODE(im3d->fim_now,iv) ;
        if( jj > 0 )
          MCW_set_widget_label( im3d->vwid->func->thr_label ,
                                FUNC_label[jj] ) ;
        else
          MCW_set_widget_label( im3d->vwid->func->thr_label ,
                                DSET_BRICK_LABEL(im3d->fim_now,iv) ) ;
      }
#endif

      /* set the pval label at the bottom of the scale */

      AFNI_set_thr_pval( im3d ) ;

      /*** 29 Jul 2003: always do buckets now ***/

      XtManageChild( im3d->vwid->func->anat_buck_av->wrowcol ) ;
      XtManageChild( im3d->vwid->func->fim_buck_av->wrowcol  ) ;
      XtManageChild( im3d->vwid->func->thr_buck_av->wrowcol  ) ;

      /** 12 Dec 2001: only refit menus if dataset has changed **/

      if( have_fim &&
          (im3d->fim_now != old_fim || im3d != old_im3d ||
           DSET_NVALS(im3d->fim_now) != old_func_nvals )  ){
        refit_MCW_optmenu( im3d->vwid->func->fim_buck_av ,
                           0 ,                            /* new minval */
                           DSET_NVALS(im3d->fim_now)-1 ,  /* new maxval */
                           im3d->vinfo->fim_index ,       /* new inival */
                           0 ,                            /* new decim? */
                           (str_func *)AFNI_bucket_label_CB, /* text routine */
                           im3d->fim_now                  /* text data */
                         ) ;
        refit_MCW_optmenu( im3d->vwid->func->thr_buck_av ,
                           0 ,                            /* new minval */
                           DSET_NVALS(im3d->fim_now)-1 ,  /* new maxval */
                           im3d->vinfo->thr_index ,       /* new inival */
                           0 ,                            /* new decim? */
                           (str_func *)AFNI_bucket_label_CB, /* text routine */
                           im3d->fim_now                  /* text data */
                         ) ;
      }

      if( im3d->anat_now != old_anat || im3d != old_im3d ||
          DSET_NVALS(im3d->anat_now) != old_anat_nvals     ){
        refit_MCW_optmenu( im3d->vwid->func->anat_buck_av ,
                           0 ,                             /* new minval */
                           DSET_NVALS(im3d->anat_now)-1 ,  /* new maxval */
                           im3d->vinfo->anat_index ,       /* new inival */
                           0 ,                             /* new decim? */
                           (str_func *)AFNI_bucket_label_CB, /* text routine */
                           im3d->anat_now                  /* text data */
                         ) ;
      }

      XtManageChild( im3d->vwid->func->buck_rowcol ) ;  /* make itvisible */
      XtManageChild( im3d->vwid->func->buck_frame ) ;
   }

   /*--- set the function type bboxes based on the current
         viewing set up (which may have changed due to lack of function) ---*/

STATUS(" -- function underlay widgets") ;

#ifdef USE_UNDERLAY_BBOX
   MCW_set_bbox( im3d->vwid->func->underlay_bbox , 1 << im3d->vinfo->underlay_type ) ;
#endif

   AFNI_assign_ulay_bricks(im3d) ;   /* 10 Jun 2014 */

   /*--------------------------------------------------------*/
   /*--- 3/24/95: deal with the new range widgets in func ---*/

   AFNI_reset_func_range( im3d ) ;

#ifdef ALLOW_DATASET_VLIST
   /*---------------------------------------------------------*/
   /*--- May 1995: if points exist in some other dataset   ---*/
   /*---           associated with this one, but not here, ---*/
   /*---           transform the points to this dataset.   ---*/

   dont_fix_pts = ! rescaled ;  /* if didn't rescale, don't need to fix */

   if( im3d->anat_now->pts == NULL ){
      int ii ;
      THD_3dim_dataset *dset_orig = NULL ;
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
#endif /* ALLOW_DATASET_VLIST */

   /*------ 06 Mar 2002: turn "SUMA to" on image popup on or off ------*/

   if( im3d->vwid->imag->pop_sumato_pb != NULL ){
     if( SESSION_HAS_SUMA(im3d->ss_now) )
       XtManageChild( im3d->vwid->imag->pop_sumato_pb ) ;
     else
       XtUnmanageChild( im3d->vwid->imag->pop_sumato_pb ) ;
   }

   /*------ 01 May 2002: turn "Jump to (MNI)" on or off ------*/

   if( im3d->vwid->imag->pop_mnito_pb != NULL ){
     if( CAN_TALTO(im3d) )
       XtManageChild( im3d->vwid->imag->pop_mnito_pb ) ;
     else
       XtUnmanageChild( im3d->vwid->imag->pop_mnito_pb ) ;
   }

   /*-------------------------------------------------------------------*/
   /*--- Sep 1995: turn "Go to atlas position" button on
         image popup on or off ---*/

STATUS(" -- managing Go to atlas position button, etc") ;

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

   if( im3d->vwid->imag->pop_instacorr_pb != NULL ){
     if( ISREADY_EITHER_ICOR(im3d) ) ENABLE_INSTACORR(im3d) ;
     else                           DISABLE_INSTACORR(im3d) ;
   }

   /*--- 25 Jul 2001: sensitize 'See TT Atlas Regions' button ---*/

#if 1
   XtSetSensitive( im3d->vwid->func->see_ttatlas_bbox->wrowcol ,
          (RwcBoolean)( im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE &&
          TT_retrieve_atlas_dset(Current_Atlas_Default_Name(), 0) != NULL) ) ;
#else
   XtSetSensitive( im3d->vwid->func->see_ttatlas_bbox->wrowcol , False ) ;
#endif

   /*------------------------------------*/
   /*--- May 1996: Time index control ---*/
   /*--- Mar 1997: Allow FIM also     ---*/

#if 0
   top = DSET_NUM_TIMES(im3d->anat_now) ;
   if( ISVALID_3DIM_DATASET(im3d->fim_now) )
      top = MAX( top , DSET_NUM_TIMES(im3d->fim_now) ) ;
#else
   top = DSET_NVALS(im3d->anat_now) ;
   if( ISVALID_3DIM_DATASET(im3d->fim_now) )
      top = MAX( top , DSET_NVALS(im3d->fim_now) ) ;
#endif

   if( top > 1 ){
     MCW_arrowval *tav = im3d->vwid->imag->time_index_av ;
STATUS(" -- turning time index control on") ;

     AV_SENSITIZE( tav , True ) ; im3d->vinfo->time_on = 1 ;
     tav->fmax = tav->imax = top - 1 ; im3d->vinfo->top_index = top ;
     if( im3d->vinfo->time_index > tav->imax ){
       im3d->vinfo->time_index = tav->imax ;
       AV_assign_ival( tav , tav->imax ) ;
       AFNI_process_timeindex(im3d) ;       /* 29 Jan 2003 */
     }
   } else {
STATUS(" -- turning time index control off") ;
     AV_SENSITIZE( im3d->vwid->imag->time_index_av , False ) ;
     im3d->vinfo->time_on = 0 ;
   }

   /*--------------------------------------------------------------*/
   /*--- 19 Nov 1996: Set FIM-able dataset to this, if possible ---*/

   if( DSET_GRAPHABLE(im3d->anat_now) )
     im3d->fimdata->fimdset = im3d->anat_now ;

   ALLOW_COMPUTE_FIM(im3d) ;

   /*----------------------------------------------------------------*/
   /*--- 19 Aug 2002: enable/disable surface chooser button, etc. ---*/

   AFNI_update_surface_widgets( im3d ) ;

   /*------------------------------------------*/
   /*--- attach to viewing windows (if any) ---*/

   AFNI_underlay_CB( NULL , im3d , NULL ) ;

   /* 04 Nov 2003: set range for image grayscaling? */

   AFNI_range_setter( im3d , im3d->s123 ) ;
   AFNI_range_setter( im3d , im3d->s231 ) ;
   AFNI_range_setter( im3d , im3d->s312 ) ;

   im3d->vinfo->tempflag = 0 ;

   old_im3d = im3d ;
   old_fim  = im3d->fim_now ;   /* remembrance */
   old_anat = im3d->anat_now ;

   old_anat_nvals = DSET_NVALS(im3d->anat_now) ; /* 21 Jul 2009 */

   /* This next line can fail if fim_now is NULL.
   fim_now can be NULL if you switch to tlrc view and
   for some reason, the transform fails on the fim image.
   We have seen this happen when:
      1st volume in list is anat and has a +orig only.
      2nd volume in list is anat2 and has a +orig and +tlrc
      3rd volume in list is a functional dset  (selected as overlay)
   You switch to anat2 and select TLRC view, BOOM.
   Our guess is that since the first dset has no tlrc xform,
   the third one, which is the overlay seems to get no TLRC daddy,
   even if the second anat has a tlrc xform .
   Should be able to test this hypothesis by expressly setting the
   anat parent of all volumes to that of the anat with the TLRC xform....

      ZSS, RICKR, with no time to fix this quite yet.    July 28 2009 */

   /** 10 Aug 2009: the fix is below -- RWCox **/

   if( ISVALID_DSET(im3d->fim_now) )
     old_func_nvals = DSET_NVALS(im3d->fim_now) ;
   else
     old_func_nvals = -1 ;

   /* DRG 25 Apr 2016  */
   /*   extra fix for percentile flag not working with warp-on-demand switch views */
   if(DSET_ONDISK(im3d->fim_now)) {
     MCW_set_bbox( im3d->vwid->func->perc_bbox ,
                 (im3d->cont_perc_thr) ? (1) : (0) ) ;
      SENSITIZE( im3d->vwid->func->perc_bbox->wbut[PERC_AUTOBUT] , TRUE ) ;
   }
   else {
      MCW_set_bbox( im3d->vwid->func->perc_bbox , 0 ) ;
      im3d->cont_perc_thr = 0;
      SENSITIZE( im3d->vwid->func->perc_bbox->wbut[PERC_AUTOBUT] , False ) ;
   }

   IM3D_CLEAR_THRSTAT(im3d) ;    /* 12 Jun 2014 */
   AFNI_setup_thrstat(im3d,1) ;  /* 27 Jun 2019 */

   AFNI_sleep(13) ;              /* 18 Oct 2005: for luck */

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Tell if AFNI_transform_vector can take a vector from old_dset
  to new_dset coordinates -- 09 Jul 2001 -- RWCox.
-------------------------------------------------------------------------*/

int AFNI_can_transform_vector( THD_3dim_dataset *old_dset ,
                               THD_3dim_dataset *new_dset  )
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

THD_fvec3 AFNI_transform_vector( THD_3dim_dataset *old_dset ,
                                 THD_fvec3 old_fv ,
                                 THD_3dim_dataset *new_dset  )
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

   /*-- 28 Aug 2002: see if there is a special warp between datasets --*/

   { THD_warp *swarp = AFNI_find_warp(new_dset,old_dset) ;
     if( swarp != NULL ) return AFNI_forward_warp_vector( swarp , old_fv ) ;
     swarp = AFNI_find_warp(old_dset,new_dset) ;
     if( swarp != NULL ) return AFNI_backward_warp_vector( swarp, old_fv ) ;
   }

   /*-- default is no change --*/

   return old_fv ;
}

/*------------------------------------------------------------------------
  09 May 2001: fix a Solaris stupidity, where the scale is resized
               improperly when the Define Function panel is opened!
--------------------------------------------------------------------------*/

#ifdef FIX_SCALE_SIZE_LATER
static void fixscale( XtPointer client_data , XtIntervalId *id )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   AFNI_fix_scale_size_direct(im3d) ;  /* 03 Jun 2019 */
#if 0
   XtVaSetValues( im3d->vwid->func->thr_scale , XmNscaleWidth,24 , NULL ) ;
#endif
}
#endif

/*------------------------------------------------------------------------*/

void AFNI_define_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   int vwarp ;

ENTRY("AFNI_define_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /*-----  define marks panel -----*/

   if( w == im3d->vwid->view->define_marks_pb ){

      AFNI_viewing_widgets  *view  = im3d->vwid->view  ;
      AFNI_marks_widgets    *marks = im3d->vwid->marks ;

      if( XtIsManaged(marks->frame) == True ){  /* close it down */

STATUS("closing marks") ;

         AFNI_marks_action_CB( NULL , (XtPointer) im3d , NULL ) ;

      } else {                                  /* open it up */

        static int first=1 ;  /* 21 Sep 2007 */

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
                            (RwcBoolean) ISVALID_VIEW(vwarp) ) ;
            SENSITIZE( marks->tlrc_big_bbox->wrowcol ,
                       (RwcBoolean) (vwarp==VIEW_TALAIRACH_TYPE) ) ;
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

         if( first && im3d->anat_now->markers == NULL ){ /* 21 Sep 2007 */
           first = 0 ;
           (void)MCW_popup_message(
                  im3d->vwid->view->define_marks_pb ,
                    " \n"
                    " No Talairach markers defined for Underlay. \n"
                    " You can add markers to a dataset via\n"
                    "   3drefit -markers datasetname\n"
                    " and then re-starting this AFNI program.\n" ,
                  MCW_USER_KILL | MCW_TIMER_KILL ) ;
         }
      }

      EXRETURN ;
   }

   /*----- define function panel -----*/

   if( w == im3d->vwid->view->define_func_pb ){
      AFNI_viewing_widgets  *view  = im3d->vwid->view  ;
      AFNI_function_widgets *func  = im3d->vwid->func ;

      if( XtIsManaged(func->frame) ){

STATUS("closing function") ;

         CLOSE_PANEL(im3d,func) ;
      } else {

STATUS("opening function" ) ;

#ifndef USING_LESSTIF
#define REMANAGE_FUNC
#endif

#ifdef REMANAGE_FUNC
STATUS("unmanaging children") ;
         XtUnmanageChild( im3d->vwid->func->rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->thr_rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->inten_rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->options_rowcol ) ;
#endif

STATUS("opening panel") ;
         OPEN_PANEL(im3d,func) ;

#ifdef REMANAGE_FUNC
STATUS("remanaging children") ;
         XtManageChild( im3d->vwid->func->thr_rowcol ) ;
         XtManageChild( im3d->vwid->func->inten_rowcol ) ;
         XtManageChild( im3d->vwid->func->options_rowcol ) ;
         XtManageChild( im3d->vwid->func->rowcol ) ;
#endif
         HIDE_SCALE(im3d) ;
         im3d->vwid->func->inten_pbar->update_me = 2 ;
         update_MCW_pbar( im3d->vwid->func->inten_pbar ) ;
         FIX_SCALE_SIZE(im3d) ; FIX_SCALE_VALUE(im3d) ;

        /* first time open? do some setup finalization [24 May 2019] */
        if( im3d->vwid->func->do_setup ){
          int ii = (int)AFNI_numenv("AFNI_THRESH_INIT_EXPON") ;
          if( ii > 0 && ii < THR_top_expon ) AFNI_set_thresh_itop(im3d,ii) ;
          im3d->vwid->func->do_setup = 0 ;
        }

#ifdef FIX_SCALE_SIZE_LATER
        (void) XtAppAddTimeOut( MAIN_app,50,fixscale,im3d ) ; /* 09 May 2001 */
#else
         AFNI_fix_scale_size_direct(im3d) ;  /* 03 Jun 2019 */
#endif

/***     XtManageChild( im3d->vwid->func->inten_bbox->wrowcol ) ; ***/
      }

      EXRETURN ;
   }

   /*-- define datamode panel --*/

   if( w == im3d->vwid->view->define_dmode_pb ){
      AFNI_viewing_widgets  *view  = im3d->vwid->view  ;
      AFNI_datamode_widgets *dmode = im3d->vwid->dmode ;

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
   Three_D_View *im3d = (Three_D_View *) client_data ;
   AFNI_marks_widgets *marks ;
   int bval , vwarp ;
   RwcBoolean transformable ;

ENTRY("AFNI_marks_edits_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

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
#ifdef POPTOG
   SENSITIZE( marks->pop_set_pb        , marks->editable ) ;
   SENSITIZE( marks->pop_clear_pb      , marks->editable ) ;
#endif
   SENSITIZE( marks->action_quality_pb , transformable ) ;
   SENSITIZE( marks->transform_pb      , False ) ;  /* require QC first */

   if( ! marks->editable ){
      AFNI_set_tog( -1 , MARKS_MAXNUM , marks->tog ) ;    /* none will */
#ifdef POPTOG
      AFNI_set_tog( -1 , MARKS_MAXNUM , marks->poptog ) ; /* be "on" */
#endif
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_see_marks_CB( Widget w ,
                        XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   AFNI_marks_widgets *marks ;
   AFNI_viewing_widgets *view ;
   int bval ;

ENTRY("AFNI_see_marks_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   view  = im3d->vwid->view ;
   marks = im3d->vwid->marks ;
   bval  = MCW_val_bbox( view->see_marks_bbox ) ;

   marks->tag_visible = marks->ov_visible = (bval == 0) ? (False) : (True) ;

   if( w != NULL )
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------
  Event handler to find #3 button press for crosshair label [12 Mar 2004]
--------------------------------------------------------------------------*/

void AFNI_crosshair_EV( Widget w , XtPointer cd ,
                        XEvent *ev , RwcBoolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_crosshair_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /*** handle events ***/

   switch( ev->type ){

     /*----- take button press -----*/

     case ButtonPress:{
       XButtonEvent *event = (XButtonEvent *)ev ;

       if( event->button == Button3 ||
           (event->button == Button1 &&
            (event->state & (ShiftMask|ControlMask))) ){

         im3d->vwid->butx = event->x_root ;  /* 17 May 2005 */
         im3d->vwid->buty = event->y_root ;
         event->button    = Button3 ;                                /* fake  */
         XmMenuPosition( im3d->vwid->imag->crosshair_menu , event ); /* where */
         XtManageChild ( im3d->vwid->imag->crosshair_menu ) ;        /* popup */
       }

       else if( event->button == Button1 ) {
         static int nn=0 ;
         if( nn < 9 )
          (void) MCW_popup_message( im3d->vwid->imag->crosshair_label ,
                                   " The road goes ever on and on\n"
                                   " Out from the door from where it began.\n"
                                   " Now, far ahead the road has gone\n"
                                   " And I must follow if I can.\n"
                                   " Pursuing it with eager feet\n"
                                   " Until it meets some other way\n"
                                   " Where many paths and errands meet\n"
                                   " And whither then I cannot say." ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
         nn++ ;
       }
     }
     break ;

     /*----- take key press -----*/

#if 0
     case KeyPress:{
       XKeyEvent *event = (XKeyEvent *) ev ;
       char      buf[32] ;
       KeySym    ks ;
       int       nbuf ;

       buf[0] = '\0' ;
       nbuf = XLookupString( event , buf , 32 , &ks , NULL ) ;

       switch( buf[0] ){
         default: break ;
       }
     }
     break ;
#endif
   }

   EXRETURN ;
}

/*------------------------------------------------------------------*/
/* Redraw the crosshair label in this controller [12 Mar 2004]
--------------------------------------------------------------------*/

void AFNI_crosshair_relabel( Three_D_View *im3d )
{
   XmString xstr ;
   RwcBoolean same ;

ENTRY("AFNI_crosshair_relabel") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   xstr = AFNI_crosshair_label( im3d ) ; if( xstr == NULL ) EXRETURN ;
   if( im3d->vinfo->old_crosshair_label == (XmString)NULL ) /* shouldn't */
     same = False ;                                         /* happen   */
   else
     same = XmStringCompare( xstr , im3d->vinfo->old_crosshair_label ) ;

   if( same == False ){
      XtVaSetValues( im3d->vwid->imag->crosshair_label ,       /* redisplay */
                        XmNlabelString , xstr ,                /* if changed */
                     NULL ) ;
      MCW_expose_widget( im3d->vwid->imag->crosshair_label ) ; /* redraw now! */
      if( im3d->vinfo->old_crosshair_label != (XmString)NULL )
        XmStringFree(im3d->vinfo->old_crosshair_label) ;       /* toss old */
      im3d->vinfo->old_crosshair_label = xstr ;                /* new old */
   } else {
      XmStringFree( xstr ) ;  /* was same --> don't need this copy */
   }

   EXRETURN ;
}

/*------------------------------------------------------------------
  callback for crosshair label popup menu [12 Mar 2004]
--------------------------------------------------------------------*/

static char *last_jumpto_xyz_string = NULL ;  /* 23 Sep 2008 */
static char *last_jumpto_ijk_string = NULL ;
static char *last_mnito_string      = NULL ;
static char *last_sumato_string     = NULL ;
static char jumpstring[128];                  /* 13 Jun 2014 */

static char *last_jumpto_ijk_olay_string = NULL ;  /* 20 Apr 2016 */

void AFNI_crosshair_pop_CB( Widget w ,
                            XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   static char *cord_dicom="RAI" , *cord_spm="LPI" ;
   char *val=NULL ;
   int ii ;

ENTRY("AFNI_crosshair_pop_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   /*-- jumpto stuff added 01 Aug 2011 --*/

   if( w == im3d->vwid->imag->crosshair_jtxyz_pb ){
     char tbuf[128] ;
     sprintf(tbuf , "Enter new x y z (%s mm):" , GLOBAL_library.cord.orcode ) ;
     MCW_choose_string( im3d->vwid->imag->crosshair_label , tbuf ,
                        last_jumpto_xyz_string,
                        (gen_func *)AFNI_jumpto_CB, (XtPointer) im3d ) ;
     EXRETURN ;
   } else if ( w == im3d->vwid->imag->crosshair_jtijk_pb ){
     MCW_choose_string( im3d->vwid->imag->crosshair_label , "Enter new i j k (UnderLay):" ,
                        last_jumpto_ijk_string ,
                        (gen_func *)AFNI_jumpto_ijk_CB , (XtPointer) im3d ) ;
     EXRETURN ;
   } else if ( w == im3d->vwid->imag->crosshair_jtijk_olay_pb ){  /* 20 Apr 2016 */
     MCW_choose_string( im3d->vwid->imag->crosshair_label , "Enter new i j k (OverLay):" ,
                        last_jumpto_ijk_olay_string ,
                        (gen_func *)AFNI_jumpto_ijk_olay_CB , (XtPointer) im3d ) ;
     EXRETURN ;
   }

   /*-- the other options --*/

        if( w == im3d->vwid->imag->crosshair_dicom_pb ) val = cord_dicom ;
   else if( w == im3d->vwid->imag->crosshair_spm_pb   ) val = cord_spm   ;
   else if( w == im3d->vwid->imag->crosshair_ijk_pb   ){  /* 04 Oct 2010 */
     im3d->vinfo->show_voxind = 1 ;
     POPDOWN_string_chooser ;
     AFNI_crosshair_relabel(im3d) ;
     MCW_set_bbox(im3d->vwid->dmode->misc_voxind_bbox,1) ;
   }

   if( val != NULL ){
     POPDOWN_string_chooser ;
     MCW_strncpy(GLOBAL_argopt.orient_code,val,4) ;
     THD_coorder_fill( GLOBAL_argopt.orient_code , &GLOBAL_library.cord ) ;
     im3d->vinfo->show_voxind = 0 ;
     MCW_set_bbox(im3d->vwid->dmode->misc_voxind_bbox,0) ;
     for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
       AFNI_crosshair_relabel  ( GLOBAL_library.controllers[ii] );
       AFNI_clus_update_widgets( GLOBAL_library.controllers[ii] ); /* 21 Dec 2007 */
     }
   }

   EXRETURN ;
}

/*------------------------------------------------------------------
  callback for non-marker buttons on the popup
--------------------------------------------------------------------*/

void AFNI_imag_pop_CB( Widget w ,
                       XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   MCW_imseq *seq=NULL ;

ENTRY("AFNI_imag_pop_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   XtVaGetValues( im3d->vwid->imag->popmenu, XmNuserData, &seq, NULL ) ;
   AFNI_view_setter(im3d,seq) ;

   /*-- jump back to old location --*/

   if( w == im3d->vwid->imag->pop_jumpback_pb ){
     int ii,jj,kk ;

     ii = im3d->vinfo->i1_old ;  /* extract old place */
     jj = im3d->vinfo->j2_old ;
     kk = im3d->vinfo->k3_old ;

     SAVE_VPT(im3d) ;  /* save current place as old one */
     AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_OVERLAY ) ; /* jump */
   }

   /*-- switch window display mode --*/

   else if( w == im3d->vwid->imag->pop_imageonly_pb ){
      if( ISQ_REALZ(seq) )
         drive_MCW_imseq( seq , isqDR_onoffwid , (XtPointer) isqDR_togwid ) ;
   }

   /*-- jump to a point --*/

   else if( w == im3d->vwid->imag->pop_jumpto_pb &&
            im3d->type == AFNI_3DDATA_VIEW         ){

      char tbuf[128] ;

      if( ISQ_REALZ(seq) ){
        sprintf(tbuf , "Enter new x y z (%s mm):" , GLOBAL_library.cord.orcode ) ;
        MCW_choose_string( seq->wbar , tbuf , last_jumpto_xyz_string ,
                           (gen_func *)AFNI_jumpto_CB , (XtPointer) im3d ) ;
      }
   }

   else if( w == im3d->vwid->imag->pop_jumpto_ijk_pb &&
            im3d->type == AFNI_3DDATA_VIEW             ){

      if( ISQ_REALZ(seq) ){
         MCW_choose_string( seq->wbar , "Enter new i j k (UnderLay):" , last_jumpto_ijk_string ,
                            (gen_func *)AFNI_jumpto_ijk_CB , (XtPointer) im3d ) ;
      }
   }

   else if( w == im3d->vwid->imag->pop_jumpto_ijk_olay_pb &&
            im3d->type == AFNI_3DDATA_VIEW                  ){  /* 20 Apr 2016 */

      if( ISQ_REALZ(seq) ){
         MCW_choose_string( seq->wbar , "Enter new i j k (OverLay):" , last_jumpto_ijk_olay_string ,
                            (gen_func *)AFNI_jumpto_ijk_olay_CB , (XtPointer) im3d ) ;
      }
   }

   /*-- 01 May 2002: jump to MNI coordinates --*/

   else if( w == im3d->vwid->imag->pop_mnito_pb &&
            im3d->type == AFNI_3DDATA_VIEW        ){
      /* configure menu to show what space coordinates might be in */
      if( ISQ_REALZ(seq) && CAN_TALTO(im3d) ){
         sprintf(jumpstring,"Enter %s x,y,z (LPI mm):", get_jump_space());
         MCW_choose_string( seq->wbar , jumpstring ,
                            last_mnito_string ,
                            (gen_func *)AFNI_mnito_CB , (XtPointer) im3d ) ;
      } else {
         BEEPIT ; /* should never happen */
      }
   }

   /*-- 19 Oct 2012: jump to cluster peak --*/

   else if( w == im3d->vwid->imag->pop_jumpto_clus_pb &&
            im3d->type == AFNI_3DDATA_VIEW            ){
     AFNI_jumpto_clus(im3d) ;
   }

   /*-- 06 Mar 2002: jump to a node in a surface --*/

   else if( w == im3d->vwid->imag->pop_sumato_pb &&
            SESSION_HAS_SUMA(im3d->ss_now)       &&
            im3d->type == AFNI_3DDATA_VIEW         ){

      if( ISQ_REALZ(seq) ){
         MCW_choose_string( seq->wbar , "Enter SUMA node ID:" , last_sumato_string ,
                            (gen_func *)AFNI_sumato_CB , (XtPointer) im3d ) ;
      }
   }

   /*-- jump to a predetermined atlas-based anatomical reference point --*/

   else if( w == im3d->vwid->imag->pop_talto_pb &&
            im3d->type == AFNI_3DDATA_VIEW      &&
            CAN_TALTO(im3d)                       ){

      {  /* initialize labels */
         char **at_labels=NULL ;
         int iii , flipxy=(GLOBAL_library.cord.xxsign < 0 && GLOBAL_library.cord.yysign < 0);
         char title_str[256];

         at_labels = atlas_chooser_formatted_labels( Current_Atlas_Default_Name() , flipxy ) ;
         if( ISQ_REALZ(seq) && at_labels ){
           if( AFNI_yesenv("AFNI_DATASET_BROWSE") ) MCW_set_browse_select(1) ;

           sprintf(title_str, "Brain Structure (%s) [%s]",
                        Current_Atlas_Default_Name() ,
                        (flipxy) ? "SPM order" : "DICOM order"
                   ) ;
           MCW_choose_strlist( seq->wbar ,
                       title_str ,
                       atlas_n_points(Current_Atlas_Default_Name()) ,
                       atlas_current_structure ,
                       at_labels ,
                       (gen_func *)AFNI_talto_CB , (XtPointer) im3d ) ;
            for (iii=0; iii<atlas_n_points(Current_Atlas_Default_Name()); ++iii) {
               if (at_labels[iii]) free(at_labels[iii]);
            }
            free(at_labels); at_labels=NULL;
         }
      }
   }

   /*---- 10 Jul 2001: Talairach "Where Am I?" ----*/

   else if( w == im3d->vwid->imag->pop_whereami_pb &&
            w != NULL                              &&
            im3d->type == AFNI_3DDATA_VIEW         &&
            CAN_TALTO(im3d)                          ){

      char *tlab ;
      tlab = AFNI_ttatlas_query( im3d ) ;
      if (AFNI_wami_output_mode() == 0) { /* old spice */

      /*- if one is already open, kill it -*/

      if( im3d->vwid->imag->pop_whereami_twin != NULL ){
         MCW_textwinkill_CB(NULL,
                            (XtPointer)im3d->vwid->imag->pop_whereami_twin,NULL);
         im3d->vwid->imag->pop_whereami_twin = NULL ;
      }

      /*- get TT atlas location, if any -*/

      /*- open a window to show it -*/

      if( tlab != NULL ){
         char *eee = getenv("AFNI_TTATLAS_FONTSIZE") ;

         if( eee != NULL ){
                if( *eee == 'B' || *eee == 'b' || isdigit(*eee) ) MCW_textwin_setbig( 1);
           else if( *eee == 'S' || *eee == 's' || *eee == '-'   ) MCW_textwin_setbig(-1);
         }

         im3d->vwid->imag->pop_whereami_twin =
           new_MCW_textwin_2001( im3d->vwid->imag->crosshair_label , tlab ,
               TEXT_READONLY , (void_func *)AFNI_pop_whereami_kill , im3d ) ;

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
          "according to a set of atlases defined in AFNI_atlas_spaces.niml\n"
          "and limited by the environment variable,AFNI_ATLAS_LIST.\n"
          "The default list includes the original Talairach Daemon database\n"
          "(kindly provided by Jack Lancaster and Peter Fox of RIC UTHSCSA,\n"
          "the cytoarchitectonic and macrolabel atlases provided by Simon\n"
          "Eickhoff and Karl Zilles, and the probabilistic atlases provided\n"
          "Rutvik Desai.\n"
          "\n"
          "The search is conducted outwards from the focus point, until\n"
          "9 different structures are found, or a 7.5 mm radius is reached,\n"
          "whichever occurs first. (Distances are rounded to nearest 1 mm,\n"
          "by default.) The defaults can be adjusted with AFNI environment\n"
          "Please see whereami_afni -help, README.environment for more\n"
          "details.\n"
          "\n"
          "In the database, voxels may have multiple labels for a particular\n"
          "atlas. For example, the voxels may a larger scale 'gyral' name\n"
          "and a finer scale 'area' name.  Locations that are doubly labeled\n"
          "will appear with a listing like\n"
          "    Within 2 mm: Right Precuneus -AND- Right Brodmann area 31\n"
          "A list of all the labels for the principal default atlas is\n"
          "presented by the 'Go to atlas location' control or from the\n"
          "command line program, 'whereami_afni -show_atlas_code', for any\n"
          "atlas.\n"
          "Note Very Well:\n"
          "* The Atlas is only useful as a ROUGH guide to determining where\n"
          "    you are in any individual brain.  Do not rely exclusively on\n"
          "    the Atlas for brain region identification: you must use your\n"
          "    knowledge, skills, and abilities as well.\n"
          "* Do NOT use this feature for surgical or therapeutic planning!!!"
         ) ;
      }
      } else { /* web */
         AFNI_alter_wami_text(im3d, tlab);
      } /* web */
      if (tlab) free(tlab) ;
   }

   /*---- 12 Jul 2001 ----*/

   else if( w == im3d->vwid->imag->pop_ttren_pb           &&
            w != NULL                                     &&
            im3d->type == AFNI_3DDATA_VIEW                &&
            im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE && /* 01 Aug 2001 */
            CAN_TALTO(im3d)                                 ){

      TTRR_popup( im3d ) ;

      if( im3d->vinfo->see_ttatlas == 0 ){  /* 03 Aug 2007: for the Daniel */
        MCW_set_bbox( im3d->vwid->func->see_ttatlas_bbox , 1 ) ;
        im3d->vinfo->see_ttatlas = 1 ;
      }
   }

   /*---- 05 Nov 2003: start the Edit Environment pseudo-plugin ----*/

   else if( w == im3d->vwid->imag->pop_environment_pb &&
            w != NULL                                   ){

     AFNI_misc_CB( im3d->vwid->dmode->misc_environ_pb ,
                   (XtPointer) im3d , (XtPointer) NULL ) ;
   }

   /*---- 17 May 2005: open Draw Dataset plugin ----*/

   else if( w == im3d->vwid->imag->pop_drawdataset_pb &&
            w != NULL                                   ){

     char cmd[128] , cc='A'+AFNI_controller_index(im3d) ;
     int xx,yy ;

#if 0
     MCW_widget_geom(im3d->vwid->top_shell,NULL,NULL,&xx,&yy); xx+=29; yy+=19;
#else
     xx = im3d->vwid->butx ; yy = im3d->vwid->buty ;
#endif
     sprintf(cmd,"OPEN_WINDOW %c.plugin.Draw_Dataset geom=+%d+%d",cc,xx,yy) ;
     (void) AFNI_driver(cmd) ;
   }

   /*---- 06 May 2009: set InstaCorr point ----*/

   else if( w == im3d->vwid->imag->pop_instacorr_pb && w != NULL ){
     int qq = AFNI_icor_setref(im3d) ;
     if( qq > 0 && im3d->giset == NULL ) AFNI_icor_setref_locked(im3d) ; /* 15 May 2009 */
   }

   /*---- 08 May 2009: jump to InstaCorr point ----*/

   else if( w == im3d->vwid->imag->pop_icorrjump_pb &&
            w != NULL && ISREADY_EITHER_ICOR(im3d) ){
     int ii,jj,kk ;

     ii = im3d->vinfo->i1_icor ;  /* extract icor place */
     jj = im3d->vinfo->j2_icor ;
     kk = im3d->vinfo->k3_icor ;
     if( ii >= 0 && jj >= 0 && kk >=0 ){
       SAVE_VPT(im3d) ;
#if 0  /* OLD */
       AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_OVERLAY ) ;
#else  /* NEW [27 Sep 2021] */
       AFNI_jumpto_dicom( im3d, im3d->vinfo->xi_icor,im3d->vinfo->yj_icor,im3d->vinfo->zk_icor) ;
#endif
     }
   }

   /*---- Apr 2013: set 3dGroupInCorr Apair point ----*/

   else if( w == im3d->vwid->imag->pop_icorrapair_pb && w != NULL ){
     AFNI_gicor_setapair_xyz( im3d , im3d->vinfo->xi ,
                                     im3d->vinfo->yj , im3d->vinfo->zk ) ;
   }

   else if( w == im3d->vwid->imag->pop_icorramirr_pb && w != NULL ){
     if( im3d->giset != NULL && GICOR_apair_allow_bit(im3d->giset) ){
       GICOR_flip_apair_mirror_bit(im3d->giset) ;
       MCW_set_widget_label( w , GICOR_apair_mirror_bit(im3d->giset)
                                 ? "GIC: Apair MirrorON*"
                                 : "GIC: Apair MirrorOFF" ) ;
       SENSITIZE_INSTACORR_GROUP(im3d,im3d->giset->ready) ;
     }
   }

   /*--- unmap of the popup itself [elided] ---*/

   /*--- exit ---*/

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   called when the talto chooser is set - now "Go to Atlas position"
   for generic atlas usage
-----------------------------------------------------------------------*/

void AFNI_talto_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   THD_dataxes  *daxes ;
   float xx,yy,zz ;
   int nn , ii,jj,kk ;
   ATLAS_POINT *tto_list=NULL;
   THD_fvec3 fv,tv ; THD_ivec3 iv ;

ENTRY("AFNI_talto_CB") ;

   /* check for errors */

   if( ! IM3D_OPEN(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;

   if( !CAN_TALTO(im3d)             ||
       cbs->reason != mcwCR_integer   ){

      POPDOWN_strlist_chooser ;
      BEEPIT ; WARNING_message("Can't 'Go to Atlas position'!?") ;
      EXRETURN ;
   }

   if (!(tto_list = atlas_points(Current_Atlas_Default_Name()))) {
      BEEPIT ; WARNING_message("Can't get atlas: %s", Current_Atlas_Default_Name()) ;
      EXRETURN ;
   }

   nn = cbs->ival ;
   if( nn < 0 || nn >= atlas_n_points(Current_Atlas_Default_Name()) ) EXRETURN ;
   atlas_current_structure = nn ;  /* index for structure in list for atlas */

   /* transform point from Dicom to local coords and go there */

   xx = tto_list[nn].xx ; yy = tto_list[nn].yy ; zz = tto_list[nn].zz ;

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
      BEEPIT ; WARNING_message("Bad 'Go atlas position' coordinates!?") ;
   }
   EXRETURN ;
}

/*-------------------------------------------------------------------------
   10 Jul 2001
---------------------------------------------------------------------------*/

void AFNI_pop_whereami_kill( Three_D_View *im3d )
{
   if( im3d == NULL ) return ;

   MCW_unregister_hint( im3d->vwid->imag->pop_whereami_twin->wtext ) ;
   MCW_unregister_help( im3d->vwid->imag->pop_whereami_twin->wtext ) ;

   im3d->vwid->imag->pop_whereami_twin = NULL ;
   return ;
}


/*-------------------------------------------------------------------------
   A newer output form for whereami_afni
---------------------------------------------------------------------------*/
static int htmlwami_open        = 0 ;

void AFNI_htmlwami_killfun( XtPointer pp ){
   Three_D_View *im3d = (Three_D_View *)pp;

   if (!im3d) return;

   im3d->vwid->imag->pop_whereami_htmlwin=NULL;
   /* not there yet
   MCW_unregister_hint( im3d->vwid->imag->pop_whereami_twin->wtext ) ;
   MCW_unregister_help( im3d->vwid->imag->pop_whereami_twin->wtext ) ;
   */

   htmlwami_open = 0 ; return ;
}

void AFNI_htmlwami_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   char *uinf=(char *)cbd , *inf=NULL ; int ii ;
   MCW_htmlwin *htmlwami_hw = im3d->vwid->imag->pop_whereami_htmlwin;

   ENTRY("AFNI_htmlwami_CB") ;

   if( uinf != NULL && *uinf != '\0' ){
      inf = (char *)malloc(sizeof(char)*(strlen(uinf)+16)) ;
      strcpy(inf,"wami:") ; strcat(inf,uinf) ;
   }

   if( htmlwami_open && htmlwami_hw != NULL ){
     XMapRaised( XtDisplay(htmlwami_hw->wshell), XtWindow(htmlwami_hw->wshell)) ;
     MCW_htmlwin_alter( htmlwami_hw , inf );
     EXRETURN ;
   } else {
     htmlwami_hw = new_MCW_htmlwin( im3d->vwid->imag->topper, inf,
                      (void_func *)AFNI_htmlwami_killfun , im3d  , NULL, 0 ) ;
     im3d->vwid->imag->pop_whereami_htmlwin = htmlwami_hw;
   }
   free(inf) ; inf = NULL ; htmlwami_open = 1 ;

   EXRETURN ;
}

void AFNI_alter_wami_text(Three_D_View *im3d, char *utlab)
{
   char *tlab=NULL;

   ENTRY("AFNI_alter_wami_text");

   if (!im3d || !im3d->vwid || !im3d->vwid->imag) EXRETURN;

   if (!utlab) tlab =
   "\n** Can not use whereami_afni functionality with this dataset**<br>\n"
   "Please set AFNI_ATLAS_LIST and AFNI_TEMPLATE_SPACE_LIST appropriately<br>\n"
   "Also set the space of the dataset to a corresponding space<br>\n" ;
   else tlab = utlab;

   if (AFNI_wami_output_mode() == 0) {
      if (!im3d->vwid->imag->pop_whereami_twin) EXRETURN;
      MCW_textwin_alter( im3d->vwid->imag->pop_whereami_twin , tlab ) ;
   } else {
      AFNI_htmlwami_CB( NULL , (XtPointer)im3d , (XtPointer) tlab );
   }
   EXRETURN;
}


/*-------------------------------------------------------------------------*/

char * AFNI_ttatlas_query( Three_D_View *im3d )
{
   static int have_TT = 1 ;
   THD_3dim_dataset *dset;
   ATLAS_LIST *atlas_alist;

   if( !IM3D_OPEN(im3d) || !CAN_TALTO(im3d) ) return NULL ;

   set_TT_whereami_version(0,0);
   set_wami_web_reqtype(WAMI_WEB_STRUCT); /* set web atlas output to simple structure */

#if 0
   /*-- make sure we have the TT atlas --*/
   have_TT = 0;
   if( TT_retrieve_atlas_dset("TT_Daemon",0)){
      have_TT = 1 ;
   }
   if( !have_TT ) return NULL ;
#endif

   if( have_TT ){
     THD_fvec3 tv ; char *tlab ;

     /*-- current position --*/

     LOAD_FVEC3(tv,im3d->vinfo->xi,im3d->vinfo->yj,im3d->vinfo->zk) ;

     /*-- transform to Talairach, if needed --*/

     if( im3d->anat_now->view_type != VIEW_TALAIRACH_TYPE )
        tv = AFNI_transform_vector( im3d->anat_now , tv ,
                                    im3d->anat_dset[VIEW_TALAIRACH_TYPE] ) ;

     /*-- get result string --*/
     /* use space of "talairach view" dataset */
     /* will want to change this for flexibility not to use preset xform */
     dset = im3d->anat_dset[VIEW_TALAIRACH_TYPE];

     atlas_alist = get_G_atlas_list(); /* get the whole atlas list */

     tlab = TT_whereami( tv.xyz[0] , tv.xyz[1] , tv.xyz[2],
                         THD_get_space(dset)  , env_atlas_list() );

     return tlab ;
   }

   return NULL ;
}

/*-------------------------------------------------------------------------
  See the TT atlas in the overlay? -- 25 Jul 2001
---------------------------------------------------------------------------*/

void AFNI_see_ttatlas_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int newsee = MCW_val_bbox(im3d->vwid->func->see_ttatlas_bbox) ;

   if( newsee == im3d->vinfo->see_ttatlas ) return ;

   im3d->vinfo->see_ttatlas = newsee ;

   if( im3d->anat_now->view_type == VIEW_TALAIRACH_TYPE )
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */

   return ;
}

/*---------------------------------------------------------------------
   called when the mnito chooser is set - 01 May 2002
-----------------------------------------------------------------------*/

void AFNI_mnito_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   float xx,yy,zz, xout, yout, zout;
   char dum1[32],dum2[32];
   int nn ;
   THD_fvec3 tv ;

ENTRY("AFNI_mnito_CB") ;

   if( ! IM3D_OPEN(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;

   if( !CAN_TALTO(im3d) || cbs->reason != mcwCR_string  ){   /* error */
      POPDOWN_string_chooser ;
      BEEPIT ; WARNING_message("Can't 'MNI To'!?") ;
      EXRETURN ;
   }

   if( last_mnito_string != NULL ) free(last_mnito_string) ;
   last_mnito_string = strdup(cbs->cval) ;

   nn = sscanf( cbs->cval , "%f%[ ,]%f%[ ,]%f" , &xx,dum1,&yy,dum2,&zz ) ;
   if( nn != 5 ){ BEEPIT ; WARNING_message("bad 'MNI To' entries!?") ; EXRETURN ; }

   xx = -xx, yy = -yy;  /* LPI to RAI coordinates */
   LOAD_ANAT_VIEW(im3d) ;
/*  tv = THD_mni_to_tta( tv ) ; */ /* Talairach coords */
   /* transform the MNI coordinates to whatever space the +tlrc view is in */
   if(wami_xform_xyz(xx, yy, zz, &xout, &yout, &zout, get_jump_space(),
      THD_get_space(im3d->anat_dset[VIEW_TALAIRACH_TYPE]))!=0) {
         BEEPIT ; WARNING_message("'MNI To' failed!?") ;
   }
   LOAD_FVEC3(tv,xout,yout,zout) ;    /* load vector with new coordinates
                                         in dset's std space of tlrc view */

   /* transform from +tlrc view space to current view (maybe orig) if needed */
   if( im3d->anat_now->view_type != VIEW_TALAIRACH_TYPE )
      tv = AFNI_transform_vector( im3d->anat_dset[VIEW_TALAIRACH_TYPE] ,
                                  tv , im3d->anat_now ) ;

   nn = AFNI_jumpto_dicom( im3d , tv.xyz[0], tv.xyz[1], tv.xyz[2] ) ;
   if( nn < 0 ){ BEEPIT ; WARNING_message("'MNI To' failed!?") ; }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   called when the jumpto chooser is set
-----------------------------------------------------------------------*/

void AFNI_jumpto_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   float xx,yy,zz ;
   char dum1[32],dum2[32];
   int nn ;

ENTRY("AFNI_jumpto_CB") ;

   if( ! IM3D_OPEN(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string ) EXRETURN ;  /* error */

   if( last_jumpto_xyz_string != NULL ) free(last_jumpto_xyz_string) ;
   last_jumpto_xyz_string = strdup(cbs->cval) ;

   nn = sscanf( cbs->cval , "%f%[ ,]%f%[ ,]%f" , &xx,dum1,&yy,dum2,&zz ) ;
   if( nn != 5 ){ BEEPIT ; WARNING_message("bad Jumpto entries!?") ; EXRETURN ; }

   THD_coorder_to_dicom( &GLOBAL_library.cord , &xx,&yy,&zz ) ;

   SAVE_VPT(im3d) ;  /* save current place as old one */
   nn = AFNI_jumpto_dicom( im3d , xx,yy,zz ) ;
   if( nn < 0 ){ BEEPIT ; WARNING_message("Jumpto failed!") ; }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------*/

int AFNI_jumpto_dicom_OLD( Three_D_View *im3d , float xx, float yy, float zz )
{
   THD_dataxes  *daxes ;
   THD_fvec3 fv ; THD_ivec3 iv ;
   int ii,jj,kk ;

ENTRY("AFNI_jumpto_dicom_OLD") ;

   LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

   fv = THD_dicomm_to_3dmm( im3d->anat_now , TEMP_FVEC3(xx,yy,zz) ) ;
   iv = THD_3dmm_to_3dind ( im3d->anat_now , fv ) ;
   ii = iv.ijk[0] ; jj = iv.ijk[1] ; kk = iv.ijk[2] ;

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz ){

      AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
      RETURN(1) ;
   } else {
      BEEPIT ; WARNING_message("Jumpto DICOM failed -- bad coordinates?!") ;
      RETURN(-1) ;
   }
}

/*---------------------------------------------------------------------*/

int AFNI_jump_and_seed( Three_D_View *im3d , float xx, float yy, float zz )
{
   THD_dataxes  *daxes ;
   THD_fvec3 fv ; THD_ivec3 iv ;
   int ii,jj,kk,qq ;
   static int iil = -1, jjl = -1, kkl = -1;

ENTRY("AFNI_jump_and_seed") ;

   LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

   fv = THD_dicomm_to_3dmm( im3d->anat_now , TEMP_FVEC3(xx,yy,zz) ) ;
   iv = THD_3dmm_to_3dind ( im3d->anat_now , fv ) ;
   ii = iv.ijk[0] ; jj = iv.ijk[1] ; kk = iv.ijk[2] ;

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz ){

      /* Note that the locations of the last click should be set
         per im3d, perhaps within function AFNI_icor_setref_anatijk().
         This current static storage might fail whith multiple
         controllers.                                               */
      if (ii != iil || jj != jjl || kk != kkl) {
         DONT_TELL_SUMA;
         AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_OPTIONAL ) ;
         TELL_SUMA;
         qq = AFNI_icor_setref_anatijk(im3d,ii,jj,kk) ;
         if( qq > 0 && im3d->giset == NULL ) AFNI_icor_setref_locked(im3d) ;
         iil = ii; jjl = jj; kkl = kk;
      }
      RETURN(1) ;
   } else {
      BEEPIT ;
      WARNING_message("AFNI_icor_seed_SUMA failed -- bad coordinates?!") ;
      RETURN(-1) ;
   }
}

/*---------------------------------------------------------------------*/

int AFNI_creepto_dicom( Three_D_View *im3d , float xx, float yy, float zz )
{
   float xc,yc,zc , dxx,dyy,dzz ; int ndd,qq,ii=0 ;

ENTRY("AFNI_creepto_dicom") ;

   xc = im3d->vinfo->xi ; yc = im3d->vinfo->yj ; zc = im3d->vinfo->zk ;

   dxx = fabsf( (xx-xc) / DSET_DX(im3d->anat_now) ) ;
   dyy = fabsf( (yy-yc) / DSET_DY(im3d->anat_now) ) ;
   dzz = fabsf( (zz-zc) / DSET_DZ(im3d->anat_now) ) ;
   ndd = (int)sqrtf(dxx*dxx+dyy*dzz+dzz*dzz) ; ndd = MIN(ndd,32) ;

   SAVE_VPT(im3d) ;
   if( ndd < 2 ){ ii = AFNI_jumpto_dicom_OLD(im3d,xx,yy,zz) ; RETURN(ii) ; }

   dxx = (xx-xc) / ndd ; dyy = (yy-yc) / ndd ; dzz = (zz-zc) / ndd ;

   for( qq=ndd-1 ; qq >= 0  ; qq-- )
     ii = AFNI_jumpto_dicom_OLD( im3d , xx-dxx*qq , yy-dyy*qq , zz-dzz*qq ) ;

   RETURN(ii) ;
}

/*---------------------------------------------------------------------*/

int AFNI_jumpto_dicom( Three_D_View *im3d , float xx, float yy, float zz )
{
   int ii ;
   SAVE_VPT(im3d) ;
   if( AFNI_yesenv("AFNI_CREEPTO") )
     ii = AFNI_creepto_dicom(im3d,xx,yy,zz) ;
   else
     ii = AFNI_jumpto_dicom_OLD(im3d,xx,yy,zz) ;
   return ii ;
}

/*----------- the two functions below date to 19 Aug 1999 -------------*/

int AFNI_jumpto_ijk( Three_D_View *im3d , int ii, int jj, int kk )
{
   THD_dataxes *daxes ;

ENTRY("AFNI_jumpto_ijk") ;

   LOAD_ANAT_VIEW(im3d) ;

   daxes = CURRENT_DAXES(im3d->anat_now) ;
   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz ){

      SAVE_VPT(im3d) ;
      AFNI_set_viewpoint( im3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
      RETURN(1) ;
   } else {
      BEEPIT ; WARNING_message("Jumpto IJK failed -- bad input values?!") ;
               WARNING_message("  i range = 0 .. %d (inclusive)",daxes->nxx-1) ;
               WARNING_message("  j range = 0 .. %d (inclusive)",daxes->nyy-1) ;
               WARNING_message("  k range = 0 .. %d (inclusive)",daxes->nzz-1) ;
      RETURN(-1) ;
   }
}

/*---------------------------------------------------------------------*/

void AFNI_jumpto_thminmax_CB( Widget w , XtPointer cd , XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int ijk=-777, ii,jj,kk ;
   float xx,yy,zz ;

ENTRY("AFNI_jumpto_thminmax_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

        if( w == im3d->vwid->func->pbar_jumpto_thmax_pb )
          ijk = im3d->fim_thresh_max_ijk ;
   else if( w == im3d->vwid->func->pbar_jumpto_thmin_pb )
          ijk = im3d->fim_thresh_min_ijk ;

   if( ijk == -777 || im3d->fim_thrindex < 0 )  EXRETURN ;
   if( ijk < 0 ){ BEEPIT ; SENSITIZE(w,False) ; EXRETURN ; }

   ii = DSET_index_to_ix(im3d->fim_now,ijk) ;
   jj = DSET_index_to_jy(im3d->fim_now,ijk) ;
   kk = DSET_index_to_kz(im3d->fim_now,ijk) ;

   MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , ii,jj,kk , xx,yy,zz ) ;
   (void)AFNI_jumpto_dicom( im3d , xx,yy,zz ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------*/
/* Jump to ijk in Overlay [20 Apr 2016] */

int AFNI_jumpto_ijk_olay( Three_D_View *im3d , int ii, int jj, int kk )
{
   THD_dataxes *daxes ;

ENTRY("AFNI_jumpto_ijk_olay") ;

   if( !ISVALID_DSET(im3d->fim_now) ){ BEEPIT; RETURN(-1); }

   daxes = im3d->fim_now->daxes ;

   if( ii >= 0 && ii < daxes->nxx &&
       jj >= 0 && jj < daxes->nyy && kk >= 0 && kk < daxes->nzz ){

      THD_fvec3 fv ;
      SAVE_VPT(im3d) ;

      fv = THD_3dind_to_dicomm_no_wod( im3d->fim_now , TEMP_IVEC3(ii,jj,kk) ) ;
      RETURN( AFNI_jumpto_dicom_OLD( im3d , fv.xyz[0], fv.xyz[1], fv.xyz[2] ) ) ;
   }

   BEEPIT ; WARNING_message("Jumpto IJK (OL) failed -- bad indexes?!") ;
   RETURN(-1) ;
}

/*---------------------------------------------------------------------*/

void AFNI_jumpto_ijk_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int ii=-1,jj=-1,kk=-1 ;
   int nn ;
   char dum1[32],dum2[32];

ENTRY("AFNI_jumpto_ijk_CB") ;

   if( ! IM3D_OPEN(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string ) EXRETURN ;  /* error */

   if( last_jumpto_ijk_string != NULL ) free(last_jumpto_ijk_string) ;
   last_jumpto_ijk_string = strdup(cbs->cval) ;

   nn = sscanf( cbs->cval , "%d%[ ,]%d%[ ,]%d" , &ii,dum1,&jj,dum2,&kk ) ;
   if( nn > 0 && nn < 3 && ii >= 0 ){  /* 1D index jump [26 Jun 2009] */
     nn = ii ;
     ii = DSET_index_to_ix(im3d->anat_now,nn) ;
     jj = DSET_index_to_jy(im3d->anat_now,nn) ;
     kk = DSET_index_to_kz(im3d->anat_now,nn) ;
   } else if( nn != 5 ){
     BEEPIT; WARNING_message("Jumpto IJK failed -- bad entries?!"); EXRETURN;
   }

   nn = AFNI_jumpto_ijk( im3d , ii,jj,kk ) ;
   if( nn < 0 ) BEEPIT ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------*/

void AFNI_jumpto_ijk_olay_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int ii=-1,jj=-1,kk=-1 ;
   int nn ;
   char dum1[32],dum2[32];

ENTRY("AFNI_jumpto_ijk_olay_CB") ;

   if( ! IM3D_OPEN(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string ) EXRETURN ;  /* error */

   if( last_jumpto_ijk_olay_string != NULL ) free(last_jumpto_ijk_olay_string) ;
   last_jumpto_ijk_olay_string = strdup(cbs->cval) ;

   nn = sscanf( cbs->cval , "%d%[ ,]%d%[ ,]%d" , &ii,dum1,&jj,dum2,&kk ) ;
   if( nn > 0 && nn < 3 && ii >= 0 ){
     nn = ii ;
     ii = DSET_index_to_ix(im3d->fim_now,nn) ;
     jj = DSET_index_to_jy(im3d->fim_now,nn) ;
     kk = DSET_index_to_kz(im3d->fim_now,nn) ;
   } else if( nn != 5 ){
     BEEPIT; WARNING_message("Jumpto IJK failed -- bad entries?!"); EXRETURN;
   }

   nn = AFNI_jumpto_ijk_olay( im3d , ii,jj,kk ) ;
   if( nn < 0 ) BEEPIT ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   called when the sumato chooser is set
-----------------------------------------------------------------------*/

void AFNI_sumato_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int nn , ii ;

ENTRY("AFNI_sumato_CB") ;

   if( !IM3D_OPEN(im3d) || im3d->type != AFNI_3DDATA_VIEW ) EXRETURN ;
   if( cbs->reason != mcwCR_string )                        EXRETURN ;
   if( !SESSION_HAS_SUMA(im3d->ss_now) )                    EXRETURN ;

   if( last_sumato_string != NULL ) free(last_sumato_string) ;
   last_sumato_string = strdup(cbs->cval) ;

   nn = -1 ;
   sscanf( cbs->cval , "%d" , &nn ) ;
   ii = SUMA_find_node_id( im3d->ss_now->su_surf[0] , nn ) ;
   if( ii < 0 ){ BEEPIT; WARNING_message("SUMA To fails -- bad entry?!"); EXRETURN; }

   (void) AFNI_jumpto_dicom( im3d ,
                             im3d->ss_now->su_surf[0]->ixyz[ii].x ,
                             im3d->ss_now->su_surf[0]->ixyz[ii].y ,
                             im3d->ss_now->su_surf[0]->ixyz[ii].z  ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   Transform current dataset based on the existing set of markers
-----------------------------------------------------------------------*/

#define BEEP_AND_RETURN { BEEPIT; EXRETURN; }

void AFNI_marks_transform_CB( Widget w ,
                              XtPointer client_data , XtPointer call_data )
{
   Three_D_View     *im3d = (Three_D_View *) client_data ;
   THD_marker_set   *markers ;
   THD_warp         *warp ;
   THD_3dim_dataset *new_dset ;
   THD_session      *ss ;
   int              vnew , vvv , sss , aaa , fff , id ;
   float            resam_size ;
   Widget           wmsg ;

ENTRY("AFNI_marks_transform_CB") ;

   /*--- sanity checks ---*/

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

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

   SET_SESSION_DSET(new_dset, GLOBAL_library.sslist->ssar[sss], aaa, vnew);
/*   GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[aaa][vnew] = new_dset ;*/

   /* reload active datasets, to allow for destruction that may
      have occurred (this code is copied from AFNI_initialize_view) */

   for( id=0 ; id <= LAST_VIEW_TYPE ; id++ ){
      im3d->anat_dset[id] = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[sss], aaa, id) ;
      im3d->fim_dset[id]  = GET_SESSION_DSET(GLOBAL_library.sslist->ssar[sss], fff, id) ;
/*      im3d->anat_dset[id] = GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[aaa][id] ;
      im3d->fim_dset[id]  = GLOBAL_library.sslist->ssar[sss]->dsset_xform_table[fff][id] ;
*/
      if( ISVALID_3DIM_DATASET(im3d->anat_dset[id]) )
        SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], True ) ;
      else
        SENSITIZE( im3d->vwid->view->view_bbox->wbut[id], False) ;
   }

STATUS("writing new dataset") ;

   DSET_overwrite_header(new_dset) ;

   /*--- have transformed this anatomy dataset
         ==> if the input was the original view, then
             destroy any other marker sets in the original view
             and make their datasets anatomical children of this one ---*/

   if( im3d->vinfo->view_type == VIEW_ORIGINAL_TYPE ){
      int id ;
      THD_3dim_dataset *dss ;

      /* perform surgery on the anat datasets in this session */

STATUS("re-anat_parenting anatomical datasets in this session") ;

      for( id=0 ; id < im3d->ss_now->num_dsset ; id++ ){
         dss = GET_SESSION_DSET(im3d->ss_now, id, 0) ;
/*         dss = im3d->ss_now->dsset_xform_table[id][0] ;*/

         if( ! ISVALID_3DIM_DATASET(dss) || dss == im3d->anat_now ) continue ;

         if( dss->markers != NULL ) SINGLE_KILL(dss->kl,dss->markers) ;
         dss->markers = NULL ;

         if( dss->anat_parent == NULL ){
           dss->anat_parent = im3d->anat_now ;
           MCW_strncpy( dss->anat_parent_name ,
                        im3d->anat_now->self_name , THD_MAX_NAME ) ;
           dss->anat_parent_idcode = im3d->anat_now->idcode ;
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

THD_warp * AFNI_make_warp( Three_D_View *im3d )
{
   THD_3dim_dataset *anat    = im3d->anat_now ;
   THD_marker_set   *markers = im3d->anat_now->markers ;
   THD_warp         *warp ;
   RwcBoolean good ;

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
         THD_talairach_12_warp *twarp = (THD_talairach_12_warp *) warp ;
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
         THD_affine_warp *awarp = (THD_affine_warp *) warp ;

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

         {
            float zbot = ATLAS_ALIGNBOX_INF;
            float ztop = ATLAS_ALIGNBOX_SUP;
            float xtop = ATLAS_ALIGNBOX_LAT;
            float ytop = ATLAS_ALIGNBOX_POS;
            float ybot = ATLAS_ALIGNBOX_ANT;
            #define GETVAL(vvv,nnn) do{ char *eee = getenv(nnn) ;                            \
                            if( eee != NULL ){                                   \
                              float val=strtod(eee,NULL); if(val>0.0) vvv = val; \
                            } } while(0)

                     GETVAL(xtop,"AFNI_ACPC_BBOX_LAT") ;  /* ZSS: Apr 2007: get new bounding box */
                     GETVAL(ybot,"AFNI_ACPC_BBOX_ANT") ;  /* from environment variables, maybe */
                     GETVAL(ytop,"AFNI_ACPC_BBOX_POS") ;
                     GETVAL(zbot,"AFNI_ACPC_BBOX_INF") ;
                     GETVAL(ztop,"AFNI_ACPC_BBOX_SUP") ;

            #undef GETVAL

            LOAD_FVEC3(awarp->warp.bot,
                       -xtop, -ybot, -zbot);
            LOAD_FVEC3(awarp->warp.top,
                        xtop, ytop, ztop);
         }

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
     error_list = (char*) XtRealloc( error_list , ll ) ;      \
     strcat( error_list , "*** ERROR:  ") ;           \
     strcat( error_list , str ) ; num_error++ ; }

#define ADD_REPORT(str)                               \
   { int ll = strlen(str) + strlen(error_list) + 16 ; \
     STATUS(str) ;                                    \
     error_list = (char*)XtRealloc( error_list , ll ) ;      \
     strcat( error_list , str ) ; num_report++ ; }

RwcBoolean AFNI_marks_quality_check( RwcBoolean make_report, Three_D_View *im3d )
{
   THD_3dim_dataset *anat    = im3d->anat_now ;
   THD_marker_set   *markers = im3d->anat_now->markers ;

   char *error_list ;
   int   num_error , num_report ;
   char  msg[128] ;
   RwcBoolean good ;

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

THD_3dim_dataset * AFNI_init_warp( Three_D_View *im3d ,
                                   THD_3dim_dataset *parent_dset ,
                                   THD_warp *warp_init , float resam_vox )
{
   THD_3dim_dataset *adam_dset ;  /* the farthest ancestor */
   THD_warp         *warp_total ; /* the warp from that ancestor */
   THD_fvec3         xnew_bot , xnew_top ;

   THD_3dim_dataset *new_dset ;
   THD_datablock    *new_dblk  , *adam_dblk  , *parent_dblk ;
   THD_dataxes      *new_daxes , *adam_daxes , *parent_daxes ;
   THD_diskptr      *new_dkptr , *adam_dkptr , *parent_dkptr ;
   THD_marker_set   *new_markers ;

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
         int   use_tlrc_big=MCW_val_bbox( im3d->vwid->marks->tlrc_big_bbox ) ;
         float xtop=ATLAS_BBOX_LAT ,
               ybot=ATLAS_BBOX_ANT ,
               ytop=ATLAS_BBOX_POS ,
               zbot=(use_tlrc_big) ? ATLAS_BBOX_INF_NEW : ATLAS_BBOX_INF ,
               ztop=ATLAS_BBOX_SUP ;

#define GETVAL(vvv,nnn) do{ char *eee = getenv(nnn) ;                            \
                            if( eee != NULL ){                                   \
                              float val=strtod(eee,NULL); if(val>0.0) vvv = val; \
                            } } while(0)

         GETVAL(xtop,"AFNI_TLRC_BBOX_LAT") ;  /* 16 Apr 2002: get new bounding box */
         GETVAL(ybot,"AFNI_TLRC_BBOX_ANT") ;  /* from environment variables, maybe */
         GETVAL(ytop,"AFNI_TLRC_BBOX_POS") ;
         GETVAL(zbot,"AFNI_TLRC_BBOX_INF") ;
         GETVAL(ztop,"AFNI_TLRC_BBOX_SUP") ;

#undef GETVAL

         LOAD_FVEC3( xnew_bot ,-xtop,-ybot,-zbot ) ;
         LOAD_FVEC3( xnew_top , xtop, ytop, ztop ) ;
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
   new_dset->Label_Dtable = NULL;                  /* ZSS Feb 26 2010 */

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
   new_dset->self_warp = NULL ;    /* 26 Aug 2002 */

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

   new_dset->death_mark  = 0 ;
   new_dset->tcat_list   = 0 ;
   new_dset->tcat_num    = 0 ;
   new_dset->tcat_len    = NULL ;

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

   new_dblk->vedim = NULL ;  /* 05 Sep 2006 */

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

   if( !ISVALID_MAT44(new_daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
     THD_daxes_to_mat44( new_daxes ) ;

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

#ifdef ALLOW_DATASET_VLIST
   new_dset->pts = NULL ;
#endif

   /*----- dataset ready for warping -----*/

   PARENTIZE(new_dset,adam_dset->parent) ;

STATUS("initialization complete") ;

   RETURN( new_dset ) ;
}

/*-----------------------------------------------------------------*/

void AFNI_copy_statistics( THD_3dim_dataset *dsold , THD_3dim_dataset *dsnew )
{
   int ibr , nvold , nvnew ;
   THD_statistics *stold , *stnew ;

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
   Three_D_View *im3d ;
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
   char    *xdef ;
   Display *display ;
   int      ival , ii,jj ;
   float    fval ;
   char    *cpt ;
   char     buf[64] ;
   float    pthr[NPANE_MAX+1] ;
   int      pov[NPANE_MAX+1] ;

ENTRY("AFNI_load_defaults") ;

   if( w == NULL ){
     ERROR_message("\n** AFNI_load_defaults: NULL input widget") ;
     EXRETURN ;
   }

   display = XtDisplay( w ) ;

   /** initialize overlay color arrays from defaults **/

   for( ii=0 ; ii < DEFAULT_NCOLOVR ; ii++ ){
     INIT_colovr[ii] = XtNewString(INIT_def_colovr[ii]) ;
     INIT_labovr[ii] = XtNewString(INIT_def_labovr[ii]) ;
   }
   for( ; ii < MAX_NCOLOVR ; ii++ )
     INIT_colovr[ii] = INIT_labovr[ii] = NULL ;

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
   NAME2INT("graph_matrix"      ,INIT_GR_gmat        ,1,MAT_MAX);    /* 10 Feb 2003 */
   NAME2INT("graph_gthick"      ,INIT_GR_gthick      ,2,10);         /* 06 Oct 2004 */

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
     for( ii=FIRST_RESAM_TYPE ; ii <= LAST_RESAM_TYPE ; ii++ )
       if( strcmp(cpt,RESAM_shortstr[ii]) == 0 ) break ;

     if( ii <= LAST_RESAM_TYPE ) INIT_resam_anat = ii ;
     myXtFree(cpt) ;
   }

   cpt = NULL ;
   NAME2STRING( "resam_func" , cpt ) ;
   if( cpt != NULL ){
     for( ii=FIRST_RESAM_TYPE ; ii <= LAST_RESAM_TYPE ; ii++ )
       if( strcmp(cpt,RESAM_shortstr[ii]) == 0 ) break ;

     if( ii <= LAST_RESAM_TYPE ) INIT_resam_func = ii ;
     (char*)myXtFree(cpt) ;
   }

   cpt = NULL ;
   NAME2STRING( "resam_thr" , cpt ) ;
   if( cpt != NULL ){
     for( ii=FIRST_RESAM_TYPE ; ii <= LAST_RESAM_TYPE ; ii++ )
       if( strcmp(cpt,RESAM_shortstr[ii]) == 0 ) break ;

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

   /* start with positive panes */

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
     for( jj=1 ; jj <= ii ; jj++ )
       if( pthr[jj] == BAD || pthr[jj] >= pthr[jj-1] ) break ;
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
     for( jj=1 ; jj <= ii ; jj++ )
       if( pthr[jj] == BAD || pthr[jj] >= pthr[jj-1] ) break ;
     if( jj <= ii ) continue ;

     /* check pov for OK-ness */

     for( jj=0 ; jj < ii ; jj++ ) if( pov[jj] == BAD ) break ;
     if( jj < ii ) continue ;

     /* get to here --> load pthr and pov into arrays */

     for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_sgn[ii][jj] = pthr[jj] ;
     for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_sgn[ii][jj] = pov[jj] ;

   }

   /* 10 Jun 2002: put RGB cyclic maps in place */

#if defined(RGBCYC_COUNT) && RGBCYC_COUNT <= NPANE_MAX
   ii = RGBCYC_COUNT ;
   for( jj=0 ; jj < ii ; jj++ ) INIT_ovin_pos[ii][jj] = RGBCYC_FIRST+jj+1 ;
   for( jj=0 ; jj < ii ; jj++ ) INIT_ovin_sgn[ii][jj] = RGBCYC_FIRST+jj+1 ;
#endif

   EXRETURN ;
}

/********************************************************************/
#ifdef USE_SONNETS

void AFNI_popup_sonnet( Widget w , int ii )  /* 12 Dec 2001 */
{
   char buf[3192] ; int jj=MCW_USER_KILL ;

   if( w == NULL ) return ;

   if( ii < 1 || ii > NUM_SONNETS ){
      ii  = (lrand48()%NUM_SONNETS) + 1 ;
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
   Three_D_View *im3d = (Three_D_View *) client_data ;
   MCW_choose_cbs *cbs ;

   if( NO_frivolities || !IM3D_OPEN(im3d) ) return ;

   if( w == im3d->vwid->prog->hidden_sonnet_pb ){  /* start the process */

      MCW_choose_integer( im3d->vwid->picture ,
                          "Sonnet " ,
                          1 , NUM_SONNETS , sonnet_index+1 ,
                          (gen_func *)AFNI_sonnet_CB , (XtPointer) im3d ) ;
      return ;
   }

   /** if get to here, finish the process **/

   cbs = (MCW_choose_cbs *) call_data ;
   if( cbs->reason != mcwCR_integer ){  /* error */
      BEEPIT ; return ;
   }

   AFNI_popup_sonnet( im3d->vwid->picture , cbs->ival ) ;
   return ;
}
#endif /* USE_SONNETS */
/********************************************************************/

/*----------------------------------------------------------------------*/
/*! Put a function on the list of n-dimensional transformations
     - modified 03 Nov 1996 from just 0D transforms
     - modified 22 Apr 1997 to add int flags to each function
     - modified 31 Jan 2002 to add slice_proj for nd=-1
     - modified 21 Jul 2003 to add func_init
------------------------------------------------------------------------*/

void AFNI_register_nD_function( int nd, char *name,
                                generic_func *func, int flags )
{
   MCW_function_list *rlist ;
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

   if( num == 0 ){
     rlist->flags=NULL; rlist->labels=NULL; rlist->funcs=NULL;
     rlist->func_data=NULL; rlist->func_code=NULL; rlist->func_init=NULL;
   }

   rlist->flags = (int *) XtRealloc( (char *)rlist->flags, sizeof(int)*(num+1) ) ;

   rlist->labels = (char **) XtRealloc( (char *)rlist->labels ,
                                        sizeof(char *)*(num+1) ) ;

   rlist->funcs = (generic_func **) XtRealloc( (char *)rlist->funcs ,
                                               sizeof(generic_func *)*(num+1) ) ;

   rlist->func_data = (void **) XtRealloc( (char *)rlist->func_data ,
                                           sizeof(void *)*(num+1) ) ;

   rlist->func_code = (int *) XtRealloc( (char *)rlist->func_code, sizeof(int)*(num+1) ) ;

   rlist->func_init = (generic_func **) XtRealloc( (char *)rlist->func_init ,
                                                   sizeof(generic_func *)*(num+1) ) ;

   rlist->flags[num]     = flags ;
   rlist->labels[num]    = XtNewString(name) ;
   rlist->funcs[num]     = func ;
   rlist->func_data[num] = NULL ;
   rlist->func_code[num] = nd ;
   rlist->func_init[num] = NULL ;

   rlist->num = num+1 ;
   return ;
}

/*-------------------------------------------------------------------------*/
/*! Add init function to last registered function. This function
    should be called just after AFNI_register_nD_function(). [21 Jul 2003] */

void AFNI_register_nD_func_init( int nd , generic_func *fin )
{
   MCW_function_list *rlist ;
   int num ;

   if( fin == NULL ) return ;

   switch( nd ){
      default: return ;

      case 0: rlist = &(GLOBAL_library.registered_0D) ; break ;
      case 1: rlist = &(GLOBAL_library.registered_1D) ; break ;
      case 2: rlist = &(GLOBAL_library.registered_2D) ; break ;

      case -1: rlist= &(GLOBAL_library.registered_slice_proj) ; break ;
   }

   num = rlist->num ; if( num <= 0 ) return ;
   rlist->func_init[num-1] = fin ;
   return ;
}

#if 0
/*---------- 18 May 2000: save/get dataset index for function calls -------*/

static int dset_ijk=-1 , dset_tin=-1 ;

void AFNI_store_dset_index( int ijk , int tin )
{
   dset_ijk = ijk ; dset_tin = tin ; return ;
}

int AFNI_needs_dset_ijk(void){ return dset_ijk ; }
int AFNI_needs_dset_tin(void){ return dset_tin ; }
#endif

/*-----------------------------------------------------------------------*/
/*!  Add a timeseries to the global library.
*//*---------------------------------------------------------------------*/

void AFNI_add_timeseries( MRI_IMAGE *tsim )
{
ENTRY("AFNI_add_timeseries") ;

   if( tsim != NULL ){
      POPDOWN_timeseries_chooser ;
      ADDTO_IMARR(GLOBAL_library.timeseries,tsim) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/* N.B.: The input time series structure is destroyed!
         Never refer to it again, even to free() it!
*//*---------------------------------------------------------------------*/

void AFNI_replace_timeseries( MRI_IMAGE *tsim )
{
   int its ; MRI_IMAGE *qsim ;

ENTRY("AFNI_replace_timeseries") ;

   if( tsim == NULL ) EXRETURN ;

   its = AFNI_tsname_in_library( tsim->name ) ;
   if( its < 0 ){
    AFNI_add_timeseries(tsim); EXRETURN;
   }

   POPDOWN_timeseries_chooser ;
   qsim = IMARR_SUBIMAGE(GLOBAL_library.timeseries,its) ;
   mri_move_guts( qsim , tsim ) ;
   mri_free(tsim) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/*** New fix for scale size problem
     - via a timeout, to avoid excessive flashing of the scale widget
     - 03 Jun 2019 [RWC in Lucca]
     - 05 Jun 2019 - modified to a fixed timeout every so often
***/
/*------------------------------------------------------------------------*/

void AFNI_fix_scale_size_direct( Three_D_View *im3d )
{
   int iqqq = AFNI_controller_index(im3d) ;
   int sel_height,sel_actual ;  XtPointer sel_ptr=NULL ;
   Dimension sel_aaa ;

   if( iqqq < 0 || ! IM3D_OPEN(im3d) ) return ; /* not valid */

   if( AFNI_yesenv("AFNI_DONT_FIX_SCALE_SIZE") ) return ; /* obey orders */

   /* check if height is correct */

   XtVaGetValues( im3d->vwid->func->thr_scale ,
                  XmNuserData , &sel_ptr , NULL ) ;
   sel_height = PTOI(sel_ptr) ; /* stored in a pointer, convert to int */

   XtVaGetValues( im3d->vwid->func->thr_scale ,
                  XmNheight , &sel_aaa , NULL ) ;
   sel_actual = (int)sel_aaa ;

   /**** INFO_message("actual = %d  nominal = %d",sel_actual,sel_height) ; ****/

   if( sel_actual == sel_height ) return ;  /* it's OK */

   /* do the work */

   XtUnmanageChild(im3d->vwid->func->thr_scale) ; AFNI_sleep(1) ;

   XtVaSetValues( im3d->vwid->func->thr_scale ,
                  XmNheight , sel_height , NULL ) ;

   XtManageChild(im3d->vwid->func->thr_scale) ;

   return ;
}

#undef  FIX_TIME
#define FIX_TIME 1357  /* ms between calls */

void AFNI_fix_scale_size_timer_CB( XtPointer client_data , XtIntervalId *id )
{
   Three_D_View *im3d ;
   int iqqq ;
   int sel_height,sel_actual ;  XtPointer sel_ptr=NULL ;

   for( iqqq=0 ; iqqq < MAX_CONTROLLERS ; iqqq++ ){
     AFNI_fix_scale_size_direct(GLOBAL_library.controllers[iqqq]) ;
   }

   (void) XtAppAddTimeOut( MAIN_app , FIX_TIME ,
                           AFNI_fix_scale_size_timer_CB , NULL ) ;

   return ;
}

/* 04/06/2020 discoraj */
int AFNI_find_session( char *dname ) {

    int ic ;
    /* find session name in list of sessions (sloppy compare) */
    for( ic=0 ; ic < GLOBAL_library.sslist->num_sess ; ic++ )
       if( strstr(GLOBAL_library.sslist->ssar[ic]->sessname,dname) != NULL )
        break;

    if( ic == GLOBAL_library.sslist->num_sess ) RETURN(-1) ;

    return ic ;
}
