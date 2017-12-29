/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#undef  TTDEBUG
#undef  USE_PTHRESH

/*---------------------------- history ----------------------------
  14 Dec 2005 [rickr]
     - process entire volume at once, not in multiple pieces
     - added -voxel option (similar to the 3dANOVA progs)
     - replaced scaling work with EDIT_convert_dtype() call
  ----------------------------------------------------------------- */

/*-------------------------- global data --------------------------*/

/** inputs **/

static EDIT_options TT_edopt ;

static int   TT_paired     = 0 ;
static int   TT_pooled     = 1 ;
static float TT_pthresh    = 0.0 ;
static int   TT_use_bval   = 0 ;
static float TT_bval       = 0.0 ;
static float TT_sd1        = 0.0 ; /* 10 Oct 2007 */
static int   TT_n1         = 0.0 ; /* 10 Oct 2007 */
static int   TT_use_editor = 0 ;
static int   TT_be_quiet   = 0 ;
static int   TT_workmem    = 266 ; /* default = 266 Megabytes */
static int   TT_voxel      = -1 ;  /* 0-based (but 1-based on cmd, like ANOVA)  */

static char * TT_base_dname = NULL ;

#define MEGA  1048576  /* 2^20 */

static THD_string_array *TT_set1 = NULL ;  /* sets of dataset names */
static THD_string_array *TT_set2 = NULL ;

static char TT_session[THD_MAX_NAME]  = "./" ;
static char TT_prefix[THD_MAX_PREFIX] = "tdif" ;
static char TT_label[THD_MAX_LABEL]   = "\0" ;

static int TT_datum = ILLEGAL_TYPE ;

static char TT_dof_prefix[THD_MAX_PREFIX] = "\0" ;  /* 27 Dec 2002 */

/*--------------------------- prototypes ---------------------------*/
void TT_read_opts( int , char ** ) ;
void TT_syntax(char *) ;

/*--------------------------------------------------------------------
   read the arguments, and load the global variables
----------------------------------------------------------------------*/

#ifdef TTDEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

void TT_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   int  ival , kk ;

   INIT_EDOPT( &TT_edopt ) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** editing option? ****/

      ival = EDIT_check_argv( argc , argv , nopt , &TT_edopt ) ;
      if( ival > 0 ){
         TT_use_editor = 1 ;
         nopt += ival ; continue ;
      }

      /**** -quiet ****/

      if( strncmp(argv[nopt],"-quiet",6) == 0 ){
DUMP1 ;
         TT_be_quiet = 1 ;
         nopt++ ; continue ;
      }

      /**** -workmem megabytes ****/

      if( strncmp(argv[nopt],"-workmem",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) TT_syntax("need argument after -workmem!") ;
         ival = strtol( argv[nopt] , NULL , 10 ) ;
         if( ival <= 0 ) TT_syntax("illegal argument after -workmem!") ;
         TT_workmem = ival ;
         INFO_message("-workmem option is now obsolete\n") ;
         nopt++ ; continue ;
      }

      /**** -datum type ****/

      if( strncmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc ) TT_syntax("need an argument after -datum!") ;

         if( strcmp(argv[nopt],"short") == 0 ){
            TT_datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            TT_datum = MRI_float ;
         } else {
            char buf[256] ;
            sprintf(buf,"-datum of type '%s' is not supported in 3dttest!",
                    argv[nopt] ) ;
            TT_syntax(buf) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -voxel voxel ****/

      if( strncmp(argv[nopt],"-voxel",6) == 0 ){        /* 14 Dec 2005 [rickr] */
         if( ++nopt >= argc ) TT_syntax("need an argument after -voxel!") ;

         TT_voxel = atoi(argv[nopt]) - 1 ; /* make zero-based */
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -session dirname ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ) TT_syntax("need argument after -session!") ;
         MCW_strncpy( TT_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ) TT_syntax("need argument after -prefix!") ;
         MCW_strncpy( TT_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

#if 0
      /**** -label string ****/

      if( strncmp(argv[nopt],"-label",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ) TT_syntax("need argument after -label!") ;
         MCW_strncpy( TT_label , argv[nopt++] , THD_MAX_LABEL ) ;
         continue ;
      }
#endif

      /** -paired **/

      if( strncmp(argv[nopt],"-paired",6) == 0 ){
         TT_paired = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -unpooled **/

      if( strncmp(argv[nopt],"-unpooled",6) == 0 ){
         TT_pooled = 0 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -dof_prefix **/

      if( strncmp(argv[nopt],"-dof_prefix",6) == 0 ){  /* 27 Dec 2002 */
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ) TT_syntax("need argument after -dof_prefix!") ;
         MCW_strncpy( TT_dof_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

#ifdef USE_PTHRESH
      /** -pthresh pval **/

      if( strncmp(argv[nopt],"-pthresh",6) == 0 ){
         char * ch ;

         if( ++nopt >= argc ) TT_syntax("-pthresh needs a value!");
         TT_pthresh = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' || TT_pthresh <= 0.0 || TT_pthresh > 0.99999 )
            TT_syntax("value after -pthresh is illegal!") ;

         WARNING_message( "-pthresh not implemented yet, will ignore!") ;
         TT_pthresh = 0.0 ;

         nopt++ ; continue ;  /* skip to next arg */
      }
#endif

      /**** after this point, the options are no longer 'free floating' ****/

      /** -base1_dset dset  : similar to -base1, but bval differs by voxel **/
      /** for M Beauchamp                              23 Jul 2008 [rickr] **/

      if( strncmp(argv[nopt],"-base1_dset",8) == 0 ){
         char *ch ;

         if( ++nopt >= argc )    TT_syntax("-base1_dset needs a dastaset!");
         if( TT_use_bval == -1 ) TT_syntax("-base1_dset with -set1 illegal!");
         if( TT_use_bval ==  1 ) TT_syntax("-base1_dset with -base1 illegal!");
         TT_base_dname = argv[nopt] ;
         TT_use_bval = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -base1 bval **/

      if( strncmp(argv[nopt],"-base1",6) == 0 ){
         char *ch ;

         if( ++nopt >= argc )    TT_syntax("-base1 needs a value!");
         if( TT_use_bval == -1 ) TT_syntax("-base1 with -set1 illegal!");
         if( TT_use_bval ==  1 ) TT_syntax("-base1 with -base1_dset illegal!");
         TT_bval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' ) TT_syntax("value after -base1 is illegal!") ;
         TT_use_bval = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** [10 Oct 2007] -sdn1 sd1 n1 **/

      if( strncmp(argv[nopt],"-sdn1",5) == 0 ){
        if( ++nopt >= argc-1 )  TT_syntax("-sdn1 needs 2 values!") ;
        if( TT_use_bval == -1 ) TT_syntax("-sdn1 with -set1 illegal!") ;
        TT_sd1 = strtod( argv[nopt]   , NULL ) ;
        TT_n1  = strtod( argv[++nopt] , NULL ) ;
        if( TT_sd1 <= 0.0 ) TT_syntax("Illegal sigma after -sdn1!") ;
        if( TT_n1  <  2   ) TT_syntax("Illegal n1 after -sdn1!") ;
        nopt++ ; continue ;
      }

      /** -set1 file file ... **/

      if( strncmp(argv[nopt],"-set1",6) == 0 ){
         int eee=0 ;
         if( TT_use_bval == 1 ) TT_syntax("-set1 with -base1 illegal!");
         TT_use_bval = -1 ;
         INIT_SARR( TT_set1 ) ;
         for( kk=nopt+1 ; kk < argc ; kk++ ){
#if 1
            if( argv[kk][0] == '-' ) break ;
#else
            if( strncmp(argv[kk],"-set2",6) == 0 ) break ;
#endif
            if( strstr(argv[kk],"[") == NULL ){
              ADDTO_SARR( TT_set1 , argv[kk] ) ;
            } else {
              THD_string_array *ss = THD_multiplex_dataset(argv[kk]); int qq;
              if( ss == NULL ) ERROR_exit("Can't deal with '%s'",argv[kk]) ;
              for( qq=0 ; qq < SARR_NUM(ss) ; qq++ )
                ADDTO_SARR(TT_set1,SARR_STRING(ss,qq)) ;
              DESTROY_SARR(ss) ; eee++ ;
            }
         }

         if( kk >= argc )       TT_syntax("-set1 not followed by -set2") ;
         if( kk-1-nopt <= 0 )   TT_syntax("-set1 has no datasets after it") ;
         if( TT_set1->num < 2 ) TT_syntax("-set1 doesn't have enough datasets") ;
         if( eee ) PRINTF_SARR(TT_set1,"++ Expanded -set1:") ;
         nopt = kk ; continue ; /* skip to arg that matched -set2 */
      }

      /** -set2 file file ... */

      if( strncmp(argv[nopt],"-set2",6) == 0 ){
         int eee=0 ;
         INIT_SARR( TT_set2 ) ;
         for( kk=nopt+1 ; kk < argc ; kk++ ){
#if 1
            /* if -set2 must be last, warn the user about trailing args */
            if( argv[kk][0] == '-' ) {
              fprintf(stderr,"** have trailing arg #%d = '%s'\n", kk, argv[kk]);
              TT_syntax("-set2 must be the last option (see: 3dttest -help)");
            }
#endif
            if( strstr(argv[kk],"[") == NULL ){
              ADDTO_SARR( TT_set2 , argv[kk] ) ;
            } else {
              THD_string_array *ss = THD_multiplex_dataset(argv[kk]); int qq;
              if( ss == NULL ) ERROR_exit("Can't deal with '%s'",argv[kk]) ;
              for( qq=0 ; qq < SARR_NUM(ss) ; qq++ )
                ADDTO_SARR(TT_set2,SARR_STRING(ss,qq)) ;
              DESTROY_SARR(ss) ; eee++ ;
            }
         }
         if( TT_set2->num < 2 ) TT_syntax("-set2 doesn't have enough datasets") ;
         if( eee ) PRINTF_SARR(TT_set2,"++ Expanded -set2:") ;
         break ;  /* end of possible inputs */
      }

      /**** unknown switch ****/

      ERROR_exit("unrecognized option %s",argv[nopt]) ;

   }  /* end of loop over options */

   if( strlen(TT_label) == 0 ){
     MCW_strncpy(TT_label,TT_prefix,THD_MAX_LABEL) ;
   }

   /*--- 30 May 2007: check TT_set? for .HEAD / .BRIK duplicates ---*/

   if( TT_set1 != NULL ){
     kk = THD_check_for_duplicates( TT_set1->num , TT_set1->ar , 1 ) ;
     if( kk > 0 ) fprintf(stderr,"\n") ;
   }

   if( TT_set2 != NULL ){
     kk = THD_check_for_duplicates( TT_set2->num , TT_set2->ar , 1 ) ;
     if( kk > 0 ) fprintf(stderr,"\n") ;
   }

   /*--- check arguments for consistency ---*/

   if( TT_use_bval == 0 )
      TT_syntax("neither -base1 nor -set1 is present!") ;

   if( TT_use_bval == -1 && TT_n1 > 0 )
      TT_syntax("-sdn1 used with -set1 is illegal!") ; /* 10 Oct 2007 */

   if( TT_use_bval == -1 &&
       ( TT_set1 == NULL || TT_set1->num < 2 ) )
      TT_syntax("-set1 has too few datasets in it!") ;

   if( TT_set2 == NULL || TT_set2->num < 2 )
      TT_syntax("-set2 has too few datasets in it!") ;

   if( TT_use_bval == 1 && TT_paired == 1 )
      TT_syntax("-paired and -base1 are mutually exclusive!") ;

   if( TT_paired == 1 && TT_set1 == NULL )
      TT_syntax("-paired requires presence of -set1!") ;

   if( TT_paired == 1 && TT_set1->num != TT_set2->num ){
      char str[256] ;
      sprintf(str,"-paired requires equal size dataset collections,\n"
                  "but -set1 has %d datasets and -set2 has %d datasets" ,
              TT_set1->num , TT_set2->num ) ;
      TT_syntax(str) ;
   }

   if( TT_pooled == 0 && TT_paired == 1 )
      TT_syntax("-paired and -unpooled are mutually exclusive!") ;

   if( TT_pooled == 0 && TT_use_bval == 1 )
      TT_syntax("-base1 and -unpooled are mutually exclusive!") ;

#if 0
   if( TT_pooled == 0 && TT_n1 > 0 )
      TT_syntax("-unpooled and -sdn1 are mutually exclusive!") ;
#endif

   if( TT_pooled == 1 && TT_dof_prefix[0] != '\0' )  /* 27 Dec 2002 */
      WARNING_message("-dof_prefix is used only with -unpooled!");

#ifdef TTDEBUG
printf("*** finished with options\n") ;
#endif

   return ;
}

/*------------------------------------------------------------------*/

void TT_syntax(char * msg)
{
   if( msg != NULL ) ERROR_exit("3dttest: %s",msg) ;

   printf(
    "Gosset (Student) t-test sets of 3D datasets\n"
    "\n"
    "   * Also see the newer program 3dttest++, which lets you *\n"
    "  ** include covariates to be regressed out of the data.  **\n"
    " *** For most purposes, 3dttest++ is to be preferred over ***\n"
    "**** this program -- 3dttest will no longer be upgraded.  ****\n"
    "****------------------------------------------------------****\n"
    " *** Also consider program 3dMEMA, which can carry out a  ***\n"
    "  ** more sophisticated type of 't-test' that also takes  **\n"
    "   * into account the variance map of each input dataset. *\n"
    "\n"
    "-----------------------------------------------------------\n"
    "*********** In short: DO NOT USE THIS PROGRAM! ************\n"
    "-----------------------------------------------------------\n"
    "\n"

    "Usage 1: 3dttest [options] -set1 datasets ... -set2 datasets ...\n"
    "   for comparing the means of 2 sets of datasets (voxel by voxel).\n"
    "\n"
    "Usage 2: 3dttest [options] -base1 bval -set2 datasets ...\n"
    "   for comparing the mean of 1 set of datasets against a constant.\n"
    "\n"
    "   ** or use -base1_dset\n"
    "\n"
    "OUTPUTS:\n"
    " A single dataset is created that is the voxel-by-voxel difference\n"
    " of the mean of set2 minus the mean of set1 (or minus 'bval').\n"
    " The output dataset will be of the intensity+Ttest ('fitt') type.\n"
    " The t-statistic at each voxel can be used as an interactive\n"
    " thresholding tool in AFNI.\n"
#ifdef USE_PTHRESH
    " If the -pthresh option (below) is used to threshold for significance,\n"
    " then the t-values will NOT be stored for each voxel; that is, the output\n"
    " dataset will be of the intensity-only ('fim') type.\n"
#endif
    "\n"

    "t-TESTING OPTIONS:\n"
    "  -set1 datasets ... = Specifies the collection of datasets to put into\n"
    "                         the first set. The mean of set1 will be tested\n"
    "                         with a 2-sample t-test against the mean of set2.\n"
    "                   N.B.: -set1 and -base1 are mutually exclusive!\n"
    "  -base1 bval        = 'bval' is a numerical value that the mean of set2\n"
    "                         will be tested against with a 1-sample t-test.\n"
    "  -base1_dset DSET   = Similar to -base1, but input a dataset where bval\n"
    "                         can vary over voxels.\n"
    "  -sdn1  sd n1       = If this option is given along with '-base1', then\n"
    "                         'bval' is taken to have standard deviation 'sd'\n"
    "                         computed from 'n1' samples.  In this case, each\n"
    "                         voxel in set2 is compared to bval using a\n"
    "                         pooled-variance unpaired 2-sample t-test.\n"
    "                         [This is for Tom Johnstone; hope we meet someday.]\n"
    "  -set2 datasets ... = Specifies the collection of datasets to put into\n"
    "                         the second set.  There must be at least 2 datasets\n"
    "                         in each of set1 (if used) and set2.\n"
    "  -paired            = Specifies the use of a paired-sample t-test to\n"
    "                         compare set1 and set2.  If this option is used,\n"
    "                         set1 and set2 must have the same cardinality.\n"
    "                   N.B.: A paired test is intended for use when the set1 and set2\n"
    "                         dataset function values may be pairwise correlated.\n"
    "                         If they are in fact uncorrelated, this test has less\n"
    "                         statistical 'power' than the unpaired (default) t-test.\n"
    "                         This loss of power is the price that is paid for\n"
    "                         insurance against pairwise correlations.\n"
    "  -unpooled          = Specifies that the variance estimates for set1 and\n"
    "                         set2 be computed separately (not pooled together).\n"
    "                         This only makes sense if -paired is NOT given.\n"
    "                   N.B.: If this option is used, the number of degrees\n"
    "                         of freedom per voxel is a variable, rather\n"
    "                         than a constant.\n"
    "  -dof_prefix ddd    = If '-unpooled' is also used, then a dataset with\n"
    "                         prefix 'ddd' will be created that contains the\n"
    "                         degrees of freedom (DOF) in each voxel.\n"
    "                         You can convert the t-value in the -prefix\n"
    "                         dataset to a z-score using the -dof_prefix dataset\n"
    "                         using commands like so:\n"
    "           3dcalc -a 'pname+orig[1]' -b ddd+orig \\\n"
    "                  -datum float -prefix ddd_zz -expr 'fitt_t2z(a,b)'\n"
    "           3drefit -substatpar 0 fizt ddd_zz+orig\n"
    "                         At present, AFNI is incapable of directly dealing\n"
    "                         with datasets whose DOF parameter varies between\n"
    "                         voxels.  Converting to a z-score (with no parameters)\n"
    "                         is one way of getting around this difficulty.\n"
#ifdef USE_PTHRESH
    "  -pthresh pval      = 'pval' is a probability level (i.e., from 0 to 1)\n"
    "                         at which to threshold the output, per voxel.\n"
    "                         N.B.: NOT IMPLEMENTED YET!\n"
#endif
#if 0
    "  -workmem mega      = 'mega' specifies the number of megabytes of RAM\n"
    "                         to use for statistical workspace.  It defaults\n"
    "                         to 266.  The program will run faster if this is\n"
    "                         larger (see the NOTES section below).\n"
#endif
    "\n"
    "  -voxel voxel       = like 3dANOVA, get screen output for a given voxel.\n"
    "                         This is 1-based, as with 3dANOVA.\n"
    "\n"
    "The -base1 or -set1 command line switches must follow all other options\n"
    "(including those described below) except for the -set2 switch.\n"
    "\n"

    "INPUT EDITING OPTIONS: The same as are available in 3dmerge.\n"
    "\n"

    "OUTPUT OPTIONS: these options control the output files.\n"
    "  -session  dirname  = Write output into given directory (default=./)\n"
    "  -prefix   pname    = Use 'pname' for the output directory prefix\n"
    "                       (default=tdif)\n"
#if 0
    "  -label    string   = Use 'string' for the label in the output\n"
    "                       dataset (the label is used for switching\n"
    "                       between datasets in AFNI)\n"
#endif
    "  -datum    type     = Use 'type' to store the output difference\n"
    "                       in the means; 'type' may be short or float.\n"
    "                       How the default is determined is described\n"
    "                       in the notes below.\n"
    "\n"
    "NOTES:\n"
#if 0
    " ** To economize on memory, 3dttest makes multiple passes through\n"
    "      the input datasets.  On each pass, the entire editing process\n"
    "      will be carried out again.  For efficiency's sake, it is\n"
    "      better to carry out the editing using 3dmerge to produce\n"
    "      temporary datasets, and then run 3dttest on them.  This applies\n"
    "      with particular force if a 'blurring' option is used.\n"
    "      Note also that editing a dataset requires that it be read into\n"
    "      memory in its entirety (so that the disk file is not altered).\n"
    "      This will increase the memory needs of the program far beyond\n"
    "      the level set by the -workmem option.\n"
#endif
    " ** The input datasets are specified by their .HEAD files,\n"
    "      but their .BRIK files must exist also! This program cannot\n"
    "      'warp-on-demand' from other datasets.\n"
    " ** This program cannot deal with time-dependent or complex-valued datasets!\n"
    " ** By default, the output dataset function values will be shorts if the\n"
    "      first input dataset is byte- or short-valued; otherwise they will be\n"
    "      floats.  This behavior may be overridden using the -datum option.\n"
    " ** In the -set1/-set2 input list, you can specify a collection of\n"
    "      sub-bricks from a single dataset using a notation like\n"
    "        datasetname+orig'[5-9]'\n"
    "      (the single quotes are necessary).  If you want to use ALL the\n"
    "      sub-bricks from a multi-volume dataset, you can't just give the\n"
    "      dataset filename -- you have to use\n"
    "        datasetname+orig'[0-$]' or datasetname'[0..$]'\n"
    "      Otherwise, the program will reject the dataset as being too\n"
    "      complicated for its pitiful understanding.  [New in July 2007]\n"
   ) ;
   printf("\n" MASTER_SHORTHELP_STRING ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

/*------------------------------------------------------------------*/

static float ptable[] = { 0.5 , 0.2 , 0.1 , 0.05 , 0.01 , 0.001 , 0.0001 , 0.00001 } ;

int main( int argc , char *argv[] )
{
   int nx,ny,nz , nxyz , ii,kk , num1,num2 , num_tt=0 , iv ,
       piece , fim_offset;
   float dx,dy,dz , dxyz ,
         num1_inv=0.0 , num2_inv , num1m1_inv=0.0 , num2m1_inv , dof ,
         dd,tt,q1,q2 , f1,f2 , tt_max=0.0 ;
   THD_3dim_dataset *dset=NULL , *new_dset=NULL ;
   THD_3dim_dataset * base_dset;
   float *av1 , *av2 , *sd1 , *sd2 , *ffim , *gfim ;
   float *base_ary=NULL;

   void  *vsp ;
   void  *vdif ;           /* output mean difference */
   char  cbuf[THD_MAX_NAME] ;
   float fbuf[MAX_STAT_AUX] , fimfac ;
   int   output_datum ;
   float npiece , memuse ;

   float *dofbrik=NULL , *dofar=NULL ;
   THD_3dim_dataset *dof_dset=NULL ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) TT_syntax(NULL) ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dttest main"); machdep() ; PRINT_VERSION("3dttest") ;
   WARNING_message("----------------------------------------------------------") ;
   WARNING_message("Are VERY you sure you want to use this program?") ;
   WARNING_message("Newer program 3dttest++ should be used instead of 3dttest!") ;
   WARNING_message("----------------------------------------------------------") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dttest",argc,argv) ;

   TT_read_opts( argc , argv ) ;

   if( ! TT_be_quiet )
      printf("3dttest: t-tests of 3D datasets, by RW Cox\n") ;

   /*-- read first dataset in set2 to get dimensions, etc. --*/

   dset = THD_open_dataset( TT_set2->ar[0] ) ;  /* 20 Dec 1999  BDW */
   if( ! ISVALID_3DIM_DATASET(dset) )
     ERROR_exit("Unable to open dataset file %s",TT_set2->ar[0]);

   nx = dset->daxes->nxx ;
   ny = dset->daxes->nyy ;
   nz = dset->daxes->nzz ;         nxyz = nx * ny * nz ;
   dx = fabs(dset->daxes->xxdel) ;
   dy = fabs(dset->daxes->yydel) ;
   dz = fabs(dset->daxes->zzdel) ; dxyz = dx * dy * dz ;

#ifdef TTDEBUG
printf("*** nx=%d ny=%d nz=%d\n",nx,ny,nz) ;
#endif

   /*-- make an empty copy of this dataset, for eventual output --*/

#ifdef TTDEBUG
printf("*** making empty dataset\n") ;
#endif

   new_dset = EDIT_empty_copy( dset ) ;

   tross_Make_History( "3dttest" , argc,argv , new_dset ) ;

   strcpy( cbuf , dset->self_name ) ; strcat( cbuf , "+TT" ) ;

   iv = DSET_PRINCIPAL_VALUE(dset) ;

   if( TT_datum >= 0 ){
      output_datum = TT_datum ;
   } else {
      output_datum = DSET_BRICK_TYPE(dset,iv) ;
      if( output_datum == MRI_byte ) output_datum = MRI_short ;
   }

#ifdef TTDEBUG
printf(" ** datum = %s\n",MRI_TYPE_name[output_datum]) ;
#endif

   iv = EDIT_dset_items( new_dset ,
                           ADN_prefix , TT_prefix ,
                           ADN_label1 , TT_prefix ,
                           ADN_directory_name , TT_session ,
                           ADN_self_name , cbuf ,
                           ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                           ADN_func_type , FUNC_TT_TYPE ,
                           ADN_nvals , FUNC_nvals[FUNC_TT_TYPE] ,
                           ADN_ntt , 0 ,                           /* 07 Jun 2007 */
                           ADN_datum_all , output_datum ,
                         ADN_none ) ;

   if( iv > 0 )
     ERROR_exit("%d errors in attempting to create output dataset!",iv ) ;

   if( THD_deathcon() && THD_is_file(new_dset->dblk->diskptr->header_name) )
      ERROR_exit(
              "Output dataset file %s already exists--cannot continue!\a",
              new_dset->dblk->diskptr->header_name ) ;

#ifdef TTDEBUG
printf("*** deleting exemplar dataset\n") ;
#endif

   THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
   if((ptr)==NULL) \
      ( fprintf(stderr,"*** Cannot allocate memory for statistics!\n"), exit(0) )

   /*-- make space for the t-test computations --*/

   /* (allocate entire volumes) 13 Dec 2005 [rickr] */
                              npiece  = 3.0 ;  /* need at least this many */
   if( TT_paired )            npiece += 1.0 ;
   else if( TT_set1 != NULL ) npiece += 2.0 ;

   npiece += mri_datum_size(output_datum) / (float) sizeof(float) ;
   npiece += mri_datum_size(output_datum) / (float) sizeof(float) ;

#if 0
   piece_size = TT_workmem * MEGA / ( npiece * sizeof(float) ) ;
   if( piece_size > nxyz ) piece_size = nxyz ;

#ifdef TTDEBUG
printf("*** malloc-ing space for statistics: %g float arrays of length %d\n",
       npiece,piece_size) ;
#endif
#endif

   av2  = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(av2) ;
   sd2  = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(sd2) ;
   ffim = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(ffim) ;
   num2 = TT_set2->num ;

   if( TT_paired ){
      av1  = sd1 = NULL ;
      gfim = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(gfim) ;
      num1 = num2 ;
   } else if( TT_set1 != NULL ){
      av1  = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(av1) ;
      sd1  = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(sd1) ;
      gfim = NULL ;
      num1 = TT_set1->num ;
   } else {
      av1  = sd1 = NULL ;
      gfim = NULL ;
      num1 = 0 ;
   }

   vdif = (void *) malloc( mri_datum_size(output_datum) * nxyz ) ; MTEST(vdif) ;
   vsp  = (void *) malloc( mri_datum_size(output_datum) * nxyz ) ; MTEST(vsp)  ;

   /* 27 Dec 2002: make DOF dataset (if prefix is given, and unpooled is on) */

   if( TT_pooled == 0 && TT_dof_prefix[0] != '\0' ){
     dofbrik = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(dofbrik) ;

     dof_dset = EDIT_empty_copy( new_dset ) ;

     tross_Make_History( "3dttest" , argc,argv , dof_dset ) ;

     EDIT_dset_items( dof_dset ,
                       ADN_prefix , TT_dof_prefix ,
                       ADN_directory_name , TT_session ,
                       ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE,
                       ADN_func_type , FUNC_BUCK_TYPE ,
                       ADN_nvals , 1 ,
                       ADN_datum_all , MRI_float ,
                      ADN_none ) ;

     if( THD_is_file(dof_dset->dblk->diskptr->header_name) )
        ERROR_exit(
                "-dof_prefix dataset file %s already exists--cannot continue!\a",
                dof_dset->dblk->diskptr->header_name ) ;

     EDIT_substitute_brick( dof_dset , 0 , MRI_float , dofbrik ) ;
   }

   /* print out memory usage to edify the user */

   if( ! TT_be_quiet ){
      memuse =    sizeof(float) * nxyz * npiece
              + ( mri_datum_size(output_datum) + sizeof(short) ) * nxyz ;

      if( dofbrik != NULL ) memuse += sizeof(float) * nxyz ;  /* 27 Dec 2002 */

      printf("--- allocated %d Megabytes memory for internal use (%d volumes)\n",
             (int)(memuse/MEGA), (int)npiece) ;
   }

   mri_fix_data_pointer( vdif , DSET_BRICK(new_dset,0) ) ;  /* attach bricks */
   mri_fix_data_pointer( vsp  , DSET_BRICK(new_dset,1) ) ;  /* to new dataset */

   /** only short and float are allowed for output **/
   if( output_datum != MRI_short && output_datum != MRI_float )
      ERROR_exit("Illegal output data type %d = %s",
                 output_datum , MRI_TYPE_name[output_datum] ) ;

   num2_inv = 1.0 / num2 ;  num2m1_inv = 1.0 / (num2-1) ;
   if( num1 > 0 ){
      num1_inv = 1.0 / num1 ;  num1m1_inv = 1.0 / (num1-1) ;
   }

   /*----- loop over pieces to process the input datasets with -----*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                            \
   do{ int pv ; (ds) = THD_open_dataset((name)) ;  /* 16 Sep 1999 */              \
       if( !ISVALID_3DIM_DATASET((ds)) )                                          \
          ERROR_exit("Can't open dataset: %s",(name)) ;                           \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny || (ds)->daxes->nzz!=nz ) \
          ERROR_exit("Axes size mismatch: %s",(name)) ;                           \
       if( !EQUIV_GRIDS((ds),new_dset) )                                          \
          WARNING_message("Grid mismatch: %s",(name)) ;                           \
       if( DSET_NUM_TIMES((ds)) > 1 )                                             \
         ERROR_exit("Can't use time-dependent data: %s",(name)) ;                 \
       if( TT_use_editor ) EDIT_one_dataset( (ds), &TT_edopt ) ;                  \
       else                DSET_load((ds)) ;                                      \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                          \
       if( DSET_ARRAY((ds),pv) == NULL )                                          \
          ERROR_exit("Can't access data: %s",(name)) ;                            \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex )                              \
          ERROR_exit("Can't use complex data: %s",(name)) ;                       \
       break ; } while (0)

#if 0   /* can do it directly now (without offsets)  13 Dec 2005 [rickr] */
/** macro to return pointer to correct location in brick for current processing **/

#define SUB_POINTER(ds,vv,ind,ptr)                                            \
   do{ switch( DSET_BRICK_TYPE((ds),(vv)) ){                                  \
         default: ERROR_exit("Illegal datum! ***");                           \
            case MRI_short:{ short * fim = (short *) DSET_ARRAY((ds),(vv)) ;  \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_byte:{ byte * fim = (byte *) DSET_ARRAY((ds),(vv)) ;     \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_float:{ float * fim = (float *) DSET_ARRAY((ds),(vv)) ;  \
                             (ptr) = (void *)( fim + (ind) ) ;                \
            } break ; } break ; } while(0)
#endif

   /** number of pieces to process **/
   /* num_piece = (nxyz + piece_size - 1) / nxyz ; */

#if 0
   nice(2) ;  /** lower priority a little **/
#endif


   /* possibly open TT_base_dset now, and convert to floats */
   if( TT_base_dname ) {
      DOPEN(base_dset, TT_base_dname) ;
      base_ary = (float *) malloc( sizeof(float) * nxyz ) ; MTEST(base_ary) ;
      EDIT_coerce_scale_type(nxyz , DSET_BRICK_FACTOR(base_dset,0) ,
              DSET_BRICK_TYPE(base_dset,0),DSET_ARRAY(base_dset,0), /* input */
              MRI_float ,base_ary  ) ;                              /* output */
      THD_delete_3dim_dataset( base_dset , False ) ; base_dset = NULL ;
   }

   /* only 1 'piece' now   13 Dec 2005 [rickr] */
   for( piece=0 ; piece < 1 ; piece++ ){

      fim_offset = 0 ;

#ifdef TTDEBUG
printf("*** start of piece %d: length=%d offset=%d\n",piece,nxyz,fim_offset) ;
#else
      if( ! TT_be_quiet ){
         printf("--- starting piece %d/%d (%d voxels) ",piece+1,1,nxyz) ;
         fflush(stdout) ;
      }
#endif

      /** process set2 (and set1, if paired) **/

      for( ii=0 ; ii < nxyz ; ii++ ) av2[ii] = 0.0 ;
      for( ii=0 ; ii < nxyz ; ii++ ) sd2[ii] = 0.0 ;

      for( kk=0 ; kk < num2 ; kk++ ){

         /** read in the data **/

         DOPEN(dset,TT_set2->ar[kk]) ;
         iv = DSET_PRINCIPAL_VALUE(dset) ;

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf(".") ; fflush(stdout) ; }  /* progress */
#else
         printf(" ** opened dataset file %s\n",TT_set2->ar[kk]);
#endif

#if 0 /* fimfac will be compute when the results are ready */
         if( piece == 0 && kk == 0 ){
            fimfac = DSET_BRICK_FACTOR(dset,iv) ;
            if( fimfac == 0.0 ) fimfac = 1.0 ;
            fimfacinv = 1.0 / fimfac ;
#ifdef TTDEBUG
printf(" ** set fimfac = %g\n",fimfac) ;
#endif
         }
#endif

         /** convert it to floats (in ffim) **/
         EDIT_coerce_scale_type(nxyz , DSET_BRICK_FACTOR(dset,iv) ,
                                DSET_BRICK_TYPE(dset,iv),DSET_ARRAY(dset,iv), /* input */
                                MRI_float ,ffim  ) ;                         /* output */
         THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

         /** get the paired dataset, if present **/

         if( TT_paired ){
            DOPEN(dset,TT_set1->ar[kk]) ;
            iv = DSET_PRINCIPAL_VALUE(dset) ;

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf(".") ; fflush(stdout) ; }  /* progress */
#else
        printf(" ** opened dataset file %s\n",TT_set1->ar[kk]);
#endif

            EDIT_coerce_scale_type(
                        nxyz , DSET_BRICK_FACTOR(dset,iv) ,
                        DSET_BRICK_TYPE(dset,iv),DSET_ARRAY(dset,iv), /* input */
                        MRI_float ,gfim  ) ;                         /* output */
            THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

            if( TT_voxel >= 0 )
              fprintf(stderr,"-- paired values #%02d: %f, %f\n",
                      kk,ffim[TT_voxel],gfim[TT_voxel]) ;

            for( ii=0 ; ii < nxyz ; ii++ ) ffim[ii] -= gfim[ii] ;
         } else if( TT_voxel >= 0 )
            fprintf(stderr,"-- set2 value #%02d: %f\n",kk,ffim[TT_voxel]);

#ifdef TTDEBUG
printf("  * adding into av2 and sd2\n") ;
#endif

         /* accumulate into av2 and sd2 */

         for( ii=0 ; ii < nxyz ; ii++ ){
            dd = ffim[ii] ; av2[ii] += dd ; sd2[ii] += dd * dd ;
         }

      }  /* end of loop over set2 datasets */

      /** form the mean and stdev of set2 **/

#ifdef TTDEBUG
printf(" ** forming mean and sigma of set2\n") ;
#endif

      for( ii=0 ; ii < nxyz ; ii++ ){
         av2[ii] *= num2_inv ;
         dd       = (sd2[ii] - num2*av2[ii]*av2[ii]) ;
         sd2[ii]  = (dd > 0.0) ? sqrt( num2m1_inv * dd ) : 0.0 ;
      }
      if( TT_voxel >= 0 )
         fprintf(stderr,"-- s2 mean = %g, sd = %g\n",
                 av2[TT_voxel],sd2[TT_voxel]) ;

      /** if set1 exists but is not paired with set2, process it now **/

      if( ! TT_paired && TT_set1 != NULL ){

         for( ii=0 ; ii < nxyz ; ii++ ) av1[ii] = 0.0 ;
         for( ii=0 ; ii < nxyz ; ii++ ) sd1[ii] = 0.0 ;

         for( kk=0 ; kk < num1 ; kk++ ){
            DOPEN(dset,TT_set1->ar[kk]) ;
            iv = DSET_PRINCIPAL_VALUE(dset) ;

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf(".") ; fflush(stdout) ; }  /* progress */
#else
         printf(" ** opened dataset file %s\n",TT_set1->ar[kk]);
#endif

            EDIT_coerce_scale_type(
                                nxyz , DSET_BRICK_FACTOR(dset,iv) ,
                                DSET_BRICK_TYPE(dset,iv),DSET_ARRAY(dset,iv), /* input */
                                MRI_float ,ffim  ) ;                         /* output */
            THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

#ifdef TTDEBUG
printf("  * adding into av1 and sd1\n") ;
#endif

            for( ii=0 ; ii < nxyz ; ii++ ){
               dd = ffim[ii] ; av1[ii] += dd ; sd1[ii] += dd * dd ;
            }
            if( TT_voxel >= 0 )
               fprintf(stderr,"-- set1 value #%02d: %g\n",kk,ffim[TT_voxel]) ;
         }  /* end of loop over set1 datasets */

         /** form the mean and stdev of set1 **/

#ifdef TTDEBUG
printf(" ** forming mean and sigma of set1\n") ;
#endif

         for( ii=0 ; ii < nxyz ; ii++ ){
            av1[ii] *= num1_inv ;
            dd       = (sd1[ii] - num1*av1[ii]*av1[ii]) ;
            sd1[ii]  = (dd > 0.0) ? sqrt( num1m1_inv * dd ) : 0.0 ;
         }
         if( TT_voxel >= 0 )
            fprintf(stderr,"-- s1 mean = %g, sd = %g\n",
                    av1[TT_voxel], sd1[TT_voxel]) ;
      }  /* end of processing set1 by itself */

      /***** now form difference and t-statistic *****/

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf("+") ; fflush(stdout) ; }  /* progress */
#else
         printf(" ** computing t-tests next\n") ;
#endif

#if 0 /* will do at end using EDIT_convert_dtype  13 Dec 2005 [rickr] */

      /** macro to assign difference value to correct type of array **/
#define DIFASS switch( output_datum ){                                        \
                 case MRI_short: sdar[ii] = (short) (fimfacinv*dd) ; break ;  \
                 case MRI_float: fdar[ii] = (float) dd             ; break ; }
#define TOP_SS  32700
#define TOP_TT (32700.0/FUNC_TT_SCALE_SHORT)

#endif

      if( TT_paired || TT_use_bval == 1 ){ /** case 1: paired estimate or 1-sample **/

        if( TT_paired || TT_n1 == 0 ){       /* the olde waye: 1 sample test */
          f2 = 1.0 / sqrt( (double) num2 ) ;
          for( ii=0 ; ii < nxyz ; ii++ ){
            av2[ii] -= (base_ary ? base_ary[ii] : TT_bval) ;  /* final mean */
            if( sd2[ii] > 0.0 ){
               num_tt++ ;
               tt      = av2[ii] / (f2 * sd2[ii]) ;
               sd2[ii] = tt;      /* final t-stat */

               tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;
            } else {
               sd2[ii] = 0.0;
            }
          }
          if( TT_voxel >= 0 )
             fprintf(stderr,"-- paired/bval mean = %g, t = %g\n",
                     av2[TT_voxel], sd2[TT_voxel]) ;

        } else {  /* 10 Oct 2007: -sdn1 was used with -base1: 'two' sample test */
          f1 = (TT_n1-1.0) * (1.0/TT_n1 + 1.0/num2) / (TT_n1+num2-2.0) ;
          f2 = (num2 -1.0) * (1.0/TT_n1 + 1.0/num2) / (TT_n1+num2-2.0) ;
          for( ii=0 ; ii < nxyz ; ii++ ){
            av2[ii] -= (base_ary ? base_ary[ii] : TT_bval) ;  /* final mean */
            q1 = f1 * TT_sd1*TT_sd1 + f2 * sd2[ii]*sd2[ii] ;
            if( q1 > 0.0 ){
              num_tt++ ;
              tt = av2[ii] / sqrt(q1) ;
              sd2[ii] = tt ;      /* final t-stat */
              tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;
            } else {
              sd2[ii] = 0.0 ;
            }
          }
        } /* end of -sdn1 special case */
#ifdef TTDEBUG
printf(" ** paired or bval test: num_tt = %d\n",num_tt) ;
#endif

      } else if( TT_pooled ){ /** case 2: unpaired 2-sample, pooled variance **/

         f1 = (num1-1.0) * (1.0/num1 + 1.0/num2) / (num1+num2-2.0) ;
         f2 = (num2-1.0) * (1.0/num1 + 1.0/num2) / (num1+num2-2.0) ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            av2[ii] -= av1[ii] ;        /* final mean */
            q1 = f1 * sd1[ii]*sd1[ii] + f2 * sd2[ii]*sd2[ii] ;
            if( q1 > 0.0 ){
               num_tt++ ;
               tt = av2[ii] / sqrt(q1) ;
               sd2[ii] = tt ;      /* final t-stat */

               tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;
            } else {
               sd2[ii] = 0.0 ;
            }
         }

         if( TT_voxel >= 0 )
            fprintf(stderr,"-- unpaired, pooled mean = %g, t = %g\n",
                    av2[TT_voxel], sd2[TT_voxel]) ;
#ifdef TTDEBUG
printf(" ** pooled test: num_tt = %d\n",num_tt) ;
#endif

      } else { /** case 3: unpaired 2-sample, unpooled variance **/
               /** 27 Dec 2002: modified to save DOF into dofar **/

         if( dofbrik != NULL ) dofar = dofbrik + fim_offset ;  /* 27 Dec 2002 */

         for( ii=0 ; ii < nxyz ; ii++ ){
            av2[ii] -= av1[ii] ;
            q1 = num1_inv * sd1[ii]*sd1[ii] ;
            q2 = num2_inv * sd2[ii]*sd2[ii] ;
            if( q1>0.0 && q2>0.0 ){               /* have positive variances? */
               num_tt++ ;
               tt = av2[ii] / sqrt(q1+q2) ;
               sd2[ii] = tt ;      /* final t-stat */

               tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;

               if( dofar != NULL )                             /* 27 Dec 2002 */
                 dofar[ii] =  (q1+q2)*(q1+q2)
                            / (num1m1_inv*q1*q1 + num2m1_inv*q2*q2) ;
            } else {
               sd2[ii] = 0.0 ;
               if( dofar != NULL ) dofar[ii] = 1.0 ;           /* 27 Dec 2002 */
            }
         }

         if( TT_voxel >= 0 )
            fprintf(stderr,"-- unpaired, unpooled mean = %g, t = %g\n",
                    av2[TT_voxel], sd2[TT_voxel]) ;
#ifdef TTDEBUG
printf(" ** unpooled test: num_tt = %d\n",num_tt) ;
#endif
      }

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf("\n") ; fflush(stdout) ; }
#endif

   }  /* end of loop over pieces of the input */

   if( TT_paired ){
      printf("--- Number of degrees of freedom = %d (paired test)\n",num2-1) ;
      dof = num2 - 1 ;
   } else if( TT_use_bval == 1 ){
      if( TT_n1 == 0 ){
        printf("--- Number of degrees of freedom = %d (1-sample test)\n",num2-1) ;
        dof = num2 - 1 ;
      } else {
        dof = TT_n1+num2-2 ;
        printf("--- Number of degrees of freedom = %d (-sdn1 2-sample test)\n",(int)dof) ;
      }
   } else {
      printf("--- Number of degrees of freedom = %d (2-sample test)\n",num1+num2-2) ;
      dof = num1+num2-2 ;
      if( ! TT_pooled )
         printf("    (For unpooled variance estimate, this is only approximate!)\n") ;
   }

   printf("--- Number of t-tests performed  = %d out of %d voxels\n",num_tt,nxyz) ;
   printf("--- Largest |t| value found      = %g\n",tt_max) ;

   kk = sizeof(ptable) / sizeof(float) ;
   for( ii=0 ; ii < kk ; ii++ ){
      tt = student_p2t( ptable[ii] , dof ) ;
      printf("--- Double sided tail p = %8f at t = %8f\n" , ptable[ii] , tt ) ;
   }

   /**----------------------------------------------------------------------**/
   /** now convert data to output format                13 Dec 2005 [rickr] **/

   /* first set mean */
   fimfac = EDIT_convert_dtype(nxyz , MRI_float,av2 , output_datum,vdif , 0.0) ;
   DSET_BRICK_FACTOR(new_dset, 0) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
   dd = fimfac; /* save for debug output */

   /* if output is of type short, limit t-stat magnitude to 32.7 */
   if( output_datum == MRI_short ){
     for( ii=0 ; ii < nxyz ; ii++ ){
       if     ( sd2[ii] >  32.7 ) sd2[ii] =  32.7 ;
       else if( sd2[ii] < -32.7 ) sd2[ii] = -32.7 ;
     }
   }

   fimfac = EDIT_convert_dtype(nxyz , MRI_float,sd2 , output_datum,vsp , 0.0) ;
   DSET_BRICK_FACTOR(new_dset, 1) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;

#ifdef TTDEBUG
printf(" ** fimfac for mean, t-stat = %g, %g\n",dd, fimfac) ;
#endif
   /**----------------------------------------------------------------------**/

   INFO_message("Writing combined dataset into %s\n", DSET_BRIKNAME(new_dset) ) ;

   fbuf[0] = dof ;
   for( ii=1 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
   (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;

#if 0 /* factors already set */
   fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac                    : 0.0 ;
   fbuf[1] = (output_datum == MRI_short                  ) ? 1.0 / FUNC_TT_SCALE_SHORT : 0.0 ;
   (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
#endif

   if( !AFNI_noenv("AFNI_AUTOMATIC_FDR") ) ii = THD_create_all_fdrcurves(new_dset) ;
   else                                    ii = 0 ;
   THD_load_statistics( new_dset ) ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   if( ii > 0 ) ININFO_message("created %d FDR curves in header",ii) ;

   if( dof_dset != NULL ){                                  /* 27 Dec 2002 */
     DSET_write( dof_dset ) ;
     WROTE_DSET( dof_dset ) ;
   }

   exit(0) ;
}
