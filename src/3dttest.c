

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994-6 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "mrilib.h"

#undef  TTDEBUG
#undef  USE_PTHRESH

/*-------------------------- global data --------------------------*/

/** inputs **/

static EDIT_options TT_edopt ;

static int   TT_paired     = 0 ;
static int   TT_pooled     = 1 ;
static float TT_pthresh    = 0.0 ;
static int   TT_use_bval   = 0 ;
static float TT_bval       = 0.0 ;
static int   TT_use_editor = 0 ;
static int   TT_be_quiet   = 0 ;
static int   TT_workmem    = 12 ;  /* default = 12 Megabytes */

#define MEGA  1048576  /* 2^20 */

static THD_string_array * TT_set1 = NULL ;  /* sets of dataset names */
static THD_string_array * TT_set2 = NULL ;

static char TT_session[THD_MAX_NAME]  = "./" ;
static char TT_prefix[THD_MAX_PREFIX] = "tdif" ;
static char TT_label[THD_MAX_LABEL]   = "\0" ;

static int TT_datum = ILLEGAL_TYPE ;

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
   float val ;
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

#ifdef USE_PTHRESH
      /** -pthresh pval **/

      if( strncmp(argv[nopt],"-pthresh",6) == 0 ){
         char * ch ;

         if( ++nopt >= argc ) TT_syntax("-pthresh needs a value!");
         TT_pthresh = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' || TT_pthresh <= 0.0 || TT_pthresh > 0.99999 )
            TT_syntax("value after -pthresh is illegal!") ;

         fprintf(stderr,
                 "*** -pthresh not implemented yet, will ignore!\n" ) ;
         TT_pthresh = 0.0 ;

         nopt++ ; continue ;  /* skip to next arg */
      }
#endif

      /**** after this point, the options are no longer 'free floating' ****/

      /** -base1 bval **/

      if( strncmp(argv[nopt],"-base1",6) == 0 ){
         char * ch ;

         if( ++nopt >= argc )    TT_syntax("-base1 needs a value!");
         if( TT_use_bval == -1 ) TT_syntax("-base1 with -set1 illegal!");
         TT_bval = strtod( argv[nopt] , &ch ) ;
         if( *ch != '\0' ) TT_syntax("value after -base1 is illegal!") ;
         TT_use_bval = 1 ;
         nopt++ ; continue ;  /* skip to next arg */
      }

      /** -set1 file file ... **/

      if( strncmp(argv[nopt],"-set1",6) == 0 ){
         if( TT_use_bval == 1 ) TT_syntax("-set1 with -base1 illegal!");
         TT_use_bval = -1 ;
         INIT_SARR( TT_set1 ) ;
         for( kk=nopt+1 ; kk < argc ; kk++ ){
            if( strncmp(argv[kk],"-set2",6) == 0 ) break ;
            ADDTO_SARR( TT_set1 , argv[kk] ) ;
         }

         if( kk >= argc )       TT_syntax("-set1 not followed by -set2") ;
         if( kk-1-nopt <= 0 )   TT_syntax("-set1 has no datasets after it") ;
         if( TT_set1->num < 2 ) TT_syntax("-set1 doesn't have enough datasets") ;
         nopt = kk ; continue ; /* skip to arg that matched -set2 */
      }

      /** -set2 file file ... */

      if( strncmp(argv[nopt],"-set2",6) == 0 ){
         INIT_SARR( TT_set2 ) ;
         for( kk=nopt+1 ; kk < argc ; kk++ ){
            ADDTO_SARR( TT_set2 , argv[kk] ) ;
         }
         if( TT_set2->num < 2 ) TT_syntax("-set2 doesn't have enough datasets") ;
         break ;  /* end of possible inputs */
      }

      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\n",argv[nopt]) ;
      exit(1) ;

   }  /* end of loop over options */

   if( strlen(TT_label) == 0 ){
      MCW_strncpy(TT_label,TT_prefix,THD_MAX_LABEL) ;
   }

   /*--- check arguments for consistency ---*/

   if( TT_use_bval == 0 )
      TT_syntax("neither -base1 nor -set1 is present!") ;

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

#ifdef TTDEBUG
printf("*** finished with options\n") ;
#endif

   return ;
}

/*------------------------------------------------------------------*/

void TT_syntax(char * msg)
{
   if( msg != NULL ){
      fprintf(stderr,"*** %s\n",msg) ;
      exit(1) ;
   }

   printf(
    "Gosset (Student) t-test sets of 3D datasets\n"
    "Usage 1: 3dttest [options] -set1 datasets ... -set2 datasets ...\n"
    "   for comparing the means of 2 sets of datasets (voxel by voxel).\n"
    "\n"
    "Usage 2: 3dttest [options] -base1 bval -set2 datasets ...\n"
    "   for comparing the mean of 1 set of datasets against a constant.\n"
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
    "                         than a constant.  NOT RECOMMENDED.\n"
#ifdef USE_PTHRESH
    "  -pthresh pval      = 'pval' is a probability level (i.e., from 0 to 1)\n"
    "                         at which to threshold the output, per voxel.\n"
    "                         N.B.: NOT IMPLEMENTED YET!\n"
#endif
    "  -workmem mega      = 'mega' specifies the number of megabytes of RAM\n"
    "                         to use for statistical workspace.  It defaults\n"
    "                         to 12.  The program will run faster if this is\n"
    "                         larger (see the NOTES section below).\n"
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
    " ** The input datasets are specified by their .HEAD files,\n"
    "      but their .BRIK files must exist also! This program cannot\n"
    "      'warp-on-demand' from other datasets.\n"
    " ** This program cannot deal with time-dependent or complex-valued datasets!\n"
    "      By default, the output dataset function values will be shorts if the\n"
    "      first input dataset is byte- or short-valued; otherwise they will be\n"
    "      floats.  This behavior may be overridden using the -datum option.\n"
    "      However, the t-statistic at each voxel will be always be\n"
    "      stored as a short integer that is 1000 times the actual t-value.\n"
   ) ;

   exit(0) ;
}

/*------------------------------------------------------------------*/

static float ptable[] = { 0.5 , 0.2 , 0.1 , 0.05 , 0.01 , 0.001 , 0.0001 , 0.00001 } ;

int main( int argc , char * argv[] )
{
   int nx,ny,nz , nxyz , ii,kk , num1,num2 , num_tt=0 , iv ,
       output_type , output_nvals , piece , num_piece , piece_len , fim_offset ;
   float dx,dy,dz , dxyz ,
         num1_inv , num2_inv , num1m1_inv , num2m1_inv , dof , tthr ,
         dd,tt,q1,q2 , f1,f2 , tt_max=0.0 ;
   THD_3dim_dataset * dset=NULL , * new_dset=NULL , * qset=NULL ;
   float * av1 , * av2 , * sd1 , * sd2 , * ffim , * gfim ;
   short * tsp , * tsar ;   /* output t-statistic */
   void  * vdif ;           /* output mean difference */
   short * sdif , * sdar ;  /* (in various formats) */
   float * fdif , * fdar ;
   void  * vfim ;
   char  cbuf[THD_MAX_NAME] ;
   int   ibuf[32] ;
   float fbuf[MAX_STAT_AUX] , fimfac , fimfacinv ;
   int   output_datum ;
   int   piece_size ;
   float npiece , memuse ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) TT_syntax(NULL) ;
   TT_read_opts( argc , argv ) ;

   if( ! TT_be_quiet )
      printf("3dttest: t-tests of 3D datasets, by R.W. Cox (rwcox@mcw.edu)\n") ;

   /*-- read first dataset in set2 to get dimensions, etc. --*/

   dset = THD_open_one_dataset( TT_set2->ar[0] ) ;
   if( ! ISVALID_3DIM_DATASET(dset) ){
      fprintf(stderr,"*** Unable to open dataset file %s\n",TT_set2->ar[0]);
      exit(1) ;
   }

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

   ibuf[0] = output_datum ; ibuf[1] = MRI_short ;

   iv = EDIT_dset_items( new_dset ,
                           ADN_prefix , TT_prefix ,
                           ADN_label1 , TT_prefix ,
                           ADN_directory_name , TT_session ,
                           ADN_self_name , cbuf ,
                           ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                           ADN_func_type , FUNC_TT_TYPE ,
                           ADN_nvals , FUNC_nvals[FUNC_TT_TYPE] ,
                           ADN_datum_array , ibuf ,
                         ADN_none ) ;

   if( iv > 0 ){
      fprintf(stderr,
              "*** %d errors in attempting to create output dataset!\n",iv ) ;
      exit(1) ;
   }

   if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "*** Output dataset file %s already exists--cannot continue!\a\n",
              new_dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

#ifdef TTDEBUG
printf("*** deleting exemplar dataset\n") ;
#endif

   THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
   if((ptr)==NULL) \
      ( fprintf(stderr,"*** Cannot allocate memory for statistics!\n"                \
                       "*** Try using the -workmem option to reduce memory needs,\n" \
                       "*** or create more swap space in the operating system.\n"    \
                ), exit(0) )

   /*-- make space for the t-test computations --*/

                              npiece  = 3.0 ;  /* need at least this many */
   if( TT_paired )            npiece += 1.0 ;
   else if( TT_set1 != NULL ) npiece += 2.0 ;

   npiece += mri_datum_size(output_datum) / (float) sizeof(float) ;
   npiece += mri_datum_size(MRI_short)    / (float) sizeof(float) ;

   piece_size = TT_workmem * MEGA / ( npiece * sizeof(float) ) ;
   if( piece_size > nxyz ) piece_size = nxyz ;

#ifdef TTDEBUG
printf("*** malloc-ing space for statistics: %g float arrays of length %d\n",
       npiece,piece_size) ;
#endif

   av2  = (float *) malloc( sizeof(float) * piece_size ) ; MTEST(av2) ;
   sd2  = (float *) malloc( sizeof(float) * piece_size ) ; MTEST(sd2) ;
   ffim = (float *) malloc( sizeof(float) * piece_size ) ; MTEST(ffim) ;
   num2 = TT_set2->num ;

   if( TT_paired ){
      av1  = sd1 = NULL ;
      gfim = (float *) malloc( sizeof(float) * piece_size ) ; MTEST(gfim) ;
      num1 = num2 ;
   } else if( TT_set1 != NULL ){
      av1  = (float *) malloc( sizeof(float) * piece_size ) ; MTEST(av1) ;
      sd1  = (float *) malloc( sizeof(float) * piece_size ) ; MTEST(sd1) ;
      gfim = NULL ;
      num1 = TT_set1->num ;
   } else {
      av1  = sd1 = NULL ;
      gfim = NULL ;
      num1 = 0 ;
   }

   vdif = (void *)  malloc( mri_datum_size(output_datum) * nxyz ) ; MTEST(vdif) ;
   tsp  = (short *) malloc( sizeof(short) * nxyz )                ; MTEST(tsp)  ;

   if( ! TT_be_quiet ){
      memuse =    sizeof(float) * piece_size * npiece
              + ( mri_datum_size(output_datum) + sizeof(short) ) * nxyz ;

      printf("--- allocated %d Megabytes memory for internal use\n",(int)(memuse/MEGA)) ;
   }

   mri_fix_data_pointer( vdif , DSET_BRICK(new_dset,0) ) ;  /* attach bricks */
   mri_fix_data_pointer( tsp  , DSET_BRICK(new_dset,1) ) ;  /* to new dataset */

   switch( output_datum ){
      default: fprintf(stderr,"Illegal input data type %s!\n",
                       MRI_TYPE_name[output_datum] ) ;
      exit(1) ;

      case MRI_short: sdif = (short *) vdif ; break ;
      case MRI_float: fdif = (float *) vdif ; break ;
   }

   num2_inv = 1.0 / num2 ;  num2m1_inv = 1.0 / (num2-1) ;
   if( num1 > 0 ){
      num1_inv = 1.0 / num1 ;  num1m1_inv = 1.0 / (num1-1) ;
   }

   /*----- loop over pieces to process the input dataset with -----*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                               \
   do{ int pv ; (ds) = THD_open_one_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                            \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; }        \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny || (ds)->daxes->nzz!=nz ){   \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }             \
       if( DSET_NUM_TIMES((ds)) > 1 ){                                               \
         fprintf(stderr,"*** Can't use time-dependent data: %s\n",(name));exit(1); } \
       if( TT_use_editor ) EDIT_one_dataset( (ds), &TT_edopt ) ;                     \
       else                THD_load_datablock( (ds)->dblk , NULL ) ;                 \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                             \
       if( DSET_ARRAY((ds),pv) == NULL ){                                            \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }          \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                                \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1); }     \
       break ; } while (0)

/** macro to return pointer to correct location in brick for current processing **/

#define SUB_POINTER(ds,vv,ind,ptr)                                            \
   do{ switch( DSET_BRICK_TYPE((ds),(vv)) ){                                  \
         default: fprintf(stderr,"\n*** Illegal datum! ***\n");exit(1);       \
            case MRI_short:{ short * fim = (short *) DSET_ARRAY((ds),(vv)) ;  \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_byte:{ byte * fim = (byte *) DSET_ARRAY((ds),(vv)) ;     \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_float:{ float * fim = (float *) DSET_ARRAY((ds),(vv)) ;  \
                             (ptr) = (void *)( fim + (ind) ) ;                \
            } break ; } break ; } while(0)

   /** number of pieces to process **/

   num_piece = (nxyz + piece_size - 1) / piece_size ;

   nice(5) ;  /** lower priority a little **/

   for( piece=0 ; piece < num_piece ; piece++ ){

      fim_offset = piece * piece_size ;
      piece_len  = (piece < num_piece-1 ) ? piece_size
                                          : (nxyz - fim_offset) ;

#ifdef TTDEBUG
printf("*** start of piece %d: length=%d offset=%d\n",piece,piece_len,fim_offset) ;
#else
      if( ! TT_be_quiet ){
         printf("--- starting piece %d/%d (%d voxels) ",piece+1,num_piece,piece_len) ;
         fflush(stdout) ;
      }
#endif

      /** process set2 (and set1, if paired) **/

      for( ii=0 ; ii < piece_len ; ii++ ) av2[ii] = 0.0 ;
      for( ii=0 ; ii < piece_len ; ii++ ) sd2[ii] = 0.0 ;

      for( kk=0 ; kk < num2 ; kk++ ){

         /** read in the data **/

         DOPEN(dset,TT_set2->ar[kk]) ;
         iv = DSET_PRINCIPAL_VALUE(dset) ;

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf(".") ; fflush(stdout) ; }  /* progress */
#else
         printf(" ** opened dataset file %s\n",TT_set2->ar[kk]);
#endif

         if( piece == 0 && kk == 0 ){
            fimfac = DSET_BRICK_FACTOR(dset,iv) ;
            if( fimfac == 0.0 ) fimfac = 1.0 ;
            fimfacinv = 1.0 / fimfac ;
#ifdef TTDEBUG
printf(" ** set fimfac = %g\n",fimfac) ;
#endif
         }

         /** convert it to floats (in ffim) **/

         SUB_POINTER(dset,iv,fim_offset,vfim) ;
         EDIT_coerce_scale_type( piece_len , DSET_BRICK_FACTOR(dset,iv) ,
                                 DSET_BRICK_TYPE(dset,iv),vfim ,      /* input  */
                                 MRI_float               ,ffim  ) ;   /* output */
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

            SUB_POINTER(dset,iv,fim_offset,vfim) ;
            EDIT_coerce_scale_type( piece_len , DSET_BRICK_FACTOR(dset,iv) ,
                                    DSET_BRICK_TYPE(dset,iv),vfim ,    /* input  */
                                    MRI_float               ,gfim  ) ; /* output */
            THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

            for( ii=0 ; ii < piece_len ; ii++ ) ffim[ii] -= gfim[ii] ;
         }

#ifdef TTDEBUG
printf("  * adding into av2 and sd2\n") ;
#endif

         /* accumulate into av2 and sd2 */

         for( ii=0 ; ii < piece_len ; ii++ ){
            dd = ffim[ii] ; av2[ii] += dd ; sd2[ii] += dd * dd ;
         }

      }  /* end of loop over set2 datasets */

      /** form the mean and stdev of set2 **/

#ifdef TTDEBUG
printf(" ** forming mean and sigma of set2\n") ;
#endif

      for( ii=0 ; ii < piece_len ; ii++ ){
         av2[ii] *= num2_inv ;
         dd       = (sd2[ii] - num2*av2[ii]*av2[ii]) ;
         sd2[ii]  = (dd > 0.0) ? sqrt( num2m1_inv * dd ) : 0.0 ;
      }

      /** if set1 exists but is not paired with set2, process it now **/

      if( ! TT_paired && TT_set1 != NULL ){

         for( ii=0 ; ii < piece_len ; ii++ ) av1[ii] = 0.0 ;
         for( ii=0 ; ii < piece_len ; ii++ ) sd1[ii] = 0.0 ;

         for( kk=0 ; kk < num1 ; kk++ ){
            DOPEN(dset,TT_set1->ar[kk]) ;
            iv = DSET_PRINCIPAL_VALUE(dset) ;

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf(".") ; fflush(stdout) ; }  /* progress */
#else
         printf(" ** opened dataset file %s\n",TT_set1->ar[kk]);
#endif

            SUB_POINTER(dset,iv,fim_offset,vfim) ;
            EDIT_coerce_scale_type( piece_len , DSET_BRICK_FACTOR(dset,iv) ,
                                    DSET_BRICK_TYPE(dset,iv),vfim ,     /* input  */
                                    MRI_float               ,ffim  ) ;  /* output */
            THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

#ifdef TTDEBUG
printf("  * adding into av1 and sd1\n") ;
#endif

            for( ii=0 ; ii < piece_len ; ii++ ){
               dd = ffim[ii] ; av1[ii] += dd ; sd1[ii] += dd * dd ;
            }
         }  /* end of loop over set1 datasets */

         /** form the mean and stdev of set1 **/

#ifdef TTDEBUG
printf(" ** forming mean and sigma of set1\n") ;
#endif

         for( ii=0 ; ii < piece_len ; ii++ ){
            av1[ii] *= num1_inv ;
            dd       = (sd1[ii] - num1*av1[ii]*av1[ii]) ;
            sd1[ii]  = (dd > 0.0) ? sqrt( num1m1_inv * dd ) : 0.0 ;
         }
      }  /* end of processing set1 by itself */

      /***** now form difference and t-statistic *****/

#ifndef TTDEBUG
         if( ! TT_be_quiet ){ printf("+") ; fflush(stdout) ; }  /* progress */
#else
         printf(" ** computing t-tests next\n") ;
#endif

      tsar = tsp + fim_offset ;  /* pointers into output t-statistic array */
      switch( output_datum ){
         case MRI_short:  sdar = sdif + fim_offset ; break ;  /* if fim is shorts */
         case MRI_float:  fdar = fdif + fim_offset ; break ;  /* if fim is floats */
      }

      /** macro to assign difference value to correct type of array **/

#define DIFASS switch( output_datum ){                                         \
                  case MRI_short: sdar[ii] = (short) (fimfacinv*dd) ; break ;  \
                  case MRI_float: fdar[ii] = (float) dd             ; break ; }

#define TOP_SS  32700
#define TOP_TT (32700.0/FUNC_TT_SCALE_SHORT)

      if( TT_paired || TT_use_bval == 1 ){ /** case 1: paired estimate or 1-sample **/

         f2 = 1.0 / sqrt( (double) num2 ) ;
         for( ii=0 ; ii < piece_len ; ii++ ){
            dd = av2[ii] - TT_bval ; DIFASS ;
            if( sd2[ii] > 0.0 ){
               num_tt++ ;
               tt       = dd / (f2 * sd2[ii]) ;
               tsar[ii] = (tt>TOP_TT) ? (TOP_SS)
                                      : (tt<-TOP_TT) ? (-TOP_SS)
                                                     : ((short)(FUNC_TT_SCALE_SHORT*tt)) ;
               tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;
            } else {
               tsar[ii] = 0 ;
            }
         }

#ifdef TTDEBUG
printf(" ** paired or bval test: num_tt = %d\n",num_tt) ;
#endif

      } else if( TT_pooled ){ /** case 2: unpaired 2-sample, pooled variance **/

         f1 = (num1-1.0) * (1.0/num1 + 1.0/num2) / (num1+num2-2.0) ;
         f2 = (num2-1.0) * (1.0/num1 + 1.0/num2) / (num1+num2-2.0) ;
         for( ii=0 ; ii < piece_len ; ii++ ){
            dd = av2[ii] - av1[ii]                           ; DIFASS ;
            q1 = f1 * sd1[ii]*sd1[ii] + f2 * sd2[ii]*sd2[ii] ;
            if( q1 > 0.0 ){
               num_tt++ ;
               tt       = dd / sqrt(q1) ;
               tsar[ii] = (tt>TOP_TT) ? (TOP_SS)
                                      : (tt<-TOP_TT) ? (-TOP_SS)
                                                     : ((short)(FUNC_TT_SCALE_SHORT*tt)) ;
               tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;
            } else {
               tsar[ii] = 0 ;
            }
         }

#ifdef TTDEBUG
printf(" ** pooled test: num_tt = %d\n",num_tt) ;
#endif

      } else { /** case 3: unpaired 2-sample, unpooled variance **/

         for( ii=0 ; ii < piece_len ; ii++ ){
            dd = av2[ii] - av1[ii]          ; DIFASS ;
            q1 = num1_inv * sd1[ii]*sd1[ii] ;
            q2 = num2_inv * sd2[ii]*sd2[ii] ;
            if( q1>0.0 && q2>0.0 ){
               num_tt++ ;
               tt       = dd / sqrt(q1+q2) ;
               tsar[ii] = (tt>TOP_TT) ? (TOP_SS)
                                      : (tt<-TOP_TT) ? (-TOP_SS)
                                                     : ((short)(FUNC_TT_SCALE_SHORT*tt)) ;
               tt = fabs(tt) ; if( tt > tt_max ) tt_max = tt ;
            } else {
               tsar[ii] = 0 ;
            }
         }

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
      printf("--- Number of degrees of freedom = %d (1-sample test)\n",num2-1) ;
      dof = num2 - 1 ;
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

   printf("--- Writing combined dataset into %s\n",
           new_dset->dblk->diskptr->header_name) ;

   fbuf[0] = dof ;
   for( ii=1 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
   (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;

   fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
   fbuf[1] = 1.0 / FUNC_TT_SCALE_SHORT ;
   (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

   THD_load_statistics( new_dset ) ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

   exit(0) ;
}
