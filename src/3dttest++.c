#include "mrilib.h"

/*------------------------------- prototypes, etc. ---------------------------*/

/*----- funcs to do the t-tests on sets of numbers -----*/

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             ) ;

/*-----*/

float_pair ttest_toz( int numx, float *xar, int numy, float *yar, int opcode,
                      float *xres, float *yres ) ;

/*----- similar funcs for the case of -singletonA -----*/

void regress_toz_singletonA( float zA ,
                             int numB , float *zB ,
                             int mcov ,
                             float *xA ,
                             float *xB , float *psinvB , float *xtxinvB ,
                             float *outvec , float *workspace             ) ;

/*-----*/

float_pair ttest_toz_singletonA( float xar, int numy, float *yar, float *xres, float *yres ) ;

/*----- convert t-stat to z-score -----*/

double GIC_student_t2z( double tt , double dof ) ;

/*----- setup the covariates matrices -----*/

void TT_matrix_setup( int kout ) ;  /* 30 Jul 2010 */

/*----- macro to truncate labels -----*/

#undef  MAX_LABEL_SIZE
#define MAX_LABEL_SIZE 12

#undef  LTRUNC
#define LTRUNC(ss) \
 do{ if( strlen(ss) > MAX_LABEL_SIZE ){(ss)[MAX_LABEL_SIZE] = '\0'; }} while(0)

/*----- macro for some memory usage info -----*/

#undef MEMORY_CHECK
#ifdef USING_MCW_MALLOC
# define MEMORY_CHECK                                            \
   do{ long long nb = mcw_malloc_total() ;                       \
       if( nb > 0 ) INFO_message("Memory usage now = %s (%s)" ,  \
                    commaized_integer_string(nb) ,               \
                    approximate_number_string((double)nb) ) ;    \
   } while(0)
#else
# define MEMORY_CHECK     /*nada*/
#endif

/*----------------------------------------------------------------------------*/
/* Constants and globals */

#undef  MAXCOV
#define MAXCOV 31

static int toz    = 0 ;  /* convert t-statistics to z-scores? */
static int twosam = 0 ;

static int brickwise     = 0 ;                 /* 28 Jan 2014 */
static int brickwise_num = 0 ;

static int singletonA    = 0 ;                 /* 19 Mar 2015 */
static float singleton_variance_ratio = 1.0f ; /* 20 Mar 2015 */
static float singleton_fixed_val = 0.0f ;      /* 08 Dec 2015 */
static int use_singleton_fixed_val = 0 ;

static NI_element         *covnel=NULL ;       /* covariates */
static NI_str_array       *covlab=NULL ;

static int        num_covset_col=0 ;
static int             allow_cov=1 ;           /* allowed by default */
static MRI_vectim   **covvim_AAA=NULL ;
static MRI_vectim   **covvim_BBB=NULL ;
static floatvec     **covvec_AAA=NULL ;
static floatvec     **covvec_BBB=NULL ;

static int   zskip_AAA = 0 ;  /* 06 Oct 2010 [a dark day at Weathertop] */
static int   zskip_BBB = 0 ;
static float zskip_fff = 0.0f ;
static int   do_zskip  = 0 ;

#define ALLOW_RANK
static int   do_ranks  = 0 ;  /* 10 Nov 2010 */
static int   do_1sam   = 1 ;  /* 10 Nov 2010 */
static int   do_means  = 1 ;  /* 05 Feb 2014 */
static int   do_tests  = 1 ;
static int   do_cov    = 1 ;  /* 22 Jun 2016 */

static unsigned int testA, testB, testAB ;

#define CENTER_NONE 0
#define CENTER_DIFF 1
#define CENTER_SAME 2

#define CMETH_MEDIAN 1
#define CMETH_MEAN   2

static char *fname_cov=NULL ;
static int mcov  = 0 ;
static int nvout = 0 ;
static int nvres = 0 ;
static int center_code = CENTER_DIFF ;
static int center_meth = CMETH_MEAN ;    /* 26 Mar 2013 */
MRI_IMAGE *Axxim=NULL , *Bxxim=NULL ;
static float *Axx=NULL , *Axx_psinv=NULL , *Axx_xtxinv=NULL ;
static float *Bxx=NULL , *Bxx_psinv=NULL , *Bxx_xtxinv=NULL ;

static char *prefix_resid = NULL ;
static int  do_resid=0 ;
static float *ABresid=NULL , *Aresid=NULL , *Bresid=NULL ; /* 07 Dec 2015 */

#undef  AXX
#define AXX(i,j) Axx[(i)+(j)*(nval_AAA)]    /* i=0..nval_AAA-1 , j=0..mcov */
#undef  BXX
#define BXX(i,j) Bxx[(i)+(j)*(nval_BBB)]    /* i=0..nval_BBB-1 , j=0..mcov */

static char *prefix = "TTnew" ;
static byte *mask   = NULL ;
static int  nmask   = 0 ;
static int  nvox    = 0 ;
static int  nmask_hits = 0 ;
static char *name_mask = NULL ; /* 10 Feb 2016 */

static int ttest_opcode = 0 ;  /* 0=pooled, 1=unpooled, 2=paired */

static int               ndset_AAA=0 , nval_AAA=0 ;
static char              *snam_AAA=NULL , *lnam_AAA=NULL ;
static char             **name_AAA=NULL ;  /* argv names for input datasets */
static char             **labl_AAA=NULL ;
static THD_3dim_dataset **dset_AAA=NULL ;
static MRI_vectim      *vectim_AAA=NULL ;

static int               ndset_BBB=0 , nval_BBB=0 ;
static char              *snam_BBB=NULL , *lnam_BBB=NULL ;
static char             **name_BBB=NULL ;
static char             **labl_BBB=NULL ;
static THD_3dim_dataset **dset_BBB=NULL ;
static MRI_vectim      *vectim_BBB=NULL ;

static int debug = 0 ;

static int do_randomsign   = 0 ;     /* 31 Dec 2015 */
static int do_sdat         = 0 ;     /* 29 Aug 2016 */
static int *randomsign_AAA = NULL ;
static int *randomsign_BBB = NULL ;
static int num_randomsign  = 0 ;     /* 02 Feb 2016 */

static int       do_clustsim = 0 ;   /* 10 Feb 2016 */
static int      num_clustsim = 0 ;
static char *prefix_clustsim = NULL ;
static char *tempdir         = "./" ;/* 20 Jul 2016 */

static int      do_Xclustsim = 0 ;   /* 30 Aug 2016 */
static int      nnlev_Xclu   = 2 ;
static int      sid_Xclu     = 2 ;
static int      npthr_Xclu   = 0 ;
static float *   pthr_Xclu   = NULL ;

static char *clustsim_prog=NULL ;    /* 30 Aug 2016 */
static char *clustsim_opt =NULL ;

static int dofsub          = 0    ;  /* 19 Jan 2016 */

/*--------------------------------------------------------------------------*/

static int string_search( char *targ , int nstr , char **str )
{
   int ii ;

   if( targ == NULL || *targ == '\0' || str == NULL || nstr < 1 ) return -1 ;

   for( ii=0 ; ii < nstr ; ii++ )
     if( str[ii] != NULL && strcmp(targ,str[ii]) == 0 ) return ii ;

   return -1 ;
}

/*----------------------------------------------------------------------------*/

static void vstep_print(void)   /* pacifier */
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ; vn++ ;
}

/*----------------------------------------------------------------------------*/

static void setup_randomsign(void)  /* moved here 02 Feb 2016 */
{
   int nflip , nb,nt , jj ;

   if( randomsign_AAA == NULL )
     randomsign_AAA = (int *)malloc(sizeof(int)*nval_AAA) ;
   nb = (int)rintf(0.15f*nval_AAA) ; if( nb < 1 ) nb = 1 ;
   nt = nval_AAA - nb ;
   do{
     for( nflip=jj=0 ; jj < nval_AAA ; jj++ ){
       randomsign_AAA[jj] = (lrand48()>>3) % 2 ;
       if( randomsign_AAA[jj] ) nflip++ ;
     }
   } while( nflip < nb || nflip > nt ) ;
#if 0
   fprintf(stderr,"++ randomsign for setA:") ;
   for( jj=0 ; jj < nval_AAA ; jj++ )
     fprintf(stderr,"%c" , randomsign_AAA[jj] ? '-' : '+' ) ;
   fprintf(stderr,"\n") ;
#endif

   if( nval_BBB > 0 ){
     if( randomsign_BBB == NULL )
       randomsign_BBB = (int *)malloc(sizeof(int)*nval_BBB) ;
     nb = (int)rintf(0.345f*nval_BBB) ; nt = nval_BBB - nb ;
     do{
       for( nflip=jj=0 ; jj < nval_BBB ; jj++ ){
         randomsign_BBB[jj] = (lrand48()>>3) % 2 ;
         if( randomsign_BBB[jj] ) nflip++ ;
       }
     } while( nflip < nb || nflip > nt ) ;
#if 0
     fprintf(stderr,"++ randomsign for setB:") ;
     for( jj=0 ; jj < nval_BBB ; jj++ )
       fprintf(stderr,"%c" , randomsign_BBB[jj] ? '-' : '+' ) ;
     fprintf(stderr,"\n") ;
#endif
   }
}

/*--------------------------------------------------------------------------*/

void display_help_menu(void)
{
   printf(
      "Gosset (Student) t-test of sets of 3D datasets.\n"
      "\n"
      "      [* Also consider program 3dMEMA, which can carry out a  *]\n"
      "      [* more sophisticated type of 't-test' that also takes  *]\n"
      "      [* into account the variance map of each input dataset. *]\n"
      "\n"
      "* Usage can be similar (but not identical) to the old 3dttest; for example:\n"
      "\n"
      "    3dttest++ -setA a+tlrc'[3]' b+tlrc'[3]' ...\n"
      "\n"
      "* OR, usage can be similar to 3dMEMA; for example:\n"
      "\n"
      "    3dttest++ -setA Green sub001 a+tlrc'[3]' \\\n"
      "                          sub002 b+tlrc'[3]' \\\n"
      "                          sub003 c+tlrc'[3]' \\\n"
      "                            ...                \\\n"
      "                -covariates Cfile\n"
      "\n"
      "* You can input 1 or 2 sets of data (labeled 'A' and 'B').\n"
      "\n"
      "* With 1 set ('-setA'), the mean across input datasets (usually subjects)\n"
      "   is tested against 0.\n"
      "\n"
      "* With 2 sets, the difference in means across each set is tested\n"
      "   against 0.  The 1 sample results for each set are also provided, since\n"
      "   these are often of interest to the investigator (e.g., YOU).\n"
      "  ++ With 2 sets, the default is to produce the difference as setA - setB.\n"
      "  ++ You can use the option '-BminusA' to get the signs reversed.\n"
      "\n"
      "* Covariates can be per-dataset (input=1 number) and/or per-voxel/per-dataset\n"
      "   (input=1 dataset sub-brick).\n"
      "  ++ Note that voxel-level covariates will slow the program down, since\n"
      "      the regression matrix for the covariates must be re-inverted for\n"
      "      each voxel separately.  For most purposes, the program is so fast\n"
      "      that this slower operation won't be important.\n"
      "\n"
      "* This program is meant (for many uses) to replace the original 3dttest,\n"
      "   which was written in 1994, \"When grass was green and grain was yellow\".\n"
      "  ++ And when the program's author still had hair.\n"
      "\n"

      "------------------\n"
      "SET INPUT OPTIONS\n"
      "------------------\n"
      "\n"
      "* At least the '-setA' option must be given.\n"
      "\n"
      "* '-setB' is optional, and if it isn't used, then the mean of the dataset\n"
      "   values from '-setA' is t-tested against 0 (1 sample t-test).\n"
      "\n"
      "* Two forms for the '-setX' (X='A' or 'B') options are allowed.  The first\n"
      "   (short) form is similar to the original 3dttest program, where the option\n"
      "   is just followed by a list of datasets to use.\n"
      "\n"
      "* The second (long) form is similar to the 3dMEMA program, where you specify\n"
      "   a label for each input dataset sub-brick (a difference between this\n"
      "   option and the version in 3dMEMA is only that you do not give a second\n"
      "   dataset ('T_DSET') with each sample in this program).\n"
      "\n"
      "***** SHORT FORM *****\n"
      "\n"
      " -setA BETA_DSET BETA_DSET ...\n"
      "[-setB]\n"
      "\n"
      "* In this form of input, you specify the datasets for each set\n"
      "   directly following the '-setX' option.\n"
      "  ++ Unlike 3dttest, you can specify multiple sub-bricks in a dataset:\n"
      "        -setA a+tlrc'[1..13(2)]'\n"
      "     which inputs 7 sub-bricks at once (1,3,5,7,9,11,13).\n"
      "   *** See the '-brickwise' option (far below) for more information ***\n"
      "   *** on how multiple sub-brick datasets will be processed herein. ***\n"
      "  ++ If multiple sub-bricks are input from a single dataset, then\n"
      "     covariates cannot be used (sorry, Charlie).\n"
      "  ++ For some limited compatibility with 3dttest, you can use '-set2' in\n"
      "     place of '-setA', and '-set1' in place of '-setB'.\n"
      "  ++ [19 Jun 2012, from Beijing Normal University, during AFNI Bootcamp]\n"
      "     For the SHORT FORM only, you can use the wildcards '*' and/or '?' in\n"
      "     the BETA_DSET filenames, along with sub-brick selectors, to make it\n"
      "     easier to create the command line.\n"
      "     To protect the wildcards from the shell, the entire filename should be\n"
      "     inside single ' or double \" quote marks.  For example:\n"
      "       3dttest++ -setA '*.beta+tlrc.HEAD[Vrel#0_Coef]' \\\n"
      "                 -setB '*.beta+tlrc.HEAD[Arel#0_Coef]' -prefix VAtest -paired\n"
      "     will do a paired 2-sample test between the symbolically selected sub-bricks\n"
      "     from a collection of single-subject datasets (in this case, 2 different tasks).\n"
      "\n"
      "***** LONG FORM *****\n"
      "\n"
      " -setA SETNAME            \\\n"
      "[-setB]  LABL_1 BETA_DSET \\\n"
      "         LABL_2 BETA_DSET \\\n"
      "         ...    ...       \\\n"
      "         LABL_N BETA_DSET\n"
      "\n"
      "* In this form of input, you specify an overall name for the set of datasets,\n"
      "   and a label to be associated with each separate input dataset.  (This label\n"
      "   is used with the '-covariates' option, described later.)\n"
      "\n"
      "   SETNAME   is the name assigned to the set (used in the output labels).\n"
      "   LABL_K    is the label for the Kth input dataset name, whose name follows.\n"
      "   BETA_DSET is the name of the dataset of the beta coefficient or GLT.\n"
      "             ++ only 1 sub-brick can be specified here!\n"
      "   Note that the labels 'SETNAME' and 'LABL_K' are limited to 12\n"
      "   characters -- any more will be thrown away without warning.\n"
      "\n"
      "     ** The program determines if you are using the short form or long **\n"
      "     ** form to specify the input datasets based on the first argument **\n"
      "     ** after the '-setX' option.  If this argument can be opened as a **\n"
      "     ** dataset, the short form is used. If instead, the next argument **\n"
      "     ** cannot be opened as a dataset,  then the long form is assumed. **\n"
      "\n"
      " -labelA SETNAME = for the short form of '-setX', this option allows you\n"
      "[-labelB]          to attach a label to the set, which will be used in\n"
      "                   the sub-brick labels in the output dataset.  If you don't\n"
      "                   give a SETNAME, then '-setA' will be named 'SetA', etc.\n"
      "\n"
      "  ***** NOTE WELL: The sign of a two sample test is A - B.          *****\n"
      "  ***              Thus, '-setB' corresponds to '-set1' in 3dttest,   ***\n"
      "  ***                and '-setA' corresponds to '-set2' in 3dttest.   ***\n"
      "  *****            This ordering of A and B matches 3dGroupInCorr.  *****\n"
      "  *****-------------------------------------------------------------*****\n"
      "  ***** ALSO NOTE: You can reverse this sign by using the option    *****\n"
      "  ***              '-BminusA', in which case the test is B - A.       ***\n"
      "  ***              The option '-AminusB' can be used to explicitly    ***\n"
      "  *****            specify the standard subtraction order.          *****\n"
      "\n"
      "---------------------------------------------------------------\n"
      "TESTING A SINGLE DATASET VERSUS THE MEAN OF A GROUP OF DATASETS\n"
      "---------------------------------------------------------------\n"
      "\n"
      "This new [Mar 2015] option allows you to test a single value versus\n"
      "a group of datasets.  To do this, replace the '-setA' option with the\n"
      "'-singletonA' option described below, and input '-setB' normally\n"
      "(that is, '-setB' must have more than 1 dataset).\n"
      "\n"
      "The '-singletonA' option comes in 3 different forms:\n"
      "\n"
      " -singletonA dataset_A\n"
      "   *OR*\n"
      " -singletonA LABL_A dataset_A\n"
      "   *OR*\n"
      " -singletonA FIXED_NUMBER\n"
      "\n"
      "* In the first form, just give the 1 sub-brick dataset name after the option.\n"
      "\n"
      "* In the second form, you can provide a dataset 'label' to be used for\n"
      "  covariates extraction.  As in the case of the long forms for '-setA' and\n"
      "  '-setB', the 'LABL_A' argument cannot be the name of an existing dataset;\n"
      "  otherwise, the program will assume you are using the first form.\n"
      "\n"
      "* In the third form, instead of giving a dataset, you give a fixed number\n"
      "  (e.g., '0.5'), to test the -setB collection against this 1 number.\n"
      "  ++ In this form, '-singleton_variance_ratio' is set to a very small number,\n"
      "     since you presumably aren't testing against an instance of a random\n"
      "     variable.\n"
      "  ++ Also, '-BminusA' is turned on when FIXED_NUMBER is used, to give the\n"
      "     effect of a 1-sample test against a constant.  For example,\n"
      "       '-singletonA 0.0 -set B x y z'\n"
      "     is equivalent to the 1-sample test with '-setA x y z'. The only advantage\n"
      "     of using '-singletonA FIXED_NUMBER' is that you can test against a\n"
      "     nonzero constant this way.\n"
      "  ++ You cannot use covariates with this FIXED_NUMBER form of '-singletonA' :-(\n"
      "\n"
      "* The output dataset will have 2 sub-bricks:\n"
      "  ++ The difference (at each voxel) between the dataset_A value and the\n"
      "     mean of the setB dataset values.\n"
      "  ++ (In the form where 'dataset_A' is replaced by a fixed)\n"
      "     (number, the output is instead the difference between)\n"
      "     (the mean of the setB values and the fixed number.   )\n"
      "  ++ The t-statistic corresponding to this difference.\n"
      "\n"
      "* If covariates are used, at each voxel the slopes of the setB data values with\n"
      "  respect to the covariates are estimated (as usual).\n"
      "  ++ These slopes are then used to project the covariates out of the mean of\n"
      "     the setB values, and are also applied similarly to the single value from\n"
      "     the singleton dataset_A (using its respective covariate value).\n"
      "  ++ That is, the covariate slopes from setB are applied to the covariate values\n"
      "     for dataset_A in order to subtract the covariate effects from dataset_A,\n"
      "     as well as from the setB mean.\n"
      "  ++ Since it impossible to independently estimate the covariate slopes for\n"
      "     dataset_A, this procedure seems (to me) like the only reasonable way to use\n"
      "     covariates with a singleton dataset.\n"
      "\n"
      "* The t-statistic is computed assuming that the variance of dataset_A is the\n"
      "  same as the variance of the setB datasets.\n"
      "  ++ Of course, it is impossible to estimate the variance of dataset_A at each\n"
      "     voxel from its single number!\n"
      "  ++ In this way, the t-statistic differs from testing the setB mean against\n"
      "     a (voxel-dependent) constant, which would not have any variance.\n"
      "  ++ In particular, the t-statistic will be smaller than in the more usual\n"
      "     'test-against-constant' case, since the test here allows for the variance\n"
      "     of the dataset_A value.\n"
      "  ++ As a special case, you can use the option\n"
      "       -singleton_variance_ratio RRR\n"
      "     to set the (assumed) variance of dataset_A to be RRR times the variance\n"
      "     of set B. Here, 'RRR' must be a positive number -- it cannot be zero,\n"
      "     so if you really want to test against a voxel-wise constant, use something\n"
      "     like 0.000001 for RRR (this is the setting automatically made when\n"
      "     'dataset_A' is replaced by a fixed number, in the third form above).\n"
      "\n"
      "* Statistical inference on a single sample (dataset_A values) isn't really\n"
      "  possible.  The purpose of '-singletonA' is to give you some guidance when\n"
      "  a voxel value in dataset_A is markedly different from the distribution of\n"
      "  values in setB.\n"
      "  ++ However, a statistician would caution you that when an elephant walks into\n"
      "     the room, it might just be a 3000 standard deviation mouse, and you can't\n"
      "     validly conclude it is a different species until you get some more data.\n"
      "\n"
      "* At present, '-singletonA' cannot be used with '-brickwise'.\n"
      "  ++ Various other options don't make sense with '-singletonA', including\n"
      "     '-paired' and '-center SAME'.\n"
      "\n"
      "* Note that there is no '-singletonB' option -- the only reason this is labeled\n"
      "  as '-singletonA' is to remind the user (you) that this option replaces the\n"
      "  '-setA' option.\n"
      "\n"
      "--------------------------------------\n"
      "COVARIATES - per dataset and per voxel\n"
      "--------------------------------------\n"
      "\n"
      " -covariates COVAR_FILE\n"
      "\n"
      "* COVAR_FILE is the name of a text file with a table for the covariate(s).\n"
      "   Each column in the file is treated as a separate covariate, and each\n"
      "   row contains the values of these covariates for one sample (dataset). Note\n"
      "   that you can use '-covariates' only ONCE -- the COVAR_FILE should contain\n"
      "   the covariates for ALL input samples from both sets.\n"
      "\n"
      "* Rows in COVAR_FILE whose first column don't match a dataset label are\n"
      "   ignored (silently).\n"
      "\n"
      "* An input dataset label that doesn't match a row in COVAR_FILE, on the other\n"
      "   hand, is a fatal error.\n"
      "  ++ The program doesn't know how to get the covariate values for such a\n"
      "     dataset, so it can't continue.\n"
      "\n"
      "* There is no provision for missing values -- the entire table must be filled!\n"
      "\n"
      "* The format of COVAR_FILE is similar to the format used in 3dMEMA and\n"
      "   3dGroupInCorr (generalized to allow for voxel-wise covariates):\n"
      "\n"
      "     FIRST LINE -->   subject IQ   age  GMfrac\n"
      "     LATER LINES -->  Elvis   143   42  Elvis_GM+tlrc[8]\n"
      "                      Fred     85   59  Fred_GM+tlrc[8]\n"
      "                      Ethel   109   49  Ethel_GM+tlrc[8]\n"
      "                      Lucy    133   32  Lucy_GM+tlrc[8]\n"
      "                      Ricky   121   37  Ricky_GM+tlrc[8]\n"
      "\n"
      "* The first line of COVAR_FILE contains column headers.  The header label\n"
      "   for the first column (#0) isn't used for anything.  The later header labels\n"
      "   are used in the sub-brick labels stored in the output dataset.\n"
      "\n"
      "* The first column contains the dataset labels that must match the dataset\n"
      "   LABL_K labels given in the '-setX' option(s).\n"
      "\n"
      "* If you used a short form '-setX' option, each dataset label is\n"
      "   the dataset's prefix name (truncated to 12 characters).\n"
      "  ++ e.g.,  Klaatu+tlrc'[3]' ==>  Klaatu\n"
      "  ++ e.g.,  Elvis.nii.gz     ==>  Elvis\n"
      "\n"
      "* '-covariates' can only be used with the short form '-setX' option\n"
      "   if each input dataset has only 1 sub-brick (so that each label\n"
      "   refers to exactly 1 volume of data).\n"
      "  ++ Duplicate labels in the dataset list or in the covariates file\n"
      "     will not work well!\n"
      "\n"
      "* The later columns in COVAR_FILE contain numbers (e.g., 'IQ' and 'age',\n"
      "    above), OR dataset names.  In the latter case, you are specifying a\n"
      "    voxel-wise covariate (e.g., 'GMfrac').\n"
      "  ++ Do NOT put the dataset names or labels in this file in quotes.\n"
      "\n"
      "* A column can contain numbers only, OR datasets names only.  But one\n"
      "   column CANNOT contain a mix of numbers and dataset names!\n"
      " ++ In the second line of the file (after the header line), a column entry\n"
      "    that is purely numeric indicates that column will be all numbers.\n"
      " ++ A column entry that is not numeric indicates that column will be\n"
      "    dataset names.\n"
      " ++ You are not required to make the columns and rows line up neatly,\n"
      "    (separating entries in the same row with 1 or more blanks is OK),\n"
      "    but your life will be much nicer if you DO make them well organized.\n"
      "\n"
      "* You cannot enter covariates as pure labels (e.g., 'Male' and 'Female').\n"
      "   To assign such categorical covariates, you must use numeric values.\n"
      "   A column in the covariates file that contains strings rather than\n"
      "   numbers is assumed to be a list of dataset names, not category labels!\n"
     "\n"
      "* If you want to omit some columns in COVAR_FILE from the analysis, you\n"
      "   can do so with the standard AFNI column selector '[...]'.  However,\n"
      "   you MUST include column #0 first (the dataset labels) and at least\n"
      "   one more column.  For example:\n"
      "     -covariates Cov.table'[0,2..4]'\n"
      "   to skip column #1 but keep columns #2, #3, and #4.\n"
      "\n"
      "* Only the -paired and -pooled options can be used with covariates.\n"
      "  ++ If you use -unpooled, it will be changed to -pooled.\n"
      "\n"
      "* If you use -paired, then the covariate values for setB will be the\n"
      "   same as those for setA, even if the dataset labels are different!\n"
      "  ++ If you want to use different covariates for setA and setB in the\n"
      "     paired test, then you'll have to subtract the setA and setB\n"
      "     datasets (with 3dcalc), and then do a 1-sample test, using the\n"
      "     differences of the original covariates as the covariates for\n"
      "     this 1-sample test.\n"
      "  ++ This subtraction technique works because a paired t-test is really\n"
      "     the same as subtracting the paired samples and then doing a\n"
      "     1-sample t-test on these differences.\n"
      "  ++ For example, you do FMRI scans on a group of subjects, then\n"
      "     train them on some task for a week, then re-scan them, and\n"
      "     you want to use their behavioral scores on the task, pre- and\n"
      "     post-training, as the covariates.\n"
      "\n"
      "* See the section 'STRUCTURE OF THE OUTPUT DATASET' for details of\n"
      "   what is calculated and stored by 3dttest++.\n"
      "\n"
      "* If you are having trouble getting the program to read your covariates\n"
      "  table file, then set the environment variable AFNI_DEBUG_TABLE to YES\n"
      "  and run the program.  A lot of progress reports will be printed out,\n"
      "  which may help pinpoint the problem; for example:\n"
      "     3dttest++ -DAFNI_DEBUG_TABLE=YES -covariates cfile.txt |& more\n"
      "\n"
      "* A maximum of 31 covariates are allowed.  If you have more, then\n"
      "   seriously consider the likelihood that you are completely deranged.\n"
      "\n"
      "* N.B.: The simpler forms of the COVAR_FILE that 3dMEMA allows are\n"
      "        NOT supported here!  Only the format described above will work.\n"
      "\n"
      "* N.B.: IF you are entering multiple sub-bricks from the same dataset in\n"
      "        one of the '-setX' options, AND you are using covariates, then\n"
      "        you must use the 'LONG FORM' of input for the '-setX' option,\n"
      "        and give each sub-brick a distinct label that matches something\n"
      "        in the covariates file.  Otherwise, the program will not know\n"
      "        which covariate to use with which input sub-brick, and bad\n"
      "        things will happen.\n"
      "\n"
      "* N.B.: Please be careful in setting up the covariates file and dataset\n"
      "        labels, as the program only does some simple error checking.\n"
      "        ++ If you REALLY want to see the regression matrices\n"
      "           used with covariates, use the '-debug' option.\n"
      "        ++ Which you give you a LOT of output (to stderr), so redirect:\n"
      "             3dttest++ .... |& tee debug.out\n"
      "\n"
      "***** CENTERING (this subject is very important -- read and think!) *******\n"
      "\n"
      " ++ This term refers to how the mean across subjects of a covariate\n"
      "    will be processed.  There are 3 possibilities:\n"
      "\n"
      " -center NONE = Do not remove the mean of any covariate.\n"
      " -center DIFF = Each set will have the means removed separately.\n"
      " -center SAME = The means across both sets will be computed and removed.\n"
      "                (This option only applies to a 2-sample test, obviously.)\n"
      "\n"
      " ++ These operations (DIFF or SAME) can be altered slightly by the following:\n"
      "      -cmeth MEAN   = When centering, subtract the mean.\n"
      "      -cmeth MEDIAN = When centering, subtract the median.\n"
      "    (Per the request of the Musical Neuroscientist, AKA Steve Gotts.)\n"
      "\n"
      " ++ If you use a voxel-wise (dataset) covariate, then the centering method\n"
      "    is applied to each voxel's collection of covariate values separately.\n"
      "\n"
      " ++ The default operation is '-center DIFF'.\n"
      "\n"
      " ++ '-center NONE' is for the case where you have pre-processed the\n"
      "    covariate values to meet your needs; otherwise, it is not recommended!\n"
      "\n"
      " ++ Centering can be important.  For example, suppose that the mean\n"
      "    IQ in setA is significantly higher than in setB, and that the beta\n"
      "    values are positively correlated with IQ.  Then the mean in\n"
      "    setA will be higher than in setB simply from the IQ effect.\n"
      "    To attempt to allow for this type of inter-group mean differences,\n"
      "    you would have to center the two groups together, rather than\n"
      "    separately (i.e., '-center SAME').\n"
      "\n"
      " ++ How to choose between '-center SAME' or '-center DIFF'?  You have\n"
      "    to understand what your model is and what effect the covariates\n"
      "    are likely to have on the data.  You shouldn't just blindly us\n"
      "    covariates 'just in case'.  That way lies statistical madness.\n"
      "  -- If the two samples don't differ much in the mean values of their\n"
      "      covariates, then the results with '-center SAME' and '-center DIFF'\n"
      "      should be nearly the same.\n"
      "  -- For fixed covariates (not those taken from datasets), the program\n"
      "      prints out the results of a t-test of the between-group mean\n"
      "      covariate values.  This test is purely informative; no action is\n"
      "      taken if the t-test shows that the two groups are significantly\n"
      "      different in some covariate.\n"
      "  -- If the two samples DO differ much in the mean values of their\n"
      "      covariates, then you should read the next point carefully.\n"
      "\n"
      " ++ The principal purpose of including covariates in an analysis (ANCOVA)\n"
      "    is to reduce the variance of the beta values due to extraneous causes.\n"
      "    Some investigators also wish to use covariates to 'factor out' significant\n"
      "    differences between groups.  However, there are those who argue\n"
      "    (convincingly) that if your two groups differ markedly in their mean\n"
      "    covariate values, then there is NO statistical test that can tell if\n"
      "    their mean beta values (dependent variable) would be the same or\n"
      "    different if their covariate values were all the same instead:\n"
      "      Miller GM and Chapman JP. 'Misunderstanding analysis of covariance',\n"
      "      J Abnormal Psych 110: 40-48 (2001) \n"
      "      http://dx.doi.org/10.1037/0021-843X.110.1.40\n"
      "      http://psycnet.apa.org/journals/abn/110/1/40.pdf\n"
      "  -- For example, if all your control subjects have high IQs and all your\n"
      "      patient subjects have normal IQs, group differences in activation can\n"
      "      be due to either cause (IQ or disease status) and you can't turn the\n"
      "      results from a set of high IQ controls into the results you would have\n"
      "      gotten from a set of normal IQ controls (so you can compare them to the\n"
      "      patients) just by linear regression and then pretending the IQ issue\n"
      "      goes away.\n"
      "  -- The decision as to whether a mean covariate difference between groups\n"
      "      makes the t-test of the mean beta difference invalid or valid isn't\n"
      "      purely a statistical question; it's also a question of interpretation\n"
      "      of the scientific issues of the study.  See the Miller & Chapman paper\n"
      "      for a lengthy discussion of this issue.\n"
      "  -- It is not clear how much difference in covariate levels is acceptable.\n"
      "      You could carry out a t-test on the covariate values between the\n"
      "      2 groups and if the difference in means is not significant at some\n"
      "      level (i.e., if p > 0.05?), then accept the two groups as being\n"
      "      'identical' in that variable.  But this is just a suggestion.\n"
      "      (In fact, the program now carries out this t-test for you; cf supra.)\n"
      "  -- Thanks to Andy Mayer for pointing out this article to me.\n"
      "\n"
      " ++ At this time, there is no option to force the SLOPES of the\n"
      "    regression vs. covariate values to be the same in the two-sample\n"
      "    analysis.  [Adding this feature would be too much like work.]\n"
#if 0
      "\n"
      " ++ (Actually, only the first letter of the argument after '-center' is)\n"
      "    (used to set the centering code, and it can be upper or lower case.)\n"
#endif
      "\n"

      "-------------\n"
      "OTHER OPTIONS\n"
      "-------------\n"
      "\n"
      " -paired   = Specifies the use of a paired-sample t-test to\n"
      "              compare setA and setB.  If this option is used,\n"
      "              setA and setB must have the same cardinality (duh).\n"
      "             ++ Recall that if '-paired' is used with '-covariates',\n"
      "                 the covariates for setB will be the same as for setA.\n"
      "\n"
      " -unpooled = Specifies that the variance estimates for setA and\n"
      "              setB be computed separately (not pooled together).\n"
      "             ++ This only makes sense if -paired is NOT given.\n"
      "             ++ '-unpooled' cannot be used with '-covariates'.\n"
      "             ++ Unpooled variance estimates are supposed to\n"
      "                 provide some protection against heteroscedasticty\n"
      "                 (significantly different inter-subject variance\n"
      "                 between the two different collections of datasets).\n"
      "             ++  Our experience is that for most FMRI data, using\n"
      "                 '-unpooled' is not needed; the option is here for\n"
      "                 those who like to experiment or who are very cautious.\n"
      "\n"
      " -toz      = Convert output t-statistics to z-scores\n"
      "             ++ -unpooled implies -toz, since t-statistics won't be\n"
      "                 comparable between voxels as the number of degrees\n"
      "                 of freedom will vary between voxels.\n"
      "\n"
      " -zskip [n]= Do not include voxel values that are zero in the analysis.\n"
      "             ++ This option can be used when not all subjects' datasets\n"
      "                 overlap perfectly.\n"
      "             ++ -zskip implies -toz, since the number of samples per\n"
      "                 voxel will now vary, so the number of degrees of\n"
      "                 freedom will be spatially variable.\n"
      "             ++ If you follow '-zskip' with a positive integer (> 1),\n"
      "                 then that is the minimum number of nonzero values (in\n"
      "                 each of setA and setB, separately) that must be present\n"
      "                 before the t-test is carried out.  If you don't give\n"
      "                 this value, but DO use '-zskip', then its default is 5\n"
      "                 (for no good reason).\n"
      "             ++ At this time, you can't use -zskip with -covariates,\n"
      "                 because that would require more extensive re-thinking\n"
      "                 and then re-programming.\n"
      "             ++ You can't use -zskip with -paired, for obvious reasons.\n"
      "             ++ [This option added 06 Oct 2010 -- RWCox]\n"
      "             ++ You can also put a decimal fraction between 0 and 1 in\n"
      "                 place of 'n' (e.g., '0.9', or '90%%').  Such a value\n"
      "                 indicates that at least 90%% (e.g.) of the values in each\n"
      "                 set must be nonzero for the t-test to proceed. [08 Nov 2010]\n"
      "                 -- In no case will the number of values tested fall below 2!\n"
      "                 -- You can use '100%%' for 'n', to indicate that all data\n"
      "                    values must be nonzero for the test to proceed.\n"
#ifdef ALLOW_RANK
      "\n"
      " -rankize  = Convert the data (and covariates, if any) into ranks before\n"
      "              doing the 2-sample analyses.  This option is intended to make\n"
      "              the statistics more 'robust', and is inspired by the paper\n"
      "                WJ Conover and RL Iman.\n"
      "                Analysis of Covariance Using the Rank Transformation,\n"
      "                Biometrics 38: 715-724 (1982).\n"
      "                http://www.jstor.org/stable/2530051\n"
      "                Also see http://www.jstor.org/stable/2683975\n"
      "             ++ Using '-rankize' also implies '-no1sam' (infra), since it\n"
      "                 doesn't make sense to do 1-sample t-tests on ranks.\n"
      "             ++ Don't use this option unless you understand what it does!\n"
      "                 The use of ranks herein should be considered experimental\n"
      "                 or speculative.\n"
#endif
      "\n"
      " -no1sam   = When you input two samples (setA and setB), normally the\n"
      "              program outputs the 1-sample test results for each set\n"
      "              (comparing to zero), as well as the 2-sample test results\n"
      "              for differences between the sets.  With '-no1sam', these\n"
      "              1-sample test results will NOT be calculated or saved.\n"
      "\n"
      " -nomeans  = You can also turn off output of the 'mean' sub-bricks, OR\n"
      " -notests  = of the 'test' sub-bricks if you want, to reduce the size of\n"
      "              the output dataset.  For example, '-nomeans -no1sam' will\n"
      "              result in only getting the t-statistics for the 2-sample\n"
      "              tests.  These options are intended for use with '-brickwise',\n"
      "              where the amount of output sub-bricks can become overwhelming.\n"
      "             ++ You CANNOT use both '-nomeans' and '-notests', because\n"
      "                 then you would be asking for no outputs at all!\n"
      "\n"
      " -nocov    = Do not output the '-covariates' results.  This option is\n"
      "             useful only for internal testing, and it's hard to see\n"
      "             why the ordinary user would want it.\n"
      "\n"
      " -mask mmm = Only compute results for voxels in the specified mask.\n"
      "             ++ Voxels not in the mask will be set to 0 in the output.\n"
      "             ++ If '-mask' is not used, all voxels will be tested.\n"
      "             ++ HOWEVER: voxels whose input data is constant (in either set)\n"
      "                 will NOT be processed and will get all zero outputs.  This\n"
      "                 inaction happens because the variance of a constant set of\n"
      "                 data is zero, and division by zero is forbidden by the\n"
      "                 Deities of Mathematics -- e.g., http://www.math.ucla.edu/~tao/\n"
      "\n"
      " -brickwise = This option alters the way this program works with input\n"
      "               datasets that have multiple sub-bricks (cf. the SHORT FORM).\n"
      "              ++ If you use this option, it must appear BEFORE either '-set'\n"
      "                  option (so the program knows how to do the bookkeeping\n"
      "                  for the input datasets).\n"
      "              ++ WITHOUT '-brickwise', all the input sub-bricks from all\n"
      "                  datasets in '-setA' are gathered together to form the setA\n"
      "                  sample (similarly for setB, of course).  In this case, there\n"
      "                  is no requirement that all input datasets have the same\n"
      "                  number of sub-bricks.\n"
      "              ++ WITH '-brickwise', all input datasets (in both sets)\n"
      "                  MUST have the same number of sub-bricks.  The t-tests\n"
      "                  are then carried out sub-brick by sub-brick; that is,\n"
      "                  if you input a collection of datasets with 10 sub-bricks\n"
      "                  in each dataset, then you will get 10 t-test results.\n"
      "              ++ Each t-test result will be made up of more than 1 sub-brick\n"
      "                  in the output dataset.  If you are doing a 2-sample test,\n"
      "                  you might want to use '-no1sam' to reduce the number of\n"
      "                  volumes in the output dataset.  In addition, if you are\n"
      "                  only interested in the statistical tests and not the means\n"
      "                  (or slopes for covariates), then the option '-nomeans'\n"
      "                  will reduce the dataset to just the t (or z) statistics\n"
      "                  -- e.g., the combination '-no1sam -nomeans' will give you\n"
      "                     one statistical sub-brick per input sub-brick.\n"
      "              ++ If you input a LOT of sub-bricks, you might want to set\n"
      "                  environment variable AFNI_AUTOMATIC_FDR to NO, in order\n"
      "                  to suppress the automatic calculation of FDR curves for\n"
      "                  each t-statistic sub-brick -- this FDR calculation can\n"
      "                  be time consuming when done en masse.\n"
      "          -->>++ The intended application of this option is to make it\n"
      "                  easy to take a collection of time-dependent datasets\n"
      "                  (e.g., from MEG or from moving-window RS-FMRI analyses),\n"
      "                  and get time-dependent t-test results.  It is possible to do\n"
      "                  the same thing with a scripted loop, but that way is painful.\n"
      "              ++ You CAN use '-covariates' with '-brickwise'. You should note that\n"
      "                  each t-test will re-use the same covariates -- that is, there\n"
      "                  is no provision for time-dependent (or sub-brick dependent)\n"
      "                  covariate values -- for that, you'd have to relapse to scripting.\n"
      "              ++ EXAMPLE:\n"
      "                  Each input dataset (meg*.nii) has 100 time points; the 'X'\n"
      "                  datasets are for one test condition and the 'Y' datasets are\n"
      "                  for another. In this example, the subjects are the same in\n"
      "                  both conditions, so the '-paired' option makes sense.\n"
      "                    3dttest++ -brickwise -prefix megXY.nii -no1sam -paired\\\n"
      "                              -setA meg01X.nii meg02X.nii meg03X.nii ... \\\n"
      "                              -setB meg01Y.nii meg02Y.nii meg03Y.nii ... \n"
      "                * The output dataset will have 200 sub-bricks: 100 differences\n"
      "                   of the means between 'X' and 'Y', and 100 t-statistics.\n"
      "                * You could extract the output dataset t-statistics (say)\n"
      "                   into a single dataset with a command like\n"
      "                     3dTcat -prefix megXY_tstat.nii megXY.nii'[1..$(2)]'\n"
      "                   (Or you could have used the '-nomeans' option.)\n"
      "                   This dataset could then be used to plot the t-statistic\n"
      "                   versus time, make a movie, or otherwise do lots of fun things.\n"
      "                * If '-brickwise' were NOT used, the output dataset would just\n"
      "                   get 2 sub-bricks, as all the inputs in setA would be lumped\n"
      "                   together into one super-sized sample (and similarly for setB).\n"
      "                * Remember that with the SHORT FORM input (needed for '-brickwise'),\n"
      "                   you can use wildcards '*' and '?' together with '[...]' sub-brick\n"
      "                   selectors.\n"
      "\n"
      " -prefix p = Gives the name of the output dataset file.\n"
      "             ++ For surface-based datasets, use something like:\n"
      "                 -prefix p.niml.dset or -prefix p.gii.dset \n"
      "                Otherwise you may end up files containing numbers but\n"
      "                not a full set of header information.\n"
      "\n"
      " -resid q  = Output the residuals into a dataset with prefix 'q'.\n"
      "             ++ The residuals are the difference between the data values\n"
      "                and their prediction from the set mean (and set covariates).\n"
      "             ++ For use in further analysis of the results (e.g., 3dFWHMx).\n"
      "             ++ Cannot be used with '-brickwise' or '-zskip' (sorry).\n"
      "\n"
      " -randomsign = Randomize the signs of the datasets.  Intended to be used\n"
      "               with the output of '-resid' to generate null hypothesis\n"
      "               statistics in a second run of the program (probably using\n"
      "               '-nomeans' and '-toz').  Cannot be used with '-singletonA'\n"
      "               or with '-brickwise'.\n"
      "             ++ You will never get an 'all positive' or 'all negative' sign\n"
      "                flipping case -- each sign will be present at least 15%%\n"
      "                of the time.\n"
      "             ++ There must be at least 7 samples in each input set to\n"
      "                use this option.\n"
      "             ++ If you following '-randomsign' with a number (e.g.,\n"
      "                '-randomsign 1000'), then you will get 1000 iterations\n"
      "                of random sign flipping, so you will get 1000 times the\n"
      "                as many output sub-bricks as usual. This is intended for\n"
      "                for use with simulations such as '3dClustSim -inset'.\n"
      "\n"
      " -clustsim   = With this option, after the commanded t-tests are done, then:\n"
      "                (a) the residuals from '-resid' are used with '-randomsign' to\n"
      "                    simulate about 10000 null 3D results, and then\n"
      "                (b) 3dClustSim is run with those to generate cluster-threshold\n"
      "                    tables, and then\n"
      "                (c) 3drefit is used to pack those tables into the main output\n"
      "                    dataset, and then\n"
      "                (d) the temporary files created in this process are deleted.\n"
      "               The goal is to provide a method for cluster-level statistical\n"
      "               inference in the output dataset, to be used with the AFNI GUI\n"
      "               Clusterize controls.\n"
      "              ++ If you want to keep the 3dClustSim table .1D files, use this\n"
      "                 option in the form '-Clustsim'.  If you want to keep ALL the\n"
      "                 temporary files, use '-CLUSTSIM'.\n"
      "              ++ Since the simulations are done with '-toz' active, the program\n"
      "                 also turns on the '-toz' option for your output dataset. This\n"
      "                 means that the output statistics will be z-scores, not t-values.\n"
      "              ++ '-clustsim' will not work with less than 7 datasets in each\n"
      "                 input set -- in particular, it doesn't work with '-singletonA'.\n"
      "          -->>++ '-clustsim' runs step (a) in multiple jobs, for speed.  By\n"
      "                 default, it tries to auto-detect the number of CPUs on the system\n"
      "                 and uses that many separate jobs.  If you put a positive integer\n"
      "                 immediately following the option, as in '-clustsim 12', it will\n"
      "                 instead use that many jobs (e.g., 12).  This capability is to\n"
      "                 be used when the CPU count is not auto-detected correctly.\n"
#if 0
      "          -->>++ '-clustsim' can use up all the memory on a computer, and even\n"
      "                 more -- causing the computer to freeze or crash.  The program\n"
      "                 tries to avoid this, but it is not always possible to detect\n"
      "                 how much memory is usable on a computer. For this reason, you\n"
      "                 can use this option in the form\n"
      "                    -clustsim NCPU NGIG\n"
      "                 where NCPU is the number of CPUs (cores) to use, and NGIG is\n"
      "                 the number of gigabytes of memory to use.  This may help you\n"
      "                 prevent the 'Texas meltdown'.\n"
#endif
      "          -->>++ It is important to use the proper '-mask' option with '-clustsim'.\n"
      "                 Otherwise, the statistics of the clustering will be skewed (badly).\n"
      "\n"
      "        ---==>>> PLEASE NOTE: This option has been tested for 1- and 2-sample\n"
      "        ---==>>> unpaired and paired tests vs. resting state data -- to see if the\n"
      "        ---==>>> false alarm rate (FAR) was near the nominal 5%% level (it was).\n"
      "        ---==>>> The FAR for the covariate effects (as opposed to the main effect)\n"
      "        ---==>>> is still somewhat biased away from the 5%% level :(\n"
      "\n"
      " -prefix_clustsim cc = Use 'cc' for the prefix for the '-clustsim' temporary\n"
      "                       files, rather than a randomly generated prefix.\n"
      "                       You might find this useful if scripting.\n"
      "                      ++ The default randomly generated prefix will start with\n"
      "                         'TT.' and be followed by 11 alphanumeric characters,\n"
      "                         as in 'TT.Sv0Ghrn4uVg'.  To mimic this, you might\n"
      "                         use something like '-prefix_clustsim TT.Zhark'.\n"
      "\n"
      " -tempdir ttt        = Store temporary files for '-Clustsim' in this directory,\n"
      "                       rather than in the current working directory: this option\n"
      "                       is for use when you have access to a fast local disk\n"
      "                       compared to general storage on a network RAID.\n"
      "                       [NOTE: if you use '-CLUSTSIM', these files aren't deleted!]\n"

#if 0 /*** hidden from user ***/
      "\n"
      " -dofsub ss  = Subtract 'ss' from the normal degrees of freedom used.\n"
      "               (This option is for special scripting purposes.)\n"
#endif

      "\n"
      " -dupe_ok  = Duplicate dataset labels are OK.  Do not generate warnings\n"
      "             for dataset pairs.\n"
      "            ** This option must preceed the corresponding -setX options.\n"
      "\n"
      " -debug    = Prints out information about the analysis, which can\n"
      "              be VERY lengthy -- not for general usage (or even for colonels).\n"
      "             ++ Two copies of '-debug' will give even MORE output!\n"
      "\n"

      "-------------------------------\n"
      "STRUCTURE OF THE OUTPUT DATASET\n"
      "-------------------------------\n"
      "\n"
      "* The output dataset is stored in float format; there is no option\n"
      "   to store it in scaled short format :-)\n"
      "\n"
      "* For each covariate, 2 sub-bricks are produced:\n"
      "  ++ The estimated slope of the beta values vs covariate\n"
      "  ++ The t-statistic of this slope\n"
      "  ++ If there are 2 sets of subjects, then each pair of sub-bricks is\n"
      "      produced for the setA-setB, setA, and setB cases, so that you'll\n"
      "      get 6 sub-bricks per covariate (plus 6 more for the mean, which\n"
      "      is treated as a special covariate whose values are all 1).\n"
      "  ++ Thus the number of sub-bricks produced is 6*(m+1) for the two-sample\n"
      "      case and 2*(m+1) for the one-sample case, where m=number of covariates.\n"
      "\n"
      "* For example, if there is one covariate 'IQ', and a two sample analysis\n"
      "   is carried out ('-setA' and '-setB' both used), then the output\n"
      "   dataset will contain the following 12 (6*2) sub-bricks:\n"
      "      #0  SetA-SetB_mean      = difference of means [covariates removed]\n"
      "      #1  SetA-SetB_Tstat\n"
      "      #2  SetA-SetB_IQ        = difference of slopes wrt covariate IQ\n"
      "      #3  SetA-SetB_IQ_Tstat\n"
      "      #4  SetA_mean           = mean of SetA [covariates removed]\n"
      "      #5  SetA_Tstat\n"
      "      #6  SetA_IQ             = slope of SetA wrt covariate IQ\n"
      "      #7  SetA_IQ_Tstat\n"
      "      #8  SetB_mean           = mean of SetB [covariates removed]\n"
      "      #9  SetB_Tstat\n"
      "      #10 SetB_IQ             = slope of SetB wrt covariate IQ\n"
      "      #11 SetB_IQ_Tstat\n"
      "\n"
      "* In the above, 'wrt' is standard mathematical shorthand for the\n"
      "   phrase 'with respect to'.\n"
      "\n"
      "* If option '-BminusA' is given, then the 'SetA-SetB' sub-bricks would\n"
      "   be labeled 'SetB-SetA' instead, of course.\n"
      "\n"
      "* If option '-toz' is used, the 'Tstat' will be replaced with 'Zscr'\n"
      "   in the statistical sub-brick labels.\n"
      "\n"
      "* If the long form of '-setA' is used, or '-labelA' is given, then\n"
      "   'SetA' in the sub-brick labels above is replaced with the\n"
      "   corresponding SETNAME.  (Mutatis mutandis for 'SetB'.)\n"
      "\n"
      "* If you produce a NIfTI-1 (.nii) file, then the sub-brick labels are\n"
      "   saved in the AFNI extension in the .nii file.  Processing further\n"
      "   in non-AFNI programs will probably cause these labels to be lost\n"
      "   (along with other AFNI niceties, such as the history field).\n"
      "\n"
      "* If you are doing a 2-sample run and don't want the 1-sample results,\n"
      "   then the '-no1sam' option can be used to eliminate these sub-bricks\n"
      "   from the output, saving space and time and mental energy.\n"
      "\n"
      "* The largest Tstat that will be output is 99.\n"
      "* The largest Zscr that will be output is 13.\n"
      "  ++ FYI: the 1-sided Gaussian tail probability of z=13 is 6.1e-39.\n"
      "\n"

      "-------------------\n"
      "HOW COVARIATES WORK\n"
      "-------------------\n"
      "\n"
      "Covariates work by forming a regression problem for each voxel, to\n"
      "estimate the mean of the input data and the slopes of the data with\n"
      "respect to variations in the covariates.\n"
      "\n"
      "For each input set of sub-bricks, a matrix is assembled.  There is one\n"
      "row for each sub-brick, and one column for each covariate, plus one\n"
      "more column for the mean.  So if there are 5 sub-bricks and 2 covariates,\n"
      "the matrix would look like so\n"
      "\n"
      "     [ 1  0.3  1.7 ]\n"
      "     [ 1  0.5  2.2 ]\n"
      " X = [ 1  2.3  3.3 ]\n"
      "     [ 1  5.7  7.9 ]\n"
      "     [ 1  1.2  4.9 ]\n"
      "\n"
      "The first column is all 1s, and models the mean value of the betas.\n"
      "The remaining columns are the covariates for each sub-brick.  (The\n"
      "numbers above are values I just made up, obviously.)\n"
      "\n"
      "The matrix is centered by removing the mean from each column except\n"
      "the first one.  In the above matrix, the mean of column #2 is 2,\n"
      "and the mean of column #3 is 4, so the centered matrix is\n"
      "\n"
      "      [ 1 -1.7 -2.3 ]\n"
      "      [ 1 -1.5 -1.8 ]\n"
      " Xc = [ 1  0.3 -0.7 ]\n"
      "      [ 1  3.7  3.9 ]\n"
      "      [ 1 -0.8  0.9 ]\n"
      "\n"
      "(N.B.: more than one centering option is available; this is the default.)\n"
      "\n"
      "The set of equations to be solved is [Xc] [b] = [z], where [b] is\n"
      "the column vector desired (first element = de-covariate-ized mean\n"
      "of the data values, remaining elements = slopes of data values\n"
      "with respect to the covariates), and [z] is the column vector of\n"
      "data values extracted from the input datasets.\n"
      "\n"
      "This set of equations is solved by forming the pseudo-inverse of the\n"
      "matrix [Xc]: [Xp] = inverse[Xc'Xc] [Xc'], so that [b] = [Xp] [z].\n"
      "(Here, ' means transpose.) For the sample matrix above, we have\n"
      "\n"
      "      [  0.2         0.2         0.2       0.2        0.2      ]\n"
      " Xp = [  0.0431649  -0.015954    0.252887  0.166557  -0.446654 ]\n"
      "      [ -0.126519   -0.0590721  -0.231052  0.0219866  0.394657 ]\n"
      "\n"
      "Because of the centering, the first column of [Xc] is orthgonal to\n"
      "the other columns, so the first row of [Xp] is all 1/N, where N is\n"
      "the number of data points (here, N=5).\n"
      "\n"
      "In reality, the pseudo-inverse [Xp] is computed using the SVD, which\n"
      "means that even a column of all zero covariates will not cause a\n"
      "singular matrix problem.\n"
      "\n"
      "In addition, the matrix [Xi] = inverse[Xc'Xc] is computed.  Its diagonal\n"
      "elements are needed in the t-test computations.  In the above example,\n"
      "\n"
      "      [ 0.2 0        0       ]\n"
      " Xi = [ 0   0.29331 -0.23556 ]\n"
      "      [ 0  -0.23556  0.22912 ]\n"
      "\n"
      "For a 1-sample t-test, the regression values computed in [b] are the\n"
      "'_mean' values stored in the output dataset.  The t-statistics are\n"
      "computed by first calculating the regression residual vector\n"
      "  [r] = [Xc][b] - [z]  (the mismatch between the data and the model)\n"
      "and then the estimated variance v of the residuals is given by\n"
      "\n"
      "        i=N\n"
      "  q = sum  { r[i]*r[i] }  and then  v = q / (N-m)\n"
      "        i=1\n"
      "\n"
      "where N=number of data points and m=number of matrix columns=number of\n"
      "parameters estimated in the regression model.  The t-statistic for the\n"
      "k-th element of [b] is then given by\n"
      "\n"
      "  t[k] = b[k] / sqrt( v * Xi[k,k] )\n"
      "\n"
      "Note that for the first element, the factor Xi[1,1] is just 1/N, as\n"
      "is the case in the simple (no covariates) t-test.\n"
      "\n"
      "For a 2-sample unpaired t-test, the '_mean' output for the k-th column\n"
      "of the matrix [X] is bA[k]-bB[k] where 'A' and 'B' refer to the 2 input\n"
      "collections of datasets.  The t-statistic is computed by\n"
      "\n"
      "  vAB  = (qA+qB) / (NA+NB-2*m)\n"
      "\n"
      "  t[k] = (bA[k]-bB[k]) / sqrt( vAB * (XiA[k,k]+XiB[k,k]) )\n"
      "\n"
      "For a 2-sample paired t-test, the t-statistic is a little different:\n"
      "\n"
      "        i=N\n"
      "  q = sum  { (rA[i]-rB[i])^2 }  and then  vAB = q / (N-m)\n"
      "        i=1\n"
      "\n"
      "and then\n"
      "\n"
      "  t[k] = (bA[k]-bB[k]) / sqrt( vAB * XiA[k,k] )\n"
      "\n"
      "A paired t-test is basically a 1-sample test with the 'data' being\n"
      "the difference [zA]-[zB] of the two input samples.\n"
      "\n"
      "Note the central role of the diagonal elements of the [Xi] matrix.\n"
      "These numbers are the variances of the estimates of the [b] if the\n"
      "data [z] is corrupted by additive white noise with variance=1.\n"
      "(In the case of an all zero column of covariates, the SVD inversion)\n"
      "(that yields [Xi] will make that diagonal element 0.  Division by 0)\n"
      "(being a not-good thing, in such a case Xi[k,k] is replaced by 1e9.)\n"
      "\n"
      "For cases with voxel-wise covariates, each voxel gets a different\n"
      "[X] matrix, and so the matrix inversions are carried out many many\n"
      "times.  If the covariates are fixed values, then only one set of\n"
      "matrix inversions needs to be carried out.\n"
      "\n"

      "-------------------------------------------\n"
      "HOW SINGLETON TESTING WORKS WITH COVARIATES\n"
      "-------------------------------------------\n"
      "\n"
      "(1) For setB, the standard regression is carried out to give the\n"
      "    covariate slope estimates (at each voxel):\n"
      "      [b] = [Xp] [z]\n"
      "    where [z]  = column vector of the setB values\n"
      "          [Xp] = pseudo-inverse of the [X] matrix for the setB covariates\n"
      "          [b]  = covariate parameter estimates\n"
      "    Under the usual assumptions, [b] has mean [b_truth] and covariance\n"
      "    matrix sigma^2 [Xi], where sigma^2 = variance of the zB values, and\n"
      "    [Xi] = inverse[X'X].  (Again, ' = tranpose.)\n"
      "    (If centering is used, [X] is replaced by [Xc] in all of the above.)\n"
      "\n"
      "(2) Call the singletonA value (at each voxel) y;\n"
      "    then the statistical model for y is\n"
      "       y = yoff + [c]'[b_truth] + Normal(0,sigma^2)\n"
      "    where the column vector [c] is the transpose of the 1-row matrix [X]\n"
      "    for the singletonA dataset -- that is, the first element of [c] is 1,\n"
      "    and the other elements are the covariate values for this dataset.\n"
      "    (The null hypothesis is that the mean offset yoff is 0.)\n"
      "    The covariate slopes [b] from step (1) are projected out of y now:\n"
      "      y0 = y - [c]'[b]\n"
      "    which under the null hypothesis has mean 0 and variance\n"
      "      sigma^2 ( 1 + [c]'[Xi][c] )\n"
      "    Here, the '1' comes from the variance of y, and the [c]'[Xi][c] comes\n"
      "    from the variance of [b] dotted with [c].  Note that in the trivial\n"
      "    case of no covariates, [X] = 1-column matrix of all 1s and [c] = scalar\n"
      "    value of 1, so [c]'[Xi][c] = 1/N where N = number of datasets in setB.\n"
      "\n"
      "(3) sigma^2 is as usual estimated by s^2 = sum[ (z_i - mean(z))^2 ] / (N-m-1)\n"
      "    where N = number of datasets in setB and m = number of covariates.\n"
      "    Under the usual assumptions, s^2 is distributed like a random variable\n"
      "    ( sigma^2 / (N-m-1) ) * ChiSquared(N-m-1).\n"
      "\n"
      "(4) Consider the test statistic\n"
      "      tau = y0 / sqrt(s^2)\n"
      "    Under the null hypothesis, this has the distribution of a random variable\n"
      "      Normal(0,1 + [c]'[Xi][c]) / sqrt( ChiSquared(N-m-1)/(N-m-1) )\n"
      "    So tau is not quite t-distributed, but dividing out the scale factor works:\n"
      "      t = y0 / sqrt( s^2 * (1 + [c]'[Xi][c]) )\n"
      "    and under the null hypothesis, this value t has a Student(N-m-1) distribution.\n"
      "    Again, note that in the case of no covariates, [c]'[Xi][c] = 1/N, so that\n"
      "      t = y / sqrt( s^2 * (1+1/N) )\n"
      "    If we were testing against a constant y, rather than y itself being random,\n"
      "    we'd have\n"
      "      t_con = y / sqrt( s^2 / (N-1) )\n"
      "    which shows that the t statistic for the '-singletonA' test will usually be\n"
      "    much smaller than the t statistic for the 'test against constant' case --\n"
      "    because we have to allow for the variance of the singleton dataset value y.\n"
      "\n"
      "Please note that the singleton dataset is assumed to be statistically\n"
      "independent of the reference datasets -- if you put the singleton dataset\n"
      "into the reference collection, then you are violating this assumption --\n"
      "a different statistic would have to be computed.\n"
      "\n"
      "A test script that simulates random values and covariates has verified the\n"
      "distribution of the results in both the null hypothesis (yoff == 0) case and the\n"
      "alternative hypothesis (yoff !=0) case -- where the value t now takes on the\n"
      "non-central Student distribution.\n"
      "\n"
      "Below is a sketch of how a covariate might be useful in singleton tests:\n"
      " * the 'z' labels are voxel values from setB\n"
      " * the 'y' label is the voxel value from singletonA\n"
      " * y is not markedly different from some of the z values\n"
      " * but for the singleton subject's age, y IS very different\n"
      " * a test WITHOUT the age covariate would not give a large t-statistic for y\n"
      " * a test WITH the age covariate will show a larger t-statistic for y\n"
      "              --------------------------------\n"
      "            D |                   z          |\n"
      "            a |                      z       |\n"
      "            t |              z  z  z   z     |\n"
      "            a |            z z z  z          |\n"
      "              |          z z  z  z  z        |\n"
      "            v |        z z   z  z z          |\n"
      "            a |       z z   z z z            |\n"
      "            l |    z  z   z   z              |\n"
      "            u |   z    z   z           y     |\n"
      "            e |      z  z                    |\n"
      "              |                              |\n"
      "              |                              |\n"
      "              |                              |\n"
      "              --------------------------------\n"
      "                     Subject age\n"
      "\n"
      "After linear regression removes the covariate effect (values at smaller\n"
      "ages are increased and values at larger ages are decreased), the cartoon\n"
      "graph would look something like this, where the modified y value is\n"
      "now clearly far away from the cluster of z values:\n"
      "              --------------------------------\n"
      "          R D |                              |\n"
      "          e a |                              |\n"
      "          g t |    z       z z               |\n"
      "          r a |   z   zz z z z  z z          |\n"
      "          e   |       z  z    zz             |\n"
      "          s v |      z  z    z     z z       |\n"
      "          s a |        z  z z z zzz    z     |\n"
      "          e l |            z  z z            |\n"
      "          d u |         z         z z        |\n"
      "            e |                              |\n"
      "              |                              |\n"
      "              |                              |\n"
      "              |                        y     |\n"
      "              --------------------------------\n"
      "                     Subject age\n"
      "\n"
      "---------------------\n"
      "A NOTE ABOUT p-VALUES (everyone's favorite subject :-)\n"
      "---------------------\n"
      "\n"
      "The 2-sided p-value of a t-statistic value T is the likelihood (probability)\n"
      "that the absolute value of the t-statistic computation would be bigger than\n"
      "the absolute value of T, IF the null hypothesis of no difference in the means\n"
      "(2-sample test) were true.  For example, with 30 degrees of freedom, a T-value\n"
      "of 2.1 has a p-value of 0.0442 -- that is, if the null hypothesis is true\n"
      "and you repeated the experiment a lot of times, only 4.42%% of the time would\n"
      "the T-value get to be 2.1 or bigger (and -2.1 or more negative).\n"
      "\n"
      "You can NOT interpret this to mean that the alternative hypothesis (that the\n"
      "means are different) is 95.58%% likely to be true.  (After all, this T-value\n"
      "shows a pretty weak effect size -- difference in the means for a 2-sample\n"
      "t-test, magnitude of the mean for a 1-sample t-test, scaled by the standard\n"
      "deviation of the noise in the samples.)  A better way to think about it is\n"
      "to pose the following question:\n"
      "     Assuming that the alternative hypothesis is true, how likely\n"
      "     is it that you would get the p-value of 0.0442, versus how\n"
      "     likely is p=0.0442 when the null hypothesis is true?\n"
      "This is the question addressed in the paper\n"
      "     Calibration of p Values for Testing Precise Null Hypotheses.\n"
      "     T Sellke, MJ Bayarri, and JO Berger.\n"
      "     The American Statistician v.55:62-71, 2001.\n"
      "     http://www.stat.duke.edu/courses/Spring10/sta122/Labs/Lab6.pdf\n"
      "The exact interpretation of what the above question means is somewhat\n"
      "tricky, depending on if you are a Bayesian heretic or a Frequentist\n"
      "true believer.  But in either case, one reasonable answer is given by\n"
      "the function\n"
      "     alpha(p) = 1 / [ 1 - 1/( e * p * log(p) ) ]\n"
      "(where 'e' is 2.71828... and 'log' is to the base 'e').  Here,\n"
      "alpha(p) can be interpreted as the likelihood that the given p-value\n"
      "was generated by the null hypothesis, versus being from the alternative\n"
      "hypothesis.  For p=0.0442, alpha=0.2726; in non-quantitative words, this\n"
      "p-value is NOT very strong evidence that the alternative hypothesis is true.\n"
      "\n"
      "Why is this so -- why isn't saying 'the null hypothesis would only give\n"
      "a result this big 4.42%% of the time' similar to saying 'the alternative\n"
      "hypothesis is 95.58%% likely to be true'?  The answer is because it is\n"
      "only somewhat more likely the t-statistic would be that value when the\n"
      "alternative hypothesis is true.  In this example, the difference in means\n"
      "cannot be very large, or the t-statistic would almost certainly be larger.\n"
      "But with a small difference in means (relative to the standard deviation),\n"
      "the alternative hypothesis (noncentral) t-value distribution isn't that\n"
      "different than the null hypothesis (central) t-value distribution.  It is\n"
      "true that the alternative hypothesis is more likely to be true than the\n"
      "null hypothesis (when p < 1/e = 0.36788), but it isn't AS much more likely\n"
      "to be true than the p-value itself seems to say.\n"
      "\n"
      "In short, a small p-value says that if the null hypothesis is true, the\n"
      "experimental results that you have aren't very likely -- but it does NOT\n"
      "say that the alternative hypothesis is vastly more likely to be correct,\n"
      "or that the data you have are vastly more likely to have come from the\n"
      "alternative hypothesis case.\n"
      "\n"
      "Some values of alpha(p) for those too lazy to calculate just now:\n"
      "     p = 0.0005 alpha = 0.010225\n"
      "     p = 0.001  alpha = 0.018431\n"
      "     p = 0.005  alpha = 0.067174\n"
      "     p = 0.010  alpha = 0.111254\n"
      "     p = 0.015  alpha = 0.146204\n"
      "     p = 0.020  alpha = 0.175380\n"
      "     p = 0.030  alpha = 0.222367\n"
      "     p = 0.040  alpha = 0.259255\n"
      "     p = 0.050  alpha = 0.289350\n"
      "You can also try this AFNI package command to plot alpha(p) vs. p:\n"
      "     1deval -dx 0.001 -xzero 0.001 -num 99 -expr '1/(1-1/(exp(1)*p*log(p)))' |\n"
      "       1dplot -stdin -dx 0.001 -xzero 0.001 -xlabel 'p' -ylabel '\\alpha(p)'\n"
      "Another example: to reduce the likelihood of the null hypothesis being the\n"
      "source of your t-statistic to 10%%, you have to have p = 0.008593 -- a value\n"
      "more stringent than usually seen in scientific publications.  To get the null\n"
      "hypothesis likelihood below 5%%, you have to get p below 0.003408.\n"
      "\n"
      "Finally, none of the discussion above is limited to the case of p-values that\n"
      "come from 2-sided t-tests.  The function alpha(p) applies (approximately) to\n"
      "many other situations.  However, it does NOT apply to 1-sided tests (which\n"
      "are not testing 'Precise Null Hypotheses').  See the paper by Sellke et al.\n"
      "for a lengthier and more precise discussion.  Another paper to peruse is\n"
      "     Revised standards for statistical evidence.\n"
      "     VE Johnson.  PNAS v110:19313-19317, 2013.\n"
      "     http://www.pnas.org/content/110/48/19313.long\n"
      "For the case of 1-sided t-tests, the issue is more complex; the paper below\n"
      "may be of interest:\n"
      "     Default Bayes Factors for Nonnested Hypthesis Testing.\n"
      "     JO Berger and J Mortera.  J Am Stat Assoc v:94:542-554, 1999.\n"
      "     http://www.jstor.org/stable/2670175 [PDF]\n"
      "     http://ftp.isds.duke.edu/WorkingPapers/97-44.ps [PS preprint]\n"
      "What I have tried to do herein is outline the p-value interpretation issue\n"
      "using (mostly) non-technical words.\n"
      "\n"
      "((***** What does this all mean for FMRI?  I'm still thinking about it. *****))\n"

      "\n"
      "--------------------\n"
      "TESTING THIS PROGRAM\n"
      "--------------------\n"
      "\n"
      "A simple 2-sample test of this program is given by the script below,\n"
      "which creates 2 datasets with standard deviation (sigma) of 1; the\n"
      "first one (ZZ_1) has mean 1 and the second one (ZZ_0) has mean 0;\n"
      "then the program tests these datasets to see if their means are different,\n"
      "and finally prints out the average value of the estimated differences\n"
      "in their means, and the average value of the associated t-statistic:\n"
      "   3dUndump -dimen 128 128 32 -prefix ZZ\n"
      "   3dcalc -a ZZ+orig -b '1D: 14@0' -expr 'gran(1,1)' -prefix ZZ_1.nii -datum float\n"
      "   3dcalc -a ZZ+orig -b '1D: 10@0' -expr 'gran(0,1)' -prefix ZZ_0.nii -datum float\n"
      "   3dttest++ -setA ZZ_1.nii -setB ZZ_0.nii -prefix ZZtest.nii -no1sam\n"
      "   echo '=== mean of mean estimates follows, should be about 1 ==='\n"
      "   3dBrickStat -mean ZZtest.nii'[0]'\n"
      "   echo '=== mean of t-statistics follows, should be about 2.50149 ==='\n"
      "   3dBrickStat -mean ZZtest.nii'[1]'\n"
      "   \\rm ZZ*\n"
      "The expected value of the t-statistic with 14 samples in setA and\n"
      "10 samples in setB is calculated below:\n"
      "   delta_mean / sigma / sqrt( 1/NA + 1/NB ) / (1 - 3/(4*NA+4*NB-9) )\n"
      " =      1     / 1     / sqrt( 1/14 + 1/10 ) / (1 - 3/87            ) = 2.50149\n"
      "where division by (1-3/(4*NA+4*NB-9)) is the correction factor\n"
      "for the skewness of the non-central t-distribution --\n"
      "see http://en.wikipedia.org/wiki/Noncentral_t-distribution .\n"

      "\n"
      "-------------------------\n"
      "VARIOUS LINKS OF INTEREST\n"
      "-------------------------\n"
      "\n"
      "* http://en.wikipedia.org/wiki/T_test\n"
      "* http://www.statsoft.com/textbook/basic-statistics/\n"
      "* http://en.wikipedia.org/wiki/Mutatis_mutandis\n"
      "\n"
      "---------------------------------------------------\n"
      "AUTHOR -- RW Cox -- don't whine TO me; wine WITH me (e.g., a nice Pinot Noir)\n"
      "---------------------------------------------------\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*--------------------------------------------------------------------------*/
/* Can this option be a label or subject name?  [ZSS Nov 29 2011] */

int is_possible_filename( char * fname )
{
    int mode;

    mode = storage_mode_from_filename(fname);

    if ( THD_is_prefix_ondisk(fname, 1) &&
         (mode > STORAGE_UNDEFINED || mode <=LAST_STORAGE_MODE ) &&
         !THD_is_directory(fname) ) return(1);

    return(0);
}

/*----------------------------------------------------------------------------*/
/* Start a job in a thread.  Save process id for later use. */

static int   njob   = 0 ;
static pid_t *jobid = NULL ;

void start_job( char *cmd )   /* 10 Feb 2016 */
{
   pid_t newid ;

   if( cmd == NULL || *cmd == '\0' ) return ;

   newid = fork() ;

   if( newid == (pid_t)-1 ){  /*--- fork failed -- should never happen ---*/

     int qq ;
     ERROR_message("----- Failure to fork for cmd = '%s'") ;
     for( qq=0 ; qq < njob ; qq++ ){
       ERROR_message("  Killing fork-ed job %d (pid=%u)",
                        qq , (unsigned int)jobid[qq]   ) ;
       kill( jobid[qq] ,SIGTERM   ) ; NI_sleep(10) ;
       waitpid( jobid[qq] , NULL , 0 ) ;
     }
     ERROR_exit("Program exits -- sorry :-(") ;

   } else if( newid > 0 ){    /*--- fork worked -- we are the original ---*/

     jobid = (pid_t *)realloc( jobid , sizeof(pid_t)*(njob+1) ) ;
     jobid[njob++] = newid ;
     return ;

   }

   /*--- we are the child -- run the command, then die die die ---*/

   system(cmd) ;
   _exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* Wait for all started jobs to finish. */

void wait_for_jobs(void)     /* 10 Feb 2016 */
{
   int qq ;

   if( njob <= 0 || jobid == NULL ) return ;
   for( qq=0 ; qq < njob ; qq++ )
     waitpid( jobid[qq] , NULL , 0 ) ;

   free(jobid) ; jobid = NULL ; njob = 0 ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Dis is de mayne porgam - RW Xoc */

int main( int argc , char *argv[] )
{
   int nopt, nbad, ii,jj,kk, kout,ivox, vstep,bstep, dconst, nconst=0, nzskip=0,nzred=0  ;
   int bb , bbase , ss ;  char *abbfmt ; /* for -brickwise -- 28 Jan 2014 */
   MRI_vectim *vimout=NULL , *rimout=NULL ;
   float *workspace=NULL , *datAAA , *datBBB=NULL , *resar ; size_t nws=0 ;
   float_pair tpair ;
   THD_3dim_dataset *outset , *bbset=NULL , *rrset=NULL ;
   char blab[64] , *stnam ;
   float dof_AB=0.0f , dof_A=0.0f , dof_B=0.0f ;
   int BminusA=-1 , ntwosam=0 ;  /* 05 Nov 2010 */
   int dupe_ok=0;  /* 1 Jun 2015 [rickr] */
   char *snam_PPP=NULL, *snam_MMM=NULL ;
   static int iwarn=0;
   FILE *fp_sdat = NULL ; short *sdatar=NULL ; char *sdat_prefix=NULL ;

   /*--- help the piteous luser? ---*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

   /*--- record things for posterity, et cetera ---*/

   mainENTRY("3dttest++ main"); machdep(); AFNI_logger("3dttest++",argc,argv);
   PRINT_VERSION("3dttest++") ; AUTHOR("Zhark++") ;

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   /*--- read the options from the command line ---*/

   PUTENV("AFNI_GLOB_SELECTORS","YES") ;  /* 19 Jun 2012 */

   nopt = 1 ;
   debug = AFNI_yesenv("AFNI_DEBUG") ;
   while( nopt < argc ){

     if( debug ) INFO_message("=== argv[%d] = %s",nopt,argv[nopt]) ;

     /*----- brickwise [28 Jan 2014] -----*/

     if( strcasecmp(argv[nopt],"-brickwise") == 0 ){ /* 28 Jan 2014 */
       if( ndset_AAA > 0 || ndset_BBB > 0 )
         ERROR_exit("-brickwise option must be BEFORE any -set option!") ;
       brickwise = 1 ; nopt++ ; continue ;
     }

     /*----- no1sam -----*/

     if( strcasecmp(argv[nopt],"-no1sam") == 0 ){   /* 10 Nov 2010 */
       do_1sam = 0 ; nopt++ ; continue ;
     }

     /*----- nocov -----*/

     if( strcasecmp(argv[nopt],"-nocov") == 0 ){    /* 22 Jul 2016 */
       do_cov = 0 ; nopt++ ; continue ;
     }

     /*----- nomeans -----*/

     if( strcasecmp(argv[nopt],"-nomeans") == 0 ){  /* 05 Feb 2014 */
       do_means = 0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-notests") == 0 ){  /* 05 Feb 2014 */
       do_tests = 0 ; nopt++ ; continue ;
     }

#ifdef ALLOW_RANK
     /*----- rankize -----*/

     if( strcasecmp(argv[nopt],"-rankize") == 0 ){  /* 10 Nov 2010 */
       do_ranks = 1 ; do_1sam = 0 ; nopt++ ; continue ;
     }
#endif

     /*----- BminusA -----*/

     if( strcasecmp(argv[nopt],"-BminusA") == 0 ){  /* 05 Nov 2010 */
       BminusA = 1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-AminusB") == 0 ){
       BminusA = 0 ; nopt++ ; continue ;
     }

     /*----- zskip -----*/

     if( strcmp(argv[nopt],"-zskip")     == 0 ||
         strcmp(argv[nopt],"-skip_zero") == 0   ){  /* 06 Oct 2010 */
       nopt++ ;
       if( nopt < argc && (isdigit(argv[nopt][0]) || argv[nopt][0] == '.') ){
         float zzz ; char *cpt ;
         zzz = (float)strtod(argv[nopt++],&cpt) ;
         if( zzz > 1.0f && *cpt == '%' ) zzz *= 0.01f ;
         if( zzz > 1.0f ){
           zskip_AAA = zskip_BBB = (int)zzz ; zskip_fff = 0.0f ; do_zskip = 1 ;
         } else {
           if( zzz <= 0.0f || zzz > 1.0f ){
             WARNING_message("Illegal value after '-zskip' -- ignoring this option :-(") ;
             zskip_AAA = zskip_BBB = 0 ; zskip_fff = 0.0f ; do_zskip = 0 ;
           } else {
             zskip_AAA = zskip_BBB = 0 ; zskip_fff = zzz  ; do_zskip = 1 ;
           }
         }
       } else {
         zskip_AAA = zskip_BBB = 5 ; zskip_fff = 0.0f ; do_zskip = 1 ;
       }
       continue ;
     }

     /*----- debug -----*/

     if( strcmp(argv[nopt],"-debug") == 0 ){  /* 22 Sep 2010 */
       debug++ ; nopt++ ; continue ;
     }

     /*----- randomsign -----*/

     if( strcasecmp(argv[nopt],"-randomsign") == 0 ){  /* 31 Dec 2015 */
       do_randomsign++ ;
       do_sdat = (argv[nopt][1] == 'R' ) ;  /* 29 Aug 2016 */
       nopt++ ;
       if( nopt < argc && isdigit(argv[nopt][0]) ){
         num_randomsign = (int)strtod(argv[nopt],NULL) ; nopt++ ;
       } else {
         num_randomsign = 1 ;
       }
       do_sdat = do_sdat && (num_randomsign > 1) ;  /* 29 Aug 2016 */
       continue ;
     }

     /*----- -clustsim njob [10 Feb 2016] -----*/

     if( strcasecmp(argv[nopt],"-clustsim") == 0 ){
       char *uuu ;
       if( do_clustsim )
         WARNING_message("Why do you use -clustsim more than once?!") ;
       if( do_Xclustsim )
         ERROR_exit("You can't use -clustsim and -Xclustsim together!") ;
       toz = 1 ;

       clustsim_prog = "3dClustSim" ;
       clustsim_opt  = argv[nopt] ;

                                  do_clustsim = 2 ;  /* 1 is no longer used */
       if( argv[nopt][1] == 'C' ) do_clustsim = 2 ;
       if( argv[nopt][2] == 'L' ) do_clustsim = 3 ;

       /* if next option is a number, it is the number of CPUs to use */

       nopt++ ;
       if( nopt < argc && isdigit(argv[nopt][0]) ){
         num_clustsim = (int)strtod(argv[nopt],NULL) ; nopt++ ;
         if( num_clustsim > 99 ){
           WARNING_message("CPU count after -clustsim is > 99") ; num_clustsim = 99 ;
         } else if( num_clustsim < 1 ){
           WARNING_message("CPU count after -clustsim is < 1" ) ; num_clustsim = 1 ;
         }
       }

       /* make sure number of CPUs are set to reasonable values */

       if( num_clustsim == 0 ){
         num_clustsim = AFNI_get_ncpu() ;  /* will be at least 1 */
         jj = (int)AFNI_numenv("OMP_NUM_THREADS") ;
         if( jj > 0 && (jj < num_clustsim || num_clustsim == 1) ) num_clustsim = jj ;
       }

       INFO_message("Number of -clustsim threads set to %d",num_clustsim) ;
       if( prefix_clustsim == NULL ){
         uuu = UNIQ_idcode_11() ;
         prefix_clustsim = (char *)malloc(sizeof(char)*32) ;
         sprintf(prefix_clustsim,"TT.%s",uuu) ; free(uuu) ;
         ININFO_message("Default clustsim prefix set to '%s'",prefix_clustsim) ;
       }
       continue ;
     }

     /*----- -Xclustsim njob [30 Aug 2016] -----*/

     if( strcasecmp(argv[nopt],"-Xclustsim") == 0 ){
       char *uuu ;
       if( do_Xclustsim )
         WARNING_message("Why do you use -Xclustsim more than once?!") ;
       if( do_clustsim )
         ERROR_exit("You can't use -clustsim and -Xclustsim together!") ;
       toz = 1 ;

       clustsim_prog = "3dClustSimX" ;
       clustsim_opt  = argv[nopt] ;
       do_Xclustsim  = (argv[nopt][2] == 'c' ) ? 1 : 2 ;

       /* if next option is a number, it is the number of CPUs to use */

       nopt++ ;
       if( nopt < argc && isdigit(argv[nopt][0]) ){
         num_clustsim = (int)strtod(argv[nopt],NULL) ; nopt++ ;
         if( num_clustsim > 99 ){
           WARNING_message("CPU count after -Xclustsim is > 99") ; num_clustsim = 99 ;
         } else if( num_clustsim < 1 ){
           WARNING_message("CPU count after -Xclustsim is < 1" ) ; num_clustsim = 1 ;
         }
       }

       /* make sure number of CPUs are set to reasonable values */

       if( num_clustsim == 0 ){
         num_clustsim = AFNI_get_ncpu() ;  /* will be at least 1 */
         jj = (int)AFNI_numenv("OMP_NUM_THREADS") ;
         if( jj > 0 && (jj < num_clustsim || num_clustsim == 1) ) num_clustsim = jj ;
       }

       INFO_message("Number of -Xclustsim threads set to %d",num_clustsim) ;
       if( prefix_clustsim == NULL ){
         uuu = UNIQ_idcode_11() ;
         prefix_clustsim = (char *)malloc(sizeof(char)*32) ;
         sprintf(prefix_clustsim,"TT.%s",uuu) ; free(uuu) ;
         ININFO_message("Default Xclustsim prefix set to '%s'",prefix_clustsim) ;
       }

       /* process next argument as possible assignments to control variables */

       if( nopt < argc && argv[nopt][0] != '-' ){
         char *cpt ; int qq,nbad ;

         cpt = strcasestr(argv[nopt],"NN=") ;
         if( cpt != NULL ){
           qq = (int)strtod(cpt+3,NULL) ;
           if( qq >= 1 && qq <= 3 ) nnlev_Xclu = qq ;
           else
             WARNING_message("Illegal value after -Xclustsim 'NN='; default NN=%d is used",nnlev_Xclu);
         }

         cpt = strcasestr(argv[nopt],"sid=") ;
         if( cpt != NULL ){
           qq = (int)strtod(cpt+4,NULL) ;
           if( qq >= 1 && qq <= 2 ) sid_Xclu = qq ;
           else
             WARNING_message("Illegal value after -Xclustsim 'sid='; default sid=%d is used",sid_Xclu);
         }

         cpt = strcasestr(argv[nopt],"pthr=") ;
         if( cpt != NULL ){
           NI_float_array *nfar = NI_decode_float_list(cpt+5,",") ;
           if( nfar != NULL || nfar->num > 0 ){
             for( nbad=qq=0 ; qq < nfar->num ; qq++ ){
               if( nfar->ar[qq] < 0.0001f || nfar->ar[qq] > 0.1f ) nbad++ ;
             }
             if( nbad > 0 ){
               ERROR_exit("Illegal values after -Xclustsim 'pthr=' ; defaults are used") ;
             } else {
               npthr_Xclu = nfar->num ;
                pthr_Xclu = nfar->ar ;
             }
           }
         }

         nopt++ ;
       }

       continue ;
     }

     /*----- -prefix_clustsim cc [11 Feb 2016] -----*/

     if( strcasecmp(argv[nopt],"-prefix_clustsim" ) == 0 ||
         strcasecmp(argv[nopt],"-prefix_Xclustsim") == 0   ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       prefix_clustsim = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix_clustsim) )
         ERROR_exit("-prefix_clustsim '%s' is not acceptable",prefix_clustsim) ;
       nopt++ ; continue ;
     }

     /*----- -tempdir [20 Jul 2016] -----*/

     if( strcasecmp(argv[nopt],"-tempdir") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       tempdir = strdup(argv[nopt]) ;
       if( !THD_filename_ok(tempdir) )
         ERROR_exit("-tempdir '%s' is not acceptable",tempdir) ;
       nopt++ ; continue ;
     }

     /*----- dofsub -----*/

     if( strcmp(argv[nopt],"-dofsub") == 0 ){  /* 19 Jan 2016 [hidden option] */
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       if( !isdigit(argv[nopt][0]) )
         ERROR_exit("Value after '%s' must be a number",argv[nopt-1]) ;
       dofsub = (int)strtod(argv[nopt],NULL) ;
       nopt++ ; continue ;
     }

     /*----- dupe_ok -----*/

     if( strcmp(argv[nopt],"-dupe_ok") == 0 ){  /* 01 Jun 2015 [rickr] */
       dupe_ok = 1 ; nopt++ ; continue ;
     }

     /*----- cmeth -----*/

     if( strcmp(argv[nopt],"-cmeth") == 0 ){  /* 26 Mar 2013 */
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
            if( strcasecmp(argv[nopt],"median") == 0 ) center_meth = CMETH_MEDIAN;
       else if( strcasecmp(argv[nopt],"mean"  ) == 0 ) center_meth = CMETH_MEAN  ;
       else
         WARNING_message("Unknown -cmeth option '%s'",argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*----- center -----*/

     if( strcmp(argv[nopt],"-center") == 0 ){  /* 30 Jul 2010 */
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       switch( argv[nopt][0] ){
         case 'n': case 'N': center_code = CENTER_NONE ; break ;
         case 'd': case 'D': center_code = CENTER_DIFF ; break ;
         case 's': case 'S': center_code = CENTER_SAME ; break ;
         default:
           WARNING_message(
             "Unknown -center option '%s' -- using 'DIFF'",argv[nopt]) ;
           center_code = CENTER_DIFF ;
         break ;
       }
       nopt++ ; continue ;
     }

     /*----- prefix -----*/

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Prefix '%s' is not acceptable",prefix) ;
       nopt++ ; continue ;
     }

     /*----- mask -----*/

     if( strcmp(argv[nopt],"-mask") == 0 ){
       bytevec *bvec ;
       if( mask != NULL )
         ERROR_exit("Can't use '-mask' twice!") ;
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       bvec = THD_create_mask_from_string(argv[nopt]) ;
       name_mask = strdup(argv[nopt]) ;  /* 10 Feb 2016 */
       if( bvec == NULL )
         ERROR_exit("Can't create mask from '-mask' option") ;
       mask = bvec->ar ; nmask = bvec->nar ;
       nmask_hits = THD_countmask( nmask , mask ) ;
       if( nmask_hits > 0 )
         INFO_message("%d voxels in -mask dataset",nmask_hits) ;
       else
         ERROR_exit("no nonzero voxels in -mask dataset") ;
       nopt++ ; continue ;
     }

     /*----- statistics options -----*/

     if( strcasecmp(argv[nopt],"-pooled") == 0 ){
       ttest_opcode = 0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-unpooled") == 0 ){
       ttest_opcode = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-paired") == 0 ){
       ttest_opcode = 2 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-toz") == 0 ){
       toz = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-labelA") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       lnam_AAA = strdup(argv[nopt]) ; LTRUNC(lnam_AAA) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-labelB") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       lnam_BBB = strdup(argv[nopt]) ; LTRUNC(lnam_BBB) ;
       nopt++ ; continue ;
     }

     /*----- the various flavours of '-set' -----*/

     if( strncmp(argv[nopt],"-set",4) == 0 ){
       char cc=argv[nopt][4] , *onam=argv[nopt] , *cpt, *labcheck;
       int nds=0 , ids , nv=0 ; char **nams=NULL , **labs=NULL , *snam=NULL ;
       THD_3dim_dataset *qset , **dset=NULL ;

       if( cc == 'A' || cc == '2' ){
              if( ndset_AAA > 1 ) ERROR_exit("Can't use '-setA' twice!") ;
         else if( singletonA    ) ERROR_exit("Can't use '-setA' and '-singletonA' together!") ;
       } else if( cc == 'B' || cc == '1' ){
         if( ndset_BBB > 0 ) ERROR_exit("Can't use '-setB' twice!") ;
       } else {
         ERROR_exit("'%s' is not a recognized '-set' option",argv[nopt]);
       }
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;

       /* if next arg is a dataset, then all of the next args are datasets */
       /* OR, if next arg has a wildcard, then the same condition applies */

       qset = THD_open_dataset( argv[nopt] ) ;
       if( ISVALID_DSET(qset) || HAS_WILDCARD(argv[nopt]) ){  /* 19 Jun 2012: globbing */
         int nexp,iex,didex ; char **fexp ;

         if( ISVALID_DSET(qset) ){ DSET_delete(qset) ; qset = NULL ; }
         nds  = nv = 0 ;
         nams = (char **)malloc(sizeof(char *)) ;
         labs = (char **)malloc(sizeof(char *)) ;
         dset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)) ;
         for( ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){  /* process until arg */
           if( HAS_WILDCARD(argv[nopt]) ){                       /* starts with '-'  */
             MCW_file_expand( 1 , argv+nopt , &nexp , &fexp ) ; didex = 1 ;
             if( nexp == 0 )
               ERROR_exit("Option %s: cannot expand wildcard dataset from '%s'",onam,argv[nopt]) ;
           } else {
             nexp = 1 ; fexp = argv+nopt ; didex = 0 ;
           }
           for( iex=0 ; iex < nexp ; iex++ ){       /* loop over number of expansions */
             qset = THD_open_dataset( fexp[iex] ) ;
             if( !ISVALID_DSET(qset) )
               ERROR_exit("Option %s: cannot open dataset '%s'",onam,fexp[iex]) ;
             if( brickwise && brickwise_num == 0 ){
               brickwise_num = DSET_NVALS(qset) ;
               if( brickwise_num == 1 )
                 WARNING_message("-brickwise option used, but first dataset has only 1 sub-brick");
             }
             nds++ ;              /* 1 more dataset in the heap */
             if( brickwise ){
               if( DSET_NVALS(qset) != brickwise_num )
                 ERROR_exit("Option %s: dataset '%s' has %d, not %d, sub-bricks (for -brickwise)",
                            onam , fexp[iex] , DSET_NVALS(qset) , brickwise_num ) ;
               nv++ ;             /* just 1 more 'value' in the t-test in this case */
             } else {
               nv += DSET_NVALS(qset) ; /* as many 'values' as there are sub-bricks */
             }
             nams = (char **)realloc(nams,sizeof(char *)*nds) ;
             labs = (char **)realloc(labs,sizeof(char *)*nds) ;
             dset = (THD_3dim_dataset **)realloc(dset,sizeof(THD_3dim_dataset *)*nds) ;
             nams[nds-1] = strdup(fexp[iex]) ; dset[nds-1] = qset ;
             labs[nds-1] = strdup(THD_trailname(fexp[iex],0)) ;
             cpt = strchr(labs[nds-1]+1,'+')    ; if( cpt != NULL ) *cpt = '\0' ;
             cpt = strstr(labs[nds-1]+1,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
             cpt = strstr(labs[nds-1]+1,".1D")  ; if( cpt != NULL ) *cpt = '\0' ;
             LTRUNC(labs[nds-1]) ;
           }
           if( didex ) MCW_free_expand(nexp,fexp) ;
         }

         if( nv > nds ) allow_cov = 0 ;  /* multiple sub-bricks from 1 input */
         if( nv < 2 )
           ERROR_exit("Option %s (short form): need at least 2 datasets or sub-bricks",onam) ;

       } else {  /* not a dataset => label label dset label dset ... */

         if( brickwise )
           ERROR_exit("You can't use -brickwise and use the LONG FORM for a set of datasets") ;

         if( strstr(argv[nopt],"+orig") != NULL ||  /* 25 Apr 2014 */
             strstr(argv[nopt],"+tlrc") != NULL ||
             strstr(argv[nopt],".nii" ) != NULL   )
           WARNING_message("-set%c: group label '%s' looks like a dataset name but isn't -- is this OK ?!?",
                           cc , argv[nopt] ) ;

         snam = strdup(argv[nopt]) ; LTRUNC(snam) ;
         for( nopt++ ; nopt < argc && argv[nopt][0] != '-' ; nopt+=2 ){
           if( nopt+1 >= argc || argv[nopt+1][0] == '-' ){
             ERROR_message(
              "Option %s: ends prematurely after option %s.\n"
              "   Make sure you are properly formatting your -set[A/B] parameters.\n"
              "   Search for 'SHORT FORM' and 'LONG FORM' in the output of %s -help\n"
               ,onam, argv[nopt], argv[0]) ;
             exit(1);
           }

           /* Check if the label looks like a dataset name;
               if so, warn the luser s/he's being an idjit [25 Apr 2014] */

           if( strstr(argv[nopt],"+orig") != NULL ||
               strstr(argv[nopt],"+tlrc") != NULL ||
               strstr(argv[nopt],".nii" ) != NULL   )
             WARNING_message("-set%c: dataset label '%s' looks like a dataset name -- is this OK ?!?",
                             cc , argv[nopt] ) ;

           qset = THD_open_dataset( argv[nopt+1] ) ;
           if( !ISVALID_DSET(qset) )
             ERROR_exit("Option %s: can't open dataset '%s'",onam,argv[nopt+1]) ;
           if( DSET_NVALS(qset) > 1 )
             ERROR_exit("Option %s: dataset '%s' has more than one sub-brick",
                        onam,argv[nopt+1]) ;
           nds++ ; nv++ ;
           nams = (char **)realloc(nams,sizeof(char *)*nds) ;
           labs = (char **)realloc(labs,sizeof(char *)*nds) ;
           dset = (THD_3dim_dataset **)realloc(dset,sizeof(THD_3dim_dataset *)*nds) ;
           nams[nds-1] = strdup(argv[nopt+1]) ; dset[nds-1] = qset ;
           labcheck = argv[nopt];
           labs[nds-1] = strdup(argv[nopt]  ) ; LTRUNC(labs[nds-1]) ;
           /* check syntax */
           if (!iwarn &&
               (is_possible_filename( labcheck ) )) {
              WARNING_message(
               "Label %s (%s) appears to be a file on disk.\n"
               "  Perhaps your command line syntax for %s is incorrect.\n"
               "  Look for 'SHORT FORM' and 'LONG FORM' in output of %s -help\n"
               "  Similar warnings will be muted.\n"
               ,labcheck, labs[nds-1], onam, argv[0]) ;
              ++iwarn;
           }
         }

         if( nv < 2 )
           ERROR_exit("Option %s (long form): need at least 2 datasets",onam) ;
       }

       /* check for grid size mismatch */

       for( nbad=0,ids=1 ; ids < nds ; ids++ ){
         if( DSET_NVOX(dset[ids]) != DSET_NVOX(dset[0]) ){
           ERROR_message("Option %s: dataset '%s' does not match first one in size",
                         onam,nams[ids]) ; nbad++ ;
           ININFO_message("'%s' has %d voxels, but first one '%s' has %d voxels",
                          nams[ids],DSET_NVOX(dset[ids]) ,
                          nams[0]  ,DSET_NVOX(dset[0])    ) ;
         }
       }
       if( nbad > 0 ) ERROR_exit("Cannot go on after such an error!") ;

       /* check for duplicate labels */

       { int max_report = 7;
         for( ii=0 ; ii < nds ; ii++ ){
           for( jj=ii+1 ; jj < nds ; jj++ )
             if( strcmp(labs[ii],labs[jj]) == 0 ) {
               nbad++ ;
               if( ! dupe_ok ) {
                 if( nbad < max_report )
                   WARNING_message("label match %d=%s, %d=%s\n",
                                   ii, labs[ii], jj, labs[jj]);
                 else if( nbad == max_report ) WARNING_message(" ...");
               }
             }
         }
       }
       if( nbad > 0 ){  /* duplicate labels :-( */
         WARNING_message("Duplicate labels for datasets in option '%s'",onam) ;
         if( ! dupe_ok ) fprintf(stderr,"   (consider -dupe_ok)\n\n");
         allow_cov = -1 ;
       }

       /* assign results to global variables */

       if( cc == 'A' || cc == '2' ){
         ndset_AAA = nds  ; snam_AAA = snam ; nval_AAA = nv ;
          name_AAA = nams ; labl_AAA = labs ; dset_AAA = dset ;
       } else {
         ndset_BBB = nds  ; snam_BBB = snam ; nval_BBB = nv ;
          name_BBB = nams ; labl_BBB = labs ; dset_BBB = dset ;
       }

       continue ;  /* nopt already points to next option */

     } /*----- end of '-set' -----*/

     /*----- -resid prefix [07 Dec 2015] -----*/

     if( strcmp(argv[nopt],"-resid") == 0 ){
       do_resid = 1 ;
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       prefix_resid = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix_resid) )
         ERROR_exit("-resid prefix '%s' is not acceptable",prefix_resid) ;
       nopt++ ; continue ;
     }

     /*----- -singletonA [19 Mar 2015] -----*/

     if( strcmp(argv[nopt],"-singletonA") == 0 ){
       char *cpt, *qnam , *lnam ;
       int nds=0 , ids , nv=0 ; char **nams=NULL , **labs=NULL , *snam=NULL ;
       THD_3dim_dataset *qset , **dset=NULL ;

       if( ndset_AAA > 1 )
         ERROR_exit("Cannot use '-singletonA' and '-setA' together!") ;
       if( singletonA )
         ERROR_exit("Cannot use '-singletonA' twice!") ;
       if( brickwise )
         ERROR_exit("Cannot use '-singletonA' and '-brickwise' together :-(") ;
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       if( HAS_WILDCARD(argv[nopt]) )
         ERROR_exit("Argument after '-singletonA' has wildcard -- this is not allowed!") ;

       /* if the next arg is a number, then we use a fixed value [08 Dec 2015] */

       { float val = (float)strtod(argv[nopt],&cpt) ;
         if( val != -666.0f && *cpt == '\0' ){
           singleton_fixed_val = val ;
           use_singleton_fixed_val = 1 ;
           singleton_variance_ratio = 0.000001f ;
           BminusA = 1 ;
           INFO_message("-singletonA testing against fixed value = %g",val) ;
           ININFO_message(" ==> option -BminusA is automatically turned on") ;
         }
       }

       /* if next arg is a dataset, then it is the singleton dataset;
          otherwise, it is the label and the FOLLOWING arg is the dataset */

       if( use_singleton_fixed_val ){  /* 08 Dec 2015 */
         qset = NULL ;
         lnam = qnam = (char *)malloc(sizeof(char)*32) ;
         sprintf(qnam,"C=%g",singleton_fixed_val) ; nopt++ ;
       } else {
         qset = THD_open_dataset( argv[nopt] ) ;

         if( ISVALID_DSET(qset) ){   /* 1st arg is dataset name (and the label) */
           lnam = argv[nopt] ;
           qnam = argv[nopt] ; nopt++ ;
         } else {                    /* 1st arg is dataset label, second is name */
           if( strstr(argv[nopt],"+orig") != NULL ||
               strstr(argv[nopt],"+tlrc") != NULL ||
               strstr(argv[nopt],".nii" ) != NULL   )
             WARNING_message("-singletonA: dataset label '%s' looks like a dataset name but isn't -- is this OK ?!?",
                             argv[nopt] ) ;
           if( is_possible_filename(argv[nopt]) )
             WARNING_message("-singletonA: dataset label '%s' looks like it is also a filename on disk -- is this OK ?!?",
                             argv[nopt] ) ;

           qset = THD_open_dataset( argv[nopt+1] ) ;
           if( !ISVALID_DSET(qset) )
             ERROR_exit("Option -singletonA: can't open dataset '%s'",argv[nopt+1]) ;

           lnam = argv[nopt] ;
           qnam = argv[nopt+1] ; nopt += 2 ;
         }

         if( DSET_NVALS(qset) > 1 )
           ERROR_exit("-singletonA dataset '%s' has more than 1 sub-brick -- not allowed!",qnam) ;
       }

       nams = (char **)malloc(sizeof(char *)) ;
       labs = (char **)malloc(sizeof(char *)) ;
       dset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)) ;

       nams[0] = strdup(qnam) ;
       dset[0] = qset ;
       labs[0] = strdup(THD_trailname(lnam,0)) ;
       cpt = strchr(labs[0]+1,'+')    ; if( cpt != NULL ) *cpt = '\0' ;
       cpt = strstr(labs[0]+1,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
       LTRUNC(labs[0]) ;

       ndset_AAA = 1    ; snam_AAA = strdup("sngltnA") ; nval_AAA = 1    ;
        name_AAA = nams ; labl_AAA = labs              ; dset_AAA = dset ;

       singletonA = 1 ;
       continue ;  /* nopt already points to next option */

     }  /* end of '-singletonA' */

     /*----- singleton_variance_ratio -----*/

     if( strcmp(argv[nopt],"-singleton_variance_ratio") == 0 ){
       float val ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       val = (float)strtod(argv[nopt],NULL) ;
       if( val <= 0.0f ) ERROR_exit("value after %s must be positive",argv[nopt-1]) ;
       if( val >  9.9f ) WARNING_message("value after %s is kind of large = %g",argv[nopt-1],val) ;
       singleton_variance_ratio = val ;
       nopt++ ; continue ;
     }

     /*----- covariates -----*/

     if( strcasecmp(argv[nopt],"-covariates") == 0 ){  /* 20 May 2010 */
       char *lab ; float sig , men ; char nlab[2048] , dlab[2048] ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       if( covnel != NULL ) ERROR_exit("can't use -covariates twice!") ;
       covnel = THD_mixed_table_read( argv[nopt] ) ; fname_cov = strdup(argv[nopt]) ;
       if( covnel == NULL ){
         ERROR_message("Can't read table from -covariates file '%s'",argv[nopt]) ;
         ERROR_message("Try re-running this program with the extra option -DAFNI_DEBUG_TABLE=YES") ;
         ERROR_exit(   "Can't continue after the above error!") ;
       }
       INFO_message("Covariates file: %d columns, each with %d rows",
                    covnel->vec_num , covnel->vec_len ) ;
       mcov = covnel->vec_num - 1 ;
       if( mcov < 1 )
         ERROR_exit("Need at least 2 columns in -covariates file!") ;
       else if( mcov > MAXCOV )
         ERROR_exit("%d covariates in file, more than max allowed (%d)",mcov,MAXCOV) ;
       lab = NI_get_attribute( covnel , "Labels" ) ;
       if( lab != NULL ){
         ININFO_message("Covariate column labels: %s",lab) ;
         covlab = NI_decode_string_list( lab , ";," ) ;
         if( covlab == NULL || covlab->num < mcov+1 )
           ERROR_exit("can't decode labels properly?!") ;
       } else {
         ERROR_exit("Can't get labels from -covariates file '%s'",argv[nopt]) ;
       }
       nlab[0] = dlab[0] = '\0' ;
       for( nbad=0,jj=1 ; jj <= mcov ; jj++ ){
         if( covnel->vec_typ[jj] == NI_FLOAT ){  /* numeric column */
           meansigma_float(covnel->vec_len,(float *)covnel->vec[jj],&men,&sig) ;
           if( sig <= 0.0f ){
             ERROR_message(
               "Numeric covariate column '%s' is constant '%f'; can't be used!" ,
               covlab->str[jj] , men ) ; nbad++ ;
           }
           strcat(nlab,covlab->str[jj]) ; strcat(nlab," ") ;
         } else {                                /* string column: */
           char **qpt = (char **)covnel->vec[jj] ; int nsame=1 ;
           for( kk=1 ; kk < covnel->vec_len ; kk++ ){
             if( strcmp(qpt[0],qpt[kk]) == 0 ) nsame++ ;
           }
           if( nsame == covnel->vec_len ){
             ERROR_message(
               "Dataset covariate column '%s' is constant '%s'; can't be used!",
               covlab->str[jj] , qpt[0] ) ; nbad++ ;
           }
           num_covset_col++ ;              /* count number of them */
           strcat(dlab,covlab->str[jj]) ; strcat(dlab," ") ;
         }
         LTRUNC(covlab->str[jj]) ;
       }
       if( nbad > 0 ) ERROR_exit("Cannot continue past above ERROR%s :-(",
                                  (nbad==1) ? "\0" : "s" ) ;
       if( mcov-num_covset_col > 0 )
         ININFO_message("Found %d numeric column%s: %s",
                        mcov-num_covset_col , (mcov-num_covset_col==1) ? "\0" : "s" ,
                        nlab ) ;
       if( num_covset_col > 0 )
         ININFO_message("Found %d dataset name column%s: %s",
                      num_covset_col , (num_covset_col==1) ? "\0" : "s" , dlab ) ;
       nopt++ ; continue ;
     }

     /*----- bad user, bad bad bad -----*/

     ERROR_exit("3dttest++: don't recognize option '%s' (argv[%d])",argv[nopt],nopt) ;

   }  /*-------------------- end of option parsing --------------------*/

   /*----- check some stuff -----*/

   if( !brickwise ) brickwise_num = 1 ;      /* 28 Jan 2014 */

   if( brickwise && do_randomsign )          /* 02 Feb 2016 */
     ERROR_exit("You can't use -brickwise and -randomsign together!") ;

   if( brickwise && (do_clustsim || do_Xclustsim) )
     ERROR_exit("You can't use -brickwise and %s together!",clustsim_opt) ;

   if( do_ranks && (do_clustsim || do_Xclustsim ) )
     ERROR_exit("Can't use -rankize and %s together :-(",clustsim_opt) ;

   if( do_randomsign && (do_clustsim || do_Xclustsim) )
     ERROR_exit("You can't use -randomsign and %s together!",clustsim_opt) ;

   if( name_mask == NULL && do_Xclustsim )
     ERROR_exit("-Xclustsim requires -mask :(") ;

   if( do_randomsign && num_randomsign > 1 ) /* 02 Feb 2016 */
     brickwise_num = num_randomsign ;

   if( do_randomsign && do_resid )           /* 02 Feb 2016 */
     ERROR_exit("You can't do -resid and -randomsign together!") ;

   if( do_tests+do_means == 0 )
     ERROR_exit("You can't use -nomeans and -notests together! (Duh)") ;

   if( debug ) INFO_message("brickwise_num set to %d",brickwise_num) ;

   if( do_clustsim || do_Xclustsim ){
     if( DSET_NY(dset_AAA[0]) < 4 || DSET_NZ(dset_AAA[0]) < 4 )  /* 21 Jul 2016 */
       ERROR_exit("You cannot use the '%s' option except on 3D datasets :-(",clustsim_opt) ;

     do_resid = 1 ;
     if( prefix_resid == NULL ){
       prefix_resid = (char *)malloc(sizeof(char)*(strlen(prefix_clustsim)+32)) ;
       sprintf(prefix_resid,"%s.resid.nii",prefix_clustsim) ;
     } else {
       if( !PREFIX_IS_NIFTI(prefix_resid) ){
         prefix_resid = (char *)realloc(prefix_resid,sizeof(char)*(strlen(prefix_resid)+32)) ;
         strcat(prefix_resid,".nii") ;
         INFO_message("running %s --> changed '-resid' prefix to NIFTI form: %s",
                      clustsim_prog , prefix_resid ) ;
       }
     }
   }

   twosam = (nval_BBB > 1) ; /* 2 sample test? */

   if( singletonA && !twosam )
     ERROR_exit("-singletonA was used, but -setB was not: this makes no sense!") ;

   if( singletonA ){ do_1sam = 0 ; ttest_opcode = 0 ; }

   if( singletonA && center_code == CENTER_SAME && mcov > 0 ){
     WARNING_message("-singletonA and -center SAME don't work together;\n"
                     "         ==> centering will be on -setB covariates only\n") ;
     center_code = CENTER_DIFF ;
   }

   if( nval_AAA <= 0 )
     ERROR_exit("No '-setA' or '-singletonA' option?\n"
               "********* Please please read the -help instructions again!") ;

   if( nval_AAA != nval_BBB && ttest_opcode == 2 )
     ERROR_exit("Can't do '-paired' with unequal set sizes: #A=%d #B=%d",
                nval_AAA , nval_BBB ) ;

   if( use_singleton_fixed_val && mcov > 0 )  /* 08 Dec 2015 */
     ERROR_exit("You can't use a fixed -singletonA constant AND use -covariates!") ;

   if( !use_singleton_fixed_val ) nvox = DSET_NVOX(dset_AAA[0]) ;
   else                           nvox = DSET_NVOX(dset_BBB[0]) ;
   if( twosam && DSET_NVOX(dset_BBB[0]) != nvox )
     ERROR_exit("-setA and -setB datasets don't match number of voxels") ;

   if( nmask > 0 && nmask != nvox )
     ERROR_exit("-mask doesn't match datasets number of voxels") ;

   if( do_zskip && mcov > 0 )
     ERROR_exit("-zskip and -covariates cannot be used together [yet] :-(") ;

   if( do_zskip && ttest_opcode == 2 )
     ERROR_exit("-zskip and -paired cannot be used together :-(") ;

   if( do_zskip && zskip_fff > 0.0f && zskip_fff <= 1.0f ){
     zskip_AAA = (int)(zskip_fff*nval_AAA) ; if( zskip_AAA < 2 ) zskip_AAA = 2 ;
     if( nval_BBB > 0 ){
       zskip_BBB = (int)(zskip_fff*nval_BBB) ; if( zskip_BBB < 2 ) zskip_BBB = 2 ;
     }
   }

   if( do_zskip && nval_AAA < zskip_AAA )
     ERROR_exit("-zskip (%d) is more than number of data values in setA (%d)",
                zskip_AAA , nval_AAA ) ;

   if( do_zskip && nval_BBB > 0 && nval_BBB < zskip_BBB )
     ERROR_exit("-zskip (%d) is more than number of data values in setB (%d)",
                nval_BBB ) ;

   if( do_zskip ){
     INFO_message("-zskip: require %d (out of %d) nonzero values for setA",
                  zskip_AAA,nval_AAA) ;
     if( nval_BBB > 0 )
       ININFO_message("    and require %d (out of %d) nonzero values for setB",
                      zskip_BBB,nval_BBB) ;
   }

   if( (!singletonA && (nval_AAA - mcov < 2) ) ||
       ( twosam     && (nval_BBB - mcov < 2) )   )
     ERROR_exit("Too many covariates (%d) compared to number of datasets in each -set",mcov) ;

   if( mcov > 0 && allow_cov <= 0 ){
     switch( allow_cov ){
       case 0:
         ERROR_exit(
          "-covariates not allowed with -set that has multiple sub-bricks from one dataset");
       case -1:
         ERROR_exit(
          "-covariates not allowed with -set that has duplicate dataset labels") ;
     }
   }

   if( ttest_opcode == 1 && mcov > 0 ){
     WARNING_message("-covariates does not support unpooled variance: switching to pooled") ;
     ttest_opcode = 0 ;
   }

   if( do_zskip && !toz ){
     toz = 1 ; INFO_message("-zskip also turns on -toz") ;
   }
   if( ttest_opcode == 1 && !toz ){
     toz = 1 ; INFO_message("-unpooled also turns on -toz") ;
   }

   if( !do_1sam && !twosam ){
     WARNING_message("-no1sam and no -setB datasets!  What do you mean?") ;
     do_1sam = 1 ;
   }

   if( brickwise && do_resid ) /* 07 Dec 2015 */
     ERROR_exit("You can't use -brickwise and -resid together :-(") ;

   if( do_zskip && do_resid )  /* 31 Dec 2015 */
     ERROR_exit("You can't use -resid and -zskip together :-(") ;

   if( do_randomsign && nval_AAA < 7 )
     ERROR_exit("You can't use -randomsign with nval_AAA=%d < 7",nval_AAA) ;
   if( do_randomsign && nval_BBB > 0 && nval_BBB < 7 )
     ERROR_exit("You can't use -randomsign with nval_BBB=%d < 7",nval_BBB) ;

   if( (do_clustsim || do_Xclustsim) && nval_AAA < 7 )
     ERROR_exit("You can't use %s with nval_AAA=%d < 7",clustsim_opt,nval_AAA) ;
   if( (do_clustsim || do_Xclustsim) && nval_BBB > 0 && nval_BBB < 7 )
     ERROR_exit("You can't use %s with nval_BBB=%d < 7",clustsim_opt,nval_BBB) ;

#ifdef ALLOW_RANK
   if( do_ranks && !twosam ){
     WARNING_message("-rankize only works with two-sample tests ==> ignoring") ;
     do_ranks = 0 ; do_1sam = 1 ;
   } else if( do_ranks && singletonA ){
     WARNING_message("-rankize doesn't work with -singletonA ==> ignoring") ;
     do_ranks = 0 ;
   }
#else
   if( do_ranks ){
     WARNING_message("-rankize is disabled at this time") ;
     do_ranks = 0 ;
   }
#endif

   if( nmask == 0 ){
     INFO_message("no mask ==> processing all %d voxels",nvox) ;
     nmask_hits = nvox ;
   }

   if( snam_AAA == NULL )
     snam_AAA = (lnam_AAA != NULL) ? lnam_AAA : strdup("SetA") ;
   if( snam_BBB == NULL )
     snam_BBB = (lnam_BBB != NULL) ? lnam_BBB : strdup("SetB") ;

   if( BminusA == 1 && !twosam ){
     WARNING_message("-BminusA disabled: you didn't input setB!") ;
     BminusA = 0 ;
   }
   if( BminusA == -1 ){
     BminusA = 0 ;
     if( twosam ) INFO_message("2-sample test: '-AminusB' option is assumed") ;
   }

   if( twosam ){
     snam_PPP = (BminusA) ? snam_BBB : snam_AAA ;
     snam_MMM = (BminusA) ? snam_AAA : snam_BBB ;
     if( singletonA )
       INFO_message("results will be %s - %s", snam_PPP,snam_MMM) ;
     else
       INFO_message("%s test: results will be %s - %s",
                    ttest_opcode == 2 ? "paired":"2-sample", snam_PPP,snam_MMM) ;
   }

   /*----- set up covariates in a very lengthy aside now -----*/

   if( mcov > 0 ){
     THD_3dim_dataset **qset ; int nkbad ;

     /*-- convert covariates to vectors to be loaded into matrices --*/

     /* for covariates which are just numbers, create float vector
        covvec_BBB[jj] for the jj-th covariate (jj=0..mcov-1),
        which holds the array of covariate values, nval_BBB long. */

     /* for covariates which are datasets,
        create covvim_BBB[jj] for the jj-th covariate (jj=0..mcov-1),
        which holds the vectim of covariate values, nval_BBB long.  */

     /* note that if covariates are used, nval_XXX == ndset_XXX */

     INFO_message("loading covariates") ;

     nbad = 0 ; /* total error count */
     if( twosam ){
       qset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*ndset_BBB) ;
       covvim_BBB = (MRI_vectim **)malloc(sizeof(MRI_vectim *)*mcov) ;
       covvec_BBB = (floatvec   **)malloc(sizeof(floatvec   *)*mcov) ;
       for( jj=0 ; jj < mcov ; jj++ ){                /* loop over covariates */
         covvim_BBB[jj] = NULL ;         /* initialize output vectors to NULL */
         covvec_BBB[jj] = NULL ;
         for( nkbad=kk=0 ; kk < ndset_BBB ; kk++ ){     /* loop over datasets */
           ii = string_search( labl_BBB[kk] ,     /* ii = covariate row index */
                               covnel->vec_len ,
                               (char **)covnel->vec[0] ) ;
           if( ii < 0 ){                     /* can't find it ==> this is bad */
             if( jj == 0 )
               ERROR_message("Can't find label '%s' in covariates file" ,
                             labl_BBB[kk] ) ;
               nbad++ ; nkbad++ ; continue ;
           }
           if( covnel->vec_typ[jj+1] == NI_STRING ){  /* a dataset name field */
             char **qpt = (char **)covnel->vec[jj+1] ;   /* column of strings */
             qset[kk] = THD_open_dataset(qpt[ii]) ;      /* covariate dataset */
             if( qset[kk] == NULL ){
               ERROR_message("Can't open dataset '%s' from covariates file" ,
                             qpt[ii] ) ; nbad++ ; nkbad++ ;
             } else if( DSET_NVALS(qset[kk]) > 1 ){
               ERROR_message("Dataset '%s' from covariates file has %d sub-bricks",
                             qpt[ii] , DSET_NVALS(qset[kk]) ) ; nbad++ ; nkbad++ ;
             }
           } /* end of creating dataset #kk in column #jj */
           else {                                           /* a number field */
             float *fpt = (float *)covnel->vec[jj+1] ;    /* column of floats */
             if( covvec_BBB[jj] == NULL )             /* create output vector */
               MAKE_floatvec(covvec_BBB[jj],ndset_BBB) ;
             covvec_BBB[jj]->ar[kk] = fpt[ii] ;             /* save the value */
           }
         } /* end of kk loop over BBB datasets */
         if( covnel->vec_typ[jj+1] == NI_STRING ){     /* a dataset covariate */
           if( nkbad == 0 ){  /* all dataset opens good ==> convert to vectim */
             covvim_BBB[jj] = THD_dset_list_to_vectim( ndset_BBB, qset, mask ) ;
             if( covvim_BBB[jj] == NULL ){
               ERROR_message("Can't assemble dataset vectors for covariate #%d",jj+1) ;
               nbad++ ;
             }
           }
           for( kk=0 ; kk < ndset_BBB ; kk++ )         /* tossola la trashola */
             if( qset[kk] != NULL ) DSET_delete(qset[kk]) ;
         }
       } /* end of jj loop = covariates column index */
       free(qset) ;
     } /* end of BBB covariates datasets processing */

     /* repeat for the AAA datasets */

     if( ndset_AAA > 0 ){
       qset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*ndset_AAA) ;
       covvim_AAA = (MRI_vectim **)malloc(sizeof(MRI_vectim *)*mcov) ;
       covvec_AAA = (floatvec   **)malloc(sizeof(floatvec   *)*mcov) ;
       for( jj=0 ; jj < mcov ; jj++ ){                /* loop over covariates */
         covvim_AAA[jj] = NULL ;         /* initialize output vectors to NULL */
         covvec_AAA[jj] = NULL ;
         for( nkbad=kk=0 ; kk < ndset_AAA ; kk++ ){     /* loop over datasets */
           ii = string_search( labl_AAA[kk] ,     /* ii = covariate row index */
                               covnel->vec_len ,
                               (char **)covnel->vec[0] ) ;
           if( ii < 0 ){                     /* can't find it ==> this is bad */
             if( jj == 0 )
               ERROR_message("Can't find label '%s' in covariates file" ,
                             labl_AAA[kk] ) ;
               nbad++ ; nkbad++ ; continue ;
           }
           if( covnel->vec_typ[jj+1] == NI_STRING ){  /* a dataset name field */
             char **qpt = (char **)covnel->vec[jj+1] ;   /* column of strings */
             qset[kk] = THD_open_dataset(qpt[ii]) ;      /* covariate dataset */
             if( qset[kk] == NULL ){
               ERROR_message("Can't open dataset '%s' from covariates file" ,
                             qpt[ii] ) ; nbad++ ; nkbad++ ;
             } else if( DSET_NVALS(qset[kk]) > 1 ){
               ERROR_message("Dataset '%s' from covariates file has %d sub-bricks",
                             qpt[ii] , DSET_NVALS(qset[kk]) ) ; nbad++ ; nkbad++ ;
             }
           } /* end of creating dataset #kk in column #jj */
           else {                                           /* a number field */
             float *fpt = (float *)covnel->vec[jj+1] ;    /* column of floats */
             if( covvec_AAA[jj] == NULL )             /* create output vector */
               MAKE_floatvec(covvec_AAA[jj],ndset_AAA) ;
             covvec_AAA[jj]->ar[kk] = fpt[ii] ;             /* save the value */
           }
         } /* end of kk loop over AAA datasets */
         if( covnel->vec_typ[jj+1] == NI_STRING ){      /* a dataset covariate */
           if( nkbad == 0 ){   /* all dataset opens good ==> convert to vectim */
             covvim_AAA[jj] = THD_dset_list_to_vectim( ndset_AAA, qset, mask ) ;
             if( covvim_AAA[jj] == NULL ){
               ERROR_message("Can't assemble dataset vectors for covariate #%d",jj+1) ;
               nbad++ ;
             }
           }
           for( kk=0 ; kk < ndset_AAA ; kk++ )       /* toss out the trashola */
             if( qset[kk] != NULL ) DSET_delete(qset[kk]) ;
         }
       } /* end of jj loop = covariates column index */
       free(qset) ;
     } /* end of AAA covariates datasets processing */

     /*- Alas Babylon! -*/

     if( nbad > 0 ) ERROR_exit("Cannot continue past above ERROR%s :-(",
                                (nbad==1) ? "\0" : "s" ) ;

     /*-- end of loading covariate vectors --*/

     /*-- next, create the (empty) covariate regression matrices --*/

     /*-- setA matrix --*/

     Axxim = mri_new( nval_AAA , mcov+1 , MRI_float ) ;
     Axx   = MRI_FLOAT_PTR(Axxim) ;

     /*-- setB matrix --*/

     if( twosam && ttest_opcode != 2 ){  /* un-paired 2-sample case */
       Bxxim = mri_new( nval_BBB , mcov+1 , MRI_float ) ;
       Bxx   = MRI_FLOAT_PTR(Bxxim) ;
     } else if( twosam && ttest_opcode == 2 ){  /* paired case */
       Bxxim = Axxim ; Bxx = Axx ;   /* identical matrix to setA */
     }

     /*-- fill them in and (pseudo)invert them --*/

     TT_matrix_setup(0) ;  /* 0 = voxel index (just sayin') */

     if( num_covset_col > 0 ) MEMORY_CHECK ;

     if( twosam && num_covset_col < mcov && !singletonA ){     /* 19 Oct 2010 */
       int toz_sav = toz ; float pp ; /* test covariates for equality-ishness */

       toz = 1 ;
       INFO_message(
         "Two samples: t-testing fixed covariates for similarity between groups") ;
       for( jj=0 ; jj < mcov ; jj++ ){
         if( covnel->vec_typ[jj+1] == NI_STRING ){
           ININFO_message(" %s: values come from datasets ==> skipping test" ,
                          covlab->str[jj+1] ) ;
         } else {
           tpair = ttest_toz( ndset_AAA , covvec_AAA[jj]->ar ,
                              ndset_BBB , covvec_BBB[jj]->ar , 0 , NULL,NULL ) ;
           pp = normal_t2p( fabs((double)tpair.b) ) ;
           ININFO_message(" %s: mean of setA-setB=%s ; 2-sided p-value=%.4f" ,
                          covlab->str[jj+1] , MV_format_fval(tpair.a) , pp ) ;
         }
       }
       toz = toz_sav ;
     }

   }  /*-- end of covariates setup --*/

   /*-------------------- create empty output dataset ---------------------*/

   if( singletonA )
     nvres = nvout = 2 ;
   else {
     int mct = (do_cov) ? mcov : 0 ;
     nvres = ((twosam && do_1sam) ? 6 : 2) * (mcov+1) ; /* # of output volumes (calculated) */
     nvout = ((twosam && do_1sam) ? 6 : 2) * (mct+1)  ; /* # of output volumes (saved) */
   }

   if( !do_means || !do_tests ) nvout /= 2 ; /* no mean or stat sub-bricks? [05 Feb 2014] */

   outset = EDIT_empty_copy( use_singleton_fixed_val ? dset_BBB[0] : dset_AAA[0] ) ;

   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , nvout * brickwise_num ,
                      ADN_ntt       , 0      ,
                      ADN_brick_fac , NULL   ,
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_deconflict_prefix(outset) )
     ERROR_exit("Output dataset '%s' already exists!",prefix) ;

   tross_Make_History( "3dttest++" , argc,argv , outset ) ;

   /* temp dataset for 1 set of outputs [28 Jan 2014] */

   bbset = EDIT_empty_copy(outset) ;
   EDIT_dset_items( bbset, ADN_nvals,nvout, ADN_none ) ;

   if( do_sdat ){  /* 29 Aug 2016 */
     char *cpt ;
     sdat_prefix = (char *)malloc(sizeof(char)*(strlen(prefix)+32)) ;
     strcpy(sdat_prefix,prefix) ;
     cpt = strstr(sdat_prefix,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
     cpt = strstr(sdat_prefix,".sdat");
     if( cpt == NULL ) strcat(sdat_prefix,".sdat") ;
     fp_sdat = fopen( sdat_prefix , "w" ) ;
     if( fp_sdat == NULL )
       ERROR_exit("unable to open file %s for output",sdat_prefix) ;
     sdatar = (short *)malloc(sizeof(short)*nvox) ;
     INFO_message("opened file %s for output",sdat_prefix) ;
   }

   /*** make up some brick labels [[[man, this is tediously boring work]]] ***/

   if( mcov > 0 ){
     nws       = sizeof(float)*(4*mcov+2*nval_AAA+2*nval_BBB+32) ;
     workspace = (float *)malloc(nws) ;

     if( twosam ){
       testAB = testA = testB = (unsigned int)(-1) ;
       if( !do_1sam ) testA = testB = 0 ;            /* 10 Nov 2010 */
     } else {
       testAB = testB = 0 ; testA = (unsigned int)(-1) ;
     }
   }

   /* degrees of freedom for the t-statistics */

   if( singletonA ){
     dof_A = dof_B = dof_AB = nval_BBB - (mcov+1) ;
   } else {
     dof_A = nval_AAA - (mcov+1) ;
     if( twosam ){
      dof_B  = nval_BBB - (mcov+1) ;
      dof_AB = (ttest_opcode==2) ? dof_A : dof_A+dof_B ;
     }
   }

   /* dofsub hack [19 Jan 2016] */

   dof_A = (dof_A > dofsub) ? dof_A-dofsub : 2 ;
   if( twosam ){
     dof_B  = (dof_B  > dofsub) ? dof_B -dofsub : 2 ;
     dof_AB = (dof_AB > dofsub) ? dof_AB-dofsub : 2 ;
   }

/*-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:*/
/*--------- macros for adding sub-brick labels and statistics codes --------*/

   /* format for sub-brick index (good up to 99,999 sub-bricks) */

        if( brickwise_num <=     10 ) abbfmt = "#%d"   ;
   else if( brickwise_num <=    100 ) abbfmt = "#%02d" ;
   else if( brickwise_num <=   1000 ) abbfmt = "#%03d" ;
   else if( brickwise_num <=  10000 ) abbfmt = "#%04d" ;
   else if( brickwise_num <= 100000 ) abbfmt = "#%05d" ;
   else                               abbfmt = "#%06d" ;

  /* add sub-brick index if doing multiple tests (using abbfmt from above) */

#undef  ADD_BRICK_INDEX
#define ADD_BRICK_INDEX if(brickwise_num>1)sprintf(blab+strlen(blab),abbfmt,bb)

  /* mean (effect size) label for 2 sample results */

#undef  MEAN_LABEL_2SAM
#define MEAN_LABEL_2SAM(npp,nmm,lll)                         \
 do{ sprintf(blab,"%s-%s_%s",npp,nmm,lll); ADD_BRICK_INDEX;  \
     EDIT_BRICK_LABEL(outset,ss+bbase,blab);                 \
     ss++; } while(0)

  /* mean label for 1 sample results */

#define MEAN_LABEL_1SAM(nnn,lll)                     \
 do{ sprintf(blab,"%s_%s",nnn,lll); ADD_BRICK_INDEX; \
     EDIT_BRICK_LABEL(outset,ss+bbase,blab);         \
     ss++; } while(0)

  /* test statistic for 2 sample results, covariates label */

#undef  TEST_LABEL_2SAM_COV
#define TEST_LABEL_2SAM_COV(npp,nmm,lll)                      \
 do{ sprintf(blab,"%s-%s_%s_%s",npp,nmm,lll,stnam) ;          \
     ADD_BRICK_INDEX; EDIT_BRICK_LABEL(outset,ss+bbase,blab); \
     if( toz ) EDIT_BRICK_TO_FIZT(outset,ss+bbase) ;          \
     else      EDIT_BRICK_TO_FITT(outset,ss+bbase,dof_AB) ;   \
     ss++; } while(0)

  /* test statistic for 2 sample results, mean label */

#undef  TEST_LABEL_2SAM_MEAN
#define TEST_LABEL_2SAM_MEAN(npp,nmm)                         \
 do{ sprintf(blab,"%s-%s_%s",npp,nmm,stnam) ;                 \
     ADD_BRICK_INDEX; EDIT_BRICK_LABEL(outset,ss+bbase,blab); \
     if( toz ) EDIT_BRICK_TO_FIZT(outset,ss+bbase) ;          \
     else      EDIT_BRICK_TO_FITT(outset,ss+bbase,dof_AB) ;   \
     ss++; } while(0)

  /* test statistic for 1 sample result, covariates label */

#undef  TEST_LABEL_1SAM_COV
#define TEST_LABEL_1SAM_COV(nnn,lll,ddd)                      \
 do{ sprintf(blab,"%s_%s_%s",nnn,lll,stnam) ;                 \
     ADD_BRICK_INDEX; EDIT_BRICK_LABEL(outset,ss+bbase,blab); \
     if( toz ) EDIT_BRICK_TO_FIZT(outset,ss+bbase) ;          \
     else      EDIT_BRICK_TO_FITT(outset,ss+bbase,ddd) ;      \
     ss++ ; } while(0)

  /* test statistic for 1 sample result, mean label */

#undef  TEST_LABEL_1SAM_MEAN
#define TEST_LABEL_1SAM_MEAN(nnn,ddd)                         \
 do{ sprintf(blab,"%s_%s",nnn,stnam) ;                        \
     ADD_BRICK_INDEX; EDIT_BRICK_LABEL(outset,ss+bbase,blab); \
     if( toz ) EDIT_BRICK_TO_FIZT(outset,ss+bbase) ;          \
     else      EDIT_BRICK_TO_FITT(outset,ss+bbase,ddd) ;      \
     ss++ ; } while(0)

/*-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:*/

   stnam = (toz) ? "Zscr" : "Tstat" ;    /* name of statistic */

   if( singletonA ){  /* special case [20 Mar 2015] */
     bbase = ss = bb = 0 ;
     if( do_means ) MEAN_LABEL_2SAM(snam_PPP,snam_MMM,"diff") ;
     if( do_tests ) TEST_LABEL_2SAM_MEAN(snam_PPP,snam_MMM) ;
     goto LABELS_ARE_DONE ;
   }

   for( bb=0 ; bb < brickwise_num ; bb++ ){ /** loop over tests to perform **/
     bbase = bb*nvout ; ss = 0 ;
     if( mcov <= 0 || !do_cov ){         /*--- no covariates ---*/
       if( !twosam ){   /* 1 sample only = the simplest case */
         if( do_means ) MEAN_LABEL_1SAM (snam_AAA,"mean") ;
         if( do_tests ) TEST_LABEL_1SAM_MEAN(snam_AAA,dof_A ) ;
       } else {         /* 2 samples */
         ntwosam = (do_means+do_tests) ; /* how many 2 sample outputs per test */
         if( do_means ) MEAN_LABEL_2SAM (snam_PPP,snam_MMM,"mean") ;
         if( do_tests ) TEST_LABEL_2SAM_MEAN(snam_PPP,snam_MMM) ;
         if( do_1sam ){
           if( do_means ) MEAN_LABEL_1SAM (snam_AAA,"mean") ;
           if( do_tests ) TEST_LABEL_1SAM_MEAN(snam_AAA,dof_A ) ;
           if( do_means ) MEAN_LABEL_1SAM (snam_BBB,"mean") ;
           if( do_tests ) TEST_LABEL_1SAM_MEAN(snam_BBB,dof_B ) ;
         }
       }
     } else {                            /*--- have covariates ---*/
       if( testAB ){                     /* 2-sample results */
         ntwosam = (do_means+do_tests)*(mcov+1) ;
         if( do_means ) MEAN_LABEL_2SAM (snam_PPP,snam_MMM,"mean") ;
         if( do_tests ) TEST_LABEL_2SAM_MEAN(snam_PPP,snam_MMM) ;
         for( jj=1 ; jj <= mcov ; jj++ ){
           if( do_means ) MEAN_LABEL_2SAM (snam_PPP,snam_MMM,covlab->str[jj]) ;
           if( do_tests ) TEST_LABEL_2SAM_COV(snam_PPP,snam_MMM,covlab->str[jj]) ;
         }
       }
       if( testA ){                      /* 1-sample results for setA */
         if( do_means ) MEAN_LABEL_1SAM (snam_AAA,"mean") ;
         if( do_tests ) TEST_LABEL_1SAM_MEAN(snam_AAA,dof_A ) ;
         for( jj=1 ; jj <= mcov ; jj++ ){
           if( do_means ) MEAN_LABEL_1SAM (snam_AAA,covlab->str[jj]) ;
           if( do_tests ) TEST_LABEL_1SAM_COV(snam_AAA,covlab->str[jj],dof_A) ;
         }
       }
       if( testB ){                      /* 1-sample results for setB */
         if( do_means ) MEAN_LABEL_1SAM (snam_BBB,"mean") ;
         if( do_tests ) TEST_LABEL_1SAM_MEAN(snam_BBB,dof_B ) ;
         for( jj=1 ; jj <= mcov ; jj++ ){
           if( do_means ) MEAN_LABEL_1SAM (snam_BBB,covlab->str[jj]) ;
           if( do_tests ) TEST_LABEL_1SAM_COV(snam_BBB,covlab->str[jj],dof_B) ;
         }
       }
     } /* end of have covariates */
   } /* end of brickwise loop */

LABELS_ARE_DONE:  /* target for goto above */

   /*----- create space to store results before dataset-izing them -----*/

   MAKE_VECTIM(vimout,nmask_hits,nvres) ; vimout->ignore = 0 ;

   /* make residual dataset [07 Dec 2015] */

   if( do_resid ){
     rrset = EDIT_empty_copy(outset) ;
     tross_Make_History( "3dttest++" , argc,argv , rrset ) ;
     EDIT_dset_items( rrset,
                        ADN_nvals  , nval_AAA+nval_BBB ,
                        ADN_prefix , prefix_resid ,
                      ADN_none ) ;
     for( jj=0 ; jj < nval_AAA ; jj++ ){
       sprintf(blab,"Ares%04d",jj) ; EDIT_BRICK_LABEL(rrset,jj,blab) ;
     }
     for( jj=0 ; jj < nval_BBB ; jj++ ){
       sprintf(blab,"Bres%04d",jj) ; EDIT_BRICK_LABEL(rrset,jj+nval_AAA,blab) ;
     }
     MAKE_VECTIM(rimout,nmask_hits,nval_AAA+nval_BBB) ; rimout->ignore = 0 ;
   }

   /**********==========---------- process data ----------==========**********/

   /*----- convert each input set of datasets to a vectim -----*/

   if( !brickwise ){    /* load data now if not doing brickwise tests */
     INFO_message("loading input datasets") ;
     if( !use_singleton_fixed_val ){
       vectim_AAA = THD_dset_list_to_vectim( ndset_AAA , dset_AAA , mask ) ;
       for( ii=0 ; ii < ndset_AAA ; ii++ ) DSET_unload(dset_AAA[ii]) ;
     }
     if( twosam ){
       vectim_BBB = THD_dset_list_to_vectim( ndset_BBB , dset_BBB , mask ) ;
       for( ii=0 ; ii < ndset_BBB ; ii++ ) DSET_unload(dset_BBB[ii]) ;
     }
     MEMORY_CHECK ;
   }

   /*--- loop and process ---*/

   vstep = (nmask_hits > 6666) ? nmask_hits/50 : 0 ;
   if( brickwise_num > 1 ){
     vstep = 0 ; bstep = brickwise_num/50 ; if( bstep == 0 ) bstep = 1 ;
     if( do_randomsign )
       fprintf(stderr,"++ t-test randomsign:") ;
     else
       fprintf(stderr,"++ t-test brickwise:") ;
   } else {
     bstep =0 ;
   }

   for( bb=0 ; bb < brickwise_num ; bb++ ){  /* for each 'brick' to process */
     bbase = bb*nvout ;

     if( do_randomsign ) setup_randomsign() ; /* moved here 02 Feb 2016 */

     if( bstep > 0 && bb%bstep==bstep/2 ) vstep_print() ;

     if( brickwise ){           /* need to load data for this sub-brick now */
       int keep[1] ; keep[0] = bb ;
       INFO_message("++++++++++ loading input data: volume [%d]",bb) ;
       if( vectim_AAA != NULL ){ VECTIM_destroy(vectim_AAA); vectim_AAA=NULL; }
       if( vectim_BBB != NULL ){ VECTIM_destroy(vectim_BBB); vectim_BBB=NULL; }
       vectim_AAA = THD_dset_list_censored_to_vectim( ndset_AAA , dset_AAA ,
                                                      mask , 1 , keep       ) ;
       for( ii=0 ; ii < ndset_AAA ; ii++ ) DSET_unload_one(dset_AAA[ii],bb) ;
       if( twosam ){
         vectim_BBB = THD_dset_list_censored_to_vectim( ndset_BBB , dset_BBB ,
                                                        mask , 1 , keep       ) ;
         for( ii=0 ; ii < ndset_BBB ; ii++ ) DSET_unload_one(dset_BBB[ii],bb) ;
       }
       if( debug ) MEMORY_CHECK ;
     }

     if( vstep > 0 ) fprintf(stderr,"++ t-testing:") ;
     nconst = nzred = nzskip = 0 ;

     /*------- the actual work is in this loop over voxels! -------*/

     for( kout=ivox=0 ; ivox < nvox ; ivox++ ){  /* for each voxel to process */

       if( mask != NULL && mask[ivox] == 0 ) continue ;  /* don't process me */

       if( bb == 0 )
         vimout->ivec[kout] = ivox ;  /* table of what voxels are in vimout */

       if( vstep > 0 && kout%vstep==vstep/2 ) vstep_print() ;

       if( use_singleton_fixed_val )
                    datAAA = &singleton_fixed_val ;           /* 08 Dec 2015 */
       else         datAAA = VECTIM_PTR(vectim_AAA,kout) ;    /* data arrays */

       if( twosam ) datBBB = VECTIM_PTR(vectim_BBB,kout) ;

       resar = VECTIM_PTR(vimout,kout) ;                    /* results array */
       memset( resar , 0 , sizeof(float)*nvres ) ;          /* (set to zero) */

       if( randomsign_AAA != NULL ){        /* randomize signs [31 Dec 2015] */
         for( ii=0 ; ii < nval_AAA ; ii++ )
           if( randomsign_AAA[ii] ) datAAA[ii] = -datAAA[ii] ;
       }
       if( randomsign_BBB != NULL ){
         for( ii=0 ; ii < nval_BBB ; ii++ )
           if( randomsign_BBB[ii] ) datBBB[ii] = -datBBB[ii] ;
       }

       if( do_resid ){
         if( bb == 0 ) rimout->ivec[kout] = ivox ;
         ABresid = VECTIM_PTR(rimout,kout) ;
         memset( ABresid , 0 , sizeof(float)*(nval_AAA+nval_BBB) ) ;
         Aresid = ABresid ;
         if( nval_BBB > 0 ) Bresid = ABresid + nval_AAA ;
       }

       if( debug > 1 ){
         INFO_message("voxel#%d data:",kout) ;
         fprintf(stderr,"  A =") ;
         for( ii=0 ; ii < nval_AAA ; ii++ ) fprintf(stderr," %g",datAAA[ii]) ;
         if( twosam ){
           fprintf(stderr,"\n  B =") ;
           for( ii=0 ; ii < nval_BBB ; ii++ ) fprintf(stderr," %g",datBBB[ii]) ;
           fprintf(stderr,"\n") ;
         }
       }

       /* skip processing for input voxels whose data is constant */

       if( nval_AAA > 1 ){
         for( ii=1 ; ii < nval_AAA && datAAA[ii] == datAAA[0] ; ii++ ) ; /*nada*/
         dconst = (ii == nval_AAA) ;
       } else {
         dconst = 0 ;  /* for singletonA */
       }
       if( twosam && !dconst ){
         for( ii=1 ; ii < nval_BBB && datBBB[ii] == datBBB[0] ; ii++ ) ; /*nada*/
         dconst = (ii == nval_BBB) ;
       }
       if( dconst ){
         if( debug > 1 ) INFO_message("skip output voxel#%d for constancy",kout) ;
         nconst++ ; kout++ ; continue ;
       }

       if( mcov == 0 ){  /*--- no covariates ==> standard t-tests ---*/
         float *zAAA=datAAA, *zBBB=datBBB ; int nAAA=nval_AAA, nBBB=nval_BBB, nz,qq ;

         if( do_zskip ){  /* 06 Oct 2010: skip zero values? */
           if( !singletonA ){
             for( ii=nz=0 ; ii < nval_AAA ; ii++ ) nz += (datAAA[ii] == 0.0f) ;
             if( nz > 0 ){            /* copy nonzero vals to a new array */
               nAAA = nval_AAA - nz ;
               if( nAAA < zskip_AAA ){ kout++ ; nzskip++ ; continue ; }
               zAAA = (float *)malloc(sizeof(float)*nAAA) ;
               for( ii=qq=0 ; ii < nval_AAA ; ii++ )
                 if( datAAA[ii] != 0.0f ) zAAA[qq++] = datAAA[ii] ;
             }
           }
           if( twosam ){
             for( ii=nz=0 ; ii < nval_BBB ; ii++ ) nz += (datBBB[ii] == 0.0f) ;
             if( nz > 0 ){            /* copy nonzero vals to a new array */
               nBBB = nval_BBB - nz ;
               if( nBBB < zskip_BBB ){
                 if( zAAA != datAAA && zAAA != NULL ) free(zAAA) ;
                 kout++ ; nzskip++ ; continue ;
               }
               zBBB = (float *)malloc(sizeof(float)*nBBB) ;
               for( ii=qq=0 ; ii < nval_BBB ; ii++ )
                 if( datBBB[ii] != 0.0f ) zBBB[qq++] = datBBB[ii] ;
             }
           }
           if( (zAAA != datAAA && zAAA != NULL) || (zBBB != datBBB && zBBB != NULL) )
             nzred++ ;
         }

         if( twosam ){
           if( do_1sam ){
             tpair = ttest_toz( nAAA,zAAA , 0 ,NULL   , 0 , NULL,NULL ) ; /* 1 sample setA */
             resar[2] = tpair.a ; resar[3] = tpair.b ;
             tpair = ttest_toz( nBBB,zBBB , 0 ,NULL   , 0 , NULL,NULL ) ; /* 1 sample setB */
             resar[4] = tpair.a ; resar[5] = tpair.b ;
           }
#ifdef ALLOW_RANK
           if( do_ranks ) rank_order_2floats( nAAA,zAAA , nBBB,zBBB ) ;
#endif
           if( singletonA ){
             tpair = ttest_toz_singletonA( zAAA[0] , nBBB,zBBB , Aresid,Bresid ) ;
           } else {
             tpair = ttest_toz( nAAA,zAAA , nBBB,zBBB , ttest_opcode , Aresid,Bresid) ; /* 2 sample A-B */
           }
           resar[0] = tpair.a ; resar[1] = tpair.b ;
           if( BminusA ){ resar[0] = -resar[0] ; resar[1] = -resar[1] ; }
           if( debug > 1 ) fprintf(stderr,"   resar[0]=%g  [1]=%g\n",resar[0],resar[1]) ;
         } else {
           tpair = ttest_toz( nAAA,zAAA , 0 ,NULL   , ttest_opcode , Aresid,NULL ) ; /* 1 sample setA */
           resar[0] = tpair.a ; resar[1] = tpair.b ;
           if( debug > 1 ) fprintf(stderr,"   resar[0]=%g  [1]=%g\n",resar[0],resar[1]) ;
         }

         if( zBBB != datBBB && zBBB != NULL ) free(zBBB) ;
         if( zAAA != datAAA && zAAA != NULL ) free(zAAA) ;

       } else {          /*--- covariates ==> regression analysis ---*/

         /*-- if covariate datasets are being used,
              must fill in the Axx and Bxx matrices now --*/

         if( num_covset_col > 0 ) TT_matrix_setup(kout) ;

         /*-- and do the work --*/

         if( nws > 0 ) memset(workspace,0,nws) ;

#ifdef ALLOW_RANK
         if( do_ranks ) rank_order_2floats( nval_AAA, datAAA, nval_BBB, datBBB ) ;
#endif
         if( singletonA ){
           regress_toz_singletonA( datAAA[0] ,
                                   nval_BBB , datBBB ,
                                   mcov ,
                                   Axx ,
                                   Bxx , Bxx_psinv , Bxx_xtxinv , resar , workspace ) ;
         } else {
           regress_toz( nval_AAA , datAAA , nval_BBB , datBBB , ttest_opcode ,
                        mcov ,
                        Axx , Axx_psinv , Axx_xtxinv ,
                        Bxx , Bxx_psinv , Bxx_xtxinv , resar , workspace ) ;
         }
       }

       if( BminusA && ntwosam ){  /* negate 2 sample results? [05 Nov 2010] */
         for( ii=0 ; ii < ntwosam ; ii++ ) resar[ii] = -resar[ii] ;
       }

       kout++ ;

     }  /*------- end of loop over voxels -------*/

     /*--- print messages for this set of t-tests ---*/

     if( vstep > 0 ) fprintf(stderr,"!\n") ;

     if( brickwise_num == 1 ){
       if( nconst > 0 )
         ININFO_message("skipped %d voxel%s completely for having constant data" ,
                        nconst , (nconst==1) ? "\0" : "s" ) ;

       if( nzred > 0 )
         ININFO_message("-zskip: %d voxel%s had some values skipped in their t-tests",
                        nzred , (nzred==1) ? "\0" : "s" ) ;

       if( nzskip > 0 )
         ININFO_message("-zskip: skipped %d voxel%s completely for having too few nonzero values" ,
                        nzskip , (nzskip==1) ? "\0" : "s" ) ;
     }

     /*--- load results from vimout into output dataset ---*/

     if( debug ) ININFO_message("saving results into output volumes") ;

     if( !do_sdat ){
       for( kk=0 ; kk < nvout ; kk++ )        /* load dataset with 0s */
         EDIT_substitute_brick( bbset , kk , MRI_float , NULL ) ;

       if( do_means+do_tests == 2 && do_cov ){ /* simple copy to temp dataset */
         THD_vectim_to_dset( vimout , bbset ) ;
       } else {                                /* complicated copy */
         int *list = (int *)malloc(sizeof(int)*nvout) ;
         ss = (do_means) ? 0 : 1 ;
         for( kk=0 ; kk < nvout ; kk++ ) list[kk] = ss + 2*kk ;
         THD_vectim_indexed_to_dset( vimout , nvout,list , bbset ) ;
         free(list) ;
       }

       for( kk=0 ; kk < nvout ; kk++ ){       /* move results into final output dataset */
         EDIT_substitute_brick( outset , kk+bbase , MRI_float , DSET_ARRAY(bbset,kk) ) ;
         DSET_NULL_ARRAY(bbset,kk) ;
       }
     } else {  /* 29 Aug 2016 */
#define SFAC 0.0002f
       float rval ;
       ss = (do_means) ? 0 : 1 ;
       for( kout=0 ; kout < nmask_hits ; kout++ ){
         resar = VECTIM_PTR(vimout,kout) ;
         rval  = resar[ss] / SFAC ;
         sdatar[kout] = SHORTIZE(rval) ;
       }
       fwrite(sdatar,sizeof(short),nmask_hits,fp_sdat) ;
     }

     /* and load residual dataset [07 Dec 2015] */

     if( do_resid ){
       for( kk=0 ; kk < nval_AAA+nval_BBB ; kk++ )        /* load dataset with 0s */
         EDIT_substitute_brick( rrset , kk , MRI_float , NULL ) ;
       THD_vectim_to_dset( rimout , rrset ) ;
     }

   } /*----- end of brickwise loop -----*/

   if( brickwise_num > 1 ) fprintf(stderr,"!\n") ;

   if( do_sdat ){      /*----- 29 Aug 2016 -----*/
     fclose(fp_sdat) ;
     INFO_message("output short-ized file %s",sdat_prefix) ;
     exit(0) ;
   }

   /*-------- get rid of the input data and workspaces now --------*/

   INFO_message("---------- End of analyses -- freeing workspaces ----------") ;

   if( !use_singleton_fixed_val ){
     for( ii=0 ; ii < ndset_AAA ; ii++ ) DSET_unload(dset_AAA[ii]) ;
   }
   for( ii=0 ; ii < ndset_BBB ; ii++ ) DSET_unload(dset_BBB[ii]) ;

   if( workspace  != NULL ) free(workspace) ;
   if( vectim_AAA != NULL ) VECTIM_destroy(vectim_AAA) ;
   if( vectim_BBB != NULL ) VECTIM_destroy(vectim_BBB) ;
   if( vimout     != NULL ) VECTIM_destroy(vimout) ;
   if( bbset      != NULL ) DSET_delete(bbset) ;
   if( rimout     != NULL ) VECTIM_destroy(rimout) ;

   if( covvim_AAA != NULL ){
     for( jj=0 ; jj < mcov ; jj++ )
       if( covvim_AAA[jj] != NULL ) VECTIM_destroy(covvim_AAA[jj]) ;
     free(covvim_AAA) ;
   }
   if( covvec_AAA != NULL ){
     for( jj=0 ; jj < mcov ; jj++ )
       if( covvec_AAA[jj] != NULL ) KILL_floatvec(covvec_AAA[jj]) ;
     free(covvec_AAA) ;
   }

   if( covvim_BBB != NULL ){
     for( jj=0 ; jj < mcov ; jj++ )
       if( covvim_BBB[jj] != NULL ) VECTIM_destroy(covvim_BBB[jj]) ;
     free(covvim_BBB) ;
   }
   if( covvec_BBB != NULL ){
     for( jj=0 ; jj < mcov ; jj++ )
       if( covvec_BBB[jj] != NULL ) KILL_floatvec(covvec_BBB[jj]) ;
     free(covvec_BBB) ;
   }

   MEMORY_CHECK ;

   /*---------- finalizationing ----------*/

   if( do_tests && !AFNI_noenv("AFNI_AUTOMATIC_FDR") ){
     INFO_message("Creating FDR curves in output dataset") ;
     mri_fdr_setmask(mask) ;
     kk = THD_create_all_fdrcurves(outset) ;
     if( kk > 0 ){
       ININFO_message("Added %d FDR curve%s to dataset",kk,(kk==1)?"\0":"s");
     } else {
       WARNING_message("Failed to add FDR curves to dataset?!") ;
     }
   }

   DSET_write(outset) ; WROTE_DSET(outset) ; DSET_unload(outset) ;

   if( rrset != NULL ){
     DSET_write(rrset) ; WROTE_DSET(rrset) ; DSET_unload(rrset) ;
   }

   if( singletonA )
     ININFO_message("results are %s - %s", snam_PPP,snam_MMM) ;
   else if( twosam )
     ININFO_message("%s test: results are %s - %s",
                    ttest_opcode == 2 ? "paired":"2-sample", snam_PPP,snam_MMM) ;

   /*--------------------------------------------------------------*/
   /*------------ Cluster Simulation now [10 Feb 2016] ------------*/

   if( do_clustsim || do_Xclustsim ){
     char fname[128] , *cmd , *ccc ; int qq,pp , nper ; double ct1,ct2 ;
     int ncsim ;
     int use_sdat ;
     char **tfname ;
     int ncmin = (do_Xclustsim) ? 36000 : 10000 ;

     use_sdat = do_Xclustsim ||
                ( name_mask != NULL && !AFNI_yesenv("AFNI_TTEST_NIICSIM") ) ;

     ncsim = (int)AFNI_numenv("AFNI_TTEST_NUMCSIM") ;  /* 0 if not set */
          if( ncsim <    10000 ) ncsim =  ncmin ;
     else if( ncsim > 10000000 ) ncsim = 10000000 ;

     cmd  = (char *)malloc(sizeof(char)*(8192+mcov*128)) ;
     nper = ncsim / num_clustsim ; if( nper*num_clustsim < ncsim ) nper++ ;

     tfname = (char **)malloc(sizeof(char *)*num_clustsim) ;

     /* loop to start randomize jobs */

     INFO_message("================ Starting %s calculations ================",clustsim_opt) ;
     ININFO_message("=== temporary files will have prefix %s ===",prefix_clustsim) ;
     ININFO_message("=== running %d -randomsign job%s (%d iterations per job) ===",
                    num_clustsim , (num_clustsim > 1)?"s":"\0" , nper ) ;
     ct1 = COX_clock_time() ;

     for( pp=0 ; pp < num_clustsim ; pp++ ){

       /* format the command to run 3dttest++ with the residuals as input */

       if( !use_sdat )
         sprintf( cmd , "3dttest++ -DAFNI_AUTOMATIC_FDR=NO -DAFNI_DONT_LOGFILE=YES"
                        " -randomsign %d -nomeans -toz" , nper ) ;
       else
         sprintf( cmd , "3dttest++ -DAFNI_AUTOMATIC_FDR=NO -DAFNI_DONT_LOGFILE=YES"
                        " -RANDOMSIGN %d -nomeans -toz" , nper ) ;

       if( twosam )
         sprintf( cmd+strlen(cmd) , " -no1sam") ;
       if( name_mask != NULL )
         sprintf( cmd+strlen(cmd) , " -mask %s",name_mask) ;
       if( ttest_opcode == 1 )
         sprintf( cmd+strlen(cmd) , " -unpooled") ;
       if( ttest_opcode == 2 )
         sprintf( cmd+strlen(cmd) , " -paired") ;

       if( mcov == 0 ){   /* no covariates == easy peasy */

         if( nval_BBB == 0 ){  /* only -setA */
           sprintf( cmd+strlen(cmd) , " -setA %s" , prefix_resid ) ;
         } else {
           sprintf( cmd+strlen(cmd) , " -setA %s'[0..%d]' -setB %s'[%d..$]'" ,
                                      prefix_resid , nval_AAA-1 ,
                                      prefix_resid , nval_AAA    ) ;
         }

       } else {  /* covariates are harder to format (must allow for labels) */

         sprintf( cmd+strlen(cmd) , " -nocov -covariates %s" , fname_cov ) ;
         switch( center_code ){
           default:
           case CENTER_DIFF: sprintf( cmd+strlen(cmd) , " -center DIFF") ; break ;
           case CENTER_SAME: sprintf( cmd+strlen(cmd) , " -center SAME") ; break ;
           case CENTER_NONE: sprintf( cmd+strlen(cmd) , " -center NONE") ; break ;
         }
         if( center_meth == CMETH_MEDIAN )
           sprintf( cmd+strlen(cmd) , " -cmeth MEDIAN") ;

         sprintf( cmd+strlen(cmd) , " -setA rAAA" ) ;
         for( jj=0 ; jj < nval_AAA ; jj++ ){
           sprintf( cmd+strlen(cmd) , " %s %s'[%d]'" , labl_AAA[jj] , prefix_resid , jj ) ;
         }
         if( nval_BBB > 0 ){
           sprintf( cmd+strlen(cmd) , " -setB rBBB" ) ;
           for( jj=0 ; jj < nval_BBB ; jj++ ){
             sprintf( cmd+strlen(cmd) , " %s %s'[%d]'" , labl_BBB[jj] , prefix_resid , jj+nval_AAA ) ;
           }
         }
       }

       tfname[pp] = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
       if( !use_sdat ){
         sprintf(tfname[pp],"%s/%s.%03d.nii",tempdir,prefix_clustsim,pp) ;
         sprintf( cmd+strlen(cmd) , " -prefix %s" , tfname[pp] ) ;
       } else {
         sprintf(tfname[pp],"%s/%s.%03d.sdat",tempdir,prefix_clustsim,pp) ;
         sprintf( cmd+strlen(cmd) , " -prefix %s" , tfname[pp] ) ;
       }
       /* the command for 3dttest++ is finished here */

       /* let only job #0 print progress to the screen */
       if( pp > 0 ) strcat(cmd," &> /dev/null") ;

       if( pp == 0 ) ININFO_message("#0 jobs command:\n   %s",cmd) ;

       start_job( cmd ) ;
       if( pp > 0 ) NI_sleep(666) ;  /* give each job a little bit to start up */
     }

     /*-- wait until all jobs stop --*/

     wait_for_jobs() ;

     ct2 = COX_clock_time() ;
     ININFO_message("===== jobs have finished (%.1f s elapsed) =====",ct2-ct1) ;
     ct1 = ct2 ;

     /* run 3dClustSim[X] using the outputs from the above as the simulations */

     if( do_clustsim ){    /*----- 3dClustsim -----*/

       sprintf(fname,"%s.CSim.cmd",prefix_clustsim) ;
       if( !use_sdat ){
         sprintf( cmd , "3dClustSim -DAFNI_DONT_LOGFILE=YES"
                        " -prefix %s.CSim -LOTS -both -nodec -cmd %s -inset" ,
                        prefix_clustsim , fname ) ;
         for( pp=0 ; pp < num_clustsim ; pp++ )
           sprintf( cmd+strlen(cmd) , " %s" , tfname[pp]) ;
         if( name_mask != NULL )
           sprintf( cmd+strlen(cmd) , " -mask %s",name_mask) ;
       } else {
         sprintf( cmd , "3dClustSim -DAFNI_DONT_LOGFILE=YES"
                        " -prefix %s.CSim -LOTS -both -nodec -cmd %s -insdat %s" ,
                        prefix_clustsim , fname , name_mask ) ;
         for( pp=0 ; pp < num_clustsim ; pp++ )
           sprintf( cmd+strlen(cmd) , " %s" , tfname[pp]) ;
       }

       ININFO_message("===== starting 3dClustSim =====\n   %s",cmd) ;
       system(cmd) ;

       /* load the 3drefit command from 3dClustSim */

       ccc = AFNI_suck_file(fname) ;
       if( ccc == NULL )
         ERROR_exit("===== 3dClustSim command failed :-((( =====") ;

       /* crop whitespace off the end */

       for( qq=strlen(ccc)-1 ; qq > 0 && isspace(ccc[qq]) ; qq-- ) ccc[qq] = '\0' ;
       if( strlen(ccc) > 8190 ) cmd = (char *)realloc(cmd,strlen(ccc)+2048) ;

       /* and run 3drefit */

       ININFO_message("===== 3drefit-ing 3dClustSim results into %s =====",DSET_HEADNAME(outset)) ;
       sprintf(cmd,"%s -DAFNI_DONT_LOGFILE=NO %s",ccc,DSET_HEADNAME(outset)) ;
       system(cmd) ;

     } else {  /*----- 3dClustSimX [30 Aug 2016] -----*/

       sprintf( cmd , "3dClustSimX -DAFNI_DONT_LOGFILE=YES "
                      " -prefix %s.CSimX.nii -NN%d -%dsid" ,
                      prefix_clustsim , nnlev_Xclu , sid_Xclu ) ;
       if( npthr_Xclu > 0 ){
         sprintf( cmd+strlen(cmd) , " -pthr") ;
         for( pp=0 ; pp < npthr_Xclu ; pp++ )
           sprintf( cmd+strlen(cmd) , " %.5f",pthr_Xclu[pp]) ;
       }
       sprintf( cmd+strlen(cmd) , " -insdat %s",name_mask) ;
       for( pp=0 ; pp < num_clustsim ; pp++ )
         sprintf( cmd+strlen(cmd) , " %s" , tfname[pp]) ;

       ININFO_message("===== starting 3dClustSimX =====\n   %s",cmd) ;
       system(cmd) ;
     }

     /* remove intermediate files */

     if( do_Xclustsim == 1 ){
       ININFO_message("===== deleting temp files =====") ;
       sprintf(cmd,"\\rm %s",prefix_resid) ;
       for( pp=0 ; pp < num_clustsim ; pp++ )
         sprintf(cmd+strlen(cmd)," %s",tfname[pp]) ;
       system(cmd) ;

     } else if( do_clustsim == 1 ){ /** currently, this case will never be executed **/
       ININFO_message("===== deleting temp files =====") ;
       sprintf(cmd,"\\rm %s.*",prefix_clustsim) ;
       if( strcmp(tempdir,"./") != 0 ){
         for( pp=0 ; pp < num_clustsim ; pp++ )
           sprintf(cmd+strlen(cmd)," %s",tfname[pp]) ;
       }
       system(cmd) ;
     } else if( do_clustsim == 2 ){                     /** the default case **/
       ININFO_message("===== deleting temp files =====") ;
       sprintf(cmd,"\\rm %s.*.niml %s",prefix_clustsim,prefix_resid) ;
       for( pp=0 ; pp < num_clustsim ; pp++ )
         sprintf(cmd+strlen(cmd)," %s",tfname[pp]) ;
       system(cmd) ;
     } else {
       ININFO_message("===== NOT deleting any temp files =====") ;
     }

     /* et viola */

#if 0
     free(ccc) ; free(cmd) ;
     for( pp=0 ; pp < num_clustsim ; pp++ ) free(tfname[pp]) ;
     free(tfname) ;
#endif
     ININFO_message("=============== %s work is finished :-) ===============",clustsim_opt) ;
   }

   /*--- e finito ------------------------------------------------------------*/

   exit(0) ;

} /*---------- end of main program -------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*----- macros for regression matrix elements -----*/

#undef  PA
#undef  PB
#undef  XA
#undef  XB
#define PA(i,j) psinvA[(i)+(j)*mm]  /* i=0..mm-1 , j=0..numA-1 */
#define PB(i,j) psinvB[(i)+(j)*mm]
#define XA(i,j) xA[(i)+(j)*(nA)]    /* i=0..nA-1 , j=0..mm-1 */
#define XB(i,j) xB[(i)+(j)*(nB)]

#undef  xtxA
#undef  xtxB
#define xtxA(i) xtxinvA[(i)+(i)*mm] /* diagonal elements */
#define xtxB(i) xtxinvB[(i)+(i)*mm]

/*----- Macros for the t-test functions -----*/

#undef  VBIG
#define VBIG 1.0e+24f

#undef  TMAX
#define TMAX 99.0f

#undef  TCLIP
#define TCLIP(t) ( ((t) < -TMAX) ? -TMAX : ((t) > TMAX) ? TMAX : (t) )

/*---------------------------------------------------------------------------*/
/*  opcode defines what to do for 2-sample tests:
      0 ==> unpaired, pooled variance
      1 ==> unpaired, unpooled variance (not implemented here)
      2 ==> paired (numA==numB required)

    xA      = numA X (mcov+1) matrix -- in column-major order
    psinvA  = (mcov+1) X numA matrix -- in column-major order
    xtxinvA = (mcov+1) X (mcov+1) matrix = inv[xA'xA]
*//*-------------------------------------------------------------------------*/

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             )
{
   int kt=0,nws , mm=mcov+1 , nA=numA , nB=numB ;
   float *betA=NULL , *betB=NULL , *zdifA=NULL , *zdifB=NULL ;
   float ssqA=0.0f , ssqB=0.0f , varA=0.0f , varB=0.0f ; double dof=0.0 ;
   register float val , den ; register int ii,jj,tt ;

ENTRY("regress_toz") ;

   if( numB == 0 ) opcode = 0 ;  /* 03 Mar 2011 */

   nws = 0 ;
   if( testA || testAB ){
     betA  = workspace + nws ; nws += mm ;
     zdifA = workspace + nws ; nws += nA ;
   }
   if( testB || testAB ){
     betB  = workspace + nws ; nws += mm ;
     zdifB = workspace + nws ; nws += nB ;
   }

   /*-- compute estimates for A parameters --*/

   if( testA || testAB ){
#if 0
     MRI_IMAGE *axxim_psinv=NULL , *axxim_xtxinv=NULL ;
     if( psinvA == NULL || xtxinvA == NULL ){  /* matrix wasn't pre-inverted */
       MRI_IMARR *impr ; MRI_IMAGE *axxim ;
       axxim = mri_new_vol_empty( nA , mcov+1 , 1 , MRI_float ) ;
       mri_fix_data_pointer(xA,axxim) ;
       impr = mri_matrix_psinv_pair(axxim,0.0f) ;
       mri_clear_data_pointer(axxim) ; mri_free(axxim) ;
       if( impr == NULL ){ ERROR_message("psinv setA matrix fails"); EXRETURN; }
       axxim_psinv  = IMARR_SUBIM(impr,0); psinvA  = MRI_FLOAT_PTR(axxim_psinv );
       axxim_xtxinv = IMARR_SUBIM(impr,1); xtxinvA = MRI_FLOAT_PTR(axxim_xtxinv);
       FREE_IMARR(impr) ;
     }
#endif
     for( ii=0 ; ii < mm ; ii++ ){  /* fit coefficients */
       for( val=0.0f,jj=0 ; jj < nA ; jj++ ) val += PA(ii,jj)*zA[jj] ;
       betA[ii] = val ;
     }
     for( jj=0 ; jj < nA ; jj++ ){  /* residuals */
       val = -zA[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XA(jj,ii)*betA[ii] ;
       zdifA[jj] = val ; ssqA += val*val ; if( Aresid ) Aresid[jj] = -val ;
     }
     if( testA ){ varA = ssqA / (nA-mm) ; if( varA <= 0.0f ) varA = VBIG ; }
#if 0
     mri_free(axxim_psinv) ; mri_free(axxim_xtxinv) ; /* if they're not NULL */
#endif
   }

   /*-- compute estimates for B parameters --*/

   if( testB || testAB ){
#if 0
     MRI_IMAGE *bxxim_psinv=NULL , *bxxim_xtxinv=NULL ;
     if( psinvB == NULL || xtxinvB == NULL ){  /* matrix wasn't pre-inverted */
       MRI_IMARR *impr ; MRI_IMAGE *bxxim ;
       bxxim = mri_new_vol_empty( nB , mcov+1 , 1 , MRI_float ) ;
       mri_fix_data_pointer(xB,bxxim) ;
       impr = mri_matrix_psinv_pair(bxxim,0.0f) ;
       mri_clear_data_pointer(bxxim) ; mri_free(bxxim) ;
       if( impr == NULL ){ ERROR_message("psinv setB matrix fails"); EXRETURN; }
       bxxim_psinv  = IMARR_SUBIM(impr,0); psinvB  = MRI_FLOAT_PTR(bxxim_psinv );
       bxxim_xtxinv = IMARR_SUBIM(impr,1); xtxinvB = MRI_FLOAT_PTR(bxxim_xtxinv);
       FREE_IMARR(impr) ;
     }
#endif
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nB ; jj++ ) val += PB(ii,jj)*zB[jj] ;
       betB[ii] = val ;
     }
     for( jj=0 ; jj < nB ; jj++ ){
       val = -zB[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XB(jj,ii)*betB[ii] ;
       zdifB[jj] = val ; ssqB += val*val ; if( Bresid ) Bresid[jj] = -val ;
     }
     if( testB ){ varB = ssqB / (nB-mm) ; if( varB <= 0.0f ) varB = VBIG ; }
#if 0
     mri_free(bxxim_psinv) ; mri_free(bxxim_xtxinv) ;
#endif
   }

   /*-- carry out 2-sample (A-B) tests, if any --*/

   if( testAB ){
     float varAB ;

     if( opcode == 2 ){  /* paired (nA==nB, xA==xB, etc.) */

       for( varAB=0.0f,ii=0 ; ii < nA ; ii++ ){
         val = zdifA[ii] - zdifB[ii] ; varAB += val*val ;
       }
       varAB /= (nA-mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA - mm ;
       dof = (dof > dofsub) ? dof-dofsub : 2 ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         den          = xtxA(tt) ; if( den <= 0.0f ) den = 1.e+9f ;
         val          = outvec[kt-1] / sqrtf( varAB*den ) ;
         outvec[kt++] = (toz) ? (float)GIC_student_t2z( (double)val , dof )
                              : TCLIP(val) ;
       }

     } else {            /* unpaired, pooled variance */

       varAB = (ssqA+ssqB)/(nA+nB-2*mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA + nB - 2*mm ;
       dof = (dof > dofsub) ? dof-dofsub : 2 ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         den          = xtxA(tt)+xtxB(tt) ; if( den <= 0.0f ) den = 1.e+9f ;
         val          = outvec[kt-1] / sqrtf( varAB*den );
         outvec[kt++] = (toz) ? (float)GIC_student_t2z( (double)val , dof )
                              : TCLIP(val) ;
       }
     } /* end of unpaired pooled variance */
   }

   /*-- carry out 1-sample A tests, if any --*/

   if( testA ){
     dof = nA - mm ;
     dof = (dof > dofsub) ? dof-dofsub : 2 ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testA & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betA[tt] ;
       den          = xtxA(tt) ; if( den <= 0.0f ) den = 1.e+9f ;
       val          = betA[tt] / sqrtf( varA * den ) ;
       outvec[kt++] = (toz) ? (float)GIC_student_t2z( (double)val , dof )
                            : TCLIP(val) ;
     }
   }

   /*-- carry out 1-sample B tests, if any --*/

   if( testB ){
     dof = nB - mm ;
     dof = (dof > dofsub) ? dof-dofsub : 2 ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testB & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betB[tt] ;
       den          = xtxB(tt) ; if( den <= 0.0f ) den = 1.e+9f ;
       val          = betB[tt] / sqrtf( varB * den ) ;
       outvec[kt++] = (toz) ? (float)GIC_student_t2z( (double)val , dof )
                            : TCLIP(val) ;
     }
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*  zA      = datum for the singleton
    zB      = numB data points for the reference sample
    mcov    = number of covariates
    xA      =    1 X (mcov+1) matrix -- in column-major order {1st columns}
    xB      = numB X (mcov+1) matrix -- in column-major order {are all one}
    psinvB  = (mcov+1) X numB matrix -- in column-major order
    xtxinvB = (mcov+1) X (mcov+1) matrix = inv[xB'xB]
*//*-------------------------------------------------------------------------*/

        /*----- off diagonal elements -----*/

#define xtxBij(i,j) xtxinvB[(i)+(j)*mm]

        /*----- variance(singleton) / variance(group) -----*/

#define SINGLETON_VARIANCE_RATIO singleton_variance_ratio

void regress_toz_singletonA( float zA ,
                             int numB , float *zB ,
                             int mcov ,
                             float *xA ,
                             float *xB , float *psinvB , float *xtxinvB ,
                             float *outvec , float *workspace             )
{
   int kt=0,nws , mm=mcov+1 , nB=numB , nA=1 ;
   float *betB=NULL , *zdifB=NULL ;
   float ssqB=0.0f , varB=0.0f , zdifA ;
   float val , den ; register int ii,jj,tt ;

ENTRY("regress_toz_singletonA") ;

   nws = 0 ;
   betB  = workspace + nws ; nws += mm ;
   zdifB = workspace + nws ; nws += nB ;

   /*-- compute estimates for B parameters --*/

   for( ii=0 ; ii < mm ; ii++ ){
     for( val=0.0f,jj=0 ; jj < nB ; jj++ ) val += PB(ii,jj)*zB[jj] ;
     betB[ii] = val ;
   }
   for( jj=0 ; jj < nB ; jj++ ){
     val = -zB[jj] ;
     for( ii=0 ; ii < mm ; ii++ ) val += XB(jj,ii)*betB[ii] ;
     zdifB[jj] = val ; ssqB += val*val ; if( Bresid ) Bresid[jj] = -val ;
   }
   varB = ssqB / (nB-mm) ; if( varB <= 0.0f ) varB = VBIG ;

   /*-- compute estimate for A (using covariate betas from B) --*/

   zdifA = zA ;
   for( ii=0 ; ii < mm ; ii++ ) zdifA -= XA(0,ii)*betB[ii] ;
   if( Aresid ) Aresid[0] = zdifA ;

   /* Below is the denominator to adjust the t-statistic, which
      basically allows for the variance of zA and the variance
      introduced by the subtracting off the covariate estimates
      themselves (in the loop above) to give the adjusted data zdifA.
      In the case where there are no covariates, then the matrix
      xB = [1 1 1 ... 1]' (numB 1s), so xtxinvB = 1/numB -- but this
      special case is actually hard-coded in ttest_toz_singletonA().
      SINGLETON_VARIANCE_RATIO allows for the variance of zA itself;
      if it were a constant (not random), then SINGLETON_VARIANCE_RATIO
      would be zero, and we'd be back in standard 1-sample t-test-ville. */

   den = SINGLETON_VARIANCE_RATIO ;  /* default value of this is 1 */
   for( jj=0 ; jj < mm ; jj++ ){
     for( ii=0 ; ii < mm ; ii++ ) den += XA(0,ii)*XA(0,jj)*xtxBij(ii,jj) ;
   }
   if( den <  SINGLETON_VARIANCE_RATIO ) den = SINGLETON_VARIANCE_RATIO ;

#if 0
{ static int first=1 ; if( first ){ fprintf(stderr,"\nden = 1+cXXc = %g\n",den) ; first=0 ; } }
#endif

   /*-- carry out singleton A - group B test --*/

   outvec[kt++] = zdifA ;
   val          = zdifA / sqrtf( varB * den ) ;
   outvec[kt++] = (toz) ? (float)GIC_student_t2z( (double)val , (double)(nB-mm) )
                        : TCLIP(val) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Various sorts of simple t-tests; output = Z-score.
   - numx = number of points in the first sample (must be > 1)
   - xar  = array with first sample
   - numy = number of points in the second sample
             - numy = 0 ==> a 1 sample test of first sample against mean=0
  DISABLED   - numy = 1 ==> a 1 sample test of first sample against mean=yar[0]
             - numy > 1 ==> a 2 sample test; opcode determines what kind
   - opcode = 0 for unpaired test with pooled variance
   - opcode = 1 for unpaired test with unpooled variance
   - opcode = 2 for paired test (numx == numy is required)
   - xres, yres = space for residuals (data-mean), if not NULL [07 Dec 2015]
   - The return value is the Z-score of the t-statistic.

   A simple 2-sample test of this function:
      3dUndump -dimen 128 128 32 -prefix ZZ
      3dcalc -a ZZ+orig -b '1D: 14@0' -expr 'gran(1,1)' -prefix ZZ_1.nii -datum float
      3dcalc -a ZZ+orig -b '1D: 10@0' -expr 'gran(0,1)' -prefix ZZ_0.nii -datum float
      3dttest++ -setA ZZ_1.nii -setB ZZ_0.nii -prefix ZZtest.nii -no1sam
      echo "=== mean of mean estimates follows, should be 1 ==="
      3dBrickStat -mean ZZtest.nii'[0]'
      echo "=== mean of t-statistics follows, should be 2.50149 ==="
      3dBrickStat -mean ZZtest.nii'[1]'
      \rm ZZ*
   The mean t-statistic with 14 samples in setA and 10 samples in setB
   is calculated as
      sigma / sqrt( 1/NA + 1/NB ) / (1 - 3/(4*NA+4*NB-9) )
    =   1   / sqrt( 1/14 + 1/10 ) / (1 - 3/87            ) = 2.50149
   where division by (1-3/(4*NA+4*NB-9)) is the correction factor
   for the skewness of the non-central t-distribution
   (see http://en.wikipedia.org/wiki/Noncentral_t-distribution).
*//*--------------------------------------------------------------------------*/

float_pair ttest_toz( int numx, float *xar, int numy, float *yar, int opcode,
                      float *xres, float *yres )
{
   float_pair result = {0.0f,0.0f} ;
   register int ii ; register float val ;
   float avx,sdx , avy,sdy , dof , tstat=0.0f,delta=0.0f ;
   int paired,pooled ;

ENTRY("ttest_toz") ;
   if( numy == 0 || yar == NULL ) opcode = 0 ;  /* 03 Mar 2011 */
   paired = (opcode==2) ; pooled = (opcode==0) ;

#if 1
   /* check inputs for stoopidities or other things that need to be changed */

   if( numx < 2 || xar == NULL                 ) RETURN(result) ; /* bad */
   if( paired && (numy != numx || yar == NULL) ) RETURN(result) ; /* bad */
#endif

   if( numy < 2 || yar == NULL ){ numy = paired = pooled = 0 ; yar = NULL ; }

   if( paired ){   /* Case 1: paired t test */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii]-yar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-yar[ii]-avx; sdx += val*val; }
     if( sdx > 0.0f )      tstat = avx / sqrtf( sdx/((numx-1.0f)*numx) ) ;
     else if( avx > 0.0f ) tstat =  19.0f ;
     else if( avx < 0.0f ) tstat = -19.0f ;
     else                  tstat =   0.0f ;
     dof = numx-1.0f ; delta = avx ;  /* delta = diff in means */

     if( xres != NULL ){
       avx = 0.0f ; for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
       avx /= numx; for( ii=0 ; ii < numx ; ii++ ) xres[ii] = xar[ii]-avx ;
     }
     if( yres != NULL ){
       avy = 0.0f ; for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
       avy /= numy; for( ii=0 ; ii < numy ; ii++ ) yres[ii] = yar[ii]-avy ;
     }

   } else if( numy == 0 ){  /* Case 2: 1 sample test against mean==0 */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-avx ; sdx += val*val ; }
     if( sdx > 0.0f )      tstat = avx / sqrtf( sdx/((numx-1.0f)*numx) ) ;
     else if( avx > 0.0f ) tstat =  19.0f ;
     else if( avx < 0.0f ) tstat = -19.0f ;
     else                  tstat =   0.0f ;
     dof = numx-1.0f ; delta = avx ; /* delta = mean */
     if( xres != NULL ) for( ii=0 ; ii < numx ; ii++ ) xres[ii] = xar[ii]-avx ;

   } else {  /* Case 3: 2 sample test (pooled or unpooled) */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii] - avx ; sdx += val*val ; }
     if( xres != NULL ) for( ii=0 ; ii < numx ; ii++ ) xres[ii] = xar[ii]-avx ;

     avy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
     avy /= numy ; sdy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ){ val = yar[ii] - avy ; sdy += val*val ; }
     if( yres != NULL ) for( ii=0 ; ii < numy ; ii++ ) yres[ii] = yar[ii]-avy ;

     delta = avx - avy ; /* difference in means */

     if( debug > 1 )
       fprintf(stderr,"  ttest: Amean=%g Asq=%g  Bmean=%g Bsq=%g  Delta=%g\n",
               avx,sdx , avy,sdy , delta ) ;

     if( sdx+sdy == 0.0f ){

            if( delta > 0.0f ) tstat =  19.0f ;
       else if( delta < 0.0f ) tstat = -19.0f ;
       else                    tstat =   0.0f ;
       dof = numx+numy-2.0f ;

     } else if( pooled ){  /* Case 3a: pooled variance estimate */

       sdx   = (sdx+sdy) / (numx+numy-2.0f) ;
       tstat = delta / sqrtf( sdx*(1.0f/numx+1.0f/numy) ) ;
       dof   = numx+numy-2.0f ;

     } else {       /* Case 3b: unpooled variance estimate */

       sdx  /= (numx-1.0f)*numx ; sdy /= (numy-1.0f)*numy ; val = sdx+sdy ;
       tstat = delta / sqrtf(val) ;
       dof   = (val*val) / (sdx*sdx/(numx-1.0f) + sdy*sdy/(numy-1.0f) ) ;

     }

   } /* end of all possible cases */

   dof = (dof > dofsub) ? dof-dofsub : 2 ;

   result.a = delta ;
   result.b = (toz) ? (float)GIC_student_t2z( (double)tstat , (double)dof )
                    : TCLIP(tstat) ;
   RETURN(result) ;
}

/*----------------------------------------------------------------------------*/
/* the simple test of a single value xar vs. the group yar;
   however, xar is assumed to have the same variance as yar[]
*//*--------------------------------------------------------------------------*/

float_pair ttest_toz_singletonA( float xar , int numy, float *yar,
                                 float *xres , float *yres )
{
   float_pair result = {0.0f,0.0f} ;
   int ii ;
   float avy , sdy , tstat , dof ;

ENTRY("ttest_toz_singletonA") ;

   avy = 0.0f ;
   for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
   avy /= numy ; sdy = 0.0f ;
   for( ii=0 ; ii < numy ; ii++ ) sdy += (yar[ii]-avy)*(yar[ii]-avy) ;
   sdy /= (numy-1.0f) ;  /* variance (estimate) for yar */
   if( yres != NULL ) for( ii=0 ; ii < numy ; ii++ ) yres[ii] = yar[ii]-avy ;

   /* Normally, the tstat of yar against a constant would have
      the denominator sqrt(sdy/numy) but in this case, the
      denominator is sqrt(sdy*(1+1/numy)) since we are assuming
      the variance of the singleton xar is the same that of yar;
      the result is that the tstat is smaller and less significant. */

   sdy *= (SINGLETON_VARIANCE_RATIO + 1.0f/numy) ; if( sdy <= 0.0f ) sdy = 1.e+9f ;

   result.a = (xar-avy) ;
   tstat    = (xar-avy) / sqrtf(sdy) ;
   dof      = numy-1.0f ;
   dof      = (dof > dofsub) ? dof-dofsub : 2 ;
   result.b = (toz) ? (float)(float)GIC_student_t2z( (double)tstat , (double)dof )
                    : TCLIP(tstat) ;
   if( xres != NULL ) xres[0] = result.a ;

   RETURN(result) ;
}

/*=======================================================================*/
/** The following routines are for the t-to-z conversion, and are
    adapted from mri_stats.c to be parallelizable (no static data).
=========================================================================*/

static double GIC_qginv( double p )
{
   double dp , dx , dt , ddq , dq ;
   int    newt ;                       /* not Gingrich, but Isaac */

   dp = (p <= 0.5) ? (p) : (1.0-p) ;   /* make between 0 and 0.5 */

   if( dp <= 1.e-37 ){
      dx = 13.0 ;                      /* 13 sigma has p < 10**(-38) */
      return ( (p <= 0.5) ? (dx) : (-dx) ) ;
   }

/**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/

   dt = sqrt( -2.0 * log(dp) ) ;
   dx = dt
        - ((.010328*dt + .802853)*dt + 2.515517)
        /(((.001308*dt + .189269)*dt + 1.432788)*dt + 1.) ;

/**  Step 2:  do 3 Newton steps to improve this
              (uses the math library erfc function) **/

   for( newt=0 ; newt < 3 ; newt++ ){
     dq  = 0.5 * erfc( dx / 1.414213562373095 ) - dp ;
     ddq = exp( -0.5 * dx * dx ) / 2.506628274631000 ;
     dx  = dx + dq / ddq ;
   }

   if( dx > 13.0 ) dx = 13.0 ;
   return ( (p <= 0.5) ? (dx) : (-dx) ) ;  /* return with correct sign */
}

#ifdef NO_GAMMA
/*-----------------------------------------------------------------------*/
/* If the system doesn't provide lgamma() for some primitive reason.
-------------------------------------------------------------------------*/

/**----- log of gamma, for argument between 1 and 2 -----**/

static double gamma_12( double y )
{
   double x , g ;
   x = y - 1.0 ;
   g = ((((((( 0.035868343 * x - 0.193527818 ) * x
                               + 0.482199394 ) * x
                               - 0.756704078 ) * x
                               + 0.918206857 ) * x
                               - 0.897056937 ) * x
                               + 0.988205891 ) * x
                               - 0.577191652 ) * x + 1.0 ;
   return log(g) ;
}

/**----- asymptotic expansion of ln(gamma(x)) for large positive x -----**/

#define LNSQRT2PI 0.918938533204672  /* ln(sqrt(2*PI)) */

static double gamma_asympt(double x)
{
   double sum ;

   sum = (x-0.5)*log(x) - x + LNSQRT2PI + 1.0/(12.0*x) - 1./(360.0*x*x*x) ;
   return sum ;
}

/**----- log of gamma, argument positive (not very efficient!) -----**/

static double GIC_lgamma( double x )
{
   double w , g ;

   if( x <= 0.0 ) return 0.0 ;  /* should not happen */

   if( x <  1.0 ) return gamma_12( x+1.0 ) - log(x) ;
   if( x <= 2.0 ) return gamma_12( x ) ;
   if( x >= 6.0 ) return gamma_asympt(x) ;

   g = 0 ; w = x ;
   while( w > 2.0 ){ w -= 1.0 ; g += log(w) ; }
   return ( gamma_12(w) + g ) ;
}

#define lgamma GIC_lgamma

#endif  /*----- NO_GAMMA ------------------------------------------------*/

/*----------------------------------------------------------------------*/

static double GIC_lnbeta( double p , double q )
{
   return (lgamma(p) + lgamma(q) - lgamma(p+q)) ;
}

/*----------------------------------------------------------------------*/

#define ZERO 0.0
#define ONE  1.0
#define ACU  1.0e-15

static double GIC_incbeta( double x , double p , double q , double beta )
{
   double betain , psq , cx , xx,pp,qq , term,ai , temp , rx ;
   int indx , ns ;

   if( p <= ZERO || q <= ZERO ) return -1.0 ;  /* error! */

   if( x <= ZERO ) return ZERO ;
   if( x >= ONE  ) return ONE ;

   /**  change tail if necessary and determine s **/

   psq = p+q ;
   cx  = ONE-x ;
   if(  p < psq*x ){
      xx   = cx ; cx   = x ; pp   = q ; qq   = p ; indx = 1 ;
   } else {
      xx   = x ; pp   = p ; qq   = q ; indx = 0 ;
   }

   term   = ONE ;
   ai     = ONE ;
   betain = ONE ;
   ns     = qq + cx*psq ;

   /** use soper's reduction formulae **/

      rx = xx/cx ;

lab3:
      temp = qq-ai ;
      if(ns == 0) rx = xx ;

lab4:
      term   = term*temp*rx/(pp+ai) ;
      betain = betain+term ;
      temp   = fabs(term) ;
      if(temp <= ACU && temp <= ACU*betain) goto lab5 ;

      ai = ai+ONE ;
      ns = ns-1 ;
      if(ns >= 0) goto lab3 ;
      temp = psq ;
      psq  = psq+ONE ;
      goto lab4 ;

lab5:
      betain = betain*exp(pp*log(xx)+(qq-ONE)*log(cx)-beta)/pp ;
      if(indx) betain=ONE-betain ;

   return betain ;
}

/*----------------------------------------------------------------------*/

#undef  ZMAX
#define ZMAX 13.0  /* largest allowed z-score == 1 sided p=6e-36 */

double GIC_student_t2z( double tt , double dof )
{
   double xx , pp , bb ;

   bb = GIC_lnbeta( 0.5*dof , 0.5 ) ;

   xx = dof/(dof + tt*tt) ;
   pp = GIC_incbeta( xx , 0.5*dof , 0.5 , bb ) ;

   if( tt > 0.0 ) pp = 1.0 - 0.5 * pp ;
   else           pp = 0.5 * pp ;

   xx = - GIC_qginv(pp) ;
   if( xx > ZMAX ) xx = ZMAX ; else if( xx < -ZMAX ) xx = -ZMAX ;
   return xx ;
}

/*===========================================================================*/
/* center covariates in the regression matrices */

void TT_centerize(void)
{
   int jj,kk ; float sum ;
   int nvv , iv ; float *vv ;

   if( center_code == CENTER_NONE ) return ;

   nvv = nval_AAA + nval_BBB ;
   vv  = (float *)malloc(sizeof(float)*nvv) ;

ENTRY("TT_centerize") ;

   /*-- process each matrix separately --*/

   if( center_code != CENTER_SAME || Bxx == NULL || Bxx == Axx || singletonA ){

     if( !singletonA ){
       for( jj=1 ; jj <= mcov ; jj++ ){
         for( kk=0 ; kk < nval_AAA ; kk++ ) vv[kk] = AXX(kk,jj) ;
         if( center_meth == CMETH_MEDIAN ) sum = qmed_float (nval_AAA,vv) ;
         else                              sum = qmean_float(nval_AAA,vv) ;
         for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,jj) -= sum ;
       }
     }

     if( Bxx != NULL & Bxx != Axx ){
       for( jj=1 ; jj <= mcov ; jj++ ){
         for( kk=0 ; kk < nval_BBB ; kk++ ) vv[kk] = BXX(kk,jj) ;
         if( center_meth == CMETH_MEDIAN ) sum = qmed_float (nval_BBB,vv) ;
         else                              sum = qmean_float(nval_BBB,vv) ;
         for( kk=0 ; kk < nval_BBB ; kk++ ) BXX(kk,jj) -= sum ;
         if( singletonA ) AXX(0,jj) -= sum ;
       }
     }

   } else {  /*-- process them together --*/

     for( jj=1 ; jj <= mcov ; jj++ ){
       for( kk=iv=0 ; kk < nval_AAA ; kk++,iv++ ) vv[iv] = AXX(kk,jj) ;
       for( kk=0    ; kk < nval_BBB ; kk++,iv++ ) vv[iv] = BXX(kk,jj) ;
       if( center_meth == CMETH_MEDIAN ) sum = qmed_float (iv,vv) ;
       else                              sum = qmean_float(iv,vv) ;
       for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,jj) -= sum ;
       for( kk=0 ; kk < nval_BBB ; kk++ ) BXX(kk,jj) -= sum ;
     }

   }

   free(vv) ;
   EXRETURN ;
}

/*===========================================================================*/
/* make the regression matrices for covariates */

void TT_matrix_setup( int kout )
{
   int jj,kk ; float sum , *fpt ;
   static MRI_IMARR *imprA=NULL , *imprB=NULL ;
   char label[32] ;

ENTRY("TT_matrix_setup") ;

   /*-- load setA matrix [first colum is all 1s] --*/

   for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,0) = 1.0f ; /* the mean */
   for( jj=1 ; jj <= mcov ; jj++ ){
     if( covvec_AAA[jj-1] != NULL ) fpt = covvec_AAA[jj-1]->ar ;
     else                           fpt = VECTIM_PTR(covvim_AAA[jj-1],kout) ;
     for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,jj) = fpt[kk] ;
   }

   /*-- load setB matrix [first colum is all 1s] --*/

   if( twosam && ttest_opcode != 2 ){  /* un-paired 2-sample case */
     for( kk=0 ; kk < nval_BBB ; kk++ ) BXX(kk,0) = 1.0f ; /* the mean */
     for( jj=1 ; jj <= mcov ; jj++ ){
       if( covvec_BBB[jj-1] != NULL ) fpt = covvec_BBB[jj-1]->ar ;
       else                           fpt = VECTIM_PTR(covvim_BBB[jj-1],kout) ;
       for( kk=0 ; kk < nval_BBB ; kk++ ) BXX(kk,jj) = fpt[kk] ;
     }
   }

#ifdef ALLOW_RANK
   if( do_ranks ){
     for( jj=1 ; jj <= mcov ; jj++ ){
       if( twosam && ttest_opcode != 2 )
         rank_order_2floats( nval_AAA , &(AXX(0,jj)) , nval_BBB , &(BXX(0,jj)) ) ;
       else
         rank_order_float( nval_AAA , &(AXX(0,jj)) ) ;
     }
   }
#endif

   TT_centerize() ; /* column de-mean-ization? */

   if( debug ){
     sprintf(label,"setA voxel#%d",kout) ;
     mri_matrix_print(stderr,Axxim,label) ;
     if( twosam && ttest_opcode != 2 ){
       sprintf(label,"setB voxel#%d",kout) ;
       mri_matrix_print(stderr,Bxxim,label) ;
     }
   }

   /*-- (pseudo) invert matrices --*/

   /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for setA */

   if( imprA != NULL ) DESTROY_IMARR(imprA) ;

   if( !singletonA ){
     imprA = mri_matrix_psinv_pair( Axxim , 0.0f ) ;
     if( imprA == NULL ) ERROR_exit("Can't invert setA covariate matrix?! :-(") ;
     Axx_psinv  = MRI_FLOAT_PTR(IMARR_SUBIM(imprA,0)) ;
     Axx_xtxinv = MRI_FLOAT_PTR(IMARR_SUBIM(imprA,1)) ;
   } else {
     Axx_psinv  = NULL ;
     Axx_xtxinv = NULL ;
   }

   if( debug && !singletonA ){
     sprintf(label,"setA psinv") ;
     mri_matrix_print(stderr,IMARR_SUBIM(imprA,0),label) ;
     sprintf(label,"setA xtxinv") ;
     mri_matrix_print(stderr,IMARR_SUBIM(imprA,1),label) ;
   }

   /* and for setB, if needed */

   if( twosam && ttest_opcode != 2 ){  /* un-paired 2-sample case */
     if( imprB != NULL ) DESTROY_IMARR(imprB) ;
     imprB = mri_matrix_psinv_pair( Bxxim , 0.0f ) ;
     if( imprB == NULL ) ERROR_exit("Can't invert setB covariate matrix?! :-(") ;
     Bxx_psinv  = MRI_FLOAT_PTR(IMARR_SUBIM(imprB,0)) ;
     Bxx_xtxinv = MRI_FLOAT_PTR(IMARR_SUBIM(imprB,1)) ;

     if( debug ){
       sprintf(label,"setB psinv") ;
       mri_matrix_print(stderr,IMARR_SUBIM(imprB,0),label) ;
       sprintf(label,"setB xtxinv") ;
       mri_matrix_print(stderr,IMARR_SUBIM(imprB,1),label) ;
     }

   } else if( twosam && ttest_opcode == 2 ){
     Bxx_psinv = Axx_psinv ; Bxx_xtxinv = Axx_xtxinv ;
   }

   EXRETURN ;
}
