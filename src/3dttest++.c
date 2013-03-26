#include "mrilib.h"

/*------------------------------- prototypes, etc. ---------------------------*/

#undef  LTRUNC
#define LTRUNC(ss) \
 do{ if( strlen(ss) > MAX_LABEL_SIZE ){(ss)[MAX_LABEL_SIZE] = '\0'; }} while(0)

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             ) ;

float_pair ttest_toz( int numx, float *xar, int numy, float *yar, int opcode ) ;

double GIC_student_t2z( double tt , double dof ) ;

void TT_matrix_setup( int kout ) ;  /* 30 Jul 2010 */

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

#undef  MAXCOV
#define MAXCOV 31

#undef  MAX_LABEL_SIZE
#define MAX_LABEL_SIZE 12

static int toz    = 0 ;  /* convert t-statistics to z-scores? */
static int twosam = 0 ;

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

static unsigned int testA, testB, testAB ;

#define CENTER_NONE 0
#define CENTER_DIFF 1
#define CENTER_SAME 2

#define CMETH_MEDIAN 1
#define CMETH_MEAN   2

static int mcov  = 0 ;
static int nvout = 0 ;
static int center_code = CENTER_DIFF ;
static int center_meth = CMETH_MEAN ;    /* 26 Mar 2013 */
MRI_IMAGE *Axxim=NULL , *Bxxim=NULL ;
static float *Axx=NULL , *Axx_psinv=NULL , *Axx_xtxinv=NULL ;
static float *Bxx=NULL , *Bxx_psinv=NULL , *Bxx_xtxinv=NULL ;

#undef  AXX
#define AXX(i,j) Axx[(i)+(j)*(nval_AAA)]    /* i=0..nval_AAA-1 , j=0..mcov */
#undef  BXX
#define BXX(i,j) Bxx[(i)+(j)*(nval_BBB)]    /* i=0..nval_BBB-1 , j=0..mcov */

static char *prefix = "TTnew" ;
static byte *mask   = NULL ;
static int  nmask   = 0 ;
static int  nvox    = 0 ;
static int  nmask_hits = 0 ;

static int ttest_opcode = 0 ;  /* 0=pooled, 1=unpooled, 2=paired */

static int               ndset_AAA=0 , nval_AAA=0 ;
static char              *snam_AAA=NULL , *lnam_AAA=NULL ;
static char             **name_AAA=NULL ;
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
      "  ++ If multiple sub-bricks are input from a single dataset, then\n"
      "     covariates cannot be used (sorry, Charlie).\n"
      "  ++ For some limited compatibility with 3dttest, you can use '-set2' in\n"
      "     place of '-setA', and '-set1' in place of '-setB'.\n"
      "  ++ [19 Jun 2012, from Beijing Normal University, during AFNI Bootcamp]\n"
      "     For the SHORT FORM only, you can use the wildcards '*' and/or '?' in\n"
      "     the BETA_DSET filenames, along with sub-brick selectors, to make it\n"
      "     easier to create the command line.\n"
      "     To protect the wildcards from the shell, the entire filename should be\n"
      "     inside quote marks.  For example:\n"
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
      "  ++ e.g.,  Fred+tlrc'[3]'  ==>  Fred\n"
      "  ++ e.g.,  Elvis.nii.gz    ==>  Elvis\n"
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
      "                 '-unpooled' is not needed.\n"
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
#endif
      "\n"
      " -no1sam   = When you input two samples (setA and setB), normally the\n"
      "              program outputs the 1-sample test results for each set\n"
      "              (comparing to zero), as well as the 2-sample test results\n"
      "              for differences between the sets.  With '-no1sam', these\n"
      "              1-sample test results will NOT be calculated or saved.\n"
      "\n"
      " -mask mmm = Only compute results for voxels in the specified mask.\n"
      "             ++ Voxels not in the mask will be set to 0 in the output.\n"
      "             ++ If '-mask' is not used, all voxels will be tested.\n"
      "             ++ HOWEVER: voxels whose input data is constant (in either set)\n"
      "                 will NOT be processed and will get all zero outputs.  This\n"
      "                 inaction happens because the variance of a constant set of\n"
      "                 data is zero, and division by zero is forbidden by the\n"
      "                 Deities of Mathematics.\n"
      "\n"
      " -prefix p = Gives the name of the output dataset file.\n"
      "          ++ For surface-based datasets, use something like:\n"
      "              -prefix p.niml.dset or -prefix p.gii.dset \n"
      "             Otherwise you may end up files containing numbers but\n"
      "             not a full set of header information.\n"
      "\n"
      " -debug    = Prints out information about the analysis, which can\n"
      "               be VERY lengthy -- not for general usage.\n"
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

      "-------------------------\n"
      "VARIOUS LINKS OF INTEREST\n"
      "-------------------------\n"
      "* http://en.wikipedia.org/wiki/T_test\n"
      "* http://www.statsoft.com/textbook/basic-statistics/\n"
      "* http://en.wikipedia.org/wiki/Mutatis_mutandis\n"
      "\n"
      "----------------------------------------------------\n"
      "AUTHOR -- RW Cox -- don't whine TO me; wine WITH me.\n"
      "----------------------------------------------------\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

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

int main( int argc , char *argv[] )
{
   int nopt, nbad, ii,jj,kk, kout,ivox, vstep, dconst, nconst=0, nzskip=0,nzred=0  ;
   MRI_vectim *vimout ;
   float *workspace=NULL , *datAAA , *datBBB=NULL , *resar ; size_t nws=0 ;
   float_pair tpair ;
   THD_3dim_dataset *outset ;
   char blab[64] , *stnam ;
   float dof_AB=0.0f , dof_A=0.0f , dof_B=0.0f ;
   int BminusA=-1 , ntwosam=0 ;  /* 05 Nov 2010 */
   char *snam_PPP=NULL, *snam_MMM=NULL ;
   static int iwarn=0;

   /*--- help the piteous luser? ---*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

   /*--- record things for posterity, et cetera ---*/

   mainENTRY("3dttest++ main"); machdep(); AFNI_logger("3dttest++",argc,argv);
   PRINT_VERSION("3dttest++") ; AUTHOR("The Bob++") ;

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   /*--- read the options from the command line ---*/

   PUTENV("AFNI_GLOB_SELECTORS","YES") ;  /* 19 Jun 2012 */

   nopt = 1 ;
   while( nopt < argc ){

     /*----- no1sam -----*/

     if( strcasecmp(argv[nopt],"-no1sam") == 0 ){   /* 10 Nov 2010 */
       do_1sam = 0 ; nopt++ ; continue ;
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
         if( ndset_AAA > 0 ) ERROR_exit("Can't use '-setA' twice!") ;
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
             nds++ ; nv += DSET_NVALS(qset) ;
             nams = (char **)realloc(nams,sizeof(char *)*nds) ;
             labs = (char **)realloc(labs,sizeof(char *)*nds) ;
             dset = (THD_3dim_dataset **)realloc(dset,sizeof(THD_3dim_dataset *)*nds) ;
             nams[nds-1] = strdup(fexp[iex]) ; dset[nds-1] = qset ;
             labs[nds-1] = strdup(THD_trailname(fexp[iex],0)) ;
             cpt = strchr(labs[nds-1]+1,'+')    ; if( cpt != NULL ) *cpt = '\0' ;
             cpt = strstr(labs[nds-1]+1,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
             LTRUNC(labs[nds-1]) ;
           }
           if( didex ) MCW_free_expand(nexp,fexp) ;
         }

         if( nv > nds ) allow_cov = 0 ;  /* multiple sub-bricks from 1 input */
         if( nv < 2 )
           ERROR_exit("Option %s (short form): need at least 2 datasets or sub-bricks",onam) ;

       } else {  /* not a dataset => label label dset label dset ... */

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
         }
       }
       if( nbad > 0 ) ERROR_exit("Cannot go on after such an error!") ;

       /* check for duplicate labels */

       for( ii=0 ; ii < nds ; ii++ ){
         for( jj=ii+1 ; jj < nds ; jj++ )
           if( strcmp(labs[ii],labs[jj]) == 0 ) nbad++ ;
       }
       if( nbad > 0 ) allow_cov = -1 ;  /* duplicate labels */

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

     /*----- covariates -----*/

     if( strcasecmp(argv[nopt],"-covariates") == 0 ){  /* 20 May 2010 */
       char *lab ; float sig , men ; char nlab[2048] , dlab[2048] ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       if( covnel != NULL ) ERROR_exit("can't use -covariates twice!") ;
       covnel = THD_mixed_table_read( argv[nopt] ) ;
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

     ERROR_exit("3dttest++: don't recognize option '%s'",argv[nopt]) ;

   }  /*-------------------- end of option parsing --------------------*/

   /*----- check some stuff -----*/

   twosam = (nval_BBB > 1) ; /* 2 sample test? */

   if( nval_AAA <= 0 )
     ERROR_exit("No '-setA' option?  Please please read the -help instructions!") ;

   if( nval_AAA != nval_BBB && ttest_opcode == 2 )
     ERROR_exit("Can't do '-paired' with unequal set sizes: #A=%d #B=%d",
                nval_AAA , nval_BBB ) ;

   nvox = DSET_NVOX(dset_AAA[0]) ;
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

   if( nval_AAA - mcov < 2 ||
       ( twosam && (nval_BBB - mcov < 2) ) )
     ERROR_exit("Too many covariates compared to number of datasets") ;

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
     WARNING_message("-covariates does not support unpooled variance") ;
     ttest_opcode = 0 ;
   }

   if( do_zskip && !toz ){
     toz = 1 ; INFO_message("-zskip also turns on -toz") ;
   }
   if( ttest_opcode == 1 && !toz ){
     toz = 1 ; INFO_message("-unpooled also turns on -toz") ;
   }

   if( !do_1sam && !twosam ){
     WARNING_message("-no1sam and no -setB datasets?  What do you mean?") ;
     do_1sam = 1 ;
   }

#ifdef ALLOW_RANK
   if( do_ranks && !twosam ){
     WARNING_message("-rankize only works with two-sample tests ==> ignoring") ;
     do_ranks = 0 ; do_1sam = 1 ;
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
     INFO_message("%s test: results are %s - %s",
                  ttest_opcode == 2 ? "paired":"2-sample", snam_PPP,snam_MMM) ;
   }

   /*----- convert each input set of datasets to a vectim -----*/

   INFO_message("loading input datasets") ;

   vectim_AAA = THD_dset_list_to_vectim( ndset_AAA , dset_AAA , mask ) ;

   if( twosam )
     vectim_BBB = THD_dset_list_to_vectim( ndset_BBB , dset_BBB , mask ) ;

   MEMORY_CHECK ;

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

     /*-- next, create the covariate regression matrices --*/

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

     /*-- fill them in and (pseudo)invert them */

     TT_matrix_setup(0) ;  /* 0 = voxel index (just sayin') */

     if( num_covset_col > 0 ) MEMORY_CHECK ;

     if( twosam && num_covset_col < mcov ){             /* 19 Oct 2010 */
       int toz_sav = toz ; float pp ;         /* test covariates for equality */

       toz = 1 ;
       INFO_message(
         "Two samples: t-testing fixed covariates for similarity between groups") ;
       for( jj=0 ; jj < mcov ; jj++ ){
         if( covnel->vec_typ[jj+1] == NI_STRING ){
           ININFO_message(" %s: values come from datasets ==> skipping test" ,
                          covlab->str[jj+1] ) ;
         } else {
           tpair = ttest_toz( ndset_AAA , covvec_AAA[jj]->ar ,
                              ndset_BBB , covvec_BBB[jj]->ar , 0 ) ;
           pp = normal_t2p( fabs((double)tpair.b) ) ;
           ININFO_message(" %s: mean of setA-setB=%s ; 2-sided p-value=%.4f" ,
                          covlab->str[jj+1] , MV_format_fval(tpair.a) , pp ) ;
         }
       }
       toz = toz_sav ;
     } else INFO_message("twosam=%d num_covset_col=%d mcov=%d",twosam,num_covset_col,mcov) ;

   }  /*-- end of covariates setup --*/

   /*-------------------- create empty output dataset ---------------------*/

   nvout  = ((twosam && do_1sam) ? 6 : 2) * (mcov+1) ; /* # of output volumes */

   outset = EDIT_empty_copy( dset_AAA[0] ) ;

   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , nvout  ,
                      ADN_ntt       , 0      ,
                      ADN_brick_fac , NULL   ,
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_deconflict_prefix(outset) )
     ERROR_exit("Output dataset '%s' already exists!",prefix) ;

   tross_Make_History( "3dttest++" , argc,argv , outset ) ;

   /* make up some brick labels [[[man, this is tediously boring work]]] */

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

   dof_A = nval_AAA - (mcov+1) ;
   if( twosam ){
    dof_B  = nval_BBB - (mcov+1) ;
    dof_AB = (ttest_opcode==2) ? dof_A : dof_A+dof_B ;
   }

   stnam = (toz) ? "Zscr" : "Tstat" ;
   if( mcov <= 0 ){                    /*--- no covariates ---*/
     if( !twosam ){
       sprintf(blab,"%s_mean",snam_AAA);       EDIT_BRICK_LABEL(outset,0,blab);
       sprintf(blab,"%s_%s"  ,snam_AAA,stnam); EDIT_BRICK_LABEL(outset,1,blab);
          if( toz ) EDIT_BRICK_TO_FIZT(outset,1) ;
          else      EDIT_BRICK_TO_FITT(outset,1,dof_A) ;
     } else {
       ntwosam = 2 ;
       sprintf(blab,"%s-%s_mean",snam_PPP,snam_MMM)      ; EDIT_BRICK_LABEL(outset,0,blab);
       sprintf(blab,"%s-%s_%s"  ,snam_PPP,snam_MMM,stnam); EDIT_BRICK_LABEL(outset,1,blab);
          if( toz ) EDIT_BRICK_TO_FIZT(outset,1) ;
          else      EDIT_BRICK_TO_FITT(outset,1,dof_AB) ;
       if( do_1sam ){
         sprintf(blab,"%s_mean",snam_AAA)                  ; EDIT_BRICK_LABEL(outset,2,blab);
         sprintf(blab,"%s_%s"  ,snam_AAA,stnam)            ; EDIT_BRICK_LABEL(outset,3,blab);
            if( toz ) EDIT_BRICK_TO_FIZT(outset,3) ;
            else      EDIT_BRICK_TO_FITT(outset,3,dof_A) ;
         sprintf(blab,"%s_mean",snam_BBB)                  ; EDIT_BRICK_LABEL(outset,4,blab);
         sprintf(blab,"%s_%s"  ,snam_BBB,stnam)            ; EDIT_BRICK_LABEL(outset,5,blab);
            if( toz ) EDIT_BRICK_TO_FIZT(outset,5) ;
            else      EDIT_BRICK_TO_FITT(outset,5,dof_B) ;
       }
     }
   } else {                            /*--- have covariates ---*/
     kk = 0 ;
     if( testAB ){                     /* 2-sample results */
       ntwosam = 2*(mcov+1) ;
       sprintf(blab,"%s-%s_mean",snam_PPP,snam_MMM);
         EDIT_BRICK_LABEL(outset,kk,blab); kk++;
       sprintf(blab,"%s-%s_%s"  ,snam_PPP,snam_MMM,stnam);
         EDIT_BRICK_LABEL(outset,kk,blab);
         if( toz ) EDIT_BRICK_TO_FIZT(outset,kk) ;
         else      EDIT_BRICK_TO_FITT(outset,kk,dof_AB) ;
         kk++;
       for( jj=1 ; jj <= mcov ; jj++ ){
         sprintf(blab,"%s-%s_%s",snam_PPP,snam_MMM,covlab->str[jj]) ;
           EDIT_BRICK_LABEL(outset,kk,blab); kk++;
         sprintf(blab,"%s-%s_%s_%s",snam_PPP,snam_MMM,covlab->str[jj],stnam) ;
           EDIT_BRICK_LABEL(outset,kk,blab);
         if( toz ) EDIT_BRICK_TO_FIZT(outset,kk) ;
         else      EDIT_BRICK_TO_FITT(outset,kk,dof_AB) ;
         kk++;
       }
     }
     if( testA ){                      /* 1-sample results */
       sprintf(blab,"%s_mean",snam_AAA) ;
         EDIT_BRICK_LABEL(outset,kk,blab); kk++;
       sprintf(blab,"%s_%s",snam_AAA,stnam) ;
         EDIT_BRICK_LABEL(outset,kk,blab);
         if( toz ) EDIT_BRICK_TO_FIZT(outset,kk) ;
         else      EDIT_BRICK_TO_FITT(outset,kk,dof_A) ;
         kk++;
       for( jj=1 ; jj <= mcov ; jj++ ){
         sprintf(blab,"%s_%s",snam_AAA,covlab->str[jj]) ;
           EDIT_BRICK_LABEL(outset,kk,blab); kk++;
         sprintf(blab,"%s_%s_%s",snam_AAA,covlab->str[jj],stnam) ;
           EDIT_BRICK_LABEL(outset,kk,blab);
           if( toz ) EDIT_BRICK_TO_FIZT(outset,kk) ;
           else      EDIT_BRICK_TO_FITT(outset,kk,dof_A) ;
           kk++;
       }
     }
     if( testB ){                      /* 1-sample results */
       sprintf(blab,"%s_mean",snam_BBB) ;
         EDIT_BRICK_LABEL(outset,kk,blab); kk++;
       sprintf(blab,"%s_%s",snam_BBB,stnam) ;
         EDIT_BRICK_LABEL(outset,kk,blab);
         if( toz ) EDIT_BRICK_TO_FIZT(outset,kk) ;
         else      EDIT_BRICK_TO_FITT(outset,kk,dof_B) ;
         kk++;
       for( jj=1 ; jj <= mcov ; jj++ ){
         sprintf(blab,"%s_%s",snam_BBB,covlab->str[jj]) ;
           EDIT_BRICK_LABEL(outset,kk,blab); kk++;
         sprintf(blab,"%s_%s_%s",snam_BBB,covlab->str[jj],stnam) ;
           EDIT_BRICK_LABEL(outset,kk,blab);
           if( toz ) EDIT_BRICK_TO_FIZT(outset,kk) ;
           else      EDIT_BRICK_TO_FITT(outset,kk,dof_B) ;
           kk++;
       }
     }
   }

   /*----- create space to store results before dataset-izing them -----*/

   MAKE_VECTIM(vimout,nmask_hits,nvout) ; vimout->ignore = 0 ;

   /**********==========---------- process data ----------==========**********/

   vstep = (nmask_hits > 666) ? nmask_hits/50 : 0 ;
   if( vstep > 0 ) fprintf(stderr,"++ t-testing:") ;

   for( kout=ivox=0 ; ivox < nvox ; ivox++ ){

     if( mask != NULL && mask[ivox] == 0 ) continue ;  /* don't process */

     vimout->ivec[kout] = ivox ;  /* table of what voxels are in vimout */

     if( vstep > 0 && kout%vstep==vstep/2 ) vstep_print() ;

                  datAAA = VECTIM_PTR(vectim_AAA,kout) ;  /* data arrays */
     if( twosam ) datBBB = VECTIM_PTR(vectim_BBB,kout) ;

     resar = VECTIM_PTR(vimout,kout) ;                    /* results array */

     /* skip processing for input voxels whose data is constant */

     for( ii=1 ; ii < nval_AAA && datAAA[ii] == datAAA[0] ; ii++ ) ; /*nada*/
     dconst = (ii == nval_AAA) ;
     if( twosam && !dconst ){
       for( ii=1 ; ii < nval_BBB && datBBB[ii] == datBBB[0] ; ii++ ) ; /*nada*/
       dconst = (ii == nval_BBB) ;
     }
     if( dconst ){
       /** if( debug ) INFO_message("skip output voxel#%d for constancy",kout) ; **/
       memset( resar , 0 , sizeof(float)*nvout ) ; nconst++ ; kout++ ; continue ;
     }

     if( mcov == 0 ){  /*--- no covariates ==> standard t-tests ---*/
       float *zAAA=datAAA, *zBBB=datBBB ; int nAAA=nval_AAA, nBBB=nval_BBB, nz,qq ;

       if( do_zskip ){  /* 06 Oct 2010: skip zero values? */
         for( ii=nz=0 ; ii < nval_AAA ; ii++ ) nz += (datAAA[ii] == 0.0f) ;
         if( nz > 0 ){            /* copy nonzero vals to a new array */
           nAAA = nval_AAA - nz ;
           if( nAAA < zskip_AAA ){
             memset( resar , 0 , sizeof(float)*nvout ) ; kout++ ; nzskip++ ; continue ;
           }
           zAAA = (float *)malloc(sizeof(float)*nAAA) ;
           for( ii=qq=0 ; ii < nval_AAA ; ii++ )
             if( datAAA[ii] != 0.0f ) zAAA[qq++] = datAAA[ii] ;
         }
         if( twosam ){
           for( ii=nz=0 ; ii < nval_BBB ; ii++ ) nz += (datBBB[ii] == 0.0f) ;
           if( nz > 0 ){            /* copy nonzero vals to a new array */
             nBBB = nval_BBB - nz ;
             if( nBBB < zskip_BBB ){
               if( zAAA != datAAA && zAAA != NULL ) free(zAAA) ;
               memset( resar , 0 , sizeof(float)*nvout ) ; kout++ ; nzskip++ ; continue ;
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
           tpair = ttest_toz( nAAA,zAAA , 0 ,NULL   , 0 ) ;
           resar[2] = tpair.a ; resar[3] = tpair.b ;
           tpair = ttest_toz( nBBB,zBBB , 0 ,NULL   , 0 ) ;
           resar[4] = tpair.a ; resar[5] = tpair.b ;
         }
#ifdef ALLOW_RANK
         if( do_ranks ) rank_order_2floats( nAAA,zAAA , nBBB,zBBB ) ;
#endif
         tpair = ttest_toz( nAAA,zAAA , nBBB,zBBB , ttest_opcode ) ;
         resar[0] = tpair.a ; resar[1] = tpair.b ;
       } else {
         tpair = ttest_toz( nAAA,zAAA , 0 ,NULL   , ttest_opcode ) ;
         resar[0] = tpair.a ; resar[1] = tpair.b ;
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
       regress_toz( nval_AAA , datAAA , nval_BBB , datBBB , ttest_opcode ,
                    mcov ,
                    Axx , Axx_psinv , Axx_xtxinv ,
                    Bxx , Bxx_psinv , Bxx_xtxinv , resar , workspace ) ;
     }

     if( BminusA && ntwosam ){  /* 05 Nov 2010 */
       for( ii=0 ; ii < ntwosam ; ii++ ) resar[ii] = -resar[ii] ;
     }

     kout++ ;
   }  /* end of loop over voxels */

   if( vstep > 0 ){ fprintf(stderr,"!\n") ; MEMORY_CHECK ; }

   if( nconst > 0 )
     INFO_message("skipped %d voxel%s completely for having constant data" ,
                  nconst , (nconst==1) ? "\0" : "s" ) ;

   if( nzred > 0 )
     INFO_message("-zskip: %d voxel%s had some values skipped in their t-tests",
                  nzred , (nzred==1) ? "\0" : "s" ) ;

   if( nzskip > 0 )
     INFO_message("-zskip: skipped %d voxel%s completely for having too few nonzero values" ,
                  nzskip , (nzskip==1) ? "\0" : "s" ) ;

   /*-------- get rid of the input data now --------*/

   if( workspace != NULL ) free(workspace) ;

                            VECTIM_destroy(vectim_AAA) ;
   if( vectim_BBB != NULL ) VECTIM_destroy(vectim_BBB) ;

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

   /*---------- fill in the output dataset with some numbers ----------*/

   INFO_message("saving results") ;

   for( kk=0 ; kk < nvout ; kk++ )
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   THD_vectim_to_dset( vimout , outset ) ;
   VECTIM_destroy( vimout ) ;

   mri_fdr_setmask(mask) ;
   kk = THD_create_all_fdrcurves(outset) ;
   if( kk > 0 )
     ININFO_message("Added %d FDR curve%s to dataset",kk,(kk==1)?"\0":"s");
   else
     WARNING_message("Failed to add FDR curves to dataset?!") ;

   if( twosam ) 
     ININFO_message("%s test: results are %s - %s",
                    ttest_opcode == 2 ? "paired":"2-sample", snam_PPP,snam_MMM) ;

   DSET_write(outset) ; WROTE_DSET(outset) ;

   exit(0) ;

} /* end of main program */

/*---------------------------------------------------------------------------*/

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
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nA ; jj++ ) val += PA(ii,jj)*zA[jj] ;
       betA[ii] = val ;
     }
     for( jj=0 ; jj < nA ; jj++ ){
       val = -zA[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XA(jj,ii)*betA[ii] ;
       zdifA[jj] = val ; ssqA += val*val ;
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
       zdifB[jj] = val ; ssqB += val*val ;
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

/*----------------------------------------------------------------------------*/
/*! Various sorts of t-tests; output = Z-score.
   - numx = number of points in the first sample (must be > 1)
   - xar  = array with first sample
   - numy = number of points in the second sample
             - numy = 0 ==> a 1 sample test of first sample against mean=0
  DISABLED   - numy = 1 ==> a 1 sample test of first sample against mean=yar[0]
             - numy > 1 ==> a 2 sample test; opcode determines what kind
   - opcode = 0 for unpaired test with pooled variance
   - opcode = 1 for unpaired test with unpooled variance
   - opcode = 2 for paired test (numx == numy is required)
   - The return value is the Z-score of the t-statistic.
*//*--------------------------------------------------------------------------*/

float_pair ttest_toz( int numx, float *xar, int numy, float *yar, int opcode )
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

   } else {  /* Case 3: 2 sample test (pooled or unpooled) */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii] - avx ; sdx += val*val ; }

     avy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
     avy /= numy ; sdy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ){ val = yar[ii] - avy ; sdy += val*val ; }

     delta = avx - avy ; /* difference in means */

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

   result.a = delta ;
   result.b = (toz) ? (float)GIC_student_t2z( (double)tstat , (double)dof )
                    : TCLIP(tstat) ;
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
#define ZMAX 13.0

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

void TT_centerize(void)
{
   int jj,kk ; float sum ;
   int nvv , iv ; float *vv ;

   if( center_code == CENTER_NONE ) return ;

   nvv = nval_AAA + nval_BBB ;
   vv  = (float *)malloc(sizeof(float)*nvv) ;

ENTRY("TT_centerize") ;

   /*-- process each matrix separately --*/

   if( center_code != CENTER_SAME || Bxx == NULL || Bxx == Axx ){

     for( jj=1 ; jj <= mcov ; jj++ ){
       for( kk=0 ; kk < nval_AAA ; kk++ ) vv[kk] = AXX(kk,jj) ;
       if( center_meth == CMETH_MEDIAN ) sum = qmed_float (nval_AAA,vv) ;
       else                              sum = qmean_float(nval_AAA,vv) ;
       for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,jj) -= sum ;
     }

     if( Bxx != NULL & Bxx != Axx ){
       for( jj=1 ; jj <= mcov ; jj++ ){
         for( kk=0 ; kk < nval_BBB ; kk++ ) vv[kk] = BXX(kk,jj) ;
         if( center_meth == CMETH_MEDIAN ) sum = qmed_float (nval_BBB,vv) ;
         else                              sum = qmean_float(nval_BBB,vv) ;
         for( kk=0 ; kk < nval_BBB ; kk++ ) BXX(kk,jj) -= sum ;
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

void TT_matrix_setup( int kout )
{
   int jj,kk ; float sum , *fpt ;
   static MRI_IMARR *imprA=NULL , *imprB=NULL ;
   char label[32] ;

ENTRY("TT_matrix_setup") ;

   /*-- load setA matrix --*/

   for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,0) = 1.0f ; /* the mean */
   for( jj=1 ; jj <= mcov ; jj++ ){
     if( covvec_AAA[jj-1] != NULL ) fpt = covvec_AAA[jj-1]->ar ;
     else                           fpt = VECTIM_PTR(covvim_AAA[jj-1],kout) ;
     for( kk=0 ; kk < nval_AAA ; kk++ ) AXX(kk,jj) = fpt[kk] ;
   }

   /*-- load setB matrix --*/

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

   imprA = mri_matrix_psinv_pair( Axxim , 0.0f ) ;
   if( imprA == NULL ) ERROR_exit("Can't invert setA covariate matrix?! :-(") ;
   Axx_psinv  = MRI_FLOAT_PTR(IMARR_SUBIM(imprA,0)) ;
   Axx_xtxinv = MRI_FLOAT_PTR(IMARR_SUBIM(imprA,1)) ;

   if( debug ){
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
