#include "mrilib.h"

/*------------------------------- prototypes, etc. ---------------------------*/

#undef  MAXCOV
#define MAXCOV 31

#undef  MAX_LABEL_SIZE
#define MAX_LABEL_SIZE 12

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

static NI_element         *covnel=NULL ;       /* covariates */
static NI_str_array       *covlab=NULL ;

static int             num_covset_col=0;
static THD_3dim_dataset ***covset_AAA=NULL ;
static THD_3dim_dataset ***covset_BBB=NULL ;

static int mcov = 0 ;
static int nout = 0 ;
static float *axx , *axx_psinv , *axx_xtxinv ;
static float *bxx , *bxx_psinv , *bxx_xtxinv ;

static char *prefix = "TTnew" ;
static byte *mask   = NULL ;
static int  nmask   = 0 ;
static int  nvox    = 0 ;
static int  nmask_hits = 0 ;

static int ttest_opcode = 0 ;  /* 0=pooled, 1=unpooled, 2=paired */

static int               ndset_AAA=0 ;
static char              *snam_AAA=NULL ;
static char             **name_AAA=NULL ;
static char             **labl_AAA=NULL ;
static THD_3dim_dataset **dset_AAA=NULL ;

static int               ndset_BBB=0 ;
static char              *snam_BBB=NULL ;
static char             **name_BBB=NULL ;
static char             **labl_BBB=NULL ;
static THD_3dim_dataset **dset_BBB=NULL ;

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

int main( int argc , char *argv[] )
{
   int nopt ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Gosset (Student) t-test of sets of 3D datasets.\n"
      "\n"
      "Usage can be similar to the old 3dttest; for example:\n"
      "\n"
      "  3dttest_new -set2 a+tlrc'[3]' b+tlrc'[3]' ...\n"
      "\n"
      "*OR* usage can be similar to 3dMEMA; for example:\n"
      "\n"
      "  3dttest_new -set2 Green sub001 a+tlrc'[3]' \\\n"
      "                          sub002 b+tlrc'[3]' \\\n"
      "                          sub003 c+tlrc'[3]' \\\n"
      "                          ...                \\\n"
      "              -covariates Cfile\n"
      "\n"
      "The 3dMEMA-like (more complicated) syntax is required to use covariates.\n"
      "\n"
      "You can input 1 or 2 sets of data.  With 1 set, the mean across\n"
      "input datasets (usually subjects) is tested against 0 (by default).\n"
      "With 2 sets, the difference in means across each set is tested\n"
      "against 0.  The 1 sample results for each set are also provided, since\n"
      "these are often of interest to the investigator (e.g., YOU).\n"
      "\n"
      "Covariates can be per-dataset (input=1 number) or per-voxel/per-dataset\n"
      "(input=1 dataset).  Note that the second option will slow the program\n"
      "down considerably.\n"
      "\n"
      "This program is meant (for most uses) to replace the original 3dttest,\n"
      "which was written in 1994 \"when grass was green and grain was yellow\".\n"
      "\n"
      "------------\n"
      "SET OPTIONS:\n"
      "------------\n"
      "* At least the '-set2' option must be given.  '-set1' is optional, and\n"
      "   if it isn't used, then the mean of the dataset values from '-set2' is\n"
      "   t-tested against 0.\n"
      "* Two forms for the '-setx' (x=1 or 2) options are allowed.  The first\n"
      "   form is similar to the original 3dttest program, where the option\n"
      "   is just followed by a list of datasets to use.\n"
      "* The second form is similar to the 3dMEMA program, where you specify\n"
      "   a label for each input dataset sub-brick (a difference between this\n"
      "   option and the version in 3dMEMA is only that you do not give a second\n"
      "   dataset ('T_DSET') with each sample in this program).\n"
      "* The second, more complicated, form is needed if you want to use the\n"
      "   '-covariates' option, since it is the dataset 'SAMP_K' labels that\n"
      "   are used to extract the covariates for each input dataset.\n"
      "\n"
      " -set1 BETA_DSET BETA_DSET ...\n"
      "  *OR*\n"
      " -set2\n"
      "   In this form of input, you specify the datasets for each set\n"
      "    directly following the '-setx' option ('x' is '1' or '2').\n"
      "   ++ Unlike 3dttest, you can specify multiple sub-bricks in a dataset:\n"
      "        a+tlrc'[1..13(2)]'\n"
      "      which inputs 7 sub-bricks at once (1,3,5,7,9,11,13).\n"
      "\n"
      "** ALTERNATIVELY **\n"
      "\n"
      " -set1 SETNAME            \\\n"
      "  *OR*   SAMP_1 BETA_DSET \\\n"
      " -set2   SAMP_2 BETA_DSET \\\n"
      "         ...    ...       \\\n"
      "         SAMP_N BETA_DSET\n"
      "   Specify the data for one of the test variables.\n"
      "   SETNAME is the name assigned to the set (used in the output labels).\n"
      "   SAMP_K  is the label for the sample K whose input dataset follows.\n"
      "   DSET    is the name of the dataset of the beta coefficient or GLT.\n"
      "           ++ only 1 sub-brick can be specified here.\n"
      "   Note that the labels 'SETNAME' and 'SAMP_K' are limited to 12\n"
      "   characters -- any more will be thrown away without warning.\n"
      "\n"
      "--------------\n"
      "OTHER OPTIONS:\n"
      "--------------\n"
      " -covariates COVAR_FILE\n"
      "* Specify the name of a text file containing a table for the covariate(s).\n"
      "   Each column in the file is treated as a separate covariate, and each\n"
      "   row contains the values of these covariates for each sample. Note that\n"
      "   you can use '-covariates' only once -- the COVAR_FILE should contain\n"
      "   the covariates for all input samples from both sets.\n"
      "* The format of COVAR_FILE is like that for 3dMEMA and for 3dGroupInCorr:\n"
      "     FIRST LINE -->   subject IQ   age  GMfrac\n"
      "     LATER LINES -->  Elvis   143   42  Elvis_GM+tlrc'[8]'\n"
      "                      Fred     85   59  Fred_GM+tlrc'[8]'\n"
      "                      Ethel   109   49  Ethel_GM+tlrc'[8]'\n"
      "                      Lucy    133   32  Lucy_GM+tlrc'[8]'\n"
      "  ++ The first column contains the labels that must match the dataset\n"
      "      SAMP_K labels given in the '-set' option(s).\n"
      "  ++ The later columns contain numbers (as in IQ and age, above), or\n"
      "      dataset names.  In the latter case, you are specifying a voxel-wise\n"
      "      covariate (e.g., GMfrac).\n"
      "  ++ The first line contains column headers.  The header label for the\n"
      "      first column isn't used for anything.  The later header labels are\n"
      "      used in the sub-brick labels sent to AFNI.\n"
      "  ++ At this time, only the -paired and -pooled options can be used with\n"
      "      covariates.  If you use -unpooled, it will be changed to -pooled.\n"
      "      -unpooled still works with a pure t-test (no -covariates option).\n"
      "  ++ If you use -paired, then the covariates for -setB will be the same\n"
      "      as those for -setA, even if the dataset labels are different!\n"
      "  ++ Each covariate column in the regression matrix will have its mean\n"
      "      removed (centered). If there are 2 sets of subjects, each set's\n"
      "      matrix will be centered separately.\n"
      "  ++ For each covariate, 2 sub-bricks are produced:\n"
      "      -- The estimated slope of the beta values vs covariate\n"
      "      -- The t-statistic of this slope\n"
      "  ++ If there are 2 sets of subjects, then each pair of sub-bricks is\n"
      "      produced for the setA-setB, setA, and setB cases, so that you'll\n"
      "      get 6 sub-bricks per covariate (plus 6 more for the mean, which\n"
      "      is treated as a special covariate whose values are all 1).\n"
      "  ++ A maximum of 31 covariates are allowed.  If you have more, then\n"
      "      seriously consider the possibility that you are completely demented.\n"
      "\n"
      " -paired   = Specifies the use of a paired-sample t-test to\n"
      "              compare set1 and set2.  If this option is used,\n"
      "              set1 and set2 must have the same cardinality (duh).\n"
      "\n"
      " -unpooled = Specifies that the variance estimates for set1 and\n"
      "              set2 be computed separately (not pooled together).\n"
      "              This only makes sense if -paired is NOT given.\n"
      "\n"
      " -mask mmm = Only compute results for voxels in the specified mask.\n"
      "\n"
      " -prefix p = Gives the name of the output dataset file.\n"
    ) ;
   }

   /*--- read the options from the command line ---*/

   nopt = 1 ;
   while( nopt < argc ){

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
       INFO_message("%d voxels in -mask dataset",nmask_hits) ;
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

     /*----- the various flavours of '-set' -----*/

     if( strncmp(argv[nopt],"-set",4) == 0 ){
       char cc=argv[nopt][4] , *onam=argv[nopt] ;
       int nds=0 , ids ; char **nams=NULL , **labs=NULL , *snam=NULL ;
       THD_3dim_dataset *qset , **dset=NULL ;

       if( cc == '1' ){
         if( ndset_AAA > 0 ) ERROR_exit("Can't use '-set1' twice!") ;
       } else if( cc == '2' ){
         if( ndset_BBB > 0 ) ERROR_exit("Can't use '-set2' twice!") ;
       } else {
         ERROR_exit("'%s' is not a recognized '-set' option",argv[nopt]);
       }
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;

       /* if next arg is a dataset, then all of the next args are datasets */

       qset = THD_open_one_dataset( argv[nopt] ) ;
       if( ISVALID_DSET(qset) ){
         nds  = 1 ;
         nams = (char **)malloc(sizeof(char *)) ;
         labs = (char **)malloc(sizeof(char *)) ;
         dset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)) ;
         nams[0] = strdup(argv[nopt]) ; dset[0] = qset ;
         labs[0] = strdup(THD_trailname(argv[nopt],0)) ; LTRUNC(labs[0]) ;
         snam = (char *)malloc(sizeof(char)*MAX_LABEL_SIZE) ;
         sprintf(snam,"Set%c",cc) ;
         nopt++ ;
         for( nopt++ ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){
           qset = THD_open_one_dataset( argv[nopt] ) ;
           if( !ISVALID_DSET(qset) )
             ERROR_exit("Option %s: can't open dataset '%s'",onam,argv[nopt]) ;
           nds++ ;
           nams = (char **)realloc(nams,sizeof(char *)*nds) ;
           labs = (char **)realloc(labs,sizeof(char *)*nds) ;
           dset = (char **)realloc(dset,sizeof(THD_3dim_dataset *)*nds) ;
           nams[nds-1] = strdup(argv[nopt]) ; dset[nds-1] = qset ;
           labs[nds-1] = strdup(THD_trailname(argv[nopt],0)) ; LTRUNC(labs[nds-1]) ;
         }

       } else {  /* not a dataset => label label dset label dset ... */

         snam = strdup(argv[nopt]) ; LTRUNC(snam) ;
         for( nopt++ ; nopt < argc && argv[nopt][0] != '-' ; nopt+=2 ){
           if( nopt+1 >= argc )
             ERROR_exit("Option %s: ends prematurely",onam) ;
           qset = THD_open_one_dataset( argv[nopt+1] ) ;
           if( !ISVALID_DSET(qset) )
             ERROR_exit("Option %s: can't open dataset '%s'",onam,argv[nopt]) ;
           nds++ ;
           nams = (char **)realloc(nams,sizeof(char *)*nds) ;
           labs = (char **)realloc(labs,sizeof(char *)*nds) ;
           dset = (char **)realloc(dset,sizeof(THD_3dim_dataset *)*nds) ;
           nams[nds-1] = strdup(argv[nopt+1]) ; dset[nds-1] = qset ;
			  labs[nds-1] = strdup(argv[nopt]  ) ; LTRUNC(labs[nds-1]) ;
         }

       }
       if( nds == 1 )
         ERROR_exit("Option %s: need at least 2 datasets",onam) ;
       for( ids=1 ; ids < nds ; ids++ ){
         if( DSET_NVOX(dset[ids]) != DSET_NVOX(dset[0]) )
           ERROR_exit("Option %s: dataset '%s' does match others in size",
                      onam,nam[nds-1]) ;
       }

       /* assign results to global variables */

       if( cc == '1' ){
         ndset_AAA = nds  ; snam_AAA = snam ;
          name_AAA = nams ; labl_AAA = labs ; dset_AAA = dset ;
       } else {
         ndset_BBB = nds  ; snam_BBB = snam ;
          name_BBB = nams ; labl_BBB = labs ; dset_BBB = dset ;
       }

       continue ;  /* nopt already points to next option */

     } /*----- end of '-set' -----*/

     /*----- covariates -----*/

     if( strcasecmp(argv[nopt],"-covariates") == 0 ){  /* 20 May 2010 */
       char *lab ; float sig ; int nbad ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       if( covnel != NULL ) ERROR_exit("can't use -covariates twice!") ;
       covnel = THD_mixed_table_read( argv[nopt] ) ;
       if( covnel == NULL )
         ERROR_exit("Can't read table from -covariates file '%s'",argv[nopt]) ;
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
       for( nbad=0,kk=1 ; kk <= mcov ; kk++ ){
         if( covnel->vec_typ[kk] == NI_FLOAT ){  /* numeric column */
           meansigma_float(covnel->vec_len,(float *)covnel->vec[kk],NULL,&sig) ;
           if( sig <= 0.0f ){
             ERROR_message("Covariate '%s' is constant; how can this be used?!" ,
                           covlab->str[kk] ) ; nbad++ ;
           }
         } else {                                /* string column */
           num_covset_col++ ;
         }
         LTRUNC(covlab->str[kk]) ;
       }
       if( nbad > 0 ) ERROR_exit("Cannot continue past above ERROR%s :-(",
                                  (nbad==1) ? "\0" : "s" ) ;
       nopt++ ; continue ;
     }

     /*----- bad user, bad bad bad -----*/

     ERROR_exit("3dttest_new: don't recognize option '%s'",argv[nopt]) ;

   }  /*-------------------- end of option parsing --------------------*/

   /*----- check some stuff -----*/

   if( ttest_opcode == 1 && mcov > 0 ){
     WARNING_message("-covariates does not support unpooled variance (yet)") ;
     ttest_opcode = 0 ;
   }

   if( ndset_BBB <= 0 )
     ERROR_exit("No '-set2' option?  Please please read the instructions!") ;

   if( ndset_AAA != ndset_BBB && ttest_opcode == 2 )
     ERROR_exit("Can't do '-paired' with unequal set sizes: #1=%d #2=%d",
                ndset_AAA , ndset_BBB ) ;

   nvox = DSET_NVOX(dset_BBB[0]) ;
   if( ndset_AAA > 0 && DSET_NVOX(dset_AAA[0]) != nvox )
     ERROR_exit("-set1 and -set2 datasets don't match number of voxels") ;

   if( nmask > 0 && nmask != nvox )
     ERROR_exit("-mask doesn't match datasets number of voxels") ;

   if( ndset_BBB - mcov < 2 ||
       ( ndset_AAA > 0 && (ndset_AAA - mcov < 2) ) )
     ERROR_exit("Too many covariates compared to number of datasets") ;

   /*--- read in covariate datasets, if any ---*/

   if( num_covset_col > 0 ){
     THD_3dim_dataset *qset ;

     if( ndset_BBB > 0 ){
       covset_BBB = (THD_3dim_dataset ***)malloc(sizeof(THD_3dim_dataset **)*ndset_BBB) ;
       for( kk=0 ; kk < ndset_BBB ; kk++ ){
         covset_BBB[kk] = (THD_3dim_dataset **)calloc(sizeof(THD_3dim_dataset *),mcov) ;
         ii = string_search( labl_BBB[kk] ,
                             covnel->vec_len , (char **)covnel->vec[0] ) ;
         if( ii < 0 ){
           ERROR_message("Can't find dataset label '%s' in covariates file" ,
                         labl_BBB[kk] ) ; nbad++ ;
         } else {
           for( jj=1 ; jj <= mcov ; jj++ ){
             if( covnel->vec_typ[jj] == NI_STRING ){
               char **qpt = (char **)covnel->vec[jj] ;
               qset = THD_open_one_dataset(qpt[ii]) ;
               if( qset == NULL ){
                 ERROR_message("Can't find dataset '%s' from covariates file" ,
                               qpt[ii] ) ; nbad++ ;
               } else {
                 covset_BBB[kk][jj-1] = qset ;
               }
             }
           }
         }
       }
     }

   } /* end of loading covariates datasets */


     /*--- setup the setA regression matrix ---*/

     nA    = shd_AAA->ndset ;
     axxim = mri_new( nA , mcov+1 , MRI_float ) ;
     axx   = MRI_FLOAT_PTR(axxim) ;
     for( kk=0 ; kk < shd_AAA->ndset ; kk++ ){  /* loop over datasets */
       ii = string_search( shd_AAA->dslab[kk] , /* find which covariate */
                           covnel->vec_len , (char **)covnel->vec[0] ) ;
       if( ii < 0 ){
         ERROR_message("Can't find dataset label '%s' in covariates file" ,
                       shd_AAA->dslab[kk] ) ;
         nbad++ ;
       } else {             /* ii-th row of covariates == kk-th dataset */
         AXX(kk,0) = 1.0f ;
         for( jj=1 ; jj <= mcov ; jj++ )
           AXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
       }
     }
     if( nbad == 0 ){  /* process the matrix */
       MRI_IMARR *impr ; float sum ;
       for( jj=1 ; jj <= mcov ; jj++ ){  /* demean the columns */
         for( sum=0.0f,kk=0 ; kk < shd_AAA->ndset ; kk++ ) sum += AXX(kk,jj) ;
         sum /= shd_AAA->ndset ;
         for( kk=0 ; kk < shd_AAA->ndset ; kk++ ) AXX(kk,jj) -= sum ;
       }
       /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
       impr = mri_matrix_psinv_pair( axxim , 0.0f ) ;
       if( impr == NULL ) ERROR_exit("Can't process setA covariate matrix?! :-(") ;
       axxim_psinv  = IMARR_SUBIM(impr,0) ; axx_psinv  = MRI_FLOAT_PTR(axxim_psinv ) ;
       axxim_xtxinv = IMARR_SUBIM(impr,1) ; axx_xtxinv = MRI_FLOAT_PTR(axxim_xtxinv) ;
     }

     /*--- setup the setB regression matrix ---*/

     if( shd_BBB != NULL && ttest_opcode != 2 ){  /* un-paired case */
       nB    = shd_BBB->ndset ;
       bxxim = mri_new( nB , mcov+1 , MRI_float ) ;
       bxx   = MRI_FLOAT_PTR(bxxim) ;
       for( kk=0 ; kk < shd_BBB->ndset ; kk++ ){  /* loop over datasets */
         ii = string_search( shd_BBB->dslab[kk] , /* find which covariate */
                             covnel->vec_len , (char **)covnel->vec[0] ) ;
         if( ii < 0 ){
           ERROR_message("Can't find dataset label '%s' in covariates file" ,
                         shd_BBB->dslab[kk] ) ;
           nbad++ ;
         } else {             /* ii-th row of covariates == kk-th dataset */
           BXX(kk,0) = 1.0f ;
           for( jj=1 ; jj <= mcov ; jj++ )
             BXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
         }
       }
       if( nbad == 0 ){  /* process the matrix */
         MRI_IMARR *impr ; float sum ;
         for( jj=1 ; jj <= mcov ; jj++ ){  /* demean the columns */
           for( sum=0.0f,kk=0 ; kk < shd_BBB->ndset ; kk++ ) sum += BXX(kk,jj) ;
           sum /= shd_BBB->ndset ;
           for( kk=0 ; kk < shd_BBB->ndset ; kk++ ) BXX(kk,jj) -= sum ;
         }
         /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
         impr = mri_matrix_psinv_pair( bxxim , 0.0f ) ;
         if( impr == NULL ) ERROR_exit("Can't process setB covariate matrix?! :-(") ;
         bxxim_psinv  = IMARR_SUBIM(impr,0) ; bxx_psinv  = MRI_FLOAT_PTR(bxxim_psinv ) ;
         bxxim_xtxinv = IMARR_SUBIM(impr,1) ; bxx_xtxinv = MRI_FLOAT_PTR(bxxim_xtxinv) ;
       }

     } else if( shd_BBB != NULL && ttest_opcode == 2 ){  /* paired case */

       bxx = axx ; bxx_psinv = axx_psinv ; bxx_xtxinv = axx_xtxinv ;

     }

     if( nbad )
       ERROR_exit("Can't continue past the above covariates errors :-((") ;

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

/*---------------------------------------------------------------------------*/
/*  opcode defines what to do for 2-sample tests:
      0 ==> unpaired, pooled variance
      1 ==> unpaired, unpooled variance (not yet implemented)
      2 ==> paired (numA==numB required)

    xA      = numA X (mcov+1) matrix -- in column-major order
    psinvA  = (mcov+1) X numA matrix -- in column-major order
    xtxinvA = (mcov+1) X (mcov+1) matrix = inv[xA'xA]
*//*-------------------------------------------------------------------------*/

#if defined(COVTEST) && 0
static int first=1 ;
#endif

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
   register float val ; register int ii,jj,tt ;

   MRI_IMAGE *bxxim_psinv=NULL , *bxxim_xtxinv=NULL ;

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
     MRI_IMAGE *axxim_psinv=NULL , *axxim_xtxinv=NULL ;
     if( psinvA == NULL || xtxinvA ){
       MRI_IMARR *impr ; MRI_IMAGE *axxim ;
       axxim = mri_new_vol_empty( nA , mcov+1 , 1 , MRI_float ) ;
       mri_fix_data_pointer(xA,axxim) ;
       impr = mri_matrix_psinv_pair(axxim,0.0f) ;
       mri_clear_data_pointer(axxim) ; mri_free(axxim) ;
       if( impr == NULL ){ ERROR_message("psinv setA matrix fails"); return; }
       axxim_psinv  = IMARR_SUBIM(impr,0); psinvA  = MRI_FLOAT_PTR(axxim_psinv );
       axxim_xtxinv = IMARR_SUBIM(impr,1); xtxinvA = MRI_FLOAT_PTR(axxim_xtxinv);
       FREE_IMARR(impr) ;
     }
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nA ; jj++ ) val += PA(ii,jj)*zA[jj] ;
       betA[ii] = val ;
     }
     for( jj=0 ; jj < nA ; jj++ ){
       val = -zA[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XA(jj,ii)*betA[ii] ;
       zdifA[ii] = val ; ssqA += val*val ;
     }
     if( testA ){ varA = ssqA / (nA-mm) ; if( varA <= 0.0f ) varA = VBIG ; }
     mri_free(axxim_psinv) ; mri_free(axxim_xtxinv) ; /* if they're not NULL */
   }

   /*-- compute estimates for B parameters --*/

   if( testB || testAB ){
     MRI_IMAGE *bxxim_psinv=NULL , *bxxim_xtxinv=NULL ;
     if( psinvB == NULL || xtxinvB == NULL ){
       MRI_IMARR *impr ; MRI_IMAGE *bxxim ;
       bxxim = mri_new_vol_empty( nB , mcov+1 , 1 , MRI_float ) ;
       mri_fix_data_pointer(xB,bxxim) ;
       impr = mri_matrix_psinv_pair(bxxim,0.0f) ;
       mri_clear_data_pointer(bxxim) ; mri_free(bxxim) ;
       if( impr == NULL ){ ERROR_message("psinv setB matrix fails"); return; }
       bxxim_psinv  = IMARR_SUBIM(impr,0); psinvB  = MRI_FLOAT_PTR(bxxim_psinv );
       bxxim_xtxinv = IMARR_SUBIM(impr,1); xtxinvB = MRI_FLOAT_PTR(bxxim_xtxinv);
       FREE_IMARR(impr) ;
     }
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nB ; jj++ ) val += PB(ii,jj)*zB[jj] ;
       betB[ii] = val ;
     }
     for( jj=0 ; jj < nB ; jj++ ){
       val = -zB[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XB(jj,ii)*betB[ii] ;
       zdifB[ii] = val ; ssqB += val*val ;
     }
     if( testB ){ varB = ssqB / (nB-mm) ; if( varB <= 0.0f ) varB = VBIG ; }
     mri_free(bxxim_psinv) ; mri_free(bxxim_xtxinv) ;
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
         val          = outvec[kt-1] / sqrtf( varAB*xtxA(tt) ) ;
         outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
       }

     } else {            /* unpaired, pooled variance */

       varAB = (ssqA+ssqB)/(nA+nB-2*mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA + nB - 2*mm ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         val          = outvec[kt-1] / sqrtf( varAB*(xtxA(tt)+xtxB(tt)) );
         outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
       }
     } /* end of unpaired pooled variance */
   }

   /*-- carry out 1-sample A tests, if any --*/

   if( testA ){
#if defined(COVTEST) && 0
#pragma omp critical
     { if( first ){
         first = 0 ;
         fprintf(stderr,"testA varA=%g xtxA=",varA) ;
         for( tt=0 ; tt < mm ; tt++ ) fprintf(stderr," %g",xtxA(tt)) ;
         fprintf(stderr,"\n") ;
       }
     }
#endif
     dof = nA - mm ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testA & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betA[tt] ;
       val          = betA[tt] / sqrtf( varA * xtxA(tt) ) ;
       outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
     }
   }

   /*-- carry out 1-sample B tests, if any --*/

   if( testB ){
     dof = nB - mm ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testB & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betB[tt] ;
       val          = betB[tt] / sqrtf( varB * xtxB(tt) ) ;
       outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
     }
   }


   return ;
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
   int paired=(opcode==2) , pooled=(opcode==0) ;

#if 0
   /* check inputs for stoopidities or other things that need to be changed */

   if( numx < 2 || xar == NULL                 ) return result ; /* bad */
   if( paired && (numy != numx || yar == NULL) ) return result ; /* bad */
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
   result.b = (float)GIC_student_t2z( (double)tstat , (double)dof ) ;
   return result ;
}
