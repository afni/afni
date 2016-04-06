/* 
   P. Taylor, March 2012
	
	This program estimates uncertainty of relevant DTI parameters using
	jackknife resampling 

	Sept. 2012: fixed some memory stuff.

   Jan. 2014: 
       + counter stuff, nicer output for user
       + cap MAXBAD at 8; only affects cases of >67 grads

   Feb. 2014:
       + getting rid of more order dependence in the commandline opts

   June. 2014:
       + internal testing/plotting options.

   May, 2015:
       + introduce CALC_THR_FA: option to only calc uncert of high-FA
         vox in order to save mucho computational time (well, hopefully)
       + debug: several iarg++ probs, affected order of calling things

   Apr, 2016:
       + use AFNI style bmatrix for inputting now...

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <time.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_linalg.h>
#include <debugtrace.h>
#include <mrilib.h>     
#include <3ddata.h>     
#include <DoTrackit.h>
#include "editvol.h"
#include "thd.h"
#include "suma_suma.h"

#define N_dti_scal (3) // @@@
#define N_dti_scal_tot (6) // 1+2+3, matches with N_dti_scal
#define N_dti_vect (3) 
#define N_plus_dtifile (4) // number of allowed additional param files in NIMLver


/*struct data {
	size_t n;
	float *tt;
	float *y;
	float *sigma;
};

// nonlinear fitting progs for DT estimation, with implementation
// adapted from GNU Scientific Library Reference Manual
// Edition 1.15, for GSL Version 1.15, (Galassi et al. 2011) 
void LevMarq( float *xS, float *xB, int m, float *A, float *sigma);
int expb_f (const gsl_vector *x, void *data, gsl_vector *f);
int expb_df (const gsl_vector *x, void *data,  gsl_matrix *J);
int expb_fdf(const gsl_vector *x, void *data, gsl_vector *f, gsl_matrix *J);
*/
// from Numerical Recipes by Press, Teukolsky, Vetterling and Flannery
void piksr2(int n, unsigned int arr[], int brr[]); 
int ConfIntSort(float *A, int N, float *vals, int CI_per);

void usage_DWUncert(int detail) 
{
	printf(
"\n"
"  Use jackknifing to estimate uncertainty of DTI parameters which are\n"
"    important for probabilistic tractography on per voxel basis.\n"
"\n"
"  Produces useful input for 3dTrackID, which does both mini- and full\n"
"    probabilistic tractography for GM ROIs in networks, part of \n"
"    FATCAT (Taylor & Saad, 2013) in AFNI.\n"
"\n"
"\n"
"  COMMAND: 3dDWUncert -inset FILE -input [base of FA/MD/etc.] \\\n"
"           {-grads | -bmatrix_Z} FILE -prefix NAME -iters NUMBER \n"
"\n\n"
"  + OUTPUT:\n"
"     1) AFNI-format file with 6 subbricks, containing uncertainty\n"
"        information. The bricks are in the following order:\n"
"            [0] bias of e1 in direction of e2\n"
"            [1] stdev of e1 in direction of e2\n"
"            [2] bias of e1 in direction of e3\n"
"            [3] stdev of e1 in direction of e3\n"
"            [4] bias of FA \n"
"            [5] stdev of FA\n"
"\n\n"
"  + RUNNING, need to provide:\n"
"    -inset  FILE     :file with b0 and DWI subbricks \n"
"                      (e.g., input to 3dDWtoDTI)\n"
"    -prefix PREFIX   :output file name part.\n"
"    -input  INPREF   :basename of DTI volumes output by,\n" 
"                      e.g., 3dDWItoDT or TORTOISE. Assumes format of name\n"
"                      is, e.g.:  INPREF_FA+orig.HEAD or INPREF_FA.nii.gz .\n"
"                      Files needed with same prefix are:\n"
"                      *FA*, *L1*, *V1*, *V2*, *V3* .\n"
"    -input_list FILE :an alternative way to specify DTI input files, where\n"
"                      FILE is a NIML-formatted text file that lists the\n"
"                      explicit/specific files for DTI input.  This option is\n"
"                      used in place of '-input INPREF'.\n"
"                      See below for a 'INPUT LIST FILE EXAMPLE'.\n"
"    -grads  FILE     :file with 3 columns for x-, y-, and z-comps\n"
"                      of DW-gradients (which have unit magnitude).\n"
"                      NB: this option also assumes that only 1st DWI\n"
"                      subbrick has a b=0 image (i.e., all averaging of\n"
"                      multiple b=0 images has been done already); if such\n"
"                      is not the case, then you should convert your grads to\n"
"                      the bmatrix format and use `-bmatrix_Z'.\n"
"  OR\n"
"    -bmatrix_Z  FILE :using this means that file with gradient info\n"
"                      is in b-matrix format, with 6 columns representing:\n"
"                      b_xx b_yy b_zz b_xy b_xz b_yz.\n"
"                      NB: here, bvalue per image is the trace of the bmatr,\n"
"                      bval = b_xx+b_yy+b_zz, such as 1000 s/mm^2. This\n"
"                      option might be used, for example, if multiple \n"
"                      b-values were used to measure DWI data; this is an\n"
"                      AFNI-style bmatrix that needs to be input.\n"
"    -bmatr  FILE     :**deprecated input option-- no longer used**: move\n"
"                      along, folks, nothin' to see here!\n"
"    -mask   MASK     :can include a mask within which to calculate uncert.\n"
"                      Otherwise, data should be masked already.\n"
"    -iters  NUMBER   :number of jackknife resample iterations,\n"
"                      e.g. 50.\n"
"    -calc_thr_FA  FF :set a threshold for the minimum FA value above which\n"
"                      one calculates uncertainty; useful if one doesn't want\n"
"                      to waste time calculating uncertainty in very low-FA\n"
"                      voxels that are likely GM/CSF.  For example, in adult\n"
"                      subjects one might set FF=0.1 or 0.15, depending on\n"
"                      SNR and user's whims (default: FF=-1, i.e., do all).\n"
"    -csf_fa NUMBER   :number marking FA value of `bad' voxels, such as \n"
"                      those with S0 value <=mean(S_i), which breaks DT\n"
"                      assumptions due to, e.g., bulk/flow motion.\n"
"                      Default value of this matches 3dDWItoDT value of\n"
"                      csf_fa=0.012345678.\n"
"\n\n"
"* * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * **\n"
"\n"
"+ DTI LIST FILE EXAMPLE:\n"
"     Consider, for example, if you hadn't used the '-sep_dsets' option when\n"
"     outputting all the tensor information from 3dDWItoDT.  Then one could\n"
"     specify the DTI inputs for this program with a file called, e.g., \n"
"     FILE_DTI_IN.niml.opts (the name *must* end with '.niml.opts'):\n"
"       <DTIFILE_opts    \n"
"         dti_V1=\"SINGLEDT+orig[9..11]\"\n"
"         dti_V2=\"SINGLEDT+orig[12..14]\"\n"
"         dti_V3=\"SINGLEDT+orig[15..17]\"\n"
"         dti_FA=\"SINGLEDT+orig[18]\"\n"
"         dti_L1=\"SINGLEDT+orig[6]\" />\n"
"     This represents the *minimum* set of input files needed when running\n"
"     3dDWUncert. (Note that MD isn't needed here.)  You can also recycle a\n"
"     NIMLly formatted file from '3dTrackID -dti_list'-- the extra inputs\n"
"     needed for the latter are a superset of those needed here, and won't\n"
"     affect anything detrimentally (I hope).\n"
"\n"
"****************************************************************************\n"
"\n"
"  + EXAMPLE:\n"
"      3dDWUncert                                 \\\n"
"      -inset TEST_FILES/DTI/fin2_DTI_3mm_1+orig  \\\n"
"      -prefix TEST_FILES/DTI/o.UNCERT            \\\n"
"      -input TEST_FILES/DTI/DT                   \\\n"
"      -grads TEST_FILES/Siemens_d30_GRADS.dat    \\\n"
"      -iters 50\n\n"
"  If you use this program, please reference the jackknifing algorithm done\n"
"  with nonlinear fitting described in: \n"
"        Taylor PA, Biswal BB (2011). Geometric analysis of the b-dependent\n"
"        effects of Rician signal noise on diffusion tensor imaging\n"
"        estimates and determining an optimal b value. MRI 29:777–788.\n"
"  and the introductory/description paper for the FATCAT toolbox:\n"
"        Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional\n"
"        And Tractographic Connectivity Analysis Toolbox. Brain \n"
"        Connectivity 3(5):523-535.\n"
"____________________________________________________________________________\n"
);	
   return;
}



int main(int argc, char *argv[]) {
	int i,j,k,n,b,ii,jj;
	int iarg;
	
	THD_3dim_dataset *dwset1=NULL; 
	THD_3dim_dataset *UNC_OUT=NULL;
	char *prefix="tracky" ;
   char *gradmat_in=NULL;

	THD_3dim_dataset *MASK=NULL;
	char in_mask[300];
	int HAVE_MASK=0;

	char in_FA[300];
	char in_V1[300];
	char in_V2[300];
	char in_V3[300];
	char in_L1[300];
	
	int xmask1=0,ymask1=0,zmask1=0;
	
	FILE *fin4;
	int Nvox=-1;   // tot number vox
	int Dim[3]={0,0,0}; // dim in each dir
	int M=0;
	// like 3dDWtoDTI, we don't need bval--
	// just leave this here in case we ever want it later.
	float bval=1.0; 
   int Ndata = 0,ni=0,nprog=0;
   time_t t_start;

	//	THD_3dim_dataset *dsetn;
	
	float *allS=NULL; 
	float **sortedout=NULL; // output for evals, evecs, Nvox x 12 
	float S0;
	int **StoreRandInd=NULL;
	float testdd[6];
	float *testS=NULL;
	float *bmatr=NULL;
	float **grads=NULL; // read in grads
	float *Wei2=NULL;
	float meanE1e2,meanE1e3,meanDelFA;
	float stdE1e2,stdE1e3,stdDelFA;
	float **OUT=NULL;
	unsigned int *randarr=NULL;
	int *ind=NULL;
	float *delE1e2=NULL,*delE1e1=NULL,*delE1e3=NULL, *delFA=NULL;
	int worstS, Nbad; // switch for testing bad ADC_i (i.e., S_i) 
	float aveADC, mostpos;
	//	int  NONLINLM=0; // only linear fitting for this approx.
	float E1e2,E1e1,E1e3;
	float temp,ang,md;
	int BADNESS;
	float randmagn;
	float CSF_FA = 0.012345678; // afni-set version

   float jknife_val = 0.7; // default jackknife fraction
   int Njkout = 0, count_jkout = 0, CI_val=0;  // just options for testing
   char FILE_jkout[300];
   char FILE_CIout[300];
   float CI_minmax[2]={-1., -1.,};
   float **OUTCI=NULL;
	THD_3dim_dataset *UNC_OUTCI=NULL;

	char evalevecs[300]; 
	
	int Nj=10;
	int Mj=0;
	int MAXBAD=0;
   int CHOOSE_SEED = 0;     // for int testing

   float CALC_THR_FA = -1.;  // DEFAULT: value for new option of only
                            // calculating uncert in some voxels, save
                            // where CSF/GM would cost time AND money!
	
	long seed1, seed2;
	const gsl_rng_type * T;
	gsl_rng *r;
         /* ZSS2PT: Don't decalre variables in the middle of a scope
                Older compilers won't like it */
	int BMAT = -1; // switch about whether using grads or bmatr input 
                 // with potentially several b-vals
	int FOUND =-1;
	int Min=0,Nb0=0;
	int *DWcheck=NULL; 
	float *DWs=NULL;  
	float DWval;
	float DWmax=0.0;
	float **grads_dyad=NULL; // will store grads in bmatr form
	float **rearr_dwi=NULL;
	// for testing names...
	char *postfix[4]={"+orig.HEAD\0",".nii.gz\0",".nii\0","+tlrc.HEAD\0"};
	
	char *DTI_SCAL_INS[N_dti_scal] = {"FA", "MD", "L1"};  // @@@
	char *DTI_VECT_INS[N_dti_vect] = {"V1", "V2", "V3"};
   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2; // "-wild_files_noAext_noAview"
   int wild_orig_name = 1; // "-wild_files_orig_name"
   char tprefix[THD_MAX_PREFIX];
   int nfiles;
 	char *infix=NULL ;
   THD_3dim_dataset **insetPARS=NULL; // [0] FA, [1] MD, [2] L1
   THD_3dim_dataset **insetVECS=NULL; // for DEF_DTI, [0] V1, [1] V2, [2] V3
   char temp_name[32];
   int hardi_pref_len=0;
   int pref_offset = 0;
   char *dti_listname=NULL;
	NI_element *nel=NULL;

   /*
   // for random number generation
	srand(time(0));
	seed = time(NULL) ;
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc (T);
	gsl_rng_set (r, seed);
	*/

	mainENTRY("3dDWUncert"); machdep(); 
   
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************
	// INFO_message("version: IOTA");
	
	if (argc == 1) { usage_DWUncert(1); exit(0); }
	
	iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_DWUncert(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}

		if( strcmp(argv[iarg],"-inset") == 0 ){ // in DWIs
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-inset'") ;
			dwset1 = THD_open_dataset( argv[iarg] ) ;
			if( dwset1 == NULL ) 
				ERROR_exit("Can't open DWI dataset '%s'", argv[iarg]) ;
			DSET_load(dwset1) ; CHECK_LOAD_ERROR(dwset1) ;
			xmask1 = DSET_NX(dwset1); ymask1 = DSET_NY(dwset1); 
			zmask1 = DSET_NZ(dwset1); 
			
			
			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-prefix") == 0 ){ // will be output
			if( ++iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix = strdup(argv[iarg]) ;
			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");
         INFO_message("Output prefix is: %s",prefix);
			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-input") == 0 ){ // initial results of 3dDWItoDTI
			if( ++iarg >= argc ) 
							ERROR_exit("Need argument after '-input'");

         infix = strdup(argv[iarg]) ;
         // for naming stuff later
         hardi_pref_len = strlen(infix); // assume an underscore

			iarg++ ; continue ;
		}
	 
      if( strcmp(argv[iarg],"-input_list") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-input_list'");
         dti_listname = strdup(argv[iarg]) ;

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-grads") == 0 ){ 
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-grads'");
			
         BMAT = 0;
         gradmat_in = strdup(argv[iarg]) ;

			iarg++ ; continue ;
		}
		else if( strcmp(argv[iarg],"-bmatrix_Z") == 0 ){ 
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-bmatrix_Z'");
         
			// if b-matrix is being used as input,
			// have to find out still which one(s) are b=0.
			BMAT = 1;
         gradmat_in = strdup(argv[iarg]) ;
         
			iarg++ ; continue ;
		}
		
      if( strcmp(argv[iarg],"-bmatr") == 0 ){ 
         
         ERROR_exit("'Tis not long for this world-- "
                    "nor for this option! This '-bmatr'\n"
                    "\toption of which you type no longer exists! "
                    "You might seek '-bmatrix_Z ...' instead.\n"
                    "\tPlease see the help for more description.");
         
			iarg++ ; continue ;
		}
      

		if( strcmp(argv[iarg],"-mask") == 0 ){
			if( ++iarg >= argc ) 
							ERROR_exit("Need argument after '-mask'");
			HAVE_MASK=1;
			
			sprintf(in_mask,"%s", argv[iarg]); 
			MASK = THD_open_dataset(in_mask) ;
			if( (MASK == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_mask);
			
			DSET_load(MASK); CHECK_LOAD_ERROR(MASK);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-iters") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-iters'");
		
			Nj = atoi(argv[iarg]);
			if(Nj <= 2)
				ERROR_exit("(Far) too few iterations.");
			iarg++ ; continue ;
		}
	 
		// optional, can change `magic' value of csf_fa value for
		// skipping `bad' voxels, which has been set `above' to match
		// value of 3dDWItoDT.
		if( strcmp(argv[iarg],"-csf_fa") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-csf_fa'");
			CSF_FA = atof(argv[iarg]);
			iarg++ ; continue ;
		}
	 

      // May, 2015: new option to set minimum value to both
      // calculating uncertainty: default is -1, but in an adult, one
      // could maybe use 0.1 or even 0.15
      if( strcmp(argv[iarg],"-calc_thr_FA") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-calc_thr_FA'");
			CALC_THR_FA = atof(argv[iarg]);
         INFO_message("Setting FA threshold for calculation to be: "
                      "%.5f !",CALC_THR_FA);

			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-pt_choose_seed") == 0) {
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '-pt_choose_seed'");
         CHOOSE_SEED = atoi(argv[iarg]);
         INFO_message("(PT) Internal option: "
                      "random seed for testing set to: %d.",CHOOSE_SEED);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-pt_jkval") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-pt_jkval'");
			jknife_val = atof(argv[iarg]);
         INFO_message("(PT) Internal option: "
                      "jackknife fraction being set to: %.5f.",jknife_val);

         if( (jknife_val >= 1 ) || (jknife_val <=0 ) )
				ERROR_exit("Jackknife fraction must be between 0 and 1, not %f.",
                       jknife_val);
			
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-pt_jkout") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-pt_jkout'");
			Njkout = atoi(argv[iarg]);
         INFO_message("(PT) Internal option: "
                      "writing %d example files of JK iterations.", Njkout);
         
         if( Njkout <=0 )
				ERROR_exit("Jackknife fraction must be between [1,Nvox], not %d.",
                       Njkout);
			
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-pt_conf") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-pt_conf'");
			CI_val = atoi(argv[iarg]);
         INFO_message("(PT) Internal option: "
                      "confidence intervaling at level: %d\%.",CI_val);

         if( (CI_val > 100 ) || (CI_val <=0 ) )
				ERROR_exit("Confidence interval percent must be (0, 100], not %d.",
                       CI_val);
			
			iarg++ ; continue ;
		}

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
  
	if (iarg < 5) {
		ERROR_message("Too few options: %d. Try -help for details.\n", iarg);
		exit(1);
	}

   // for random number generation
   if( CHOOSE_SEED )
      seed1 = seed2 = CHOOSE_SEED;
   else {
   seed1 = time(NULL);
	seed2 = time(NULL) ;
   }

	srand(seed1);
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc (T);
	gsl_rng_set (r, seed2);
  
   if( infix && dti_listname ) {
		ERROR_message("Use *either* '-input' or '-input_list'.\n");
		exit(1);
	}
   else {
      
      insetPARS = (THD_3dim_dataset **)calloc(N_dti_scal, 
                                              sizeof(THD_3dim_dataset *));
      
      insetVECS = (THD_3dim_dataset **)calloc(N_dti_vect, 
                                              sizeof(THD_3dim_dataset *)); 
      
      if(  (insetPARS == NULL) || (insetVECS == NULL) ){
         fprintf(stderr,"\n\nMemAlloc failure: prepping to store sets.\n\n");
         exit(123);
      }
      
      if(infix){
         
      
         sprintf(tprefix,"%s*",infix);
      
         // this island of coding, globbing and sorting due to ZSS;
         // see apsearch.c program for original.
         wild_list = SUMA_append_replace_string(wild_list, tprefix, " ", 1); 
      
         INFO_message("SEARCHING for files with prefix '%s'",tprefix);
      
         MCW_wildcards(wild_list, &nglob, &wglob ); 
         if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                                 &nsort, &isrt))) {
         
            for( ii=0 ; ii<nsort ; ii++) {
            
               // check for first char being an underscore; if so, remove
               pref_offset = 0;
               if( *(wsort[ii]+hardi_pref_len) == '_')
                  pref_offset = 1;
            
               snprintf(temp_name,31,"%s", 
                        wsort[ii]+hardi_pref_len+pref_offset);
            
               for( i=0 ; i<N_dti_scal ; i++ ) {
                  if ( !strcmp(DTI_SCAL_INS[i], temp_name) ) {
                     fprintf(stderr," '%s' ",DTI_SCAL_INS[i]);
                     insetPARS[i] = THD_open_dataset(wglob[isrt[ii]]);
                     if( insetPARS[i] == NULL ) 
                        ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]] );
                     DSET_load(insetPARS[i]) ; CHECK_LOAD_ERROR(insetPARS[i]) ;
                     break;
                  }
                  else continue;
               }
            
               for( i=0 ; i<N_dti_vect ; i++ ) {
                  if ( !strcmp(DTI_VECT_INS[i], temp_name) ) {
                     fprintf(stderr," '%s' ",DTI_VECT_INS[i]);
                     insetVECS[i] = THD_open_dataset(wglob[isrt[ii]]);
                     if( insetVECS[i] == NULL ) 
                        ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]] );
                     DSET_load(insetVECS[i]) ; CHECK_LOAD_ERROR(insetVECS[i]) ;
                     break;
                  }
                  else continue;
               }
            }
         
            // double check all got filled:
            for( i=0 ; i<N_dti_scal ; i++ ) 
               if( (insetPARS[i] == NULL) && !(i == 1) ) // b/c MD is not nec 
                  ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_INS[i] );
            for( i=0 ; i<N_dti_vect ; i++ ) 
               if( insetVECS[i] == NULL ) 
                  ERROR_exit("Can't open dataset: '%s' file",DTI_VECT_INS[i] );
            fprintf(stderr,"\n");			
         
            if (isrt) free(isrt); isrt = NULL;
            for (i=0; i<nglob; ++i) if (wsort[i]) free(wsort[i]);
            free(wsort); wsort = NULL;
            SUMA_ifree(wild_list);
            MCW_free_wildcards( nglob , wglob ) ;
         } 
         else {
            ERROR_message("Failed to sort");
            SUMA_ifree(wild_list);
            MCW_free_wildcards( nglob , wglob ) ;
            exit(1);
         }
      }
      else if(dti_listname) {
         char **NameVEC=NULL;
         char *NameXF=NULL;
         char **NameSCAL=NULL;
         char **NameP=NULL; // 4 of these

         NameVEC = calloc( N_dti_vect,sizeof(NameVEC));  
         for(i=0 ; i<N_dti_vect ; i++) 
            NameVEC[i] = calloc( 100,sizeof(char)); 
         NameSCAL = calloc( N_dti_scal,sizeof(NameSCAL));  
         for(i=0 ; i<N_dti_scal ; i++) 
            NameSCAL[i] = calloc( 100,sizeof(char)); 
         NameP = calloc( N_plus_dtifile,sizeof(NameP));  
         for(i=0 ; i<N_plus_dtifile ; i++) 
            NameP[i] = calloc( 100,sizeof(char)); 
         NameXF = (char *)calloc(100, sizeof(char)); 

         if( (NameVEC == NULL) || (NameSCAL == NULL) ||
             (NameXF == NULL) || (NameP == NULL)  ) {
				fprintf(stderr, "\n\n MemAlloc failure.\n\n");
				exit(126);
			}

         if( (NameVEC == NULL) || (NameSCAL == NULL) ||
             (NameXF == NULL) || (NameP == NULL)  ) {
				fprintf(stderr, "\n\n MemAlloc failure.\n\n");
				exit(126);
			}
		  
         if (!(nel = ReadDTI_inputs(dti_listname))) {
				ERROR_message("Failed to read options in %s\n",
                           dti_listname);
				exit(19);
			}
         
			if (NI_getDTI_inputs( nel,
                               NameVEC,
                               NameXF,
                               NameSCAL,
                               NameP,
                               &i, &j)) {
				ERROR_message("Failed to get DTI list of files.");
				exit(1);
			}
			NI_free_element(nel); nel=NULL;

         for( ii=0 ; ii<N_dti_scal ; ii++) {
            if( !(ii == 1)) { // because we don't use MD
               insetPARS[ii] = THD_open_dataset(NameSCAL[ii]);
               if( (insetPARS[ii] == NULL ) )
                  ERROR_exit("Can't open listed dataset '%s': "
                             "for required scalar.",
                             NameSCAL[ii]);
               DSET_load(insetPARS[ii]) ; CHECK_LOAD_ERROR(insetPARS[ii]) ;
               fprintf(stderr,"\tFound file '%s' to be labeled '%s'\n",
                       NameSCAL[ii],DTI_SCAL_INS[ii]);
            }
         }

         for( ii=0 ; ii<N_dti_vect ; ii++) {
            insetVECS[ii] = THD_open_dataset(NameVEC[ii]);
            if( (insetVECS[ii] == NULL ) )
               ERROR_exit("Can't open dataset '%s': for required vector dir.",
                          NameVEC[ii]);
            DSET_load(insetVECS[ii]) ; CHECK_LOAD_ERROR(insetVECS[ii]) ;
            fprintf(stderr,"\tFound file '%s' to be labeled '%s'\n",
                    NameVEC[ii],DTI_VECT_INS[ii]);
         }

         // double check all got filled:
         for( i=0 ; i<N_dti_scal ; i++ ) 
            if( (insetPARS[i] == NULL) && !(i == 1) ) // b/c MD is not nec 
               ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_INS[i] );
         for( i=0 ; i<N_dti_vect ; i++ ) 
            if( insetVECS[i] == NULL ) 
               ERROR_exit("Can't open dataset: '%s' file",DTI_VECT_INS[i] );
         fprintf(stderr,"\n");			
         

         for(i=0 ; i<N_dti_vect ; i++)
            free(NameVEC[i]);
         free(NameVEC);
         for(i=0 ; i<N_dti_scal ; i++) 
            free(NameSCAL[i]);
         free(NameSCAL);
         for(i=0 ; i<N_plus_dtifile ; i++) 
            free(NameP[i]);
         free(NameP);
         free(NameXF);
      }
      else{
         for( i=0 ; i<N_dti_scal ; i++){
            DSET_delete(insetPARS[i]);
            free(insetPARS[i]);
         }
         free(insetPARS);
         for( i=0 ; i<N_dti_vect ; i++){
            DSET_delete(insetVECS[i]);
            free(insetVECS[i]);
         }
         free(insetVECS);
         ERROR_message("Failed to read input.");
      }
   }
      
   Nvox = DSET_NVOX(insetPARS[0]);
   Dim[0] = DSET_NX(insetPARS[0]); 
   Dim[1] = DSET_NY(insetPARS[0]); 
   Dim[2] = DSET_NZ(insetPARS[0]); 
   
   
   
   if( (Dim[0] != xmask1) || (Dim[1] != ymask1) || (Dim[2] != zmask1))
      ERROR_exit("Input dataset does not match both mask volumes!");



   if(BMAT == 0) {
      // 3cols of grad vecs
      M = DSET_NVALS(dwset1)-1; // because 1st one is b=0
      Mj = (int) floor(jknife_val*M);
      if(Mj<7)
         Mj=7;
      INFO_message("Input format: grads. Number of DWI in inset=%d."
                   " Jackknife sample size=%d", M, Mj);
      MAXBAD = (int) (0.125*M);
      // for some really large numbers of gradients, >100, the number is 
      // just too big; Jan, 2014
      if( MAXBAD>10)
         MAXBAD = 10; 
      if(Mj-MAXBAD<6)
         MAXBAD=Mj-6;
      
      grads = calloc(M,sizeof(grads)); 
      for(i=0 ; i<M ; i++) 
         grads[i] = calloc(3,sizeof(float)); 
      grads_dyad = calloc(M,sizeof(grads_dyad)); 
      for(i=0 ; i<M ; i++) 
         grads_dyad[i] = calloc(6,sizeof(float)); 
      
      if( (grads == NULL) || (grads_dyad == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(1254);
      }
		
      if( (fin4 = fopen(gradmat_in, "r")) == NULL) {
         fprintf(stderr, "Error opening file %s.",gradmat_in);
         exit(19);
      }
      for(i=0 ; i<M ; i++) 
         for(j=0 ; j<3 ; j++) 
            fscanf(fin4, "%f",&grads[i][j]);
      fclose(fin4);
      
      // these are what go into the fitting matrices
      // diagonals and then UHT
      for(i=0 ; i<M ; i++) {
         for(j=0 ; j<3 ; j++)
            grads_dyad[i][j] = grads[i][j]*grads[i][j];
         grads_dyad[i][3] = 2.*grads[i][0]*grads[i][1];
         grads_dyad[i][4] = 2.*grads[i][0]*grads[i][2];
         grads_dyad[i][5] = 2.*grads[i][2]*grads[i][1];
      }
   }
   else if( BMAT==1) { // apr,2016: now for AFNI format!
      Min = DSET_NVALS(dwset1); 
		
      DWcheck = (int *)calloc(Min,sizeof(int)); 
      DWs = (float *)calloc(Min,sizeof(float)); //DWs
      // just use this to read in all
      grads = calloc(Min,sizeof(grads)); 
      for(i=0 ; i<Min ; i++) 
         grads[i] = calloc(6,sizeof(float)); 
      
      if( (grads == NULL) || (DWs == NULL) || (DWcheck == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(1254);
      }
      // Opening/Reading in FACT params
      if( (fin4 = fopen(gradmat_in, "r")) == NULL) {
         fprintf(stderr, "Error opening file %s.",gradmat_in);
         exit(19);
      }
      
      for(i=0 ; i<Min ; i++) {
         for(j=0 ; j<6 ; j++) {
            fscanf(fin4, "%f",&grads[i][j]);
         }
         // trace of bmatr is DWval
         DWval = grads[i][0]+grads[i][1]+grads[i][2];
         if( DWval<EPS_MASK ) {
            DWcheck[i] = 1; // flag b=0 ones
            Nb0+=1;
         }
         else {
            DWs[M] = DWval;
            if(DWval>DWmax)
               DWmax = DWval;// check for max DW, in case mult DWval
            M+=1;
         }			
      }
		  			
      fclose(fin4);
      if(Nb0<1)
         ERROR_exit("There appear to be no b=0 bricks!");

      Mj = (int) floor(jknife_val*M);
      if(Mj<7) 
         Mj=7;
      INFO_message("Input format: bmatrs. Number of DWI in inset=%d."
                   " Jackknife sample size=%d", M, Mj);
      MAXBAD = (int) (0.125*M);
      // for some really large numbers of gradients, >100, the number is 
      // just too big; Jan, 2014
      if( MAXBAD>10)
         MAXBAD = 10; 
      if(Mj-MAXBAD<6)
         MAXBAD=Mj-6;
			
      grads_dyad = calloc(M,sizeof(grads_dyad)); 
      for(i=0 ; i<M ; i++) 
         grads_dyad[i] = calloc(6,sizeof(float)); 
      // start to restructure DWIs in a copy, 
      // averaging all b=0 bricks together
      rearr_dwi = calloc(Nvox,sizeof(rearr_dwi)); 
      for(i=0 ; i<Nvox ; i++) 
         rearr_dwi[i] = calloc(M+1,sizeof(float)); 
			
      if( (grads_dyad == NULL) || (rearr_dwi == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(125);
      }
			
      M=0;
      for( j=0 ; j<Min ; j++) 
         if(DWcheck[j] == 0) {// is not a b=0
            grads_dyad[M][0] = grads[j][0];
            grads_dyad[M][1] = grads[j][1];
            grads_dyad[M][2] = grads[j][2];
            grads_dyad[M][3] = 2*grads[j][3];
            grads_dyad[M][4] = 2*grads[j][4];
            grads_dyad[M][5] = 2*grads[j][5];
            M+=1; 
            for(i=0 ; i<Nvox ; i++)
               if( THD_get_voxel(insetPARS[2],i,0)>EPS_V)  // as below
                  rearr_dwi[i][M] = THD_get_voxel(dwset1,i,j);
         }
         else 
            for(i=0 ; i<Nvox ; i++)
               if( THD_get_voxel(insetPARS[2],i,0)>EPS_V) 
                  rearr_dwi[i][0]+= THD_get_voxel(dwset1,i,j);
			
      // because this was an average
      for(i=0 ; i<Nvox ; i++)
         rearr_dwi[i][0]/= 1.0*Nb0;
   }
   else
      ERROR_exit("Neither Grads nor Bmatr were successfully input!");



	// just general array storage creation stuff
	StoreRandInd = calloc(Nj,sizeof(StoreRandInd)); 
	for(i=0 ; i<Nj ; i++) 
		StoreRandInd[i] = calloc(Mj,sizeof(int)); 
	randarr = (unsigned int *)calloc(M,sizeof(unsigned int)); 
	ind = (int *)calloc( M,sizeof(int)); 
	Wei2 = (float *)calloc(Mj,sizeof(float));
	allS = (float *)calloc(M,sizeof(float)); 
	OUT = calloc(6,sizeof(OUT)); 
	for(i=0 ; i<6 ; i++) 
		OUT[i] = calloc( Nvox,sizeof(float)); 
	sortedout = calloc((Nvox),sizeof(sortedout)); 
	for(i=0 ; i<Nvox ; i++) 
		sortedout[i] = calloc(12,sizeof(float)); 
	//bmatr = (float *)calloc(6*Mj, sizeof(float)); //!!!!floatcheck
	testS = (float *)calloc(Mj,sizeof(float));
	delE1e1 = (float *)calloc(Nj, sizeof(float));
	delE1e2 = (float *)calloc(Nj, sizeof(float));
	delE1e3 = (float *)calloc(Nj, sizeof(float));
	delFA = (float *)calloc(Nj, sizeof(float));
  
	if( (grads == NULL) || (allS == NULL) || (sortedout == NULL) 
		 || (Wei2 == NULL) || (testS == NULL) //(bmatr == NULL) || 
		 || (StoreRandInd == NULL) || (OUT == NULL) ||  (delFA == NULL)
		 || (delE1e1 == NULL) || (delE1e2 == NULL) || (delE1e3 == NULL) ) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(12);
	}

   if(CI_val){
      OUTCI = calloc(6,sizeof(OUTCI)); 
      for(i=0 ; i<6 ; i++) 
         OUTCI[i] = calloc( Nvox,sizeof(float)); 

      if( OUTCI == NULL) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(12);
      }
   }
   
	// make vec/matr for doing calcs per vox
	// (H^T Wei H)^-1 H^T Wei Y = dd
	gsl_matrix *H = gsl_matrix_alloc(Mj, 6);// of (grad x grad) quants
	gsl_matrix *HTW = gsl_matrix_alloc(6, Mj); //  (H^T Wei)
	gsl_matrix *HTWH = gsl_matrix_alloc(6, 6); //  (H^T Wei H)
	gsl_matrix *HTWHinv = gsl_matrix_alloc(6, 6); //  (H^T H)^-1
	gsl_matrix *C = gsl_matrix_alloc(6, Mj); //  
	gsl_matrix *DD = gsl_matrix_alloc(3,3); // for eigencalc
	gsl_matrix *Evec = gsl_matrix_alloc(3,3); // for eigencalc
	gsl_matrix *testD = gsl_matrix_alloc(3,3); // for eigencalc
  
	gsl_permutation *P = gsl_permutation_alloc(6);
  
	gsl_vector *Y = gsl_vector_alloc(Mj);// DW S_i 
	gsl_vector *dd = gsl_vector_alloc(6);// flattened D, answers we want...
	gsl_vector *Eval = gsl_vector_alloc(3);// weights of ADC values, est.
  
	gsl_eigen_symmv_workspace *EigenW = gsl_eigen_symmv_alloc(3);
	gsl_eigen_symm_workspace *EigenV = gsl_eigen_symm_alloc(3);
  
  
	
  
	// ****************************************************************
	// ****************************************************************
	//                    prepare
	// ****************************************************************
	// ****************************************************************
  
  
	// calculate array of random numbers to use 
	for( b=0 ; b<Nj ; b++) {
		// set up random array; the indices are co-sorted
		for( i=0; i<M ; i++) {
			randarr[i] = rand();
			ind[i] = i;  
		}
		piksr2(M, randarr, ind);
		//  now, take first Mj elements of rearranged arrays,
		//  and put them into temporary arrays to analyze.  
		for( i=0 ; i<Mj ; i++) 
			StoreRandInd[b][i] = ind[i];
	}
  
   for( i=0 ; i<Nvox ; i++ ) 
      if( ( HAVE_MASK && !(THD_get_voxel(MASK,i,0)>0) ) ||
			 ( (HAVE_MASK==0) && (THD_get_voxel(insetPARS[2],i,0)<EPS_V) ) ||
          (THD_get_voxel(insetPARS[0],i,0) < CALC_THR_FA) // extra option!
          ) 
         continue;
      else
         Ndata++;
   ni = (int) Ndata / 10.;
   if (ni < 2)
      ni = Ndata;
   INFO_message("Ratio of data/total number of voxels: %d/%d.",
                Ndata, Nvox);
   
   if( Njkout > Ndata)
      WARNING_message("Resetting number of output jackknife examples "
                      " from %d to %d", Njkout, Ndata);

   /*   for( i=0 ; i<Nvox ; i++ ) 
      if( ( HAVE_MASK && !(THD_get_voxel(MASK,i,0)>0) ) ||
			 ( (HAVE_MASK==0) && (THD_get_voxel(insetPARS[2],i,0)<EPS_V) ) ) 
         continue;
      else{
         nprog++;
         if (nprog % ni == 0)
            fprintf(stderr,"%s %.0f%% %s","[", nprog *10./ni,"]");
      }*/

   fprintf(stderr,"++ Nvox progress count: start ...\n");
   t_start = time(NULL);

   count_jkout = 0;

	for(i=0 ; i<Nvox ; i++) 
      // May, 2015: added extra condition here, allowing user to set
      // FA threshold for calculation.  default is :-1
		if( ( HAVE_MASK && !(THD_get_voxel(MASK,i,0)>0) ) ||
			 ( (HAVE_MASK==0) && (THD_get_voxel(insetPARS[2],i,0)<EPS_V) ) ||
          (THD_get_voxel(insetPARS[0],i,0) < CALC_THR_FA) ) {
			//for(j=0 ; j<6 ; j++) 
			//	OUT[j][i] = 0.0;
         continue;
		}
		else if(THD_get_voxel(insetPARS[0],i,0)==CSF_FA) {
			OUT[0][i] = OUT[2][i] = OUT[4][i] = 0.0; // zero mean
			OUT[1][i] = OUT[3][i] = PIo2; // max uncert, but prob doesn't matter.
			OUT[5][i] = 1.0; // max uncert

         if(CI_val) {
            OUTCI[0][i] = OUTCI[2][i] = -PIo2;
            OUTCI[1][i] = OUTCI[3][i] = PIo2;
            OUTCI[4][i] = - THD_get_voxel(insetPARS[0],i,0);
            OUTCI[5][i] = 1 - THD_get_voxel(insetPARS[0],i,0);
         }

		}
		else {
         // counting, technically off by 1,
         // because announcing before this is done
         nprog++;
         if (nprog % ni == 0) {
            fprintf(stderr,"\t%s %3.0f%% %s -> %.2f min\n",
                    "[", nprog *10./ni,"]", 
                    (float) difftime( time(NULL), t_start)/60. );
         }

			if( BMAT==0 ) {
				S0 = 1.0*THD_get_voxel(dwset1,i,0);
				for(j=0 ; j<M ; j++) {
					if( THD_get_voxel(dwset1,i,j+1)>=S0 )
						allS[j] = -log(0.99*S0)/bval;
					else if( THD_get_voxel(dwset1,i,j+1) <=0 ) 
						allS[j] = 0.;//-log(1.)/bval;
					else //scale all signals
						allS[j] = -log(THD_get_voxel(dwset1,i,j+1)*1.0/S0)/bval; 
				}
			}
			else{
				S0 = 1.0*rearr_dwi[i][0];
				for(j=0 ; j<M ; j++) {
					if( rearr_dwi[i][j+1]>=S0 )
						allS[j] = -log(0.99*S0)/DWs[j];
					else if( rearr_dwi[i][j+1] <=0 ) 
						allS[j] = 0.;//-log(1.)/DWs[j];
					else //scale all signals
						allS[j] = -log(rearr_dwi[i][j+1]*1.0/S0)/DWs[j];
				}
			}


			for( jj=0 ; jj<Nj ; jj++) {
				BADNESS = 0;
				
				//  H matr of grads
				for(ii=0 ; ii<Mj ; ii++) {
					for(j=0 ; j<6 ; j++) 
						gsl_matrix_set(H,ii,j,grads_dyad[StoreRandInd[jj][ii]][j]);					
					//not +1 in index!; already did log part above
					gsl_vector_set(Y,ii,allS[StoreRandInd[jj][ii]]);
				}
				
				Nbad =0;
				gsl_vector_set_zero(dd);
        
				// to get initial guess for LM, do LSq regress
				for(j=0 ; j<Mj ; j++) {
					Wei2[j] = 1.; // part 2: default per voxel: no bad signals
					for(k=0 ; k<6 ; k++) //  (H^T Wei)
						gsl_matrix_set(HTW,k,j,gsl_matrix_get(H,j,k));///Wei[j]);
				}
        
				// set up the inverse
				gsl_matrix_set_zero(HTWH);   
				gsl_matrix_set_zero(HTWHinv);
				gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,HTW,H,1.0,HTWH);
        
				// invert 
				j = gsl_linalg_LU_decomp(HTWH, P, &k);
				j = gsl_linalg_LU_invert(HTWH, P, HTWHinv);
        
				// multiply pieces, and finally with Y to get dd (answer)
				gsl_matrix_set_zero(C);
				gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,HTWHinv,HTW,1.0,C);
				gsl_vector_set_zero(dd);
				gsl_blas_dgemv(CblasNoTrans,1.0,C,Y,1.0,dd);
        
				// make sure eigenvalues are all positive
				for(j=0 ; j<3 ; j++) //diagonal elements
					gsl_matrix_set(testD,j,j,gsl_vector_get(dd,j));
				gsl_matrix_set(testD,0,1,gsl_vector_get(dd,3));
				gsl_matrix_set(testD,1,0,gsl_vector_get(dd,3));
				gsl_matrix_set(testD,0,2,gsl_vector_get(dd,4));
				gsl_matrix_set(testD,2,0,gsl_vector_get(dd,4));
				gsl_matrix_set(testD,1,2,gsl_vector_get(dd,5));
				gsl_matrix_set(testD,2,1,gsl_vector_get(dd,5));
        
				j = gsl_eigen_symm(testD, Eval, EigenV);
		  
				{ // testing/fixing badness in signals
					if( (gsl_vector_get(Eval,0) <= MINEIG) 
						 || (gsl_vector_get(Eval,1) <= MINEIG) 
						 || (gsl_vector_get(Eval,2) <= MINEIG) ) {
				
						Nbad = 0;
						b = 0;
            
						while( (Nbad < MAXBAD) && (b<Mj+3) ) {
							// part 1: so, default per voxel is no bad signals
							mostpos = -1.e10; // some large&neg val, to be baddest...
							worstS = -1; 
              
							// iterate through for eliminating potential 'bad'
							for( b=0 ; b<Mj ; b++) {
								if( Wei2[b] < 2) { 
									// in case we've already found 1 bad, to skip it now
									Wei2[b] = pow(10,6); // large weight, effectively 0s.
                  
									for(j=0 ; j<Mj ; j++) 
										for(k=0 ; k<6 ; k++) //  (H^T Wei)
											gsl_matrix_set(HTW,k,j,
                                                gsl_matrix_get(H,j,k)/Wei2[j]);
						
									// set up the inverse
									gsl_matrix_set_zero(HTWH);   
									gsl_matrix_set_zero(HTWHinv);
									gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,
														HTW,H,1.0,HTWH);
						
									// invert 
									j = gsl_linalg_LU_decomp(HTWH, P, &k);
									j = gsl_linalg_LU_invert(HTWH, P, HTWHinv);
                  
									// multiply pieces, 
                           // and finally with Y to get dd (answer)

									gsl_matrix_set_zero(C);
									gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,
														HTWHinv,HTW,1.0,C);
									gsl_vector_set_zero(dd);
									gsl_blas_dgemv(CblasNoTrans,1.0,C,Y,1.0,dd);
						
									// make sure eigenvalues are all positive
									for(j=0 ; j<3 ; j++) //diagonal elements
										gsl_matrix_set(testD,j,j,gsl_vector_get(dd,j));
									gsl_matrix_set(testD,0,1,gsl_vector_get(dd,3));
									gsl_matrix_set(testD,1,0,gsl_vector_get(dd,3));
									gsl_matrix_set(testD,0,2,gsl_vector_get(dd,4));
									gsl_matrix_set(testD,2,0,gsl_vector_get(dd,4));
									gsl_matrix_set(testD,1,2,gsl_vector_get(dd,5));
									gsl_matrix_set(testD,2,1,gsl_vector_get(dd,5));
						
									j = gsl_eigen_symm(testD, Eval, EigenV);
                  
									if( (gsl_vector_get(Eval,0) <= MINEIG) || 
										 (gsl_vector_get(Eval,1) <= MINEIG) || 
										 (gsl_vector_get(Eval,2) <= MINEIG) ) {
										// i.e., still haven't found only bad
										Wei2[b] = 1.; // reset value;  undo elimination
										for(j=0 ; j<3 ; j++) 
											if(gsl_vector_get(Eval,j) >= mostpos) {
												worstS = b;
												mostpos = gsl_vector_get(Eval,j);
											}
									}
									else {// i.e., have eliminated bads- they are 
										// recorded in Wei2 (or latest one will be
										// shortly), so we can exit loop
										worstS = b;
										b = Mj+3; // exit loop searching for bad ones
									}
                  
								}
							} //endfor
              
							// if we make it to end without finding badS, eliminate
							// worst and then go back to find another bad;  
							// reset tests to find, again
							Wei2[worstS] = pow(10,6);
							Nbad+=1;
              
						} // end while
					} //endif
				} //endsec
        
        
				// if we have so many many bad, just use sphere of average ADC 
				// as initial guess--
				// i.e., full amount of bad, and still didn't get all pos evals
				// if we have so many many bad, then call voxel corrupted, and
				// make it a sphere of average ADC and move on
				if( (Nbad >= MAXBAD) && (b<Mj+3)) { 
					aveADC = 0.0;
					for(j=0 ; j<Mj ; j++) 
						aveADC += gsl_vector_get(Y,j);
					aveADC/=Mj;
			 
					for(j=0 ; j<3 ; j++) { // sphere of rad of ave.diff
						gsl_vector_set(dd,j,aveADC);
						gsl_vector_set(dd,j+3,0);
					}
				}
        
				// use that result to make initial guess for LM;
				// we will store and return that guess.
				for(j=0 ; j<6 ; j++) 
					testdd[j] = gsl_vector_get(dd,j);
		  
				// right now, vector 'dd' (and linear array 'testdd') has linear
				// least squares fit of values, tested for multiple badness

				// ****** no longer doing nonlinear, not nec for uncert...

				// calculate all values we need now, and 'testdd' array 
				// has all we need in either case
        
				if(BADNESS ==1) {
			 
					// just need constant eigvals
					for(j=0 ; j<3 ; j++) {
						sortedout[i][j] = 1.0; 
						sortedout[i][3+j] = gsl_ran_gaussian_ziggurat(r,1.0);
					}
					randmagn = sqrt(pow(sortedout[i][3],2)+
                               pow(sortedout[i][4],2)+
										 pow(sortedout[i][5],2));
					for(j=3 ; j<6 ; j++) {
						sortedout[i][j]/= randmagn;
					}
				}
				else{
			 
					for(j=0 ; j<3 ; j++) //diagonal elements first
						gsl_matrix_set(DD,j,j,testdd[j]);
					gsl_matrix_set(DD,0,1,testdd[3]);
					gsl_matrix_set(DD,1,0,testdd[3]);
					gsl_matrix_set(DD,0,2,testdd[4]);
					gsl_matrix_set(DD,2,0,testdd[4]);
					gsl_matrix_set(DD,1,2,testdd[5]);
					gsl_matrix_set(DD,2,1,testdd[5]);
          
					j = gsl_eigen_symmv(DD, Eval, Evec, EigenW);
					j = gsl_eigen_symmv_sort(Eval, Evec, GSL_EIGEN_SORT_VAL_DESC);
          
					for(j=0 ; j<3 ; j++) {
						sortedout[i][j]=gsl_vector_get(Eval,j);
						for(k=0 ; k<3 ; k++)
							sortedout[i][3+3*j+k]=gsl_matrix_get(Evec,k,j);
					}
				}
		  
				// done calculating; now store for later calculation of mean/stdev

				// dot prod of new E1 with answer e1,e2,e3
				E1e1 = sortedout[i][3]*THD_get_voxel(insetVECS[0],i,0) 
					+ sortedout[i][4]*THD_get_voxel(insetVECS[0],i,1)
					+ sortedout[i][5]*THD_get_voxel(insetVECS[0],i,2);
				if (E1e1 <0)
					E1e1*=-1;
				E1e2 = sortedout[i][3]*THD_get_voxel(insetVECS[1],i,0) 
					+ sortedout[i][4]*THD_get_voxel(insetVECS[1],i,1)
					+ sortedout[i][5]*THD_get_voxel(insetVECS[1],i,2);
				E1e3 = sortedout[i][3]*THD_get_voxel(insetVECS[2],i,0) 
					+ sortedout[i][4]*THD_get_voxel(insetVECS[2],i,1) 
					+ sortedout[i][5]*THD_get_voxel(insetVECS[2],i,2);
		  
				ang = E1e1/sqrt(E1e1*E1e1+E1e2*E1e2);
				if(ang>1.0)
					ang = 0.99999;
				delE1e2[jj] = acos(ang);
				if(E1e2<0)
					delE1e2[jj]*= -1;
		  
				ang = E1e1/sqrt(E1e1*E1e1+E1e3*E1e3);
				if(ang>1.0)
					ang = 0.99999;
				delE1e3[jj] = acos(ang);
				if(E1e3<0)
					delE1e3[jj]*= -1;
		  
				// difference in FA
				md = (sortedout[i][0]+sortedout[i][1]+sortedout[i][2])/3.0;
				temp = pow(sortedout[i][0]-md,2)+pow(sortedout[i][1]-md,2) +
					pow(sortedout[i][2]-md,2);
				temp/= pow(sortedout[i][0],2)+pow(sortedout[i][1],2) +
					pow(sortedout[i][2],2);
				delFA[jj] = sqrt(1.5*temp)-THD_get_voxel(insetPARS[0],i,0);
			}
      
			meanE1e2 = 0;
			meanE1e3 = 0;
			meanDelFA = 0;
			stdE1e2 = 0;
			stdE1e3 = 0;
			stdDelFA = 0;
			for( jj=0 ; jj<Nj ; jj++) {
				meanE1e2+= delE1e2[jj];
				meanE1e3+= delE1e3[jj];
				meanDelFA+= delFA[jj];
				stdE1e2+= delE1e2[jj]*delE1e2[jj];
				stdE1e3+= delE1e3[jj]*delE1e3[jj];
				stdDelFA+= delFA[jj]*delFA[jj];
			}

         // output files of output from all jk iterations, in case
         //it's useful for testing.  
         if( Njkout ) 
            if( count_jkout < Njkout ) {

               sprintf(FILE_jkout,"%s_JKITER_%04d.txt", prefix, count_jkout); 

               if( (fin4 = fopen(FILE_jkout, "w")) == NULL) {
                  fprintf(stderr, "Error opening file %s.", FILE_jkout);
                  exit(19);
               }

               fprintf(fin4,"# %d  # Number of jackknife iterations\n", Nj);
               fprintf(fin4,"# %s\t%s\t%s  # delta parameters\n",
                       "delE1e2  ", "delE1e3  ", "delFA    "); 
               
        			for( jj=0 ; jj<Nj ; jj++) 
                  fprintf(fin4,"%9f\t%9f\t%9f\n",
                          delE1e2[jj], delE1e3[jj], delFA[jj]); 

               fclose(fin4);
      
               count_jkout++;
            }

         if(CI_val) {

            jj = ConfIntSort(delE1e2, Nj, CI_minmax, CI_val);
            if( fabs(CI_minmax[0] - CI_minmax[1]) < EPS_L ){
               // some kind of badness if CI is zero sized.
               OUTCI[0][i] = -PIo2;
               OUTCI[1][i] = PIo2;
            }
            else{
               OUTCI[0][i] = CI_minmax[0];
               OUTCI[1][i] = CI_minmax[1];
            }

            jj = ConfIntSort(delE1e3, Nj, CI_minmax, CI_val);
            if( fabs(CI_minmax[0] - CI_minmax[1]) < EPS_L ){
               // some kind of badness if CI is zero sized.
               OUTCI[2][i] = -PIo2;
               OUTCI[3][i] = PIo2;
            }
            else{
               OUTCI[2][i] = CI_minmax[0];
               OUTCI[3][i] = CI_minmax[1];
            }

            jj = ConfIntSort(delFA, Nj, CI_minmax, CI_val);
            if( fabs(CI_minmax[0] - CI_minmax[1]) < EPS_L ){
               // some kind of badness if CI is zero sized.
               OUTCI[4][i] = - THD_get_voxel(insetPARS[0],i,0);
               OUTCI[5][i] = 1 - THD_get_voxel(insetPARS[0],i,0);
            }
            else{
               OUTCI[4][i] = CI_minmax[0];
               OUTCI[5][i] = CI_minmax[1];
            }
         }


			meanE1e2/= Nj;
			meanE1e3/= Nj;
			meanDelFA/= Nj;
		
			stdE1e2-= Nj*meanE1e2*meanE1e2;
			stdE1e3-= Nj*meanE1e3*meanE1e3;
			stdDelFA-= Nj*meanDelFA*meanDelFA;
      
         if( stdE1e2>0)
            stdE1e2 = sqrt(stdE1e2/(Nj-1));
         else
            stdE1e2 = PIo2;

         if( stdE1e3>0)
            stdE1e3 = sqrt(stdE1e3/(Nj-1));
         else
            stdE1e3 = PIo2;

         if( stdDelFA >0 )
            stdDelFA = sqrt(stdDelFA/(Nj-1));
         else
            stdDelFA = 1;

			OUT[0][i] = meanE1e2;
			OUT[1][i] = stdE1e2;
			OUT[2][i] = meanE1e3;
			OUT[3][i] = stdE1e3;
			OUT[4][i] = meanDelFA;
			OUT[5][i] = stdDelFA;
		
		}
   //   fprintf(stderr,"\n\t ... through.\n");
   fprintf(stderr,"\t%s %3.0f%% %s -> %.2f min\n",
           "[", 100.,"]", (float) difftime( time(NULL) ,t_start)/60.);

	sprintf(evalevecs,"%s_UNC",prefix); 
	UNC_OUT = EDIT_empty_copy( insetPARS[2] ) ; 

	EDIT_add_bricklist(UNC_OUT,
							 5, NULL , NULL , NULL );
	EDIT_dset_items(UNC_OUT,
						 ADN_datum_all , MRI_float , 
						 ADN_none ) ;

	for( n=0; n<6 ; n++) {
		EDIT_substitute_brick(UNC_OUT, n, MRI_float, OUT[n]);
		OUT[n]=NULL;
	}
	EDIT_dset_items(UNC_OUT,
						 ADN_prefix    , evalevecs,
						 ADN_none ) ;
	EDIT_BRICK_LABEL(UNC_OUT,0,"bias_E1e2");      
	EDIT_BRICK_LABEL(UNC_OUT,1,"std_E1e2");      
	EDIT_BRICK_LABEL(UNC_OUT,2,"bias_E1e3");      
	EDIT_BRICK_LABEL(UNC_OUT,3,"std_E1e3");      
	EDIT_BRICK_LABEL(UNC_OUT,4,"bias_FA");      
	EDIT_BRICK_LABEL(UNC_OUT,5,"std_FA");     

	THD_load_statistics( UNC_OUT );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(UNC_OUT)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(UNC_OUT));

	tross_Make_History("3dDWUncert", argc, argv, UNC_OUT);
	THD_write_3dim_dataset(NULL, NULL, UNC_OUT, True);

	DSET_delete(UNC_OUT); 
  	free(UNC_OUT); 


   if(CI_val) { // recycle above to output confidence intervals
      
      sprintf(FILE_CIout,"%s_UNCCI",prefix); 
      UNC_OUTCI = EDIT_empty_copy( insetPARS[2] ) ; 
  
      EDIT_add_bricklist(UNC_OUTCI,
                         5, NULL , NULL , NULL );
      EDIT_dset_items(UNC_OUTCI,
                      ADN_datum_all , MRI_float , 
                      ADN_none ) ;
  
      for( n=0; n<6 ; n++) {
         EDIT_substitute_brick(UNC_OUTCI, n, MRI_float, OUTCI[n]);
         OUTCI[n]=NULL;
      }
      EDIT_dset_items(UNC_OUTCI,
                      ADN_prefix    , FILE_CIout,
                      ADN_none ) ;
      
      EDIT_BRICK_LABEL(UNC_OUTCI,0,"E1e2_min");      
      EDIT_BRICK_LABEL(UNC_OUTCI,1,"E1e2_mix");      
      EDIT_BRICK_LABEL(UNC_OUTCI,2,"E1e3_min");      
      EDIT_BRICK_LABEL(UNC_OUTCI,3,"E1e3_max");      
      EDIT_BRICK_LABEL(UNC_OUTCI,4,"FA_min");      
      EDIT_BRICK_LABEL(UNC_OUTCI,5,"FA_max");     
      
      THD_load_statistics( UNC_OUTCI );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(UNC_OUTCI)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(UNC_OUTCI));
      tross_Make_History("3dDWUncert", argc, argv, UNC_OUTCI);
      THD_write_3dim_dataset(NULL, NULL, UNC_OUTCI, True);
      DSET_delete(UNC_OUTCI); 
      free(UNC_OUTCI); 
      
   }


	
	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************
    
   free(gradmat_in);

	gsl_matrix_free(H);
	gsl_matrix_free(HTW);
	gsl_matrix_free(HTWH);
	gsl_matrix_free(HTWHinv);
	gsl_matrix_free(C);
	gsl_matrix_free(DD);
	gsl_matrix_free(Evec);
	gsl_matrix_free(testD);
  
	gsl_vector_free(Y);
	gsl_vector_free(dd);
	gsl_vector_free(Eval);
  
	gsl_permutation_free(P);
  
	gsl_eigen_symmv_free(EigenW);
	gsl_eigen_symm_free(EigenV);
	free(randarr);
	free(ind);
  
	for( i=0; i<Nvox ; i++) {
		free(sortedout[i]);
		if(BMAT != 0)
			free(rearr_dwi[i]);
	}
	free(sortedout);
	free(allS);
	free(rearr_dwi);

	if(BMAT != 0) {
		free(DWcheck);
		free(DWs);
	}
  
	for( i=0; i<Nj ; i++) 
		free(StoreRandInd[i]);
	free(StoreRandInd);
  
	if( BMAT==0 )
		for( i=0; i<M ; i++)
			free(grads[i]);
	else
		for( i=0; i<Min ; i++)
			free(grads[i]);
	free(grads);
	
	for( i=0; i<M ; i++)
		free(grads_dyad[i]);
	free(grads_dyad); // free

	free(delE1e1);
	free(delE1e3);
	free(delE1e2);
	free(delFA);
  
	free(Wei2);
   //	free(bmatr);
	free(testS);
  
	DSET_delete(dwset1);
  	DSET_delete(MASK);

	free(prefix);
   for( i=0 ; i<N_dti_scal ; i++){
      DSET_delete(insetPARS[i]);
      free(insetPARS[i]);
   }
   free(insetPARS);
   for( i=0 ; i<N_dti_vect ; i++){
      DSET_delete(insetVECS[i]);
      free(insetVECS[i]);
   }
   free(insetVECS);
   free(infix);

	free(dwset1);
  	free(MASK);

	for( i=0; i<6 ; i++) // free all
		free(OUT[i]);
	free(OUT);
	gsl_rng_free(r);

   if(CI_val) {
      for( i=0; i<6 ; i++) // free all
         free(OUTCI[i]);
      free(OUTCI);
   }

	exit(0);
}


int ConfIntSort(float *A, int N, float *vals, int CI_per)
{
   float f=100.;
   int X=0;


   f-= CI_per;
   f/= 200.;
   X = (int) floor(f * N);

   gsl_sort_float(A, 1, N);

   vals[0] = A[X];
   vals[1] = A[N-1-X];

   //fprintf(stderr,"CI: %d;  frac=%f;  N=%d, X=%d, minmax=(%f, %f)\n",
   // CI_per, f, N, X, vals[0], vals[1]);

   return (1);
}








/*
// fitting program, with adaptation from:
// GNU Scientific Library Reference Manual
// Edition 1.15, for GSL Version 1.15, (Galassi et al. 2011) 
void LevMarq( float *xS, float *xB, int N, float *A, float *sigma)
{
	const gsl_multifit_fdfsolver_type *T;
	gsl_multifit_fdfsolver *s;
	int status;
	unsigned int i, iter = 0;
	const size_t n = N;
	const size_t p = 6;
	int k;
	gsl_matrix *covar = gsl_matrix_alloc (p, p);
	float tt[N*6], y[N];
	struct data d = { n, tt, y, sigma};
	gsl_multifit_function_fdf f;
	double x_init[6];
	gsl_vector_view x = gsl_vector_view_array (x_init, p);
	const gsl_rng_type * type;
	gsl_rng * r;


	for( k=0 ; k<6 ; k++)
		x_init[k] = A[k];

	gsl_rng_env_setup();
  
	type = gsl_rng_default;
	r = gsl_rng_alloc (type);
  
	f.f = &expb_f;
	f.df = &expb_df;
	f.fdf = &expb_fdf;
	f.n = n;
	f.p = p;
	f.params = &d;
  
	// This is the data to be fitted
  
	for (i = 0; i < n; i++) {
		for( k=0 ; k<6 ; k++)
			tt[6*i+k] = xB[6*i+k];
		y[i] = xS[i];
	};
  
	T = gsl_multifit_fdfsolver_lmsder;
	s = gsl_multifit_fdfsolver_alloc (T, n, p);
	gsl_multifit_fdfsolver_set (s, &f, &x.vector);
  
	do
		{
			iter++;
			status = gsl_multifit_fdfsolver_iterate (s);
      
			if (status)
				break;
      
			status = gsl_multifit_test_delta (s->dx, s->x,
														 1e-4, 1e-4);
		}
	while (status == GSL_CONTINUE && iter < 500);
  
	gsl_multifit_covar(s->J, 0.0, covar);
  
#define FIT(i) gsl_vector_get(s->x, i)
	//#define ERR(i) sqrt(gsl_matrix_get(covar,i,i))
  
   //	{ 
      //		float chi = gsl_blas_dnrm2(s->f);  // ZSS2PT: Unused variable chi 
      //		float dof = n - p;   // ZSS2PT: Unused variable dof 
		//    float c = GSL_MAX_DBL(1, chi / sqrt(dof)); 
    
		//printf("chisq/dof = %g\n",  pow(chi, 2.0) / dof);
		//printf ("Dxx   = %.5f +/- %.5f\n", FIT(0), c*ERR(0));
		//printf ("Dyy   = %.5f +/- %.5f\n", FIT(1), c*ERR(1));
		//printf ("Dzz   = %.5f +/- %.5f\n", FIT(2), c*ERR(2));
		//printf ("Dxy   = %.10f +/- %.5f\n", FIT(3), c*ERR(3));
		//printf ("Dxz   = %.10f +/- %.5f\n", FIT(4), c*ERR(4));
		//printf ("Dyz   = %.10f +/- %.5f\n", FIT(5), c*ERR(5));
	//}
  
	//printf ("status = %s\n", gsl_strerror (status));
	// put the values where we need them
	for (i = 0; i < 6; i++)
		A[i] = FIT(i); 
  
	gsl_multifit_fdfsolver_free(s);
	gsl_matrix_free(covar);
	gsl_rng_free(r);

}


// adapted from GNU Scientific Library Reference Manual
// Edition 1.15, for GSL Version 1.15, (Galassi et al. 2011) 
int expb_f (const gsl_vector *x, void *data, gsl_vector *f)
{
	int j;
	size_t n = ((struct data *)data)->n;
	float *tt = ((struct data *)data)->tt;
	float *y = ((struct data *)data)->y;
	float *sigma = ((struct data *) data)->sigma;
	size_t i;
	float dotp,fact;
	float D[n];

	for( j=0 ; j<6 ; j++)
		D[j] = gsl_vector_get (x, j);
  
	for (i = 0; i < n; i++)
		{
			dotp = 0;
			for( j=0 ; j<6 ; j++) {
				if( j>2)
					fact = 2.;
				else
					fact = 1.;
				dotp+= fact * D[j] * tt[6*i+j];
			}
			// Model Yi = A * exp(-lambda * i) + b 
			//float t = i;
			float Yi = exp(-dotp);
			gsl_vector_set (f, i, (Yi - y[i])/sigma[i]);
		}
  
	return GSL_SUCCESS;
}

// adapted from GNU Scientific Library Reference Manual
// Edition 1.15, for GSL Version 1.15, (Galassi et al. 2011) 
int expb_df (const gsl_vector *x, void *data,  gsl_matrix *J)
{
	size_t j;
	size_t n = ((struct data *)data)->n;
	float *tt = ((struct data *)data)->tt;
	float *sigma = ((struct data *) data)->sigma;
	float dotp,fact;
	size_t i;
	float D[n];
  
	for( j=0 ; j<6 ; j++)
		D[j] = gsl_vector_get (x, j);
  
	for (i = 0; i < n; i++)
		{      
			// Jacobian matrix J(i,j) = dfi / dxj, 
			// where fi = (Yi - yi)/sigma[i],      
			//       Yi = exp(-b:D)   
			// and the xj are the parameters (D_xx,...) 
			dotp = 0;
			for( j=0 ; j<6 ; j++) {
				if( j>2)
					fact = 2.;
				else
					fact = 1.;
				dotp+= fact * D[j] * tt[6*i+j];
			}

			float e = exp(-dotp)/sigma[i];
      
			for( j=0 ; j<6 ; j++) {
				if( j>2)
					fact = 2.;
				else
					fact = 1.;
        
				gsl_matrix_set (J, i, j, -fact*tt[6*i+j]*e); 
			}

		}

	return GSL_SUCCESS;
}

// adapted from GNU Scientific Library Reference Manual
// Edition 1.15, for GSL Version 1.15, (Galassi et al. 2011) 
int expb_fdf(const gsl_vector *x, void *data, gsl_vector *f, gsl_matrix *J)
{
	expb_f (x, data, f);
	expb_df (x, data, J);
  
	return GSL_SUCCESS;
}


*/


// from Numerical Recipes by Press, Teukolsky, Vetterling and Flannery
// (indices adjusted to modern C)
void piksr2(int n, unsigned int arr[], int brr[])
//Sorts an array arr[1..n] into ascending numerical order, by straight
//  insertion, while making the corresponding rearrangement of the array
//  brr[1..n].
{
	int i,j;
	unsigned int a;
	int b;
	for (j=1;j<n;j++) { // Pick out each element in turn.
		a=arr[j];
		b=brr[j];
		i=j-1;
		while (i >= 0 && arr[i] > a) { // Look for the place to insert it.
			arr[i+1]=arr[i];
			brr[i+1]=brr[i];
			i--;
		}
		arr[i+1]=a; // Insert it.
		brr[i+1]=b;
	}
}

/*  OLD section for reading in bmatrix values-- switch now to use AFNI
format, for consistency:

{ Min = DSET_NVALS(dwset1);
		
      DWcheck = (int *)calloc(Min,sizeof(int)); 
      DWs = (float *)calloc(Min,sizeof(float)); //DWs
      // just use this to read in all
      grads = calloc(Min,sizeof(grads)); 
      for(i=0 ; i<Min ; i++) 
         grads[i] = calloc(6,sizeof(float)); 
      
      if( (grads == NULL) || (DWs == NULL) || (DWcheck == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(1254);
      }
      // Opening/Reading in FACT params
      if( (fin4 = fopen(gradmat_in, "r")) == NULL) {
         fprintf(stderr, "Error opening file %s.",gradmat_in);
         exit(19);
      }
      
      for(i=0 ; i<Min ; i++) {
         for(j=0 ; j<6 ; j++) {
            fscanf(fin4, "%f",&grads[i][j]);
         }
         // trace of bmatr is DWval
         DWval = grads[i][0]+grads[i][3]+grads[i][5];
         if( DWval<EPS_MASK ) {
            DWcheck[i] = 1; // flag b=0 ones
            Nb0+=1;
         }
         else {
            DWs[M] = DWval;
            if(DWval>DWmax)
               DWmax = DWval;// check for max DW, in case mult DWval
            M+=1;
         }			
      }
		  			
      fclose(fin4);
      if(Nb0<1)
         ERROR_exit("There appear to be no b=0 bricks!");

      Mj = (int) floor(jknife_val*M);
      if(Mj<7) 
         Mj=7;
      INFO_message("Input format: bmatrs. Number of DWI in inset=%d."
                   " Jackknife sample size=%d", M, Mj);
      MAXBAD = (int) (0.125*M);
      // for some really large numbers of gradients, >100, the number is 
      // just too big; Jan, 2014
      if( MAXBAD>10)
         MAXBAD = 10; 
      if(Mj-MAXBAD<6)
         MAXBAD=Mj-6;
			
      grads_dyad = calloc(M,sizeof(grads_dyad)); 
      for(i=0 ; i<M ; i++) 
         grads_dyad[i] = calloc(6,sizeof(float)); 
      // start to restructure DWIs in a copy, 
      // averaging all b=0 bricks together
      rearr_dwi = calloc(Nvox,sizeof(rearr_dwi)); 
      for(i=0 ; i<Nvox ; i++) 
         rearr_dwi[i] = calloc(M+1,sizeof(float)); 
			
      if( (grads_dyad == NULL) || (rearr_dwi == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(125);
      }
			
      M=0;
      for( j=0 ; j<Min ; j++) 
         if(DWcheck[j] == 0) {// is not a b=0
            grads_dyad[M][0] = grads[j][0];
            grads_dyad[M][1] = grads[j][3];
            grads_dyad[M][2] = grads[j][5];
            grads_dyad[M][3] = grads[j][1];
            grads_dyad[M][4] = grads[j][2];
            grads_dyad[M][5] = grads[j][4];
            M+=1; 
            for(i=0 ; i<Nvox ; i++)
               if( THD_get_voxel(insetPARS[2],i,0)>EPS_V)  // as below
                  rearr_dwi[i][M] = THD_get_voxel(dwset1,i,j);
         }
         else 
            for(i=0 ; i<Nvox ; i++)
               if( THD_get_voxel(insetPARS[2],i,0)>EPS_V) 
                  rearr_dwi[i][0]+= THD_get_voxel(dwset1,i,j);
			
      // because this was an average
      for(i=0 ; i<Nvox ; i++)
         rearr_dwi[i][0]/= 1.0*Nb0;
   }
 */
