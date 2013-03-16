/* 
   P. Taylor, March 2012
	
	This program estimates uncertainty of relevant DTI parameters using
	jackknife resampling 

	Sept. 2012: fixed some memory stuff.

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



struct data {
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

// from Numerical Recipes by Press, Teukolsky, Vetterling and Flannery
void piksr2(int n, unsigned int arr[], int brr[]); 

void usage_DWUncert(int detail) 
{
	printf(
"\n"
"  Use jackknifing to estimate uncertainty of DTI parameters which are\n"
"    important for probabilistic tractography on per voxel basis\n"
"\n"
"  Produces useful input for 3dProbTrackID, which does probabilistic\n"
"    tractography for GM ROIs in networks.\n"
"\n"
"  Jackknifing done with nonlinear fitting described in: \n"
"    Taylor PA, Biswal BB (2011). Geometric analysis of the b-dependent\n"
"    effects of Rician signal noise on diffusion tensor imaging\n"
"    estimates and determining an optimal b value. MRI 29:777–788.\n"
"\n"
"  COMMAND: 3dDWUncert -inset FILE -input [base of FA/MD/etc.] \\\n"
"           {-grads | -bmatr} FILE -prefix NAME -iters NUMBER \n"
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
"                      *_FA*, *_L1*, *_V1*, *_V2*, *_V3* .\n"
"    -grads  FILE     :file with 3 columns for x-, y-, and z-comps\n"
"                      of DW-gradients (which have unit magnitude).\n"
"                      NB: this option also assumes that only 1st DWI\n"
"                      subbrick has a b=0 image (i.e., all averaging of\n"
"                      multiple b=0 images has been done already); if such\n"
"                      is not the case, then you should convert your grads to\n"
"                      the bmatrix format and use `-bmatr'.\n"
"  OR\n"
"    -bmatr  FILE     :using this means that file with gradient info\n"
"                      is in b-matrix format, with 6 columns representing:\n"
"                      b_xx 2b_xy 2b_xz b_yy 2b_yz b_zz.\n"
"                      NB: here, bvalue per image is the trace of the bmatr,\n"
"                      bval = b_xx+b_yy+b_zz, such as 1000 s/mm^2. This\n"
"                      option might be used, for example, if multiple \n"
"                      b-values were used to measure DWI data; if TORTOISE\n"
"                      preprocessing has been employed, then its *.bmtxt\n"
"                      file can be used directly.\n"
"    -mask   MASK     :can include a mask within which to calculate uncert.\n"
"                      Otherwise, data should be masked already.\n"
"    -iters  NUMBER   :number of jackknife resample iterations,\n"
"                      e.g. 50.\n"
"    -csf_fa NUMBER   :number marking FA value of `bad' voxels, such as \n"
"                      those with S0 value <=mean(S_i), which breaks DT\n"
"                      assumptions due to, e.g., bulk/flow motion.\n"
"                      Default value of this matches 3dDWItoDT value of\n"
"                      csf_fa=0.012345678.\n"
"\n\n"
"  + EXAMPLE (NB: for now, you will need to have -inset as first option,\n"
"    i.e., the order of commands matters a bit for running successfully...):\n"
"      3dDWUncert \\\n"
"      -inset TEST_FILES/DTI/fin2_DTI_3mm_1+orig \\\n"
"      -prefix TEST_FILES/DTI/o.UNCERT \\\n"
"      -input TEST_FILES/DTI/DT \\\n"
"      -grads TEST_FILES/Siemens_d30_GRADS.dat \\\n"
"      -iters 50\n"
"   \n" );
	return;
}



int main(int argc, char *argv[]) {
	int i,j,k,n,b,ii,jj;
	int iarg;
	
	THD_3dim_dataset *insetV1 = NULL,*insetV2 = NULL,*insetV3 = NULL;
	THD_3dim_dataset *insetL1 = NULL;
	THD_3dim_dataset *insetFA = NULL;
	THD_3dim_dataset *dwset1=NULL; 
	THD_3dim_dataset *UNC_OUT=NULL;
	char *prefix="tracky" ;

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
	
	//	THD_3dim_dataset *dsetn;
	
	float *allS; 
	float **sortedout; // output for evals, evecs, Nvox x 12 
	float S0;
	int **StoreRandInd;
	float testdd[6];
	float *testS;
	float *bmatr;
	float **grads=NULL; // read in grads
	float *Wei2;
	float meanE1e2,meanE1e3,meanDelFA;
	float stdE1e2,stdE1e3,stdDelFA;
	float **OUT;
	unsigned int *randarr;
	int *ind;
	float *delE1e2,*delE1e1,*delE1e3, *delFA;
	int worstS, Nbad; // switch for testing bad ADC_i (i.e., S_i) 
	float aveADC, mostpos;
	//	int  NONLINLM=0; // only linear fitting for this approx.
	float E1e2,E1e1,E1e3;
	float temp,ang,md;
	int BADNESS;
	float randmagn;
	float CSF_FA = 0.012345678; // afni-set version

	char evalevecs[300]; 
	
	int Nj=10;
	int Mj=0;
	int MAXBAD=0;
	
	long seed;
	const gsl_rng_type * T;
	gsl_rng *r;
         /* ZSS2PT: Don't decalre variables in the middle of a scope
                Older compilers won't like it */
	int BMAT = 0; // switch about whether using grads or bmatr input 
                 // with potentially several b-vals
	int FOUND =-1;
	int Min=0,Nb0=0;
	int count;  /* ZSS2PT: Unused variable */
	int *DWcheck=NULL; /* ZSS2PT: Might be unused */
	float *DWs=NULL;  /* ZSS2PT: Might be unused */
	float DWval;
	float DWmax=0.0;
	float **grads_dyad=NULL; // will store grads in bmatr form
	float **rearr_dwi=NULL;
	// for testing names...
	char *postfix[4]={"+orig.HEAD\0",".nii.gz\0",".nii\0","+tlrc.HEAD\0"};
	

	
   // for random number generation
	srand(time(0));
	seed = time(NULL) ;
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc (T);
	gsl_rng_set (r, seed);
	

	mainENTRY("3dDWUncert"); machdep(); 
   
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************
	INFO_message("version: THETA");
	
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
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix = strdup(argv[iarg]) ;
			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");
			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-input") == 0 ){ // initial results of 3dDWItoDTI
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-input'");

			for( i=0 ; i<4 ; i++) {
				sprintf(in_FA,"%s_FA%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_FA)) {
					FOUND = i;
					break;
				}
			}
			
			insetFA = THD_open_dataset(in_FA) ;
			if( (insetFA == NULL ) || (FOUND==-1))
				ERROR_exit("Can't open dataset '%s': for FA.",in_FA);
			DSET_load(insetFA) ; CHECK_LOAD_ERROR(insetFA) ;
			Nvox = DSET_NVOX(insetFA) ;
			Dim[0] = DSET_NX(insetFA); Dim[1] = DSET_NY(insetFA); 
			Dim[2] = DSET_NZ(insetFA); 
			
			if( (Dim[0] != xmask1) || (Dim[1] != ymask1) || (Dim[2] != zmask1))
				ERROR_exit("Input dataset does not match both mask volumes!");

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_V1,"%s_V1%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_V1)) {
					FOUND = i;
					break;
				}
			}
			insetV1 = THD_open_dataset(in_V1);
			if( insetV1 == NULL ) 
				ERROR_exit("Can't open dataset '%s':V1",in_V1);
			DSET_load(insetV1) ; CHECK_LOAD_ERROR(insetV1) ;

			// probably could use the ending of the first data set to get
			// all... but not too bad to keep checking, I guess.

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_V2,"%s_V2%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_V2)) {
					FOUND = i;
					break;
				}
			}
			insetV2 = THD_open_dataset(in_V2);
			if( insetV2 == NULL ) 
				ERROR_exit("Can't open dataset '%s':V2",in_V2);
			DSET_load(insetV2) ; CHECK_LOAD_ERROR(insetV2) ;

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_V3,"%s_V3%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_V3)) {
					FOUND = i;
					break;
				}
			}
			insetV3 = THD_open_dataset(in_V3);
			if( insetV3 == NULL ) 
				ERROR_exit("Can't open dataset '%s':V3",in_V3);
			DSET_load(insetV3) ; CHECK_LOAD_ERROR(insetV3) ;

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_L1,"%s_L1%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_L1)) {
					FOUND = i;
					break;
				}
			}
			insetL1 = THD_open_dataset(in_L1);
			if( insetL1 == NULL ) 
				ERROR_exit("Can't open dataset '%s':L1",in_L1);
			DSET_load(insetL1) ; CHECK_LOAD_ERROR(insetL1) ;
			
			iarg++ ; continue ;
		}
	 
		if( strcmp(argv[iarg],"-grads") == 0 ){ 
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-grads'");
			
			// 3cols of grad vecs
			M = DSET_NVALS(dwset1)-1; // because 1st one is b=0
			Mj = (int) floor(0.7*M);
			if(Mj<7)
				Mj=7;
			INFO_message("Grads. Number of DWI in inset=%d. Jackknife sample size=%d",
							 M,Mj);
			MAXBAD = (int) (0.125*M);
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
			
			// Opening/Reading in FACT params
			if( (fin4 = fopen(argv[iarg], "r")) == NULL) {
				fprintf(stderr, "Error opening file %s.",argv[iarg]);
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
			
			iarg++ ; continue ;
		}
		else if( strcmp(argv[iarg],"-bmatr") == 0 ){ 
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-bmatr'");
			 
			// if b-matrix is being used as input,
			// have to find out still which one(s) are b=0.
			BMAT = 1;
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
			if( (fin4 = fopen(argv[iarg], "r")) == NULL) {
				fprintf(stderr, "Error opening file %s.",argv[iarg]);
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

			Mj = (int) floor(0.7*M);
			if(Mj<7)
				Mj=7;
			INFO_message("Bmatrs. Number of DWI in inset=%d. Jackknife sample size=%d",
							 M,Mj);
			MAXBAD = (int) (0.125*M);
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
						if( THD_get_voxel(insetL1,i,0)>EPS_V)  // as below
							rearr_dwi[i][M] = THD_get_voxel(dwset1,i,j);
				}
				else 
					for(i=0 ; i<Nvox ; i++)
						if( THD_get_voxel(insetL1,i,0)>EPS_V) 
							rearr_dwi[i][0]+= THD_get_voxel(dwset1,i,j);
			
			// because this was an average
			for(i=0 ; i<Nvox ; i++)
				rearr_dwi[i][0]/= 1.0*Nb0;

			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-mask") == 0 ){
			iarg++ ; if( iarg >= argc ) 
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
			iarg++ ; 
			if( iarg >= argc ) 
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
	 
		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
  
	if (iarg < 5) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}
  
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
	bmatr = (float *)calloc(6*Mj, sizeof(float)); //!!!!floatcheck
	testS = (float *)calloc(Mj,sizeof(float));
	delE1e1 = (float *)calloc(Nj, sizeof(float));
	delE1e2 = (float *)calloc(Nj, sizeof(float));
	delE1e3 = (float *)calloc(Nj, sizeof(float));
	delFA = (float *)calloc(Nj, sizeof(float));
  
	if( (grads == NULL) || (allS == NULL) || (sortedout == NULL) 
		 || (Wei2 == NULL) || (bmatr == NULL) || (testS == NULL)
		 || (StoreRandInd == NULL) || (OUT == NULL) ||  (delFA == NULL)
		 || (delE1e1 == NULL) || (delE1e2 == NULL) || (delE1e3 == NULL) ) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(12);
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
  
	for(i=0 ; i<Nvox ; i++) 
		if( ( HAVE_MASK && !(THD_get_voxel(MASK,i,0)>0) ) ||
			 ( (HAVE_MASK==0) && (THD_get_voxel(insetL1,i,0)<EPS_V) ) ) {
			for(j=0 ; j<6 ; j++) 
				OUT[j][i] = 0.0;
		}
		else if(THD_get_voxel(insetFA,i,0)==CSF_FA) {
			OUT[0][i] = OUT[2][i] = OUT[4][i] = 0.0; // zero mean
			OUT[1][i] = OUT[3][i] = PIo2; // max uncert, but prob doesn't matter.
			OUT[5][i] = 1.0; // max uncert
		}
		else {

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
									Wei2[b] = pow(10,6); // large weight, effectively zeros.
                  
									for(j=0 ; j<Mj ; j++) 
										for(k=0 ; k<6 ; k++) //  (H^T Wei)
											gsl_matrix_set(HTW,k,j,gsl_matrix_get(H,j,k)/Wei2[j]);
						
									// set up the inverse
									gsl_matrix_set_zero(HTWH);   
									gsl_matrix_set_zero(HTWHinv);
									gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,
														HTW,H,1.0,HTWH);
						
									// invert 
									j = gsl_linalg_LU_decomp(HTWH, P, &k);
									j = gsl_linalg_LU_invert(HTWH, P, HTWHinv);
                  
									// multiply pieces, and finally with Y to get dd (answer)
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
					randmagn = sqrt(pow(sortedout[i][3],2)+pow(sortedout[i][4],2)+
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
				E1e1 = sortedout[i][3]*THD_get_voxel(insetV1,i,0) 
					+ sortedout[i][4]*THD_get_voxel(insetV1,i,1)
					+ sortedout[i][5]*THD_get_voxel(insetV1,i,2);
				if (E1e1 <0)
					E1e1*=-1;
				E1e2 = sortedout[i][3]*THD_get_voxel(insetV2,i,0) 
					+ sortedout[i][4]*THD_get_voxel(insetV2,i,1)
					+ sortedout[i][5]*THD_get_voxel(insetV2,i,2);
				E1e3 = sortedout[i][3]*THD_get_voxel(insetV3,i,0) 
					+ sortedout[i][4]*THD_get_voxel(insetV3,i,1) 
					+ sortedout[i][5]*THD_get_voxel(insetV3,i,2);
		  
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
				delFA[jj] = sqrt(1.5*temp)-THD_get_voxel(insetFA,i,0);
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
			meanE1e2/= Nj;
			meanE1e3/= Nj;
			meanDelFA/= Nj;
		
			stdE1e2-= Nj*meanE1e2*meanE1e2;
			stdE1e3-= Nj*meanE1e3*meanE1e3;
			stdDelFA-= Nj*meanDelFA*meanDelFA;
      
			stdE1e2 = sqrt(stdE1e2/(Nj-1));
			stdE1e3 = sqrt(stdE1e3/(Nj-1));
			stdDelFA = sqrt(stdDelFA/(Nj-1));
		
			OUT[0][i] = meanE1e2;
			OUT[1][i] = stdE1e2;
			OUT[2][i] = meanE1e3;
			OUT[3][i] = stdE1e3;
			OUT[4][i] = meanDelFA;
			OUT[5][i] = stdDelFA;
		
		}
  
	sprintf(evalevecs,"%s_UNC",prefix); 
	UNC_OUT = EDIT_empty_copy( insetL1 ) ; 
  
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

	
	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************
    
  
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
	free(bmatr);
	free(testS);
  
	DSET_delete(insetFA);
	DSET_delete(insetL1);
	DSET_delete(insetV1);
	DSET_delete(insetV2);
	DSET_delete(insetV3);
	DSET_delete(dwset1);
  	DSET_delete(MASK);

	free(prefix);
	free(insetV1);
	free(insetV2);
	free(insetV3);
	free(insetL1);
	free(insetFA);
	free(dwset1);
  	free(MASK);

	for( i=0; i<6 ; i++) // free all
		free(OUT[i]);
	free(OUT);
	gsl_rng_free(r);




	return 0;
}



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
  
	{ 
		float chi = gsl_blas_dnrm2(s->f);  /* ZSS2PT: Unused variable chi */
		float dof = n - p;   /* ZSS2PT: Unused variable dof */
		//    float c = GSL_MAX_DBL(1, chi / sqrt(dof)); 
    
		//printf("chisq/dof = %g\n",  pow(chi, 2.0) / dof);
		//printf ("Dxx   = %.5f +/- %.5f\n", FIT(0), c*ERR(0));
		//printf ("Dyy   = %.5f +/- %.5f\n", FIT(1), c*ERR(1));
		//printf ("Dzz   = %.5f +/- %.5f\n", FIT(2), c*ERR(2));
		//printf ("Dxy   = %.10f +/- %.5f\n", FIT(3), c*ERR(3));
		//printf ("Dxz   = %.10f +/- %.5f\n", FIT(4), c*ERR(4));
		//printf ("Dyz   = %.10f +/- %.5f\n", FIT(5), c*ERR(5));
	}
  
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
			/* Model Yi = A * exp(-lambda * i) + b */
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
			/* Jacobian matrix J(i,j) = dfi / dxj, */
			/* where fi = (Yi - yi)/sigma[i],      */
			/*       Yi = exp(-b:D)   */
			/* and the xj are the parameters (D_xx,...) */
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





// from Numerical Recipes by Press, Teukolsky, Vetterling and Flannery
// (indices adjusted to modern C)
void piksr2(int n, unsigned int arr[], int brr[])
/*Sorts an array arr[1..n] into ascending numerical order, by straight
  insertion, while making the corresponding rearrangement of the array
  brr[1..n].*/
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
