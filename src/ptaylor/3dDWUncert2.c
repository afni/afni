/* 
   P. Taylor, March 2012

   use jackknifing to estimate uncertainty of DTI parameters which are
   important for probabilistic tractography on per voxel basis

   jackknifing done with nonlinear fitting described in 
   Taylor & Biswal (2011)

   produces useful input for 3dProbTrackID, which does probabilistic
   tractography for GM ROIs in networks

   since this is just for uncert, why do nonlinear at all??? it's slow...

 */



// !!! how to require these options?


// !!!! need to use brick factors??



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include "mrilib.h"     
#include "3ddata.h"     
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
#include "DoTrackit.h"

// from <editvol.h>
#define EDIT_DSET_ORIENT(ds,ox,oy,oz)                              \
 do{ THD_ivec3 orixyz ;                                            \
     LOAD_IVEC3( orixyz , (ox),(oy),(oz) ) ;                       \
     EDIT_dset_items( (ds) , ADN_xyzorient , orixyz , ADN_none ) ; \
 } while(0)

// from 3dUndump.c
#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

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

void usage_DWUncert1(int detail) 
{
  printf(
"\n Use jackknifing to estimate uncertainty of DTI parameters which are\n"
"   important for probabilistic tractography on per voxel basis\n"
"\n"
"   Jackknifing done with nonlinear fitting described in \n"
"   Taylor & Biswal (2011)\n"
"\n"
"   Produces useful input for 3dProbTrackID, which does probabilistic\n"
"   tractography for GM ROIs in networks\n"
"\n"
"  Current output:\n"
"      + a single file with 6 subbricks, containing uncertainty information.\n"
"        the bricks are in the following order:\n"
"            - bias of e1 in direction of e2\n"
"            - stdev of e1 in direction of e2\n"
"            - bias of e1 in direction of e3\n"
"            - stdev of e1 in direction of e3\n"
"            - bias of FA \n"
"            - stdev of FA\n"
"\n"
"  Required inputs:\n"
"    -inset  [FILE]     File with bo and M DWI subbricks (input to 3dDWtoDTI)\n"
"    -prefix [FILENAME] Will be prefix out output file name\n"
"    -input  [FILEBASE] prefix of output from 3dDWtoDTI   \n"
"    -grads  [FILE]     file with 3cols, for x-, y-, and z-comps of grads\n"
"    -iters  [NUMBER]   number of jackknife resample iterations\n"
"\n"
"\n Example usage:\n"
"3dDWUncert2 \\\n"
"     -inset TEST_FILES/DTI/fin2_DTI_3mm_1+orig \\\n"
"     -prefix TEST_FILES/DTI/o.UNCERT_TESTb \\\n"
"     -input TEST_FILES/DTI/DT \\\n"
"     -grads TEST_FILES/Siemens_d30_GRADS.dat \\\n"
"     -iters 50\n"
"   \n" );
  return;
}



int main(int argc, char *argv[]) {
  int i,j,k,m,n,aa,b,ii,jj,kk,mm,gg,nn,hh,bb,cc;
  int iarg;

  THD_3dim_dataset *insetV1 = NULL,*insetV2 = NULL,*insetV3 = NULL;//@@
  THD_3dim_dataset *insetL1 = NULL,*insetDT = NULL,*insetL3 = NULL;
  THD_3dim_dataset *insetFA = NULL;
  THD_3dim_dataset *dwset1=NULL; 
  THD_3dim_dataset *UNC_OUT=NULL;
  char *prefix="tracky" ;

  char in_FA[THD_MAX_NAME];
  char in_V1[THD_MAX_NAME];
  char in_V2[THD_MAX_NAME];
  char in_V3[THD_MAX_NAME];
  char in_DT[THD_MAX_NAME];
  char in_L1[THD_MAX_NAME];
  char in_L2[THD_MAX_NAME];
  char in_L3[THD_MAX_NAME];

  int xmask1=0,ymask1=0,zmask1=0;

  FILE *fin4, *fin1, *fout1;
  int Nvox=-1;   // tot number vox
  int Dim[3]; // dim in each dir
  int M=0;
  // like 3dDWtoDTI, we don't need bval--
  // just leave this here in case we ever want it later.
  int bval=1.0; 

  char READS_ch;
  short int READS_sh; 
  int READS_in;
  float READS_fl;

  char dset_or[3] = "RAI";
  char voxel_order[3] = "---";
  THD_3dim_dataset *dsetn;

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
  int  NONLINLM=0; 
  float E1e2,E1e1,E1e3;
  float dummy,temp,ang,md;
  int BADNESS;
  float randmagn;

  char evalevecs[THD_MAX_NAME]; 
  float dummy_fl;

  int Nj=0;
  int Mj=0;
  int MAXBAD=0;

  long seed;
  const gsl_rng_type * T;
  gsl_rng *r;
  // for random number generation
  srand(time(0));
  seed = time(NULL) ;
  gsl_rng_env_setup();
  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  gsl_rng_set (r, seed);

  mainENTRY("3dDWUncert2"); machdep(); 
   
  // ****************************************************************
  // ****************************************************************
  //                    load AFNI stuff
  // ****************************************************************
  // ****************************************************************
  if (argc == 1) { usage_DWUncert1(1); exit(0); }

  iarg = 1;
  while( iarg < argc && argv[iarg][0] == '-' ){
    if( strcmp(argv[iarg],"-help") == 0 || 
	strcmp(argv[iarg],"-h") == 0 ) {
      usage_DWUncert1(strlen(argv[iarg])>3 ? 2:1);
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
      
      M = DSET_NVALS(dwset1)-1; // because 1st one is b=0
      
      grads = malloc(M*sizeof(grads)); 
      for(i=0 ; i<M ; i++) 
	grads[i] = malloc(3*sizeof(float)); 
      
      Mj = (int) floor(0.75*M);
      INFO_message("Number of DWI in inset=%d. Jackknife sample size=%d",
		   M,Mj);

      MAXBAD = (int) (0.25*M);
      if(Mj-MAXBAD<6)
	MAXBAD=Mj-6;

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
      sprintf(in_FA,"%s_FA+orig", argv[iarg]); 

      insetFA = THD_open_dataset(in_FA) ;
      if( insetFA == NULL ) 
	ERROR_exit("Can't open dataset '%s':FA",in_FA);
      DSET_load(insetFA) ; CHECK_LOAD_ERROR(insetFA) ;
      Nvox = DSET_NVOX(insetFA) ;
      Dim[0] = DSET_NX(insetFA); Dim[1] = DSET_NY(insetFA); 
      Dim[2] = DSET_NZ(insetFA); 

      if( (Dim[0] != xmask1) || (Dim[1] != ymask1) || (Dim[2] != zmask1))
	ERROR_exit("Input dataset does not match both mask volumes!");
	
      
      // this stores the original data file orientation for later use,
      // as well since we convert everything to RAI temporarily, as
      // described below
      voxel_order[0]=ORIENT_typestr[insetFA->daxes->xxorient][0];
      voxel_order[1]=ORIENT_typestr[insetFA->daxes->yyorient][0];
      voxel_order[2]=ORIENT_typestr[insetFA->daxes->zzorient][0];
      
      sprintf(in_V1,"%s_V1+orig", argv[iarg]); 
      insetV1 = THD_open_dataset(in_V1);
      if( insetV1 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V1",in_V1);
      DSET_load(insetV1) ; CHECK_LOAD_ERROR(insetV1) ;

      sprintf(in_DT,"%s_DT+orig", argv[iarg]); 
      insetDT = THD_open_dataset(in_DT);
      if( insetDT == NULL ) 
	ERROR_exit("Can't open dataset '%s':DT",in_DT);
      DSET_load(insetDT) ; CHECK_LOAD_ERROR(insetDT) ;

      sprintf(in_V2,"%s_V2+orig", argv[iarg]); 
      insetV2 = THD_open_dataset(in_V2);
      if( insetV2 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V2",in_V2);
      DSET_load(insetV2) ; CHECK_LOAD_ERROR(insetV2) ;

      sprintf(in_V3,"%s_V3+orig", argv[iarg]); 
      insetV3 = THD_open_dataset(in_V3);
      if( insetV3 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V3",in_V3);
      DSET_load(insetV3) ; CHECK_LOAD_ERROR(insetV3) ;

      sprintf(in_L1,"%s_L1+orig", argv[iarg]); 
      insetL1 = THD_open_dataset(in_L1);
      if( insetL1 == NULL ) 
	ERROR_exit("Can't open dataset '%s':L1",in_L1);
      DSET_load(insetL1) ; CHECK_LOAD_ERROR(insetL1) ;

      iarg++ ; continue ;
    }

    if( strcmp(argv[iarg],"-grads") == 0 ){ // 3cols of grad vecs
      iarg++ ; 
      if( iarg >= argc ) 
	ERROR_exit("Need argument after '-grads'");
      
      // Opening/Reading in FACT params
      if( (fin4 = fopen(argv[iarg], "r")) == NULL) {
	fprintf(stderr, "Error opening file %s.",argv[iarg]);
	exit(19);
      }

      for(i=0 ; i<M ; i++) 
	for(j=0 ; j<3 ; j++) 
	  fscanf(fin4, "%f",&grads[i][j]);
      fclose(fin4);

      iarg++ ; continue ;
    }

     if( strcmp(argv[iarg],"-iters") == 0 ){
      iarg++ ; 
      if( iarg >= argc ) 
	ERROR_exit("Need integer argument after '-iters'");
	
      Nj = atoi(argv[iarg]);
      printf("\n%d",Nj);
      if(Nj <= 2)
	ERROR_exit("(Far) too few iterations.");
      iarg++ ; continue ;
     }


     ERROR_message("Bad option '%s'\n",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);
  }
  
  if (iarg < 4) {
   ERROR_message("Too few options. Try -help for details.\n");
   exit(1);
  }


    dsetn = r_new_resam_dset(insetFA, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(insetFA); 
    insetFA=dsetn;
    dsetn=NULL;

    dsetn = r_new_resam_dset(insetV1, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(insetV1); 
    insetV1=dsetn;
    dsetn=NULL;

    dsetn = r_new_resam_dset(insetDT, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(insetDT); 
    insetDT=dsetn;
    dsetn=NULL;

    dsetn = r_new_resam_dset(insetV2, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(insetV2); 
    insetV2=dsetn;
    dsetn=NULL;

    dsetn = r_new_resam_dset(insetV3, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(insetV3); 
    insetV3=dsetn;
    dsetn=NULL;

    dsetn = r_new_resam_dset(insetL1, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(insetL1); 
    insetL1=dsetn;
    dsetn=NULL;

    dsetn = r_new_resam_dset(dwset1, NULL, 0.0, 0.0, 0.0,
			     dset_or, RESAM_NN_TYPE, NULL, 1, 0);
    DSET_delete(dwset1); 
    dwset1=dsetn;
    dsetn=NULL;


    // just general array storage creation stuff
    StoreRandInd = malloc(Nj*sizeof(StoreRandInd)); 
    for(i=0 ; i<Nj ; i++) 
      StoreRandInd[i] = malloc(Mj*sizeof(int)); 
    randarr = (unsigned int *)malloc(M*sizeof(unsigned int)); 
    ind = (int *)malloc( M * sizeof(int)); 
    Wei2 = (float *)malloc(Mj * sizeof(float));
    allS = (float *)malloc(M*sizeof(float)); 
    OUT = malloc(6*sizeof(OUT)); 
    for(i=0 ; i<6 ; i++) 
      OUT[i] = malloc( Nvox*sizeof(float)); 
    sortedout = malloc((Nvox)*sizeof(sortedout)); 
    for(i=0 ; i<Nvox ; i++) 
      sortedout[i] = malloc(12*sizeof(float)); 
    bmatr = (float *)malloc(6*Mj * sizeof(float)); //!!!!floatcheck
    testS = (float *)malloc(Mj * sizeof(float));
    delE1e1 = (float *)malloc(Nj * sizeof(float));
    delE1e2 = (float *)malloc(Nj * sizeof(float));
    delE1e3 = (float *)malloc(Nj * sizeof(float));
    delFA = (float *)malloc(Nj * sizeof(float));

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
      if(THD_get_voxel(dwset1,i,0)<EPS_MASK) {
	for(j=0 ; j<6 ; j++) 
	  OUT[j][i] = 0.0;
      }
      else {
	S0 = 1.0*THD_get_voxel(dwset1,i,0);
	for(j=0 ; j<M ; j++) {
	  if( THD_get_voxel(dwset1,i,j+1)>=S0 )
	    allS[j] = 0.99*S0;
	  else if( THD_get_voxel(dwset1,i,j+1) <=0 ) 
	    allS[j] = 1.;
	  else //scale all signals
	    allS[j] = THD_get_voxel(dwset1,i,j+1)*1.0/S0; 
	}

	for( jj=0 ; jj<Nj ; jj++) {
	  BADNESS = 0;
	  
	  //  H matr of grads
	  for(ii=0 ; ii<Mj ; ii++) {
	    for(j=0 ; j<3 ; j++) 
	      gsl_matrix_set(H,ii,j,grads[StoreRandInd[jj][ii]][j]
			     *grads[StoreRandInd[jj][ii]][j]);
	    gsl_matrix_set(H,ii,3,2*grads[StoreRandInd[jj][ii]][0]
			   *grads[StoreRandInd[jj][ii]][1]);
	    gsl_matrix_set(H,ii,4,2*grads[StoreRandInd[jj][ii]][0]
			   *grads[StoreRandInd[jj][ii]][2]);
	    gsl_matrix_set(H,ii,5,2*grads[StoreRandInd[jj][ii]][1]
			   *grads[StoreRandInd[jj][ii]][2]);
	    //not +1 in index!
	    gsl_vector_set(Y,ii,-log( allS[StoreRandInd[jj][ii]])/bval);
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
		      else {// i.e., have eliminated bads- they are recorded
			// in Wei2 (or latest one will be shortly), 
			// so we can exit loop
			worstS = b;
			b = Mj+3; // to exit loop searching for bad ones
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
	  // so, we can either go on to LM loop, or stop here and just 
	  // keep LLS results -- should always go on to nonlinear!
	  	  
	  // start the LM loop of calculating
	  if( NONLINLM ==1) {
	    
	    for(j=0 ; j<Mj ; j++) {
	      bmatr[6*j+0] = bval*pow(grads[StoreRandInd[jj][j]][0],2);
	      bmatr[6*j+1] = bval*pow(grads[StoreRandInd[jj][j]][1],2);
	      bmatr[6*j+2] = bval*pow(grads[StoreRandInd[jj][j]][2],2);
	      bmatr[6*j+3] = bval*grads[StoreRandInd[jj][j]][0]*
		grads[StoreRandInd[jj][j]][1];
	      bmatr[6*j+4] = bval*grads[StoreRandInd[jj][j]][0]*
		grads[StoreRandInd[jj][j]][2];
	      bmatr[6*j+5] = bval*grads[StoreRandInd[jj][j]][2]*
		grads[StoreRandInd[jj][j]][1];
	      testS[j] = allS[StoreRandInd[jj][j]]; //not +1 in index!
	    }
	    
	    for(j=0 ; j<Mj ; j++) 
	      Wei2[j] = 1.; // part 2: default per voxel: no bad signals
	    
	    // always use that result to make initial guess for LM;  
	    // we will store and return that guess.
	    
	    // FIT!
	    LevMarq(testS, bmatr, Mj, testdd, Wei2);

	    // make sure eigenvalues are all positive
	    for(j=0 ; j<3 ; j++) //diagonal elements
	      gsl_matrix_set(testD,j,j,testdd[j]);
	    gsl_matrix_set(testD,0,1,testdd[3]);
	    gsl_matrix_set(testD,1,0,testdd[3]);
	    gsl_matrix_set(testD,0,2,testdd[4]);
	    gsl_matrix_set(testD,2,0,testdd[4]);
	    gsl_matrix_set(testD,1,2,testdd[5]);
	    gsl_matrix_set(testD,2,1,testdd[5]);
	    
	    j = gsl_eigen_symm(testD, Eval, EigenV);
	    
	    { // testing/fixing badness in signals
	      if( (gsl_vector_get(Eval,0) <= MINEIG) 
		  || (gsl_vector_get(Eval,1) <= MINEIG) 
		  || (gsl_vector_get(Eval,2) <= MINEIG) ) {
		
		Nbad = 0;
		b = 0;
		
		while( (Nbad < MAXBAD) && (b<Mj+3) ) {
		  // part 1: so, default per voxel is no bad signals
		  mostpos = -1.e10; // to be baddest...
		  worstS = -1; 

		  // iterate through for eliminating potential 'bad'
		  for( b=0 ; b<Mj ; b++) {
		    if( Wei2[b] < 2) { 
		      // in case we've already found 1 bad, to skip it now
		      Wei2[b] = pow(10,6); // large weight, effectively zeros.
		      
		      // FIT with a test value weighted out
		      LevMarq(testS, bmatr, Mj, testdd, Wei2);
		      
		      // make sure eigenvalues are all positive
		      for(j=0 ; j<3 ; j++) //diagonal elements
			gsl_matrix_set(testD,j,j,testdd[j]);
		      gsl_matrix_set(testD,0,1,testdd[3]);
		      gsl_matrix_set(testD,1,0,testdd[3]);
		      gsl_matrix_set(testD,0,2,testdd[4]);
		      gsl_matrix_set(testD,2,0,testdd[4]);
		      gsl_matrix_set(testD,1,2,testdd[5]);
		      gsl_matrix_set(testD,2,1,testdd[5]);
		      
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
		      else {// i.e., have eliminated bads- 
			// they are recorded in Wei2 (or latest one will 
			// be shortly), so we can exit loop
			worstS = b;
			b = Mj+3; // to exit loop searching for bad ones
		      }
		      
		    }
		  } // endfor
		  
		  // if we make it to end without finding bad S, eliminate
		  // worst and then go back to find another bad;  
		  // reset tests to find, again
		  Wei2[worstS] = pow(10,6);
		  Nbad+=1;
		  
		} // end while
	      } // endif
	    } // endsec
	    
	    { // 2nd time around
	      // if we have so many many bad, then call voxel corrupted, and
	      // make it a sphere of average ADC and move on
	      if((Nbad >= MAXBAD) && (b<Mj+3)) {
		//i.e., full amount of bad, and still didn't get all pos evals

		BADNESS = 1;		
	      }
	    }
	  }
	  
	  // done with BOTH lin fit and nonlinear fit.  
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
	  
	  // done calculating; now store for later calculation of mean, stdev.

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
	    + sortedout[i][4]*THD_get_voxel(insetV2,i,1) 
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
      // just get one of right dimensions!
      UNC_OUT = EDIT_empty_copy( insetDT ) ; 
      EDIT_dset_items(UNC_OUT,
		      ADN_datum_all , MRI_float , 
		      ADN_prefix    , evalevecs,
		      ADN_none ) ;
      if( THD_is_ondisk(DSET_HEADNAME(UNC_OUT)) )
	ERROR_exit("Can't overwrite existing dataset '%s'",
		   DSET_HEADNAME(UNC_OUT));
      
      
      EDIT_DSET_ORIENT(UNC_OUT, // have to make sure this way is fine enough!!!
		       ORCODE(voxel_order[0]),
		       ORCODE(voxel_order[1]),
		       ORCODE(voxel_order[2]));
      
      for( n=0; n<6 ; n++)
	EDIT_substitute_brick(UNC_OUT, n, MRI_float, OUT[n]);
      
      EDIT_BRICK_LABEL(UNC_OUT,0,"bias_E1e2");      
      EDIT_BRICK_LABEL(UNC_OUT,1,"std_E1e2");      
      EDIT_BRICK_LABEL(UNC_OUT,2,"bias_E1e3");      
      EDIT_BRICK_LABEL(UNC_OUT,3,"std_E1e3");      
      EDIT_BRICK_LABEL(UNC_OUT,4,"bias_FA");      
      EDIT_BRICK_LABEL(UNC_OUT,5,"std_FA");      

      THD_update_statistics( UNC_OUT );
      THD_write_3dim_dataset(NULL, NULL, UNC_OUT, True);
      //      DSET_delete(UNC_OUT); // why not delete??!!!

    
    
    
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
    }
    free(sortedout);
    free(allS);

    
    /*
    for( i=0; i<6 ; i++) 
      free(OUT[i]);
      free(OUT);*/

    
    for( i=0; i<Nj ; i++) 
      free(StoreRandInd[i]);
    free(StoreRandInd);
    
    for( i=0; i<M ; i++) 
      free(grads[i]);
    
    free(delE1e1);
    free(delE1e3);
    free(delE1e2);
    free(delFA);
        
    free(grads);
    free(Wei2);
    free(bmatr);
    free(testS);
    
    // printf("\n\n\t  Done done.\n\n");
    
    // !!! need to free afni-sets?
    DSET_delete(insetFA);
    DSET_delete(insetL1);
    DSET_delete(insetV1);
    DSET_delete(insetV2);
    DSET_delete(insetV3);
    DSET_delete(dwset1);
    
    free(prefix);
    free(insetV1);
    free(insetV2);
    free(insetV3);
    free(insetL1);
    free(insetFA);
    free(dwset1);
    
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
    float chi = gsl_blas_dnrm2(s->f);
    float dof = n - p;
    float c = GSL_MAX_DBL(1, chi / sqrt(dof)); 
    
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
