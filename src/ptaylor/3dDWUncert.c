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

   Oct, 2016: 
       + totally redone inner workings 
       + OpenMPed it
       + should work when no b=0 is there
       + should be much faster

   Oct, 2016b:
       + made parallel not distribute zeros -> more full parallel
       + helpful time messaging of completion
   
*/


// SCALE BY 1000??

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
//#include <rsfc.h>    
//#include <gsl/gsl_rng.h>
#include "DoTrackit.h"
#include "colorbasic.h"
#include "readglob.h"
#include "basic_boring.h"
#include "diffusiony.h"
#include "gsl/gsl_permutation.h"
#include <gsl/gsl_types.h>
#include <gsl/gsl_permute.h>

#ifdef USE_OMP
#include <omp.h>
#endif

/*
  Do the uncertainty calc-- now with OpenMP
*/
 
int Calc_DTI_uncert( float **UU,
                     int *minds, //short *mskd,
                     int Nvox,
                     THD_3dim_dataset **PARS,
                     THD_3dim_dataset **VECS,
                     THD_3dim_dataset *DWI,
                     float **bseven,
                     int **StoreRandInd,
                     int Mj,
                     int Nj,
                     int MAXBAD,
                     int M,
                     int Ntodo
                     );


/*int test_func( float **UU, 
               short *mskd,
               int Nvox);
*/

void usage_3dDWUncert(int detail) 
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
"  This version has been reprogrammed to include parallelized running via\n"
"  OpenMP (as of Oct, 2016).  So, it has the potential to run a lot more \n"
"  quickly, assuming you have an OpenMPable setup for AFNI. The types/formats\n"
"   of inputs and outputs have not changed from before.\n"
"\n"
"****************************************************************************\n"
"\n"
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
"  COMMAND: 3dDWUncert -inset FILE -input [base of FA/MD/etc.] \\\n"
"           {-grads | -bmatrix_Z} FILE -prefix NAME -iters NUMBER \n"
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
"\n"
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
"\n"
"    -iters  NUMBER   :number of jackknife resample iterations,\n"
"                      e.g. 50.\n"
"    -mask   MASK     :can include a mask within which to calculate uncert.\n"
"                      Otherwise, data should be masked already.\n"
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
   int i,j,k,m,n,mm;
   int Ntodo=0;
   int iarg;

   THD_3dim_dataset *dwiset1 = NULL;
   THD_3dim_dataset *MASK=NULL;
   char *prefix="PREFIX" ;
   // char in_name[300];
   THD_3dim_dataset *outset = NULL;

   THD_3dim_dataset **insetPARS=NULL; // [0] FA, [1] MD, [2] L1
   THD_3dim_dataset **insetVECS=NULL; // [0] V1, [1] V2, [2] V3

   char *infix=NULL;
   int hardi_pref_len=0;
   int pref_offset = 0;
   char *dti_listname=NULL;
	NI_element *nel=NULL;

   MRI_IMAGE *flim0=NULL,*flim=NULL;

   // FILE *fout0, *fout1;

   int Nvox=-1;   // tot number vox
   int Dim[4]={0,0,0,0};
   short *mskd=NULL;     // define mask of where time series are nonzero
   int  *minds=NULL;     // the mask as a list of inds, to better
                         // divide amongst procs
   float **unc_out=NULL; 

   int Nj=300;
   float CALC_THR_FA=-1.;    // DEFAULT: value for new option of only
                             // calculating uncert in some voxels,
                             // save where CSF/GM would cost time AND
                             // money!
   float CSF_FA = 0.012345678; // afni-set version
   float csf_val = 1.;

   int BMAT = -1;
   int Ngrad=0, Ncol=0;

   int CHOOSE_SEED=-1;

   float jknife_val = 0.7; // default jackknife fraction
   int Njkout = 0, count_jkout = 0, CI_val=0;  // just options for testing

   float **bseven=NULL;
   int Mj=0, MAXBAD=0;
	int **StoreRandInd=NULL;

   mainENTRY("3d3dDWUncert"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   // INFO_message("version: NU");
	
   /** scan args **/
   if (argc == 1) { usage_3dDWUncert(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_3dDWUncert(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      // NO ARG:
      //if( strcmp(argv[iarg],"-TESTING") == 0) {
      //   TEST_OK=1;
      //   iarg++ ; continue ;
      //}

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-prefix'");
         prefix = strdup(argv[iarg]) ;
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '-prefix'");
         iarg++ ; continue ;
      }
	 
      if( strcmp(argv[iarg],"-inset") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-inset'");

         dwiset1 = THD_open_dataset(argv[iarg]);
         if( (dwiset1 == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

         DSET_load(dwiset1); CHECK_LOAD_ERROR(dwiset1);

         Nvox = Basic_Info_Dim_and_Nvox( dwiset1, 
                                         Dim, 4);

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
         flim0 = mri_read_1D (strdup(argv[iarg]));
         if (flim0 == NULL) 
            ERROR_exit("Error reading gradient vector file");
         flim = mri_transpose(flim0);
         mri_free(flim0);

			iarg++ ; continue ;
		}
		else if( strcmp(argv[iarg],"-bmatrix_Z") == 0 ){ 
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-bmatrix_Z'");
         
			// if b-matrix is being used as input,
			// have to find out still which one(s) are b=0.
         BMAT = 1;
         flim0 = mri_read_1D (strdup(argv[iarg]));
         if (flim0 == NULL) 
            ERROR_exit("Error reading matrix file");
         flim = mri_transpose(flim0);
         mri_free(flim0);

			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");

         MASK = THD_open_dataset(argv[iarg]);
         if( MASK == NULL )
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

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
                      "%.5f",CALC_THR_FA);

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
	
   // ****************************************************************
   // ****************************************************************
   // ****************************************************************
   // ****************************************************************

   // TEST BASIC INPUT PROPERTIES
   if (iarg < 3) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }

   if( infix && dti_listname ) {
		ERROR_message("Use *either* '-input' or '-input_list'.\n");
		exit(2);
	}

   if( MASK ) {
      i = Basic_Compare_DSET_dims( dwiset1, MASK, 3 );
   }

   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   // ------------------------ grads/bmatr ---------------------------

   Ncol = flim->nx;     // assoc with either grad or bmatr
   Ngrad  = flim->ny;   // number of grad/bmatr entered
   INFO_message("In matrix/grad file: Ngrad = %d, Ncol = %d",Ngrad, Ncol);

   // make sure everything is consistent with input type and with
   // number of input DWIs
   if(BMAT==0) { 
      if(Ncol!=3) 
         ERROR_exit("There are %d columns, but should be 3 for"
                    "'-grad ...' input!", Ncol);
      if(Ngrad!=Dim[3]-1) 
         ERROR_exit("There are %d rows, but should be %d-1 (one less"
                    "than Nvol) for '-grad ...' input!", 
                    Ngrad, Dim[3]);
   }
   if( BMAT==1) {
      if(Ncol!=6) 
         ERROR_exit("There are %d columns, but should be 6 for"
                    "'-bmatrix_Z ...' input!", Ncol);
      if(Ngrad!=Dim[3]) 
         ERROR_exit("There are %d rows, but should be %d (same as"
                    "in Nvol) for '-bmatrix_Z ...' input!", 
                    Ngrad, Dim[3]);
   }

   // in long term, need Nx7 array
   bseven = calloc( Dim[3], sizeof(bseven));
   for( i=0 ; i<Dim[3] ; i++) 
      bseven[i] = calloc( 7, sizeof(float)); 
   if( (bseven == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(32);
   }
   
   if(BMAT==0) 
      // preserves magn
      i = Basic_Grads_to_B7( bseven,
                             flim,
                             Dim[3]-1); // Ngrad = Nvol - 1
   else if(BMAT==1)
      i = Basic_Bmats_to_B7( bseven,
                             flim,
                             Dim[3]); // Nbmat = Nvol
   
   // the full Bmatrix should now be set

   mri_free(flim);

   // set up numbers for jackknife subsampling
   Mj = (int) floor(jknife_val*Dim[3]);
   if(Mj<7) 
      Mj=7;
   INFO_message("Input format: bmatrs. Number of DWI in inset=%d."
                " Jackknife sample size=%d", Dim[3], Mj);
   MAXBAD = (int) (0.125*Dim[3]);
   // for some really large numbers of gradients, >100, the number is 
   // just too big; Jan, 2014
   if( MAXBAD>10)
      MAXBAD = 10; 
   if(Mj-MAXBAD<6)
      MAXBAD=Mj-6;

   // -------------------- rand. stuff-------------------------------
   
   // will store the indices to select random subset of grads
   StoreRandInd = calloc(Nj,sizeof(StoreRandInd)); 
	for(i=0 ; i<Nj ; i++) 
		StoreRandInd[i] = calloc(Mj,sizeof(int)); 
   if(  ( StoreRandInd == NULL) ) {
      fprintf(stderr,"\n\nMemAlloc failure: prepping index selection.\n\n");
      exit(12);
   }

   // this makes the array of index selections
   i = Make_Jackknife_Inds_keep0th( StoreRandInd, 
                                    Dim[3],
                                    Mj,
                                    Nj,
                                    CHOOSE_SEED);

   //Show_2DMatrix_Ints(StoreRandInd,Nj,Mj);

   // -------------------- glob for data sets ----------------------   

   insetPARS = (THD_3dim_dataset **)calloc(N_DTI_SCAL, 
                                           sizeof(THD_3dim_dataset *));
   insetVECS = (THD_3dim_dataset **)calloc(N_DTI_VECT, 
                                           sizeof(THD_3dim_dataset *)); 
   if(  (insetPARS == NULL) || (insetVECS == NULL) ) {
      fprintf(stderr,"\n\nMemAlloc failure: prepping to store sets.\n\n");
      exit(13);
   }

   if(infix)                                     // glob
      glob_for_DTI( infix,
                    insetPARS,
                    insetVECS,
                    hardi_pref_len );
   else if(dti_listname)                         // listed
      list_for_DTI( infix,
                    insetPARS,
                    insetVECS  );
   else{                                         // ooops
      for( i=0 ; i<N_DTI_SCAL ; i++){
         DSET_delete(insetPARS[i]);
         free(insetPARS[i]);
      }
      free(insetPARS);
      
      for( i=0 ; i<N_DTI_VECT ; i++){
         DSET_delete(insetVECS[i]);
         free(insetVECS[i]);
      }
      free(insetVECS);
      ERROR_message("Failed to read input.");
   }

   // check to match dimensions-- one comp should be good enough
   i = Basic_Compare_DSET_dims( dwiset1, 
                                insetPARS[0],
                                3);
   i = Basic_Compare_DSET_dims( dwiset1, 
                                insetVECS[0],
                                3);

   // -------------------- 

   mskd = (short *)calloc(Nvox,sizeof(short)); 
   
   unc_out = calloc( 6, sizeof(unc_out));   // major output set
   for( i=0 ; i<6 ; i++) 
      unc_out[i] = calloc( Nvox, sizeof(float)); 

   if( (mskd == NULL) || (unc_out == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(4);
   } 


   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************
	

   // go through once: define data vox
   INFO_message("First pass masking: user-defined and nonzero values");
   for( i=0 ; i<Nvox ; i++ ) {
      if(THD_get_voxel(insetPARS[0],i,0) >= CALC_THR_FA) {
         if( MASK ) {
            if( THD_get_voxel(MASK,i,0)>0 )
               mskd[i] = 1;
         }
         else
            if( fabs(THD_get_voxel(dwiset1,i,0))+
                fabs(THD_get_voxel(dwiset1,i,1))+
                fabs(THD_get_voxel(dwiset1,i,2))+
                fabs(THD_get_voxel(dwiset1,i,3))+
                fabs(THD_get_voxel(dwiset1,i,4)) > EPS_V)
               mskd[i] = 1;
      }
   }
   
   INFO_message("Second pass masking: default values for 'bad' voxels");
   for( i=0 ; i<Nvox ; i++ ) 
      if( mskd[i] ) {
         if( fabs(THD_get_voxel(insetPARS[0],i,0) - CSF_FA) < 0.0000001) {
            mskd[i] = 0; // unset from further calcs
            // give default vals
            unc_out[0][i] = unc_out[2][i] = unc_out[4][i] = 0.;
            unc_out[1][i] = unc_out[3][i] = PIo2;
            unc_out[5][i] = 1.;
         }
      }
   
   Ntodo = 0;
   for( i=0 ; i<Nvox ; i++ ) 
      if( mskd[i] ) {
         Ntodo++;
      }

   minds = (int *)calloc(Ntodo, sizeof(int)); 
   if( (minds == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(4);
   } 
   j=0;
   for( i=0 ; i<Nvox ; i++ ) 
      if( mskd[i] ) {
         minds[j] = i;
         j++;
      }

   INFO_message("Have %d total voxels in the data set, "
                "of which %d are to be resampled.", Nvox, Ntodo);

   // --------------------------------------------------------------------
   
   INFO_message("Number of iterations to do: %d",Nj);

   /*i = test_func( unc_out, 
     mskd,
     Nvox);*/

   i = Calc_DTI_uncert( unc_out, 
                        minds, //mskd,
                        Nvox,
                        insetPARS,
                        insetVECS,
                        dwiset1,
                        bseven,
                        StoreRandInd,
                        Mj,
                        Nj,
                        MAXBAD,
                        Dim[3],
                        Ntodo
                        );

   INFO_message("Done processing");


   // **************************************************************
   // **************************************************************
   //                 Store and output
   // **************************************************************
   // **************************************************************

   outset = EDIT_empty_copy( dwiset1 ) ; 

   EDIT_dset_items( outset,
                    ADN_datum_all , MRI_float, 
                    ADN_ntt       , 6, 
                    ADN_nvals     , 6,
                    ADN_prefix    , prefix ,
                    ADN_none ) ;

   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outset)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",
                 DSET_HEADNAME(outset));

	for( n=0; n<6 ; n++) {
		EDIT_substitute_brick(outset, n, MRI_float, unc_out[n]);
		unc_out[n]=NULL;
	}

	EDIT_BRICK_LABEL(outset, 0, "bias_E1e2");      
	EDIT_BRICK_LABEL(outset, 1, "std_E1e2");      
	EDIT_BRICK_LABEL(outset, 2, "bias_E1e3");      
	EDIT_BRICK_LABEL(outset, 3, "std_E1e3");      
	EDIT_BRICK_LABEL(outset, 4, "bias_FA");      
	EDIT_BRICK_LABEL(outset, 5, "std_FA");     

   THD_load_statistics(outset);
   tross_Make_History("3dDWUncert", argc, argv, outset);
   THD_write_3dim_dataset(NULL, NULL, outset, True);
   
   INFO_message("DONE.");

   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   if(outset){
      DSET_delete(outset);
      free(outset);
   }

   // -------------------------------------------------------

   if(unc_out) {
      for( i=0 ; i<6 ; i++) 
         free(unc_out[i]);
      free(unc_out);
   }
   
   if(StoreRandInd){
      for( i=0 ; i<Nj ; i++) 
         free(StoreRandInd[i]);
      free(StoreRandInd);
   }

   if(bseven) {
      for( i=0 ; i<Dim[3] ; i++) 
         free(bseven[i]);
      free(bseven);
   }

   for( i=0 ; i<N_DTI_SCAL ; i++){
      DSET_delete(insetPARS[i]);
      free(insetPARS[i]);
   }
   free(insetPARS);

   for( i=0 ; i<N_DTI_VECT ; i++){
      DSET_delete(insetVECS[i]);
      free(insetVECS[i]);
   }
   free(insetVECS);

   if(dwiset1){
      DSET_delete(dwiset1);
      free(dwiset1);
   }

   if( MASK ) {
      DSET_delete(MASK);
      free(MASK);
   }
      
   if(mskd) 
      free(mskd);

   if(minds) 
      free(minds);

   if(prefix)
      free(prefix);

   if(infix)
      free(infix);
   if(dti_listname)
      free(dti_listname);

   // ! do earlier
   NI_free_element(nel); nel=NULL;

   return 0;
}


// ---------------------------------------------------------------
// ---------------------------------------------------------------
// ---------------------------------------------------------------

/*int test_func( float **UU, 
               short *mskd,
               int Nvox)
{
   int Nthreads;

#ifdef USE_OMP
   INFO_message("Start OMPing");
   AFNI_SETUP_OMP(0) ;
   Nthreads = omp_get_max_threads();
   INFO_message("OMP thread count = %d", Nthreads);
#else
   INFO_message("Not using OMP-- will be slow.") ;
#endif

   AFNI_OMP_START;
#pragma omp parallel if(1)
   {
   
      int i;
      
#pragma omp for
      for( i=0 ; i<Nvox ; i++ ) {
         if( mskd[i] ) { 
            UU[0][i]+= 11.11;
            UU[3][i]+= (float) i;
         }
      }

   } // end OMP
   AFNI_OMP_END ;
   
   INFO_message("Finished OMPing");
   
   return 0;
   }*/

// ######################################################################

int Calc_DTI_uncert( float **UU,
                     int *minds, //short *mskd,
                     int Nvox,
                     THD_3dim_dataset **PARS,
                     THD_3dim_dataset **VECS,
                     THD_3dim_dataset *DWI,
                     float **bseven,
                     int **StoreRandInd,
                     int Mj,
                     int Nj,
                     int MAXBAD,
                     int M,
                     int Ntodo
                     )
{
   int Nthreads=1;
   int j;

   int ApproxTenPerc=0;

#ifdef USE_OMP
   INFO_message("Start OMPing");
   AFNI_SETUP_OMP(0) ;
   Nthreads = omp_get_max_threads();
   INFO_message("OMP thread count = %d", Nthreads);
#else
   INFO_message("Not using OMP-- will be slow.");
#endif

   ApproxTenPerc = (int) (0.1*Ntodo)/Nthreads;
   INFO_message("ApproxTenPerc = %d", ApproxTenPerc);

   if( ApproxTenPerc == 0 ) {
      if( Ntodo < 100 )
         WARNING_message("Very small number of voxels to calculate! "
                         "Only %d??", Ntodo);
      else
         WARNING_message("Really?  %d voxels to analyze and %d threads??",
                         Ntodo, Nthreads);
      ApproxTenPerc = 1;
   }
   else if(ApproxTenPerc < 0 ) 
      ERROR_exit("ERROR! %d voxels to analyze and %d threads?? "
                    "This leads to ~10 percent value of %d?!??",
                    Ntodo, Nthreads, ApproxTenPerc);

   AFNI_OMP_START;
#pragma omp parallel //if(1)
   {
      int aa;
      int i;
      int ithr;
      int nprog=0;
      time_t t_start;

      int POSDEF=0;
      int ii,kk,jj,ll;
   
      float Wei[Mj];
      short Weibad[Mj];
      float mostpos;
      int worstS;
      
      gsl_vector *x       = gsl_vector_alloc(Mj);    // ln( S_i )
      gsl_matrix *B       = gsl_matrix_alloc(Mj, 7); // bmatrix + 1
      gsl_matrix *BTW     = gsl_matrix_alloc(7, Mj); // (B^T Wei)
      gsl_matrix *BTWB    = gsl_matrix_alloc(7, 7);  // (B^T Wei B)
      gsl_matrix *BTWBinv = gsl_matrix_alloc(7, 7);  // (B^T Wei B)^-1
      gsl_matrix *C       = gsl_matrix_alloc(7, Mj); // (BTWBinv)(BTW)

      gsl_vector *dd      = gsl_vector_alloc(7);     // flat D + ln(S0)
      gsl_vector *Eval    = gsl_vector_alloc(3);     // 
      gsl_matrix *Evec    = gsl_matrix_alloc(3,3);   // 
      gsl_matrix *testD   = gsl_matrix_alloc(3,3);   // 
      gsl_eigen_symm_workspace *EigenV = gsl_eigen_symm_alloc(3);
      
      //int iter = 0;
      //int whichvox=0;

      ithr = omp_get_thread_num();
      if( !ithr ) {
         fprintf(stderr,"++ Nvox progress proxy count on thread[%d]: "
                 "start ...\n", ithr);
         t_start = time(NULL);
      }

#pragma omp for
      for( aa=0 ; aa<Ntodo ; aa++ ) {
         //if( mskd[i] ) { 
         i = minds[aa];
         for( ll=0 ; ll<Nj ; ll++ ) {
            /*for( ii=0 ; ii<Nvox ; ii++ ) // ONLY FOR TESTING
              if(mskd[ii]){
              whichvox=ii;
              j++;
              if(j==1){
              INFO_message("THIS VOX: %d",whichvox);
              break;
              }
              }*/
            
            for( ii=0 ; ii<Mj ; ii++ ) 
               Wei[ii] = pow(THD_get_voxel(DWI, i, 
                                           StoreRandInd[ll][ii]), 2);
            
            ii = Make_Uncert_Matrs_init( i,
                                         bseven,
                                         DWI,
                                         StoreRandInd[ll], 
                                         Wei,
                                         x,
                                         B,
                                         BTW,
                                         Mj
                                         );
   
            ii = Make_Uncert_Matrs_final( 
                                         B,
                                         BTW,
                                         BTWB,
                                         BTWBinv,
                                         C
                                          );
            
            ii = Calc_DTI_lin_tensor( x,
                                      dd,
                                      C,
                                      testD,
                                      Eval,
                                      EigenV,
                                      &POSDEF
                                      );
            
            if( !POSDEF ) {
               //INFO_message("Vox: %10d has neg eigval(s)", i);
               
               for( ii=0 ; ii<Mj ; ii++ ) 
                  Weibad[ii] = 0;
               
               for( jj=0 ; jj<MAXBAD ; jj++ ) {
                  //INFO_message("fitting iter: %d",jj);
                  
                  mostpos = -1.e10; // ceiling in this nonPOSDEF zone 
                  worstS = -1;
                  // start getting rid of bad ones, but not the [0]th
                  for( kk=1 ; kk<Mj ; kk++ ) {
                     if( !Weibad[kk] ) {
                        for( ii=0 ; ii<Mj ; ii++ ) {
                           Wei[ii] = pow(THD_get_voxel(DWI, 
                                                       i, 
                                                       StoreRandInd[ll][ii]), 
                                         2);
                           if ( Weibad[ii] || (ii==kk) ) 
                              Wei[ii]/= 10.e8;
                        }
                        //Show_1DArray_Floats(Wei,Mj);
                        ii = Make_Uncert_Matrs_init( i,
                                                     bseven,
                                                     DWI,
                                                     StoreRandInd[ll], 
                                                     Wei,
                                                     x,
                                                     B,
                                                     BTW,
                                                     Mj
                                                     );
                        
                        ii = Make_Uncert_Matrs_final( B,
                                                      BTW,
                                                      BTWB,
                                                      BTWBinv,
                                                      C 
                                                      );
               
                        ii = Calc_DTI_lin_tensor( x,
                                                  dd,
                                                  C,
                                                  testD,
                                                  Eval,
                                                  EigenV,
                                                  &POSDEF
                                                  );
                        if(POSDEF) {
                           jj = MAXBAD+10; // break the cycle!
                           break;
                        }
                        if(gsl_vector_min(Eval)>mostpos) {
                           mostpos = gsl_vector_min(Eval);
                           worstS = kk;
                        }
                     }
                  }
                  // another one stored as bad for next go 'round
                  Weibad[worstS]=1; 
               }
               //if(POSDEF)
               //INFO_message("Fixed one! %d",i);
            }
            
            // still per voxel thing
            if(POSDEF) { // if things were fine in tensor calc
               //INFO_message("OK fit!");
               ii = Calc_Eigs_Uncert( i,
                                      UU,
                                      dd,
                                      testD, // have to recreate
                                      Eval,
                                      Evec,
                                      PARS,
                                      VECS
                                      );
            }
            else{ // never found a posdef!
                  //WARNING_message("Vox %10d couldn't be fit!", i);
                  //UU[0][i]+= 0.; --> don't need to add zeros
                  //UU[2][i]+= 0.;
                  //UU[4][i]+= 0.;
               UU[1][i]+= PIo2;
               UU[3][i]+= PIo2;
               UU[5][i]+= 1.;
            }

         }

         ithr = omp_get_thread_num();
         // counter for user
         if( ithr == 0 ) {
            nprog++;
            if( (nprog % ApproxTenPerc) == 0 ) {
               fprintf(stderr,"\t%s %3.0f%% %s -> %.2f min\n",
                       "[", nprog *10./ApproxTenPerc,"]", 
                       (float) difftime( time(NULL), t_start)/60. );
            }
         }
      }
      

      // free stuff, per usual...
      gsl_vector_free(x);
      gsl_matrix_free(B);
      gsl_matrix_free(BTW);
      gsl_matrix_free(BTWB);
      gsl_matrix_free(BTWBinv);
      gsl_matrix_free(C);

      gsl_vector_free(dd);
      gsl_vector_free(Eval);
      gsl_matrix_free(Evec);
      gsl_matrix_free(testD);
      
      gsl_eigen_symm_free(EigenV);
            
   } // end OMP
   AFNI_OMP_END ;
   
   
   INFO_message("Finished OMPing.  Finalizing...");
   
   // after all tensor diffs have been entered, finalize
   j = Finalize_Uncert_Array( UU,
                              minds,
                              Ntodo,
                              Nj
                              );

   return 0;
}





