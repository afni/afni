// PA Taylor, Dec 2013

/*
  Feb, 2014:
  + fixing a bit of helpfile listing

  April 2014:
  + adding row output

  Aug 2014
  + adding ability to input a (multi-b0)+dwi file, and have
    the b0s get averaged and put as 0th brick

  May 2015
  + minor new option for outputting bval file: -out_bval_row_sep

  Apr 2016
  + new import/export with b-value weighted gradients
  + minor new option for outputting bval file: -out_bval_col_sep

  Sept 2016
  + much internal workings reprogrammed, to allow for more flexibility
  + added new '-bref_mean_top' option for people with repeated bref>0
    reference values/grads

*/


// 3dDWItoDT and BMTXT_AFNI.txt: Bxx, Byy, Bzz, Bxy, Bxz, Byz
// TORTOISE:  B_xx 2B_xy 2B_xz B_yy 2B_yz B_zz
// BMTXT.txt from TORT's "afni export: Bxx, Byy, Bzz, 2Bxy, 2Bxz, 2Byz

/* G_{ij} = diffusion weighting matrix (dyadic form)
        GxGx GxGy GxGz
        GxGy GyGy GyGz
        GxGz GyGz GzGz 
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
#include "matrix.h"

// max number of bvecs total is 1111 from this lazy method
#define MAXGRADS (12006) 
#define MINBVAL ( 0.000001 )   // prevent dividing by zero

int GradConv_Gsign_from_GmatA( float *grad, float *matr ); // assumes I/O has unit or zero mag!
int GradConv_GmatA_from_Gsign( float *matr, float *grad );

float GradCloseness(float **X, int N, int DCF);
float CalcInnerProdAngle( float *A, float *B, int N );
float SimpleDP( float *A, float *B, int N);


void usage_1dDW_Grad_o_Mat(int detail) 
{
	printf(
"  \n"
"Simple function to manipulate DW gradient vector files, b-value\n"
"files, and b-/g-matrices. Let: g_i be one of Ng spatial gradients\n"
"in three dimensions; the g-matrix is G_{ij} = g_i*g_j (i.e., dyad\n"
"of gradients, without b-value included); and the DW-scaled\n"
"b-matrix is B_{ij} = b*g_i*g_j.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"As of right now, one can input:\n"
"  + 3 rows of gradients (as output from dcm2nii, for example);\n"
"  + 3 columns of gradients;\n"
"  + 6 columns of g- or b-matrices, in `diagonal-first' order:\n" 
"         Bxx, Byy, Bzz, Bxy, Bxz, Byz,\n"
"    which is used in 3dDWItoDT, for example;\n"
"  + 6 columns of g- or b-matrices, in `row-first' order:\n" 
"         Bxx, 2*Bxy, 2*Bxz, Byy, 2*Byz, Bzz, \n"
"    which is output by TORTOISE, for example;\n\n"
"  + when specifying input file, one can use the brackets '{ }'\n"
"    in order to specify a subset of rows to keep (NB: probably\n"
"    can't use this grad-filter when reading in row-data right\n"
"    now).\n"
"During processing, one can:\n"
"  + flip the sign of any of the x-, y- or z-components, which\n"
"    may be necessary to do to make the scanned data and tracking\n"
"    work happily together;\n"
"  + filter out all `zero' rows of recorded reference images;\n"
"  \n"
"One can then output:\n"
"  + 3 columns of gradients;\n"
"  + 6 columns of g- or b-matrices, in 'diagonal-first' order;\n" 
"  + 6 columns of g- or b-matrices, in 'row-first' order;\n" 
"  + as well as including a column of b-values (such as used in;\n" 
"    DTI-Studio);\n"
"  + as well as including a row of zeros at the top;\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
" + RUNNING:\n"
"    1dDW_Grad_o_Mat                                                 \\\n"
"         { -in_grad_cols  | -in_grad_cols_bwt |                     \\\n"
"           -in_gmatT_cols | -in_gmatA_cols |                        \\\n"
"           -in_bmatT_cols | -in_gmatA_cols |                        \\\n"
"           -in_grad_rows }  INFILE                                  \\\n"
"         { -flip_x | -flip_y | -flip_z }                            \\\n"
"         { -keep_b0s } { -put_zeros_top } { -out_bval_col }         \\\n"
"         { -bref_mean_top }                                         \\\n"
"         { -in_bvals BVAL_IN }                                      \\\n"
"         { -bmax_ref THRESH }                                       \\\n"
"         { -out_grad_cols  | -out_grad_cols_bwt |                   \\\n"
"           -out_gmatT_cols | -out_gmatA_cols |                      \\\n"
"           -out_bmatT_cols | -out_gmatA_cols |                      \\\n"
"           -out_grad_rows }  OUTFILE                                \\\n"
"         { -out_bval_row_sep | -out_bval_col_sep BB }               \n\n"
"    where:\n"
"        (one of the following six formats of input must be given):\n"
"    -in_grad_rows  INFILE :input file of 3 rows of gradients (e.g., dcm2nii-\n"
"                           format output).\n"
"    -in_grad_cols  INFILE :input file of 3 columns of gradients.  \n"
" -in_grad_cols_bwt INFILE :input file of 3 columns of gradients, each\n"
"                           weighted by the b-value.\n"
"    -in_gmatA_cols INFILE :input file of 6 columns of g-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"    -in_gmatT_cols INFILE :input file of 6 columns of g-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"    -in_bmatA_cols INFILE :input file of 6 columns of b-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"    -in_bmatT_cols INFILE :input file of 6 columns of b-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"        (one of the following five formats of output must be given):\n"
"\n"
"   -out_grad_cols OUTFILE :output file of 3 columns of gradients.  \n"
" -out_grad_cols_bwt OUTFILE :output file of 3 columns of gradients, each  \n"
"                           weighted by the b-value.\n"
"  -out_gmatA_cols OUTFILE :output file of 6 columns of g-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"  -out_gmatT_cols OUTFILE :output file of 6 cols of g-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"  -out_bmatA_cols OUTFILE :output file of 6 columns of b-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"  -out_bmatT_cols OUTFILE :output file of 6 cols of b-matr in 'T(ORTOISE)'\n"
"                          `row first'-format. (See above.)\n"
"  -out_grad_rows  OUTFILE :output file of 3 rows of gradients.\n"
"\n"
"        (and any of the following options may be used):\n"
"    -proc_dset    DSET    :input a dataset DSET of X 'b=0' and Y DWI bricks,\n"
"                           matching the X zero- and Y nonzero-gradient \n"
"                           entries in the INFILE. The 'processing' will:\n"
"                                  1) extract all the 'b=0' bricks,\n"
"                                  2) average them,\n"
"                                  3) store the result in the zeroth brick of\n"
"                                     the output PREFIX data set, and\n"
"                                  4) place the DWIs (kept in their original\n"
"                                     order) as the next Y bricks of PREFIX.\n"
"                           This option cannot be used with '-keep_b0s'.\n"
"                           The output set has Y+1 bricks.  The option is\n"
"                           probably mostly useful only if X>1.\n"
"    -pref_dset    PREFIX  :output dataset filename prefix (required and iff\n"
"                           using '-proc_dset', above).\n"
"    -dwi_comp_fac N_REP   :option for averaging DWI bricks in DSET that have\n"
"                           been acquired with exactly N_REP repeated sets of\n"
"                           gradients. *You* the user must know how many\n"
"                           repetitions have been performed (this program\n"
"                           will perform a simplistic gradient comparison\n"
"                           using dot products to flag possible errors, but\n"
"                           this is by no means bulletproof.  Use wisely.\n"
"\n"
"    -flip_x               :change sign of first column of gradients\n"
"    -flip_y               :change sign of second column of gradients\n"
"    -flip_z               :change sign of third column of gradients\n"
"\n"
"    -bref_mean_top        :when averaging the reference X 'b0' values (which\n"
"                           is default behavior), have the mean of the X \n"
"                           values be represented in the top row; default \n"
"                           behavior is to have nothing representing the b0\n"
"                           information in the top row (for historical\n"
"                           functionality reasons).  NB: if your reference\n"
"                           'b0' actually has b>0, you might not want to \n"
"                           average the b0 refs together, because their\n"
"                           images could have differing contrast if the\n"
"                           same reference vector wasn't used for each.\n"
"    -keep_b0s             :default function is to get rid of all reference\n"
"                           image, but this option acts as switch to keep\n"
"                           them.\n"
"    -put_zeros_top        :whatever the output format is, add a row at the\n"
"                           top with all zeros.\n"
"    -bmax_ref THRESH      :THRESH is a scalar number below which b-values\n"
"                           (in BVAL_IN) are considered `zero' or reference.\n"
"                           Sometimes, for the reference images, the scanner\n"
"                           has a value like b=5 s/mm^2, instead of strictly\n"
"                           b=0 strictly. One can still flag such values as\n"
"                           being associated with a reference image and\n"
"                           trim it out, using, for the example case here, \n"
"                           '-bmax_ref 5.1'.\n"
"\n"
"    -in_bvals BVAL_IN     :BVAL_IN is a file of b-values, such as the 'bval'\n"
"                           file generated by dcm2nii.\n"
"    -out_bval_col         :switch to put a column of the bvalues as the\n"
"                           first column in the output data.\n"
"    -out_bval_row_sep BB  :output a file BB of bvalues in a single row.\n"
"    -out_bval_col_sep BB  :output a file BB of bvalues in a single row.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  If you use this program, please reference the introductory/description\n"
"  paper for the FATCAT toolbox:\n"
"        Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional\n"
"        And Tractographic Connectivity Analysis Toolbox. Brain \n"
"        Connectivity 3(5):523-535.\n"
"____________________________________________________________________________\n"
);
	return;
}

int main(int argc, char *argv[]) 
{  
   int CHECK = 0;
	int iarg;
   char *Fname_input = NULL;
   char *Fname_output = NULL;
   char *Fname_outputBV = NULL;
   char *Fname_bval = NULL;
   int opt;
   FILE *fin=NULL, *fout=NULL, *finbv=NULL, *foutBV=NULL;
   int i,j,k;
   int BZER=0,idx=0,idx2=0;

   MRI_IMAGE *flim=NULL;
   MRI_IMAGE *preREADIN=NULL;
   MRI_IMAGE *preREADBVAL=NULL;
   float *READIN=NULL;
   float *READBVAL=NULL;

   float INP_MATR[MAXGRADS][7]; // b- or g-matrix
   float INP_GRAD[MAXGRADS][4]; // b- or g-matrix

   int INV[3] = {1,1,1}; // if needing to switch
   int FLAG[MAXGRADS];
   float temp;
   int YES_B = 0;
   int EXTRA_ZEROS=0;
   int PUT_MEAN_BREF_TOP=0;
   int HAVE_BVAL = 0;
   int HAVE_BVAL_EFF = 0; // can calc bval from input, not sep file
   int USE_BWT = 0;       // output depends on bwt
   float MAX_BVAL = 0.;   // calc max. bval for possible use scaling
   int BVAL_OUT = 0; 
   int BVAL_OUT_SEP = 0; 
   float BMAX_REF = 0.01; // i.e., essentially zero
   int IN_FORM = 0;       // 0 for row, 1 for col
   int OUT_FORM = 1;      // 1 for col, 2 for bmatr 
   int HAVE_BMAX_REF=0 ;  // referring to user input value
   int count_in=0, count_out=0;

	THD_3dim_dataset *dwset=NULL, *dwout=NULL; 
   int Nbrik = 0;
	char *prefix=NULL ;
   float **temp_arr=NULL, **temp_grad=NULL;
   int Ndwi = 0, dwi=0, Ndwout = 0, Ndwi_flagged = 0, Ndwout_final = 0;
   int Nvox = 0;
   int DWI_COMP_FAC = 0;
   int ct_dwi = 0;
   float MaxDP = 0;

   float tmp[6] = {0.,0.,0.,0.,0.,0.}; // Aug,2016
   float bref_vec[4]  = {0.,0.,0.,0.};          // Aug,2016
   int Nrow=0, Ncol=0, EXTRA_row=0, EXTRA_col=0,Nrow_final=0;
   int ctr;
   float **OUT=NULL;
   int *BOUT=NULL;


	mainENTRY("1dDW_Grad_o_Mat"); machdep();
    
   if (argc == 1) { usage_1dDW_Grad_o_Mat(1); exit(0); }

   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
         usage_1dDW_Grad_o_Mat(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
      
      if( strcmp(argv[iarg],"-flip_x") == 0) {
			INV[0] = -1;
			iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-flip_y") == 0) {
			INV[1] = -1;
			iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-flip_z") == 0) {
			INV[2] = -1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-keep_b0s") == 0) {
			YES_B = 1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-put_zeros_top") == 0) {
			EXTRA_ZEROS = 1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-bref_mean_top") == 0) {
			PUT_MEAN_BREF_TOP = 1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_grad_rows") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_grad_rows'\n") ;

         IN_FORM = 0;  //, which is just default, because of dcm2nii
         Fname_input = argv[iarg];
         count_in++;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_grad_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_grad_cols'\n") ;

         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 1;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_grad_cols_bwt") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_grad_cols_bwt'\n") ;

         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 1;
         HAVE_BVAL_EFF = 1;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_gmatT_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_matT_cols'\n") ;
         
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 2;
         
         iarg++ ; continue ;
		} 

      if( strcmp(argv[iarg],"-in_gmatA_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_matA_cols'\n") ;
         
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 3;
         
         iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-in_bmatT_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_matT_cols'\n") ;
         
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 4;
         HAVE_BVAL_EFF = 1;
         
         iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-in_bmatA_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_matA_cols'\n") ;
         
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 5;
         HAVE_BVAL_EFF = 1;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_grad_rows") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_grad_cols'\n") ;

         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 0;

         iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-out_grad_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_grad_cols'\n") ;

         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 1;

         iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-out_grad_cols_bwt") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_grad_cols_bwt'\n") ;

         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 1;
         USE_BWT = 1;
         
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_gmatT_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_gmatT_cols'\n") ;
         
         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 2;
         
         iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-out_gmatA_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_gmatA_cols'\n") ;
         
         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 3;
         
         iarg++ ; continue ;
		}  
      if( strcmp(argv[iarg],"-out_bmatT_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_bmatT_cols'\n") ;

         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 4;
         USE_BWT = 1;

         iarg++ ; continue ;
		}
      
      if( strcmp(argv[iarg],"-out_bmatA_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_bmatA_cols'\n") ;

         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 5;
         USE_BWT = 1;
                  
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_bvals") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_bvals'\n") ;
         
         Fname_bval = argv[iarg];
         HAVE_BVAL = 1;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-bmax_ref") == 0) { 
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-bmax_ref'\n");
         
         BMAX_REF = atof(argv[iarg]);
         HAVE_BMAX_REF = 1;
         
         iarg++ ; continue ;
		}
      
      if( strcmp(argv[iarg],"-out_bval_col") == 0) {
			BVAL_OUT = 1;
         USE_BWT = 1;
			iarg++ ; continue ;
		}

      // May,2015
      if( strcmp(argv[iarg],"-out_bval_row_sep") == 0) {
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_bval_row_sep'\n") ;
         
         Fname_outputBV = argv[iarg];
         BVAL_OUT_SEP = 1;
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_bval_col_sep") == 0) {
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_bval_col_sep'\n") ;
         
         Fname_outputBV = argv[iarg];
         BVAL_OUT_SEP = 2;

         iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-proc_dset") == 0 ){ // in DWIs
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-proc_dset'") ;
			dwset = THD_open_dataset( argv[iarg] ) ;
			if( dwset == NULL ) 
				ERROR_exit("Can't open DWI dataset '%s'", argv[iarg]) ;
			DSET_load(dwset) ; CHECK_LOAD_ERROR(dwset) ;
			
			iarg++ ; continue ;
		}
		
      if( strcmp(argv[iarg],"-pref_dset") == 0 ){ // will be output
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-pref_dset'");
			prefix = strdup(argv[iarg]) ;
			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-pref_dset'");
			iarg++ ; continue ;
		}
		
      if( strcmp(argv[iarg],"-dwi_comp_fac") == 0) { 
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-dwi_comp_fac'\n");
         
         DWI_COMP_FAC = atoi(argv[iarg]);
         if (DWI_COMP_FAC <=1)
            ERROR_exit("The compression factor after '-dwi_comp_fac'"
                       "must be >1!");

         iarg++ ; continue ;
		}

      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
      
   }

   //  * * * * * * * * * * * * * * * * * * * * * * * * * * * 

   if( (Fname_input == NULL) ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Option '-in_*' requires argument.\n");
      exit(1);
   }
   if( (Fname_output == NULL) ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Option '-out_*' requires arg.\n");
      exit(2);
   }

   if( count_in > 1 ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Can't have >1 vec file input.\n");
      exit(3);
   }
   if( count_out > 1 ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Can't have >1 output file opt.\n");
      exit(4);
   }

   if(YES_B && dwset) {
      fprintf(stderr,
              "\n** Bad Command-lining! "
              "Can't have '-keep_b0s' and '-proc_dset' together.\n");
      exit(5);
   }

   if(PUT_MEAN_BREF_TOP && (YES_B || EXTRA_ZEROS) ) {
      ERROR_message("Incompatible combo of options: can't have "
                    "-put_meanbref_top with either -keep_b0s "
                    "or -put_zeros_top");
      exit(5);
   }

   if( !prefix && dwset) {
      fprintf(stderr,
              "\n** Bad Command-lining! "
              "Need an output '-pref_dset' when using '-proc_dset'.\n");
      exit(6);
   }
   
   if(YES_B && DWI_COMP_FAC) {
      fprintf(stderr,
              "\n** Bad Command-lining! "
              "Can't have '-keep_b0s' and '-dwi_comp_fac' together.\n");
      exit(7);
   }
   
   // the final authority on whether bvalue information has been
   // input, if the output requires it in some way
   if((!HAVE_BVAL_EFF && !HAVE_BVAL) && (USE_BWT || BVAL_OUT_SEP )) {
      fprintf(stderr,
              "\n** Bad Command-lining! "
              "\n\t Can't have ask for outputting bvals with no '-in_bvals FILE'"
              "\n\t or without using one of the '-in_bmat* FILE' options"
              "\n\t or without using the '-in_grad_cols_bwt FILE' option.\n");
      exit(8);
   }


   // ********************************************************************
   // ************************* start reading ****************************
   // ********************************************************************

   flim = mri_read_1D (Fname_input);
   if (flim == NULL) {
         ERROR_exit("Error reading gradient vector file");
      }
   if( IN_FORM )
      preREADIN = mri_transpose(flim); // eff *undoes* autotranspose
   else
      preREADIN = mri_copy(flim);
   mri_free(flim);
   idx = preREADIN->ny;

   if( HAVE_BVAL ) {
      flim = mri_read_1D (Fname_bval);
      if (flim == NULL) {
         ERROR_exit("Error reading b-value file");
      }
      if( flim->ny == 1)
         preREADBVAL = mri_transpose(flim); // eff *undoes* autotransp
      else
         preREADBVAL = mri_copy(flim); 
      mri_free(flim);
      idx2 = preREADBVAL->ny;  // 'idx' is the number of grads

   }

   // ------------- lots of checks for various things.  Ugh. -------------

   if(idx>= MAXGRADS ) {
      printf("Error, too many input grads.\n");
      mri_free (preREADIN);
      if( HAVE_BVAL ) mri_free (preREADBVAL);
      exit(4);
   }

   if( ( (preREADIN->nx != 3 ) && (preREADIN->ny != 3 )) &&
       (preREADIN->nx != 6 ) )
      printf("Probably an error, "
             "because there aren't 3 or 6 numbers in columns!\n");

   if( HAVE_BVAL && ( idx != idx2 ) ) {
      printf("Error, because the number of bvecs (%d)\n"
             "and bvals (%d) don't appear to match!\n", idx, idx2);
      mri_free (preREADIN);
      mri_free (preREADBVAL);
      exit(3);
   }

   if(dwset) {
      Nbrik = DSET_NVALS(dwset);

      if( idx != Nbrik ) {
         fprintf(stderr,
                 "\n** ERROR: the number of bvecs (%d) does not match the "
                 "number of briks in '-proc_dset' (%d).\n", idx, Nbrik);
         exit(4);
      }
   }

   READIN = MRI_FLOAT_PTR( preREADIN );
   if( HAVE_BVAL )
      READBVAL = MRI_FLOAT_PTR( preREADBVAL );


   // 0 is grad row;  
   // 1 is grad col;
   // 2 is gmatrRow col T;
   // 3 is gmatrDiag col A;
   // 4 is bmatrRow col T;
   // 5 is bmatrDiag col A;

   //if( IN_FORM == 0 ) // grad rows, no binfo
   // for( i=0; i<idx ; i++ ) 
   //    for ( j=0; j<3 ; j++ )
   //       INP_GRAD[i][j+1] = *(READIN +j*idx +i) ;
   //else 
   if ( IN_FORM <= 1 )  // grad cols, no binfo
      for( i=0; i<idx ; i++ ) 
         for ( j=0; j<3 ; j++ )
            INP_GRAD[i][j+1] = *(READIN + 3*i+j);
   
   // A/row/3dDWItoDT: Bxx, Byy, Bzz, Bxy, Bxz, Byz
   // T/diag/TORTOISE:  b_xx 2b_xy 2b_xz b_yy 2b_yz b_zz
   else if ( (IN_FORM == 3) || (IN_FORM ==5 ) ) { // diag matr
      for( i=0; i<idx ; i++ ) { 
         for( j=0; j<3 ; j++ ) {
            INP_MATR[i][j+1] = *(READIN+6*i+j);
            INP_MATR[i][3+j+1] = *(READIN+6*i+3+j);
         }

         for( j=1; j<4 ; j++ ) 
            if(INP_MATR[i][j] < 0 )
               CHECK++;
      }
      if(CHECK > 0)
         INFO_message("Warning: you *said* you input a mat'A',"
                      " but the matr diagonals don't appear to be uniformly"
                      " positive. If input cols 0, 3 and 5 are positive,"
                      " then you might have meant mat'T'?");
   }
   else if ( (IN_FORM ==2 ) || (IN_FORM ==4 ) ) { // row matr
      CHECK = 0;
      for( i=0; i<idx ; i++ ) {
         INP_MATR[i][1] = *(READIN +6*i);
         INP_MATR[i][2] = *(READIN +6*i+3);
         INP_MATR[i][3] = *(READIN +6*i+5);
         INP_MATR[i][4] = *(READIN +6*i+1)/2.;
         INP_MATR[i][5] = *(READIN +6*i+2)/2.;
         INP_MATR[i][6] = *(READIN +6*i+4)/2.;
      }
      for( i=0; i<idx ; i++ ) 
         for( j=1; j<4 ; j++ ) 
            if(INP_MATR[i][j] < 0 )
               CHECK++;
      if(CHECK > 0)
         INFO_message("Warning: you *said* you input a mat'T',"
                      " but the matr diagonals don't appear to be uniformly"
                      " positive. If input cols 0, 1 and 2 are positive,"
                      " then you might have meant mat'A'?");
   }
   else{
      fprintf(stderr, "Coding error with format number (%d), not allowed.\n",
              IN_FORM);
      exit(2);
   }

   INFO_message("Have read in data.");

   // ---------- getting magnitudes, where appropriate ---------------------
   
   // NB: even in cases without a real bvalue, we'll assume that they
   // at least have unit magnitudes, so we can treat their bvals as
   // either 0 or nonzero (-> prob 1).  We just don't want divide by
   // zeros later, but also want to reduce the number of special cases.

   // get bval info: this 'if' is the cases where HAVE_BVAL_EFF==1
   if( ( IN_FORM==4 ) || (IN_FORM==5 ) ||         // bmatrices
       ( IN_FORM==2 ) || (IN_FORM==3 ) ) {        // gmatrices
      for( i=0; i<idx ; i++ ) {
         INP_MATR[i][0] = INP_GRAD[i][0] =
            INP_MATR[i][1] + INP_MATR[i][2] + INP_MATR[i][3];
         if( INP_MATR[i][0] > MINBVAL)
            for( j=1 ; j<7 ; j++ )
               INP_MATR[i][j]/= INP_MATR[i][0];
      }
   }  // -> now have magn col and unit norm (or zero) gmatr. Hopefully.
   else if ( ( IN_FORM==1 ) || ( IN_FORM==0 )) { // row or col grads input
      for( i=0; i<idx ; i++ ) {
         INP_MATR[i][0] = INP_GRAD[i][0] =
            sqrt( INP_GRAD[i][1]*INP_GRAD[i][1] + 
                  INP_GRAD[i][2]*INP_GRAD[i][2] + 
                  INP_GRAD[i][3]*INP_GRAD[i][3] );
         if( INP_GRAD[i][0] > MINBVAL)
            for( j=1 ; j<4 ; j++ )
               INP_GRAD[i][j]/= INP_GRAD[i][0];
      }
   }// -> now have magn col and unit norm (or zero) grads. Hopefully.

   // ---> check in case where nonbvals were used, to have unit normal
   // ---> magnitudes if b>0.  This gets rid of rounding, etc. errors.
   if( !HAVE_BVAL_EFF )
      for( i=0; i<idx ; i++ ) {
         if ( INP_MATR[i][0] > 0.5 ) // could equiv check matr or grad [0]
            INP_MATR[i][0] = INP_GRAD[i][0] = 1;
      }

   if ( HAVE_BVAL ) // these will take precedence if they are input separately
      for( i=0; i<idx ; i++ ) {
         INP_MATR[i][0] = INP_GRAD[i][0] = *(READBVAL + i);
      }

   INFO_message("Have organized the data internally.");

   // * * *  ** * * * * * * * * ** ** * * ** * * ** * ** * ** * * *
   // at this point, all cases,
   //    + grad[i][0]  has bval
   //    + matr[i][0]  has bval
   //    + matr[i][>0] normalized and in diagonal form
   //    + grad[i][>0] normalized
   // ... this should be true even if the inputs were just grads or 
   // gmats without real separate bvalue info other than just binary
   // 0/1.  Simpler to deal with in this way.
   // * * *  ** * * * * * * * * ** ** * * ** * * ** * ** * ** * * *

   // ------------------------ check report max bval -----------------
   // in case we want max bval: should be able to get from either
   // OUT_* array.
   for( i=0 ; i<idx ; i++) 
      if( MAX_BVAL < INP_GRAD[i][0] )
         MAX_BVAL = INP_GRAD[i][0];

   if( USE_BWT || BVAL_OUT_SEP || HAVE_BVAL || HAVE_BVAL_EFF )
      INFO_message("Maximum bvalue input is: %.0f\n",MAX_BVAL);
   
   // --------------------- equivalence ---------------------------------

   // make sure everything is equivalent between grad and matr
   // info. can just deal with one now, whatever was input.
   for( i=0; i<idx ; i++ ) 
      if( IN_FORM > 1)
         j = GradConv_Gsign_from_GmatA( INP_GRAD[i]+1, INP_MATR[i]+1);
      else
         j = GradConv_GmatA_from_Gsign( INP_MATR[i]+1, INP_GRAD[i]+1);

   // ************************ NOTE ***************************************

   // **FROM HERE ON**: just use grads, and then make whatever output
   // **is necessary!  simpler that way...

   // *********************************************************************

   // ----------------------- flip, if requested --------------------------

   for( i=0 ; i<idx ; i++) 
      for( j=0 ; j<3 ; j++)
         INP_GRAD[i][j+1]*= INV[j];
   
   // --------------------- find reference values -------------------------

   // count/identify bzeros, even if only grad/gmatrix values were input
   BZER=0;
   for( i=0 ; i<idx ; i++) {
      if( INP_GRAD[i][0] >= BMAX_REF ) // a DWI
         FLAG[i] = 1;
      else{                            // a bref
         // keep track of its properties
         bref_vec[0]+= INP_GRAD[i][0];
         // if averaging bref, then all entries *should* either have
         // the same gradient or have b=0; in either case, just
         // keeping last value should be fine.
         bref_vec[1] = INP_GRAD[i][1]; // keep track of ref b; Aug,2016
         bref_vec[2] = INP_GRAD[i][2]; // keep track of ref b; Aug,2016
         bref_vec[3] = INP_GRAD[i][3]; // keep track of ref b; Aug,2016
         BZER++;
      }
   }
   if(BZER)
      bref_vec[0]/= (float) BZER; // have all props; can use later.
   else 
      WARNING_message("NO reference gradients/values found.");

   // this opt was exclusive with keeping mean properties which would
   // go at top; makes output easier below
   if( EXTRA_ZEROS ) 
      for( i=0 ; i<4 ; i++) 
         bref_vec[i] = 0;

   // -------------------- summarize outputs -----------------------

   if(YES_B) {
      INFO_message("Chose to *keep* %d b0s,\tas well as  \t%d grads\n",
             BZER,idx-BZER);
      BZER=0;
      Ndwi_flagged = idx;

      // flag everything to be copied over.
      for( i=0 ; i<idx ; i++) 
         FLAG[i] = 1; 
   }
   else {
      Ndwi_flagged = Ndwi = idx - BZER;
      INFO_message("Getting rid of %d b0s,\tleaving the %d grads\n",
             BZER, Ndwi);
   
      if( DWI_COMP_FAC ) {
         if( Ndwi % DWI_COMP_FAC != 0 ) {
            fprintf(stderr, "\n** ERROR can't compress: "
                    "Ndwi=%d, and %d/%d has a nonzero remainder (=%d).\n",
                    Ndwi,Ndwi,DWI_COMP_FAC, Ndwi % DWI_COMP_FAC );
            exit(1);
         }
         else {
            Ndwi_flagged = Ndwi/DWI_COMP_FAC;
            INFO_message("You have chosen a compression factor of %d, "
                         "with %d DWIs,\n"
                         "\tso that afterward there will be %d DWIs.",
                         DWI_COMP_FAC, Ndwi, Ndwi_flagged);
         }
      }
   }

   if( EXTRA_ZEROS ) 
      INFO_message("... and have an extra row of zeros to place at the top.");

   if( PUT_MEAN_BREF_TOP ) {
      INFO_message("... and placing mean properties at top:\n"
                   "\t mean bref = %f\n"
                   "\t representative grad: (%.5f, %.5f, %.5f).",
                   bref_vec[0],bref_vec[1],bref_vec[2],bref_vec[3]);
      if (!BZER) {
         WARNING_message("Hey! You wanted mean bref properties at the top\n"
                         "\t of the file, but I couldn't find any brefs!!");
         WARNING_message("Do you have a bref with bvalue >0 ??");
      }
   }

   // ==================================================================

   // ---------------- prep to output array info ----------------------

   // output dimensions for array (treat 'row' as col dimensions at
   // the moment) 
   Nrow = idx; //Ndwi_flagged;
   if ( EXTRA_ZEROS || PUT_MEAN_BREF_TOP )
      EXTRA_row = 1;

   if( (OUT_FORM == 0) || (OUT_FORM == 1) )
      Ncol = 3;
   else
      Ncol = 6;

   if( BVAL_OUT ) // extra row for outputting bval
      EXTRA_col;

   OUT = calloc( idx+EXTRA_row, sizeof(OUT));
   for( i=0 ; i<(idx+EXTRA_row) ; i++) 
      OUT[i] = calloc( Ncol+EXTRA_col, sizeof(float)); 

   BOUT = (int *) calloc(idx+EXTRA_row, sizeof(int));

   if( ( OUT == NULL ) || ( BOUT == NULL )) {
      fprintf(stderr, "\n\n MemAlloc failure in out arrays.\n\n");
      exit(12);
   }

   // -------------- fill in arrays to write out ----------------------

   // First thing: check about:
   if ( EXTRA_row ) {  
      // grad/matr part
      if( Ncol == 3 )
         for( k=0 ; k<3 ; k++) 
            tmp[k] = bref_vec[k+1];
      else
         k = GradConv_GmatA_from_Gsign( tmp, bref_vec+1);
      
      if( USE_BWT ) // weight if nec
         for ( k=0 ; k<Ncol ; k++)
            tmp[k]*= bref_vec[0];
      
      if( EXTRA_col )
         OUT[0][0] = bref_vec[0];

      for ( k=0 ; k<Ncol ; k++) 
         OUT[0][EXTRA_col+k] = tmp[k]; // start at either 0 or 1

      BOUT[0] = (int) rintf(bref_vec[0]);  // and bval entry 
   }

   // now go through all flagged rows
   ctr = EXTRA_row; // start at 0 or 1
   for( i=0 ; i<idx ; i++) 
      if( FLAG[i] ) {

         if( Ncol == 3 )
            for( k=0 ; k<3 ; k++) 
               tmp[k] = INP_GRAD[i][k+1];
         else
            k = GradConv_GmatA_from_Gsign( tmp, INP_GRAD[i]+1);
         if( USE_BWT ) // weight if nec
            for ( k=0 ; k<Ncol ; k++)
               tmp[k]*= INP_GRAD[i][0];

         if( EXTRA_col )
            OUT[ctr][0] = INP_GRAD[i][0];

         for ( k=0 ; k<Ncol ; k++) 
            OUT[ctr][EXTRA_col+k] = tmp[k]; // start at either 0 or 1
         
         BOUT[ctr] = (int) rintf(INP_GRAD[i][0]);  // and bval entry 

         ctr++;
      }

   Nrow_final = ctr;
   if( DWI_COMP_FAC ) // take care of case with compression
      Nrow_final = Ndwi_flagged + EXTRA_row;
   
   
   INFO_message("Output dims to be: %d x %d", 
                Nrow_final,
                Ncol+EXTRA_col);


   // -----------------------------------------------------------

   if( (fout = fopen(Fname_output, "w")) == NULL) {
      fprintf(stderr, "\n\nError opening file %s.\n",Fname_output);
      exit(1);
   }
   
   // 0 is grad row;  
   // 1 is grad col;
   // 2 is gmatrRow col T;
   // 3 is gmatrDiag col A;
   // 4 is bmatrRow col T;
   // 5 is bmatrDiag col A;

   if( OUT_FORM>0) {
      for( i=0 ; i<Nrow_final ; i++) {
         if( EXTRA_col)
            fprintf(fout,"%8d  ", (int) OUT[i][0]);
         for( k=0 ; k<Ncol ; k++) 
            fprintf(fout,"%11.5f  ", OUT[i][EXTRA_col+k]);
         fprintf(fout,"\n");
      }
   }
   else if( OUT_FORM==0 ) {
      if(BVAL_OUT)
         WARNING_message("Ignoring '-out_bval_col' option, since "
                         " you are outputting in rows.");
      
      for( k=0 ; k<Ncol ; k++) {
         for( i=0 ; i<Nrow_final ; i++) 
            fprintf(fout,"%11.5f  ", OUT[i][k]);
         fprintf(fout,"\n");
      }
   }
   fclose(fout);

   INFO_message("\t -> DONE with grad/matr. "
                "Check output file '%s'.\n\n",
                Fname_output);

   if( BVAL_OUT_SEP ) {
      if( (foutBV = fopen(Fname_outputBV, "w")) == NULL) {
         fprintf(stderr, "\n\nError opening file %s.\n",Fname_outputBV);
         exit(1);
      }

      for( i=0 ; i<Nrow_final ; i++) {
         fprintf(foutBV,"%8d  ", BOUT[i]);
         if( BVAL_OUT_SEP==2 )
            fprintf(foutBV,"\n");
      }
      if( BVAL_OUT_SEP==1 ) 
         fprintf(foutBV,"\n");

      fclose(foutBV);
      INFO_message("\t -> DONE with b-value file '%s'.\n\n",
                   Fname_outputBV);
   }

   if(dwset) {
      INFO_message("Processing the B0+DWI file now.");
      if(!BZER) {
         fprintf(stderr, "\n** Error in processing data set: "
                 "no b=0 values from bvecs/bval info!\n");
         exit(5);
      }

      // FLAG marks where DWIs are if not using '-keep_b0s'!

      Nvox = DSET_NVOX(dwset);
      Ndwout = Ndwi+1;

      temp_arr = calloc( Ndwout,sizeof(temp_arr));
      for( i=0 ; i<Ndwout ; i++) 
         temp_arr[i] = calloc( Nvox,sizeof(float)); 
      temp_grad = calloc( Ndwi,sizeof(temp_grad));
      for( i=0 ; i<Ndwi ; i++) 
         temp_grad[i] = calloc( 3,sizeof(float)); 

      if( (temp_arr == NULL) || (temp_grad == NULL) ) {
            fprintf(stderr, "\n\n MemAlloc failure.\n\n");
            exit(123);
      }

      dwi = 0; // keep track of DWI contraction
      for( i=0 ; i<Nbrik ; i++)
         if( !FLAG[i] ) // b=0
            for( j=0 ; j<Nvox ; j++)
               temp_arr[0][j]+= THD_get_voxel(dwset,j,i);
         else {
            for( j=0 ; j<3 ; j++)
               temp_grad[dwi][j]= INP_GRAD[i][j+1];
            dwi++;
            for( j=0 ; j<Nvox ; j++)
               temp_arr[dwi][j]+= THD_get_voxel(dwset,j,i);
         }
      if( dwi != Ndwi ) {
         fprintf(stderr, "\n** Mismatch in internal DWI counting!\n");
         exit(6);
      }

      // average the values
      for( j=0 ; j<Nvox ; j++)
         temp_arr[0][j]/= BZER; // can't be zero here.
      
      if( DWI_COMP_FAC ) {
         INFO_message("Compressing DWI file");

         for( k=1 ; k<DWI_COMP_FAC ; k++)
            for( i=0 ; i<Ndwi_flagged ; i++)
               for( j=0 ; j<Nvox ; j++)
                  temp_arr[1+i][j]+= temp_arr[1+k*Ndwi_flagged+i][j];
         
         for( i=0 ; i<Ndwi_flagged ; i++)
            for( j=0 ; j<Nvox ; j++)
               temp_arr[1+i][j]/= DWI_COMP_FAC;

         INFO_message("Checking closeness of compressed gradient values");
         MaxDP = GradCloseness(temp_grad, Ndwi, DWI_COMP_FAC);

         INFO_message("The max angular difference between matched/compressed\n"
                      "\tgradients is: %f", MaxDP);
         if( MaxDP > 2)
            WARNING_message("The max angular difference seem kinda big-- you\n"
                            " sure about the compression factor?");
      }

      Ndwout_final = Ndwi_flagged + 1;
      INFO_message("Writing the processed data set.");
      dwout = EDIT_empty_copy( dwset ); 
      EDIT_dset_items(dwout,
                      ADN_nvals, Ndwout_final,
                      ADN_ntt, 0,
                      ADN_datum_all, MRI_float , 
                      ADN_prefix, prefix,
                      ADN_none );

      for( i=0; i<Ndwout_final ; i++) {
         EDIT_substitute_brick(dwout, i, MRI_float, temp_arr[i]);
         temp_arr[i]=NULL;
      }

      // if necessary
      for( i=Ndwout_final ; i<Ndwout ; i++)
         temp_arr[i]=NULL;

      THD_load_statistics( dwout );
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dwout)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(dwout));
      tross_Make_History("1dDW_Grad_o_Mat", argc, argv, dwout);
      THD_write_3dim_dataset(NULL, NULL, dwout, True);
      DSET_delete(dwout); 
      free(dwout); 
      DSET_delete(dwset); 
      free(dwset); 

      for( i=0 ; i<Ndwout_final ; i++)
         free(temp_arr[i]);
      free(temp_arr);
   }
   
   mri_free(preREADIN);
   if( HAVE_BVAL )
      mri_free(preREADBVAL);
   if(prefix)
      free(prefix);

   
   if(BOUT)
      free(BOUT);
   if(OUT){
      for( i=0 ; i<(idx+EXTRA_row) ; i++)
         free(OUT[i]);
      free(OUT);
   }


   if(dwset) {
      INFO_message("\t ->DONE with data_set. Check '%s'.\n\n",
                   DSET_FILECODE(dwout));
   }
   exit(0);   
}


// take a len=6 matr and an empt len =3 grad file, calc grads from matr
int GradConv_Gsign_from_GmatA( float *grad, float *matr )
{
   int i;
   int signum[3] = {1,1,1};

   if( (matr[0]<0) || (matr[1]<0) || (matr[2]<0) )
      ERROR_exit("Matrices don't appear to be correct format-- check again") ;

   // get signs for grads
   for( i=0 ; i<3 ; i++)
      if( matr[3+i] < 0 )
         signum[2-i] = -1; // if all neg, then same as having all pos because of symmetry still

   for( i=0 ; i<3 ; i++)
      if ( matr[i] >= 0 ) {
         grad[i] = (float) sqrt(matr[i]);
         grad[i]*= signum[i];
      }
      else {
         WARNING_message("matrices don't appear to be correct format-- check again") ;
         grad[i] = 0;
      }

   return 1;
}

// !! Assumes these are scaled to be unit magnitude!!
// simple conversion of grads to G-matr, diagonal form
int GradConv_GmatA_from_Gsign( float *matr, float *grad )
{
   int i;

   for( i=0 ; i<3 ; i++)
      matr[i] =  grad[i]*grad[i];
   matr[3] = grad[0]*grad[1];
   matr[4] = grad[0]*grad[2];
   matr[5] = grad[1]*grad[2];

   return 1;
}

float GradCloseness(float **X, int N, int DCF)
{
   int rep=0;
   int i,k;

   float maxang = 0, ang=0;
   

   rep = N/DCF; // from usage before, must be an exact int

   for( k=1 ; k<DCF ; k++)
      for( i=0 ; i<rep ; i++ ) {
         ang = CalcInnerProdAngle(X[i],X[i+k*rep],3);
         //fprintf(stderr,"\n%5d %5d %6f", k,i,ang);
         //fprintf(stderr,"\t, %5f %5f %5f, %5f %5f %5f",
         //        X[i][0],X[i][1],X[i][2],
         //        X[i+k*rep][0],X[i+k*rep][1],X[i+k*rep][2]);

         if( ang>maxang )
            maxang = ang;
      }

   return maxang;




}

// little dot product
float CalcInnerProdAngle( float *A, float *B, int N ) 
{
   float out = 0., d1 = 0., d2 = 0;
   int i;
   int NEG = 0;

   out = SimpleDP(A,B,N);
   d1 = SimpleDP(A,A,N);
   d2 = SimpleDP(B,B,N); 

   if( (d1<=0.0001) || (d2<=0.0001) ) {
      WARNING_message("It looks like there might be a b=0 gradient which got"
                      "misclassified?? Near- (or equal-) zero magnitude.");
      out/= 1.;
   }
   else{
      out/= sqrt(d1) * sqrt(d2);
   }

   if( out<0 ) {
      NEG = 1;
      out*=-1;
   }

   if( out>1.01 ) {
      WARNING_message("It looks like there might be a problem in the grads?\n"
                      "\tOne has large magnitude (%f>1): setting to unity.");
   } 

   if( out>1)
      out = 1;
   
   out = (float) acos(out);
   out*= 180./PI;

   if(NEG)
      WARNING_message("Gradient is 180 out of phase with matched partner.\n"
                      "\tMight still be ok -> checking abs ang diff: %5f",
                      out);

   return out;

}

// little dot product
float SimpleDP( float *A, float *B, int N ) 
{
   float out = 0.;
   int i;

   for( i=0 ; i<N ; i++ )
      out+=A[i]*B[i];

   return out;

}
