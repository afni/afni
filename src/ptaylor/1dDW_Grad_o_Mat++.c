// PA Taylor, Jan, 2017

/*
  ------------------ start: *new* 1dDW_Grad_o_Mat --------------------

  Jan 2017:
  + change default behavior to be more in line with 
  expected use (default) use.  Main one: don't average b=0s
  by default!

  May 2017:
  + fixed up helpfile

  Sep 20 2017:
  + allow user to put in a bypass for the "checks" with minimal negative
    values: push on through those tiny negs; they get replaced with 0s.
    new opt name: -check_abs_min VVV

*/


// 3dDWItoDT and BMTXT_AFNI.txt: Bxx  Byy  Bzz  Bxy  Bxz  Byz
// TORTOISE uses (internal):     Bxx 2Bxy 2Bxz  Byy 2Byz  Bzz
// BMTXT.txt from old TORT:      Bxx  Byy  Bzz 2Bxy 2Bxz 2Byz

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

// assumes I/O has unit or zero mag!
int GradConv_Gsign_from_GmatA( float *grad, float *matr ); 
int GradConv_GmatA_from_Gsign( float *matr, float *grad );

float GradCloseness(float **X, int N, int DCF);
float CalcInnerProdAngle( float *A, float *B, int N );
float SimpleDP( float *A, float *B, int N);

void usage_1dDW_Grad_o_Mat(int detail) 
{
	printf(
"  \n"
"  Simple function to manipulate DW gradient vector files, b-value\n"
"  files, and b- or g-matrices. Let: g_i be one of Ng spatial gradients\n"
"  in three dimensions; |g_i| = 1, and the g-matrix is G_{ij} = g_i * g_j\n"
"  (i.e., dyad of gradients, without b-value included); and the DW-scaled\n"
"  b-matrix is B_{ij} = b * g_i * g_j.\n"
"\n"
"  **This new version of the function** will replace the original/older \n"
"  version (1dDW_Grad_o_Mat).  The new has similar functionality, but\n"
"  improved defaults:\n"
"     + it does not average b=0 volumes together by default;\n"
"     + it does not remove top b=0 line from top by default;\n"
"     + output has same scaling as input by default (i.e., by bval or not);\n"
"       and a switch is used to turn *off* scaling, for unit magn output\n"
"       (which is cleverly concealed under the name '-unit_mag_out').\n"
"\n"
"  Wherefore, you ask?  Well, times change, and people change.\n"
"  The above functionality is still available, but each just requires\n"
"  selection with command line switches.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"As of right now, one can input:\n"
"  + 3 rows of gradients (as output from dcm2nii, for example);\n"
"  + 3 columns of gradients;\n"
"  + 6 columns of g- or b-matrices, in `diagonal-first' (-> matA) order:\n" 
"         Bxx, Byy, Bzz, Bxy, Bxz, Byz,\n"
"    which is used in 3dDWItoDT, for example;\n"
"  + 6 columns of g- or b-matrices, in `row-first' (-> matT) order:\n" 
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
"  + filter out all `zero' rows of recorded reference images, \n"
"    THOUGH this is not really recommended.\n"
"  \n"
"One can then output:\n"
"  + 3 columns of gradients;\n"
"  + 6 columns of g- or b-matrices, in 'diagonal-first' order;\n" 
"  + 6 columns of g- or b-matrices, in 'row-first' order;\n" 
"  + as well as including a column of b-values (such as used in, e.g.,\n" 
"    DSI-Studio);\n"
"  + as well as explicitly include a row of zeros at the top;\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
" + RUNNING:\n"
"    1dDW_Grad_o_Mat++                                               \\\n"
"         { -in_row_vec  | -in_col_vec  |                            \\\n"
"           -in_col_matA | -in_col_matT }  INFILE                    \\\n"
"         { -flip_x | -flip_y | -flip_z | -no_flip }                 \\\n"
"         { -out_row_vec  | -out_col_vec  |                          \\\n"
"           -out_col_matA | -out_col_matT }  OUTFILE                 \\\n"
"         { -in_bvals BVAL_FILE }                                    \\\n"
"         { -out_col_bval }                                          \\\n"
"         { -out_row_bval_sep BB | -out_col_bval_sep BB }            \\\n"
"         { -unit_mag_out }                                          \\\n"
"         { -bref_mean_top }                                         \\\n"
"         { -bmax_ref THRESH }                                       \\\n"
"         { -put_zeros_top   }                                       \\\n"
"    where:\n"
"        (one of the following formats of input must be given):\n"
"    -in_row_vec   INFILE  :input file of 3 rows of gradients (e.g.,\n"
"                           dcm2nii-format output).\n"
"    -in_col_vec   INFILE  :input file of 3 columns of gradients.  \n"
"    -in_col_matA  INFILE  :input file of 6 columns of b- or g-matrix in\n"
"                           'A(FNI)' `diagonal first'-format. (See above.)\n"
"    -in_col_matT  INFILE  :input file of 6 columns of b- or g-matrix in \n"
"                           'T(ORTOISE)' `row first'-format. (See above.)\n"
"\n"
"        (one of the following formats of output must be given):\n"
"    -out_row_vec  OUTFILE :output file of 3 rows of gradients.\n"
"    -out_col_vec  OUTFILE :output file of 3 columns of gradients.\n"
"    -out_col_matA OUTFILE :output file of 6 columns of b- or g-matrix in\n"
"                           'A(FNI)' `diagonal first'-format. (See above.)\n"
"    -out_col_matT OUTFILE :output file of 6 cols of b- or g-matrix in\n"
"                           'T(ORTOISE)' `row first'-format. (See above.)\n"
"\n"
"        (and any of the following options may be used):\n"
"\n"
"    -in_bvals  BVAL_FILE  :BVAL_FILE is a file of b-values, either a single\n"
"                           row (such as the 'bval' file generated by\n"
"                           dcm2nii) or a single column of numbers.  Must\n"
"                           have the same number of entries as the number\n"
"                            of grad vectors or matrices.\n"
"    -out_col_bval         :switch to put a column of the bvalues as the\n"
"                           first column in the output data.\n"
"    -out_row_bval_sep BB  :output a file BB of bvalues in a single row.\n"
"    -out_col_bval_sep BB  :output a file BB of bvalues in a single column.\n"
"\n"
"    -unit_mag_out         :switch so that each vector/matrix from the INFILE\n"
"                           is scaled to either unit or zero magnitude.\n"
"                           (Supplementary input bvalues would be ignored\n"
"                           in the output matrix/vector, but not in the\n"
"                           output bvalues themselves.)  The default\n"
"                           behavior of the function is to leave the output\n"
"                           scaled however it is input (while also applying\n"
"                           any input BVAL_FILE). \n"
"\n"
"    -flip_x               :change sign of first column of gradients (or of\n"
"                           the x-component parts of the matrix)\n"
"    -flip_y               :change sign of second column of gradients (or of\n"
"                           the y-component parts of the matrix)\n"
"    -flip_z               :change sign of third column of gradients (or of\n"
"                           the z-component parts of the matrix)\n"
"    -no_flip              :don't change any gradient/matrix signs.  This\n"
"                           is an extraneous switch, as the default is to\n"
"                           not flip any signs (this is mainly used for\n"
"                           some scripting convenience\n"
"\n"
"    -check_abs_min VVV    :By default, this program checks input matrix \n"
"                           formats for consistency (having positive semi-\n"
"                           definite diagonal matrix elements).  It will fail\n"
"                           if those don't occur. However, sometimes there is\n"
"                           just a tiny values <0, like a rounding error; \n"
"                           you can specify to push throughfor negative \n"
"                           diagonal elements with magnitude <VVV, with those\n"
"                           values getting replaced by zero.  Be judicious\n"
"                           with this power! (E.g., maybe VVV ~ 0.0001 might\n"
"                           be OK... but if you get looots of negatives, then\n"
"                           you really, really need to check your data for\n"
"                           badness.\n"
"\n"
"       (and the follow options are probably mainly extraneous, nowadays)\n"
"    -bref_mean_top        :when averaging the reference X 'b0' values (the\n"
"                           default behavior), have the mean of the X \n"
"                           values be represented in the top row; default \n"
"                           behavior is to have nothing representing the b0\n"
"                           information in the top row (for historical\n"
"                           functionality reasons).  NB: if your reference\n"
"                           'b0' actually has b>0, you might not want to \n"
"                           average the b0 refs together, because their\n"
"                           images could have differing contrast if the\n"
"                           same reference vector wasn't used for each.\n"
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
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  EXAMPLES\n"
"\n"
"   # An example of type-conversion from a TORTOISE-style matrix to column\n"
"   # gradients (if the matT file has bweights, so will the grad values):\n"
"\n"
"   1dDW_Grad_o_Mat++                                    \\\n"
"      -in_col_matT   BMTXT_TORT.txt                     \\\n"
"      -out_col_vec   GRAD.dat                           \n"
"\n"
"\n"
"   # An example of filtering (note the different styles of parentheses\n"
"   # for the column- and row-type files) and type-conversion (to an\n"
"   # AFNI-style matrix that should have the bvalue weights afterwards):\n"
"\n"
"   1dDW_Grad_o_Mat++                                    \\\n"
"      -in_col_vec    GRADS_col.dat'{0..10,12..30}'      \\\n"
"      -in_bvals      BVALS_row.dat'[0..10,12..30]'      \\\n"
"      -out_col_matA  FILT_matA.dat                      \n"
"\n"
"\n"
"   # An example of filtering *without* type-conversion.  Here, note\n"
"   # the '-unit_mag_out' flag is used so that the output row-vec does\n"
"   # not carry the bvalue weight with it;  it does not affect the output\n"
"   # bval file.  As Levon might say, the '-unit_mag_out' option acts to\n"
"   #   'Take a load off bvecs, take a load for free;\n"
"   #    Take a load off bvecs, and you put the load right on bvals only.'\n"
"   # This example might be useful for working with dcm2nii* output:\n"
"\n"
"   1dDW_Grad_o_Mat++                                      \\\n"
"      -in_row_vec        ap.bvec'[0..10,12..30]'          \\\n"
"      -in_bvals          ap.bval'[0..10,12..30]'          \\\n"
"      -out_row_vec       FILT_ap.bvec                     \\\n"
"      -out_row_bval_sep  FILT_ap.bval                     \\\n"
"      -unit_mag_out\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"\n"
"  If you use this program, please reference the introductory/description\n"
"  paper for the FATCAT toolbox:\n"
"        Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional\n"
"        And Tractographic Connectivity Analysis Toolbox. Brain \n"
"        Connectivity 3(5):523-535.\n"
"___________________________________________________________________________\n"
          );
	return;
}

int main(int argc, char *argv[]) 
{  
   int CHECK = 0, CHECK_OK = 0;
   float check_min = 0.;   // Sep 20, 2017: now all for non-neg stuff
   int CHECK_REP = 1;      // Sep 20, 2017: by def, replace tiny negs with 0
	int iarg;
   char *Fname_input = NULL;
   char *Fname_output = NULL;
   char *Fname_outputBV = NULL;
   char *Fname_bval = NULL;
   int opt;
   FILE *fin=NULL, *fout=NULL, *finbv=NULL, *foutBV=NULL;
   int i,j,k;
   int BZER=0;//,idx=0,idx2=0;

   MRI_IMAGE *flim=NULL;
   MRI_IMAGE *preREADIN=NULL;
   MRI_IMAGE *preREADBVAL=NULL;
   float *READIN=NULL;
   float *READBVAL=NULL;

   int Nvm = -1;                          // number of vec/mats
   int UNIT_MAGN_OUT = 0;
   float **INP_VEC=NULL;    
   float **INP_MAT=NULL;
   int *FLAG=NULL;
   
   float magn = 1.;
   int INV[3] = {1,1,1}; // if needing to switch
   
   int USE_BWT = 1;       // DO mult by magn: on by default!
   float MAX_BVAL = 0.;   // calc max. bval for possible use scaling
   int EXTRA_ZEROS=0;

   float temp;
   int PUT_MEAN_BREF_TOP=0;
   int HAVE_BVAL = 0;
   int HAVE_BVAL_EFF = 0; // can calc bval from input, not sep file
   int BVAL_OUT = 0; 
   int BVAL_OUT_SEP = 0; 
   float BMAX_REF = MINBVAL; // i.e., essentially zero
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
   float *BOUT=NULL;


	mainENTRY("1dDW_Grad_o_Mat++"); machdep();
    
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
      // mainly for scripting; can clean up stuff.
      if( strcmp(argv[iarg],"-no_flip") == 0) {
			INV[0] = 1;
			INV[1] = 1;
			INV[2] = 1;
			iarg++ ; continue ;
		}

      // -------------- inp format opts -----------------------------

      if( strcmp(argv[iarg],"-in_row_vec") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_row_vec'\n") ;
         IN_FORM = 0;  //, which is just default, because of dcm2nii
         Fname_input = argv[iarg];
         count_in++;
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_col_vec") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_col_vec'\n") ;
         IN_FORM = 1;
         Fname_input = argv[iarg];
         count_in++;
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-in_col_matA") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_col_matA'\n") ;
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 2;
         iarg++ ; continue ;
		} 

      if( strcmp(argv[iarg],"-in_col_matT") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_col_matT'\n") ;
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 3;
         iarg++ ; continue ;
		}
      
      // -------------- other inp ---------------------------------

      // !!!!!!!!!!!!!!!!!! and have a column opt!!!!
      if( strcmp(argv[iarg],"-in_bvals") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_bvals'\n") ;
         Fname_bval = argv[iarg];
         HAVE_BVAL = 1;
         iarg++ ; continue ;
		}

      // now, though, this shouldn't be so important
      if( strcmp(argv[iarg],"-bmax_ref") == 0) { 
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-bmax_ref'\n");
         BMAX_REF = atof(argv[iarg]);
         HAVE_BMAX_REF = 1;
         iarg++ ; continue ;
		}

      // -------------- outp format opts --------------------------

      if( strcmp(argv[iarg],"-out_row_vec") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_row_vec'\n") ;
         OUT_FORM = 0;  //, which is just default, because of dcm2nii
         Fname_output = argv[iarg];
         count_out++;
         iarg++ ; continue ;
		}
      
      if( strcmp(argv[iarg],"-out_col_vec") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_col_vec'\n") ;
         OUT_FORM = 1;
         Fname_output = argv[iarg];
         count_out++;
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_col_matA") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_col_matA'\n") ;
         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 2;
         iarg++ ; continue ;
		} 

      if( strcmp(argv[iarg],"-out_col_matT") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_col_matT'\n") ;
         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 3;
         iarg++ ; continue ;
		}

      // -------------- other outp opts ----------------------------

      if( strcmp(argv[iarg],"-unit_mag_out") == 0) { 
			USE_BWT = 0;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_col_bval") == 0) { // not extra file
			BVAL_OUT = 1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_row_bval_sep") == 0) {
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_row_bval_sep'\n") ;
         Fname_outputBV = argv[iarg];
         BVAL_OUT_SEP = 1;
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_col_bval_sep") == 0) {
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_col_bval_sep'\n") ;
         Fname_outputBV = argv[iarg];
         BVAL_OUT_SEP = 2;
         iarg++ ; continue ;
		}
      
      // [PT: Sep 20, 2017] Allow tiny negatives in diags
      if( strcmp(argv[iarg],"-check_abs_min") == 0) {
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-check_abs_min'\n") ;
         check_min = atof(argv[iarg]);
         iarg++ ; continue ;
		}

      // FOR NOW: leave out, because this causes problems with grad calcs!
      // [PT: Sep 20, 2017] Def: replace tiny negs; can propagate, too
      //if( strcmp(argv[iarg],"-check_leave_neg") == 0) { 
      //		CHECK_REP = 0;
		//	iarg++ ; continue ;
		//}

      // -------------------------------------------------------------

      if( strcmp(argv[iarg],"-put_zeros_top") == 0) {
			EXTRA_ZEROS = 1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-bref_mean_top") == 0) {
			PUT_MEAN_BREF_TOP = 1;
			iarg++ ; continue ;
		}
		
      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
   }
   
   //  * * * * * * * * * * * * * * * * * * * * * * * * * * * 
   
   if( (Fname_input == NULL) ) 
      ERROR_exit("Bad command lining!  Option '-in_*' requires argument.");
   
   if( (Fname_output == NULL) ) 
      ERROR_exit("Bad command lining!  Option '-out_*' requires arg.");

   if( count_in > 1 ) 
      ERROR_exit("Bad command lining!  Can't have >1 vec file input.");
   
   if( count_out > 1 ) 
      ERROR_exit("Bad command lining!  Can't have >1 output file opt.");
   
   if(PUT_MEAN_BREF_TOP && EXTRA_ZEROS ) 
      ERROR_exit("Incompatible combo of options: can't have "
                 "-bref_mean_top with -put_zeros_top");
   
   if( !prefix && dwset) 
      ERROR_exit("Bad command lining! "
                 "Need an output '-pref_dset' when using '-proc_dset'");
   
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
   Nvm = preREADIN->ny;  // number of vec/mats
   
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
      if( preREADBVAL->ny != Nvm ) {
         ERROR_message("ERROR! Number of vec/mats (%d) doesn't match number "
                       "of bvals in bval file (%d)", Nvm, preREADBVAL->ny);
         mri_free (preREADIN);
         mri_free (preREADBVAL);
         exit(4);
      }
   }

   // ------------- lots of checks for various things.  Ugh. -------------
   
   if(Nvm >= MAXGRADS ) {
      ERROR_message("ERROR, too many input grads.");
      mri_free (preREADIN);
      if( HAVE_BVAL ) mri_free (preREADBVAL);
      exit(4);
   }
   
   if( (preREADIN->nx != 3 ) && (preREADIN->nx != 6 ) )
      ERROR_exit("Input file doesn't have 3 or 6 columns!\n");
   
   if(dwset) {
      Nbrik = DSET_NVALS(dwset);
      
      if( Nvm != Nbrik ) {
         ERROR_message("ERROR: the number of bvecs (%d) does not match the "
                       "number of briks in '-proc_dset' (%d).\n", Nvm, Nbrik);
         mri_free (preREADIN);
         DSET_delete(dwset); 
         free(dwset); 
         exit(4);
      }
   }
   
   INP_VEC = calloc(Nvm, sizeof(INP_VEC)); 
   for(i=0 ; i<Nvm ; i++) 
      INP_VEC[i] = calloc(4, sizeof(float)); 
   INP_MAT = calloc(Nvm, sizeof(INP_MAT)); 
   for(i=0 ; i<Nvm ; i++) 
      INP_MAT[i] = calloc(7, sizeof(float)); 
   FLAG = (int *)calloc(Nvm, sizeof(int)); 
                                                
   if( (INP_MAT == NULL) || (INP_VEC == NULL) || (FLAG == NULL)) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(14);
   }
   
   // initialize to have unit magn in magn col
   for( i=0; i<Nvm ; i++ ) {
      INP_VEC[i][0] = 1.;
      INP_MAT[i][0] = 1.;
   }
   
   // ---------- Start reading float pointer ----------------------

   // Put bval info here, if it was input separately.  Later, these
   // will also get multiplied by the magnitude of the vecs/mats
   if( HAVE_BVAL ) { 
      READBVAL = MRI_FLOAT_PTR( preREADBVAL );
      for( i=0; i<Nvm ; i++ ) {
         INP_VEC[i][0]*= *(READBVAL + i);
         INP_MAT[i][0]*= *(READBVAL + i);
      }
   }

   READIN = MRI_FLOAT_PTR( preREADIN );

   // 0 or 1 is vec (have been transposed);  
   // 2 is mat A;
   // 3 is mat T;
   if ( (IN_FORM == 0) || (IN_FORM == 1) )  // vec (now each) cols
      for( i=0; i<Nvm ; i++ )
         for ( j=0; j<3 ; j++ )
            INP_VEC[i][j+1] = *(READIN + 3*i+j);
   else if ( IN_FORM == 2) { // matA cols
      for( i=0; i<Nvm ; i++ ) {
         for ( j=0; j<3 ; j++ ) {
            INP_MAT[i][j+1]   = *(READIN+6*i+j);
            INP_MAT[i][3+j+1] = *(READIN+6*i+j+3);
         }
         for( j=1; j<4 ; j++ ) 
            if( INP_MAT[i][j] < 0. ) {
               CHECK++;
               // [PT: Sep 20, 2017] New patchability
               if( fabs(INP_MAT[i][j]) < check_min ) {
                  CHECK_OK++;
                  if( CHECK_REP )
                     INP_MAT[i][j] = 0.;
               }
            }
      }
      if(CHECK > 0) {
         WARNING_message("You *said* you input with 'matA'-style,\n"
                         "\t but the matr diagonals don't appear to be "
                         "uniformly\n"
                         "\t positive. If input cols 0, 3 and 5 are positive,\n"
                         "\t then you might have meant 'matT'?");
         WARNING_message("You have %d diagonal matrix elements "
                         "with <0 values,\n"
                         "\t and of these %d had magnitude less "
                         "than your min %f",
                         CHECK, CHECK_OK, check_min);
         if( CHECK_REP && CHECK_OK)
            INFO_message("NB: %d of those 'tiny' negatives were "
                         "replaced with 0.",
                         CHECK_OK);
         if( CHECK_OK != CHECK)
            ERROR_exit("There were %d diagonal elements with too large of "
                       "negative value.\n"
                       "\t If you want to push through this, "
                       "see '-check_abs_min'.\n"
                       "\t Bye!", CHECK-CHECK_OK);
      }
   }
   else if ( IN_FORM == 3) { // matT cols
      for( i=0; i<Nvm ; i++ ) {
         INP_MAT[i][1] = *(READIN +6*i);
         INP_MAT[i][2] = *(READIN +6*i+3);
         INP_MAT[i][3] = *(READIN +6*i+5);
         INP_MAT[i][4] = *(READIN +6*i+1)/2.;
         INP_MAT[i][5] = *(READIN +6*i+2)/2.;
         INP_MAT[i][6] = *(READIN +6*i+4)/2.;
         
         for( j=1; j<4 ; j++ ) 
            if( INP_MAT[i][j] < 0. ) {
               CHECK++;
               // [PT: Sep 20, 2017] New patchability
               if( fabs(INP_MAT[i][j]) < check_min ) {
                  CHECK_OK++;
                  if( CHECK_REP )
                     INP_MAT[i][j] = 0.;
               }
            }
      }
      if(CHECK > 0) {
         WARNING_message("You *said* you input a 'matT',\n"
                         "\t but the matr diagonals don't appear to be "
                         "uniformly\n"
                         "\t positive. If input cols 0, 1 and 2 are positive,\n"
                         "\t then you might have meant 'matA'?");
         WARNING_message("You have %d diagonal matrix elements "
                         "with <0 values,\n"
                         "\t and of these %d had magnitude less than "
                         "your min %f",
                         CHECK, CHECK_OK, check_min);
         if( CHECK_REP && CHECK_OK)
            INFO_message("NB: %d of those 'tiny' negatives were replaced "
                         "with 0.",
                         CHECK_OK);
         if( CHECK_OK != CHECK)
            ERROR_exit("There were %d diagonal elements with too large of "
                       "negative value.\n"
                       "\t If you want to push through this, "
                       "see '-check_abs_min'.\n"
                       "\t Bye!", CHECK-CHECK_OK);
      }
   }
   else
      ERROR_exit("Coding error with format number (%d), not allowed.",
                 IN_FORM);
   
   INFO_message("Have read in data.");
   
   // ---------- getting magnitudes, where appropriate ---------------------
   
   // at this point, we only have two cases: vecs or matAs.  Now, we
   // need to 1) calc vecs from mats and vice versa, and 2) separate
   // magn and vec/mat parts.

   // MAT case
   if ( ( IN_FORM==2 ) || (IN_FORM==3 ) ) {
      for( i=0; i<Nvm ; i++ ) {
         magn = INP_MAT[i][1] + INP_MAT[i][2] + INP_MAT[i][3];
         if( magn > MINBVAL)
            for( j=1 ; j<7 ; j++ )
               INP_MAT[i][j]/= magn;
         INP_MAT[i][0]*= magn;
         INP_VEC[i][0]*= magn;
      }
   }  // -> now have magn col0 and unit norm (or zero) mat. Hopefully.
   else if ( ( IN_FORM==0 ) || ( IN_FORM==1 )) { // vec
      for( i=0; i<Nvm ; i++ ) {
         magn = sqrt( INP_VEC[i][1]*INP_VEC[i][1] + 
                      INP_VEC[i][2]*INP_VEC[i][2] + 
                      INP_VEC[i][3]*INP_VEC[i][3] );
         if( magn > MINBVAL)
            for( j=1 ; j<4 ; j++ )
               INP_VEC[i][j]/= magn;
         INP_MAT[i][0]*= magn;
         INP_VEC[i][0]*= magn;
      }
   } // -> now have magn col0 and unit norm (or zero) vec. Hopefully.

   INFO_message("Have organized the data internally.");

   // * * *  ** * * * * * * * * ** ** * * ** * * ** * ** * ** * * *
   // at this point, all cases,
   //    + vec[i][0]  has bval
   //    + mat[i][0]  has bval
   //    + vec[i][>0] normalized
   //    + mat[i][>0] normalized and in diagonal form
   // ... this should be true even if the inputs were just grads or 
   // gmats without real separate bvalue info other than just binary
   // 0/1.  Simpler to deal with in this way.
   // * * *  ** * * * * * * * * ** ** * * ** * * ** * ** * ** * * *

   // ------------------------ check report max bval -----------------
   // in case we want max bval: should be able to get from either
   // OUT_* array.
   for( i=0 ; i<Nvm ; i++) 
      if( MAX_BVAL < INP_VEC[i][0] )
         MAX_BVAL = INP_VEC[i][0];

   INFO_message("Maximum bvalue appears to be: %.5f\n", MAX_BVAL);
      
   // --------------------- equivalence ---------------------------------

   // make sure everything is equivalent between grad and matr
   // info. can just deal with one now, whatever was input.
   for( i=0; i<Nvm ; i++ ) 
      if( (IN_FORM ==2 ) || (IN_FORM ==3 ) )
         j = GradConv_Gsign_from_GmatA( INP_VEC[i]+1, INP_MAT[i]+1);
      else
         j = GradConv_GmatA_from_Gsign( INP_MAT[i]+1, INP_VEC[i]+1);

   // ************************ NOTE ***************************************

   // **FROM HERE ON**: just use grads, and then make whatever output
   // **is necessary!  simpler that way. (SO, really didn't need to
   // **calculate GmatA just above, but whatever.)

   // *********************************************************************

   // ----------------------- flip, if requested --------------------------

   for( i=0 ; i<Nvm ; i++) 
      for( j=0 ; j<3 ; j++)
         INP_VEC[i][j+1]*= INV[j];
   
   INFO_message("My view of grads at the moment (postflip, if asked for):");
   fprintf(stderr,"\n%15s\t%15s\t%15s\t%15s", 
           "bval scale", "bvecs: x", "y", "z");
   for( i=0 ; i<Nvm ; i++) {
      fprintf(stderr,"\n");
      for( j=0 ; j<4 ; j++)
         fprintf(stderr,"%15.5f\t",INP_VEC[i][j]);
   }
   fprintf(stderr,"\n\n");

   // --------------------- find reference values -------------------------

   // count/identify bzeros, even if only grad/gmatrix values were input
   BZER=0;
   for( i=0 ; i<Nvm ; i++) {
      if( INP_VEC[i][0] > BMAX_REF )   // a DWI
         FLAG[i] = 1;
      else{                            // a bref
         // keep track of its properties
         bref_vec[0]+= INP_VEC[i][0];
         // if averaging bref, then all entries *should* either have
         // the same gradient or have b=0; in either case, just
         // keeping last value should be fine.
         bref_vec[1] = INP_VEC[i][1]; // keep track of ref b; Aug,2016
         bref_vec[2] = INP_VEC[i][2]; // keep track of ref b; Aug,2016
         bref_vec[3] = INP_VEC[i][3]; // keep track of ref b; Aug,2016
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

   INFO_message("Number of ref vec/mats (b<%f): %d\n   "
                "Number of diffusion weighted  vec/mats: %d",
                BMAX_REF, BZER, Nvm-BZER);

   if( !PUT_MEAN_BREF_TOP ) {
      INFO_message("Keeping all vec/mats.");
      BZER=0;
      Ndwi_flagged = Nvm;
      // flag everything to be copied over.
      for( i=0 ; i<Nvm ; i++) 
         FLAG[i] = 1; 

      if( EXTRA_ZEROS ) 
         INFO_message("... and placing an extra row of zeros at the top.");

   }
   else {
      Ndwi_flagged = Ndwi = Nvm - BZER;
      INFO_message("Averaging %d brefs, leaving 1 bref and %d weighted ones.", 
                   BZER, Ndwi);

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
   Nrow = Nvm; //Ndwi_flagged;
   if ( EXTRA_ZEROS || PUT_MEAN_BREF_TOP )
      EXTRA_row = 1;

   if( (OUT_FORM == 0) || (OUT_FORM == 1) )
      Ncol = 3;
   else if( (OUT_FORM == 2) || (OUT_FORM == 3) ) 
      Ncol = 6;

   if( BVAL_OUT ) // extra row for outputting bval
      EXTRA_col = 1;

   OUT = calloc( Nvm+EXTRA_row, sizeof(OUT));
   for( i=0 ; i<(Nvm+EXTRA_row) ; i++) 
      OUT[i] = calloc( Ncol+EXTRA_col, sizeof(float)); 
   BOUT = (float *) calloc(Nvm+EXTRA_row, sizeof(float));

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

      if( USE_BWT )                 // weight if nec: on by default
         for ( k=0 ; k<Ncol ; k++)
            tmp[k]*= bref_vec[0];
      
      if( EXTRA_col )
         OUT[0][0] = bref_vec[0];

      // g- or b-mats: deal with row or diag format
      if( OUT_FORM==3 ) {      // TORT-style
         OUT[0][EXTRA_col+0] = tmp[0];
         OUT[0][EXTRA_col+1] = 2.*tmp[3];
         OUT[0][EXTRA_col+2] = 2.*tmp[4];
         OUT[0][EXTRA_col+3] = tmp[1];
         OUT[0][EXTRA_col+4] = 2.*tmp[5];
         OUT[0][EXTRA_col+5] = tmp[2];
      }
      else{
         for ( k=0 ; k<Ncol ; k++) 
            OUT[0][EXTRA_col+k] = tmp[k]; // start at either 0 or 1
      }

      BOUT[0] = bref_vec[0];  // and bval entry 
   }

   // now go through all flagged rows
   ctr = EXTRA_row; // start at 0 or 1
   for( i=0 ; i<Nvm ; i++) 
      if( FLAG[i] ) {

         if( Ncol == 3 )
            for( k=0 ; k<3 ; k++) 
               tmp[k] = INP_VEC[i][k+1];
         else
            k = GradConv_GmatA_from_Gsign( tmp, INP_VEC[i]+1);
         if( USE_BWT ) // weight if nec
            for ( k=0 ; k<Ncol ; k++)
               tmp[k]*= INP_VEC[i][0];

         if( EXTRA_col )
            OUT[ctr][0] = INP_VEC[i][0];

         // g- or b-mats: deal with row or diag format
         if( OUT_FORM==3 ) {      // TORT-style
            OUT[ctr][EXTRA_col+0] = tmp[0];
            OUT[ctr][EXTRA_col+1] = 2.*tmp[3];
            OUT[ctr][EXTRA_col+2] = 2.*tmp[4];
            OUT[ctr][EXTRA_col+3] = tmp[1];
            OUT[ctr][EXTRA_col+4] = 2.*tmp[5];
            OUT[ctr][EXTRA_col+5] = tmp[2];
         }
         else{
            for ( k=0 ; k<Ncol ; k++) 
               OUT[ctr][EXTRA_col+k] = tmp[k]; // start at either 0 or 1
         }

         BOUT[ctr] = INP_VEC[i][0];  // and bval entry 

         ctr++;
      }

   Nrow_final = ctr;

   INFO_message("Output dims to be: %d x %d", 
                Nrow_final,
                Ncol+EXTRA_col);
   
   // -----------------------------------------------------------
   
   if( (fout = fopen(Fname_output, "w")) == NULL) {
   fprintf(stderr, "\n\nError opening file %s.\n",Fname_output);
   exit(1);
   }
   
   // 0 is vec row;  
   // 1 is vec col;
   // 2 is matA;
   // 3 is matT
   if( OUT_FORM>0) {
      for( i=0 ; i<Nrow_final ; i++) {
         if( EXTRA_col)
            fprintf(fout,"%10.3f  ", OUT[i][0]);
         for( k=0 ; k<Ncol ; k++) 
            fprintf(fout,"%15.6f  ", OUT[i][EXTRA_col+k]);
         fprintf(fout,"\n");
      }
   }
   else if( OUT_FORM==0 ) {
      if(BVAL_OUT)
         WARNING_message("Ignoring '-out_col_bval' option, since "
                         " you are outputting in rows.");
      
      for( k=0 ; k<Ncol ; k++) {
         for( i=0 ; i<Nrow_final ; i++) 
            fprintf(fout,"%15.6f  ", OUT[i][k]);
         fprintf(fout,"\n");
      }
   }
   fclose(fout);

   INFO_message("DONE with grad/matr. "
                "Check output file '%s'\n\n",
                Fname_output);

   // ---- bval out sep, if asked for -------------
   if( BVAL_OUT_SEP ) {
      if( (foutBV = fopen(Fname_outputBV, "w")) == NULL) {
         fprintf(stderr, "\n\nError opening file %s.\n", Fname_outputBV);
         exit(1);
      }
      
      for( i=0 ; i<Nrow_final ; i++) {
         fprintf(foutBV,"%10.3f ", BOUT[i]);
         if( BVAL_OUT_SEP==2 )
            fprintf(foutBV,"\n");
      }
      if( BVAL_OUT_SEP==1 ) 
         fprintf(foutBV,"\n");
      
      fclose(foutBV);
      INFO_message("DONE with b-value file '%s'\n\n",
                   Fname_outputBV);
   }
   
   // ------------------------ FREE STUFF ---------------------------
   
   mri_free(preREADIN);
   if( HAVE_BVAL )
      mri_free(preREADBVAL);
   if(prefix)
      free(prefix);
   
   if( INP_VEC ) {
      for(i=0 ; i<Nvm ; i++) 
         free(INP_VEC[i]);
      free(INP_VEC);
   }
   if( INP_MAT ) {
      for(i=0 ; i<Nvm ; i++) 
         free(INP_MAT[i]);
      free(INP_MAT);
   }
   free(FLAG);
   
   if(BOUT)
      free(BOUT);
   if(OUT){
      for( i=0 ; i<(Nvm+EXTRA_row) ; i++)
         free(OUT[i]);
      free(OUT);
   }


   if(dwset) {
      INFO_message("\t-> DONE with data_set. Check '%s'\n\n",
                   DSET_FILECODE(dwout));
   }
   exit(0);   
}














// =======================================================================



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
