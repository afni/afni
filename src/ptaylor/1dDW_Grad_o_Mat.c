// PA Taylor, Dec 2013


// 3dDWItoDT: Bxx, Byy, Bzz, Bxy, Bxz, Byz
// TORTOISE:  b_xx 2b_xy 2b_xz b_yy 2b_yz b_zz

// G_{ij} = diffusion weighting matrix (dyadic form)
/*      GxGx GxGy GxGz
        GxGy GyGy GyGz
        GxGz GyGz GzGz */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
# include "matrix.h"

// max number of bvecs total is 1111 from this lazy method
#define MAXGRADS (12006) 

int GradConv_Gsign_from_BmatA( float *grad, float *matr );
int GradConv_BmatA_from_Gsign( float *matr, float *grad );


void usage_1dDW_grad_o_mat(int detail) 
{
	printf(
"  \n"
"Simple function to manipulate DW gradient vector files, b-value\n"
"files, and b-/g-matrices. Let: g_i be one of Ng spatial gradients\n"
"in three dimensions; the g-matrix is G_{ij} = g_i*g_j (i.e., dyad\n"
"of gradients, without b-value included); and the DW-scaled\n"
"b-matrix is B_{ij} = b*g_i*g_j.\n\n"
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
"  + as well as including a row of zeros at the top;\n\n"
"RUNNING:\n"
"    1dDW_GradOrMat \\\n"
"         { -in_grad_rows | -in_grad_cols | -in_gmatT_cols | \n"
"           -in_gmatA_cols | -in_bmatT_cols | -in_bmatA_cols} INFILE \\\n"
"         { -flip_x -flip_y -flip_z } \\\n"
"         { -keep_b0s -put_zeros_top -out_bval_col } \\\n"
"         { -in_bvals BVAL_IN } \\\n"
"         { -bmax_ref THRESH } \\\n"
"         { -out_grad_cols | -out_gmatT_cols | -out_gmatA_cols | \n"
"           -out_bmatT_cols | -out_gmatA_cols } OUTFILE \n\n"
"    where:\n"
"        (one of the following six formats of input must be given):\n"
"    -in_grad_rows INFILE  :input file of 3 rows of gradients (e.g., dcm2nii-\n"
"                           format output)."
"    -in_grad_cols INFILE  :input file of 3 columns of gradients.  \n"
"    -in_gmatA_cols INFILE :input file of 6 columns of g-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"    -in_gmatT_cols INFILE :input file of 6 columns of g-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"    -in_bmatA_cols INFILE :input file of 6 columns of b-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"    -in_bmatT_cols INFILE :input file of 6 columns of b-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"        (one of the following five formats of output must be given):\n"
"    -out_grad_cols INFILE  :output file of 3 columns of gradients.  \n"
"    -out_gmatA_cols INFILE :output file of 6 columns of g-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"    -out_gmatT_cols INFILE :output file of 6 columns of g-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"    -out_bmatA_cols INFILE :output file of 6 columns of b-matrix in 'A(FNI)'\n"
"                           `diagonal first'-format. (See above.)\n"
"    -out_bmatT_cols INFILE :output file of 6 columns of b-matr in 'T(ORTOISE)'\n"
"                           `row first'-format. (See above.)\n"
"        (and any of the following options may be used):\n"
"    -flip_x               :change sign of first column of gradients\n"
"    -flip_y               :change sign of second column of gradients\n"
"    -flip_z               :change sign of third column of gradients\n"
"    -keep_b0s             :default function is to get rid of all reference image,\n"
"                           but this option acts as switch to keep them.\n"
"    -put_zeros_top        :whatever the output format is, add a row at the top \n"
"                           with all zeros.\n"
"    -in_bvals BVAL_IN     :BVAL_IN is a file of b-values, such as the 'bval' file\n"
"                           generated by dcm2nii.\n"
"    -bmax_ref THRESH      :BVAL_IN is a scalar number below which b-values are\n"
"                           considered `zero' or non-reference; sometimes, \n"
"                           for the reference images, the scanner has b=5 s/mm^2\n"
"                           instead of b=0 strictly. One can still flag as being\n"
"                           a reference image and filter it out, using, for\n"
"                           here, '-bvals 5.1'\n"
"    -out_bval_col         :switch to put a column of the bvalues as the first\n"
"                           column in the output data.\n\n"
          );
	return;
}

int main(int argc, char *argv[]) 
{  
   int CHECK = 0;
	int iarg;
   char *Fname_input = NULL;
   char *Fname_output = NULL;
   char *Fname_bval = NULL;
   int opt;
   FILE *fin, *fout, *finbv;
   int i,j,k;
   int BZER=0,idx=0,idx2=0;

   MRI_IMAGE *flim=NULL;
   MRI_IMAGE *preREADIN=NULL;
   MRI_IMAGE *preREADBVAL=NULL;
   float *READIN=NULL;
   float *READBVAL=NULL;

   float OUT_MATR[MAXGRADS][7]; // b- or g-matrix
   float OUT_GRAD[MAXGRADS][4]; // b- or g-matrix

   int INV[3] = {1,1,1}; // if needing to switch
   int FLAG[MAXGRADS];
   float temp;
   int YES_B = 0;
   int EXTRA_ZEROS=0;
   int HAVE_BVAL = 0;
   int BVAL_OUT = 0; 
   float BMAX_REF = 1; // i.e., essentially zero
   int IN_FORM = 0; // 0 for row, 1 for col
   int OUT_FORM = 1; // 1 for col, 2 for bmatr 
   int HAVE_BMAX_REF=0 ; // referring to user input value
   int count_in=0, count_out=0;

   if (argc == 1) { usage_1dDW_grad_o_mat(1); exit(0); }

   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
         usage_1dDW_grad_o_mat(strlen(argv[iarg])>3 ? 2:1);
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

      if( strcmp(argv[iarg],"-in_grad_rows") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_grad_rows'\n") ;

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
         
         iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-in_bmatA_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-in_matA_cols'\n") ;
         
         Fname_input = argv[iarg];
         count_in++;
         IN_FORM = 5;
         
         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_grad_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_grad_cols'\n") ;

         Fname_output = argv[iarg];
         count_out++;

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
         
         iarg++ ; continue ;
		}
      
      if( strcmp(argv[iarg],"-out_bmatA_cols") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-out_bmatA_cols'\n") ;

         Fname_output = argv[iarg];
         count_out++;
         OUT_FORM = 5;
         
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
      exit(0);
   }
   if( (Fname_output == NULL) ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Option '-out_*' requires arg.\n");
      exit(0);
   }

   if( count_in > 1 ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Can't have >1 vec file input.\n");
      exit(0);
   }
   if( count_out > 1 ) {
      fprintf(stderr,
              "\n\tBad Command-lining!  Can't have >1 output file opt.\n");
      exit(0);
   }

   // ********************************************************************
   // ************************* start reading ****************************
   // ********************************************************************

   flim = mri_read_1D (Fname_input);
   if (flim == NULL) {
         ERROR_exit("Error reading gradient vector file");
      }
   preREADIN = mri_transpose(flim); // this effectively *undoes* autotranspose
   mri_free(flim);
   idx = preREADIN->ny;

   if( HAVE_BVAL ) {
      flim = mri_read_1D (Fname_bval);
      if (flim == NULL) {
         ERROR_exit("Error reading b-value file");
      }
      preREADBVAL = mri_transpose(flim); // effectively *undoes* autotranspose
      mri_free(flim);
      idx2 = preREADBVAL->ny;

   }

   if(idx>= MAXGRADS ) {
      printf("Error, too many input grads.\n");
      mri_free (preREADIN);
      mri_free (preREADBVAL);
      exit(4);
   }

   if( ( (preREADIN->nx != 3 ) && (preREADIN->ny != 3 )) &&
       (preREADIN->nx != 6 ) )
      printf("Probably an error, because there aren't 3 or 6 numbers in columns!\n");

   if( HAVE_BVAL && ( idx != idx2 ) ) {
      printf("Error, because there aren't number of bvecs (%d)\n"
             "and bvals (%d) don't appear to match!\n", idx, idx2);
      mri_free (preREADIN);
      mri_free (preREADBVAL);
      exit(3);
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

   if( IN_FORM == 0 ) // grad rows, no binfo
      for( i=0; i<idx ; i++ ) 
         for ( j=0; j<3 ; j++ )
            OUT_GRAD[i][j+1] = *(READIN +j*idx +i) ;
   else if ( IN_FORM == 1 )  // grad cols, no binfo
      for( i=0; i<idx ; i++ ) 
         for ( j=0; j<3 ; j++ )
            OUT_GRAD[i][j+1] = *(READIN + 3*i+j);
   
   // A/row/3dDWItoDT: Bxx, Byy, Bzz, Bxy, Bxz, Byz
   // T/diag/TORTOISE:  b_xx 2b_xy 2b_xz b_yy 2b_yz b_zz
   else if ( (IN_FORM == 3) || (IN_FORM ==5 ) ) { // diag matr
      for( i=0; i<idx ; i++ ) { 
         for( j=0; j<3 ; j++ ) {
            OUT_MATR[i][j+1] = *(READIN+6*i+j);
            OUT_MATR[i][3+j+1] = *(READIN+6*i+3+j);
         }

         for( j=0; j<3 ; j++ ) 
            if(OUT_MATR[i][j] < 0 )
               CHECK++;
      }
      if(CHECK > 0)
         INFO_message("Warning: you *said* you input a mat'T',"
                      " but the matr diagonals don't appear to be uniformly"
                      " positive. If input cols 0, 3 and 5 are positive,"
                      " then you might have meant mat'A'?");
   }
   else if ( (IN_FORM ==2 ) || (IN_FORM ==4 ) ) { // row matr
      CHECK = 0;
      for( i=0; i<idx ; i++ ) {
         OUT_MATR[i][1] = *(READIN +6*i);
         OUT_MATR[i][2] = *(READIN +6*i+3);
         OUT_MATR[i][3] = *(READIN +6*i+5);
         OUT_MATR[i][4] = *(READIN +6*i+1)/2.;
         OUT_MATR[i][5] = *(READIN +6*i+2)/2.;
         OUT_MATR[i][6] = *(READIN +6*i+4)/2.;
      }
      for( i=0; i<idx ; i++ ) 
         for( j=0; j<3 ; j++ ) 
            if(OUT_MATR[i][j] < 0 )
               CHECK++;
      if(CHECK > 0)
         INFO_message("Warning: you *said* you input a mat'A',"
                      " but the matr diagonals don't appear to be uniformly"
                      " positive. If input cols 0, 1 and 2 are positive,"
                      " then you might have meant mat'T'?");
   }
   else{
      fprintf(stderr, "Coding error with format number (%d), not allowed.\n",
              IN_FORM);
      exit(2);
   }
   
   // get bval info
   if( ( (IN_FORM ==4 ) || (IN_FORM ==5 ) ) ) { //bval
      for( i=0; i<idx ; i++ ) {
         OUT_MATR[i][0] = OUT_GRAD[i][0] =
            OUT_MATR[i][1] + OUT_MATR[i][2] + OUT_MATR[i][3];
         if( OUT_MATR[i][0] > 0.000001)
            for( j=1 ; j<7 ; j++ )
               OUT_MATR[i][j]/= OUT_MATR[i][0];
      }
   }
   else if ( HAVE_BVAL )
      for( i=0; i<idx ; i++ ) {
         OUT_MATR[i][0] = OUT_GRAD[i][0] =  *(READBVAL + i);
      }
   else if ( OUT_FORM > 3 || BVAL_OUT || HAVE_BMAX_REF ) {
      fprintf(stderr, "ERROR:  you asked for b-value dependent output, but gave me no bvals to work with.\n");
      exit(2);
   }
      
   // * * *  ** * * * * * * * * ** ** * * ** * * ** * ** * ** * * *
   // at this point, all IN_FORM >1 cases which need bval have led to:
   //    + grad[0] has bval
   //    + matr[0] has bval
   //    + matr file normalized and in diagonal form
   // * * *  ** * * * * * * * * ** ** * * ** * * ** * ** * ** * * *

   for( i=0; i<idx ; i++ ) 
      if( IN_FORM > 1)
         j = GradConv_Gsign_from_BmatA( OUT_GRAD[i]+1, OUT_MATR[i]+1);
      else
         j = GradConv_BmatA_from_Gsign( OUT_MATR[i]+1, OUT_GRAD[i]+1);


   // flip if necessary
   for( i=0 ; i<idx ; i++) {
      for( j=0 ; j<3 ; j++) 
         OUT_GRAD[i][j+1]*= INV[j];
      OUT_MATR[i][4]*= INV[0]*INV[1];
      OUT_MATR[i][5]*= INV[0]*INV[2];
      OUT_MATR[i][6]*= INV[1]*INV[2];
   }
   
   BZER=0;
   for( i=0 ; i<idx ; i++) {
      if( HAVE_BVAL || (IN_FORM ==4) || (IN_FORM ==5) )
         if( OUT_GRAD[i][0] >= BMAX_REF ) 
            FLAG[i] = 1;
         else{
            if( YES_B ) 
               FLAG[i] = 1;
            BZER++;
         }
      else {
         temp = 0.;
         for( j=1 ; j<4 ; j++) 
            temp+= pow(OUT_GRAD[i][j],2);
         
         if( temp > 0.1 )
            FLAG[i] = 1;
         else{
            if( YES_B ) 
               FLAG[i] = 1;
            BZER++;
         }
      }
   }
   
   if(YES_B) {
      printf("\tChose to *keep* %d b0s,\tas well as  \t%d grads\n",
             BZER,idx-BZER);
      BZER=0;
   }
   else
      printf("\tGetting rid of  %d b0s,\tleaving the \t%d grads\n",
             BZER,idx-BZER);

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

   if( EXTRA_ZEROS ) {
      if( BVAL_OUT )
         fprintf(fout,"%8d  ", 0);

      if( OUT_FORM == 1 )
         for( k=1 ; k<4 ; k++ )
            fprintf(fout,"%11.5f  ", 0.0);
      else if ( OUT_FORM > 1 ) // bit superfluous at this point
         for( k=1 ; k<7 ; k++ )
            fprintf(fout,"%11.5f  ", 0.0);
      fprintf(fout,"\n");
   }

   for(i=0 ; i<idx ; i++){ 
      if(FLAG[i]) {

         if( BVAL_OUT )
            fprintf(fout,"%8d  ", (int) OUT_GRAD[i][0]);
         
         if( (OUT_FORM == 4) || (OUT_FORM ==5) )
            for( k=1 ; k<7 ; k++ )
               OUT_MATR[i][k]*= OUT_MATR[i][0];

         if( OUT_FORM == 1 ) // grad col
            for( k=1 ; k<4 ; k++ )
               fprintf(fout,"%11.5f  ", OUT_GRAD[i][k]);
         
         else if( (OUT_FORM == 3) || (OUT_FORM == 5) ) { // gmat
            for( k=1 ; k<6 ; k++ )
               fprintf(fout,"%11.5f  ", OUT_MATR[i][k]);
            fprintf(fout,"%11.5f", OUT_MATR[i][k]);
         }
         else if ( (OUT_FORM == 2 ) || (OUT_FORM ==4)) { // bmat
            fprintf(fout,"%11.5f  ", OUT_MATR[i][1]);
            fprintf(fout,"%11.5f  ", 2*OUT_MATR[i][4]);
            fprintf(fout,"%11.5f  ", 2*OUT_MATR[i][5]);
            fprintf(fout,"%11.5f  ", OUT_MATR[i][2]);
            fprintf(fout,"%11.5f  ", 2*OUT_MATR[i][6]);
            fprintf(fout,"%11.5f",   OUT_MATR[i][3]);
         }
            
         fprintf(fout,"\n");
      }
   }
   fclose(fout);
   
   mri_free(preREADIN);
   mri_free(preREADBVAL);
      

   printf("\tDone.  Check output file `%s' for results\n\n",Fname_output);
   
   return 0;
   
}


// take a len=6 matr and an empt len =3 grad file, calc grads from matr
int GradConv_Gsign_from_BmatA( float *grad, float *matr )
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
         INFO_message("WARNING: matrices don't appear to be correct format-- check again") ;
         grad[i] = 0;
      }

   return 1;
}

// simple conversion of grads to G-matr, diagonal form
int GradConv_BmatA_from_Gsign( float *matr, float *grad )
{
   int i;
   
   for( i=0 ; i<3 ; i++)
      matr[i] =  grad[i]*grad[i];
   matr[3] = grad[0]*grad[1];
   matr[4] = grad[0]*grad[2];
   matr[5] = grad[1]*grad[2];

   return 1;
}
