
/* 
   P. Taylor, Dec 2015
	
   A smaller beta-version of 3ddot, using the same functions, but
   hopefully faster for large data sets.

   For user use right now, just implementing eta2 test, and only doing
   full matrix to file.

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <time.h>
#include <debugtrace.h>
#include <mrilib.h>     
#include <3ddata.h>     
#include "editvol.h"
#include "thd.h"
#include "suma_suma.h"
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <colorbasic.h>
#include <DoTrackit.h>     


void usage_dot_beta(int detail) 
{
	printf(
"\n"
"  Beta version of updating 3ddot.  Right now, *only* doing eta2 tests,\n"
"  and only outputting a full matrix to a text file.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND: 3ddot_beta  -input FILE  -doeta2    \\\n"
"           {-mask MASK } -prefix PREFIX \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + OUTPUT:\n"
"     1) A single text file with the correlation-like matrix.  If the input\n"
"        data set has N bricks, then the matrix will be NxN.\n"
"\n"
"  + RUNNING:\n"
"    -input FILE      :file with N bricks.\n"
"    -prefix PREFIX   :output test file will be called PREFIX_eta2.dat.\n"
"\n"
"    -doeta2          :right now, required switch (more tests might be\n"
"                      present in the future, if demand calls for it).\n"
"\n"
"    -mask   MASK     :can include a mask within which to take values.\n"
"                      Otherwise, data should be masked already.\n\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"    3ddot_beta                    \\\n"
"      -input  RSFC_MAPS_cat+orig  \\\n"
"      -mask   mask.nii.gz         \\\n"
"      -doeta2                     \\\n"
"      -prefix My_Matrix_File   \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"___________________________________________________________________________\n"
);
	return;
}



int main(int argc, char *argv[]) {
   
   FILE *fout1;
   int i, j, k, ii;
	int iarg;

   char *prefix=NULL;
   char *maskname=NULL;
   char *inpname=NULL;

   THD_3dim_dataset *MASK=NULL;
   THD_3dim_dataset *INP=NULL;

	int Nvox=-1;            // tot number vox
	int Dim[4]={0,0,0,0};     // dim in each dir
   int Nmask = 0;
   
   byte *mskd=NULL; // not great, but another format of mask

   float **dset=NULL;   // input data set
   float **MAT=NULL;    // correlation matrix
   
   int WHICH_TEST = 0; // 1 = eta2  (**only choice right now**)
   char out_eta2[300];
   

   // ###################################################################
   // #########################  load  ##################################
   // ###################################################################
   
   mainENTRY("3ddot_beta"); machdep(); 
	if (argc == 1) { usage_dot_beta(1); exit(0); }
   
   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_dot_beta(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
      
      if( strcmp(argv[iarg],"-input") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-input'");
         inpname = strdup(argv[iarg]) ;
         
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-prefix'");
         prefix = strdup(argv[iarg]) ;
         iarg++ ; continue ;
      }
   
      if( strcmp(argv[iarg],"-mask") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");
         maskname = strdup(argv[iarg]) ;
      
         iarg++ ; continue ;
      }

		if( strcmp(argv[iarg],"-doeta2") == 0) {
			WHICH_TEST=1;
			iarg++ ; continue ;
		}

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);

   }
   
   
   // ###################################################################
   // ####################   some checks  ###############################
   // ###################################################################
   
   if(!prefix)
      ERROR_exit("Need to give a '-prefix'.");
   
   if(!inpname)
      ERROR_exit("Need to input a (multibrick) file name after '-input'.");
   
   if( !WHICH_TEST )
      ERROR_exit("Need to pick a comparison test: right now, only can "
                 "choose '-doeta2'");
   
   // ###################################################################
   
   INFO_message("Start loading data sets...");

   if(inpname) {
      INP = THD_open_dataset(inpname);
      DSET_load(INP);  CHECK_LOAD_ERROR(INP);
   }
   
   Nvox = Basic_Dim_and_Nvox( INP,
                              Dim, 4, 
                              inpname);
   
   if(Nvox<0)
      ERROR_exit("Error reading Nvox from eigenvalue file.");
   
   mskd = (byte *)calloc(Nvox,sizeof(byte)); 
   if( (mskd == NULL)) { 
      fprintf(stderr, "\n\n MemAlloc failure (masks).\n\n");
      exit(122);
   }
   
   if(maskname) {
      MASK = THD_open_dataset(maskname);
      DSET_load(MASK);  CHECK_LOAD_ERROR(MASK);
      
      if( 1 != DSET_NVALS(MASK) )
         ERROR_exit("Mask file '%s' is not scalar-- "
                    "it has %d bricks!",
                    maskname, DSET_NVALS(MASK));
   
      for( k=0 ; k<Nvox ; k++ )
         if (THD_get_voxel(MASK, k, 0) > 0 ) {
            mskd[k] = 1;
            Nmask++;
         }

      INFO_message("Number of voxels in the mask: %d", Nmask);

      DSET_delete(MASK);
      free(MASK);
      free(maskname);
   }
   else {
      for( k=0 ; k<Nvox ; k++ )
         if( fabs(THD_get_voxel(INP,k,0) > EPS_V) ) {
            mskd[k] = 1;
            Nmask++;
         }

      INFO_message("Number of (nonzero) voxels in the data ste: %d", 
                   Nmask);

   }
   
   INFO_message("Start prepping the matrices...");

   dset = calloc( Dim[3], sizeof(dset));      
   for(i=0 ; i<Dim[3] ; i++) 
      dset[i] = calloc(Nmask, sizeof(float)); 

   MAT = calloc( Dim[3], sizeof(MAT));      
   for(i=0 ; i<Dim[3] ; i++) 
      MAT[i] = calloc(Dim[3], sizeof(float)); 

   if( (dset == NULL) || (MAT == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(17);
   }

   // copy over the time series
   ii = 0;
   for( k=0 ; k<Nvox ; k++ )
      if( mskd[k] ) {
         for(i=0 ; i<Dim[3] ; i++) 
            dset[i][ii] = THD_get_voxel(INP,k,i);
         ii++;
      }
   DSET_delete(INP);
   free(INP);

   INFO_message("Start writing more ellipses...");

   if( WHICH_TEST == 1) {
      sprintf(out_eta2,"%s_eta2.dat",prefix); 
      
      // Do the calculation: symmetric matrix, and the diagonal
      // should be 1.
      for(i=0 ; i<Dim[3] ; i++) {
         MAT[i][i] = 1.;
         for(j=i+1 ; j<Dim[3] ; j++) {
            MAT[i][j] = THD_eta_squared( Nmask, dset[i] , dset[j] );
            MAT[j][i] = MAT[i][j];
         }
      }

      // write -------------------------------

      if( (fout1 = fopen(out_eta2, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",out_eta2);
         exit(19);
      }
            
      for(i=0 ; i<Dim[3] ; i++) {
         for(j=0 ; j<Dim[3] ; j++) {
            //fprintf(stdout, "%8.6f  ", MAT[i][j]);
            fprintf(fout1, "%8.6f    ", MAT[i][j]);
         }
         //fprintf(stdout, "\n");
         fprintf(fout1, "\n");
      }
      fclose(fout1);
      INFO_message("Wrote output to:    %s", out_eta2);

   }
   else
      ERROR_exit("Inconceivable to be doing a test other than 'eta2'!");

   INFO_message("Freeing mem...");

   // #################################################################
   // ##########################  free  ###############################
   // #################################################################

   free(mskd);
   free(prefix);
   for(i=0 ; i<Dim[3] ; i++) 
      free(dset[i]);
   free(dset);
   for(i=0 ; i<Dim[3] ; i++) 
      free(MAT[i]);
   free(MAT);

   INFO_message("Done.");

	exit(0);
}

