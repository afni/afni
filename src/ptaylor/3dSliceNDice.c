/* 
   [PT: April, 2018] Calc slicewise Dice coefficients for dsets.
   Output 3 1D text files, one for each FOV.
*/


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
#include "basic_boring.h"

void usage_SliceNDice(int detail) 
{
   printf(
" # ----------------------------------------------------------------------\n"
" \n"
" This program is for calculating the Dice coefficient between two\n"
" volumes on a slice-by-slice basis.  The user enters two volumes on the\n"
" same grid, and Dice coefficients along each axis are calculated; three\n"
" separate text (*.1D) files are output.\n"
" \n"
" The Dice coefficient (Dice, 1945) is known by many names and in many\n"
" applications.  In the present context it is defined as follows.\n"
" Consider two sets voxels (i.e., masks), A and B.  The Dice coefficient\n"
" D is the ratio of their intersection to their union. Let N(x) be a\n"
" function that calculates the number of voxels in a set x. Then:\n"
" \n"
"     D = 2*N(intersection of A and B)/(N(A) + N(B)).\n"
" \n"
" The range of D is 0 (no overlap of A and B at all) to 1 (perfect\n"
" overlap of A and B), inclusively.\n"
" \n"
" This program calculates D in a slicewise manner across all 3 major\n"
" axes of a dset; other programs of interest for a volumewise Dice\n"
" coefficient or more general overlap calculations include 3dABoverlap,\n"
" for example.\n"
" \n"
" Nonzero values in a dset are considered part of the mask.  3dcalc\n"
" might be useful in creating a mask from a dset if things like\n"
" thresholding are required.\n"
" \n"
" written by PA Taylor (NIMH, NIH).\n"
" \n"
" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"   \n"
" USAGE ~1~\n"
" \n"
"     Input: \n"
"       + two single-volume datasets\n"
" \n"
"     Output:\n"
" \n"
"       + three text files, each a *.1D file of columns of numbers (and\n"
"         note that the executed 3dSliceNDice command is echoed into a\n"
"         comment in the top line of each 1D file by output). File name\n"
"         indicates along which axis the particular results were\n"
"         calculated, such as ending in '0_RL.1D', '1_AP.1D', '2_IS.1D',\n"
"         etc.\n"
" \n"
"         For each file, there are currently 5 columns of data output,\n"
"         in the following order:\n"
"         [index] the i, j, or k index of the slice (starting from 0).\n"
"         [coord] the x, y, or z coordinate of the slice.\n"
"         [size of A ROI] the number of voxels in set A's ROI in the slice.\n"
"         [size of B ROI] the number of voxels in set B's ROI in the slice.\n"
"         [Dice coef] the Dice coefficient of that slice.\n"
" \n"
"         1dplot can be useful for viewing output results quickly.\n"
" \n"
" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" COMMAND ~1~\n"
" \n"
"     3dSliceNDice                                  \\\n"
"         -insetA    AA                             \\\n"
"         -insetB    BB                             \\\n"
"         -prefix    PP                             \\\n"
"         {-out_range all|AorB|AandB}\n"
" \n"
"     where\n"
" \n"
"   -insetA    AA   :name of an input set to make a mask from; mask will\n"
"                    be made from nonzero values in AA;\n"
"   -insetB    BB   :name of an input set to make a mask from; mask will\n"
"                    be made from nonzero values in BB;\n"
" \n"
"   -prefix    PP   :prefix of output files.\n"
"                    Three output text files will be named\n"
"                    according to the orientation of the input AA\n"
"                    and BB files.  So, outputs might look like:\n"
"                      PP_0_RL.1D or PP_0_RL.1D,\n"
"                      PP_1_AP.1D or PP_0_PA.1D,\n"
"                      PP_2_IS.1D or PP_0_SI.1D.\n"
" \n"
"   -out_domain all|AorB|AandB\n"
"                   :optional specification of the slices over which to\n"
"                    output Dice coefficient results along each axis,\n"
"                    via keyword.  Argument options at present:\n"
"                    'all':   report Dice values for all slices (default);\n"
"                    'AorB':  report values only in slices where sets A or\n"
"                             B (or both) have at least one nonzero voxel;\n"
"                    'AandB': report values only in slices where both sets\n"
"                             A and B have at least one nonzero voxel;\n"
"                    'Amask': report values only in slices where set A \n"
"                             has at least one nonzero voxel;\n"
"                    'Bmask': report values only in slices where set B \n"
"                             has at least one nonzero voxel;\n"
" \n"
"   -no_cmd_echo    :turn OFF recording the command line call to\n"
"                    3dSliceNDice in the output *.1D files (default is\n"
"                    to do the recording).\n"
" \n"
" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" EXAMPLES ~1~\n"
" \n"
"   1. Report slicewise overlap of two masks through full FOV along each \n"
"      axis.\n"
"        3dSliceNDice                  \\\n"
"            -insetA mask_1.nii.gz     \\\n"
"            -insetB mask_2.nii.gz     \\\n"
"            -prefix mask_olap_all\n"
" \n"
"   2. Report slicewise overlap of two masks only for slices where both \n"
"      dsets have >0 voxels in their masks\n"
"        3dSliceNDice                  \\\n"
"            -insetA mask_1.nii.gz     \\\n"
"            -insetB mask_2.nii.gz     \\\n"
"            -out_domain AandB         \\\n"
"            -prefix mask_olap_AandB\n"
" \n"
"   To view the SliceNDice results: NB, you can use 1dplot for viewing\n"
"   either of the about output results, choosing slice number or DICOM\n"
"   coordinate value for the abscissa (x-axis) value.\n"
" \n"
"   # use integer index values along x-axis of the plot, for one\n"
"   # encoding direction of the volume:\n"
"   1dplot -x mask_olap_all_1_PA.1D'[0]' mask_olap_all_1_PA.1D'[4]'\n"
" \n"
"   # use DICOM coordinate values along x-axis of the plot: \n"
"   1dplot -x mask_olap_all_1_PA.1D'[1]' mask_olap_all_1_PA.1D'[4]'\n"
" \n"
" # ----------------------------------------------------------------------\n"
);
	return;
}

int main(int argc, char *argv[]) {
   int i,j,k,m,n,mm,nn;
   int idx=0;
   int iarg;

   THD_3dim_dataset *insetA = NULL;
   THD_3dim_dataset *insetB = NULL;
   char *prefix="PREFIX" ;
   char tprefixx[THD_MAX_PREFIX];
   char ori[3] = "no";

   FILE *fout0, *fout1;

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;
   byte ***mskdA=NULL, ***mskdB=NULL;

   byte **orange=NULL;

   int TEST_OK = 1;     // Not in testing mode anymore

   slidice *SLIDICE=NULL;

   int ARGV_IN  = 1;    // echo the 3dslicendice command at the top of
                        // the output 1D files
   int FOV_TYPE = 0;    // 0: output full FOV; 1: *only* where A or B
                        // is nonzero;

   mainENTRY("3dSliceNDice"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   // INFO_message("version: NU");
	
   /** scan args **/
   if (argc == 1) { usage_SliceNDice(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ) {
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_SliceNDice(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      /*
      // NO ARG:
      if( strcmp(argv[iarg],"-TESTING") == 0) {
         TEST_OK=1;
         iarg++ ; continue ;
      }
      */
      
      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-prefix'");
         prefix = strdup(argv[iarg]) ;
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '-prefix'");
         iarg++ ; continue ;
      }
	 
      if( strcmp(argv[iarg],"-insetA") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-insetA'");

         insetA = THD_open_dataset(argv[iarg]);
         if( (insetA == NULL ))
            ERROR_exit("Can't open dataset '%s'.",
                       argv[iarg]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-insetB") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-insetB'");
         
         insetB = THD_open_dataset(argv[iarg]);
         if( (insetB == NULL ))
            ERROR_exit("Can't open dataset '%s'.",
                       argv[iarg]);
         
         iarg++ ; continue ;
      }
      
      // what is length of output?
      if( strcmp(argv[iarg],"-out_domain") == 0 ){
      iarg++ ; if( iarg >= argc ) 
                  ERROR_exit("Need argument after '-out_domain'");
      
      if( strcmp(argv[iarg],"all") == 0 ) 
         FOV_TYPE = 0; // def: full FOV
      else if( strcmp(argv[iarg],"AorB") == 0 )
         FOV_TYPE = 1; // only where A or B is nonzero
      else if( strcmp(argv[iarg],"AandB") == 0 )
         FOV_TYPE = 2; // only where A and B are both nonzero
      else if( strcmp(argv[iarg],"Amask") == 0 )
         FOV_TYPE = 3; // only where A has >0 voxel
      else if( strcmp(argv[iarg],"Bmask") == 0 )
         FOV_TYPE = 4; // only where B has >0 voxel
      else 
         ERROR_exit("Illegal after '-out_domain': need 'all' or 'AorB'");

      iarg++ ; continue ;
   }
      
      // don't echo command into text file
      if( strcmp(argv[iarg],"-no_cmd_echo") == 0 ){
         ARGV_IN = 0;
         iarg++ ; continue ;
      }

      
      // ----------------- finish up -----------------
      
      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
}
	
   // TEST BASIC INPUT PROPERTIES
   if (iarg < 2) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }
	
if( !TEST_OK ) {
   ERROR_message("HEY! Just testing/building mode right now!\n");
   exit(5);
 }

DSET_load(insetA); CHECK_LOAD_ERROR(insetA);
DSET_load(insetB); CHECK_LOAD_ERROR(insetB);

// Compare dsets: if no match, this will exit with error
i = Basic_Compare_DSET_dims(insetA, insetB, 4);

Dim = (int *)calloc(4, sizeof(int));
Nvox = Basic_Info_Dim_and_Nvox( insetA, Dim, 4);

if ( Dim[3] > 1 ) {
   ERROR_message("HEY! this only works on pairs of single volumes!\n");
   exit(2);
 }

// ****************************************************************
// ****************************************************************
//                    pre-stuff, make storage
// ****************************************************************
// ****************************************************************

mskdA = (byte ***) calloc( Dim[0], sizeof(byte **) );
for ( i = 0 ; i < Dim[0] ; i++ ) 
   mskdA[i] = (byte **) calloc( Dim[1], sizeof(byte *) );
for ( i = 0 ; i < Dim[0] ; i++ ) 
   for ( j = 0 ; j < Dim[1] ; j++ ) 
      mskdA[i][j] = (byte *) calloc( Dim[2], sizeof(byte) );

mskdB = (byte ***) calloc( Dim[0], sizeof(byte **) );
for ( i = 0 ; i < Dim[0] ; i++ ) 
   mskdB[i] = (byte **) calloc( Dim[1], sizeof(byte *) );
for ( i = 0 ; i < Dim[0] ; i++ ) 
   for ( j = 0 ; j < Dim[1] ; j++ ) 
      mskdB[i][j] = (byte *) calloc( Dim[2], sizeof(byte) );

if( (mskdA == NULL) || (mskdB == NULL) ) { 
   fprintf(stderr, "\n\n MemAlloc failure (arrs).\n\n");
   exit(13);
 }

// *************************************************************
// *************************************************************
//                    Beginning of main loops
// *************************************************************
// *************************************************************
	 
// go through once: define data masks
for( k=0 ; k<Dim[2] ; k++ ) 
   for( j=0 ; j<Dim[1] ; j++ ) 
      for( i=0 ; i<Dim[0] ; i++ ) {
         idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]);
         if( THD_get_voxel(insetA, idx, 0) )
            mskdA[i][j][k] = 1;
         if( THD_get_voxel(insetB, idx, 0) )
            mskdB[i][j][k] = 1;
      }

SLIDICE = Create_slidice( Dim, insetA, insetB);

// where the magic happens
i = Dice_em_up_calcs( SLIDICE, 
                      mskdA,
                      mskdB);
      
// **************************************************************
// **************************************************************
//                 Store and output
// **************************************************************
// **************************************************************

mm = SLIDICE->maxd;
orange = calloc( 3, sizeof(orange) );
for(i=0 ; i<3 ; i++) 
   orange[i] = calloc( mm, sizeof(byte) );

if( (orange == NULL) ) { 
   fprintf(stderr, "\n\n MemAlloc failure (arrs).\n\n");
   exit(12);
 }

i = Find_slidice_orange( SLIDICE, 
                         FOV_TYPE,
                         orange);

for( nn=0 ; nn<3 ; nn++ ) {
      
   // attach the orientation to the file name
   for( i=0 ; i<2 ; i++ ) {
      if( !nn ) 
         ori[i] = ORIENT_tinystr[insetA->daxes->xxorient][i];
      else if( nn==1 )
         ori[i] = ORIENT_tinystr[insetA->daxes->yyorient][i];
      else
         ori[i] = ORIENT_tinystr[insetA->daxes->zzorient][i];
   }

   sprintf(tprefixx,"%s_%d_%s.1D", prefix, nn, ori);

   if( (fout0 = fopen(tprefixx, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s.", tprefixx);
      exit(19);
   }
   
   if( ARGV_IN == 1 ) {
      fprintf(fout0, "#");
      for( j=0 ; j<argc ; j++ ) 
         fprintf(fout0, " %s", argv[j]);
      fprintf(fout0, "\n");
   }

   for( i=0 ; i<Dim[nn] ; i++ ) {
      if( orange[nn][i] )
         fprintf( fout0, "%5d %12.5f %12d %12d %12.5f \n", 
                  i,
                  SLIDICE->coors[nn][i], 
                  SLIDICE->sizeA[nn][i], 
                  SLIDICE->sizeB[nn][i], 
                  SLIDICE->dice[nn][i]);
   }

   fclose(fout0);
 }
   
// ************************************************************
// ************************************************************
//                    Freeing
// ************************************************************
// ************************************************************
	
if(insetA){
   DSET_delete(insetA);
   free(insetA);
 }

if(insetB){
   DSET_delete(insetB);
   free(insetB);
 }

if(mskdA) {
   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) {
         free(mskdA[i][j]);
      }
   for( i=0 ; i<Dim[0] ; i++) {
      free(mskdA[i]);
   }
   free(mskdA);
 }

if(mskdB) {
   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) {
         free(mskdB[i][j]);
      }
   for( i=0 ; i<Dim[0] ; i++) {
      free(mskdB[i]);
   }
   free(mskdB);
 }

if(orange) {
   for( j=0 ; j<3 ; j++) 
      free(orange[j]);
   free(orange);
 }

if(SLIDICE)
   Free_slidice(SLIDICE,1);

if(prefix)
   free(prefix);

if(Dim)
   free(Dim);
	
return 0;
}

