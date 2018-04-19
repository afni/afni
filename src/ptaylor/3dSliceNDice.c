/* 
   Description
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
"\n"
" This program is for calculating the Dice coefficient between two volumes \n"
" on a slice-by-slice basis.  The user enters two volumes on the same grid,\n"
" and Dice coefficients along each axis are calculated; three separate text\n"
" (*.1D) files are output.\n"
"\n"
" The Dice coefficient (Dice, 1945) is known by many names.  In the present\n"
" context, it is defined as follows.  Consider two sets voxels, A and B.\n"
" The Dice coefficient D is the ratio of their intersection to their union:\n"
"     D = 2*(intersection of A and B)/(union of A and B).\n"
" The range of D is 0 (no overlap of A and B at all) to 1 (perfect overlap\n"
" of A and B), inclusively.\n"
"\n"
" This program calculates D in a slicewise manner across all 3 major axes\n"
" of a dset;  other programs of interest for a volumewise Dice coefficient\n"
" or more general overlap calculations include 3dABoverlap, for example.\n"
"\n"
" written by PA Taylor (NIMH, NIH).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  \n"
"  + USAGE: ***\n"
"\n"
"      Input: \n"
"         + two single-volume datasets\n"
"\n"
"      Output:\n"
"         + three text files, each a *.1D file of columns of numbers. File\n"
"           name indicates along which axis the particular results were\n"
"           calculated, such as ending in '0_RL.1D', '1_AP.1D', '2_IS.1D',\n"
"           etc.\n"
"           For each file, there are currently 5 columns of data output,\n"
"           in the following order:\n"
"           [index] the i, j, or k index of the slice (starting from 0).\n"
"           [coord] the x, y, or z coordinate of the slice.\n"
"           [size of A ROI] the number of voxels in set A's ROI in the slice.\n"
"           [size of B ROI] the number of voxels in set B's ROI in the slice.\n"
"           [Dice coef] the Dice coefficient of that slice.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND:  *** \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + RUNNING, need to provide:\n"
"  *** \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  + OUTPUT: \n"
"  *** \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"  *** \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
" reference *** \n"
"____________________________________________________________________________\n"
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

   int TEST_OK = 0;

   slidice *SLIDICE=NULL;

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
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_SliceNDice(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      // NO ARG:
      if( strcmp(argv[iarg],"-TESTING") == 0) {
         TEST_OK=1;
         iarg++ ; continue ;
      }

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

      for( i=0 ; i<Dim[nn] ; i++ )
         fprintf( fout0, "%5d %12.5f %12d %12d %12.5f \n", 
                  i,
                  SLIDICE->coors[nn][i], 
                  SLIDICE->sizeA[nn][i], 
                  SLIDICE->sizeB[nn][i], 
                  SLIDICE->dice[nn][i]);

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

   if(SLIDICE)
      Free_slidice(SLIDICE,1);

   if(prefix)
      free(prefix);

   if(Dim)
      free(Dim);
	
   return 0;
}
