
/* 
   P. Taylor, April 2014
	
   Convert standard TORTOISE DTs (diagonal-first format) to standard
   AFNI (lower triangular, row-wise) format.  NB: Starting from
   TORTOISE v2.0.1, there is an 'AFNI output' format as well, which
   would not need to be converted.

   NB: this program is likely no longer necessary if using 'AFNI
   export' from TORTOISE!

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
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include "DoTrackit.h"
#include <diffusiony.h>



void usage_TORTOISEtoHere(int detail) 
{
	printf(
"\n"
"  Convert standard TORTOISE DTs (diagonal-first format) to standard\n"
"  AFNI (lower triangular, row-wise) format.  NB: Starting from\n"
"  TORTOISE v2.0.1, there is an 'AFNI output' format as well, which\n"
"  would not need to be converted.\n"
"\n"
"  Part of FATCAT (Taylor & Saad, 2013) in AFNI.\n"
"\n"
"  *** NB: this program is likely no longer necessary if using 'AFNI\n"
"  ***     export' from TORTOISE!\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND: 3dTORTOISEtoHere -dt_tort DTFILE  {-scale_fac X }   \\\n"
"           {-flip_x | -flip_y | -flip_z}  -prefix PREFIX \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + OUTPUT:\n"
"     1) An AFNI-style DT file with the following ordering of the 6 bricks:\n"
"          Dxx,Dxy,Dyy,Dxz,Dyz,Dzz.\n"
"        In case it is useful, one can apply 'flips' to the eventual (or\n"
"        underlying, depending how you look at it) eigenvector directions,\n"
"        as well as rescale the associated eigenvalues.\n"
"\n"
"  + RUNNING:\n"
"    -dt_tort DTFILE  :diffusion tensor file, which should have six bricks\n"
"                      of DT components ordered in the TORTOISE manner, i.e.,\n"
"                      diagonals first:\n"
"                      Dxx,Dyy,Dzz,Dxy,Dxz,Dyz.\n"
"    -prefix PREFIX   :output file name prefix. Will have N+1 bricks when\n"
"                      GRADFILE has N rows of gradients.\n"
"    -flip_x          :change sign of first element of (inner) eigenvectors.\n"
"    -flip_y          :change sign of second element of (inner) eigenvectors.\n"
"    -flip_z          :change sign of third element of (inner) eigenvectors.\n"
"              -> Only a single flip would ever be necessary; the combination\n"
"                 of any two flips is mathematically equivalent to the sole\n"
"                 application of the remaining one.\n"
"                 Normally, it is the *gradients* that are flipped, not the\n"
"                 DT, but if, for example, necessary files are missing, then\n"
"                 one can apply the requisite changes here.\n"
"    -scale_fac  X    :optional switch to rescale the DT elements, dividing\n"
"                      by a number X>0.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"      3dTORTOISEtoHere          \\\n"
"      -dt_tort DTI/DT_DT+orig   \\\n"
"      -scale_fac 1000           \\\n"
"      -prefix AFNI_DT   \n"
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


int main(int argc, char *argv[]) {
   int i, k, ii;
	int iarg;

   char *prefix=NULL;
   char *dtsname=NULL;

   THD_3dim_dataset *DTS=NULL;

	int Nvox=-1;            // tot number vox
	int Dim[3]={0,0,0};     // dim in each dir
   int INV[3] = {1,1,1};   // if needing to flip any directions.
   float LAMSCALE = 1.;

   float **D=NULL;
   THD_3dim_dataset *DT_OUT=NULL;
   
   // ###################################################################
   // #########################  load  ##################################
   // ###################################################################

   mainENTRY("3dTORTOISEtoHere"); machdep(); 
	if (argc == 1) { usage_TORTOISEtoHere(1); exit(0); }
   
   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_TORTOISEtoHere(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
     
      if( strcmp(argv[iarg],"-dt_tort") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-eig_vecs'");
         dtsname = strdup(argv[iarg]) ;
         
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

      if( strcmp(argv[iarg],"-scale_fac") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-scale_fac'");
         LAMSCALE = atof(argv[iarg]);
         if(LAMSCALE <= 0 )
            ERROR_exit("The '-scale_fac' value must be >0.");
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

   if(!dtsname)
      ERROR_exit("Need to input diffusion tensor file after '-dt_in'.");


   // ###################################################################

   if(dtsname) {
      DTS = THD_open_dataset(dtsname);
      DSET_load(DTS);  CHECK_LOAD_ERROR(DTS);

      if( 6 != DSET_NVALS(DTS) )
         ERROR_exit("DT file '%s' must have 6 bricks-- "
                    "it has %d bricks!",
                    dtsname, DSET_NVALS(DTS));
   }

   Nvox = DSET_NVOX(DTS);
   Dim[0] = DSET_NX(DTS); 
   Dim[1] = DSET_NY(DTS); 
   Dim[2] = DSET_NZ(DTS); 
   
   if(Nvox<0)
      ERROR_exit("Error reading Nvox from eigenvalue file.");
   
   D = calloc(6,sizeof(D)); 
   for(i=0 ; i<6 ; i++) 
		D[i] = calloc( Nvox,sizeof(float)); 

   INFO_message("Converting.");
   i = DT_TORTOISEtoAFNI( D, Nvox, DTS, INV, LAMSCALE);

   INFO_message("Writing the DT.");
   DT_OUT = EDIT_empty_copy( DTS ); 
   EDIT_dset_items(DT_OUT,
                   ADN_nvals, 6,
						 ADN_datum_all, MRI_float , 
                   ADN_prefix, prefix,
						 ADN_none );

   for( i=0; i<6 ; i++) {
		EDIT_substitute_brick(DT_OUT, i, MRI_float, D[i]);
		D[i]=NULL;
	}

	THD_load_statistics( DT_OUT );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(DT_OUT)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(DT_OUT));
	tross_Make_History("3dTORTOISEtoHere", argc, argv, DT_OUT);
	THD_write_3dim_dataset(NULL, NULL, DT_OUT, True);
	DSET_delete(DT_OUT); 
  	free(DT_OUT); 
   
   // #################################################################
   // ##########################  free  ###############################
   // #################################################################

   DSET_delete(DTS);
   free(DTS);
   
   for( i=0 ; i<6 ; i++)
      free(D[i]);
   free(D);

   free(prefix);
   free(dtsname);

	return 0;
}

