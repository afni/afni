
/* 
   P. Taylor, April 2014
	
   Convert eigen{values,vectors} to DTs.

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
#include "DoTrackit.h"
#include <diffusiony.h>

#define N_dti_vals (3) 
#define N_dti_vecs (3) 



void usage_EigsToDT(int detail) 
{
	printf(
"\n"
"  Convert set of DTI eigenvectors and eigenvalues to a diffusion tensor,\n"
"  while also allowing for some potentially useful value-scaling and vector-\n"
"  flipping.\n"
"\n"
"  May be helpful in converting output from different software packages.\n"
"  Part of FATCAT (Taylor & Saad, 2013) in AFNI.\n"
"  It is essentially the inverse of the existing AFNI command: 3dDTeig.\n" 
"\n"
"  Minor note and caveat:\n"
"  This program has been checked for consistency with 3dDWItoDT outputs (that\n"
"  is using its output eigenvalues and eigenvectors to estimate a DT, which\n"
"  was then compared with that of the original 3dDWItoDT fit).\n"
"  This program will *mostly* return the same DTs that one would get from\n"
"  using the eigenvalues and eigenvectors of 3dDWItoDT to very high agreement\n"
"  The values generally match to <10**-5 or so, except in CSF where there can\n"
"  be small/medium differences, apparently due to the noisiness or non-\n"
"  tensor-fittability of the original DWI data in those voxels.\n"
"  However, these discrepancies *shouldn't* really affect most cases of using\n"
"  DTI data.  This is probably generally true for reconstructing DTs of most\n"
"  software program output:  the results match well for most WM and GM, but\n"
"  there might be trouble in partial-volumed and CSF regions, where the DT\n"
"  model likely did not fit well anyways.  Caveat emptor.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND: 3dEigsToDT -eig_vals NAME1 -eig_vecs NAME2 {-mask MASK } \\\n"
"           {-flip_x | -flip_y | flip_z} {-scale_eigs X} -prefix PREFIX \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + OUTPUT:\n"
"     1) AFNI-format DT file with 6 subbricks in the same format as output\n"
"          by, for example, 3dDWItoDT (the lower triangular, row-wise\n"
"          elements of the tensor in symmetric matrix form)\n"
"            [0] Dxx\n"
"            [1] Dxy\n"
"            [2] Dyy\n"
"            [3] Dxz\n"
"            [4] Dyz\n"
"            [5] Dzz\n"
"\n"
"  + RUNNING:\n"
"    -eig_vals NAME1  :Should be a searchable descriptor for finding all\n"
"                      three required eigenvalue files.  Thus, on a Linux\n"
"                      commandline, one would expect:\n"
"                      $ ls NAME1\n"
"                      to list all three eigenvalue files in descending order\n"
"                      of magnitude. This program will also only take\n"
"                      the first three matches (not including doubling of\n"
"                      BRIK/HEAD files in AFNI-format).\n"
"    -eig_vecs NAME2  :Should be a searchable descriptor for finding all\n"
"                      three required eigenvector files.  Thus, on a Linux\n"
"                      commandline, one would expect:\n"
"                      $ ls NAME2\n"
"                      to list all three eigenvector files in order matching\n"
"                      the eigenvalue files. This program will also only take\n"
"                      the first three matches (not including doubling of\n"
"                      BRIK/HEAD files in AFNI-format).\n"
"              -> Try to make NAME1 and NAME2 as specific as possible, so\n"
"                 that the search&load gets everything as right as possible.\n"
"                 Also, if using the wildcard character, '*', then make sure\n"
"                 to enclose the option value with apostrophes (see EXAMPLE,\n"
"                 below).\n"
"    -prefix PREFIX   :output file name prefix. Would suggest putting a 'DT'\n"
"                      label in it.\n"
"    -mask   MASK     :can include a mask within which to calculate uncert.\n"
"                      Otherwise, data should be masked already.\n\n"
"    -flip_x          :change sign of first element of eigenvectors.\n"
"    -flip_y          :change sign of second element of eigenvectors.\n"
"    -flip_z          :change sign of third element of eigenvectors.\n"
"              -> Only a single flip would ever be necessary; the combination\n"
"                 of any two flips is mathematically equivalent to the sole\n"
"                 application of the remaining one.\n"
"    -scale_eigs  X   :rescale the eigenvalues, dividing by a number that is\n"
"                      X>0. Could be used to reintroduce the DW scale of the\n"
"                      original b-values, if some other program has\n"
"                      remorselessly scaled it away.\n"
"\n"
"* * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * **\n"
"\n"
"  + EXAMPLE:\n"
"      3dEigsToDT            \\\n"
"      -eig_vals 'DTI/DT_L*'   \\\n"
"      -eig_vecs 'DTI/DT_V*'   \\\n"
"      -prefix DTI/NEW_DT    \\\n"
"      -scale_eigs 1000      \\\n"
"      -flip_y\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  If you use this program, please reference the introductory/description\n"
"    paper for the FATCAT toolbox:\n"
"    Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And\n"
"    Tractographic Connectivity Analysis Toolbox. Brain Connectivity.\n\n");
	return;
}



int main(int argc, char *argv[]) {
   int i, k, ii;
	int iarg;


   char *prefix=NULL;
   char *maskname=NULL;
   char *eigvecs=NULL;
   char *eigvals=NULL;

   THD_3dim_dataset *MASK=NULL;
   THD_3dim_dataset **EVECS=NULL;
   THD_3dim_dataset **EVALS=NULL;

	int Nvox=-1;            // tot number vox
	int Dim[3]={0,0,0};     // dim in each dir
   int INV[3] = {1,1,1};   // if needing to flip any directions.
   float LAMSCALE = 1.;

   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2; // "-wild_files_noAext_noAview"

   byte *mskd2=NULL; // not great, but another format of mask

   float **dt=NULL;
   THD_3dim_dataset *DT_OUT=NULL;

   // ###################################################################
   // #########################  load  ##################################
   // ###################################################################

   mainENTRY("3dEigsToDT"); machdep(); 
	if (argc == 1) { usage_EigsToDT(1); exit(0); }
   
   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_EigsToDT(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
     
      if( strcmp(argv[iarg],"-eig_vecs") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-eig_vecs'");
         eigvecs = strdup(argv[iarg]) ;
         
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-eig_vals") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-eig_vals'");
         eigvals = strdup(argv[iarg]) ;
      
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
   
      if( strcmp(argv[iarg],"-mask") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");
         maskname = strdup(argv[iarg]) ;
      
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

      if( strcmp(argv[iarg],"-scale_eigs") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need integer argument after '-scale_eigs'");
         LAMSCALE = atof(argv[iarg]);
         if(LAMSCALE <= 0 )
            ERROR_exit("The '-scale_eigs' value must be >0.");
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

   if(!eigvals)
      ERROR_exit("Need to input eigenvalue files after '-eig_vals'.");

   if(!eigvecs)
      ERROR_exit("Need to input eigenvector files after '-eig_vecs'.");



   // ###################################################################


   if(eigvals){

      wild_list = SUMA_append_replace_string(wild_list, eigvals, " ", 1); 

      INFO_message("SEARCHING for files with prefix '%s'",eigvals);
      
      MCW_wildcards(wild_list, &nglob, &wglob ); 
      if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                              &nsort, &isrt))) {
         
         if( nsort < N_dti_vals ) 
            ERROR_exit("Only found %d files in eigval search: need %d.",
                       nsort, N_dti_vals);
         
         EVALS = (THD_3dim_dataset **)calloc(N_dti_vals, 
                                             sizeof(THD_3dim_dataset *));
         if( EVALS == NULL ) 
            ERROR_exit("Memory allocation error for EVALS, oddly.");
         
         
         for( ii=0 ; ii< N_dti_vals; ii++) {
            EVALS[ii] = THD_open_dataset(wglob[isrt[ii]]);
            if( EVALS[ii] == NULL ) 
               ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]]);
            DSET_load(EVALS[ii]);  CHECK_LOAD_ERROR(EVALS[ii]);
            INFO_message("Loaded L%d file: '%s'",ii+1, wglob[isrt[ii]]);

            if( 1 != DSET_NVALS(EVALS[ii]) )
               ERROR_exit("Eigenvalue file '%s' is not scalar-- "
                          "it has %d bricks!",
                          wglob[isrt[ii]],DSET_NVALS(EVALS[ii]));

         }
         
         if (isrt) free(isrt); isrt = NULL;
         for (i=0; i<nglob; ++i) if (wsort[i]) free(wsort[i]);
         free(wsort); wsort = NULL;
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
      } 
      else {
         ERROR_message("Failed to sort");
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
         exit(1);
      }
   }
   

   Nvox = DSET_NVOX(EVALS[0]);
   Dim[0] = DSET_NX(EVALS[0]); 
   Dim[1] = DSET_NY(EVALS[0]); 
   Dim[2] = DSET_NZ(EVALS[0]); 
   
   for( ii=0 ; ii< N_dti_vals; ii++) 
      if(  ( Dim[0] != DSET_NX(EVALS[ii]) ) ||
           ( Dim[1] != DSET_NY(EVALS[ii]) ) ||
           ( Dim[2] != DSET_NZ(EVALS[ii]) ) )
         ERROR_exit("Mismatch in voxel dimensions among eigenvalue files.");
   if(Nvox<0)
      ERROR_exit("Error reading Nvox from eigenvalue file.");

   if(eigvecs){
      
      wild_list = SUMA_append_replace_string(wild_list, eigvecs, " ", 1); 
      
      INFO_message("SEARCHING for files with prefix '%s'",eigvecs);
      
      MCW_wildcards(wild_list, &nglob, &wglob ); 
      if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                              &nsort, &isrt))) {
         
         if( nsort < N_dti_vecs ) 
            ERROR_exit("Only found %d files in eigval search: need %d.",
                       nsort, N_dti_vecs);
         
         EVECS = (THD_3dim_dataset **)calloc(N_dti_vecs, 
                                             sizeof(THD_3dim_dataset *));
         if( EVECS == NULL ) 
            ERROR_exit("Memory allocation error for EVECS, oddly.");
         
         
         for( ii=0 ; ii< N_dti_vecs; ii++) {
            EVECS[ii] = THD_open_dataset(wglob[isrt[ii]]);
            if( EVECS[ii] == NULL ) 
               ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]]);
            DSET_load(EVECS[ii]);  CHECK_LOAD_ERROR(EVECS[ii]);
            INFO_message("Loaded V%d file: '%s'",ii+1, wglob[isrt[ii]]);

            if(  ( Dim[0] != DSET_NX(EVECS[ii]) ) ||
                 ( Dim[1] != DSET_NY(EVECS[ii]) ) ||
                 ( Dim[2] != DSET_NZ(EVECS[ii]) ) )
               ERROR_exit("Mismatch in voxel dimensions between L1 file\n"
                          "\t and eigenvector file: '%s'.",
                          wglob[isrt[ii]]);
            if( 3 != DSET_NVALS(EVECS[ii]) )
               ERROR_exit("Eigenvector file '%s' doesn't have 3 bricks;"
                          "it has %d!",
                          wglob[isrt[ii]],DSET_NVALS(EVECS[ii]));
         }

         if (isrt) free(isrt); isrt = NULL;
         for (i=0; i<nglob; ++i) if (wsort[i]) free(wsort[i]);
         free(wsort); wsort = NULL;
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
      } 
      else {
         ERROR_message("Failed to sort");
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;
         exit(1);
      }
   }
   
   mskd2 = (byte *)calloc(Nvox,sizeof(byte)); 
   if( (mskd2 == NULL)) { 
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
         if (THD_get_voxel(MASK, k, 0) > 0 )
            mskd2[k] = 1;


      DSET_delete(MASK);
      free(MASK);
      free(maskname);
   }
   else {
      for( k=0 ; k<Nvox ; k++ )
         if( fabs(THD_get_voxel(EVALS[0],k,0) > EPS_V) )
            mskd2[k] = 1;
   }
      

	dt = calloc(6,sizeof(dt)); 
	for(i=0 ; i<6 ; i++) 
		dt[i] = calloc( Nvox,sizeof(float)); 
   if( dt == NULL ) { 
      fprintf(stderr, "\n\n MemAlloc failure (DTs).\n\n");
      exit(122);
   }

   INFO_message("Calculate sum of dyads.");
   i = Dyadize( dt, Nvox, EVALS, LAMSCALE, EVECS, INV, mskd2 );

   DT_OUT = EDIT_empty_copy( EVALS[0] ); 
	EDIT_add_bricklist(DT_OUT,
							 5, NULL , NULL , NULL );
	EDIT_dset_items(DT_OUT,
						 ADN_datum_all, MRI_float , 
                   ADN_prefix, prefix,
						 ADN_none );

	for( i=0; i<6 ; i++) {
		EDIT_substitute_brick(DT_OUT, i, MRI_float, dt[i]);
		dt[i]=NULL;
	}

	EDIT_BRICK_LABEL(DT_OUT,0,"Dxx");      
	EDIT_BRICK_LABEL(DT_OUT,1,"Dxy");      
	EDIT_BRICK_LABEL(DT_OUT,2,"Dyy");      
	EDIT_BRICK_LABEL(DT_OUT,3,"Dxz");      
	EDIT_BRICK_LABEL(DT_OUT,4,"Dyz");      
	EDIT_BRICK_LABEL(DT_OUT,5,"Dzz");     

	THD_load_statistics( DT_OUT );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(DT_OUT)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(DT_OUT));
	tross_Make_History("3dEigsToDT", argc, argv, DT_OUT);
	THD_write_3dim_dataset(NULL, NULL, DT_OUT, True);
	DSET_delete(DT_OUT); 
  	free(DT_OUT); 
   
   // #################################################################
   // ##########################  free  ###############################
   // #################################################################
   
   for( i=0 ; i<N_dti_vals ; i++) {
      DSET_delete(EVALS[i]);
      free(EVALS[i]);
   }
   free(EVALS);

   for( i=0 ; i<N_dti_vecs ; i++) {
      DSET_delete(EVECS[i]);
      free(EVECS[i]);
   }
   free(EVECS);

	for(i=0 ; i<6 ; i++) 
      free(dt[i]);
   free(dt);

   free(prefix);
   free(eigvecs);
   free(eigvals);

	exit(0);
}

