
/* 
   For viewing 3vec or RGB colors easily in SUMA:
   for particular use with AJJ coloring
	
   P. Taylor, Oct 2015.
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


void usage_Vec_to_RGBind(int detail) 
{
	printf(
"\n"
"  ******************** beta testing **************************************\n"
"\n"
"    Tractographic Connectivity Analysis Toolbox. Brain Connectivity.\n\n");
	return;
}



int main(int argc, char *argv[]) {
   int i, k, ii;
	int iarg;

   char *prefix=NULL;
   char *invec=NULL;       // e.g., V1 from DT fit
   char *inscal=NULL;      // e.g., attach FA map for brightness
   char *maskname=NULL;

   THD_3dim_dataset *VEC=NULL;
   THD_3dim_dataset *SCAL=NULL;
   THD_3dim_dataset *MASK=NULL;

	int Nvox=-1;              // tot number vox
	int Dim[4]={0,0,0,0};     // dim in each dir

   float magn2=0;

   byte *mskd2=NULL; // not great, but another format of mask
   // all either calc'ed or given
   float **RGB=NULL;
   float **HSL=NULL;
   
   float *outarr=NULL;  // will hold intens values (e.g., FA)

   THD_3dim_dataset *OUT=NULL;

   // ###################################################################
   // #########################  load  ##################################
   // ###################################################################

   mainENTRY("3dVec_to_RGBind"); machdep(); 
	if (argc == 1) { usage_Vec_to_RGBind(1); exit(0); }
   
   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_Vec_to_RGBind(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
     
      if( strcmp(argv[iarg],"-in_vec") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_vec'");
         invec = strdup(argv[iarg]) ;
         
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-in_scal") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_scal'");
         inscal = strdup(argv[iarg]) ;
      
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

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
   }

   // ###################################################################
   // ####################   some checks  ###############################
   // ###################################################################

   if(!prefix)
      ERROR_exit("Need to give a '-prefix'.");

   if(!invec)
      ERROR_exit("Need to input a 3-vector (such as V1) '-in_vec'.");

   // ###################################################################

   // ------------------------ start invec --------------------------------

   VEC = THD_open_dataset(invec);

   Nvox = Basic_Dim_and_Nvox( VEC, 
                              Dim, 4, 
                              invec);

   if( Dim[3] != 3 )
      ERROR_exit("Input vector file '%s' does not have 3 bricks-- "
                 "it has %d bricks!",
                 invec, Dim[3]);

   // ------------------------ stop invec --------------------------------

   // ------------------------ start alloc --------------------------------

   RGB = calloc( Nvox, sizeof(RGB));         // N x 3, at moment
   for(i=0 ; i<Nvox ; i++) 
      RGB[i] = calloc(3, sizeof(float)); 
   HSL = calloc( 3, sizeof(HSL));            // 3 x N, for outputting
   for(i=0 ; i<3 ; i++) 
      HSL[i] = calloc(Nvox, sizeof(float)); 

   outarr = (float *)calloc(Nvox,sizeof(float)); // 1 x N, for outputting

   if( (RGB == NULL) || (HSL == NULL) || (outarr == NULL)) { 
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(17);
   }

   // ------------------------ stop alloc --------------------------------

   // ------------------------ start mask --------------------------------

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
      for( k=0 ; k<Nvox ; k++ ) {
         // some condition for having been masked.  All the vecs
         // should have been normalized to magnitude one, so this is a
         // 'generous' condition, I think.
         magn2 = 0;
         for( i=0 ; i<3 ; i++ )
            magn2+= THD_get_voxel(VEC,k,i)*THD_get_voxel(VEC,k,i);
         if( magn2 > 0.1) 
            mskd2[k] = 1;
      }
   }

   // ------------------------ stop mask --------------------------------

   // ------------------------ start inscal --------------------------------
   if(inscal) {
      SCAL = THD_open_dataset(inscal);

      if( Basic_compare_DSET_dims( SCAL, VEC, 3, inscal, invec) ) {
         ERROR_exit("Input scalar %s and vector %s files do not have "
                    "matching dimensions!", inscal, invec);
      }

      // fill in arr for additional output, if SCAL has been input
      for( k=0 ; k<Nvox ; k++ ) {
         if(mskd2[k])
            outarr[k] = THD_get_voxel(SCAL, k, 0);
      }
   }
   else
      for( k=0 ; k<Nvox ; k++ )  // no FA -> uniform intensity in mask
         if(mskd2[k])
            outarr[k] = 1.;

   // ------------------------ stop inscal --------------------------------

   // ----------------------- start RGB/HSL------------------------------

   // only input, at the moment:  take XYZ -> RGB
   if( invec ) {
      if( Color_Vec_XYZdset_to_RGB( VEC, RGB, mskd2, Nvox ) ) 
         ERROR_exit("Couldn't convert VEC to RGB");
   }

   // RGB -> HSL
   if( Color_Vec_RGB_to_HSL( RGB, HSL, mskd2, Nvox ) ) 
      ERROR_exit("Couldn't convert RGB to HSL");

   // ------------------------ stop RGB/HSL------------------------------

   // ----------------------- start output------------------------------




   OUT = EDIT_empty_copy( VEC ); 
   
	EDIT_dset_items( OUT,
                    ADN_datum_all, MRI_float , 
                    ADN_prefix, prefix,
                    ADN_none );                   // 3 for HSL 

   EDIT_add_bricklist( OUT,
                       1, NULL , NULL , NULL );   // one more for FA

   EDIT_substitute_brick(OUT, 0, MRI_float, HSL[0]);
   EDIT_substitute_brick(OUT, 1, MRI_float, HSL[1]); 
   EDIT_substitute_brick(OUT, 2, MRI_float, HSL[2]); 
   EDIT_substitute_brick(OUT, 3, MRI_float, outarr);
   outarr=NULL;
   
   for( i=0 ; i<3 ; i++ )
      HSL[i]=NULL;
   
	EDIT_BRICK_LABEL(OUT,0,"Hue");      
	EDIT_BRICK_LABEL(OUT,1,"Sat");      
	EDIT_BRICK_LABEL(OUT,2,"Lum");      
	EDIT_BRICK_LABEL(OUT,3,"Int");      

	THD_load_statistics( OUT );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(OUT)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(OUT));
	tross_Make_History("3dVec_to_RGBind", argc, argv, OUT);
	THD_write_3dim_dataset(NULL, NULL, OUT, True);
	DSET_delete(OUT); 
  	free(OUT); 

   // #################################################################
   // ##########################  free remaining ######################
   // #################################################################
   
   
   DSET_delete(VEC);
   free(VEC);
   free(invec);
   
   if (inscal){
      DSET_delete(SCAL);
      free(SCAL);
      free(inscal);
   }
      
   free(prefix);

   for( i=0 ; i<Nvox ; i++ )
      free(RGB[i]);
   free(RGB);
   
   for( i=0 ; i<3 ; i++ )
      free(HSL[i]);
   free(HSL);

	exit(0);
}

