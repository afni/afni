
/* 
   P. Taylor, April 2014
	
   For some simulation-based work, take DTs and gradients and make
   some noisy DWIs.

   update: April, 2016
   + allow for b-value weighted grads to be input

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



void usage_DTtoNoisyDWI(int detail) 
{
	printf(
"\n"
"  Take an AFNI-style DT file as input, such as might be output by 3dDWItoDT\n"
"  (which means that the DT elements are ordered: Dxx,Dxy,Dyy,Dxz,Dyz,Dzz),\n"
"  as well as a set of gradients, and then generate a synthetic set of DWI\n"
"  measures with a given SNR. Might be useful for simulations/testing.\n"
"\n"
"  Part of FATCAT (Taylor & Saad, 2013) in AFNI.\n"
"  It is similar in premise to 3dDTtoDWI, however this allows for the modeled\n"
"  inclusion of Rician noise (such as appears in MRI magnitude images).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND: 3dDTtoNoisyDWI -dt_in DTFILE -grads GRADFILE -noise_frac0 FF \\\n"
"           {-bval BB} {-S0 SS} {-mask MASK } -prefix PREFIX \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + OUTPUT:\n"
"     1) If N gradients are input, then the output is a file with N+1 bricks\n"
"     that mimics a set of B0+DWI data (0th brick is the B0 reference).\n"
"\n"
"  + RUNNING:\n"
"    -dt_in DTFILE    :diffusion tensor file, which should have six bricks\n"
"                      of DT components ordered in the AFNI (i.e., 3dDWItoDT)\n"
"                      manner:\n"
"                      Dxx,Dxy,Dyy,Dxz,Dyz,Dzz.\n"
"    -grads GRADFILE  :text file of gradients arranged in three columns.\n"
"                      It is assumed that there is no row of all zeros in the\n"
"                      GRADFILE (i.e., representing the b=0 line).\n"
"                      If there are N rows in GRADFILE, then the output DWI\n"
"                      file will have N+1 bricks (0th will be the b=0\n"
"                      reference set of noise S0 measures).\n"
"    -noise_DWI  FF   :fractional value of noise in DWIs. The magnitude will\n"
"                      be set by the b=0 reference signal, S0. Rician noise\n"
"                      is used, which is characterized by a standard\n"
"                      deviation, sigma, so that FF = sigma/S0 = 1/SNR0.\n"
"                      For example, FF=0.05 roughly corresponds to an \n"
"                      SNR0=20 'measurement'.\n"
"    -noise_B0   FF2  :optional switch to use a different fraction of Rician\n"
"                      noise in the b=0 reference image; one might consider\n"
"                      it realistic to have a much lower level of noise in\n"
"                      the reference signal, S0, mirroring the fact that\n"
"                      generally multiple averages of b=0 acquisitions are\n"
"                      averaged together. If no fraction is entered here,\n"
"                      then the simulation will run with FF2=FF.\n"
"    -prefix PREFIX   :output file name prefix. Will have N+1 bricks when\n"
"                      GRADFILE has N rows of gradients.\n"
"    -mask   MASK     :can include a mask within which to calculate uncert.\n"
"                      Otherwise, data should be masked already.\n\n"
"    -bval BB         :optional DW factor to use if one has DT values scaled\n"
"                      to something physical (NB: AFNI 3dDWItoDT works in a \n"
"                      world of b=1, so the default setting here is BB=1; one\n"
"                      probably doesn't need to change this if using DTs made\n"
"                      by 3dDWItoDT).\n"
"    -S0  SS          :optional reference b=0 signal strength.  Default value\n"
"                      SS=1000.  This just sets scale of output.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"    3dDTtoNoisyDWI             \\\n"
"      -dti_in DTI/DT_DT+orig   \\\n"
"      -grads GRADS.dat         \\\n"
"      -noise_DWI 0.1           \\\n"
"      -noise_B0  0             \\\n"
"      -prefix NEW_DWIs_SNR10   \n"
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
   char *maskname=NULL;
   char *gradsname=NULL;
   char *dtsname=NULL;

   THD_3dim_dataset *MASK=NULL;
   THD_3dim_dataset *DTS=NULL;
   MRI_IMAGE *GRADS=NULL, *GRADS_IN=NULL;

   int Ngrads=0, Nfull=0;
	int Nvox=-1;            // tot number vox
	int Dim[3]={0,0,0};     // dim in each dir

   float NOISESCALE_DWI = -1.;
   float NOISESCALE_B0 = -1;
   float S0 = 1000.;
   float bval = 1.;
   int NOISE_IN_S0 = 0;

   byte *mskd2=NULL; // not great, but another format of mask

   float **dwi=NULL;
   THD_3dim_dataset *DWI_OUT=NULL;

   const gsl_rng_type * T;
   gsl_rng *r;
   long seed;


   srand(time(0));
   seed = time(NULL) ;
   gsl_rng_env_setup();
   T = gsl_rng_default;
   r = gsl_rng_alloc (T);
   gsl_rng_set (r, seed);
   
   // ###################################################################
   // #########################  load  ##################################
   // ###################################################################

   mainENTRY("3dDTtoNoisyDWI"); machdep(); 
	if (argc == 1) { usage_DTtoNoisyDWI(1); exit(0); }
   
   iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_DTtoNoisyDWI(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
     
      if( strcmp(argv[iarg],"-dt_in") == 0) {
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
   
      if( strcmp(argv[iarg],"-mask") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");
         maskname = strdup(argv[iarg]) ;
      
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-grads") == 0) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");
         gradsname = strdup(argv[iarg]) ;
      
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-noise_DWI") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need numerical argument after '-noise_DWI'");

         NOISESCALE_DWI = atof(argv[iarg]);
         
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-noise_B0") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need numerical argument after '-noise_B0'");

         NOISESCALE_B0 = atof(argv[iarg]);
         
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-S0") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need numerical argument after '-S0'");

         S0 = atof(argv[iarg]);
         if(S0 <= 0 )
            ERROR_exit("The '-S0' value must be >0.");
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-bval") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need numerical argument after '-bval'");

         bval = atof(argv[iarg]);
         if(bval <= 0 )
            ERROR_exit("The '-bval' value must be >0.");
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

   if(!gradsname)
      ERROR_exit("Need to input gradient file after '-grads'.");

   if( NOISESCALE_DWI<0 )
      ERROR_exit("Fractional noise value after '-snr0' needs to be >0. "
                 "It sets the noise scale of ref signal S0.");

   if(NOISESCALE_DWI > 0)
      INFO_message("You have chosen an SNR0 of approximately %.2f for DWIs",
                   1./NOISESCALE_DWI);
   else
      INFO_message("You have noiseless (i.e., infinite SNR) set of DWIs");

   if( NOISESCALE_B0 < 0 )
      NOISESCALE_B0 = NOISESCALE_DWI;

   if(NOISESCALE_B0 > 0)
      INFO_message("You have chosen an SNR0 of approximately %.2f for the B0",
                   1./NOISESCALE_B0);
   else
      INFO_message("You have noiseless (i.e., infinite SNR) reference B0.");


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
         if( fabs(THD_get_voxel(DTS,k,0) > EPS_V) )
            mskd2[k] = 1;
   }
      

   GRADS_IN = mri_read_1D (gradsname);
   GRADS = mri_transpose(GRADS_IN); // get rid of autotranspose...
   if (GRADS == NULL) 
         ERROR_exit("Error reading gradient vector file");
   mri_free(GRADS_IN);

   Ngrads = GRADS->ny;

   if(Ngrads < 6) 
      ERROR_exit("Too few grads (there appear to be only %d).",Ngrads);
   if(GRADS->nx !=3 ) 
      ERROR_exit("Wrong number of columns in the grad file: "
                 " am reading %d instead of 3.",GRADS->nx);
   Nfull = Ngrads+1;
   INFO_message("Have surmised there are %d total grads; "
                "output file will have %d bricks", Ngrads,Nfull);
   
   dwi = calloc(Nfull,sizeof(dwi)); 
   for(i=0 ; i<Nfull ; i++) 
		dwi[i] = calloc( Nvox,sizeof(float)); 

   INFO_message("Calculating the DWIs.");
   i = RicianNoiseDWIs( dwi, Nvox, Ngrads, DTS, 
                        NOISESCALE_DWI, NOISESCALE_B0,
                        GRADS, mskd2,
                        S0, bval, r);

   INFO_message("Writing the DWIs.");
   DWI_OUT = EDIT_empty_copy( DTS ); 
   EDIT_dset_items(DWI_OUT,
                   ADN_nvals, Nfull,
						 ADN_datum_all, MRI_float , 
                   ADN_prefix, prefix,
						 ADN_none );

   for( i=0; i<Nfull ; i++) {
		EDIT_substitute_brick(DWI_OUT, i, MRI_float, dwi[i]);
		dwi[i]=NULL;
	}

	THD_load_statistics( DWI_OUT );
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(DWI_OUT)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(DWI_OUT));
	tross_Make_History("3dDTtoNoisyDWI", argc, argv, DWI_OUT);
	THD_write_3dim_dataset(NULL, NULL, DWI_OUT, True);
	DSET_delete(DWI_OUT); 
  	free(DWI_OUT); 
   
   // #################################################################
   // ##########################  free  ###############################
   // #################################################################

   DSET_delete(DTS);
   free(DTS);
   
   for( i=0 ; i<Nfull ; i++)
      free(dwi[i]);
   free(dwi);

   free(prefix);
   free(gradsname);
   free(dtsname);
   mri_free(GRADS);

	exit(0);
}

