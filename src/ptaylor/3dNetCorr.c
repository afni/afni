/* 
   Calculate correlation coefficients of mean time series of a set of
   ROIs labelled by ints.  Written by PA Taylor (March, 2013).

   Jan. 2014
   + changed output format of *.netcc files to match that of 
   .grid files from Tracking
       
   Apr. 2014
   + changed output format of *.netts files to match that of 
   .grid files from Tracking (oops, should have done earlier)
   + new options:  insert ROI integer label into file, and have
   time series as individual files

   Apr. 2014, part II: revenge of the WB correlation
   + add in some individual file outputs
   + add in ability to do WB correlations

   June 2014
   + Partial correlation option
   
   Sept 2014: 
   + use label table of netrois, if exists

   Dec 2014:
   + niml.dset output form -> viewing in SUMA

   Jan 2015:
   + nifti outputtable

   Apr 2017:
   + change behavior around empty ROIs
   + allow WB dsets to be named by str labels

   May 2017:
   + bug fix in checksum to find null time series

*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <rsfc.h>    
#include <gsl/gsl_rng.h>
#include <3ddata.h>    
#include "DoTrackit.h"
#include "Fat_Labels.h"
#include <gsl/gsl_statistics_double.h>
#include "suma_suma.h"

//#define MAX_SELROI (200) // can't have more than this in SELROI

#ifdef USE_OMP
#include <omp.h>
#endif

#define MAX_PARAMS (4) // CC, FZ, PC, PCB right now


void usage_NetCorr(int detail) 
{
   printf(
"\n"
"  Calculate correlation matrix of a set of ROIs (using mean time series of\n"
"  each). Several networks may be analyzed simultaneously, one per brick.\n"
"\n"
"  Written by PA Taylor (March, 2013), part of FATCAT (Taylor & Saad,\n"
"  2013) in AFNI.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  \n"
"  + USAGE: Input a set of 4D data and a set of ROI masks (i.e., a bunch of \n"
"         ROIs in a brik each labelled with a distinct integer), and get a\n"
"         matrix of correlation values for it.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND: 3dNetCorr -prefix PREFIX {-mask MASK} {-fish_z} {-part_corr} \\\n"
"                -inset FILE -in_rois INROIS {-ts_out} {-ts_label}         \\\n"
"                {-ts_indiv} {-ts_wb_corr} {-ts_wb_Z} {-nifti}             \\\n"
"                {-push_thru_many_zeros} {-ts_wb_strlabel}                 \\\n"
"                {-output_mask_nonnull}\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + OUTPUT: \n"
"        Output will be a simple text file, first with the number N of ROIs\n"
"        in the set, then an empty line, then a list of the ROI labels in the\n"
"        file (i.e., col/row labels), empty line, and then an NxN matrix of\n"
"        correlation values (diagonals should be unity). One can also output\n"
"        the Fisher Z-transform of the matrix (with zeros along diag).\n"
"        If multiple subbricks are entered, one gets multiple files output,\n"
"        one per subbrick/network.\n"
"        Naming convention of outputs: PREFIX_\?\?\?.netcc, where `\?\?\?'\n"
"        represents a zero-padded version of the network number, based on the\n"
"        number of subbricks in the `in_rois' option (i.e., 000, 001,...).\n"
"        If the `-ts_out' option is used, the mean time series per ROI, one\n"
"        line, are output in PREFIX_\?\?\?.netts files.\n"
"        Labeltables are now also supported; when an '-inset FILE' contains\n"
"        a labeltable, the labels will then be passed to the *.netcc file.\n"
"        These labels may then be referred to in plotting/output, such as\n"
"        using fat_mat_sel.py.\n"
"        +NEW+ (Dec. 2014): A PREFIX_\?\?\?.niml.dset is now also output\n"
"        automatically.  This NIML/SUMA-esque file is mainly for use in SUMA,\n"
"        for visualizing connectivity matrix info in a 3D brain.  It can be\n"
"        opened via, for example:\n"
"        $ suma -vol ROI_FILE  -gdset FILE.niml.dset\n"
"\n"
"        It is now also possible to output whole brain correlation maps,\n"
"        generated from the average time series of each ROI,\n"
"        as either Pearson r or Fisher-transformed Z-scores (or both); see\n"
"        the '-ts_wb*' options below.\n"
"\n"
"        [As of April, 2017] There is now more checking done for having any\n"
"        null time series in ROIs.  They are bad to have around, esp. when\n"
"        they fill an ROI.  A new file called 'PREFIX.roidat' is now output,\n"
"        whose columns contain information for each ROI in the used mask:\n"
"        [Nvox] [Nvox with non-null ts] [non-null frac] # [ROI number] [label]\n"
"        The program also won't run now by default if an ROI contains more\n"
"        than 10 percent null time series; one can use a '-push*' option\n"
"        (see below) to still calculate anyways, but it will definitely cease\n"
"        if any ROI is full of null time series.\n"
"        ... And the user can flag to output a binary mask of the non-null\n"
"        time series, called 'PREFIX_mask_nnull*', with the new option\n"
"        '-output_mask_nonnull'.  This might be useful to check if your data\n"
"        are well-masked, if you haven't done so already (and you know who\n"
"        you are...).\n"
"\n"
"        [As of April, 2017] On a minor note, one can also apply string labels\n"
"        to the WB correlation/Z-score output files;  see the option\n"
"        '-ts_wb_strlabel', below.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + RUNNING, need to provide:\n"
"    -prefix PREFIX   :output file name part (see description below).\n"
"    -inset  FILE     :time series file (4D data set). \n\n"
"    -mask   MASK     :can include a whole brain mask within which to\n"
"                      calculate correlation. (Otherwise, data should be\n"
"                      masked already; the program will try to analyze.)\n"
"    -in_rois INROIS  :can input a set of ROIs, each labelled with distinct\n"
"                      integers. Multiple subbricks can be input, each will\n"
"                      be treated as a separate network.\n"
"    -fish_z          :switch to also output a matrix of Fisher Z-transform\n"
"                      values for the corr coefs (r):\n"
"                          Z = atanh(r) ,\n"
"                      (with Z=4 being output along matrix diagonals where\n"
"                      r=1, as the r-to-Z conversion is ceilinged at \n"
"                      Z = atanh(r=0.999329) = 4, which is still *quite* a\n"
"                      high Pearson-r value.\n"
"    -part_corr       :output the partial correlation matrix. It is \n"
"                      calculated from the inverse of regular Pearson\n"
"                      matrix, R, as follows: let M = R^{I} be in the inverse\n"
"                      of the Pearson cc matrix.  Then each element p_{ij} of\n"
"                      the partial correlation (PC) matrix is given as:\n"
"                      p_{ij} = -M_{ij}/sqrt( M_{ii} * M_{jj} ).\n"
"                      This will also calculate the PC-beta (PCB) matrix,\n"
"                      which is not symmetric, and whose values are given as:\n"
"                      b_{ij} = -M_{ij}/M_{ii}.\n"
"                      Use as you wish.  For both PC and PCB, the diagonals\n"
"                      should be uniformly (negative) unity.\n"
"    -ts_out          :switch to output the mean time series of the ROIs that\n"
"                      have been used to generate the correlation matrices.\n"
"                      Output filenames mirror those of the correlation\n"
"                      matrix files, with a '.netts' postfix.\n"
"    -ts_label        :additional switch when using '-ts_out'. Using this\n"
"                      option will insert the integer ROI label at the start\n"
"                      of each line of the *.netts file created. Thus, for\n"
"                      a time series of length N, each line will have N+1\n"
"                      numbers, where the first is the integer ROI label\n"
"                      and the subsequent N are scientific notation values.\n"
"    -ts_indiv        :switch to create a directory for each network that\n"
"                      contains the average time series for each ROI in\n"
"                      individual files (each file has one line).\n"
"                      The directories are labelled PREFIX_000_INDIV/,\n"
"                      PREFIX_001_INDIV/, etc. (one per network). Within each\n"
"                      directory, the files are labelled ROI_001.netts,\n"
"                      ROI_002.netts, etc., with the numbers given by the\n"
"                      actual ROI integer labels.\n"
"    -ts_wb_corr      :switch to perform whole brain correlation for each\n"
"                      ROI's average time series; this will automatically\n"
"                      create a directory for each network that contains the\n"
"                      set of whole brain correlation maps (Pearson 'r's).\n"
"                      The directories are labelled as above for '-ts_indiv'\n"
"                      Within each directory, the files are labelled\n"
"                      WB_CORR_ROI_001+orig, WB_CORR_ROI_002+orig, etc., with\n"
"                      the numbers given by the actual ROI integer labels.\n"
"    -ts_wb_Z         :same as above in '-ts_wb_corr', except that the maps\n"
"                      have been Fisher transformed to Z-scores the relation:\n"
"                      Z=atanh(r). \n"
"                      To avoid infinities in the transform, Pearson values \n"
"                      are effectively capped at |r| = 0.999329 (where\n"
"                      |Z| = 4.0;  hope that's good enough).\n"
"                      Files are labelled WB_Z_ROI_001+orig, etc.\n"
"\n"
"    -ts_wb_strlabel  :by default, '-ts_wb_{corr,Z}' output files are named\n"
"                      using the int number of a given ROI, such as:\n"
"                        WB_Z_ROI_001+orig.\n"
"                      with this option, one can replace the int (such as\n"
"                      '001') with the string label (such as 'L-thalamus')\n"
"                      *if* one has a labeltable attached to the file.\n"
"    -nifti           :output any correlation map files as NIFTI files\n"
"                      (default is BRIK/HEAD). Only useful if using\n"
"                      '-ts_wb_corr' and/or '-ts_wb_Z'.\n"
"\n"
"   -output_mask_nonnull\n"
"                     :internally, this program checks for where there are\n"
"                      nonnull time series, because we don't like those, in\n"
"                      general.  With this flag, the user can output the\n"
"                      determined mask of non-null time series.\n"
"   -push_thru_many_zeros\n"
"                     :by default, this program will grind to a halt and\n"
"                      refuse to calculate if any ROI contains >10 percent\n"
"                      of voxels with null times series (i.e., each point is\n"
"                      0), as of April, 2017.  This is because it seems most\n"
"                      likely that hidden badness is responsible. However,\n"
"                      if the user still wants to carry on the calculation\n"
"                      anyways, then this option will allow one to push on\n"
"                      through.  However, if any ROI *only* has null time\n"
"                      series, then the program will not calculate and the\n"
"                      user will really, really, really need to address\n"
"                      their masking.\n"
"\n"
"    -ignore_LT       :switch to ignore any label table labels in the \n"
"                      '-in_rois' file, if there are any labels attached.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"      3dNetCorr                                  \\\n"
"         -inset REST_in_DWI.nii.gz               \\\n"
"         -in_rois ROI_ICMAP_GM+orig              \\\n"
"         -fish_z                                 \\\n"
"         -ts_wb_corr                             \\\n"
"         -mask mask_DWI+orig                     \\\n"
"         -prefix FMRI/REST_corr\n"
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
   int i,j,k,m,n,mm;
   int iarg;
   THD_3dim_dataset *insetTIME = NULL;
   THD_3dim_dataset *MASK=NULL;
   THD_3dim_dataset *ROIS=NULL;
   char *prefix="NETCORR" ;
   char in_name[300];
   char in_mask[300];
   char in_rois[300];
   char OUT_grid[300];
   char OUT_indiv[300];
   char OUT_indiv0[300];
   //  int *SELROI=NULL; // if selecting subset of ROIs
   //  int HAVE_SELROI=0;
   
   int NIFTI_OUT = 0;

   byte ***mskd=NULL; // define mask of where time series are nonzero
   byte *mskd2=NULL; // not great, but another format of mask
   int HAVE_MASK=0;
   int HAVE_ROIS=0;
   int FISH_OUT=0;
   int PART_CORR=0;
   int TS_OUT=0;
   int TS_LABEL=0;
   int TS_INDIV=0;
   int TS_WBCORR_r=0;
   int TS_WBCORR_Z=0;
   int *NROI_REF=NULL,*INVROI_REF=NULL;
   int **ROI_LABELS_REF=NULL, **INV_LABELS_REF=NULL,**ROI_COUNT=NULL;
   int ***ROI_LISTS=NULL;
   double ***ROI_AVE_TS=NULL; // double because of GSL 
   float ***Corr_Matr=NULL; 
   float ***PCorr_Matr=NULL, ***PBCorr_Matr=NULL; 

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;
   int *Nlist=NULL;


   Dtable *roi_dtable=NULL;
   char *LabTabStr=NULL;
	char ***ROI_STR_LABELS=NULL;

   // for niml.dset -> graph viewing in SUMA
   char ***gdset_roi_names=NULL;
   SUMA_DSET *gset=NULL;
   float ***flat_matr=NULL;
   float *xyz=NULL;
   char OUT_gdset[300];
   NI_group *GDSET_netngrlink=NULL;
   char *NAME_gdset=NULL;
   int Noutmat = 1;  // num of matr to output: start with CC for sure
   char **ParLab=NULL;
   int FM_ctr = 0;  // for counting through flatmatr entries
   int OLD_LABEL=0; // ooollld style format of regions: Nnumber:Rnumber
   int IGNORE_LT=0; // ignore label table

   double checksum = 0.;
   int **ROI_COUNTnz=NULL;
   byte ***mskdnz=NULL; // use to check for nonzero locs
   byte *mskd2nz=NULL; // use to check for nonzero locs: use for mask app
   int DO_PUSH = 0;
   int *FLAG_nulls=NULL;
   int DO_STRLABEL = 0;
   int DO_OUTPUT_NONNULL=0;
   char prefix_nonnull[300];
   THD_3dim_dataset *MASK_nonnull=NULL;  // output nonnull mask, if
                                         // user wants

   int idx = 0;
   int Nmask = 0;
   FILE *fout1,*fin,*fout2;

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */
   mainENTRY("3dNetCorr"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   //  INFO_message("version: BETA");

   /** scan args **/
   if (argc == 1) { usage_NetCorr(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_NetCorr(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-prefix'");
         prefix = strdup(argv[iarg]) ;
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '-prefix'");
         iarg++ ; continue ;
      }
	 
      if( strcmp(argv[iarg],"-inset") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-input'");

         sprintf(in_name,"%s", argv[iarg]); 
         insetTIME = THD_open_dataset(in_name) ;
         if( (insetTIME == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",in_name);
         // just 0th time point for output...

         Dim = (int *)calloc(4,sizeof(int));
         DSET_load(insetTIME); CHECK_LOAD_ERROR(insetTIME);
         Nvox = DSET_NVOX(insetTIME) ;
         Dim[0] = DSET_NX(insetTIME); Dim[1] = DSET_NY(insetTIME); 
         Dim[2] = DSET_NZ(insetTIME); Dim[3]= DSET_NVALS(insetTIME); 

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");
         HAVE_MASK= 1;

         sprintf(in_mask,"%s", argv[iarg]); 
         MASK = THD_open_dataset(in_mask) ;
         if( (MASK == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",in_mask);

         DSET_load(MASK); CHECK_LOAD_ERROR(MASK);
			
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-in_rois") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_rois'");
      
         sprintf(in_rois,"%s", argv[iarg]); 
         ROIS = THD_open_dataset(in_rois) ;
         if( (ROIS == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",in_rois);
      
         DSET_load(ROIS); CHECK_LOAD_ERROR(ROIS);
         HAVE_ROIS=DSET_NVALS(ROIS); //number of subbricks
		
         iarg++ ; continue ;
      }
    
      if( strcmp(argv[iarg],"-fish_z") == 0) {
         FISH_OUT=1;
         iarg++ ; continue ;
      }

      // [Apr, 2017, PT]
      if( strcmp(argv[iarg],"-push_thru_many_zeros") == 0) {
         DO_PUSH=1;
         iarg++ ; continue ;
      }

      // [Apr, 2017, PT]
      if( strcmp(argv[iarg],"-output_mask_nonnull") == 0) {
         DO_OUTPUT_NONNULL=1;
         iarg++ ; continue ;
      }

      // [Apr, 2017, PT]
      if( strcmp(argv[iarg],"-ts_wb_strlabel") == 0) {
         DO_STRLABEL=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nifti") == 0) {
         NIFTI_OUT=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-part_corr") == 0) {
         PART_CORR=2; // because we calculate two matrices here
         iarg++ ; continue ;
      }
       if( strcmp(argv[iarg],"-ts_out") == 0) {
         TS_OUT=1;
         iarg++ ; continue ;
      }
    
      if( strcmp(argv[iarg],"-ts_label") == 0) {
         TS_LABEL=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ts_indiv") == 0) {
         TS_INDIV=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ts_wb_corr") == 0) {
         TS_WBCORR_r=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ts_wb_Z") == 0) {
         TS_WBCORR_Z=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-old_labels") == 0) {
         OLD_LABEL=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ignore_LT") == 0) {
         IGNORE_LT=1;
         iarg++ ; continue ;
      }


      /*  if( strcmp(argv[iarg],"-sel_roi") == 0 ){
          iarg++ ; if( iarg >= argc ) 
          ERROR_exit("Need argument after '-in_rois'");
      
          SELROI = (int *)calloc(MAX_SELROI,sizeof(int));
      
          if( (fin = fopen(argv[iarg], "r")) == NULL)  {
          fprintf(stderr, "Error opening file %s.",argv[iarg]);
          exit(1);
          }

          idx=0;
          while( !feof(fin) && (idx<MAX_SELROI-1) ){
          fscanf(fin, "%d",&SELROI[idx]);
          fscanf(fin," ");
          idx++;
          }
          HAVE_SELROI=idx;
          printf("HAVE_SELROI=%d\n",HAVE_SELROI);
          if(HAVE_SELROI<=0) {
          ERROR_message("Error reading in `-sel_roi'-- appears to have no ROIs listed.\n");
          exit(1);
          }

          iarg++ ; continue ;
          }*/


      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
  
   INFO_message("Reading in.");

   if( !TS_OUT && TS_LABEL) {
      ERROR_message("with '-ts_label', you also need '-ts_out'.\n");
      exit(1);
   }

   if (iarg < 3) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }
	
   if(!HAVE_ROIS) {
      ERROR_message("Need to load ROIs with >=1 subbrick...\n");
      exit(1);
   }

   if(Nvox != DSET_NVOX(ROIS)) {
      ERROR_message("Data sets of `-inset' and `in_rois' have "
                    "different numbers of voxels per brik!\n");
      exit(1);
   }
	
   if( (HAVE_MASK>0) && (Nvox != DSET_NVOX(MASK)) ) {
      ERROR_message("Data sets of `-inset' and `mask' have "
                    "different numbers of voxels per brik!\n");
      exit(1);
   }

	
   // ****************************************************************
   // ****************************************************************
   //                    make storage
   // ****************************************************************
   // ****************************************************************
	
   Nlist = (int *)calloc(1,sizeof(int)); 
   mskd2 = (byte *)calloc(Nvox,sizeof(byte)); 

   mskd = (byte ***) calloc( Dim[0], sizeof(byte **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (byte **) calloc( Dim[1], sizeof(byte *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (byte *) calloc( Dim[2], sizeof(byte) );

   mskd2nz = (byte *)calloc(Nvox,sizeof(byte)); 

   mskdnz = (byte ***) calloc( Dim[0], sizeof(byte **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskdnz[i] = (byte **) calloc( Dim[1], sizeof(byte *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskdnz[i][j] = (byte *) calloc( Dim[2], sizeof(byte) );

   if( (mskd == NULL) || (Nlist == NULL) || (mskd2 == NULL) ||
       (mskdnz == NULL) || (mskd2nz == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (masks).\n\n");
      exit(122);
   }
	
   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************
	
   INFO_message("Allocating...");

   if( HAVE_MASK ) 
      INFO_message("Applying user's mask");
   else
      INFO_message("User didn't enter mask: will make my own, "
                   "based on where I find nonzero time series.");

   idx = 0;
   // go through once: define data vox, and calc rank for each
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            // first, we make a mask of nonzero time series
            checksum = 0.;
            for( m=0 ; m<Dim[3] ; m++ ) 
               // [PT: May 27, 2017] fixed index 0 -> m
               checksum+= fabs(THD_get_voxel(insetTIME,idx,m)); 
            if( checksum > EPS_V ) {
               mskdnz[i][j][k] = 1;
               mskd2nz[idx] = 1;
            }

            // if user input a mask, then use that
            if( HAVE_MASK ) {
               if( THD_get_voxel(MASK,idx,0)>0 ) {
                  mskd[i][j][k] = 1;
                  mskd2[idx] = 1;
                  Nmask++;
               }
            }
            else    // ... use checksum results for nonzero time series
               if( checksum > EPS_V ) {
                  mskd[i][j][k] = 1;
                  mskd2[idx] = 1;
                  Nmask++;
               }
            idx+= 1; // skip, and mskd and KW are both still 0 from calloc
         }
   
   // [PT: Apr, 2017] output nonnull mask that we've calc'ed.  It is
   // the applied one *if* the user had not input a mask; otherwise,
   // they can compare it with their own and/or with the ROI map they
   // use.
   if ( DO_OUTPUT_NONNULL ) {
      INFO_message("Preparing mask of non-null time series for output.");

      MASK_nonnull = EDIT_empty_copy( insetTIME );

      if( NIFTI_OUT ) 
         sprintf(prefix_nonnull,"%s_%s.nii.gz", prefix, "mask_nnull");
      else
         sprintf(prefix_nonnull,"%s_%s", prefix, "mask_nnull");

      EDIT_dset_items( MASK_nonnull,
                       ADN_prefix    , prefix_nonnull,
                       ADN_datum_all , MRI_byte,
                       ADN_brick_fac , NULL,
                       ADN_nvals     , 1,
                       ADN_none );

      EDIT_substitute_brick(MASK_nonnull, 0, MRI_byte, mskd2nz);
      mskd2nz=NULL; // to not get into trouble...
      free(mskd2nz);

      THD_load_statistics(MASK_nonnull);
      tross_Copy_History( insetTIME , MASK_nonnull ) ;
      tross_Make_History( "3dNetCorr", argc, argv, MASK_nonnull );
      if( !THD_ok_overwrite() && 
          THD_is_ondisk(DSET_HEADNAME(MASK_nonnull)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(MASK_nonnull));
      THD_write_3dim_dataset(NULL, NULL, MASK_nonnull, True);
      INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(MASK_nonnull));
   }



   if (HAVE_MASK) {
      DSET_delete(MASK);
      free(MASK);
   }

   // obviously, this should always be TRUE at this point...
   if(HAVE_ROIS>0) {
     
      FLAG_nulls = (int *)calloc(HAVE_ROIS, sizeof(int)); 
      NROI_REF = (int *)calloc(HAVE_ROIS, sizeof(int)); 
      INVROI_REF = (int *)calloc(HAVE_ROIS, sizeof(int)); 
      if( (NROI_REF == NULL) || (INVROI_REF == NULL) ||
          (FLAG_nulls == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(122);
      }
     
      for( i=0 ; i<HAVE_ROIS ; i++) 
         INVROI_REF[i] = (int) THD_subbrick_max(ROIS, i, 1);
     
      ROI_LABELS_REF = calloc( HAVE_ROIS,sizeof(ROI_LABELS_REF));  
      for(i=0 ; i<HAVE_ROIS ; i++) 
         ROI_LABELS_REF[i] = calloc(INVROI_REF[i]+1,sizeof(int)); 
      INV_LABELS_REF = calloc( HAVE_ROIS,sizeof(INV_LABELS_REF));  
      for(i=0 ; i<HAVE_ROIS ; i++) 
         INV_LABELS_REF[i] = calloc(INVROI_REF[i]+1,sizeof(int)); 
     
      if( (ROI_LABELS_REF == NULL) || (INV_LABELS_REF == NULL) 
          ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }

      // PT: Apr, 2017: apply mask to ROIs, zero out those not in it
      // initially
      INFO_message("Applying mask to ROIs.");
      mm = THD_applydsetmask( ROIS, mskd2 );
      //INFO_message("FYI, there were %d voxels across all networks that got"
      //             "\n\tmasked out here.  If that seems like a lot to you, then"
      //             "\n\tconsider checking your masks and network maps again.", mm);

      INFO_message("Labelling regions internally.");

      // Step 3A-2: find out the labels in the ref, organize them
      //            both backwards and forwards.
      i = ViveLeRoi(ROIS, 
                    ROI_LABELS_REF, // ordered list of ROILABEL ints, [1..M]; 
                    //    maxval is N.
                    INV_LABELS_REF, // ith values at the actual input locs;
                    //    maxval is M.
                    NROI_REF,       // M: # of ROIs per brik
                    INVROI_REF);    // N: max ROI label per brik
      if( i != 1)
         ERROR_exit("Problem loading/assigning ROI labels");
     
      ROI_STR_LABELS = (char ***) calloc( HAVE_ROIS, sizeof(char **) );
      for ( i=0 ; i<HAVE_ROIS ; i++ ) 
         ROI_STR_LABELS[i] = (char **) calloc( NROI_REF[i]+1, sizeof(char *) );
      for ( i=0 ; i<HAVE_ROIS ; i++ ) 
         for ( j=0 ; j<NROI_REF[i]+1 ; j++ ) 
            ROI_STR_LABELS[i][j] = (char *) calloc( 100 , sizeof(char) );
      if(  (ROI_STR_LABELS == NULL)) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }

      // Sept 2014:  Labeltable stuff
      if( IGNORE_LT ) {
         INFO_message("Ignoring any '-in_rois' label table (if there is one).");
      }
      else{
         if ((ROIS->Label_Dtable = DSET_Label_Dtable(ROIS))) {
            if ((LabTabStr = Dtable_to_nimlstring( DSET_Label_Dtable(ROIS),
                                                   "VALUE_LABEL_DTABLE"))) {
               //fprintf(stdout,"%s", LabTabStr);
               if (!(roi_dtable = Dtable_from_nimlstring(LabTabStr))) {
                  ERROR_exit("Could not parse labeltable.");
               }
            } 
            else {
               INFO_message("No label table from '-in_rois'.");
            }
         }
      }

      i = Make_ROI_Output_Labels( ROI_STR_LABELS,
                                  ROI_LABELS_REF, 
                                  HAVE_ROIS,
                                  NROI_REF,
                                  roi_dtable, 
                                  1 );//!!!opts.DUMP_with_LABELS


      ROI_COUNT = calloc( HAVE_ROIS,sizeof(ROI_COUNT));  
      for(i=0 ; i<HAVE_ROIS ; i++) 
         ROI_COUNT[i] = calloc(NROI_REF[i],sizeof(int)); 

      ROI_COUNTnz = calloc( HAVE_ROIS,sizeof(ROI_COUNTnz));  
      for(i=0 ; i<HAVE_ROIS ; i++) 
         ROI_COUNTnz[i] = calloc(NROI_REF[i],sizeof(int)); 

      if( (ROI_COUNT == NULL) || (ROI_COUNTnz == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
	
      // find num of vox per ROI
      for( m=0 ; m<HAVE_ROIS ; m++ ) {
         idx=0;
         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) {
                  // count total num of vox per ROI
                  if( (THD_get_voxel(ROIS,idx,m) > 0 ) && mskd[i][j][k] ) {
                     ROI_COUNT[m][INV_LABELS_REF[m][(int) 
                                                    THD_get_voxel(ROIS,idx,m)]-1]++;
                  }
                  // count num of nonzero vox per ROI (above is just within mask)
                  if( (THD_get_voxel(ROIS,idx,m) > 0 ) && mskdnz[i][j][k] ) {
                     ROI_COUNTnz[m][INV_LABELS_REF[m][(int) 
                                                    THD_get_voxel(ROIS,idx,m)]-1]++;
                  }
                  idx++;
               }
      }
      
      /*
      // describe the nums of nonzeros still left in
      {
         fprintf(stderr,"\n\n ROI counts \n\n");
         for(i=0 ; i<HAVE_ROIS ; i++) {
            fprintf(stderr,"%10s %10s  %8s  #  %6s  %s\n", 
                    "ROI_vol", "ROI_nnull", "frac", "ROI", "ROI_label");
            for(j=0 ; j<NROI_REF[i] ; j++) {
               checksum = (ROI_COUNTnz[i][j]) / ((double) ROI_COUNT[i][j]);
               // badness count!
               if (checksum < 0.9 ) 
                  FLAG_nulls[i]+= 1;
               fprintf(stderr,"%10d %10d  %8.5f  #  %6d  %s\n",
                       ROI_COUNT[i][j], ROI_COUNTnz[i][j], (float) checksum, 
                       ROI_LABELS_REF[i][j+1], ROI_STR_LABELS[i][j+1]);
            }
         }
      }
      */

      // describe the nums of nonzeros still left in
      if( 1 ) {
         for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file
            
            sprintf(OUT_grid,"%s_%03d.roidat", prefix, k);
            if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
               fprintf(stderr, "Error opening file %s.", OUT_grid);
               exit(19);
            }
            fprintf(fout1, "# %8s %10s  %8s  #  %6s  %s\n", 
                    "N_vox", "N_nonnull", "frac", "ROI", "ROI_label");
            for(j=0 ; j<NROI_REF[k] ; j++) {
               checksum = (ROI_COUNTnz[k][j]) / ((double) ROI_COUNT[k][j]);
               // badness count!
               
               if (checksum <= EPS_V )
                  FLAG_nulls[k] = -1; // ultimate badness
               else if ( (checksum < 0.9) && (FLAG_nulls[k]>=0 ) )
                  FLAG_nulls[k]+= 1; //can be badness
               fprintf(fout1,"%10d %10d  %8.5f  #  %6d  %s\n",
                       ROI_COUNT[k][j], ROI_COUNTnz[k][j], (float) checksum, 
                       ROI_LABELS_REF[k][j+1], ROI_STR_LABELS[k][j+1]);
            }
            fclose(fout1);  
         }
      }

      for( k=0 ; k<HAVE_ROIS ; k++) {
         if( FLAG_nulls[k] < 0 ) {
            WARNING_message("Some null/empty time series in ROIs. See file:"
                            "\n\t%s_%03d.roivol", prefix, k);
            ERROR_message("Network [%d] has at least one ROI with null "
                          "time series! If you want, you *can* "
                          "\n\t use the '-push_thru_many_zeros' option "
                          "(see the help), but it ain't recommended.",
                          k, FLAG_nulls[k]); 
            exit(1);
         }
         else if( FLAG_nulls[k] > 0 ) {
            WARNING_message("Some null/empty time series in ROIs. See file:"
                            "\n\t %s_%03d.roivol", prefix, k);
            if ( DO_PUSH ) {
               WARNING_message("Network [%d] has %d ROIs with >10 percent "
                               "null time series!", k, FLAG_nulls[k]);
            }
            else {
               ERROR_message("Network [%d] has %d ROIs with >10 percent "
                             "null time series! If you want, you *can*"
                             "\n\t use the '-push_thru_many_zeros' option "
                             "(see the help), but it ain't recommended.",
                             k, FLAG_nulls[k]); 
               exit(1);
            }
         }
      }










      // make list of vox per ROI
      ROI_LISTS = (int ***) calloc( HAVE_ROIS, sizeof(int **) );
      for ( i=0 ; i<HAVE_ROIS ; i++ ) 
         ROI_LISTS[i] = (int **) calloc( NROI_REF[i], sizeof(int *) );
      for ( i=0 ; i <HAVE_ROIS ; i++ ) 
         for ( j=0 ; j<NROI_REF[i] ; j++ ) 
            ROI_LISTS[i][j] = (int *) calloc( ROI_COUNT[i][j], sizeof(int) );

      // make average time series per voxel
      ROI_AVE_TS = (double ***) calloc( HAVE_ROIS, sizeof(double **) );
      for ( i=0 ; i<HAVE_ROIS ; i++ ) 
         ROI_AVE_TS[i] = (double **) calloc( NROI_REF[i], sizeof(double *) );
      for ( i=0 ; i <HAVE_ROIS ; i++ ) 
         for ( j=0 ; j<NROI_REF[i] ; j++ ) 
            ROI_AVE_TS[i][j] = (double *) calloc( Dim[3], sizeof(double) );

      // store corr coefs
      Corr_Matr = (float ***) calloc( HAVE_ROIS, sizeof(float **) );
      for ( i=0 ; i<HAVE_ROIS ; i++ ) 
         Corr_Matr[i] = (float **) calloc( NROI_REF[i], sizeof(float *) );
      for ( i=0 ; i <HAVE_ROIS ; i++ ) 
         for ( j=0 ; j<NROI_REF[i] ; j++ ) 
            Corr_Matr[i][j] = (float *) calloc( NROI_REF[i], sizeof(float) );

      if( (ROI_LISTS == NULL) || (ROI_AVE_TS == NULL) 
          || (Corr_Matr == NULL)) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
	  
      if(PART_CORR) {
         PCorr_Matr = (float ***) calloc( HAVE_ROIS, sizeof(float **) );
         for ( i=0 ; i<HAVE_ROIS ; i++ ) 
            PCorr_Matr[i] = (float **) calloc( NROI_REF[i], sizeof(float *) );
         for ( i=0 ; i <HAVE_ROIS ; i++ ) 
            for ( j=0 ; j<NROI_REF[i] ; j++ ) 
               PCorr_Matr[i][j] = (float *) calloc( NROI_REF[i], sizeof(float));

         PBCorr_Matr = (float ***) calloc( HAVE_ROIS, sizeof(float **) );
         for ( i=0 ; i<HAVE_ROIS ; i++ ) 
            PBCorr_Matr[i] = (float **) calloc( NROI_REF[i], sizeof(float *) );
         for ( i=0 ; i <HAVE_ROIS ; i++ ) 
            for ( j=0 ; j<NROI_REF[i] ; j++ ) 
               PBCorr_Matr[i][j] = (float *) calloc( NROI_REF[i], sizeof(float));
         
         if( (PCorr_Matr == NULL) || (PBCorr_Matr == NULL) ) {
            fprintf(stderr, "\n\n MemAlloc failure.\n\n");
            exit(123);
         }
      }

      // reuse this to help place list indices
      for( i=0 ; i<HAVE_ROIS ; i++ ) 
         for( j=0 ; j<NROI_REF[i] ; j++ )
            ROI_COUNT[i][j] = 0;

      INFO_message("Getting volumes.");

      for( m=0 ; m<HAVE_ROIS ; m++ ) {
         idx=0;
         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) {
                  if( (THD_get_voxel(ROIS,idx,m) > 0) && mskd[i][j][k] ) {
                     mm = INV_LABELS_REF[m][(int) THD_get_voxel(ROIS,idx,m)]-1;
                     ROI_LISTS[m][mm][ROI_COUNT[m][mm]] = idx;
                     ROI_COUNT[m][mm]++;
                  }
                  idx++;
               }
      }
   }	

   // bit of freeing
   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) {
         free(mskd[i][j]);
         free(mskdnz[i][j]);
      }
   for( i=0 ; i<Dim[0] ; i++) {
      free(mskd[i]);
      free(mskdnz[i]);
   }
   free(mskd);
   free(mskdnz);

   INFO_message("Calculating average time series.");

   // ROI values
   for(i=0 ; i<HAVE_ROIS ; i++) 
      for( j=0 ; j<NROI_REF[i] ; j++ ) {
         Nlist[0]=ROI_COUNT[i][j];
         if ( Nlist[0] )    // otherwise, the ROI's ts is just 0s
            k = CalcAveRTS(ROI_LISTS[i][j], ROI_AVE_TS[i][j], 
                           insetTIME, Dim, Nlist);
      }
  
   INFO_message("Calculating correlation matrix.");
   if(PART_CORR)
      INFO_message("... and calculating partial correlation matrix.");

   for(i=0 ; i<HAVE_ROIS ; i++) {
      for( j=0 ; j<NROI_REF[i] ; j++ ) 
         for( k=j ; k<NROI_REF[i] ; k++ ) {
            Corr_Matr[i][j][k] = Corr_Matr[i][k][j] = (float) 
               CORR_FUN(ROI_AVE_TS[i][j], ROI_AVE_TS[i][k], Dim[3]);
         }

      if(PART_CORR)
         mm = CalcPartCorrMatr(PCorr_Matr[i], PBCorr_Matr[i],
                               Corr_Matr[i], NROI_REF[i]);
   }
  
   // **************************************************************
   // **************************************************************
   //                 Store and output
   // **************************************************************
   // **************************************************************

   INFO_message("Writing output: %s ...", prefix);


   // - - - - - - - - NIML prep - - - - - - - - - - - - - - 
   if(FISH_OUT)
      Noutmat++;
   if(PART_CORR)
      Noutmat+=2;

   ParLab = (char **)calloc(Noutmat, sizeof(char *)); 
   for (j=0; j<Noutmat; ++j) 
      ParLab[j] = (char *)calloc(32, sizeof(char));
   if( (ParLab == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(121);
   }
   
   // NIML output 
   flat_matr = (float ***) calloc( HAVE_ROIS, sizeof(float **) );
   for ( i = 0 ; i < HAVE_ROIS ; i++ ) 
      flat_matr[i] = (float **) calloc( Noutmat, sizeof(float *) );
   for ( i = 0 ; i < HAVE_ROIS ; i++ ) 
      for ( j = 0 ; j < Noutmat ; j++ ) 
         flat_matr[i][j] = (float *) calloc( NROI_REF[i]*NROI_REF[i], 
                                             sizeof(float));

   gdset_roi_names = (char ***)calloc(HAVE_ROIS, sizeof(char **));
	for (i=0; i< HAVE_ROIS ; i++ ) {
      gdset_roi_names[i] = (char **)calloc(NROI_REF[i], sizeof(char *));
      for (j=0; j<NROI_REF[i]; ++j) {
         gdset_roi_names[i][j] = (char *)calloc(32, sizeof(char));
         if( OLD_LABEL )
            snprintf(gdset_roi_names[i][j],31,"N%03d:R%d", i, 
                     ROI_LABELS_REF[i][j]);
         else{
            snprintf(gdset_roi_names[i][j],31,"%s",
                     ROI_STR_LABELS[i][j+1]);
            //fprintf(stderr," %s ",
            //       ROI_STR_LABELS[i][j+1]);
         }
      }
   }

   if(  (flat_matr == NULL) || ( gdset_roi_names == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(14);
      }
   

   for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file

      sprintf(OUT_grid,"%s_%03d.netcc",prefix,k); // zero counting now
      if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",OUT_grid);
         exit(19);
      }
    
      // same format as .grid files now
      fprintf(fout1,"# %d  # Number of network ROIs\n",NROI_REF[k]); // NROIs
      fprintf(fout1,"# %d  # Number of netcc matrices\n",
              FISH_OUT+PART_CORR+1); // Num of params

      // Sept 2014:  label_table stuff
      // don't need labeltable to make them, can do anyways
      fprintf(fout1, "# WITH_ROI_LABELS\n");
      for( i=1 ; i<NROI_REF[k] ; i++ ) 
         fprintf(fout1," %10s \t",ROI_STR_LABELS[k][i]); 
      fprintf(fout1,"  %10s\n",ROI_STR_LABELS[k][i]);
   
      // THIS IS FOR KNOWING WHICH MATR WE'RE AT
      // it's always zero for CC; they match one-to-one with later vars
      FM_ctr = 0; 
      ParLab[FM_ctr] = strdup("CC"); 

      for( i=1 ; i<NROI_REF[k] ; i++ ) // labels of ROIs
         fprintf(fout1," %10d \t",ROI_LABELS_REF[k][i]);// at =NROI, have '\n'
      fprintf(fout1,"  %10d\n# %s\n",ROI_LABELS_REF[k][i],"CC");
      for( i=0 ; i<NROI_REF[k] ; i++ ) {
         for( j=0 ; j<NROI_REF[k]-1 ; j++ ) {// b/c we put '\n' after last one.
            fprintf(fout1,"%12.4f\t",Corr_Matr[k][i][j]);
            flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = Corr_Matr[k][i][j];
         }
         fprintf(fout1,"%12.4f\n",Corr_Matr[k][i][j]);
         flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = Corr_Matr[k][i][j];
      }
    
      if(FISH_OUT) {
         FM_ctr++; 
         ParLab[FM_ctr] = strdup("FZ"); 

         fprintf(fout1,"# %s\n", "FZ");
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            for( j=0 ; j<NROI_REF[k]-1 ; j++ ) {// b/c we put '\n' after last
               fprintf(fout1,"%12.4f\t",BOBatanhf(Corr_Matr[k][i][j]));
               flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = 
                  BOBatanhf(Corr_Matr[k][i][j]);            
               /* fprintf(fout1,"%12.4f\t",FisherZ(Corr_Matr[k][i][j]));
               flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = 
               FisherZ(Corr_Matr[k][i][j]);*/
            }
            fprintf(fout1,"%12.4f\n",BOBatanhf(Corr_Matr[k][i][j]));
            flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = 
               BOBatanhf(Corr_Matr[k][i][j]);
            /*fprintf(fout1,"%12.4f\n",FisherZ(Corr_Matr[k][i][j]));
            flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = 
               FisherZ(Corr_Matr[k][i][j]);*/
         }
      }
    
      if(PART_CORR) {
         FM_ctr++; 
         ParLab[FM_ctr] = strdup("PC"); 

         fprintf(fout1,"# %s\n", "PC");
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            for( j=0 ; j<NROI_REF[k]-1 ; j++ ) {// b/c we put '\n' after last
               fprintf(fout1,"%12.4f\t",PCorr_Matr[k][i][j]);
               flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = PCorr_Matr[k][i][j];
            }
            fprintf(fout1,"%12.4f\n",PCorr_Matr[k][i][j]);
            flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = PCorr_Matr[k][i][j];
         }

         FM_ctr++; 
         ParLab[FM_ctr] = strdup("PCB"); 

         fprintf(fout1,"# %s\n", "PCB");
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            for( j=0 ; j<NROI_REF[k]-1 ; j++ ) {// b/c we put '\n' after last
               fprintf(fout1,"%12.4f\t",PBCorr_Matr[k][i][j]);
               flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = PBCorr_Matr[k][i][j];
            }
            fprintf(fout1,"%12.4f\n",PBCorr_Matr[k][i][j]);
            flat_matr[k][FM_ctr][i*NROI_REF[k]+j] = PBCorr_Matr[k][i][j];
         }
      }

      fclose(fout1);    
   
      // more nimling
      gset = SUMA_FloatVec_to_GDSET(flat_matr[k], Noutmat, 
                                    NROI_REF[k]*NROI_REF[k], 
                                    "full", ParLab, 
                                    NULL, NULL, NULL);
      if( xyz = THD_roi_cmass(ROIS, k, ROI_LABELS_REF[k]+1, NROI_REF[k], 0)) {
         if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
                                            xyz, NULL, NULL, 
                                            gdset_roi_names[k],
                                            NULL, NULL,
                                            NROI_REF[k]))) { 
            ERROR_message("Failed to add node list");
            exit(1);  
         }
         free(xyz);
      } 
      else {
         ERROR_message("Failed in THD_roi_cmass"); exit(1);
      }
      sprintf(OUT_gdset,"%s_%03d",prefix,k);
      GDSET_netngrlink = 
         Network_link(SUMA_FnameGet( OUT_gdset, "f",NULL));
      NI_add_to_group(gset->ngr, GDSET_netngrlink);
      NAME_gdset = SUMA_WriteDset_ns( OUT_gdset,
                                      gset, SUMA_ASCII_NIML, 1, 0);
      if (!NAME_gdset && !SUMA_IS_DSET_STDXXX_FORMAT(SUMA_ASCII_NIML)) { 
         ERROR_message("Failed to write dataset."); exit(1); 
      } else {
         if (NAME_gdset) SUMA_free(NAME_gdset); NAME_gdset = NULL;      
      }
      SUMA_FreeDset(gset);
      gset=NULL;
   }   
   
   if(TS_OUT) {
      for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file

         sprintf(OUT_grid,"%s_%03d.netts",prefix,k);
         if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
            fprintf(stderr, "Error opening file %s.",OUT_grid);
            exit(19);
         }
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            if(TS_LABEL)
               fprintf(fout1,"%d\t",ROI_LABELS_REF[k][i+1]); // labels go 1...M
            for( j=0 ; j<Dim[3]-1 ; j++ ) // b/c we put '\n' after last one.
               fprintf(fout1,"%.3e\t",ROI_AVE_TS[k][i][j]);
            fprintf(fout1,"%.3e\n",ROI_AVE_TS[k][i][j]);
         }
         fclose(fout1);  

      }
   }

   if( TS_INDIV ) {
      for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file
         sprintf(OUT_indiv0,"%s_%03d_INDIV", prefix, k);
         mkdir(OUT_indiv0, 0777);
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            sprintf(OUT_indiv,"%s/ROI_%03d.netts",
                    OUT_indiv0,ROI_LABELS_REF[k][i+1]);
            if( (fout2 = fopen(OUT_indiv, "w")) == NULL) {
               fprintf(stderr, "\nError opening file '%s'.\n",OUT_indiv);
               exit(19);
            }

            for( j=0 ; j<Dim[3]-1 ; j++ ) // b/c we put '\n' after last one.
               fprintf(fout2,"%.3e\t",ROI_AVE_TS[k][i][j]);
            fprintf(fout2,"%.3e\n",ROI_AVE_TS[k][i][j]);
          
            fclose(fout2);  
         }
      }
   }
  
   if( TS_WBCORR_r || TS_WBCORR_Z ) {
      
      INFO_message("Starting whole brain correlations.");
      
      i = WB_netw_corr( TS_WBCORR_r, 
                        TS_WBCORR_Z,                 
                        HAVE_ROIS, 
                        prefix,
                        NIFTI_OUT,
                        NROI_REF,
                        Dim,
                        ROI_AVE_TS,
                        ROI_LABELS_REF,
                        ROI_STR_LABELS,
                        DO_STRLABEL,
                        insetTIME,
                        mskd2,
                        Nmask,
                        argc,
                        argv);
   }
   
   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
   
   DSET_delete(ROIS);
   free(ROIS);

   for ( i = 0 ; i < HAVE_ROIS ; i++ ) {
      for (j = 0; j < NROI_REF[i]; ++j) 
         free(gdset_roi_names[i][j]);
      free(gdset_roi_names[i]);
   }
   free(gdset_roi_names);
   
   for ( i = 0 ; i < HAVE_ROIS ; i++ ) 
      for ( j = 0 ; j < Noutmat ; j++ ) 
         free(flat_matr[i][j]);
   for ( i = 0 ; i < HAVE_ROIS ; i++ ) 
      free(flat_matr[i]);
   free(flat_matr);

   for( i=0 ; i<Noutmat ; i++)  
      free(ParLab[i]);
   free(ParLab);




   if(LabTabStr)
      free(LabTabStr); 
   if(roi_dtable)
      free(roi_dtable);

   for ( i=0 ; i<HAVE_ROIS ; i++ ) 
      for ( j=0 ; j<NROI_REF[i]+1 ; j++ ) 
         free(ROI_STR_LABELS[i][j]);
   for ( i=0 ; i<HAVE_ROIS ; i++ ) 
      free(ROI_STR_LABELS[i]);
   free(ROI_STR_LABELS);


   DSET_delete(insetTIME);
   free(insetTIME);

   free(mskd2);
   free(Nlist);

   free(Dim); // need to free last because it's used for other arrays...
   free(prefix);

   //  if(HAVE_SELROI)
   //  free(SELROI);

   if(HAVE_ROIS >0) {
		
      free(FLAG_nulls);

      for( i=0 ; i<HAVE_ROIS ; i++) {
         for( j=0 ; j<NROI_REF[i] ; j++) {
            free(ROI_LISTS[i][j]);
            free(ROI_AVE_TS[i][j]);
            free(Corr_Matr[i][j]);
            if(PART_CORR) {
               free(PCorr_Matr[i][j]);
               free(PBCorr_Matr[i][j]);
            }
         }
         free(ROI_LISTS[i]);
         free(ROI_AVE_TS[i]);
         free(Corr_Matr[i]);
         if(PART_CORR){
            free(PCorr_Matr[i]);
            free(PBCorr_Matr[i]);
         }
         free(ROI_LABELS_REF[i]);
         free(INV_LABELS_REF[i]);
         free(ROI_COUNT[i]);
      }
      free(ROI_LISTS);
      free(ROI_AVE_TS);
      free(Corr_Matr);
      if(PART_CORR) {
         free(PCorr_Matr);
         free(PBCorr_Matr);
      }
      free(ROI_LABELS_REF);
      free(INV_LABELS_REF);
      free(ROI_COUNT);
      free(NROI_REF);
      free(INVROI_REF);
   }
	
   return 0;
}
