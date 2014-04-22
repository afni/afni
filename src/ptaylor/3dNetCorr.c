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
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <rsfc.h>    
#include <gsl/gsl_rng.h>
#include <3ddata.h>    
#include "DoTrackit.h"
#include <gsl/gsl_statistics_double.h>

//#define MAX_SELROI (200) // can't have more than this in SELROI

#ifdef USE_OMP
#include <omp.h>
#endif

#ifdef USE_OMP
#include "3dNetCorr.c"
#endif

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
"  + COMMAND: 3dNetCorr -prefix PREFIX {-mask MASK} {-fish_z}        \\\n"
"                -inset FILE -in_rois INROIS {-ts_out} {-ts_label} \\\n"
"                {-ts_indiv} {-ts_wb_corr}  \n"
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
"                          Z = 0.5 ln( [1+r]/[1-r] ) ,\n"
"                      (with zeros being output along matrix diagonals where\n"
"                      r=1).\n"
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
"                      set of whole brain correlation maps.\n"
"                      The directories are labelled as above for '-ts_indiv'\n"
"                      Within each directory, the files are labelled\n"
"                      WB_CORR_ROI_001+orig, WB_CORR_ROI_002+orig, etc., with\n"
"                      the numbers given by the actual ROI integer labels.\n\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"      3dNetcorr                                  \\\n"
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
   THD_3dim_dataset *OUT_CORR_MAP=NULL;
   MRI_IMAGE *mri=NULL;
   char *prefix="NETCORR" ;
   char in_name[300];
   char in_mask[300];
   char in_rois[300];
   char OUT_grid[300];
   char OUT_indiv[300];
   char OUT_indiv0[300];
   //  int *SELROI=NULL; // if selecting subset of ROIs
   //  int HAVE_SELROI=0;

   int ***mskd=NULL; // define mask of where time series are nonzero
   byte *mskd2=NULL; // not great, but another format of mask
   int HAVE_MASK=0;
   int HAVE_ROIS=0;
   int FISH_OUT=0;
   int TS_OUT=0;
   int TS_LABEL=0;
   int TS_INDIV=0;
   int TS_WBCORR=0;
   int *NROI_REF=NULL,*INVROI_REF=NULL;
   int **ROI_LABELS_REF=NULL, **INV_LABELS_REF=NULL,**ROI_COUNT=NULL;
   int ***ROI_LISTS=NULL;
   double ***ROI_AVE_TS=NULL; // double because of GSL 
   float **AVE_TS_fl=NULL;    // not great, but another format of TS
   float ***Corr_Matr=NULL; 

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;
   int *Nlist=NULL;

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
         TS_WBCORR=1;
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

   mskd = (int ***) calloc( Dim[0], sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );

   if( (mskd == NULL) || (Nlist == NULL) || (mskd2 == NULL)) { 
      fprintf(stderr, "\n\n MemAlloc failure (masks).\n\n");
      exit(122);
   }
	
   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************
	
   INFO_message("Allocating...");

   // go through once: define data vox, and calc rank for each
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( HAVE_MASK ) {
               if( THD_get_voxel(MASK,idx,0)>0 ) {
                  mskd[i][j][k] = 1;
                  mskd2[idx] = 1;
                  Nmask++;
               }
            }
            else // simple automask attempt
               if( fabs(THD_get_voxel(insetTIME,idx,0))+
                   fabs(THD_get_voxel(insetTIME,idx,1))+
                   fabs(THD_get_voxel(insetTIME,idx,2))+
                   fabs(THD_get_voxel(insetTIME,idx,3))+
                   fabs(THD_get_voxel(insetTIME,idx,4)) > EPS_V) {
                  mskd[i][j][k] = 1;
                  mskd2[idx] = 1;
                  Nmask++;
               }
            idx+= 1; // skip, and mskd and KW are both still 0 from calloc
         }
   
   
   // obviously, this should always be TRUE at this point...
   if(HAVE_ROIS>0) {
     
      NROI_REF = (int *)calloc(HAVE_ROIS, sizeof(int)); 
      INVROI_REF = (int *)calloc(HAVE_ROIS, sizeof(int)); 
      if( (NROI_REF == NULL) || (INVROI_REF == NULL) ) {
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
     
      ROI_COUNT = calloc( HAVE_ROIS,sizeof(ROI_COUNT));  
      for(i=0 ; i<HAVE_ROIS ; i++) 
         ROI_COUNT[i] = calloc(NROI_REF[i],sizeof(int)); 

      if( (ROI_COUNT == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
	
      // find num of vox per ROI
      for( m=0 ; m<HAVE_ROIS ; m++ ) {
         idx=0;
         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) {
                  if( (THD_get_voxel(ROIS,idx,m) > 0 ) && mskd[i][j][k] ) {
                     ROI_COUNT[m][INV_LABELS_REF[m][(int) 
                                                    THD_get_voxel(ROIS,idx,m)]-1]++;
                  }
                  idx++;
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
  
   INFO_message("Calculating average time series.");


   // ROI values
   for(i=0 ; i<HAVE_ROIS ; i++) 
      for( j=0 ; j<NROI_REF[i] ; j++ ) {
         Nlist[0]=ROI_COUNT[i][j];
         k = CalcAveRTS(ROI_LISTS[i][j], ROI_AVE_TS[i][j], 
                        insetTIME, Dim, Nlist);
      }
  
   INFO_message("Calculating correlation matrix.");

   for(i=0 ; i<HAVE_ROIS ; i++) 
      for( j=0 ; j<NROI_REF[i] ; j++ ) 
         for( k=j ; k<NROI_REF[i] ; k++ ) {
            Corr_Matr[i][j][k] = Corr_Matr[i][k][j] = (float) 
               CORR_FUN(ROI_AVE_TS[i][j], ROI_AVE_TS[i][k], Dim[3]);
         }
  
   // **************************************************************
   // **************************************************************
   //                 Store and output
   // **************************************************************
   // **************************************************************
  
   INFO_message("Writing output.");

   for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file

      sprintf(OUT_grid,"%s_%03d.netcc",prefix,k); // zero counting now
      if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",OUT_grid);
         exit(19);
      }
    
      // same format as .grid files now
      fprintf(fout1,"# %d  # Number of network ROIs\n",NROI_REF[k]); // NROIs
      fprintf(fout1,"# %d  # Number of matrices\n",FISH_OUT+1); // Num of params
      //    fprintf(fout1,"%d\n\n",NROI_REF[k]);
      for( i=1 ; i<NROI_REF[k] ; i++ ) // labels of ROIs
         fprintf(fout1,"%8d    \t",ROI_LABELS_REF[k][i]);// because at =NROI, ->\n
      fprintf(fout1,"%8d\n# %s\n",ROI_LABELS_REF[k][i],"CC");
      for( i=0 ; i<NROI_REF[k] ; i++ ) {
         for( j=0 ; j<NROI_REF[k]-1 ; j++ ) // b/c we put '\n' after last one.
            fprintf(fout1,"%12.4f\t",Corr_Matr[k][i][j]);
         fprintf(fout1,"%12.4f\n",Corr_Matr[k][i][j]);
      }
    
      if(FISH_OUT) {
         fprintf(fout1,"# %s\n", "FZ");
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            for( j=0 ; j<NROI_REF[k]-1 ; j++ ) // b/c we put '\n' after last one.
               fprintf(fout1,"%12.4f\t",FisherZ(Corr_Matr[k][i][j]));
            fprintf(fout1,"%12.4f\n",FisherZ(Corr_Matr[k][i][j]));
         }
      }
    
      fclose(fout1);    
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
  
   if( TS_WBCORR ) {
      INFO_message("Starting whole brain correlations.");
      // make average time series per voxel
      AVE_TS_fl = calloc( 1,sizeof(AVE_TS_fl));  
      for(i=0 ; i<1 ; i++) 
         AVE_TS_fl[i] = calloc(Dim[3],sizeof(float)); 

      if( (AVE_TS_fl == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure (time series out).\n\n");
         exit(123);
      }

      for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file
         sprintf(OUT_indiv0,"%s_%03d_INDIV", prefix, k);
         mkdir(OUT_indiv0, 0777);
         for( i=0 ; i<NROI_REF[k] ; i++ ) {
            for( j=0 ; j<Dim[3] ; j++)
               AVE_TS_fl[0][j] = (float) ROI_AVE_TS[k][i][j];
            sprintf(OUT_indiv,"%s/WB_CORR_ROI_%03d",
                    OUT_indiv0,ROI_LABELS_REF[k][i+1]);
            mri = mri_float_arrays_to_image(AVE_TS_fl,Dim[3],1);
            OUT_CORR_MAP = THD_Tcorr1D(insetTIME, mskd2, Nmask,
                                       mri,
                                       "pearson", OUT_indiv);

				tross_Copy_History( insetTIME , OUT_CORR_MAP ) ;
            tross_Make_History( "3dTcorr1D" , argc,argv , OUT_CORR_MAP );
            if( !THD_ok_overwrite() && 
                THD_is_ondisk(DSET_HEADNAME(OUT_CORR_MAP)) )
               ERROR_exit("Can't overwrite existing dataset '%s'",
                          DSET_HEADNAME(OUT_CORR_MAP));
            THD_write_3dim_dataset(NULL, NULL, OUT_CORR_MAP, True);
            INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(OUT_CORR_MAP));
            DSET_delete(OUT_CORR_MAP);
            free(OUT_CORR_MAP);
            OUT_CORR_MAP=NULL;

         }
      }

      mri_free(mri);
      for( i=0 ; i<1 ; i++) 
         free(AVE_TS_fl[i]);
      free(AVE_TS_fl);
   }
  
  
  
   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************

   DSET_delete(insetTIME);
   DSET_delete(ROIS);
   free(ROIS);
   free(insetTIME);
   if (HAVE_MASK) {
      DSET_delete(MASK);
      free(MASK);
   }

   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) {
         free(mskd[i][j]);
      }
   for( i=0 ; i<Dim[0] ; i++) {
      free(mskd[i]);
   }
   free(mskd);
   free(mskd2);
   free(Nlist);

   free(Dim); // need to free last because it's used for other arrays...
   free(prefix);

   //  if(HAVE_SELROI)
   //  free(SELROI);

   if(HAVE_ROIS >0) {
		
      for( i=0 ; i<HAVE_ROIS ; i++) {
         for( j=0 ; j<NROI_REF[i] ; j++) {
            free(ROI_LISTS[i][j]);
            free(ROI_AVE_TS[i][j]);
            free(Corr_Matr[i][j]);
         }
         free(ROI_LISTS[i]);
         free(ROI_AVE_TS[i]);
         free(Corr_Matr[i]);
         free(ROI_LABELS_REF[i]);
         free(INV_LABELS_REF[i]);
         free(ROI_COUNT[i]);
      }
      free(ROI_LISTS);
      free(ROI_AVE_TS);
      free(Corr_Matr);
      free(ROI_LABELS_REF);
      free(INV_LABELS_REF);
      free(ROI_COUNT);
      free(NROI_REF);
      free(INVROI_REF);
   }
	
   return 0;
}
