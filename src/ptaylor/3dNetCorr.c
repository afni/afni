/* 
   Calculate correlation coefficients of mean time series of a set of
   ROIs labelled by ints.  Written by PA Taylor (March, 2013).

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <rsfc.h>    
#include <3ddata.h>    
#include "DoTrackit.h"
#include <gsl/gsl_statistics_double.h>

//#define MAX_SELROI (200) // can't have more than this in SELROI

void usage_NetCorr(int detail) 
{
  printf(
"\n"
"  Calculate correlation matrix of a set of ROIs (using mean time series of\n"
"  each). Written by PA Taylor (March, 2013).\n\n"
"  \n"
"  USAGE: Input a set of 4D data and a set of ROI masks (i.e., a bunch of \n"
"         ROIs in a brik each labelled with a distinct integer), and get a\n"
"         matrix of correlation values for it.\n"
"  \n"
"  COMMAND: 3dNetCorr -prefix PREFIX {-mask MASK} {-fish_z} \\\n"
"                -inset FILE -in_rois INROIS   \n"
"  \n\n"
"  + RUNNING, need to provide:\n"
"    -prefix PREFIX   :output file name part (see description below).\n"
"    -inset  FILE     :time series file (4D data set). \n\n"
"    -mask   MASK     :can include a whole brain mask within which to\n"
"                      calculate correlation. (Otherwise, data should be\n"
"                      masked already, but it's not *so* necessary here).\n"
"    -in_rois INROIS  :can input a set of ROIs, each labelled with distinct\n"
"                      integers. Multiple subbricks can be input, each will\n"
"                      be treated as a separate network.\n"
"    -fish_z          :switch to also output a matrix of Fisher Z-transform\n"
"                      values for the corr coefs (r):\n"
"                          Z = 0.5 ln( [1+r]/[1-r] )\,\n"
"                      (with zeros being output along matrix diagonals where\n"
"                      r=1).\n"
"    -ts_out          :switch to output the mean time series of the ROIs that\n"
"                      have been used to generate the correlation matrices.\n"
"                      Output filenames mirror those of the correlation\n"
"                      matrix files, with a '.netts' postfix.\n\n"
"  + OUTPUT: \n"
"        Output will be a simple text file, first with the number N of ROIs\n"
"        in the set, then an empty line, then a list of the ROI labels in the\n"
"        file (i.e., col/row labels), empty line, and then an NxN matrix of\n"
"        correlation values (diagonals should be unity). One can also output\n"
"        the Fisher Z-transform of the matrix (with zeros along diag).\n"
"        If multiple subbricks are entered, one gets multiple files output,\n"
"        one per subbrick/network.\n"
"        Naming convention of outputs: PREFIX_???.netcc, where the `???'\n"
"        represent a zero-padded version of the network number, based on the\n"
"        number of subbricks in the `in_rois' option (i.e., 001, 002,...).\n"
"        If the `-ts_out' option is used, the mean time series per ROI, one\n"
"        line, are output in PREFIX_???.netts files.\n\n");
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
  //  int *SELROI=NULL; // if selecting subset of ROIs
  //  int HAVE_SELROI=0;

  int ***mskd; // define mask of where time series are nonzero
  int HAVE_MASK=0;
  int HAVE_ROIS=0;
  int FISH_OUT=0;
  int TS_OUT=0;
  int *NROI_REF=NULL,*INVROI_REF=NULL;
  int **ROI_LABELS_REF=NULL, **INV_LABELS_REF=NULL,**ROI_COUNT=NULL;
  int ***ROI_LISTS=NULL;
  double ***ROI_AVE_TS=NULL; // double because of GSL 
  float ***Corr_Matr=NULL; 

  int Nvox=-1;   // tot number vox
  int *Dim=NULL;
  int *Nlist;

  int idx;
  FILE *fout1,*fin;

  mainENTRY("3dNetCorr"); machdep(); 
  
  // ****************************************************************
  // ****************************************************************
  //                    load AFNI stuff
  // ****************************************************************
  // ****************************************************************

  INFO_message("version: BETA");
	
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
  
  if (iarg < 3) {
    ERROR_message("Too few options. Try -help for details.\n");
    exit(1);
  }
	
  if(!HAVE_ROIS) {
    ERROR_message("Need to load ROIs with >=1 subbrick...\n");
    exit(1);
  }

  if(Nvox != DSET_NVOX(ROIS)) {
    ERROR_message("Data sets of `-inset' and `in_rois' have different numbers of voxels per brik!\n");
    exit(1);
  }
	
  if( (HAVE_MASK>0) && (Nvox != DSET_NVOX(MASK)) ) {
    ERROR_message("Data sets of `-inset' and `mask' have different numbers of voxels per brik!\n");
    exit(1);
  }

	
  // ****************************************************************
  // ****************************************************************
  //                    make storage
  // ****************************************************************
  // ****************************************************************
	
  Nlist = (int *)calloc(1,sizeof(int)); 

  mskd = (int ***) calloc( Dim[0], sizeof(int **) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    for ( j = 0 ; j < Dim[1] ; j++ ) 
      mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );

  if( (mskd == NULL) || (Nlist == NULL) ) { 
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(122);
  }
	
  // *************************************************************
  // *************************************************************
  //                    Beginning of main loops
  // *************************************************************
  // *************************************************************
	
  // go through once: define data vox, and calc rank for each
  idx = 0;
  for( k=0 ; k<Dim[2] ; k++ ) 
    for( j=0 ; j<Dim[1] ; j++ ) 
      for( i=0 ; i<Dim[0] ; i++ ) {
        if( HAVE_MASK ) {
          if( THD_get_voxel(MASK,idx,0)>0 )
            mskd[i][j][k] = 1;
        }
        else
          if( fabs(THD_get_voxel(insetTIME,idx,0))+
              fabs(THD_get_voxel(insetTIME,idx,1))+
              fabs(THD_get_voxel(insetTIME,idx,2))+
              fabs(THD_get_voxel(insetTIME,idx,3))+
              fabs(THD_get_voxel(insetTIME,idx,4)) > EPS_V)
            mskd[i][j][k] = 1;
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
     
    if( (ROI_LABELS_REF == NULL) || (ROI_LABELS_REF == NULL) 
        ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(123);
    }
     
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

    if( (ROI_LISTS == NULL) || (ROI_AVE_TS == NULL)|| (Corr_Matr == NULL)) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(123);
    }
	  
    // reuse this to help place list indices
    for( i=0 ; i<HAVE_ROIS ; i++ ) 
      for( j=0 ; j<NROI_REF[i] ; j++ )
        ROI_COUNT[i][j] = 0;

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
  
  // ROI values
  for(i=0 ; i<HAVE_ROIS ; i++) 
    for( j=0 ; j<NROI_REF[i] ; j++ ) {
      Nlist[0]=ROI_COUNT[i][j];
      k = CalcAveRTS(ROI_LISTS[i][j], ROI_AVE_TS[i][j], 
                    insetTIME, Dim, Nlist);
    }
  
  for(i=0 ; i<HAVE_ROIS ; i++) 
    for( j=0 ; j<NROI_REF[i] ; j++ ) 
      for( k=j ; k<NROI_REF[i] ; k++ ) {
        Corr_Matr[i][j][k] = Corr_Matr[i][k][j] = (float) 
          gsl_stats_correlation(ROI_AVE_TS[i][j], 1, 
                                ROI_AVE_TS[i][k], 1, 
                                Dim[3]);
      }
  
  // **************************************************************
  // **************************************************************
  //                 Store and output
  // **************************************************************
  // **************************************************************
  
  for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file

    sprintf(OUT_grid,"%s_%03d.netcc",prefix,k+1);
    if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s.",OUT_grid);
      exit(19);
    }
    
    fprintf(fout1,"%d\n\n",NROI_REF[k]);
    for( i=1 ; i<NROI_REF[k] ; i++ ) // labels of ROIs
      fprintf(fout1,"%d\t",ROI_LABELS_REF[k][i]); // because at =NROI, -> \n
    fprintf(fout1,"%d\n\n",ROI_LABELS_REF[k][i]);
    for( i=0 ; i<NROI_REF[k] ; i++ ) {
      for( j=0 ; j<NROI_REF[k]-1 ; j++ ) // b/c we put '\n' after last one.
        fprintf(fout1,"%.4f\t",Corr_Matr[k][i][j]);
      fprintf(fout1,"%.4f\n",Corr_Matr[k][i][j]);
    }
    
    if(FISH_OUT) {
      fprintf(fout1,"\n");
      for( i=0 ; i<NROI_REF[k] ; i++ ) {
        for( j=0 ; j<NROI_REF[k]-1 ; j++ ) // b/c we put '\n' after last one.
          fprintf(fout1,"%.4f\t",FisherZ(Corr_Matr[k][i][j]));
        fprintf(fout1,"%.4f\n",FisherZ(Corr_Matr[k][i][j]));
      }
    }
    
    fclose(fout1);    
  }

  if(TS_OUT) {
    for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file

      sprintf(OUT_grid,"%s_%03d.netts",prefix,k+1);
      if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
        fprintf(stderr, "Error opening file %s.",OUT_grid);
        exit(19);
      }
      for( i=0 ; i<NROI_REF[k] ; i++ ) {
        for( j=0 ; j<Dim[3]-1 ; j++ ) // b/c we put '\n' after last one.
          fprintf(fout1,"%.3e\t",ROI_AVE_TS[k][i][j]);
        fprintf(fout1,"%.3e\n",ROI_AVE_TS[k][i][j]);
      }
      fclose(fout1);  

    }
  }

  
  INFO_message("Correlation calculated.");
  
  
  // ************************************************************
  // ************************************************************
  //                    Freeing
  // ************************************************************
  // ************************************************************
	
  DSET_delete(insetTIME);
  DSET_delete(MASK);
  DSET_delete(ROIS);
  free(MASK);
  free(ROIS);
  free(insetTIME);

  for( i=0 ; i<Dim[0] ; i++) 
    for( j=0 ; j<Dim[1] ; j++) {
      free(mskd[i][j]);
    }
  for( i=0 ; i<Dim[0] ; i++) {
    free(mskd[i]);
  }
  free(mskd);
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
