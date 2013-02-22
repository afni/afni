/* 
   REHO/Kendall W code, written by PA Taylor (July, 2012).

   ReHo (regional homogeneity) is just a renaming of the Kendall's W
	(or Kendall's coefficient of concordance, KCC, (Kendall & Babington
	Smith, 1939)) for a time series.  Application to fMRI data was
	described in paper: <<Regional homogeneity approach to fMRI data
	analysis>> by Zang, Jiang, Lu, He, and Tiana (2004, NeuroImage).

	+ updated, Sept. 2012: just memory freeing stuff.
	+ updated, Dec. 2012: just to include mask, if wanted.
	+ updated, Jan. 2013: *lot* of methodology changes, in order to
                         allow mask of ROIs to be input, calc. ReHo
                         per ROI and output column of values
   + updated, Feb. 2013: for large ROIs, no longer *int* issue
                         more mask use
                         EPS_v thing sorted
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

void usage_ReHo(int detail) 
{
  printf(
"\n"
"  REHO/Kendall W code, written by PA Taylor (July, 2012).\n\n"
"  ReHo (regional homogeneity) is just a renaming of the Kendall's W\n"
"  (or Kendall's coefficient of concordance, KCC, (Kendall & Babington\n"
"  Smith, 1939)) for set of time series.  Application to fMRI data was\n"
"  described in paper: <<Regional homogeneity approach to fMRI data\n"
"  analysis>> by Zang, Jiang, Lu, He, and Tiana (2004, NeuroImage),\n"
"  where it was applied to the study of both task and resting state\n"
"  functional connectivity (RSFC).\n"
"  \n"
"  USAGE: This program is made to read in data from 4D time series data set\n"
"         and to calculate Kendall's W per voxel using neighborhood voxels. \n"
"         Instead of the time series values themselves, Kendall's W uses the\n"
"         relative rank ordering of a 'hood over all time points to evaluate\n"
"         a parameter W in range 0-1, with 0 reflecting no trend of agreement\n"
"         between time series and 1 reflecting perfect agreement. From W, one\n"
"         can simply get Friedman's chi-square value (with degrees of freedom\n"
"         equal to `the length of the time series minus one'), so this can\n"
"         also be calculated here and returned in the second sub-brick:\n"
"         chi-sq = (N_n)*(N_t - 1)*W,   with N_dof = N_t - 1,\n"
"         where N_n is the size of neighborhood; N_t is the number of \n"
"         time points; W is the ReHo or concordance value; and N_dof is the\n"
"         number of degrees of freedom. A switch is included to have the \n"
"         chi-sq value output as a subbrick of the ReHo/W. (In estimating W,\n"
"         tied values are taken into account by averaging appropriate \n"
"         rankings and adjusting other factors in W appropriately, which \n"
"         only makes a small difference in value, but the computational time\n"
"         still isn't that bad).\n"
"  \n"
"  COMMAND:  3dReHo -prefix PREFIX -inset FILE {-nneigh 7|19|27} {-chi_sq} \\\n"
"                   {-mask MASK}  {-in_rois INROIS} \n"
"  \n\n"
"  + RUNNING, need to provide:\n"
"    -prefix PREFIX   :output file name part.\n"
"    -nneigh NUMBER   :number of voxels in neighborhood, inclusive; can be: \n"
"                      7   (for facewise neighbors, only),\n"
"                      19  (for face- and edge-wise neighbors),\n"
"                      27  (for face-, edge-, and node-wise neighbors).\n"
"                      The default is: 27.\n"
"    -inset  FILE     :time series file. \n\n"
"    -chi_sq          :switch to output Friedman chi-sq value per voxel\n"
"                      as a subbrick.\n"
"    -mask   MASK     :can include a whole brain mask within which to\n"
"                      calculate ReHo. Otherwise, data should be masked\n"
"                      already.\n"
"    -in_rois INROIS  :can input a set of ROIs, each labelled with distinct\n"
"                      integers. ReHo will be calculated per ROI. The output\n"
"                      will be similar to the format of 3dROIstats: one row\n"
"                      of numbers per INROIS subbrick, and the number of \n"
"                      columns determined by the number of ROIs per subbrick\n"
"                      (but only numbers are output). The output of this is\n"
"                      in a file called PREFIX_ROI_reho.vals, and if\n"
"                      `-chi_sq' values are being output, then those for the\n"
"                      ROI values will be output in an analogously formatted\n"
"                      file called PREFIX_ROI_reho.chi.\n"
"                      Voxelwise ReHo will still be calculated and output.\n"
"  + OUTPUT: \n"
"         [A] single file with name, e.g., PREFIX+orig.BRIK, which may have\n"
"              two subbricks (2nd subbrick if `-chi_sq' switch is used):\n"
"              [0] contains the ReHo (Kendall W) value per voxel;\n"
"              [1] contains Friedman chi-square of ReHo per voxel (optional);\n"
"                  note that the number of degrees of freedom of this value\n"
"                  is the length of time series minus 1.\n"
"         [B] can get list of ROI ReHo values, as well (optional).\n\n"
"  \n" );
  return;
}

int main(int argc, char *argv[]) {
  int i,j,k,m,n,mm;
  int iarg;
  THD_3dim_dataset *insetTIME = NULL;
  THD_3dim_dataset *inset0 = NULL;
  THD_3dim_dataset *outsetREHO=NULL;
  THD_3dim_dataset *MASK=NULL;
  THD_3dim_dataset *ROIS=NULL;
  char *prefix="REHO" ;
  int Nneigh=27,N_TYPE=0;
  char in_name[300];
  char in_name0[300];
  char out_pref[300];
  char in_mask[300];
  char in_rois[300];
  char out_rois[300];

  FILE *fout0;
  float **INDEX; // hold ranks
  int ***mskd; // define mask of where time series are nonzero
  float *KW,*chisq; // value of Kendall's W and Friedman's chi-sq of it
  int *Nties; // record number of ties per voxel time course (as t^3-t)
  int CHI_ON = 0; // default to not output chi-sq
  int HAVE_MASK=0;
  int HAVE_ROIS=0;
  float **ROI_KW=NULL,**ROI_chisq=NULL; // if mask of ROIs is input
  int *NROI_REF=NULL,*INVROI_REF=NULL;
  int **ROI_LABELS_REF=NULL, **INV_LABELS_REF=NULL,**ROI_COUNT=NULL;
  int *LIST_OF_NEIGHS=NULL; // hold list of neighbors for indiv vox.
  int ***ROI_LISTS=NULL;

  int Nvox=-1;   // tot number vox
  int *Dim=NULL;
  int *here=NULL;
  int *ndof=NULL;

  int idx;
  FILE *fout1;

  mainENTRY("3dReHo"); machdep(); 
  
  // ****************************************************************
  // ****************************************************************
  //                    load AFNI stuff
  // ****************************************************************
  // ****************************************************************

  // INFO_message("version: LAMBDA");
	
  /** scan args **/
  if (argc == 1) { usage_ReHo(1); exit(0); }
  iarg = 1; 
  while( iarg < argc && argv[iarg][0] == '-' ){
    if( strcmp(argv[iarg],"-help") == 0 || 
        strcmp(argv[iarg],"-h") == 0 ) {
      usage_ReHo(strlen(argv[iarg])>3 ? 2:1);
      exit(0);
    }
		
    if( strcmp(argv[iarg],"-chi_sq") == 0) {
      CHI_ON=1;
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
	 
    if( strcmp(argv[iarg],"-inset") == 0 ){
      iarg++ ; if( iarg >= argc ) 
                 ERROR_exit("Need argument after '-input'");

      sprintf(in_name,"%s", argv[iarg]); 
      insetTIME = THD_open_dataset(in_name) ;
      if( (insetTIME == NULL ))
        ERROR_exit("Can't open time series dataset '%s'.",in_name);
      // just 0th time point for output...
      sprintf(in_name0,"%s[0]", argv[iarg]); 
      inset0 = THD_open_dataset(in_name0) ;
      if( (inset0 == NULL ))
        ERROR_exit("Can't open 0th brick of dataset as '%s[0]'.",in_name0);

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
      HAVE_MASK=1;

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

    if( strcmp(argv[iarg],"-nneigh") == 0 ){
      iarg++ ; if( iarg >= argc ) 
                 ERROR_exit("Need argument after '-nneigh'");
			
      //INFO_message("Size of neighborhood is: %s",argv[iarg]);
      Nneigh = atoi(argv[iarg]);
      if( Nneigh == 27 ) 
        N_TYPE = 0;
      else if( Nneigh == 19 )
        N_TYPE = 1;
      else if( Nneigh == 7 )
        N_TYPE = 2;
      else 
        ERROR_exit("Illegal after '-nneigh': need '27', '19' or '7'");
      iarg++ ; continue ;
    }
		
    ERROR_message("Bad option '%s'\n",argv[iarg]) ;
    suggest_best_prog_option(argv[0], argv[iarg]);
    exit(1);
  }
	
  if (iarg < 3) {
    ERROR_message("Too few options. Try -help for details.\n");
    exit(1);
  }
	
  if( (HAVE_ROIS>0) && (Nvox != DSET_NVOX(ROIS)) ) {
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
	
  LIST_OF_NEIGHS = (int *)calloc(27,sizeof(int)); // max of 27 per vox

  KW = (float *)calloc(Nvox,sizeof(float)); 
  chisq = (float *)calloc(Nvox,sizeof(float)); 
  here = (int *)calloc(3,sizeof(int)); 
  ndof = (int *)calloc(1,sizeof(int)); 

  mskd = (int ***) calloc( Dim[0], sizeof(int **) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    for ( j = 0 ; j < Dim[1] ; j++ ) 
      mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );

  Nties = (int *)calloc(Nvox,sizeof(int)); 

  // hold ranks
  INDEX = calloc( Nvox,sizeof(INDEX));  
  for(i=0 ; i<Nvox ; i++) 
    INDEX[i] = calloc(Dim[3],sizeof(float)); 
	
  // this statement will never be executed if allocation fails above
  if( (INDEX == NULL) || (mskd == NULL) || (KW == NULL) 
      || (here == NULL) || (Nties == NULL) || (ndof == NULL)
      || (LIST_OF_NEIGHS == NULL) ) { 
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
     
    ROI_KW = calloc(HAVE_ROIS,sizeof(ROI_KW)); 
    for(i=0 ; i<HAVE_ROIS ; i++) 
      ROI_KW[i] = calloc(NROI_REF[i],sizeof(float)); 
    ROI_chisq = calloc(HAVE_ROIS,sizeof(ROI_chisq)); 
    for(i=0 ; i<HAVE_ROIS ; i++) 
      ROI_chisq[i] = calloc(NROI_REF[i],sizeof(float)); 
    ROI_COUNT = calloc( HAVE_ROIS,sizeof(ROI_COUNT));  
    for(i=0 ; i<HAVE_ROIS ; i++) 
      ROI_COUNT[i] = calloc(NROI_REF[i],sizeof(int)); 

    if( (ROI_KW == NULL) || (ROI_COUNT == NULL) || (ROI_chisq == NULL)) {
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
              ROI_COUNT[m][INV_LABELS_REF[m][(int) THD_get_voxel(ROIS,idx,m)]-1]++;
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
    if( (ROI_LISTS == NULL) ) {
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
	
  // calculate ranks
  i = CalcRanksForReHo(INDEX,insetTIME,Nties,mskd,Dim);

  idx = 0;
  // calculate KendallW for each voxel
  for( k=0 ; k<Dim[2] ; k++ ) 
    for( j=0 ; j<Dim[1] ; j++ ) 
      for( i=0 ; i<Dim[0] ; i++ ) {
        if(mskd[i][j][k]) {
          here[0] = i; here[1] = j; here[2] = k; 
          m = FindVoxHood(LIST_OF_NEIGHS,here,Dim,mskd,N_TYPE,ndof);
          KW[idx] = ReHoIt(LIST_OF_NEIGHS,INDEX,Nties,Dim,
                           mskd,ndof);
          chisq[idx] = ndof[0]*(Dim[3]-1)* KW[idx];
        }
        idx+=1;
      }

  // ROI values
  if(HAVE_ROIS>0) {
		
    for(i=0 ; i<HAVE_ROIS ; i++) 
      for( j=0 ; j<NROI_REF[i] ; j++ ) {
        ndof[0]=ROI_COUNT[i][j];
        ROI_KW[i][j] = ReHoIt(ROI_LISTS[i][j],INDEX,Nties,Dim,
                              mskd,ndof);
        ROI_chisq[i][j] = ndof[0]*(Dim[3]-1)* ROI_KW[i][j];
      }
  }
	
  // **************************************************************
  // **************************************************************
  //                 Store and output
  // **************************************************************
  // **************************************************************
	
  outsetREHO = EDIT_empty_copy( inset0 ) ; 
  if(CHI_ON)
    EDIT_add_bricklist(outsetREHO,
                       1, NULL , NULL , NULL );
  EDIT_dset_items( outsetREHO,
                   ADN_datum_all , MRI_float , 
                   ADN_prefix    , prefix ,
                   ADN_none ) ;
  if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetREHO)) )
    ERROR_exit("Can't overwrite existing dataset '%s'",
               DSET_HEADNAME(outsetREHO));
  EDIT_substitute_brick(outsetREHO, 0, MRI_float, KW); 
  KW=NULL;
  if(CHI_ON)
    EDIT_substitute_brick(outsetREHO, 1, MRI_float, chisq); 
  chisq=NULL;
  EDIT_BRICK_LABEL(outsetREHO,0,"ReHo");      
  if(CHI_ON) 
    EDIT_BRICK_LABEL(outsetREHO,1,"frchisq");      
  THD_load_statistics(outsetREHO);
  tross_Make_History("3dReHo", argc, argv, outsetREHO);
  THD_write_3dim_dataset(NULL, NULL, outsetREHO, True);

  if(HAVE_ROIS>0) {
    sprintf(out_rois,"%s_ROI_reho.vals", prefix); 
    if( (fout1 = fopen(out_rois, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s.",out_rois);
      exit(19);
    }
    for(i=0 ; i<HAVE_ROIS ; i++) {
      for( j=0 ; j<NROI_REF[i] ; j++ ) 
        fprintf(fout1,"%.4f\t",ROI_KW[i][j]);
      fprintf(fout1,"\n");
    }
    fclose(fout1);    

    if(CHI_ON){
      sprintf(out_rois,"%s_ROI_reho.chi", prefix); 
      if( (fout1 = fopen(out_rois, "w")) == NULL) {
        fprintf(stderr, "Error opening file %s.",out_rois);
        exit(19);
      }
      for(i=0 ; i<HAVE_ROIS ; i++) {
        for( j=0 ; j<NROI_REF[i] ; j++ ) 
          fprintf(fout1,"%.4f\t",ROI_chisq[i][j]);
        fprintf(fout1,"\n");
      }
      fclose(fout1);    
    }
  }

  INFO_message("ReHo (Kendall's W) calculated.");
  if(CHI_ON)
    INFO_message("Friedman chi-sq of W calculated (%d deg of freedom per vox)",Dim[3]-1);

  // ************************************************************
  // ************************************************************
  //                    Freeing
  // ************************************************************
  // ************************************************************
	
  DSET_delete(insetTIME);
  DSET_delete(inset0);
  DSET_delete(outsetREHO);
  DSET_delete(MASK);
  DSET_delete(ROIS);
  free(KW); 
  free(chisq);
  free(here);
  free(MASK);
  free(ROIS);
	
  for( i=0 ; i<Dim[0] ; i++) 
    for( j=0 ; j<Dim[1] ; j++) {
      free(mskd[i][j]);
    }
  for( i=0 ; i<Dim[0] ; i++) {
    free(mskd[i]);
  }
  for( i=0 ; i<Nvox ; i++) 
    free(INDEX[i]);
	
  free(INDEX);
  free(mskd);
  free(Nties);
  free(ndof);

  free(Dim); // need to free last because it's used for other arrays...
  free(insetTIME);
  free(inset0);
  free(outsetREHO);
  free(prefix);
  free(LIST_OF_NEIGHS);

  if(HAVE_ROIS >0) {
		
    for( i=0 ; i<HAVE_ROIS ; i++) {
      for( j=0 ; j<NROI_REF[i] ; j++) 
        free(ROI_LISTS[i][j]);
      free(ROI_LISTS[i]);
      free(ROI_KW[i]);
      free(ROI_chisq[i]);
      free(ROI_LABELS_REF[i]);
      free(INV_LABELS_REF[i]);
      free(ROI_COUNT[i]);
    }
    free(ROI_LISTS);
    free(ROI_KW);
    free(ROI_chisq);
    free(ROI_LABELS_REF);
    free(INV_LABELS_REF);
    free(ROI_COUNT);
    free(NROI_REF);
    free(INVROI_REF);
  }
	
  return 0;
}
