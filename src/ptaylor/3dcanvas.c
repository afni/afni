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
//#include "DoTrackit.h"

void usage_FUNCNAME(int detail) 
{
  printf(
"\n"
"  *** \n"
"  *** \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  \n"
"  + USAGE: ***\n"
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
  // int i,j,k,m,n,mm;
  // int idx;
  // int iarg;
  // THD_3dim_dataset *insetTIME = NULL;
  // THD_3dim_dataset *inset0 = NULL;
  // THD_3dim_dataset *MASK=NULL;
  // char *prefix="REHO" ;
  // char in_name[300];
  // char in_mask[300];

  // FILE *fout0, *fout1;
  // int Nvox=-1;   // tot number vox
  // int *Dim=NULL;

  mainENTRY("3dFUNCNAME"); machdep(); 
  
  // ****************************************************************
  // ****************************************************************
  //                    load AFNI stuff
  // ****************************************************************
  // ****************************************************************

  // INFO_message("version: NU");
	
  /** scan args **/
  if (argc == 1) { usage_FUNCNAME(1); exit(0); }
  iarg = 1; 
  while( iarg < argc && argv[iarg][0] == '-' ){
    if( strcmp(argv[iarg],"-help") == 0 || 
        strcmp(argv[iarg],"-h") == 0 ) {
      usage_FUNCNAME(strlen(argv[iarg])>3 ? 2:1);
      exit(0);
    }
		
    // NO ARG:
    if( strcmp(argv[iarg],"-my_opt") == 0) {
       // CHI_ON=1;
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
      //sprintf(in_name0,"%s[0]", argv[iarg]); 
      //inset0 = THD_open_dataset(in_name0) ;
      //if( (inset0 == NULL ))
      //  ERROR_exit("Can't open 0th brick of dataset as '%s[0]'.",in_name0);

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

    //  EXAMPLE: option with numerical input: ATOF
    /*if( strcmp(argv[iarg],"-neigh_Y") == 0 ){
      iarg++ ; if( iarg >= argc ) 
                 ERROR_exit("Need argument after '-nneigh'");
      
      NEIGH_R[1] = atof(argv[iarg]);

      iarg++ ; continue ;
      }*/


    ERROR_message("Bad option '%s'\n",argv[iarg]) ;
    suggest_best_prog_option(argv[0], argv[iarg]);
    exit(1);
  }
	

  // TEST BASIC INPUT PROPERTIES
  if (iarg < 3) {
    ERROR_message("Too few options. Try -help for details.\n");
    exit(1);
  }
	
	
  // ****************************************************************
  // ****************************************************************
  //                    pre-stuff, make storage
  // ****************************************************************
  // ****************************************************************


  // *************************************************************
  // *************************************************************
  //                    Beginning of main loops
  // *************************************************************
  // *************************************************************
	

  // go through once: define data vox
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


  // **************************************************************
  // **************************************************************
  //                 Store and output
  // **************************************************************
  // **************************************************************


  // ************************************************************
  // ************************************************************
  //                    Freeing
  // ************************************************************
  // ************************************************************
	
  // insetTIME gets freed earlier to save some space

  DSET_delete(inset0);
  free(MASK);
	
  return 0;
}
