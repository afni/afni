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
   // THD_3dim_dataset *MASK=NULL;
   // char *prefix="REHO" ;
   // char in_name[300];
   // char in_mask[300];

   // FILE *fout0, *fout1;
   // int Nvox=-1;   // tot number vox
   // int *Dim=NULL;

   int TEST_OK = 0;

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

         sprintf(in_name,"%s", argv[iarg]); 
         insetTIMEA = THD_open_dataset(in_name) ;
         if( (insetTIMEA == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",in_name);

         Dim = (int *)calloc(4,sizeof(int));
         DSET_load(insetTIMEA); CHECK_LOAD_ERROR(insetTIMEA);
         Nvox = DSET_NVOX(insetTIMEA) ;
         Dim[0] = DSET_NX(insetTIMEA); Dim[1] = DSET_NY(insetTIMEA); 
         Dim[2] = DSET_NZ(insetTIMEA); Dim[3] = DSET_NVALS(insetTIMEA); 

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
	
   if( !TEST_OK ) {
      ERROR_message("HEY! Just testing/building mode right now!\n");
      exit(5);
   }

   if 

	
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
               if( fabs(THD_get_voxel(insetTIMEA,idx,0))+
                   fabs(THD_get_voxel(insetTIMEA,idx,1))+
                   fabs(THD_get_voxel(insetTIMEA,idx,2))+
                   fabs(THD_get_voxel(insetTIMEA,idx,3))+
                   fabs(THD_get_voxel(insetTIMEA,idx,4)) > EPS_V)
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
	
   if(insetTIMEA)
      free(insetTIMEA);

   if( MASK )
      free(MASK);
	
   return 0;
}
