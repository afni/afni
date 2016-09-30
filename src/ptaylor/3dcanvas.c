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
#include "DoTrackit.h"

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
   int i,j,k,m,n,mm;
   int idx=0;
   int iarg;

   THD_3dim_dataset *insetA = NULL;
   THD_3dim_dataset *MASK=NULL;
   char *prefix="PREFIX" ;
   // char in_name[300];

   // FILE *fout0, *fout1;

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;
   int ***mskd=NULL; // define mask of where time series are nonzero

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

         insetA = THD_open_dataset(argv[iarg]);
         if( (insetA == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

         Dim = (int *)calloc(4,sizeof(int));
         DSET_load(insetA); CHECK_LOAD_ERROR(insetA);
         Nvox = DSET_NVOX(insetA) ;
         Dim[0] = DSET_NX(insetA); Dim[1] = DSET_NY(insetA); 
         Dim[2] = DSET_NZ(insetA); Dim[3] = DSET_NVALS(insetA); 

         iarg++ ; continue ;
      }


      if( strcmp(argv[iarg],"-mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");

         MASK = THD_open_dataset(argv[iarg]);
         if( MASK == NULL )
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

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

   

	
   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   mskd = (int ***) calloc( Dim[0], sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );



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
            if( MASK ) {
               if( THD_get_voxel(MASK,idx,0)>0 )
                  mskd[i][j][k] = 1;
            }
            else
               if( fabs(THD_get_voxel(insetA,idx,0))+
                   fabs(THD_get_voxel(insetA,idx,1))+
                   fabs(THD_get_voxel(insetA,idx,2))+
                   fabs(THD_get_voxel(insetA,idx,3))+
                   fabs(THD_get_voxel(insetA,idx,4)) > EPS_V)
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
	
   if(insetA){
      DSET_delete(insetA);
      free(insetA);
   }

   if( MASK ) {
      DSET_delete(MASK);
      free(MASK);
   }
      

   if(mskd) {
      for( i=0 ; i<Dim[0] ; i++) 
         for( j=0 ; j<Dim[1] ; j++) {
            free(mskd[i][j]);
         }
      for( i=0 ; i<Dim[0] ; i++) {
         free(mskd[i]);
      }
      free(mskd);
   }

   if(prefix)
      free(prefix);

   if(Dim)
      free(Dim);
	
   return 0;
}
