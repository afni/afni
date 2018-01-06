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


int check_make_rai( THD_3dim_dataset *A, 
                    char *dset_or );






void usage_IntraVolMot(int detail) 
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
   byte ***mskd=NULL; // define mask of where time series are nonzero
   byte *mskd2=NULL; // not great, but another format of mask

   int TEST_OK = 0;
   double checksum = 0.;

   int *Nmk=NULL;                            // num of vox in slice mask
   int MIN_NMK = -1;                         // calc how many vox/sli
                                             // are needed for calc
   int maxk = -1, upk = -1, delsli = -1;     // some loop pars
   
   float **diffarr=NULL;
   THD_3dim_dataset *diffdset=NULL;          // output dset of diffs

	char dset_or[4] = "RAI";
	THD_3dim_dataset *dsetn=NULL;

   mainENTRY("3dIntraVolMot"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   INFO_message("version: 2018_01_05");
	
   /** scan args **/
   if (argc == 1) { usage_IntraVolMot(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_IntraVolMot(strlen(argv[iarg])>3 ? 2:1);
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

   mskd = (byte ***) calloc( Dim[0], sizeof(byte **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (byte **) calloc( Dim[1], sizeof(byte *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (byte *) calloc( Dim[2], sizeof(byte) );

   // Store number of voxels in axial slice mask
   Nmk = (int *)calloc(Dim[2], sizeof(int)); 
   mskd2 = (byte *)calloc(Nvox,sizeof(byte)); 

   if( (mskd == NULL) || (Nmk == NULL) || (mskd2 == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (masks).\n\n");
      exit(12);
   }

   MIN_NMK = (int) (0.1 * Dim[0]*Dim[1]); // min num of vox in mask per sli
   if( MIN_NMK <= 0 )
      ERROR_exit("Min num of vox per slice was somehow %d?? No thanks!", 
                 MIN_NMK);

   // array for diffs; later, maybe just do one by one!
   diffarr = calloc( Dim[3], sizeof(diffarr) );
   for(i=0 ; i<Dim[3] ; i++) 
      diffarr[i] = calloc( Nvox, sizeof(float) ); 

   if( (diffarr == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (arrs).\n\n");
      exit(13);
   }


   // ===================== resamp, if nec =======================

   // !!! lazy way-- make function different later...
   
   if (check_make_rai( insetA, dset_or ) ) {
      dsetn = r_new_resam_dset( insetA, NULL, 0.0, 0.0, 0.0,
                                dset_or, RESAM_NN_TYPE, NULL, 1, 0);
      DSET_delete(insetA); 
      insetA=dsetn;
      dsetn=NULL;
   }

   if( MASK )
      if (check_make_rai( MASK, dset_or ) ) {
         dsetn = r_new_resam_dset( MASK, NULL, 0.0, 0.0, 0.0,
                                   dset_or, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(MASK); 
         MASK=dsetn;
         dsetn=NULL;
      }
   
   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************
	
   INFO_message("Masking and counting.");

   // go through once: define data vox
   idx = 0;
   for( k=0 ; k<Dim[2] ; k++ )  
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( MASK ) {
               if( THD_get_voxel(MASK,idx,0)>0 ) {
                  mskd[i][j][k] = 1;
                  mskd2[idx] = 1;
                  Nmk[k]+=1;
               }
            }
            else{
               checksum = 0.;
               for( m=0 ; m<Dim[3] ; m++ ) 
                  checksum+= fabs(THD_get_voxel(insetA,idx,m)); 
               if( checksum > EPS_V ) {
                  mskd[i][j][k] = 1;
                  mskd2[idx] = 1;
                  Nmk[k]+=1;
               }
            }
            idx+= 1; 
         }
   
   // will only look at slices with "enough" vox.  Also, ignore top
   // level, because we can't take diff above it.
   for( k=0 ; k<Dim[2] ; k++ ) 
      if( Nmk[k] < MIN_NMK )
         Nmk[k] = 0;
   Nmk[Dim[2]-1]=0; 

   // **************************************************************
   // **************************************************************
   //                 Calculate stuff
   // **************************************************************
   // **************************************************************

   INFO_message("Start processing.");
   
   // Calc (scaled) diff values with single pass through.  We don't
   // use the Nmk[k] check initially, because we want idx to still get
   // bigger.
   idx = 0;
   delsli = Dim[0]*Dim[1];
   maxk = Dim[2] - 1;
   for( k=0 ; k<maxk ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( mskd2[idx] && Nmk[k] ) {
               if( mskd[i][j][k+1] ) {
                  upk = idx + delsli; // index one slice up
                  for( m=0 ; m<Dim[3] ; m++ ) {
                     diffarr[m][idx] = 0.5*(THD_get_voxel(insetA, idx, m) -
                                            THD_get_voxel(insetA, upk, m) );
                     diffarr[m][idx]/= fabs(THD_get_voxel(insetA, idx, m)) +
                        fabs(THD_get_voxel(insetA, upk, m)) +
                        0.0000001;
                  }
               }
            }
            idx+=1;
         }
   
   // **************************************************************
   // **************************************************************
   //                 Write stuff
   // **************************************************************
   // **************************************************************

   if ( 1 ) {
      INFO_message("Preparing output of diffs.");
      
      diffdset = EDIT_empty_copy( insetA );

      EDIT_dset_items( diffdset,
                       ADN_prefix    , prefix,
                       ADN_datum_all , MRI_float,
                       ADN_brick_fac , NULL,
                       ADN_nvals     , Dim[3],
                       ADN_none );

      for( m=0 ; m<Dim[3] ; m++) {
         EDIT_substitute_brick(diffdset, m, MRI_float, diffarr[m]);
         diffarr[m]=NULL; // to not get into trouble...
         free(diffarr[m]);
      }

      THD_load_statistics( diffdset );
      tross_Copy_History( insetA, diffdset );
      tross_Make_History( "3dIntraVolMot", argc, argv, diffdset );
      if( !THD_ok_overwrite() && 
          THD_is_ondisk(DSET_HEADNAME(diffdset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(diffdset));
      THD_write_3dim_dataset(NULL, NULL, diffdset, True);
      INFO_message("Wrote dataset: %s\n", DSET_BRIKNAME(diffdset));
   }


   // ************************************************************
   // ************************************************************
   //                    Free dsets, arrays, etc.
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
   if(mskd2)
      free(mskd2);

   if(diffarr){
      for( i=0 ; i<Dim[3] ; i++) 
         free(diffarr[i]);
      free(diffarr);
   }

   free(Nmk);

   if(prefix)
      free(prefix);

   if(Dim)
      free(Dim);
	
   return 0;
}




int check_make_rai( THD_3dim_dataset *A, 
                    char *dset_or )
{

   if( ORIENT_typestr[A->daxes->xxorient][0] != dset_or[0] )
      return 1;
   if( ORIENT_typestr[A->daxes->yyorient][0] != dset_or[1] )
      return 1;
   if( ORIENT_typestr[A->daxes->zzorient][0] != dset_or[2] )
      return 1;

   INFO_message("No need to resample.");
   return 0;
}



