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

#define NRSFC (6) // ALFF, mALFF, fALFF, 
                  // RSFA, mRSFA, fRSFA


void Spect_to_RSFC( THD_3dim_dataset *A,
                    int *Dim,
                    int ***mskd,
                    int MIN_bp, int MAX_bp, 
                    int MIN_full, int MAX_full,
                    float **ap,
                    int Npar );


void usage_AmpToRSFC(int detail) 
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
"\n"
"-in_amps\n"
"-prefix\n"
"-band\n"
"\n"
"-mask\n"
"-nifti\n"
"\n"
"\n"
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
"___________________________________________________________________________\n"
          );
	return;
}

int main(int argc, char *argv[]) {
   int i,j,k,l,m,n,mm,ii;
   int idx;
   int iarg;
   THD_3dim_dataset *insetTIME = NULL;
   // THD_3dim_dataset *inset0 = NULL;
   THD_3dim_dataset *MASK=NULL;
   char *prefix="REHO" ;
   char in_name[300];
   char in_mask[300];
   
   THD_3dim_dataset *outset=NULL;
   char outname[300];

   int NIFTI_OUT=0;

   int HAVE_MASK = 0;
   int ***mskd; // define mask of where time series are nonzero
   double temp_sum;

   // FILE *fout0, *fout1;
   int Nvox=-1;   // tot number vox
   int Dim[4]={0,0,0,0};
   
   float fbot = -1., ftop = -1;
   float delF = -1;
   float *allF=NULL;

   float **allPar=NULL;
   int Npar=NRSFC;   // currently... see list below
   char *namePar[NRSFC]={"ALFF", "MALFF", "FALFF",
                         "RSFA", "MRSFA", "FRSFA"};

   int MIN_full=0, MAX_full=-1; // indices of full spect
   int MIN_bp=0, MAX_bp = -1; // indices of lff/bp region

   mainENTRY("3dAmpToRSFC"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   // INFO_message("version: NU");
	
   /** scan args **/
   if (argc == 1) { usage_AmpToRSFC(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_AmpToRSFC(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      if( strncmp(argv[iarg],"-band",5) == 0 ){
         if( ++iarg >= argc-1 ) ERROR_exit("need 2 arguments after -band!") ;

         fbot = strtod(argv[iarg++],NULL) ;
         ftop = strtod(argv[iarg++],NULL) ;
         continue ;
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

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-prefix'");
         prefix = strdup(argv[iarg]) ;
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '-prefix'");
         iarg++ ; continue ;
      }
	 
      if( strcmp(argv[iarg],"-in_amps") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_amps'");

         sprintf(in_name,"%s", argv[iarg]); 
         insetTIME = THD_open_dataset(in_name) ;
         if( (insetTIME == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",in_name);
         // just 0th time point for output...
         //sprintf(in_name0,"%s[0]", argv[iarg]); 
         //inset0 = THD_open_dataset(in_name0) ;
         //if( (inset0 == NULL ))
         //  ERROR_exit("Can't open 0th brick of dataset as '%s[0]'.",in_name0);

         DSET_load(insetTIME); CHECK_LOAD_ERROR(insetTIME);

         Nvox = DSET_NVOX(insetTIME) ;
         Dim[0] = DSET_NX(insetTIME); Dim[1] = DSET_NY(insetTIME); 
         Dim[2] = DSET_NZ(insetTIME); Dim[3]= DSET_NVALS(insetTIME); 
         delF = DSET_TR(insetTIME);

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

      if( strcmp(argv[iarg],"-nifti") == 0) {
         NIFTI_OUT=1;
         iarg++ ; continue ;
      }

      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
	
   // ---------------------------------------------------------------

   // TEST BASIC INPUT PROPERTIES
   if (iarg < 3) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }
	
   if( (fbot<0) || (ftop<0) ) {
      ERROR_message("Think somebody forgot to specify upper and lower"
                    " frequency bounds using '-band ... ...'.");
      exit(11);
   }
   if( fbot > ftop )
      ERROR_exit("Can't have ftop < fbot! Try entering frequency"
                    "band limits again");
   if( !insetTIME ) {
      ERROR_message("Think somebody forgot to specify an input file"
                    " using '-in_amps ...'.");
      exit(12);
   }
   if( MASK ) 
      if ( Dim[0] != DSET_NX(MASK) || Dim[1] != DSET_NY(MASK) ||
           Dim[2] != DSET_NZ(MASK) ) {
         ERROR_message("Mask and inset don't appear to have the same "
                       "dimensions.\n");
         exit(1);
      }
  

	
   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   // array of freqs-- starts at delta F, not zero, as the current
   // input data sets must!
   allF = (float *)calloc(Dim[3], sizeof(float));

   // will be the output
   allPar = calloc(Npar,sizeof(allPar)); 
   for(i=0 ; i<Npar ; i++) 
      allPar[i] = calloc(Nvox,sizeof(float)); 

   // MASK
   mskd = (int ***) calloc( Dim[0], sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );

   if( (mskd == NULL) || (allF == NULL) || (allPar == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure (mask).\n\n");
      exit(33);
   }


   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************

   // Populate freq bands. For now, delF is constant.  Later.... who
   // knows, so make flexible
   allF[0] = DSET_TIMEORIGIN(insetTIME);
   if( allF[0] < EPS_V )
      ERROR_exit("The t-axis (here, frequency) origin is 0!"
                 "\n\t-> but you shouldn't have a baseline 0-frequency!");
   for( i=1 ; i<Dim[3] ; i++ )
      allF[i] = allF[i-1] + delF;

   // fill in rest of freq ranges; MIN_full=0 already
   MAX_full = Dim[3]-1;
   // these should be in order, so we can pass through like this.
   for( i=0 ; i<Dim[3] ; i++ ) {
      ii = Dim[3] - 1 - i;
      if( allF[ii] >= fbot )
         MIN_bp = ii;
      if( allF[i] <= ftop )
         MAX_bp = i;
   }
   if(MAX_bp < MIN_bp) // shouldn't happen...
      ERROR_exit("Something went horribly wrong with reading in the "
                 "bandpass limits! bot:%f, top:%f",MIN_bp, MAX_bp);

   INFO_message("Actual BP range: indices [%d, %d] -> "
                "freqs [%.4f, %.4f", MIN_bp, MAX_bp, 
                allF[MIN_bp], allF[MAX_bp]);
   INFO_message("Full freq range: indices [%d, %d] -> "
                "freqs [%.4f, %.4f", MIN_full, MAX_full, 
                allF[MIN_full], allF[MAX_full]);
   
   // go through once: define data vox
   idx = 0;
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( HAVE_MASK ) {
               if( THD_get_voxel(MASK,idx,0)>0 )
                  mskd[i][j][k] = 1;
            }
            else {
               temp_sum = 0.;
               for ( l=0 ; l<Dim[3] ; l++ )
                  temp_sum+= abs(THD_get_voxel(insetTIME,idx,l));
               if ( temp_sum > EPS_V )
                  mskd[i][j][k] = 1;
            }
            idx++;
         }
   INFO_message("Done masking.");

   Spect_to_RSFC( insetTIME,
                  Dim,
                  mskd,
                  MIN_bp, MAX_bp, 
                  MIN_full, MAX_full,
                  allPar,
                  Npar
                  );

   INFO_message("Done calculating parameters.");

   // **************************************************************
   // **************************************************************
   //                 Store and output
   // **************************************************************
   // **************************************************************

   for( m=0; m<Npar ; m++) {
      outset = EDIT_empty_copy(insetTIME) ;
      if(NIFTI_OUT)
         sprintf(outname,"%s_%s.nii.gz",prefix, namePar[m]);
      else
         sprintf(outname,"%s_%s",prefix, namePar[m]);
      
      INFO_message(" writing: %s %s", prefix, outname);
      
      EDIT_dset_items( outset,
                       ADN_nvals     , 1 ,
                       ADN_datum_all , MRI_float , 
                       ADN_prefix    , outname ,
                       ADN_none ) ;
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(outset));
      EDIT_substitute_brick(outset, 0, MRI_float, allPar[m]); 
      allPar[m]=NULL;
      THD_load_statistics(outset);
      tross_Make_History("3dAmpToRSFC", argc, argv, outset);
      THD_write_3dim_dataset(NULL, NULL, outset, True);
      
      if(outset) {
         DSET_delete(outset);
         free(outset);
      }
   }



   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   if(allF)
      free(allF);

   if(MASK) {
      DSET_delete(MASK);
      free(MASK);
   }
   if(insetTIME) {
      DSET_delete(insetTIME);
      free(insetTIME);
   }
  
   if(mskd) {
      for( i=0 ; i<Dim[0] ; i++) 
         for( j=0 ; j<Dim[1] ; j++) 
            free(mskd[i][j]);
      for( i=0 ; i<Dim[0] ; i++) 
         free(mskd[i]);
      free(mskd);
   }



   if(allPar) { // have freed other parts of this above
      free(allPar);
   }


   return 0;
}






void Spect_to_RSFC( THD_3dim_dataset *A,
                    int *Dim,
                    int ***mskd,
                    int MIN_bp, int MAX_bp, 
                    int MIN_full, int MAX_full,
                    float **ap,
                    int Npar
                    )
{
   int i,j,k,l;
   int idx=0, ctr=0;
   float L1num=0., L2num=0., L1den=0., L2den=0.;
   float tmp1, mean_alff=0., mean_rsfa=0.;
   float facN, facNNmin1;

   INFO_message("Start calculating spectral parameters");

   // scaling factors based on 'N'; think this is correct and even
   // accounts for the possible use of ofac!=1 in the lombscargle
   // program -> !! check !!
   facN = sqrt(Dim[3]);
   facNNmin1 = facN * sqrt(Dim[3]-1);

   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( mskd[i][j][k] ) {
               
               L1den=0.;
               L2den=0.;
               for( l=MIN_full ; l<=MAX_full ; l++ ) {
                  tmp1 = THD_get_voxel(A,idx,l);
                  L1den+= tmp1;
                  L2den+= tmp1*tmp1;
                  if( (MIN_bp <= l) && (l <= MAX_bp) ) {
                     ap[0][idx]+= tmp1;         // alff
                     ap[3][idx]+= tmp1*tmp1;    // rsfa
                     
                  }
               }
               ap[1][idx] = ap[0][idx];         // -> malff
               ap[2][idx] = ap[0][idx] / L1den; // falff
               ap[3][idx] = sqrt(ap[3][idx]);
               ap[4][idx] = ap[3][idx];         // -> mrsfa
               ap[5][idx] = ap[3][idx] / sqrt(L2den); // frsfa
               
               mean_rsfa+= ap[3][idx];
               mean_alff+= ap[0][idx];
               ctr++;
            }
            idx++;
         }

   mean_alff/= ctr;
   mean_rsfa/= ctr;

   // loop back again for scaling mALFF and mRSFA
   idx = 0;
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( mskd[i][j][k] ) {
               ap[0][idx]/= facN;
               ap[1][idx]/= mean_alff;
               ap[3][idx]/= facNNmin1;
               ap[4][idx]/= mean_rsfa;
            }
            idx++;
         }
   
  
}
