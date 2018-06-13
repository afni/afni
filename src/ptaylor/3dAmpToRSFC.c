/* 
   Description

   [PT: Sept 26, 2014] Add in attribute output for Ntpts pre- and
   post-censoring.


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
                    int DTYPE,
                    int *Dim,
                    int ***mskd,
                    int MIN_bp, int MAX_bp, 
                    int MIN_full, int MAX_full,
                    float **ap,
                    int Npar,
                    int nt_cen, int nt_orig);


void usage_AmpToRSFC(int detail) 
{
   printf(
"\n"
"  This program is for converting spectral amplitudes into standard RSFC\n"
"  parameters.  This function is made to work directly with the outputs of\n"
"  3dLombScargle, but you could use other inputs that have similar \n"
"  formatting. (3dLombScargle's main algorithm is special because it\n"
"  calculates spectra from time series with nonconstant sampling, such as if\n"
"  some time points have been censored during processing-- check it out!.)\n"
"\n"
"  At present, 6 RSFC parameters get returned in separate volumes:\n"
"     ALFF, mALFF, fALFF, RSFA, mRSFA and fRSFA.\n"
"  For more information about each RSFC parameter, see, e.g.:   \n"
"     ALFF/mALFF -- Zang et al. (2007),\n"
"     fALFF --      Zou et al. (2008),\n"
"     RSFA --       Kannurpatti & Biswal (2008).\n"
"  You can also see the help of 3dRSFC, as well as the Appendix of \n"
"  Taylor, Gohel, Di, Walter and Biswal (2012) for a mathematical\n"
"  description and set of relations.\n"
"\n"
"  NB: *if* you want to input an unbandpassed time series and do some\n"
"  filtering/other processing at the same time as estimating RSFC parameters,\n"
"  then you would want to use 3dRSFC, instead.\n" 
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND: \n"
"        3dAmpToRSFC { -in_amp AMPS | -in_pow POWS } -prefix PREFIX \\\n"
"            -band FBOT FTOP  { -mask MASK } { -nifti }\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + RUNNING:\n"
"\n"
"   -in_amp AMPS   :input file of one-sided spectral amplitudes, such as\n"
"                    output by 3dLombScargle.  It is also assumed that the\n"
"                    the frequencies are uniformly spaced with a single DF\n"
"                    ('delta f'), and that the zeroth brick is at 1*DF (i.e.\n"
"                    that the zeroth/baseline frequency is not present in the\n"
"         or         spectrum.\n"
"   -in_pow POWS    :input file of a one-sided power spectrum, such as\n"
"                    output by 3dLombScargle.  Similar freq assumptions\n"
"                    as in '-in_amp ...'.\n"
"\n"
"   -band FBOT FTOP :lower and upper boundaries, respectively, of the low\n"
"                    frequency fluctuations (LFFs), which will be in the\n"
"                    inclusive interval [FBOT, FTOP], within the provided\n"
"                    input file's frequency range.\n" 
"   -prefix PREFIX  :output file prefix; file names will be: PREFIX_ALFF*,\n"
"                    PREFIX_FALFF*, etc.\n"
"\n"
"   -mask MASK      :volume mask of voxels to include for calculations; if\n"
"                    no mask is included, values are calculated for voxels\n"
"                    whose values are not identically zero across time.\n"
"   -nifti          :output files as *.nii.gz (default is BRIK/HEAD).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  + OUTPUT: \n"
"       Currently, 6 volumes of common RSFC parameters, briefly:\n"
"          PREFIX_ALFF+orig    :amplitude of low freq fluctuations\n"
"                               (L1 sum).\n"
"          PREFIX_MALFF+orig   :ALFF divided by the mean value within\n"
"                               the input/estimated whole brain mask\n"
"                               (a.k.a. 'mean-scaled ALFF').\n"
"          PREFIX_FALFF+orig   :ALFF divided by sum of full amplitude\n"
"                               spectrum (-> 'fractional ALFF').\n"
"          PREFIX_RSFA+orig    :square-root of summed square of low freq\n"
"                               fluctuations (L2 sum).\n"
"          PREFIX_MRSFA+orig   :RSFA divided by the mean value within\n"
"                               the input/estimated whole brain mask\n"
"                               (a.k.a. 'mean-scaled RSFA').\n"
"          PREFIX_FRSFA+orig   :ALFF divided by sum of full amplitude\n"
"                               spectrum (a.k.a. 'fractional RSFA').\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"        3dAmpToRSFC                         \\\n"
"            -in_amp SUBJ_01_amp.nii.gz     \\\n"
"            -prefix  SUBJ_01                \\\n"
"            -mask    mask_WB.nii.gz         \\\n"
"            -band    0.01  0.1              \\\n"
"            -nifti \n"
"\n"
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
   int DTYPE=0;

   int nt_orig = -1;  // attribute info on Npts before censoring
   int nt_cen  = -1;  // attribute info on Npts after censoring
   
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
	 
      if( strcmp(argv[iarg],"-in_amp") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_amp'");

         sprintf(in_name,"%s", argv[iarg]); 
         DTYPE = 1; // for amps

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-in_pow") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_pow'");
         
         sprintf(in_name,"%s", argv[iarg]); 
         DTYPE = 2; // for pow
         
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

   if( !DTYPE ) {
      ERROR_message("Think somebody forgot to specify an input file"
                    " using '-in_amp ...' or '-in_pow ...'.");
      exit(12);
   }
   else{
         insetTIME = THD_open_dataset(in_name) ;
         if( (insetTIME == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",in_name);
         
         DSET_load(insetTIME); CHECK_LOAD_ERROR(insetTIME);

         Nvox = DSET_NVOX(insetTIME) ;
         Dim[0] = DSET_NX(insetTIME); Dim[1] = DSET_NY(insetTIME); 
         Dim[2] = DSET_NZ(insetTIME); Dim[3]= DSET_NVALS(insetTIME); 
         delF = DSET_TR(insetTIME);

         // [PT: Sep 20, 2017] Get attribute info: follow Bob's lead!
         ATR_int *atr;
         int *attin;
         atr = THD_find_int_atr( insetTIME->dblk , "N_TS_ORIG" ) ;
         if( atr == NULL )
            ERROR_exit("Input pow/amp set has no N_TS_ORIG attribute-- booo.");
         if( atr->nin != 2 )
            ERROR_exit("Input pow/amp attribute N_TS_ORIG is too short [%d]!",
                       atr->nin);
         attin = atr->in ;
         nt_orig = attin[0];
         nt_cen  = attin[1];
         INFO_message("Original time series:  %d points before censoring, "
                      "and %d after", nt_orig, nt_cen);

         if( (nt_orig <= 0) || (nt_cen <= 0) )
            ERROR_exit("Can't have non-positive numbers of original time "
                       "series points, even *after* censoring!");
   }

   if( (fbot<0) || (ftop<0) ) {
      ERROR_message("Think somebody forgot to specify upper and lower"
                    " frequency bounds using '-band ... ...'.");
      exit(11);
   }
   if( fbot > ftop )
      ERROR_exit("Can't have ftop < fbot! Try entering frequency"
                    "band limits again");
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
                "freqs [%.4f, %.4f]", MIN_bp, MAX_bp, 
                allF[MIN_bp], allF[MAX_bp]);
   INFO_message("Full freq range: indices [%d, %d] -> "
                "freqs [%.4f, %.4f]", MIN_full, MAX_full, 
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
                  DTYPE,
                  Dim,
                  mskd,
                  MIN_bp, MAX_bp, 
                  MIN_full, MAX_full,
                  allPar,
                  Npar,
                  nt_cen, nt_orig
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
                    int DTYPE,
                    int *Dim,
                    int ***mskd,
                    int MIN_bp, int MAX_bp, 
                    int MIN_full, int MAX_full,
                    float **ap,
                    int Npar,
                    int nt_cen, int nt_orig
                    )
{
   int i,j,k,l;
   int idx=0, ctr=0;
   float L1num=0., L2num=0., L1den=0., L2den=0.;
   float tmp1, mean_alff=0., mean_rsfa=0.;
   //float facN, facNNmin1;
   float facL, facLMmin1, fac2oL;

   INFO_message("Start calculating spectral parameters");

   // scaling factors based on 'N'; think this is correct and even
   // accounts for the possible use of ofac!=1 in the lombscargle
   // program -> !! check !!
   facL = sqrt(Dim[3]); // essentially sqrt(L)
   //fac2oL = sqrt(2./Dim[3]); // essentially sqrt(L)
   facLMmin1 = facL * sqrt(nt_cen-1.); // L*(M-1)

   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( mskd[i][j][k] ) {
               
               L1den=0.;
               L2den=0.;
               for( l=MIN_full ; l<=MAX_full ; l++ ) {
                  tmp1 = THD_get_voxel(A,idx,l); 
                  if(DTYPE==2)             // 1pow -> 1amp
                     tmp1 = sqrt(tmp1);
                  L1den+= tmp1;
                  L2den+= tmp1*tmp1;
                  if( (MIN_bp <= l) && (l <= MAX_bp) ) {
                     ap[0][idx]+= tmp1;         // alff
                     ap[3][idx]+= tmp1*tmp1;    // rsfa
                     
                  }
               }

               // !! Don't need these, because have L=N/2
               // one-sidedness -> full values; each sum is only over
               // half the freqs
               //ap[0][idx]*= 2.;
               //L1den*= 2.;
               //ap[3][idx]*= 2.;
               //L2den*= 2.;

               // now the rest of the pars
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
               // fALFF and fRSFA need no further scaling.
               ap[0][idx]/= (facL*facL); // [PT: JUn 12 2018]: have rescaled
                                  // AMPS-> don't need/want the
                                  // 'Mmin1' here now;Mmin1;
                                  // //sqrt(nt_cen-1); // ALFF-- don't
                                  // need sqrt(2) ! normalize
                                  // properly, I think
               ap[1][idx]/= mean_alff;   // mALFF
               ap[3][idx]/= facL * sqrt(Dim[3]-1); // [PT: JUn 12 2018]: have rescaled
                                  // AMPS-> don't need/want the
                                  // 'Mmin1' here now; // RSFA
               ap[4][idx]/= mean_rsfa;   // mRSFA
            }
            idx++;
         }
   
  
}
