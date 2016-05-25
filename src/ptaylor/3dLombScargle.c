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
#include "LS_funcs.h"
#include "DoTrackit.h"

void usage_LombScargle(int detail) 
{
   printf(
"\n"
"  Make a (normalized or non-normalized) periodogram or amplitude-spectrum\n"
"  of a time series that has a non-constant sampling rate.\n"
"\n"
"  Of particular interest is the application of this functionality to \n"
"  resting state time series that may have been censored.  The theory behind\n"
"  the mathematics and algorithms of this is due to separate groups, mainly\n"
"  in the realm of astrophysical applications: Vaníček (1969, 1971), \n"
"  Lomb (1976), Scargle (1982), and Press & Rybicki (1989). Shoutout to them.\n"
"\n"
"  This particular implementation is due to Press & Rybicki (1989), by\n"
"  essentially translating their published Fortran implementation into C,\n"
"  while using GSL for the FFT, instead of NR's realft(), and making\n"
"  adjustments based on that.\n"
"\n"
"  The adaption was done with fairly minimal changes here by PA Taylor (v1.2,\n"
"  May, 2016).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  \n"
"  + USAGE: \n"
"      Input a 4D volumetric time series (BRIK/HEAD or NIFTI data set)\n"
"      as well as an optional 1D file of 0s and 1s that defines which points\n"
"      to censor out (i.e., each 0 represents a point/volume to censor out);\n"
"      if no 1D file is input, the program will check for volumes that are\n"
"      uniformly zero and consider those to be censored.\n"
"\n"
"      The output is a LS periodogram, describing spectral magnitudes\n"
"      up to some 'maximum frequency'-- the default max here is what\n"
"      the Nyquist frequency of the time series *would have been* without\n"
"      any censoring.  (Interestingly, this analysis can actually be\n"
"      legitimately applied in cases to estimate frequency content >Nyquist.\n"
"      Wow!)\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  + OUTPUT: \n"
"      1) PREFIX_time.1D:  a 1D file of the sampled time points (in units of\n"
"                          seconds) of the analyzed (and possibly censored)\n"
"                          data set.\n"
"      2) PREFIX_freq.1D:  a 1D file of the frequency sample points (in units\n"
"                          of 1/seconds) of the output periodogram/spectrum\n"
"                          data set.\n"
"      3) PREFIX+orig:     volumetric data set containing a LS periodogram\n"
"                          (normalized magnitude spectrum), one per voxel;\n"
"                          you can also output the spectrum of amplitudes,\n"
"                          instead, if desired.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND:  3dLombScargle -prefix PREFIX -inset FILE {-in_censor1D CC}\\\n"
"                  {-mask MASK} {-do_no_normize} {-out_spectr_amp} \n"
"                  {-in_upsamp N1} {-in_mult_nyq N2}\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + RUNNING:\n"
"  -prefix PREFIX   :output prefix name for data volume, time point 1D file\n"
"                    and frequency 1D file.\n"
"  -inset FILE      :time series of volumes, a 4D volumetric data set.\n"
"\n"
"  -in_censor1D CC  :single row or column of 1s and 0s describing which\n"
"                    volumes of FILE are kept in the sampling or are censored\n"
"                    out, respectively. The list of numbers must be of the\n"
"                    same length as the number of volumes in FILE.\n"
"                    If not entered, all volumes are kept in sampling.\n"
"  -mask MASK       :optional, mask of volume to analyze; additionally, any\n"
"                    voxel with uniformly zero values across time will\n"
"                    produce a zero-spectrum.\n"
"\n"
"  -do_no_normize   :switch to output the non-variance-normalized periodogram\n"
"                    or amplitude spectrum (default is to normalize).\n"
"                    For a time series with variance V, a normalized\n"
"                    periodogram value Pn is related to a non-normalized\n"
"                    value P0 as:\n"
"                    Pn = P0/V.\n"
"  -out_spectr_amp  :switch to output the amplitude spectrum of the freqs\n"
"                    instead of the periodogram.  In the formulation used\n"
"                    here, for a time series of length N, the periodogram\n"
"                    value P is related to the amplitude value A as:\n"
"                    P = (A/2)**2.\n"
"       ---> You can both normalize and amplitude-ize the output values,\n"
"            if you wish. Or do neither. Or just do one of them. Your choice.\n"
"\n"
"  -in_mult_nyq N2  :L-S periodograms can include frequencies above what\n"
"                    would typically be considered Nyquist (here defined\n"
"                    as:\n"
"                     f_N = 0.5*(number of samples)/(total time interval)\n"
"                    By default, the maximum frequency will be what\n"
"                    f_N *would* have been if no censoring of points had\n"
"                    occured. (This makes it easier to compare L-S spectra\n"
"                    across a group with the same scan protocol, even if\n"
"                    there are slight differences in censoring, per subject.)\n"
"                    Acceptable values are >0.\n"
"  -in_upsamp N1    :During the extirpolation process, one can upsample\n"
"                    a bit.  If you are really interested in changing this,\n"
"                    check out the above-cited works for more info. Default\n"
"                    is N1=1.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"        3dLombScargle -prefix LSout -inset TimeSeries.nii.gz \\\n"
"             -mask mask.nii.gz -in_censor1D censor_list.txt\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" \n"
"____________________________________________________________________________\n"
          );
	return;
}

int main(int argc, char *argv[]) {
   int i,j,k,l,m,n,mm;
   int idx;
   int iarg;
   THD_3dim_dataset *insetTIME = NULL;
   THD_3dim_dataset *inset0 = NULL;
   THD_3dim_dataset *MASK=NULL;
   char *prefix=NULL ;
   char in_name[300];
   char in_name0[300];
   char in_mask[300];
   char *in_censor=NULL;
   THD_3dim_dataset *outset_LS=NULL;
   char outset_name[300];

   float temp_sum = 0.;

   char out_TS[300];
   char out_LS[300];

   int HAVE_MASK = 0;
   int ***mskd; // define mask of where time series are nonzero
   float **all_ls=NULL; // will hold output data
   //float **all_ts=NULL; // will hold output data

   FILE *fout0, *fout1;
   int Nvox=-1;            // tot number vox
   int *Dim=NULL;
   float sampleTR = -1;
   int Npts_cen = -1;

   MRI_IMAGE *flim=NULL;
   MRI_IMAGE *in_cen_im=NULL;
   short *censor_sh=NULL; // which points of time series are *IN*;
                          // read in from 1D file of 1s and 0s.
   float *censor_flt=NULL;

   float *tpts = NULL;
   double *wk1=NULL, *wk2=NULL;

   float my_hifac = -1.0; // how many mults of f_Nyquist we want
                          // output: choosing not to upgrade for our
                          // purposes
   float my_ofac = 1.0;   // upsampling within range-- may just let
                          // equal to 1 for our purposes.
   int Npts_wrk = 0;      // output array size-- to be calc'ed
   int Npts_out, jmax;    // holders for output
   float prob;            // ... and another holder for output
   //int DEMEAN_TS = 0;
   //float ts_mean;

   int DO_NORMALIZE = 1;      // default is to normalize spectrum
                              // output
   int DO_AMPLITUDEIZE = 0;   // default is to output periodogram

   mainENTRY("3dLombScargle"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   INFO_message("Reading in options.");
	
   /** scan args **/
   if (argc == 1) { usage_LombScargle(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_LombScargle(strlen(argv[iarg])>3 ? 2:1);
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
                     ERROR_exit("Need argument after '-inset'");

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
         sampleTR = DSET_TR_SEC(insetTIME);
      
         INFO_message("TR in MR volume appears to be: %f s", sampleTR);

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

      if( strcmp(argv[iarg],"-in_censor1D") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '-in_censor_1D'\n") ;
       
         in_censor = strdup(argv[iarg]) ;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-in_upsamp") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_upsamp'");
         my_ofac = atof(argv[iarg]);
         if( my_ofac <=0 ) {
            ERROR_message("Can't enter an upsampling factor <=0!");
            exit(2);
         }

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-in_mult_nyq") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-in_mult_nyq'");
         my_hifac = atof(argv[iarg]);
         if( my_hifac <=0 ) {
            ERROR_message("Can't enter a Nyquist factor <=0!");
            exit(2);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-do_not_normize") == 0) {
         INFO_message("Will normalize output");
			DO_NORMALIZE=0;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-out_spectr_amp") == 0) {
         INFO_message("Will output spectral amplitudes");
			DO_AMPLITUDEIZE=1;
			iarg++ ; continue ;
		}

      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
	
   // TEST BASIC INPUT PROPERTIES
   if (iarg < 3) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }
  
   if( MASK ) 
      if ( Dim[0] != DSET_NX(MASK) || Dim[1] != DSET_NY(MASK) ||
           Dim[2] != DSET_NZ(MASK) ) {
         ERROR_message("Mask and inset don't appear to have the same "
                       "dimensions.\n");
         exit(1);
      }
  
  
   INFO_message("Data read in.  Continuing");
  
	
   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   // ---------------------------------------------------------------
   // deal with censoring input
   if( in_censor ) {

      flim = mri_read_1D (in_censor);
      if (flim == NULL) {
         ERROR_exit("Error reading censor 1D file");
      }
      if( flim->ny == 1)
         // effectively *undoes* autotransp
         in_cen_im = mri_to_short( 1.0 , mri_transpose(flim));
      else
         in_cen_im = mri_to_short( 1.0 , mri_copy(flim)); 
      mri_free(flim);

      i = in_cen_im->ny;
      INFO_message("1D file has %d time points", i);
      if ( i != Dim[3] ) {
         mri_free (in_cen_im);
         ERROR_exit("Error: censor file has %d points, "
                    " but the volume has %d bricks", i, Dim[3]);
      }
      censor_sh = MRI_SHORT_PTR( in_cen_im );

   }
   else {
      WARNING_message("NB: no censor file input-- "
                      "doing internal checks for 0-full volumes to censor!");
     
      censor_sh = (short *)calloc(Dim[3],sizeof(short));
      if( (censor_sh == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(233);
      }

      for( l=0 ; l<Dim[3] ; l++ ) {
         temp_sum = 0.;
         idx = 0;
         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) {
                  temp_sum+= abs(THD_get_voxel(insetTIME,idx,l));
                  idx++;
               }
         if( temp_sum > EPS_V ) 
            censor_sh[l] = 1;
      }
   }

   // use censor_sh to find out number of non-censored points, make
   // float array of times.
   Npts_cen = 0;
   for( i=0; i<Dim[3] ; i++) {
      if(censor_sh[i]) {
         Npts_cen++;
      }
   }
   censor_flt = (float *)calloc(Npts_cen, sizeof(float));
   
   if( (censor_flt == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(233);
   }
   
   j = 0;
   for( i=0; i<Dim[3] ; i++)
      if(censor_sh[i]) {
         censor_flt[j] = i * sampleTR;
         j++;
      }  
   // -- - - - - - - - -  - - - - - - -  - - - - - - - - --  

   // populate float array of sampled times
   sprintf(out_TS,"%s_time.1D",prefix); 
   if( (fout0 = fopen(out_TS, "w")) == NULL) {
      fprintf(stderr, "\n\nError opening file %s.\n",out_TS);
      exit(1);
   }
   for( i=0; i<Npts_cen ; i++) 
      fprintf(fout0,"%.5f\n", censor_flt[i]);
   fprintf(fout0,"\n");
   fclose(fout0);
   INFO_message("Done writing (floating) time points 1D file: %s",out_TS);
  
   // ---------------------------------------------------------------

   // MASK
   mskd = (int ***) calloc( Dim[0], sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );
   if( (mskd == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure (mask).\n\n");
      exit(233);
   }
   // go through once: define data vox
   idx = 0;
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            // also, check against data with all zero values
            temp_sum = 0.;
            for ( l=0 ; l<Dim[3] ; l++ )
               if( censor_sh[l] )
                  temp_sum+= abs(THD_get_voxel(insetTIME,idx,l));

            // Of primary concern: only work on voxels that aren't
            // uniformly zero in censored part-- otherwise, algorithm
            // gives a nan.
            if ( temp_sum > EPS_V ) {
               if ( HAVE_MASK ) // check mask
                  if( THD_get_voxel(MASK,idx,0)>0 )
                     mskd[i][j][k] = 1;
               if( !HAVE_MASK ) // don't need to check mask
                  mskd[i][j][k] = 1;
            }
            idx+= 1; // skip, and mskd is still 0 from calloc
         }
   INFO_message("Done masking.");

   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************
	
   // Calculate 'hifac' in order to have constant effective upper
   // frequency for a given time series length and TR; this is so that
   // different censoring still leads to having the same output
   // frequencies.  Though, if user inputs hifac, then use that.
   if ( my_hifac <= 0) {
      my_hifac = Dim[3];
      my_hifac/= (float) Npts_cen;
      INFO_message("Effective Nyquist multiplicative factor "
                   "for upper frequency is %.4f", my_hifac);
   }
  
   // calculate Npts_wrk and Npts_out
   PR89_suppl_calc_Ns( Npts_cen, 
                       my_ofac, 
                       my_hifac, 
                       &Npts_out,  // i.e., nout
                       &Npts_wrk); // i.e., ndim =nwk
   INFO_message("Have %d points after censoring.", Npts_cen);
   INFO_message("Have %d points for outputting.", Npts_out);
   INFO_message("Planning to have %d points for working.", Npts_wrk);

   tpts = (float *)calloc(Npts_cen, sizeof(float));
   wk1 = (double *)calloc(Npts_wrk, sizeof(double));
   wk2 = (double *)calloc(Npts_wrk, sizeof(double));

   /*all_ts = (float **) calloc( Npts_cen, sizeof(float *) );
     for ( i = 0 ; i < Npts_cen ; i++ ) 
     all_ts[i] = (float *) calloc( Nvox, sizeof(float) );*/

   all_ls = (float **) calloc( Npts_out, sizeof(float *) );
   for ( i = 0 ; i < Npts_out ; i++ ) 
      all_ls[i] = (float *) calloc( Nvox, sizeof(float) );

   if( //(all_ts == NULL) || 
      (wk1 == NULL) || (wk2 == NULL) ||
      (all_ls == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure (time point arrays).\n\n");
      exit(234);
   }

   // ---------------------------------------------------------------
   // populate TS with censored info
   /*
     idx = 0;
     for( k=0 ; k<Dim[2] ; k++ ) 
     for( j=0 ; j<Dim[1] ; j++ ) 
     for( i=0 ; i<Dim[0] ; i++ ) {
     if( mskd[i][j][k] ) {
     m=0;
     for( l=0 ; l<Dim[3] ; l++ )
     if(censor_sh[l]) {
     tpts[m] = THD_get_voxel(insetTIME,idx,l);
     m++;
     }
     fasper( censor_flt-1, tpts-1, Npts_cen, 
     my_ofac, my_hifac, 
     wk1-1, all_ls[idx]-1, Npts_wrk,
     &Npts_out, &jmax, &prob);
     }
     idx++;
     }*/

   // ---------------------------------------------------------------

   idx = 0;
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( mskd[i][j][k] ) {
               m=0;
               //ts_mean = 0.;
               for( l=0 ; l<Dim[3] ; l++ )
                  if(censor_sh[l]) {
                     tpts[m] = THD_get_voxel(insetTIME,idx,l);
                     //ts_mean+= tpts[m];
                     m++;
                  }

               //if( DEMEAN_TS) {
               //   ts_mean/= Npts_cen;
               //   INFO_message("DEMEAN: %f", ts_mean);
               //   for( l=0 ; l<Npts_cen ; l++ )
               //      tpts[l]-= ts_mean;
               //      }

               PR89_fasper( censor_flt-1, tpts-1, Npts_cen, 
                            my_ofac, my_hifac, 
                            wk1-1, wk2-1, Npts_wrk,
                            Npts_out, &jmax, &prob,
                            DO_NORMALIZE,
                            DO_AMPLITUDEIZE);

               for( l=0 ; l<Npts_out ; l++ ) 
                  all_ls[l][idx] = wk2[l];
               if (!(all_ls[20][idx] > 0)) {
                  for( l=0 ; l<Npts_out ; l++ ) 
                     fprintf(stderr," LS %.2f ", all_ls[l][idx]);
                  fprintf(stderr,"\n");
                  for( l=0 ; l<m ; l++ ) 
                     fprintf(stderr," TS %.2f ", tpts[l]);
                  fprintf(stderr,"\n");

               }
            }
            idx++;
         }

   INFO_message("Done Lomb-Scargling.");
   INFO_message("Number of frequencies output = %d", (int) Npts_out);

   // store abcissa/freq values
   sprintf(out_LS,"%s_freq.1D",prefix); 
   if( (fout0 = fopen(out_LS, "w")) == NULL) {
      fprintf(stderr, "\n\nError opening file %s.\n",out_LS);
      exit(1);
   }
   for( k=0 ; k<Npts_out ; k++ ) 
      fprintf(fout0,"%.5f\n", wk1[k]);
   fprintf(fout0,"\n");
   fclose(fout0);
   INFO_message("Done writing frequency 1D file: %s",out_LS);

   // **************************************************************
   // **************************************************************
   //                 Store and output
   // **************************************************************
   // **************************************************************

   // for output data set
   outset_LS = EDIT_empty_copy( inset0 ) ; 
   EDIT_add_bricklist( outset_LS,
                       Npts_out-1, NULL , NULL , NULL );

   EDIT_dset_items( outset_LS,
                    ADN_datum_all , MRI_float , 
                    ADN_prefix    , prefix ,
                    ADN_none ) ;
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outset_LS)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",
                 DSET_HEADNAME(outset_LS));

   // copy data over
   for( i=0 ; i<Npts_out ; i++ ) {
      EDIT_substitute_brick(outset_LS, i, MRI_float, all_ls[i]); 
      all_ls[i]=NULL;
   }

   THD_load_statistics(outset_LS);
   tross_Make_History("3dLombScargle", argc, argv, outset_LS);
   THD_write_3dim_dataset(NULL, NULL, outset_LS, True);
  
   INFO_message("Done writing data file: %s", prefix);


   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   INFO_message("Freeing...");

   DSET_delete(insetTIME);
   free(insetTIME);
   DSET_delete(inset0);
   free(inset0);
   DSET_delete(MASK);
   free(MASK);

   DSET_delete(outset_LS);
   free(outset_LS);

   for( i=0 ; i<Npts_out ; i++) {
      free(all_ls[i]);
   }
   free(all_ls);

   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) {
         free(mskd[i][j]);
      }
   for( i=0 ; i<Dim[0] ; i++) {
      free(mskd[i]);
   }
   free(mskd);

   if(prefix)
      free(prefix);
   if(in_censor)
      free(in_censor);
   if(censor_sh)
      free(censor_sh); 
   if(censor_flt)
      free(censor_flt);
   if(tpts)
      free(tpts);
   if(wk1)
      free(wk1);
   if(wk2)
      free(wk2);
   INFO_message("...Done");

   return 0;
}
