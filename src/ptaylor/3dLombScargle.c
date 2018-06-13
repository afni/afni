/* 
   Description

   [PT: Sept, 2014] Fixed up now. Will revisit tapers and windows laterz. 

   [PT: Sept 19, 2014] Fixed N of points for delF calc. 
 
   [PT: Sept 26, 2014] Add in attribute output for Ntpts pre- and
   post-censoring.

   [PT: June 12, 2018] 
   + no more norm(al)ize
   + don't scale output by number of tpts after censoring
     -> with accompanying change in 3dAmpToRSFC
   + for now, leave in factor of 2 in Amps here...


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
"  Make a periodogram or amplitude-spectrum of a time series that has a\n"
"  non-constant sampling rate. The spectra output by this program are \n"
"  'one-sided', so that they represent the half-amplitude or power\n"
"  associated with a frequency, and they would require a factor of 2 to \n"
"  account for both the the right- and left-traveling frequency solutions \n"
"  of the Fourier transform (see below 'OUTPUT' and 'NOTE').\n"
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
"  several adjustments based on that. \n"
"\n"
"  The Lomb-Scargle adaption was done with fairly minimal changes here by\n"
"  PA Taylor (v1.4, June, 2016). \n"
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
"      The frequency spectrum will be in the range [df, f_N], where:\n"
"        df = 1/T, and T is the total duration of the uncensored time series;\n"
"        f_N = 1/dt, and dt is the sampling time (i.e., TR);\n"
"        and the interval of frequencies is also df.\n"
"      These ranges and step sizes should be *independent* of the censoring\n"
"      which is a nice property of the Lomb-Scargle-iness.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"  + OUTPUT: \n"
"    1) PREFIX_time.1D    :a 1D file of the sampled time points (in units of\n"
"                          seconds) of the analyzed (and possibly censored)\n"
"                          data set.\n"
"    2) PREFIX_freq.1D    :a 1D file of the frequency sample points (in units\n"
"                          of 1/seconds) of the output periodogram/spectrum\n"
"                          data set.\n"
"    3) PREFIX_amp+orig   :volumetric data set containing a LS-derived\n"
"             or           amplitude spectrum (by default, named 'amp') or a\n"
"       PREFIX_pow+orig    power spectrum (see '-out_pow_spec', named 'pow')\n" 
"                          one per voxel. \n"
"                          Please note that the output amplitude and power\n"
"                          spectra are 'one-sided', to represent the \n"
"                          *half* amplitude or power of a given frequency\n"
"                          (see the following note).\n"
"\n"
"  + A NOTE ABOUT Fourier+Parseval matters (please forgive the awkward\n"
"   formatting):\n"
"      In the formulation used here, for a time series x[n] of length N, \n"
"      the periodogram value S[k] is related to the amplitude value |X[k]|:\n"
"       (1)     S[k] = (|X[k]|)**2,\n"
"      for each k-th harmonic.\n"
"\n"
"      Parseval's theorem relates time fluctuations to spectral amplitudes,\n"
"      stating that (for real time series with zero mean):\n"
"       (2)     sum_n{ x[n]**2 } = (1/N) * sum_k{ |X[k]|**2 }, \n"
"                                = (1/N) * sum_k{ S[k] }, \n"
"      where n=0,1,..,N-1 and k=0,1,..,N-1 (NB: A[0]=0, for zero mean \n"
"      series). The LHS is essentially the variance of the time series \n"
"      (times N-1).  The above is derived from Fourier transform maths, and\n"
"      the Lomb-Scargle spectra are approximations to Fourier, so the above\n"
"      can be expected to approximately hold, if all goes well.\n"
"\n"
"      Another Fourier-related result is that for real, discrete time series,\n"
"      the spectral amplitudes/power values are symmetric and periodic in N.\n"
"      Therefore, |X[k]| = |X[-k]| = |X[N-k-1]| (in zero-base array \n"
"      counting);\n" 
"      the distinction between positive- and negative-indexed frequencies\n"
"      can be thought of as signifying right- and left-traveling waves, which\n"
"      both contribute to the total power of a specific frequency.\n"
"      The upshot is that one could write the Parseval formula as:\n"
"       (3)     sum_n{ x[n]**2 } = (2/N) * sum_l{ |X[l]|**2 }, \n"
"                                = (2/N) * sum_l{ S[l] }, \n"
"      where n=0,1,..,N-1 and l=0,1,..,(N/2)-1 (note the factor of 2 now\n"
"      appearing on the RHS relations). These symmetries/considerations\n"
"      are the reason why ~N/2 frequency values are output here (we assume \n"
"      that only real-valued time series are input), without any loss of\n"
"      information.\n"
"\n"
"      Additionally, with a view toward expressing the overall amplitude\n"
"      or power of a given frequency, which many people might want to use to \n"
"      estimate spectral 'functional connectivity' parameters such as ALFF,\n"
"      fALFF, RSFA, etc. (using, for example, 3dAmptoRSFC), we therefore \n"
"      note that the *total* amplitude or power of a given frequency would\n"
"      be:\n"
"            A[k] = 2*|X[k]|                 \n"
"            P[k] = 2*S[k] = 2*|X[k]|**2 = 0.5*A[k]**2    \n"
"      instead of just that of the left/right traveling part. These types of\n"
"      quantities (A and P) are also referred to as 'two-sided' spectra. The\n"
"      resulting Parseval relation could then be written:\n"
"       (4)     sum_n{ x[n]**2 } = (1/(2N)) * sum_l{ A[l]**2 }, \n"
"                                = (1/N) * sum_l{ P[l] }, \n"
"      where n=0,1,..,N-1 and l=0,1,..,(N/2)-1. Somehow, it just seems easier\n"
"      to output the one-sided values, X and S, so that the Parsevalian\n"
"      summation rules look more similar.\n"
"\n"
"      With all of that in mind, the 3dLombScargle results are output as\n"
"      follows. For amplitudes, the following approx. Parsevellian relation\n"
"      should hold between the 'holey' time series x[m] of M points and\n"
"      the frequency series Y[l] of L~M/2 points (where {|Y[l]|} approaches\n"
"      the Fourier amplitudes {|X[l]|} as the number of censored points \n"
"      decreases and M->N):\n"
"       (5)     sum_m{ x[m]**2 } = (1/L) * sum_l{ Y[l]**2 }, \n"
"      where m=0,1,..,M-1 and l=0,1,..,L-1. For the power spectrum T[l]\n"
"      of L~M/2 values, then:\n"
"       (6)     sum_m{ x[m]**2 } = (1/L) * sum_l{ T[l] } \n"
"      for the same ranges of summations.\n"
"\n"
"      So, please consider that when using the outputs of here. 3dAmpToRSFC\n"
"      is prepared for this when calculating spectral parameters (from \n"
"      amplitudes).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMAND:  3dLombScargle -prefix PREFIX -inset FILE \\\n"
"                  {-censor_1D C1D} {-censor_str CSTR} \\\n"
"                  {-mask MASK} {-out_pow_spec}  \\\n"
"                  {-nyq_mult N2}  {-nifti}  \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + RUNNING:\n"
"  -prefix PREFIX   :output prefix name for data volume, time point 1D file\n"
"                    and frequency 1D file.\n"
"  -inset FILE      :time series of volumes, a 4D volumetric data set.\n"
"\n"
"  -censor_1D C1D   :single row or column of 1s (keep) and 0s (censored)\n"
"                    describing which volumes of FILE are kept in the\n"
"                    sampling and which are censored out, respectively. The\n"
"                    length of the list of numbers must be of the\n"
"                    same length as the number of volumes in FILE.\n"
"                    If not entered, then the program will look for subbricks\n"
"                    of all-zeros and assume those are censored out.\n"
"  -censor_str CSTR :AFNI-style selector string of volumes to *keep* in\n"
"                    the analysis.  Such as: \n"
"                         '[0..4,7,10..$]'\n"
"                    Why we refer to it as a 'censor string' when it is\n"
"                    really the list of volumes to keep... well, it made\n"
"                    sense at the time.  Future historians can duel with\n"
"                    ink about it.\n"
"\n"
"  -mask MASK       :optional, mask of volume to analyze; additionally, any\n"
"                    voxel with uniformly zero values across time will\n"
"                    produce a zero-spectrum.\n"
"\n"
"  -out_pow_spec    :switch to output the amplitude spectrum of the freqs\n"
"                    instead of the periodogram.  In the formulation used\n"
"                    here, for a time series of length N, the power spectral\n"
"                    value S is related to the amplitude value X as:\n"
"                    S = (X)**2. (Without this opt, default output is \n"
"                    amplitude spectrum.)\n"
"\n"
"  -nyq_mult N2     :L-S periodograms can include frequencies above what\n"
"                    would typically be considered Nyquist (here defined\n"
"                    as:\n"
"                     f_N = 0.5*(number of samples)/(total time interval)\n"
"                    By default, the maximum frequency will be what\n"
"                    f_N *would* have been if no censoring of points had\n"
"                    occured. (This makes it easier to compare L-S spectra\n"
"                    across a group with the same scan protocol, even if\n"
"                    there are slight differences in censoring, per subject.)\n"
"                    Acceptable values are >0. (For those reading the \n"
"                    algorithm papers, this sets the 'hifac' parameter.)\n"
"                    If you don't have a good reason for changing this,\n"
"                    dooon't change it!\n"
"  -nifti           :switch to output *.nii.gz volume file\n"
"                    (default format is BRIK/HEAD).\n"

"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"        3dLombScargle -prefix LSout -inset TimeSeries.nii.gz \\\n"
"             -mask mask.nii.gz -censor_1D censor_list.txt\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" \n"
"____________________________________________________________________________\n"
          );
	return;
}

/*  Not included right now!  Have to see what normalizing means, in light of Parseval
scaling...
"  -do_normize      :switch to output the variance-normalized periodogram\n"
"                    or amplitude spectrum (default is *not* to normalize,\n"
"                    because you should probably have processed/afni_proc'ed\n"
"                    your FMRI data to be nice units of percent-signal change\n"
"                    already...). For a time series with variance V, a \n"
"                    normalized periodogram value Pn is related to a non-\n"
"                    normalized value P0 as:\n"
"                    Pn = P0/V.\n"

"  -upsamp_fac N1   :During the extirpolation process, one can upsample\n"
"                    a bit.  If you are really interested in changing this,\n"
"                    check out the above-cited works for more info. Default\n"
"                    is N1=1. (For those reading the algorithm papers, this\n"
"                    sets the 'ofac' parameter.)\n"

"  -welch_win NW    :use Welch windowing method to estimate the spectrum; the\n"
"                    frequency output is essentially smoothed, but the peaks\n"
"                    should be better estimates (smaller variance). The \n"
"                    actual number of windows used is 2*NW - 1, as the \n"
"                    windows will overlap by ~50%%. By default, NW=1; also \n"
"                    by default, each window (even if NW=1) is tapered, \n"
"                    currently using a (L2-normed) Hann function.\n" 

Fun things like Welch-windowing capability\n"
"  and time series tapering have been added now.

"  -taper_off       :turn off tapering (for any number of windows, >=1). In \n"
"                    general, the tapering should/does reduce aliasing and\n"
"                    possibly spurious higher frequencies (or so they say!),\n"
"                    so turn this off at your own imminent peril.\n"


*/

int main(int argc, char *argv[]) {
   int i,j,k,l,m,n,mm,w,pp;
   int idx;
   int iarg;
   THD_3dim_dataset *insetTIME = NULL;
   //THD_3dim_dataset *inset0 = NULL;
   THD_3dim_dataset *MASK=NULL;
   char *prefix=NULL ;
   //   char in_name[300];
   char in_mask[300];
   char *in_censor=NULL;
   char *str_censor=NULL;
   int  *int_cens=NULL;
   THD_3dim_dataset *outset_LS=NULL;
   char outset_name[300];

   char *out_type[2] = {"pow","amp"};

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

   float *tpts = NULL, *tpts_win=NULL;
   double *wk1=NULL, *wk2=NULL;

   double my_hifac = -1.0; // how many mults of f_Nyquist we want
                          // output: choosing not to upgrade for our
                          // purposes
   double my_ofac = 1.0;   // upsampling within range-- may just let
                          // equal to 1 for our purposes.
   int Npts_wrk = 0;      // output array size-- to be calc'ed
   int Npts_out, jmax;    // holders for output
   float prob;            // ... and another holder for output
   //int DEMEAN_TS = 0;
   //float ts_mean;

   int DO_NORMALIZE = 0;      // default is to NOT normalize spectrum
                              // output
   int DO_AMPLITUDEIZE = 1;   // default is to output spectr amps

   int NSEG = 1;              // number of non-overlapping Welch wins
   int NWIN, NWINp1;          // to be numbers of Welch windows
   double delF;               // to keep constant as we go -> use to
                              // set ofac
   double win_ofac;
   int win_Npts_out, win_Npts_wrk;  // per win, are <= Npts_{out,work}
   int **WinInfo=NULL;
   float *WinDelT=NULL, *WinVec=NULL;
   int DO_TAPER = 0;          // off by default
   int NIFTI_OUT=0;
   int mk_info=1;

   float AVE=0., VAR=0.;

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

         //sprintf(in_name,"%s", argv[iarg]); 
         insetTIME = THD_open_dataset(argv[iarg]);
         if( (insetTIME == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.", argv[iarg]); 

         Dim = (int *)calloc(4,sizeof(int));
         DSET_load(insetTIME); CHECK_LOAD_ERROR(insetTIME);
         Nvox = DSET_NVOX(insetTIME) ;
         Dim[0] = DSET_NX(insetTIME); Dim[1] = DSET_NY(insetTIME); 
         Dim[2] = DSET_NZ(insetTIME); Dim[3]= DSET_NVALS(insetTIME); 
         sampleTR = DSET_TR(insetTIME);
      
         if( Dim[3] < 2 ) 
            ERROR_exit("Input 'inset' is too short, only has %d vols."
                       "  -> hardly a time *series*. My have Nvol>2.", 
                       Dim[3]);

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

      /*
      if( strcmp(argv[iarg],"-welch_win") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '-welch_win'\n") ;
       
         NSEG = atoi(argv[iarg]);
         if( NSEG <=0 ) {
            ERROR_message("Can't enter a negative number of segments for "
                          "windowing! Check '-welch_win ...'. ");
            exit(2);
         }

         iarg++ ; continue ;
      }
      */

      if( strcmp(argv[iarg],"-censor_1D") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '-censor_1D'\n") ;
       
         in_censor = strdup(argv[iarg]) ;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-censor_str") == 0 ){
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '-censor_str'\n") ;
       
         str_censor = strdup(argv[iarg]) ;

         iarg++ ; continue ;
      }


      /*  unused-- shouldn't have, for consistency across groups
      if( strcmp(argv[iarg],"-upsamp_fac") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-upsamp_fac'");
         my_ofac = atof(argv[iarg]);
         if( my_ofac <=0 ) {
            ERROR_message("Can't enter an upsampling factor <=0!");
            exit(2);
         }
         iarg++ ; continue ;
         }*/

      if( strcmp(argv[iarg],"-nyq_mult") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-nyq_mult'");
         my_hifac = (double) atof(argv[iarg]);
         if( my_hifac <=0 ) {
            ERROR_message("Can't enter a Nyquist factor <=0!");
            exit(2);
         }
         iarg++ ; continue ;
      }

      /*if( strcmp(argv[iarg],"-do_normize") == 0) {
         INFO_message("Will normalize output");
			DO_NORMALIZE=1;
			iarg++ ; continue ;
         }*/

      if( strcmp(argv[iarg],"-out_pow_spec") == 0) {
         INFO_message("Will output the *power spectrum*, "
                      "not spectral amplitudes (see 'help' "
                      "if unsure of choice)");
			DO_AMPLITUDEIZE=0;
			iarg++ ; continue ;
		}

      /*
      if( strcmp(argv[iarg],"-taper_on") == 0) {
         INFO_message("Un-releasing the tapers.");
			DO_TAPER=1;
			iarg++ ; continue ;
		}
      */

      if( strcmp(argv[iarg],"-nifti") == 0) {
         NIFTI_OUT=1;
         iarg++ ; continue ;
      }

      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }

   // -------------------------------------------------------------

   // TEST BASIC INPUT PROPERTIES
   if (iarg < 3) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }
  
   if( !insetTIME )
      ERROR_exit("Hey! No input time series data set! Use '-inset ...'.");

   if( MASK ) 
      if ( Dim[0] != DSET_NX(MASK) || Dim[1] != DSET_NY(MASK) ||
           Dim[2] != DSET_NZ(MASK) ) {
         ERROR_message("Mask and inset don't appear to have the same "
                       "spatial dimensions.\n");
         exit(1);
      }
  
   INFO_message("Data read in.  Continuing");
	
   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   // ---------------------------------------------------------------
   // Welch window stuff
   NWIN = 2*NSEG - 1;         // 50% overlap of windows
   NWINp1 = NWIN+1;           // for counting/ratios   

   if( NSEG == 1 )
      INFO_message("Single window.");
   //      INFO_message("Not using Welch windows.");
   else
      INFO_message("For Welch windowing, using:\n"
                   "\t%d segments of the time series,\n"
                   "\tfor a total of %d overlapping windows.",NSEG, NWIN);

   WinDelT = (float *)calloc(NWIN,sizeof(float));
   WinInfo = (int **) calloc( NWIN, sizeof(int *) );
   for ( i = 0 ; i < NWIN ; i++ ) 
      WinInfo[i] = (int *) calloc( 2, sizeof(int) );

   if( (WinInfo == NULL) || (WinDelT == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure (time point arrays).\n\n");
      exit(334);
   }

   // ---------------------------------------------------------------
   if( in_censor && str_censor) {
      ERROR_exit("Can use either '-censor_1D ...' or '-censor_str ...'"
                 " but not *both!");
   }

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
         ERROR_exit("Censor file has %d points, "
                    " but the volume has %d bricks", i, Dim[3]);
      }
      censor_sh = MRI_SHORT_PTR( in_cen_im );

   }
   else {

      censor_sh = (short *)calloc( Dim[3], sizeof(short) );
      if( (censor_sh == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(233);
      }

      if(str_censor) {
         INFO_message("Translating censor string: %s", str_censor);
         // int_cens[0] = Ncen = "how many indices there will be".
         // len(int_cens) = Ncen+1
         int_cens = MCW_get_intlist( Dim[3] , str_censor );
         INFO_message("--> Keeping %d volumes", int_cens[0]);
         
         //fprintf(stderr, "\n");
         for( i=1 ; i<int_cens[0]+1 ; i++ ) {
          censor_sh[int_cens[i]] = 1;
         // fprintf(stderr, "%5d,",  int_cens[i]);
         }
         //fprintf(stderr, "\n");
         free(int_cens);
      }
      else {
         WARNING_message("no censor file input\n\t-> doing internal "
                         "checks for 0-full volumes to censor.");
         
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
      fprintf(stderr, "\n\nError opening file %s.\n", out_TS);
      exit(1);
   }
   for( i=0; i<Npts_cen ; i++) 
      fprintf(fout0,"%.5f\n", censor_flt[i]);
   fprintf(fout0,"\n");
   fclose(fout0);
   INFO_message("Done writing (float) time points 1D file: %s", out_TS);
  
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
      my_hifac/= (float) Npts_cen; // So this is const across group
      INFO_message("Effective Nyquist multiplicative factor "
                   "for upper frequency is %.4f", my_hifac);
   }
   // Want this const across group and across windows.
   delF = 1.0/(Dim[3]*sampleTR*my_ofac); 

   INFO_message("Total Ntpts=%d,  TR=%.4f, my_ofac=%.4f", 
                Dim[3], sampleTR, my_ofac);
   INFO_message("Frequency unit: Delta f = %e", delF);

   // calculate Npts_wrk and Npts_out, as if no windows (for single
   // alloc-- would be max lengths of things; will calculate "per
   // window" ones, as needs be)
   PR89_suppl_calc_Ns( Npts_cen, Dim[3],
                         my_ofac, 
                         my_hifac, 
                         &Npts_out,  // i.e., nout
                         &Npts_wrk); // i.e., ndim =nwk
   INFO_message("Full time series: have %d total points after censoring.", 
                Npts_cen);
   INFO_message("Have %d points for outputting.", Npts_out);
   //INFO_message("Planning to have %d points for working.", Npts_wrk);

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

   //fprintf(stderr," !!!!! Npts_cen = %d ", Npts_cen);
   //fprintf(stderr," !!!!! NSEG = %d ", NSEG);
   //fprintf(stderr," !!!!! NWIN = %d ", NWIN);

   // window calcs
   WelchWindowInfo( censor_flt, Npts_cen, NSEG, 
                    WinInfo, WinDelT, NWIN );

   // right now, welch window; *presently* all windows have same
   // length; later, this could move into a loop below, if necessary.

   WinVec = (float *)calloc(WinInfo[0][1], sizeof(float));
   tpts_win = (float *)calloc(Npts_cen, sizeof(float));
   if( (WinVec == NULL) || (tpts_win == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure (window vec array).\n\n");
      exit(244);
   }
   
   MakeWindowVec( WinVec, WinInfo[0][1] );

   // ---------------------- get time series ---------------------

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

               // ---------- per window now -----------------
               for( w=0 ; w<NWIN ; w++ ) {

                  // clean from last iteration
                  for( pp=0 ; pp<Npts_wrk ; pp++ )
                     wk1[pp] = wk2[pp] = 0.;

                  win_ofac = 1./(delF * WinDelT[w]); // calc'ed per win
                  PR89_suppl_calc_Ns( WinInfo[w][1], -1,
                                      win_ofac,   
                                      my_hifac,    // const for all wins
                                      &win_Npts_out,  // i.e., nout
                                      &win_Npts_wrk); // i.e., ndim =nwk

                  /*if(mk_info) {
                    INFO_message("Window[%d] ofac: %.3f  \t-->  %d points.", 
                    w, win_ofac, Npts_out);
                    INFO_message("windelt[%d]: %.3f  \t-->  winnumpts: %d .", 
                    w, WinDelT[w], WinInfo[w][1]);
                    INFO_message("%d   %d", win_Npts_out, win_Npts_wrk);
                    INFO_message("offset: %d", WinInfo[w][0]);
                    
                    if (w == (NWIN-1))
                    mk_info=0;
                    }
                  */
                  
                  /*for( pp=0 ; pp<WinInfo[w][1] ; pp++ )
                    tpts_win[pp] = tpts[pp+WinInfo[w][0]];
                    if(NSEG>1)
                    for( pp=0 ; pp<WinInfo[w][1] ; pp++ )
                    tpts_win[pp]*= WinVec[pp];
                  */
                  
                  if(DO_TAPER)
                     PR89_fasper( censor_flt - 1 + WinInfo[w][0], 
                                  tpts - 1 + WinInfo[w][0], WinInfo[w][1],
                                  tpts_win - 1, WinVec - 1,
                                  win_ofac,
                                  wk1-1, wk2-1, Npts_wrk,
                                  Npts_out, &jmax, &prob, // use npts_out!
                                  DO_NORMALIZE,
                                  DO_AMPLITUDEIZE);
                  else
                     PR89_fasper( censor_flt - 1 + WinInfo[w][0], 
                                  tpts - 1 + WinInfo[w][0], WinInfo[w][1],
                                  tpts_win - 1, NULL,
                                  win_ofac,
                                  wk1-1, wk2-1, Npts_wrk,
                                  Npts_out, &jmax, &prob, // use npts_out!
                                  DO_NORMALIZE,
                                  DO_AMPLITUDEIZE);

                  /*
                  fprintf(stderr,"\n WK1 nout:%d ",win_Npts_out);
                  for( pp=0 ; pp<win_Npts_out ; pp++ )
                     fprintf(stderr," %f ", wk1[pp]);
                  fprintf(stderr,"\n ");
                  fprintf(stderr,"\n WK2*N ");
                  for( pp=0 ; pp<win_Npts_out ; pp++ )
                     fprintf(stderr," %f ", wk2[pp]/Npts_out);
                  fprintf(stderr,"\n ");
                  */


                  for( l=0 ; l<Npts_out ; l++ ) {
                     all_ls[l][idx]+= (float) wk2[l]; 
                  }

               }
               for( l=0 ; l<Npts_out ; l++ ) { 
                  // normalizing and accounting for wins
                  //all_ls[l][idx]*= ((float) Npts_cen) / NWIN; 
                  all_ls[l][idx]/= (float) NWIN; 
                  //if( DO_NORMALIZE)
                  // all_ls[l][idx]/= Npts_cen; //Dim[3];
                  if( DO_AMPLITUDEIZE )
                     all_ls[l][idx] = sqrt(all_ls[l][idx]);
               }
            }
            idx++;
         }

   
   /*for( w=0 ; w<NWIN ; w++ ) {
      win_ofac = (1./delF) / WinDelT[w]; // calc'ed per win
      PR89_suppl_calc_Ns( WinInfo[w][1],
      win_ofac,   
      my_hifac,    // const for all wins
      &win_Npts_out,  // i.e., nout
      &win_Npts_wrk); // i.e., ndim =nwk
      
      INFO_message("Window[%d] ofac: %.3f  \t-->  %d points.", 
                   w, win_ofac, win_Npts_out);
                   }*/
   
   INFO_message("Done Lomb-Scargling.");
   //INFO_message("Number of frequencies output = %d", (int) Npts_out);

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
   outset_LS = EDIT_empty_copy( insetTIME ) ; 
   if( NIFTI_OUT )
      sprintf(outset_name,"%s_%s.nii.gz",
              prefix,out_type[DO_AMPLITUDEIZE]); 
   else
      sprintf(outset_name,"%s_%s",
              prefix,out_type[DO_AMPLITUDEIZE]); 

   // EDIT_add_bricklist( outset_LS,
   //                  Npts_out-1, NULL , NULL , NULL );

   EDIT_dset_items( outset_LS,
                    ADN_datum_all , MRI_float , 
                    ADN_ntt   , Npts_out, 
                    ADN_nvals , Npts_out,
                    ADN_ttorg , delF ,
                    ADN_ttdel , delF ,
                    ADN_tunits, UNITS_HZ_TYPE,
                    ADN_prefix, outset_name ,
                    ADN_none ) ;
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outset_LS)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",
                 DSET_HEADNAME(outset_LS));

   // copy data over
   for( i=0 ; i<Npts_out ; i++ ) {
      EDIT_substitute_brick(outset_LS, i, MRI_float, all_ls[i]); 
      all_ls[i]=NULL;
   }

   // ! Makin' an attribute, int array of len=2: Num of tpts without
   // ! censoring, and Num of tpts left after censoring
   { int *attin=malloc(sizeof(int)*(2)) ;
      attin[0] = (int) Dim[3] ; 
      attin[1] = (int) Npts_cen;
      THD_set_int_atr( outset_LS->dblk ,
                       "N_TS_ORIG" , 2 , attin ) ;
      free(attin) ;
   }
   
   THD_load_statistics(outset_LS);
   tross_Make_History("3dLombScargle", argc, argv, outset_LS);
   THD_write_3dim_dataset(NULL, NULL, outset_LS, True);
  
   INFO_message("Done writing spectral vol file: %s", outset_name);


   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   INFO_message("Freeing...");

   DSET_delete(insetTIME);
   free(insetTIME);
   //DSET_delete(inset0);
   //free(inset0);
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

   if(WinInfo) {
      for( i=0 ; i<NSEG ; i++) 
         free(WinInfo[i]);
      free(WinInfo);
   }
   if(WinDelT)
      free(WinDelT);

   if(prefix)
      free(prefix);
   if(in_censor)
      free(in_censor);
   if(str_censor)
      free(str_censor);
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
   if(WinVec)
      free(WinVec);
   if(tpts_win)
      free(tpts_win);

   INFO_message("...Done");

   return 0;
}
