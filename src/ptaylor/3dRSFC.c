/*
  Program to calculate ALFF, mALFF, fALFF (as well as RSFA and similar
  variations) for resting state time series.  This program is
  **heavily** based on the existing 3dBandPass by RW Cox, with the
  amendments to calculate RSFC parameters written by PA Taylor (July,
  2012).

  All options of 3dBandPass may be used here (with a couple other
  parameter options, as well): essentially, the motivation of this
  program is to produce ALFF, etc. values of the actual RSFC time
  series that you calculate.  Therefore, all the 3dBandPass processing
  you normally do en route to making your final `resting state time
  series' is done here to generate your LFFs, from which the
  amplitudes in the LFF band are calculated at the end.  In order to
  calculate fALFF, the same initial time series are put through the
  same processing steps which you have chosen but *without* the
  bandpass part; the spectrum of this second time series is used to
  calculate the fALFF denominator.

  Sept. 2012:  improving some memory stuff
  Aug.  2016:  change >f_N initialization to avoid division errors when 
               there are *a lot* of time points.

*/



#include "mrilib.h"
#include <gsl/gsl_errno.h>    // @@ for ffting here
#include <gsl/gsl_fft_real.h>
#include <gsl/gsl_fft_halfcomplex.h>


#ifdef USE_OMP
#include "mri_blur3d_variable.c"
#include "thd_satcheck.c"
#endif

void THD_vectim_localpv( MRI_vectim *mrv , float rad ) ;
int THD_vectim_subset_pv( MRI_vectim *mrv, int nind, int *ind,
								  float *ar, unsigned short xran[] ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int do_norm=0 , qdet=2 , have_freq=0 , do_automask=0 ;
   float dt=0.0f , fbot=0.0f,ftop=999999.9f , blur=0.0f ;
   MRI_IMARR *ortar=NULL ; MRI_IMAGE *ortim=NULL ;
   THD_3dim_dataset **ortset=NULL ; int nortset=0 ;
   THD_3dim_dataset *inset=NULL , *outset=NULL;
   char *prefix="RSFC" ;
   byte *mask=NULL ;
   int mask_nx=0,mask_ny=0,mask_nz=0,nmask , verb=1 , 
		nx,ny,nz,nvox , nfft=0 , kk ;
   float **vec , **ort=NULL ; int nort=0 , vv , nopt , ntime  ;
   MRI_vectim *mrv ;
   float pvrad=0.0f ; int nosat=0 ;
   int do_despike=0 ;

	// @@ non-BP variables
	float fbotALL=0.0f, ftopALL=999999.9f; // do full range version
	int NumDen = 0; // switch for doing numerator or denom
	THD_3dim_dataset *outsetALL=NULL ; 	
	int m, mm;
	float delf; // harmonics
	int ind_low,ind_high,N_ny, ctr;
	float sqnt,nt_fac;
	gsl_fft_real_wavetable *real1, *real2; // GSL stuff
	gsl_fft_real_workspace *work;
	double *series1, *series2;	
	double *xx1,*xx2;
	float numer,denom,val;
	float *alff=NULL,*malff=NULL,*falff=NULL,
         *rsfa=NULL,*mrsfa=NULL,*frsfa=NULL; // values
	float meanALFF=0.0f,meanRSFA=0.0f; // will be for mean in brain region
	THD_3dim_dataset *outsetALFF=NULL;
	THD_3dim_dataset *outsetmALFF=NULL;
	THD_3dim_dataset *outsetfALFF=NULL;
	THD_3dim_dataset *outsetRSFA=NULL;
	THD_3dim_dataset *outsetmRSFA=NULL;
	THD_3dim_dataset *outsetfRSFA=NULL;
	char out_lff[300];
	char out_alff[300];
	char out_malff[300];
	char out_falff[300];
	char out_rsfa[300];
	char out_mrsfa[300];
	char out_frsfa[300];
	char out_unBP[300];
	int SERIES_OUT = 1;
	int UNBP_OUT = 0; 
	int DO_RSFA = 1;
	int BP_LAST = 0; // option for only doing filter to LFFs at very end of proc
	float de_rsfa=0.0f,nu_rsfa=0.0f;
	double pow1=0.0,pow2=0.0;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
		printf(
"\n  Program to calculate common resting state functional connectivity (RSFC)\n"
"  parameters (ALFF, mALFF, fALFF, RSFA, etc.) for resting state time\n"
"  series.  This program is **heavily** based on the existing\n"
"  3dBandPass by RW Cox, with the amendments to calculate RSFC\n"
"  parameters written by PA Taylor (July, 2012).\n"
"  This program is part of FATCAT (Taylor & Saad, 2013) in AFNI. Importantly,\n"
"  its functionality can be included in the `afni_proc.py' processing-script \n"
"  generator; see that program's help file for an example including RSFC\n"
"  and spectral parameter calculation via the `-regress_RSFC' option.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  All options of 3dBandPass may be used here (with a couple other\n"
"  parameter options, as well): essentially, the motivation of this\n"
"  program is to produce ALFF, etc. values of the actual RSFC time\n"
"  series that you calculate.  Therefore, all the 3dBandPass processing\n"
"  you normally do en route to making your final `resting state time\n"
"  series' is done here to generate your LFFs, from which the\n"
"  amplitudes in the LFF band are calculated at the end.  In order to\n"
"  calculate fALFF, the same initial time series are put through the\n"
"  same processing steps which you have chosen but *without* the\n"
"  bandpass part; the spectrum of this second time series is used to\n"
"  calculate the fALFF denominator.\n"
" \n"
"  For more information about each RSFC parameter, see, e.g.:   \n"
"  ALFF/mALFF -- Zang et al. (2007),\n"
"  fALFF --      Zou et al. (2008),\n"
"  RSFA --       Kannurpatti & Biswal (2008).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
" + USAGE: 3dRSFC [options] fbot ftop dataset\n"
"\n"
"* One function of this program is to prepare datasets for input\n"
"   to 3dSetupGroupInCorr.  Other uses are left to your imagination.\n"
"\n"
"* 'dataset' is a 3D+time sequence of volumes\n"
"   ++ This must be a single imaging run -- that is, no discontinuities\n"
"       in time from 3dTcat-ing multiple datasets together.\n"
"\n"
"* fbot = lowest frequency in the passband, in Hz\n"
"   ++ fbot can be 0 if you want to do a lowpass filter only;\n"
"       HOWEVER, the mean and Nyquist freq are always removed.\n"
"\n"
"* ftop = highest frequency in the passband (must be > fbot)\n"
"   ++ if ftop > Nyquist freq, then it's a highpass filter only.\n"
"\n"
"* Set fbot=0 and ftop=99999 to do an 'allpass' filter.\n"
"  ++ Except for removal of the 0 and Nyquist frequencies, that is.\n"
"\n"
"* You cannot construct a 'notch' filter with this program!\n"
"  ++ You could use 3dRSFC followed by 3dcalc to get the same effect.\n"
"  ++ If you are understand what you are doing, that is.\n"
"  ++ Of course, that is the AFNI way -- if you don't want to\n"
"     understand what you are doing, use Some other PrograM, and\n"
"     you can still get Fine StatisticaL maps.\n"
"\n"
"* 3dRSFC will fail if fbot and ftop are too close for comfort.\n"
"  ++ Which means closer than one frequency grid step df,\n"
"     where df = 1 / (nfft * dt) [of course]\n"
"\n"
"* The actual FFT length used will be printed, and may be larger\n"
"   than the input time series length for the sake of efficiency.\n"
"  ++ The program will use a power-of-2, possibly multiplied by\n"
"     a power of 3 and/or 5 (up to and including the 3rd power of\n"
"     each of these: 3, 9, 27, and 5, 25, 125).\n"
"\n"
"* Note that the results of combining 3dDetrend and 3dRSFC will\n"
"   depend on the order in which you run these programs.  That's why\n"
"   3dRSFC has the '-ort' and '-dsort' options, so that the\n"
"   time series filtering can be done properly, in one place.\n"
"\n"
"* The output dataset is stored in float format.\n"
"\n"
"* The order of processing steps is the following (most are optional), and\n"
"  for the LFFs, the bandpass is done between the specified fbot and ftop,\n"
"  while for the `whole spectrum' (i.e., fALFF denominator) the bandpass is:\n"
"  done only to exclude the time series mean and the Nyquist frequency:\n"
" (0) Check time series for initial transients [does not alter data]\n"
" (1) Despiking of each time series\n"
" (2) Removal of a constant+linear+quadratic trend in each time series\n"
" (3) Bandpass of data time series\n"
" (4) Bandpass of -ort time series, then detrending of data\n"
"      with respect to the -ort time series\n"
" (5) Bandpass and de-orting of the -dsort dataset,\n"
"      then detrending of the data with respect to -dsort\n"
" (6) Blurring inside the mask [might be slow]\n"
" (7) Local PV calculation     [WILL be slow!]\n"
" (8) L2 normalization         [will be fast.]\n"
" (9) Calculate spectrum and amplitudes, for RSFC parameters.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"--------\n"
"OPTIONS:\n"
"--------\n"
" -despike        = Despike each time series before other processing.\n"
"                   ++ Hopefully, you don't actually need to do this,\n"
"                      which is why it is optional.\n"
" -ort f.1D       = Also orthogonalize input to columns in f.1D\n"
"                   ++ Multiple '-ort' options are allowed.\n"
" -dsort fset     = Orthogonalize each voxel to the corresponding\n"
"                    voxel time series in dataset 'fset', which must\n"
"                    have the same spatial and temporal grid structure\n"
"                    as the main input dataset.\n"
"                   ++ At present, only one '-dsort' option is allowed.\n"
" -nodetrend      = Skip the quadratic detrending of the input that\n"
"                    occurs before the FFT-based bandpassing.\n"
"                   ++ You would only want to do this if the dataset\n"
"                      had been detrended already in some other program.\n"
" -dt dd          = set time step to 'dd' sec [default=from dataset header]\n"
" -nfft N         = set the FFT length to 'N' [must be a legal value]\n"
" -norm           = Make all output time series have L2 norm = 1\n"
"                   ++ i.e., sum of squares = 1\n"
" -mask mset      = Mask dataset\n"
" -automask       = Create a mask from the input dataset\n"
" -blur fff       = Blur (inside the mask only) with a filter\n"
"                    width (FWHM) of 'fff' millimeters.\n"
" -localPV rrr    = Replace each vector by the local Principal Vector\n"
"                    (AKA first singular vector) from a neighborhood\n"
"                    of radius 'rrr' millimiters.\n"
"                   ++ Note that the PV time series is L2 normalized.\n"
"                   ++ This option is mostly for Bob Cox to have fun with.\n"
"\n"
" -input dataset  = Alternative way to specify input dataset.\n"
" -band fbot ftop = Alternative way to specify passband frequencies.\n"
"\n"
" -prefix ppp     = Set prefix name of output dataset. Name of filtered time\n"
"                   series would be, e.g., ppp_LFF+orig.*, and the parameter\n"
"                   outputs are named with obvious suffices.\n"
" -quiet          = Turn off the fun and informative messages. (Why?)\n"
" -no_rs_out      = Don't output processed time series-- just output\n"
"                   parameters (not recommended, since the point of\n"
"                   calculating RSFC params here is to have them be quite\n"
"                   related to the time series themselves which are used for\n"
"                   further analysis)."
" -un_bp_out      = Output the un-bandpassed series as well (default is not \n"
"                   to).  Name would be, e.g., ppp_unBP+orig.* .\n"
"                   with suffix `_unBP'.\n"
" -no_rsfa        = If you don't want RSFA output (default is to do so).\n"
" -bp_at_end      = A (probably unnecessary) switch to have bandpassing be \n"
"                   the very last processing step that is done in the\n"
"                   sequence of steps listed above; at Step 3 above, only \n"
"                   the time series mean and nyquist are BP'ed out, and then\n"
"                   the LFF series is created only after Step 9.  NB: this \n"
"                   probably makes only very small changes for most\n"
"                   processing sequences (but maybe not, depending usage).\n"
"\n"
" -notrans        = Don't check for initial positive transients in the data:\n"
"  *OR*             ++ The test is a little slow, so skipping it is OK,\n"
" -nosat               if you KNOW the data time series are transient-free.\n"
"                   ++ Or set AFNI_SKIP_SATCHECK to YES.\n"
"                   ++ Initial transients won't be handled well by the\n"
"                      bandpassing algorithm, and in addition may seriously\n"
"                      contaminate any further processing, such as inter-\n"
"                      voxel correlations via InstaCorr.\n"
"                   ++ No other tests are made [yet] for non-stationary \n"
"                      behavior in the time series data.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  If you use this program, please reference the introductory/description\n"
"  paper for the FATCAT toolbox:\n"
"        Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional\n"
"        And Tractographic Connectivity Analysis Toolbox. Brain \n"
"        Connectivity 3(5):523-535.\n"
"____________________________________________________________________________\n"
);
		PRINT_AFNI_OMP_USAGE(
" 3dRSFC" ,
" * At present, the only part of 3dRSFC that is parallelized is the\n"
"   '-blur' option, which processes each sub-brick independently.\n"
									) ;
		PRINT_COMPILE_DATE ; exit(0) ;
   }
	
   /*-- startup --*/
	
   mainENTRY("3dRSFC"); machdep();
   AFNI_logger("3dRSFC",argc,argv);
   PRINT_VERSION("3dRSFC (from 3dBandpass by RW Cox): version THETA"); 
	AUTHOR("PA Taylor");
	
   nosat =  AFNI_yesenv("AFNI_SKIP_SATCHECK") ;
	
   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

		if( strcmp(argv[nopt],"-despike") == 0 ){  /* 08 Oct 2010 */
			do_despike++ ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-nfft") == 0 ){
			int nnup ;
			if( ++nopt >= argc ) ERROR_exit("need an argument after -nfft!") ;
			nfft = (int)strtod(argv[nopt],NULL) ;
			nnup = csfft_nextup_even(nfft) ;
			if( nfft < 16 || nfft != nnup )
				ERROR_exit("value %d after -nfft is illegal! Next legal value = %d",nfft,nnup) ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-blur") == 0 ){
			if( ++nopt >= argc ) ERROR_exit("need an argument after -blur!") ;
			blur = strtod(argv[nopt],NULL) ;
			if( blur <= 0.0f ) WARNING_message("non-positive blur?!") ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-localPV") == 0 ){
			if( ++nopt >= argc ) ERROR_exit("need an argument after -localpv!") ;
			pvrad = strtod(argv[nopt],NULL) ;
			if( pvrad <= 0.0f ) WARNING_message("non-positive -localpv?!") ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-prefix") == 0 ){
			if( ++nopt >= argc ) ERROR_exit("need an argument after -prefix!") ;
			prefix = strdup(argv[nopt]) ;
			if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-automask") == 0 ){
			if( mask != NULL ) ERROR_exit("Can't use -mask AND -automask!") ;
			do_automask = 1 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-mask") == 0 ){
			THD_3dim_dataset *mset ;
			if( ++nopt >= argc ) ERROR_exit("Need argument after '-mask'") ;
			if( mask != NULL || do_automask ) ERROR_exit("Can't have two mask inputs") ;
			mset = THD_open_dataset( argv[nopt] ) ;
			CHECK_OPEN_ERROR(mset,argv[nopt]) ;
			DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
			mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
			mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
			if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[nopt]) ;
			nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
			if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
			if( nmask < 1 ) ERROR_exit("Mask is too small to process") ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-norm") == 0 ){
			do_norm = 1 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-quiet") == 0 ){
			verb = 0 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-no_rs_out") == 0 ){ // @@
			SERIES_OUT = 0 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-un_bp_out") == 0 ){ // @@
			UNBP_OUT = 1 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-no_rsfa") == 0 ){ // @@
			DO_RSFA = 0 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-bp_at_end") == 0 ){ // @@
			BP_LAST = 1 ; nopt++ ; continue ;
		}




		if( strcmp(argv[nopt],"-notrans") == 0 || strcmp(argv[nopt],"-nosat") == 0 ){
			nosat = 1 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-ort") == 0 ){
			if( ++nopt >= argc ) ERROR_exit("need an argument after -ort!") ;
			if( ortar == NULL ) INIT_IMARR(ortar) ;
			ortim = mri_read_1D( argv[nopt] ) ;
			if( ortim == NULL ) ERROR_exit("can't read from -ort '%s'",argv[nopt]) ;
			mri_add_name(argv[nopt],ortim) ;
			ADDTO_IMARR(ortar,ortim) ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-dsort") == 0 ){
			THD_3dim_dataset *qset ;
			if( ++nopt >= argc ) ERROR_exit("need an argument after -dsort!") ;
			if( nortset > 0 ) ERROR_exit("only 1 -dsort option is allowed!") ;
			qset = THD_open_dataset(argv[nopt]) ;
			CHECK_OPEN_ERROR(qset,argv[nopt]) ;
			ortset = (THD_3dim_dataset **)realloc(ortset,
															  sizeof(THD_3dim_dataset *)*(nortset+1)) ;
			ortset[nortset++] = qset ;
			nopt++ ; continue ;
		}

		if( strncmp(argv[nopt],"-nodetrend",6) == 0 ){
			qdet = 0 ; nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-dt") == 0 ){
			if( ++nopt >= argc ) ERROR_exit("need an argument after -dt!") ;
			dt = (float)strtod(argv[nopt],NULL) ;
			if( dt <= 0.0f ) WARNING_message("value after -dt illegal!") ;
			nopt++ ; continue ;
		}

		if( strcmp(argv[nopt],"-input") == 0 ){
			if( inset != NULL ) ERROR_exit("Can't have 2 -input options!") ;
			if( ++nopt >= argc ) ERROR_exit("need an argument after -input!") ;
			inset = THD_open_dataset(argv[nopt]) ;
			CHECK_OPEN_ERROR(inset,argv[nopt]) ; 

			nopt++ ; continue ;
		}

		if( strncmp(argv[nopt],"-band",5) == 0 ){
			if( ++nopt >= argc-1 ) ERROR_exit("need 2 arguments after -band!") ;
			if( have_freq ) WARNING_message("second -band option replaces first one!") ;
			fbot = strtod(argv[nopt++],NULL) ;
			ftop = strtod(argv[nopt++],NULL) ;
			have_freq = 1 ; continue ;
		}

		ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
   }

   /** check inputs for reasonablositiness **/

   if( !have_freq ){
		if( nopt+1 >= argc )
			ERROR_exit("Need frequencies on command line after options!") ;
		fbot = (float)strtod(argv[nopt++],NULL) ;
		ftop = (float)strtod(argv[nopt++],NULL) ;
   }

   if( inset == NULL ){
		if( nopt >= argc )
			ERROR_exit("Need input dataset name on command line after options!") ;
		inset = THD_open_dataset(argv[nopt]) ;
		CHECK_OPEN_ERROR(inset,argv[nopt]) ;	 

		nopt++ ;
   }
   DSET_UNMSEC(inset) ;

   if( fbot < 0.0f  ) ERROR_exit("fbot value can't be negative!") ;
   if( ftop <= fbot ) ERROR_exit("ftop value %g must be greater than fbot value %g!",ftop,fbot) ;

   ntime = DSET_NVALS(inset) ;
   if( ntime < 9 ) ERROR_exit("Input dataset is too short!") ;

   if( nfft <= 0 ){
		nfft = csfft_nextup_even(ntime) ;
		if( verb ) INFO_message("Data length = %d  FFT length = %d",ntime,nfft) ;
		(void)THD_bandpass_set_nfft(nfft) ;
   } else if( nfft < ntime ){
		ERROR_exit("-nfft %d is less than data length = %d",nfft,ntime) ;
   } else {
		kk = THD_bandpass_set_nfft(nfft) ;
		if( kk != nfft && verb )
			INFO_message("Data length = %d  FFT length = %d",ntime,kk) ;
   }

   if( dt <= 0.0f ){
		dt = DSET_TR(inset) ;
		if( dt <= 0.0f ){
			WARNING_message("Setting dt=1.0 since input dataset lacks a time axis!") ;
			dt = 1.0f ;
		}
   }
   ftopALL = 1./dt ;// Aug,2016: should solve problem of a too-large
                    // value for THD_bandpass_vectors(), while still
                    // being >f_{Nyquist}

   if( !THD_bandpass_OK(ntime,dt,fbot,ftop,1) ) ERROR_exit("Can't continue!") ;

   nx = DSET_NX(inset); ny = DSET_NY(inset); nz = DSET_NZ(inset); nvox = nx*ny*nz;

   /* check mask, or create it */

   if( verb ) INFO_message("Loading input dataset time series" ) ;
   DSET_load(inset) ;

   if( mask != NULL ){
		if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
			ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( do_automask ){
		mask = THD_automask( inset ) ;
		if( mask == NULL )
			ERROR_message("Can't create -automask from input dataset?") ;
		nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
		if( verb ) INFO_message("Number of voxels in automask = %d",nmask);
		if( nmask < 1 ) ERROR_exit("Automask is too small to process") ;

   } else {
		mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
		memset(mask,1,sizeof(byte)*nvox) ;
		// if( verb ) // @@ alert if aaaalllllll vox are going to be analyzed!
		INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /* A simple check of dataset quality [08 Feb 2010] */

   if( !nosat ){
		float val ;
		INFO_message(
						 "Checking dataset for initial transients [use '-notrans' to skip this test]") ;
		val = THD_saturation_check(inset,mask,0,0) ; kk = (int)(val+0.54321f) ;
		if( kk > 0 )
			ININFO_message(
								"Looks like there %s %d non-steady-state initial time point%s :-(" ,
								((kk==1) ? "is" : "are") , kk , ((kk==1) ? " " : "s") ) ;
		else if( val > 0.3210f )  /* don't ask where this threshold comes from! */
			ININFO_message(
								"MAYBE there's an initial positive transient of 1 point, but it's hard to tell\n") ;
		else
			ININFO_message("No widespread initial positive transient detected :-)") ;
   }

   /* check -dsort inputs for match to inset */

   for( kk=0 ; kk < nortset ; kk++ ){
		if( DSET_NX(ortset[kk])    != nx ||
			 DSET_NY(ortset[kk])    != ny ||
			 DSET_NZ(ortset[kk])    != nz ||
			 DSET_NVALS(ortset[kk]) != ntime )
			ERROR_exit("-dsort %s doesn't match input dataset grid" ,
						  DSET_BRIKNAME(ortset[kk]) ) ;
   }

   /* convert input dataset to a vectim, which is more fun */

	// @@ convert BP'ing ftop/bot into indices for the DFT (below)
	delf = 1.0/(ntime*dt); 
	ind_low = (int) rint(fbot/delf);
	ind_high = (int) rint(ftop/delf);
	if( ntime % 2 ) // nyquist number
		N_ny = (ntime-1)/2;
	else
		N_ny = ntime/2;
	sqnt = sqrt(ntime);
	nt_fac = sqrt(ntime*(ntime-1));

	// @@ if BP_LAST==0:
	// now we go through twice, doing LFF bandpass for NumDen==0 and
	// `full spectrum' processing for NumDen==1.
	// if BP_LAST==1:
	// now we go through once, doing only `full spectrum' processing
	for( NumDen=0 ; NumDen<2 ; NumDen++) {
		//if( NumDen==1 ){ // full spectrum
		//	fbot = fbotALL;
		//	ftop = ftopALL;
		//}
		
		// essentially, just doesn't BP here, and the perfect filtering at end
		// is used for both still; this makes the final output spectrum
		// contain only frequencies in range of 0.01-0.08
		if( BP_LAST==1 )
			INFO_message("Only doing filtering to LFFs at end!");
		
		
		mrv = THD_dset_to_vectim( inset , mask , 0 ) ;
		if( mrv == NULL ) ERROR_exit("Can't load time series data!?") ;
		if( NumDen==1 )
			DSET_unload(inset) ; // @@ only unload on 2nd pass

		/* similarly for the ort vectors */

		if( ortar != NULL ){
			for( kk=0 ; kk < IMARR_COUNT(ortar) ; kk++ ){
				ortim = IMARR_SUBIM(ortar,kk) ;
				if( ortim->nx < ntime )
					ERROR_exit("-ort file %s is shorter than input dataset time series",
								  ortim->name ) ;
				ort  = (float **)realloc( ort , sizeof(float *)*(nort+ortim->ny) ) ;
				for( vv=0 ; vv < ortim->ny ; vv++ )
					ort[nort++] = MRI_FLOAT_PTR(ortim) + ortim->nx * vv ;
			}
		}

		/* all the real work now */

		if( do_despike ){
			int_pair nsp ;
			if( verb ) INFO_message("Testing data time series for spikes") ;
			nsp = THD_vectim_despike9( mrv ) ;
			if( verb ) ININFO_message(" -- Squashed %d spikes from %d voxels",nsp.j,nsp.i) ;
		}

		if( verb ) INFO_message("Bandpassing data time series") ;

		if( (BP_LAST==0) && (NumDen==0) )
			(void)THD_bandpass_vectim( mrv , dt,fbot,ftop , qdet , nort,ort ) ;
		else
			(void)THD_bandpass_vectim( mrv , dt,fbotALL,ftopALL, qdet,nort,ort ) ;

		/* OK, maybe a little more work */

		if( nortset == 1 ){
			MRI_vectim *orv ;
			orv = THD_dset_to_vectim( ortset[0] , mask , 0 ) ;
			if( orv == NULL ){
				ERROR_message("Can't load -dsort %s",DSET_BRIKNAME(ortset[0])) ;
			} else {
				float *dp , *mvv , *ovv , ff ;
				if( verb ) INFO_message("Orthogonalizing to bandpassed -dsort") ;
				//(void)THD_bandpass_vectim( orv , dt,fbot,ftop , qdet , nort,ort ) ; //@@
				if( (BP_LAST==0) && (NumDen==0) )
					(void)THD_bandpass_vectim(orv,dt,fbot,ftop,qdet,nort,ort);
				else
					(void)THD_bandpass_vectim(orv,dt,fbotALL,ftopALL,qdet,nort,ort);

				THD_vectim_normalize( orv ) ;
				dp = malloc(sizeof(float)*mrv->nvec) ;
				THD_vectim_vectim_dot( mrv , orv , dp ) ;
				for( vv=0 ; vv < mrv->nvec ; vv++ ){
					ff = dp[vv] ;
					if( ff != 0.0f ){
						mvv = VECTIM_PTR(mrv,vv) ; ovv = VECTIM_PTR(orv,vv) ;
						for( kk=0 ; kk < ntime ; kk++ ) mvv[kk] -= ff*ovv[kk] ;
					}
				}
				VECTIM_destroy(orv) ; free(dp) ;
			}
		}

		if( blur > 0.0f ){
			if( verb )
				INFO_message("Blurring time series data spatially; FWHM=%.2f",blur) ;
			mri_blur3D_vectim( mrv , blur ) ;
		}
		if( pvrad > 0.0f ){
			if( verb )
				INFO_message("Local PV-ing time series data spatially; radius=%.2f",pvrad) ;
			THD_vectim_normalize( mrv ) ;
			THD_vectim_localpv( mrv , pvrad ) ;
		}
		if( do_norm && pvrad <= 0.0f ){
			if( verb ) INFO_message("L2 normalizing time series data") ;
			THD_vectim_normalize( mrv ) ;
		}

		/* create output dataset, populate it, write it, then quit */
		if( (NumDen==0) ) { // @@ BP'ed version;  will do filt if BP_LAST

			if(BP_LAST) // do bandpass here for BP_LAST
				(void)THD_bandpass_vectim(mrv,dt,fbot,ftop,qdet,0,NULL);

			if( verb ) INFO_message("Creating output dataset in memory, then writing it") ;
			outset = EDIT_empty_copy(inset) ;
			if(SERIES_OUT){
				sprintf(out_lff,"%s_LFF",prefix); 
				EDIT_dset_items( outset , ADN_prefix,out_lff , ADN_none ) ;
				tross_Copy_History( inset , outset ) ;
				tross_Make_History( "3dBandpass" , argc,argv , outset ) ;
			}
			for( vv=0 ; vv < ntime ; vv++ )
				EDIT_substitute_brick( outset , vv , MRI_float , NULL ) ;
		
#if 1
			THD_vectim_to_dset( mrv , outset ) ;
#else
			AFNI_OMP_START ;
#pragma omp parallel
			{ float *far , *var ; int *ivec=mrv->ivec ; int vv,kk ;
#pragma omp for
				for( vv=0 ; vv < ntime ; vv++ ){
					far = DSET_BRICK_ARRAY(outset,vv) ; var = mrv->fvec + vv ;
					for( kk=0 ; kk < nmask ; kk++ ) far[ivec[kk]] = var[kk*ntime] ;
				}
			}
			AFNI_OMP_END ;
#endif
			VECTIM_destroy(mrv) ;
			if(SERIES_OUT){ // @@
				DSET_write(outset) ; if( verb ) WROTE_DSET(outset) ;
			}
		}
		else{ // @@ non-BP'ed version
			if( verb ) INFO_message("Creating output dataset 2 in memory") ;

			// do this here because LFF version was also BP'ed at end.
			if(BP_LAST) // do bandpass here for BP_LAST
				(void)THD_bandpass_vectim(mrv,dt,fbotALL,ftopALL,qdet,0,NULL);

			outsetALL = EDIT_empty_copy(inset) ;
			if(UNBP_OUT){ 
				sprintf(out_unBP,"%s_unBP",prefix); 
				EDIT_dset_items( outsetALL, ADN_prefix, out_unBP, ADN_none );
				tross_Copy_History( inset , outsetALL ) ;
				tross_Make_History( "3dRSFC" , argc,argv , outsetALL ) ;
			}
			for( vv=0 ; vv < ntime ; vv++ )
				EDIT_substitute_brick( outsetALL , vv , MRI_float , NULL ) ;
		
#if 1
			THD_vectim_to_dset( mrv , outsetALL ) ;
#else
			AFNI_OMP_START ;
#pragma omp parallel
			{ float *far , *var ; int *ivec=mrv->ivec ; int vv,kk ;
#pragma omp for
				for( vv=0 ; vv < ntime ; vv++ ){
					far = DSET_BRICK_ARRAY(outsetALL,vv) ; var = mrv->fvec + vv ;
					for( kk=0 ; kk < nmask ; kk++ ) far[ivec[kk]] = var[kk*ntime] ;
				}
			}
			AFNI_OMP_END ;
#endif
			VECTIM_destroy(mrv) ;
			if(UNBP_OUT){ 
				DSET_write(outsetALL) ; if( verb ) WROTE_DSET(outsetALL) ;
			}
		}
	}// end of NumDen loop


	// @@
	INFO_message("Starting the (f)ALaFFel calcs") ;

	// allocations
	series1 = (double *)calloc(ntime,sizeof(double)); 
	series2 = (double *)calloc(ntime,sizeof(double)); 
	xx1 = (double *)calloc(2*ntime,sizeof(double)); 
	xx2 = (double *)calloc(2*ntime,sizeof(double)); 
	alff = (float *)calloc(nvox,sizeof(float)); 
	malff = (float *)calloc(nvox,sizeof(float)); 
	falff = (float *)calloc(nvox,sizeof(float)); 

	if( (series1 == NULL) || (series2 == NULL) 
		 || (xx1 == NULL) || (xx2 == NULL) 
		 || (alff == NULL) || (malff == NULL) || (falff == NULL)) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}
	if(DO_RSFA) {
		rsfa = (float *)calloc(nvox,sizeof(float)); 
		mrsfa = (float *)calloc(nvox,sizeof(float)); 
		frsfa = (float *)calloc(nvox,sizeof(float)); 
		if( (rsfa == NULL) || (mrsfa == NULL) || (frsfa == NULL)) { 
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(123);
		}	
	}
	
	
	work = gsl_fft_real_workspace_alloc (ntime);
	real1 = gsl_fft_real_wavetable_alloc (ntime);
	real2 = gsl_fft_real_wavetable_alloc (ntime);
	gsl_complex_packed_array compl_freqs1 = xx1;
	gsl_complex_packed_array compl_freqs2 = xx2;




	// *********************************************************************
	// *********************************************************************
	// **************    Falafelling = ALFF/fALFF calcs    *****************
	// *********************************************************************
	// *********************************************************************

	// Be now have the BP'ed data set (outset) and the non-BP'ed one
	// (outsetALL).  now we'll FFT both, get amplitudes in appropriate
	// ranges, and calculate:  ALFF, mALFF, fALFF,

	ctr = 0;
	for( kk=0; kk<nvox ; kk++) {
		if(mask[kk]) {
			
			// BP one, and unBP one, either for BP_LAST or !BP_LAST
			for( m=0 ; m<ntime ; m++ ) {
				series1[m] = THD_get_voxel(outset,kk,m);
				series2[m] = THD_get_voxel(outsetALL,kk,m);
			}
			
			
			mm = gsl_fft_real_transform(series1, 1, ntime, real1, work);
			mm = gsl_fft_halfcomplex_unpack(series1, compl_freqs1, 1, ntime);
			mm = gsl_fft_real_transform(series2, 1, ntime, real2, work);
			mm = gsl_fft_halfcomplex_unpack(series2, compl_freqs2, 1, ntime);

			numer = 0.0f; 
			denom = 0.0f;
			de_rsfa = 0.0f;
			nu_rsfa = 0.0f;
			for( m=1 ; m<N_ny ; m++ ) {
				mm = 2*m;
				pow2 = compl_freqs2[mm]*compl_freqs2[mm] +
					compl_freqs2[mm+1]*compl_freqs2[mm+1]; // power
				//pow2*=2;// factor of 2 since ampls are even funcs
				denom+= (float) sqrt(pow2); // amplitude 
				de_rsfa+= (float) pow2;
				
				if( ( m>=ind_low ) && ( m<=ind_high ) ){
					pow1 = compl_freqs1[mm]*compl_freqs1[mm]+
						compl_freqs1[mm+1]*compl_freqs1[mm+1];
					//pow1*=2;
					numer+= (float) sqrt(pow1);
					nu_rsfa+= (float) pow1;
				}
			}

			if( denom>0.000001 )
			  falff[kk] = numer/denom;
			else
			  falff[kk] = 0.;
			alff[kk] = 2*numer/sqnt;// factor of 2 since ampl is even funct
			meanALFF+= alff[kk];

			if(DO_RSFA){
			  nu_rsfa = sqrt(2*nu_rsfa); // factor of 2 since ampls 
			  de_rsfa = sqrt(2*de_rsfa); // are even funcs
			  if( de_rsfa>0.000001 )
			    frsfa[kk] = nu_rsfa/de_rsfa;
			  else
			    frsfa[kk]=0.;
			  rsfa[kk] = nu_rsfa/nt_fac;
			  meanRSFA+= rsfa[kk];
			}
			
			ctr+=1;
		}
	}
	meanALFF/= ctr;
	meanRSFA/= ctr;

	gsl_fft_real_wavetable_free(real1);
	gsl_fft_real_wavetable_free(real2);
	gsl_fft_real_workspace_free(work);

	// ALFFs divided by mean of brain value
	for( kk=0 ; kk<nvox ; kk++ ) 
		if(mask[kk]){
			malff[kk] = alff[kk]/meanALFF;
			if(DO_RSFA)
				mrsfa[kk] = rsfa[kk]/meanRSFA;
		}
	// **************************************************************
	// **************************************************************
	//                 Store and output
	// **************************************************************
	// **************************************************************
	
	outsetALFF = EDIT_empty_copy( inset ) ; 
	sprintf(out_alff,"%s_ALFF",prefix); 
	EDIT_dset_items( outsetALFF,
                    ADN_nvals, 1,
						  ADN_datum_all , MRI_float , 
						  ADN_prefix    , out_alff,
						  ADN_none ) ;
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetALFF)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outsetALFF));
	EDIT_substitute_brick(outsetALFF, 0, MRI_float, alff); 
	alff=NULL;
	THD_load_statistics(outsetALFF);
	tross_Make_History("3dRSFC", argc, argv, outsetALFF);
	THD_write_3dim_dataset(NULL, NULL, outsetALFF, True);

	outsetfALFF = EDIT_empty_copy( inset ) ;
	sprintf(out_falff,"%s_fALFF",prefix); 
	EDIT_dset_items( outsetfALFF,
                    ADN_nvals, 1,
						  ADN_datum_all , MRI_float , 
						  ADN_prefix    , out_falff,
						  ADN_none ) ;
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetfALFF)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outsetfALFF));
	EDIT_substitute_brick(outsetfALFF, 0, MRI_float, falff); 
	falff=NULL;
	THD_load_statistics(outsetfALFF);
	tross_Make_History("3dRSFC", argc, argv, outsetfALFF);
	THD_write_3dim_dataset(NULL, NULL, outsetfALFF, True);



	outsetmALFF = EDIT_empty_copy( inset ) ;
	sprintf(out_malff,"%s_mALFF",prefix); 
	EDIT_dset_items( outsetmALFF,
                    ADN_nvals, 1,
                    ADN_datum_all , MRI_float , 
						  ADN_prefix    , out_malff,
						  ADN_none ) ;
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetmALFF)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outsetmALFF));
	EDIT_substitute_brick(outsetmALFF, 0, MRI_float, malff); 
	malff=NULL;
	THD_load_statistics(outsetmALFF);
	tross_Make_History("3dRSFC", argc, argv, outsetmALFF);
	THD_write_3dim_dataset(NULL, NULL, outsetmALFF, True);

	if(DO_RSFA){
     outsetRSFA = EDIT_empty_copy( inset ) ;
		sprintf(out_rsfa,"%s_RSFA",prefix); 
		EDIT_dset_items( outsetRSFA,
                       ADN_nvals, 1,
                       ADN_datum_all , MRI_float , 
							  ADN_prefix    , out_rsfa,
							  ADN_none ) ;
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetRSFA)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetRSFA));
		EDIT_substitute_brick(outsetRSFA, 0, MRI_float, rsfa); 
		rsfa=NULL;
		THD_load_statistics(outsetRSFA);
		tross_Make_History("3dRSFC", argc, argv, outsetRSFA);
		THD_write_3dim_dataset(NULL, NULL, outsetRSFA, True);
		
      outsetfRSFA = EDIT_empty_copy( inset ) ;
		sprintf(out_frsfa,"%s_fRSFA",prefix); 
		EDIT_dset_items( outsetfRSFA,
                       ADN_nvals, 1,
                       ADN_datum_all , MRI_float , 
							  ADN_prefix    , out_frsfa,
							  ADN_none ) ;
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetfRSFA)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetfRSFA));
		EDIT_substitute_brick(outsetfRSFA, 0, MRI_float, frsfa); 
		frsfa=NULL;
		THD_load_statistics(outsetfRSFA);
		tross_Make_History("3dRSFC", argc, argv, outsetfRSFA);
		THD_write_3dim_dataset(NULL, NULL, outsetfRSFA, True);
		
		outsetmRSFA = EDIT_empty_copy( inset ) ; 
		sprintf(out_mrsfa,"%s_mRSFA",prefix); 
		EDIT_dset_items( outsetmRSFA,
                       ADN_nvals, 1,
                       ADN_datum_all , MRI_float , 
							  ADN_prefix    , out_mrsfa,
							  ADN_none ) ;
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetmRSFA)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetmRSFA));
		EDIT_substitute_brick(outsetmRSFA, 0, MRI_float, mrsfa); 
		mrsfa=NULL;
		THD_load_statistics(outsetmRSFA);
		tross_Make_History("3dRSFC", argc, argv, outsetmRSFA);
		THD_write_3dim_dataset(NULL, NULL, outsetmRSFA, True);
	}



	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************

	DSET_delete(inset);
	DSET_delete(outsetALL);
	DSET_delete(outset);
	DSET_delete(outsetALFF);
	DSET_delete(outsetmALFF);
	DSET_delete(outsetfALFF);
	DSET_delete(outsetRSFA);
	DSET_delete(outsetmRSFA);
	DSET_delete(outsetfRSFA);

	free(inset);
	free(outsetALL);
	free(outset);
	free(outsetALFF);
	free(outsetmALFF);
	free(outsetfALFF);
	free(outsetRSFA);
	free(outsetmRSFA);
	free(outsetfRSFA);

	free(rsfa);
	free(mrsfa);
	free(frsfa);
	free(alff);
	free(malff);
	free(falff);
	free(mask);
	free(series1);
	free(series2);
	free(xx1);
	free(xx2);

	exit(0) ;
}

/*----------------------------------------------------------------------------*/

void THD_vectim_localpv( MRI_vectim *mrv , float rad )
{
	unsigned short xran[3] = { 32701 , 22013 , 0x330e } ;
	MCW_cluster *nbhd ;
	int iv,kk,nn,nx,ny,nz,nxy , nind , *ind , xx,yy,zz , aa,bb,cc ;
	float *pv , *fv ;
	MRI_vectim *qrv ;  /* workspace to hold results */

	nbhd = MCW_spheremask( mrv->dx,mrv->dy,mrv->dz , rad ) ;
	if( nbhd->num_pt <= 1 ){ THD_vectim_normalize(mrv); return; }
	ind = (int *)malloc(sizeof(int)*nbhd->num_pt) ;
	pv  = (float *)malloc(sizeof(float)*mrv->nvals) ;

	kk = thd_floatscan( mrv->nvec*mrv->nvals , mrv->fvec ) ;
	if( kk > 0 )
		WARNING_message("fixed %d float error%s before localPV",kk,(kk==1)?"\0":"s");

	qrv = THD_vectim_copy(mrv) ;  /* 08 Apr 2010: workspace */

	nx = mrv->nx ; ny = mrv->ny ; nz = mrv->nz ; nxy = nx*ny ;
	for( iv=0 ; iv < mrv->nvec ; iv++ ){
		ind[0] = kk = mrv->ivec[iv] ; IJK_TO_THREE(kk,aa,bb,cc,nx,nxy) ;
		for( nind=nn=1 ; nn < nbhd->num_pt ; nn++ ){
			xx = aa + nbhd->i[nn] ; if( xx < 0 || xx >= nx ) continue ;
			yy = bb + nbhd->j[nn] ; if( yy < 0 || yy >= ny ) continue ;
			zz = cc + nbhd->k[nn] ; if( zz < 0 || zz >= nz ) continue ;
			ind[nind] = THREE_TO_IJK(xx,yy,zz,nx,nxy) ; nind++ ;
		}

		nn = THD_vectim_subset_pv( mrv , nind,ind , pv , xran ) ;
		fv = VECTIM_PTR(qrv,iv) ; /* 08 Apr 2010: result goes in here, not mrv! */
		if( nn > 0 ){
			for( kk=0 ; kk < mrv->nvals ; kk++ ) fv[kk] = pv[kk] ;
		} else {                                             /* should not happen */
			THD_normalize( mrv->nvals , fv ) ;
		}
	}

	memcpy( mrv->fvec , qrv->fvec , sizeof(float)*mrv->nvec*mrv->nvals ) ;
	VECTIM_destroy(qrv) ;

	kk = thd_floatscan( mrv->nvec*mrv->nvals , mrv->fvec ) ;
	if( kk > 0 )
		WARNING_message("fixed %d float error%s after localPV",kk,(kk==1)?"\0":"s");

	KILL_CLUSTER(nbhd) ; free(ind) ; free(pv) ; return ;
}

/*----------------------------------------------------------------------------*/

int THD_vectim_subset_pv( MRI_vectim *mrv, int nind, int *ind,
								  float *ar, unsigned short xran[] )
{
   int nvals , jj,kk,nkk ; register int ii ; float *fv , **xvec ;

   if( mrv == NULL || nind <= 0 || ind == NULL || ar == NULL ) return 0 ;

   nvals = mrv->nvals ;
   xvec  = (float **)malloc(sizeof(float *)*nind) ;

   for( nkk=jj=0 ; jj < nind ; jj++ ){
		kk = THD_vectim_ifind( ind[jj] , mrv ) ; if( kk < 0 ) continue ;
		xvec[nkk] = VECTIM_PTR(mrv,kk) ; nkk++ ;
   }

   if( nkk == 0 ){ free(xvec) ; return 0 ; }

   if( nkk == 1 ){
		for( ii=0 ; ii < nvals ; ii++ ) ar[ii] = xvec[0][ii] ;
		THD_normalize( nvals , ar ) ;
		free(xvec) ; return 1 ;
   }

   (void)principal_vector( nvals,nkk , 1,xvec , ar , xvec[0] , NULL,xran ) ;
   return nkk ;
}
