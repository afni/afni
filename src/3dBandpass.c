#include "mrilib.h"

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
   THD_3dim_dataset *inset=NULL , *outset ;
   char *prefix="bandpass" ;
   byte *mask=NULL ;
   int mask_nx=0,mask_ny=0,mask_nz=0,nmask , verb=1 , 
       nx,ny,nz,nvox , nfft=0 , kk ;
   float **vec , **ort=NULL ; int nort=0 , vv , nopt , ntime  ;
   MRI_vectim *mrv ;
   float pvrad=0.0f ; int nosat=0 ;
   int do_despike=0 ;

   /*-- help? --*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "\n"
       "--------------------------------------------------------------------------\n"
       "** NOTA BENE:  For the purpose of preparing resting-state FMRI datasets **\n"
       "** for analysis (e.g., with 3dGroupInCorr),  this program is now mostly **\n"
       "** superseded by the afni_proc.py script.  See the 'afni_proc.py -help' **\n"
       "** section 'Resting state analysis (modern)' to get our current rs-FMRI **\n"
       "** pre-processing recommended sequence of steps. -- RW Cox, et alii.    **\n"
       "--------------------------------------------------------------------------\n"
       "** If you insist on doing your own bandpassing, I now recommend using   **\n"
       "** program 3dTproject instead of this program.  3dTproject also can do  **\n"
       "** censoring and other nuisance regression at the same time -- RW Cox.  **\n"
       "--------------------------------------------------------------------------\n"
       "\n"
       "Usage: 3dBandpass [options] fbot ftop dataset\n"
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
       "  ++ You could use 3dBandpass followed by 3dcalc to get the same effect.\n"
       "  ++ If you are understand what you are doing, that is.\n"
       "  ++ Of course, that is the AFNI way -- if you don't want to\n"
       "     understand what you are doing, use Some other PrograM, and\n"
       "     you can still get Fine StatisticaL maps.\n"
       "\n"
       "* 3dBandpass will fail if fbot and ftop are too close for comfort.\n"
       "  ++ Which means closer than one frequency grid step df,\n"
       "     where df = 1 / (nfft * dt) [of course]\n"
       "\n"
       "* The actual FFT length used will be printed, and may be larger\n"
       "   than the input time series length for the sake of efficiency.\n"
       "  ++ The program will use a power-of-2, possibly multiplied by\n"
       "     a power of 3 and/or 5 (up to and including the 3rd power of\n"
       "     each of these: 3, 9, 27, and 5, 25, 125).\n"
       "\n"
       "* Note that the results of combining 3dDetrend and 3dBandpass will\n"
       "   depend on the order in which you run these programs.  That's why\n"
       "   3dBandpass has the '-ort' and '-dsort' options, so that the\n"
       "   time series filtering can be done properly, in one place.\n"
       "\n"
       "* The output dataset is stored in float format.\n"
       "\n"
       "* The order of processing steps is the following (most are optional):\n"
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
       "\n"
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
       " -prefix ppp     = Set prefix name of output dataset.\n"
       " -quiet          = Turn off the fun and informative messages. (Why?)\n"
       "\n"
       " -notrans        = Don't check for initial positive transients in the data:\n"
       "  *OR*             ++ The test is a little slow, so skipping it is OK,\n"
       " -nosat               if you KNOW the data time series are transient-free.\n"
       "                   ++ Or set AFNI_SKIP_SATCHECK to YES.\n"
       "                   ++ Initial transients won't be handled well by the\n"
       "                      bandpassing algorithm, and in addition may seriously\n"
       "                      contaminate any further processing, such as inter-voxel\n"
       "                      correlations via InstaCorr.\n"
       "                   ++ No other tests are made [yet] for non-stationary behavior\n"
       "                      in the time series data.\n"
     ) ;
     PRINT_AFNI_OMP_USAGE(
       "3dBandpass" ,
       "* At present, the only part of 3dBandpass that is parallelized is the\n"
       "  '-blur' option, which processes each sub-brick independently.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- startup --*/

   mainENTRY("3dBandpass"); machdep();
   AFNI_logger("3dBandpass",argc,argv);
   PRINT_VERSION("3dBandpass"); AUTHOR("RW Cox");

   WARNING_message("(-: For most purposes, 3dTproject is superior to 3dBandpass :-)") ;
   WARNING_message("(-: Even better is to use afni_proc.py for pre-processing!! :-)") ;

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
     CHECK_OPEN_ERROR(inset,argv[nopt]) ; nopt++ ;
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
     if( verb ) INFO_message("No mask ==> processing all %d voxels",nvox);
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

   mrv = THD_dset_to_vectim( inset , mask , 0 ) ;
   if( mrv == NULL ) ERROR_exit("Can't load time series data!?") ;
   DSET_unload(inset) ;

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

   /* check whether processing leaves any DoF remaining  18 Mar 2015 [rickr] */
   {
      int nbprem = THD_bandpass_remain_dim(ntime, dt, fbot, ftop, 1);
      int bpused, nremain;
      int wlimit;               /* warning limit */

      bpused = ntime - nbprem;  /* #dim lost in bandpass step */

      nremain = nbprem - nort;  /* #dim left in output */
      if( nortset == 1 ) nremain--;
      nremain -= (qdet+1);

      if( verb ) INFO_message("%d dimensional data reduced to %d by:\n"
                    "    %d (bandpass), %d (-ort), %d (-dsort), %d (detrend)",
                    ntime, nremain, bpused, nort, nortset?1:0, qdet+1);

      /* possibly warn (if 95% lost) user or fail */
      wlimit = ntime/20;
      if( wlimit < 3 ) wlimit = 3;
      if( nremain < wlimit && nremain > 0 )
         WARNING_message("dimensionality reduced from %d to %d, be careful!",
                         ntime, nremain);
      if( nremain <= 0 ) /* FAILURE */
         ERROR_exit("dimensionality reduced from %d to %d, failing!",
                    ntime, nremain);
   }

   /* all the real work now */

   if( do_despike ){
     int_pair nsp ;
     if( verb ) INFO_message("Testing data time series for spikes") ;
     nsp = THD_vectim_despike9( mrv ) ;
     if( verb ) ININFO_message(" -- Squashed %d spikes from %d voxels",nsp.j,nsp.i) ;
   }

   if( verb ) INFO_message("Bandpassing data time series") ;
   (void)THD_bandpass_vectim( mrv , dt,fbot,ftop , qdet , nort,ort ) ;

   /* OK, maybe a little more work */

   if( nortset == 1 ){
     MRI_vectim *orv ;
     orv = THD_dset_to_vectim( ortset[0] , mask , 0 ) ;
     if( orv == NULL ){
       ERROR_message("Can't load -dsort %s",DSET_BRIKNAME(ortset[0])) ;
     } else {
       float *dp , *mvv , *ovv , ff ;
       if( verb ) INFO_message("Orthogonalizing to bandpassed -dsort") ;
       (void)THD_bandpass_vectim( orv , dt,fbot,ftop , qdet , nort,ort ) ;
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

   if( verb ) INFO_message("Creating output dataset in memory, then writing it") ;
   outset = EDIT_empty_copy(inset) ;
   /* do not copy scalars    11 Sep 2015 [rickr] */
   EDIT_dset_items( outset , ADN_prefix,prefix ,
                             ADN_brick_fac,NULL ,
                    ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dBandpass" , argc,argv , outset ) ;

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
   DSET_write(outset) ; if( verb ) WROTE_DSET(outset) ;

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

  kk = thd_floatscan( (size_t)mrv->nvec*(size_t)mrv->nvals , mrv->fvec ) ;
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

  memcpy( mrv->fvec , qrv->fvec , sizeof(float)*(size_t)mrv->nvec*(size_t)mrv->nvals ) ;
  VECTIM_destroy(qrv) ;

  kk = thd_floatscan( (size_t)mrv->nvec*(size_t)mrv->nvals , mrv->fvec ) ;
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
