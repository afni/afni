#include "mrilib.h"

static char prefix[THD_MAX_PREFIX] = "pgram" ;

static float taper = 0.1f ;

static void PGRAM_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbrik, float *val ) ;

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, nvals , nfft , nbin ;

   /*----- Help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dPeriodogram [options] dataset\n"
             "Computes the periodogram of each voxel time series.\n"
             "(Squared FFT = a crude estimate of the power spectrum)\n"
             "\n"
             "Options:\n"
             " -prefix p = use string 'p' for the prefix of the\n"
             "               output dataset [DEFAULT = 'pgram']\n"
             " -taper    = fraction of data to taper [DEFAULT = 0.1]\n"
             "\n"
             "Notes:\n"
             "* Output is in float format.\n"
             "* FFT length is chosen by the program.\n"
             "* There is no '-mask' option.  The hyper-clever user could\n"
             "   use something like\n"
             "     '3dcalc( -a dset+orig -b mask+orig -expr a*b )'\n"
             "   to apply a binary mask on the command line.\n"
             "* Data is not scaled to be exactly like the Power plugin.\n"
             "* This is a really quick hack for DH and PB and SfN.\n"
             "* Author = RWCox -- who wants his bribe now (should I say that?)\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dPeriodogram main"); machdep(); AFNI_logger("3dPeriodogram",argc,argv);
   PRINT_VERSION("3dPeriodogram"); AUTHOR("RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!");
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("%s is not a valid prefix!",prefix);
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-taper") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-taper needs an argument!");
         taper = (float)strtod(argv[nopt],NULL) ;
         if( taper < 0.0f || taper > 1.0f ) ERROR_exit("-taper value out of range 0..1");
         nopt++ ; continue ;
      }

      /*-- Quien sabe'? --*/

      ERROR_exit("Unknown option: %s",argv[nopt]) ;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit("No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s",argv[nopt]);
   DSET_load(old_dset) ;
   if( !DSET_LOADED(old_dset) )
     ERROR_exit("Can't load dataset %s",argv[nopt]) ;

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 9 )
      ERROR_exit("Number of time points = %d < min value of 9!",nvals) ;

   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing inputs on command line ignored: %s ...",argv[nopt]) ;

   nfft = csfft_nextup_even(nvals) ;
   nbin = nfft / 2 ;
   INFO_message("Lengths: Input=%d  FFT=%d  Output=%d",nvals,nfft,nbin) ;

   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    MRI_float ,            /* output datum  */
                    0 ,                    /* ignore count  */
                    1 ,                    /* detrend (yes) */
                    nbin ,                 /* number of briks in output */
                    PGRAM_tsfunc ,         /* timeseries processor function */
                    NULL                   /* extra data for tsfunc */
                 ) ;

   if( new_dset != NULL ){
     float tr=DSET_TR(old_dset) , df ;

     if( DSET_TIMEUNITS(old_dset) == UNITS_MSEC_TYPE ) tr *= 0.001f ;

     if( tr <= 0.0f ) tr = 1.0f ;
     df = 1.0f / (nfft*tr) ;

     tross_Copy_History( old_dset , new_dset ) ;
     tross_Make_History( "3dPeriodogram" , argc,argv , new_dset ) ;
     EDIT_dset_items( new_dset ,
                         ADN_ntt    , nbin ,
                         ADN_ttorg  , df ,
                         ADN_ttdel  , df ,
                         ADN_tunits , UNITS_HZ_TYPE ,
                       NULL ) ;

     DSET_write( new_dset ) ; WROTE_DSET( new_dset ) ;
   } else {
     ERROR_exit("Unable to compute output dataset!?") ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void PGRAM_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbrik, float *val )
{
   static float    *tar=NULL ;
   static complex *cxar=NULL ;
   static int     first=1 ;

   int nbin=nbrik , nfft=nbin*2 ;
   float pfact ;

   register int kk ;

ENTRY("PGRAM_tsfunc") ;

   /** is this a "notification"? **/

   if( val == NULL ){
      if( npts > 0 ){  /* the "start notification" */

      } else {  /* the "end notification" */

INFO_message("free-ing") ;
        if( tar  != NULL ){ free(tar) ; tar  = NULL; }
        if( cxar != NULL ){ free(cxar); cxar = NULL; }
        first = 1 ;

      }
      EXRETURN ;
   }

   if( first ){
     int ntaper , ktbot ; float phi ;

INFO_message("malloc-ing: nfft=%d",nfft) ;
     cxar = (complex *)malloc(sizeof(complex)*nfft) ;
     tar  = (float *  )malloc(sizeof(float  )*nfft) ;
     ntaper = (int)(0.5 * taper * npts + 0.49f) ; /* will taper data over */
     phi    = PI / MAX(ntaper,1) ;                /* kk=0..ntaper-1 on left */
     ktbot  = npts - ntaper ;                     /* kk=ktbot..npts1 on right */
     pfact  = 0.0f ;                              /* sum of taper**2 */
INFO_message("taper=%g npts=%d ntaper=%d phi=%g",taper,npts,ntaper,phi) ;

     for( kk=0 ; kk < npts ; kk++ ){                        /* Hamming-ize */
       if( kk < ntaper )
         tar[kk] = 0.54f - 0.46f * cosf(kk*phi) ;           /* ramp up */
       else if( kk >= ktbot )
         tar[kk] = 0.54f + 0.46f * cosf((kk-ktbot+1)*phi) ; /* ramp down */
       else
         tar[kk] = 1.0f ;                                   /* in the middle */

       pfact += tar[kk]*tar[kk] ;
     }
     pfact = 1.0f / pfact ;
INFO_message("pfact=%g",pfact) ;
     first = 0 ;
   }

   /** do the work **/

   for( kk=0 ; kk < npts && ts[kk] == 0.0f ; kk++ ) ; /*nada*/
   if( kk == npts ){
     for( kk=0 ; kk < nbin ; kk++ ) val[kk] = 0.0f ;  /* input was all zero */
     EXRETURN ;
   }

   for( kk=0 ; kk < nfft ; kk++ ){
     if( kk < npts ) cxar[kk].r = tar[kk] * ts[kk] ;
     else            cxar[kk].r = 0.0f ;
     cxar[kk].i = 0.0f ;
   }

   csfft_cox( -1 , nfft , cxar ) ;

   for( kk=0 ; kk < nbin ; kk++ ){
     val[kk] = CSQR(cxar[kk+1]) * pfact ;
   }

   EXRETURN ;
}
