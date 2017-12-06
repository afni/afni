#include "mrilib.h"

/*------- Adapted from 3dTstat.c --------*/

static char prefix[THD_MAX_PREFIX] = "tsort" ;
static int datum                   = MRI_float ;
static int inc                     = 1 ;
static int do_random               = 0 ;

static void SORTS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
static void SORTS_itsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
static void SORTS_rtsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
static void SORTS_ranfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
static void SORTS_FFTfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
extern int *z_iqsort (float *x , int nx );

static unsigned short xran_pm[3] = { 23456 , 34567 , 54321 } ;
#define SET_XRAN(xr,rss) \
 ( (xr)[0]=((rss)>>16), (xr)[1]=((rss)&65535), (xr)[2]=(xr)[0]+(xr)[1]+17 )

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, ii , nvals , rank=0 ;

   /*----- Help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTsort [options] dataset\n"
             "Sorts each voxel and produces a new dataset.\n"
             "\n"
             "Options:\n"
             " -prefix p = use string 'p' for the prefix of the\n"
             "               output dataset [DEFAULT = 'tsort']\n"
             " -inc      = sort into increasing order [default]\n"
             " -dec      = sort into decreasing order\n"
             " -rank     = output rank instead of sorted values\n"
             "             ranks range from 1 to Nvals\n"
             " -ind      = output sorting index. (0 to Nvals -1)\n"
             "             See example below.\n"
             " -val      = output sorted values (default)\n"
             " -random   = randomly shuffle (permute) the time points in each voxel\n"
             "             * Each voxel is permuted independently!\n"
             "             * Why is this here? Someone asked for it :)\n"
             " -ranFFT   = randomize each time series by scrambling the FFT phase\n"
             "             * Each voxel is treated separately!\n"
             "             * Why is this here? cf. Matthew 7:7-8 :)\n"
             " -ranDFT   = Almost the same as above, but:\n"
             "             * In '-ranFFT', the FFT length is taken\n"
             "               to be the next integer >= data length\n"
             "               for which the FFT algorithm is efficient.\n"
             "               This will result in data padding unless\n"
             "               the data length is exactly 'nice' for FFT.\n"
             "             * In '-ranDFT', the DFT length is exactly\n"
             "               the data length. If the data length is\n"
             "               a large-ish prime number (say 997), this\n"
             "               operation can be slow.\n"
             "             * The DFT/FFT algorithm is reasonably fast\n"
             "               when the data length prime factors contain\n"
             "               only 2s, 3s, and/or 5s.\n"
             "             * Using '-ranDFT' can preserve the spectral\n"
             "               (temporal correlation) structure of the\n"
             "               original data a little better than '-ranFFT'.\n"
             "             * The only reason to use '-ranFFT' instead of\n"
             "               '-ranDFT' is for speed. For example, with\n"
             "               997 time points, '-ranFFT' was about 13 times\n"
             "               faster (FFT length=1000) than '-ranDFT'.\n"
             " -datum D  = Coerce the output data to be stored as \n"
             "             the given type D, which may be  \n"
             "             byte, short, or float (default).         \n"
             "\n"
             "Notes:\n"
             "* Each voxel is sorted (or processed) separately.\n"
             "* Sub-brick labels are not rearranged!\n"
             "* This program is useful only in limited cases.\n"
             "   It was written to sort the -stim_times_IM\n"
             "   beta weights output by 3dDeconvolve.\n"
             "* Also see program 1dTsort, for sorting text files of numbers.\n"
             "\n"
             "Examples:\n"
             "setenv AFNI_1D_TIME YES\n"
             "echo '8 6 3 9 2 7' > test.1D\n"
             "    3dTsort -overwrite test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
             "    3dTsort -overwrite -rank test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
             "\n"
             "    3dTsort -overwrite -ind test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
             "    3dTsort -overwrite -dec test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dTsort main"); machdep(); AFNI_logger("3dTsort",argc,argv);
   PRINT_VERSION("3dTsort"); AUTHOR("RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   rank = 0;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!\n");
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("%s is not a valid prefix!\n",prefix);
         nopt++ ; continue ;
      }

      /*-- -inc or -dec --*/

      if( strncmp(argv[nopt],"-inc",4) == 0 ){
        inc = 1 ; nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-dec",4) == 0 ){
        inc = 0 ; nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-rank",5) == 0){
         rank = 1; nopt++; continue;
      }
      if( strncmp(argv[nopt],"-val",4) == 0){
         rank = 0; nopt++; continue;
      }
      if( strncmp(argv[nopt],"-ind",5) == 0){
         rank = -1; nopt++; continue;
      }
      if( strncmp(argv[nopt],"-random",6) == 0){
         unsigned int spm ;
         rank = 0; do_random = 1;
         init_rand_seed(0) ;
         spm = lrand48() ;
         SET_XRAN(xran_pm,spm) ;
         nopt++; continue;
      }
      if( strncmp(argv[nopt],"-ranFFT",6) == 0){
         unsigned int spm ;
         rank = 0; do_random = -1;
         init_rand_seed(0) ;
         spm = lrand48() ;
         SET_XRAN(xran_pm,spm) ;
         nopt++; continue;
      }
      if( strncmp(argv[nopt],"-ranDFT",6) == 0){
         unsigned int spm ;
         rank = 0; do_random = -2;
         init_rand_seed(0) ;
         spm = lrand48() ;
         SET_XRAN(xran_pm,spm) ;
         nopt++; continue;
      }
      if( strncasecmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc )
           ERROR_exit("need an argument after -datum!\n") ;
         if( strcasecmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcasecmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcasecmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            ERROR_exit( "-datum of type '%s' not supported "
                        "in 3dTsort!\n",argv[nopt]) ;
         }
         nopt++; continue;
      }
      /*-- Quien sabe'? --*/

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit(" No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s\n",argv[nopt]);
   DSET_load(old_dset) ;
   if( !DSET_LOADED(old_dset) )
     ERROR_exit("Can't load dataset %s\n",argv[nopt]) ;
   if (DSET_NUM_TIMES(old_dset)<2) {
      ERROR_exit( "Need at least 2 time points in series.\n"
                  "Have only %d\n"
                  "If using mutli-column 1D files, use\n"
                  "  setenv AFNI_1D_TIME YES\n"
                  , DSET_NUM_TIMES(old_dset));
   }
   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing inputs on command line ignored: %s ...",argv[nopt]) ;

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 2 )
     ERROR_exit("Can't use dataset with < 2 values per voxel!\n") ;

   /*------------- ready to compute new dataset -----------*/

   if( do_random == 1 ){   /* shuffling */
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_ranfunc ,        /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;
   } else if ( do_random < 0 ){  /* scrambling */
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_FFTfunc ,        /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;
   } else if (!rank) {
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_tsfunc ,         /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;
   } else if (rank == 1) {
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_itsfunc ,        /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;
   } else {
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_rtsfunc ,        /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;
   }
   if( new_dset != NULL ){
     tross_Copy_History( old_dset , new_dset ) ;
     tross_Make_History( "3dTsort" , argc,argv , new_dset ) ;
     if( DSET_NUM_TIMES(old_dset) > 1 )
       EDIT_dset_items( new_dset ,
                         ADN_ntt    , DSET_NVALS(old_dset) ,
                         ADN_ttorg  , DSET_TIMEORIGIN(old_dset) ,
                         ADN_ttdel  , DSET_TR(old_dset) ,
                         ADN_tunits , UNITS_SEC_TYPE ,
                       NULL ) ;
     DSET_write( new_dset ) ; WROTE_DSET( new_dset ) ;
   } else {
     ERROR_exit("Unable to compute output dataset!\n") ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void SORTS_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val )
{
   int ii , nval ;

   /** is this a "notification"? **/

   if( val == NULL ){
#if 0
      if( npts > 0 ){  /* the "start notification" */
      } else {  /* the "end notification" */
      }
#endif
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;
   if( inc == 0 ){
     for( ii=0 ; ii < nval ; ii++ ) val[ii] = -val[ii] ;
   }
   qsort_float( nval , val ) ;
   if( inc == 0 ){
     for( ii=0 ; ii < nval ; ii++ ) val[ii] = -val[ii] ;
   }
   return ;
}

/*--------------------------------------------------------------------*/

static void SORTS_itsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val )
{
   int ii , nval ;
   int *rnk;
   /** is this a "notification"? **/

   if( val == NULL ){
#if 0
      if( npts > 0 ){  /* the "start notification" */
      } else {  /* the "end notification" */
      }
#endif
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;

   /* Using an inverse sorting function, for excitement */
#if 0 /* the dumber way */
   for( ii=0 ; ii < nval ; ii++ ) val[ii] = -val[ii] ;
   rnk = z_iqsort( val, nval ) ;
   if( inc == 1 ){
     for( ii=0 ; ii < nval ; ii++ )  val[ii] = rnk[ii] +1 ;
   }else {
      for( ii=0 ; ii < nval ; ii++ ) val[ii] = nval - rnk[ii] ;
   }
#else /* the dumb way */
   rnk = z_iqsort( val, nval ) ;
   if( inc == 1 ){
     for( ii=0 ; ii < nval ; ii++ )  val[rnk[ii]] = nval - ii ;
   }else {
      for( ii=0 ; ii < nval ; ii++ ) val[rnk[ii]] = ii+1 ;
   }
#endif
   free(rnk); rnk=NULL;
   return ;
}

/*--------------------------------------------------------------------*/

static void SORTS_rtsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val )
{
   int ii , nval ;
   int *rnk;
   /** is this a "notification"? **/

   if( val == NULL ){
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;

   /* Using an inverse sorting function, for excitement */
   rnk = z_iqsort( val, nval ) ;
   if( inc == 1 ){
     for( ii=0 ; ii < nval ; ii++ )  val[nval - ii-1] =  rnk[ii];
   }else {
      for( ii=0 ; ii < nval ; ii++ ) val[ii] = rnk[ii] ;
   }
   free(rnk); rnk=NULL;
   return ;
}

/*--------------------------------------------------------------------*/

static int    p_nxy  = 0 ;     /* total length of data */
static float *p_xyar = NULL ;  /* array to hold both samples */
static int   *p_ijar = NULL ;  /* permutation array */

static void float_permute( int nxy , float *ar )
{
   int ii,jj,tt ;

   if( nxy < 2 ) return ;  /* how did this happen? */

   if( nxy > p_nxy ){  /* make workspaces */
     p_nxy  = nxy ;
     p_xyar = (float *)realloc(p_xyar,sizeof(float)*p_nxy) ;
     p_ijar = (int   *)realloc(p_ijar,sizeof(int  )*p_nxy) ;
   }

   /* initialize the permutation a little randomly */

   tt = nrand48(xran_pm) % p_nxy ;
   for( ii=0 ; ii < p_nxy ; ii++ ) p_ijar[ii] = (ii+tt)%p_nxy ;

   /* create a random-ish permutation */
   /* https://en.wikipedia.org/wiki/Random_permutation */

   for( ii=0 ; ii < p_nxy-1 ; ii++ ){
     jj = (nrand48(xran_pm)>>3) % (p_nxy-ii) ; /* jj in 0..p_nxy-ii-1 inclusive */
                                           /* so ii+jj in ii..p_nxy-1 inclusive */
     if( jj > 0 ){  /* swap */
       tt = p_ijar[ii] ; p_ijar[ii] = p_ijar[ii+jj] ; p_ijar[ii+jj] = tt ;
     }
   }

   for( ii=0 ; ii < p_nxy ; ii++ ) p_xyar[ii] = ar[ii] ;
   for( ii=0 ; ii < p_nxy ; ii++ ) ar[ii] = p_xyar[p_ijar[ii]] ;

   return ;
}

static void SORTS_ranfunc( double tzero, double tdelta ,
                           int npts, float ts[],
                           double ts_mean, double ts_slope,
                           void *ud, int nbriks, float *val )
{
   int ii , nval ;

   /** is this a "notification"? **/

   if( val == NULL ){
#if 0
      if( npts > 0 ){  /* the "start notification" */
      } else {  /* the "end notification" */
      }
#endif
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;
   float_permute( nval , val ) ;
   return ;
}

/*--------------------------------------------------------------------*/

static int s_nx=0 , s_nfft=0 , s_nfft2=0 , s_nftop=0 ;
static complex *s_cxar=NULL ;
static float  **s_pref=NULL ;
static float   *s_ffar=NULL ;
static float   *s_ffit=NULL ;

#define CXROT(cc,th)                   \
 do{ complex rr;                       \
     rr.r = cos(th); rr.i = sin(th);   \
     CMULTBY(cc,rr) ; } while(0)

#define TPI 6.283185f

static void float_scramble( int nx , float *ar )
{
   int ii,jj ; float theta ;

   if( nx < 5 ) return ;  /* how did this happen? */

   if( nx != s_nx ){
     s_nx   = nx ;
     if( do_random == -1 ){
       s_nfft = csfft_nextup_even(s_nx) ; s_nfft2 = s_nfft/2 ;
     } else {
       s_nfft  = s_nx ;
       s_nfft2 = (s_nfft %2 == 0 ) ? s_nfft/2 : 0;
     }
     s_nftop = s_nfft / 2 ;
     s_cxar = (complex *)realloc(s_cxar,sizeof(complex)*s_nfft) ;
     s_ffar = (float *  )realloc(s_ffar,sizeof(float)  *s_nfft) ;
     if( s_pref != NULL ){
       free(s_pref[0]) ; free(s_pref[1]) ; free(s_pref[2]) ; free(s_pref) ;
     }
     s_pref = THD_build_polyref( 3 , s_nx ) ;
     if( s_ffit == NULL ) s_ffit = (float *)malloc(sizeof(float)*3) ;
     csfft_scale_inverse(1) ;
     INFO_message("time series length = %d ==> FFT length = %d",
                  s_nx,s_nfft) ;
   }

   memcpy( s_ffar , ar , sizeof(float)*s_nx ) ;

   THD_generic_detrend_LSQ( s_nx , s_ffar , -1 , 3 , s_pref , s_ffit ) ;

   for( ii=0 ; ii < s_nfft ; ii++ ){
     s_cxar[ii].r = s_ffar[ii%s_nx] ;
     s_cxar[ii].i = 0.0f ;
   }

   csfft_cox( -1 , s_nfft , s_cxar ) ;

   for( ii=1 ; ii < s_nftop ; ii++ ){
     theta = TPI * (float)erand48(xran_pm) ;
     CXROT(s_cxar[ii],theta) ;
     s_cxar[s_nfft-ii].r =  s_cxar[ii].r ;
     s_cxar[s_nfft-ii].i = -s_cxar[ii].i ;
   }
   s_cxar[0].r = s_cxar[0].i ;
   if( s_nfft2 > 0 ){
     s_cxar[s_nfft2].i = 0.0f ;
     if( nrand48(xran_pm)%2 == 1 )
       s_cxar[s_nfft2].r = -s_cxar[s_nfft2].r ;
   }

   csfft_cox( 1 , s_nfft , s_cxar ) ;

   for( ii=0 ; ii < s_nx ; ii++ ) ar[ii] = s_cxar[ii].r ;
   THD_generic_retrend( s_nx , ar , -1 , 3 , s_pref , s_ffit ) ;
   return ;
}

static void SORTS_FFTfunc( double tzero, double tdelta ,
                           int npts, float ts[],
                           double ts_mean, double ts_slope,
                           void *ud, int nbriks, float *val )
{
   int ii , nval ;

   /** is this a "notification"? **/

   if( val == NULL ){
#if 0
      if( npts > 0 ){  /* the "start notification" */
      } else {  /* the "end notification" */
      }
#endif
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;
   float_scramble( nval , val ) ;
   return ;
}
