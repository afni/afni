#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/************************************************************************/
/******* Hack to remove large spikes from time series data (oog). *******/
/******* 30 Aug 2002 - RWCox                                      *******/
/************************************************************************/

#define TFAC  0.1    /* scale factor for -ssave dataset */
#define ITFAC 10.0   /* inverse of above */

/*----------------------------------------------------------------------*/

static INLINE float mytanh( float x )
{
  register float ex , exi ;
       if( x >  7.0f ) return  1.0f ;  /* 03 Sep: check for stupid inputs */
  else if( x < -7.0f ) return -1.0f ;
  ex = exp(x) ; exi = 1.0f/ex ;
  return (ex-exi)/(ex+exi) ;
}

/*============================================================================*/
/***************** The following stuff was added 29 Nov 2013 ******************/
/*----------------------------------------------------------------------------*/

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*--- fast median of 9 values ---*/

static INLINE float median9f(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+9 > num ) jj = num-9;      \
   qqq[0] = vec[jj+0]; qqq[1] = vec[jj+1]; qqq[2] = vec[jj+2]; \
   qqq[3] = vec[jj+3]; qqq[4] = vec[jj+4]; qqq[5] = vec[jj+5]; \
   qqq[6] = vec[jj+6]; qqq[7] = vec[jj+7]; qqq[8] = vec[jj+8]; \
   med    = median9f(qqq);     qqq[0] = fabsf(qqq[0]-med);     \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med);     \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med);     \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med);     \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med);     \
   mad    = median9f(qqq); }

/*-------------------------------------------------------------------------*/
/*! Remove spikes from a time series, in a very simplistic way.
    Return value is the number of spikes that were squashed [RWCox].
*//*-----------------------------------------------------------------------*/

int DES_despike9( int num , float *vec , float *wks )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( num < 9 || vec == NULL ) return 0 ;

   zme = wks ; zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ){ if( wks == NULL ) free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   return nsp ;
}
#undef mead9

/*----------------------------------------------------------------------------*/
/* Similar code to the above, but for spans of length 25 instead of 9 */

static INLINE float median25f(float *p)
{
    register float temp ;
    SORT2(p[0], p[1]) ;   SORT2(p[3], p[4]) ;   SORT2(p[2], p[4]) ;
    SORT2(p[2], p[3]) ;   SORT2(p[6], p[7]) ;   SORT2(p[5], p[7]) ;
    SORT2(p[5], p[6]) ;   SORT2(p[9], p[10]) ;  SORT2(p[8], p[10]) ;
    SORT2(p[8], p[9]) ;   SORT2(p[12], p[13]) ; SORT2(p[11], p[13]) ;
    SORT2(p[11], p[12]) ; SORT2(p[15], p[16]) ; SORT2(p[14], p[16]) ;
    SORT2(p[14], p[15]) ; SORT2(p[18], p[19]) ; SORT2(p[17], p[19]) ;
    SORT2(p[17], p[18]) ; SORT2(p[21], p[22]) ; SORT2(p[20], p[22]) ;
    SORT2(p[20], p[21]) ; SORT2(p[23], p[24]) ; SORT2(p[2], p[5]) ;
    SORT2(p[3], p[6]) ;   SORT2(p[0], p[6]) ;   SORT2(p[0], p[3]) ;
    SORT2(p[4], p[7]) ;   SORT2(p[1], p[7]) ;   SORT2(p[1], p[4]) ;
    SORT2(p[11], p[14]) ; SORT2(p[8], p[14]) ;  SORT2(p[8], p[11]) ;
    SORT2(p[12], p[15]) ; SORT2(p[9], p[15]) ;  SORT2(p[9], p[12]) ;
    SORT2(p[13], p[16]) ; SORT2(p[10], p[16]) ; SORT2(p[10], p[13]) ;
    SORT2(p[20], p[23]) ; SORT2(p[17], p[23]) ; SORT2(p[17], p[20]) ;
    SORT2(p[21], p[24]) ; SORT2(p[18], p[24]) ; SORT2(p[18], p[21]) ;
    SORT2(p[19], p[22]) ; SORT2(p[8], p[17]) ;  SORT2(p[9], p[18]) ;
    SORT2(p[0], p[18]) ;  SORT2(p[0], p[9]) ;   SORT2(p[10], p[19]) ;
    SORT2(p[1], p[19]) ;  SORT2(p[1], p[10]) ;  SORT2(p[11], p[20]) ;
    SORT2(p[2], p[20]) ;  SORT2(p[2], p[11]) ;  SORT2(p[12], p[21]) ;
    SORT2(p[3], p[21]) ;  SORT2(p[3], p[12]) ;  SORT2(p[13], p[22]) ;
    SORT2(p[4], p[22]) ;  SORT2(p[4], p[13]) ;  SORT2(p[14], p[23]) ;
    SORT2(p[5], p[23]) ;  SORT2(p[5], p[14]) ;  SORT2(p[15], p[24]) ;
    SORT2(p[6], p[24]) ;  SORT2(p[6], p[15]) ;  SORT2(p[7], p[16]) ;
    SORT2(p[7], p[19]) ;  SORT2(p[13], p[21]) ; SORT2(p[15], p[23]) ;
    SORT2(p[7], p[13]) ;  SORT2(p[7], p[15]) ;  SORT2(p[1], p[9]) ;
    SORT2(p[3], p[11]) ;  SORT2(p[5], p[17]) ;  SORT2(p[11], p[17]) ;
    SORT2(p[9], p[17]) ;  SORT2(p[4], p[10]) ;  SORT2(p[6], p[12]) ;
    SORT2(p[7], p[14]) ;  SORT2(p[4], p[6]) ;   SORT2(p[4], p[7]) ;
    SORT2(p[12], p[14]) ; SORT2(p[10], p[14]) ; SORT2(p[6], p[7]) ;
    SORT2(p[10], p[12]) ; SORT2(p[6], p[10]) ;  SORT2(p[6], p[17]) ;
    SORT2(p[12], p[17]) ; SORT2(p[7], p[17]) ;  SORT2(p[7], p[10]) ;
    SORT2(p[12], p[18]) ; SORT2(p[7], p[12]) ;  SORT2(p[10], p[18]) ;
    SORT2(p[12], p[20]) ; SORT2(p[10], p[20]) ; SORT2(p[10], p[12]) ;
    return (p[12]);
}

/*--- get the local median and MAD of values vec[j-12 .. j+12] ---*/

#undef  mead25
#define mead25(j)                                              \
 { float qqq[25] ; int jj=(j)-12 ; register int pp;            \
   if( jj < 0 ) jj = 0; else if( jj+25 > num ) jj = num-25;    \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = vec[jj+pp] ;         \
   med = median25f(qqq) ;                                      \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = fabsf(qqq[pp]-med) ; \
   mad = median25f(qqq); }

int DES_despike25( int num , float *vec , float *wks )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( vec == NULL ) return 0 ;
   if( num <  25   ) return DES_despike9(num,vec,wks) ;

   zme = wks ; zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead25(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ){ if( wks == NULL ) free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   return nsp ;
}
#undef mead25
#undef SORT2
#undef SWAP

/*----------------------------------------------------------------------------*/

int DES_workspace_size( int ntim, int nref )
{
   return 2*ntim ;
}

/*----------------------------------------------------------------------------*/

static int use_des25 = 0 ;

float DES_solve( MRI_IMAGE *psinv , float *z , float *coef , float *wks )
{
   float *psar , *iar , zi ; int ii,jj , ntim,nref ;

   ntim = psinv->ny ;
   nref = psinv->nx ;
   psar = MRI_FLOAT_PTR(psinv) ;

   /* step 1: despike the data the simplistic way */

   if( use_des25 )
     (void) DES_despike25( ntim , z , wks ) ;
   else
     (void) DES_despike9( ntim , z , wks ) ;

   /* least squares solve the equations with the modified data */

   for( jj=0 ; jj < nref ; jj++ ) coef[jj] = 0.0f ;

   for( ii=0 ; ii < ntim ; ii++ ){
     iar = psar + ii*nref ; zi = z[ii] ;
     for( jj=0 ; jj < nref ; jj++ ) coef[jj] += iar[jj]*zi ;
   }

   return 0.0f ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * DES_get_psinv( int ntim , int nref , float **ref )
{
   MRI_IMAGE *refim , *psinv ; float *refar , *jar ; int ii , jj ;

   refim = mri_new(ntim,nref,MRI_float) ;
   refar = MRI_FLOAT_PTR(refim) ;
   for( jj=0 ; jj < nref ; jj++ ){
     jar = refar + jj*ntim ;
     for( ii=0 ; ii < ntim ; ii++ ) jar[ii] = ref[jj][ii] ;
   }
   mri_matrix_psinv_svd(1) ;
   psinv = mri_matrix_psinv(refim,NULL,0.0f) ;
   mri_free(refim) ;
   return psinv ;
}

/*----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset , *oset=NULL , *tset=NULL ;
   int nvals , iv , nxyz , ii,jj,kk , iarg , kz,kzold ;
   float cut1=2.5,cut2=4.0 , sq2p,sfac , fq ;
   MRI_IMAGE *flim ;
   char *prefix="despike" , *tprefix=NULL ;

   int corder=-1 , nref , ignore=0 , polort=2 , nuse , nomask=0 ;
   int nspike, nbig, nproc ;
   float **ref ;
   float  c21,ic21 , pspike,pbig ;
   short  *sar , *qar ;
   byte   *tar , *mask=NULL ;
   float  *zar , *yar ;
   int     in_datum , out_datum ;
   int     localedit=0 ;  /* 04 Apr 2007 */
   int     verb=1 ;

   int     do_NEW = 0 ;   /* 29 Nov 2013 */
   MRI_IMAGE *NEW_psinv=NULL ;
   int     dilate = 4 ;   /* 04 Dec 2013 */
   int     ctim   = 0 ;

   /*----- Read command line -----*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("\n"
             "Usage: 3dDespike [options] dataset\n"
             "\n"
             "Removes 'spikes' from the 3D+time input dataset and writes\n"
             "a new dataset with the spike values replaced by something\n"
             "more pleasing to the eye.\n"
             "\n"
             "------------------\n"
             "Outline of Method:\n"
             "------------------\n"
             " * L1 fit a smooth-ish curve to each voxel time series\n"
             "    [see -corder option for description of the curve]\n"
             "    [see -NEW option for a different & faster fitting method]\n"
             " * Compute the MAD of the difference between the curve and\n"
             "    the data time series (the residuals).\n"
             " * Estimate the standard deviation 'sigma' of the residuals\n"
             "    from the MAD.\n"
             " * For each voxel value, define s = (value-curve)/sigma.\n"
             " * Values with s > c1 are replaced with a value that yields\n"
             "    a modified s' = c1+(c2-c1)*tanh((s-c1)/(c2-c1)).\n"
             " * c1 is the threshold value of s for a 'spike' [default c1=2.5].\n"
             " * c2 is the upper range of the allowed deviation from the curve:\n"
             "    s=[c1..infinity) is mapped to s'=[c1..c2)   [default c2=4].\n"
             "\n"
             "An alternative method for replacing the spike value is provided\n"
             "by the '-localedit' option, and that method is preferred by\n"
             "many users.\n"
             "\n"
             "The input dataset can be stored in short or float formats.\n"
             "The output dataset will always be stored in floats. [Feb 2017]\n"
             "\n"
             "--------\n"
             "Options:\n"
             "--------\n"
             " -ignore I  = Ignore the first I points in the time series:\n"
             "               these values will just be copied to the\n"
             "               output dataset [default I=0].\n"
             " -corder L  = Set the curve fit order to L:\n"
             "               the curve that is fit to voxel data v(t) is\n"
             "\n"
             "                       k=L [        (2*PI*k*t)          (2*PI*k*t) ]\n"
             " f(t) = a+b*t+c*t*t + SUM  [ d * sin(--------) + e * cos(--------) ]\n"
             "                       k=1 [  k     (    T   )    k     (    T   ) ]\n"
             "\n"
             "               where T = duration of time series;\n"
             "               the a,b,c,d,e parameters are chosen to minimize\n"
             "               the sum over t of |v(t)-f(t)| (L1 regression);\n"
             "               this type of fitting is is insensitive to large\n"
             "               spikes in the data.  The default value of L is\n"
             "               NT/30, where NT = number of time points.\n"
             "\n"
             " -cut c1 c2 = Alter default values for the spike cut values\n"
             "               [default c1=2.5, c2=4.0].\n"
             "\n"
             " -prefix pp = Save de-spiked dataset with prefix 'pp'\n"
             "               [default pp='despike']\n"
             "\n"
             " -ssave ttt = Save 'spikiness' measure s for each voxel into a\n"
             "               3D+time dataset with prefix 'ttt' [default=no save]\n"
             "\n"
             " -nomask    = Process all voxels\n"
             "               [default=use a mask of high-intensity voxels, ]\n"
             "               [as created via '3dAutomask -dilate 4 dataset'].\n"
             "\n"
             " -dilate nd = Dilate 'nd' times (as in 3dAutomask).  The default\n"
             "               value of 'nd' is 4.\n"
             "\n"
             " -q[uiet]   = Don't print '++' informational messages.\n"
             "\n"
             " -localedit = Change the editing process to the following:\n"
             "                If a voxel |s| value is >= c2, then replace\n"
             "                the voxel value with the average of the two\n"
             "                nearest non-spike (|s| < c2) values; the first\n"
             "                one previous and the first one after.\n"
             "                Note that the c1 cut value is not used here.\n"
             "\n"
             " -NEW       = Use the 'new' method for computing the fit, which\n"
             "              should be faster than the L1 method for long time\n"
             "              series (200+ time points); however, the results\n"
             "              are similar but NOT identical. [29 Nov 2013]\n"
             "              * You can also make the program use the 'new'\n"
             "                method by setting the environment variable\n"
             "                  AFNI_3dDespike_NEW\n"
             "                to the value YES; as in\n"
             "                  setenv AFNI_3dDespike_NEW YES  (csh)\n"
             "                  export AFNI_3dDespike_NEW=YES  (bash)\n"
             "              * If this variable is set to YES, you can turn off\n"
             "                the '-NEW' processing by using the '-OLD' option.\n"
             "          -->>* For time series more than 500 points long, the\n"
             "                '-OLD' algorithm is tremendously slow.  You should\n"
             "                use the '-NEW' algorith in such cases.\n"
             "             ** At some indeterminate point in the future, the '-NEW'\n"
             "                method will become the default!\n"
             "          -->>* As of 29 Sep 2016, '-NEW' is the default if there\n"
             "                is more than 500 points in the time series dataset.\n"
             "\n"
             " -NEW25     = A slightly more aggressive despiking approach than\n"
             "              the '-NEW' method.\n"
             "\n"
             "--------\n"
             "Caveats:\n"
             "--------\n"
             "* Despiking may interfere with image registration, since head\n"
             "   movement may produce 'spikes' at the edge of the brain, and\n"
             "   this information would be used in the registration process.\n"
             "   This possibility has not been explored or calibrated.\n"
             "\n"
             "* [LATER] Actually, it seems like the registration problem\n"
             "   does NOT happen, and in fact, despiking seems to help!\n"
             "\n"
             "* Check your data visually before and after despiking and\n"
             "   registration!\n"
            ) ;

      PRINT_AFNI_OMP_USAGE("3dDespike",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /** AFNI package setup and logging **/

   mainENTRY("3dDespike main"); machdep(); AFNI_logger("3dDespike",argc,argv);
   PRINT_VERSION("3dDespike") ; AUTHOR("RW Cox") ;

   /** parse options **/

   if( AFNI_yesenv("AFNI_3dDespike_NEW") ) do_NEW = 1 ;  /* 29 Nov 2013 */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-q",2) == 0 ){       /* 04 Apr 2007 */
        verb = 0 ; iarg++ ; continue ;
      }
      if( strncmp(argv[iarg],"-v",2) == 0 ){
        verb++ ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-NEW") == 0 ){       /* 29 Nov 2013 */
        do_NEW = 1 ; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-NEW25") == 0 ){     /* 29 Sep 2016 */
        do_NEW = 1 ; use_des25 = 1 ; cut1 = 2.5f ; cut2 = 3.2f ; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-OLD") == 0 ){
        do_NEW = 0 ; iarg++ ; continue ;
      }

      /** -localedit **/

      if( strcmp(argv[iarg],"-localedit") == 0 ){  /* 04 Apr 2007 */
        localedit = 1 ; iarg++ ; continue ;
      }

      /** don't use masking **/

      if( strcmp(argv[iarg],"-nomask") == 0 ){
        nomask = 1 ; iarg++ ; continue ;
      }

      /** dilation count [04 Dec 2013] **/

      if( strcmp(argv[iarg],"-dilate") == 0 ){
        dilate = (int)strtod(argv[++iarg],NULL) ;
             if( dilate <=  0 ) dilate = 1 ;
        else if( dilate >  99 ) dilate = 99 ;
        iarg++ ; continue ;
      }

      /** output dataset prefix **/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) ) ERROR_exit("-prefix is not good");
        iarg++ ; continue ;
      }

      /** ratio dataset prefix **/

      if( strcmp(argv[iarg],"-ssave") == 0 ){
        tprefix = argv[++iarg] ;
        if( !THD_filename_ok(tprefix) ) ERROR_exit("-ssave prefix is not good");
        iarg++ ; continue ;
      }

      /** trigonometric polynomial order **/

      if( strcmp(argv[iarg],"-corder") == 0 ){
        corder = strtol( argv[++iarg] , NULL , 10 ) ;
        if( corder < 0 ) ERROR_exit("Illegal value of -corder");
        iarg++ ; continue ;
      }

      /** how much to ignore at start **/

      if( strcmp(argv[iarg],"-ignore") == 0 ){
        ignore = strtol( argv[++iarg] , NULL , 10 ) ;
        if( ignore < 0 ) ERROR_exit("Illegal value of -ignore");
        iarg++ ; continue ;
      }

      /** thresholds for s ratio **/

      if( strcmp(argv[iarg],"-cut") == 0 ){
        cut1 = strtod( argv[++iarg] , NULL ) ;
        cut2 = strtod( argv[++iarg] , NULL ) ;
        if( cut1 < 1.0 || cut2 < cut1+0.5 )
          ERROR_exit("Illegal values after -cut");
        iarg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[iarg]) ;
   }

   c21 = cut2-cut1 ; ic21 = 1.0/c21 ;

   /*----- read input dataset -----*/

   if( iarg >= argc ) ERROR_exit("No input dataset!!??");

   dset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(dset,argv[iarg]) ;
   in_datum = DSET_BRICK_TYPE(dset,0) ;
   if( (in_datum != MRI_short && in_datum != MRI_float) || !DSET_datum_constant(dset) )
     ERROR_exit("Can't process non-short, non-float dataset!") ;

   out_datum = MRI_float ;
   if( verb && (in_datum == MRI_short) ){
     INFO_message("Input dataset is in short format, but output will be in float format") ;
   }

   nvals = DSET_NVALS(dset) ; nuse = nvals - ignore ;
   if( nuse < 15 )
     ERROR_exit("Can't use dataset with < 15 time points per voxel!") ;

   if( nuse > 500 && !do_NEW ){
     if( verb )
       INFO_message("Switching to '-NEW' method since number of time points = %d > 500",nuse) ;
     do_NEW = 1 ;
   }
   if( use_des25 && nuse <= 99 ){
     if( verb ){
       INFO_message("'-NEW25' method was ordered, but need more than 99 time points for that") ;
       INFO_message("  switching to the '-NEW' method instead") ;
     }
     use_des25 = 0 ;
   }

   if( verb ) INFO_message("ignoring first %d time points, using last %d",ignore,nuse);
   if( corder > 0 && 4*corder+2 > nuse ){
     ERROR_exit("-corder %d is too big for NT=%d",corder,nvals) ;
   } else if( corder < 0 ){
     corder = rint(nuse/30.0) ; if( corder > 50 && !do_NEW ) corder = 50 ;
     if( verb ) INFO_message("using %d time points => -corder %d",nuse,corder) ;
   } else {
     if( verb ) INFO_message("-corder %d set from command line",corder) ;
   }
   nxyz = DSET_NVOX(dset) ;
   if( verb ) INFO_message("Loading dataset %s",argv[iarg]) ;
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   /*-- create automask --*/

   if( !nomask ){
     mask = THD_automask( dset ) ;
     ii = THD_countmask( DSET_NVOX(dset) , mask ) ;
     if( verb && ii > 0 )
       INFO_message("%d voxels in the automask [out of %d in dataset]",ii,DSET_NVOX(dset)) ;
     else if( ii == 0 )
       ERROR_exit("Nothing to process -- automask is empty :(") ;

     for( ii=0 ; ii < dilate ; ii++ )
       THD_mask_dilate( DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset), mask, 3 ) ;

     ii = THD_countmask( DSET_NVOX(dset) , mask ) ;
     if( verb )
       INFO_message("%d voxels in the dilated automask [out of %d in dataset]",ii,DSET_NVOX(dset)) ;
     if( ii == 0 )
       ERROR_exit("Nothing to process -- no voxels in automask?!") ;
   } else {
     if( verb ) INFO_message("processing all %d voxels in dataset",DSET_NVOX(dset)) ;
   }

   /*-- create empty despiked dataset --*/

   oset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , out_datum ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) )
     ERROR_exit("output dataset already exists: %s",DSET_HEADNAME(oset));

   tross_Copy_History( oset , dset ) ;
   tross_Make_History( "3dDespike" , argc , argv , oset ) ;

   /* copy the ignored bricks, if any */

   for( iv=0 ; iv < ignore ; iv++ ){
     flim = THD_extract_float_brick(iv,dset) ;
     EDIT_substitute_brick( oset , iv , MRI_float , MRI_FLOAT_PTR(flim) ) ;
     mri_clear_and_free(flim) ;
   }

   /* create rest of the bricks (will be filled with zeros) */

   for( iv=ignore ; iv < nvals ; iv++ )
     EDIT_substitute_brick( oset , iv , out_datum , NULL ) ;

   /*-- setup to save a threshold statistic dataset, if desired --*/

   if( tprefix != NULL ){
     float *fac ;
     tset = EDIT_empty_copy( dset ) ;
     fac  = (float *) malloc( sizeof(float) * nvals ) ;
     for( ii=0 ; ii < nvals ; ii++ ) fac[ii] = TFAC ;
     EDIT_dset_items( tset ,
                        ADN_prefix    , tprefix ,
                        ADN_brick_fac , fac ,
                        ADN_datum_all , MRI_byte ,
                        ADN_func_type , FUNC_FIM_TYPE ,
                      ADN_none ) ;
     free(fac) ;

     tross_Copy_History( tset , dset ) ;
     tross_Make_History( "3dDespike" , argc , argv , tset ) ;

#if 0
     if( THD_is_file(DSET_HEADNAME(tset)) )
       ERROR_exit("-ssave dataset already exists");
#endif

     tross_Copy_History( tset , dset ) ;
     tross_Make_History( "3dDespike" , argc , argv , tset ) ;

     for( iv=0 ; iv < nvals ; iv++ )
       EDIT_substitute_brick( tset , iv , MRI_byte , NULL ) ;
   }

   /*-- setup to find spikes --*/

   sq2p  = sqrt(0.5*PI) ;
   sfac  = sq2p / 1.4826f ;

   /* make ref functions */

   nref = 2*corder+3 ;
   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   for( jj=0 ; jj < nref ; jj++ )
     ref[jj] = (float *) malloc( sizeof(float) * nuse ) ;

   /* r(t) = 1 */

   for( iv=0 ; iv < nuse ; iv++ ) ref[0][iv] = 1.0 ;
   jj = 1 ;

   /* r(t) = t - tmid */

   { float tm = 0.5 * (nuse-1.0) ; float fac = 2.0 / nuse ;
     for( iv=0 ; iv < nuse ; iv++ ) ref[1][iv] = (iv-tm)*fac ;
     jj = 2 ;

     /* r(t) = (t-tmid)**jj [NB: polort==2] */

     for( ; jj <= polort ; jj++ )
       for( iv=0 ; iv < nuse ; iv++ )
         ref[jj][iv] = pow( (iv-tm)*fac , (double)jj ) ;
   }

   for( kk=1 ; kk <= corder ; kk++ ){  /* sines and cosines */
     fq = (2.0*PI*kk)/nuse ;

     /* r(t) = sin(2*PI*k*t/N) */

     for( iv=0 ; iv < nuse ; iv++ )
       ref[jj][iv] = sin(fq*iv) ;
     jj++ ;

     /* r(t) = cos(2*PI*k*t/N) */

     for( iv=0 ; iv < nuse ; iv++ )
       ref[jj][iv] = cos(fq*iv) ;
     jj++ ;
   }

   /****** setup for the NEW solution method [29 Nov 2013] ******/

   if( do_NEW ){
     NEW_psinv = DES_get_psinv(nuse,nref,ref) ;
     if( verb )
       INFO_message("Procesing time series with %s model fit algorithm",
                    (use_des25) ? "NEW25" : "NEW" ) ;
   } else {
     if( verb )
       INFO_message("Procesing time series with OLD model fit algorithm") ;
   }

   /*--- loop over voxels and do work ---*/

#define Laplace_t2p(val) ( 1.0 - nifti_stat2cdf( (val), 15, 0.0, 1.4427 , 0.0 ) )

   if( verb ){
    if( !localedit ){
      INFO_message("smash edit thresholds: %.1f .. %.1f MADs",cut1*sq2p,cut2*sq2p) ;
      ININFO_message("  [ %.3f%% .. %.3f%% of normal distribution]",
                     200.0*qg(cut1*sfac) , 200.0*qg(cut2*sfac) ) ;
      ININFO_message("  [ %.3f%% .. %.3f%% of Laplace distribution]" ,
                   100.0*Laplace_t2p(cut1) , 100.0*Laplace_t2p(cut2) ) ;
    } else {
      INFO_message("local edit threshold:  %.1f MADS",cut2*sq2p) ;
      ININFO_message("  [ %.3f%% of normal distribution]",
                    200.0*qg(cut2*sfac) ) ;
      ININFO_message("  [ %.3f%% of Laplace distribution]",
                   100.0*Laplace_t2p(cut1) ) ;
    }
   }

   kzold  = -1 ;
   nspike =  0 ; nbig = 0 ; nproc = 0 ; ctim = NI_clock_time() ;

   /* OpenMP-ized across voxels */

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 666 )
 { int ii , iv , iu , id , jj ;
   float *far , *dar , *var , *fitar , *ssp , *fit , *zar ;
   short *sar , *qar ; byte *tar ;
   float fsig , fq , cls , snew , val ;
   float *NEW_wks=NULL ;

#pragma omp critical (DESPIKE_malloc)
  { far   = (float *) malloc( sizeof(float) * nvals ) ;
    dar   = (float *) malloc( sizeof(float) * nvals ) ;
    var   = (float *) malloc( sizeof(float) * nvals ) ;
    fitar = (float *) malloc( sizeof(float) * nvals ) ;
    ssp   = (float *) malloc( sizeof(float) * nvals ) ;
    fit   = (float *) malloc( sizeof(float) * nref  ) ;
    if( do_NEW )
      NEW_wks = (float *)malloc(sizeof(float)*DES_workspace_size(nuse,nref)) ;
  }

#ifdef USE_OMP
   if( verb )
     INFO_message("start OpenMP thread #%d",omp_get_thread_num()) ;
#endif

#pragma omp for
   for( ii=0 ; ii < nxyz ; ii++ ){   /* ii = voxel index */

      if( mask != NULL && mask[ii] == 0 ) continue ;   /* skip this voxel */

#ifndef USE_OMP
      kz = DSET_index_to_kz(dset,ii) ;       /* starting a new slice */
      if( kz != kzold ){
        if( verb ){
          fprintf(stderr, "++ start slice %2d",kz ) ;
          if( nproc > 0 ){
            pspike = (100.0*nspike)/nproc ;
            pbig   = (100.0*nbig  )/nproc ;
            fprintf(stderr,
                    "; so far %d data points, %d edits [%.3f%%], %d big edits [%.3f%%]",
                    nproc,nspike,pspike,nbig,pbig ) ;
          }
          fprintf(stderr,"\n") ;
        }
        kzold = kz ;
      }
#else
      if( verb && ii % 2345 == 1234 ) fprintf(stderr,".") ;
#endif

      /*** extract ii-th time series into far[] ***/

#if 1  /* 20 Feb 2017 */
      if( ignore > 0 ){
        (void)THD_extract_array(ii,dset,0,dar) ;
        for( iv=0 ; iv < nuse ; iv++ )
          far[iv] = dar[iv+ignore] ;
      } else {
        (void)THD_extract_array(ii,dset,0,far) ;
      }
#else
      switch( datum ){
        case MRI_short:
          for( iv=0 ; iv < nuse ; iv++ ){
            qar = DSET_ARRAY(dset,iv+ignore) ;   /* skip ignored data */
            far[iv] = (float)qar[ii] ;
          }
        break ;
        case MRI_float:
          for( iv=0 ; iv < nuse ; iv++ ){
            zar = DSET_ARRAY(dset,iv+ignore) ;
            far[iv] = zar[ii] ;
          }
        break ;
      }
#endif
      AAmemcpy(dar,far,sizeof(float)*nuse) ;   /* copy time series into dar[] */

      /*** solve for L1 fit ***/

      if( do_NEW )
        cls = DES_solve( NEW_psinv , far , fit , NEW_wks ) ; /* 29 Nov 2013 */
      else
        cls = cl1_solve( nuse , nref , far , ref , fit,0 ) ; /* the slow part */

      if( cls < 0.0f ){                      /* fit failed! */
#if 0
        fprintf(stderr,"curve fit fails at voxel %d %d %d\n",
                DSET_index_to_ix(dset,ii) ,
                DSET_index_to_jy(dset,ii) ,
                DSET_index_to_kz(dset,ii)  ) ;
#endif
        continue ;                           /* skip this voxel */
      }

      for( iv=0 ; iv < nuse ; iv++ ){        /* detrend */
        val =  fit[0]
             + fit[1]*ref[1][iv]             /* quadratic part of curve fit */
             + fit[2]*ref[2][iv] ;
        for( jj=3 ; jj < nref ; jj++ )       /* rest of curve fit */
          val += fit[jj] * ref[jj][iv] ;

        fitar[iv] = val ;                    /* save curve fit value */
        var[iv]   = dar[iv]-val ;            /* remove fitted value = resid */
        far[iv]   = fabsf(var[iv]) ;         /* abs value of resid */
      }

      /*** compute estimate standard deviation of detrended data ***/

      fsig = sq2p * qmed_float(nuse,far) ;   /* also mangles far array */

      /*** process time series for spikes, editing data in dar[] ***/

      if( fsig > 0.0f ){                     /* data wasn't fit perfectly */

        /* find spikiness for each point in time */

        fq = 1.0f / fsig ;
        for( iv=0 ; iv < nuse ; iv++ ){
          ssp[iv] = fq * var[iv] ;           /* spikiness s = how many sigma out */
        }

        /* save spikiness in -ssave datset */

        if( tset != NULL ){
          for( iv=0 ; iv < nuse ; iv++ ){
            tar     = DSET_ARRAY(tset,iv+ignore) ;
            snew    = ITFAC*fabsf(ssp[iv]) ;  /* scale for byte storage */
            tar[ii] = BYTEIZE(snew) ;         /* cf. mrilib.h */
          }
        }

        /* process values of |s| > cut1, editing dar[] */

        for( iv=0 ; iv < nuse ; iv++ ){ /* loop over time points */
          if( !localedit ){             /** classic 'smash' edit **/
            if( ssp[iv] > cut1 ){
              snew = cut1 + c21*mytanh((ssp[iv]-cut1)*ic21) ;   /* edit s down */
              dar[iv] = fitar[iv] + snew*fsig ;
#pragma omp critical (DESPIKE_counter)
              { nspike++ ; if( ssp[iv] > cut2 ) nbig++ ; }
            } else if( ssp[iv] < -cut1 ){
              snew = -cut1 + c21*mytanh((ssp[iv]+cut1)*ic21) ;  /* edit s up */
              dar[iv] = fitar[iv] + snew*fsig ;
#pragma omp critical (DESPIKE_counter)
              { nspike++ ; if( ssp[iv] < -cut2 ) nbig++ ; }
            }
          } else {                      /** local edit: 04 Apr 2007 **/
            if( ssp[iv] >= cut2 || ssp[iv] <= -cut2 ){
              for( iu=iv+1 ; iu < nuse ; iu++ )  /* find non-spike above */
                if( ssp[iu] < cut2 && ssp[iu] > -cut2 ) break ;
              for( id=iv-1 ; id >= 0   ; id-- )  /* find non-spike below */
                if( ssp[id] < cut2 && ssp[id] > -cut2 ) break ;
              switch( (id>=0) + 2*(iu<nuse) ){   /* compute replacement val */
                case 3: val = 0.5*(dar[iu]+dar[id]); break; /* iu and id OK */
                case 2: val =      dar[iu]         ; break; /* only iu OK   */
                case 1: val =              dar[id] ; break; /* only id OK   */
               default: val = fitar[iv]            ; break; /* shouldn't be */
              }
              dar[iv] = val ;
#pragma omp critical (DESPIKE_counter)
              { nspike++ ; nbig++ ; }
            }
          }
        } /* end of loop over time points */
#pragma omp atomic
        nproc += nuse ;  /* number data points processed */

      } /* end of processing time series when fsig is positive */

      /* put dar[] time series (possibly edited above) into output bricks */

        for( iv=0 ; iv < nuse ; iv++ ){
          zar = DSET_ARRAY(oset,iv+ignore) ; /* output brick */
          zar[ii] = dar[iv] ;                /* original or mutated data */
        }

   } /* end of loop over voxels #ii */

#pragma omp critical (DESPIKE_malloc)
   { free(fit); free(ssp); free(fitar); free(var); free(dar); free(far);
     if( do_NEW ) free(NEW_wks) ; }

 } /* end OpenMP */
 AFNI_OMP_END ;

#ifdef USE_OMP
   if( verb ) fprintf(stderr,"\n") ;
#endif
   ctim = NI_clock_time() - ctim ;
   INFO_message( "Elapsed despike time = %s" , nice_time_string(ctim) ) ;
   if( ctim > 34567 && !do_NEW )
     ININFO_message("That was SLOW -- try the '-NEW' option for a speedup") ;

#ifdef USE_OMP
   if( verb ) fprintf(stderr,"\n") ;
#endif

   /*--- finish up ---*/

   if( do_NEW ) mri_free(NEW_psinv) ;

   DSET_delete(dset) ; /* delete input dataset */

   if( verb ){
     if( nproc > 0 ){
       pspike = (100.0*nspike)/nproc ;
       pbig   = (100.0*nbig  )/nproc ;
       INFO_message("FINAL: %d data points, %d edits [%.3f%%], %d big edits [%.3f%%]",
               nproc,nspike,pspike,nbig,pbig ) ;
     } else {
       INFO_message("FINAL: no good voxels found to process!!??") ;
     }
   }

   /* write results */

   DSET_write(oset) ;
   if( verb ) WROTE_DSET(oset) ;
   DSET_delete(oset) ;

   if( tset != NULL ){
     DSET_write(tset) ;
     if( verb ) WROTE_DSET(tset) ;
     DSET_delete(tset) ;
   }

   exit( THD_get_write_error_count() ) ;
}
