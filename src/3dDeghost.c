#include "mrilib.h"

static int verb = 1 ;

#undef  FREEIF
#define FREEIF(x) if((x)!=NULL)free((void *)(x))

THD_3dim_dataset * THD_deghoster( THD_3dim_dataset *inset  ,
                                  THD_3dim_dataset *filset ,
                                  int pe , int fe , int se  ) ;
void orfilt_vector( int nvec , float *vec ) ;

static int orfilt_len = 11 ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   char *prefix = "Deghost" ;
   int iarg ;
   int fe=1 , pe=2 , se=3 , nvals ;
   THD_3dim_dataset *inset=NULL , *outset , *filset=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dDeghost [options] dataset\n"
       "\n"
       "* This program tries do remove N/2 (AKA Nyquist) ghosts from an EPI\n"
       "  magnitude time series dataset.\n"
       "* If you apply it to some other kind of dataset (e.g., spiral), weird\n"
       "  things will probably transpire.\n"
       "* The input EPI dataset should NOT be filtered, masked, cropped,\n"
       "  registered, or pre-processed in any way!\n"
       "* This program will not work well if the input EPI dataset is heavily\n"
       "  'shaded' -- that is, its intensity varies dramatically inside the brain.\n"
       "* The output dataset is always stored in float format.\n"
       "* Only the Amitabha Buddha knows if this program is actually useful.\n"
       "\n"
       "========\n"
       "OPTIONS:\n"
       "========\n"
       "  -input dataset = Another way to specify the input dataset\n"
       "  -prefix pp     = Use 'pp' for prefix of output dataset\n"
       "  -FPS abc       = Define the Frequency, Phase, and Slice\n"
       "                   directions in the dataset based on the\n"
       "                   axis orientations inside the dataset header\n"
       "                   (e.g., see the output of 3dinfo).  The 'abc'\n"
       "                   code is a permutaton of the digits '123'.\n"
       "                 *  The first digit 'a' specifies which dataset\n"
       "                    axis/index is the Frequency encoding direction.\n"
       "                 *  The second digit 'b' specifies which dataset\n"
       "                    direction is the Phase encoding direction.\n"
       "                 *  The third digit 'c' specifies which dataset\n"
       "                    direction is the Slice encoding direction.\n"
       "             -->>** The default value for 'abc' is '123'; that is,\n"
       "                    the dataset is ordered so that the first index\n"
       "                    (x-axis) is frequency, the second index is phase,\n"
       "                    and the third index is slice.  In most cases,\n"
       "                    this is how the reconstruction software will\n"
       "                    store the images.  Only in unusual cases should\n"
       "                    you need the '-FPS' option!\n"
       "  -filt N        = Length of time series filter to apply when\n"
       "                    estimating ghosting parameters.  Set N to 0 or 1\n"
       "                    to turn this feature off; otherwise, N should be an\n"
       "                    odd positive integer from 3 to 19 [default N=%d].\n"
       "                 * Longer filter lengths ARE allowed, but will be slow\n"
       "                    (cases with N <= 19 are hand coded for speed).\n"
       "                 * Datasets with fewer than 4 time points will not\n"
       "                    be filtered.  For longer datasets, if the filter\n"
       "                    length is too big, it will be shortened ruthlessly.\n"
       "=======\n"
       "METHOD:\n"
       "=======\n"
       "Would you believe me if I said magic? Would you accept secret algorithms\n"
       "known only to the Olmecs? How about something so ad hoc that it cannot\n"
       "be described without embarrasment and shame?\n"
       "\n"
       "-- Feb 2014 - Zhark the Phantasmal\n"
      , orfilt_len
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dDeghost main"); machdep(); AFNI_logger("3dDeghost",argc,argv);
   PRINT_VERSION("3dDeghost") ;

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[iarg],"-filt") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       orfilt_len = (int)strtod(argv[iarg],NULL) ;
       if( orfilt_len > 1 && orfilt_len%2 == 0 ){
         orfilt_len++ ;
         INFO_message("-filt %d has been adjusted to %d (must be odd)" ,
                      orfilt_len-1 , orfilt_len) ;
       }
       if( orfilt_len > 19 )
         WARNING_message("-filt %d is over the recommended limit of 19",orfilt_len) ;
       iarg++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal value after -prefix!\n");
       iarg++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[iarg],"-input") == 0 || strcasecmp(argv[iarg],"-inset") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       if( inset != NULL )
         ERROR_exit("You can't give the input dataset twice!") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
       iarg++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[iarg],"-FPS") == 0 ){  /* stolen from 3dAllineate.c */
       char *fps ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option '%s'",argv[iarg-1]) ;
       fps = argv[iarg] ;
       if( strlen(fps) < 3 ) ERROR_exit("Code '%s' after '%s' is too short",
                                        fps , argv[iarg-1] ) ;
       switch( fps[0] ){
         default: ERROR_exit("Illegal '%s' F code '%c' :-(" , argv[iarg-1],fps[0] );
         case 'i': case 'I': case 'x': case 'X': case '1':  fe = 1; break;
         case 'j': case 'J': case 'y': case 'Y': case '2':  fe = 2; break;
         case 'k': case 'K': case 'z': case 'Z': case '3':  fe = 3; break;
       }
       switch( fps[1] ){
         default: ERROR_exit("Illegal '%s' P code '%c' :-(" , argv[iarg-1],fps[1] );
         case 'i': case 'I': case 'x': case 'X': case '1':  pe = 1; break;
         case 'j': case 'J': case 'y': case 'Y': case '2':  pe = 2; break;
         case 'k': case 'K': case 'z': case 'Z': case '3':  pe = 3; break;
       }
       switch( fps[2] ){
         default: ERROR_exit("Illegal '%s' S code '%c' :-(" , argv[iarg-1],fps[2] );
         case 'i': case 'I': case 'x': case 'X': case '1':  se = 1; break;
         case 'j': case 'J': case 'y': case 'Y': case '2':  se = 2; break;
         case 'k': case 'K': case 'z': case 'Z': case '3':  se = 3; break;
       }
       if( fe+pe+se != 6 ) ERROR_exit("Code '%s' after '%s' is nonsensical",
                                      fps , argv[iarg-1] ) ;
       iarg++ ; continue ;
     }

     /*---*/

     ERROR_exit("Unknown option: %s\n",argv[iarg]);
   }

   if( inset == NULL && iarg >= argc )
     ERROR_exit("No dataset name on command line?\n");

   /*-- read input if needed --*/

   if( inset == NULL ){
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
     DSET_load( inset ) ; CHECK_LOAD_ERROR(inset) ;
   }

   /*-- filter input? --*/

   nvals = DSET_NVALS(inset) ;
   if( orfilt_len > nvals/2 ){
     orfilt_len = nvals/2 ; if( orfilt_len%2 == 0 ) orfilt_len++ ;
   }

   if( orfilt_len > 1 && nvals > 1 ){
     MRI_vectim *invect ; int ii ;
     if( verb )
       INFO_message("Filtering input dataset: filter length=%d",orfilt_len) ;
     invect = THD_dset_to_vectim(inset,NULL,0) ;
     THD_vectim_applyfunc( invect , orfilt_vector ) ;
     filset = EDIT_empty_copy( inset ) ;
     for( ii=0 ; ii < nvals ; ii++ )
       EDIT_substitute_brick( filset , ii , MRI_float , NULL ) ;
     THD_vectim_to_dset( invect , filset ) ;
     VECTIM_destroy(invect) ;
   } else {
     if( verb )
       INFO_message("Time series filtering is turned off") ;
   }

   /***** outsource the work *****/

   outset = THD_deghoster( inset , (filset!=NULL)?filset:inset , pe,fe,se ) ;
   if( outset == NULL ) ERROR_exit("THD_deghoster fails :-(((") ;
   if( filset != NULL ) DSET_delete(filset) ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dDeghost" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}

/*============================================================================*/
/*** Stuff for the time series filtering ***/

#undef  DTYPE
#define DTYPE float
#include "cs_qsort_small.h"

#undef  OFILT
#define OFILT(j) 0.2f*(ar[j-1]+ar[j+1]+3.0f*ar[j])            /* odd lengths */

#undef  EFILT
#define EFILT(j) 0.2f*(ar[j-2]+ar[j+1])+0.3f*(ar[j-1]+ar[j])  /* even lengths */

static float orfilt( int n , float *ar )
{
   int nby2 ;
   switch( n ){                /* fast cases */
     case 1:   return ar[0] ;
     case 2:   return 0.5f*(ar[0]+ar[1]) ;

     case 3:   qsort3_float(ar)  ; return OFILT(1) ;
     case 5:   qsort5_float(ar)  ; return OFILT(2) ;
     case 7:   qsort7_float(ar)  ; return OFILT(3) ;
     case 9:   qsort9_float(ar)  ; return OFILT(4) ;
     case 11:  qsort11_float(ar) ; return OFILT(5) ;
     case 13:  qsort13_float(ar) ; return OFILT(6) ;
     case 15:  qsort15_float(ar) ; return OFILT(7) ;
     case 17:  qsort17_float(ar) ; return OFILT(8) ;
     case 19:  qsort19_float(ar) ; return OFILT(9) ;
     case 21:  qsort21_float(ar) ; return OFILT(10) ;
     case 25:  qsort25_float(ar) ; return OFILT(12) ;
     case 27:  qsort27_float(ar) ; return OFILT(13) ;

     case  4:  qsort4_float(ar)  ; return EFILT(2) ;
     case  6:  qsort6_float(ar)  ; return EFILT(3) ;
     case  8:  qsort8_float(ar)  ; return EFILT(4) ;
     case  10: qsort10_float(ar) ; return EFILT(5) ;
     case  12: qsort12_float(ar) ; return EFILT(6) ;
     case  14: qsort14_float(ar) ; return EFILT(7) ;
     case  16: qsort16_float(ar) ; return EFILT(8) ;
     case  18: qsort18_float(ar) ; return EFILT(9) ;
     case  20: qsort20_float(ar) ; return EFILT(10) ;
   }

   /* general case for n not in above list -- will be slower */

   qsort_float(n,ar) ;
   nby2 = n/2 ;
   return (n%2==0) ? EFILT(nby2) : OFILT(nby2) ;
}

#undef OFILT
#undef EFILT

/*----------------------------------------------------------------------------*/

void orfilt_vector( int nvec , float *vec )
{
   static float *ar=NULL ; static int nar=0 ;
   static float *vv=NULL ; static int nvv=0 ;
   int ii , ibot,itop , nby2=orfilt_len/2 , nv1=nvec-1,nii ;

   if( orfilt_len == 0 ){
     if( ar != NULL ){ free(ar); nar = 0; ar = NULL; }
     if( vv != NULL ){ free(vv); nvv = 0; vv = NULL; }
     return ;
   }
   if( orfilt_len == 1 ) return ;

   if( ar == NULL || nar < orfilt_len ){
     nar = orfilt_len ; ar = (float *)realloc(ar,sizeof(float)*nar) ;
   }
   if( vv == NULL || nvv < nvec ){
     nvv = nvec ; vv = (float *)realloc(vv,sizeof(float)*nvv) ;
   }

   for( ii=0 ; ii < nvec ; ii++ ){
     ibot = ii-nby2 ; if( ibot < 0   ) ibot = 0 ;
     itop = ii+nby2 ; if( itop > nv1 ) itop = nv1 ;
     nii  = itop - ibot + 1 ;
     memcpy( ar , vec+ibot , sizeof(float)*nii ) ;
     vv[ii] = orfilt( nii , ar ) ;
   }

   memcpy( vec , vv , sizeof(float)*nvec ) ;
   return ;
}
/*============================================================================*/

/***------------------------------------------------------------------------***/

 /* 'vec' variables are in smask = brain voxels whose N/2 location is outside */
static int    nvec ;
static float *bvec=NULL, *gvec=NULL, *xvec=NULL, *yvec=NULL, *ctvec=NULL, *stvec=NULL ;
static byte  *smask=NULL ;

 /* 'vim' variables cover the whole slice */
static int    nvim ;
static float *bvim=NULL, *gvim=NULL, *xvim=NULL, *yvim=NULL, *ctvim=NULL, *stvim=NULL ;

static float noise_estimate=0.0f ;  /* initial estimate of noise level */

 /* these parameters define theta(x,y) */
static int   ntheta_par=2 ;
static double theta_par[2] , d_par ;

void compute_theta( int npt , float *xpt   , float *ypt ,
                              float *cpt   , float *spt  )
{
   float th0=theta_par[0] , th1=theta_par[1] , thth ; int ii ;
   for( ii=0 ; ii < npt ; ii++ ){
     thth    = th1*(xpt[ii]-th0) ;
     cpt[ii] = fabsf(cosf(thth)) ;
     spt[ii] = fabsf(sinf(thth)) ;
   }
   return ;
}

void compute_thvec(void)  /* simplest model */
{
   compute_theta( nvec,xvec,yvec , ctvec,stvec ) ;
}

compute_thvim(void)
{
   compute_theta( nvim,xvim,yvim , ctvim,stvim ) ;
}

/***------------------------------------------------------------------------***/
/*** Given Iy, Iyn, theta (in form of cos and sin), and noise level d,
     find M that is the solution to

      F(M) =    Iy*cos(theta)^2 / sqrt( M^2 * cos(theta)^2 + d^2 )
             + Iyn*sin(theta)^2 / sqrt( M^2 * sin(theta)^2 + d^2 ) - 1 = 0

     If d=0, M is solved for analytically (mzero).  If d > 0, then 3 steps
     of Newton's method are used -- unless d is too big, in which case M=0
     is the return value.
*//*------------------------------------------------------------------------***/

#undef  COMPUTE_ff_df
#define COMPUTE_ff_df(mm)                                                   \
 { mq = (mm)*(mm); mcd = sqrt(mq*ctq+dq) ; msd = sqrt(mq*stq+dq) ;          \
   ff = iy*ctq/mcd + iyn*stq/msd - 1.0f ;                                   \
   df = -iy*(mm)*ctq*ctq/(mcd*mcd*mcd) - iyn*(mm)*stq*stq/(msd*msd*msd) ; }

float find_mhat( float iy , float iyn , float ct , float st , float d )
{
   float mzero, ff,df, ctq=ct*ct, stq=st*st, mq, dq=d*d , mval , msd,mcd ;

   mzero = iy*ct  + iyn*st  ;                  /* analytic solution for d=0 */

   if( d <= 0.0f             ) return mzero ;  /* easiest case */
   if( d >= iy*ctq + iyn*stq ) return 0.0f  ;  /* unlikely */

   COMPUTE_ff_df(mzero) ;
   if( ff >= 0.0f || df >= 0.0f ) return mzero ;  /* should not happen */
   mval = mzero - ff/df ;                         /* Newton step #1 */
   if( mval <= 0.0 ) return 0.0 ;                 /* should not happen */

   COMPUTE_ff_df(mval) ;
   if( df >= 0.0f ) return mval ;                 /* should not happen */
   mval = mval - ff/df ;                          /* Newton step #2 */
   if( mval <= 0.0 ) return 0.0 ;                 /* should not happen */

   COMPUTE_ff_df(mval) ;
   if( df >= 0.0f ) return mval ;                 /* should not happen */
   mval = mval - ff/df ;                          /* Newton step #3 */
   if( mval <= 0.0 ) return 0.0 ;                 /* should not happen */

   return mval ;
}

/***------------------------------------------------------------------------***/
/*** Given Iy, Iyn, theta (in form of cos and sin), and noise level d,
     find My and Myn that are the solution to

       Iy^2  = My^2 * cos(theta)^2 + Myn^2 * sin(theta)^2 + d^2
       Iyn^2 = My^2 * sin(theta)^2 + Myn^2 * cos(theta)^2 + d^2
*//*------------------------------------------------------------------------***/

float_pair find_mpair( float iy , float iyn , float ct , float st , float d )
{
   float ctq=ct*ct , stq=st*st , dq=d*d , iyt,iynt , my,myn , den ;
   float_pair result={0.0f,0.0f} ;

   den = ctq*ctq - stq*stq ; if( den <= 0.0f ) return result ;  /* bad inputs */

   iyt  = iy *iy  - dq ; if( iyt  < 0.0f ) iyt  = 0.0f ;
   iynt = iyn*iyn - dq ; if( iynt < 0.0f ) iynt = 0.0f ;

   my  = ( iyt*ctq - iynt*stq) / den ; if( my  < 0.0f ) my  = 0.0f ;
   myn = (-iyt*stq + iynt*ctq) / den ; if( myn < 0.0f ) myn = 0.0f ;

   result.a = sqrtf(my) ; result.b = sqrtf(myn) ; return result ;
}


/***------------------------------------------------------------------------***/
/* this function is the target for powell_newuoa_con() */

double theta_func( int npar , double *thpar )
{
   int ii ; double sum=0.0 ; float mhat,e1,e2 ;

   theta_par[0] = thpar[0] ; theta_par[1] = thpar[1] ; d_par = thpar[2] ;
   compute_thvec() ;

/** INFO_message("========== theta_func(%g,%g,%g) ==========",thpar[0],thpar[1],thpar[2]) ; **/
   for( ii=0 ; ii < nvec ; ii++ ){
     mhat = find_mhat( bvec[ii],gvec[ii] , ctvec[ii],stvec[ii] , d_par ) ;
     e1   = bvec[ii]-sqrt(mhat*mhat*ctvec[ii]*ctvec[ii]+d_par*d_par) ;
     e2   = gvec[ii]-sqrt(mhat*mhat*stvec[ii]*stvec[ii]+d_par*d_par) ;
     sum += e1*e1 + e2*e2 ;
/** ININFO_message("  bvec=%g gvec=%g ctvec=%g stvec=%g ==> mhat=%g e1=%g e2=%g",
               bvec[ii],gvec[ii] , ctvec[ii],stvec[ii] , mhat,e1,e2) ; **/
   }

   /** penalty for nonzero parameters **/

#if 1
   sum += 0.022*nvec*noise_estimate*noise_estimate
          * ( fabs(thpar[0]) + 99.9*fabs(thpar[1]) ) ;
#endif

/** ININFO_message("RESULT = %g",sum) ; **/
   return sum ;
}

/***------------------------------------------------------------------------***/

void optimize_theta(void)
{
   double thpar[3] , thbot[3] , thtop[3] ; int nite ;

   thpar[0] =  0.00 ;
   thbot[0] = -9.99 ;
   thtop[0] =  9.99 ;

   thpar[1] =  0.000;  /* initial values */
   thbot[1] = -0.002;  /* lower limits */
   thtop[1] =  0.099;  /* upper limits */

   thpar[2] =  0.111*noise_estimate ;
   thbot[2] =  0.001*noise_estimate ;
   thtop[2] =  3.333*noise_estimate ;

   nite = powell_newuoa_con( 3 , thpar,thbot,thtop ,
                             299 , 0.111 , 0.0001 , 999 , theta_func ) ;

   if( fabs(thpar[1]) < 0.00111 ) thpar[0] = thpar[1] = 0.0 ;
   theta_par[0] = thpar[0] ;
   theta_par[1] = thpar[1] ;
   d_par        = thpar[2] ;

   return ;
}

/***------------------------------------------------------------------------***/

#define CLFRAC 0.4f

byte * DEG_automask_image( MRI_IMAGE *im )
{
   byte *bmask , *cmask ;
   float *iar , cval ;
   int nx=im->nx , ny=im->ny , nz=im->nz , nvox=im->nvox , ii ;

   THD_automask_set_clipfrac(CLFRAC) ;
   bmask = mri_automask_image(im) ;
   cmask = (byte *)malloc(sizeof(byte)*nx*ny*nz) ;
   memcpy(cmask,bmask,sizeof(byte)*nx*ny*nz) ;
   iar  = MRI_FLOAT_PTR(im) ;
   cval = THD_cliplevel(im,CLFRAC) ;

   THD_mask_dilate(nx,ny,nz,cmask,3,2) ;  /* embiggen the mask */
   THD_mask_dilate(nx,ny,nz,cmask,3,2) ;
   THD_mask_dilate(nx,ny,nz,cmask,3,2) ;
   THD_mask_dilate(nx,ny,nz,cmask,3,2) ;
   for( ii=0 ; ii < nvox ; ii++ )       /* enlittle it now (cromulently) */
     if( !bmask[ii] && cmask[ii] && iar[ii] < cval ) cmask[ii] = 0 ;

   free(bmask) ; return cmask ;
}

/***------------------------------------------------------------------------***/

#undef  FREEUP
#define FREEUP                                                   \
 do{ mri_free(medim); FREEIF(bmask); FREEIF(amask);              \
     FREEIF(smask); FREEIF(bvec ); FREEIF(gvec ); FREEIF(xvec ); \
     FREEIF(yvec ); FREEIF(ctvec); FREEIF(stvec); FREEIF(bvim ); \
     FREEIF(gvim ); FREEIF(xvim ); FREEIF(yvim ); FREEIF(ctvim); \
     FREEIF(stvim); orfilt_vector(0,NULL);                       \
     FREEIF(xzero_t); FREEIF(thet1_t); FREEIF(dparr_t);          \
 } while(0)

THD_3dim_dataset * THD_deghoster( THD_3dim_dataset *inset ,
                                  THD_3dim_dataset *filset,
                                  int pe , int fe , int se )
{
   MRI_IMAGE *medim=NULL , *tim=NULL , *oim=NULL ;
   float cval, *mar=NULL , *tar=NULL , *oar=NULL ;
   float *xzero_t=NULL , *thet1_t=NULL , *dparr_t=NULL , t1med,t1bmv;
   byte *bmask=NULL , *amask=NULL , sm ;
   int nvox , nx,ny,nz , dp=0,df=0,ds=0 , np=0,nf=0,ns=0,np2,nf2 ;
   int pp,ff,ss,nfp , ii , ppg , nsm,ism , vv,nv , iim , sskip ;
   THD_3dim_dataset *outset=NULL ;
   float iy,iyn ; float_pair mp ;

   /* create brain mask (bmask) */

   medim = THD_median_brick(inset) ;
   bmask = DEG_automask_image(medim) ;  /* brain mask (we hope) */

   nx = medim->nx ; ny = medim->ny ; nz = medim->nz ; nvox = medim->nvox ;
   nv = DSET_NVALS(inset) ;

   /* estimate noise level from data outside the mask (crudely) */

   mar  = MRI_FLOAT_PTR(medim) ;
   cval = THD_cliplevel(medim,CLFRAC) ;
   for( noise_estimate=0.0f,iim=ii=0 ; ii < nvox ; ii++ ){
     if( !bmask[ii] && mar[ii] < cval ){
       noise_estimate += mar[ii] ; iim++ ;
     }
   }
   if( iim < 9 ){ FREEUP; return NULL; }  /* should not happen */
   noise_estimate /= iim ;  /* initial estimate of noise level */
   if( verb > 1 )
     INFO_message("Global crude noise_estimate = %g",noise_estimate) ;

   /* chop out all sub-threshold voxels (amask) */

   amask = (byte *)malloc(sizeof(byte)*nvox) ;  /* clipped brain mask */
   memcpy(amask,bmask,sizeof(byte)*nvox) ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( amask[ii] && mar[ii] < cval ) amask[ii] = 0 ;

   /* setting up slice coordinates f,p,s */

        if( pe == 1 ){ dp = 1     ; np = nx ; }
   else if( pe == 2 ){ dp = nx    ; np = ny ; }
   else if( pe == 3 ){ dp = nx*ny ; np = ns ; }

        if( fe == 1 ){ df = 1     ; nf = nx ; }
   else if( fe == 2 ){ df = nx    ; nf = ny ; }
   else if( fe == 3 ){ df = nx*ny ; nf = nz ; }

        if( se == 1 ){ ds = 1     ; ns = nx ; }
   else if( se == 2 ){ ds = nx    ; ns = ny ; }
   else if( se == 3 ){ ds = nx*ny ; ns = nz ; }

#undef  IJK
#define IJK(f,p,s) ((f)*df+(p)*dp+(s)*ds)

   nvim = nfp = nf * np ; np2 = np / 2 ; nf2 = nf / 2 ;
   smask = (byte * )malloc(sizeof(byte) *nfp) ;
   bvec  = (float *)malloc(sizeof(float)*nfp) ;
   gvec  = (float *)malloc(sizeof(float)*nfp) ;
   xvec  = (float *)malloc(sizeof(float)*nfp) ;
   yvec  = (float *)malloc(sizeof(float)*nfp) ;
   ctvec = (float *)malloc(sizeof(float)*nfp) ;
   stvec = (float *)malloc(sizeof(float)*nfp) ;

   bvim  = (float *)malloc(sizeof(float)*nvim) ;
   gvim  = (float *)malloc(sizeof(float)*nvim) ;
   xvim  = (float *)malloc(sizeof(float)*nvim) ;
   yvim  = (float *)malloc(sizeof(float)*nvim) ;
   ctvim = (float *)malloc(sizeof(float)*nvim) ;
   stvim = (float *)malloc(sizeof(float)*nvim) ;

   xzero_t = (float *)malloc(sizeof(float)*nv) ;
   thet1_t = (float *)malloc(sizeof(float)*nv) ;
   dparr_t = (float *)malloc(sizeof(float)*nv) ;

   /* copy input to output (will be ghost edited later) */

   outset = EDIT_empty_copy(inset) ;
   for( vv=0 ; vv < nv ; vv++ ){
     oim = THD_extract_float_brick(vv,inset) ;
     oar = MRI_FLOAT_PTR(oim) ;
     EDIT_BRICK_FACTOR( outset , vv , 0.0f ) ;
     EDIT_substitute_brick( outset , vv , MRI_float , oar ) ;
     mri_clear_and_free(oim) ;
   }

   /* loop over slices */

   for( ss=0 ; ss < ns ; ss++ ){

     /* make copy of brain mask in this slice,
        then edit it down to voxels in the brain
        whose N/2 point is outside the brain (smask) */

     for( iim=nsm=pp=0 ; pp < np ; pp++ ){
       if( pp >= np2 ) ppg = pp-np2 ; else ppg = pp+np2 ;
       for( ff=0 ; ff < nf ; ff++,iim++ ){
         smask[iim] = sm = amask[IJK(ff,pp,ss)] && !bmask[IJK(ff,ppg,ss)] ;
         xvim[iim] = ff-nf2 ; yvim[iim] = pp-np2 ;
         if( sm ){ xvec[nsm] = xvim[iim]; yvec[nsm] = yvim[iim]; nsm++; }
       }
     }
     if( nsm < nfp/20 ){      /* skip this slice */
       if( verb )
         INFO_message("deghost: skipping slice #%d -- too few points in smask",ss) ;
       continue ;
     }
     nvec = nsm ;
     if( verb )
       INFO_message("deghost: processing slice #%d",ss) ;

     /* smask is now the mask of brain voxels whose
        Nyquist ghost locations are NOT in the brain mask */

     /* loop over time points, estimate the ghost correction parameters */

     for( vv=0 ; vv < nv ; vv++ ){

       tim = THD_extract_float_brick(vv,filset) ;
       tar = MRI_FLOAT_PTR(tim) ;

       /* extract the vector of image values in smask,
          and the vector of image values at the ghost locations */

       for( iim=ism=pp=0 ; pp < np ; pp++ ){
         if( pp >= np2 ) ppg = pp-np2 ; else ppg = pp+np2 ;
         for( ff=0 ; ff < nf ; ff++,iim++ ){
           bvim[iim] = tar[IJK(ff,pp,ss)] ;
           gvim[iim] = tar[IJK(ff,ppg,ss)] ;
           if( smask[iim] ){ bvec[ism] = bvim[iim]; gvec[ism++] = gvim[iim]; }
         }
       }

       /* fit the theta parameters from the smask region and save them */

       optimize_theta() ;

       xzero_t[vv] = theta_par[0] ;
       thet1_t[vv] = theta_par[1] ;
       dparr_t[vv] = d_par ;

       mri_free(tim) ; tim = NULL ;
     }

     /* now check the slice parameters for reasonability */

     sskip = 0 ;
     if( nv > 4 ){
       orfilt_len = 3 ;
       orfilt_vector(nv,xzero_t) ;
       orfilt_vector(nv,thet1_t) ;
       orfilt_vector(nv,dparr_t) ;
       qmedmadbmv_float(nv,thet1_t,&t1med,NULL,&t1bmv) ;
       if( verb )
         ININFO_message("  slice #%d -- median(theta1)=%g stdev=%g ratio=%g",
                        ss,t1med,t1bmv,(t1bmv>0.0f)?t1med/t1bmv:0.0f) ;
       if( t1med == 0.0f || fabsf(t1med) <= 0.111f*t1bmv ){
         sskip = 1 ;
         ININFO_message("  skipping slice #%d -- theta1 too small",ss) ;
       }
     }
     if( sskip ) continue ;  /* skip processing this slice */

     /* loop over time points, estimate the un-ghosted image */

     for( vv=0 ; vv < nv ; vv++ ){

       tim = THD_extract_float_brick(vv,inset) ; /* input data for slice */
       tar = MRI_FLOAT_PTR(tim) ;
       oar = DSET_ARRAY(outset,vv) ;             /* output data for volume */

       /* compute theta at each voxel */

       if( thet1_t[vv] == 0.0f ) continue ; /* ghost amplitude is 0 ==> skip this time point */

       if( verb > 1 )
         ININFO_message("  slice=%d index=%d  theta = %g  %g  %g",
                        ss,vv,xzero_t[vv],thet1_t[vv],dparr_t[vv]) ;

       theta_par[0] = xzero_t[vv]; theta_par[1] = thet1_t[vv]; d_par = dparr_t[vv];
       compute_thvim() ;

       /* compute output values at each voxel:
           (a) inside the smask                  == voxel in brain, N/2 ghost isn't
           (b) not in the smask but in the bmask == voxel && N/2 ghost are in brain
           (c) otherwise                         == voxel is unimportant effluvium  */

       for( iim=pp=0 ; pp < np ; pp++ ){
         if( pp >= np2 ) ppg = pp-np2 ; else ppg = pp+np2 ;
         for( ff=0 ; ff < nf ; ff++,iim++ ){
           iy  = tar[IJK(ff,pp,ss)] ;
           iyn = tar[IJK(ff,ppg,ss)] ;
           if( smask[iim] ){
             oar[IJK(ff,pp,ss)] = find_mhat( iy,iyn , ctvim[iim],stvim[iim] , d_par ) ;
             oar[IJK(ff,ppg,ss)] = 0.0f ;
           } else if( bmask[IJK(ff,pp,ss)] && bmask[IJK(ff,ppg,ss)] ){
             if( ppg > pp ){
               mp = find_mpair( iy,iyn , ctvim[iim],stvim[iim] , d_par ) ;
               oar[IJK(ff,pp,ss)] = mp.a ; oar[IJK(ff,ppg,ss)] = mp.b ;
             }
           } else {
             /* nada: output is already a copy of input */
           }
         }
       }
     }

   } /* end of loop over slices */

   FREEUP ;
   return outset ;
}
