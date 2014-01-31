#include "mrilib.h"

static int verb = 1 ;

#undef  FREEIF
#define FREEIF(x) if((x)!=NULL)free((void *)(x))

THD_3dim_dataset * THD_deghoster( THD_3dim_dataset *inset, int pe,int fe,int se ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   char *prefix = "Deghost" ;
   int iarg ;
   int fe=1 , pe=2 , se=3 ;
   THD_3dim_dataset *inset=NULL , *outset ;

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
       "Options:\n"
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
       "\n"
       "-- Jan 2014 - Zhark the Phantasmal\n"
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

   /***** outsource the work *****/

   outset = THD_deghoster( inset , pe,fe,se ) ;
   if( outset == NULL ) ERROR_exit("THD_deghoster fails :-(((") ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dDeghost" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}

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

     If d=0, M is solved for analytically (mzero).  If d > 0, then 2 steps
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

   mzero = iy*ct  + iyn*st  ;

   if( d <= 0.0f             ) return mzero ;  /* easiest case */
   if( d >= iy*ctq + iyn*stq ) return 0.0f  ;  /* unlikely */

   COMPUTE_ff_df(mzero) ;
   if( ff >= 0.0f || df >= 0.0f ) return mzero ;  /* should not happen */

   mval = mzero - ff/df ;                         /* Newton step #1 */

   COMPUTE_ff_df(mval) ;
   if( df >= 0.0f ) return mval ;                 /* should not happen */

   mval = mzero - ff/df ;                         /* Newton step #2 */

   return mval ;
}

/***------------------------------------------------------------------------***/
/* this function is the target for powell_newuoa_con() */

double theta_func( int npar , double *thpar )
{
   int ii ; double sum=0.0 ; float mhat,e1,e2 ;

   theta_par[0] = thpar[0] ; theta_par[1] = thpar[1] ; d_par = thpar[2] ;
   compute_thvec() ;

   for( ii=0 ; ii < nvec ; ii++ ){
     mhat = find_mhat( bvec[ii],gvec[ii] , ctvec[ii],stvec[ii] , d_par ) ;
     e1   = bvec[ii]-sqrt(mhat*mhat*ctvec[ii]*ctvec[ii]+d_par*d_par) ;
     e1   = gvec[ii]-sqrt(mhat*mhat*stvec[ii]*stvec[ii]+d_par*d_par) ;
     sum += e1*e1 + e2*e2 ;
   }

   /** penalty for nonzero parameters **/

   sum += 0.011*nvec*noise_estimate*noise_estimate
          * ( fabs(thpar[0]) + 99.9*fabs(thpar[1]) ) ;

   return sum ;
}

/***------------------------------------------------------------------------***/

void optimize_theta(void)
{
   double thpar[3] , thbot[3] , thtop[3] ;

   thpar[0] =  0.0 ;
   thbot[0] = -9.9 ;
   thtop[0] =  9.9 ;

   thpar[1] =  0.000;  /* initial values */
   thbot[1] = -0.001;  /* lower limits */
   thtop[1] =  0.09 ;  /* upper limits */

   thpar[2] =  0.111*noise_estimate ;
   thbot[2] =  0.0 ;
   thtop[2] =  2.222*noise_estimate ;

   powell_newuoa_con( 3 , thpar,thbot,thtop ,
                      1999 , 0.1 , 0.001 , 4999 , theta_func ) ;

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

   THD_mask_dilate(nx,ny,nz,cmask,3) ;  /* embiggen the mask */
   THD_mask_dilate(nx,ny,nz,cmask,3) ;
   THD_mask_dilate(nx,ny,nz,cmask,3) ;
   THD_mask_dilate(nx,ny,nz,cmask,3) ;
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
     FREEIF(stvim);                                              \
 } while(0)

THD_3dim_dataset * THD_deghoster( THD_3dim_dataset *inset, int pe,int fe,int se )
{
   MRI_IMAGE *medim=NULL , *tim=NULL ;
   float cval, *mar=NULL , *tar=NULL ;
   byte *bmask=NULL , *amask=NULL , sm ;
   int nvox , nx,ny,nz , dp=0,df=0,ds=0 , np=0,nf=0,ns=0,np2,nf2 ;
   int pp,ff,ss,nfp , ii , ppg , nsm,ism , vv,nv , iim ;

   /* create brain mask */

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
   noise_estimate /= iim ;  /* initial estimate of noise level */
   INFO_message("noise_estimate = %g",noise_estimate) ;

   /* chop out all sub-threshold voxels */

   amask = (byte *)malloc(sizeof(byte)*nvox) ;  /* clipped brain mask */
   memcpy(amask,bmask,sizeof(byte)*nvox) ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( amask[ii] && mar[ii] < cval ) amask[ii] = 0 ;

   /* setting up slice coordinates f,p,s */

        if( pe == 1 ){ dp = 1     ; np = nx ; }
   else if( pe == 2 ){ dp = ny    ; np = ny ; }
   else if( pe == 3 ){ dp = ny*nz ; np = ns ; }

        if( fe == 1 ){ df = 1     ; nf = nx ; }
   else if( fe == 2 ){ df = ny    ; nf = ny ; }
   else if( fe == 3 ){ df = ny*nz ; nf = nz ; }

        if( se == 1 ){ ds = 1     ; ns = nx ; }
   else if( se == 2 ){ ds = ny    ; ns = ny ; }
   else if( se == 3 ){ ds = ny*nz ; ns = nz ; }

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

   /* loop over slices */

   for( ss=0 ; ss < ns ; ss++ ){

     /* make copy of brain mask in this slice, then edit it down */

     for( iim=nsm=pp=0 ; pp < np ; pp++ ){
       if( pp >= np2 ) ppg = pp-np2 ; else ppg = pp+np2 ;
       for( ff=0 ; ff < nf ; ff++,iim++ ){
         smask[iim] = sm = amask[IJK(ff,pp,ss)] && !bmask[IJK(ff,ppg,ss)] ;
         xvim[iim] = ff-nf2 ; yvim[iim] = pp-np2 ;
         if( sm ){ xvec[nsm] = xvim[iim]; yvec[nsm] = yvim[iim]; nsm++; }
       }
     }
     if( nsm < nfp/20 ){      /* skip this slice */
       if( verb ) WARNING_message("THD_deghost: skipping slice #%d: nsm=%d nfp=%d",ss,nsm,nfp) ;
       continue ;
     }
     nvec = nsm ;

     /* smask is now the mask of brain voxels whose
        Nyquist ghost locations are NOT in the brain mask */

     /* loop over time points */

     for( vv=0 ; vv < nv ; vv++ ){

       tim = THD_extract_float_brick(vv,inset) ;
       tar = MRI_FLOAT_PTR(tim) ;

       /* extract the vector of image values in smask,
          and the vector of image values at the ghost locations */

       for( iim=ism=pp=0 ; pp < np ; pp++ ){
         if( pp >= np2 ) ppg = pp-np2 ; else ppg = pp+np2 ;
         for( ff=0 ; ff < nf ; ff++,iim++ ){
           bvim[iim] = tar[IJK(ff,pp,ss)] ;
           gvim[iim] = tar[IJK(ff,ppg,ss)] ;
           if( smask[ff+pp*nf] ){ bvec[ism] = bvim[iim]; gvec[ism++] = gvim[iim]; }
         }
       }

       /* fit the theta parameters from the smask region,
          then compute theta over the entire image (in and out of smask) */

       optimize_theta() ;
INFO_message("slice=%d index=%d  theta = %g  %g  %g",ss,vv,theta_par[0],theta_par[1],d_par) ;
       /** compute_thvim() ; **/

       mri_free(tim) ; tim = NULL ;
     }

   }

   FREEUP ;
   return NULL ;
}
