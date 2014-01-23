#include "mrilib.h"

static int verb = 1 ;

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
static float *bvec, *gvec, *xvec, *yvec, *ctvec, *stvec ;
static byte  *smask ;

 /* 'vim' variables cover the whole slice */
static int    nvim ;
static float *bvim, *gvim, *xvim, *yvim, *ctvim, *stvim ;

 /* these parameters define theta(x,y) */
static int   ntheta_par=2 ;
static double theta_par[2] ;

void compute_theta( int npt , float *xpt   , float *ypt ,
                              float *cpt   , float *spt  )
{
   float th0=theta_par[0] , th1=theta_par[1] , thth ; int ii ;
   for( ii=0 ; ii < npt ; ii++ ){
     thth = th0 + th1 * xpt[ii] ;
     cpt[ii] = cosf(thth) ;
     spt[ii] = sinf(thth) ;
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
/* this function is the target for powell_newuoa_con() */

double theta_func( int npar , double *thpar )
{
   int ii ; double sum ; float mhat,e1,e2 ;

   theta_par[0] = thpar[0] ; theta_par[1] = thpar[1] ;
   compute_thvec() ;

   for( ii=0 ; ii < nvec ; ii++ ){
#if 0
     mhat = bvec[ii]*ctvec[ii] + gvec[ii]*stvec[ii] ;
     e1   = bvec[ii]-mhat*ctvec[ii] ;
     e2   = gvec[ii]-mhat*stvec[ii] ;
     sum += e1*e1 + e2*e2 ;
#else
     e1   = bvec[ii]*stvec[ii] - gvec[ii]*ctvec[ii] ;
     sum += e1*e1 ;
#endif
   }

   return sum ;
}

/***------------------------------------------------------------------------***/

void optimize_theta(void)
{
   double thpar[2] , thbot[2] , thtop[2] ;

   thpar[0] =  0.0 ; thpar[1] =  0.00 ;  /* initial values */
   thbot[0] = -0.1 ; thbot[1] = -0.04 ;  /* lower limits */
   thtop[0] =  0.1 ; thtop[1] =  0.04 ;  /* upper limits */
   iter = powell_newuoa_con( 2 , thpar,thbot,thtop ,
                             49 , 0.1 , 0.001 , 666 , theta_func ) ;
   theta_par[0] = thpar[0] ;
   theta_par[1] = thpar[1] ;
   return ;
}

/***------------------------------------------------------------------------***/

#define CLFRAC 0.4f

byte * DEG_automask_image( MRI_IMAGE *im )
{
   byte *bmask ;
   float *iar , cval ;
   int nx=im->nx , ny=im->ny , nz=im->nz , nvox=im->nvox , ii ;

   THD_automask_set_clipfrac(CLFRAC) ;
   bmask = mri_automask_image(im) ;
   cmask = (byte *)malloc(sizeof(byte)*nx*ny*nz) ;
   memcpy(cmask,bmask,sizeof(byte)*nx*ny*nz) ;
   iar  = MRI_FLOAT_PTR(im) ;
   cval = THD_cliplevel(im,CLFRAC) ;

   THD_mask_dilate(nx,ny,nz,cmask,3) ;
   THD_mask_dilate(nx,ny,nz,cmask,3) ;
   THD_mask_dilate(nx,ny,nz,cmask,3) ;
   THD_mask_dilate(nx,ny,nz,cmask,3) ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( !bmask[ii] && cmask[ii] && iar[ii] < cval ) cmask[ii] = 0 ;

   free(bmask) ;
   return cmask ;
}

/***------------------------------------------------------------------------***/

THD_3dim_dataset * THD_deghoster( THD_3dim_dataset *inset, int pe,int fe,int se )
{
   MRI_IMAGE *medim , *tim ;
   float cval , *mar , *tar ;
   byte *bmask , *amask , sm ;
   int nvox , nx,ny,nz , dp=0,df=0,ds=0 , np=0,nf=0,ns=0,np2 ;
   int pp,ff,ss,nfp , ii , ppg , nsm,ism , vv,nv , iim ;

   /* create brain mask */

   medim = THD_median_brick(inset) ;
   bmask = DEG_automask_image(medim) ;  /* brain mask */

   nx = medim->nx ; ny = medim->ny ; mz = medim->nz ; nvox = medim->nvox ;
   nv = DSET_NVALS(inset) ;

   /* chop out sub-threshold voxels */

   mar   = MRI_FLOAT_PTR(medim) ;
   cval  = THD_cliplevel(medim,CLFRAC) ;
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

   nvim = nfp = nf * np ; np2 = np / 2 ;
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
       if( verb ) WARNING_message("THD_deghost: skipping slice #%d",ss) ;
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
       compute_thvim() ;

       /*
