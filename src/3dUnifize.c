#include "mrilib.h"

static int verb = 1 ;

#ifdef USE_OMP
# include <omp.h>
#endif

/*---------------------------------------------------------------------------*/
#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)
#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*----- fast median-of-7 -----*/

static INLINE float median7(float *p)
{
    register float temp ;
    SORT2(p[0],p[1]) ; SORT2(p[4],p[5]) ; SORT2(p[1],p[2]) ;
    SORT2(p[5],p[6]) ; SORT2(p[0],p[1]) ; SORT2(p[4],p[5]) ;
    SORT2(p[0],p[4]) ; SORT2(p[2],p[6]) ; SORT2(p[1],p[3]) ;
    SORT2(p[3],p[5]) ; SORT2(p[1],p[3]) ; SORT2(p[2],p[3]) ;
    SORT2(p[3],p[4]) ; SORT2(p[2],p[3]) ; return(p[3]) ;
}

/*---------------------------------------------------------------------------*/
/* Shrink a 3D image down by a factor of 2 in all dimensions,
   by taking the median of each point and its 6 nearest neighbors. */

#undef  FSUB
#define FSUB(far,i,j,k,ni,nij) far[(i)+(j)*(ni)+(k)*(nij)]

MRI_IMAGE * mri_double_down( MRI_IMAGE *fim )
{
   MRI_IMAGE *gim=NULL ;
   float *far , *gar , par[7] ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk ;
   int iuu,iup,ium , juu,jum,jup , kuu,kum,kup ;

ENTRY("mri_double_down") ;

   if( fim == NULL ) RETURN(NULL) ;

   /* process non-float image? */

   if( fim->kind != MRI_float ){
     MRI_IMAGE *qim = mri_to_float(fim) ;
     gim = mri_double_down(qim) ; mri_free(qim) ; RETURN(gim) ;
   }

   /* f=input  g=output */

   nxf = fim->nx ; nyf = fim->ny ; nzf = fim->nz ;
   nxg = nxf / 2 ; if( nxg < 1 ) nxg = 1 ;
   nyg = nyf / 2 ; if( nyg < 1 ) nyg = 1 ;
   nzg = nzf / 2 ; if( nzg < 1 ) nzg = 1 ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_float) ;
   gar = MRI_FLOAT_PTR(gim) ;
   far = MRI_FLOAT_PTR(fim) ;

   /* for each output voxel (gim), take the median of the
      corresponding input voxel and its 6 nearest neighbors */

   for( kk=0 ; kk < nzg ; kk++ ){
    kuu = 2*kk ; kum = kuu-1 ; if( kum <  0   ) kum = 0 ;
                 kup = kuu+1 ; if( kup >= nzf ) kup = nzf-1 ;
    for( jj=0 ; jj < nyg ; jj++ ){
      juu = 2*jj ; jum = juu-1 ; if( jum <  0   ) jum = 0 ;
                   jup = juu+1 ; if( jup >= nyf ) jup = nyf-1 ;
      for( ii=0 ; ii < nxg ; ii++ ){
        iuu = 2*ii ; ium = iuu-1 ; if( ium <  0   ) ium = 0 ;
                     iup = iuu+1 ; if( iup >= nxf ) iup = nxf-1 ;
        par[0] = FSUB(far,iuu,juu,kuu,nxf,nxyf) ;  /* load par */
        par[1] = FSUB(far,ium,juu,kuu,nxf,nxyf) ;  /* with the */
        par[2] = FSUB(far,iup,juu,kuu,nxf,nxyf) ;  /* 7 values */
        par[3] = FSUB(far,iuu,jum,kuu,nxf,nxyf) ;  /* at and   */
        par[4] = FSUB(far,iuu,jup,kuu,nxf,nxyf) ;  /* around   */
        par[5] = FSUB(far,iuu,juu,kum,nxf,nxyf) ;  /* the voxel */
        par[6] = FSUB(far,iuu,juu,kup,nxf,nxyf) ;
        FSUB(gar,ii,jj,kk,nxg,nxyg) = median7(par) ;
   }}}

   RETURN(gim) ;
}

/*---------------------------------------------------------------------------*/
/* Expand a 3D image by a factor of 2 in all directions, by averaging.
   Plus 1 more in x if xadd is nonzero, etc.
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * mri_double_up( MRI_IMAGE *fim , int xadd,int yadd,int zadd )
{
   MRI_IMAGE *gim ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk , im,jm,km,ip,jp,kp ;
   float *far , *gar ;

ENTRY("mri_double_up") ;

   if( fim == NULL ) RETURN(NULL) ;

   /* process a non-float image? */

   if( fim->kind != MRI_float ){
     MRI_IMAGE *qim = mri_to_float(fim) ;
     gim = mri_double_up(qim,xadd,yadd,zadd) ; mri_free(qim) ; RETURN(gim) ;
   }

   /* f=input  g=output */

   nxf = fim->nx ; nyf = fim->ny ; nzf = fim->nz  ;

   nxg = (nxf == 1) ? 1 : (2*nxf+(xadd != 0)) ;
   nyg = (nyf == 1) ? 1 : (2*nyf+(yadd != 0)) ;
   nzg = (nzf == 1) ? 1 : (2*nzf+(zadd != 0)) ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_float) ;
   gar = MRI_FLOAT_PTR(gim) ;
   far = MRI_FLOAT_PTR(fim) ;

   /* for even output indexes, use the corresponding index in the input;
      for odd output indexes, use the neighboring indexes in the input. */

   for( kk=0 ; kk < nzg ; kk++ ){
    kp = km = kk/2 ;  if( kp >= nzf ) kp = km = nzf-1 ;
    if( kk%2 ){ kp++; if( kp >= nzf ) kp = nzf-1; }
    for( jj=0 ; jj < nyg ; jj++ ){
      jp = jm = jj/2 ;  if( jp >= nyf ) jp = jm = nyf-1 ;
      if( jj%2 ){ jp++; if( jp >= nyf ) jp = nyf-1; }
      for( ii=0 ; ii < nxg ; ii++ ){
        ip = im = ii/2 ;  if( ip >= nxf ) ip = im = nxf-1 ;
        if( ii%2 ){ ip++; if( ip >= nxf ) ip = nxf-1; }
        FSUB(gar,ii,jj,kk,nxg,nxyg) =
          0.125f * ( FSUB(far,im,jm,km,nxf,nxyf) + FSUB(far,ip,jm,km,nxf,nxyf)
                    +FSUB(far,im,jp,km,nxf,nxyf) + FSUB(far,ip,jp,km,nxf,nxyf)
                    +FSUB(far,im,jm,kp,nxf,nxyf) + FSUB(far,ip,jm,kp,nxf,nxyf)
                    +FSUB(far,im,jp,kp,nxf,nxyf) + FSUB(far,ip,jp,kp,nxf,nxyf) ) ;
   }}}

   RETURN(gim) ;
}

/*---------------------------------------------------------------------------*/
/* Find the percentile perc (0..100) in a radius of vrad voxels.
   (Not really used in this program, but here it is if you want it.) */

MRI_IMAGE * mri_local_percentile( MRI_IMAGE *fim , float vrad , float perc )
{
   MRI_IMAGE *aim , *bim , *cim , *dim ;
   float     *aar , *bar , *car , *dar , *nbar ;
   byte      *ams , *bms ;
   MCW_cluster *nbhd ;
   int ii,jj,kk , nbar_num , nx,ny,nz,nxy ;
   float fq,val ; int qq,qp,qm ;

ENTRY("mri_local_percentile") ;

   if( fim == NULL || vrad < 4.0f || perc < 0.0f || perc > 100.0f ) RETURN(NULL) ;

   /* compute automask */

   aim = mri_to_float(fim) ; aar = MRI_FLOAT_PTR(aim) ;
   ams = mri_automask_image(aim) ;
   if( ams == NULL ){ mri_free(aim) ; RETURN(NULL) ; }

   /* apply automask to image copy */

   for( ii=0 ; ii < aim->nvox ; ii++ ) if( ams[ii] == 0 ) aar[ii] = 0.0f ;
   free(ams) ;

   /* shrink image by 2 for speedup */

   bim = mri_double_down(aim) ; bar = MRI_FLOAT_PTR(bim) ; mri_free(aim) ;
   bms = (byte *)malloc(sizeof(byte)*bim->nvox) ;
   for( ii=0 ; ii < bim->nvox ; ii++ ) bms[ii] = (bar[ii] != 0.0f) ;

   /* neighborhood has 1/2 radius in the shrunken volume */

   nbhd = MCW_spheremask( 1.0f,1.0f,1.0f , 0.5f*vrad+0.001f ) ;
   nbar = (float *)malloc(sizeof(float)*nbhd->num_pt) ;

   cim = mri_new_conforming(bim,MRI_float) ; car = MRI_FLOAT_PTR(cim) ;
   SetSearchAboutMaskedVoxel(1) ;

   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ; nxy = nx*ny ;

   /* for each voxel:
        extract neighborhood array, sort it, get result */

   for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ){
         nbar_num = mri_get_nbhd_array( bim,bms , ii,jj,kk , nbhd,nbar ) ;
         if( nbar_num < 1 ){
           val = 0.0f ;
         } else {
           qsort_float(nbar_num,nbar) ;
           if( nbar_num == 1 || perc <= 0.000001f ){
             val = nbar[0] ;
           } else if( perc >= 99.9999f ){
             val = nbar[nbar_num-1] ;
           } else {
             fq = (0.01f*perc)*nbar_num ;
             qq = (int)fq ; qp = qq+1 ; if( qp == nbar_num ) qp = qq ;
                            qm = qq-1 ; if( qm <  0        ) qm = 0  ;
             val = 0.3333333f * ( nbar[qm] + nbar[qq] + nbar[qp] ) ;
           }
         }
         FSUB(car,ii,jj,kk,nx,nxy) = val ;
   }}}

   mri_free(bim) ; free(bms) ; free(nbar) ; KILL_CLUSTER(nbhd) ;

   dim = mri_double_up( cim , fim->nx%2 , fim->ny%2 , fim->nz%2 ) ;

   mri_free(cim) ;

   RETURN(dim) ;
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)  /* for -verb voxel loop message */
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*---------------------------------------------------------------------------*/
/* Get the local (vrad radius) average value between percentiles p1 and p2. */

MRI_IMAGE * mri_local_percmean( MRI_IMAGE *fim , float vrad , float p1, float p2 )
{
   MRI_IMAGE *aim , *bim , *cim , *dim ;
   float     *aar , *bar , *car , *dar ;
   byte      *ams , *bms ;
   MCW_cluster *nbhd ;
   int ii , nx,ny,nz,nxy,nxyz ;

ENTRY("mri_local_percmean") ;

   if( p1 > p2 ){ float val = p1; p1 = p2; p2 = val; }

   if( fim == NULL || vrad < 4.0f || p1 < 0.0f || p2 > 100.0f ) RETURN(NULL) ;

   /* just one percentile? */

   if( p1 == p2 ) RETURN( mri_local_percentile(fim,vrad,p1) ) ;

   if( verb ) fprintf(stderr,"A") ;

   /* create automask of copy of input image */

   aim = mri_to_float(fim) ; aar = MRI_FLOAT_PTR(aim) ;
   ams = mri_automask_image(aim) ;
   if( ams == NULL ){ mri_free(aim) ; RETURN(NULL) ; }

   /* apply automask to copy of input image */

   for( ii=0 ; ii < aim->nvox ; ii++ ) if( ams[ii] == 0 ) aar[ii] = 0.0f ;
   free(ams) ;

   /* shrink image by 2 for speed */

   if( verb ) fprintf(stderr,"D") ;

   bim = mri_double_down(aim) ; bar = MRI_FLOAT_PTR(bim) ; mri_free(aim) ;
   bms = (byte *)malloc(sizeof(byte)*bim->nvox) ;
   for( ii=0 ; ii < bim->nvox ; ii++ ) bms[ii] = (bar[ii] != 0.0f) ;

   /* create neighborhood mask (1/2 radius in the shrunken copy) */

   nbhd = MCW_spheremask( 1.0f,1.0f,1.0f , 0.5f*vrad+0.001f ) ;

   cim = mri_new_conforming(bim,MRI_float) ; car = MRI_FLOAT_PTR(cim) ;
   SetSearchAboutMaskedVoxel(1) ;

   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   /* for each output voxel,
        extract neighborhood array, sort it, average desired range.
        Since this is the slowest part of the code, it is now OpenMP-ized. */

#ifndef USE_OMP       /* old serial code */
 { int vvv,vstep , ii,jj,kk , nbar_num ; float val , *nbar ;
   nbar = (float *)malloc(sizeof(float)*nbhd->num_pt) ;
   vstep = (verb) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"\n + Voxel loop: ") ;
   for( vvv=kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,vvv++ ){
         if( vstep && vvv%vstep == vstep-1 ) vstep_print() ;
         nbar_num = mri_get_nbhd_array( bim,bms , ii,jj,kk , nbhd,nbar ) ;
         if( nbar_num < 1 ){              /* no data */
           val = 0.0f ;
         } else {
           qsort_float(nbar_num,nbar) ;   /* sort */
           if( nbar_num == 1 ){           /* stoopid case */
             val = nbar[0] ;
           } else {             /* average values from p1 to p2 percentiles */
             int q1,q2,qq ;
             q1 = (int)( 0.01f*p1*(nbar_num-1) ) ;  /* p1 location */
             q2 = (int)( 0.01f*p2*(nbar_num-1) ) ;  /* p2 location */
             for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
             val /= (q2-q1+1.0f) ;
           }
         }
         FSUB(car,ii,jj,kk,nx,nxy) = val ;
   }}}
   free(nbar) ;
   if( vstep ) fprintf(stderr,"!") ;
 }
#else              /* new parallel code [06 Mar 2013 = Snowquestration Day!] */
 AFNI_OMP_START ;
 if( verb ) fprintf(stderr,"V") ;
#pragma omp parallel
 { int vvv , ii,jj,kk,qq , nbar_num ; float val , *nbar ;
   nbar = (float *)malloc(sizeof(float)*nbhd->num_pt) ;
#pragma omp for
   for( vvv=0 ; vvv < nxyz ; vvv++ ){
     ii = vvv % nx ; kk = vvv / nxy ; jj = (vvv-kk*nxy) / nx ;
     nbar_num = mri_get_nbhd_array( bim,bms , ii,jj,kk , nbhd,nbar ) ;
     if( nbar_num < 1 ){              /* no data */
       val = 0.0f ;
     } else {
       qsort_float(nbar_num,nbar) ;   /* sort */
       if( nbar_num == 1 ){           /* stoopid case */
         val = nbar[0] ;
       } else {             /* average values from p1 to p2 percentiles */
         int q1,q2,qq ;
         q1 = (int)( 0.01f*p1*(nbar_num-1) ) ;  /* p1 location */
         q2 = (int)( 0.01f*p2*(nbar_num-1) ) ;  /* p2 location */
         for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
         val /= (q2-q1+1.0f) ;
         if( verb && vvv%66666==0 ) fprintf(stderr,".") ;
       }
     }
     car[vvv] = val ;
   }
   free(nbar) ;
 } /* end parallel code */
 AFNI_OMP_END ;
#endif

   mri_free(bim) ; free(bms) ; KILL_CLUSTER(nbhd) ;

   /* expand output image back to original size */

   dim = mri_double_up( cim , fim->nx%2 , fim->ny%2 , fim->nz%2 ) ;

   if( verb ) fprintf(stderr,"U") ;

   mri_free(cim) ;

   RETURN(dim) ;
}

/*---------------------------------------------------------------------------*/

static float Upbot = 70.0f ;  /* percentile bottom and top */
static float Uptop = 80.0f ;
static float Uprad = 18.3f ;  /* sphere radius */

#define PKVAL 1000.0f
#define PKMID  666.0f

/* White Matter uniformization */

MRI_IMAGE * mri_WMunifize( MRI_IMAGE *fim )
{
   MRI_IMAGE *pim, *gim ; float *par,*gar , pval ; int ii ;

ENTRY("mri_WMunifize") ;

   if( fim == NULL ) RETURN(NULL) ;

   /* create image of local high-intensity value */

   pim = mri_local_percmean( fim , Uprad , Upbot,Uptop ) ;
   if( pim == NULL ) RETURN(NULL) ;
   gim = mri_to_float(fim) ;    /* output = copy of input image */
   gar = MRI_FLOAT_PTR(gim) ;
   par = MRI_FLOAT_PTR(pim) ;   /* scaling image */

   /* scale output by the pim created above */

   for( ii=0 ; ii < gim->nvox ; ii++ ){
     pval    = par[ii] ;
     gar[ii] = (pval <= 0.0f) ? 0.0f : (PKVAL * gar[ii] / pval) ;
   }

   if( verb ) fprintf(stderr,"W") ;

   mri_free(pim) ;
   RETURN(gim) ;
}

/*---------------------------------------------------------------------------*/
/* Gray Matter normalization */

void mri_GMunifize( MRI_IMAGE *gim )
{
   float *gar=MRI_FLOAT_PTR(gim) , *pval , pupper,plower,pmid,pfac ;
   int ii,jj , npval , nvox=gim->nvox ;

ENTRY("mri_GMunifize") ;

   /* extract all values above the WM-unifized peak value */

   for( npval=ii=0 ; ii < nvox ; ii++ )
     if( gar[ii] > PKVAL ) npval++ ;
   if( npval < 111 ) EXRETURN ;   /* 1/6 of being beastly bad */

   pval = (float *)malloc(sizeof(float)*npval) ;
   for( ii=jj=0 ; ii < nvox ; ii++ )
     if( gar[ii] > PKVAL ) pval[jj++] = gar[ii] ;

   /* get the median of these large values */

   pupper = qmed_float(npval,pval) ; free(pval) ;

   /* reflect below the peak value to get the upper cutoff for GM */

   pupper = PKVAL - 1.987654321f * (pupper-PKVAL) ;

   /* set the lower cutoff for GM from the AFNI auto-clip level */

   plower = THD_cliplevel(gim,0.4321f) ;

   /* extract all values between these 2 cutoffs */

   for( npval=ii=0 ; ii < nvox ; ii++ )
     if( gar[ii] >= plower && gar[ii] <= pupper) npval++ ;
   if( npval < 111 ) EXRETURN ;    /* badly bad */

   pval = (float *)malloc(sizeof(float)*npval) ;
   for( ii=jj=0 ; ii < nvox ; ii++ )
     if( gar[ii] >= plower && gar[ii] <= pupper) pval[jj++] = gar[ii] ;

   /* compute the median of these intermediate-value 'GM' voxels */

   pmid = qmed_float(npval,pval) ; free(pval) ;

   /* scale globally to put this pmid value at a standard value for GM */

   pfac = (PKVAL-PKMID) / (PKVAL-pmid) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     if( gar[ii] > 0.0f ){
       gar[ii] = pfac * (gar[ii]-PKVAL) + PKVAL ;
       if( gar[ii] < 0.0f ) gar[ii] = 0.0f ;
     } else {
       gar[ii] = 0.0f ;
     }
   }

   if( verb ) fprintf(stderr,"G") ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , ct , do_GM=0 ;
   char *prefix = "Unifized" ;
   THD_3dim_dataset *inset=NULL , *outset ;
   MRI_IMAGE *imin , *imout ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dUnifize [options] inputdataset\n"
            "* The input dataset is supposed to be a T1-weighted volume,\n"
            "  preferably already skull-stripped (e.g., via 3dSkullStrip).\n"
            "* The output dataset has the white matter (WM) intensity approximately\n"
            "  uniformized across space, and scaled to peak at about 1000.\n"
            "* The output dataset is always stored in float format!\n"
            "* If the input dataset has more than 1 sub-brick, only sub-brick\n"
            "  #0 will be processed.\n"
            "* Method: Obi-Wan's personal variant of Ziad's sneaky trick.\n"
            "  (If you want to know what his trick is, you'll have to ask him, or\n"
            "   read Obi-Wan's source code, which is a world of fun and exaltation.)\n"
            "* The principal motive for this program is for use in an image\n"
            "  registration script, and it may or may not be useful otherwise.\n"
            "* If you have trouble getting 3dSkullStrip to run because of a large\n"
            "  shading artifact, 3dUnifize may help there.\n"
            "\n"
            "--------\n"
            "Options:\n"
            "--------\n"
            "  -prefix pp = Use 'pp' for prefix of output dataset.\n"
            "  -input dd  = Alternative way to specify input dataset.\n"
            "  -GM        = Also scale to unifize 'gray matter' = lower intensit¥ voxels\n"
            "               (to aid in registering images from different scanners).\n"
            "              ++ This option is recommended for use with 3dQwarp when\n"
            "                 aligning 2 T1-weighted volumes, in order to make the\n"
            "                 WM-GM contrast about the same for the datasets, even\n"
            "                 if they don't come from the same scanner/pulse-sequence.\n"
            "  -Urad rr   = Sets the radius (in voxels) of the ball used for the sneaky trick.\n"
            "               ++ Default value is %.1f, and should be changed proportionally\n"
            "                  if the dataset voxel size differs significantly from 1 mm.\n"
            "  -quiet     = Don't print so many fun progress messages (but whyyyy?).\n"
            "\n"
            "--------------------------------------\n"
            "Special options for Jedi Masters ONLY:\n"
            "--------------------------------------\n"
            "  -rbt R b t = Specify the 3 parameters for the algorithm, as 3 numbers\n"
            "               following the '-rbt':\n"
            "                 R = radius; same as given by option '-Urad'     [default=%.1f]\n"
            "                 b = bottom percentile of normalizing data range [default=%.1f]\n"
            "                 r = top percentile of normalizing data range    [default=%.1f]\n"
            "\n"
            "  -clfrac cc = Set the automask 'clip level fraction' to 'cc', which\n"
            "               must be a number between 0.1 and 0.9.\n"
            "               A small 'cc' means to make the initial threshold\n"
            "               for clipping (a la 3dClipLevel) smaller, which\n"
            "               will tend to make the mask larger.  [default=0.1]\n"
            "               ++ [22 May 2013] The previous version of this program used a\n"
            "                  clip level fraction of 0.5, which proved to be too large\n"
            "                  for some users.  Thus, the default value was lowered to 0.1.\n"
            "                  If you strongly desire the old behavior, use '-clfrac 0.5'.\n"
            "\n"
            "-- Feb 2013 - by Obi-Wan Unifobi\n"
#ifdef USE_OMP
            "-- This code uses OpenMP to speed up the slowest part (voxel-wise histograms).\n"
#endif
            , Uprad , Uprad , Upbot , Uptop ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dUnifize main"); machdep(); AFNI_logger("3dUnifize",argc,argv);
   PRINT_VERSION("3dUnifize") ;
   ct = NI_clock_time() ;

   /*-- scan command line --*/

   THD_automask_set_clipfrac(0.1f) ;  /* 22 May 2013 */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-clfrac") == 0 || strcmp(argv[iarg],"-mfrac") == 0 ){    /* 22 May 2013 */
       float clfrac = (float)strtod( argv[++iarg] , NULL ) ;
       if( clfrac < 0.1f || clfrac > 0.9f )
         ERROR_exit("-clfrac value %f is illegal!",clfrac) ;
       THD_automask_set_clipfrac(clfrac) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal value after -prefix!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-inset") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( inset  != NULL ) ERROR_exit("Can't use '%s' twice"    ,argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Urad") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Uprad = (float)strtod(argv[iarg],NULL) ;
       if( Uprad <   5.0f || Uprad > 40.0f )
         ERROR_exit("Illegal value %f after option -Urad",Uprad) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-param") == 0 ||      /*--- HIDDEN OPTION ---*/
         strcmp(argv[iarg],"-rbt"  ) == 0    ){
       if( ++iarg >= argc-2 ) ERROR_exit("Need 3 arguments (R pb pt) after '%s'",argv[iarg-1]) ;
       Uprad = (float)strtod(argv[iarg++],NULL) ;
       Upbot = (float)strtod(argv[iarg++],NULL) ;
       Uptop = (float)strtod(argv[iarg++],NULL) ;
       if( Uprad <   5.0f || Uprad > 40.0f ||
           Upbot <  30.0f || Upbot > 80.0f ||
           Uptop <= Upbot || Uptop > 90.0f   )
         ERROR_exit("Illegal values (R pb pt) after '%s'",argv[iarg-4]) ;
       continue ;
     }

     if( strcasecmp(argv[iarg],"-GM") == 0 ){
       do_GM++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s\n",argv[iarg]);
   }

   /* read input dataset, if not already there */

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No dataset name on command line?\n") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }

   if( verb ) fprintf(stderr," + Pre-processing: ") ;

   /* load input from disk */

   DSET_load( inset ) ; CHECK_LOAD_ERROR(inset) ;
   if( DSET_NVALS(inset) > 1 )
     WARNING_message("Only processing sub-brick #0 (out of %d)",DSET_NVALS(inset)) ;

   /* make a float copy for processing */

   imin = mri_to_float( DSET_BRICK(inset,0) ) ; DSET_unload(inset) ;
   if( imin == NULL ) ERROR_exit("Can't copy input dataset brick?!") ;

   /* do the actual work */

   imout = mri_WMunifize(imin) ;          /* local WM scaling */
   free(imin) ;

   if( imout == NULL ){                   /* this is bad-ositiness */
     if( verb ) fprintf(stderr,"\n") ;
     ERROR_exit("Can't compute Unifize-d dataset for some reason :-(") ;
   }

   if( do_GM ) mri_GMunifize(imout) ;     /* global GM scaling */

   if( verb ) fprintf(stderr,"\n") ;

   /* create output dataset, and write it into the historical record */

   outset = EDIT_empty_copy( inset )  ;
   EDIT_dset_items( outset ,
                       ADN_prefix , prefix ,
                       ADN_nvals  , 1 ,
                       ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dUnifize" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   DSET_delete(outset) ; DSET_delete(inset) ;

   /* vamoose the ranch */

   if( verb ) INFO_message("===== CPU time = %.1f sec  Elapsed = %.1f\n",
                           COX_cpu_time() , 0.001*(NI_clock_time()-ct) ) ;
   exit(0) ;
}
