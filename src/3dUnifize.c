#include "mrilib.h"

static int verb = 1 ;

#define USE_ALL_VALS 1
#ifndef USE_ALL_VALS
static int USE_ALL_VALS = 0 ;  /* 17 May 2016 */
#endif

#ifdef USE_OMP
# include <omp.h>
#endif

static int do_EPI    = 0 ;  /* 01 Mar 2017 */
static int do_double = 1 ;  /* duplo? */

/*---------------------------------------------------------------------------*/

void mri_invertcontrast_inplace( MRI_IMAGE *im , float uperc , byte *mask )
{
   byte *mmm=NULL ;
   int nvox , nhist , ii ; float *hist=NULL , *imar , ucut ;

   if( im == NULL || im->kind != MRI_float ) return ;
        if( uperc <  90.0f ) uperc =  90.0f ;
   else if( uperc > 100.0f ) uperc = 100.0f ;

   mmm = mask ;
   if( mmm == NULL ) mmm = mri_automask_image(im) ;

   nvox = im->nvox ;
   hist = (float *)malloc(sizeof(float)*nvox) ;
   imar = MRI_FLOAT_PTR(im) ;
   for( nhist=ii=0 ; ii < nvox ; ii++ ){ if( mmm[ii] ) hist[nhist++] = imar[ii]; }
   if( nhist < 100 ){
     if( mmm != mask ) free(mmm) ;
     free(hist) ; return ;
   }
   qsort_float(nhist,hist) ;
   ii = (int)rintf(nhist*uperc*0.01f) ; ucut = hist[ii] ; free(hist) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if(  mmm[ii]                    ) imar[ii] = ucut - imar[ii] ;
     if( !mmm[ii] || imar[ii] < 0.0f ) imar[ii] = 0.0f ;
   }
   if( mmm != mask ) free(mmm) ;
   return ;
}

/*---------------------------------------------------------------------------*/

void mri_clipedges_inplace( MRI_IMAGE *qm , float uclip , float vclip )
{
   int ii,jj,kk , ip,jp,kp , im,jm,km , nx,ny,nz,nxy,nxyz , pp,qq , nclip ;
   float *imar , *qmar ;

   if( qm == NULL || qm->kind != MRI_float ) return ;

   nx = qm->nx ; ny = qm->ny ; nz = qm->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   qmar = MRI_FLOAT_PTR(qm) ;
   imar = malloc(sizeof(float)*nxyz) ;

#define ISEDGE(i,j,k) (imar[(i)+(j)*nx+(k)*nxy]==0.0f)

   if( verb ) fprintf(stderr,"e") ;
   do{
     memcpy(imar,qmar,sizeof(float)*nxyz) ;
     for( nclip=pp=0 ; pp < nxyz ; pp++ ){
       if( imar[pp] < uclip ) continue ;
       kk = pp / nxy ; ii = pp % nx ; jj = (pp-kk*nxy)/nx ;
       kp = kk+1 ; if( kp >= nz ) kp = nz-1 ;
       km = kk-1 ; if( km <  0  ) km = 0 ;
       jp = jj+1 ; if( jp >= ny ) jp = ny-1 ;
       jm = jj-1 ; if( jm <  0  ) jm = 0 ;
       ip = ii+1 ; if( ip >= nx ) ip = nx-1 ;
       im = ii-1 ; if( im <  0  ) im = 0 ;
       if( ISEDGE(ip,jj,kk) + ISEDGE(im,jj,kk) +
           ISEDGE(ii,jp,kk) + ISEDGE(ii,jm,kk) +
           ISEDGE(ii,jj,kp) + ISEDGE(ii,jj,km)  > 2 ){
         qmar[pp] = 0.0f ; nclip++ ;
       }
     }
     for( pp=0 ; pp < nxyz ; pp++ ){
       if( imar[pp] < vclip ) continue ;
       kk = pp / nxy ; ii = pp % nx ; jj = (pp-kk*nxy)/nx ;
       kp = kk+1 ; if( kp >= nz ) kp = nz-1 ;
       km = kk-1 ; if( km <  0  ) km = 0 ;
       jp = jj+1 ; if( jp >= ny ) jp = ny-1 ;
       jm = jj-1 ; if( jm <  0  ) jm = 0 ;
       ip = ii+1 ; if( ip >= nx ) ip = nx-1 ;
       im = ii-1 ; if( im <  0  ) im = 0 ;
       if( ISEDGE(ip,jj,kk) + ISEDGE(im,jj,kk) +
           ISEDGE(ii,jp,kk) + ISEDGE(ii,jm,kk) +
           ISEDGE(ii,jj,kp) + ISEDGE(ii,jj,km)  > 4 ){
         qmar[pp] = 0.0f ; nclip++ ;
       }
     }
   } while(nclip > 0) ;

   free(imar) ; return ;
}

/*---------------------------------------------------------------------------*/
#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)
#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*----- fast median-of-7 [mangles input array] -----*/

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

   if( do_double ){
     bim = mri_double_down(aim) ; bar = MRI_FLOAT_PTR(bim) ; mri_free(aim) ;
   } else {
     bim = aim ; bar = MRI_FLOAT_PTR(bim) ;
   }
   bms = (byte *)malloc(sizeof(byte)*bim->nvox) ;
   for( ii=0 ; ii < bim->nvox ; ii++ ) bms[ii] = (bar[ii] != 0.0f) ;

   /* neighborhood has 1/2 radius in the shrunken volume */

   if( do_double )
     nbhd = MCW_spheremask( 1.0f,1.0f,1.0f , 0.5f*vrad+0.001f ) ;
   else
     nbhd = MCW_spheremask( 1.0f,1.0f,1.0f ,      vrad        ) ;

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

   if( do_double ){
     dim = mri_double_up( cim , fim->nx%2 , fim->ny%2 , fim->nz%2 ) ;
     mri_free(cim) ;
   } else {
     dim = cim ;
   }

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
   float vbot=0.0f ;

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

   if( verb && do_double ) fprintf(stderr,"D") ;

   if( do_double ){
     bim = mri_double_down(aim) ; bar = MRI_FLOAT_PTR(bim) ; mri_free(aim) ;
   } else {
     bim = aim ; bar = bar = MRI_FLOAT_PTR(bim) ;
   }

   bms = (byte *)malloc(sizeof(byte)*bim->nvox) ;
   for( ii=0 ; ii < bim->nvox ; ii++ ) bms[ii] = (bar[ii] != 0.0f) ;

   if( !USE_ALL_VALS ){
     vbot = 0.00666f * mri_max(bim) ;
   }

   /* create neighborhood mask (1/2 radius in the shrunken copy) */

   if( do_double )
     nbhd = MCW_spheremask( 1.0f,1.0f,1.0f , 0.5f*vrad+0.001f ) ;
   else
     nbhd = MCW_spheremask( 1.0f,1.0f,1.0f ,      vrad        ) ;

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
             int q1,q2,qq , qb;
             if( !USE_ALL_VALS ){ /* Ignore tiny values [17 May 2016] */
               for( qb=0 ; qb < nbar_num && nbar[qb] <= vbot ; qb++ ) ; /*nada*/
               if( qb == nbar_num ){
                 val = 0.0f ;
               } else if( qb == nbar_num-1 ){
                 val = nbar[qb] ;
               } else {
                 q1 = (int)( 0.01f*p1*(nbar_num-1-qb)) + qb; if( q1 > nbar_num-1 ) q1 = nbar_num-1;
                 q2 = (int)( 0.01f*p2*(nbar_num-1-qb)) + qb; if( q2 > nbar_num-1 ) q2 = nbar_num-1;
                 for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
                 val /= (q2-q1+1.0f) ;
               }
             } else {             /* Use all values [the olden way] */
               q1 = (int)( 0.01f*p1*(nbar_num-1) ) ;  /* p1 location */
               q2 = (int)( 0.01f*p2*(nbar_num-1) ) ;  /* p2 location */
               for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
               val /= (q2-q1+1.0f) ;
             }
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
         int q1,q2,qq , qb;
         if( !USE_ALL_VALS ){ /* Ignore tiny values [17 May 2016] */
           for( qb=0 ; qb < nbar_num && nbar[qb] <= vbot ; qb++ ) ; /*nada*/
           if( qb == nbar_num ){
             val = 0.0f ;
           } else if( qb == nbar_num-1 ){
             val = nbar[qb] ;
           } else {
             q1 = (int)( 0.01f*p1*(nbar_num-1-qb)) + qb; if( q1 > nbar_num-1 ) q1 = nbar_num-1;
             q2 = (int)( 0.01f*p2*(nbar_num-1-qb)) + qb; if( q2 > nbar_num-1 ) q2 = nbar_num-1;
             for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
             val /= (q2-q1+1.0f) ;
           }
         } else {             /* Use all values [the olden way] */
           q1 = (int)( 0.01f*p1*(nbar_num-1) ) ;  /* p1 location */
           q2 = (int)( 0.01f*p2*(nbar_num-1) ) ;  /* p2 location */
           for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
           val /= (q2-q1+1.0f) ;
         }
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

   if( do_double ){
     dim = mri_double_up( cim , fim->nx%2 , fim->ny%2 , fim->nz%2 ) ;
     mri_free(cim) ;
   } else {
     dim = cim ;
   }

   if( verb ) fprintf(stderr,"U") ;

   RETURN(dim) ;
}

/*---------------------------------------------------------------------------*/

static float Upbot = 70.0f ;  /* percentile bottom and top */
static float Uptop = 80.0f ;
static float Uprad = 18.3f ;  /* sphere radius */

#define PKVAL 1000.0f
#define PKMID  666.0f

static MRI_IMAGE *sclim = NULL ;     /* 25 Jun 2013 */
static char     *sspref = NULL ;

/* White Matter uniformization */

MRI_IMAGE * mri_WMunifize( MRI_IMAGE *fim )
{
   MRI_IMAGE *pim, *gim ; float *par,*gar , pval ; int ii ;

ENTRY("mri_WMunifize") ;

   if( fim == NULL ) RETURN(NULL) ;

   /* create image of local high-intensity value */

   if( do_double == 1 ) do_double = (fim->nvox > 1000000) ;
#if 0
   INFO_message("do_double = %d",do_double) ;
#endif

   pim = mri_local_percmean( fim , Uprad , Upbot,Uptop ) ;
   if( pim == NULL ) RETURN(NULL) ;
   gim = mri_to_float(fim) ;    /* output = copy of input image */
   gar = MRI_FLOAT_PTR(gim) ;
   par = MRI_FLOAT_PTR(pim) ;   /* scaling image */

   /* scale output by the pim created above */

   for( ii=0 ; ii < gim->nvox ; ii++ ){
     pval = par[ii] = (par[ii] <= 0.0f) ? 0.0f : PKVAL / par[ii] ;
     gar[ii] = gar[ii] * pval ;
   }

   if( verb ) fprintf(stderr,"W") ;

   if( sclim != NULL ) mri_free(sclim) ;  /* 25 Jun 2013: save scale image */
   sclim = pim ;
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

   plower *= 0.333f ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( gar[ii] >= plower ){
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
   int do_T2=0 ; float T2_uperc=98.5f ; byte *T2_mask=NULL ;
   char *prefix = "Unifized" ;
   THD_3dim_dataset *inset=NULL , *outset=NULL ;
   MRI_IMAGE *imin , *imout ;
   float clfrac=0.2f ;
   int do_mask = 1 ; /* 08 Aug 2018 = 8/8/18 */

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
       "Usage: 3dUnifize [options] inputdataset\n"
       "\n"
       "* The input dataset is supposed to be a T1-weighted volume,\n"
       "  possibly already skull-stripped (e.g., via 3dSkullStrip).\n"
       "  ++ However, this program can be a useful step to take BEFORE\n"
       "     3dSkullStrip, since the latter program can fail if the input\n"
       "     volume is strongly shaded -- 3dUnifize will (mostly) remove\n"
       "     such shading artifacts.\n"
       "\n"
       "* The output dataset has the white matter (WM) intensity approximately\n"
       "  uniformized across space, and scaled to peak at about 1000.\n"
       "\n"
       "* The output dataset is always stored in float format!\n"
       "\n"
       "* If the input dataset has more than 1 sub-brick, only sub-brick\n"
       "  #0 will be processed!\n"
       "\n"
       "* If you have a lot of tissue inferior to the brain, you might have\n"
       "  to cut it off (using 3dZeropad -I -xxx to cut off the most inferior\n"
       "  xxx slices -- where you pick the number xxx visually), before\n"
       "  using 3dUnifize.\n"
       "\n"
       "* Want to correct EPI datasets for nonuniformity?\n"
       "  You can try the new and experimental [Mar 2017] '-EPI' option.\n"
       "\n"
       "* Method: Obi-Wan's personal variant of Ziad's sneaky trick.\n"
       "  (If you want to know what his trick is, you'll have to ask him, or\n"
       "   read Obi-Wan's source code [which is a world of ecstasy and exaltation],\n"
       "   or just read all the way to the end of this help output.)\n"
       "\n"
       "* The principal motive for this program is for use in an image\n"
       "  registration script, and it may or may not be useful otherwise.\n"
       "\n"
       "* This program replaces the older (and very different) 3dUniformize,\n"
       "  which is no longer maintained and may sublimate at any moment.\n"
       "  (In other words, we do not recommend the use of 3dUniformize.)\n"
       "\n"
       "--------\n"
       "Options:\n"
       "--------\n"
       "\n"
       "  -prefix pp = Use 'pp' for prefix of output dataset.\n"
       "\n"
       "  -input dd  = Alternative way to specify input dataset.\n"
       "\n"
       "  -T2        = Treat the input as if it were T2-weighted, rather than\n"
       "               T1-weighted. This processing is done simply by inverting\n"
       "               the image contrast, processing it as if that result were\n"
       "               T1-weighted, and then re-inverting the results.\n"
       "              ++ This option is NOT guaranteed to be useful for anything!\n"
       "              ++ Of course, nothing in AFNI comes with a guarantee :-)\n"
       "              ++ If you want to be REALLY sneaky, giving this option twice\n"
       "                 will skip the second inversion step, so the result will\n"
       "                 look like a T1-weighted volume (except at the edges and\n"
       "                 near blood vessels).\n"
       "              ++ Might be useful for skull-stripping T2-weighted datasets.\n"
       "              ++ Don't try the '-T2 -T2' trick on FLAIR-T2-weighted datasets.\n"
       "                 The results aren't pretty!\n"
       "\n"
       "  -GM        = Also scale to unifize 'gray matter' = lower intensity voxels\n"
       "               (to aid in registering images from different scanners).\n"
       "              ++ For many datasets (especially those created by averaging),\n"
       "                 using '-GM' will increase the WM-GM contrast somewhat;\n"
       "                 however, that depends on the original WM-GM contrast.\n"
       "              ++ This option is recommended for use with 3dQwarp when\n"
       "                 aligning 2 T1-weighted volumes, in order to make the\n"
       "                 WM-GM contrast about the same for the datasets, even\n"
       "                 if they don't come from the same scanner/pulse-sequence.\n"
       "              ++ Note that standardizing the contrasts with 3dUnifize will help\n"
       "                 3dQwarp match the source dataset to the base dataset.  If you\n"
       "                 later want the original source dataset to be warped, you can\n"
       "                 do so using the 3dNwarpApply program.\n"
       "              ++ In particular, the template dataset MNI152_2009_template.nii.gz\n"
       "                 (supplied with AFNI) has been treated with '-GM'. This dataset\n"
       "                 is the one used by the @SSwarper script, so that script applies\n"
       "                 3dUnifize with this '-GM' option to help with the alignment.\n"
       "\n"
       "  -Urad rr   = Sets the radius (in voxels) of the ball used for the sneaky trick.\n"
       "               ++ Default value is %.1f, and should be changed proportionally\n"
       "                  if the dataset voxel size differs significantly from 1 mm.\n"
       "\n"
       "  -ssave ss  = Save the scale factor used at each voxel into a dataset 'ss'.\n"
       "               ++ This is the white matter scale factor, and does not include\n"
       "                  the factor from the '-GM' option (if that was included).\n"
       "               ++ The input dataset is multiplied by the '-ssave' image\n"
       "                  (voxel-wise) to get the WM-unifized image.\n"
       "               ++ Another volume (with the same grid dimensions) could be\n"
       "                  scaled the same way using 3dcalc, if that is needed.\n"
       "               ++ This saved scaled factor does NOT include any GM scaling :(\n"
       "\n"
       "  -quiet     = Don't print the fun fun fun progress messages (but whyyyy?).\n"
       "               ++ For the curious, the codes used are:\n"
       "                   A = Automask\n"
       "                   D = Duplo down (process a half-size volume)\n"
       "                   V = Voxel-wise histograms to get local scale factors\n"
       "                   U = duplo Up (convert local scale factors to full-size volume)\n"
       "                   W = multiply by White matter factors\n"
       "                   G = multiply by Gray matter factors [cf the -GM option]\n"
       "                   I = contrast inversion              [cf the -T2 option]\n"
       "                   M = compute median volume           [for the -EPI option]\n"
       "                   E = compute scaled EPI datasets     [for the -EPI option]\n"
       "               ++ 'Duplo down' means to scale the input volume to be half the\n"
       "                  grid size in each direction for speed when computing the\n"
       "                  voxel-wise histograms.  The sub-sampling is done using the\n"
       "                  median of the central voxel value and its 6 nearest neighbors.\n"
       "\n"
       "  -noduplo   = Do NOT use the 'duplo down' step; this can be useful for lower\n"
       "               resolution datasets.\n"
       "               ++ If a dataset has less than 1 million voxels in a 3D volume,\n"
       "                  'duplo down' will not be used.\n"
       "\n"
       "  -EPI       = Assume the input dataset is a T2 (or T2*) weighted EPI time\n"
       "               series. After computing the scaling, apply it to ALL volumes\n"
       "               (TRs) in the input dataset. That is, a given voxel will be\n"
       "               scaled by the same factor at each TR.\n"
       "               ++ This option also implies '-noduplo' and '-T2'.\n"
       "               ++ This option turns off '-GM' if you turned it on.\n"
       "           -->>++ This option is experimental; check your results!\n"
       "               ++ Remember: the program tries to uniform-ize the White Matter\n"
       "                  regions, so the overall appearance of the image may become\n"
       "                  less uniform, especially if it was fairly uniform already.\n"
       "               ++ For most purposes in AFNI processing, uniform-izing\n"
       "                  EPI datasets is not needed.\n"
       "                  -- If you are having trouble getting a good result from\n"
       "                     3dAutomask, try adding the option '-clfrac 0.2'.\n"
       "                  -- There is no reason to apply 3dUnifize to EPI datasets\n"
       "                     that do not have significant shading artifacts.\n"
       "                  -- EPI data from 7T systems might be 'improved' by 3dUnifize.\n"
       "                  -- You might need to run 3dDespike before using 3dUnifize.\n"
       "\n"
       "------------------------------------------\n"
       "Special options for Jedi AFNI Masters ONLY:\n"
       "------------------------------------------\n"
       "  -rbt R b t = Specify the 3 parameters for the algorithm, as 3 numbers\n"
       "               following the '-rbt':\n"
       "                 R = radius; same as given by option '-Urad'     [default=%.1f]\n"
       "                 b = bottom percentile of normalizing data range [default=%.1f]\n"
       "                 r = top percentile of normalizing data range    [default=%.1f]\n"
       "\n"
       "  -T2up uu   = Set the upper percentile point used for T2-T1 inversion.\n"
       "               The default value is 98.5 (for no good reason), and 'uu' is\n"
       "               allowed to be anything between 90 and 100 (inclusive).\n"
       "               ++ The histogram of the data is built, and the uu-th percentile\n"
       "                  point value is called 'U'. The contrast inversion is simply\n"
       "                  given by output_value = max( 0 , U - input_value ).\n"
       "\n"
       "  -clfrac cc = Set the automask 'clip level fraction' to 'cc', which\n"
       "               must be a number between 0.1 and 0.9.\n"
       "               A small 'cc' means to make the initial threshold\n"
       "               for clipping (a la 3dClipLevel) smaller, which\n"
       "               will tend to make the mask larger.  [default=0.1]\n"
       "               ++ [22 May 2013] The previous version of this program used a\n"
       "                  clip level fraction of 0.5, which proved to be too large\n"
       "                  for some users, who had images with very strong shading issues.\n"
       "                  Thus, the default value for this parameter was lowered to 0.1.\n"
       "               ++ [24 May 2016] The default value for this parameter was\n"
       "                  raised to 0.2, since the lower value often left a lot of\n"
       "                  noise outside the head on non-3dSkullStrip-ed datasets.\n"
       "                  You can still manually set -clfrac to 0.1 if you need to\n"
       "                  correct for very large shading artifacts.\n"
       "               ++ If the results of 3dUnifize have a lot of noise outside the head,\n"
       "                  then using '-clfrac 0.5' (or even larger) will probably help.\n"
#ifndef USE_ALL_VALS
       "\n"
       "  -useall    = The 'old' way of operating was to use all dataset values\n"
       "               in the local WM histogram.  The 'new' way [May 2016] is to\n"
       "               only use positive values.  If you want to use the 'old' way,\n"
       "               then this option is what you want.\n"
#endif
       "\n"
       "-- Feb 2013 - by Obi-Wan Unifobi\n"
       "            - can always be found at the Everest Bakery in Namche Bazaar,\n"
       "              if you have any questions about this program\n"
#ifdef USE_OMP
       "-- This code uses OpenMP to speed up the slowest part (voxel-wise histograms).\n"
#endif
       , Uprad , Uprad , Upbot , Uptop ) ;

     printf("\n"
      "----------------------------------------------------------------------------\n"
      "HOW IT WORKS (Ziad's sneaky trick is revealed at last! And more.)\n"
      "----------------------------------------------------------------------------\n"
      "The basic idea is that white matter in T1-weighted images is reasonably\n"
      "uniform in intensity, at least when averaged over 'large-ish' regions.\n"
      "\n"
      "The first step is to create a local white matter intensity volume.\n"
      "Around each voxel (inside the volume 'automask'), the ball of values\n"
      "within a fixed radius (default=18.3 voxels) is extracted and these\n"
      "numbers are sorted.  The values in the high-intensity range of the\n"
      "histogram (default=70%% to 80%%) are averaged.  The result from this\n"
      "step is a smooth 3D map of the 'white matter intensity' (WMI).\n"
      "\n"
      " [The parameters of the above process can be altered with the '-rbt' option.]\n"
      " [For speed, the WMI map is produced on an image that is half-size in all   ]\n"
      " [directions ('Duplo down'), and then is expanded back to the full-size     ]\n"
      " [volume ('Duplo up').  The automask procedure can be somewhat controlled   ]\n"
      " [via the '-clfrac' option.  The default setting is designed to deal with   ]\n"
      " [heavily shaded images, where the WMI varies by a factor of 5 or more over ]\n"
      " [the image volume.                                                         ]\n"
      "\n"
      "The second step is to scale the value at every voxel location x in the input\n"
      "volume by the factor 1000/WMI(x), so that the 'white matter intensity' is\n"
      "now uniform-ized to be 1000 everywhere.  (This is Ziad's 'trick'; it is easy,\n"
      "works well, and doesn't require fitting some spatial model to the data: the\n"
      "data provides its own model.)\n"
      "\n"
      "If the '-GM' option is used, then this scaled volume is further processed\n"
      "to make the lower intensity values (presumably gray matter) have a contrast\n"
      "similar to that from a collection of 3 Tesla MP-RAGE images that were\n"
      "acquired at the NIH.  (This procedure is not Ziad's fault, and should be\n"
      "blamed on the reclusive Obi-Wan Unifobi.)\n"
      "\n"
      "From the WM-uniform-ized volume, the median of all values larger than 1000\n"
      "is computed; call this value P.  P-1000 represents the upward dispersion\n"
      "of the high-intensity (white matter) voxels in the volume.  This value is\n"
      "'reflected' below 1000 to Q = 1000 - 2*(P-1000), and Q is taken to be the\n"
      "upper bound for gray matter voxel intensities.  A lower bound for gray\n"
      "matter voxel values is estimated via the 'clip fraction' algorithm as\n"
      "implemented in program 3dClipLevel; call this lower bound R.  The median\n"
      "of all values between R and Q is computed; call this value G, which is taken\n"
      "to be a 'typical' gray matter voxel instensity.  Then the values z in the\n"
      "entire volume are linearly scaled by the formula\n"
      "   z_out = (1000-666)/(1000-G) * (z_in-1000) + 1000\n"
      "so that the WM uniform-ized intensity of 1000 remains at 1000, and the gray\n"
      "matter median intensity of G is mapped to 666.  (Values z_out that end up\n"
      "negative are set to 0; as a result, some of CSF might end up as 0.)\n"
      "The value 666 was chosen because it gave results visually comparable to\n"
      "various NIH-generated 3 Tesla T1-weighted datasets.  (Any suggestions that\n"
      "this value was chosen for other reasons will be treated as 'beastly'.)\n"
      "\n"
      "To recap: the WM uniform-ization process provides a linear scaling factor\n"
      "that varies for each voxel ('local'), while the GM normalization process\n"
      "uses a global linear scaling.  The GM process is optional, and is simply\n"
      "designed to make the various T1-weighted images look similar.\n"
      "\n"
      "-----** CAVEAT **-----\n"
      "This procedure was primarily developed to aid in 3D registration, especially\n"
      "when using 3dQwarp, so that the registration algorithms are trying to match\n"
      "images that are alike.  It is *NOT* intended to be used for quantification\n"
      "purposes, such as Voxel Based Morphometry!  That would better be done via\n"
      "the 3dSeg program, which is far more complicated.\n"
      "----------------------------------------------------------------------------\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dUnifize main"); machdep(); AFNI_logger("3dUnifize",argc,argv);
   PRINT_VERSION("3dUnifize") ;
   ct = NI_clock_time() ;

   /*-- scan command line --*/

   THD_automask_set_clipfrac(0.1f) ;  /* 22 May 2013 */
   THD_automask_extclip(1) ;          /* 19 Dec 2014 */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-nodouble") == 0 || strcmp(argv[iarg],"-noduplo") == 0 ){
       do_double = 0 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-EPI") == 0 ){
       do_EPI = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-clfrac") == 0 || strcmp(argv[iarg],"-mfrac") == 0 ){    /* 22 May 2013 */
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       clfrac = (float)strtod( argv[iarg] , NULL ) ;
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

     if( strcmp(argv[iarg],"-ssave") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       sspref = strdup(argv[iarg]) ;
       if( !THD_filename_ok(sspref) ) ERROR_exit("Illegal value after -ssave!") ;
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
       if( Uprad <   5.0f || Uprad > 99.0f )
         ERROR_exit("Illegal value %f after option -Urad",Uprad) ;
       iarg++ ; continue ;
     }

#ifndef USE_ALL_VALS
     if( strcmp(argv[iarg],"-useall") == 0 ){   /* 17 May 2016 */
       USE_ALL_VALS = 1 ; iarg++ ; continue ;
     }
#else
     if( strcmp(argv[iarg],"-useall") == 0 ){
       WARNING_message("-useall option is disabled in this version") ;
       iarg++ ; continue ;
     }
#endif

     if( strcmp(argv[iarg],"-param") == 0 ||      /*--- HIDDEN OPTION ---*/
         strcmp(argv[iarg],"-rbt"  ) == 0    ){
       if( ++iarg >= argc-2 ) ERROR_exit("Need 3 arguments (R pb pt) after '%s'",argv[iarg-1]) ;
       Uprad = (float)strtod(argv[iarg++],NULL) ;
       Upbot = (float)strtod(argv[iarg++],NULL) ;
       Uptop = (float)strtod(argv[iarg++],NULL) ;
       if( Uprad <   5.0f || Uprad > 99.0f ||
           Upbot <  30.0f || Upbot > 80.0f ||
           Uptop <= Upbot || Uptop > 90.0f   )
         ERROR_exit("Illegal values (R pb pt) after '%s'",argv[iarg-4]) ;
       continue ;
     }

     if( strcasecmp(argv[iarg],"-GM") == 0 ){
       do_GM++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-T2") == 0 ){  /* 18 Dec 2014 */
       do_T2++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-T2up") == 0 ){  /* 18 Dec 2014 */
       T2_uperc = (float)strtod( argv[++iarg] , NULL ) ;
       if( T2_uperc < 90.0f || T2_uperc > 100.0f )
         ERROR_exit("-T2up value is out of range 90..100 :-(") ;
       iarg++ ; continue ;
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

   if( do_EPI ){
     float uu ;
     do_T2 = 1 ; do_double = 0 ;
     if( do_GM ){ INFO_message("-EPI turns off -GM") ; do_GM = 0 ; }
     uu = cbrtf(fabsf(DSET_DX(inset)*DSET_DY(inset)*DSET_DZ(inset))) ;
     uu = 18.3 / uu ; if( uu < 4.14f ) uu = 4.14f ;
     Uprad = uu ;
     INFO_message("-EPI changes -Urad to %.1f voxels",Uprad) ;
   }

   if( verb ) fprintf(stderr," + Pre-processing: ") ;

   /* load input from disk */

   DSET_load( inset ) ; CHECK_LOAD_ERROR(inset) ;
   if( ! do_EPI && DSET_NVALS(inset) > 1 ){
     WARNING_message("Only processing sub-brick #0 (out of %d)",DSET_NVALS(inset)) ;
   } else if( do_EPI && DSET_NVALS(inset) == 1 ){
     WARNING_message("-EPI was used, but only 1 volume in the input dataset") ;
     do_EPI = 0 ;
   }

   /* make a float copy of the input brick for processing */

   if( !do_EPI ){
     imin = THD_extract_float_brick(0,inset) ;
     if( imin == NULL ) ERROR_exit("Can't copy input dataset brick?!") ;
   } else {
     if( verb ) fprintf(stderr,"M") ;
     imin = THD_median_brick(inset) ;
     if( imin == NULL ) ERROR_exit("Can't compute median of EPI dataset?!") ;
   }
   if( ! do_EPI ) DSET_unload(inset) ;

#if 0
THD_cliplevel_search(imin) ; exit(0) ;  /* experimentation only */
#endif

   THD_automask_set_clipfrac(clfrac) ;
   if( verb > 1 ) THD_automask_verbose(verb-1) ;

   /* invert T2? */

   if( do_T2 ){
     if( verb ) fprintf(stderr,"I") ;
     T2_mask = mri_automask_image(imin) ;
     mri_invertcontrast_inplace( imin , T2_uperc , T2_mask ) ;
   }

   /* do the actual work */

   imout = mri_WMunifize(imin) ;          /* local WM scaling */
   free(imin) ;

   if( sspref != NULL && sclim != NULL ){  /* 25 Jun 2013 */
     STATUS("output -ssave") ;
     outset = EDIT_empty_copy( inset )  ;
     EDIT_dset_items( outset ,
                         ADN_prefix , sspref ,
                         ADN_nvals  , 1 ,
                         ADN_ntt    , 0 ,
                      ADN_none ) ;
     EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(sclim) ) ;
     tross_Copy_History( inset , outset ) ;
     tross_Make_History( "3dUnifize" , argc,argv , outset ) ;
     DSET_write(outset) ; outset = NULL ;
   }

   /*-- Compute the EPI output here [01 Mar 2017] --*/

   if( do_EPI ){
     int nvals=DSET_NVALS(inset) , nxyz=DSET_NVOX(inset) , ii,iv ;
     float *scar , *imar ;

     if( sclim == NULL )  /* should never happen */
       ERROR_exit("Can't process EPI data: scale image missing :(") ;
     scar = MRI_FLOAT_PTR(sclim) ;

     outset = EDIT_empty_copy( inset )  ;  /* create shell of output */
     EDIT_dset_items( outset ,
                        ADN_prefix    , prefix ,
                        ADN_datum_all , MRI_float ,
                        ADN_brick_fac , NULL ,
                      ADN_none ) ;

     DSET_load(inset) ;

     /* scale each volume */

     if( verb ) fprintf(stderr,"E") ;
     for( iv=0 ; iv < nvals ; iv++ ){
       if( iv%100 == 47 ) fprintf(stderr,".") ;
       /* get input volume */
       imin = THD_extract_float_brick(iv,inset) ;
       DSET_unload_one(inset,iv) ;
       if( imin == NULL )
         ERROR_exit("Can't load sub-brick #%d of input dataset :(",iv) ;
       imar = MRI_FLOAT_PTR(imin) ;
       /* invert contrast */
       mri_invertcontrast_inplace( imin , T2_uperc , T2_mask ) ;
       /* scale */
       for( ii=0 ; ii < nxyz ; ii++ ) imar[ii] *= scar[ii] ;
       /* invert contrast back */
       mri_invertcontrast_inplace( imin , T2_uperc , T2_mask ) ;
       /* put result into output dataset */
       EDIT_substitute_brick( outset , iv , MRI_float , MRI_FLOAT_PTR(imin) ) ;
       /* toss the empty shell of the processed image */
       mri_clear_and_free(imin) ;
     }
     if( verb ) fprintf(stderr,"\n") ;

     /* write the output and head back to the bakery */

     DSET_unload(inset) ;
     tross_Copy_History( inset , outset ) ;
     tross_Make_History( "3dUnifize" , argc,argv , outset ) ;
     DSET_write(outset) ;
     WROTE_DSET(outset) ;
     exit(0) ;

   } /* end of -EPI output */

   /*-- Continue the standard (1 volume) processing --*/

   if( sclim != NULL ){ mri_free(sclim) ; sclim = NULL ; }

   if( imout == NULL ){                   /* this is bad-ositiness */
     if( verb ) fprintf(stderr,"\n") ;
     ERROR_exit("Can't compute Unifize-d dataset for some reason :-(") ;
   }

   if( do_GM ) mri_GMunifize(imout) ;     /* global GM scaling */

   if( do_T2 == 1 ){          /* re-invert T2? */
     if( verb ) fprintf(stderr,"I") ;
     mri_invertcontrast_inplace( imout , T2_uperc , T2_mask ) ;
   } else if( do_T2 == 2 ){   /* don't re-invert, but clip off bright edges */
     mri_clipedges_inplace( imout , PKVAL*1.111f , PKVAL*1.055f ) ;
   }

   if( do_mask ){  /* 08 Aug 2018 */
     byte *mmm = mri_automask_image(imout) ;
     float *fff = MRI_FLOAT_PTR(imout) ;
     int ii , nvox=imout->nvox ;
     if( verb ) fprintf(stderr,"m") ;
     for( ii=0 ; ii < nvox ; ii++ ){ if( !mmm[ii] ) fff[ii] = 0.0f ; }
     free(mmm) ;
   }

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

   if( verb ){
     double cput = COX_cpu_time() ;
     if( cput > 0.05 )
       INFO_message("===== CPU time = %.1f sec  Elapsed = %.1f\n",
                             COX_cpu_time() , 0.001*(NI_clock_time()-ct) ) ;
     else
       INFO_message("===== Elapsed = %.1f sec\n", 0.001*(NI_clock_time()-ct) ) ;
   }
   exit(0) ;
}
