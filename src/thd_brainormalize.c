#include "mrilib.h"
#include "thd_brainormalize.h"

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

static int verb = 0 ;
static int specie = HUMAN;
void mri_brainormalize_verbose( int v ){ verb = v ; THD_automask_verbose(v); }


#define  THD_BN_DXYZ    1.0
#define  THD_BN_NX      167
#define  THD_BN_NY      212
#define  THD_BN_NZ      175
#define  THD_BN_ZHEIGHT 170.0
#define  THD_BN_XORG    -83.0
#define  THD_BN_YORG    -89.0
#define  THD_BN_ZORG    -82.0
#define  THD_BN_XCM     0.0
#define  THD_BN_YCM     20.0
#ifdef THD_BN_CMTOP
   #define  THD_BN_ZCM  20.0
#else
   #define  THD_BN_ZCM  0.0
#endif   


#define HUMAN_RAT      1.0   /* Ratio is size of human / human dimensions */
#define MONKEY_RAT      2.0   /* Ratio is size of human / monkey dimensions */
#define MARMOSET_RAT    5.0   /* Ratio is size of human / marmoset dimensions */
#define RAT_RAT         8.0   /* Ratio is size of human / rat dimensions */

static float thd_bn_dxyz = 0.0;
static int thd_bn_nx     = 0;
static int thd_bn_ny     = 0;
static int thd_bn_nz     = 0;
static float thd_bn_zheight = 0.0; /* height of box, from top slice */
static float thd_bn_xorg = 0.0 ;  /* the box for the master dataset grid */
static float thd_bn_yorg = 0.0 ;
static float thd_bn_zorg = 0.0 ;
static float thd_bn_xcm = 0.0;
static float thd_bn_ycm = 0.0;
static float thd_bn_zcm = 0.0;
static float thd_bn_rat = 0.0;

void mri_speciebusiness( int v ) { 
   if (v < HUMAN || v >= N_SPECIES) {
      fprintf(stderr,"** Bad specie %d\nDefaulting to Human.\n", v);
      specie = HUMAN;
   }  else {
      specie = v;
   }
}
void mri_brainormalize_initialize(float dx, float dy, float dz)
{
   
   /* set the resolution */
   thd_bn_dxyz = MIN(fabs(dx), fabs(dy)); 
   thd_bn_dxyz = MIN(thd_bn_dxyz, fabs(dz));
   
   if (specie == MONKEY) {
      /* do the monkey thing, smaller box, basically, half the size of human*/
      thd_bn_nx     = (int)(THD_BN_NX/MONKEY_RAT);
      thd_bn_ny     = (int)(THD_BN_NY/MONKEY_RAT);
      thd_bn_nz     = (int)(THD_BN_NZ/MONKEY_RAT);
      thd_bn_zheight = THD_BN_ZHEIGHT/MONKEY_RAT; 
      thd_bn_xorg =  THD_BN_XORG/MONKEY_RAT;  
      thd_bn_yorg =  THD_BN_YORG/MONKEY_RAT ;
      thd_bn_zorg =  THD_BN_ZORG/MONKEY_RAT;
      thd_bn_xcm = THD_BN_XCM/MONKEY_RAT;
      thd_bn_ycm = THD_BN_YCM/MONKEY_RAT;
      thd_bn_zcm = THD_BN_ZCM/MONKEY_RAT;
      thd_bn_rat = MONKEY_RAT;
   } else if (specie == MARMOSET) {
      fprintf(
         stderr,
         "Error mri_brainormalize_initialize:\n"
         "MARMOSET is handled only in 3dSkullStrip via XYZ scaling\n"
         "and MONKEY options.\n");
      exit(1);
   } else if (specie == RAT) {
      thd_bn_nx     = (int)(THD_BN_NX/RAT_RAT);
      thd_bn_ny     = (int)(THD_BN_NY/RAT_RAT);
      thd_bn_nz     = (int)(THD_BN_NZ/RAT_RAT);
      thd_bn_zheight = THD_BN_ZHEIGHT/RAT_RAT; 
      thd_bn_xorg =  THD_BN_XORG/RAT_RAT;  
      thd_bn_yorg =  THD_BN_YORG/RAT_RAT ;
      thd_bn_zorg =  THD_BN_ZORG/RAT_RAT;
      thd_bn_xcm = THD_BN_XCM/RAT_RAT;
      thd_bn_ycm = THD_BN_YCM/RAT_RAT;
      thd_bn_zcm = THD_BN_ZCM/RAT_RAT;
      thd_bn_rat = RAT_RAT;
   } else {
      thd_bn_dxyz = THD_BN_DXYZ;
      thd_bn_nx     = THD_BN_NX;
      thd_bn_ny     = THD_BN_NY;
      thd_bn_nz     = THD_BN_NZ;
      thd_bn_zheight = THD_BN_ZHEIGHT; 
      thd_bn_xorg =  THD_BN_XORG;  
      thd_bn_yorg =  THD_BN_YORG ;
      thd_bn_zorg =  THD_BN_ZORG;
      thd_bn_xcm = THD_BN_XCM;
      thd_bn_ycm = THD_BN_YCM;
      thd_bn_zcm = THD_BN_ZCM;
      thd_bn_rat = HUMAN_RAT;
   }
   /* Set the reinterpolation resolution to the smallest delta */
   thd_bn_nx = (int)( (float)thd_bn_nx / thd_bn_dxyz );
   thd_bn_ny = (int)( (float)thd_bn_ny / thd_bn_dxyz );
   thd_bn_nz = (int)( (float)thd_bn_nz / thd_bn_dxyz );
   return;
}

float THD_BN_rat()
{
   return thd_bn_rat;
}
float THD_BN_xcm ()
{
   return thd_bn_xcm;
}
float THD_BN_ycm ()
{
   return thd_bn_ycm;
}
float THD_BN_zcm ()
{
   return thd_bn_zcm;
}

float THD_BN_xorg()
{
   return thd_bn_xorg;
}
float THD_BN_yorg()
{
   return thd_bn_yorg;
}
float THD_BN_zorg()
{
   return thd_bn_zorg;
}
float THD_BN_dxyz()
{
   return thd_bn_dxyz;
}
int THD_BN_nx()
{
   return thd_bn_nx;
}
int THD_BN_ny()
{
   return thd_bn_ny;
}
int THD_BN_nz()
{
   return thd_bn_nz;
}
float THD_BN_zheight()
{
   return thd_bn_zheight;
}


/*------------------------------------------------------------------*/

#undef  DALL
#define DALL 4096  /* Allocation size for cluster arrays */

/*--------------------------------------------------------------------------*/
/*! Put (i,j,k) into the cluster, if it is nonzero. */

#undef  DPUT
#define DPUT(i,j,k,d)                                               \
  do{ ijk = (i)+(j)*nx+(k)*nxy ;                                    \
      if( mmm[ijk] == 0 ) break ;                                   \
      if( nnow == nall ){ /* increase array lengths */              \
        nall += DALL + nall/8 ;                                     \
        inow = (short *) realloc((void *)inow,sizeof(short)*nall) ; \
        jnow = (short *) realloc((void *)jnow,sizeof(short)*nall) ; \
        know = (short *) realloc((void *)know,sizeof(short)*nall) ; \
      }                                                             \
      inow[nnow] = (i); jnow[nnow] = (j); know[nnow] = (k);         \
      nnow++ ; mmm[ijk] = 0 ; ddd[ijk] = (d) ;                      \
    } while(0)

/*--------------------------------------------------------------------------*/

short * THD_mask_distize( int nx, int ny, int nz, byte *mmm, byte *ccc )
{
   short *ddd , dnow ;
   int ii,jj,kk , nxy=nx*ny , nxyz=nx*ny*nz , ijk ;
   int ip,jp,kp , im,jm,km , icl ;
   int nccc,nmmm , nall,nnow ;
   short *inow , *jnow , *know ;
   float drat ;

   if( mmm == NULL || ccc == NULL ) return NULL ;

   ddd = (short *)malloc( sizeof(short)*nxyz ) ;
   nccc = nmmm = 0 ;
   for( ii=0 ; ii < nxyz ; ii++ ){
          if( ccc[ii] ){ ddd[ii] =  1; nccc++; nmmm++; }
     else if( mmm[ii] ){ ddd[ii] = -1; nmmm++; }
     else              { ddd[ii] =  0; }
   }
   if( nccc == 0 ){ free((void *)ddd); return NULL; }

   nall  = nccc+DALL ;                            /* # allocated pts */
   inow  = (short *) malloc(sizeof(short)*nall) ; /* coords of pts */
   jnow  = (short *) malloc(sizeof(short)*nall) ;
   know  = (short *) malloc(sizeof(short)*nall) ;
   nnow  = 0 ;

   for( ii=0 ; ii < nxyz ; ii++ ){
     if( ccc[ii] ){
       inow[nnow] = ii % nx ;
       jnow[nnow] = (ii%nxy)/nx ;
       know[nnow] = ii / nxy ;
       mmm[ii]    = 0 ;
       nnow++ ;
     }
   }

   for( icl=0 ; icl < nnow ; icl++ ){
     ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ; ijk = ii+jj*nx+kk*nxy ;
     im = ii-1      ; jm = jj-1      ; km = kk-1      ;
     ip = ii+1      ; jp = jj+1      ; kp = kk+1      ; dnow = ddd[ijk]+1 ;

     if( im >= 0 ) DPUT(im,jj,kk,dnow) ;
     if( ip < nx ) DPUT(ip,jj,kk,dnow) ;
     if( jm >= 0 ) DPUT(ii,jm,kk,dnow) ;
     if( jp < ny ) DPUT(ii,jp,kk,dnow) ;
     if( km >= 0 ) DPUT(ii,jj,km,dnow) ;
     if( kp < nz ) DPUT(ii,jj,kp,dnow) ;
   }

   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (ddd[ii] > 0) ;

   free((void *)inow); free((void *)jnow); free((void *)know);

   return ddd ;
}

/*--------------------------------------------------------------------------*/
/*! Put (i,j,k) into the current cluster, if it is nonzero. */

#undef  CPUT
#define CPUT(i,j,k)                                                   \
  do{ ijk = (i)+(j)*nx+(k)*nxy ;                                      \
      if( mmm[ijk] ){                                                 \
        if( nnow == nall ){ /* increase array lengths */              \
          nall += DALL + nall/4 ;                                     \
          inow = (short *) realloc((void *)inow,sizeof(short)*nall) ; \
          jnow = (short *) realloc((void *)jnow,sizeof(short)*nall) ; \
          know = (short *) realloc((void *)know,sizeof(short)*nall) ; \
        }                                                             \
        inow[nnow] = (i); jnow[nnow] = (j); know[nnow] = (k);         \
        nnow++ ; mmm[ijk] = 0 ;                                       \
      } } while(0)

/*--------------------------------------------------------------------------*/
/*! Remove clusters below csize from the binary mask mmm. */

static void clustedit3D( int nx, int ny, int nz, byte *mmm, int csize )
{
   int ii,jj,kk, icl , nxy,nxyz , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nnow , nall , nsav , nkill ;
   short *inow , *jnow , *know ;
   short *isav , *jsav , *ksav ;

   if( nx < 1 || ny < 1 || nz < 1 || mmm == NULL || csize < 2 ) return ;

   nxy = nx*ny ; nxyz = nxy*nz ;

   nall  = 8 ;                                    /* # allocated pts */
   inow  = (short *) malloc(sizeof(short)*nall) ; /* coords of pts */
   jnow  = (short *) malloc(sizeof(short)*nall) ;
   know  = (short *) malloc(sizeof(short)*nall) ;

   nsav  = nkill = 0 ;
   isav  = (short *) malloc(sizeof(short)) ;
   jsav  = (short *) malloc(sizeof(short)) ;
   ksav  = (short *) malloc(sizeof(short)) ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

#if 0
   if( verb ) fprintf(stderr," + clustedit3D: threshold size=%d voxels\n",csize) ;
#endif

   ijk_last = 0 ;
   while(1) {
     /* find next nonzero point */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */

     ijk_last = ijk+1 ;         /* start here next time */

     /* init current cluster list with this point */

     mmm[ijk] = 0 ;                                /* clear found point */
     nnow     = 1 ;                                /* # pts in cluster */
     inow[0]  = ijk % nx ;
     jnow[0]  = (ijk%nxy)/nx ;
     know[0]  = ijk / nxy ;

     /*--
        for each point in cluster:
           check 6 neighboring points for nonzero entries in mmm
           enter those into cluster (and clear them in mmm)
           continue until end of cluster is reached
           (note that cluster size nnow is expanding as we progress)
     --*/

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1      ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1      ;

       if( im >= 0 ) CPUT(im,jj,kk) ;
       if( ip < nx ) CPUT(ip,jj,kk) ;
       if( jm >= 0 ) CPUT(ii,jm,kk) ;
       if( jp < ny ) CPUT(ii,jp,kk) ;
       if( km >= 0 ) CPUT(ii,jj,km) ;
       if( kp < nz ) CPUT(ii,jj,kp) ;
     }

     /* see if now cluster is large enough to live */

     if( nnow >= csize ){
       kk = nsav+nnow ;
       isav = (short *)realloc((void *)isav,sizeof(short)*kk) ;
       jsav = (short *)realloc((void *)jsav,sizeof(short)*kk) ;
       ksav = (short *)realloc((void *)ksav,sizeof(short)*kk) ;
       memcpy(isav+nsav,inow,sizeof(short)*nnow) ;
       memcpy(jsav+nsav,jnow,sizeof(short)*nnow) ;
       memcpy(ksav+nsav,know,sizeof(short)*nnow) ;
       nsav = kk ;
#if 0
       if( verb )
         fprintf(stderr," + clustedit3D: saved cluster with %d voxels\n",nnow);
#endif
     } else {
       nkill += nnow ;
     }

   } /* loop ends when all nonzero points are clustered */

   free((void *)inow); free((void *)jnow); free((void *)know);

   /* copy saved points back into mmm */

   for( ii=0 ; ii < nsav ; ii++ )
     mmm[ IJK(isav[ii],jsav[ii],ksav[ii]) ] = 1 ;

   free((void *)isav); free((void *)jsav); free((void *)ksav) ;

#if 0
   if( verb )
     fprintf(stderr," + clustedit3D totals:"
                    " saved=%d killed=%d nxyz=%d\n",nsav,nkill,nxyz) ;
#endif
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Find the cliplevel in just part of the image,
    with i=ibot..itop (inclusive), et cetera.     */

static float partial_cliplevel( MRI_IMAGE *im , float mfrac ,
                                int ibot,int itop ,
                                int jbot,int jtop , int kbot,int ktop )
{
   int ncut,ib , qq,nold ;
   int ii,jj,kk , nx,ny,nz,nxy ;
   int *hist ;
   short *sar ;
   byte *bar ;
   int nhist , npos , nhalf , val ;
   double dsum ;

ENTRY("partial_cliplevel") ;
   if( im == NULL || im->kind != MRI_short ) RETURN(1.0) ;

   if( mfrac <= 0.0 || mfrac >= 0.99 ) mfrac = 0.50 ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;

   if( ibot <  0  ) ibot = 0 ;
   if( jbot <  0  ) jbot = 0 ;
   if( kbot <  0  ) kbot = 0 ;
   if( itop >= nx ) itop = nx-1 ;
   if( jtop >= ny ) jtop = ny-1 ;
   if( ktop >= nz ) ktop = nz-1 ;

   if( itop < ibot || jtop < jbot || ktop < kbot ) RETURN(1.0) ;

   /*-- allocate histogram --*/

   nhist = 32767 ;
   hist  = (int *) calloc(sizeof(int),nhist+1) ;

   /*-- make histogram of positive entries --*/

   dsum = 0.0 ;  /* will be sum of squares */
   npos = 0 ;    /* will be number of positive values */
   sar  =  MRI_SHORT_PTR(im) ;
   for( kk=kbot ; kk <= ktop ; kk++ ){
    for( jj=jbot ; jj <= jtop ; jj++ ){
     for( ii=ibot ; ii <= itop ; ii++ ){
       val = sar[IJK(ii,jj,kk)] ;
       if( val > 0 && val <= nhist ){
         hist[val]++ ;
         dsum += (double)(val)*(double)(val); npos++;
       }
   }}}

   if( npos <= 999 ){ free((void *)hist); RETURN(1.0); }

   /*-- initialize cut position to include upper 65% of positive voxels --*/

   qq = 0.65 * npos ; ib = rint(0.5*sqrt(dsum/npos)) ;
   for( kk=0,ii=nhist-1 ; ii >= ib && kk < qq ; ii-- ) kk += hist[ii] ;

   /*-- algorithm --*/

   ncut = ii ;   /* initial cut position */
   qq   = 0 ;    /* iteration count */
   do{
     for( npos=0,ii=ncut ; ii < nhist ; ii++ ) npos += hist[ii]; /* number >= cut */
     nhalf = npos/2 ;
     for( kk=0,ii=ncut ; ii < nhist && kk < nhalf ; ii++ )  /* find median of */
       kk += hist[ii] ;                                     /* valuess >= cut */
     nold = ncut ;                                          /* last cut */
     ncut = mfrac * ii ;                                    /* new cut */
     qq++ ;
  } while( qq < 20 && ncut != nold ) ; /* iterate until done, or at most 20 times */

   free((void *)hist) ;
   RETURN( (float)(ncut) ) ;
}

/*--------------------------------------------------------------------------*/

typedef struct {
   float clip_000, clip_100, clip_010, clip_110,
         clip_001, clip_101, clip_011, clip_111 ;
   float x0,x1,dxi , y0,y1,dyi , z0,z1,dzi ;
   float clip_min , clip_max ;
} clipvec ;

/*! Get the cliplevel for each octant about the center-of-mass. */

static clipvec get_octant_clips( MRI_IMAGE *im , float mfrac )
{
   float xcm,ycm,zcm , sum,val , clip_min , clip_max ;
   int ii,jj,kk , nx,ny,nz,nxy , ic,jc,kc , it,jt,kt , ijk ;
   short *sar ;
   clipvec cv ;

ENTRY("get_octant_clips") ;

   ZZME(cv) ;
   cv.clip_000 = -1 ;  /* flags error return */

   if( im == NULL || im->kind != MRI_short ) RETURN(cv) ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;
   it = nx-1   ; jt = ny-1   ; kt = nz-1   ;

   /* compute CM of image */

   sar = MRI_SHORT_PTR(im) ; if( sar == NULL ) RETURN(cv) ;

   xcm = ycm = zcm = sum = 0.0 ;
   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,ijk++ ){
       val = (float)sar[ijk]; if( val <= 0.0 ) continue;
       sum += val ;
       xcm += val * ii ;
       ycm += val * jj ;
       zcm += val * kk ;
   }}}
   if( sum == 0.0 ) RETURN(cv) ;
   ic = (int)rint(xcm/sum); jc = (int)rint(ycm/sum); kc = (int)rint(zcm/sum);

   /* compute cliplevel in each octant about the CM */

   val = 0.5 * partial_cliplevel( im,mfrac , 0,it , 0,jt , 0,kt ) ;

   cv.clip_000 = partial_cliplevel( im,mfrac,  0  ,ic+2,  0  ,jc+2,  0  ,kc+2 );
   cv.clip_100 = partial_cliplevel( im,mfrac, ic-2,it  ,  0  ,jc+2,  0  ,kc+2 );
   cv.clip_010 = partial_cliplevel( im,mfrac,  0  ,ic+2, jc-2,jt  ,  0  ,kc+2 );
   cv.clip_110 = partial_cliplevel( im,mfrac, ic-2,it  , jc-2,jt  ,  0  ,kc+2 );
   cv.clip_001 = partial_cliplevel( im,mfrac,  0  ,ic+2,  0  ,jc+2, kc-2,kt   );
   cv.clip_101 = partial_cliplevel( im,mfrac, ic-2,it  ,  0  ,jc+2, kc-2,kt   );
   cv.clip_011 = partial_cliplevel( im,mfrac,  0  ,ic+2, jc-2,jt  , kc-2,kt   );
   cv.clip_111 = partial_cliplevel( im,mfrac, ic-2,it  , jc-2,jt  , kc-2,kt   );

   if( cv.clip_000 < val ) cv.clip_000 = val ;  /* don't let   */
   if( cv.clip_100 < val ) cv.clip_100 = val ;  /* clip levels */
   if( cv.clip_010 < val ) cv.clip_010 = val ;  /* get too     */
   if( cv.clip_110 < val ) cv.clip_110 = val ;  /* small!      */
   if( cv.clip_001 < val ) cv.clip_001 = val ;
   if( cv.clip_101 < val ) cv.clip_101 = val ;
   if( cv.clip_011 < val ) cv.clip_011 = val ;
   if( cv.clip_111 < val ) cv.clip_111 = val ;

   clip_min =              cv.clip_000  ;
   clip_min = MIN(clip_min,cv.clip_100) ;
   clip_min = MIN(clip_min,cv.clip_010) ;
   clip_min = MIN(clip_min,cv.clip_110) ;
   clip_min = MIN(clip_min,cv.clip_001) ;
   clip_min = MIN(clip_min,cv.clip_101) ;
   clip_min = MIN(clip_min,cv.clip_011) ;
   clip_min = MIN(clip_min,cv.clip_111) ;  cv.clip_min = clip_min ;

   clip_max =              cv.clip_000  ;
   clip_max = MAX(clip_max,cv.clip_100) ;
   clip_max = MAX(clip_max,cv.clip_010) ;
   clip_max = MAX(clip_max,cv.clip_110) ;
   clip_max = MAX(clip_max,cv.clip_001) ;
   clip_max = MAX(clip_max,cv.clip_101) ;
   clip_max = MAX(clip_max,cv.clip_011) ;
   clip_max = MAX(clip_max,cv.clip_111) ;  cv.clip_max = clip_max ;

   /* (x0,y0,z0) = center of lowest octant
      (x1,y1,z1) = center of highest octant */

   cv.x0  = 0.5*ic ; cv.x1 = 0.5*(ic+it) ;
   cv.y0  = 0.5*jc ; cv.y1 = 0.5*(jc+jt) ;
   cv.z0  = 0.5*kc ; cv.z1 = 0.5*(kc+kt) ;
   cv.dxi = (cv.x1 > cv.x0) ? 1.0/(cv.x1-cv.x0) : 0.0 ;
   cv.dyi = (cv.y1 > cv.y0) ? 1.0/(cv.y1-cv.y0) : 0.0 ;
   cv.dzi = (cv.z1 > cv.z0) ? 1.0/(cv.z1-cv.z0) : 0.0 ;

#if 0
   if( verb )
    fprintf(stderr," + get_octant_clips:  min clip=%.1f\n"
                   "   clip_000=%.1f  clip_100=%.1f  clip_010=%.1f  clip_110=%.1f\n"
                   "   clip_001=%.1f  clip_101=%.1f  clip_011=%.1f  clip_111=%.1f\n"
                   "   (x0,y0,z0)=(%.1f,%.1f,%.1f) (x1,y1,z1)=(%.1f,%.1f,%.1f)\n" ,
            val ,
            cv.clip_000 , cv.clip_100 , cv.clip_010 , cv.clip_110 ,
            cv.clip_001 , cv.clip_101 , cv.clip_011 , cv.clip_111 ,
            cv.x0 , cv.y0 , cv.z0 , cv.x1 , cv.y1 , cv.z1  ) ;
#endif

   RETURN(cv) ;
}

/*--------------------------------------------------------------------------*/
/*! Return the cliplevel at any point,
    tri-linearly interpolated between octant centers. */

static INLINE float pointclip( int ii, int jj, int kk , clipvec *cv )
{
   float x1,y1,z1 , x0,y0,z0 , val ;

   /* get relative position in box defined by octant centers */

   x1 = (ii-cv->x0)*cv->dxi; if(x1 < 0.0) x1=0.0; else if(x1 > 1.0) x1=1.0;
   y1 = (jj-cv->y0)*cv->dyi; if(y1 < 0.0) y1=0.0; else if(y1 > 1.0) y1=1.0;
   z1 = (kk-cv->z0)*cv->dzi; if(z1 < 0.0) z1=0.0; else if(z1 > 1.0) z1=1.0;

   x0 = 1.0-x1 ; y0 = 1.0-y1 ; z0 = 1.0-z1 ;

   val =  cv->clip_000 * x0*y0*z0 + cv->clip_100 * x1*y0*z0
        + cv->clip_010 * x0*y1*z0 + cv->clip_110 * x1*y1*z0
        + cv->clip_001 * x0*y0*z1 + cv->clip_101 * x1*y0*z1
        + cv->clip_011 * x0*y1*z1 + cv->clip_111 * x1*y1*z1 ;
   return val ;
}

/*--------------------------------------------------------------------------*/
/*! Make a mask from the 3D image, based on gradually changing cliplevels.
    This feature is to allow for gradual shading in the MRI volume.        */

static byte * mri_short2mask( MRI_IMAGE *im )
{
   int ii,jj,kk,ijk , nx,ny,nz,nxy,nxyz ;
   clipvec bvec , tvec ;
   short *sar ;
   byte *mask ;
   float bval , tval ;

ENTRY("mri_short2mask") ;
   if( im == NULL || im->kind != MRI_short ) RETURN(NULL) ;
   sar = MRI_SHORT_PTR(im) ; if( sar == NULL ) RETURN(NULL) ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   bvec = get_octant_clips( im , 0.40f ) ;
   if( bvec.clip_000 < 0.0 ) RETURN(NULL) ;

   tvec = bvec ;
   tvec.clip_000 *= 9.91 ;
   tvec.clip_100 *= 9.91 ;
   tvec.clip_010 *= 9.91 ;
   tvec.clip_110 *= 9.91 ;
   tvec.clip_001 *= 9.91 ;
   tvec.clip_101 *= 9.91 ;
   tvec.clip_011 *= 9.91 ;
   tvec.clip_111 *= 9.91 ;

   /* create mask, clipping at a level that varies spatially */

#if 0
   if( verb ) fprintf(stderr," + mri_short2mask: clipping\n") ;
#endif

   mask = (byte *) malloc( sizeof(byte)*nxyz ) ;
   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,ijk++ ){
       bval = pointclip( ii,jj,kk , &bvec ) ; /* cliplevel here */
#if 0
       tval = pointclip( ii,jj,kk , &tvec ) ; /* cliplevel here */
       mask[ijk] = (sar[ijk] >= bval && sar[ijk] <= tval) ; /* binarize */
#else
       mask[ijk] = (sar[ijk] >= bval) ; /* binarize */
#endif
   }}}

   /* remove small clusters */

   clustedit3D( nx,ny,nz , mask , (int)rint(0.02*nxyz) ) ;

   if( verb > 1) fprintf(stderr," + mri_short2mask: %d voxels survive\n",
                             mask_count(nxyz,mask) ) ;

   RETURN(mask) ;
}

/*--------------------------------------------------------------------------*/

typedef struct { short i,j,k,val; int basin; } shortvox ;

/*--------------------------------------------------------------------------*/
/*! Sort array of shortvox into increasing order (decreasing if dec != 0). */

static sort_shortvox( int n , shortvox *ar , int dec , float botperc, float topperc )
{
   int ii,jj , sbot,stop,nsv , sval , pbot,ptop ;
   int *hsv , *csv ;
   shortvox *tar ;

   if( n < 2 || ar == NULL ) return ;

   /* decreasing order desired?  flip values */

   if( dec ){
     float tmp ;
     for( ii=0 ; ii < n ; ii++ ) ar[ii].val = -ar[ii].val ;
     tmp = botperc ; botperc = topperc ; topperc = tmp ;
   }

   /* find range of values */

   sbot = stop = ar[0].val ;
   for( ii=1 ; ii < n ; ii++ ){
     sval = ar[ii].val ;
          if( sval < sbot ) sbot = sval ;
     else if( sval > stop ) stop = sval ;
   }
   nsv = stop-sbot+1 ;   /* number of distinct values */
   if( nsv <= 1 ) return ;

   /* build hsv[i] = how many have value = sbot+i
            csv[i] = how many have value < sbot+i, i=0..nsv-1 */

   hsv = (int *)calloc(sizeof(int),nsv) ;
   csv = (int *)calloc(sizeof(int),nsv+1) ;
   for( ii=0 ; ii <  n   ; ii++ ) hsv[ar[ii].val-sbot]++ ;
   for( ii=1 ; ii <= nsv ; ii++ ) csv[ii] = csv[ii-1]+hsv[ii-1] ;
   free((void *)hsv) ;

   if( botperc > 0.0 && botperc < 50.0 ){
     jj = (int)rint(0.01*botperc*n) ;
     for( ii=0 ; ii < nsv && csv[ii] <= jj ; ii++ ) ;
     pbot = ii+sbot ;
     if( verb ) fprintf(stderr," + sort_shortvox: sbot=%d pbot=%d\n",sbot,pbot) ;
   } else {
     pbot = sbot ;
   }

   if( topperc > 0.0 && topperc < 50.0 ){
     jj = (int)rint(0.01*(100.0-topperc)*n) ;
     for( ii=0 ; ii < nsv && csv[ii] <= jj ; ii++ ) ;
     ptop = ii+sbot ;
     if( verb ) fprintf(stderr," + sort_shortvox: stop=%d ptop=%d\n",stop,ptop) ;
   } else {
     ptop = stop ;
   }

   /* copy from ar into temp array tar,
      putting each one into its place as given by csv */

   tar = (shortvox *)calloc(sizeof(shortvox),n) ;
   for( ii=0 ; ii < n ; ii++ ){
     sval = ar[ii].val - sbot ;   /* sval is in 0..nsv-1 now */
     tar[ csv[sval] ] = ar[ii] ;
     csv[sval]++ ;
   }

   if( pbot > sbot ){
     for( ii=0 ; ii < n ; ii++ )
       if( tar[ii].val < pbot ) tar[ii].val = pbot ;
   }
   if( ptop < stop ){
     for( ii=0 ; ii < n ; ii++ )
       if( tar[ii].val > ptop ) tar[ii].val = ptop ;
   }

   /* copy back into ar */

   memcpy( ar , tar , sizeof(shortvox)*n ) ;
   free((void *)tar) ; free((void *)csv) ;

   /* unflip? */

   if( dec )
     for( ii=0 ; ii < n ; ii++ ) ar[ii].val = -ar[ii].val ;

   return ;
}

/*--------------------------------------------------------------------------*/

typedef struct { int num, nall, depth, *ivox; } basin ;

#define DBALL 32768

#define BDEP(i) (baslist[i]->depth)

#define INIT_BASIN(iv)                                       \
 { register int qb=nbtop;                                    \
   if( qb >= nball ){                                        \
     register int qqb=1.2*nball+DBALL,zb ;                   \
     baslist = (basin **)realloc((void *)baslist,            \
                                 sizeof(basin *)*qqb) ;      \
     for( zb=nball ; zb < qqb ; zb++ ) baslist[zb] = NULL ;  \
     nball = qqb ;                                           \
   }                                                         \
   baslist[qb] = (basin *) malloc(sizeof(basin)) ;           \
   baslist[qb]->num     = 1 ;                                \
   baslist[qb]->nall    = 1 ;                                \
   baslist[qb]->depth   = svox[iv].val ;                     \
   baslist[qb]->ivox    = (int *)malloc(sizeof(int)) ;       \
   baslist[qb]->ivox[0] = (iv) ;                             \
   svox[iv].basin       = qb ; nbtop++ ;                     \
 }

#define KILL_BASIN(ib)                                       \
 { if( baslist[ib] != NULL ){                                \
     free((void *)baslist[ib]->ivox) ;                       \
     free((void *)baslist[ib]) ;                             \
     baslist[ib] = NULL ; }                                  \
 }

#define ADDTO_BASIN(ib,iv)                                   \
 { register basin *bb = baslist[ib] ;                        \
   if( bb->num == bb->nall ){                                \
     bb->nall = (int)(1.2*bb->nall)+32 ;                     \
     bb->ivox = (int *)realloc( (void *)bb->ivox ,           \
                                 sizeof(int)*bb->nall ) ;    \
   }                                                         \
   bb->ivox[bb->num] = (iv) ; bb->num++ ;                    \
   svox[iv].basin = (ib) ; }

#define MERGE_BASIN(ib,ic)                                   \
 { register basin *bb = baslist[ib], *cc = baslist[ic] ;     \
   int zz = bb->num + cc->num ;                              \
   if( bb->nall < zz ){                                      \
     bb->nall = zz+1 ;                                       \
     bb->ivox = (int *)realloc( (void *)bb->ivox ,           \
                                  sizeof(int)*bb->nall ) ;   \
   }                                                         \
   memcpy( bb->ivox + bb->num ,                              \
           cc->ivox , sizeof(int) * cc->num ) ;              \
   bb->num = zz ;                                            \
   for( zz=0 ; zz < cc->num ; zz++ )                         \
     svox[cc->ivox[zz]].basin = (ib) ;                       \
   KILL_BASIN(ic) ; }

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_watershedize( MRI_IMAGE *sim , float prefac )
{
   MRI_IMAGE *tim ;
   int ii,jj,kk , pp,qq , nx,ny,nz,nxy,nxyz , nvox ;
   int ip,jp,kp , im,jm,km ;
   short *sar , *tar ;
   shortvox *svox ;
   int *isvox , *bcount,*bname ;
   int nb,vb,mb,m,mu,mq,mz , bp[6] , hpf ;

   basin **baslist ;
   int nball , nbtop ;

ENTRY("watershedize") ;

   if( sim == NULL || sim->kind != MRI_short ) RETURN(NULL) ;
   sar = MRI_SHORT_PTR(sim) ; if( sar == NULL ) RETURN(NULL) ;

   nx = sim->nx; ny = sim->ny; nz = sim->nz; nxy = nx*ny; nxyz = nxy*nz;

   /* count number of voxels > 0 */

   for( nvox=0,pp=0 ; pp < nxyz ; pp++ ) if( sar[pp] > 0 ) nvox++ ;
   if( nvox <= 999 ) RETURN(NULL) ;

   if( verb ) fprintf(stderr," + mri_watershedize: %d voxels input\n",nvox) ;

   /* create voxel lists */

   svox  = (shortvox *) malloc( sizeof(shortvox)* nvox ) ;
   isvox = (int *)      malloc( sizeof(int)     * nxyz ) ;
   for( qq=pp=0 ; pp < nxyz ; pp++ ){
     if( sar[pp] > 0 ){                  /* save this one: */
       ii             = pp % nx ;        /* spatial indexes */
       jj             = (pp%nxy) / nx ;
       kk             = pp / nxy ;
       svox[qq].i     = ii ;
       svox[qq].j     = jj ;
       svox[qq].k     = kk ;
       svox[qq].val   = sar[pp] ;        /* value */
       svox[qq].basin = -1 ;             /* which basin */
       qq++ ;
       isvox[pp]      = qq ;             /* where in list */
     } else {
       isvox[pp] = -1 ;                  /* voxel not in list */
     }
   }

   /* sort voxel list into descending order */

   if( verb ) fprintf(stderr," + mri_watershedize: sorting voxels\n") ;

   sort_shortvox( nvox , svox , 1 , 0.00 , 0.02 ) ;

   /* create basin for first (deepest) voxel */

   nball    = DBALL ;
   nbtop    = 0 ;
   baslist  = (basin **) calloc(sizeof(basin *),nball) ;

   INIT_BASIN(0) ;

   hpf      = (int)rint(prefac*svox[0].val) ;      /* preflood */

   /* scan voxels as they get shallower, and basinate them */

   if( verb ){
     fprintf(stderr," + mri_watershedize: basinating voxels\n") ;
     fprintf(stderr,"  data range: %d..%d preflood_height=%d\n",
             svox[nvox-1].val , svox[0].val , hpf ) ;
   }

   for( pp=1 ; pp < nvox ; pp++ ){

     ii = svox[pp].i; jj = svox[pp].j; kk = svox[pp].k;  /* where */
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;                 /* nbhrs */
     im = ii-1 ; jm = jj-1 ; km = kk-1 ;

     if( verb && pp%100000 == 0 ) fprintf(stderr, (pp%1000000)?".":"!") ;

     /* macro checks if (a,b,c) voxel is in the list;
        if so and it is already in a basin, then
        make a list of basins encountered:
          nb = number of unique basins encountered (0..6)
          mb = index of deepest basin encountered (0..nb-1)
          vb = value (depth) of deepest basin encountered
          bp[m] = index of m-th basin encountered (m=0..nb-1) */

#undef  BASECHECK
#define BASECHECK(a,b,c)                                   \
    { qq = isvox[IJK(a,b,c)] ;                             \
      if( qq >= 0 && svox[qq].basin >= 0 ){                \
        qq = svox[qq].basin ;                              \
        for( m=0 ; m < nb && bp[m] != qq ; m++ ) ;         \
        if( m == nb ){                                     \
          bp[nb] = qq ;                                    \
          if( BDEP(qq) > vb ){ mb = nb; vb = BDEP(qq); }   \
          nb++ ;                                           \
        }                                                  \
      }                                                    \
    }

     nb = 0 ; vb = -1 ; mb = -1 ;         /* initialize counters */
     if( ip < nx ) BASECHECK(ip,jj,kk) ;  /* check each neighbor */
     if( im >= 0 ) BASECHECK(im,jj,kk) ;  /* for basin-ositiness */
     if( jp < ny ) BASECHECK(ii,jp,kk) ;
     if( jm >= 0 ) BASECHECK(ii,jm,kk) ;
     if( kp < nz ) BASECHECK(ii,jj,kp) ;
     if( km >= 0 ) BASECHECK(ii,jj,km) ;

     if( nb == 0 ){  /*** this voxel is isolated ==> create new basin ****/

       INIT_BASIN(pp) ;

     } else {        /*** this voxel has deeper neighbors ***/

       mq = bp[mb] ;                      /* assign voxel to best basin */
       ADDTO_BASIN( mq , pp ) ;

                       /* if have more than one neighbor, other */
       if( nb > 1 ){   /* basins could be merged with the best  */
         mz = svox[pp].val ;          /* depth of this voxel */
         for( m=0 ; m < nb ; m++ ){
           if( m == mb ) continue ;        /* can't merge with itself */
           mu = bp[m] ;
           if( BDEP(mu)-mz <= hpf ){       /* basin not TOO much deeper */
             MERGE_BASIN(mq,mu) ;
           }
         }
       }
     }
   } /* end of loop over voxels */

   /* at this point, all voxels in svox are assigned to a basin */

   free((void *)isvox) ;

   /* count number of basines left */

   for( mu=m=0 ; m < nbtop ; m++ )
     if( baslist[m] != NULL ) mu++ ;

   if( verb ) fprintf(stderr,"\n++ %d active basins left, out of %d\n",mu,nbtop) ;

   bcount = (int *) calloc(sizeof(int),mu) ;     /* number in each basin */
   bname  = (int *) calloc(sizeof(int),mu) ;
   isvox  = (int *) calloc(sizeof(int),nbtop) ;  /* new index */

   for( m=ii=0 ; m < nbtop ; m++ )
     if( baslist[m] != NULL ){ isvox[m] = ii; bname[ii] = ii; ii++; KILL_BASIN(m); }
   free((void *)baslist) ;

   for( pp=0 ; pp < nvox ; pp++ ){
     m  = svox[pp].basin ;           /* old basin name for this voxel */
     ii = isvox[m] ;                 /* new basin name for this voxel */
     svox[pp].basin = ii ;           /* reassign name in this voxel */
     bcount[ii]++ ;                  /* count number in this basin */
   }

   tim = mri_new_conforming( sim , MRI_short ) ;  /* output image */
   MRI_COPY_AUX(tim,sim) ;
   tar = MRI_SHORT_PTR(tim) ;

   for( ii=0 ; ii < mu ; ii++ ) bcount[ii] = -bcount[ii] ;
   qsort_intint( mu , bcount , bname ) ;  /* sort into decreasing order */
   for( ii=0 ; ii < mu ; ii++ ) bcount[ii] = -bcount[ii] ;

   if( verb )
     fprintf(stderr," + top 9 basin counts: %d %d %d %d %d %d %d %d %d\n",
             bcount[0] , bcount[1] , bcount[2] , bcount[3] ,
             bcount[4] , bcount[5] , bcount[6] , bcount[7] , bcount[8] ) ;

   for( ii=0 ; ii < mu ; ii++ ) isvox[ii] = ii ;
   qsort_intint( mu , bname , isvox ) ;

   for( pp=0 ; pp < nvox ; pp++ ){
     m = svox[pp].basin ; jj = isvox[m]+1 ; if( jj > 32767 ) jj = 32767 ;
     tar[IJK(svox[pp].i,svox[pp].j,svox[pp].k)] = jj ;
   }

   free((void *)isvox) ; free((void *)svox );
   free((void *)bcount); free((void *)bname);

   return tim ;
}

/*------------------------------------------------------------------------*/

static byte * make_peel_mask( int nx, int ny, int nz , byte *mmm, int pdepth )
{
   int nxy=nx*ny,nxyz=nxy*nz , ii,jj,kk , ijk , bot,top , pd=pdepth ;
   byte *ppp ;

   if( mmm == NULL || pdepth <= 1 ) return NULL ;

   ppp = (byte *)calloc(sizeof(byte),nxyz) ;

   for( kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
      ijk = jj*nx + kk*nxy ;
      for( bot=0 ; bot < nx && !mmm[bot+ijk]; bot++ ) ;
      top = bot+pd ; if( top >= nx ) continue ;
      for( ii=bot+1 ; ii <= top && mmm[ii+ijk] ; ii++ ) ;
      if( ii <= top ){
        top = ii; for( ii=bot ; ii <= top ; ii++ ) ppp[ii+ijk] = 1;
      }
   }}

   for( kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
      ijk = jj*nx + kk*nxy ;
      for( top=nx-1 ; top >= 0 && !mmm[top+ijk]; top-- ) ;
      bot = top-pd ; if( bot < 0 ) continue ;
      for( ii=top-1 ; ii >= bot && mmm[ii+ijk] ; ii-- ) ;
      if( ii >= bot ){
        bot = ii; for( ii=top ; ii >= bot ; ii-- ) ppp[ii+ijk] = 1;
      }
   }}

   for( kk=0 ; kk < nz ; kk++ ){
    for( ii=0 ; ii < nx ; ii++ ){
      ijk = ii + kk*nxy ;
      for( bot=0 ; bot < ny && !mmm[bot*nx+ijk] ; bot++ ) ;
      top = bot+pd ;
      if( top >= ny ) continue ;
      for( jj=bot+1 ; jj <= top && mmm[jj*nx+ijk] ; jj++ ) ;
      if( jj <= top ){
        top = jj; for( jj=bot ; jj <= top ; jj++ ) ppp[jj*nx+ijk] = 1;
      }
   }}

   for( kk=0 ; kk < nz ; kk++ ){
    for( ii=0 ; ii < nx ; ii++ ){
      ijk = ii + kk*nxy ;
      for( top=ny-1 ; top >= 0 && !mmm[top*nx+ijk] ; top-- ) ;
      bot = top-pd ; if( bot < 0 ) continue ;
      for( jj=top-1 ; jj >= bot && mmm[jj*nx+ijk] ; jj-- ) ;
      if( jj >= bot ){
        bot = jj; for( jj=top ; jj >= bot ; jj-- ) ppp[jj*nx+ijk] = 1;
      }
   }}

#if 1
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       ijk = ii + jj*nx ;
       for( top=nz-1 ; top >= 0 && !mmm[top*nxy+ijk] ; top-- ) ;
       bot = top-pd ; if( bot < 0 ) continue ;
       for( kk=top-1 ; kk >= bot && mmm[kk*nxy+ijk] ; kk-- ) ;
       if( kk >= bot ){
         bot = kk; for( kk=top ; kk >= bot ; kk-- ) ppp[kk*nxy+ijk] = 1;
       }
    }}
#endif

   kk = mask_count(nxyz,ppp) ;
   if( kk == 0 ){ free((void *)ppp) ; return NULL ; }
   if( verb ) fprintf(stderr," + Initial peel mask has %d voxels\n",kk ) ;
   THD_mask_erode( nx,ny,nz, ppp, 1 ) ;
   THD_mask_clust( nx,ny,nz, ppp ) ;
   kk = mask_count(nxyz,ppp) ;
   if( kk == 0 ){ free((void *)ppp) ; return NULL ; }
   if( verb ) fprintf(stderr," + Final   peel mask has %d voxels\n",kk ) ;

   return ppp ;
}

/*------------------------------------------------------------------------*/

static void zedit_mask( int nx, int ny, int nz, byte *mmm, int zdepth, int zbot )
{
   int nxy=nx*ny,nxyz=nxy*nz , ii,jj,kk , ijk , bot,top ;
   int zd=zdepth , zb=zbot , zt , zslab , zz ;
   byte *ppp , *zzz ;

   if( mmm == NULL ) return ;

   if( zd < 1  ) zd = 1  ;
   if( zb < zd ) zb = zd ;
   zslab = 2*zd+1 ;

   for( kk=nz-1 ; kk >= zb ; kk-- ){
     jj = mask_count( nxy , mmm+kk*nxy ) ;
     if( jj > 0.005*nxy ) break ;
   }
   zt = kk-zd ; if( zt < zb ) return ;

   ppp = (byte *)calloc(sizeof(byte),nxyz) ;
   zzz = (byte *)calloc(sizeof(byte),nxy*zslab) ;

   for( zz=zb ; zz <= zt ; zz++ ){
     memcpy( zzz , mmm+(zz-zd)*nxy , nxy*zslab ) ;
     THD_mask_erode( nx,ny,zslab, zzz, 1 ) ;
     THD_mask_clust( nx,ny,zslab, zzz ) ;
     memcpy( ppp+zz*nxy , zzz+zd*nxy , nxy ) ;
   }
   free((void *)zzz) ;
   memcpy( mmm+zb*nxy , ppp+zb*nxy , (zt-zb+1)*nxy ) ;
   free((void *)ppp) ;
   THD_mask_erode( nx,ny,nz, mmm, 1 ) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;
}

/*======================================================================*/
   /** ZSS: Definitions moved to thd_brainormalize.h **/
/*----------------------------------------------------------------------*/
/*! Index warping function for mri_warp3D() call. */

static float ai,bi , aj,bj , ak,bk ;  /* set below */

static void ijkwarp( float  i, float  j, float  k ,
                     float *x, float *y, float *z  )
{
  *x = ai*i + bi ;
  *y = aj*j + bj ;
  *z = ak*k + bk ;
}

static void ijk_invwarp(   float x, float y, float z ,  
                           float  *i, float  *j, float  *k)
{
  *i = ( x - bi ) / ai;
  *j = ( y - bj ) / aj;
  *k = ( z - bk ) / ak;
}


/*! 
   \brief takes in voxel indices into the Spat Normed volume (RAI) and
   returns voxel indices and coordinates in the original volume. Used
   to figure out shift to apply to surface model to align it with original volume.
   
   \param ispat (float) 3D i index into spat norm volume
   \param jspat (float) 3D j index into spat norm volume
   \param kspat (float) 3D k index into spat norm volume
   \param iorig (float*) 3D i index into original volume
   \param jorig (float*) 3D j index into original volume
   \param korig (float*) 3D k index into original volume
   \param origset (THD_3dim_dataset *) Le original dataset
   \param *xrai_orig (float *) X coordinate in original volume (dicomm)
   \param *yrai_orig (float *) Y coordinate in original volume (dicomm)
   \param *zrai_orig (float *) Z coordinate in original volume (dicomm)
   
   ZSS Sometime in April 05
   
   \sa brainnormalize_inv_coord
*/
void brainnormalize_coord( float  ispat, float  jspat, float  kspat ,
                           float *iorig, float *jorig, float *korig ,
                           THD_3dim_dataset *origset,
                           float *xrai_orig, float *yrai_orig, float *zrai_orig)
{
   THD_dataxes * daxes ;
   THD_fvec3     fv, fvdic ;
   float irai, jrai, krai;
   
   /* find out corresponding indices in original dset */
   ijkwarp(ispat, jspat, kspat , &irai, &jrai, &krai);
   

    
   /* These indices assume an RAI dset orientation */
      /* Find out what these indices should be in origset's orientation */
      switch( origset->daxes->xxorient ){
        case ORI_R2L_TYPE: *iorig =  irai ; break ;
        case ORI_L2R_TYPE: *iorig =  origset->daxes->nxx - irai ; break ;
        case ORI_P2A_TYPE: *iorig =  origset->daxes->nxx - jrai  ; break ;
        case ORI_A2P_TYPE: *iorig =  jrai ; break ;
        case ORI_I2S_TYPE: *iorig =  krai ; break ;
        case ORI_S2I_TYPE: *iorig =  origset->daxes->nxx - krai ; break ;
      }
      switch( origset->daxes->yyorient ){
        case ORI_R2L_TYPE: *jorig =  irai ; break ;
        case ORI_L2R_TYPE: *jorig =  origset->daxes->nyy - irai  ; break ;
        case ORI_P2A_TYPE: *jorig =  origset->daxes->nyy - jrai ; break ;
        case ORI_A2P_TYPE: *jorig =  jrai ; break ;
        case ORI_I2S_TYPE: *jorig =  krai ; break ;
        case ORI_S2I_TYPE: *jorig =  origset->daxes->nyy - krai ; break ;
      }
      switch( origset->daxes->zzorient ){
        case ORI_R2L_TYPE: *korig =  irai ; break ;
        case ORI_L2R_TYPE: *korig =  origset->daxes->nzz - irai ; break ;
        case ORI_P2A_TYPE: *korig =  origset->daxes->nzz - jrai ; break ;
        case ORI_A2P_TYPE: *korig =  jrai ; break ;
        case ORI_I2S_TYPE: *korig =  krai ; break ;
        case ORI_S2I_TYPE: *korig =  origset->daxes->nzz - krai ; break ;
      }
      
            
      /* change indices into mm coords in orig dset*/
      daxes = CURRENT_DAXES(origset) ;

      fv.xyz[0] = daxes->xxorg + *iorig * daxes->xxdel ;  /* 3dfind_to_3dmm */
      fv.xyz[1] = daxes->yyorg + *jorig * daxes->yydel ;
      fv.xyz[2] = daxes->zzorg + *korig * daxes->zzdel ;

      fvdic = THD_3dmm_to_dicomm(origset,   fv );                /* 3dmm_to_dicomm  */
      *xrai_orig = fvdic.xyz[0];
      *yrai_orig = fvdic.xyz[1]; 
      *zrai_orig = fvdic.xyz[2];
       
   /* report for sanity */
   if (0) {
      fprintf(stderr,   "brainnormalize_coord:\n"
                     " ijk_spat_rai = [%f %f %f]\n"
                     " ijk_orig_rai = [%f %f %f] (in rai order, not native to iset!)\n"
                     " ijk_orig     = [%f %f %f] (in native order)\n"
                     " XYZ_orig     = [%f %f %f]\n"
                     " Origin spat = [%f %f %f]\n", 
                     ispat, jspat, kspat,
                     irai, jrai, krai ,
                     *iorig, *jorig, *korig ,
                     *xrai_orig, *yrai_orig, *zrai_orig,
                     thd_bn_xorg, thd_bn_yorg, thd_bn_zorg);   
   }      
   return;
} 

/*----------------------------------------------------------------------
   (a) shortize input and flip brick so that orientation is RAI
   (b) find clip levels and create a binary mask
   (c) find S-most slice that has at least 10% above clip level;
       zero out mask above that slice and also more than 160 mm below
   (d) apply mask to image volume
   (e) resample to master dataset grid, with CM at (0,20,0)
   \param imout_origp (MRI_IMAGE **) a contraption by ZSS to return a SpatNormed
   version that will eventually be rewritten in im's orientation. Pass NULL if
   you want nothing to do with it.
------------------------------------------------------------------------*/

MRI_IMAGE * mri_brainormalize( MRI_IMAGE *im, int xxor, int yyor, int zzor, MRI_IMAGE **imout_origp , MRI_IMAGE **imout_edge)
{
   MRI_IMAGE *sim , *tim , *bim ;
   short *sar , sval ;
   int ii,jj,kk,ijk,ktop,kbot , nx,ny,nz,nxy,nxyz ;
   float val , icm,jcm,kcm,sum , dx,dy,dz ;
   byte *mask , *bar ;
   float sim_dx, sim_dy, sim_dz, sim_xo, sim_yo, sim_zo;
   int sim_nx, sim_ny, sim_nz;
   int *zcount , *hist,*gist , z1,z2,z3 ;
   MRI_IMAGE *imout_orig = NULL;
   
ENTRY("mri_brainormalize") ;

   if( verb) fprintf(stderr,"++mri_brainormalize: normalizing\n") ;
   
   if( im == NULL || xxor < 0 || xxor > LAST_ORIENT_TYPE ||
                     yyor < 0 || yyor > LAST_ORIENT_TYPE ||
                     zzor < 0 || zzor > LAST_ORIENT_TYPE   ) {
                     
                     ERROR_message("NULL input or bad orientation\n");
                     RETURN(NULL) ;
   }

   if( im->nx < 16 || im->ny < 16 || im->nz < 16 ) {
      ERROR_message("Too few slices (< 16) in at least one dimension.\n");
      RETURN(NULL) ;
   }
   val = mri_maxabs(im) ; if( val <= 0.0 ) RETURN(NULL) ;

   /* make a short copy */

   if( verb > 1) fprintf(stderr,"++mri_brainormalize: copying input\n") ;

   if( im->kind == MRI_short || im->kind == MRI_byte || im->kind == MRI_rgb )
     tim = mri_to_short( 1.0 , im ) ;
   else
     tim = mri_to_short( 32767.0/val , im ) ;

   /* flip to RAI orientation */

   ii = jj = kk = 0 ;
   switch( xxor ){
     case ORI_R2L_TYPE: ii =  1 ; break ;
     case ORI_L2R_TYPE: ii = -1 ; break ;
     case ORI_P2A_TYPE: jj = -1 ; break ;
     case ORI_A2P_TYPE: jj =  1 ; break ;
     case ORI_I2S_TYPE: kk =  1 ; break ;
     case ORI_S2I_TYPE: kk = -1 ; break ;
   }
   switch( yyor ){
     case ORI_R2L_TYPE: ii =  2 ; break ;
     case ORI_L2R_TYPE: ii = -2 ; break ;
     case ORI_P2A_TYPE: jj = -2 ; break ;
     case ORI_A2P_TYPE: jj =  2 ; break ;
     case ORI_I2S_TYPE: kk =  2 ; break ;
     case ORI_S2I_TYPE: kk = -2 ; break ;
   }
   switch( zzor ){
     case ORI_R2L_TYPE: ii =  3 ; break ;
     case ORI_L2R_TYPE: ii = -3 ; break ;
     case ORI_P2A_TYPE: jj = -3 ; break ;
     case ORI_A2P_TYPE: jj =  3 ; break ;
     case ORI_I2S_TYPE: kk =  3 ; break ;
     case ORI_S2I_TYPE: kk = -3 ; break ;
   }

   if( ii==1 && jj==2 && kk==3 ){      /* no flip needed */
     sim = tim ;
   } else {                            /* flipization */
     if( verb > 1)
       fprintf(stderr,"++mri_brainormalize: flipping to RAI orientation\n") ;
     sim = mri_flip3D( ii,jj,kk , tim ) ;
     mri_free(tim) ;
     if( sim == NULL ) RETURN(NULL) ;  /* bad orientation codes? */
   }

   sar = MRI_SHORT_PTR(sim) ;
   if( sar == NULL ){ mri_free(sim); RETURN(NULL); }  /* bad image? */

   /* make a binary mask */

   nx = sim->nx ; ny = sim->ny ; nz = sim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   dx = fabs(sim->dx) ; if( dx == 0.0 ) dx = 1.0 ;
   dy = fabs(sim->dy) ; if( dy == 0.0 ) dy = 1.0 ;
   dz = fabs(sim->dz) ; if( dz == 0.0 ) dz = 1.0 ;
      
   /* save some info to create an output image with the same number of slices as original image*/
   sim_dx = sim->dx; sim_dy = sim->dy; sim_dz = sim->dz;
   sim_xo = 0.0; sim_yo = 0.0; sim_zo = 0.0;  /* origins are added after this function returns.*/
   sim_nx = sim->nx; sim_ny = sim->ny; sim_nz = sim->nz; 
   
   if( verb > 1) fprintf(stderr,"++mri_brainormalize: making mask\n") ;
   mask = mri_short2mask( sim ) ;

   if( mask == NULL ){ mri_free(sim); RETURN(NULL); }

   /* fill in any isolated holes in mask */

   (void) THD_mask_fillin_once( nx,ny,nz , mask , 2 ) ;  /* thd_automask.c */
          THD_mask_dilate     ( nx,ny,nz , mask , 5 ) ;
          THD_mask_dilate     ( nx,ny,nz , mask , 5 ) ;

   kk = mask_count(nxyz,mask) ;
   if( verb > 1)
     fprintf(stderr,"++mri_brainormalize: filled in mask now has %d voxels\n",kk) ;

   if( kk <= 999 ){ free((void *)mask); mri_free(sim); RETURN(NULL); }

   /* descending from Superior:
        count biggest blob in each slice
        find Superiormost location that has 3
          slices in a row with "a lot" of stuff
        zero out all stuff out above that slice */

   zcount = (int *)  malloc( sizeof(int) *nz  ) ;  /* slice counts */
   for( kk=nz-1 ; kk >= 0 ; kk-- ){
     zcount[kk] = mask_count( nxy , mask+kk*nxy ) ;
   }

#if 0
   if( verb ){
     fprintf(stderr,"++mri_brainormalize: zcount from top slice #%d\n",nz-1) ;
     for( kk=nz-1 ; kk >= 0 ; kk-- ){
       fprintf(stderr," %.3f",((double)(zcount[kk]))/((double)nxy) ) ;
       if( (nz-kk)%10 == 0 && kk > 0 ) fprintf(stderr,"\n") ;
     }
     fprintf(stderr,"\n") ;
   }
#endif

   /* search down for topmost slice that meets the criterion */

   z1 = (int)(0.010*nxy) ;
   z2 = (int)(0.015*nxy) ;
   z3 = (int)(0.020*nxy) ;
   for( kk=nz-1 ; kk > 2 ; kk-- )
     if( zcount[kk] >= z1 && zcount[kk-1] >= z2 && zcount[kk-2] >= z3 ) break ;

   free((void *)zcount) ;
   if( kk <= 2 ){ free((void *)mask); mri_free(sim); RETURN(NULL); }

   /* zero out all above the slice we just found */

   ktop = kk ;
   if( ktop < nz-1 ){
     if( verb )
       fprintf(stderr,"++mri_brainormalize: top clip above slice %d\n",ktop) ;
     memset( mask+(ktop+1)*nxy , 0 , nxy*(nz-1-ktop)*sizeof(byte) ) ;
   }

   /* find slice index THD_BN_zheight mm below that top slice */

   jj = (int)( ktop-thd_bn_zheight/dz ) ;
   if( jj >= 0 ){
     if( verb )
       fprintf(stderr,"++mri_brainormalize: bot clip below slice %d\n",jj) ;
     memset( mask , 0 , nxy*(jj+1)*sizeof(byte) ) ;
   }

   kk = mask_count(nxyz,mask) ;
   if( kk <= 999 ){ free((void *)mask); mri_free(sim); RETURN(NULL); }

   /* apply mask to image (will also remove any negative values) */

   if( verb > 1)
     fprintf(stderr,
      "++mri_brainormalize: applying mask to image; %d voxels\n",kk) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( !mask[ii] ) sar[ii] = 0 ;

   free((void *)mask) ;  /* done with this mask */
   
   if (verb > 1) WRITE_MRI_IMAGE_3D_RAI(sim,"sim");

   /* compute CM of masked image (indexes, not mm) */

   icm = jcm = kcm = sum = 0.0 ;
#ifndef THD_BN_CMTOP
   kbot = 0 ;
   ktop = nz-1 ;
#else
   kbot = (int)rint( ktop-110.0/dz ); if( kbot < 0 ) kbot = 0;
#endif
   for( ijk=kbot*nxy,kk=kbot ; kk <= ktop ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,ijk++ ){
       val = (float)sar[ijk] ;
       sum += val ;
       icm += val * ii ;
       jcm += val * jj ;
       kcm += val * kk ;
   }}}
   if( sum == 0.0 ){ mri_free(sim); RETURN(NULL); }  /* huh? */
   
   ai = thd_bn_dxyz/dx ; 
   bi = icm/sum - (thd_bn_xcm-thd_bn_xorg)/dx ;
   aj = thd_bn_dxyz/dy ; 
   bj = jcm/sum - (thd_bn_ycm-thd_bn_yorg)/dy ;
   ak = thd_bn_dxyz/dz ; 
   bk = kcm/sum - (thd_bn_zcm-thd_bn_zorg)/dz ;

   if (specie == RAT) { /* Dec 07 */
      /* nudge center of mass up by 5 mm , lots of meat under brain */
      bk = bk + 5.0/dz;   
   }
   if( verb > 1) 
      fprintf(stderr,"++mri_brainormalize: warping to standard grid\n" 
                     "old icm = [%f %f %f],\n"
                     "a = [%f %f %f], b = [%f %f %f]\n"
                     "thd_bn_nxyz=[%d %d %d]\n",
                     icm/sum, jcm/sum, kcm/sum,
                     ai, aj, ak, bi, bj, bk, 
                     thd_bn_nx,thd_bn_ny,thd_bn_nz) ;
                     
   mri_warp3D_method( MRI_CUBIC ) ;
   tim = mri_warp3D( sim , thd_bn_nx,thd_bn_ny,thd_bn_nz , ijkwarp ) ;
   mri_free(sim) ;

   tim->dx = tim->dy = tim->dz = thd_bn_dxyz ;
   tim->xo = thd_bn_xorg ;
   tim->yo = thd_bn_yorg ;
   tim->zo = thd_bn_zorg ;
   
   nx = tim->nx ; ny = tim->ny ; nz = tim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   sar = MRI_SHORT_PTR(tim) ;
   if( verb > 1) 
      fprintf(stderr,"++mri_brainormalize: sar points to %d values.\n", nxyz);
   if( verb > 1) 
      fprintf(stderr,
         "++mri_brainormalize: sar[%d] = %d, sar[%d]=%d, sar[%d]=%d\n",
         0, sar[0], nxyz/2, sar[nxyz/2], nxyz-1, sar[nxyz-1]);
   
   if (verb > 1) WRITE_MRI_IMAGE_3D_RAI(tim,"tim");

   
   /*-- rescale to partially uniformize --*/

   if( verb ) fprintf(stderr,"++mri_brainormalize: Rescaling.\n");
   { clipvec bvec ; float bval , sv ;
     bvec = get_octant_clips( tim , 0.40f ) ;
     if( bvec.clip_000 > 0.0f ){
       for( ijk=kk=0 ; kk < nz ; kk++ ){
        for( jj=0 ; jj < ny ; jj++ ){
         for( ii=0 ; ii < nx ; ii++,ijk++ ){
           bval = pointclip( ii,jj,kk , &bvec ) ; /* cliplevel here */
           sv   = 1000.0f * sar[ijk] / bval ;
           sar[ijk] = SHORTIZE(sv) ;
           if (sar[ijk] < 0) {
            /* if( verb ) fprintf(stderr,"--mri_brainormalize: Negative values! sar[%d %d %d (%d)] = %d\nBlasphemy set to 0.\n",
                           ii, jj, kk, ijk, sar[ijk]); */
            sar[ijk] = 0;  /* ZSS April 11 06 */
           }
         }
     }}}
   }

   /*-- build another mask now --*/
   if( !AFNI_noenv("REMASK") ){
     int sbot,stop , nwid , cbot,ctop , ibot,itop ;
     float dsum , ws , *wt ;
     int pval[128] , wval[128] , npk , tval , nmask,nhalf ;
     float pdif ;
     short mbot,mtop ;

     if( verb > 1) fprintf(stderr,"++mri_brainormalize: Remasking.\n");
     
     /* build histogram */
     if( verb > 1) fprintf(stderr,"++mri_brainormalize:     Build histogram\n");
     hist = (int *) calloc(sizeof(int),32768) ;
     gist = (int *) calloc(sizeof(int),32768) ;

     memset( hist , 0 , sizeof(int)*32768 ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) { 
         /* fprintf(stderr," hist[%d] was %d\n", sar[ii], hist[sar[ii]]); */
         hist[sar[ii]]++ ;
     }
      if( verb > 1) fprintf(stderr,"++mri_brainormalize:     Find min\n");
     for( sbot=1 ; sbot < 32768 && hist[sbot]==0 ; sbot++ ) ; /* nada */
     if( sbot == 32768 ) goto Remask_Done ;
     for( stop=32768-1 ; stop > sbot && hist[stop]==0 ; stop-- ) ; /* nada */
     if( stop == sbot ) goto Remask_Done ;

     /* find median */
     
     if( verb > 1) fprintf(stderr,"++mri_brainormalize:     Find Median\n");
     nmask = 0 ;
     for( ii=sbot ; ii <= stop ; ii++ ) nmask += hist[ii] ;
     nhalf = nmask / 2 ; nmask = 0 ;
     for( ii=sbot ; ii <= stop && nmask < nhalf ; ii++ ) nmask += hist[ii] ;
     cbot = 0.40 * ii ;
     ctop = 1.60 * ii ;

#if 0
     /* smooth histogram */

     nwid = rint(0.10*cbot) ;

     if( nwid <= 0 ){
       memcpy( gist , hist , sizeof(int)*32768 ) ;
     } else {
       ws = 0.0f ;
       wt = (float *)malloc(sizeof(float)*(2*nwid+1)) ;
       for( ii=0 ; ii <= 2*nwid ; ii++ ){
         wt[ii] = nwid-abs(nwid-ii) + 0.5f ;
         ws += wt[ii] ;
       }
       for( ii=0 ; ii <= 2*nwid ; ii++ ) wt[ii] /= ws ;
       for( jj=cbot ; jj <= ctop ; jj++ ){
         ibot = jj-nwid ; if( ibot < sbot ) ibot = sbot ;
         itop = jj+nwid ; if( itop > stop ) itop = stop ;
         ws = 0.0 ;
         for( ii=ibot ; ii <= itop ; ii++ )
           ws += wt[nwid-jj+ii] * hist[ii] ;
         gist[jj] = rint(ws) ;
       }
       free(wt) ;
     }

     /* scan for peaks */

     npk = 0 ;
     for( ii=cbot+2 ; ii <= ctop-2 ; ii++ ){
       if( gist[ii] > gist[ii-1] &&
           gist[ii] > gist[ii-2] &&
           gist[ii] > gist[ii+1] &&
           gist[ii] > gist[ii+2]   ){
             pval[npk]=ii; wval[npk++] = gist[ii];
           }

       else if( gist[ii] == gist[ii+1] &&   /* very special case */
                gist[ii] >  gist[ii-1] &&
                gist[ii] >  gist[ii-2] &&
                gist[ii] >  gist[ii+2]   ){
                  pval[npk]=ii+0.5; wval[npk++] = gist[ii];
                }

       else if( gist[ii] == gist[ii+1] &&   /* super special case */
                gist[ii] == gist[ii-1] &&
                gist[ii] >  gist[ii-2] &&
                gist[ii] >  gist[ii+2]   ){
                  pval[npk]=ii; wval[npk++] = gist[ii];
                }
     }

     if( npk > 2 ){  /* find largest two peaks and keep only them */
       float pval_top, pval_2nd, wval_top, wval_2nd , www; int iii,itop ;
       www = wval[0] ; iii = 0 ;
       for( ii=1 ; ii < npk ; ii++ ){
         if( wval[ii] > www ){ www = wval[ii] ; iii = ii ; }
       }
       pval_top = pval[iii] ; wval_top = www ; itop = iii ;
       www = -1 ; iii = -1 ;
       for( ii=0 ; ii < npk ; ii++ ){
         if( ii != itop && wval[ii] > www ){ www = wval[ii] ; iii = ii ; }
       }
       pval_2nd = pval[iii] ; wval_2nd = www ;

       /* make sure peaks are increasing in pval */

       if( pval_top < pval_2nd ){
         pval[0] = pval_top ; wval[0] = wval_top ;
         pval[1] = pval_2nd ; wval[1] = wval_2nd ;
       } else {
         pval[0] = pval_2nd ; wval[0] = wval_2nd ;
         pval[1] = pval_top ; wval[1] = wval_top ;
       }
       npk = 2 ;
     }

     if( npk == 2 ){
       jj = gist[pval[0]] ; tval = pval[0] ;
       for( ii=pval[0]+1 ; ii < pval[1] ; ii++ ){
         if( gist[ii] < jj ){ tval = ii ; jj = gist[ii] ; }
       }

       pdif = 1.5f * (tval-pval[0]) ;
       if( pdif < pval[1]-pval[0] ) pdif = pval[1]-pval[0] ;
       mbot = (short)(pval[0]-pdif) ;
       if( mbot < cbot ) mbot = cbot ;

       pdif = 1.5f * (pval[1]-tval) ;
       if( pdif < pval[1]-pval[0] ) pdif = pval[1]-pval[0] ;
       mtop = (short)(pval[1]+pdif) ;
       if( mtop > ctop ) mtop = ctop ;
     } else {
       mbot = cbot ; mtop = ctop ;
     }
     mtop = stop+1 ;  /* effectively, no threshold here */
#endif

     mbot = cbot ; mtop = 32767 ;

     if( verb > 1)
      fprintf(stderr,"++mri_brainormalize: masking standard image %d..%d\n",mbot,mtop) ;
     else if (verb) fprintf(stderr,"++mri_brainormalize: Eroding, clustering ...\n");
     mask = (byte *) malloc( sizeof(byte)*nxyz ) ;
     for( ii=0 ; ii < nxyz ; ii++ )
       mask[ii] = (sar[ii] > mbot) && (sar[ii] < mtop) ;

     if( verb > 1)
      fprintf(stderr,"++mri_brainormalize: eroding...\n");
   
     THD_mask_erode( nx,ny,nz, mask, 1 ) ;
     if( verb > 1)
      fprintf(stderr,"++mri_brainormalize: clustering 1...\n");
     THD_mask_clust( nx,ny,nz, mask ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) mask[ii] = !mask[ii] ;
     if( verb > 1)
      fprintf(stderr,"++mri_brainormalize: clustering 2...\n");
     THD_mask_clust( nx,ny,nz, mask ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) mask[ii] = !mask[ii] ;

     for( ii=0 ; ii < nxyz ; ii++ ) if( !mask[ii] ) sar[ii] = 0 ;
     free((void *)mask) ;

   Remask_Done:
      if( verb > 1)
      fprintf(stderr,"++mri_brainormalize: Remask Done...\n");
     free((void *)hist) ; free((void *)gist) ;

   }
#if 0
   else
#endif
   {
     /*-- clip top 1% of values that have survived --*/

   
     if( verb > 1)
      fprintf(stderr,"++mri_brainormalize: clipping top...\n");
     else if (verb) fprintf(stderr,"++mri_brainormalize: Clipping extreme values.\n");
     hist = (int *) calloc(sizeof(int),32768) ;
     for( ii=0 ; ii < nxyz ; ii++ ) hist[sar[ii]]++ ;
     for( ii=kk=0 ; ii < 32767 ; ii++ ) kk += hist[ii] ;
     kk = (int)(0.01*kk) ; ktop = 0 ;
     for( jj=0,ii=32767 ; ii > 0 && jj < kk ; ii-- ){
       jj += hist[ii] ; if( hist[ii] > 0 && ktop == 0 ) ktop = ii ;
     }
     jj = ii ;
     if( verb > 1) fprintf(stderr," + 99%% clipping at %d (from %d)\n",jj,ktop) ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( sar[ii] > jj ) sar[ii] = jj ;

     free((void *)hist) ;
   }

   /* distize? */

   if( AFNI_yesenv("DISTIZE") ){
     byte *ccc = (byte *)calloc(sizeof(byte),nxyz);
     short *ddd ;
     int kbot=(int)rint(0.45*nz) , ktop=(int)rint(0.65*nz) ,
         jbot=(int)rint(0.30*ny) , jtop=(int)rint(0.70*ny) ,
         ibot=(int)rint(0.30*nx) , itop=(int)rint(0.70*nx)  ;

     mask = (byte *)malloc( sizeof(byte)*nxyz ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) mask[ii] = (sar[ii] > 0) ;
     for( kk=kbot ; kk <= ktop ; kk++ ){
      for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++ ){
         ijk = ii + jj*nx + kk*nxy ;
         ccc[ijk] = mask[ijk] ;
     }}}
     if( verb ) fprintf(stderr," + distizing\n") ;
     ddd = THD_mask_distize( nx,ny,nz , mask , ccc ) ;
     if( ddd != NULL ){
       int id,jd,kd , ijk , dijk ; float ff ;
       for( ijk=0 ; ijk < nxyz ; ijk++ ){
         if( ddd[ijk] > 0 ){
           ii = ijk % nx ; jj = (ijk%nxy)/nx ; kk = ijk / nxy ;
                if( ii < ibot ) id = ibot-ii ;
           else if( ii > itop ) id = ii-itop ; else id = 0 ;
                if( jj < jbot ) jd = jbot-jj ;
           else if( jj > jtop ) jd = jj-jtop ; else jd = 0 ;
                if( kk < kbot ) kd = kbot-kk ;
           else if( kk > ktop ) kd = kk-ktop ; else kd = 0 ;
           dijk = id+jd+kd+1 ;
           ff = (100.0f * ddd[ijk]) / (float)dijk - 98.9f ;
           if( ff > 255.0f ) ff = 255.0f ;
           sar[ijk] = (short)ff ;
         } else {
           sar[ijk] = 0 ;
         }
       }
       free((void *)ddd) ;
     }
     free((void *)mask); free((void *)ccc);
   }

   /*-- convert output to bytes --*/

   bim = mri_new_conforming( tim , MRI_byte ) ;
   MRI_COPY_AUX(bim,tim) ;
   bar = MRI_BYTE_PTR(bim) ;

   jj = 0 ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( sar[ii] > jj ) jj = sar[ii] ;

   if( jj > 255 ){
     float fac = 255.0 / jj ;
     if( verb ) fprintf(stderr," + scaling by fac=%g\n",fac) ;
     for( ii=0 ; ii < nxyz ; ii++ ) bar[ii] = (byte)(fac*sar[ii]+0.49) ;   /* Note that sar is not scaled down and sar is used in creating
                                                                              imout_orig So the output image is not in the same range as the
                                                                              input. You could fix this by scaling sar by far before setting
                                                                              bar, but why restrict yourself to a smaller data range. You will
                                                                              not regain the exact values before SpatNorm anyway. */
   } else {
     for( ii=0 ; ii < nxyz ; ii++ ) bar[ii] = (byte)sar[ii] ;
   }

   /* create a spat norm of the original volume ZSS */
   if (imout_origp) {
      mri_warp3D_method( MRI_LINEAR) ;
      if (verb > 1) fprintf(stderr,"thd_brainormalize (ZSS):\n n: %d %d %d\n d: %f %f %f\n o: %f %f %f\n ", sim_nx, sim_ny, sim_nz, sim_dx, sim_dy, sim_dz, sim_xo, sim_yo, sim_zo);
      imout_orig = mri_warp3D( tim, sim_nx, sim_ny, sim_nz, ijk_invwarp );
      imout_orig->dx = sim_dx; imout_orig->dy = sim_dy; imout_orig->dz = sim_dz; 
      imout_orig->xo = sim_xo; imout_orig->yo = sim_yo; imout_orig->zo = sim_zo; 
      /* Normally you're done here but because of linear interpolation, zero values (masked in the normalization process) end up non zero with 
      linear interpolation. To set what was zero to zero again, without causing a shift in the final volume's position because of NN interpolation,
      To apply the zero mask again, warp a mask version of the dset and threshold at 0.5 before masking imout_origp (a good tip from Rick) 
      NOTE: tim will be modified*/
      {
         int ii, maxmasked, minmasked, n_masked;
         short *osar, *osar_NN, *tim_NN, maskval = 10000;
         float meanmasked;
         MRI_IMAGE *imout_orig_NN;
         /* create a mask version now */
         mri_warp3D_method( MRI_LINEAR) ;
         if (verb > 1) fprintf(stderr,"thd_brainormalize (ZSS):\n Masking \n ");
         tim_NN = MRI_SHORT_PTR(tim);
         for (ii = 0; ii<nxyz; ++ii) { if (tim_NN[ii]) tim_NN[ii] = maskval; }
         imout_orig_NN = mri_warp3D( tim, sim_nx, sim_ny, sim_nz, ijk_invwarp );
         /* mask values in imout_orig that are less than 0.5 in imout_orig_NN */
         osar_NN = MRI_SHORT_PTR(imout_orig_NN);
         osar    = MRI_SHORT_PTR(imout_orig);
         maxmasked = 0; minmasked = 300000; n_masked = 0;
         meanmasked = 0;
         for (ii = 0; ii<sim_nx *sim_ny* sim_nz; ++ii) { 
            if (osar_NN[ii] < maskval/2) { 
               if (osar[ii]) {
                  if (osar[ii] > maxmasked) maxmasked = osar[ii];
                  else if (osar[ii] < minmasked) minmasked = osar[ii];
                  meanmasked += osar[ii];
                  ++n_masked;
               }
               osar[ii] = 0; 
            } 
         }
         if (n_masked) meanmasked /= n_masked;
         if (verb > 1) fprintf(stderr,"thd_brainormalize (ZSS):\n (N, mean, max, min) of values masked: (%d, %f, %d, %d)\n", 
            n_masked, meanmasked, minmasked, maxmasked);
         mri_free(imout_orig_NN) ;
      }
      *imout_origp = imout_orig;
   }
   
   if (imout_edge) { /* create an edge version NOT EXISTING YET */
      *imout_edge = NULL;
   }
      
   mri_free(tim) ;

   /*-- done!!! --*/

   RETURN(bim) ;
}
