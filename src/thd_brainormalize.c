#include "mrilib.h"

#undef INLINE
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

static int verb = 0 ;
void mri_brainormalize_verbose( int v ){ verb = v ; }

/*---------------------------------------------------------------------*/
/*! Count number of nonzeros in mask array */

static int mask_count( int nvox , byte *mmm )
{
   int ii , nn ;
   for( nn=ii=0 ; ii < nvox ; ii++ ) nn += (mmm[ii] != 0) ;
   return nn ;
}

/*------------------------------------------------------------------*/

#undef  DALL
#define DALL 4096  /* Allocation size for cluster arrays */

/*--------------------------------------------------------------------------*/
/*! Put (i,j,k) into the current cluster, if it is nonzero. */

#undef  CPUT
#define CPUT(i,j,k)                                                   \
  do{ ijk = (i)+(j)*nx+(k)*nxy ;                                      \
      if( mmm[ijk] ){                                                 \
        if( nnow == nall ){ /* increase array lengths */              \
          nall += DALL ;                                              \
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

   if( verb ) fprintf(stderr,"++ clustedit3D: threshold size=%d voxels\n",csize) ;

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
       if( verb )
         fprintf(stderr,"++ clustedit3D: saved cluster with %d voxels\n",nnow);
     } else {
       nkill += nnow ;
     }

   } /* loop ends when all nonzero points are clustered */

   free((void *)inow); free((void *)jnow); free((void *)know);

   /* copy saved points back into mmm */

   for( ii=0 ; ii < nsav ; ii++ )
     mmm[ IJK(isav[ii],jsav[ii],ksav[ii]) ] = 1 ;

   free((void *)isav); free((void *)jsav); free((void *)ksav) ;

   if( verb )
     fprintf(stderr,"++ clustedit3D totals:"
                    " saved=%d killed=%d nxyz=%d\n",nsav,nkill,nxyz) ;
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
} clipvec ;

/*! Get the cliplevel for each octant about the center-of-mass. */

static clipvec get_octant_clips( MRI_IMAGE *im , float mfrac )
{
   float xcm,ycm,zcm , sum,val ;
   int ii,jj,kk , nx,ny,nz,nxy , ic,jc,kc , it,jt,kt , ijk ;
   short *sar ;
   clipvec cv ;

ENTRY("get_octant_clips") ;

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

   /* (x0,y0,z0) = center of lowest octant
      (x1,y1,z1) = center of highest octant */

   cv.x0  = 0.5*ic ; cv.x1 = 0.5*(ic+it) ;
   cv.y0  = 0.5*jc ; cv.y1 = 0.5*(jc+jt) ;
   cv.z0  = 0.5*kc ; cv.z1 = 0.5*(kc+kt) ;
   cv.dxi = (cv.x1 > cv.x0) ? 1.0/(cv.x1-cv.x0) : 0.0 ;
   cv.dyi = (cv.y1 > cv.y0) ? 1.0/(cv.y1-cv.y0) : 0.0 ;
   cv.dzi = (cv.z1 > cv.z0) ? 1.0/(cv.z1-cv.z0) : 0.0 ;

   if( verb )
    fprintf(stderr,"++ get_octant_clips:\n"
                   "    min clip=%.1f\n"
                   "    clip_000=%.1f\n"
                   "    clip_100=%.1f\n"
                   "    clip_010=%.1f\n"
                   "    clip_110=%.1f\n"
                   "    clip_001=%.1f\n"
                   "    clip_101=%.1f\n"
                   "    clip_011=%.1f\n"
                   "    clip_111=%.1f\n"
                   "    (x0,y0,z0) = (%.1f,%.1f,%.1f)\n"
                   "    (x1,y1,z1) = (%.1f,%.1f,%.1f)\n" ,
            val ,
            cv.clip_000 , cv.clip_100 , cv.clip_010 , cv.clip_110 ,
            cv.clip_001 , cv.clip_101 , cv.clip_011 , cv.clip_111 ,
            cv.x0 , cv.y0 , cv.z0 , cv.x1 , cv.y1 , cv.z1  ) ;

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
   clipvec cvec ;
   short *sar ;
   byte *mask ;
   float cval ;

ENTRY("mri_short2mask") ;
   if( im == NULL || im->kind != MRI_short ) RETURN(NULL) ;
   sar = MRI_SHORT_PTR(im) ; if( sar == NULL ) RETURN(NULL) ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   cvec = get_octant_clips( im , 0.40 ) ;
   if( cvec.clip_000 < 0.0 ) RETURN(NULL) ;

   /* create mask, clipping at a level that varies spatially */

   if( verb ) fprintf(stderr,"++ mri_short2mask: clipping\n") ;

   mask = (byte *) malloc( sizeof(byte)*nxyz ) ;
   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,ijk++ ){
       cval = pointclip( ii,jj,kk , &cvec ) ; /* cliplevel here */
       mask[ijk] = (sar[ijk] >= cval) ;       /* binarize */
   }}}

   if( verb ) fprintf(stderr,"++ mri_short2mask: %d voxels above clip\n",
                             mask_count(nxyz,mask) ) ;

   /* remove small clusters */

   clustedit3D( nx,ny,nz , mask , (int)rint(0.02*nxyz) ) ;

   /* fill in any isolated holes in mask */

   if( verb ) fprintf(stderr,"++ mri_short2mask: filling in holes\n") ;

#if 1
   (void) THD_mask_fillin_once( nx,ny,nz , mask , 2 ) ;
          THD_mask_dilate     ( nx,ny,nz , mask , 5 ) ;
          THD_mask_dilate     ( nx,ny,nz , mask , 5 ) ;
#else
   for( kk=1 ; kk < nz-1 ; kk++ ){
    for( jj=1 ; jj < ny-1 ; jj++ ){
     for( ii=1 ; ii < nx-1 ; ii++ ){
       if( !mask[IJK(ii,jj,kk)  ] &&
            mask[IJK(ii+1,jj,kk)] &&  mask[IJK(ii-1,jj,kk)] &&
            mask[IJK(ii,jj+1,kk)] &&  mask[IJK(ii,jj-1,kk)] &&
            mask[IJK(ii,jj,kk+1)] &&  mask[IJK(ii,jj,kk-1)]   )
         mask[IJK(ii,jj,kk)] = 1 ;
   }}}
#endif

   RETURN(mask) ;
}

/*--------------------------------------------------------------------------*/

typedef struct { short i,j,k,val,basin; } shortvox ;

/*--------------------------------------------------------------------------*/

static void isort_shortvox( int n , shortvox *ar )
{
   register int  j , p ;    /* array indices */
   register shortvox temp ; /* a[j] holding place */
   register shortvox *a = ar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j].val > a[j-1].val ){   /* out of order */
        p    = j ;
        temp = a[j] ;

        do{
           a[p] = a[p-1] ; /* at this point, a[p-1] < temp, so move it up */
          p-- ;
        } while( p > 0 && temp.val > a[p-1].val ) ;

        a[p] = temp ;       /* finally, put temp in its place */
     }
   }
}

/********************************************************************************/
/* qsrec : recursive part of quicksort (stack implementation)                   */

#define QS_STACK  1024  /* stack size */
#define QS_SWAPF(x,y) ( temp=(x),(x)=(y),(y)= temp)
#define QS_SWAPI(i,j) (itemp=(i),(i)=(j),(j)=itemp)

static void qsrec_doubleint( int n , shortvox *ar , int cutoff )
{
   int i , j ;             /* scanning indices */
   shortvox *a = ar ;
   shortvox temp , pivot ; /* holding places */

   int left, right, mst, stack[QS_STACK], nnew , itemp ;

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff ) return ;

   /* initialize stack to start with whole array */

   stack[0] = 0   ;
   stack[1] = n-1 ;
   mst      = 2   ;

   /* loop while the stack is nonempty */

   while( mst > 0 ){
      right = stack[--mst] ;  /* work on subarray from left -> right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;           /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( a[left].val < a[i].val     ){ QS_SWAPF(a[left] ,a[i]    ); }
      if( a[left].val < a[right].val ){ QS_SWAPF(a[left] ,a[right]); }
      if( a[i].val    < a[right].val ){ QS_SWAPF(a[right],a[i]    ); }

      pivot  = a[i] ;                      /* a[i] is the median-of-3 pivot! */
      a[i]   = a[right] ;

      i = left ;                           /* initialize scanning */
      j = right ;

      /*----- partition: move elements bigger than pivot up and elements
                         smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i].val > pivot.val ; ) ; /* scan i up,   until a[i] <= pivot */
        for( ; a[--j].val < pivot.val ; ) ; /* scan j down, until a[j] >= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAPF( a[i] , a[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right]  = a[i] ;             /* restore the pivot */
      a[i]      = pivot ;

      /*----- push subarrays [left..i-1] and [i+1..right] onto stack, if big -----*/

      nnew = 0 ;
      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; nnew++ ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; nnew++ ; }

      /* if just added two subarrays to stack, make sure shorter one comes first */

      if( nnew == 2 && stack[mst-3] - stack[mst-4] > stack[mst-1] - stack[mst-2] ){
        QS_SWAPI( stack[mst-4] , stack[mst-2] ) ;
        QS_SWAPI( stack[mst-3] , stack[mst-1] ) ;
      }

   }  /* end of while stack is non-empty */
}

#ifndef QS_CUTOFF
#define QS_CUTOFF 10
#endif

static void qsort_shortvox( int n , shortvox *a )
{
   qsrec_doubleint( n , a , QS_CUTOFF ) ;
   isort_doubleint( n , a ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

#undef  DBALL
#define DBALL 1024

static MRI_IMAGE * watershedize( MRI_IMAGE *sim )
{
   int ii,jj,kk , pp,qq , nx,ny,nz,nxy,nxyz , nvox ;
   int ip,jp,kp , im,jm,km ;
   short *sar ;
   shortvox *svox ;
   int *isvox ;
   int *basin , nbasin , nball ;
   int nb,vb,mb,m , bp[6] , vp[6] ;

ENTRY("watershedize") ;

   if( sim == NULL || sim->kind != MRI_short ) RETURN(NULL) ;
   sar = MRI_SHORT_PTR(sim) ; if( sar == NULL ) RETURN(NULL) ;

   nx = sim->nx; ny = sim->ny; nz = sim->nz; nxy = nx*ny; nxyz = nxy*nz;

   /* count number of voxels > 0 */

   for( nvox=0,pp=0 ; pp < nxyz ; pp++ ) if( sar[pp] > 0 ) nvox++ ;
   if( nvox <= 999 ) RETURN(NULL) ;

   /* create voxel lists */

   svox  = (shortvox *) malloc( sizeof(shortvox)* nvox ) ;
   isvox = (int *)      malloc( sizeof(int)     * nxyz ) ;
   for( qq=pp=0 ; pp < nxyz ; pp++ ){
     if( sar[pp] > 0 ){
       ii             = pp % nx ;
       jj             = (pp%nxy) / nx ;
       kk             = pp / nxy ;
       isvox[pp]      = qq ;
       svox[qq].i     = ii ;
       svox[qq].j     = jj ;
       svox[qq].k     = kk ;
       svox[qq].val   = sar[pp] ;
       svox[qq].basin = -1 ;
     } else {
       isvox[pp] = -1 ;
     }
   }

   /* sort voxel list into descending order */

   qsort_shortvox( nvox , svox ) ;

   nbasin   = 1 ;
   nball    = DBALL ;
   basin    = (int *)malloc(sizeof(int)*nball) ;
   basin[0] = svox[0].val ;

   for( pp=1 ; pp < nvox ; pp++ ){
     ii = svox[pp].i; jj = svox[pp].j; kk = svox[pp].k;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     im = ii-1 ; jm = jj-1 ; km = kk-1 ;

#undef  BASECHECK
#define BASECHECK(a,b,c)                                   \
  do{ qq = isvox[IJK(a,b,c)] ;                             \
      if( qq >= 0 && svox[qq].basin >= 0 ){                \
        bp[nb] = svox[qq].basin ; vp[nb] = basin[bp[nb]] ; \
        if( vp[nb] > vb ){ mb = nb ; vb = vp[nb] ; }       \
        nb++ ;                                             \
      }                                                    \
  } while(0)

     nb = 0 ; vb = -1 ; mb = -1 ;
     if( ip < nx ) BASECHECK(ip,jj,kk) ;
     if( im >= 0 ) BASECHECK(im,jj,kk) ;
     if( jp < ny ) BASECHECK(ii,jp,kk) ;
     if( jm >= 0 ) BASECHECK(ii,jm,kk) ;
     if( kp < nz ) BASECHECK(ii,jj,kp) ;
     if( km >= 0 ) BASECHECK(ii,jj,km) ;

     /* at this point, nb = # of neighbors already in basins
                       bp[m] = list of these basins, m=0..nb-1
                       vp[m] = peak values of these basins
                       mb    = m with vp[m] largest ('best')
                       vb    = value of best basin
     */

     if( nb == 0 ){  /* this voxel is isolated ==> create new basin */

       if( nbasin == nball ){
         nball += DBALL ; basin = (int *)realloc((void *)basin,nball) ;
       }
       basin[nbasin] = svox[pp].val ;
       svox[pp].basin = nbasin++ ;

     } else {        /* this voxel has larger neighbors */

       svox[pp].basin = bp[mb] ; /* the best basin found above */

     }
   }
}

/*======================================================================*/

#define CMTOP

#define XCM    0.0   /* center of mass of output dataset goes here */
#define YCM   20.0

#ifdef CMTOP
# define ZCM  20.0
#else
# define ZCM   0.0
#endif

#define XORG -83.0   /* the box for the master dataset grid */
#define YORG -89.0
#define ZORG -82.0
#define NX   167
#define NY   212
#define NZ   175
#define DXYZ   1.0

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

/*----------------------------------------------------------------------
   (a) shortize input and flip brick so that orientation is RAI
   (b) find clip levels and create a binary mask
   (c) find S-most slice that has at least 10% above clip level;
       zero out mask above that slice and also more than 160 mm below
   (d) apply mask to image volume
   (e) resample to master dataset grid, with CM at (0,20,0)
------------------------------------------------------------------------*/

MRI_IMAGE * mri_brainormalize( MRI_IMAGE *im, int xxor, int yyor, int zzor )
{
   MRI_IMAGE *sim , *tim ;
   short *sar , sval ;
   int ii,jj,kk,ijk,ktop,kbot , nx,ny,nz,nxy,nxyz ;
   float val , icm,jcm,kcm,sum , dx,dy,dz ;
   byte *mask ;
   int *zcount , z1,z2,z3 ;

ENTRY("mri_brainormalize") ;

   if( im == NULL || xxor < 0 || xxor > LAST_ORIENT_TYPE ||
                     yyor < 0 || yyor > LAST_ORIENT_TYPE ||
                     zzor < 0 || zzor > LAST_ORIENT_TYPE   ) RETURN(NULL) ;

   if( im->nx < 16 || im->ny < 16 || im->nz < 16 ) RETURN(NULL) ;

   val = mri_maxabs(im) ; if( val <= 0.0 ) RETURN(NULL) ;

   /* make a short copy */

   if( verb ) fprintf(stderr,"++ mri_brainormalize: copying input\n") ;

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
     if( verb )
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

   if( verb ) fprintf(stderr,"++mri_brainormalize: making mask\n") ;
   mask = mri_short2mask( sim ) ;

   if( mask == NULL ){ mri_free(sim); RETURN(NULL); }

   kk = mask_count(nxyz,mask) ;
   if( verb )
     fprintf(stderr,"++mri_brainormalize: mask now has %d voxels\n",kk) ;

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

   if( verb ){
     fprintf(stderr,"++mri_brainormalize: zcount from top slice #%d\n",nz-1) ;
     for( kk=nz-1 ; kk >= 0 ; kk-- ){
       fprintf(stderr," %.3f",((double)(zcount[kk]))/((double)nxy) ) ;
       if( (nz-kk)%10 == 0 && kk > 0 ) fprintf(stderr,"\n") ;
     }
     fprintf(stderr,"\n") ;
   }

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

   /* find slice index 165 mm below that slice */

   jj = (int)rint( ktop-165.0/dz ) ;
   if( jj >= 0 ){
     if( verb )
       fprintf(stderr,"++mri_brainormalize: bot clip below slice %d\n",jj) ;
     memset( mask , 0 , nxy*(jj+1)*sizeof(byte) ) ;
   }

   kk = mask_count(nxyz,mask) ;
   if( kk <= 999 ){ free((void *)mask); mri_free(sim); RETURN(NULL); }

   /* apply mask to image (will also remove any negative values) */

   if( verb )
     fprintf(stderr,"++mri_brainormalize: applying mask to image; %d\n",kk) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( !mask[ii] ) sar[ii] = 0 ;
   free((void *)mask) ;

   /* compute CM of masked image (indexes, not mm) */

   icm = jcm = kcm = sum = 0.0 ;
#ifndef CMTOP
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

   ai = DXYZ/dx ; bi = icm/sum - ai*(XCM-XORG)/DXYZ ;
   aj = DXYZ/dy ; bj = jcm/sum - aj*(YCM-YORG)/DXYZ ;
   ak = DXYZ/dz ; bk = kcm/sum - ak*(ZCM-ZORG)/DXYZ ;

   if( verb ) fprintf(stderr,"++mri_brainormalize: warping to standard grid\n") ;

   mri_warp3D_method( MRI_CUBIC ) ;
   tim = mri_warp3D( sim , NX,NY,NZ , ijkwarp ) ;
   mri_free(sim) ;

   tim->dx = tim->dy = tim->dz = DXYZ ;
   tim->xo = XORG ;
   tim->yo = YORG ;
   tim->zo = ZORG ;
   RETURN(tim) ;
}
