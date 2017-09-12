/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

#define CFRAC      0.20
#define LIM(x,b,t) ((x<b) ? b : (x>t) ? t : x)

void qsort_sh( int n , short * a ) ;  /* at end of file */

/*-------- 06 Jul 2000 - RWCox ----------------------------------------*/

THD_3dim_dataset * WINsorize( THD_3dim_dataset *inset ,
                              int nrep , int cbot , int ctop ,
                              float irad , char *prefix ,
                              int keep_zero , int clipval , byte *mask )
{
   THD_3dim_dataset *outset ;
   short *shin , *shout , *di,*dj,*dk , *tmp , val,nval ;
   MCW_cluster *cl ;
   int jj,kk , krep,kdiff, nx,ny,nz,nxy,nxyz , nd,dd ;
   int ip,jp,kp , nx1,ny1,nz1 , verb=1 ;
   int nrep_until ;
   register int ii,ijk ;

   /*- check inputs -*/

   if( inset == NULL || DSET_BRICK_TYPE(inset,0) != MRI_short ) return NULL ;
   DSET_load(inset) ;
   if( DSET_BRICK_ARRAY(inset,0) == NULL ) return NULL ;

   if( nrep == 0 ) return NULL ;

   if( nrep < 0 ){ nrep_until = abs(nrep) ; nrep = 999 ; }
   else          { nrep_until = 2 ; }

   if( irad < 0.0 ){ verb=0 ; irad = -irad ; }

   if( irad < 1.01 ) irad = 1.01 ;
   if( !THD_filename_ok(prefix) ) prefix = "Winsor" ;

   /*- build list of points to use -*/

   cl = MCW_build_mask( 1.0,1.0,1.0 , irad ) ;

   if( cl == NULL || cl->num_pt < 6 ){ KILL_CLUSTER(cl); return NULL; }

   ADDTO_CLUSTER(cl,0,0,0,0) ;

   di = cl->i ; dj = cl->j ; dk = cl->k ; nd = cl->num_pt ;

   if( verb ) fprintf(stderr,"+++ WINsorize irad=%f nbhd=%d\n",irad,nd) ;

   /*- make output array -*/

   nx = DSET_NX(inset) ; nx1 = nx-1 ;
   ny = DSET_NY(inset) ; ny1 = ny-1 ;
   nz = DSET_NZ(inset) ; nz1 = nz-1 ; nxy = nx*ny ; nxyz = nxy*nz ;

   shout = (short *) malloc(sizeof(short)*nxyz) ;
   tmp   = (short *) malloc(sizeof(short)*nd) ;

   if( nrep > 1 ){
      shin  = (short *) malloc(sizeof(short)*nxyz) ;
      memcpy( shin , DSET_BRICK_ARRAY(inset,0) , sizeof(short)*nxyz ) ;
   } else {
      shin = DSET_BRICK_ARRAY(inset,0) ;
   }

   if( cbot <= 0 || cbot >= nd-1 ){
      cbot = rint( CFRAC*nd ) ;
      if( cbot <= 0 ) cbot = 1 ;
      if( verb ) fprintf(stderr,"+++ WINsorize cbot=%d\n",cbot) ;
   }
   if( ctop <= cbot || cbot >= nd-1 ){
      ctop = nd-1-cbot ;
      if( verb ) fprintf(stderr,"+++ WINsorize ctop=%d\n",ctop) ;
   }

   /*- do the work -*/

   for( krep=0 ; krep < nrep ; krep++ ){

      kdiff = 0 ;  /* count of how many voxels were changed */

      for( kk=0 ; kk < nz ; kk++ ){        /* loops over 3D voxel indices */
         for( jj=0 ; jj < ny ; jj++ ){
            ijk = jj*nx+kk*nxy ;
            for( ii=0 ; ii < nx ; ii++,ijk++ ){

               if( mask != NULL && !mask[ijk] ){ shout[ijk]=shin[ijk]; continue; }

               val = shin[ijk] ;                        /* current voxel */

               if( clipval > 0 && val <= clipval )      /* 19 Oct 2001 */
                  val = shout[ijk] = 0 ;

               if( keep_zero && val == 0 ) continue ;   /* don't filter 0 */

               for( dd=0 ; dd < nd ; dd++ ){            /* loop over nbhd */
                  ip = ii+di[dd] ; ip = LIM(ip,0,nx1) ;
                  jp = jj+dj[dd] ; jp = LIM(jp,0,ny1) ;
                  kp = kk+dk[dd] ; kp = LIM(kp,0,nz1) ;
                  tmp[dd] = shin[ip+jp*nx+kp*nxy] ;
               }

               qsort_sh( nd , tmp ) ;

               shout[ijk] = nval = LIM(val,tmp[cbot],tmp[ctop]) ;

               if( nval != val ) kdiff++ ;
            }
         }
      }

      /* prepare for next iteration */

      if( verb ) fprintf(stderr,"+++ WINsorize iter%2d: # changed=%d\n",krep+1,kdiff) ;

      if( kdiff < nrep_until ) break ;

      if( krep < nrep-1 )
         memcpy( shin , shout , sizeof(short)*nxyz ) ;
   }

   /*- toss the trashola */

   KILL_CLUSTER(cl) ;
   free(tmp) ;
   if( shin != DSET_BRICK_ARRAY(inset,0) ) free(shin) ;

   /*- make output dataset */

   outset = EDIT_empty_copy( inset )  ;

   EDIT_dset_items( outset ,
                       ADN_prefix , prefix ,
                       ADN_nvals  , 1 ,
                       ADN_ntt    , 0 ,
                    ADN_none ) ;

   EDIT_substitute_brick( outset , 0 , MRI_short , shout ) ;

   return outset ;
}

/*****************************************************************************/

void isort_sh( int n , short * ar )
{
   register int  j , p ;        /* array indices */
   register short temp ;        /* a[j] holding place */
   register short * a = ar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j] < a[j-1] ){       /* out of order */
        p    = j ;
        temp = a[j] ;

        do{
           a[p] = a[p-1] ;      /* now have a[p-1] > temp, so move it up */
           p-- ;
        } while( p > 0 && temp < a[p-1] ) ;

        a[p] = temp ;           /* finally, put temp in its place */
     }
   }
}


/*****************************************************************************/
/* qsrec : recursive part of quicksort (stack implementation) */

#define QS_SWAP(x,y) (temp=(x),(x)=(y),(y)=temp)
#ifndef QS_STACK
# define QS_STACK 9999
#endif

static void qsrec_sh( int n , short * ar , int cutoff )
{
   register int i , j ;           /* scanning indices */
   register short temp , pivot ;  /* holding places */
   register short * a = ar ;

   int left , right , mst , stack[QS_STACK] ;

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff ) return ;

   /* initialize stack to start with whole array */

   stack[0] = 0   ;
   stack[1] = n-1 ;
   mst      = 2   ;

   /* loop while the stack is nonempty */

   while( mst > 0 ){
      right = stack[--mst] ;         /* subarray from left -> right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;     /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( a[left] > a[i]     ) QS_SWAP( a[left]  , a[i]     ) ;
      if( a[left] > a[right] ) QS_SWAP( a[left]  , a[right] ) ;
      if( a[i] > a[right]    ) QS_SWAP( a[right] , a[i]     ) ;

      pivot = a[i] ;                 /* a[i] is the median-of-3 pivot! */
      a[i]  = a[right] ;

      i = left ;                     /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAP( a[i] , a[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right] = a[i] ;              /* restore the pivot */
      a[i]     = pivot ;

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

   }  /* end of while stack is non-empty */
}

/******************************************************************************/
/* quick_sort :  sort an array partially recursively, and partially insertion */

#ifndef QS_CUTOFF
#define QS_CUTOFF 40
#endif

void qsort_sh( int n , short * a )
{
   if( n < QS_CUTOFF )
      qsrec_sh( n , a , QS_CUTOFF ) ;

   isort_sh( n , a ) ;
}
