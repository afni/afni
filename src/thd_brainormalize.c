#include "mrilib.h"

static int verb = 0 ;
void mri_brainormalize_verbose( int v ){ verb = v ; }

/*---------------------------------------------------------------------*/

static int mask_count( int nvox , byte *mmm )
{
   int ii , nn ;
   for( nn=ii=0 ; ii < nvox ; ii++ ) nn += (mmm[ii] != 0) ;
   return nn ;
}

/*------------------------------------------------------------------*/

# define DALL 1024  /* Allocation size for cluster arrays */

/*! Put (i,j) into the current cluster, if it is nonzero. */

# define CPUT(i,j)                                              \
  do{ ijk = (i)+(j)*nx ;                                        \
      if( mmm[ijk] ){                                           \
        if( nnow == nall ){ /* increase array lengths */        \
          nall += DALL ;                                        \
          inow = (short *) realloc(inow,sizeof(short)*nall) ;   \
          jnow = (short *) realloc(jnow,sizeof(short)*nall) ;   \
        }                                                       \
        inow[nnow] = (i) ; jnow[nnow] = (j) ;                   \
        nnow++ ; mmm[ijk] = 0 ;                                 \
      } } while(0)

/*------------------------------------------------------------------*/
/*! Count the biggest cluster of nonzeros in the 2D array.
    Array will be zeroed out in the process.
--------------------------------------------------------------------*/

static int bigclustsize2D( int nx , int ny , byte *mmm )
{
   int ii,jj, icl ,  nxy , ijk , ijk_last ;
   int ip,jp , im,jm ;
   int nbest , nnow , nall ;
   short *inow , *jnow  ;

   if( nx < 1 || ny < 1 || mmm == NULL ) return 0 ;

   nxy = nx*ny ;

   nbest = 0 ;
   nall  = 8 ;                                    /* # allocated pts */
   inow  = (short *) malloc(sizeof(short)*nall) ; /* coords of pts */
   jnow  = (short *) malloc(sizeof(short)*nall) ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

   ijk_last = 0 ;
   while(1) {
     /* find next nonzero point */

     for( ijk=ijk_last ; ijk < nxy ; ijk++ ) if( mmm[ijk] ) break ;
     if( ijk == nxy ) break ;  /* didn't find any! */

     ijk_last = ijk+1 ;         /* start here next time */

     /* init current cluster list with this point */

     mmm[ijk] = 0 ;                                /* clear found point */
     nnow     = 1 ;                                /* # pts in cluster */
     inow[0]  = ijk % nx ;
     jnow[0]  = ijk / nx ;

     /*--
        for each point in cluster:
           check neighboring points for nonzero entries in mmm
           enter those into cluster (and clear them in mmm)
           continue until end of cluster is reached
           (note that cluster size nnow is expanding as we progress)
     --*/

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ;
       im = ii-1      ; jm = jj-1      ;
       ip = ii+1      ; jp = jj+1      ;

       if( im >= 0 ) CPUT(im,jj) ;
       if( ip < nx ) CPUT(ip,jj) ;
       if( jm >= 0 ) CPUT(ii,jm) ;
       if( jp < ny ) CPUT(ii,jp) ;
     }

     /* see if now cluster is larger than best yet */

     if( nnow > nbest ) nbest = nnow ;

   } /* loop ends when all nonzero points are clustered */

   free(inow) ; free(inow) ; return nbest ;
}
