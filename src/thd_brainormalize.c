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

/*======================================================================*/

/*----------------------------------------------------------------------
   (a) shortize input and flip brick so that orientation is RAI
   (b) find clip level and create a binary mask
   (c) find S-most slice that has at least 10% above clip level;
       zero out mask above that slice and also more than 160 mm below
   (d) apply mask to image volume
   (e) set CM of surviving voxels to (0,20,0)
   (f) resample to master dataset grid
------------------------------------------------------------------------*/

MRI_IMAGE * mri_brainormalize( MRI_IMAGE *im, int xxor, int yyor, int zzor )
{
   MRI_IMAGE *sim , *tim ;
   short *sar , sval ;
   int ii,jj,kk , nx,ny,nz,nxy,nxyz , joff,koff ;
   float val , xcm,ycm,zcm,sum ;
   byte *mask , *mmm ;

ENTRY("mri_brainormalize") ;

   if( im == NULL || xxor < 0 || xxor > LAST_ORIENT_TYPE ||
                     yyor < 0 || yyor > LAST_ORIENT_TYPE ||
                     zzor < 0 || zzor > LAST_ORIENT_TYPE   ) RETURN(NULL) ;

   if( im->nx < 16 || im->ny < 16 || im->nz < 16 ) RETURN(NULL) ;

   val = mri_maxabs(im) ; if( val <= 0.0 ) RETURN(NULL) ;

   /* make a short copy */

   if( im->kind == MRI_short || im->kind == MRI_byte )
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

   if( ii==1 && jj==2 && kk==3 ){
     sim = tim ;
   } else {
     sim = mri_flip3D( ii,jj,kk , tim ) ;
     mri_free(tim) ;
     if( sim == NULL ) RETURN(NULL) ;
   }

   sar = MRI_SHORT_PTR(sim) ;
   if( sar == NULL ){ mri_free(sim); RETURN(NULL); }

   /* get clip level and make a binary mask */

   sval = (short) THD_cliplevel( sim , 0.333 ) ;
   if( sval <= 0 ){ mri_free(sim); RETURN(NULL); }

   nx = sim->nx ; ny = sim->ny ; nz = sim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   mask = (byte *) malloc( sizeof(byte)*nxyz ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mask[ii] = (sar[ii] >= sval) ;

   /* descend from top, searching for a slice with a large blob */

   mmm = (byte *) malloc( sizeof(byte)*nxy ) ;
   jj  = 0.1*nxy ;
   for( kk=nz-1 ; kk > 0 ; kk-- ){
     memcpy( mmm , mask + kk*nxy , sizeof(byte)*nxy ) ;
     if( bigclustsize2D(nx,ny,mmm) > jj ) break ;
   }
   free(mmm) ;
   if( kk == 0 ){ free(mask); mri_free(sim); RETURN(NULL); }

   /* zero out all above that slice */

   if( kk < nz-1 )
     memset( mask+(kk+1)*nxy , 0 , nxy*(nz-1-kk)*sizeof(byte) ) ;

   /* find slice index 160 mm below that slice */

   val = fabs(sim->dz) ; if( val == 0.0 ) val = 1.0 ;
   jj  = (int)rint( kk-160.0/val ) ;
   if( jj >= 0 )
     memset( mask , 0 , nxy*(jj+1)*sizeof(byte) ) ;

   /* apply mask to image */

   for( ii=0 ; ii < nxyz ; ii++ )
     if( mask[ii] == 0 ) sar[ii] = 0 ;
   free(mask) ;

   /* compute CM of masked image */

   xcm = ycm = zcm = sum = 0.0 ;
   for( kk=0 ; kk < nz ; kk++ ){
     koff = kk*nxy ;
     for( jj=0 ; jj < ny ; jj++ ){
       joff = koff + jj*nx ;
       for( ii=0 ; ii < nx ; ii++ ){
         val = (float)abs(sar[ii+joff]) ;
         sum += val ;
         xcm += val * ii ;
         ycm += val * jj ;
         zcm += val * kk ;
       }
     }
   }
   if( sum == 0.0 ){ mri_free(sim); RETURN(NULL); }
   xcm = (xcm/sum) ;
}
