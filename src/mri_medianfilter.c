#include "mrilib.h"

#define SKIPVOX(i) ( mask != NULL && !mask[i] )

static int use_dxyz = 0 ;
void mri_medianfilter_usedxyz( int i ){ use_dxyz = i; }

/*-----------------------------------------------------------------------*/
/*! Compute the median filter of an input image.
    Output image is always in float format.
-------------------------------------------------------------------------*/

MRI_IMAGE *mri_medianfilter( MRI_IMAGE *imin, float irad, byte *mask, int verb )
{
   MRI_IMAGE *imout ;
   float *fin , *fout , *tmp ;
   short *sin ; byte *bin ; void *vin ;
   short *di , *dj , *dk ;
   int nd, ii,jj,kk, ip,jp,kp, nx,ny,nz, nxy, ijk, dd,nt,pjk, kd  ;
   MCW_cluster *cl ;
   float dz ;

ENTRY("mri_medianfilter") ;

   if( imin == NULL || irad <= 0.0f ) RETURN(NULL) ;

   /** if not a good input data type, floatize and try again **/

   if( imin->kind != MRI_float &&
       imin->kind != MRI_short &&
       imin->kind != MRI_byte    ){

     MRI_IMAGE *qim ;
     qim = mri_to_float( imin ) ;
     imout = mri_medianfilter( qim , irad , mask , verb ) ;
     mri_free( qim ) ;
     RETURN(imout) ;
   }

   /** build cluster of points for median-izing **/

   if( use_dxyz ){
     if( irad < 1.01f ) irad = 1.01f ;
     dz = (imin->nz == 1) ? 666.0f : 1.0f ;
     cl = MCW_build_mask( 1.0f,1.0f,dz , irad ) ;
   } else {
     float dm ;
     dz = (imin->nz == 1) ? 666.0f : imin->dz ;
     dm = MIN(imin->dx,imin->dy) ;
     if( imin->nz == 1 ){ dz = 666.0f ; }
     else               { dz = imin->dz ; dm = MIN(dm,dz) ; }
     dm *= 1.01f ; if( irad < dm ) irad = dm ;
     cl = MCW_build_mask( imin->dx,imin->dy,dz , irad ) ;
   }

   if( cl == NULL || cl->num_pt < 6 ){ KILL_CLUSTER(cl); RETURN(NULL); }

   ADDTO_CLUSTER(cl,0,0,0,0) ;

   di = cl->i    ; dj = cl->j    ; dk = cl->k    ; nd  = cl->num_pt ;
   nx = imin->nx ; ny = imin->ny ; nz = imin->nz ; nxy = nx*ny ;

   if( verb ){
     fprintf(stderr,"++ Median mask=%d",nd) ;
     if( mask != NULL )
       fprintf(stderr," Data mask=%d",THD_countmask(nxy*nz,mask)) ;
   }

   imout = mri_new_conforming( imin , MRI_float ) ;
   fout  = MRI_FLOAT_PTR( imout ) ;
   vin   = mri_data_pointer( imin ) ;
   tmp   = (float *) malloc(sizeof(float)*nd) ;
   switch( imin->kind ){
     case MRI_float:  fin = (float *)vin ; break ;
     case MRI_short:  sin = (short *)vin ; break ;
     case MRI_byte :  bin = (byte  *)vin ; break ;
   }

   if( verb ){ kd = (int)rint(0.03*nz); if( kd < 1 ) kd = 1; }

   for( kk=0 ; kk < nz ; kk++ ){
    if( verb && kk%kd == 0 ) fprintf(stderr,".") ;
    for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx ; ii++ ){
        ijk = ii + jj*nx + kk*nxy ;
        if( SKIPVOX(ijk) ){ fout[ijk] = 0.0; continue; }

        /* extract neighborhood values */

        switch( imin->kind ){
          case MRI_float:
            for( nt=dd=0 ; dd < nd ; dd++ ){
              ip = ii+di[dd] ; if( ip < 0 || ip >= nx ) continue ;
              jp = jj+dj[dd] ; if( jp < 0 || jp >= ny ) continue ;
              kp = kk+dk[dd] ; if( kp < 0 || kp >= nz ) continue ;
              pjk = ip+jp*nx+kp*nxy ;
              if( SKIPVOX(pjk) ) continue ;
              tmp[nt++] = fin[pjk] ;
            }
          break ;
          case MRI_short:
            for( nt=dd=0 ; dd < nd ; dd++ ){
              ip = ii+di[dd] ; if( ip < 0 || ip >= nx ) continue ;
              jp = jj+dj[dd] ; if( jp < 0 || jp >= ny ) continue ;
              kp = kk+dk[dd] ; if( kp < 0 || kp >= nz ) continue ;
              pjk = ip+jp*nx+kp*nxy ;
              if( SKIPVOX(pjk) ) continue ;
              tmp[nt++] = sin[pjk] ;
            }
          break ;
          case MRI_byte:
            for( nt=dd=0 ; dd < nd ; dd++ ){
              ip = ii+di[dd] ; if( ip < 0 || ip >= nx ) continue ;
              jp = jj+dj[dd] ; if( jp < 0 || jp >= ny ) continue ;
              kp = kk+dk[dd] ; if( kp < 0 || kp >= nz ) continue ;
              pjk = ip+jp*nx+kp*nxy ;
              if( SKIPVOX(pjk) ) continue ;
              tmp[nt++] = bin[pjk] ;
            }
          break ;
        }

        fout[ijk] = qmed_float( nt , tmp ) ;  /* the actual median-izing */
   }}}
   if( verb ) fprintf(stderr,"\n") ;

   KILL_CLUSTER(cl); free((void *)tmp);  /* toss the trash */
   RETURN(imout) ;
}
