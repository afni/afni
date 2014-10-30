#include "mrilib.h"

#define IAR(i,j) innar[(i)+(j)*nx]
#define OAR(i,j) outar[(i)+(j)*nx]

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_median21( MRI_IMAGE *innim )
{
   float *innar , *outar , qar[21] ;
   MRI_IMAGE *outim ;
   int ii,ip,iq,im,in , jj,jp,jq,jm,jn , kk , nx,ny ;

   if( innim == NULL || innim->kind != MRI_float ) return NULL ;

   innar = MRI_FLOAT_PTR(innim) ; nx = innim->nx ; ny = innim->ny ;
   outim = mri_new_conforming( innim , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;

   for( jj=0 ; jj < ny ; jj++ ){
     jm = jj-1 ; if( jm <  0  ) jm++ ;
     jn = jm-1 ; if( jn <  0  ) jn++ ;
     jp = jj+1 ; if( jp >= ny ) jp-- ;
     jq = jp+1 ; if( jq >= ny ) jq-- ;
     for( ii=0 ; ii < nx ; ii++ ){
       im = ii-1 ; if( im <  0  ) im++ ;
       in = im-1 ; if( in <  0  ) in++ ;
       ip = ii+1 ; if( ip >= nx ) ip-- ;
       iq = ip+1 ; if( iq >= nx ) iq-- ;

       kk = 0 ;
       qar[kk++] = IAR(im,jn); qar[kk++] = IAR(ii,jn); qar[kk++] = IAR(ip,jn);
       qar[kk++] = IAR(in,jm); qar[kk++] = IAR(im,jm); qar[kk++] = IAR(ii,jm); qar[kk++] = IAR(ip,jm); qar[kk++] = IAR(iq,jm);
       qar[kk++] = IAR(in,jj); qar[kk++] = IAR(im,jj); qar[kk++] = IAR(ii,jj); qar[kk++] = IAR(ip,jj); qar[kk++] = IAR(iq,jj);
       qar[kk++] = IAR(in,jp); qar[kk++] = IAR(im,jp); qar[kk++] = IAR(ii,jp); qar[kk++] = IAR(ip,jp); qar[kk++] = IAR(iq,jp);
       qar[kk++] = IAR(im,jq); qar[kk++] = IAR(ii,jq); qar[kk++] = IAR(ip,jq);

       OAR(ii,jj) = qmed_float(21,qar) ;
     }
   }

   return outim ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_sharpness( MRI_IMAGE *inim )
{
   int nx,ny , ii,ip,im , jj,jp,jm ;
   float *innar , *outar , avv,qvv ;
   MRI_IMAGE *outim , *innim ;

   if( inim == NULL ) return NULL ;

   innim = mri_to_float(inim) ;
   innar = MRI_FLOAT_PTR(innim) ; nx = innim->nx ; ny = innim->ny ;

   outim = mri_new_conforming( innim , MRI_float ) ; /* all zero */
   outar = MRI_FLOAT_PTR(outim) ;

   { float *qar = (float *)malloc(sizeof(float)*nx*ny) ; int nq=0 ;
     for( ii=0 ; ii < nx*ny ; ii++ )
       if( innar[ii] != 0.0f ) qar[nq++] = fabsf(innar[ii]) ;
     if( nq < 32 ) qvv = 0.0f ;
     else          qvv = 0.18f * qmed_float(nq,qar) ;
     free(qar) ;
   }
   if( qvv == 0.0f ){ mri_free(innim) ; return outim ; }  /* input is all 0! */

   for( jj=0 ; jj < ny ; jj++ ){
     jm = jj-1 ; if( jm <  0  ) jm++ ;
     jp = jj+1 ; if( jp >= ny ) jp-- ;
     for( ii=0 ; ii < nx ; ii++ ){
       im = ii-1 ; if( im <  0  ) im++ ;
       ip = ii+1 ; if( ip >= nx ) ip-- ;
       avv =  fabsf(IAR(im,jm)) + fabsf(IAR(im,jp)) + fabsf(IAR(ip,jm)) + fabsf(IAR(ip,jp))
            + fabsf(IAR(ii,jm)) + fabsf(IAR(ii,jp)) + fabsf(IAR(im,jj)) + fabsf(IAR(ip,jj))
            + fabsf(IAR(ii,jj)) ;
       if( avv < qvv ) avv = qvv ;
       OAR(ii,jj) = fabsf(            IAR(im,jm) + IAR(im,jp) + IAR(ip,jm) + IAR(ip,jp)
                          +  4.0f * ( IAR(ii,jm) + IAR(ii,jp) + IAR(im,jj) + IAR(ip,jj) )
                          - 20.0f * IAR(ii,jj) ) / avv ;
      }
    }

    innim = mri_median21(outim) ;
    mri_free(outim) ; return innim ;
}
