#include "mrilib.h"

/*--------------------------------------------------------------------------*/

#define WT0 0.3
#define WT1 0.2
#define WT2 0.15

void MRI_5blur_inplace_3D( MRI_IMAGE *im )
{
   int ii,jj,kk , nx,ny,nz , nxy,nxyz , off,top ;
   float *f , *r ;

   if( im == NULL || im->kind != MRI_float ) return ;

   nx = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   f  = MRI_FLOAT_PTR(im) ;

   /* blur in x direction */

#undef  D
#define D 1
   if( nx > 3 ){
     top = nx-2 ;
     r   = (float *) malloc(sizeof(float)*nx) ;
     for( kk=0 ; kk < nz ; kk++ ){
       for( jj=0 ; jj < ny ; jj++ ){
         off = jj*nx + kk*nxy ;
         r[0] =                                WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D]; off+=D;
         r[1] =                   WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D]; off+=D;
         for( ii=2 ; ii < top ; ii++,off+=D ){
           r[ii] = WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D];
         }
         r[ii] =   WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]; off+=D; ii++;
         r[ii] =   WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]             ;
         off = jj*nx + kk*nxy ;
         for( ii=0 ; ii < nx ; ii++ ) f[ii*D+off] = r[ii] ;
       }
     }
     free(r) ;
   }

   /* blur in y direction */

#undef  D
#define D nx
   if( ny > 3 ){
     top = ny-2 ;
     r   = (float *) malloc(sizeof(float)*ny) ;
     for( kk=0 ; kk < nz ; kk++ ){
       for( ii=0 ; ii < nx ; ii++ ){
         off = ii + kk*nxy ;
         r[0] =                                WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D]; off+=D;
         r[1] =                   WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D]; off+=D;
         for( jj=2 ; jj < top ; jj++,off+=D ){
           r[jj] = WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D];
         }
         r[jj] =   WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]; off+=D; jj++;
         r[jj] =   WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]             ;
         off = ii + kk*nxy ;
         for( jj=0 ; jj < ny ; jj++ ) f[jj*D+off] = r[jj] ;
       }
     }
     free(r) ;
   }

   /* blur in z direction */

#undef  D
#define D nxy
   if( nz > 3 ){
     top = nz-2 ;
     r   = (float *) malloc(sizeof(float)*nz) ;
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ){
         off = ii + jj*nx ;
         r[0] =                                WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D]; off+=D;
         r[1] =                   WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D]; off+=D;
         for( kk=2 ; kk < top ; kk++,off+=D ){
           r[kk] = WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]+WT2*f[off+2*D];
         }
         r[kk] =   WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]+WT1*f[off+D]; off+=D; kk++;
         r[kk] =   WT2*f[off-2*D]+WT1*f[off-D]+WT0*f[off]             ;
         off = ii + jj*nx ;
         for( kk=0 ; kk < nz ; kk++ ) f[kk*D+off] = r[kk] ;
       }
     }
     free(r) ;
   }

   return ;
}
