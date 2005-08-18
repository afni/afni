#include "mrilib.h"

/****************************************************************************
  Functions for dealing with data extracted from a neighborhood of a voxel
  in a dataset.
  -- 19 Aug 2005 - RWCox
*****************************************************************************/

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

/*---------------------------------------------------------------------------*/
/*! Extract a nbhd from a dataset sub-brick.
  The 3-index of a voxel from the row is in (xx,yy,zz).
-----------------------------------------------------------------------------*/

MRI_IMAGE * THD_get_dset_nbhd( THD_3dim_dataset *dset, int ival, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd    )
{
   int kind , nx,ny,nz,nxy , ibase , npt , nout , aa,bb,cc,kk,ii ;
   void *brick ;
   MRI_IMAGE *im ;

ENTRY("THD_get_dset_nbhd") ;

   if( dset == NULL || nbhd == NULL || nbhd->num_pt <= 0 ) RETURN(NULL) ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy = nx*ny ;
   nz = DSET_NZ(dset) ; npt = nbhd->num_pt ; nout = 0 ;

   ibase = xx + yy*nx + zz*nxy ;
   if( !INMASK(ibase) ) RETURN(NULL) ;

   kind  = DSET_BRICK_TYPE(dset,ival) ;
   brick = DSET_ARRAY(dset,ival) ;
   im    = mri_new( npt , 1 , kind ) ;

   /*-- extract data, based on kind of data in sub-brick --*/

   switch( kind ){

     default: mri_free(im) ; RETURN(NULL) ;  /* bad */

     case MRI_short:{
       short *rr = MRI_SHORT_PTR(im) , *vv = (short *)brick ;
       for( ii=0 ; ii < npt ; ii++ ){
         aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
         bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
         cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
         kk = aa + bb*nx + cc*nxy ;
         if( INMASK(kk) ) rr[nout++] = vv[kk] ;
       }
     }
     break ;

     case MRI_byte:{
       byte *rr = MRI_BYTE_PTR(im) , *vv = (byte *)brick ;
       for( ii=0 ; ii < npt ; ii++ ){
         aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
         bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
         cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
         kk = aa + bb*nx + cc*nxy ;
         if( INMASK(kk) ) rr[nout++] = vv[kk] ;
       }
     }
     break ;

     case MRI_float:{
       float *rr = MRI_FLOAT_PTR(im) , *vv = (float *)brick ;
       for( ii=0 ; ii < npt ; ii++ ){
         aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
         bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
         cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
         kk = aa + bb*nx + cc*nxy ;
         if( INMASK(kk) ) rr[nout++] = vv[kk] ;
       }
     }
     break ;

     case MRI_complex:{
       complex *rr = MRI_COMPLEX_PTR(im) , *vv = (complex *)brick ;
       for( ii=0 ; ii < npt ; ii++ ){
         aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
         bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
         cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
         kk = aa + bb*nx + cc*nxy ;
         if( INMASK(kk) ) rr[nout++] = vv[kk] ;
       }
     }
     break ;

     case MRI_rgb:{
       byte *rr = MRI_BYTE_PTR(im) , *vv = (byte *)brick ;
       for( ii=0 ; ii < npt ; ii++ ){
         aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
         bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
         cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
         kk = aa + bb*nx + cc*nxy ;
         if( INMASK(kk) ){
           rr[3*nout  ] = vv[3*kk  ] ;
           rr[3*nout+1] = vv[3*kk+1] ;
           rr[3*nout+2] = vv[3*kk+2] ; nout++ ;
         }
       }
     }
     break ;
   }

   if( nout == 0 ){ mri_free(im) ; RETURN(NULL) ; }

   im->nx = im->nvox = nout ; RETURN(im) ;
}
