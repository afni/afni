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
  The 3-index of a voxel from the row is in (xx,yy,zz).  The result
  is a 1D MRI_IMAGE *im, with im->nx = number of points extracted.
  If mask!=NULL, then it is a mask of allowed points.
-----------------------------------------------------------------------------*/

MRI_IMAGE * THD_get_dset_nbhd( THD_3dim_dataset *dset, int ival, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd    )
{
   int kind , nx,ny,nz,nxy , npt , nout , aa,bb,cc,kk,ii ;
   void *brick ;
   MRI_IMAGE *im ;
   float fac ;

ENTRY("THD_get_dset_nbhd") ;

   if( dset == NULL || nbhd == NULL || nbhd->num_pt <= 0 ) RETURN(NULL) ;
   if( ival < 0 || ival >= DSET_NVALS(dset) )              RETURN(NULL) ;

   im = mri_get_nbhd( DSET_BRICK(dset,ival) , mask , xx,yy,zz , nbhd ) ;

   fac = DSET_BRICK_FACTOR(dset,ival) ;
   if( im != NULL && fac != 0.0f && fac != 1.0f ){
     MRI_IMAGE *qim = mri_scale_to_float( fac , im ) ;
     mri_free(im) ; im = qim ;
   }

   RETURN(im) ;
}


static byte SearchAboutMaskedVoxel = 0;

void SetSearchAboutMaskedVoxel(int v)
{
   SearchAboutMaskedVoxel = v;
}
/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_get_nbhd( MRI_IMAGE *inim , byte *mask ,
                          int xx, int yy, int zz, MCW_cluster *nbhd )
{
   int kind , nx,ny,nz,nxy,nxyz , npt , nout , aa,bb,cc,kk,ii ;
   MRI_IMAGE *im ;
   void *brick ;

ENTRY("mri_get_nbhd") ;

   if( inim == NULL || nbhd == NULL ) RETURN(NULL) ;

   nx = inim->nx ;
   ny = inim->ny ; nxy  = nx*ny  ;
   nz = inim->nz ; nxyz = nxy*nz ; npt = nbhd->num_pt ; nout = 0 ;

   kk = xx + yy*nx + zz*nxy ;
   if (SearchAboutMaskedVoxel) {
      if( npt == 0 || kk < 0 || kk >= nxyz                ) RETURN(NULL) ;
   } else {
      if( npt == 0 || kk < 0 || kk >= nxyz || !INMASK(kk) ) RETURN(NULL) ;
   }
   kind  = inim->kind ;
   brick = mri_data_pointer(inim) ;  if( brick == NULL ) RETURN(NULL) ;
   im    = mri_new( npt , 1 , kind ) ;

   /*-- extract data, based on kind of data in sub-brick --*/

   switch( kind ){

     default: break ;   /* bad bad bad */

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
