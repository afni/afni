#include "mrilib.h"

#include "despike_inc.c"

/*-----------------------------------------------------------------*/

static float r2D( int n , float *a , float *b , float *x )
{
   float sax=0.0f , sbx=0.0f , sxx=0.0f ; int ii ;

   for( ii=0 ; ii < n ; ii++ ){
     sax += x[ii]*a[ii] ;
     sbx += x[ii]*b[ii] ;
     sxx += x[ii]*x[ii] ;
   }
   if( sxx <= 0.0001f ) return 0.0f ;
   sax = (sax*sax+sbx*sbx)/sxx ;
#if 0
   if( sax > 1.0f ) sax = 1.0f ;
#endif
   return sax ;
}

/*----------------------------------------------------------------*/

MRI_IMAGE * mri_vec_to_pvmap( MRI_IMAGE *inim )
{
   int nx , ny , ii ;
   float_pair svals ;
   MRI_IMAGE *outim ;
   float *uvec , *vvec , *outar , *iar ;
   unsigned short xran[3] ;
   static int ncall=0 ;

   if( inim == NULL || inim->kind != MRI_float ) return NULL ;

   nx = inim->nx ; if( nx < 9 ) return NULL ;
   ny = inim->ny ; if( ny < 9 ) return NULL ;

   uvec = (float *)malloc(sizeof(float)*nx) ;
   vvec = (float *)malloc(sizeof(float)*nx) ;

   xran[0] = (unsigned short)(nx+ny+73) ;
   xran[1] = (unsigned short)(nx-ny+473+ncall) ; ncall++ ;
   xran[2] = (unsigned short)(nx*ny+7) ;

   iar   = MRI_FLOAT_PTR(inim) ;
   svals = principal_vector_pair( nx , ny , 0 , iar ,
                                  uvec , vvec , NULL , NULL , xran ) ;

#if 0
INFO_message("mri_vec_to_pvmap: svals = %g %g",svals.a,svals.b) ;
for( ii=0 ; ii < nx ; ii++ ){
  printf(" %g %g\n",uvec[ii],vvec[ii]) ;
}
#endif

   if( svals.a < 0.0f || svals.b < 0.0f ){
     free(uvec) ; free(vvec) ; return NULL ;
   }

   outim = mri_new( ny , 1 , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;

   THD_normalize(nx,uvec) ;
   THD_normalize(nx,vvec) ;

#if 0
INFO_message("uvec %g   svec %g",svals.a,svals.b) ;
for( ii=0 ; ii < nx ; ii++ )
  fprintf(stderr," %7.4f %7.4f\n",uvec[ii],vvec[ii]) ;
#endif

   for( ii=0 ; ii < ny ; ii++ ){
     outar[ii] = r2D( nx , uvec , vvec , iar+ii*nx ) ;
   }

   free(uvec) ; free(vvec) ; return outim ;
}

/*-----------------------------------------------------------------*/

MRI_IMAGE * THD_dataset_to_pvmap( THD_3dim_dataset *dset , byte *mask )
{
   int nvox, npt, nmask, ii,jj ;
   MRI_IMAGE *inim, *tim, *outim ;
   float *inar, *tar, *outar, *dar ;

   if( !ISVALID_DSET(dset) ) return NULL ;

   nvox = DSET_NVOX(dset) ;
   npt  = DSET_NVALS(dset) ;
   if( nvox < 9 || npt < 9 ) return NULL ;

   if( mask != NULL ){
     nmask = THD_countmask( nvox , mask ) ;
     if( nmask < 9 ) return NULL ;
   } else {
     nmask = nvox ;
   }

   inim = mri_new( npt , nmask , MRI_float ) ;
   inar = MRI_FLOAT_PTR(inim) ;
   dar  = (float *)malloc(sizeof(float)*npt) ;
   tar  = (float *)malloc(sizeof(float)*npt*3) ;

   DSET_load(dset) ;

   for( jj=ii=0 ; ii < nvox ; ii++ ){
     if( mask == NULL || mask[ii] != 0 ){
       THD_extract_array( ii , dset , 0 , dar ) ;
       DES_despike25( npt , dar , tar ) ;
       THD_cubic_detrend( npt , dar ) ;
       THD_normalize( npt , dar ) ;
       memcpy( inar+jj*npt , dar , sizeof(float)*npt ) ;
       jj++ ;
     }
   }

   free(tar) ; free(dar) ;

   tim = mri_vec_to_pvmap( inim ) ;

   mri_free(inim) ;

   if( nmask == nvox ) return tim ;

   outim = mri_new( nvox , 1 , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   tar   = MRI_FLOAT_PTR(tim) ;

   for( jj=ii=0 ; ii < nvox ; ii++ ){
     if( mask == NULL || mask[ii] != 0 ) outar[ii] = tar[jj++] ;
     else                                outar[ii] = 0.0f ;
   }

   mri_free(tim) ;
   return outim ;
}
