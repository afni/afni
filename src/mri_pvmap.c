#include "mrilib.h"

#include "despike_inc.c"

/*-----------------------------------------------------------------*/
/* This function computes the "correlation" of vector x
   with the pair of vectors a and b (presumed to be L2 norm 1).
*//*---------------------------------------------------------------*/

static float r2D( UAint n , float *a , float *b , float *x )
{
   float sax=0.0f , sbx=0.0f , sxx=0.0f ; UAint ii ;

   for( ii=0 ; ii < n ; ii++ ){
     sax += x[ii]*a[ii] ; /* a dot x */
     sbx += x[ii]*b[ii] ; /* b dot x */
     sxx += x[ii]*x[ii] ; /* x dot x */
   }
   if( sxx <= 0.0001f ) return 0.0f ;
   sax = (sax*sax+sbx*sbx)/sxx ; /* multiple correlation squared */
#if 0
   if( sax > 1.0f ) sax = 1.0f ; /* should be oompossibull */
#endif
   return sax ;
}

/*----------------------------------------------------------------*/
/* Return the two principal vectors computed during the pvmapping */
/*----------------------------------------------------------------*/

static UAint  nvec=0 ;
static float *uvec=NULL , *vvec=NULL ;
static float  ulam=0.0f ,  vlam=0.0f ;

MRI_IMAGE * mri_pvmap_get_vecpair(void)
{
  MRI_IMAGE *uvim ;

  if( nvec == 0 || uvec == NULL || vvec == NULL ) return NULL ;

  uvim = mri_new( nvec , 2 , MRI_float ) ;
  memcpy( MRI_FLOAT_PTR(uvim)      , uvec , sizeof(float)*nvec ) ;
  memcpy( MRI_FLOAT_PTR(uvim)+nvec , vvec , sizeof(float)*nvec ) ;
  return uvim ;
}

/*------------------------------------*/
/* Return the pair of singular values */
/*------------------------------------*/

float_pair mri_pvmap_get_lampair(void)
{
   float_pair uvlam ;
   uvlam.a = ulam ; uvlam.b = vlam ; return uvlam ;
}

/*----------------------------------------------------------------*/
/* Inputs:
     nx  = number of points per time series (>= 9)
     ny  = number of time series (>= 9)
     vlist[j] = j-th time series (j=0..ny-1)
   Output:
     newly malloc-ed array, length ny, of the pvmap intensity
     (r2D) for each time series
*//*-------------------------------------------------------------*/

static float * mri_veclist_to_pvmap( UAint nx , UAint ny , float **vlist )
{
   float_pair svals ;
   float     *outar ;
   unsigned short xran[3] ;  /* seed for initializing iterations */
   UAint ii ;
   static int ncall=-109 ;

ENTRY("mri_veclist_to_pvmap") ;

   if( vlist == NULL ) RETURN(NULL) ;
   if( nx    <   9   ) RETURN(NULL) ;
   if( ny    <   9   ) RETURN(NULL) ;

   /* get space for the principal vector pair */

   if( nx != nvec || uvec == NULL || vvec == NULL ){
     uvec = (float *)realloc(uvec,sizeof(float)*nx) ;
     vvec = (float *)realloc(vvec,sizeof(float)*nx) ;
     nvec = nx ;
   }

   /* random number seed used to initialize
      iterating vectors for the principal vector pair */

   xran[0] = (unsigned short)(nx+ny+73) ;
   xran[1] = (unsigned short)(nx-ny+473+ncall) ; ncall++ ;
   xran[2] = (unsigned short)(nx*ny-777) ;

   /* compute the first 2 principal vectors */

   svals = principal_vector_pair( nx , ny , 1 , vlist ,  /* cf. cs_pv.c */
                                  uvec , vvec , NULL , NULL , xran ) ;

   ulam = svals.a ; vlam = svals.b ;  /* singular values */

   ININFO_message("   first 2 singular values = %g %g",ulam,vlam) ;

   if( ulam < 0.0f || vlam < 0.0f ) RETURN(NULL) ;  /* should not happen */

   outar = (float *)malloc(sizeof(float)*ny) ;

   THD_normalize(nx,uvec) ;  /* sum of squares = 1 */
   THD_normalize(nx,vvec) ;

   /* compute pvmap value for each input vector */

   for( ii=0 ; ii < ny ; ii++ ){
     outar[ii] = r2D( nx , uvec , vvec , vlist[ii] ) ;
   }

   RETURN(outar) ;
}

/*----------------------------------------------------------------*/
/* Modified extensively to compute in time series length chunks
   of maximum length NXMAX, then average (if more than one chunk).
   Longer time series tend to take a very long time to compute.
                                       [Rewritten Dec 2021, RWCox]
*//*--------------------------------------------------------------*/

#define NXMAX 1024

MRI_IMAGE * mri_vec_to_pvmap( MRI_IMAGE *inim )
{
   Aint nx , ny , ii , numpart , lenpart , pp , xbot,xtop ;
   float_pair svals ;
   MRI_IMAGE *outim ;
   float **vlist , *pvout=NULL , *pvpart=NULL , *iar ;

ENTRY("mri_vec_to_pvmap") ;

   if( inim == NULL || inim->kind != MRI_float ) RETURN(NULL) ;

   nx = (Aint)inim->nx ; if( nx < 9 ) RETURN(NULL) ;
   ny = (Aint)inim->ny ; if( ny < 9 ) RETURN(NULL) ;

   vlist = (float **)malloc(sizeof(float *)*ny) ;

   numpart = (nx-1)/NXMAX + 1 ;  /* number of parts */
   lenpart = nx / numpart ;      /* length of each part (except last) */

   iar = MRI_FLOAT_PTR(inim) ;

   for( pp=0 ; pp < numpart ; pp++ ){  /* loop over parts */
     xbot = lenpart * pp ;
     xtop = (pp < numpart-1) ? (xbot+lenpart) : nx ; /* last part goes to end */
     /* pointers to start of each sub-vector for this part */
     for( ii=0 ; ii < ny ; ii ++ ) vlist[ii] = iar + (xbot+ii*nx) ;
     /* make PVmap for this part */
     pvpart = mri_veclist_to_pvmap( (xtop-xbot) , ny , vlist ) ;
     if( pp == 0 ){  /* first part = save as output */
       pvout = pvpart ;
     } else {        /* later part = sum into output */
       for( ii=0 ; ii < ny ; ii++) pvout[ii] += pvpart[ii] ;
       free(pvpart) ;
     }
   }

   free(vlist) ;

   if( numpart > 1 ){             /* output = average of input part PVmaps */
     float fac = 1.0f / numpart ;
     for( ii=0 ; ii < ny ; ii++ ) pvout[ii] *= fac ;
   }

   /* manufacture output image with no data 'empty',
      then put the output array into it as its data (sneaky, huh?) */

   outim = mri_new_empty( ny , 1 , MRI_float ) ;
   mri_set_data_pointer( outim , pvout ) ;

   RETURN(outim) ;
}

/*-----------------------------------------------------------------*/

MRI_IMAGE * THD_dataset_to_pvmap( THD_3dim_dataset *dset , byte *mask )
{
   int nvox, npt, nmask, ii,jj , polort ;
   MRI_IMAGE *inim, *tim, *outim ;
   float *inar, *tar, *outar, *dar ;

ENTRY("THD_dataset_to_pvmap") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   nvox = DSET_NVOX(dset) ;
   npt  = DSET_NVALS(dset) ;
   if( nvox < 9 || npt < 9 ) RETURN(NULL) ;

   if( mask != NULL ){
     nmask = THD_countmask( nvox , mask ) ;
     if( nmask < 9 ) RETURN(NULL) ;
   } else {
     nmask = nvox ;
   }

   inim = mri_new( npt , nmask , MRI_float ) ;
   inar = MRI_FLOAT_PTR(inim) ;
   dar  = (float *)malloc(sizeof(float)*npt) ;
   tar  = (float *)malloc(sizeof(float)*npt*3) ;

   DSET_load(dset) ;

   polort = npt / 50 ;                   /* 24 Apr 2019 */
        if( polort <  2 ) polort = 2 ;   /* change detrending */
   else if( polort > 20 ) polort = 20 ;

   for( jj=ii=0 ; ii < nvox ; ii++ ){
     if( mask == NULL || mask[ii] != 0 ){
       THD_extract_array( ii , dset , 0 , dar ) ;
       DES_despike25( npt , dar , tar ) ;    /* despiking */
#if 0
       THD_cubic_detrend( npt , dar ) ;      /* detrending */
#else
       THD_generic_detrend_LSQ( npt , dar , polort , 0,NULL,NULL ) ;
#endif
       THD_normalize( npt , dar ) ;          /* L2 normalize */
       memcpy( inar+jj*npt , dar , sizeof(float)*npt ) ;
       jj++ ;
     }
   }

   free(tar) ; free(dar) ;

   tim = mri_vec_to_pvmap( inim ) ;

   mri_free(inim) ;

   if( nmask == nvox ) RETURN(tim) ;

   outim = mri_new( nvox , 1 , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   tar   = MRI_FLOAT_PTR(tim) ;

   for( jj=ii=0 ; ii < nvox ; ii++ ){
     if( mask == NULL || mask[ii] != 0 ) outar[ii] = tar[jj++] ;
     else                                outar[ii] = 0.0f ;
   }

   mri_free(tim) ;

   RETURN(outim) ;
}
