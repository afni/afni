#include "mrilib.h"

/*---------------------------------------------------------------------------*/

THD_3dim_dataset * THD_svdblur( THD_3dim_dataset *inset, byte *mask,
                                float rad, int pdim, int nort, float **ort )
{
   THD_3dim_dataset *outset=NULL ;
   MCW_cluster *nbhd=NULL ;
   char *prefix="./SVDblur" ;
   int kk,nx,ny,nz,nxy,nxyz,nt , nmask ;
   float na,nb,nc , dx,dy,dz ;

ENTRY("THD_svdblur") ;

   /*---- error checking and basic setup ----*/

   if( !ISVALID_DSET(inset)     ) RETURN(NULL) ;
   if( mask == NULL             ) RETURN(NULL) ;
   if( rad  == 0.0f             ) RETURN(NULL) ;
   if( nort >  0 && ort == NULL ) RETURN(NULL) ;

   nt = DSET_NVALS(inset) ; if( nt < 9+nort ) RETURN(NULL) ;

   nx = DSET_NX(inset) ;
   ny = DSET_NY(inset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(inset) ; nxyz = nxy*nz ;

   nmask = THD_countmask( nxyz , mask ) ; if( nmask < 9 ) RETURN(NULL) ;

   DSET_load(inset) ; if( !DSET_LOADED(inset) ) RETURN(NULL) ;

   /*---- create spherical neighborhood (as a cluster) -----*/

   if( rad < 0.0f ){ dx = dy = dz = 1.0f; rad = -rad; rad = MAX(rad,1.01f); }
   else            { dx = fabsf(DSET_DX(inset));
                     dy = fabsf(DSET_DY(inset));
                     dz = fabsf(DSET_DZ(inset)); }
   nbhd = MCW_spheremask( dx,dy,dz , rad ) ;
   MCW_radsort_cluster( nbhd , dx,dy,dz ) ; /* ensures first value is centroid */

   /*---- create output dataset ----*/

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset, ADN_prefix,prefix, ADN_brick_fac,NULL, ADN_none );
   for( kk=0 ; kk < nt ; kk++ )                  /* create zero-filled bricks */
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   /**** the real work now begins ****/

   { int kk , xx,yy,zz , vv,ii ;
     MRI_IMARR *imar ; MRI_IMAGE *pim ;
     float *tsar ;

     for( kk=0 ; kk < nxyz ; kk++ ){  /* voxel loop */
       if( !mask[kk] ) continue ;
       IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;
       imar = THD_get_dset_nbhd_array( inset , mask , xx,yy,zz , nbhd ) ;
       if( imar == NULL ) continue ;
       if( nort > 0 ){
         for( vv=0 ; vv < IMARR_COUNT(imar) ; vv++ ){
           tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,vv)) ;
           THD_generic_detrend_LSQ( nt , tsar , -1 , nort , ort , NULL ) ;
         }
       }
       pim = mri_svdproj( imar , pdim ) ;
       DESTROY_IMARR(imar) ;
       if( pim != NULL ){
         THD_insert_series( kk, outset, nt, MRI_float, MRI_FLOAT_PTR(pim), 0 ) ;
         mri_free(pim) ;
       }
     } /* end of voxel loop */
   }

   /*** cleanup and exit ***/

   RETURN(outset) ;
}

/*------------------------------------------------------------------------*/

MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                     int xx, int yy, int zz, MCW_cluster *nbhd )
{
   MRI_IMARR *imar ;
   int nvox, *ivox , nx,ny,nz , nxy,nxyz , npt, aa,bb,cc,kk,ii ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ; npt = nbhd->num_pt ;

   kk = xx + yy*nx + zz*nxy ; if( kk < 0 || kk >= nxyz ) return(NULL) ;

   ivox = (int *)malloc(sizeof(int)*npt) ; nvox = 0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;
     if( mask == NULL || mask[kk] ) ivox[nvox++] = kk ;
   }
   if( nvox == 0 ){ free(ivox) ; return(NULL) ; }  /* no voxels to extract */

   imar = THD_extract_many_series( nvox, ivox, dset ) ;
   free(ivox) ; return(imar) ;
}

/*------------------------------------------------------------------------*/
/* Project the vectors in imar along the principal nev-dimensional
   subspace; if nev <= 0, just return the first eigenvector instead.
--------------------------------------------------------------------------*/

MRI_IMAGE * mri_svdproj( MRI_IMARR *imar , int nev )
{
   int nx , nvec , ii,jj , itop , doproj=(nev > 0) ;
   float *far , *xar ; MRI_IMAGE *tim ;
   float *vnorm=NULL ;
   register float sum ;
   float *uvec ;

   if( imar == NULL ) return(NULL) ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return(NULL) ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return(NULL) ;

   if( nvec == 1 ){  /* trivial case */
     tim = mri_to_float( IMARR_SUBIM(imar,0) ) ;
     if( nev <= 0 ) THD_normalize( tim->nx , MRI_FLOAT_PTR(tim) ) ;
     return(tim) ;
   }

#undef  U
#define U(i,j) uvec[(i)+(j)*nx]     /* ditto */

   if( !doproj ) nev = 1 ;
   uvec = (float *)malloc( sizeof(float)*nx*nev ) ;

   nev = mri_principal_vectors( imar , nev , NULL , uvec ) ;
   if( nev <= 0 ){ free(uvec); return(NULL); }

   /** create output **/

   tim = mri_new( nx , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;                  /* zero filled */
   xar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,0)) ;  /* central vector */

   if( doproj ){ /* project input time series (1st vector in imar) onto subspace */

     for( jj=0 ; jj < nev ; jj++ ){
       sum = 0.0f ;
       for( ii=0 ; ii < nx ; ii++ ) sum += U(ii,jj) * xar[ii] ;
       for( ii=0 ; ii < nx ; ii++ ) far[ii] += sum * U(ii,jj) ;
     }

   } else {  /* just save first eigenvector */

     sum = 0.0f ;
     for( ii=0 ; ii < nx ; ii++ ) sum += U(ii,0) * xar[ii] ;
     if( sum < 0.0f )
       for( ii=0 ; ii < nx ; ii++ ) far[ii] = -U(ii,0) ;
     else
       for( ii=0 ; ii < nx ; ii++ ) far[ii] =  U(ii,0) ;

   }

   /** finito **/

   free(uvec); return(tim);
}
#undef U

/*------------------------------------------------------------------------*/

MRI_IMAGE * mri_first_principal_vector( MRI_IMARR *imar )
{
   return mri_svdproj( imar , 0 ) ;
}

/*----------------------------------------------------------------------------*/
/*! Compute the nvec principal singular vectors of a set of m columns, each
    of length n, stored in the image array imar.

    The singular values (largest to smallest) are stored in sval, and
    the left singular vectors [first nvec columns of U in X = U S V'] are
    stored into uvec[i+j*n] for i=0..n-1, j=0..nvec-1.  These columns
    are L2-normalized and orthogonal.

    The return value is the number of vectors computed.  If the return
    value is not positive, something bad happened.  Normally, the return
    value would be the same as nvec, but it cannot be larger than MIN(n,m).

    If sval==NULL, then the output into sval is skipped.
    If uval==NULL, then the output into uval is skipped.
    If both are NULL, exactly why did you want to call this function?
*//*--------------------------------------------------------------------------*/

int mri_principal_vectors( MRI_IMARR *imar, int nvec, float *sval, float *uvec )
{
   int nn , mm , nsym , ii,jj,kk,qq ;
   double *asym , *deval ;
   float **xpt ;
   register double sum , qsum ; register float *xj , *xk ;

   if( imar == NULL || IMARR_COUNT(imar) < 1 ) return(-555) ;

   nn = IMARR_SUBIM(imar,0)->nx ;
   mm = IMARR_COUNT(imar) ;

   nsym = MIN(nn,mm) ;  /* size of the symmetric matrix to create */

   if( nsym < 1 || (uvec == NULL && sval == NULL) ) return(-666) ;

   /* pointers to each vector */

   xpt = (float **)malloc(sizeof(float *)*mm) ;
   for( kk=0 ; kk < mm ; kk++ ) xpt[kk] = MRI_FLOAT_PTR(IMARR_SUBIM(imar,kk)) ;

   /* number of eigenpairs to compute */

        if( nvec > nsym ) nvec = nsym ;  /* can't compute more vectors than nsym */
   else if( nvec <= 0   ) nvec = 1 ;

#pragma omp critical (MALLOC)
   { asym  = (double *)malloc(sizeof(double)*nsym*nsym) ;  /* symmetric matrix */
     deval = (double *)malloc(sizeof(double)*nsym) ;       /* its eigenvalues */
   }

#undef  A
#define A(i,j) asym[(i)+(j)*nsym]

   /** setup matrix to eigensolve: choose smaller of [X]'[X] and [X][X]' **/
   /**     since [X] is n x m, [X]'[X] is m x m and [X][X]' is n x n     **/

   if( nn > mm ){                       /* more rows than columns:  */
                                        /* so [A] = [X]'[X] = m x m */
     int n1 = nn-1 ;
     for( jj=0 ; jj < mm ; jj++ ){
       xj = xpt[jj] ;
       for( kk=0 ; kk <= jj ; kk++ ){
         sum = 0.0 ; xk = xpt[kk] ;
         for( ii=0 ; ii < n1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
         if( ii == n1 ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

   } else {                             /* more columns than rows:  */
                                        /* so [A] = [X][X]' = n x n */
     float *xt ; int m1=mm-1 ;
#pragma omp critical (MALLOC)
     xt = (float *)malloc(sizeof(float)*nn*mm) ;
     for( jj=0 ; jj < mm ; jj++ ){      /* form [X]' into array xt */
       for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xpt[jj][ii] ;
     }

     for( jj=0 ; jj < nn ; jj++ ){
       xj = xt + jj*mm ;
       for( kk=0 ; kk <= jj ; kk++ ){
         sum = 0.0 ; xk = xt + kk*mm ;
         for( ii=0 ; ii < m1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
         if( ii == m1 ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

#pragma omp critical (MALLOC)
     free(xt) ;  /* don't need this no more */
   }

   /** compute the nvec eigenvectors corresponding to largest eigenvalues **/
   /** these eigenvectors are stored on top of first nvec columns of asym **/

   ii = symeig_irange( nsym, asym, deval, nsym-nvec, nsym-1, (uvec==NULL) ) ;

   if( ii != 0 ){
#pragma omp critical (MALLOC)
     { free(deval) ; free(asym) ; free(xpt) ; }
     return(-33333) ;  /* eigensolver failed!? */
   }

   /** Store singular values (sqrt of eigenvalues), if desired:
       Note that symeig_irange returns things smallest to largest,
       but we want largest to smallest, so have to reverse the order **/

   if( sval != NULL ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = deval[nvec-1-jj] ;
       sval[jj] = (sum <= 0.0) ? 0.0 : sqrt(sum) ;
     }
   }

   /** if no output vectors desired, we are done done done!!! **/

   if( uvec == NULL ){
#pragma omp critical (MALLOC)
     { free(deval) ; free(asym) ; free(xpt) ; }
     return(nvec) ;
   }

   /** SVD is [X] = [U] [S] [V]', where [U] = desired output vectors

       case n <= m: [A] = [X][X]' = [U] [S][S]' [U]'
                    so [A][U] = [U] [S][S]'
                    so eigenvectors of [A] are just [U]

       case n > m:  [A] = [X]'[X] = [V] [S]'[S] [V]'
                    so [A][V] = [V] [S'][S]
                    so eigenvectors of [A] are [V], but we want [U]
                    note that [X][V] = [U] [S]
                    so pre-multiplying each column vector in [V] by matrix [X]
                    will give the corresponding column in [U], but scaled;
                    below, just L2-normalize the column to get output vector **/

   if( nn <= mm ){                  /* copy eigenvectors into output directly */
                                     /* (e.g., more vectors than time points) */
     for( jj=0 ; jj < nvec ; jj++ ){
       qq = nvec-1-jj ;                  /* eigenvalues are in reversed order */
       for( ii=0 ; ii < nn ; ii++ )
         uvec[ii+jj*nn] = (float)asym[ii+qq*nn] ;
     }

   } else {  /* n > m: transform eigenvectors to get left singular vectors */
             /* (e.g., more time points than vectors) */

     for( jj=0 ; jj < nvec ; jj++ ){
       qq = nvec-1-jj ; qsum = 0.0 ;  /* eigenvalues are in reversed order */
       for( ii=0 ; ii < nn ; ii++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < mm ; kk++ ) sum += xpt[kk][ii] * asym[kk+qq*mm] ;
         uvec[ii+jj*nn] = sum ; qsum += sum*sum ;
       }
       if( qsum > 0.0 ){       /* L2 normalize */
         register float fac ;
         fac = (float)(1.0/sqrt(qsum)) ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii+jj*nn] *= fac ;
       }
     }
   }

   /** free at last!!! **/

#pragma omp critical (MALLOC)
   { free(deval) ; free(asym) ; free(xpt) ; }

   return(nvec) ;
}
#undef A

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_average_vector( MRI_IMARR *imar )
{
   int nx , nvec , jj ;
   float *far , *xar ; MRI_IMAGE *tim ;
   register int ii ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return NULL ;

   /** create output **/

   tim = mri_new( nx , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;            /* zero filled */

   /** sum them up, then average **/

   for( jj=0 ; jj < nvec ; jj++ ){
     xar = MRI_FLOAT_PTR( IMARR_SUBIM(imar,jj) ) ;
     for( ii=0 ; ii < nx ; ii++ ) far[ii] += xar[ii] ;
   }
   if( nvec > 1 ){
     register fac ;
     fac = 1.0f / nvec ;
     for( ii=0 ; ii < nx ; ii++ ) far[ii] *= fac ;
   }

   return tim;
}
