#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*************************************************************************
 **   warp3D version of mri_3dalign.c -- RWCox -- November 2004         **
 *************************************************************************/

/*-----------------------------------------------------------------------*/
/*! Compute the pseudo-inverse of a matrix stored in a 2D float image.
    If the input is mXn, the output is nXm.  wt[] is an optional array
    of positive weights, m of them.  The result can be used to solve
    the weighted least squares problem
      [imc] [b] = [v]
    where [b] is an n-vector and [v] is an m-vector, where m > n.
-------------------------------------------------------------------------*/

static MRI_IMAGE * mri_psinv( MRI_IMAGE *imc , float *wt )
{
   float *rmat=MRI_FLOAT_PTR(imc) ;
   int m=imc->nx , n=imc->ny , ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,ww ;
   MRI_IMAGE *imp ; float *pmat ;
   register double sum ;
   int do_svd=0 ;

   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */

#define R(i,j) rmat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define A(i,j) amat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define P(i,j) pmat[(i)+(j)*n]   /* i=0..n-1 , j=0..m-1 */

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = R(ii,jj) ;

   /* weight rows? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       for( jj=0 ; jj < n ; jj++ ) A(ii,jj) *= ww ;
     }
   }

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     sum = 0.0 ;
     for( ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ; else do_svd = 1 ;
     xfac[jj] = sum ;
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
   }

   /*** compute using Choleski or SVD ***/

   if( do_svd || AFNI_yesenv("AFNI_WARPDRIVE_SVD") ){ /***--- SVD method ---***/

#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

     umat = (double *)calloc( sizeof(double),m*n ); /* left singular vectors */
     vmat = (double *)calloc( sizeof(double),n*n ); /* right singular vectors */
     sval = (double *)calloc( sizeof(double),n   ); /* singular values */

     /* compute SVD of scaled matrix */

     svd_double( m , n , amat , sval , umat , vmat ) ;

     free((void *)amat) ;  /* done with this */

     /* find largest singular value */

     smax = sval[0] ;
     for( ii=1 ; ii < n ; ii++ ) if( sval[ii] > smax ) smax = sval[ii] ;

     if( smax <= 0.0 ){                        /* this is bad */
       fprintf(stderr,"** ERROR: SVD fails in mri_warp3D_align_setup!\n");
       free((void *)xfac); free((void *)sval);
       free((void *)vmat); free((void *)umat); return NULL;
     }

     for( ii=0 ; ii < n ; ii++ )
       if( sval[ii] < 0.0 ) sval[ii] = 0.0 ;  /* should not happen */

#define PSINV_EPS 1.e-8

     /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

     del = PSINV_EPS * smax*smax ;
     for( ii=0 ; ii < n ; ii++ )
       sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

     /* create pseudo-inverse */

     imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
     pmat = MRI_FLOAT_PTR(imp) ;

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < m ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < n ; kk++ ) sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
         P(ii,jj) = (float)sum ;
       }
     }
     free((void *)sval); free((void *)vmat); free((void *)umat);

   } else { /***----- Choleski method -----***/

     vmat = (double *)calloc( sizeof(double),n*n ); /* normal matrix */

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj <= ii ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < m ; kk++ ) sum += A(kk,ii) * A(kk,jj) ;
         V(ii,jj) = sum ;
       }
       V(ii,ii) += PSINV_EPS ;   /* note V(ii,ii)==1 before this */
     }

     /* Choleski factor */

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < ii ; jj++ ){
         sum = V(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= V(ii,kk) * V(jj,kk) ;
         V(ii,jj) = sum / V(jj,jj) ;
       }
       sum = V(ii,ii) ;
       for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * V(ii,kk) ;
       if( sum <= 0.0 ){
         fprintf(stderr,"** ERROR: Choleski fails in mri_warp3D_align_setup!\n");
         free((void *)xfac); free((void *)amat); free((void *)vmat); return NULL ;
       }
       V(ii,ii) = sqrt(sum) ;
     }

     /* create pseudo-inverse */

     imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
     pmat = MRI_FLOAT_PTR(imp) ;

     sval = (double *)calloc( sizeof(double),n ) ; /* row #jj of A */

     for( jj=0 ; jj < m ; jj++ ){
       for( ii=0 ; ii < n ; ii++ ) sval[ii] = A(jj,ii) ; /* extract row */

       for( ii=0 ; ii < n ; ii++ ){  /* forward solve */
         sum = sval[ii] ;
         for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * sval[kk] ;
         sval[ii] = sum / V(ii,ii) ;
       }
       for( ii=n-1 ; ii >= 0 ; ii-- ){  /* backward solve */
         sum = sval[ii] ;
         for( kk=ii+1 ; kk < n ; kk++ ) sum -= V(kk,ii) * sval[kk] ;
         sval[ii] = sum / V(ii,ii) ;
       }

       for( ii=0 ; ii < n ; ii++ ) P(ii,jj) = (float)sval[ii] ;
     }
     free((void *)amat); free((void *)vmat); free((void *)sval);
   }

   /* rescale rows from norming */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ) P(ii,jj) *= xfac[ii] ;
   }
   free((void *)xfac);

   /* rescale cols for weight? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       for( jj=0 ; jj < n ; jj++ ) P(jj,ii) *= ww ;
     }
   }

   return imp;
}

/*-----------------------------------------------------------------------*/

#define WW(i,j,k) wf[(i)+(j)*nx+(k)*nxy]

static void mri_warp3D_align_edging_default( int  nx   , int  ny   , int  nz   ,
                                             int *xfade, int *yfade, int *zfade )
{
   char *ef=my_getenv("AFNI_VOLREG_EDGING") , *eq ;

   if( ef == NULL ){                  /* the 5% solution */
     *xfade = (int)(0.05*nx+0.5) ;
     *yfade = (int)(0.05*ny+0.5) ;
     *zfade = (int)(0.05*nz+0.5) ;
   } else {
     float ff = strtod(ef,&eq) ;
     if( ff < 0 ){                   /* again, 5% */
       *xfade = (int)(0.05*nx+0.5) ;
       *yfade = (int)(0.05*ny+0.5) ;
       *zfade = (int)(0.05*nz+0.5) ;
     } else {
       if( *eq == '%' ){            /* the whatever % solution */
         *xfade = (int)(0.01*ff*nx+0.5) ;
         *yfade = (int)(0.01*ff*ny+0.5) ;
         *zfade = (int)(0.01*ff*nz+0.5) ;
       } else {                     /* the fixed value solution */
         *xfade = (int)( MIN(0.25*nx,ff) ) ;
         *yfade = (int)( MIN(0.25*ny,ff) ) ;
         *zfade = (int)( MIN(0.25*nz,ff) ) ;
       }
     }
   }
}

/*--------------------------------------------------------------------------*/
/*! Get a default value of delta for parameter #kpar, such that the
    RMS index change is about 1.
----------------------------------------------------------------------------*/

static void mri_warp3D_get_delta( MRI_warp3D_align_basis *bas , int kpar )
{
   float *pvec , dpar ;
   int   ii,jj,kk , nx,ny,nz,nxy , nite , ntot ;
   float xx,yy,zz , tx,ty,tz , dt,dtot ;
   float *wf ;

   if( bas == NULL || kpar < 0 || kpar >= bas->nparam ) return ;
   if( bas->param[kpar].fixed ) return ;

   /* load parameter vector with the identity value for all params */

   pvec = (float *)malloc(sizeof(float) * bas->nparam) ;
   for( ii=0 ; ii < bas->nparam ; ii++ )
     pvec[ii] = (bas->param[ii].fixed) ? bas->param[ii].val_fixed
                                       : bas->param[ii].ident ;

   nx = bas->imbase->nx ; ny = bas->imbase->ny ; nz = bas->imbase->nz ;
   nxy = nx*ny ;

   /* initial value for delta */

   dpar = 0.001 * ( fabs(bas->param[kpar].ident) + 1.0 ) ;
   nite = 0 ; wf = MRI_FLOAT_PTR(bas->imww) ;

   /* iterate:
       - compute transformation over all points with positive weight
       - compute distance moved
       - compute RMS of positive distances moved
       - adjust dpar up or down to move RMS distance moved towards 1.0 */

   if( bas->verb )
     fprintf(stderr,"+   get_delta param#%d [%s]: %f" ,
            kpar+1, bas->param[kpar].name , dpar ) ;

   while(1){
     pvec[kpar] = bas->param[kpar].ident + dpar ;  /* set param */
     bas->vwset( bas->nparam , pvec ) ;            /* load transform */
     ntot = 0 ; dtot = 0.0f ;
     for( kk=0 ; kk < nz ; kk++ ){
      zz = (float)kk ;
      for( jj=0 ; jj < ny ; jj++ ){
       yy = (float)jj ;
       for( ii=0 ; ii < nx ; ii++ ){
         if( WW(ii,jj,kk) == 0.0f ) continue ;     /* not counted */
         xx = (float)ii ;
                                                   /* forward transform */
         bas->vwfor( xx,yy,zz , &tx,&ty,&tz ) ;
         dt = (tx-xx)*(tx-xx) + (ty-yy)*(ty-yy) + (tz-zz)*(tz-zz) ;
         if( dt > 0.0f ){ ntot++ ; dtot += dt ; }
                                                   /* inverse transform */
         bas->vwinv( xx,yy,zz , &tx,&ty,&tz ) ;
         dt = (tx-xx)*(tx-xx) + (ty-yy)*(ty-yy) + (tz-zz)*(tz-zz) ;
         if( dt > 0.0f ){ ntot++ ; dtot += dt ; }
     }}}
     if( ntot > 0 ){
       dtot = sqrt( dtot/ntot ) ;        /* RMS positive distance moved */
       if( dtot > 0.909f && dtot < 1.100f ) break ;     /* good enough! */
       dtot = 1.0f / dtot ;                   /* dpar adjustment factor */
            if( dtot > 50.0f ) dtot = 50.0f ;
       else if( dtot < 0.02f ) dtot = 0.02f ;
     } else {
       dtot = 50.0f ;                     /* no movement? how peculiar! */
     }
     dpar *= dtot ;                          /* adjust dpar, up or down */
     if( bas->verb ) fprintf(stderr," %f",dpar) ;
     nite++ ; if( nite > 9 ) break ;
   } /* end of iteration loop */

   if( bas->verb ) fprintf(stderr,"\n") ;

   bas->param[kpar].delta = dpar ;   /* save result, whatever it is */
   free((void *)pvec) ;
   return ;
}

/*--------------------------------------------------------------------*/

static MRI_IMAGE * mri_warp3D_align_fitim( MRI_warp3D_align_basis *bas ,
                                           MRI_IMAGE *cim ,
                                           int warp_mode , float delfac )
{
   MRI_IMAGE *fitim , *pim , *mim ;
   float *fitar , *car=MRI_FLOAT_PTR(cim) ;
   int nfree=bas->nfree , *ima=MRI_INT_PTR(bas->imap) , nmap=bas->imap->nx ;
   int npar =bas->nparam ;
   float *pvec , *par , *mar ;
   int ii , pp ;
   float dpar , delta ;

   /*-- create image containing basis columns --*/

   fitim = mri_new( nmap , nfree+1 , MRI_float ) ;
   fitar = MRI_FLOAT_PTR(fitim) ;
   pvec  = (float *)malloc(sizeof(float) * npar) ;

#define FMAT(i,j) fitar[(i)+(j)*nmap]  /* col dim=nmap, row dim=nfree+1 */

   /* column #nfree = base image itself */

   for( ii=0 ; ii < nmap ; ii++ ) FMAT(ii,nfree) = car[ima[ii]] ;

   pvec = (float *)malloc(sizeof(float) * npar) ;

   /* for each parameter:
       apply inverse transform to base image with param value up and down
       compute central difference to approximate derivative of base
        image wrt parameter
       store as a column in the fitim matrix */

   mri_warp3D_method( warp_mode ) ;  /* set interpolation mode */
   mri_warp3D_set_womask( bas->imsk ) ;

   for( pp=0 ; pp < npar ; pp++ ){

     if( bas->param[pp].fixed ) continue ;  /* don't do this one! */

     /* init all params to their identity transform value */

     for( ii=0 ; ii < npar ; ii++ )
       pvec[ii] = (bas->param[ii].fixed) ? bas->param[ii].val_fixed
                                         : bas->param[ii].ident ;

     /* change in the pp-th parameter to use for derivative */

     dpar = delfac * bas->param[pp].delta ;

     if( bas->verb )
       fprintf(stderr,"+   difference base by %f in param#%d [%s]\n",
               dpar , pp+1 , bas->param[pp].name ) ;

     pvec[pp] = bas->param[pp].ident + dpar ;   /* set positive change */
     bas->vwset( npar , pvec ) ;                 /* put into transform */
     pim = mri_warp3D( cim , 0,0,0 , bas->vwinv ) ;      /* warp image */

     pvec[pp] = bas->param[pp].ident - dpar ;   /* set negative change */
     bas->vwset( npar , pvec ) ;
     mim = mri_warp3D( cim , 0,0,0 , bas->vwinv ) ;

     /* compute derivative */

     delta = bas->scale_init / ( 2.0f * dpar ) ;
     par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
     for( ii=0 ; ii < nmap ; ii++ )
       FMAT(ii,pp) = delta * ( par[ima[ii]] - mar[ima[ii]] ) ;

     mri_free(pim) ; mri_free(mim) ;  /* no longer needed */
   }

   mri_warp3D_set_womask( NULL ) ;
   free((void *)pvec) ;

   return(fitim) ;
}

/*-------------------------------------------------------------------------
   Input:  pointer to a filled in MRI_warp3D_align_basis struct.
   Output: 0 if setup went OK, 1 if it failed.
           Some elements in the MRI_warp3D_align_basis struct
           will be filled in for internal use in MRI_warp3D_align_one().
           They can be freed later with  MRI_warp3D_align_cleanup().
---------------------------------------------------------------------------*/

int mri_warp3D_align_setup( MRI_warp3D_align_basis *bas )
{
   MRI_IMAGE *cim , *fitim ;
   int nx, ny, nz, nxy, nxyz , ii,jj,kk , nmap, *im ;
   float *wf , *wtar , clip , clip2 ;
   int   *ima , pp , wtproc , npar , nfree ;
   byte  *msk ;
   int ctstart ;

ENTRY("mri_warp3D_align_setup") ;

   ctstart = NI_clock_time() ;

   /*- check for good inputs -*/

   if( bas == NULL     || bas->imbase == NULL ) RETURN(1) ;
   if( bas->nparam < 1 || bas->param  == NULL ) RETURN(1) ;
   if( bas->vwfor == NULL ||
       bas->vwinv == NULL || bas->vwset == NULL ) RETURN(1) ;

   /*- set defaults in bas, if values weren't set by user -*/

   if( bas->scale_init <= 0.0f ) bas->scale_init = 1.0f ;
   if( bas->delfac     <= 0.0f ) bas->delfac     = 1.0f ;
   if( bas->regmode    <= 0    ) bas->regmode    = MRI_LINEAR ;
   if( bas->max_iter   <= 0    ) bas->max_iter   = 9 ;

   /* process the weight image? */

   wtproc = (bas->imwt == NULL) ? 1 : bas->wtproc ;
   npar   = bas->nparam ;

   nfree = npar ;
   for( pp=0 ; pp < npar ; pp++ )
     if( bas->param[pp].fixed ) nfree-- ;
   if( nfree <= 0 ) RETURN(1) ;
   bas->nfree = nfree ;

   /*- clean out anything from last call -*/

   mri_warp3D_align_cleanup( bas ) ;

   /*-- need local copy of input base image --*/

   cim = mri_to_float( bas->imbase ) ;
   nx=cim->nx ; ny=cim->ny ; nz=cim->nz ; nxy = nx*ny ; nxyz=nxy*nz ;

   /*-- make weight image up from the base image if it isn't supplied --*/

   if( bas->verb ) fprintf(stderr,"++ mri_warp3D_align_setup ENTRY\n") ;

   if( bas->imwt == NULL   ||
       bas->imwt->nx != nx ||
       bas->imwt->ny != ny ||
       bas->imwt->nz != nz   ) bas->imww = mri_copy( cim ) ;
   else                        bas->imww = mri_to_float( bas->imwt ) ;

   if( bas->twoblur > 0.0f ){
     float bmax = (float)pow((double)nxyz,0.33333333) * 0.03 ;
     if( bmax < bas->twoblur ){
       if( bas->verb )
         fprintf(stderr,"+   shrink bas->twoblur from %.3f to %.3f\n",
                        bas->twoblur , bmax ) ;
       bas->twoblur = bmax ;
     }
   }

   if( bas->verb ) fprintf(stderr,"+   processing weight:") ;

   /* make sure weight is non-negative */

   wf = MRI_FLOAT_PTR(bas->imww) ;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = fabs(wf[ii]) ;

   /* trim off edges of weight */

   if( wtproc ){
     int ff ;
     int xfade=bas->xedge , yfade=bas->yedge , zfade=bas->zedge ;

     if( xfade < 0 || yfade < 0 || zfade < 0 )
       mri_warp3D_align_edging_default(nx,ny,nz,&xfade,&yfade,&zfade) ;

     if( bas->twoblur > 0.0f ){
       xfade += (int)rint(1.5*bas->twoblur) ;
       yfade += (int)rint(1.5*bas->twoblur) ;
       zfade += (int)rint(1.5*bas->twoblur) ;
     }

     if( 3*zfade >= nz ) zfade = (nz-1)/3 ;
     if( 3*xfade >= nx ) xfade = (nx-1)/3 ;
     if( 3*yfade >= ny ) yfade = (ny-1)/3 ;

     if( bas->verb ) fprintf(stderr," [edge(%d,%d,%d)]",xfade,yfade,zfade) ;

     for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
       for( ff=0 ; ff < zfade ; ff++ )
         WW(ii,jj,ff) = WW(ii,jj,nz-1-ff) = 0.0f ;

     for( kk=0 ; kk < nz ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ff=0 ; ff < xfade ; ff++ )
         WW(ff,jj,kk) = WW(nx-1-ff,jj,kk) = 0.0f ;

     for( kk=0 ; kk < nz ; kk++ )
      for( ii=0 ; ii < nx ; ii++ )
       for( ff=0 ; ff < yfade ; ff++ )
        WW(ii,ff,kk) = WW(ii,ny-1-ff,kk) = 0.0f ;

   }

   /* spatially blur weight a little */

   if( wtproc ){
     float blur ;
     blur = 1.0f + MAX(1.5f,bas->twoblur) ;
     if( bas->verb ) fprintf(stderr," [blur(%.1f)]",blur) ;
     EDIT_blur_volume_3d( nx,ny,nz ,       1.0f,1.0f,1.0f ,
                          MRI_float , wf , blur,blur,blur  ) ;
   }

   /* get rid of low-weight voxels */

   clip  = 0.035 * mri_max(bas->imww) ;
   clip2 = 0.5*THD_cliplevel(bas->imww,0.4) ;
   if( clip2 > clip ) clip = clip2 ;
   if( bas->verb ) fprintf(stderr," [clip(%.1f)]",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] < clip ) wf[ii] = 0.0f ;

   /* keep only the largest cluster of nonzero voxels */

   { byte *mmm = (byte *)malloc( sizeof(byte)*nxyz ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] > 0.0f) ;
     THD_mask_clust( nx,ny,nz, mmm ) ;
     THD_mask_erode( nx,ny,nz, mmm ) ;  /* cf. thd_automask.c */
     THD_mask_clust( nx,ny,nz, mmm ) ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( !mmm[ii] ) wf[ii] = 0.0f ;
     free((void *)mmm) ;
   }

   if( bas->verb ) fprintf(stderr,"\n") ;

   /*-- make integer index map of weight > 0 voxels --*/

   nmap = 0 ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > 0.0f ) nmap++ ;

   if( bas->verb )
     fprintf(stderr,"+   using %d [%.3f%%] voxels\n",nmap,(100.0*nmap)/nxyz);

   if( nmap < 7*nfree+13 ){
     fprintf(stderr,"** mri_warp3D_align error: weight image too zero-ish!\n") ;
     mri_warp3D_align_cleanup( bas ) ; mri_free(cim) ;
     RETURN(1) ;
   }

   bas->imap = mri_new( nmap , 1 , MRI_int ) ;
   ima       = MRI_INT_PTR(bas->imap) ;
   bas->imsk = mri_new_conforming( bas->imww , MRI_byte ) ;
   msk       = MRI_BYTE_PTR(bas->imsk) ;
   for( ii=jj=0 ; ii < nxyz ; ii++ ){
     if( wf[ii] > 0.0f ){ ima[jj++] = ii; msk[ii] = 1; }
   }

   /* make copy of sqrt(weight), only at mapped indexes */

   wtar = (float *)malloc(sizeof(float)*nmap) ;
   for( ii=0 ; ii < nmap ; ii++ ) wtar[ii] = sqrt(wf[ima[ii]]) ;

   /*-- for parameters that don't come with a step size, find one --*/

   clip = bas->tolfac ; if( clip <= 0.0f ) clip = 0.03f ;

   for( ii=0 ; ii < npar ; ii++ ){
     if( bas->param[ii].fixed ) continue ; /* don't need this */
     if( bas->param[ii].delta <= 0.0f )
       mri_warp3D_get_delta( bas , ii ) ;  /* find step size */
     if( bas->param[ii].toler <= 0.0f ){   /* and set default tolerance */
       bas->param[ii].toler = clip * bas->param[ii].delta ;
       if( bas->verb )
         fprintf(stderr,"+   set toler param#%d [%s] = %f\n",
                 ii+1,bas->param[ii].name,bas->param[ii].toler) ;
     }
   }

   /* don't need the computed weight image anymore */

   mri_free(bas->imww) ; bas->imww = NULL ; wf = NULL ;

   /*-- create image containing basis columns, then pseudo-invert it --*/

   if( bas->verb ) fprintf(stderr,"+  Compute Derivatives of Base\n") ;
   fitim = mri_warp3D_align_fitim( bas , cim , bas->regmode , bas->delfac ) ;
   if( bas->verb ) fprintf(stderr,"+   calculate pseudo-inverse\n") ;
   bas->imps = mri_psinv( fitim , wtar ) ;
   mri_free(fitim) ;

   if( bas->imps == NULL ){  /* bad bad bad */
     fprintf(stderr,"** mri_warp3D_align error: can't invert Base matrix!\n") ;
     free((void *)wtar) ; mri_warp3D_align_cleanup( bas ) ; mri_free(cim) ;
     RETURN(1) ;
   }

   /*--- twoblur? ---*/

   if( bas->twoblur > 0.0f ){
     float *car=MRI_FLOAT_PTR(cim) ;
     float blur = bas->twoblur ;
     float bfac = blur ;
          if( bfac < 1.1234f ) bfac = 1.1234f ;
     else if( bfac > 1.3456f ) bfac = 1.3456f ;

     if( bas->verb ) fprintf(stderr,"+  Compute Derivatives of Blurred Base\n") ;
     EDIT_blur_volume_3d( nx,ny,nz ,       1.0f,1.0f,1.0f ,
                          MRI_float , car, blur,blur,blur  ) ;
     fitim = mri_warp3D_align_fitim( bas , cim , MRI_LINEAR , bfac*bas->delfac ) ;
     if( bas->verb ) fprintf(stderr,"+   calculate pseudo-inverse\n") ;
     bas->imps_blur = mri_psinv( fitim , wtar ) ;
     mri_free(fitim) ;
     if( bas->imps_blur == NULL ){  /* bad */
       fprintf(stderr,"** mri_warp3D_align error: can't invert Blur matrix!\n") ;
     }
   }

   /*--- done ---*/

   mri_free(cim) ; free((void *)wtar) ;

   if( bas->verb ){
     double st = (NI_clock_time()-ctstart) * 0.001 ;
     fprintf(stderr,"++ mri_warp3D_align_setup EXIT: %.2f seconds elapsed\n",st);
   }

   RETURN(0);
}

/*-----------------------------------------------------------------------
   Input:   basis  = MRI_warp3D_align_basis *
            im     = MRI_IMAGE * to align to base image

   Output:  Return value is aligned image.
            Note that returned image is floats.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_warp3d_align_one( MRI_warp3D_align_basis *bas, MRI_IMAGE *im )
{
   float *fit , *dfit , *qfit , *tol ;
   int iter , good,ngood , ii, pp , skip_first ;
   MRI_IMAGE *tim , *fim ;
   float *pmat=MRI_FLOAT_PTR(bas->imps) , /* pseudo inverse: n X m matrix */
         *tar , tv , sfit ;
   int n=bas->imps->nx ,          /* = nfree+1 */
       m=bas->imps->ny ,          /* = imap->nx = length of ima */
    npar=bas->nparam   ,          /* = number of warp parameters */
   nfree=bas->nfree    ,          /* = number of free warp parameters */
    *ima=MRI_INT_PTR(bas->imap) , /* = indexes in fim of voxels to use */
    *pma ;                        /* = map of free to total params */
   int ctstart ;
   int do_twopass=(bas->imps_blur != NULL && bas->twoblur > 0.0f) , passnum=1 ;
   char      *save_prefix ;
   static int save_index=0 ;

#define AITMAX  3.33
#define NMEM    5
   float *fitmem[NMEM] ;
   int mm , last_aitken , num_aitken=0 ;

ENTRY("mri_warp3D_align_one") ;

   ctstart = NI_clock_time() ;

   save_prefix = getenv("AFNI_WARPDRIVE_SAVER") ;

   pma = (int *)malloc(sizeof(int) * nfree) ;
   for( pp=ii=0 ; ii < npar ; ii++ )
     if( !bas->param[ii].fixed ) pma[pp++] = ii ;

   fit  = (float *)malloc(sizeof(float) * npar ) ;
   dfit = (float *)malloc(sizeof(float) * npar ) ;
   qfit = (float *)malloc(sizeof(float) * nfree) ;
   tol  = (float *)malloc(sizeof(float) * npar ) ;

   for( mm=0 ; mm < NMEM ; mm++ ) fitmem[mm] = NULL ;

   /*--- loop back point for two pass alignment ---*/

   bas->num_iter = 0 ;
   mri_warp3D_set_womask( bas->imsk ) ;

 ReStart:

   mri_warp3D_method( (do_twopass && passnum==1) ? MRI_LINEAR : bas->regmode ) ;

   /* load initial fit parameters;
      if they are all the identity transform value,
      then skip the first transformation of the fim volume */

   if( passnum == 1 ){
     skip_first = 1 ;
     for( pp=0 ; pp < npar ; pp++ ){
       if( bas->param[pp].fixed ){
         fit[pp] = bas->param[pp].val_fixed ;
       } else {
         fit[pp] = bas->param[pp].val_init ;
         skip_first = skip_first && (fit[pp] == bas->param[pp].ident) ;
       }
     }
   } else {
     skip_first = 0 ;  /* and fit[] is unchanged */
   }

   fitmem[0] = (float *)malloc(sizeof(float)*npar) ;
   memcpy( fitmem[0] , fit , sizeof(float)*npar) ;

   for( pp=0 ; pp < npar ; pp++ ) tol[pp] = bas->param[pp].toler ;

   if( do_twopass && passnum == 1 ){
     float fac = (1.0f+bas->twoblur) ;
     if( fac < 3.0f ) fac = 3.0f ;
     for( pp=0 ; pp < npar ; pp++ ) tol[pp] *= fac ;
   }

   if( bas->verb ) fprintf(stderr,"++ mri_warp3d_align_one: START PASS #%d\n",passnum) ;

   /* setup base image for registration into fim,
      and pseudo-inverse of base+derivative images into pmat */

   if( do_twopass && passnum==1 ){    /* first pass ==> registering blurred images */
     float *far , blur=bas->twoblur ;
     int nx=im->nx , ny=im->ny , nz=im->nz ;
     fim = mri_to_float( im ) ; far = MRI_FLOAT_PTR(fim) ;
     EDIT_blur_volume_3d( nx,ny,nz ,       1.0f,1.0f,1.0f ,
                          MRI_float , far, blur,blur,blur  ) ;
     pmat = MRI_FLOAT_PTR(bas->imps_blur) ;
   } else {                           /* registering original image */
     if( im->kind == MRI_float ) fim = im ;
     else                        fim = mri_to_float( im ) ;
     pmat = MRI_FLOAT_PTR(bas->imps) ;
   }

   /*-- iterate fit --*/

   iter = 0 ; good = 1 ; last_aitken = 3 ;
   while( good ){
     if( skip_first ){
       tim = fim ; skip_first = 0 ;
     } else {
       bas->vwset( npar , fit ) ;
       tim = mri_warp3D( fim , 0,0,0 , bas->vwfor ) ; /* warp on current params */
     }
     tar = MRI_FLOAT_PTR(tim) ;

     /* find least squares fit of base + derivatives to warped image */

     sfit = 0.0f ;
     for( pp=0 ; pp < npar  ; pp++ ) dfit[pp] = 0.0f ;
     for( pp=0 ; pp < nfree ; pp++ ) qfit[pp] = 0.0f ;
     for( ii=0 ; ii < m ; ii++ ){
       tv = tar[ima[ii]] ; sfit += P(nfree,ii) * tv ;
       for( pp=0 ; pp < nfree ; pp++ ) qfit[pp] += P(pp,ii) * tv ;
     }
     if( tim != fim ) mri_free( tim ) ;
     for( pp=0 ; pp < nfree ; pp++ ) dfit[pp] = qfit[pma[pp]] ;
     for( pp=0 ; pp < npar  ; pp++ ){
       fit[pp] += dfit[pp] ;
            if( fit[pp] > bas->param[pp].max ) fit[pp] = bas->param[pp].max ;
       else if( fit[pp] < bas->param[pp].min ) fit[pp] = bas->param[pp].min ;
     }

     if( bas->verb ){
       fprintf(stderr,"+   Delta:") ;
       for( pp=0 ; pp < npar ; pp++ ) fprintf(stderr," %13.6g",dfit[pp]) ;
       fprintf(stderr,"\n") ;
       fprintf(stderr,"+   Total: scale factor=%g\n"
                      "+  #%5d:",sfit,iter+1) ;
       for( pp=0 ; pp < npar ; pp++ ) fprintf(stderr," %13.6g", fit[pp]) ;
       fprintf(stderr,"\n") ;
     }

     /* save fit results for a while into the past, and then maybe do Aitken */

     if( fitmem[NMEM-1] != NULL ) free((void *)fitmem[NMEM-1]) ;
     for( mm=NMEM-1 ; mm > 0 ; mm-- ) fitmem[mm] = fitmem[mm-1] ;
     fitmem[0] = (float *)malloc(sizeof(float)*npar) ;
     memcpy( fitmem[0] , fit , sizeof(float)*npar) ;

     iter++ ;
     if( iter > last_aitken+NMEM && !AFNI_noenv("AFNI_WARPDRIVE_AITKEN") ){
       double s0,s1,s2 , dd , de,df ;
       num_aitken = 0 ;
       for( pp=0 ; pp < npar ; pp++ ){
         dd = fabs(fitmem[1][pp]-fit[pp]) ;
         if( dd <= tol[pp] ) continue ; /* done here */
         de = dd ;
         for( mm=2 ; mm < NMEM ; mm++ ){
           df = fabs(fitmem[mm][pp]-fitmem[mm-1][pp]) ;
           if( df <= de ) break ;
           de = df ;
         }
         if( mm == NMEM ){  /* do Aitken */
           s2 = fit[pp] ; s1 = fitmem[1][pp] ; s0 = fitmem[2][pp] ;
           de = ( (s2-s1) - (s1-s0) ) ;
           if( de != 0.0 ){
             de = -(s2-s1)*(s2-s1) / de ; dd *= AITMAX ;
             if( fabs(de) > dd ){ de = (de > 0.0) ? dd : -dd ; }
             fit[pp] += de ;
                  if( fit[pp] > bas->param[pp].max ) fit[pp] = bas->param[pp].max ;
             else if( fit[pp] < bas->param[pp].min ) fit[pp] = bas->param[pp].min ;
             num_aitken++ ;
           }
         }
       }
       if( num_aitken > 0 ) last_aitken = iter ;

       if( bas->verb && num_aitken > 0 ){
         fprintf(stderr,"+   Aitken on %d params:\n"
                        "+________:",num_aitken) ;
         for( pp=0 ; pp < npar ; pp++ ){
           if( fit[pp] != fitmem[0][pp] ){
             fprintf(stderr," %13.6g", fit[pp]) ; fitmem[0][pp] = fit[pp] ;
           } else {
             fprintf(stderr," _____________") ;
           }
         }
         fprintf(stderr,"\n") ;
       }
     }

     /* save intermediate result? */

     if( save_prefix != NULL ){
       char sname[THD_MAX_NAME] ; FILE *fp ;
       mri_warp3D_set_womask( NULL ) ;
       bas->vwset( npar , fit ) ;
       tim = mri_warp3D( fim , 0,0,0 , bas->vwfor ) ;
       mri_warp3D_set_womask( bas->imsk ) ;
       sprintf(sname,"%s_%04d.mmm",save_prefix,save_index++) ;
       fprintf(stderr,"+   Saving intermediate image to binary file %s\n",sname) ;
       fp = fopen( sname , "w" ) ;
       if( fp != NULL ){
         tar = MRI_FLOAT_PTR(tim) ;
         fwrite( tar, tim->pixel_size, tim->nvox, fp ) ; fclose(fp) ;
       }
       mri_free( tim ) ;
     }

     /* loop back for more iterations? */

     if( last_aitken == iter ) continue ;  /* don't test, just loop */
     if( fitmem[2]   == NULL ) continue ;

     ngood = 0 ;
     for( pp=0 ; pp < npar ; pp++ )
       if( !bas->param[pp].fixed )
         ngood += ( ( fabs(fitmem[1][pp]-fitmem[0][pp]) <= tol[pp] ) &&
                    ( fabs(fitmem[2][pp]-fitmem[1][pp]) <= tol[pp] )   ) ;

     good = (ngood < nfree) && (iter < bas->max_iter) ;

   } /* end while */

   bas->num_iter += iter ;

   for( mm=0 ; mm < NMEM ; mm++ )
     if( fitmem[mm] != NULL ){ free((void *)fitmem[mm]); fitmem[mm] = NULL; }

   /*--- do the second pass? ---*/

   if( do_twopass && passnum == 1 ){
     if( bas->verb )
       fprintf(stderr,"+++++++++++++ Loop back for next pass +++++++++++++\n");
     mri_free(fim) ; fim = NULL ; passnum++ ; goto ReStart ;
   } else {
     if( bas->verb )
       fprintf(stderr,"+++++++++++++ Convergence test passed +++++++++++++\n");
   }

   /*--- done! ---*/

   for( pp=0 ; pp < npar ; pp++ ) bas->param[pp].val_out = fit[pp] ;

   /*-- do the actual realignment to get the output image --*/

   if( bas->regfinal > 0 ) mri_warp3D_method( bas->regfinal ) ;
   mri_warp3D_set_womask( NULL ) ;
   bas->vwset( npar , fit ) ;
   tim = mri_warp3D( fim , 0,0,0 , bas->vwfor ) ;

   if( fim != im ) mri_free(fim) ;  /* if it was a copy, junk it */
   free((void *)dfit) ; free((void *)fit) ;
   free((void *)qfit) ; free((void *)pma) ; free((void *)tol) ;

   if( bas->verb ){
     double st = (NI_clock_time()-ctstart) * 0.001 ;
     fprintf(stderr,"++ mri_warp3d_align_one EXIT: %.2f seconds elapsed\n",st) ;
   }

   RETURN( tim ) ;
}

/*--------------------------------------------------------------------*/

void mri_warp3D_align_cleanup( MRI_warp3D_align_basis *bas )
{
   if( bas == NULL ) return ;
   if( bas->imww != NULL ){ mri_free(bas->imww) ; bas->imww = NULL ; }
   if( bas->imap != NULL ){ mri_free(bas->imap) ; bas->imap = NULL ; }
   if( bas->imps != NULL ){ mri_free(bas->imps) ; bas->imps = NULL ; }
   if( bas->imsk != NULL ){ mri_free(bas->imsk) ; bas->imsk = NULL ; }

   if( bas->imps_blur != NULL ){ mri_free(bas->imps_blur) ; bas->imps_blur = NULL ; }
}
