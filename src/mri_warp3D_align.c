#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*************************************************************************
 **   warp3D version of mri_3dalign.c -- RWCox -- November 2004         **
 *************************************************************************/

/*-----------------------------------------------------------------------*/

static void MRI_IMAGE * mri_psinv( MRI_IMAGE *imc , float *wt )
{
   float *rmat = MRI_FLOAT_PTR(imc) , ww ;
   int m=imc->nx , n=imc->ny , ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,sum ;
   MRI_IMAGE *imp ; float *pmat ;

   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   umat = (double *)calloc( sizeof(double),m*n ) ;  /* left singular vectors */
   vmat = (double *)calloc( sizeof(double),n*n ) ;  /* right singular vectors */
   sval = (double *)calloc( sizeof(double),n   ) ;  /* singular values */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */

#define R(i,j) rmat[(i)+(j)*m]
#define A(i,j) amat[(i)+(j)*m]
#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]
#define P(i,j) pmat[(i)+(j)*n]

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = R(ii,jj) ;

   /* weight rows? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = sqrt(wt[ii]) ;
       for( jj=0 ; jj < n ; jj++ ) A(ii,jj) *= ww ;
     }
   }

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     sum = 0.0l ;
     for( ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0l ) sum = 1.0l/sqrt(sum) ;
     xfac[jj] = sum ;
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
   }

   /* compute SVD of scaled matrix */

   svd_double( m , n , amat , sval , umat , vmat ) ;

   free((void *)amat) ;  /* done with this */

   /* find largest singular value */

   smax = sval[0] ;
   for( ii=1 ; ii < n ; ii++ )
     if( sval[ii] > smax ) smax = sval[ii] ;

   if( smax <= 0.0l ){                        /* this is bad */
     free((void *)xfac); free((void *)sval);
     free((void *)vmat); free((void *)umat); return;
   }

   for( ii=0 ; ii < n ; ii++ )
     if( sval[ii] < 0.0l ) sval[ii] = 0.0l ;

#define PSINV_EPS 1.e-8

   /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

   del = PSINV_EPS * smax*smax ;
   for( ii=0 ; ii < n ; ii++ )
     sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

   /* create pseudo-inverse */

   imp  = mri_new( n , m , MRI_float ) ;
   pmat = MRI_FLOAT_PTR(imp) ;

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ){
       sum = 0.0l ;
       for( kk=0 ; kk < n ; kk++ )
         sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
       P(ii,jj) = (float)( sum * xfac[ii] ) ;  /* rescale rows */
     }
   }

   /* rescale cols for weight? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = sqrt(wt[ii]) ;
       for( jj=0 ; jj < n ; jj++ ) P(jj,ii) *= ww ;
     }
   }

   free((void *)xfac); free((void *)sval);
   free((void *)vmat); free((void *)umat); return imp;
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
     if( ff < 0 ){                   /* again */
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

static void mri_warp3D_get_delta( MRI_warp3D_align_basis *bas , int kpar )
{
   float *pvec , dpar ;
   int   ii,jj,kk , nx,ny,nz,nxy , nite , ntot ;
   float xx,yy,zz , fx,fy,fz , df,db , dtot , bx,by,bz ;
   float *wf ;

   if( bas == NULL || kpar < 0 || kpar >= bas->nparam ) return ;

   pvec = (float *)malloc(sizeof(float) * bas->nparam) ;
   for( ii=0 ; ii < bas->nparam ; ii++ )
     pvec[ii] = bas->param[ii].ident ;

   nx = bas->imbase->nx ; ny = bas->imbase->ny ; nz = bas->imbase->nz ;
   nxy = nx*ny ;

   dpar = 0.001 * ( fabs(bas->param[kpar].ident) + 1.0 ) ;
   nite = 0 ; wf = MRI_FLOAT_PTR(bas->imww) ;

   while(1){
     pvec[kpar] = bas->param[kpar].ident + dpar ;
     bas->vwset( bas->nparam , pvec ) ;
     ntot = 0 ; dtot = 0.0f ;
     for( kk=0 ; kk < nz ; kk++ ){
      zz = (float)kk ;
      for( jj=0 ; jj < ny ; jj++ ){
       yy = (float)jj ;
       for( ii=0 ; ii < nx ; ii++ ){
         if( WW(ii,jj,kk) == 0.0f ) continue ;  /* not counted */
         xx = (float)ii ;

         bas->vwfor( xx,yy,zz , &fx,&fy,&fz ) ;
         df = (fx-xx)*(fx-xx) + (fy-yy)*(fy-yy) + (fz-zz)*(fz-zz) ;
         if( df > 0.0f ){ ntot++ ; dtot += df ; }

         bas->vwinf( xx,yy,zz , &bx,&by,&bz ) ;
         db = (bx-xx)*(bx-xx) + (by-yy)*(by-yy) + (bz-zz)*(bz-zz) ;
         if( db > 0.0f ){ ntot++ ; dtot += db ; }
     }}}
     if( ntot > 0 ){
       dtot = sqrt( dtot/ntot ) ;  /* RMS nonzero distance moved */
       if( dtot > 0.618f && dtot < 1.618f ) break ;
       dtot = 1.0f / dtot ;
            if( dtot > 50.0f ) dtot = 50.0f ;
       else if( dtot < 0.02f ) dtot = 0.02f ;
     } else {
       dtot = 50.0f ;
     }
     dpar *= dtot ; nite++ ;
     if( nite > 9 ){
       fprintf(stderr,
               "**  Can't auto-find delta for warp3D_align param #%d [%s]\n",
               kpar+1,bas->param[kpar].name) ;
       break ;
     }
   }

   bas->param[kpar].delta = dpar ;
   free((void *)pvec) ;
   return ;
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
   MRI_IMAGE *pim , *mim , *dim , *imww , *cim , *fitim ;
   double * chol_fitim=NULL ;
   int nx, ny, nz, nxy, nxyz , ii,jj,kk , nmap, *im ;
   float *wf , *wtar , *fitar , *car , *pvec , dpar ;
   int   *ima , pp ;

ENTRY("mri_warp3D_align_setup") ;

   if( bas == NULL     || bas->imbase == NULL ) RETURN(1) ;
   if( bas->nparam < 1 || bas->param  == NULL ) RETURN(1) ;
   if( bas->vwfor == NULL ||
       bas->vwinv == NULL || bas->vwset == NULL ) RETURN(1) ;

   if( bas->scale_init <= 0.0f ) bas->scale_init = 1.0f ;
   if( bas->regmode    <= 0    ) bas->regmode    = MRI_LINEAR ;
   if( bas->max_iter   <= 0    ) bas->max_iter   = 9 ;

   if( bas->imww != NULL ){ mri_free(bas->imww) ; bas->imww = NULL ; }
   if( bas->imww != NULL ){ mri_free(bas->imap) ; bas->imap = NULL ; }
   if( bas->imps != NULL ){ mri_free(bas->imps) ; bas->imps = NULL ; }

   /*-- local copy of input image --*/

   cim = mri_to_float( bas->imbase ) ; car = MRI_FLOAT_PTR(cim) ;
   nx=cim->nx ; ny=cim->ny ; nz=cim->nz ; nxy = nx*ny ; nxyz=nxy*nz ;

   /*-- make weight up from the base if it isn't supplied --*/

   if( bas->imwt == NULL ) imww = mri_to_float( cim )
   else                    imww = mri_to_float( bas->imwt ) ;
   bas->imww = imww ;

   /* make sure weight is non-negative */

   wf = MRI_FLOAT_PTR(imww) ;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = fabs(f[ii]) ;

   /* blur made-up weight */

   if( bas->imwt == NULL )
     EDIT_blur_volume_3d( nx,ny,nz ,       1.0f,1.0f,1.0f ,
                          MRI_float , wf , 3.0f,3.0f,3.0f  ) ;

   /* get rid of low-weight voxels (made-up or not) */

   clip = 0.025 * mri_max(imww) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] < clip ) wf[ii] = 0.0f ;

   /* clip off edges of made-up weight */

   if( bas->imwt == NULL ){
     int ff ;
     int xfade=bas->xedge , yfade=bas->yedge , zfade=bas->zedge ;

     if( xfade < 0 || yfade < 0 || zfade < 0 )
       mri_warp3D_align_edging_default(nx,ny,nz,&xfade,&yfade,&zfade) ;

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

   /*-- for parameters that don't come with a step size, find one --*/

   for( ii=0 ; ii < bas->nparam ; ii++ ){
     if( bas->param[ii].delta <= 0.0f )
       mri_warp3D_get_delta( bas , ii ) ;
     if( bas->param[ii].toler <= 0.0f )
       bas->param[kpar].toler = 0.02f * bas->param[ii].delta ;
   }

   /*-- make integer index map of wt > 0 voxels --*/

   nmap = 0 ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > 0.0f ) nmap++ ;
   if( nmap < 2*bas->nparam ){
     fprintf(stderr,"** warp3D_align error: weight image is zero!\n") ;
     mri_free(bas->imww) ; bas->imww = NULL ;
     RETURN(1) ;
   }

   bas->imap = mri_new( nmap , 1 , MRI_int ) ;
   ima       = MRI_INT_PTR(bas->imap) ;
   for( ii=jj=0 ; ii < nxyz ; ii++ ) if( wf[ii] > 0.0f ) ima[jj++] = ii ;

   wtar = (float *)malloc(sizeof(float)*nmap) ;
   for( ii=0 ; ii < nmap ; ii++ ) wtar[ii] = wf[ima[ii]] ;

   mri_free(bas->imww) ; bas->imww = imww = NULL ; wf = NULL ;

   /*-- create image containing basis columns --*/

   fitim = mri_new( nmap , bas->nparam + 1 , MRI_float ) ;
   fitar = MRI_FLOAT_PTR(fitim) ;

#define FMAT(i,j) fitar[(i)+(j)*nmap]

   for( ii=0 ; ii < nmap ; ii++ ) FMAT(ii,0) = car[ima[ii]] ;

   mri_warp3D_method( bas->regmode ) ;

   pvec = (float *)malloc(sizeof(float) * bas->nparam) ;

   for( pp=0 ; pp < bas->nparam ; pp++ ){

     for( ii=0 ; ii < bas->nparam ; ii++ ) pvec[ii] = bas->param[ii].ident ;

     dpar = bas->delfac * bas->param[pp].delta ;
     pvec[pp] = bas->param[pp].ident + dpar ;
     bas->vwset( bas->nparam , pvec ) ;
     pim = mri_warp3D( cim , 0,0,0 , bas->vwinv ) ;

     pvec[pp] = bas->param[pp].ident - dpar ;
     bas->vwset( bas->nparam , pvec ) ;
     mim = mri_warp3D( cim , 0,0,0 , bas->vwinv ) ;

     delta = bas->scale_init / ( 2.0f * dpar ) ;
     par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
     for( ii=0 ; ii < nmap ; ii++ )
       FMAT(ii,pp+1) = delta * ( par[ima[ii]] - mar[ima[ii]] ) ;
     mri_free(pim) ; mri_free(mim) ;
   }

   free((void *)pvec) ; mri_free(cim) ;

   /*-- initialize linear least squares --*/

   bas->imps = mri_psinv( fitim , wtar ) ;

   free((void *)wtar) ;

   RETURN(0);
}

/*-----------------------------------------------------------------------
   Input:   basis  = MRI_3dalign_basis * return from setup routine above.
            im     = MRI_IMAGE * to align to base image

   Output:  Return value is aligned image;
            *dx, *dy, *dz, *th1, *th2, *th3 are set to estimated
            alignment parameters.  Note that returned image is floats.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_3dalign_one( MRI_3dalign_basis * basis , MRI_IMAGE * im ,
                             float *th1 , float *th2 , float *th3 ,
                             float *dx  , float *dy  , float *dz    )
{
   MRI_IMARR * fitim ;
   double * chol_fitim=NULL ;
   float * fit , *dfit ;
   int iter , good , ii ;
   float dxt , dyt , dzt , ftop,fbot ;
   MRI_IMAGE * tim , * fim ;

ENTRY("mri_3dalign_one") ;

   fitim      = basis->fitim ;
   chol_fitim = basis->chol_fitim ;

   /* use original image if possible */

   if( im->kind == MRI_float ) fim = im ;
   else                        fim = mri_to_float( im ) ;

   iter = 0 ;

   THD_rota_method( regmode ) ;

   /* convert displacement threshold from voxels to mm in each direction */

   dxt = (im->dx != 0.0) ? (fabs(im->dx) * dxy_thresh) : dxy_thresh ;
   dyt = (im->dy != 0.0) ? (fabs(im->dy) * dxy_thresh) : dxy_thresh ;
   dzt = (im->dz != 0.0) ? (fabs(im->dz) * dxy_thresh) : dxy_thresh ;

   if( NONZERO_INITVALS ){                                    /* 04 Sep 2000 */
      fit = (float *) malloc(sizeof(float)*7) ;
      fit[0] = 1.0 ;
      fit[1] = init_dth1; fit[2] = init_dth2; fit[3] = init_dth3; /* degrees */
      fit[4] = init_dx  ; fit[5] = init_dy  ; fit[6] = init_dz  ; /* mm      */

      good = 1 ;
   } else {

      /* 06 Jun 2002: do initial fit with trimmed image, if ordered */

      if( DOTRIM ){
        tim = IMTRIM(fim) ;
        fit = mri_delayed_lsqfit( tim , fitim , chol_fitim ) ;
        mri_free( tim ) ;
      } else {                                /* L2 fit input image */
        fit = mri_delayed_lsqfit( fim , fitim , chol_fitim ) ;
      }

      good = ( 10.0*fabs(fit[4]) > dxt        || 10.0*fabs(fit[5]) > dyt        ||
               10.0*fabs(fit[6]) > dzt        || 10.0*fabs(fit[1]) > phi_thresh ||
               10.0*fabs(fit[2]) > phi_thresh || 10.0*fabs(fit[3]) > phi_thresh   ) ;
   }

   if( verbose )
      fprintf(stderr,
             "\nFirst fit: %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g\n",
             fit[0] , fit[1] , fit[2] , fit[3] , fit[4] , fit[5] , fit[6] ) ;

   /*-- iterate fit --*/

   while( good ){
      tim = THD_rota3D( fim ,
                        ax1,fit[1]*DFAC , ax2,fit[2]*DFAC , ax3,fit[3]*DFAC ,
                        dcode , fit[4],fit[5],fit[6] ) ;

      TRIM(tim) ; /* 06 Jun 2002: trim it if ordered to */

      dfit = mri_delayed_lsqfit( tim , fitim , chol_fitim ) ; /* delta angle/shift */
      mri_free( tim ) ;

      fit[1] += dfit[1] ; fit[2] += dfit[2] ; fit[3] += dfit[3] ;  /* accumulate  */
      fit[4] += dfit[4] ; fit[5] += dfit[5] ; fit[6] += dfit[6] ;  /* angle/shift */

      if( verbose ){
         fprintf(stderr,
                 "Delta fit: %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g\n",
                 dfit[0], dfit[1], dfit[2], dfit[3], dfit[4], dfit[5], dfit[6] ) ;
         fprintf(stderr,
                 "Total fit: %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g\n",
                 dfit[0], fit[1], fit[2], fit[3], fit[4], fit[5], fit[6] ) ;
      }

      good = (++iter < max_iter) &&
             ( fabs(dfit[4]) > dxt        || fabs(dfit[5]) > dyt        ||
               fabs(dfit[6]) > dzt        || fabs(dfit[1]) > phi_thresh ||
               fabs(dfit[2]) > phi_thresh || fabs(dfit[3]) > phi_thresh   ) ;

      free(dfit) ; dfit = NULL ;
   } /* end while */

   if( verbose ) fprintf(stderr,"Iteration complete at %d steps\n",iter) ;

   /*-- save final alignment parameters --*/

   if( th1 != NULL ) *th1 = fit[1]*DFAC ;  /* convert to radians */
   if( th2 != NULL ) *th2 = fit[2]*DFAC ;
   if( th3 != NULL ) *th3 = fit[3]*DFAC ;
   if( dx  != NULL ) *dx  = fit[4] ;
   if( dy  != NULL ) *dy  = fit[5] ;
   if( dz  != NULL ) *dz  = fit[6] ;

   /*-- do the actual realignment --*/

   if( ! noreg ){
      if( final_regmode < 0 ) final_regmode = regmode ;  /* 20 Nov 1998 */
      THD_rota_method( final_regmode ) ;
      tim = THD_rota3D( fim ,
                        ax1,fit[1]*DFAC , ax2,fit[2]*DFAC , ax3,fit[3]*DFAC ,
                        dcode , fit[4],fit[5],fit[6] ) ;
   } else {
      tim = NULL ;
   }

   if( tim != NULL && clipit &&
       (final_regmode == MRI_QUINTIC || final_regmode==MRI_CUBIC  ||
        final_regmode == MRI_HEPTIC  || final_regmode==MRI_FOURIER  ) ){

      register int ii ;
      register float ftop , fbot , * tar ;

      ftop = mri_max( fim ); fbot = mri_min( fim );
      tar  = MRI_FLOAT_PTR(tim) ;
      for( ii=0 ; ii < tim->nvox ; ii++ ){
              if( tar[ii] < fbot ) tar[ii] = fbot ;
         else if( tar[ii] > ftop ) tar[ii] = ftop ;
      }
   }

   if( fim != im ) mri_free(fim) ;  /* if it was a copy, junk it */

   RETURN( tim );  /* 10-4, good buddy */
}

/*--------------------------------------------------------------------*/

MRI_IMARR * mri_3dalign_many( MRI_IMAGE * im , MRI_IMAGE * imwt , MRI_IMARR * ims ,
                              float *th1 , float *th2 , float *th3 ,
                              float *dx  , float *dy  , float *dz   )
{
   int kim ;
   MRI_IMAGE * tim ;
   MRI_IMARR * alim ;
   MRI_3dalign_basis * basis ;

ENTRY("mri_3dalign_many") ;

   basis = mri_3dalign_setup( im , imwt ) ;
   if( basis == NULL ) RETURN( NULL );

   INIT_IMARR( alim ) ;

#define PK(x) ( (x!=NULL) ? (x+kim) : NULL )

   for( kim=0 ; kim < ims->num ; kim++ ){
      tim = mri_3dalign_one( basis , ims->imarr[kim] ,
                             PK(th1), PK(th2), PK(th3),
                             PK(dx) , PK(dy) , PK(dz)  ) ;
      ADDTO_IMARR(alim,tim) ;
   }

   mri_3dalign_cleanup( basis ) ;
   RETURN( alim );
}

/*--------------------------------------------------------------------*/

void mri_3dalign_cleanup( MRI_3dalign_basis * basis )
{
ENTRY("mri_3dalign_cleanup") ;
   if( basis == NULL ) EXRETURN ;

   if( basis->fitim      != NULL ){ DESTROY_IMARR( basis->fitim ) ; }
   if( basis->chol_fitim != NULL ){ free(basis->chol_fitim) ; }

   free(basis) ; EXRETURN ;
}
