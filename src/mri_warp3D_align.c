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
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,sum,ww ;
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
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ;
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

   if( smax <= 0.0 ){                        /* this is bad */
     free((void *)xfac); free((void *)sval);
     free((void *)vmat); free((void *)umat); return NULL;
   }

   for( ii=0 ; ii < n ; ii++ )
     if( sval[ii] < 0.0 ) sval[ii] = 0.0 ;

#define PSINV_EPS 1.e-8

   /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

   del = PSINV_EPS * smax*smax ;
   for( ii=0 ; ii < n ; ii++ )
     sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

   /* create pseudo-inverse */

#define P(i,j) pmat[(i)+(j)*n]

   imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
   pmat = MRI_FLOAT_PTR(imp) ;

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < n ; kk++ )
         sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
       P(ii,jj) = (float)( sum * xfac[ii] ) ;  /* rescale rows */
     }
   }

   /* rescale cols for weight? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
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

   /* load parameter vector with the identity value for all params */

   pvec = (float *)malloc(sizeof(float) * bas->nparam) ;
   for( ii=0 ; ii < bas->nparam ; ii++ )
     pvec[ii] = bas->param[ii].ident ;

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
       if( dtot > 0.618f && dtot < 1.618f ) break ;     /* good enough! */
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

/*-------------------------------------------------------------------------
   Input:  pointer to a filled in MRI_warp3D_align_basis struct.
   Output: 0 if setup went OK, 1 if it failed.
           Some elements in the MRI_warp3D_align_basis struct
           will be filled in for internal use in MRI_warp3D_align_one().
           They can be freed later with  MRI_warp3D_align_cleanup().
---------------------------------------------------------------------------*/

int mri_warp3D_align_setup( MRI_warp3D_align_basis *bas )
{
   MRI_IMAGE *pim , *mim , *cim , *fitim ;
   double * chol_fitim=NULL ;
   int nx, ny, nz, nxy, nxyz , ii,jj,kk , nmap, *im ;
   float *wf , *wtar , *fitar , *car , *pvec , dpar , clip,delta ;
   float *par , *mar ;
   int   *ima , pp , wtproc , npar , nfree ;
   byte  *msk ;

ENTRY("mri_warp3D_align_setup") ;

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
       bas->imwt->nz != nz   ) bas->imww = mri_to_float( cim ) ;
   else                        bas->imww = mri_to_float( bas->imwt ) ;

   if( bas->verb ) fprintf(stderr,"+   processing weight") ;

   /* make sure weight is non-negative */

   wf = MRI_FLOAT_PTR(bas->imww) ;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = fabs(wf[ii]) ;

   if( bas->verb ) fprintf(stderr,".") ;

   /* spatially blur weight a little */

   if( wtproc ){
     EDIT_blur_volume_3d( nx,ny,nz ,       1.0f,1.0f,1.0f ,
                          MRI_float , wf , 3.0f,3.0f,3.0f  ) ;
     if( bas->verb ) fprintf(stderr,".") ;
   }

   /* get rid of low-weight voxels */

   clip = 0.025 * mri_max(bas->imww) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] < clip ) wf[ii] = 0.0f ;

   if( bas->verb ) fprintf(stderr,".") ;

   /* clip off edges of weight */

   if( wtproc ){
     int ff ;
     int xfade=bas->xedge , yfade=bas->yedge , zfade=bas->zedge ;

     if( xfade < 0 || yfade < 0 || zfade < 0 )
       mri_warp3D_align_edging_default(nx,ny,nz,&xfade,&yfade,&zfade) ;

     if( 2*zfade >= nz ) zfade = (nz-1)/2 ;
     for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
       for( ff=0 ; ff < zfade ; ff++ )
         WW(ii,jj,ff) = WW(ii,jj,nz-1-ff) = 0.0f ;

     if( 2*xfade >= nx ) xfade = (nx-1)/2 ;
     for( kk=0 ; kk < nz ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ff=0 ; ff < xfade ; ff++ )
         WW(ff,jj,kk) = WW(nx-1-ff,jj,kk) = 0.0f ;

     if( 2*yfade >= ny ) yfade = (ny-1)/2 ;
     for( kk=0 ; kk < nz ; kk++ )
      for( ii=0 ; ii < nx ; ii++ )
       for( ff=0 ; ff < yfade ; ff++ )
        WW(ii,ff,kk) = WW(ii,ny-1-ff,kk) = 0.0f ;

     if( bas->verb ) fprintf(stderr,".") ;
   }

   /* get rid of isolated voxels */

   for( kk=1 ; kk < nz-1 ; kk++ ){
    for( jj=1 ; jj < ny-1 ; jj++ ){
     for( ii=1 ; ii < nx-1 ; ii++ ){
       if( WW(ii  ,jj  ,kk  ) >  0.0f &&
           WW(ii-1,jj  ,kk  ) == 0.0f &&
           WW(ii+1,jj  ,kk  ) == 0.0f &&
           WW(ii  ,jj+1,kk  ) == 0.0f &&
           WW(ii  ,jj-1,kk  ) == 0.0f &&
           WW(ii  ,jj  ,kk+1) == 0.0f &&
           WW(ii  ,jj  ,kk-1) == 0.0f   ) WW(ii,jj,kk) = 0.0f ;
   }}}

   if( bas->verb ) fprintf(stderr,".\n") ;

   /*-- make integer index map of weight > 0 voxels --*/

   nmap = 0 ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > 0.0f ) nmap++ ;

   if( bas->verb )
     fprintf(stderr,"+   using %d [%.3f%%] voxels\n",nmap,(100.0*nmap)/nxyz);

   if( nmap < 2*nfree+2 ){
     fprintf(stderr,"** warp3D_align error: weight image is zero!\n") ;
     mri_warp3D_align_cleanup( bas ) ;
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

   for( ii=0 ; ii < npar ; ii++ ){
     if( bas->param[ii].fixed ) continue ; /* don't need this */
     if( bas->param[ii].delta <= 0.0f )
       mri_warp3D_get_delta( bas , ii ) ;
     if( bas->param[ii].toler <= 0.0f ){   /* and set default tolerance */
       bas->param[ii].toler = 0.03f * bas->param[ii].delta ;
       if( bas->verb )
         fprintf(stderr,"+   set toler param#%d [%s] = %f\n",
                 ii+1,bas->param[ii].name,bas->param[ii].toler) ;
     }
   }

   /* don't need the computed weight image anymore */

   mri_free(bas->imww) ; bas->imww = NULL ; wf = NULL ;

   /*-- create image containing basis columns --*/

   fitim = mri_new( nmap , nfree + 1 , MRI_float ) ;
   fitar = MRI_FLOAT_PTR(fitim) ; car = MRI_FLOAT_PTR(cim) ;

#define FMAT(i,j) fitar[(i)+(j)*nmap]  /* col dim=nmap, row dim=nfree+1 */

   /* column #nfree = base image itself */

   for( ii=0 ; ii < nmap ; ii++ ) FMAT(ii,nfree) = car[ima[ii]] ;

   pvec = (float *)malloc(sizeof(float) * npar) ;

   /* for each parameter:
       apply inverse transform to base image with param value up and down
       compute central difference to approximate derivative of base
        image wrt parameter
       store as a column in the fitim matrix */

   mri_warp3D_method( bas->regmode ) ;  /* set interpolation mode */
   mri_warp3D_set_womask( bas->imsk ) ;

   for( pp=0 ; pp < npar ; pp++ ){

     if( bas->param[pp].fixed ) continue ;  / * don't do this one! */

     /* init all params to their identity transform value */

     for( ii=0 ; ii < npar ; ii++ ){
       if( bas->param[ii].fixed )
         pvec[ii] = bas->param[ii].val_fixed ;
       else
         pvec[ii] = bas->param[ii].ident ;
     }

     /* change in the pp-th parameter to use for derivative */

     dpar = bas->delfac * bas->param[pp].delta ;

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

   free((void *)pvec) ; mri_free(cim) ;  /* more trashola */
   mri_warp3D_set_womask( NULL ) ;

   /*-- setup linear least squares --*/

   if( bas->verb ) fprintf(stderr,"+   compute pseudo-inverse\n") ;

   bas->imps = mri_psinv( fitim , wtar ) ;

   free((void *)wtar) ; mri_free(fitim) ;

   if( bas->imps == NULL ){  /* bad bad bad */
     fprintf(stderr,"** warp3D_align error: can't invert matrix!\n") ;
     mri_warp3D_align_cleanup( bas ) ;
     RETURN(1) ;
   }

   if( bas->verb ) fprintf(stderr,"++ mri_warp3D_align_setup EXIT\n") ;
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
   float *fit , *dfit ;
   int iter , good,ngood , ii , pp , skip_first ;
   MRI_IMAGE *tim , *fim ;
   float *pmat=MRI_FLOAT_PTR(bas->imps) , /* pseudo inverse: n X m matrix */
         *tar , tv , sfit ;
   int n=bas->imps->nx ,          /* = nfree+1 */
       m=bas->imps->ny ,          /* = imap->nx = length of ima */
    npar=bas->nparam   ,          /* = number of warp parameters */
   nfree=bas->nfree    ,          /* = number of free warp parameters */
    *ima=MRI_INT_PTR(bas->imap) ; /* = indexes in fim of voxels to use */

ENTRY("mri_warp3D_align_one") ;

   /* use original image if possible */

   if( im->kind == MRI_float ) fim = im ;
   else                        fim = mri_to_float( im ) ;

   mri_warp3D_method( bas->regmode ) ;
   mri_warp3D_set_womask( bas->imsk ) ;

   fit  = (float *)malloc(sizeof(float) * npar) ;  /* nfree? */
   dfit = (float *)malloc(sizeof(float) * npar) ;

   /* load initial fit parameters;
      if they are all the identity transform value,
      then skip the first transformation of the fim volume */

   skip_first = 1 ;
   for( pp=0 ; pp < npar ; pp++ ){
     fit[pp] = bas->param[pp].val_init ;
     skip_first = skip_first && (fit[pp] == bas->param[pp].ident) ;
   }

   if( bas->verb ) fprintf(stderr,"++ mri_warp3d_align_one ENTRY\n") ;

   /*-- iterate fit --*/

   iter = 0 ; good = 1 ;
   while( good ){
     if( skip_first ){
       tim = fim ; skip_first = 0 ;
     } else {
       bas->vwset( npar , fit ) ;
       tim = mri_warp3D( fim , 0,0,0 , bas->vwfor ) ;
     }
     tar = MRI_FLOAT_PTR(tim) ;

     sfit = 0.0f ;
     for( pp=0 ; pp < npar ; pp++ ) dfit[pp] = 0.0f ;
     for( ii=0 ; ii < m ; ii++ ){
       tv = tar[ima[ii]] ; sfit += P(npar,ii) * tv ;
       for( pp=0 ; pp < npar ; pp++ ) dfit[pp] += P(pp,ii) * tv ;
     }
     if( tim != fim ) mri_free( tim ) ;

     for( pp=0 ; pp < npar ; pp++ ) fit[pp] += dfit[pp] ;

     if( bas->verb ){
       fprintf(stderr,"+   Delta:") ;
       for( pp=0 ; pp < npar ; pp++ ) fprintf(stderr," %13.6g",dfit[pp]) ;
       fprintf(stderr,"\n") ;
       fprintf(stderr,"+   Total: scale factor=%g\n+        :",sfit) ;
       for( pp=0 ; pp < npar ; pp++ ) fprintf(stderr," %13.6g", fit[pp]) ;
       fprintf(stderr,"\n") ;
      }

      iter++ ; ngood = 0 ;
      for( pp=0 ; pp < npar ; pp++ )
        ngood += ( fabs(dfit[pp]) <= bas->param[pp].toler ) ;
      good = (ngood < npar) && (iter < bas->max_iter) ;

   } /* end while */

   bas->num_iter = iter ;
   for( pp=0 ; pp < npar ; pp++ ) bas->param[pp].val_out = fit[pp] ;

   if( bas->verb ) fprintf(stderr,"++ mri_warp3d_align_one EXIT: %d steps\n",iter) ;

   /*-- do the actual realignment --*/

   mri_warp3D_set_womask( NULL ) ;
   bas->vwset( npar , fit ) ;
   tim = mri_warp3D( fim , 0,0,0 , bas->vwfor ) ;

   if( fim != im ) mri_free(fim) ;  /* if it was a copy, junk it */
   free((void *)dfit) ; free((void *)fit) ;

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
}
