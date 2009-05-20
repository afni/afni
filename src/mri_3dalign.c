/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*************************************************************************
 **       3D version of mri_2dalign.c -- RWCox -- October 1998          **
 *************************************************************************/

#define MAX_ITER     5
#define DXY_THRESH   0.07         /* pixels */
#define PHI_THRESH   0.21         /* degrees */
#define DFAC         (PI/180.0)

static float dxy_thresh = DXY_THRESH ,
             phi_thresh = PHI_THRESH ,
             delfac     = 1.5 ;

static int max_iter = MAX_ITER ;
static int ax1 = 0 , ax2 = 1 , ax3 = 2 ;
static int dcode = -1 ;

static int wproc = 0 ;   /* 06 Jun 2002: process imwt? */
static int wtrim = 0 ;   /* 06 Jun 2002: trimming stuff */

static float sinit = 1.0 ; /* 22 Mar 2004: init scale? */

#define DOTRIM (basis->xa >= 0)

#define IMTRIM(qqq) mri_cut_3D( qqq , basis->xa,basis->xb ,  \
                                      basis->ya,basis->yb ,  \
                                      basis->za,basis->zb  )

/*! Macro to replace an image with a trimmed copy, if needed. */

#define TRIM(imq)                      \
  do{ if( DOTRIM ){                    \
        MRI_IMAGE *qim = IMTRIM(imq) ; \
        mri_free(imq) ; imq = qim ;    \
  } } while(0)

/*! Macro to print out a volume range in verbose mode. */

#define VRANG(str,imq)                            \
 do{ if(verbose){                                 \
       double tt=mri_max(imq), bb=mri_min(imq) ;  \
       fprintf(stderr,"  %s range: min=%g  max=%g\n",str,bb,tt); } } while(0)

/*--------------------------------------------------------------------*/

void mri_3dalign_wtrimming( int ttt ){ wtrim = ttt; } /* 06 Jun 2002 */

void mri_3dalign_wproccing( int ttt ){ wproc = ttt; } /* 06 Jun 2002 */

void mri_3dalign_scaleinit( float ttt )               /* 22 Mar 2004 */
{
  if( ttt > 0.0 ) sinit = ttt ;
}

/*--------------------------------------------------------------------*/

void mri_3dalign_params( int maxite ,
                         float dxy , float dph , float dfac ,
                         int bx1 , int bx2 , int bx3 , int dc )
{
   if( maxite > 0   ) max_iter    = maxite ; else max_iter    = MAX_ITER    ;
   if( dxy    > 0.0 ) dxy_thresh  = dxy    ; else dxy_thresh  = DXY_THRESH  ;
   if( dph    > 0.0 ) phi_thresh  = dph    ; else phi_thresh  = PHI_THRESH  ;
   if( dfac   > 0.0 ) delfac      = dfac   ;

   if( bx1 >= 0 && bx1 <= 2 ) ax1 = bx1 ;
   if( bx2 >= 0 && bx2 <= 2 ) ax2 = bx2 ;
   if( bx3 >= 0 && bx3 <= 2 ) ax3 = bx3 ;

   dcode = dc ;
   return ;
}

/*--------------------------------------------------------------------*/

static float init_dth1=0.0 , init_dth2=0.0 , init_dth3=0.0 ;
static float init_dx  =0.0 , init_dy  =0.0 , init_dz  =0.0 ;

#define CLEAR_INITVALS mri_3dalign_initvals(0.0,0.0,0.0,0.0,0.0,0.0)

#define NONZERO_INITVALS                                        \
 ( init_dth1 != 0.0 || init_dth2 != 0.0 || init_dth3 != 0.0 ||  \
   init_dx   != 0.0 || init_dy   != 0.0 || init_dz   != 0.0   )

void mri_3dalign_initvals( float th1,float th2,float th3 ,
                           float dx ,float dy ,float dz   )
{
   init_dth1 = th1 ; init_dth2 = th2 ; init_dth3 = th3 ;  /* degrees */
   init_dx   = dx  ; init_dy   = dy  ; init_dz   = dz  ;  /* mm      */
}

/*--------------------------------------------------------------------*/

static int regmode = MRI_QUINTIC ;
static int verbose = 0 ;
static int noreg   = 0 ;
static int clipit  = 0 ;

void mri_3dalign_method( int rmode , int verb , int norgg , int clip )
{
   regmode = rmode ;
   verbose = verb ;
   noreg   = norgg ;
   clipit  = clip ;
   return ;
}

/*-------------------------------------------------------------------*/

static float blurit = 0.0 ;
void mri_3dalign_blurring( float bl ){ blurit = bl ; return ; }

static int final_regmode = -1 ;            /* 20 Nov 1998 */
void mri_3dalign_final_regmode( int frm )
{
   final_regmode = frm ;
   return ;
}

/*-------------------------------------------------------------------*/

static int xedge=-1 , yedge=-1 , zedge=-1 ;
static int xfade    , yfade    , zfade    ;

static int force_edging=0 ;

void mri_3dalign_edging( int x , int y , int z )  /* 10 Dec 2000 */
{
   xedge = x ; yedge = y ; zedge = z ;
}

void mri_3dalign_force_edging( int n )
{
   force_edging = n ;
}

void mri_3dalign_edging_default( int nx , int ny , int nz )
{
   char *ef=my_getenv("AFNI_VOLREG_EDGING") , *eq ;

   if( ef == NULL ){                  /* the 5% solution */
      xfade = (int)(0.05*nx+0.5) ;
      yfade = (int)(0.05*ny+0.5) ;
      zfade = (int)(0.05*nz+0.5) ;
   } else {
      float ff = strtod(ef,&eq) ;
      if( ff < 0 ){                   /* again */
         xfade = (int)(0.05*nx+0.5) ;
         yfade = (int)(0.05*ny+0.5) ;
         zfade = (int)(0.05*nz+0.5) ;
      } else {
         if( *eq == '%' ){            /* the whatever % solution */
            xfade = (int)(0.01*ff*nx+0.5) ;
            yfade = (int)(0.01*ff*ny+0.5) ;
            zfade = (int)(0.01*ff*nz+0.5) ;
         } else {                     /* the fixed value solution */
            xfade = (int)( MIN(0.25*nx,ff) ) ;
            yfade = (int)( MIN(0.25*ny,ff) ) ;
            zfade = (int)( MIN(0.25*nz,ff) ) ;
         }
      }
   }
}

/*--------------------------------------------------------------------
   Inputs: imbase = base image for alignment
           imwt   = image of weight factors to align to
                      (if NULL, will generate one internally)

   Output: pointer to a MRI_3dalign_basis struct, for later use.
           The malloc-ed data in there can be freed using
           routine MRI_3dalign_cleanup.
----------------------------------------------------------------------*/

MRI_3dalign_basis * mri_3dalign_setup( MRI_IMAGE *imbase , MRI_IMAGE *imwt )
{
   MRI_IMAGE *bim , *pim , *mim , *dim , *imww , *cim ;
   float *dar , *par , *mar ;
   float delta , dx,dy,dz ;
   int ii ;
   MRI_IMARR * fitim  =NULL;
   double * chol_fitim=NULL ;
   MRI_3dalign_basis *basis = NULL ;

ENTRY("mri_3dalign_setup") ;

   if( !MRI_IS_3D(imbase) ){
     fprintf(stderr,"\n*** mri_3dalign_setup: cannot use nD images!\a\n") ;
     RETURN( NULL );
   }

   /*--- create output struct ---*/

   /* from malloc    12 Feb 2009 [lesstif patrol] */
   basis = (MRI_3dalign_basis *) calloc( 1, sizeof(MRI_3dalign_basis) ) ;

   /*-- local copy of input image --*/

   cim = mri_to_float( imbase ) ;

   dx = fabs(cim->dx) ; if( dx == 0.0 ) dx = 1.0 ;
   dy = fabs(cim->dy) ; if( dy == 0.0 ) dy = 1.0 ;
   dz = fabs(cim->dz) ; if( dz == 0.0 ) dz = 1.0 ;

   /*--- get the weighting image ---*/

   if( imwt != NULL &&
       (imwt->nx != cim->nx || imwt->ny != cim->ny || imwt->nz != cim->nz) ){

      fprintf(stderr,"*** WARNING: in mri_3dalign_setup, weight image mismatch!\n") ;
      imwt = NULL ;
   }

   /* make weight up from the base if it isn't supplied */

   if( imwt == NULL ){
      int nx=cim->nx , ny=cim->ny , nz=cim->nz , nxy = nx*ny , nxyz=nxy*nz ;
      int ii ;
      float * f , clip ;

      /* copy base image */

      imww = mri_to_float( cim ) ; f = MRI_FLOAT_PTR(imww) ;

      if( verbose ) fprintf(stderr,"  initializing weight") ;

      for( ii=0 ; ii < nxyz ; ii++ ) f[ii] = fabs(f[ii]) ;  /* 16 Nov 1998 */

#if 1
      EDIT_blur_volume_3d( nx,ny,nz , dx,dy,dz ,
                           MRI_float , f , 3.0*dx , 3.0*dy , 3.0*dz ) ;
#else
      MRI_5blur_inplace_3D( imww ) ;  /* 07 Jun 2002 */
#endif

      if( verbose ) fprintf(stderr,":") ;

      clip  = 0.025 * mri_max(imww) ;
      for( ii=0 ; ii < nxyz ; ii++ ) if( f[ii] < clip ) f[ii] = 0.0 ;

   } else {
      imww = mri_to_float( imwt ) ;  /* just copy input weight image */

      if( wproc ){  /* 06 Jun 2002: process input weight */
        int nx=cim->nx , ny=cim->ny , nz=cim->nz , nxy = nx*ny , nxyz=nxy*nz ;
        int ii ;
        float * f , clip ;

        if( verbose ) fprintf(stderr,"  processing weight") ;
        f = MRI_FLOAT_PTR(imww) ;
        for( ii=0 ; ii < nxyz ; ii++ ) f[ii] = fabs(f[ii]) ;  /* 16 Nov 1998 */
#if 1
        EDIT_blur_volume_3d( nx,ny,nz , dx,dy,dz ,
                             MRI_float , f , 3.0*dx , 3.0*dy , 3.0*dz ) ;
#else
        MRI_5blur_inplace_3D( imww ) ;  /* 07 Jun 2002 */
#endif
        if( verbose ) fprintf(stderr,":") ;
        clip  = 0.025 * mri_max(imww) ;
        for( ii=0 ; ii < nxyz ; ii++ ) if( f[ii] < clip ) f[ii] = 0.0 ;
      }
   }

   /*-- 10 Dec 2000: user-controlled fade out around the edges --*/

   if( imwt == NULL || force_edging ){
     int ff , ii,jj,kk ;
     int nx=cim->nx , ny=cim->ny , nz=cim->nz , nxy = nx*ny ;
     float *f = MRI_FLOAT_PTR(imww) ;

     xfade = xedge ; yfade = yedge ; zfade = zedge ;  /* static variables */

     if( xfade < 0 || yfade < 0 || zfade < 0 )
        mri_3dalign_edging_default(nx,ny,nz) ;        /* reassign fades */

#define FF(i,j,k) f[(i)+(j)*nx+(k)*nxy]

      for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
            for( ff=0 ; ff < zfade ; ff++ )
               FF(ii,jj,ff) = FF(ii,jj,nz-1-ff) = 0.0 ;

      for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
            for( ff=0 ; ff < xfade ; ff++ )
               FF(ff,jj,kk) = FF(nx-1-ff,jj,kk) = 0.0 ;

      for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
            for( ff=0 ; ff < yfade ; ff++ )
               FF(ii,ff,kk) = FF(ii,ny-1-ff,kk) = 0.0 ;
   }

   /*-- 06 Jun 2002: compute wtrimmed volume size --*/

   basis->xa = -1 ;  /* flag for no wtrim */

   if( wtrim ){
     int xa=-1,xb , ya,yb , za,zb ;
     MRI_autobbox( imww , &xa,&xb , &ya,&yb , &za,&zb ) ;
     if( xa >= 0 ){
       float nxyz = imww->nx * imww->ny * imww->nz ;
       float nttt = (xb-xa+1)*(yb-ya+1)*(zb-za+1) ;
       float trat = 100.0 * nttt / nxyz ;

       if( verbose )
         fprintf(stderr,"  wtrim: [%d..%d]x[%d..%d]x[%d..%d]"
                        " = %d voxels kept, out of %d (%.1f%%)\n" ,
                 xa,xb , ya,yb , za,zb , (int)nttt , (int)nxyz , trat ) ;

       /* keep trimming if saves at least 10% per volume */

       if( trat < 90.0 ){
         basis->xa = xa ; basis->xb = xb ;
         basis->ya = ya ; basis->yb = yb ;
         basis->za = za ; basis->zb = zb ;

         TRIM(imww) ;
       } else if( verbose ){
         fprintf(stderr,"  skipping use of trim - too little savings\n");
       }
     }
   }

   VRANG("weight",imww) ;
   if( verbose ){
     float *f=MRI_FLOAT_PTR(imww) , perc ;
     int ii , nxyz = imww->nvox , nzer=0 ;
     for( ii=0 ; ii < nxyz ; ii++ ) nzer += (f[ii] == 0.0) ;
     perc = (100.0*nzer)/nxyz ;
     fprintf(stderr,"  # zero weights=%d out of %d (%.1f%%)\n",nzer,nxyz,perc);
   }

   /*-- base image --*/

   INIT_IMARR( fitim ) ;             /* array of fitting images */

   if( DOTRIM ) bim = IMTRIM(cim) ;  /* a trimmed duplicate */
   else         bim = cim ;          /* base image */
   ADDTO_IMARR( fitim , bim ) ;

   THD_rota_method( regmode ) ;

#ifndef MEGA
#define MEGA (1024*1024)
#endif
   if( verbose ) fprintf(stderr ,
                         "  mri_3dalign: using %d Mbytes of workspace\n" ,
                         10 * bim->nvox * bim->pixel_size / MEGA ) ;

   /*-- d/d(th1) image [angles in degrees here] --*/

   if( verbose ) fprintf(stderr,"  initializing d/d(th1)") ;

   delta = 2.0*delfac/( cim->nx + cim->ny + cim->nz ) ;
   if( verbose ) fprintf(stderr,"[delta=%g]",delta) ;

   pim = THD_rota3D( cim , ax1,delta , ax2,0.0 , ax3,0.0 ,
                     dcode , 0.0 , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   mim = THD_rota3D( cim , ax1,-delta , ax2,0.0 , ax3,0.0 ,
                     dcode , 0.0 , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   dim   = mri_new_conforming( cim , MRI_float ) ;
   delta = sinit * 0.5 * DFAC / delta ;
   dar   = MRI_FLOAT_PTR(dim) ; par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
   for( ii=0 ; ii < dim->nvox ; ii++ )
      dar[ii] = delta * ( mar[ii] - par[ii] ) ;
   mri_free(pim) ; mri_free(mim) ;
   TRIM( dim ) ; ADDTO_IMARR( fitim , dim ) ;
   VRANG("d/d(th1)",dim) ;

   /*-- d/d(th2) image --*/

   if( verbose ) fprintf(stderr,"  initializing d/d(th2)") ;

   delta = 2.0*delfac/( cim->nx + cim->ny + cim->nz ) ;
   if( verbose ) fprintf(stderr,"[delta=%g]",delta) ;

   pim = THD_rota3D( cim , ax1,0.0 , ax2,delta , ax3,0.0 ,
                     dcode , 0.0 , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   mim = THD_rota3D( cim , ax1,0.0 , ax2,-delta , ax3,0.0 ,
                     dcode , 0.0 , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   dim   = mri_new_conforming( cim , MRI_float ) ;
   delta = sinit * 0.5 * DFAC / delta ;
   dar   = MRI_FLOAT_PTR(dim) ; par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
   for( ii=0 ; ii < dim->nvox ; ii++ )
      dar[ii] = delta * ( mar[ii] - par[ii] ) ;
   mri_free(pim) ; mri_free(mim) ;
   TRIM( dim ) ; ADDTO_IMARR( fitim , dim ) ;
   VRANG("d/d(th2)",dim) ;

   /*-- d/d(th3) image --*/

   if( verbose ) fprintf(stderr,"  initializing d/d(th3)") ;

   delta = 2.0*delfac/( cim->nx + cim->ny + cim->nz ) ;
   if( verbose ) fprintf(stderr,"[delta=%g]",delta) ;

   pim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,delta ,
                     dcode , 0.0 , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   mim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,-delta ,
                     dcode , 0.0 , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   dim   = mri_new_conforming( cim , MRI_float ) ;
   delta = sinit * 0.5 * DFAC / delta ;
   dar   = MRI_FLOAT_PTR(dim) ; par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
   for( ii=0 ; ii < dim->nvox ; ii++ )
      dar[ii] = delta * ( mar[ii] - par[ii] ) ;
   mri_free(pim) ; mri_free(mim) ;
   TRIM( dim ) ; ADDTO_IMARR( fitim , dim ) ;
   VRANG("d/d(th3)",dim) ;

   /*-- d/dx image --*/

   if( verbose ) fprintf(stderr,"  initializing d/dx") ;

   delta = delfac * dx ;
   if( verbose ) fprintf(stderr,"[delta=%g]",delta) ;

   pim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,0.0 ,
                     dcode , delta , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   mim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,0.0 ,
                     dcode , -delta , 0.0 , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   dim   = mri_new_conforming( cim , MRI_float ) ;
   delta = sinit * 0.5 / delta ;
   dar   = MRI_FLOAT_PTR(dim) ; par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
   for( ii=0 ; ii < dim->nvox ; ii++ )
      dar[ii] = delta * ( mar[ii] - par[ii] ) ;
   mri_free(pim) ; mri_free(mim) ;
   TRIM( dim ) ; ADDTO_IMARR( fitim , dim ) ;
   VRANG("d/dx",dim) ;

   /*-- d/dy image --*/

   if( verbose ) fprintf(stderr,"  initializing d/dy") ;

   delta = delfac * dy ;
   if( verbose ) fprintf(stderr,"[delta=%g]",delta) ;

   pim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,0.0 ,
                     dcode , 0.0 , delta , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   mim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,0.0 ,
                     dcode , 0.0 , -delta , 0.0 ) ;
   if( verbose ) fprintf(stderr,":") ;

   dim   = mri_new_conforming( cim , MRI_float ) ;
   delta = sinit * 0.5 / delta ;
   dar   = MRI_FLOAT_PTR(dim) ; par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
   for( ii=0 ; ii < dim->nvox ; ii++ )
      dar[ii] = delta * ( mar[ii] - par[ii] ) ;
   mri_free(pim) ; mri_free(mim) ;
   TRIM( dim ) ; ADDTO_IMARR( fitim , dim ) ;
   VRANG("d/dy",dim) ;

   /*-- d/dz image --*/

   if( verbose ) fprintf(stderr,"  initializing d/dz") ;

   delta = delfac * dz ;
   if( verbose ) fprintf(stderr,"[delta=%g]",delta) ;

   pim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,0.0 ,
                     dcode , 0.0 , 0.0 , delta ) ;
   if( verbose ) fprintf(stderr,":") ;

   mim = THD_rota3D( cim , ax1,0.0 , ax2,0.0 , ax3,0.0 ,
                     dcode , 0.0 , 0.0 , -delta ) ;
   if( verbose ) fprintf(stderr,":") ;

   dim   = mri_new_conforming( cim , MRI_float ) ;
   delta = sinit * 0.5 / delta ;
   dar   = MRI_FLOAT_PTR(dim) ; par = MRI_FLOAT_PTR(pim) ; mar = MRI_FLOAT_PTR(mim) ;
   for( ii=0 ; ii < dim->nvox ; ii++ )
      dar[ii] = delta * ( mar[ii] - par[ii] ) ;
   mri_free(pim) ; mri_free(mim) ;
   TRIM( dim ) ; ADDTO_IMARR( fitim , dim ) ;
   VRANG("d/dz",dim) ;

   /*-- done with input copy, unless it is same as base for lsqfit --*/

   if( cim != bim ) mri_free(cim) ;

   /*-- initialize linear least squares --*/

   if( verbose ) fprintf(stderr,"  initializing least squares\n") ;

   chol_fitim = mri_startup_lsqfit( fitim , imww ) ;

   mri_free(imww) ;

   /*-- save stuff --*/

   basis->fitim      = fitim ;
   basis->chol_fitim = chol_fitim ;

   RETURN( basis );
}

/*-----------------------------------------------------------------------
   Input:   basis  = MRI_3dalign_basis * return from setup routine above.
            im     = MRI_IMAGE * to align to base image

   Output:  Return value is aligned image;
            *dx, *dy, *dz, *th1, *th2, *th3 are set to estimated
            alignment parameters.  Note that returned image is floats.

   20 May 2009: modified to return a complex-valued image if the input
                is complex-valued (estimation is done on abs image)
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_3dalign_one( MRI_3dalign_basis *basis , MRI_IMAGE *im ,
                             float *th1 , float *th2 , float *th3 ,
                             float *dx  , float *dy  , float *dz    )
{
   MRI_IMARR *fitim ;
   double *chol_fitim=NULL ;
   float *fit , *dfit ;
   int iter , good , ii ;
   float dxt , dyt , dzt , ftop,fbot ;
   MRI_IMAGE *tim=NULL , *fim ;

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
        mri_free( tim ) ; tim = NULL ;
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
      mri_free( tim ) ; tim = NULL ;

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

   tim = NULL ;
   if( ! noreg ){
      if( final_regmode < 0 ) final_regmode = regmode ;  /* 20 Nov 1998 */
      THD_rota_method( final_regmode ) ;

      if( im->kind == MRI_complex ){   /* 20 May 2009: special case! */
        MRI_IMARR *impair ; MRI_IMAGE *rim,*iim , *xim,*yim ;
        impair = mri_complex_to_pair(im) ;
        if( impair == NULL ){
          ERROR_message("mri_complex_to_pair fails in mri_3dalign_one!") ;
        } else {
          rim = IMAGE_IN_IMARR(impair,0) ;
          iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
          xim = THD_rota3D( rim ,
                            ax1,fit[1]*DFAC, ax2,fit[2]*DFAC, ax3,fit[3]*DFAC,
                            dcode , fit[4],fit[5],fit[6] ) ; mri_free(rim) ;
          yim = THD_rota3D( iim ,
                            ax1,fit[1]*DFAC, ax2,fit[2]*DFAC, ax3,fit[3]*DFAC,
                            dcode , fit[4],fit[5],fit[6] ) ; mri_free(iim) ;
          tim = mri_pair_to_complex(xim,yim) ; mri_free(xim) ; mri_free(yim) ;
        }

      } else {                  /* real-valued input */
        tim = THD_rota3D( fim ,
                          ax1,fit[1]*DFAC , ax2,fit[2]*DFAC , ax3,fit[3]*DFAC ,
                          dcode , fit[4],fit[5],fit[6] ) ;
      }
   }

   if( tim != NULL && tim->kind == MRI_float && clipit &&
       (final_regmode == MRI_QUINTIC || final_regmode==MRI_CUBIC  ||
        final_regmode == MRI_HEPTIC  || final_regmode==MRI_FOURIER  ) ){

      register int ii ;
      register float ftop , fbot , *tar ;

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
